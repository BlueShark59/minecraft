use std::collections::HashMap;

use bevy::{
    asset::RenderAssetUsages,
    prelude::*,
    render::mesh::{Indices, PrimitiveTopology},
};

use crate::{
    ChunkFace, Map, Modification,
    blocks::BlockList,
    chunks::{Chunk, apply_modifications, load_chunk_face},
    types::{BlockType, CHUNK_HEIGHT, CHUNK_SIZE, ChunkPos},
};

// The faces of each block, used for mesh generation.
pub const FACES: &[(IVec3, [[f32; 3]; 4], [[f32; 2]; 4])] = &[
    // +X
    (
        IVec3::new(1, 0, 0),
        [[1., 0., 0.], [1., 1., 0.], [1., 1., 1.], [1., 0., 1.]],
        [[0., 1.], [0., 0.], [1., 0.], [1., 1.]],
    ),
    // -X
    (
        IVec3::new(-1, 0, 0),
        [[0., 0., 1.], [0., 1., 1.], [0., 1., 0.], [0., 0., 0.]],
        [[0., 1.], [0., 0.], [1., 0.], [1., 1.]],
    ),
    // +Y (top)
    (
        IVec3::new(0, 1, 0),
        [[0., 1., 0.], [0., 1., 1.], [1., 1., 1.], [1., 1., 0.]],
        [[0., 1.], [0., 0.], [1., 0.], [1., 1.]],
    ),
    // -Y (bottom)
    (
        IVec3::new(0, -1, 0),
        [[0., 0., 1.], [0., 0., 0.], [1., 0., 0.], [1., 0., 1.]],
        [[0., 1.], [0., 0.], [1., 0.], [1., 1.]],
    ),
    // +Z
    (
        IVec3::new(0, 0, 1),
        [[1., 0., 1.], [1., 1., 1.], [0., 1., 1.], [0., 0., 1.]],
        [[0., 1.], [0., 0.], [1., 0.], [1., 1.]],
    ),
    // -Z
    (
        IVec3::new(0, 0, -1),
        [[0., 0., 0.], [0., 1., 0.], [1., 1., 0.], [1., 0., 0.]],
        [[0., 1.], [0., 0.], [1., 0.], [1., 1.]],
    ),
];

// Optimized mesh data structure for better cache locality.
#[derive(Default)]
struct MeshData
{
    positions: Vec<[f32; 3]>,
    normals: Vec<[f32; 3]>,
    uvs: Vec<[f32; 2]>,
    indices: Vec<u32>,
}

impl MeshData
{
    fn with_capacity(face_count_estimate: usize) -> Self
    {
        let vertex_count = face_count_estimate * 4;
        let index_count = face_count_estimate * 6;
        Self {
            positions: Vec::with_capacity(vertex_count),
            normals: Vec::with_capacity(vertex_count),
            uvs: Vec::with_capacity(vertex_count),
            indices: Vec::with_capacity(index_count),
        }
    }

    // Inline add_face to avoid function call overhead
    #[inline(always)]
    fn add_face_direct(&mut self, x: f32, y: f32, z: f32, face_idx: usize)
    {
        let base_index = self.positions.len() as u32;
        let (dir, verts, base_uvs) = unsafe { FACES.get_unchecked(face_idx) };

        // Convert face direction to f32 for faster access.
        let dir_f32 = [dir.x as f32, dir.y as f32, dir.z as f32];

        // Add vertices with manual unrolling for better performance.
        self.positions
            .push([x + verts[0][0], y + verts[0][1], z + verts[0][2]]);
        self.positions
            .push([x + verts[1][0], y + verts[1][1], z + verts[1][2]]);
        self.positions
            .push([x + verts[2][0], y + verts[2][1], z + verts[2][2]]);
        self.positions
            .push([x + verts[3][0], y + verts[3][1], z + verts[3][2]]);

        // Add normals - same for all 4 vertices.
        self.normals
            .extend_from_slice(&[dir_f32, dir_f32, dir_f32, dir_f32]);

        // Add UVs.
        self.uvs.extend_from_slice(base_uvs);

        // Add indices for two triangles.
        self.indices.extend_from_slice(&[
            base_index,
            base_index + 1,
            base_index + 2,
            base_index,
            base_index + 2,
            base_index + 3,
        ]);
    }
}

// Pre-computed block transparency lookup table for faster access.
struct TransparencyCache
{
    cache: Vec<bool>, // indexed by BlockType as u16.
}

impl TransparencyCache
{
    fn new(block_list: &BlockList) -> Self
    {
        let mut cache = vec![true; 256]; // Assuming BlockType fits in u16.
        for (&block_type, block) in &block_list.data
        {
            cache[block_type as u16 as usize] = block.transparent;
        }
        Self { cache }
    }

    #[inline(always)]
    fn is_transparent(&self, block_type: BlockType) -> bool
    {
        // Using get_unchecked for maximum performance since we control the input.
        unsafe { *self.cache.get_unchecked(block_type as u16 as usize) }
    }

    #[inline(always)]
    fn is_opaque(&self, block_type: BlockType) -> bool
    {
        !self.is_transparent(block_type)
    }
}

// Optimized chunk meshing with better cache locality and fewer allocations.
pub fn mesh_chunk(
    chunk: &Chunk,
    block_list: &BlockList,
    neighbor_chunks: &HashMap<ChunkPos, Chunk>,
    seed: u64,
    modifications: &std::collections::HashMap<ChunkPos, Vec<Modification>>,
) -> HashMap<Handle<Image>, Mesh>
{
    const CS: usize = CHUNK_SIZE as usize;
    const CH: usize = CHUNK_HEIGHT as usize;
    const CS_I32: i32 = CHUNK_SIZE as i32;
    const CH_I32: i32 = CHUNK_HEIGHT as i32;

    // Pre-compute transparency cache for faster lookups.
    let transparency_cache = TransparencyCache::new(block_list);

    // Estimate mesh sizes to reduce allocations (rough estimate: ~20% of blocks
    // have exposed faces).
    let estimated_faces = (CS * CS * CH) / 20;

    // Use a more efficient data structure.
    let mut per_tex: HashMap<Handle<Image>, MeshData> = HashMap::new();
    let mut face_cache: HashMap<(ChunkPos, ChunkFace), Chunk> = HashMap::new();

    // Pre-allocate arrays for neighbor chunk lookups to avoid repeated HashMap
    // access.
    let mut neighbor_cache: [Option<&Chunk>; 4] = [None; 4]; // East, West, North, South
    let directions = [
        (ChunkPos { x: 1, y: 0 }),  // East
        (ChunkPos { x: -1, y: 0 }), // West
        (ChunkPos { x: 0, y: 1 }),  // North
        (ChunkPos { x: 0, y: -1 }), // South
    ];

    for (i, &dir) in directions.iter().enumerate()
    {
        neighbor_cache[i] = neighbor_chunks.get(&(chunk.pos + dir));
    } 
    // Optimized neighbor block checking.
    let mut is_opaque_fast = |x: i32, y: i32, z: i32| -> bool {
        // Early bounds check for Y.
        if y < 0 || y >= CH_I32
        {
            return false;
        }

        // Check if we need a neighbor chunk.
        let (target_chunk, adj_x, adj_z) = if x < 0
        {
            (neighbor_cache[1], x + CS_I32, z) // West
        }
        else if x >= CS_I32
        {
            (neighbor_cache[0], x - CS_I32, z) // East
        }
        else if z < 0
        {
            (neighbor_cache[3], x, z + CS_I32) // South
        }
        else if z >= CS_I32
        {
            (neighbor_cache[2], x, z - CS_I32) // North
        }
        else
        {
            (Some(chunk), x, z) // Current chunk
        };
        match target_chunk
        {
            Some(target) =>
            {
                let idx = (y as usize) * CS * CS + (adj_z as usize) * CS + (adj_x as usize);
                let block_type = target.blocks[idx];
                transparency_cache.is_opaque(block_type)
            },
            None =>
            {
                // Generate face for missing neighbor chunk.
                let target_pos = if x < 0
                {
                    ChunkPos { x: chunk.pos.x - 1, y: chunk.pos.y }
                }
                else if x >= CS_I32
                {
                    ChunkPos { x: chunk.pos.x + 1, y: chunk.pos.y }
                }
                else if z < 0
                {
                    ChunkPos { x: chunk.pos.x, y: chunk.pos.y - 1 }
                }
                else
                {
                    ChunkPos { x: chunk.pos.x, y: chunk.pos.y + 1 }
                };

                let face_type = if x < 0
                {
                    ChunkFace::East
                }
                else if x >= CS_I32
                {
                    ChunkFace::West
                }
                else if z < 0
                {
                    ChunkFace::South
                }
                else
                {
                    ChunkFace::North
                };

                let cache_key = (target_pos, face_type);
                let temp_chunk = face_cache.entry(cache_key).or_insert_with(|| {
                    let mut chunk_face = load_chunk_face(seed, target_pos, face_type);
                    if let Some(mods) = modifications.get(&target_pos)
                    {
                        apply_modifications(&mut chunk_face, mods);
                    }
                    chunk_face
                });
                let idx = (y as usize) * CS * CS + (adj_z as usize) * CS + (adj_x as usize);
                let block_type = temp_chunk.blocks[idx];
                transparency_cache.is_opaque(block_type)
            },
        }
    }; 
    // Pre-calculate face direction offsets for neighbor checking.
    const FACE_OFFSETS: [(i32, i32, i32); 6] = [
        (1, 0, 0),  // +X
        (-1, 0, 0), // -X
        (0, 1, 0),  // +Y
        (0, -1, 0), // -Y
        (0, 0, 1),  // +Z
        (0, 0, -1), // -Z
    ];

    // Main meshing loop with optimized iteration order for better cache locality.
    for y_local in 0 .. CH
    {
        for z_local in 0 .. CS
        {
            for x_local in 0 .. CS
            {
                let block_idx = y_local * CS * CS + z_local * CS + x_local;
                let block_type = chunk.blocks[block_idx];

                if block_type == BlockType::Air
                {
                    continue;
                }

                // Cache block data lookup.
                let block_data = &block_list.data[&block_type];
                let face_textures = &block_data.faces;

                let x_f32 = x_local as f32;
                let y_f32 = y_local as f32;
                let z_f32 = z_local as f32;
                let x_i32 = x_local as i32;
                let y_i32 = y_local as i32;
                let z_i32 = z_local as i32;

                // Check each face with unrolled loop for better performance.
                for face_idx in 0 .. 6
                {
                    let (offset_x, offset_y, offset_z) = FACE_OFFSETS[face_idx];
                    let neighbor_x = x_i32 + offset_x;
                    let neighbor_y = y_i32 + offset_y;
                    let neighbor_z = z_i32 + offset_z;

                    // Skip if neighbor is opaque.
                    if is_opaque_fast(neighbor_x, neighbor_y, neighbor_z)
                    {
                        continue;
                    }

                    let texture_handle = &face_textures[face_idx];
                    let mesh_data = per_tex
                        .entry(texture_handle.clone())
                        .or_insert_with(|| MeshData::with_capacity(estimated_faces));

                    // Use the optimized direct add function.
                    mesh_data.add_face_direct(x_f32, y_f32, z_f32, face_idx);
                }
            }
        }
    } // Convert the per-texture mesh data into actual Mesh objects.
    per_tex
        .into_iter()
        .map(|(tex, mesh_data)| {
            let mut mesh = Mesh::new(PrimitiveTopology::TriangleList, RenderAssetUsages::default());
            mesh.insert_attribute(Mesh::ATTRIBUTE_POSITION, mesh_data.positions);
            mesh.insert_attribute(Mesh::ATTRIBUTE_NORMAL, mesh_data.normals);
            mesh.insert_attribute(Mesh::ATTRIBUTE_UV_0, mesh_data.uvs);
            mesh.insert_indices(Indices::U32(mesh_data.indices));
            (tex, mesh)
        })
        .collect()
}

// This system remeshes chunks that have changed since the last frame.
pub fn remesh_changed_chunks(
    mut commands: Commands,
    mut meshes: ResMut<Assets<Mesh>>,
    mut materials: ResMut<Assets<StandardMaterial>>,
    map: Res<Map>,
    block_list: Res<BlockList>,
    query: Query<(Entity, &Chunk, Option<&Children>), Changed<Chunk>>,
    new_chunks: Query<Entity, Added<Chunk>>,
    chunk_map: Res<crate::world::ChunkMap>,
    all_chunks_query: Query<&Chunk>,
)
{
    use std::collections::HashSet;

    // We only want to remesh chunks that were changed in this frame.
    let just_added: HashSet<_> = new_chunks.iter().collect();

    for (chunk_entity, chunk, children_opt) in &query
    {
        if just_added.contains(&chunk_entity)
        {
            // This chunk was just added, so we don't need to remesh it.
            continue;
        }

        // If the chunk has children, we need to despawn them.
        if let Some(children) = children_opt
        {
            for &child in children
            {
                commands.entity(child).despawn();
            }
        }

        // Get neighboring chunks data.
        let neighbor_data =
            crate::world::get_neighbor_chunk_data(chunk.pos, &chunk_map, &all_chunks_query);

        // Build the new meshes for the chunk, one per texture.
        let meshes_by_tex =
            mesh_chunk(chunk, &*block_list, &neighbor_data, map.seed, &map.modified);

        // Spawn one child per (texture, mesh).
        commands.entity(chunk_entity).with_children(|parent| {
            for (tex_handle, mesh) in meshes_by_tex
            {
                let mesh_handle = meshes.add(mesh);

                // Standard material.
                let mat_handle = materials.add(StandardMaterial {
                    base_color_texture: Some(tex_handle.clone()),
                    alpha_mode: AlphaMode::Mask(0.5), // keep cut-out alpha
                    ..default()
                });

                // Spawn the mesh.
                parent.spawn((
                    Mesh3d(mesh_handle),
                    MeshMaterial3d(mat_handle),
                    Visibility::default(),
                ));
            }
        });
    }
}

// This system triggers remeshing for chunks that have modifications requiring
// remesh (like when adjacent chunks have border blocks modified).
pub fn trigger_chunk_remeshing(
    mut map: ResMut<Map>,
    chunk_map: Res<crate::world::ChunkMap>,
    mut chunks: Query<&mut Chunk>,
)
{
    let mut chunks_to_clean = Vec::new();

    for (chunk_pos, modifications) in &map.modified
    {
        // Check if this chunk has any dummy modifications (used for triggering
        // remeshing).
        let has_remesh_trigger = modifications.iter().any(|m| m.index == usize::MAX);

        if has_remesh_trigger
        {
            // Find the chunk entity and trigger a change.
            if let Some(&chunk_entity) = chunk_map.loaded_chunks.get(chunk_pos)
            {
                if let Ok(mut chunk) = chunks.get_mut(chunk_entity)
                {
                    // Simply touch the chunk to trigger the Change<Chunk> detection.
                    // We clone the position to force a change without actually changing data.
                    chunk.pos = chunk.pos;
                }
            }

            // Remove the dummy modifications but keep real ones.
            chunks_to_clean.push(*chunk_pos);
        }
    }

    // Clean up dummy modifications.
    for chunk_pos in chunks_to_clean
    {
        if let Some(modifications) = map.modified.get_mut(&chunk_pos)
        {
            modifications.retain(|m| m.index != usize::MAX);

            // If no real modifications remain, remove the entry completely.
            if modifications.is_empty()
            {
                map.modified.remove(&chunk_pos);
            }
        }
    }
}
