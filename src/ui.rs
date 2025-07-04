use bevy::{
    app::AppExit,
    diagnostic::{DiagnosticsStore, FrameTimeDiagnosticsPlugin},
    prelude::*,
};

use crate::gamestate::{GameState, MenuStack};

const MAIN_FONT: &str = "fonts/minecraft.otf";

// Marker struct to help identify the FPS UI component, since there may be many
// Text components.
#[derive(Component)]
pub struct FpsText
{
    pub timer: Timer,
}

// This systems periodically updates the FPS text in the UI.
pub fn text_update_system(
    diagnostics: Res<DiagnosticsStore>,
    mut query: Query<(&mut TextSpan, &mut FpsText)>,
    time: Res<Time>,
)
{
    for (mut span, mut fps_text) in &mut query
    {
        // Only update the counter if the timer period has just ended.
        if fps_text.timer.tick(time.delta()).just_finished()
        {
            // Get the FPS diagnostic.
            if let Some(fps) = diagnostics.get(&FrameTimeDiagnosticsPlugin::FPS)
            {
                // Get the smoothed FPS value.
                if let Some(value) = fps.smoothed()
                {
                    // Update the text.
                    **span = format!("{value:.0}");
                }
            }
        }
    }
}

// Setup function for creating UI elements like FPS counter and crosshair.
pub fn setup_ui(mut commands: Commands, assets: Res<AssetServer>)
{
    // Crosshair.
    commands.spawn((
        Sprite::from_image(assets.load("textures/crosshair.png")),
        Transform::from_translation(Transform::IDENTITY.translation + Vec3::new(0.0, 0.0, 1.0)),
    ));

    // FPS counter.
    commands
        .spawn((
            // Create a Text with multiple possible spans.
            Text::new("FPS: "),
            TextFont { font: assets.load(MAIN_FONT), font_size: 30.0, ..default() },
        ))
        .with_child((
            // Create a TextSpan that will be updated with the FPS value.
            TextSpan::default(),
            TextFont { font: assets.load(MAIN_FONT), font_size: 30.0, ..Default::default() },
            // Initialize the timer.
            FpsText { timer: Timer::from_seconds(0.5, TimerMode::Repeating) },
        ));
}

// Helper function to create a color from u8 RGBA values.
const fn rgba(r: u8, g: u8, b: u8, a: u8) -> Color
{
    Color::srgba(r as f32 / 255.0, g as f32 / 255.0, b as f32 / 255.0, a as f32 / 255.0)
}

const BUTTON_COLOR_BASE: Color = rgba(76, 86, 106, 255);
const BUTTON_COLOR_HOVER: Color = rgba(67, 76, 94, 255);
const BUTTON_COLOR_PRESSED: Color = rgba(59, 66, 82, 255);
const BUTTON_BORDER_COLOR: Color = Color::BLACK;
const BUTTON_FONT_COLOR: Color = Color::WHITE;

const BUTTON_FONT_SIZE: f32 = 30.0;
const BUTTON_WIDTH: f32 = 350.0;
const BUTTON_HEIGHT: f32 = 80.0;
const BUTTON_BORDER_SIZE: f32 = 5.0;

const BUTTON_CORNER_RADIUS: f32 = 5.0;

const BUTTON_PADDING: f32 = 10.0;

pub const fn get_semitransparent_panel_width(cols: u8) -> f32
{
    // Calculate the width of a semi-transparent panel based on the number of
    // columns.
    return (cols as f32 * BUTTON_WIDTH) + ((cols as f32 + 1.0) * BUTTON_PADDING);
}

pub const fn get_semitransparent_panel_height(rows: u8) -> f32
{
    // Calculate the height of a semi-transparent panel based on the number of
    // rows.
    return (rows as f32 * BUTTON_HEIGHT) + ((rows as f32 + 1.0) * BUTTON_PADDING);
}

// Marker component to identify which button was pressed.
#[derive(Component, Clone)]
pub enum ButtonAction
{
    NewWorld,
    LoadWorld,
    Settings,
    Quit,
    MainMenu,
    Back,
}

// Helper function to create a button with a specific marker component.
pub fn create_button(text: &str, action: ButtonAction, assets: &Res<AssetServer>) -> impl Bundle
{
    (
        Button,
        action,
        Node {
            width: Val::Px(BUTTON_WIDTH),
            height: Val::Px(BUTTON_HEIGHT),
            border: UiRect::all(Val::Px(BUTTON_BORDER_SIZE)),
            justify_content: JustifyContent::Center,
            align_items: AlignItems::Center,
            ..default()
        },
        BorderColor(BUTTON_BORDER_COLOR),
        BorderRadius::all(Val::Px(BUTTON_CORNER_RADIUS)),
        BackgroundColor(BUTTON_COLOR_BASE),
        children![(
            Text::new(text),
            TextFont { font: assets.load(MAIN_FONT), font_size: BUTTON_FONT_SIZE, ..default() },
            TextColor(BUTTON_FONT_COLOR),
            TextShadow::default(),
        )],
    )
}

// Setup function for creating in-game UI elements like FPS counter and
// crosshair.
pub fn setup_ingame_ui(mut commands: Commands, assets: Res<AssetServer>)
{
    // Crosshair.
    commands.spawn((
        crate::gamestate::InGameUI,
        Sprite::from_image(assets.load("textures/crosshair.png")),
        Transform::from_translation(Transform::IDENTITY.translation + Vec3::new(0.0, 0.0, 1.0)),
    ));

    // FPS counter.
    commands
        .spawn((
            crate::gamestate::InGameUI,
            Text::new("FPS: "),
            TextFont { font: assets.load(MAIN_FONT), font_size: 30.0, ..default() },
        ))
        .with_child((
            TextSpan::default(),
            TextFont { font: assets.load(MAIN_FONT), font_size: 30.0, ..Default::default() },
            FpsText { timer: Timer::from_seconds(0.5, TimerMode::Repeating) },
        ));
}

pub fn button_system(
    mut interaction_query: Query<
        (&Interaction, &mut BackgroundColor, &mut BorderColor, &ButtonAction),
        (Changed<Interaction>, With<Button>),
    >,
    mut menu_stack: ResMut<MenuStack>,
    mut next_state: ResMut<NextState<GameState>>,
    mut app_exit_events: EventWriter<AppExit>,
)
{
    // Handle visual button states.
    for (interaction, mut color, mut border_color, action) in &mut interaction_query
    {
        match *interaction
        {
            Interaction::Pressed =>
            {
                *color = BUTTON_COLOR_PRESSED.into();
                border_color.0 = BUTTON_BORDER_COLOR;

                // Process the associated action.
                match action
                {
                    ButtonAction::NewWorld =>
                    {
                        next_state.set(GameState::InGame);
                    },
                    ButtonAction::LoadWorld => (),
                    ButtonAction::Settings =>
                    {
                        next_state.set(GameState::Settings);
                    },
                    ButtonAction::Quit =>
                    {
                        app_exit_events.write(AppExit::Success);
                    },
                    ButtonAction::MainMenu =>
                    {
                        next_state.set(GameState::MainMenu);
                    },
                    ButtonAction::Back =>
                    {
                        // Go back to the previous menu state.
                        menu_stack.0.pop();
                        if let Some(state) = menu_stack.0.last()
                        {
                            next_state.set(state.clone());
                        }
                        else
                        {
                            // If no previous state, go to the main menu (should not happen).
                            next_state.set(GameState::MainMenu);
                        }
                    },
                }
            },
            Interaction::Hovered =>
            {
                *color = BUTTON_COLOR_HOVER.into();
                border_color.0 = BUTTON_BORDER_COLOR;
            },
            Interaction::None =>
            {
                *color = BUTTON_COLOR_BASE.into();
                border_color.0 = BUTTON_BORDER_COLOR;
            },
        }
    }
}
