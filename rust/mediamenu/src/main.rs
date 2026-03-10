#[cfg(feature = "real")]
mod searches;

use battery::{Error, Manager};
use chrono::Local;
use crossterm::event::{self, Event};
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;
use directories::BaseDirs;
use env_logger::Target;
use log::error;
use mediathekviewtui::run_app;
use mediathekviewtui::types::{AppConfig, KeyAction, ParsableKeyAction};
use ratatui::layout::Constraint::{Length, Min};
use ratatui::layout::{Alignment, Constraint, Direction, Flex, Layout, Rect};
use ratatui::style::Color;
use ratatui::text::Text;
use ratatui::widgets::{Block, Borders, Clear, Paragraph};
use ratatui::{
    style::{Modifier, Style, Stylize},
    widgets::{List, ListItem, ListState},
    Frame,
};
use sdl2::controller::Button;
#[cfg(feature = "real")]
use searches::mk_searches;
use simple_regex::RegexBuilder;
use std::collections::HashMap;
use std::fs::OpenOptions;
use std::io::stdout;
use std::path::Path;
use strum_macros::EnumIter;

const SELECTION_ITEM_COUNT: u16 = 4;

#[derive(Debug, Clone, EnumIter)]
enum SelectionItem {
    MediathekViewTui,
    JellyTui,
    JellyTuiT,
    Exit,
}

const ALL_SELECTION_ITEMS: [SelectionItem; 4] = [
    SelectionItem::MediathekViewTui,
    SelectionItem::JellyTui,
    SelectionItem::JellyTuiT,
    SelectionItem::Exit,
];

#[derive(Debug, Clone)]
struct State {
    selection: ListState,
    items: [SelectionItem; 4],
    popup_text: Option<String>,
}

fn mk_text(content: &str) -> Text<'_> {
    Text::raw(content).centered()
}

impl<'a> Into<ListItem<'a>> for SelectionItem {
    fn into(self) -> ListItem<'a> {
        let content = match self {
            Self::MediathekViewTui => mk_text("mediathekviewtui"),
            Self::JellyTui => mk_text("jellytui"),
            Self::JellyTuiT => mk_text("jellytui(t)"),
            Self::Exit => mk_text("exit"),
        };
        ListItem::new(content)
    }
}

// TODO remove some unwarps. Just log error and continue.
#[tokio::main]
async fn main() {
    let base_dirs = BaseDirs::new().unwrap();
    let data_dir = base_dirs.data_dir().join("mediamenu");
    std::fs::create_dir_all(&data_dir).unwrap();
    let log_path = data_dir.join("debug.log");
    let log_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(log_path)
        .unwrap();
    let _ = env_logger::builder()
        .target(Target::Pipe(Box::new(log_file)))
        .init();
    let mut terminal = ratatui::init();
    color_eyre::install().unwrap();

    let mut state = State {
        selection: ListState::default(),
        items: ALL_SELECTION_ITEMS,
        popup_text: None,
    };
    stdout()
        .execute(crossterm::terminal::Clear(ClearType::All))
        .unwrap();
    let manager = battery::Manager::new();
    terminal
        .draw(|frame| render(&mut state, frame, &manager))
        .unwrap();
    loop {
        match event::read() {
            Ok(Event::Key(key_event)) if key_event.is_press() => {
                if state.popup_text.is_some() {
                    state.popup_text = None
                } else {
                    match key_event.code {
                        event::KeyCode::Up => {
                            state.selection.select_previous();
                            terminal
                                .draw(|frame| render(&mut state, frame, &manager))
                                .unwrap();
                        }
                        event::KeyCode::Down => {
                            state.selection.select_next();
                            terminal
                                .draw(|frame| render(&mut state, frame, &manager))
                                .unwrap();
                        }
                        event::KeyCode::Enter => {
                            let Some(s) = state.selection.selected() else {
                                continue;
                            };
                            match state.items.get(s) {
                                Some(SelectionItem::MediathekViewTui) => {
                                    let data_dir = base_dirs.data_dir().join("mediathekviewtui");
                                    if let Err(err) = std::fs::create_dir_all(&data_dir) {
                                        error!("{}", err.to_string());
                                        state.popup_text = Some(err.to_string());
                                        continue;
                                    };
                                    #[cfg(feature = "real")]
                                    let config_searches = mk_searches();
                                    #[cfg(not(feature = "real"))]
                                    let config_searches = vec![];
                                    let app_config = AppConfig {
                                        player: &String::from("mpv"),
                                        player_args: &None,
                                        enable_continue_watching: true,
                                        sdl_controller_button_bindings: &HashMap::from([
                                            (
                                                Button::DPadDown,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::Down,
                                                ),
                                            ),
                                            (
                                                Button::DPadUp,
                                                KeyAction::ParsableKeyAction(ParsableKeyAction::Up),
                                            ),
                                            (
                                                Button::DPadLeft,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::Left,
                                                ),
                                            ),
                                            (
                                                Button::DPadRight,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::Right,
                                                ),
                                            ),
                                            (
                                                Button::A,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::Open,
                                                ),
                                            ),
                                            (
                                                Button::B,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::Quit,
                                                ),
                                            ),
                                            (
                                                Button::Y,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::Download,
                                                ),
                                            ),
                                            (
                                                Button::X,
                                                KeyAction::SearchWithInitial(Box::new(|item| {
                                                    item.map_or("".to_string(), |i| {
                                                        let series_regex = RegexBuilder::new()
                                                            .literal('(')
                                                            .one_or_more(
                                                                RegexBuilder::new().digit(),
                                                            )
                                                            .literal('/')
                                                            .one_or_more(
                                                                RegexBuilder::new().digit(),
                                                            )
                                                            .literal(')')
                                                            .end_of_line()
                                                            .to_regex();
                                                        match series_regex {
                                                            Ok(regex) => format!(
                                                                "!{} {}",
                                                                i.channel,
                                                                regex.replace(&i.title, "")
                                                            ),
                                                            Err(_) => {
                                                                format!(
                                                                    "!{} {}",
                                                                    i.channel, i.title,
                                                                )
                                                            }
                                                        }
                                                    })
                                                })),
                                            ),
                                            (
                                                Button::RightShoulder,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::RemoveItem,
                                                ),
                                            ),
                                            (
                                                Button::LeftShoulder,
                                                KeyAction::ParsableKeyAction(
                                                    ParsableKeyAction::DeleteView,
                                                ),
                                            ),
                                        ]),
                                        keybindings: &HashMap::new(),
                                    };
                                    if let Err(err) = run_app(
                                        &mut terminal,
                                        data_dir,
                                        config_searches,
                                        app_config,
                                        |frame| render_header(frame, &manager),
                                    )
                                    .await
                                    {
                                        error!("{}", err.to_string());
                                        state.popup_text = Some(err.to_string());
                                    };
                                    terminal.clear().unwrap();
                                    terminal
                                        .draw(|frame| render(&mut state, frame, &manager))
                                        .unwrap();
                                }
                                Some(SelectionItem::JellyTui) => {
                                    let path = Path::new("/home/florian/.config/jellytui/");
                                    let config = match jellytui::config::Config::load(Some(path)) {
                                        Ok(config) => config,
                                        Err(err) => {
                                            error!("{}", err.to_string());
                                            state.popup_text = Some(err.to_string());
                                            continue;
                                        }
                                    };
                                    if let Err(err) = jellytui::run_app(
                                        Some(&mut terminal),
                                        Some(path),
                                        config,
                                        |frame: &mut Frame| render_header(frame, &manager),
                                    )
                                    .await
                                    {
                                        error!("{}", err.to_string());
                                        state.popup_text = Some(err.to_string());
                                    };
                                    terminal.clear().unwrap();
                                    terminal
                                        .draw(|frame| render(&mut state, frame, &manager))
                                        .unwrap();
                                }
                                Some(SelectionItem::JellyTuiT) => {
                                    let path = Path::new("/home/florian/.config/jellytui-t/");
                                    let config = match jellytui::config::Config::load(Some(path)) {
                                        Ok(config) => config,
                                        Err(err) => {
                                            error!("{}", err.to_string());
                                            state.popup_text = Some(err.to_string());
                                            continue;
                                        }
                                    };
                                    if let Err(err) = jellytui::run_app(
                                        Some(&mut terminal),
                                        Some(path),
                                        config,
                                        |frame: &mut Frame| render_header(frame, &manager),
                                    )
                                    .await
                                    {
                                        error!("{}", err.to_string());
                                        state.popup_text = Some(err.to_string());
                                    };
                                    terminal.clear().unwrap();
                                    terminal
                                        .draw(|frame| render(&mut state, frame, &manager))
                                        .unwrap();
                                }
                                Some(SelectionItem::Exit) => {
                                    break;
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
            }
            _ => {}
        }
    }
    ratatui::restore()
}

fn render(state: &mut State, frame: &mut Frame, manager: &Result<Manager, Error>) {
    let main_area = render_header(frame, manager);
    let [center_area] = Layout::vertical([Constraint::Length(SELECTION_ITEM_COUNT)])
        .flex(Flex::Center)
        .areas(main_area);
    frame.render_stateful_widget(
        List::new(ALL_SELECTION_ITEMS)
            .highlight_style(Style::new().red().add_modifier(Modifier::BOLD)),
        center_area,
        &mut state.selection,
    );

    let popup_width = 60.min(main_area.width - 4);
    let popup_height = 6.min(main_area.height - 4);

    let popup_area = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Length((main_area.width - popup_width) / 2),
            Constraint::Length(popup_width),
            Constraint::Min(0),
        ])
        .split(
            Layout::default()
                .direction(Direction::Vertical)
                .constraints([
                    Constraint::Length((main_area.height - popup_height) / 2),
                    Constraint::Length(popup_height),
                    Constraint::Min(0),
                ])
                .split(main_area)[1],
        );

    if let Some(popup_text) = &state.popup_text {
        let popup = Paragraph::new(popup_text.clone())
            .block(
                Block::default()
                    .title("Fehler")
                    .borders(Borders::ALL)
                    .border_style(Style::default().fg(Color::Red)),
            )
            .alignment(Alignment::Center)
            .wrap(ratatui::widgets::Wrap { trim: true });

        frame.render_widget(Clear, popup_area[1]);
        frame.render_widget(popup, popup_area[1]);
    }
}

fn render_header(frame: &mut Frame, manager: &Result<Manager, Error>) -> Rect {
    let vertical = Layout::vertical([Length(1), Min(0)]);
    let [header_area, main_area] = vertical.areas(frame.area());
    let battery_str = match manager {
        Ok(manager) => match manager.batteries() {
            Ok(batteries) => {
                let battery_strs = batteries.map(|b| match b {
                    Ok(mut battery) => match manager.refresh(&mut battery) {
                        Ok(_) => (battery.state_of_charge().value.abs() * 100.0).to_string(),
                        Err(e) => e.to_string(),
                    },
                    Err(e) => e.to_string(),
                });
                battery_strs.collect::<Vec<_>>().join("/")
            }
            Err(e) => e.to_string(),
        },
        Err(e) => e.to_string(),
    };
    frame.render_widget(
        Paragraph::new(format!("battery: {}%", battery_str)).right_aligned(),
        header_area,
    );
    let now = Local::now();
    frame.render_widget(
        Paragraph::new(format!("{}%", now.to_rfc2822())).left_aligned(),
        header_area,
    );
    main_area
}
