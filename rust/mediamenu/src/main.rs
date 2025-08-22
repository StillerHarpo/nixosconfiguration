#[cfg(feature = "real")]
mod searches;

use battery::{Error, Manager};
use chrono::Local;
use crokey::key;
use crossterm::event::{self, Event};
use crossterm::terminal::ClearType;
use crossterm::ExecutableCommand;
use mediathekviewtui::types::{Binding, KeyAction};
use mediathekviewtui::{run_app, types::Config};
use ratatui::layout::Constraint::{Length, Min};
use ratatui::layout::{Constraint, Flex, Layout, Rect};
use ratatui::text::Text;
use ratatui::widgets::Paragraph;
use ratatui::{
    style::{Modifier, Style, Stylize},
    widgets::{List, ListItem, ListState},
    Frame,
};
#[cfg(feature = "real")]
use searches::mk_searches;
use std::collections::HashMap;
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
}

fn mk_text(content: &str) -> Text {
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

#[tokio::main]
async fn main() {
    let config = Config {
        player: "mpv",
        player_args: None,
        #[cfg(feature = "real")]
        searches: mk_searches(),
        #[cfg(not(feature = "real"))]
        searches: vec![],
        keybindings: HashMap::from([
            (Binding::KeyboardKey(key!(down)), KeyAction::Down),
            (Binding::KeyboardKey(key!(up)), KeyAction::Up),
            (Binding::KeyboardKey(key!(left)), KeyAction::Left),
            (Binding::KeyboardKey(key!(right)), KeyAction::Right),
            (Binding::KeyboardKey(key!(esc)), KeyAction::Quit),
            (Binding::KeyboardKey(key!(enter)), KeyAction::Open),
        ]),
    };
    let mut terminal = ratatui::init();
    color_eyre::install().unwrap();

    let mut state = State {
        selection: ListState::default(),
        items: ALL_SELECTION_ITEMS,
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
            Ok(Event::Key(key_event)) if key_event.is_press() => match key_event.code {
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
                            run_app(&mut terminal, &config, |frame| {
                                render_header(frame, &manager)
                            })
                            .await;
                            terminal.clear().unwrap();
                            terminal
                                .draw(|frame| render(&mut state, frame, &manager))
                                .unwrap();
                        }
                        Some(SelectionItem::JellyTui) => {
                            let path = Path::new("/home/florian/.config/jellytui/");
                            let config = jellytui::config::Config::load(Some(path)).unwrap();
                            jellytui::run_app(
                                Some(&mut terminal),
                                Some(path),
                                config,
                                |frame: &mut Frame| render_header(frame, &manager),
                            )
                            .await
                            .unwrap();
                            terminal.clear().unwrap();
                            terminal
                                .draw(|frame| render(&mut state, frame, &manager))
                                .unwrap();
                        }
                        Some(SelectionItem::JellyTuiT) => {
                            let path = Path::new("/home/florian/.config/jellytui-t/");
                            let config = jellytui::config::Config::load(Some(path)).unwrap();
                            jellytui::run_app(
                                Some(&mut terminal),
                                Some(path),
                                config,
                                |frame: &mut Frame| render_header(frame, &manager),
                            )
                            .await
                            .unwrap();
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
            },
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
