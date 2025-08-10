#[cfg(feature = "real")]
mod searches;

use battery::{Error, Manager};
use chrono::Local;
use crokey::key;
use crossterm::event::{self, Event};
use crossterm::terminal::{disable_raw_mode, enable_raw_mode, ClearType};
use crossterm::ExecutableCommand;
use mediathekviewtui::types::{Binding, KeyAction};
use mediathekviewtui::{run_app, types::Config};
use ratatui::layout::Rect;
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
use std::process::Command;
use strum::IntoEnumIterator;
use strum_macros::EnumIter;

#[derive(Debug, Clone)]
struct State {
    selection: ListState,
    items: Vec<SelectionItem>,
}

#[derive(Debug, Clone, EnumIter)]
enum SelectionItem {
    MediathekViewTui,
    JellyTui,
    JellyTuiT,
    Exit,
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
        items: SelectionItem::iter().collect(),
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
                            run_app(&mut terminal, &config).await;
                            terminal.clear().unwrap();
                            terminal
                                .draw(|frame| render(&mut state, frame, &manager))
                                .unwrap();
                        }
                        Some(SelectionItem::JellyTui) => {
                            run_external_tui("jellytui", vec![]);
                            terminal
                                .draw(|frame| render(&mut state, frame, &manager))
                                .unwrap();
                        }
                        Some(SelectionItem::JellyTuiT) => {
                            run_external_tui(
                                "jellytui",
                                vec!["-b", "/home/florian/.config/jellytui-t/"],
                            );
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
    let items = state
        .items
        .iter()
        .map(|x| <SelectionItem as Into<ListItem>>::into(x.clone()))
        .collect::<Vec<_>>();
    let list_height: usize = items.iter().map(|x| x.height()).sum();
    let area = Rect::new(
        frame.area().x,
        (frame.area().height - list_height as u16) / 2,
        frame.area().width,
        frame.area().height,
    );
    frame.render_stateful_widget(
        List::new(items).highlight_style(Style::new().red().add_modifier(Modifier::BOLD)),
        area,
        &mut state.selection,
    );
    let battery_str = match manager {
        Ok(manager) => match manager.batteries() {
            Ok(batteries) => {
                let battery_strs = batteries.map(|b| match b {
                    Ok(mut battery) => match manager.refresh(&mut battery) {
                        Ok(_) => battery.state_of_charge().value.to_string(),
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
    // TODO bottom of screen
    frame.render_widget(
        Paragraph::new(format!("battery: {}%", battery_str)).right_aligned(),
        frame.area(),
    );
    let now = Local::now();
    frame.render_widget(
        Paragraph::new(format!("{}%", now.to_rfc2822())).left_aligned(),
        frame.area(),
    )
}

fn run_external_tui(prog: &str, args: Vec<&str>) {
    disable_raw_mode().unwrap();
    // Clears entire screen and moves cursor to (0,0)
    stdout()
        .execute(crossterm::terminal::Clear(ClearType::All))
        .unwrap();
    Command::new(prog).args(args).status().unwrap();
    stdout()
        .execute(crossterm::terminal::Clear(ClearType::All))
        .unwrap();
    enable_raw_mode().unwrap()
}
