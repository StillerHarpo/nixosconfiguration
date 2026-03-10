use core::fmt;
use serde_json::Value;
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufReader, Write};
use std::process::{Command, Stdio};
use swayipc::Connection;
use swayipc::Node;

const BROWSE: &str = "librewolf --new-window ";
const WORK_BROWSE: &str = "librewolf -P work --new-window ";
const PRIVATE_BROWSE: &str = "sudo -En librewolf-airvpn ";

pub const PROGRAMS: [(&str, &str); 16] = [
    ("librewolf", "librewolf"),
    (
        "grim (screenshot to file)",
        "GRIM_DEFAULT_DIR=~/screenshots/ grim -g \"$(slurp)\"",
    ),
    (
        "grim (screenshot to clipboard)",
        "grim -g \"$(slurp)\" - | wl-copy",
    ),
    (
        "grim (screenshot to text)",
        "grim -g \"$(slurp)\" - | tesseract - stdout | wl-copy",
    ),
    ("chromium", "chromium"),
    ("signal", "signal-desktop"),
    ("element", "element-desktop"),
    ("steam", "steam"),
    (
        "alacritty (Privileged)",
        "alacritty -T \"Alacritty (Privileged)\"",
    ),
    ("pavucontrol", "pavucontrol"),
    ("wdisplays", "wdisplays"),
    ("blueman-manager", "blueman-manager"),
    ("remmina", "remmina"),
    ("slack", "slack"),
    // Hotfix
    (
        "passmenu",
        "PASSWORD_STORE_DIR=\"${PASSWORD_STORE_DIR}\" wofi-pass",
    ),
    ("firefox-airvpn", "sudo -En firefox-airvpn"),
];

fn parse_key_value_map<'a>(what: &'a str, v: Value) -> Result<Vec<(String, String)>, String> {
    match v {
        Value::Object(map) => map
            .into_iter()
            .map(|pair| match pair.1 {
                Value::String(s) => Ok((pair.0, s)),
                _ => Err("not a string".to_string()),
            })
            .collect(),
        _ => Err(format!("object {} is not a map", what)),
    }
}

fn parse_bookmarks() -> Result<Vec<(String, String)>, String> {
    let file = File::open("/home/florian/.my-bookmarks.json").map_err(|err| err.to_string())?;
    let reader = BufReader::new(file);
    // Read the JSON contents of the file
    let v = serde_json::from_reader(reader).map_err(|err| err.to_string())?;
    match v {
        Value::Object(map) => {
            let nested = map
                .into_iter()
                .map(|kv| {
                    if kv.0 == "normal" {
                        let key_values = parse_key_value_map("normal", kv.1)?;
                        Ok(key_values
                            .into_iter()
                            .map(|v| (v.0, format!("{} {}", BROWSE, v.1)))
                            .collect::<Vec<_>>())
                    } else if kv.0 == "work" {
                        let key_values = parse_key_value_map("work", kv.1)?;
                        Ok(key_values
                            .into_iter()
                            .map(|v| (v.0, format!("{} {}", WORK_BROWSE, v.1)))
                            .collect::<Vec<_>>())
                    } else if kv.0 == "private" {
                        let key_values = parse_key_value_map("private", kv.1)?;
                        Ok(key_values
                            .into_iter()
                            .map(|v| (v.0, format!("{} {}", PRIVATE_BROWSE, v.1)))
                            .collect::<Vec<_>>())
                    } else {
                        Err(format!("Unknown key: {}", kv.0))
                    }
                })
                .collect::<Result<Vec<_>, _>>()?;
            Ok(nested.into_iter().flatten().collect())
        }
        _ => return Err("my-bookmarks.json doesn't contain a map".to_string()),
    }
}

/// move input into wofi and gives back result
fn exec_wofi<A: ToString>(s: A) -> Result<std::process::Output, std::io::Error> {
    let mut child = Command::new("wofi")
        .arg("--dmenu")
        .arg("--insensitive")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let child_stdin = child.stdin.as_mut().unwrap();
    child_stdin.write_all(s.to_string().as_bytes()).unwrap();
    child_stdin.flush().unwrap();
    child.wait_with_output()
}

/// prints error, if it exists on wofi and exits
fn error_on_wofi<A, B: ToString>(v: Result<A, B>) -> A {
    match v {
        Err(s) => {
            exec_wofi(s).unwrap();
            std::process::exit(1)
        }
        Ok(res) => res,
    }
}

fn main() {
    let mut conn = error_on_wofi(Connection::new());
    let tree = error_on_wofi(conn.get_tree());
    let windows: HashMap<String, WindowId> = get_windows_of_workspace(tree)
        .iter()
        .map(|wow| (format!("{wow}"), wow.window.id))
        .collect();
    let programs: Vec<(&str, &str)> = Vec::from(PROGRAMS);
    // TODO print error on wofi
    let bookmarks: Vec<(String, String)> = error_on_wofi(parse_bookmarks());
    let windows_and_cmds: Vec<(String, OpenOrRaise)> = windows
        .into_iter()
        .map(|nid| (nid.0, OpenOrRaise::WindowId(nid.1)))
        .chain(
            programs
                .into_iter()
                .map(|prgs| (prgs.0.to_string(), OpenOrRaise::Cmd(prgs.1.to_string()))),
        )
        .chain(
            bookmarks
                .into_iter()
                .map(|bkms| (bkms.0, OpenOrRaise::Cmd(bkms.1))),
        )
        .collect();
    let input: Vec<String> = windows_and_cmds.clone().into_iter().map(|x| x.0).collect();
    let output = exec_wofi(input.join("\n")).unwrap();
    let output_string = std::str::from_utf8(&output.stdout).unwrap();
    match &windows_and_cmds
        .into_iter()
        .find(|kvs| kvs.0 == &output_string[..output_string.len() - 1])
    {
        None => {
            if output_string.len() > 0 {
                let search_string = str::replace(output_string, " ", "+");
                conn.run_command(format!(
                    "exec sudo -En librewolf-airvpn 'duckduckgo.com\\?q={search_string}'"
                ))
                .unwrap();
            }
        }
        Some(oor) => run_open_or_raise(&oor.1, &mut conn),
    }
}

/// Give back all the windows of an workspace with the name of that workspace
fn get_windows_of_workspace(node: Node) -> Vec<WindowOnWorkspace> {
    match node.name {
        None => vec![],
        Some(name) => node
            .nodes
            .into_iter()
            .flat_map(|node| add_workspace_names(node, name.clone()))
            .collect(),
    }
}

/// Add workspace name to windows
fn add_workspace_names(node: Node, name: String) -> Vec<WindowOnWorkspace> {
    get_windows(node)
        .into_iter()
        .map(|window| WindowOnWorkspace {
            name: name.clone(),
            window,
        })
        .collect()
}

/// Get all the windows from a tree
fn get_windows(node: Node) -> Vec<Window> {
    if node.nodes.len() == 0 && node.floating_nodes.len() == 0 {
        match node.name {
            None => return vec![],
            Some(name) => {
                return vec![Window {
                    name,
                    app_id: node.app_id,
                    id: WindowId(node.id),
                }]
            }
        }
    }
    let windows: Vec<Window> = node
        .nodes
        .into_iter()
        .flat_map(|n| get_windows(n))
        .chain(node.floating_nodes.into_iter().flat_map(|n| get_windows(n)))
        .collect();
    windows
}

fn run_open_or_raise(open_or_raise: &OpenOrRaise, connection: &mut Connection) {
    match open_or_raise {
        OpenOrRaise::Cmd(cmd) => {
            let payload = &format!("exec {cmd}");
            match connection.run_command(payload) {
                Ok(_) => (),
                Err(msg) => panic!("Running {payload} failed: {msg}"),
            };
        }
        OpenOrRaise::WindowId(id) => {
            let payload = &format!("[con_id={}] focus", id.0);
            match connection.run_command(payload) {
                Ok(_) => (),
                Err(msg) => panic!("Running {payload} failed: {msg}"),
            };
        }
    }
}

#[derive(Debug, Copy, Clone)]
struct WindowId(i64);

impl fmt::Display for WindowId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({})", self.0)
    }
}

/// an window in sway
struct Window {
    /// human readable name of the window
    name: String,
    app_id: Option<String>,
    id: WindowId,
}

impl fmt::Display for Window {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.app_id {
            None => write!(f, "{} {}", self.name, self.id),
            Some(app_id) => write!(f, "{} - {} {}", app_id, self.name, self.id),
        }
    }
}

/// an Window together with the workspace it appears at
struct WindowOnWorkspace {
    /// human readable name of the workspace
    name: String,
    window: Window,
}

impl fmt::Display for WindowOnWorkspace {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "[{}] {}", self.name, self.window)
    }
}

/// Either raise or open a new window
#[derive(Clone, Debug)]
enum OpenOrRaise {
    /// the command to open the window
    Cmd(String),
    /// the window id of the window to raisk
    WindowId(WindowId),
}
