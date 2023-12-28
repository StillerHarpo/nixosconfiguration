use core::fmt;
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};
use swayipc::Connection;
use swayipc::Node;

mod internal;
use internal::*;

fn main() {
    let mut conn = Connection::new().unwrap();
    let tree = conn.get_tree().unwrap();
    let windows: HashMap<String, WindowId> = get_windows_of_workspace(tree)
        .iter()
        .map(|wow| (format!("{wow}"), wow.window.id))
        .collect();
    let bookmarks: HashMap<&str, &str> = HashMap::from(PROGRAMS);
    let windows_and_cmds: HashMap<String, OpenOrRaise> = windows
        .into_iter()
        .map(|nid| (nid.0, OpenOrRaise::WindowId(nid.1)))
        .chain(
            bookmarks
                .into_iter()
                .map(|bkms| (String::from(bkms.0), OpenOrRaise::Cmd(bkms.1))),
        )
        .collect();
    let input: Vec<String> = windows_and_cmds.keys().cloned().collect();
    let mut child = Command::new("wofi")
        .arg("--dmenu")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .unwrap();
    let child_stdin = child.stdin.as_mut().unwrap();
    child_stdin.write_all(input.join("\n").as_bytes()).unwrap();
    child_stdin.flush().unwrap();
    let output = child.wait_with_output().unwrap();
    let output_string = std::str::from_utf8(&output.stdout).unwrap();
    match windows_and_cmds.get(&output_string[..output_string.len() - 1]) {
        None => {
            if output_string.len() > 0 {
                conn.run_command(format!(
                    "exec firefox -new-window duckduckgo.com\\?q={output_string}"
                ))
                .unwrap();
            }
        }
        Some(oor) => run_open_or_raise(oor, &mut conn),
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
    id: WindowId,
}

impl fmt::Display for Window {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.name, self.id)
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
    Cmd(&'static str),
    /// the window id of the window to raisk
    WindowId(WindowId),
}
