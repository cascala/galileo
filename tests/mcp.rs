//! `galileo mcp`: the Model Context Protocol server, driven as a client would drive it.

use galileo_rs::mcp::Json;
use std::io::{BufRead, BufReader, Write};
use std::process::{Child, ChildStdin, ChildStdout, Command, Stdio};

struct Client {
    child: Child,
    stdin: ChildStdin,
    stdout: BufReader<ChildStdout>,
    id: u32,
}

impl Client {
    fn start(args: &[&str]) -> Client {
        let mut child = Command::new(env!("CARGO_BIN_EXE_galileo"))
            .arg("mcp")
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .unwrap();
        let stdin = child.stdin.take().unwrap();
        let stdout = BufReader::new(child.stdout.take().unwrap());
        let mut c = Client {
            child,
            stdin,
            stdout,
            id: 0,
        };
        let r = c.request("initialize", r#"{"protocolVersion":"2025-06-18","capabilities":{},"clientInfo":{"name":"test","version":"1"}}"#);
        assert_eq!(
            r.get("result")
                .unwrap()
                .get("protocolVersion")
                .unwrap()
                .as_str(),
            Some("2025-06-18")
        );
        writeln!(
            c.stdin,
            r#"{{"jsonrpc":"2.0","method":"notifications/initialized"}}"#
        )
        .unwrap();
        c
    }

    fn request(&mut self, method: &str, params: &str) -> Json {
        self.id += 1;
        writeln!(
            self.stdin,
            r#"{{"jsonrpc":"2.0","id":{},"method":"{}","params":{}}}"#,
            self.id, method, params
        )
        .unwrap();
        self.stdin.flush().unwrap();
        let mut line = String::new();
        self.stdout.read_line(&mut line).unwrap();
        Json::parse(&line).unwrap()
    }

    /// (text of the first content block, structuredContent, isError, number of images)
    fn call(&mut self, tool: &str, args: &str) -> (String, Json, bool, usize) {
        let r = self.request(
            "tools/call",
            &format!(r#"{{"name":"{}","arguments":{}}}"#, tool, args),
        );
        let r = r.get("result").unwrap().clone();
        let Some(Json::Arr(content)) = r.get("content") else {
            panic!("no content")
        };
        let text = content[0]
            .get("text")
            .unwrap()
            .as_str()
            .unwrap()
            .to_string();
        let images = content
            .iter()
            .filter(|c| c.get("type").and_then(Json::as_str) == Some("image"))
            .count();
        let st = r.get("structuredContent").cloned().unwrap_or(Json::Null);
        (
            text,
            st,
            r.get("isError").unwrap().as_bool().unwrap(),
            images,
        )
    }

    fn exec(&mut self, code: &str) -> (String, Json, bool, usize) {
        self.call(
            "execute",
            &Json::Obj(vec![("code".into(), Json::Str(code.into()))]).dump(),
        )
    }
}

impl Drop for Client {
    fn drop(&mut self) {
        let _ = self.child.kill();
        let _ = self.child.wait();
    }
}

#[test]
fn tools_and_resources() {
    let mut c = Client::start(&[]);
    let r = c.request("tools/list", "{}");
    let Some(Json::Arr(tools)) = r.get("result").unwrap().get("tools") else {
        panic!()
    };
    let names: Vec<&str> = tools
        .iter()
        .map(|t| t.get("name").unwrap().as_str().unwrap())
        .collect();
    assert_eq!(names, ["execute", "inspect", "complete", "reset"]);
    let r = c.request("resources/read", r#"{"uri":"galileo://docs/language"}"#);
    assert!(r.dump().contains("Galileo"));
    let r = c.request("nonsense", "{}");
    assert!(r.get("error").is_some());
}

#[test]
fn execute_keeps_definitions() {
    let mut c = Client::start(&[]);
    let (t, st, err, _) = c.exec("x = 3; x + 7");
    assert_eq!((t.as_str(), err), ("10", false));
    assert_eq!(st.get("status").unwrap().as_str(), Some("ok"));
    let (t, _, _, _) = c.exec("explain = \"off\"; f(t) := t^2;\nf(x)");
    assert_eq!(t, "9");
    let (t, st, err, _) = c.exec("1/(");
    assert!(err && t.starts_with("Syntax error"), "{}", t);
    assert_eq!(
        st.get("error").unwrap().get("name").unwrap().as_str(),
        Some("SyntaxError")
    );
    let (t, _, _, images) = c.exec("plot(sin(x), x, 0, 3)");
    assert_eq!((t.as_str(), images), ("(plot: image 1)", 1));
    let (t, _, _, _) = c.call("inspect", r#"{"name":"solve"}"#);
    assert!(t.contains("solve(f, x)"));
    let (t, _, _, _) = c.call("complete", r#"{"prefix":"eigv"}"#);
    assert_eq!(t, "eigvec");
    c.call("reset", "{}");
    let (t, _, _, _) = c.exec("explain = \"off\"; x");
    assert_eq!(t, "x");
}

#[test]
fn time_limit_keeps_the_session() {
    let mut c = Client::start(&["--timeout", "2"]);
    c.exec("y = 5;");
    let (t, st, err, _) = c.exec("k = 0\nwhile k >= 0\n  k = k + 1\nend");
    assert!(err, "{}", t);
    assert_eq!(st.get("status").unwrap().as_str(), Some("timeout"));
    let (t, _, _, _) = c.exec("y + 1");
    assert_eq!(t, "6");
}

#[test]
fn files_stay_in_memory_or_in_the_folder() {
    let mut c = Client::start(&[]);
    let (t, _, err, _) = c.exec("read(\"/etc/hostname\")");
    assert!(err, "{}", t);

    let dir = std::env::temp_dir().join(format!("galileo-mcp-test-{}", std::process::id()));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    std::fs::write(dir.join("data.csv"), "1,2\n3,4\n").unwrap();
    let mut c = Client::start(&["--dir", dir.to_str().unwrap()]);
    let (t, _, _, _) = c.exec("explain = \"off\"; det(read(\"data.csv\"))");
    assert_eq!(t, "-2");
    c.exec("save(\"hello\", \"../../out.txt\")");
    // ".." stops at the folder
    assert_eq!(
        std::fs::read_to_string(dir.join("out.txt")).unwrap().trim(),
        "hello"
    );
    let _ = std::fs::remove_dir_all(&dir);
}
