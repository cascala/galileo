// Galileo for VS Code: run .gg scripts and send lines to a Galileo prompt.
// Plain JavaScript with no dependencies, so there is no build step.
"use strict";

const vscode = require("vscode");
const fs = require("fs");
const path = require("path");

const PROMPT_NAME = "Galileo";
const RUN_NAME = "Galileo Run";

let prompt; // { terminal, cwd, docker }

function activate(context) {
  context.subscriptions.push(
    vscode.commands.registerCommand("galileo.runFile", runFile),
    vscode.commands.registerCommand("galileo.sendSelection", sendSelection),
    vscode.commands.registerCommand("galileo.loadFile", loadFile),
    vscode.commands.registerCommand("galileo.startPrompt", () => startPrompt(true)),
    vscode.window.onDidCloseTerminal((t) => {
      if (prompt && t === prompt.terminal) prompt = undefined;
    })
  );
}

function deactivate() {}

// ---- settings and finding galileo ------------------------------------------

function settings() {
  const c = vscode.workspace.getConfiguration("galileo");
  return {
    command: c.get("command", "galileo") || "galileo",
    useDocker: c.get("useDocker", "auto"),
    image: c.get("dockerImage", "cascala/galileo") || "cascala/galileo",
    explain: c.get("explain", "brief"),
  };
}

/** The full path of a program: as given when it has a folder, else looked up on PATH */
function which(cmd, env = process.env, platform = process.platform) {
  const exts = platform === "win32" ? (env.PATHEXT || ".EXE;.CMD;.BAT").split(";").concat([""]) : [""];
  const isFile = (p) => {
    try {
      return fs.statSync(p).isFile();
    } catch {
      return false;
    }
  };
  if (cmd.includes("/") || cmd.includes("\\")) {
    for (const e of exts) if (isFile(cmd + e)) return cmd + e;
    return undefined;
  }
  const dirs = (env.PATH || env.Path || "").split(path.delimiter).filter(Boolean);
  for (const d of dirs) for (const e of exts) {
    const p = path.join(d, cmd + e);
    if (isFile(p)) return p;
  }
  return undefined;
}

/** How to run Galileo: { docker: false, command } or { docker: true, image }, or undefined */
function runner(s = settings(), find = which) {
  if (s.useDocker !== "always") {
    const cmd = find(s.command);
    if (cmd) return { docker: false, command: cmd };
    if (s.useDocker === "never") return undefined;
  }
  if (find("docker")) return { docker: true, image: s.image };
  return undefined;
}

function explainEnv(s) {
  return s.explain === "off" || s.explain === "teach" ? s.explain : undefined;
}

async function noGalileo() {
  const pick = await vscode.window.showErrorMessage(
    "Galileo was not found. Install it (cargo install --git https://github.com/cascala/galileo), set galileo.command to its path, or install Docker to use the cascala/galileo image.",
    "Open Settings",
    "Get Galileo"
  );
  if (pick === "Open Settings") vscode.commands.executeCommand("workbench.action.openSettings", "galileo.");
  if (pick === "Get Galileo") vscode.env.openExternal(vscode.Uri.parse("https://github.com/cascala/galileo#try-it"));
}

// ---- folders and paths ------------------------------------------------------

/** The folder the prompt works in: the file's workspace folder, else the file's own folder */
function homeFolder(uri) {
  if (uri && uri.scheme === "file") {
    const ws = vscode.workspace.getWorkspaceFolder(uri);
    return ws ? ws.uri.fsPath : path.dirname(uri.fsPath);
  }
  const ws = vscode.workspace.workspaceFolders;
  return ws && ws.length ? ws[0].uri.fsPath : undefined;
}

/** A path as Galileo inside Docker sees it (the folder is mounted at /work), or undefined if outside it */
function dockerPath(file, folder) {
  const rel = path.relative(folder, file);
  if (rel.startsWith("..") || path.isAbsolute(rel)) return undefined;
  return rel.split(path.sep).join("/");
}

function quote(s) {
  return '"' + s.replace(/"/g, '\\"') + '"';
}

/** The shell line that runs a file (in its own folder, so its loads find their files) */
function runLine(r, file, s, platform = process.platform) {
  const dir = path.dirname(file);
  const name = path.basename(file);
  if (r.docker) {
    const e = explainEnv(s);
    return ["docker run --rm", "-v", quote(dir + ":/work"), e ? "-e GALILEO_EXPLAIN=" + e : "", r.image, quote(name)]
      .filter(Boolean)
      .join(" ");
  }
  let cmd = /\s/.test(r.command) ? quote(r.command) : r.command;
  if (platform === "win32" && cmd.startsWith('"')) cmd = "& " + cmd; // PowerShell
  return cmd + " " + quote(file);
}

// ---- commands ---------------------------------------------------------------

async function activeGalileoFile() {
  const ed = vscode.window.activeTextEditor;
  if (!ed) return undefined;
  const doc = ed.document;
  if (doc.isUntitled || doc.uri.scheme !== "file") {
    vscode.window.showWarningMessage("Save the file first, so Galileo can read it.");
    return undefined;
  }
  if (doc.isDirty) await doc.save();
  return doc.uri;
}

async function runFile(uri) {
  if (!(uri instanceof vscode.Uri)) uri = await activeGalileoFile();
  else {
    const open = vscode.workspace.textDocuments.find((d) => d.uri.toString() === uri.toString());
    if (open && open.isDirty) await open.save();
  }
  if (!uri) return;
  const s = settings();
  const r = runner(s);
  if (!r) return noGalileo();
  let t = vscode.window.terminals.find((x) => x.name === RUN_NAME && x.exitStatus === undefined);
  if (!t) {
    const env = {};
    const e = explainEnv(s);
    if (e && !r.docker) env.GALILEO_EXPLAIN = e;
    t = vscode.window.createTerminal({ name: RUN_NAME, cwd: path.dirname(uri.fsPath), env });
  }
  t.show(true);
  t.sendText(runLine(r, uri.fsPath, s));
}

/** The Galileo prompt terminal, started if needed; resolves once it can take input */
async function startPrompt(show, uri) {
  if (prompt && prompt.terminal.exitStatus === undefined) {
    if (show) prompt.terminal.show(true);
    return prompt;
  }
  const s = settings();
  const r = runner(s);
  if (!r) {
    noGalileo();
    return undefined;
  }
  const cwd = homeFolder(uri || (vscode.window.activeTextEditor && vscode.window.activeTextEditor.document.uri));
  const e = explainEnv(s);
  const opts = { name: PROMPT_NAME, cwd };
  if (r.docker) {
    opts.shellPath = which("docker") || "docker";
    opts.shellArgs = ["run", "-it", "--rm"]
      .concat(cwd ? ["-v", cwd + ":/work"] : [])
      .concat(e ? ["-e", "GALILEO_EXPLAIN=" + e] : [])
      .concat([r.image]);
  } else {
    opts.shellPath = r.command;
    opts.shellArgs = [];
    if (e) opts.env = { GALILEO_EXPLAIN: e };
  }
  const terminal = vscode.window.createTerminal(opts);
  terminal.show(true);
  prompt = { terminal, cwd, docker: r.docker };
  // give the program a moment to start before typing into it
  await new Promise((res) => setTimeout(res, r.docker ? 2500 : 600));
  return prompt;
}

/** Type text into the prompt, one line at a time */
function typeInto(terminal, text) {
  const lines = text.replace(/\r\n?/g, "\n").replace(/\n+$/, "").split("\n");
  for (const l of lines) terminal.sendText(l, true);
}

async function sendSelection() {
  const ed = vscode.window.activeTextEditor;
  if (!ed) return;
  const doc = ed.document;
  const lines = [];
  for (let k = 0; k < doc.lineCount; k++) lines.push(doc.lineAt(k).text);
  let text;
  let next;
  if (ed.selection.isEmpty) {
    const [a, b] = statementAt(lines, ed.selection.active.line);
    text = lines.slice(a, b + 1).join("\n");
    next = nextStatementLine(lines, b + 1);
  } else {
    text = doc.getText(ed.selection);
  }
  if (!text.trim()) {
    if (next !== undefined) moveTo(ed, next);
    return;
  }
  const p = await startPrompt(false, doc.uri);
  if (!p) return;
  p.terminal.show(true);
  typeInto(p.terminal, text);
  if (next !== undefined) moveTo(ed, next);
}

function moveTo(ed, line) {
  const pos = new vscode.Position(line, 0);
  ed.selection = new vscode.Selection(pos, pos);
  ed.revealRange(new vscode.Range(pos, pos));
}

async function loadFile() {
  const uri = await activeGalileoFile();
  if (!uri) return;
  const p = await startPrompt(false, uri);
  if (!p) return;
  let name = uri.fsPath;
  if (p.docker) {
    name = p.cwd && dockerPath(uri.fsPath, p.cwd);
    if (!name) {
      vscode.window.showWarningMessage(
        "This file is outside the folder the Galileo prompt in Docker can see. Close the Galileo terminal and load it again."
      );
      return;
    }
  }
  p.terminal.show(true);
  typeInto(p.terminal, "load(" + quote(name) + ")");
}

// ---- statements that span lines ---------------------------------------------

const OPENERS = new Set(["for", "while", "if", "function"]);

/**
 * (open brackets, ends in a continuation \, open for/while/if/function blocks),
 * mirroring how the Galileo prompt decides to keep reading
 */
function scan(src) {
  let depth = 0;
  let blocks = 0;
  let cont = false;
  let i = 0;
  while (i < src.length) {
    const ch = src[i];
    if (ch === '"') {
      i++;
      while (i < src.length && src[i] !== '"' && src[i] !== "\n") i++;
      i++;
      cont = false;
      continue;
    }
    if (ch === "/" && src[i + 1] === "/") {
      while (i < src.length && src[i] !== "\n") i++;
      continue;
    }
    if (!/\s/.test(ch)) cont = false;
    if (ch === "(" || ch === "[" || ch === "{") depth++;
    else if (ch === ")" || ch === "]" || ch === "}") depth--;
    else if (ch === "\\") cont = true;
    else if (/[A-Za-z_]/.test(ch)) {
      let j = i;
      while (j < src.length && /[A-Za-z0-9_]/.test(src[j])) j++;
      const w = src.slice(i, j);
      const prev = i > 0 ? src[i - 1] : "";
      if (depth === 0 && prev !== ".") {
        if (OPENERS.has(w)) {
          // if(c, a, b) with commas is the function, not a block
          const rest = src.slice(j).match(/^\s*\(([^()]*)\)/);
          if (!(w === "if" && rest && rest[1].includes(","))) blocks++;
        } else if (w === "end") blocks--;
      }
      i = j;
      continue;
    }
    i++;
  }
  return { depth, cont, blocks };
}

function needsMore(src) {
  const s = scan(src);
  return s.depth > 0 || s.cont || s.blocks > 0;
}

/** The lines [a, b] of the statement starting on line k (it runs on while it is unfinished) */
function statementAt(lines, k) {
  let b = k;
  let src = lines[k] + "\n";
  while (needsMore(src) && b + 1 < lines.length) {
    b++;
    src += lines[b] + "\n";
  }
  return [k, b];
}

/** The next line at or after k with something on it besides a comment */
function nextStatementLine(lines, k) {
  for (let n = k; n < lines.length; n++) {
    const t = lines[n].trim();
    if (t && !t.startsWith("//")) return n;
  }
  return lines.length > 0 ? Math.min(k, lines.length - 1) : undefined;
}

module.exports = {
  activate,
  deactivate,
  // for the tests
  _internal: { which, runner, runLine, dockerPath, needsMore, statementAt, nextStatementLine, scan },
};
