// Tests for the parts of the extension that do not need VS Code itself
const assert = require("assert");
const fs = require("fs");
const os = require("os");
const path = require("path");
const vscode = require("./vscode-mock");
const ext = require("../extension");
const { which, runner, runLine, dockerPath, needsMore, statementAt, nextStatementLine } = ext._internal;

let failed = 0;
async function test(name, f) {
  try {
    await f();
    console.log("ok  ", name);
  } catch (e) {
    failed++;
    console.log("FAIL", name, "\n", e);
  }
}

(async () => {
  await test("statements that continue", () => {
    assert(needsMore("plot(sin(x),"));
    assert(needsMore("A = [1 2"));
    assert(needsMore("k = 1 + \\"));
    assert(needsMore("k = 1 + \\   // more"));
    assert(needsMore("for k = 1:3"));
    assert(needsMore("function y = sq(x)"));
    assert(needsMore("if x > 1\n  y = 2\nelse"));
    assert(!needsMore("for k = 1:3 k^2 end"));
    assert(!needsMore("if(x > 1, 2, 3)"));
    assert(!needsMore("A \\ b"));
    assert(!needsMore('s = "( \\"'));
    assert(!needsMore("x = 1 // (unclosed in a comment"));
    assert(!needsMore("p.end + 1"));
  });

  await test("the statement under the cursor", () => {
    const lines = ["a = 1", "plot(sin(x),", "  x, 0, 1)", "", "// note", "for k = 1:2", "  k", "end", "b"];
    assert.deepStrictEqual(statementAt(lines, 0), [0, 0]);
    assert.deepStrictEqual(statementAt(lines, 1), [1, 2]);
    assert.deepStrictEqual(statementAt(lines, 5), [5, 7]);
    assert.strictEqual(nextStatementLine(lines, 3), 5);
    assert.strictEqual(nextStatementLine(lines, 9), 8);
    // an unfinished statement at the end stops at the last line
    assert.deepStrictEqual(statementAt(["f(1,", "2"], 0), [0, 1]);
  });

  await test("finding programs", () => {
    const d = fs.mkdtempSync(path.join(os.tmpdir(), "gg-"));
    const p = path.join(d, "galileo");
    fs.writeFileSync(p, "");
    assert.strictEqual(which("galileo", { PATH: d }, "linux"), p);
    assert.strictEqual(which("nothere", { PATH: d }, "linux"), undefined);
    assert.strictEqual(which(p, {}, "linux"), p);
  });

  await test("native or Docker", () => {
    const has = (...names) => (c) => (names.includes(c) ? "/bin/" + c : undefined);
    const s = { command: "galileo", useDocker: "auto", image: "cascala/galileo" };
    assert.deepStrictEqual(runner(s, has("galileo", "docker")), { docker: false, command: "/bin/galileo" });
    assert.deepStrictEqual(runner(s, has("docker")), { docker: true, image: "cascala/galileo" });
    assert.strictEqual(runner(s, has()), undefined);
    assert.strictEqual(runner({ ...s, useDocker: "never" }, has("docker")), undefined);
    assert.deepStrictEqual(runner({ ...s, useDocker: "always" }, has("galileo", "docker")), { docker: true, image: "cascala/galileo" });
  });

  await test("the line that runs a file", () => {
    const s = { explain: "teach" };
    assert.strictEqual(runLine({ docker: false, command: "/usr/bin/galileo" }, "/w/my orbit.gg", s, "linux"), '/usr/bin/galileo "/w/my orbit.gg"');
    assert.strictEqual(
      runLine({ docker: false, command: "C:\\Program Files\\galileo.exe" }, "C:\\w\\a.gg", s, "win32"),
      '& "C:\\Program Files\\galileo.exe" "C:\\w\\a.gg"'
    );
    assert.strictEqual(
      runLine({ docker: true, image: "cascala/galileo" }, "/w/sub/a.gg", s, "linux"),
      'docker run --rm -v "/w/sub:/work" -e GALILEO_EXPLAIN=teach cascala/galileo "a.gg"'
    );
    assert.strictEqual(dockerPath("/w/sub/a.gg", "/w"), "sub/a.gg");
    assert.strictEqual(dockerPath("/elsewhere/a.gg", "/w"), undefined);
  });

  await test("activation registers the commands", () => {
    ext.activate({ subscriptions: [] });
    for (const c of ["galileo.runFile", "galileo.sendSelection", "galileo.loadFile", "galileo.startPrompt"]) {
      assert(vscode.commands.registered[c], c);
    }
    const pkg = require("../package.json");
    const declared = pkg.contributes.commands.map((c) => c.command).sort();
    assert.deepStrictEqual(declared, Object.keys(vscode.commands.registered).sort());
  });

  await test("Shift+Enter sends the statement and moves on", async () => {
    const d = fs.mkdtempSync(path.join(os.tmpdir(), "gg-"));
    fs.writeFileSync(path.join(d, "galileo"), "");
    process.env.PATH = d + path.delimiter + process.env.PATH;
    const lines = ["a = 1", "plot(sin(x),", "  x, 0, 1)", "", "b = 2"];
    const ed = {
      document: {
        uri: { scheme: "file", fsPath: path.join(d, "t.gg") },
        lineCount: lines.length,
        lineAt: (k) => ({ text: lines[k] }),
        getText: () => "",
      },
      selection: new vscode.Selection(new vscode.Position(1, 3), new vscode.Position(1, 3)),
      revealRange() {},
    };
    vscode.window.activeTextEditor = ed;
    await vscode.commands.registered["galileo.sendSelection"]();
    const t = vscode.window.terminals.find((x) => x.name === "Galileo");
    assert(t, "a Galileo terminal was opened");
    assert.strictEqual(t.opts.shellPath, path.join(d, "galileo"));
    assert.strictEqual(t.opts.cwd, d);
    assert.deepStrictEqual(t.sent, ["plot(sin(x),", "  x, 0, 1)"]);
    assert.strictEqual(ed.selection.active.line, 4);
    // the same terminal is used again
    await vscode.commands.registered["galileo.sendSelection"]();
    assert.deepStrictEqual(t.sent.slice(2), ["b = 2"]);
    assert.strictEqual(vscode.window.terminals.filter((x) => x.name === "Galileo").length, 1);
  });

  if (failed) {
    console.log(failed + " failed");
    process.exit(1);
  }
})();
