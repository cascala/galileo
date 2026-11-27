// Tokenizes sample Galileo with the extension's grammar, the way VS Code does
const assert = require("assert");
const fs = require("fs");
const path = require("path");
const oniguruma = require("vscode-oniguruma");
const vsctm = require("vscode-textmate");

const wasm = fs.readFileSync(path.join(path.dirname(require.resolve("vscode-oniguruma")), "onig.wasm")).buffer;

/** [text, scope] for each token of each line; scope is the innermost one */
async function tokens(src) {
  await oniguruma.loadWASM(wasm);
  const registry = new vsctm.Registry({
    onigLib: Promise.resolve({
      createOnigScanner: (p) => new oniguruma.OnigScanner(p),
      createOnigString: (s) => new oniguruma.OnigString(s),
    }),
    loadGrammar: async () =>
      vsctm.parseRawGrammar(fs.readFileSync(path.join(__dirname, "../syntaxes/galileo.tmLanguage.json"), "utf8"), "x.json"),
  });
  const g = await registry.loadGrammar("source.galileo");
  let state = vsctm.INITIAL;
  const out = [];
  for (const line of src.split("\n")) {
    const r = g.tokenizeLine(line, state);
    for (const t of r.tokens) {
      const text = line.slice(t.startIndex, t.endIndex);
      if (text.trim()) out.push([text.trim(), t.scopes[t.scopes.length - 1]]);
    }
    state = r.ruleStack;
  }
  return out;
}

function scopeOf(toks, text) {
  const t = toks.find(([x]) => x === text);
  assert(t, "no token " + JSON.stringify(text) + " in " + JSON.stringify(toks));
  return t[1];
}

(async () => {
  const src = [
    "// orbit.gg",
    "load(\"my setup.gg\")",
    "f(x) := x^2 + 2.5e3",
    "v = sqrt(G*M/r) // speed",
    "x = A \\ b",
    "plot(v, \\",
    "  r, 0, 5)",
    "for k = 1:3",
    "  s = \"done\"",
    "end",
    "function y = sq(x)",
    "  y = x*pi",
    "end",
    "clear",
  ].join("\n");
  const t = await tokens(src);
  const checks = [
    ["orbit.gg", "comment.line.double-slash.galileo"],
    ["load", "keyword.other.command.galileo"],
    ['"my setup.gg"', "string.unquoted.argument.galileo"],
    ["f", "entity.name.function.galileo"],
    [":=", "keyword.operator.galileo"],
    ["2.5e3", "constant.numeric.galileo"],
    ["sqrt", "support.function.galileo"],
    ["speed", "comment.line.double-slash.galileo"],
    ["\\", "keyword.operator.solve.galileo"],
    ["for", "keyword.control.galileo"],
    ["done", "string.quoted.double.galileo"],
    ["end", "keyword.control.galileo"],
    ["function", "keyword.control.galileo"],
    ["sq", "entity.name.function.galileo"],
    ["pi", "constant.language.galileo"],
    ["clear", "keyword.other.command.galileo"],
  ];
  for (const [text, scope] of checks) assert.strictEqual(scopeOf(t, text), scope, text);
  // the \ that ends a line is a continuation, not the solve operator
  const conts = t.filter(([x]) => x === "\\").map(([, s]) => s);
  assert.deepStrictEqual(conts, ["keyword.operator.solve.galileo", "punctuation.separator.continuation.line.galileo"]);
  // every example script in the repository tokenizes
  const ex = path.join(__dirname, "../../../examples");
  for (const f of fs.readdirSync(ex).filter((f) => f.endsWith(".gg"))) await tokens(fs.readFileSync(path.join(ex, f), "utf8"));
  console.log("ok   grammar (" + checks.length + " checks)");
})().catch((e) => {
  console.log("FAIL grammar\n", e);
  process.exit(1);
});
