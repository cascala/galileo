// A stand-in for the vscode module, enough to load extension.js outside VS Code
const Module = require("module");
const calls = [];
const config = {};
const mock = {
  calls,
  config,
  Uri: class Uri {
    static parse(s) {
      return { toString: () => s };
    }
  },
  Position: class {
    constructor(line, character) {
      Object.assign(this, { line, character });
    }
  },
  Selection: class {
    constructor(a, b) {
      Object.assign(this, { anchor: a, active: b, isEmpty: a.line === b.line && a.character === b.character });
    }
  },
  Range: class {
    constructor(a, b) {
      Object.assign(this, { start: a, end: b });
    }
  },
  workspace: {
    getConfiguration: () => ({ get: (k, d) => (k in config ? config[k] : d) }),
    getWorkspaceFolder: () => undefined,
    workspaceFolders: undefined,
    textDocuments: [],
  },
  window: {
    terminals: [],
    activeTextEditor: undefined,
    createTerminal(opts) {
      const t = { name: opts.name, opts, sent: [], exitStatus: undefined, show() {}, sendText(s) { this.sent.push(s); } };
      calls.push(["createTerminal", opts]);
      this.terminals.push(t);
      return t;
    },
    showErrorMessage: async (m) => calls.push(["error", m]),
    showWarningMessage: async (m) => calls.push(["warning", m]),
    onDidCloseTerminal: () => ({ dispose() {} }),
  },
  commands: {
    registered: {},
    registerCommand(name, f) {
      this.registered[name] = f;
      return { dispose() {} };
    },
    executeCommand: async () => {},
  },
  env: { openExternal: async () => {} },
};
const load = Module._load;
Module._load = function (req, ...rest) {
  return req === "vscode" ? mock : load.call(this, req, ...rest);
};
module.exports = mock;
