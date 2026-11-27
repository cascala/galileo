# Galileo for VS Code

Write [Galileo](https://github.com/cascala/galileo) scripts (`.gg` files) in VS Code and
run them as you go.

* **Syntax colors** for comments, strings, numbers, constants (`pi`, `e`, `i`, `inf`),
  commands such as `load(...)` and `clear`, function calls and definitions (`f(x) := ...`),
  `for`/`while`/`if`/`function` blocks, and the `\` that continues a line.
* **Shift+Enter** sends the statement under the cursor to a Galileo prompt in the terminal
  and moves to the next one. A statement that spans lines (open brackets, a block, a line
  ending in `\`) is sent whole. With a selection, the selection is sent.
* **Run File**: the play button in the editor title runs the whole script, like
  `galileo file.gg`.
* **Load File into the Prompt** runs the script with `load(...)` in the open prompt, so
  everything it defines stays there to explore.
* **Open Prompt** starts an interactive Galileo prompt.
* `Ctrl+/` (`Cmd+/`) toggles `//` comments; brackets close themselves; blocks indent.

## Getting Galileo

The extension runs the `galileo` program. Any of these works:

* `cargo install --git https://github.com/cascala/galileo` (needs [Rust](https://rustup.rs)).
* Set **galileo.command** to the full path of a `galileo` you built.
* Install [Docker](https://www.docker.com): when `galileo` is not found, the extension uses
  the `cascala/galileo` image and mounts your workspace folder (or the script's folder) as
  `/work`, so `load(...)` finds your files.

## Settings

| Setting | Default | |
|---|---|---|
| `galileo.command` | `galileo` | The program to run: a name on your `PATH` or a full path. |
| `galileo.useDocker` | `auto` | `auto` uses Docker only when `galileo` is not found; also `always` or `never`. |
| `galileo.dockerImage` | `cascala/galileo` | The image used with Docker. |
| `galileo.explain` | `brief` | Explanations after results: `brief`, `off`, or `teach` (step by step). |

The prompt starts in your workspace folder, so `load(lessons/orbit.gg)` works with paths
relative to it. Run File runs a script in its own folder, so the scripts it loads are
found next to it.

## Notebooks

For notebooks with typeset math and interactive plots, use
[iGalileo](https://github.com/cascala/igalileo), the Jupyter kernel; VS Code's Jupyter
extension can open its notebooks.

## Building

```
cd editors/vscode
npm install
npm test               # grammar and command tests, no VS Code needed
npx vsce package       # makes galileo-0.1.0.vsix
code --install-extension galileo-0.1.0.vsix
```
