# Changelog

## 0.1.0

* Syntax colors for `.gg` files: comments, strings, numbers, constants, commands such as
  `load(...)`, function calls and definitions, `for`/`while`/`if`/`function` blocks, and the
  `\` that continues a line.
* Comment toggling, bracket matching and auto-closing, indentation inside blocks.
* **Galileo: Run File** (the play button in the editor title) runs the script in a terminal.
* **Shift+Enter** sends the statement under the cursor (all its lines, when it spans
  several) or the selection to a Galileo prompt, then moves to the next statement.
* **Galileo: Load File into the Prompt** and **Galileo: Open Prompt**.
* Uses `galileo` from your `PATH`, or the `cascala/galileo` Docker image when it is missing.
