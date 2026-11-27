# Working on Galileo (cascala/galileo)

* The language reference lives in `docs/language.md` (hand-written, its ```galileo examples
  are run as tests) and `docs/builtins.md` (generated from `src/help.rs`).
* Changing the syntax (`src/parser.rs`) or adding a command means updating the reference in
  the same commit: describe the change in `docs/language.md`, give a new command a help
  entry in `src/help.rs`, then run `GALILEO_BLESS=1 cargo test --test reference` and commit
  the renewed `docs/`. `cargo test --test reference` (and CI) fails until this is done.
