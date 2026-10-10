# Continuous integration

GitHub Actions runs the [test suite](getting-started.md#run-the-tests) on SBCL, ECL and CCL when a version tag (`v*`) is pushed. Commits and pull requests do not start a run.

| Trigger | SBCL | ECL | CCL |
|---|---|---|---|
| Version tag (`v*`) | yes | yes | yes |
| Manual run | `all`, `sbcl` | `all`, `ecl` | `all`, `ccl` |

A new run cancels the run it supersedes.

## Run the full matrix

```sh
gh workflow run ci.yml -f lisps=all
gh run watch
```

`lisps` takes `all`, `sbcl`, `ecl` or `ccl`. ECL builds from source, so its run is the slow one.

## What a run does

- Installs [Roswell](https://github.com/roswell/roswell) and the implementation.
- Links Roswell's Quicklisp to `~/quicklisp` and clones `trivial-high-precision-timer` beside LASM, as in [Getting started](getting-started.md#install).
- Runs `tests/ci.lisp`, which exits nonzero on any failure.
- On SBCL, builds the standalone binary with `ros build lasm.ros` and runs an example through it.

The same suite runs locally:

```sh
ros -L sbcl-bin -Q -l tests/ci.lisp
```

## Differences between implementations

A few tests are skipped where an implementation cannot run them; see the [limitations](#limitations).

## Limitations

- ECL and CCL cannot use an outer `macrolet` macro in an inner `macrolet`'s expander inside `definstruction`; those cases run on SBCL only. Tracked in [#56](https://github.com/takeiteasy/lasm/issues/56).
- CCL on arm64 macOS crashes in one reader test during a full run, so it is skipped there. Tracked in [#57](https://github.com/takeiteasy/lasm/issues/57).
