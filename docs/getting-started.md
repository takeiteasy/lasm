# Getting started

After cloning, run the example and test commands from the repository root.

## Install

Clone LASM and its timer dependency into Quicklisp's local projects
directory.[^dependencies]

```sh
git clone https://git.sr.ht/~takeiteasy/lasm ~/quicklisp/local-projects/lasm
git clone -b trunk https://git.sr.ht/~takeiteasy/trivial-high-precision-timer ~/quicklisp/local-projects/trivial-high-precision-timer
```

Load LASM in SBCL, ECL or CCL:

```lisp
(ql:quickload :lasm)
```

For the [command line](cli.md), link both projects into Roswell's local
projects directory:

```sh
ln -s ~/quicklisp/local-projects/lasm ~/.roswell/local-projects/lasm
ln -s ~/quicklisp/local-projects/trivial-high-precision-timer ~/.roswell/local-projects/
ros lasm.ros --help
```

## Try an example

The [DCPU-16 example](examples.md) is an ASDF system that assembles and
runs a program:

```lisp
(asdf:load-asd #p"examples/dcpu16/dcpu16.asd")
(asdf:load-system :dcpu16)
```

From the command line:

```sh
ros lasm.ros run tests/fixtures/cli/counter.asm -m tests/fixtures/cli/sixtyfoo.lisp
```

## Run the tests

```sh
sbcl --non-interactive --eval '(asdf:test-system :lasm)'
ecl --eval '(asdf:test-system :lasm)' --eval '(ext:quit)'
ccl -n --eval '(asdf:test-system :lasm)' --eval '(ccl:quit)'
```

The suite tests each [example](examples.md) in its own Lisp process and
reports failures with a nonzero exit status.[^tests]

## Next

Start with [Machine model](machine-model.md), [Instructions](instructions.md),
or the [documentation index](README.md).

[^dependencies]: LASM and `trivial-high-precision-timer` are not on Quicklisp.
  Eclector, which reads [snapshot](snapshots.md) and [items](items.md) files, is. Without Quicklisp,
  make their system files, Eclector's and CFFI's available to ASDF, then run
  `(asdf:load-system :lasm)`.
[^tests]: On SBCL the suite caches a bootstrapped core as `examples.core`
  beside compiled LASM files; delete that core after changing Quicklisp
  dependencies. ECL starts each script with `ecl --shell` and CCL with `ccl -b -l`; the scripts need `ecl` on `PATH`. Set
  `LASM_BENCH=1` to run the external benchmarks with STAR at `../star`.
