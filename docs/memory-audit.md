# Memory audit

Measurements use macOS arm64, SBCL 2.6.5-85913ede1 and the sibling STAR
ANIMA-16 implementation. MiB means 1,048,576 bytes. These are local
microbenchmarks, not full game workload budgets.

## Runtime memory

STAR uses about 50 MiB of live dynamic heap after full GC. Each CPU owns
128 KiB of packed 16-bit RAM plus registers and device state; construction
allocates approximately 129 KiB per CPU. A thousand instances therefore
add approximately 126 MiB of state, excluding game objects, debugger data
and execution headroom. Descriptors and compiled semantics are shared
within a process; separate processes each carry a Lisp runtime.

Words up to 16 bits use a shared descriptor dispatch table, at most
512 KiB of entries per machine type on a 64-bit host. STAR's first step
builds its decode table and compiles the first instruction's semantics in
approximately 0.19 seconds. Warm the decoder and instruction semantics
before entering a latency-sensitive loop. Wider words retain candidate
scanning.

## Execution measurements

One million alternating `ADD A, #1` and `SET PC, #loop` instructions:

| Implementation | Time | Cumulative allocation |
| --- | ---: | ---: |
| Audit baseline, revision `3c58dfa` | 2.34 s | 816.2 MiB |
| Indexed dispatch alone | 0.759 s | 739.5 MiB |
| Dispatch and allocation cleanup | 0.615 s | 76.2 MiB |
| Current word-semantics registration | 0.616 s | 75.6 MiB |

The combined change reduces allocation by approximately 91% and takes
about one quarter of the baseline execution time. Emission order is
precomputed where field geometry permits, decode builds only its result
lists, and generated semantics read mapped operands without copying them.

Cumulative allocation is memory churn, not simultaneously retained memory.
Live heap after execution is approximately 50.4 MiB. The benchmark also
runs under a 128 MiB heap. This short run does not establish long-running
game behavior or guarantee a process RSS limit.

## CPU scaling

Each CPU receives 1,667 cycles per frame, approximating 100 kHz at 60 Hz.
The benchmark runs 60 frames sequentially on one host thread, excluding
assembly, CPU construction, dispatch warm-up, rendering and game logic.
Clock-interrupt runs enable the emulated clock and verify handler execution.

| Workload | 10 CPUs: mean frame time | 100 CPUs: mean / p95 |
| --- | ---: | ---: |
| Register loop | 6.83 ms | 68.10 / 69.16 ms |
| Memory reads and writes | 6.57 ms | 64.14 / 65.30 ms |
| Active clock interrupts | 7.01 ms | 68.60 / 69.62 ms |

A hundred fully active CPUs exceed a 16.7 ms frame budget on this host.
Ten leave some room for other work, but these synthetic programs do not
establish a production CPU limit. Simulated clock rates, instruction mix,
sleeping CPUs and scheduling policy all affect the budget. Parallel host
execution is not measured here.

## Build memory

A forced STAR build allocates about 1,308 MiB cumulatively. Word-instruction
registration uses compact quoted forms during compilation; semantics compile
on first execution. Registration remains available at compile time and
redefined instructions replace earlier definitions.

With a 256 MiB SBCL heap, a fresh forced build plus 188 STAR checks peaks at
170 MB process RSS and retains 51.6 MiB of dynamic heap after full GC. The
full LASM suite followed by a forced STAR build and STAR's suite also passes
under the 256 MiB heap, peaking at 200 MB RSS and retaining 64.8 MiB after
full GC. Peak RSS, cumulative allocation and post-GC live heap measure
different kinds of memory use. Build figures are separate from the
precompiled runtime measurements above.

## Reproduce

With Quicklisp dependencies installed and STAR checked out alongside LASM:

```sh
/usr/bin/time -l sbcl --script bench/memory-audit.lisp ../star/star.asd
sbcl --script bench/cpu-scaling.lisp ../star/star.asd 100 60
```

Run once to populate compiled files, then measure a fresh process. Add
`--dynamic-space-size 128` before `--script` for the smaller runtime heap.
The memory script reports post-GC heap and allocation counters; macOS
`time` reports peak process RSS. Counters and compiler settings introduce
small variation. Both scripts require SBCL.

For build measurements, run:

```sh
/usr/bin/time -l sbcl --dynamic-space-size 256 --script bench/build-memory-audit.lisp ../star/star.asd --star-tests
/usr/bin/time -l sbcl --dynamic-space-size 256 --script bench/build-memory-audit.lisp ../star/star.asd --combined --star-tests
```

The script reports allocation and heap use per compiled STAR file and
post-GC live heap. `--phases` reports inclusive allocation for selected LASM
definition and registration functions. `--sprof` samples allocation during
`basic.lisp` compilation. macOS `time -l` reports peak process RSS. Run
fresh measurements in separate processes with compiled dependencies already
cached; `--combined` keeps the LASM test suite's heap history.

Validation covers 2,451 LASM checks and 188 STAR checks, exhaustive
comparison with candidate scanning for two 16-bit LASM fixtures and all
65,536 STAR instruction words, plus nested and simultaneous decoding,
self-modifying code, definition replacement and memory-read ordering.
