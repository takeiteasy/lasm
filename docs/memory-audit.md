# Memory audit

Measurements use macOS arm64, SBCL 2.6.5-85913ede1 and the sibling STAR
ANIMA-16 implementation. MiB means 1,048,576 bytes. These are local
microbenchmarks, not full game workload budgets.

## Runtime memory

STAR uses about 49 MiB of live dynamic heap after full GC. Each CPU owns
128 KiB of packed 16-bit RAM plus registers and device state; construction
allocates approximately 129 KiB per CPU. A thousand instances therefore
add approximately 126 MiB of state, excluding game objects, debugger data
and execution headroom. Descriptors and compiled semantics are shared
within a process; separate processes each carry a Lisp runtime.

Words up to 16 bits use a shared descriptor dispatch table, at most
512 KiB of entries per machine type on a 64-bit host. STAR's first decode
builds its table in approximately 0.15 seconds. Warm the decoder before
entering a latency-sensitive loop. Wider words retain candidate scanning.

## Execution measurements

One million alternating `ADD A, #1` and `SET PC, #loop` instructions:

| Implementation | Time | Cumulative allocation |
| --- | ---: | ---: |
| Audit baseline, revision `3c58dfa` | 2.34 s | 816.2 MiB |
| Indexed dispatch alone | 0.759 s | 739.5 MiB |
| Dispatch and allocation cleanup | 0.615 s | 76.2 MiB |

The combined change reduces allocation by approximately 91% and takes
about one quarter of the baseline execution time. Emission order is
precomputed where field geometry permits, decode builds only its result
lists, and generated semantics read mapped operands without copying them.

Cumulative allocation is memory churn, not simultaneously retained memory.
Live heap after execution is approximately 49.4 MiB. The benchmark also
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

The baseline forced STAR build allocates 3.34 GiB cumulatively, and build
plus tests peaks at 940.3 MiB RSS. Full GC reduces the build's live heap
from 737.2 MiB to 48.6 MiB. A 256 MiB build heap fails even though the
precompiled runtime runs under a 128 MiB heap. Build-memory profiling is
separate from execution optimization; these build figures describe the
baseline revision, not a new measurement of the optimized compiler.
Rebuilding STAR after LASM's full suite in the same default-heap process
can exhaust the heap. STAR's build and suite pass in a fresh default-heap
process; use separate processes when validating both projects.

## Reproduce

With Quicklisp dependencies installed and STAR checked out alongside LASM:

```sh
/usr/bin/time -l sbcl --script examples/memory-audit.lisp ../star/star.asd
sbcl --script examples/cpu-scaling.lisp ../star/star.asd 100 60
```

Run once to populate compiled files, then measure a fresh process. Add
`--dynamic-space-size 128` before `--script` for the smaller runtime heap.
The memory script reports post-GC heap and allocation counters; macOS
`time` reports peak process RSS. Counters and compiler settings introduce
small variation. Both examples require SBCL.

For build measurements, load LASM, then evaluate
`(asdf:load-system :star/anima16 :force t)` and run STAR's tests in a fresh
process. Keep this separate from precompiled runtime loading.

Validation covers 2,438 LASM checks and 188 STAR checks, exhaustive
comparison with candidate scanning for two 16-bit LASM fixtures and all
65,536 STAR instruction words, plus nested and simultaneous decoding,
self-modifying code, definition replacement and memory-read ordering.
