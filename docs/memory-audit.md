# Memory audit

Measurements use macOS arm64, SBCL 2.6.5-85913ede1, and the sibling STAR
ANIMA-16 implementation. MiB means 1,048,576 bytes. These are local
microbenchmarks.

## Runtime memory

| Measure | Result |
| --- | ---: |
| STAR live heap after full GC | About 50 MiB |
| Packed RAM per CPU | 128 KiB |
| Allocation to construct one CPU | About 129 KiB |
| Estimated state for 1,000 CPUs | About 126 MiB, excluding other objects |
| Shared 16-bit decode table per machine type | At most 512 KiB on a 64-bit host |

A first STAR step takes about 0.014 s. The decode table fills entries on first
lookup, and each instruction's semantics compile on first use.[^tiers] Wider
instruction words scan candidates instead of using the table.

## Execution measurements

One million alternating `ADD A, #1` and `SET PC, #loop` instructions take
0.56 s and allocate 75.6 MiB cumulatively in the measured setup. Live
heap after execution is about 50.4 MiB. Cumulative allocation measures
memory churn, not simultaneously retained memory.

## CPU scaling

Each CPU receives 1,667 cycles per frame for 60 frames on one host thread.
Assembly, CPU construction, warm-up, rendering, and game logic are excluded.

| Workload | 10 CPUs, mean frame | 100 CPUs, mean / p95 |
| --- | ---: | ---: |
| Register loop | 6.83 ms | 68.10 / 69.16 ms |
| Memory reads and writes | 6.57 ms | 64.14 / 65.30 ms |
| Active clock interrupts | 7.01 ms | 68.60 / 69.62 ms |

These fully active synthetic CPUs exceed a 16.7 ms frame budget at 100
instances on this host. Sleeping CPUs, instruction mix, scheduling, and
other work change the budget.

## Debugger history

`bench/debugger-history.lisp` steps a two-instruction loop 200,000 times
through `debug-continue`, then reverses over a watchpoint.

| Workload | Time |
| --- | ---: |
| Plain `run` | 0.09 s |
| `debug-continue`, no history | 0.10 s |
| `debug-continue`, `:history` | 0.17 s |
| `:history`, conditional breakpoint every other step | 0.19 s |
| `debug-reverse-continue`, watch on an untouched page or register write | under 1 ms |

## Build memory

A forced STAR build allocates about 1,790 MiB cumulatively. Under a
256 MiB SBCL heap, a forced build plus 221 STAR checks peaks at 178 MB
process RSS and retains 54.6 MiB of dynamic heap after full GC. A combined
LASM and STAR test run peaks at 200 MB RSS and retains 64.8 MiB.

## Reproduce

With Quicklisp dependencies installed and STAR checked out at `../star`:

```sh
/usr/bin/time -l sbcl --script bench/memory-audit.lisp ../star/star.asd
sbcl --script bench/cpu-scaling.lisp ../star/star.asd 100 60
sbcl --script bench/debugger-history.lisp 200000
/usr/bin/time -l sbcl --dynamic-space-size 256 --script bench/build-memory-audit.lisp ../star/star.asd --star-tests
```

Run once to cache compiled files, then measure a fresh process. The scripts
report allocation and post-GC heap; macOS `time -l` reports peak RSS.

## Limitations

The workloads are short, synthetic, and sequential. They do not establish
a production CPU count, a long-running RSS limit, or parallel performance.

[^tiers]: A word-encoded instruction first compiles under a fast policy
  (`compilation-speed 3`, `debug 0`), then recompiles at the default policy
  after `*semantics-promotion-calls*` calls (1,000), so warm speed matches
  fully optimized code.
