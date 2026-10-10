# Checking `OpcodeFaults` against the host runtime

## Goal

`OpcodeFaults` (in `WoofWare.PawPrint.Semantics`) says which exceptions each IL
instruction raises by itself. The escape analysis reads it as a sound
over-approximation, so any exception CoreCLR raises that the table leaves out
makes the analysis claim something false. #1822 found one: the table followed
ECMA-335 III.3.19 and said `ckfinite` raises `ArithmeticException`, while
CoreCLR raises `OverflowException`.

Most entries were written from the specification. This plan checks them against
the runtime PawPrint emulates, in both directions:

- **Soundness.** Everything the host raises is in the table.
- **Precision.** Every logic fault the table lists is raised by some input.
  This catches entries that list exceptions needlessly, which cost the analysis
  precision. Resource exhaustion (`OutOfMemory`, `StackOverflow`) is not
  checked this way, for the reasons below.

## Method

Each instruction runs as a `DynamicMethod` on the test host, with operands
built for it. Two facts constrain how:

- **Only type-correct IL may be emitted.** On .NET 10 arm64, `ckfinite` applied
  to an int32 killed the process with an illegal instruction, which no handler
  can catch. So the operand types come from ECMA-335 III.1.5's tables. The
  oracle never tries an operand type to see whether the runtime rejects it.
- **The result must be consumed.** The JIT may drop a computation, or a load,
  whose value nothing reads. Each probe returns its result.

The host is the oracle, so a run checks the code the JIT emits for the host's
architecture. CI runs Linux x64, and a local run on a Mac runs arm64.

Approach: one test per family of instructions, each with its own input
construction. The alternative was a single generator of well-typed
one-instruction programs over a typing model of every instruction. It was
rejected: a typing model that is wrong anywhere crashes the host, and the
instructions that take a metadata token need their own setup regardless.

## Stages

Each stage is one PR.

1. **Instructions whose operands are all numbers** (`TestOpcodeFaultsOnHost`):
   arithmetic, `*.ovf*`, comparisons, shifts, `neg`, `not`, `conv.*` and
   `ckfinite`, 60 in all. On a grid of boundary operands, the exceptions
   raised must equal the table's entry exactly. On random operands, they must
   be a subset of it. It replaces #1822's single `ckfinite` test. Before
   anything was written, a probe found the table already agreed with the host
   for this family.
2. **Arrays, pointers and block operations** (`TestOpcodeFaultsOnHostMemory`):
   `ldelem*`, `stelem*`, `ldlen`, `ldind*`, `stind*`, `cpblk`, `initblk`, and
   `throw` of null, plus the token-bearing `ldelem`, `stelem`, `ldelema`,
   `ldobj`, `stobj`, `cpobj` and `initobj`. Inputs: null, a valid array or
   address, an index out of range (including a negative one and a native int
   beyond int32), and an array whose element type does not match what is
   stored. The host raised nothing the table omits. It never raised
   `ArrayTypeMismatchException` from `stelem.<type>` of a primitive, which the
   table listed, so those entries now leave it out. `cpblk` and `initblk`
   through a null address raise `NullReferenceException` at every length tried,
   up to 64 KiB, on x64 as on arm64.
3. **The other instructions that take a metadata token**
   (`TestOpcodeFaultsOnHostTokens`): `castclass`, `isinst`, `unbox`,
   `unbox.any`, `box`, `newarr` (negative and too-long lengths), `refanyval`,
   `mkrefany`, `ldvirtftn`, `callvirt` on null, `ldftn`, `ldtoken`, `sizeof`,
   `ldstr`, and the entries for a static constructor that throws (`ldsfld`,
   `ldsflda`, `stsfld`, `call`, `callvirt`, `newobj`, `jmp`, `calli`, and
   `ldfld`, `ldflda` and `stfld` naming a static field), tested through types
   whose static constructors always throw. Every entry agreed with the host.
   A test requires every token-bearing instruction except the `constrained.`
   prefix to be checked here or in stage 2.

## The interpreter against the same inputs

The stages above show that the table agrees with the host. The interpreter
checks, on every fault it raises, that the table lists that fault
(`IlMachineStateExecution.raiseOpcodeFaultWithMessage`). Nothing checks the
other direction: that the interpreter raises a fault wherever the host does.
Before #1839, `ckfinite` was an example: the interpreter refused it outright.

So each stage's inputs also run under PawPrint. For each instruction, the probe
bodies are emitted into a fabricated assembly (`PersistedAssemblyBuilder`), as
static methods with exactly the IL of the host probes. A C# driver calls each
one on every input the stage uses, in the order the stage enumerates them, and
prints the index and exception type of every input that raised. The test runs
the driver under PawPrint and compares those lines with what the in-process
probes raised for the same inputs. The in-process probes remain the oracle; the
driver never runs on the real runtime.

There is one guest per instruction, so that one refusal stops only its own
instruction, and the guests run in parallel. Measured on stage 1's grid, a call
that raises nothing costs about 0.3 ms under PawPrint and one that raises about
10 ms, so stage 1, about 110,000 calls with 5,000 faults, costs about 1.5
minutes of CPU, which is about 1.3 minutes of the default suite's wall time.
Since each case runs a guest, the fixture is in the `Guest` category.

An instruction whose first run disagrees with the host is parked
(`TestCaseData.Explicit`) with the disagreement as the reason. The fix to the
interpreter is a change of its own, which unparks it.

Stage 1 comes first (`TestOpcodeFaultsOnPawPrint`). On its first run, 46 of the
60 instructions raised exactly what the host raised on every input. The other
14 are parked, and every disagreement is a refusal rather than a wrong
exception:

- `add.ovf.un` and `sub.ovf.un` are unimplemented.
- `CliType.zeroOfPrimitive` makes a native int's zero a null managed pointer,
  which is what a C# array initialiser leaves in an element it does not store.
  `add`, `sub` and `mul` compute with it as 0, but `div`, `div.un`, `rem`,
  `rem.un`, `and`, `or`, `xor`, `shl`, `shr`, `shr.un` and `mul.ovf.un` refuse
  it. CoreCLR raises `DivideByZeroException` for the divisions and computes
  the rest.
- `conv.r.un` refuses a float operand, which CoreCLR converts without raising.

Stages 2 and 3 do not yet run under PawPrint.

## Not checkable in process

These are listed in the tests with the reason, rather than silently skipped.

- `StackOverflow`: CoreCLR ends the process instead of raising it.
- `OutOfMemory`, except `newarr` with a length beyond the maximum: no other
  allocation fails reliably.
- `calli` through a null pointer: CoreCLR crashes. The table's
  `NullReference` entry records PawPrint's own deliberate divergence
  (`docs/divergences.md`).
- `rethrow`, which the table leaves `Unmodelled`.
- Failures to bind an instruction's metadata token, which the table excludes by
  design.
