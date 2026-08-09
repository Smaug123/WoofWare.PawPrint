# Pointer arithmetic on a byref to a whole typed slot

## The verified blocker

`SprintfBasic` is parked with a comment claiming the next two primitives are
`RuntimeMethodHandle.GetMethodDef` and `MetadataImport`'s mdtParamDef enumeration
(`EnumParams`). **That claim is now wrong.** Un-parking the case and running it shows
sprintf never reaches `GetParameters()`:

```
refusing to do pointer arithmetic on a bare stack slot address:
  <variable 0 in method frame <frame #1904> of thread 0>

GUEST STACK:
  MethodBaseInvoker.InvokeDirectByRefWithFewArgs @ IL index 60
  MethodBaseInvoker.InvokeWithOneArg          @ IL index 161
  RuntimeMethodInfo.Invoke                    @ IL index 125
  MethodBase.Invoke                           @ IL index 11
  FormatParser`4.buildCaptureFunc             @ IL index 291
  ... parseAndCreateFuncFactoryAux ...
  PrintfModule.PrintFormatToStringThen
  SprintfBasic.main
```

`MethodBase.Invoke` takes its argument types from `Signature._arguments`, not from
`ParameterInfo`, so PR #837 (which fills `_arguments`) moved sprintf straight past the
`GetParameters()` chain and onto reflective *invocation*. The park comment was written
from a spike, not from a run; this plan is written from a run.

`buildCaptureFunc` calls `MakeGenericMethod(...).Invoke(null, new object[1]{...})` — one
argument, static target, no byrefs.

## What is actually failing

Upstream (`src/libraries/.../Reflection/MethodBaseInvoker.cs:153`):

```csharp
StackAllocatedByRefs byrefs = default;
IntPtr* pByRefFixedStorage = (IntPtr*)&byrefs;

for (int i = 0; i < _argCount; i++)
{
    *(ByReference*)(pByRefFixedStorage + i) = ...;
}
```

where `StackAllocatedByRefs` (`MethodBase.cs:221`) is

```csharp
[InlineArray(MaxStackAllocArgCount)]     // 4
internal ref struct StackAllocatedByRefs { internal ref byte _arg0; }
```

The IL at index 60 is `ldloc.1; ldloc.2; conv.i; ldc.i4 8; mul; add` — a byref that
addresses a *whole local slot* plus a computed offset. `ArithmeticTarget.decompose`
(`BinaryArithmetic.fs:61`) has arms for every byte- or element-addressed root
(`StackMemoryByte`, `NativeMemoryByte`, `ArrayElement`, `StringCharAt`,
`HeapObjectField`) and for any projection chain, but a bare root with no projections
falls through to a `failwith`. Roots that address a whole typed value —
`LocalVariable`, `Argument`, `StaticField`, `HeapValue` — therefore cannot be offset at
all, not even by zero.

This is not sprintf-specific. `docs`-free repro, which fails identically:

```csharp
Pair pair = default;                 // struct { int A; int B; }
int* p = (int*)&pair;
for (int i = 0; i < 1; i++) { if (p + i != p) return 1; }
```

(`WoofWare.PawPrint.Test/sourcesPure/StructLocalPointerArithmetic.cs`, written first and
observed failing.)

## Options

### Decision 1 — what does `&wholeValue + n` produce?

**Option A: zero-offset identity only.** `v = 0` returns the pointer unchanged; nonzero
keeps failing.

- Smallest possible change, and it is all sprintf needs: `_argCount` is 1 there, so the
  loop only ever computes `p + 0`. Verified by spike — with this one line, sprintf runs
  all the way to the next primitive.
- But `*(p + 1)` stays broken, so the guest test has to be trimmed to the zero case, and
  `MethodBase.Invoke` with 2–4 arguments stays blocked behind the same `failwith`.
- Blast radius: nil. Reversible: trivially.

**Option B (recommended): zero-offset identity, plus offset → field resolution.**
Nonzero offsets read the pointed-to value, ask `CliType.getFieldAt` which field lands on
that byte offset, and append a `Field` projection; when no single field lands there, fall
back to a byte view exactly as the existing `FieldTarget` arm does; fail loudly when the
value has no byte image either.

- This is not a new mechanism: the `FieldTarget` arm of `addOffsetToManagedPtr`
  (`BinaryArithmetic.fs:288`) already does precisely this dance for a byref that is
  *already* a field. Option B says a byref to a whole slot is the same thing with base
  offset 0.
- Keeps the result typed at the field, which is the only reason `StackAllocatedByRefs`
  can work at all: its fields are `ref byte`, and a managed pointer has no byte image, so
  a byte view of that struct is unrepresentable. Field resolution is the only route that
  reaches the case sprintf's caller needs for argument counts above one.
- Blast radius: confined to one function; every root kind that reaches it currently
  throws, so no existing behaviour changes. Reversible: yes.

**Option C: byte-view the whole value.** `&local + n` becomes
`Byref (root, [ReinterpretAs typeOfLocal ; ByteOffset n])`.

- Smallest *general* change, reusing all the `ByteViewTarget` machinery.
- Rejected: it fails on the case that motivated the work. A byte view demands a byte
  image, and `StackAllocatedByRefs` is byte-imageless (see the
  `explicit-layout-with-refs-is-field-inaccessible` finding — refs make a struct
  byte-imageless regardless of layout). It also discards field typing for cases that have
  it, which is information downstream reads and writes want.

### Decision 2 — at offset 0, identity or field #0?

**Identity** (return the incoming pointer unchanged).

Resolving `&local + 0` to `&local._field0` would give one address two structural forms,
and PawPrint's byref equality is structural: `Unsafe.AreSame(p, p + 0)` and `ceq` would
report *false* where real .NET reports true. Every existing arm of
`addOffsetToManagedPtr` is already identity-preserving at zero (`offset + 0` returns the
same root and index), so identity is the consistent answer as well as the faithful one.

It also keeps this change out of the *write* path: the pointer a subsequent `stobj` sees
is byte-for-byte the one it sees today.

### Decision 3 — which roots are "a whole typed value"?

Include `LocalVariable`, `Argument`, `StaticField`, `HeapValue`: all four are a single
slot holding one `CliType`, all four are readable through `IlMachineState.readManagedByref`,
and the same offset→field rule is meaningful for each. Widening all four together rather
than only the two sprintf needs, per the `byref-access-surfaces-widen-together` lesson.

Leave `PeByteRange` and `ExposedClassObject` failing, with their own messages saying why:
a PE byte range is byte-addressed (arithmetic on it wants a byte cursor, not fields, and
nothing asks for it yet), and `ExposedClassObject` is a single objref cache cell with no
interior structure to offset into. Crashing beats a plausible-but-wrong answer here.

## Non-goals

- **`RuntimeMethodHandle_InvokeMethod`.** Spiked and confirmed: with Decision 1 applied,
  sprintf's next failure is
  `Unimplemented native method (PInvokeImpl QCall!RuntimeMethodHandle_InvokeMethod)`,
  reached through `MethodBaseInvoker.InterpretedInvoke_Method`. That is the reflective
  invoke primitive — it must unmarshal N byrefs out of the caller's stack buffer, call a
  guest method through the `SuspendedForManagedCall` re-entry pattern
  (`NativeRuntimeTypeQCall.fs:654` is the worked example), and box the result into an
  `ObjectHandleOnStack`. It is its own feature and its own PR.
- **A narrow `stobj` through a pointer to a wider slot.** Sprintf writes
  `*(ByReference*)&byrefs`, i.e. a one-pointer value through a pointer to a four-pointer
  struct. With Decision 2 that pointer is unchanged by this PR, so whatever the write
  path does today it keeps doing. The guest test pins the behaviour that matters
  (`*(pairPtr + 0) = 7` must leave `pair.B` alone) so a regression here is visible, but
  making argument counts 2–4 work end to end belongs with the QCall that needs them.
- **Un-parking `SprintfBasic`.** It cannot pass until the QCall lands. Its park comment
  gets rewritten to name the real remaining chain instead of the stale `GetMethodDef`
  claim.

## Test plan

- `sourcesPure/StructLocalPointerArithmetic.cs` (differential, written first and observed
  failing): identity at zero via a loop-derived offset so Roslyn cannot fold it; a store
  through the whole-struct pointer leaving the sibling field intact; walking a
  two-`int` struct up and an `[InlineArray(4)]` of `long` up and back down. Offsets come
  from loop variables throughout, because a literal `p + 0` folds away at compile time
  and the test would then cover nothing.
- Unit tests over the four whole-value roots, including the byte-view fallback for an
  offset that lands mid-field and the loud failure for `PeByteRange`/`ExposedClassObject`.
- Mutation pass over the new arm, checking that each mutant dies to a test that could
  plausibly observe it (per `mutation-runs-recheck-e2e-tests`).

## Decision 1, revised: byte cursor, not field resolution

Option B shipped first and Codex review found two P1 divergences in it. Both reproduce, and
both were verified by running guest programs rather than by reading the code:

```csharp
// A wider access landing where a narrower field begins.
[StructLayout(LayoutKind.Explicit, Size = 8)]
struct FourBytes { [FieldOffset(4)] byte A; ... [FieldOffset(7)] byte D; }
// *(int*)((byte*)&v + 4) returned 1 (just `A`) instead of 0x04030201.

// A pointer round trip.
int* p = (int*)&pair; int* q = p + 1; q -= 1;
// q == p was false: `q` came back as the field at offset 0, not as `p`.
```

Both are silent wrong answers, which is worse than the `failwith` they replaced.

The diagnosis generalises past the two symptoms: **advancing a pointer moves an address, it
does not choose a type view.** The access width arrives later, with the `ldind`/`stind` that
dereferences the pointer. Resolving an offset to "whichever field begins there" decides that
width too early, and gets it wrong in both directions — too narrow for a wide read, and
unable to get back to the whole-slot form on the way home.

So the non-zero path is now Option C after all: a byte cursor
(`ReinterpretAs Byte ; ByteOffset n`), which is how every other byte-addressed root in this
function already behaves. That fixes both findings at once — the width comes from the
dereference, and `normaliseByteOffset` drops a zero `ByteOffset` so the round trip really
returns.

What survives from the original Decision 1 is the *zero* case: the identity, not a
zero-length byte cursor. Those two are indistinguishable to a guest (`ceq` strips a trailing
`ReinterpretAs`), so the reason is the write path rather than equality: a slot whose value
has no byte image can be written whole but not through a byte cursor, and that is exactly
`StackAllocatedByRefs`. Sprintf's `stobj` through `&byrefs + 0` depends on it. A unit test
pins that rationale directly, since no C# guest can express the shape (a pointer to a struct
of byrefs is not a legal C# unmanaged type — CoreLib needs `#pragma warning disable CS9184`
to declare it).

What Option C's original rejection got wrong: it claimed a byte view "fails on the case that
motivated the work". It does not, because that case is at offset zero, which is the identity.
The imageless-struct problem only appears at non-zero offsets — argument counts of 2 to 4 —
where a byte cursor now fails loudly at the dereference rather than answering wrongly. That
is the right failure mode, and it belongs to whichever change first needs those counts.

## Results

Implemented as: a `WholeValueTarget` case on `ArithmeticTarget`, the `[]` arm of `decompose`
classifying the four whole-value roots into it (and refusing `PeByteRange` /
`ExposedClassObject` with their own messages), and an `addOffsetToManagedPtr` arm that is the
identity at zero and a byte cursor otherwise.

One deviation from the plan, for the better: the zero-offset identity lives *inside* the new
arm rather than at the top of `addOffsetToManagedPtr`. A global short-circuit would have
changed an existing arm — `StringTarget` anchors a `ReinterpretAs Char` view even at offset
zero (`addByteOffsetUnderReinterpret` appends the reinterpret unconditionally) — so a
top-level `if v = 0 then ptr` would have silently stopped emitting that anchor. Keeping the
identity inside the new arm makes the change purely additive: every pointer shape that works
today produces exactly the same result.

### Mutation testing

Eight mutants against `TestBinaryArithmetic` plus the guest test; all killed, and — per
`mutation-runs-recheck-e2e-tests` — each by a test that could plausibly observe it.

| Mutant | Killed by |
| --- | --- |
| Drop the zero identity | `adding zero…`, `…no byte image, a byte cursor cannot` |
| Byte cursor loses the offset | all four byte-cursor tests, guest test |
| `LocalVariable` not a whole-value root | `adding zero…`, guest test |
| `Argument` not a whole-value root | `adding zero…`, guest test |
| `StaticField` not a whole-value root | `adding zero…`, guest test |
| `HeapValue` not a whole-value root | seven unit tests |
| `PeByteRange` accepted | `a whole PE byte range refuses…`, alone |
| `ExposedClassObject` accepted | `a RuntimeType cache cell refuses…`, alone |
| Cursor-minus-slot delta negated | `subtracting a byte cursor…`, guest test |
| Distinct slots return a zero delta | `subtracting pointers to two distinct…`, alone |
| Argument refusal dropped entirely | `subtracting unrelated argument pointers…`, alone |
| `sameArgumentRoot` ignores the slot index | `subtracting unrelated argument pointers…`, alone |
| Offset cancellation tested by negation again | two `accumulating byte offsets…` cases, guest test |

Two notes on what the mutants taught, beyond the score:

- Under the *first* (field-resolution) implementation, the guest test killed the
  drop-the-zero-identity mutant. Under this one it does not, because a zero-length byte cursor
  compares equal to the bare pointer — so the identity's real justification is the write path,
  not equality, and it needed the test that says so. A mutant changing its killer set is a
  signal that the reason for the code changed too.
- The regression tests for the two Codex findings are stronger than any mutant: both were
  *observed failing* against the field-resolution implementation and passing against this one,
  rather than being killed by a synthetic edit.

Two tests earned their keep by asserting their own premises: the imageless-value test first
failed on its guard (`CliType.ToBytes` succeeds for a struct with an *object reference* field
— it is managed *pointers* that have no byte image), which would otherwise have made it pass
while covering nothing.

### Verified afterwards

- Full suite: 2357 passed, 0 failed.
- `SprintfBasic`, un-parked, fails with
  `Unimplemented native method (PInvokeImpl QCall!RuntimeMethodHandle_InvokeMethod)` — the
  blocker the plan predicted, and the next PR.

## Pointer difference, added on second review

The second Codex pass found a P2: advancing a whole-slot pointer now worked, but *measuring*
the advance did not. `subManagedPtrManagedPtr` had no case pairing a byte cursor with the
whole-slot byref it came from, so `int* q = p + 1; q - p;` hit its final TODO. Verified by
running it, and it was a loud failure rather than a wrong answer — but a newly reachable one,
created by this change: before it, `p + 1` threw first.

This is `byref-access-surfaces-widen-together` almost verbatim — one half of a paired surface
widened without the other. Fixed by adding three arms: cursor-minus-slot (the delta is the
cursor's offset, since the slot's own address is the zero point), its mirror, and
slot-minus-slot (zero for the same slot; a loud refusal for two distinct ones, because
separate locals are separate storage here and have no byte distance to invent).

## Argument slots, added on third review

The third Codex pass found the same shape one level down: `subManagedPtrManagedPtr` refuses
*any* argument-rooted pointer before it decomposes anything, so `cursor - p` inside a method
with a by-value struct parameter still threw even after the difference arms landed. Verified by
running it.

The refusal is now conditional on the two pointers *not* sharing one argument slot. Two
different argument slots, and an argument paired with anything else, stay refused with the same
message — there is no byte distance between separate storage to report. Two pointers into one
argument slot have one, which is exactly what `&arg + n` produces.

A side effect worth naming: with the blanket refusal relaxed, `&arg.First - &arg.Second` now
also reaches the existing `FieldTarget` arm and yields a field-offset delta, where before it
threw. That is the same rule (one storage location, so a defined distance) rather than a second
decision.

Three review rounds, three findings, all in the same family: a surface widened without its
pair. Worth remembering that "did I widen the *inverse* operation too?" is the question to ask
before sending pointer work for review, not after.

## Overflow and two stale comments, added on fourth review

Rebased onto `origin/main` first, which had moved to #838 (`stind` through a byref to a
pointer-typed field) — adjacent code, so the suite was re-run rather than assumed, and
sprintf's blocker re-confirmed as the QCall.

The fourth Codex pass found three things, all fair:

- **P2, an interpreter crash.** `byte* q = p + int.MinValue; q += 1;` threw a *host*
  `OverflowException`. `appendProjection` tested offset cancellation as `n = -m`, and this
  codebase is `Checked`, so negating `Int32.MinValue` throws even though the sum
  (`-2147483647`) is perfectly representable and real .NET completes. Now tested as
  `m + n = 0`, which is equivalent — offsets cancel exactly when their sum is zero, and that
  sum cannot itself overflow — and strictly more total. Pre-existing (reachable through any
  byte view), but newly reachable through whole-slot pointers, so fixed here.
- **Two P3 stale comments**, both mine, both artefacts of changing the design mid-flight:
  the `WholeValueTarget` doc comment still described field resolution, and the zero-offset
  comment still gave `ceq` as the reason for the identity. The mutation pass had already shown
  that reason to be false (a zero-length byte cursor compares equal, so no guest can tell), and
  the plan said so, but the code comment had not caught up. A comment that gives future
  maintainers the opposite semantics from the code is worth as much as a bug.

### Found while writing the regression test, and deliberately left alone

`q -= int.MinValue` still fails loudly: undoing that offset needs `+2147483648`, which the
int32 symbolic-offset model cannot express in one step even though the endpoint is
representable. Supporting it means accumulating offsets in int64 and narrowing only at the end
— a change to the offset model itself, not to this arm. The guest test says so at the point
where it stops short, rather than quietly testing something easier.

## Fifth review: one pre-existing bug parked, one finding declined with evidence

- **P1, narrow `stobj` through a wide slot, declined as out of scope and parked instead.**
  `*(Narrow*)&wide = ...` replaces the whole `Wide` slot rather than writing only the bytes the
  store covers, so the next `wide.B` fails in `CliValueType.FindFieldById`. Codex framed this as
  exposed by the new arithmetic, and a computed zero offset does reach it — but it is
  pre-existing and needs no arithmetic at all: the first half of
  `sourcesPure/NarrowStructStoreThroughWideSlot.cs` is just `ldloca wide; stobj Narrow`, and it
  fails identically on `origin/main` (verified by running it in a throwaway worktree at
  c355bfe, not by reasoning). The fix belongs to `writeManagedByrefCore`, which takes its
  whole-root path whenever the pointer names the slot itself. Parked in `TestPureCases`'
  `unimplemented` with that diagnosis, so the gap is recorded in the repo rather than only in a
  review thread.

- **P2, canonicalising a zero-offset cursor back to the bare slot, declined — it would pull
  against P1.** The two findings point in opposite directions, which running them settles.
  A narrow `stobj` through a cursor that has round-tripped to offset zero writes only its own
  four bytes and is *correct today*; through the bare whole-slot pointer it clobbers the slot
  (P1). Canonicalising the cursor to the bare form would therefore take a working path and give
  it the P1 bug. The case P2 cites — an imageless slot written after `+n; -n` — is not reachable
  from any guest or CoreLib path (`MethodBaseInvoker` never returns to zero), and fails loudly
  rather than wrongly. So the ordering is: fix the write path first, after which both forms agree
  and the canonicalisation becomes safe *and* unobservable. `StructLocalPointerArithmetic.cs`
  now pins the currently-correct cursor behaviour, so a future canonicalisation cannot regress
  it silently.

  A tempting wrong version of this fix is worth naming: collapsing *any* trailing `ReinterpretAs`
  at offset zero would also discard a type view the guest established deliberately with
  `Unsafe.As`, turning a subsequent read of the reinterpreted type into a read of the slot's own
  type. Only an arithmetic-introduced byte anchor could safely be dropped, and the representation
  does not distinguish the two.

### Verified afterwards

- Full suite: 2358 passed, 0 failed, rebased on c355bfe.
