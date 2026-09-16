# Array-rooted byref arithmetic: byte units, and the missing subtraction arms

Rung J of the ASP.NET ladder now runs the handler, closes System.Text.Json's converter type and
reads its fields' attributes (`docs/plans/2026-09-08-field-metadata-token.md`). It stops next in
`System.Text.Unicode.Utf8.FromUtf16`, transcoding the response body. This is the plan for that
stop, which turns out to be two defects, not one.

## What is measured

A fifteen-line guest reproduces the stop with no ASP.NET and no JSON:

```csharp
char[] src = { 'a', 'b' };
byte[] dst = new byte[16];
Utf8.FromUtf16 (src, new Span<byte> (dst, 13, 3), out int charsRead, out int bytesWritten);
```

```
refusing to subtract array element pointer from incompatible pointer:
  <element 4 of array <object #37>> vs <<element 0 of array <object #37>> as System.Char>
  ... in Utf8.FromUtf16 at IL offset 281
```

That is `charsRead = (int)(pInputBufferRemaining - pOriginalSource)` (Utf8.cs:115). The source is
a **two**-element `char[]`, fully consumed, so the correct end pointer is element 2 — the
interpreter has it at element 4.

Sweeping the source length pins that down. With one char the refusal names element 2; with two,
element 4; with three, the index escapes the refusal entirely and faults as
`array index 4 >= length 3`. The reported index is the *byte* offset, not the cell index.

A probe in `addOffsetToManagedPtr`'s array arm (temporary `eprintfn`, never committed) confirms
the mechanism directly. The guest above reaches it twice:

```
PROBE-ARRAYADD: elementSize=2  offset=4  index=0     <- the char[] source
PROBE-ARRAYADD: elementSize=1  offset=2  index=13    <- the byte[] destination
```

Two chars is four bytes; the arm adds 4 to the *cell* index.

## The two defects

**(A) `add`/`sub` of an integer to a plain array-element byref uses element units where
ECMA-335 says bytes.** `BinaryArithmetic.fs:340`:

```fsharp
| ArithmeticTarget.ArrayTarget (arr, index) ->
    let index = checkedAddInt32 "array index" index v
    ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) |> Choice1Of2
```

`v` is the raw IL operand of `add`/`sub`, which is a byte count: C# lowers `p + n` on a `T*` to
`ldc n; sizeof T; mul; add`. All three sibling arms in the same `match` agree — stack memory and
native memory add `v` to a byte offset, and the string arm hands `v` to
`addByteOffsetUnderReinterpret` as bytes. Only the array arm reads it as elements. It was written
that way in #247 ("Complete the implementation of BinaryArithmetic") with no comment, replacing
`failwith "TODO: arrays"`.

**(B) `sub` of two array-rooted byrefs where exactly one side carries a byte view has no arm.**
Stack memory, native memory and strings each have *both* mixed-direction arms
(`BinaryArithmetic.fs:626`, `:674`, `:709`). Arrays have only byte-view↔byte-view and
whole↔whole, so the mixed pair falls through to the catch-all refusal at `:777`.

They interact, and the order matters: **(B)'s refusal is currently masking (A)**. Fix (B) alone
and `Utf8.FromUtf16` returns `charsRead = 4` for a two-character input — a wrong number, silently,
where today there is a loud stop. Fix (A) alone and rung J still refuses, but honestly.

## Why no test caught (A)

A probe that failed the arm whenever `v <> 0` was run against the whole Guest suite: 466 hits,
and **every one of them has `elementSize = 1`**. For a byte array the two units coincide, so no
active guest can tell them apart. The arm is also nearly unreachable from C#: `T*` arithmetic goes
through `conv.u` and becomes a byte view, and `Unsafe.Add` / `Unsafe.AddByteOffset` are handled as
intrinsics (`Intrinsics.fs:2608`, `:2688`) which already do the byte arithmetic correctly. What
reaches this arm is BCL internals — and hand-written IL.

## Decisions

### 1. How the array arm advances (defect A)

**1a. Fold whole cells; refuse a remainder.** `v % elementSize = 0` gives
`ArrayElement (arr, i + v / elementSize)`, anything else fails. Smallest change, and every result
stays a plain cell byref so a reference-holding array is never asked for a byte view. But it
refuses `&a[0] + 1` on an `int[]`, which is legal IL that CoreCLR answers.

**1b. Advance through the byte-view machinery, the same rule `Unsafe.AddByteOffset` already
uses.** Reinterpret as `System.Byte`, add `v`, and normalise with
`ByteOffsetNormalisationContext.withArrayElementSize arr elementSize` so a whole-cell advance
folds back into the cell index and a partial one stays a byte cursor. One rule shared with the
intrinsic rather than a second hand-rolled one; a reference-holding element is then refused where
it should be, at the *read*, by the existing byte-addressability rules rather than here.

**Chosen: 1b**, subject to one thing to verify first — that the arm can obtain the element's
size (it can: `ManagedPointerByteView.arrayElementSize`, already used by
`subtractArrayByteLocations` in the same file). If a byte-view result turns out to break an
existing guest that walks an `object[]` by whole cells, that is the signal to fall back to 1a.

### 2. The missing subtraction arms (defect B)

**2a. Add the two mixed arms**, mirroring the string precedent, computing through
`subtractArrayByteLocations` with a zero byte offset on the whole-element side.

**2b. Normalise in `decompose`** so a plain array byref presents as a zero-offset byte view and
one arm serves both. Fewer arms, but `decompose` is shared with `add`, which genuinely wants
`ArrayTarget` distinct so it can return a plain cell byref.

**Chosen: 2a.** This is completing a table, not extending a list: the other three storage
families already have all three representation pairs, and arrays are the only gap. 2b changes a
classifier shared with a consumer that does not want the change.

### 3. One PR or two

(A) is a behaviour change with no active guest observer and no C# expression; (B) is a gap in a
table. They are separable, and AGENTS.md asks for one feature per PR and for the dependency to
land first.

**3a. Two PRs, (A) then (B).** (A) is tested by a fabricated-IL guest (`FabricatedGuest.fs`, the
mechanism `TestFabricatedCpblk` and a dozen siblings use, which compares against the real
runtime) emitting `ldelema char; ldc.i4.4; add; ldind.u2` — the shape C# cannot spell, and the
only way to observe (A) without (B). (B) is then tested by the `Utf8.FromUtf16` guest above,
which needs both.

**3b. One PR.** Justified only by the interaction: shipping (B) first would be a regression from
"loud stop" to "silent wrong answer".

**Recommendation: 3a**, in that order, since (A)-then-(B) never passes through the dangerous
intermediate state. Confirm before starting.

## Tests

For (A), `TestFabricatedArrayByrefAdd.fs`: an emitted method that takes `char[]`, does
`ldelema; ldc.i4 4; add; ldind.u2`, and returns the character. Element 2 of `"abcde"` is `'c'`;
element 4 is `'e'`; the two are distinguishable, and the real runtime says which. A second case
over `int[]` with a non-whole-cell offset, to fix what 1b does with a remainder.

For (B), `sourcesPure/Utf8FromUtf16Span.cs`: the guest above, checking `charsRead`,
`bytesWritten` and the transcoded bytes, with the destination span starting partway into its
array so both operands are at non-zero indices.

Mutation, for (A): (m1) keep the element-denominated arithmetic — the fabricated guest must die;
(m2) divide by the wrong element size; (m3) drop the normalisation so a whole-cell advance stays
a byte view — check whether any guest can see it, and say so honestly if none can.

## Outcome of (A)

Landed as its own change, with three corrections found after the plan was written.

**The floor division and the overflow guard were both deleted, by mutation.** A mutant replacing
the arm's hand-rolled floor division with truncation survived: the byte-view normalisation the
result passes through already floors, so the arithmetic was unobservable. A later mutant
hardcoding the element stride also survived: the stride was reaching only a predictive int32
overflow guard, whose threshold was approximate (truncating where the advance floors) and
unobservable, sitting within one cell of `Int32.MaxValue`. Normalisation's own file is `Checked`
and traps at the right point with the right stride, so that trap is caught and named instead.

**The cursor is anchored on the element's shape, not `System.Byte`.** Codex found that
`ldarg.0; ldc.i4.1; add; ldc.i4.1; sub; ldind.ref` on a `string[]` refused: the mid-cell step made
the byref a byte cursor, the step back left the view in place at offset zero, and an object
reference cannot be read through a byte view. Anchoring on the element's shape is what
`Conv_I`/`Conv_U` already do, and for the same reason — a cell-aligned access then takes the
identity-preserving short-circuit.

**The plan's framing of decisions 1 and 2 was wrong about novelty.** A second opinion (Fable)
established, and this was then measured on `main`, that a cell-aligned byte cursor over an array
element is *not* a new representation: `Conv_U`/`Conv_I`, `Unsafe.As` and
`MemoryMarshal.GetArrayDataReference` all already produce it. So the two shapes that regress on
this branch —

* `(p + 1 - 1) - p` on an `int[]`, which is defect (B); and
* `*(p + 1 - 1)` on an `int*[]`, a whole-cell read of a pointer cell through a byte view

— are pre-existing consumer gaps that this change merely makes reachable from `ldelema; add;
sub`. Both are parked as guests (`MixedArrayByrefSubtraction.cs`,
`PointerArrayCellThroughByteView.cs`), each measured on `main` first; the second is stated
outright as future work in the prose of two guests that already exist. Consequently the
alternative of collapsing a cell-aligned cursor back to a bare byref, which was tempting as a
one-line fix for both symptoms, is wrong: it would hide the gaps for this one producer while
leaving them open for the three that already exist. It would also strip `Conv_U`'s anchor on the
*first* `p + 1` of a `fixed` block, not merely on a round trip.

## Remaining work

1. **(B), the two mixed subtraction arms for array roots.** This is what unblocks rung J, and it
   un-parks `MixedArrayByrefSubtraction.cs`.
2. **(C), `RuntimePointer` paired with a `NativeInt` template** in `readArrayBytesAs`'s whole-cell
   short-circuit, matching what `destinationNeedsWholeCellStore` already accepts on the write side
   and what a bare `ldind.i` already does. Un-parks `PointerArrayCellThroughByteView.cs`.
3. **(D), optional.** `IntrinsicHelpers.offsetManagedPointerByElements` refuses a bare array byref
   whose element size differs from `T`, which is the *only* thing `Conv_U`'s anchor buys. Making
   it total the way this arm now is would leave that anchor carrying no information, after which
   collapsing a cell-aligned cursor becomes the safe simplification it is not today.

## Ladder

To be filled in after (B): re-run `run-ladder.sh -o DIR RungJ` with `LADDER_FLAVOUR=linux`.
