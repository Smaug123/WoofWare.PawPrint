# Plan: one storage-location identity, shared by everything that asks "same location?"

Status: **in progress.** Stage 1 implemented (`storage-location-hoist`, 3023 passing). Stages 2,
3′ and 4 proposed; Stage 3 withdrawn.

Reviewed by Codex, which found four defects in the first draft. All four were confirmed against
the tree and all four changed the plan: §5 Stage 2's type shape, the withdrawal of Stage 3, the
addition of Stage 3′, and #916 becoming a hard prerequisite of Stage 4. Each is recorded inline
where it applies rather than in a changelog, so a reader of the stage sees the reasoning.

Goal: give the question *"do these two byrefs name the same storage?"* a single implementation,
and make its unavoidable partiality explicit in a type rather than in each caller's fallback path.

Implement this plan with each stage on its own branch, stacked as necessary on previous branches,
so that a reviewer can review each branch in isolation.

## 1. The finding: the answer already exists, private to the wrong file

This is not a new abstraction. `CellAwareMemOps.fs` already contains the honest decision
procedure, `private`, serving only `Memmove`/`Array.Copy` direction choice:

| what | where | what it answers |
|---|---|---|
| `byteLocation` | `CellAwareMemOps.fs:141-188` | precise: `(ByteStorageIdentity * int64) option` — container plus flat byte offset, `None` when it cannot resolve |
| `SharedStorageKey` / `sharedStorageKeyOfRoot` | `:205-230` | coarse: could these two share storage at all? |
| `shouldCopyBackwards` | `:239+` | the consumer, using the three-way structure: resolved-and-same → compute overlap; resolved-and-different → disjoint; unresolved → fail loud |

Meanwhile a *second*, structurally different answer to nearly the same question lives at
`ManagedPointerSource.ceqNormalised` (`:1066`): structural equality of normalised projection
chains, refusing any residual that contains a `Field`. It is shared by `ceq` on byrefs
(`EvalStackValueComparisons.fs:468`), `NativeIntSource` pointer comparison
(`NativeIntSourceComparison.fs:213`), and `Unsafe.AreSame`.

Three things say this is the right change, and none of them is the race detector:

1. **PR #916 is open and blocked on exactly this.** Its own body: *"the real answer is
   layout-aware byte-offset byref identity. That cannot be a total rule while reference- and
   pointer-containing values remain byte-imageless, so it is its own change."* That is a
   description of `byteLocation`, which already exists.
2. **`docs/developer/pointers-and-byte-representations.md` already prescribes it** — *"code
   computes deterministic byte positions from the storage identity and the in-storage byte
   offset"*, and *"APIs that compare byrefs structurally should require
   `NormalisedManagedPointerSource` … so equivalent byte locations use one canonical
   representation."* The call site at `EvalStackValueComparisons.fs:468` instead calls
   `unsafeAssumeNormalisedForComparison`, whose name records that the prescription is being
   violated.
3. **The recurring-finding pattern.** The "widen all four byref surfaces together" family of
   defects recurs because no single representation answers the question the surfaces keep asking.

## 2. Why a total key is impossible, and why that must be preserved

Two invariants were asserted and then disproved on #916's branch (by Codex, measured on both
runtimes):

- overlapping explicit layouts do **not** collapse to byte ranges — they stay field-backed;
- distinct fields do **not** occupy disjoint extents under explicit layout.

So `HeapObjectField (addr, A)` and `HeapObjectField (addr, B)` can be two keys naming one
location. Together with byte-imageless reference-containing storage, no total injective key
exists. The three-way outcome is therefore the honest object, not a limitation to be engineered
away.

**This is the main hazard of the whole plan.** A key that presents itself as total would convert
#916's loud refusals into silent wrong answers — the exact trade this project inverts. Every stage
below must keep "undecidable" a first-class outcome.

## 3. Out of scope, recorded so it is not rediscovered

**The write-elision family is not part of this.** `IlMachineManagedByref.fs` elides a store when
the new value equals the old one, at 13 sites (nine `ReferenceEquals` shortcuts, plus
`isProvableNoOpWrite` at `:299` called from `:2476, :2727, :2789, :2897`). An earlier framing of
this work lumped them in; that was wrong. They ask *"is the value I am about to write equal to the
value already there?"* at an **already-known** location. That is value equality, not location
identity, and they need no location key. (They do matter to a race detector, because a write
elided above the heap chokepoint is invisible to any write-observing instrumentation — but that is
a different problem.)

**Making `stfld`/`stelem` name locations is not part of this.** `ldflda` builds
`ByrefRoot.HeapObjectField (addr, fieldId)` (`UnaryMetadataFieldOps.fs:467`) while `stfld` passes
`addr, fieldId` naked to `ManagedHeap.setFieldById` (`:220`) — taking a field's address gives it a
first-class identity, storing to it does not. Closing that asymmetry pays off only for a consumer
that folds over accesses, so it is deferred under the standing "no new functionality" constraint.

## 4. The obstacle that shapes the stages: compile order

`byteLocation` needs `IlMachineState`, `IlMachineManagedByref.walkProjectionByteOffset` and
`ManagedPointerByteView`. The comparison that should consume it is *earlier in the build*:

| file | fsproj slot |
|---|---|
| `ManagedPointerSource.fs` (`ceqNormalised`) | 32 |
| `NativeIntSourceComparison.fs` (caller) | 35 |
| `EvalStackValueComparisons.fs` (caller) | 58 |
| `IlMachineStateModel.fs` (`IlMachineState`) | 71 |
| `IlMachineManagedByref.fs` | 77 |
| `ManagedPointerByteView.fs` | 87 |
| `CellAwareMemOps.fs` (`byteLocation`) | 90 |

So Stage 4 is **not a move; it is an inversion.** A function at slot 32 cannot call one that needs
a type from slot 71. Both of `ceqNormalised`'s callers are also too early, so the resolution has to
reach the opcode handlers — `UnaryConstIlOp.fs` (20 `ceq` call sites), `NullaryIlOp.fs` (5),
`Intrinsics.fs` (2) — which do have state.

Two genuinely different ways to bridge that, to be decided before Stage 4 is written:

- **(a) Dependency rejection.** `ceq`'s byref arm returns a description rather than a verdict:
  `Decided of bool | NeedsByteLocation of ManagedPointerSource * ManagedPointerSource`, and the 27
  handler call sites interpret it. Matches the gospel's "compute a description, then do it", keeps
  the partiality visible in the type, and only the one arm that needs state defers. Cost: 27 call
  sites change shape.
- **(b) Eager resolution at the call sites.** Handlers resolve both operands to a
  `LocationResolution` *before* calling `ceq`, which takes them as parameters — mirroring how
  `ceq` is already handed `counters : PointerHashState`. Cost: resolution work on every `ceq`
  including the overwhelming majority that compare integers, and `byteLocation` is not cheap
  (it walks projections and materialises type templates).

Recommend **(a)**: (b) pays the resolution cost unconditionally for a branch that is rarely taken,
and the existing `counters` precedent is not really analogous — `PointerHashState` is a small early
data value, not the whole machine state.

## 5. Stages

### Stage 1 — Hoist the location machinery into its own file

**Dependencies**: none.

**Implements**: §1.

Move the cluster out of `CellAwareMemOps.fs` into a new `StorageLocation.fs`, slotted after
`ManagedPointerByteView.fs` (87) and before `CellAwareMemOps.fs` (90), and make it public to the
library. The move set is larger than just `byteLocation` — verified closed at `53fa6ad`:

| member | line | why it comes too |
|---|---|---|
| `rootTemplate` | `:80` | `byteLocation`'s only transitive dependency; sole use is `:133` |
| `tryProjectionByteOffset` | `:123` | wraps `rootTemplate` + `walkProjectionByteOffset` |
| `byteLocation` | `:140` | the precise resolver |
| `SharedStorageKey` | `:205` | the coarse key |
| `sharedStorageKeyOfRoot` / `sharedStorageKey` | `:213` / `:234` | its constructors |

No logic change. `ByteStorageIdentity` stays where it is (slot 32) since its early consumers —
`BinaryArithmetic.fs`, `NativeIntSource.fs` — need it there.

**Correctness oracle**: a pure move. Verify the moved bodies with
`diff <(git show origin/main:WoofWare.PawPrint/CellAwareMemOps.fs | sed -n '141,230p') …` rather
than by eye, per the technique in `fsharp-private-is-assembly-scoped`. Full suite passes
(baseline 2592 at #916's measurement; re-baseline first).

### Stage 2 — Make the outcome a truthful three-way type

**Dependencies**: Stage 1.

**Implements**: §2.

Today the partiality is encoded as `option` plus a separate fallback call, with
`shouldCopyBackwards` interpreting the combination. Replace with one function returning

```fsharp
type LocationResolution =
    /// Not a byref (`Null`, `NativeIntPlaceholder`): cannot share storage with anything.
    | Unrelatable
    /// A byref. `Coarse` is always available and is what two resolutions degrade to when
    /// either lacks a precise offset; `Precise` adds the flat byte coordinate when the
    /// projection chain resolves, making overlap decidable by arithmetic.
    | Located of coarse : SharedStorageKey * precise : (ByteStorageIdentity * int64) option
```

**Both fields, not a three-way choice.** The first draft of this plan made `Exact` and
`SameStorageUnknownOffset` alternatives. Codex showed that breaks behaviour: `shouldCopyBackwards`
degrades to the coarse key *pairwise* — when **either** side fails to resolve, **both** fall back —
so a resolution that has dropped its coarse key cannot be compared against one that only has a
coarse key. Concretely, `Exact (ByteStorageIdentity.Array arr, off)` has lost the element index
that `SharedStorageKey.ArrayCell (arr, index)` carries, so the pair is incomparable and the
consumer must either call a possibly-aliasing move disjoint or reject unrelated moves. Keeping
`Coarse` populated on every byref preserves the existing degradation exactly.

This is the AGENTS.md guideline — *"if callers use a classifier to justify a later operation, keep
that classifier's contract truthful and load-bearing"* — applied to a classifier that currently
spreads its contract across two functions and a caller's `match`.

**Correctness oracle**: property test over generated `ManagedPointerSource` pairs — the new
classification agrees with the old two-call pattern, keeping the pre-move private functions in the
test file as the reference implementation. The existing `Memmove` overlap tests must be unchanged;
if any of them change, the move was not behaviour-preserving.

### Stage 3 — **Dropped.** Do not collapse `SharedStorageKey` into `ByteStorageIdentity`

The first draft proposed merging the two vocabularies, since `SharedStorageKey.HeapValue addr` ≡
`ByteStorageIdentity.HeapObject addr`, `HeapObjectField` is duplicated verbatim, and `ArrayCell` /
`StringChar` are `Array` / `String` plus an index. The duplication is real, but merging is
actively harmful and the stage is withdrawn.

`ByteStorageIdentity` is not merely a naming vocabulary: it is the **proof that a container has
byte coordinates at all**, and downstream arithmetic consumes it as such —
`NativeIntSource.SyntheticCrossArrayOffset` stores one as each of `_TargetRoot` and `_SourceRoot`
(`NativeIntSource.fs:11,13`) to manufacture a deterministic cross-storage byte distance.
`SharedStorageKey.RuntimeTypeAux` is explicitly byte-imageless — the `ExposedClassObject` cell is a
single object reference, which is why `byteLocation` returns `None` for it. Adding it to
`ByteStorageIdentity` would make `SyntheticCrossArrayOffset` representable over a container with no
byte coordinates, and would make `Located (_, Some (RuntimeTypeAux, offset))` representable too.

That is a "make illegal states unrepresentable" regression traded for removing two duplicated DU
cases. The two types are deliberately different widths and should stay distinct: `SharedStorageKey`
is the wider "could these share storage", `ByteStorageIdentity` the narrower "this storage has byte
coordinates". Record that relationship in both types' doc comments; do not unify them.

### Stage 3′ — Classify cross-field aliasing before any consumer trusts a precise location

**Dependencies**: Stage 2.

**Implements**: §2. This is the stage §2's hazard actually demands, and the first draft of this
plan omitted it.

`byteLocation` resolves a bare `HeapObjectField` root without consulting layout, so two
explicit-layout fields `A` and `B` that occupy the *same address* resolve to
`Some (HeapObjectField (addr, A), 0)` and `Some (HeapObjectField (addr, B), 0)` — **different
containers**, which every consumer reads as "disjoint, no overlap possible". That is precisely the
aliasing §2 says cannot be decided, being silently decided the unsafe way.

So `Located` must not advertise a precise offset for a field-rooted byref whose declaring type has
explicit layout, unless the layout proves the fields disjoint. Conservative classification:
resolve to `Located (coarse, None)` — same-storage-unknown-offset — for two field roots on one
object under `LayoutKind.Explicit`, so consumers refuse rather than assume.

**Suspected pre-existing defect, to be confirmed separately.** On `origin/main`,
`shouldCopyBackwards` already takes the distinct-containers branch for this shape and returns
`false` (copy forwards). If two overlapping explicit-layout fields can be the endpoints of one
`Buffer.Memmove`, that is a live wrong-direction copy today, independent of this plan. Do not fold
the fix into this stage: write the guest, confirm it against real .NET, and file it. If it is
*not* reachable, say why in the issue — that answer determines whether this stage is a correctness
fix or only a guard.

**Correctness oracle**: a guest with two `[FieldOffset(0)]` fields of one type, byrefs taken to
both, compared and `Memmove`d. Under `origin/main` the comparison decides; after this stage it
refuses. Mutation: removing the explicit-layout check must fail exactly that test.

### Stage 4 — Give byref comparison access to the resolution

**Dependencies**: Stage 2, Stage 3′, **and PR #916**. See below — this is not optional.

**Implements**: §4. This is the payoff, and it **changes behaviour** — it decides comparisons
currently refused. Its own PR, described as a behaviour change, not folded into a refactor.

**#916 is a hard prerequisite, and the first draft wrongly listed only Stage 2.** Two things this
stage's oracle needs exist only on the unmerged `aresame-refuse-undecidable` branch:

- `WoofWare.PawPrint.Test/TestByrefComparison.fs` is **absent from `origin/main`** — verified at
  `53fa6ad`. The first draft cited it as the oracle; it was read out of #916's PR description, not
  the tree.
- At this merge base `ceqNormalised` refuses only `ReinterpretAs`-then-`Field`. A *plain* field
  residual is still compared structurally and can return `false` outright, as the parked
  `AreSameFirstFieldVersusReinterpretedWhole.cs` shows. So the "deferral" this stage converts is
  not yet a refusal — on `origin/main` it is a silent wrong answer, and #916 is what makes it
  loud first.

Either land #916 first and stack on it, or fold its refusal into this stage — but the dependency
must be explicit either way. Recommend the former: #916 is reviewed and its "cost: zero, 2592
passing" measurement is worth keeping as a separate bisection point.

Resolve the §4 (a)/(b) choice first. Then the refusal becomes a deferral, and the handler resolves
it: `Located` on both sides with `Some` precise on each and equal container decides by comparing
offsets; distinct containers decide unequal **only when Stage 3′ has ruled out cross-field
aliasing**; `Unrelatable` decides unequal; anything with `None` precise keeps refusing.

**Correctness oracle**: the three guests #916 parks — each should move from refused to decided,
with the expected answer taken from real .NET (they are parked, so the harness runs them there and
nowhere else; see `park-a-test-to-validate-its-oracle`). Mutation-check both directions as #916
did: disabling the new decision must fail only the newly-decided tests, and widening it past the
`None`-precise case must fail the containment tests.

## 6. What would falsify this plan

- **If `byteLocation` returns `Unrelatable`/`None` for most real byref pairs**, Stage 4 unblocks
  nothing and the whole stack is churn. Measure first: instrument `byteLocation` on the existing
  suite and count the three outcomes before writing Stage 4. #916's "cost: zero, 2592 passing"
  note suggests the shapes reaching these predicates are far narrower than their input types
  admit, which cuts both ways.
- **If Stage 3′ finds cross-field aliasing is unreachable in practice**, Stage 4's distinct-container
  rule needs no guard and Stage 3′ collapses to a documentation note. Settle it with the guest, not
  by argument.

Two risks I raised while drafting are **already resolved**, checked at `53fa6ad`:

- *Stage 1 might not be a pure move.* It is: the dependency cluster closes at `rootTemplate`,
  whose sole use is `tryProjectionByteOffset` (see the Stage 1 table).
- *The `ceq` call sites might not have state in hand.* They do — every one reads
  `state.PointerHashState` to build the argument it already passes
  (`UnaryConstIlOp.fs:218, 281, 346, 409`, …), so `state` is in scope and option (a) closes.
