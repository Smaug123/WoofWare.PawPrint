# Plan: one storage-location identity, shared by everything that asks "same location?"

Status: **proposed.** Nothing implemented.

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
    /// Container and flat byte offset both known: overlap is decidable by arithmetic.
    | Exact of ByteStorageIdentity * int64
    /// Same container provable, offset within it not: overlap is undecidable, callers must
    /// fail loud rather than guess.
    | SameStorageUnknownOffset of SharedStorageKey
    /// No shared storage possible.
    | Unrelatable
```

This is the AGENTS.md guideline — *"if callers use a classifier to justify a later operation, keep
that classifier's contract truthful and load-bearing"* — applied to a classifier that currently
spreads its contract across two functions and a caller's `match`.

**Correctness oracle**: property test over generated `ManagedPointerSource` pairs — the new
classification agrees with the old two-call pattern, keeping the pre-move private functions in the
test file as the reference implementation. The existing `Memmove` overlap tests must be unchanged;
if any of them change, the move was not behaviour-preserving.

### Stage 3 — Collapse `SharedStorageKey` into `ByteStorageIdentity`

**Dependencies**: Stage 2. Parallel with Stage 4.

**Implements**: §1 (the duplication half).

The two vocabularies overlap: `SharedStorageKey.HeapValue addr` ≡
`ByteStorageIdentity.HeapObject addr`; `HeapObjectField` is duplicated verbatim; `ArrayCell` and
`StringChar` are `Array`/`String` plus an index refinement; and `RuntimeTypeAux` has **no**
`ByteStorageIdentity` counterpart (`ByteStorageIdentity` cannot name the `ExposedClassObject`
cell). Express the refinement once and add the missing case.

**Correctness oracle**: property — the collapsed key induces the *same equivalence relation* as the
old pair, i.e. for generated pointers `p`, `q`: `newKey p = newKey q` iff
`oldKey p = oldKey q`. Equivalence-relation equality is the property that actually matters here;
key-by-key structural equality would be a stronger claim than the consumers need and would
fail spuriously on the added case.

### Stage 4 — Give byref comparison access to the resolution

**Dependencies**: Stage 2. Parallel with Stage 3.

**Implements**: §4. This is the payoff, and it **changes behaviour** — it decides comparisons that
#916 currently refuses. It must be its own PR, described as a behaviour change, not folded into a
refactor.

Resolve the §4 (a)/(b) choice first. Then `ceqNormalised`'s field-bearing-residual refusal becomes
a deferral, and the handler resolves it via `LocationResolution`: `Exact` on both sides with equal
container and offset decides equal; equal container and differing offset decides unequal;
`Unrelatable` pairs decide unequal; `SameStorageUnknownOffset` keeps refusing.

**Correctness oracle**: the three guests #916 parked in `TestByrefComparison.fs` — each should move
from refused to decided, with the expected answer taken from real .NET (they are parked, so the
harness already runs them there and nowhere else; see `park-a-test-to-validate-its-oracle`).
Mutation-check in both directions as #916 did: disabling the new decision must fail only the
newly-decided tests, and widening it past `SameStorageUnknownOffset` must fail the containment
tests. Add an explicit case for two `HeapObjectField` roots on one object under explicit layout —
per §2 that pair must stay refused, and it is the one a careless widening would break.

## 6. What would falsify this plan

- **If `byteLocation` returns `Unrelatable`/`None` for most real byref pairs**, Stage 4 unblocks
  nothing and the whole stack is churn. Measure first: instrument `byteLocation` on the existing
  suite and count the three outcomes before writing Stage 4. #916's "cost: zero, 2592 passing"
  note suggests the shapes reaching these predicates are far narrower than their input types
  admit, which cuts both ways.
Two risks I raised while drafting are **already resolved**, checked at `53fa6ad`:

- *Stage 1 might not be a pure move.* It is: the dependency cluster closes at `rootTemplate`,
  whose sole use is `tryProjectionByteOffset` (see the Stage 1 table).
- *The `ceq` call sites might not have state in hand.* They do — every one reads
  `state.PointerHashState` to build the argument it already passes
  (`UnaryConstIlOp.fs:218, 281, 346, 409`, …), so `state` is in scope and option (a) closes.
