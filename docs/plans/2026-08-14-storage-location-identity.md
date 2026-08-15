# Plan: one storage-location identity, shared by everything that asks "same location?"

Status: **all stages resolved.** Stage 1 merged (#982). Stage 2 merged (#984). Stage 3 withdrawn.
Stage 3′ merged (#987) — option (a), both keys canonicalised, and it fixed a live silent
data-corruption bug rather than only a modelling infelicity. The coarse-key half that this status
line previously flagged as untested is now covered: `TestStorageLocation.fs` asserts it at the
resolver, so reverting `SharedStorageKey.HeapObjectField` to a per-field key no longer leaves the
suite green. Stage 4 implemented (#1016), after two prerequisites that only became visible once it
was built (#992, #993 — both closed). §6's measurement is answered rather than pending.

What remains is not a stage of this plan: `AreSameProjectionCrossesArrayElement.cs` stays parked on
the unrelated byte-cursor gap of #729, and distinct-container inequality was deliberately left
undecided (see Stage 4) for a later change to justify on its own terms.

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
procedure, `private`, serving only `Memmove`/`Array.Copy` direction choice.

*(The line references in this table are as of the finding, before Stage 1. They now live in
`StorageLocation.fs`; the table is kept as the evidence the finding rested on rather than
rewritten to point at the code it caused.)*

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
  `Decided of bool | NeedsByteLocation of ManagedPointerSource * ManagedPointerSource`, and the
  handler call sites interpret it. Matches the gospel's "compute a description, then do it", keeps
  the partiality visible in the type, and only the one arm that needs state defers.

  **Cost: six sites, not the 27 first written here** (Codex; recounted). Direct
  `EvalStackValueComparisons.ceq` callers are `UnaryConstIlOp.fs:218, 281, 346, 409`,
  `NullaryIlOp.fs:1731` and `Intrinsics.fs:745` — six. The 27 came from a grep that counted
  `state.PointerHashState` reads rather than `ceq` calls. Two further propagation points call
  `ceqNormalised` directly rather than through `ceq`: `Unsafe.AreSame` (`Intrinsics.fs:2475`) and
  `NativeIntSourceComparison.fs:213`. And a **seventh** propagation point that is neither:
  `Interlocked.CompareExchange(ref IntPtr, ...)` at `Intrinsics.fs:810` calls
  `NativeIntSourceComparison.equalsForCli` directly, so if comparison starts returning
  `NeedsByteLocation` that CAS path must resolve or propagate it too. No `Unsafe.AreSame` guest
  reaches it, so it needs an oracle of its own — a guest doing an `Interlocked.CompareExchange`
  over a `ref IntPtr` whose operands are byrefs into one container.

  So the honest figure is **six `ceq` call sites, two direct `ceqNormalised` callers, and the CAS
  path** — still a materially cheaper change than this plan assumed throughout, but not the clean
  six. Enumerate rather than grep: the 27 above came from grepping, and so did missing this one.
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

### Stage 3′ — Make a field of a heap object a *view into* that object, not its own storage

**Dependencies**: Stage 2.

**Implements**: §2. This is the stage §2's hazard actually demands, and the first draft of this
plan omitted it. **Rewritten after reading the layer below `byteLocation`** — the first version of
this stage proposed refusing precision, which measurement shows is both unnecessary and worse.

**The premise of the first version was wrong: field-offset layout *is* carried here.**
`walkProjectionByteOffset`'s `Field` arm does `let fieldOffset, _ = CliType.getFieldLayoutById
field template`, bottoming out in `CliValueType.GetFieldLayoutById` (`CliType.fs:1942`), which
reads the field's real offset out of the value's field list — explicit `FieldOffset` included.
The recurring claim that "byref comparison does not carry field-offset layout" is true of
`ceqNormalised` (slot 32) and **false of `byteLocation`** (slot 88). That is the whole reason the
inversion in §4 is worth doing.

So the aliasing problem is not one problem but two, and only one of them is real:

| shape | what `byteLocation` does | verdict |
|---|---|---|
| *projection*: `Byref (root, [Field A])` vs `[Field B])` — explicit-layout **struct** | folds both to offset 0 of one container | **already correct**; degrades to `None` if the value is byte-backed, never answers wrongly |
| *root*: `ByrefRoot.HeapObjectField (addr, A)` vs `(addr, B)` — explicit-layout **class** | `ByteStorageIdentity.HeapObjectField (addr, field)` — **a distinct container per field**, offset 0 in each | **the defect**: consumers read distinct containers as disjoint |

The tree already knows this split. `AreSameExplicitLayoutOverlappingFields.cs` (projection) and
`AreSameHeapFieldsOverlappingExplicitLayout.cs` (root) are both parked, and the latter's comment
states it: *"this one is about roots being wrongly treated as disjoint storage, that one about
projections. A fix for one does not automatically cover the other."*

**So the fix is to canonicalise the container, not to refuse.** One heap object is one storage; a
field of it is a view into that storage at an offset. Map the root to
`(ByteStorageIdentity.HeapObject addr, offsetOfFieldWithinObject)`, and two fields of one object
become comparable by arithmetic — deciding the overlapping *and* the non-overlapping case
correctly, where refusal decides neither. This is the AGENTS.md guideline again:
`ByteStorageIdentity.HeapObjectField` currently asserts a disjointness it has no right to, and the
remedy is to make the classifier truthful rather than to make its consumers timid.

The two options, stated explicitly per AGENTS.md:

- **(a) Canonicalise.** As above. Preserves the most information, fixes both consumers at once
  (comparison *and* `Memmove` direction), and removes a DU case's false claim. Blast radius: every
  `ByteStorageIdentity.HeapObjectField` consumer, notably
  `NativeIntSource.SyntheticCrossArrayOffset`, which stores a `ByteStorageIdentity` as proof of
  byte-addressability. Needs the field's offset within the object, which `rootTemplate` currently
  discards (it calls `AllocatedNonArrayObject.DereferenceFieldById`, yielding the template but not
  the offset) — check whether a layout accessor exists there or must be added.

  **Both keys must be canonicalised, not just the precise one** (Codex, on this revision — a real
  hole in the first draft of it). `sharedStorageKeyOfRoot` maps the root to
  `SharedStorageKey.HeapObjectField (addr, field)` (`StorageLocation.fs:175`), which also carries
  the `FieldId`. Canonicalising only `ByteStorageIdentity` fixes nothing whenever precision is
  *unavailable*: `overlapVerdict` then compares coarse keys, finds two fields of one object
  unequal, and falls through to `CopyForwards` — exactly the silent-corruption path this stage
  exists to close, merely moved from the precise branch to the coarse one. So the coarse key for a
  heap-object field must become per-*object* too. Note that this widens what `Undecidable` covers,
  which is correct: two fields of one object genuinely might overlap.
- **(b) Refuse.** Resolve to `Located (coarse, None)` for a `HeapObjectField` root, so consumers
  degrade to `Undecidable` and fail loudly. Smallest possible change, trivially reversible, and
  consistent with what `ceqNormalised` just did. But it converts a wrong answer into a crash rather
  than into a right answer, and it leaves the parked heap guest parked.

**Recommend (a)**, with (b) available as a deliberate staging step if the blast radius in fact
turns out wide. Measure the consumer count before committing.

**A live defect on the `Memmove` surface, delegated separately.** #916 widened the *comparison*
surface to refuse this shape but did not touch `shouldCopyBackwards`, which still resolves two
fields of one object to distinct containers and copies **forwards**. That is the missed-surface
pattern this repo keeps hitting. Under investigation on its own branch; if it reproduces it is a
live wrong-direction copy on `main` today, and fixing it via (a) subsumes this stage.

**Correctness oracle**: *not* the `AreSame` unpark — that is a Stage 4 result and this stage cannot
reach it (Codex). `Unsafe.AreSame` calls `ceqNormalised` directly and does not consume
`StorageLocation.resolve` until the §4 inversion lands, so `AreSameHeapFieldsOverlappingExplicitLayout.cs`
stays parked however correct the canonicalisation is. Claiming it here would have made the stage
unsatisfiable in isolation. Instead:

- **Resolver-level assertions**, in the `TestStorageLocation.fs` style: two `HeapObjectField` roots
  on one object resolve to one `ByteStorageIdentity` and one `SharedStorageKey`, with offsets that
  differ exactly when the declared `FieldOffset`s differ.
- **The `Memmove` guest**, which *does* consume this — `shouldCopyBackwards` is a direct caller of
  `resolve`, so the direction change is observable end-to-end at this stage.

Mutation: reverting the precise container to per-field must fail the precise assertion and the
`Memmove` guest; reverting only the *coarse* key must still fail an assertion, or the coarse hole
above is untested.

### Stage 4 — Give byref comparison access to the resolution

**Status: implemented in #1016.** What follows is the reasoning that produced it, kept because
three of its conclusions were reached by falsifying an earlier draft and are worth not
rediscovering. Where the implementation diverged from the plan, it is marked inline; the
divergences are recorded at the end of the stage.

**Dependencies**: Stage 2, Stage 3′, and PR #916 (**merged** as `55b7d9b`; see below). Two further
prerequisites emerged only once the stage was built, and both are now discharged: #992 (field byte
offsets were not identity-grade, because `LayoutKind.Auto` and nominal alignment were unmodelled —
closed by #997, #1002, #1006, #1011) and #993 (the projection walk accumulated into an unchecked
int32 — closed by #1014). Neither was visible from the plan; both were found by a throwaway probe
of the stage, which is the argument for probing rather than planning further.

**Implements**: §4. This is the payoff, and it **changes behaviour** — it decides comparisons
currently refused. Its own PR, described as a behaviour change, not folded into a refactor.

**#916 merged as `55b7d9b`, and landed wider than this plan described.** The prerequisite is
discharged; `TestByrefComparison.fs` and the four `AreSame*` guests are on `main`. Two corrections
to what this plan assumed of it:

- It does **not** refuse only `ReinterpretAs`-then-`Field`. The merged `ceqNormalised` refuses four
  shapes: non-trailing `ReinterpretAs`; residuals that diverge at *different* fields
  (`tryDecideResiduals`' final arm); distinct roots where either byref may have left its root's
  extent; and two `HeapObjectField` roots on one object. So more of Stage 4's work is already done
  than expected — every shape it would decide now fails loudly rather than answering wrongly.
- **#916's squash commit message is stale relative to its own merged code.** It argues that
  diverging-field chains "still answer `false`, which is sound — … overlapping explicit layouts are
  stored byte-backed and so never carry `Field` projections to begin with." The merged code refuses
  them, and its comments record the opposite as *measured*: such values **stay field-backed**, and
  `Unsafe.AreSame(ref u.A, ref u.B)` was measured answering `false` here against `true` on real
  .NET. The message describes an earlier revision. Noted because it cost time here and will mislead
  a future bisector; the code and §2 agree, the message is the outlier.

Resolve the §4 (a)/(b) choice first. Then the refusal becomes a deferral, and the handler resolves
it:

- both sides `Located` with `Some` precise and **equal** container → decide by comparing offsets;
- `Unrelatable` on either side → decide unequal;
- anything with `None` precise → keep refusing;
- **distinct** containers → decide unequal **only when both byrefs are known to remain within
  their roots**. See below; this is the arm that is easy to get wrong.

**A precise offset is only meaningful relative to a root the byref has not left** (Codex — this
caught a real unsoundness in the rule as first written here). `ByteStorageIdentity.StackLocal
local0` with offset 1000 does not mean "1000 bytes into local0" if local0 is smaller than that; the
byref has walked out, and the container name is then a statement about how it was *built*, not
where it points. ECMA-335 promises no relative placement between two independently declared locals, so
`local0 + 1000` may well *be* `local1` — which is exactly why `TestByrefComparison.fs`'s
"an unbounded cursor on a local root is refused" exists. A naive distinct-container rule would
answer `false` there and silently regress a deliberate refusal into a possibly-wrong answer. One-
past-the-end `PeByteRange` byrefs have the same shape.

So the deferred result must carry the extent information — `mayLeaveRootExtent` already computes
it in `ManagedPointerSource.fs` — and the distinct-container inference must be gated on both sides
being in-extent.

**But `mayLeaveRootExtent` is not yet strong enough to be that gate** (Codex, round 3). For
`[ReinterpretAs Big; Field F]` it computes a known byte displacement of 0 and returns `false`
— "did not leave its root" — even when `F` lies beyond the original local or static slot, because
only `ByteOffset` steps contribute to the displacement it sums. That is sound *today* purely
because `hasNonTrailingReinterpret` refuses this shape earlier, so the predicate is never asked
about it. Stage 4 would defer the shape instead of refusing it, at which point the predicate's
answer becomes load-bearing and is wrong: two distinct precise containers, gate satisfied, silent
`false`.

This is the same defect shape as the two above — a classifier that is only accidentally truthful
because a caller upstream filters its hard cases. So Stage 4 must either extend the extent
description to account for a `Field` resolved against a reinterpreted (possibly larger) type, or
conservatively classify any chain containing a non-trailing `ReinterpretAs` as
possibly-out-of-extent. The conservative option keeps today's refusal for that shape, which is the
right default: it loses nothing relative to `main`.

#### The rule Stage 4 should actually be built on

Three review rounds have now each found a *different* counterexample to "distinct containers ⇒
distinct addresses": overlapping explicit-layout fields, byrefs displaced out of their root, and
`Field` resolved against a reinterpreted larger type. Three independent falsifications of one
shortcut is not three bugs to patch; it is evidence the shortcut is the wrong default.

**So invert Stage 4's default. Refuse unless the pair is positively proved equal or unequal, rather
than deciding unless a known exception fires.** Concretely, the only pairs it should decide are:

1. both `Located` with `Some` precise, **equal** container → compare offsets. Sound because one
   container means one flat coordinate system, which is the whole content of `ByteStorageIdentity`.
2. `Unrelatable` on either side → unequal. Sound because a non-byref shares storage with nothing.

Everything else refuses, including distinct containers. That is weaker than what this plan proposed
three times over, and it still decides all four parked guests — because canonicalisation (Stage 3′)
moves them into case 1 rather than relying on case-distinct reasoning. The distinct-container
inference buys only pairs that are *already* answered correctly by `ceqNormalised`'s final arm on
`main`, so declining to make it costs nothing and removes the entire class of error above.

If a later change wants distinct-container inequality, it should arrive with its own extent proof
and its own guests, as a separate decision. Note this does *not* affect `AreSameProjectionCrossesArrayElement`: array element
roots already resolve to one canonical `ByteStorageIdentity.Array arr` with `arrayBytePosition`
offsets, so that pair is an *equal*-container comparison and never reaches this arm. The gating
matters for roots that stay distinct after canonicalisation — locals, arguments, statics, PE ranges.

**Correctness oracle**: the four guests #916 parks — `AreSameExplicitLayoutOverlappingFields.cs`,
`AreSameHeapFieldsOverlappingExplicitLayout.cs`, `AreSameFirstFieldVersusReinterpretedWhole.cs`,
`AreSameProjectionCrossesArrayElement.cs`. Each should move from refused to decided, with the
expected answer taken from real .NET (they are parked, so the harness runs them there and nowhere
else; see `park-a-test-to-validate-its-oracle`). Mutation-check both directions as #916 did:
disabling the new decision must fail only the newly-decided tests, and widening it past the
`None`-precise case must fail the containment tests.

**Those four are not a sufficient oracle on their own** (Codex). All four expect `true`, so an
implementation that called any two precise locations in one `ByteStorageIdentity` equal — ignoring
their offsets entirely — would pass every one of them, while misreporting every non-overlapping
pair of fields. The oracle needs at least one **same-container, different-offset** case whose
answer is `false`.

Conveniently one already exists and needs no exotic type: `Unsafe.AreSame(ref s.X, ref s.Y)` on an
ordinary **sequential** two-field struct is *currently refused*, because both residuals contain a
`Field` and `tryDecideResiduals` falls to its final arm. Real .NET answers `false`. So it is a new
decision (not a pre-existing pass that could go green vacuously), it is the direct negative of the
explicit-layout guest, and the pair of them together pins that offsets are actually compared.

Worth noting what that implies for §6's payoff figure: the refusal is not confined to exotic
layouts. Comparing byrefs to two different fields of *any* struct is refused today. The four parked
guests are what the corpus happens to contain, not the extent of the shape.

Note what each of the four needs, since they do not all fall to the same change and the stage
should not be declared done on a subset:

| guest | needs | outcome in #1016 |
|---|---|---|
| `AreSameFirstFieldVersusReinterpretedWhole` | the inversion alone — `[Field X]` folds to X's offset and the bare chain to 0, over a sequential struct that is certainly field-backed | un-parked, passes |
| `AreSameProjectionCrossesArrayElement` | the inversion alone — two `ArrayElement` roots already resolve to one `ByteStorageIdentity.Array` with `arrayBytePosition` offsets | **still parked — this row was wrong**, see below |
| `AreSameExplicitLayoutOverlappingFields` | the inversion alone — see the storage check below | un-parked, passes |
| `AreSameHeapFieldsOverlappingExplicitLayout` | Stage 3′ option (a): the canonical per-object container | un-parked, passes |

Three of the four therefore fall out of the inversion alone; only the heap-root case needs Stage 3′.

**That prediction was right about the comparison and wrong about the guest.** The reasoning for
`AreSameProjectionCrossesArrayElement` holds exactly as written — the comparison no longer blocks
it. But the guest still fails, and strictly *earlier* than before: at IL offset 71, in the byte-view
*read* that builds one of the operands, with "byte-view read at offset 8 for 1 bytes does not fit in
single primitive cell of size 8". That is the byte-cursor gap of #729 — a cursor may not leave the
cell it started in — and has nothing to do with byref identity. The table row reasoned about the
one dependency it was looking at and concluded the guest would pass, which does not follow: a guest
passes only when *every* gap on its path is closed. Un-park it when a byte view can cross out of its
originating cell.

**Why the struct case is safe, since the obvious objection is that it is not.** The fold only works
if the value is field-backed: `CliValueType.GetFieldLayoutById` goes through `FindFieldById` →
`FieldStorage`, which **fails** for `CliValueTypeStorage.RawBytes`, so a byte-backed value would
degrade to `None` and stay refused. And `BulkMoveAcrossOverlappedStructPadding.cs` says, as
measured, that *"an explicit-layout struct with any overlap is stored byte-backed"* — which would
sink it.

It does not, because that sentence is about a different mechanism. `RawBytes` has exactly one
origin, `CliValueType.StorageFromFields` (`CliType.fs:1214`):

```fsharp
match fields, layout with
| [], Layout.Custom (size = size) when size > 0 -> CliValueTypeStorage.RawBytes (...)
| _ -> CliValueTypeStorage.Fields { ... }
```

The `RawBytes` arm requires an **empty field list**. Every other `RawBytes` occurrence in
`CliType.fs` copies, zeroes or updates storage that already exists. A struct declaring two `int`
fields is therefore `Fields`-backed however they overlap, and `GetFieldLayoutById` returns 0 for
both. The "byte-backed" in that parked comment refers to the reference-containing path that fails
in `CliType.OfBytesLike` — a distinct notion sharing a name. Worth flagging: "byte-backed" is used
in this codebase for at least two different things, and conflating them is what nearly cost this
stage a guest.

#### What #1016 actually built

The inversion shipped as written. `ManagedPointerSource.ceqNormalisedDeferred` returns a
`CeqOutcome` — `Decided of bool`, or `NeedsByteLocation` carrying both byrefs and the diagnostic the
refusal would have raised — and `StorageLocation.resolveCeq` interprets it, deciding only cases 1
and 2 above and re-raising the diagnostic for everything else. Carrying the diagnostic is what makes
the deferral free: a caller that cannot resolve reproduces the previous failure verbatim rather than
inventing a worse one. Seven propagation points, one widened signature (`UnaryConstIlOp.execute`
takes `baseClassTypes`), one line of ripple in `AbstractMachine`.

Three divergences from the plan, none of them design changes:

1. **The call-site count.** An earlier draft of this plan said 27; the true number is six, plus the
   `Interlocked.CompareExchange` path that reaches comparison through
   `NativeIntSourceComparison.equalsForCli` rather than through `ceq`. Threading turned out to be
   mechanical, which is what the probe was run to find out.
2. **`AreSameProjectionCrossesArrayElement` did not un-park**, per the correction above. The stage
   is therefore declared done on three of the four guests it named — legitimately, since the fourth
   is blocked on an unrelated gap and not on anything this stage owns.
3. **The negative oracle needed a new guest after all.** This plan said one "already exists" for the
   sequential two-field struct. It did not: no guest in the corpus compared byrefs to two distinct
   fields of an ordinary struct, precisely because the shape was refused and so nothing could assert
   it. `AreSameSequentialStructDistinctFields.cs` is that guest, written for this stage.
   `AreSameByteOffsetsSpanningInt32.cs` is a second negative, at the int32 boundary, and exists
   because #993's fix and this stage only compose: while comparison refused every chain needing
   field offsets, no guest could observe the accumulator's width through `AreSame` at all.

Both mutations this stage asked for were run. Dropping the offset comparison from `resolveCeq` fails
the two negatives with exit code 1 and leaves all three overlapping guests green — which is the
failure mode Codex predicted and the reason the negatives were required. Reverting the projection
walk to a wrapping int32 fails `AreSameByteOffsetsSpanningInt32.cs` alone.

## 6. What would falsify this plan

- **If `byteLocation` returns `Unrelatable`/`None` for most real byref pairs**, Stage 4 unblocks
  nothing and the whole stack is churn.

  **Measured, and the instrumentation this bullet asked for is unnecessary.** Since #916 merged,
  every shape Stage 4 would decide `failwith`s. The suite is green at 3027. Therefore the frequency
  of such shapes across all active guests is **exactly zero, by construction** — a counter would
  only re-derive that, because a non-zero count would be a red suite. So Stage 4's payoff is *not*
  "the suite gets more correct"; it is precisely **the four parked `AreSame*` guests**, three of
  which fall out of the inversion alone (see the Stage 4 table).

  **Settled by #1016, and the answer is "three, not four".** `AreSameProjectionCrossesArrayElement`
  is blocked on the unrelated byte-cursor gap of #729, so the realised payoff is one guest smaller
  than this bullet forecast. Against that, two negative-control guests had to be *written* for the
  stage rather than un-parked, which is itself the endogeneity argument below made concrete: they
  could not have existed earlier, because the shapes they assert were refused.

  Whether that falsifies the plan is a judgement call, and worth stating plainly rather than
  burying: four guests is a modest return for an inversion. Two things argue it is
  still worth doing. First, the alternative to deciding these shapes is not "answer them cheaply
  elsewhere" — it is leaving four known-divergent behaviours permanently refused, since
  `ceqNormalised` at slot 32 provably cannot see the layout and no smaller change reaches it.
  Second, zero-frequency-on-the-suite is a statement about the *guest corpus*, not about real
  programs: the corpus contains what has been written, and these shapes are refused precisely
  because nobody could write a passing guest that used them. The count is endogenous. Concretely:
  `Unsafe.AreSame(ref s.X, ref s.Y)` on an ordinary sequential struct — no explicit layout, no
  reinterpretation — is refused today. The shape is not exotic; the *corpus* is.

  And the recount above cuts the other way on cost: six call sites plus two direct callers is a
  small change, so the return does not need to be large to justify it. What would genuinely
  falsify it is if `Decided | NeedsByteLocation` cannot be threaded without restructuring the
  handlers — cheap to test on two sites before committing, and still the first thing Stage 4
  should do. **That test was run, and threading is mechanical**: one widened signature and a
  one-line ripple, no handler restructured. The probe that established it also surfaced #992 and
  #993, neither of which any amount of further planning would have found — the plan had reviewed
  three rounds of Codex without either being noticed.
- **If Stage 3′ finds cross-field aliasing is unreachable in practice**, Stage 4's distinct-container
  rule needs no guard and Stage 3′ collapses to a documentation note. **Resolved: it is reachable**,
  and the tree records it as measured on both runtimes — see the merged `tryDecideResiduals` and
  `HeapObjectField` comments in `ManagedPointerSource.fs`, and the two parked guests. Stage 3′ does
  not collapse; it narrows to the *root* case, the projection case already being correct.

Two risks I raised while drafting are **already resolved**, checked at `53fa6ad`:

- *Stage 1 might not be a pure move.* It is: the dependency cluster closes at `rootTemplate`,
  whose sole use is `tryProjectionByteOffset` (see the Stage 1 table).
- *The `ceq` call sites might not have state in hand.* They do — every one reads
  `state.PointerHashState` to build the argument it already passes
  (`UnaryConstIlOp.fs:218, 281, 346, 409`, …), so `state` is in scope and option (a) closes.
