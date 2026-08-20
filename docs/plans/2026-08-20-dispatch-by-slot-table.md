# Dispatch by slot table

## The defect

`VirtualSlotLayout` answers **slot identity** -- which slot number a declaration owns -- and says
so explicitly: "MethodImpls are deliberately not consulted ... a MethodImpl overwrites a slot's
implementation but not the slot number its body was declared at, so it belongs to slot *content*
-- dispatch ... rather than to slot identity."

Dispatch needs content. PawPrint has no content table, so #1084 approximated one with a set of
declarations known to name the slot. Six review rounds and seven defects later it is correct on
the shapes it accepts and *declines* five ways, and `VirtualDispatchGenericDefinitionSlots.cs`
stays parked.

## What upstream actually does

Content is built in two phases at two different load levels, not one.

**Phase A, `BuildMethodTable`** (methodtablebuilder.cpp:1649-1697), against the parent's phase-A
table:

1. `CopyParentVtable` (:1144, called :1649) -- inherit the parent's slots; indices are
   prefix-stable.
2. `PlaceVirtualMethods` (:5366-5482) -- each declared instance virtual either replaces the slot
   it matches or takes the next free one. This is the identity table PawPrint already builds, and
   it also assigns each method the slot `MethodDesc::GetSlot()` returns: for a non-newslot
   override that is *the overridden parent slot* (`SetVirtualMethodOverride`,
   methodtablebuilder.h:1512-1517), not a fresh one.
3. `PlaceMethodImpls` (:6409-6544), which runs *after* (2): `content[slotOf(Declaration)] :=
   Body`. Not last-write-wins -- a second MethodImpl on one slot with a *different* body is the
   load error `IDS_CLASSLOAD_MI_MULTIPLEOVERRIDES` (:6335-6345); the same body twice is tolerated.
   Virtual statics are diverted before this (:5749-5758, :6445-6446), and only a decl naming this
   class or an ancestor writes the vtable at all -- an interface decl writes the dispatch map
   (`AddMethodImplDispatchMapping`, :6363-6366).
4. `SetupMethodTable2`'s unification fixed point (:11334-11385), skipped for interfaces (:11318):

       do { changed = false
            for i ascending:
              let m = content[i]
              if slotOf(m) <> i && content[slotOf(m)] <> m
                then content[i] := content[slotOf(m)]; changed = true }
       while changed

   Upstream's own comment: "MethodImpl came to mean 'unify the slots of methods A and B'". The
   iteration is genuinely needed -- its worked example at :11337 takes two passes.

**Phase B, `CLASS_LOAD_EXACTPARENTS`** (class.cpp:1158-1163), against the parent's phase-B table,
and gated on `HasVTableMethodImpl` -- set by a class-decl MethodImpl on this type (:5943) and
*inherited* from the parent class (:5737-5738), so it means "this type or an ancestor has one":

5. `CopyExactParentSlots` (:9468-9536) -- for each slot below `GetNumParentVirtuals`, if the
   occupant is not one of this type's own methods (`pMD->GetMethodTable() == pMT` skips locally
   owned ones), re-copy it from the exact parent's *phase-B* table.
6. `PropagateCovariantReturnMethodImplSlots` (class.cpp:1426-1546) -- skipped for interfaces and
   value types. For each slot `i` below the parent's `GetNumVirtuals` whose occupant differs from
   the parent's, where that occupant `IsMethodImpl` (:1499-1501) and some occupant of slot `i`
   at-or-above this type in the chain is a MethodImpl carrying `[PreserveBaseOverrides]`
   (:1509-1515): for `j` in `[i, parentNumVirtuals)`, if `content[j]` is the parent's occupant of
   slot `i`, set `content[j] := content[i]`.

Mirroring both phases rather than folding them into one is deliberate. Folding would have the
child unify against the parent's *post*-phase-B table, where upstream has it unify against the
pre-phase-B one and patch the difference in (5). The two agree on every shape either of us could
construct, but not provably: (6)'s inner loop starts at `j = i`, so a slot *below* `i` holding the
same value is left alone, which is exactly how two pre-phase-B-equal slots can end up
post-phase-B-different. Since (5) and (6) are both no-ops unless the chain contains a class
MethodImpl, mirroring costs a second fold up the chain only for hierarchies that have one.

### Covariant returns

Step 4 alone handles everything Roslyn emits; step 6 exists for a shape Roslyn cannot produce.
Measured, not assumed -- compiled and read back with `System.Reflection.Metadata`:

| shape | what Roslyn emits |
| --- | --- |
| covariant override | `newslot` + `.override` naming the *immediately-overridden method* + `[PreserveBaseOverrides]` |
| covariant override two levels down, parent silent | same, decl naming the grandparent's method |
| override with the *same* return type | plain non-newslot override, no MethodImpl at all |

Because the decl always names the immediately-overridden method, the write lands on the newest
slot of the chain and step 4's home-chasing carries the older slots along. Tracing `L0`/`L1`/`L2`:
`L2.M` takes slot 2, its MethodImpl sets `content[1] := L2.M`, and the fixed point then sees
`content[0] = L1.M` whose own slot is 1, so `content[0] := L2.M`. Step 6 finds nothing to do.

Step 6 changes something only when a `.override` skips a live intermediate slot, which is
class.cpp's own worked example (:1443-1465): `C : B` whose decl names `A::M` directly leaves
`content[1] = B.M` after unification, and only the attribute pass repoints it. C# cannot spell
that; hand-written IL can. Note the attribute *is* present on ordinary C# covariant overrides
including intermediate ones, so the gate in code must be the real one above and not "hand-written
IL only".

The validation half -- `ValidateMethodsWithCovariantReturnTypes` (class.cpp:1360-1422) throwing
`MI_BADRETURNTYPE`, and `IsEligibleForCovariantReturns` (:2143-2158) rejecting value types -- is a
separate concern from propagation and out of scope here.

## Option set

**Core representation**

- **(A) A content table beside the identity table.** Extend the existing definition walk to
  return, in one base-first fold, the identity vtable (unchanged), a `slotOf` map, and the phase-A
  and phase-B content vtables. Dispatch = find the target's slot index in its declaring type's
  table, read that index of the receiver's phase-B table. **Chosen.**
- (B) Keep the walk, but let it ask the identity table "is this my slot" at each level. No new
  table, smaller diff -- but every one of the seven defects is a content question, and a walk
  deciding "does this write reach my slot" re-derives the content table implicitly and
  incompletely. This is what #1084 is, and it is the thing being replaced.
- (C) Slot-index-only: give dispatch the index and keep signature matching for content. Cannot
  express aliasing (two declarations, one slot) at all.

**`slotOf` is memoisation, not a new source of truth.** It is a method's index in *its own*
declaring type's identity table -- the same question the dispatch entry point already asks. What
it cannot be recovered from is the *receiver's* identity table alone: `B.M` overriding `A.M` by
placement leaves only `B.M` as the leaf occupant, while step 4 asks `slotOf(A.M)`. So it is
accumulated during the one walk that already computes placement.

**Step 4 must mirror the pass structure exactly.** Its result is order-dependent, and the obvious
refactor is a different function. Verified by simulation: with three slots holding `[M2, M3, M1]`
and homes `M1->1, M2->2, M3->0`, ascending in-place passes converge to all-`M1`, descending to
all-`M3`, and "chase each home chain to its root" gives `[M3, M1, M2]` -- and does not terminate
at all on a cycle.

**Termination.** Upstream bounds nothing; it trusts structure validated elsewhere. PawPrint must
interpret IL no loader would accept, so: run the literal loop with a pass cap of
`numVirtuals + 1` and `failwith` past it. Searched for divergence over every `(content, slotOf)`
configuration for `n <= 4` (66282 configurations, including many unreachable through any loader)
and 300000 random ones for each `n` in 5..9: no divergence, and worst case exactly `n` passes. The
cap therefore never fires on real input and converts an unproven conjecture into a crash.

**Scope of the replacement.** Interface dispatch keeps the existing walk. The class content table
is self-contained -- nothing in steps 1-6 reads interface data, `PlaceInterfaceMethods` writing
only the dispatch map -- so the class cases can be exactly right without touching interfaces. The
converse does *not* hold: upstream resolves an interface call to a class slot number and then
reads the class vtable (`*pImplSlot = GetRestoredSlot(slotNumber)`, methodtable.cpp:5587-5597), so
every defect this fixes is *also* reachable through an interface spelling, and the old walk will
keep answering those. That divergence is already parked as
`InterfaceSlotHiddenByDerivedMethod.cs`; the stage-2 corpus should tag which shapes have an
interface spelling so it is enumerated rather than rediscovered. The natural future consumer of
this table is the last step of interface dispatch: resolve to a class slot, then one table read.

**Caching** -- measure first. The table build resolves the whole chain and runs a fixed point per
callvirt, where the old walk stopped at its first match and shape-prefiltered MethodImpl rows
because `System.Int32` has around 110 of them; #1084 alone cost +5.0%. Benchmark specifically on
an `Int32`/`String`-heavy guest, not only on the dispatch-saturated one. If a cache is needed: a
`Map` on `IlMachineState`, which the layout functions already thread, keyed on the definition,
which every instantiation shares. Pure function of immutable metadata, so deterministic and not
scheduling-visible.

**Sharing content across instantiations** is sound, the same way sharing identity is, but the
argument needs one more step than layout's did. MethodImpl decl resolution loads real
MethodTables and matches by canonical-MT identity (`FindDeclMethodOnClassInHierarchy`,
:6003-6024), so a TypeSpec decl is the threat: `C<T> : B<T>` with `.override B<int32>::M`. Fable
measured the host on exactly that fabricated shape -- `C<int>`, `C<string>` and `C<long>` *all*
throw `TypeLoadException` (`MI_DECLARATIONNOTFOUND`), because every instantiation is gated on the
canonical definition's build and that build resolves the decl against `B<!0>` and misses. So no
instantiation can silently disagree with the definition. The obligation this places on PawPrint:
definition-level decl resolution must **refuse** when a TypeSpec decl parent does not match the
definition's parent chain as spelled, rather than unifying `!0` with `int32` because some
instantiation would have matched.

## Staging

- **1 -- move.** Done: `VirtualSlotLayout.fs` lifted out of `NativeRuntimeTypeHelpers.fs` to
  compile position 86, before `IlMachineStateExecution.fs` at 102. Pure deletion plus 28
  qualified-name rewrites and four comments that asserted the old order. Suite unchanged from main
  at 3837.
- **2 -- the table.** Add the phase-A/phase-B content tables and their oracle. No dispatch change.
- **3 -- dispatch.** Read the table; delete the 384-line approximation; un-park
  `VirtualDispatchGenericDefinitionSlots.cs`; re-benchmark.

Stage 3's gate is *not* only "class target and `walkBaseTypes`": virtual-instance-ness, the
presence of a MethodDef row, and the SZ-array carve-out all survive from #1084. Dispatch must also
refuse an index at or past the receiver's vtable length, which adversarial IL reaches by
`callvirt` with a receiver not derived from the declaring type. And the table answers *which
MethodDef* -- reconstructing that ancestor's instantiation from the receiver's chain, to
concretise the answer, is still the caller's job via the `SlotOwner.Substitution` rebasing that
already exists.

## Oracle for stage 2

A content table is not readable through any managed API -- `GetSlot` answers identity,
`GetBaseDefinition` answers layout. What *is* readable is dispatch itself: for a pair
(declaration D, receiver type C), `callvirt D` on a C runs exactly `content[slotOf(D)]` of C.

So: extend the `TestFabricatedSlotAliasing` harness from four hand-picked cases to a corpus --
fabricate hierarchies with `PersistedAssemblyBuilder`, each body tagging its own identity, and
compare PawPrint against `RealRuntime.executeAssemblyInPlace` for every (D, C) pair.

Three constraints on how, all of which the sketch got wrong:

- **The channel is 8 bits and has no stdout.** `RealRuntimeResult` is
  `NormalExit | UnhandledException | FailFast` with no output capture, and an exit code at or above
  128 is indistinguishable from `128 + signo`. So "every pair in one exit code" is lossless only up
  to seven boolean facts, and a hash-fold would make the property probabilistically vacuous.
  Instead: parameterise the driver on a pair index through argv, which
  `executeAssemblyInPlace : string[] -> string -> RealRuntimeResult` already takes, and exit with
  the tag, keeping tags below 128 -- one process per pair, lossless. Or chunk at most seven
  booleans per driver, as the existing fixture's bitmask does.
- **Non-vacuity needs asserting, twice over.** Keep the existing `expectedOnHost` assertion per
  item, and additionally assert that the corpus contains items where the content table disagrees
  with the identity table *and* with the old walk -- otherwise the property degenerates to shapes
  everything already gets right. Then mutation-test the table build.
- **C# cannot spell the interesting shapes.** Differently-named MethodImpl aliases have no C#
  syntax, so emit the driver's IL into the fabricated image itself, which also sidesteps the
  CS0012 cross-assembly ceiling. And the generator *will* produce images CoreCLR refuses --
  `MI_MULTIPLEOVERRIDES`, the TypeSpec shape above, constraint mismatches. Refusal is a legitimate
  oracle answer, so decide up front how PawPrint's refusal (today a `failwith` in the layout walk,
  not a guest exception) is compared against the host's `UnhandledException: TypeLoadException`,
  or those items are unassertable.

Corpus shapes that must be present, each for a specific reason:

- newslot shadow, and covariant chains of depth 3 (step 4's home-chasing)
- a `.override` naming a grandparent across a live intermediate slot (step 6, the only thing that
  reaches it)
- `[PreserveBaseOverrides]` on a non-MethodImpl occupant, and a chain whose only MethodImpl is
  interface-decl (step 6 must *not* fire: `HasVTableMethodImpl` is false)
- alias retirement, and two aliased slots written by one type (#1084's rounds 4 and 6)
- a stacked shape: a `[PreserveBaseOverrides]` MethodImpl mid-chain, a value-aliased lower slot,
  and a further-derived override -- the corner where folding the two phases could have diverged
- a generic base with a TypeSpec decl (must refuse, matching the host's `TypeLoadException`)
- two MethodImpls on one slot, same body and different bodies (tolerated; load error)
