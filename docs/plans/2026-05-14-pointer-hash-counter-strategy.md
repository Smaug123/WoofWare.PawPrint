# Plan: counter-based pointer-bit synthesis (supersedes FNV-1a-of-ToString)

Date: 2026-05-14
Status: Proposed
Branch: `castcache-synthetic-hash-bits` (extends current branch in place)
Supersedes the synthesis strategy in `docs/plans/2026-05-13-castcache-synthetic-hash-bits.md`.

## Context

The current branch (commits `b241a5e` … `d02dbe6`) introduces
`Int64Source.OpaqueHashBits` — a tagged variant carrying synthesised
bits derived from a pointer's identity, produced whenever a bit-mixing
or arithmetic operation fires on a `WidenedNativeInt`. The tag's
load-bearing job is to refuse round-trip back to a pointer, so a
synthesised hash cannot be reinterpreted as managed-pointer provenance.

The synthesis itself, as currently implemented, is FNV-1a over a
canonical string projection of the source (`canonicalHashKey` →
`fnv1aHash` in `CliNumericType.fs`). This has three issues that surfaced
during review:

1. **Determinism by detour.** `NativeIntSource.GetHashCode` goes through
   `System.HashCode.Combine`, which folds in a per-process randomised
   seed. The current branch routes around this by hashing
   `ToString`'d output instead. The determinism contract therefore
   depends on `ToString` being structurally stable for every member of
   every handle DU forever, with no compiler-enforced guarantee.

2. **Hash-function accretion.** Commit `d02dbe6` teaches
   `canonicalHashKey` that `MethodTablePtr h` and
   `TypeHandlePtr (Closed h)` for Concrete/OneDimArrayZero/Array shapes
   are aliases for the same CoreCLR `MethodTable*`. This is correct
   behaviour, but it lives inside the hashing function as a string-key
   special case. The next aliasing rule we discover will land there
   too, gradually rebuilding a partial type-identity system inside a
   hash function.

3. **Collisions are possible in principle.** FNV-1a is a 64-bit hash;
   the birthday bound is ~2^32 distinct pointers before a collision is
   likely. PawPrint will not approach this in practice, but a collision
   between two distinct pointers would silently make them compare equal
   under `ceq` against their bit pattern — an undetectable correctness
   bug for as long as it lasted.

The cast-cache motivating use case (`KeyToBucket` in
`CastCache.cs`) requires synthesis only at one demand site: the
`int32` array index that consumes the hash. Everything before that
demand site is decidable algebraically (shape low-bit checks,
identity equality of pointers, alignment masks) provided the
synthesis honours its low-bit contract and assigns distinct bits to
distinct pointers.

An AST-based alternative was explored in a now-deleted spike. The
conclusion was that the AST's only unique contribution over a
suitable eager scheme is a forcing function for *undecidable*
comparisons (e.g. sorting pointers by value), and no known code path
in PawPrint asks such questions. The bookkeeping cost of the AST
exceeded its practical benefit.

## Design

### Counter-based synthesis

Replace FNV-1a with a counter-assignment scheme. The interpreter
maintains a map from canonical pointer keys to assigned bit patterns.
The first time a pointer of canonical key `k` is presented for
materialisation, it is assigned the next counter value, shifted left
by 2 and OR'd with the shape's required low-bit pattern (so
alignment / TypeDesc-tag checks continue to work). Subsequent
materialisations of the same `k` return the previously-assigned bits.

```fsharp
type PointerHashCounters =
    {
        NextCounter : uint64
        Assigned : Map<CanonicalPointerKey, uint64>
    }
```

Properties:

- **No collisions.** Distinct keys get distinct bits by construction.
- **Determinism is structural.** Counter assignments depend only on
  the order in which the interpreter first materialises each canonical
  key. Given a fixed program and a fixed scheduler, that order is
  deterministic.
- **Canonicalisation moves to where it belongs.** `CanonicalPointerKey`
  is a structured DU whose equality is F#'s structural equality. The
  `MethodTablePtr` / `TypeHandlePtr` alias is a constructor choice in
  the canonical-key projection, not a string-transform special case.
  Adding future aliases means adding new arms to the projection — a
  single function, type-checked.

### Canonical pointer key

```fsharp
[<RequireQualifiedAccess>]
type CanonicalPointerKey =
    /// Concrete / OneDimArrayZero / Array shapes share their MethodTable*
    /// with their TypeHandle, so both encodings collapse here.
    | MethodTable of ConcreteTypeHandle
    /// TypeDesc-shaped (Byref / Pointer / FunctionPointer) and
    /// open-generic / generic-parameter targets keep distinct keys.
    | TypeHandle of RuntimeTypeHandleTarget
    | FunctionPointer of /* whatever NativeIntSource.FunctionPointer carries */
    | FieldHandle of /* ... */
    | MethodHandle of /* ... */
    | MethodTableAuxiliaryData of /* ... */
    | GcHandle of /* ... */
    | EventPipeProvider of /* ... */
    | EventPipeEvent of /* ... */
    | AssemblyHandle of /* ... */
    | ModuleHandle of /* ... */
    | MetadataImportHandle of /* ... */
    /// Verbatim values don't reach the canonicalisation path
    /// (`materialiseHashBits` returns the numeric value directly).
    /// Null managed pointer becomes Verbatim 0L before this point.

let canonicalKey (src : NativeIntSource) : CanonicalPointerKey = ...
```

The projection lifts each `NativeIntSource` constructor to its
canonical key. The Concrete/Array `MethodTablePtr` ↔ `TypeHandlePtr`
alias collapses at construction. Other shapes pass through
identity-preserving.

### Low-bit contract

The existing `typeHandleLowAddressBitsForHash` stays, repurposed
slightly:

```fsharp
let private lowBitsForKey (key : CanonicalPointerKey) : uint64 =
    match key with
    | CanonicalPointerKey.MethodTable _ -> 0UL                // MethodTable aligned
    | CanonicalPointerKey.TypeHandle (RuntimeTypeHandleTarget.OpenGenericTypeDefinition _)
    | CanonicalPointerKey.TypeHandle (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _))
    | CanonicalPointerKey.TypeHandle (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _))
    | CanonicalPointerKey.TypeHandle (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _)) -> 0UL
    | CanonicalPointerKey.TypeHandle _ -> 2UL                 // TypeDesc-shaped
    | _ -> 0UL                                                // other handles aligned by convention
```

Counter bits occupy bits 2..63; shape bits occupy bits 0..1.

### Materialisation helper

```fsharp
/// The single named site at which synthesised bits come into existence.
/// Every bit op that lifts a `WidenedNativeInt` calls this with a `reason`
/// string identifying the demand site; the reason flows into any
/// diagnostic emitted from here.
let materialiseHashBits
    (reason : string)
    (src : NativeIntSource)
    (counters : PointerHashCounters)
    : uint64 * PointerHashCounters
    =
    match src with
    | NativeIntSource.Verbatim n -> uint64 n, counters
    | NativeIntSource.ManagedPointer ManagedPointerSource.Null -> 0UL, counters
    | NativeIntSource.ManagedPointer _ ->
        failwith $"materialiseHashBits %s{reason}: refusing to synthesise bits for managed pointer %O{src} (would erase byref provenance)"
    | NativeIntSource.SyntheticCrossArrayOffset _ ->
        failwith $"materialiseHashBits %s{reason}: refusing to synthesise bits for cross-array offset %O{src}"
    | _ ->
        let key = canonicalKey src
        match Map.tryFind key counters.Assigned with
        | Some bits -> bits, counters
        | None ->
            let n = counters.NextCounter
            let bits = ((n + 1UL) <<< 2) ||| lowBitsForKey key
            let counters' =
                { NextCounter = n + 1UL ; Assigned = Map.add key bits counters.Assigned }
            bits, counters'
```

`reason` is a `string` because the cost of constructing it is small
relative to the cost of a bit-mixing op, and the diagnostic value when
something goes wrong is high.

### State threading

`PointerHashCounters` becomes a field of `IlMachineState`. Bit ops in
`CliNumericType.fs` that previously called `materialiseHashBits`
synchronously must now thread state. The current branch's
`Int64Source.bitXor` (etc.) signature changes from pure to
state-passing:

```fsharp
// Before:
val bitXor : Int64Source -> Int64Source -> Int64Source

// After:
val bitXor
    : reason : string
    -> Int64Source
    -> Int64Source
    -> PointerHashCounters
    -> Int64Source * PointerHashCounters
```

This propagates to callers — primarily `BinaryArithmetic.execute`,
which already threads `IlMachineState`. The state-threading cost is
the main implementation tax of this design and is unavoidable: any
synthesis scheme that gives stable bits per pointer needs *some* state
(either explicit, as here, or process-global mutable, which we won't
use because it breaks deterministic replay).

### What stays from the current branch

- `Int64Source.OpaqueHashBits of int64`. The carrier and tag stay.
- The bit-pattern equality / unsigned-comparison rules in
  `EvalStackValueComparisons.fs` (`ceq`, `cgtUn`, `cltUn`) stay.
- The refusals: `conv.u` / `conv.i` / `conv.r4` / `conv.r8` /
  `ToBytes` / `convToNativeInt` of `OpaqueHashBits` keep failing
  loudly.
- The narrow-conversion permissions: `conv.i1` / `conv.i2` / `conv.i4`
  / `conv.u1` / `conv.u2` / `conv.u4` of `OpaqueHashBits` keep
  returning a plain integer.
- The `WidenedNativeInt × Verbatim` and `× OpaqueHashBits` arms in
  `BinaryArithmetic.execute` stay structurally; they change only to
  thread the state through the materialiser.
- The `WidenedNativeInt × WidenedNativeInt` ceq arm in
  `EvalStackValueComparisons.fs` (route through NativeInt arms for
  identity-equality of pointers) stays.

### What changes

- Delete `Int64Source.canonicalHashKey` (the string-projection).
- Delete `Int64Source.fnv1aHash`.
- Delete `Int64Source.typeHandleLowAddressBitsForHash` if `lowBitsForKey`
  fully replaces it; otherwise keep with renamed signature.
- `materialiseHashBits` moves from `Int64Source` module to a small new
  module (proposed: `PointerHashSynthesis.fs`, placed alongside
  `CliNumericType.fs`) so it can be called from both `CliNumericType`
  and `BinaryArithmetic` without circular dependency. The module
  exports the `CanonicalPointerKey` DU and the `PointerHashCounters`
  record.
- `IlMachineState` gains a `PointerHashCounters` field; the
  constructor initialises it to
  `{ NextCounter = 0UL ; Assigned = Map.empty }`.
- Bit-op functions in `CliNumericType.Int64Source` (`shl`, `shr`,
  `shrUn`, `bitAnd`, `bitOr`, `bitXor`, `bitNot`, `negate`) gain a
  `reason : string` parameter and return
  `Int64Source * PointerHashCounters`, threading state.
- `BinaryArithmetic.execute`'s arms for
  `WidenedNativeInt × Verbatim`, `WidenedNativeInt × OpaqueHashBits`,
  `WidenedNativeInt × WidenedNativeInt` switch from inline
  `Int64Source.materialiseHashBits` calls to the new helper, threading
  state via the `IlMachineState` it already receives.
- The `NullDereferenceTest.cs` blocker note is updated: the next
  blocker is the `conv.u` of an `OpaqueHashBits` value (the
  `(nuint)RotateLeft((ulong)source, _)` step). Address in a follow-up
  by adding a parallel `NativeIntSource.SynthesisedBits` variant whose
  rules mirror `OpaqueHashBits` for round-trip refusal, deref
  refusal, and indexing permission. Or, decide to intercept
  `CastCache.TryGet` at the boundary instead; this plan stays
  agnostic.

## Forward path: synthesis is a strategy

The choice of counter assignment is one valid strategy out of
several. PawPrint's design direction (per `memory/threading_model.md`)
already treats nondeterminism as something the user fuzzes over:
thread schedules vary, and tests are expected to be invariant under
that variation, with sensitivity discovered by fuzzing. Pointer-bit
synthesis is the same shape of problem. The real CLR's bit patterns
depend on OS allocator behaviour, ASLR, and GC compaction history;
managed code that observes those bits in any way is non-portable by
construction. The runtime's job is to let users discover such
sensitivity, not to hide it.

In a future PR, the materialisation helper will gain a strategy
parameter. The shape we anticipate:

```fsharp
[<RequireQualifiedAccess>]
type PointerHashStrategy =
    /// Deterministic counter assignment in canonical-key registration order.
    /// The default; recommended for most testing.
    | Counter

    /// Stateless structural hash. Useful when reproducibility across runs
    /// without snapshotting counter state is wanted (e.g. comparing two
    /// runs that legitimately register pointers in different orders).
    | StatelessHash

    /// Pseudo-random bits seeded from a user-supplied seed. The fuzzing
    /// mode: rerun the same test with many seeds; results that vary by
    /// seed depend on pointer magnitudes and are flagged.
    | Random of seed : uint64
```

The strategy lives in `IlMachineState` alongside the scheduler
configuration. This PR does not introduce the strategy DU — only the
counter scheme — because doing both at once is speculative generality;
adding the DU is a mechanical change against a single helper boundary
once a second strategy is actually wanted.

The framing matters for *this* PR because it informs how
`materialiseHashBits` is shaped: it takes state, returns updated
state, and is the single audited site at which bits come into
existence. That seam is what makes adding strategies later cheap.

## Touch-point inventory

| File | Change |
| --- | --- |
| New: `WoofWare.PawPrint/PointerHashSynthesis.fs` | `CanonicalPointerKey` DU, `canonicalKey`, `lowBitsForKey`, `PointerHashCounters`, `materialiseHashBits` |
| `WoofWare.PawPrint/CliNumericType.fs` | Delete `canonicalHashKey`, `fnv1aHash`, inline `materialiseHashBits`; `Int64Source` bit ops thread state and gain `reason` parameter |
| `WoofWare.PawPrint/BinaryArithmetic.fs` | Existing `WidenedNativeInt × *` arms swap inline materialisation for state-threading calls into the new helper |
| `WoofWare.PawPrint/IlMachineState.fs` | New `PointerHashCounters` field; initialiser |
| `WoofWare.PawPrint.fsproj` | Add `PointerHashSynthesis.fs` before `CliNumericType.fs` |
| `WoofWare.PawPrint.Test/sourcesPure/` | Add a determinism-of-counter test (see below) |

Estimated diff size: smaller than the current branch's, because the
state-threading mechanically updates ~10 op signatures but most ops
keep their internal structure.

## Test plan

The existing tests (`Int64HashBitMix.cs`, `Int64HashMulFirst.cs`) keep
passing; their assertions about behaviour-under-bit-twiddling are
strategy-agnostic.

New tests:

1. **Counter determinism within a run.** A C# test that bit-twiddles
   two pointers `A` and `B` and asserts:
   - `(ulong)A ^ (ulong)A == 0` (same pointer → same bits)
   - `(ulong)A ^ (ulong)B != 0` (distinct pointers → distinct bits)
   - `(ulong)A == (ulong)A` (compared as nuint, after a round trip)
   - `(ulong)A & 3` produces the expected shape low bits

2. **Order-stable counter assignment.** F# unit test in
   `WoofWare.PawPrint.Test` that constructs two
   `IlMachineState`-equivalent fixtures, materialises three pointers
   in the same order, and asserts the assigned bits match. This pins
   the determinism contract in code without depending on the harness's
   replay machinery.

3. **Alias unification.** F# unit test that constructs
   `MethodTablePtr h` and `TypeHandlePtr (Closed (ConcreteTypeHandle.Concrete _))`
   referring to the same underlying type, materialises both, and
   asserts the bits are identical. This is the `d02dbe6` contract,
   relocated.

4. **No collisions across plausible distinct types.** Materialise
   N pointers (N small, e.g. 100, drawn from
   `TestPureCases.sourcesPure` types) and assert all assigned bits
   are distinct. The current FNV-1a scheme could not assert this
   absolutely; the counter scheme can.

## Validation

1. `nix develop -c dotnet build` — clean.
2. `nix develop -c dotnet fantomas .` — formatted.
3. `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "Name~HashBitMix"` — passes.
4. `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "Name~PointerHashCounter"` — passes.
5. Full suite: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal` — no regressions.
6. Commit as a new commit on `castcache-synthetic-hash-bits` (this
   branch is still pre-merge to `main`, so we extend it rather than
   branch off).
7. `codex review --base main`. Address findings.

## Risks

- **State threading.** `Int64Source` bit ops becoming
  state-passing is the biggest mechanical change. The compiler
  catches every miss (type-changed signatures), so the risk is
  bounded to build failures rather than runtime bugs. Estimated
  ~12 op signatures change, with maybe ~30 call sites that need
  threading.
- **`reason` strings.** Adding a `reason` parameter to every op is
  forgettable noise that could decay into `""`. Mitigation: enforce
  by convention initially; if it decays in practice, promote to a
  typed enum of demand sites.
- **`canonicalKey` arm coverage.** Adding a new
  `NativeIntSource` constructor in the future without a matching
  `canonicalKey` arm would make the match incomplete. Warnings as
  errors catches this at compile time.

## Non-goals

- **The strategy DU.** Not in this PR (see "Forward path"). The
  helper's signature stays state-passing so adding strategies later
  is a mechanical change.
- **`NativeIntSource.SynthesisedBits`** (the parallel variant the
  current branch's `unimplemented` note anticipates) — separate
  follow-up, choice between that and intercepting `CastCache.TryGet`
  is open.
- **Removing `internCastCacheSentinelTable`.** As before, follow-up
  cleanup.

## Why this design over the alternatives

### vs. status quo (FNV-1a-of-ToString)

- Eliminates `ToString` determinism dependency (which is currently
  unverifiable by the compiler).
- Eliminates the (small) collision possibility.
- Moves canonical-key handling from a string-projection inside a
  hash function to a structured-data function inside the
  materialiser. Future aliasing rules land in one DU constructor,
  with compile-time guarantees of coverage.
- Costs: state threading through ~12 op signatures.

### vs. AST symbolic representation

- The AST's unique contribution over counter+eager is a forcing
  function for undecidable comparisons (e.g. sorting pointers by
  value). No known PawPrint code path asks such questions; the
  forcing function would be dormant.
- The AST's algebraic simplification (`x ^ x = 0`,
  `(p & 3) == shape`) is duplicated for free under counter
  assignment, because counter-assigned bits respect both
  same-pointer-same-bits and the shape low-bit contract.
- The AST's inspectability advantage is marginal: an 11-node
  `Mul(Xor(Or(...)))` tree is not meaningfully more readable than
  a 64-bit integer plus an `Assigned` map showing which pointer is
  which counter. The map is the inspectable form.
- The AST's implementation cost (new module, simplifier,
  width-threaded carrier across all integer sources) is large for
  the marginal benefit. Counter assignment costs ~1 small module
  and the existing state threading.

### vs. intercepting `CastCache.TryGet` higher up

- Intercepting bypasses the bit-twiddling entirely, sidestepping
  this whole question for the cast cache. Trace fidelity loss
  is one method boundary.
- But the bit-twiddling primitives are useful beyond the cast
  cache (anywhere the BCL does pointer-bit alignment or
  tagging). Building them on counter synthesis benefits all such
  paths, including ones we haven't found yet.
- An interception can still be added on top of this plan if the
  cast-cache trace fidelity ends up not being worth it. The two
  are not exclusive.

## Why this PR sequencing

The current branch is pre-merge and the FNV-1a synthesis is the
substantive concern in review. Replacing the synthesis with counters
is in-scope as a course correction within the same branch — small
enough not to warrant a separate branch, large enough that it should
land as a discrete commit on top of the existing four, with a clear
message naming the change and the reasoning above.
