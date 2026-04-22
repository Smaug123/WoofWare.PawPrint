# No-op refactor: replace the flat string pool with sibling `char[]` allocations

## Context

`System.String` is modelled on main with two pieces of special-case storage:

- `ManagedHeap.StringArrayData : ImmutableArray<char>` — a flat bump-allocated pool
  containing *every* string's chars back-to-back, with null terminators.
- `ManagedHeap.StringContents : ImmutableDictionary<ManagedHeapAddress, string>` — a
  side-table from a String heap address to its managed `string` contents, consulted by
  `ManagedHeap.stringsEqual` (the only consumer; used by the `string.Equals(string, string)`
  intrinsic).

The String heap object itself carries two real CLR-shaped fields: `_firstChar` (a
`Char`) and `_stringLength` (an `Int32`). `_firstChar` is populated with
`StringArrayData.[dataAddr]` at allocation time, but the remaining chars live only in
`StringArrayData`.

This layout is load-bearing for the current runtime, but it blocks the `memmove`
programme (`docs/plans/2026-04-20-memmove.md`). PR C in that stack ("string char
addressing") ended up growing a parallel `ByrefRoot.StringCharAt` constructor and
threading it through byref read/write, byref arithmetic, byte-view intrinsics, and
four `SetField "_firstChar"` sync sites — nearly all of which are structurally
identical to the existing `ByrefRoot.ArrayElement` machinery. The reason `ArrayElement`
couldn't be reused was simply that the chars weren't in an array; they were in
`StringArrayData`.

If the chars sat in a sibling `char[]` in the ordinary `Arrays` map, PR C collapses
into ~two IL-dispatch cases and inherits the entirety of the array-byref machinery
from PRs A and B for free.

The user's specific concern about PR C's shape — "is there some structural reason the
CLR requires these as special cases?" — is answered by this refactor: there isn't,
the specialness is an artefact of our pool, and removing the pool removes the
specialness.

## Guiding principle

This is a pure representational refactor. No observable behaviour changes, no IL
semantics change, no currently-failing test starts passing, no currently-passing test
changes its output. The only visible difference is internal data-structure shape.

## Current state on main

### Allocation (`IlMachineState.allocateManagedString`)

```
allocateStringData contents.Length    // grows StringArrayData by len+1
setStringData dataAddr contents       // writes contents into the pool
_firstChar = CliType.ofChar StringArrayData.[dataAddr]
_stringLength = Int32 contents.Length
allocateManagedObject with these two fields
ManagedHeap.recordStringContents addr contents    // populates StringContents
```

Two entry points: `UnaryStringTokenIlOp` (Ldstr, with the `InternedStrings`
cache keyed by StringToken) and `IlMachineStateExecution` (TypeInitializationException
constructing its `_typeName`). Both go through `allocateManagedString`, so there is
one allocation seam.

### `_firstChar` / `_stringLength` consumers

A subagent search on the pre-refactor branch confirmed that `_firstChar` is only
observed through named access; nothing iterates string fields generically and expects
`_firstChar` to be present. The producers of `ByrefRoot.HeapObjectField(_, "_firstChar")`
on main are exactly one: `UnaryMetadataIlOp.fs` line 1104 (the `Ldflda` handler). The
value is read via `AllocatedNonArrayObject.DereferenceField "_firstChar"` (Ldfld on an
ObjectRef) or via the generic Ldfld-through-ManagedPointer path; both take the field
name by string match inside `CliValueType.DereferenceField`.

### `StringArrayData` / `StringContents` consumers

- `StringArrayData`: read once in `allocateManagedString` for the `_firstChar` seed;
  indexed nowhere else on main.
- `StringContents`: read only through `ManagedHeap.getStringContents`, which is called
  only from `ManagedHeap.stringsEqual`, which is called only from the
  `string.Equals(string, string)` intrinsic handler (`Intrinsics.fs:503` on main).

No cross-string aliasing: the pool is a bump allocator, no two strings share char
storage, and interning lives at the IL level (`InternedStrings` keyed by
`StringToken`), completely orthogonal to the pool.

## Goal

Replace the pool and the cache with a sibling `char[]` allocation per string, stored
in the normal `Arrays` map, with a side-table recording the String-to-sibling link.
Intercept `Ldfld`/`Ldflda` on `System.String._firstChar` to read/address the sibling's
element 0, so that the String heap object no longer needs to carry `_firstChar` as a
stored field.

Observable behaviour is identical before and after. Nothing on main moves from
"passing" to "failing" or vice versa. The test suite runs unchanged.

### Why this sets up the memmove rebase

After this refactor lands:

- PR C of the memmove stack collapses to: "`Ldflda System.String._firstChar` returns a
  byref rooted at `ArrayElement(siblingAddr, 0)`, [] instead of the HeapObjectField
  root." Exactly one IL-dispatch site changes.
- `ByrefRoot.StringCharAt`, `StringDataOffsets`, `resolveStringCharAt`, `writeStringChar`,
  `recordStringDataOffset`, and all the `StringCharAt` branches in
  `BasicCliType.normaliseArrayByteOffset` / `Intrinsics` / `readManagedByref` /
  `writeManagedByref` / `BinaryArithmetic` disappear — they were emulating
  `ArrayElement` byref semantics, and now they *are* `ArrayElement` byrefs.
- The four `SetField "_firstChar"` sync sites in `Intrinsics`/`IlMachineState` go away:
  the sibling array is the single source of truth; there is no `_firstChar` field to
  keep in sync.

## Changes

### `ManagedHeap.fs`

**Remove:**

- `StringArrayData : ImmutableArray<char>`
- `StringContents : ImmutableDictionary<ManagedHeapAddress, string>`
- `allocateString : int -> ManagedHeap -> int * ManagedHeap`
- `setStringData : int -> string -> ManagedHeap -> ManagedHeap`
- `recordStringContents : ManagedHeapAddress -> string -> ManagedHeap -> ManagedHeap`
- `getStringContents : ManagedHeapAddress -> ManagedHeap -> string option`

**Add:**

- `StringCharArrays : Map<ManagedHeapAddress, ManagedHeapAddress>` — maps the String
  heap address to the heap address of its sibling `char[]`. Populated at allocation
  time. The sibling array lives in the ordinary `Arrays` map; nothing else special.
- `resolveStringChars : ManagedHeapAddress -> ManagedHeap -> AllocatedArray` — returns
  the sibling array for a given String address. Internal helper for `stringsEqual`
  and the Ldfld interception.

**Change `stringsEqual`:** instead of comparing `StringContents[a1]` against
`StringContents[a2]`, read the two sibling arrays, take the first `_stringLength`
chars of each (the terminator is not part of the comparison), and compare element-wise.
`_stringLength` is read from the String heap object's stored field. If either address
is not a registered string (no entry in `StringCharArrays`), fail loudly with the same
message shape as before.

### `IlMachineState.fs`

**Remove:**

- `allocateStringData : int -> IlMachineState -> int * IlMachineState`
- `setStringData : int -> string -> IlMachineState -> IlMachineState`

**Change `allocateManagedString`:**

```
let allocateManagedString loggerFactory baseClassTypes contents state =
    // validate String.Fields shape exactly as today (unchanged)
    //
    // 1. Allocate a char[] of length (contents.Length + 1) with null terminator,
    //    populated with contents + '\0'.
    let siblingAddr, state = <existing char[] allocation machinery> state
    //    This uses AllocatedArray with ElementType = Char and Length = len + 1,
    //    exactly as a normal `new char[n]` would.
    //
    // 2. Build the String heap object with _stringLength only (no _firstChar).
    //    Fields list = [ _stringLength ] (one element, Int32 contents.Length).
    //
    // 3. Allocate the String object; record the linkage.
    let addr, state = allocateManagedObject stringType fields state
    let state =
        { state with
            ManagedHeap =
                { state.ManagedHeap with
                    StringCharArrays = state.ManagedHeap.StringCharArrays.Add (addr, siblingAddr)
                }
        }
    addr, state
```

The metadata-shape check stays as today (asserts `[_firstChar; _stringLength]`), but
construction only stores `_stringLength`. Document inline: "String's `_firstChar` field
is synthesised via Ldfld/Ldflda interception in UnaryMetadataIlOp — see the sibling
char[] at `state.ManagedHeap.StringCharArrays.[addr]`." Failure mode if the BCL's
`String` shape ever changes is unchanged (same `failwith`).

### `UnaryMetadataIlOp.fs`

**Add two interception cases**, symmetric, each guarded on "the field is
`System.String._firstChar`":

- `Ldfld`: when `field.DeclaringType` is `System.String` and `field.Name` is
  `_firstChar`, read `resolveStringChars addr heap`'s element 0 and push that char;
  skip the default `AllocatedNonArrayObject.DereferenceField` / `CliType.getField`
  path.
- `Ldflda`: in the same guard, produce
  `ManagedPointerSource.Byref (ByrefRoot.ArrayElement (siblingAddr, 0), [])` instead
  of `Byref (ByrefRoot.HeapObjectField (strAddr, "_firstChar"), [])`.

The guard lives at the top of the per-variant `ObjectRef` branch (and the
`ManagedPointer` branch for Ldfld), before the generic field dispatch. The check is a
structural match on the field's declaring type — not a pattern on the field name in
isolation, and not a blanket name match like on the earlier branch. "Is this a String
and is the requested field `_firstChar`?" If yes, route to the sibling; if no, fall
through unchanged.

(If later we grow more sibling-backed types, we can generalise the check by routing it
through a tiny predicate. For now, one type has this treatment, so a direct check is
clearest.)

### `UnaryStringTokenIlOp.fs`

No change. `InternedStrings`-keyed `ldstr` path continues to call
`IlMachineState.allocateManagedString`; the implementation behind the seam changes but
the signature doesn't.

### `Intrinsics.fs`

No change in the `string.Equals(string, string)` handler — it still calls
`ManagedHeap.stringsEqual`, which is still the one function that understands how to
compare two string addresses. The implementation behind `stringsEqual` changes; the
call site doesn't.

### `tui-design.md`

Update the one prose reference: "shows the string content from `StringArrayData`"
→ "shows the string content from the sibling char array recorded in
`StringCharArrays`."

## Validation

No new tests. This refactor must pass the existing test suite unchanged. The suite
already exercises:

- Ldstr with interning (many tests).
- `String.Equals(string, string)` over interned and runtime-constructed strings.
- `TypeInitializationException` allocation paths (exception tests).
- `Ldfld _firstChar` through the various paths (implicitly, via any BCL code that
  walks String internally — most BCL string code reads `_firstChar` through
  `Unsafe.Add`/`RuntimeHelpers.GetRawData`, which routes through our intrinsics, but
  the plain-field read path is exercised by anything that emits a naked
  `ldfld _firstChar`, e.g. `string.get_Chars(0)` fallbacks).

Run:

```
nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal
```

and confirm the same pass/fail set as before the refactor. No entries added to or
removed from `unimplemented`.

## Risks and invariants

1. **Metadata shape check stays.** The `_firstChar; _stringLength` assertion in
   `allocateManagedString` is the one place we hard-check the BCL's String layout;
   keep it. If .NET ever reshapes `System.String`, we want a loud failure with a clear
   name, not a silent drift.

2. **`DereferenceField "_firstChar"` on a String.** After the refactor, the String
   heap object does not store `_firstChar`. Any generic path that calls
   `CliValueType.DereferenceField "_firstChar"` on a String's `CliValueType` will
   throw "Field '_firstChar' not found". The existing dispatch paths that do this
   (two in `UnaryMetadataIlOp.Ldfld`) are covered by the interception. If a new path
   ever reaches `DereferenceField "_firstChar"` on a String without going through
   `Ldfld`, it will fail loudly — which is the correct outcome.

3. **`_stringLength` byref stability.** `Ldflda` on `_stringLength` continues to
   produce `Byref(HeapObjectField(strAddr, "_stringLength"), [])` — unchanged. The
   field is still a real stored field.

4. **`StringCharArrays` side-table lifetime.** Keyed by the String's heap address.
   Strings are immutable and never collected (we have no GC), so entries are
   permanent. No invalidation concern.

5. **Sibling allocation ordering.** Allocate the sibling `char[]` first, then the
   String object, then record the linkage. This means `FirstAvailableAddress`
   increments twice per string. Fine — all tests exercising allocation patterns take
   this as opaque.

6. **Interning still works.** `InternedStrings` is keyed by `StringToken`, not by
   content, and lives in `IlMachineState`, not the heap. Completely unaffected.

7. **Determinism.** No new mutable state; side-table is immutable like the rest of the
   heap. Allocation order is deterministic.

8. **`stringsEqual` complexity.** Goes from O(1) dictionary lookup to O(min len)
   element-wise comparison. Fine — it was only used by `string.Equals`, which is O(n)
   anyway in the real BCL; nothing depends on the dictionary-lookup shortcut.

## After this lands

Rebase `memmove-pr-c-string-char-addressing` on the new main. The existing diff
largely evaporates — no new ByrefRoot constructor, no parallel data table, no
`SetField "_firstChar"` sync sites, no StringCharAt branches anywhere. PR C becomes
a change to the `Ldflda System.String._firstChar` interception: instead of returning
`Byref(ArrayElement(siblingAddr, 0), [])`, it returns the same but exposes the full
char range for pointer arithmetic. In practice this is already the case — so PR C
may collapse to zero lines of runtime change plus its tests.
