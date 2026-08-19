# Array-valued custom attribute fixed arguments

## The failure

A guest that reflects over an attribute whose constructor takes an array aborts:

```
WoofWare.PawPrint.GuestFailureException: CustomAttribValueLowering.toCliType:
TODO: lowering CustomAttribFixedArg.Array to a managed array is not implemented
(encountered Array (Some [U1 2uy; U1 1uy]))
```

`Array (Some [U1 2uy; U1 1uy])` is `NullableAttribute(new byte[] { 2, 1 })`, which Roslyn emits
on any member whose nullable annotations vary across a generic instantiation — so this is on the
path of ordinary reflection over ordinary C#, not an exotic shape.

A repro added as `WoofWare.PawPrint.Test/sourcesPure/CustomAttributeArrayArg.cs` shows there are
in fact **two** refusals, and the first one is upstream of the reported one:

```
TODO: CustomAttribute.CreateCustomAttributeInstance: ctor parameter of type
arr[<type defined in PawPrintTestAssembly>] is neither a primitive, an SZARRAY of primitives,
nor an enum; TYPE (0x50) and TAGGED_OBJECT (0x51) fixed args are not yet decoded
```

- **Gap 1 (decode).** `NativeCustomAttribute.resolveArgShape` builds the decoder's
  `CustomAttribArgShape` from the ctor parameter's `TypeDefn`. Its fast path
  (`CustomAttribute.tryShapeWithoutResolution`) handles `T[]` only when `T` is a `PrimitiveType`;
  for `MyEnum[]` it returns `None`, the resolution path then asks "is this parameter an enum?",
  gets `false` for the *array*, and refuses. So enum arrays are not decodable at all.
- **Gap 2 (lower).** `CustomAttribValueLowering.toCliType` has no way to build a managed array,
  and no information with which to build one: a decoded `CustomAttribFixedArg.Array` deliberately
  does not record its element type, because ECMA-335 II.23.3 does not put one in the blob for a
  fixed arg. The element type comes from the constructor signature.

## What CoreCLR does

`CustomAttribute_CreateCustomAttributeInstance` (`customattribute.cpp:900`) walks the ctor's
`MetaSig`, and for each parameter takes `pSig->GetLastTypeHandleThrowing()`, replacing it with
`th.GetArrayElementTypeHandle()` when the parameter is an array — i.e. it hands `GetDataFromBlob`
the *element* type handle. `GetDataFromBlob`'s `SERIALIZATION_TYPE_SZARRAY` case
(`customattribute.cpp:836`) reads a 4-byte count, treats `-1` as "null array" (leaving the slot
null), and otherwise calls `ReadArray` with an element serialization type derived from that same
element handle: `SERIALIZATION_TYPE_ENUM` if the element is an enum, else its internal
`CorElementType`.

`ReadArray` (`customattribute.cpp:526`) allocates an array of the declared element type — via
`AllocatePrimitiveArray`, `AllocateObjectArray(size, th)`, or `LoadArrayTypeThrowing(th)` +
`AllocateSzArray` for enums — and fills it. The values it writes are exactly what `GetDataFromBlob`
returns per element; an enum element is copied as a bare integer of `th.GetSize()` bytes.

Two facts to carry over:

1. The array's element type is the *declared* one. A `MyEnum[]` really is a `MyEnum[]`, even though
   the blob only holds bare integers, and a `byte[]` really is a `byte[]`.
2. `NumElem == 0xFFFFFFFF` (null array) and `NumElem == 0` (empty array) are different results, and
   both are reachable from C#.

PawPrint's blob decoder (`CustomAttribute.readElem`, `CustomAttribArgShape.SzArray`) already
implements the read side correctly, including both of those, and `TestCustomAttributeBlob` covers
it. Nothing about the *bytes* is missing. What is missing is (a) building the shape for an enum
element, and (b) turning the decoded list into a heap object.

## Design

### The information the lowering needs

The decoded value cannot carry its element type (the blob has none), and `CustomAttribArgShape`
cannot either: its `Enum` case carries only an `EnumUnderlyingType` — the *width*, which is all the
decoder needs — so `MyEnum[]` and `int16[]` have the same shape. The lowering must therefore be
given the concretised element type from outside.

Two structurally different ways to supply it:

**Option A — pass the concretised parameter type alongside the decoded value.**
`toCliType` gains a `ConcreteTypeHandle` parameter naming the argument slot's declared type, used
only in the array case. Cheap to write. Downsides: it obliges every caller to concretise a handle,
including the two named-arg call sites that need no such thing and the common scalar case that
today costs no type load at all (`resolveArgShape`'s fast path exists precisely to avoid that); and
the value/handle pairing is unchecked, so "lowered a `String` against an `int[]` handle" is
representable.

**Option B — a runtime-side plan that both decode and lowering are driven from.** Introduce, in
`CustomAttribValueLowering.fs`, a type mirroring `CustomAttribArgShape` but with the array case
carrying the concretised element handle:

```fsharp
[<RequireQualifiedAccess>]
type CustomAttribArgPlan =
    | Primitive of PrimitiveType
    | Enum of underlying : EnumUnderlyingType
    | SzArray of elementType : ConcreteTypeHandle * elements : CustomAttribArgPlan
```

`CustomAttribArgPlan.shape` projects it back to the `CustomAttribArgShape` the decoder wants, so the
*same* plan drives both the read and the lowering. A value/plan mismatch is then impossible for any
value the decoder produced, which is the property we actually want; the mismatch arms in the
lowering become genuine "the decoder disagreed with the plan it was given" bugs rather than caller
errors. It also keeps the scalar fast path: only the array cases concretise anything.

Cost of B over A: one three-case DU and a three-line projection duplicating `CustomAttribArgShape`'s
structure. Blast radius is one file plus its one caller; both options are equally reversible.

**Choosing B.** The duplication is small and the invariant it buys is the one that matters here.
Note the plan type cannot live in `WoofWare.PawPrint.Domain` beside the shape regardless of which
option is taken: `CustomAttribute.fs` compiles before `TypeConcretisation.fs`, which is where
`ConcreteTypeHandle` is defined, so it would need a file reordering; Domain is a published package,
so a new public type there has external blast radius; and neither Domain consumer — the decoder or
the IL dumper's `AttributeFormatting` — has any use for a concretised handle. So a runtime-side type
is being introduced under either option; B just gives it structure.

Nested arrays (`byte[][]`) fall out of B's recursion for free. C# forbids them in attribute
arguments, but ECMA-335's grammar admits them, CoreCLR's `ReadArray` handles them, and PawPrint's
decoder already recurses — so refusing them would be extra code to write, not less.

### Changes

**`WoofWare.PawPrint/CustomAttribValueLowering.fs`**

- Add `CustomAttribArgPlan` (above) and `CustomAttribArgPlan.shape`.
- `tryToPureCliType` keeps its `Array` `Error` arm; only its message changes, to say that lowering
  an array needs a `CustomAttribArgPlan` as well as heap allocation.
- `toCliType` gains a leading `plan : CustomAttribArgPlan` parameter and becomes recursive:
  - `SzArray _`, `Array None` → `CliType.ObjectRef None`, state unchanged.
  - `SzArray (elementType, elementPlan)`, `Array (Some elements)` → take
    `IlMachineState.cliTypeZeroOfHandle` of `elementType` as the element zero, allocate
    `ConcreteTypeHandle.OneDimArrayZero elementType` of that length, then lower each element with
    `elementPlan` and store it.
  - Both mismatch directions (`SzArray` plan with a non-array value; a scalar plan with an `Array`
    value) `failwith`, naming both sides.
  - Everything else keeps today's behaviour.
- Each element is written as `EvalStackValue.toCliTypeCoerced elementZero (EvalStackValue.ofCliType
  lowered)` — the same coercion `stelem` performs. This is load-bearing for enums, whose lowering
  yields the bare underlying integer while the array cell is the `EnumLike` `CliType.ValueType`
  wrapper; for every other element type it is the identity, and applying it uniformly is what keeps
  the cell shape equal to the `ElementZero` the array's stride was measured from. One element type
  is not quite an identity: `Float32` widens to double on the evaluation stack and narrows back,
  which canonicalises a signalling-NaN bit pattern where CoreCLR's `ReadArray` memcpys the raw bits.
  No C#-emitted blob carries one, and the scalar argument path already does the same widening when
  it pushes, so this is accepted rather than worked around.

**`WoofWare.PawPrint/Native/NativeCustomAttribute.fs`**

- `resolveArgShape` becomes `resolveArgPlan`, returning `CustomAttribArgPlan`, and recursive:
  - `TypeDefn.PrimitiveType pt` → `Primitive pt` (no type load, as today).
  - `TypeDefn.OneDimensionalArrayLowerBoundZero elt` → concretise `elt`, recurse on `elt` for the
    element plan, emit `SzArray`. This closes gap 1: an enum element is now resolved by the same
    enum path a scalar enum parameter takes, one level down.
  - otherwise → today's enum resolution, or a refusal. The refusal message loses its "nor an
    SZARRAY of primitives" clause, but must not go on to claim that only TYPE (0x50) and
    TAGGED_OBJECT (0x51) remain: `TypeDefn.Array (elt, rank)` — a multidimensional or non-SZ array
    parameter, which only hand-written IL can produce — still lands here, and CoreCLR refuses it too
    (`GetDataFromBlob` has no `ELEMENT_TYPE_ARRAY` case and falls into `badBlob`). The message names
    the parameter type and says it is not one the fixed-args grammar can decode.
- The fixed-args fold keeps the plans; `readFixedArgs` is fed `List.map CustomAttribArgPlan.shape`,
  and the lowering fold zips each decoded arg with its plan.
- `CustomAttribute.tryShapeWithoutResolution` loses its only runtime caller (the IL dumper still
  uses it) — the two `TypeDefn` cases it covers are matched directly, because the array case now
  needs the concretised element anyway.
- `resolveNamedArgShape` returns a bare `PrimitiveType` rather than a `CustomAttribArgShape`, which
  is what it has always actually computed: every other arm throws. Its two consumers then build what
  they need — `CustomAttribArgShape.Primitive pt` for the decoder, `CustomAttribArgPlan.Primitive pt`
  for the lowering's two named-arg call sites (the non-null `SerString` value, and the boxed-primitive
  payload) — and the boxing site's `match shape with Primitive pt -> pt | other -> failwith "logic
  error"` arm disappears, because the value it was re-deriving is now the return type. There is
  deliberately no general shape-to-plan converter: one would have to be partial in exactly the array
  case.
- The named-arg handler still refuses `SZARRAY`, but its message is
  rewritten: the fixed-arg path is no longer blocked, and what the named-arg path additionally
  needs is its own thing — CoreCLR resolves the element type from the blob's serialization-type
  byte rather than from a signature (`customattribute.cpp:1128`), and writes the *array* type into
  `pType` when the value is null so the managed caller picks the right `GetProperty` overload
  (`customattribute.cpp:1136`). That is a separate slice and stays out of this change.

### Ordering and re-entrancy

Plan resolution can load assemblies, so it threads state, and it happens before the QCall's
`ensureTypeInitialised` — exactly where shape resolution happens today, and for the same reason:
it cannot suspend, so a re-entered handler simply redoes it. All array *allocation* happens in the
final argument-push fold, after the blob cursor write-back and after the class-init check has been
passed, so a suspension never leaves a half-built array behind.

## Tests

**End-to-end (`sourcesPure/CustomAttributeArrayArg.cs`, already written and observed failing).**
One attribute whose ctor takes `byte[]`, `int[]`, `string[]`, `Level[]` (a `short`-underlying enum)
and a trailing `int`, applied twice: once fully populated (including a `string[]` holding a
non-empty string, `null`, and `""`), once with `null` for three of the arrays and `new int[0]` for
the fourth. Checks element values, that `null` and empty stay distinct, that the trailing scalar
survives (so a wrong element width, which desynchronises the cursor, is caught rather than silently
absorbed), and `GetType()` on two of the arrays (so an array of the wrong element type fails even
when its contents are right). Being a pure case it is differentially compared against real .NET.

**Unit (`TestCustomAttribValueLowering.fs`).** Against a corelib-only fixture:

- `Array None` under an `SzArray` plan → `ObjectRef None`, state unchanged by reference.
- `byte[]` of two elements: length, both cell values, the allocation's `ConcreteType` is
  `OneDimArrayZero byteHandle`, and `ElementStride` is 1 — i.e. the array is a `byte[]`, not an
  `int[]` holding small numbers.
- Empty array: length 0, and still carries the byte element zero and stride (an empty array has no
  cell to sample, so this is the case where a wrong element zero would otherwise be invisible).
- `string[]` holding a non-empty string, the null sentinel and `""`: contents, and that `""` is the
  canonical interned empty string (the same rule the scalar case already pins).
- An enum array (`System.Security.SecurityRuleSet`, byte-underlying, so a wrong width is visible):
  cells are the `EnumLike` `CliType.ValueType` wrapper holding the underlying value, not a bare
  integer — the property the `toCliTypeCoerced` step exists for — *and* the allocation's
  `ConcreteType` is `OneDimArrayZero` of the enum's handle, not of its underlying type's. The e2e
  guest pins that second half too, but only there, and a pure case can end up parked for an
  unrelated reason.
- A nested `byte[][]`.
- Both mismatch arms throw.
- Property: for a generated list of primitive args, lowering them as an array agrees element-by-element
  with lowering each one on its own, and the array's length is the list's length.

## Out of scope

- Named-arg (`CustomAttribute_CreatePropertyOrFieldData`) SZARRAY values; see above.
- `TYPE` (0x50) and `TAGGED_OBJECT` (0x51) arguments, scalar or array. `Type[]` and `object[]` ctor
  parameters therefore still refuse, as they do today.
