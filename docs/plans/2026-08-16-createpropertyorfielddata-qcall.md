# Plan: `CustomAttribute_CreatePropertyOrFieldData` QCall

## Context

`CustomAttribute_CreateCustomAttributeInstance` (`Native/NativeCustomAttribute.fs`) implements the
*first* half of `RuntimeCustomAttributeData.AddCustomAttributes`: it decodes the fixed-arg section of
a `CustomAttrib` blob, allocates the attribute, runs its ctor, and hands back the named-arg count.
The caller then loops that many times
(`RuntimeCustomAttributeData.cs:1559`, pinned runtime source):

```csharp
for (int j = 0; j < cNamedArgs; j++)
{
    GetPropertyOrFieldData(decoratedModule, ref blobStart, blobEnd,
                           out string name, out bool isProperty, out RuntimeType? type, out object? value);
    ...
}
if (blobStart != blobEnd) throw new CustomAttributeFormatException();
```

`GetPropertyOrFieldData` is the managed wrapper over the QCall this plan covers
(`RuntimeCustomAttributeData.cs:1900`; native side `src/coreclr/vm/customattribute.cpp:1022`).
It decodes exactly one `NamedArg` from ECMA-335 II.23.3 and advances the cursor.

Note the trailing `blobStart != blobEnd` check: the caller verifies our cursor write-back, so a
cursor arithmetic bug is *guest-observable* rather than silent.

### Measured reachability (2026-08-16, `CSharpExample` playground on `origin/main` @ `81b75bf2`)

All four rows below were measured on this machine, not inferred.

| Guest program | Where it stops |
| --- | --- |
| `typeof(D).GetCustomAttributes(typeof(MarkerAttribute), false)`, attribute has an `int` ctor param plus a `string` property and an `int` field named arg | **`CustomAttribute_CreatePropertyOrFieldData`** |
| same, but with a throwaway probe handler installed, *property* named arg | `System.RuntimeMethodHandle::GetMethodDef(RuntimeMethodHandleInternal)` **InternalCall**, 17 frames out |
| same, *field* named arg | **`RuntimeFieldHandle_SetValue`** QCall, 9 frames out |
| attribute with only the implicit parameterless ctor plus a field named arg | reaches this QCall, cursor at index 4 |

Exact stub signature as PawPrint sees it (from the unimplemented-QCall message):

```
System.Reflection.CustomAttribute::<CreatePropertyOrFieldData>g____PInvoke|32_0(
    QCallModule, *(IntPtr), IntPtr, StringHandleOnStack, *(Int32),
    ObjectHandleOnStack, ObjectHandleOnStack) -> void
```

`[MarshalAs(UnmanagedType.Bool)] out bool` lowers to `int32*` (the wrapper does `ldloca.s` on an
`int32` local and `cgt.un` back to a `bool` after the call), and the `LibraryImport`'s
`StringMarshalling.Utf16` is vestigial — there are no `string` parameters.

### Measured pointer shapes

The probe printed the decoded blob bounds on both cursor provenances:

* **our own handler's write** (ctor-with-parameters path): cursor `ArrayElement(arr, 8)`, end
  `ArrayElement(arr, 31)`, bytes
  `54 0E 05 4C 61 62 65 6C 02 68 69 | 53 08 05 43 6F 75 6E 74 2A 00 00 00`
  — i.e. `[Marker(7, Label = "hi", Count = 42)]`, exactly as ECMA-335 II.23.3 predicts.
* **managed `blobStart + 4` arithmetic** (parameterless-ctor path, where *managed* code computes
  the cursor rather than us): cursor `ArrayElement(arr, 4)`, end `ArrayElement(arr, 16)`.

Both land as a **plain `ArrayElement` byref with no projections**, so the existing
`blobPointerBounds` helper (which requires exactly that shape) works unchanged on both. This was
the single largest implementation risk and it is now retired.

### Downstream is blocked — this QCall cannot be tested end-to-end yet

This is the fact that decides the PR's scope, and it is **measured, not assumed** (rows 2 and 3 above).
Both directions of the named-arg loop body hit a *different* unimplemented primitive immediately
after this QCall returns:

* **property** → `RuntimeMethodHandle::GetMethodDef` InternalCall. Note this is *past*
  `attributeType.GetProperty(name)`, which now succeeds — an earlier iteration of this plan
  (2026-08-09) recorded `MetadataImport.Enum` with token type `0x17000000` as the blocker here, and
  that has since been implemented (`propertyDefinitionsForTypeDefinition` in
  `NativeMetadataImport.fs`). The blocker moved; it did not go away.
* **field** → `RuntimeFieldHandle_SetValue` QCall (still unimplemented, as in the earlier plan).

There is no third route: the only named-arg shape the managed loop can service without one of these
two is a property whose setter is non-public (`if (!setMethod.IsPublic) continue;`), which C# cannot
express in an attribute application.

So landing this QCall is one link in a chain before a guest can read a named attribute argument.
That is the same shape as the `Assembly.GetName()` QCall chain: this PR gets **unit coverage plus a
parked guest test naming both blockers**, and the guest test un-parks when the later of them lands.
The plan is explicit about this because it changes what "done" means for the PR.

## What CoreCLR does (`customattribute.cpp:1022-1176`)

```
BYTE* pBlob = *ppBlobStart;
if (pBlob + 2 > pBlobEnd) throw CustomAttributeFormatException;

propOrField = *pBlob++;              // 0x53 FIELD -> *pbIsProperty = FALSE
                                     // 0x54 PROPERTY -> *pbIsProperty = TRUE
                                     // anything else -> throw
fieldType   = *pBlob++;              // CorSerializationType
if (fieldType == SZARRAY) { arrayType = *pBlob; bounds-check; pBlob++; }
if (fieldType == ENUM || arrayType == ENUM) {
    // a SerString naming the enum type, resolved against pModule's assembly
    pEnum = GetDataFromBlob(SERIALIZATION_TYPE_TYPE, …);
    if (fieldType == ENUM) pType.Set(enum type) else nullTH = enum type;
}
pName.Set(GetDataFromBlob(SERIALIZATION_TYPE_STRING, …));   // the member name; may be NULL

switch (fieldType) {
  TAGGED_OBJECT: pType.Set(typeof(object)); FALLTHROUGH
  TYPE, STRING:  pValue.Set(GetDataFromBlob(fieldType, …));
                 if (value == NULL) {
                     // Note the test is on fieldType, so a *TAGGED_OBJECT* with a null value
                     // matches neither arm and keeps the typeof(object) set above.
                     if (fieldType == STRING) pType.Set(typeof(string));
                     else if (fieldType == TYPE) pType.Set(typeof(Type));
                 }
                 break;
  SZARRAY:       size = GetDataFromBlob(I4);  // 0xFFFFFFFF = null array
                 if (size != -1) pValue.Set(ReadArray(arrayType, size, …));
                 if (value == NULL) pType.Set(<arrayType>[]);
                 break;
  default:       primitive -> pMTValue = CoreLibBinder::GetElementType((CorElementType)fieldType)
                 (or, for ENUM, fieldType := underlying element type of the enum)
                 val = GetDataFromBlob(fieldType, …);
                 pValue.Set(pMTValue->Box(&val));
}
*ppBlobStart = pBlob;
```

`CorSerializationType` (`corhdr.h:931-949`) aliases `CorElementType` for `BOOLEAN`(0x02)..`R8`(0x0D),
`STRING`(0x0E) and `SZARRAY`(0x1D); `TYPE`=0x50, `TAGGED_OBJECT`=0x51, `FIELD`=0x53,
`PROPERTY`=0x54, `ENUM`=0x55.

Four details worth naming because they are easy to get wrong:

1. `pType` is written **only** in the cases above. For a non-null string, a boxed primitive, or an
   array with elements, it stays null and the managed caller infers the property type from
   `value.GetType()`. The distinction is observable: `type is null` selects
   `attributeType.GetProperty(name)` rather than the type-filtered
   `GetProperty(name, type, Type.EmptyTypes)` overload.
2. For the **in-scope primitives**, the box's type comes from the **blob's** `fieldType` byte, not
   from the decoded value's shape: `U4` boxes as `System.UInt32` even though CLI eval-stack rules
   normalise its value to `Int32`. Scope that claim carefully, because it does **not** generalise to
   `ENUM`: there `pMTValue` is the *resolved enum type* (from the SerString name), and `fieldType` is
   reassigned to the underlying element type only to size the read — so the box is the enum, not its
   underlying primitive. The Enum follow-up will read this document; do not let it read the general
   claim.
3. For `ENUM`, the enum's type name in the blob precedes the member name. Getting that order wrong
   desynchronises the cursor for every subsequent named arg.
4. `pName` may legitimately be NULL (the `0xFF` SerString sentinel); the native code asserts only
   `bObjectCreated || pName.Get() == NULL`. Managed then throws out of `GetProperty(null)`, wrapped
   into `CustomAttributeFormatException`. We should pass null through rather than invent a name.

CoreCLR's SZARRAY bounds check reads `arrayType = *pBlob` *before* testing `pBlob + 1 > pBlobEnd`,
i.e. it over-reads by one byte before checking. We mirror the intended effect (bounds-check, then
read) rather than the literal instruction order; there is no observable difference for a
well-formed blob, and for a malformed one CoreCLR's behaviour is undefined anyway.

## Architectural decisions

### D1. Where the blob decoding lives

* **(a) Pure decoder in `WoofWare.PawPrint.Domain/CustomAttribute.fs`**, returning an inert
  description; the native handler interprets that into heap objects. *(chosen)*
* (b) Decode inline in the native handler, allocating as bytes are consumed.

(a) matches how the fixed-arg reader was built, keeps the parser property-testable without an
`IlMachineState`, and is the data-description-over-behaviour call from the design principles.
(b) would fuse two concerns with completely different failure modes.

### D2. How the decode factors, given that enum widths need type resolution

A named arg is *self-describing* — the blob carries its own `FieldOrPropType` — whereas a fixed arg
takes its type from the ctor signature. But there is one exception that drives the whole factoring:
an **`ENUM` named arg's value width is still not in the blob**. The blob names the enum *type* by a
reflection string; the width comes from that type's `value__`. So the decoder cannot be a single
total `blob -> NamedArg` function, for exactly the reason `readFixedArgs` already can't be.

Main has already solved this shape once: `CustomAttribArgShape` is "a ctor parameter type resolved
to the point where the bytes can be read", and `NativeCustomAttribute.resolveArgShape` does the
resolving beside the machine state.

* **(a) Reuse that same two-step split.** *(chosen)* Three pieces:

  ```fsharp
  /// ECMA-335 II.23.3 `FieldOrPropType`: the serialization type a named argument carries in
  /// the blob itself, as opposed to a fixed argument's, which comes from the ctor signature.
  [<RequireQualifiedAccess>]
  type CustomAttribFieldOrPropType =
      | Primitive of PrimitiveType              // BOOLEAN..R8, STRING
      | SzArray of CustomAttribFieldOrPropType   // 0x1D
      | Enum of typeName : string option         // 0x55, followed by a SerString
      | Type                                     // 0x50
      | TaggedObject                             // 0x51

  [<RequireQualifiedAccess>]
  type CustomAttribNamedArgKind =
      | Field       // 0x53
      | Property    // 0x54

  type CustomAttribNamedArgHeader =
      {
          Kind : CustomAttribNamedArgKind
          ElemType : CustomAttribFieldOrPropType
          /// `None` for the SerString null sentinel; CoreCLR hands the caller a null name.
          Name : string option
      }
  ```

  1. `readNamedArgHeader : ImmutableArray<byte> -> int -> Result<CustomAttribNamedArgHeader * int, string>`
     — pure, and **total over the II.23.3 type grammar** (see D3).
  2. the handler maps `CustomAttribFieldOrPropType -> CustomAttribArgShape`, which is where enum
     resolution *would* go and where the out-of-scope cases fail loudly (D4);
  3. `readElem : CustomAttribArgShape -> ImmutableArray<byte> -> int -> Result<CustomAttribFixedArg * int, string>`
     decodes the value — the **same function** the fixed-arg path uses, since the value encoding is
     byte-identical between fixed and named args (both are ECMA-335 `Elem`).

  (3) requires hoisting the `readOne` closure out of `readFixedArgs` to module level. That is a pure
  mechanical refactor and the existing `TestCustomAttributeBlob.fs` cases keep it honest.

* (b) A self-contained `readNamedArg` that decodes type, name and value in one pass, rejecting
  `Enum` outright. Rejected: it duplicates the value decoder, and it hard-codes "no enums ever"
  into the parser rather than into the resolution step, so adding enum support later would mean
  rewriting the parser instead of extending the resolver.

* (c) Decode the `FieldOrPropType` into a `TypeDefn` and reuse `readFixedArgs` with a one-element
  list. Rejected: `TypeDefn` cannot express "enum named by a reflection string" or `TAGGED_OBJECT`,
  and widening it to do so would corrupt the metadata-signature type for the benefit of one blob
  format.

### D3. How much of the type grammar the *decoder* understands

* **(a) Total over the II.23.3 `FieldOrPropType` grammar** — all five forms above get a DU case and
  are read correctly, including the `SzArray` element byte and the `Enum` type-name SerString.
  Partiality lives downstream, in shape resolution and value lowering. *(chosen)*
* (b) Decoder understands `Primitive` only; every other tag byte is an `Error`.

(a) is chosen because the type grammar is a small, fully-specified byte grammar — making the reader
partial is the odd choice, and it costs about ten lines to be total. It also buys precise
diagnostics for free: the handler can say "TAGGED_OBJECT named args are not supported" rather than
"unknown serialization type byte 0x51", and it can say so *knowing the cursor position*. And it
means the `Enum` type-name string is consumed at the right point in the cursor walk (detail 3
above) the day resolution lands, rather than that ordering being re-derived later.

This is not speculative generality: each of the three "unsupported" cases feeds a *tested* `failwith`
diagnostic plus decoder tests, so none is dead code. (The "precise diagnostics" half of the argument
is the weaker half — a partial parser also knows its offset. The stronger half is that the
`Enum` arm's SerString consumption pins the cursor ordering under test *now*, rather than that
ordering being re-derived when resolution lands.)

**Caveat: the recursive `SzArray` is more liberal than CoreCLR, and the resolution step must be the
thing that says so.** ECMA-335's `FieldOrPropType` grammar reads as recursive, but CoreCLR's
named-arg path reads exactly *one* `arrayType` byte (`customattribute.cpp:1066`) and only rejects a
nested `SZARRAY` later, inside `ReadArray`'s `th.IsNull() → badBlob` branch (line ~592) — i.e. after
it has already consumed the member name and the element count, so the cursor trajectory differs too.
For every element type CoreCLR *accepts*, the one-byte read and the recursion coincide (including
`Enum`, whose name is read at the same point). There is no observable divergence in this PR, because
SzArray named args refuse loudly either way. But when SzArray support lands, the **shape-resolution
step must reject a nested `SzArray`** rather than assume the decoder did; record that here so it is
not rediscovered.

Relatedly, `Primitive of PrimitiveType` over-admits — `PrimitiveType.IntPtr`, `Object`,
`TypedReference` have no `CorSerializationType` byte and so cannot be produced by
`readFieldOrPropType`. That matches the precedent already set by `CustomAttribArgShape` and is
guarded by `readPrimitiveValue`'s error arm, so it is acceptable; but note that arm's message
currently says "as a CustomAttrib fixed-arg" (`CustomAttribute.fs:590`), which becomes wrong the
moment the named-arg path shares it. **Generalise that message as part of the hoist in step A1.**

### D4. Which named args this PR supports end-to-end

In scope, fully: **`Primitive`** — `BOOLEAN, CHAR, I1, U1, I2, U2, I4, U4, I8, U8, R4, R8, STRING`.
That is the whole of `[Foo(Flag = true, Name = "x", Count = 3)]`, which is what almost every named
argument in practice is, and it is implementable with helpers that already exist.

Out of scope, each failing loudly with a message naming the exact construct and why:

* **`SzArray`** — the underlying gap is that `CustomAttribValueLowering` cannot allocate a managed
  array from a `CustomAttribFixedArg.Array` (`CustomAttribValueLowering.fs:93-96` `failwith`s
  today). That same gap blocks the **fixed-arg** path, so it should be one PR that fixes both, not
  half a fix wedged in here.

  **All four out-of-scope constructs are refused at the same place: the shape-mapping step, each
  with its own named-arg-specific message.** `SzArray` must *not* be allowed to decode and then
  trip the lowering `failwith`, for three reasons: that message says nothing about named args, so
  the diagnostic would misattribute; it would be the only one of the four failing at a different
  place, so the "each construct fails with its own message" test would be asserting two different
  things; and the null-array sentinel (`NumElem = 0xFFFFFFFF` → `CustomAttribFixedArg.Array None`)
  reaches that same lowering arm, so even the *degenerate* array would fail there rather than here.
  One refusal point, four messages.
* **`Enum`** — needs `TypeName::GetTypeReferencedByCustomAttribute`: a reflection type-name parser
  plus assembly-qualified resolution against `pModule`'s assembly. Confirmed by grep that **no such
  resolver exists anywhere in the codebase today**. It is a substantial feature of its own, and is
  the only reason `pModule` is a parameter at all.
* **`Type`**, **`TaggedObject`** — same resolver dependency (`Type`), plus recursion (`TaggedObject`).

### D5. `pType` when CoreCLR leaves it alone

The managed wrapper pre-nulls its `typeLocal` before the call
(`RuntimeCustomAttributeData.cs:1908-1910`), so "write null" and "leave untouched" are
indistinguishable *to the caller*. This PR **writes null explicitly**, so the handler's contract
does not depend on a caller-side initialisation it cannot see. The one in-scope case where a real
type is written is `STRING` with the null sentinel → `typeof(string)`.

**This decision only means something if a test can fail when it is violated, and by default no such
test exists.** The unit fixture's slot builders start every out-slot null too
(`TestNativeCustomAttribute.fs:144-156`, callers passing `CliType.ObjectRef None`), so a handler
that never touches the type slot passes every "type slot is null" assertion — the assertion would be
reading back the fixture's own initialisation, not the handler's behaviour. That is precisely the
vacuity this project has been burned by before.

So the test plan **must seed every out-slot (type, name, value) with a non-null sentinel** and
assert the handler overwrote it — including overwriting it *to null*. Without that, D5 is an
unenforced comment and should not be claimed as a contract at all.

### D6. Malformed blob: `failwith` or a guest `CustomAttributeFormatException`?

CoreCLR throws `CustomAttributeFormatException`, which is catchable, and which `AddCustomAttributes`
deliberately lets escape (it even wraps setter failures into one).

* **(a) `failwith` with a precise message.** *(chosen)*
* (b) Raise the guest exception via the `raise-guest-exception` skill.

(a) for consistency: the sibling `CreateCustomAttributeInstance` handler already `failwith`s on
precisely this class of error ("failed to parse fixed args from CustomAttrib blob"), and a
`CustomAttrib` blob that violates II.23.3 cannot be produced by a compiler — it needs hand-written
IL. Splitting the two handlers' behaviour would be worse than either choice applied consistently.

**If you prefer (b), it should be a separate change that converts both handlers together.** Flagged
rather than silently decided, because it is a user-visible fidelity question.

### D7. Single-phase handler, no re-entry marker

Unlike `CreateCustomAttributeInstance`, nothing in the D4 scope runs guest code or triggers class
initialisation: `boxValueType` allocates without running a cctor (matching `MethodTable::Box`),
`concretizeType` on a `PrimitiveType` is a pure lookup, and `getOrAllocateType` returns
`ManagedHeapAddress * IlMachineState` with no suspension path. So the handler completes in one step
and writes `*ppBlobStart` last, as CoreCLR does. Adding `Enum` support later reintroduces a type
load and may reintroduce the two-phase structure; called out in the follow-ups.

## Implementation

### A. `WoofWare.PawPrint.Domain/CustomAttribute.fs`

1. Hoist the `readOne` closure (and the `readPrimitiveValue`/`readPrimitive`/`readUInt32` helpers it
   closes over) out of `readFixedArgs` to module level as `internal readElem`. Pure refactor;
   `readFixedArgs` becomes the prolog check plus a fold over `readElem`. While hoisting, generalise
   `readPrimitiveValue`'s error message (`CustomAttribute.fs:590`), which currently says "as a
   CustomAttrib fixed-arg" and becomes wrong the moment the named-arg path shares the function.
2. Add `CustomAttribFieldOrPropType`, `CustomAttribNamedArgKind`, `CustomAttribNamedArgHeader` (D2).
3. Add `readFieldOrPropType : ImmutableArray<byte> -> int -> Result<CustomAttribFieldOrPropType * int, string>`
   — the byte→`PrimitiveType` table (`PrimitiveType` is a plain DU with no ECMA numbering, so the
   table is explicit), `0x1D` recursing for the element type, `0x55` reading a `SerString` name,
   `0x50`/`0x51` as leaves.
4. Add `readNamedArgHeader` (D2): `0x53`/`0x54` → kind; `readFieldOrPropType`; `readSerString` for
   the name. Ordering per CoreCLR: **type (including any enum name) before member name**.

These operate on a whole-blob `ImmutableArray<byte>` with an offset, matching the existing readers;
the handler materialises the `[cursor, end)` slice exactly as `CreateCustomAttributeInstance` does.

### B. `WoofWare.PawPrint/Native/NativeCustomAttribute.fs`

A second arm in the existing `match`, keyed on `"CustomAttribute_CreatePropertyOrFieldData"` plus
the seven-parameter signature. The existing comment about the Roslyn-mangled stub name already
covers this entry point (`…|32_0`).

```
operation = "CustomAttribute.CreatePropertyOrFieldData"

decode arg0 QCallModule           -> decode-and-ignore, exactly as the sibling handler does, so
                                     argument positions don't shuffle when Enum/Type land
arg1 -> blobCursorSlot            (managedPointerOfPointerArgument, then read through it for *ppBlobStart)
arg2 -> blobEndPtr                (IntPtr by value)
arg3 -> nameHandle                (NativeCall.stringHandleOnStackTarget)
arg4 -> isPropertySlot            (managedPointerOfPointerArgument; int32 cell)
arg5 -> typeHandleSlot            (NativeCall.objectHandleOnStackTarget)
arg6 -> valueHandleSlot           (NativeCall.objectHandleOnStackTarget)

blobPointerBounds on cursor and end  (reuse; both shapes confirmed by the probe above)
same-array check                     (reuse the existing diagnostic)
materialiseBytes [start, end)        (reuse)

CustomAttribute.readNamedArgHeader bytes 0   -> Error msg -> failwith "%s{operation}: %s{msg}"
map header.ElemType -> CustomAttribArgShape  -> the SOLE refusal point for all four out-of-scope
                                                constructs (SzArray, Enum, Type, TaggedObject),
                                                each with its own message, per D4
CustomAttribute.readElem shape bytes offset  -> Error msg -> failwith

write isProperty      (Int32 0/1)
write name            (null | canonical empty string | fresh string, per internCanonicalEmptyString)
write value:
   String None   -> ObjectRef None
   String (Some) -> the string object itself (no boxing)
   numeric       -> concretizeType (TypeDefn.PrimitiveType p) for the *blob's* type byte,
                    CustomAttribValueLowering.toCliType for the value,
                    UnaryMetadataObjectOps.boxValueType   (EvalStackValue.toCliTypeCoerced inside it
                                                           narrows the eval-stack value into the
                                                           box's field shape, e.g. Int32 -> Bool)
write type:
   String None -> IlMachineState.getOrAllocateType (Closed <System.String handle>)
   otherwise   -> ObjectRef None
write *ppBlobStart = Byref (ArrayElement (arr, startIdx + consumed), [])
NativeHandlerResult.completed
```

The box's corelib type comes from the **blob's** type byte, not the decoded value's F# shape (detail
2 above). This mirrors `CoreLibBinder::GetElementType((CorElementType)fieldType)`.

### C. `WoofWare.PawPrint/Native/NativeQCall.fs`

One entry alongside the existing one:

```fsharp
"CustomAttribute_CreatePropertyOrFieldData",
NativeCustomAttribute.tryExecuteQCall "CustomAttribute_CreatePropertyOrFieldData"
```

## Tests

### 1. Decoder (`WoofWare.PawPrint.Test/TestCustomAttributeBlob.fs`)

* **Outside oracle** *(the strongest of these, and the reason to write it first)*: Roslyn-compile a
  C# attribute application carrying several named arguments, read the raw blob with
  `MetadataReader`, and compare our `readNamedArgHeader` + `readElem` output against
  `System.Reflection.Metadata`'s own `CustomAttributeDecoder` (`NamedArguments`: name, kind, type,
  value). A round-trip against a reference encoder *we also wrote* cannot catch a shared
  misreading of II.23.3; this can. The test project already has `Roslyn.compile`.

  `CustomAttribute.DecodeValue<TType>(ICustomAttributeTypeProvider<TType>)` is public and
  `CustomAttributeNamedArgument<TType>` exposes exactly `Name`/`Kind`/`Type`/`Value`, so the
  comparison is available; for a primitives-plus-string corpus the provider needs only its
  simple-type and SZ-array members, and no enum resolution. Two mechanics: SRM decodes the *whole*
  blob, so the test locates the named-args region by first calling `readFixedArgs` (separately
  tested code, and a wrong offset produces loud garbage rather than a quiet pass); and the test
  must **assert the final cursor lands exactly on blob end**, mirroring the managed
  `blobStart != blobEnd` check that a real guest gets for free.
* **Round-trip property**: generate a named-arg header + value over the supported alphabet, encode
  with a reference encoder, decode, assert equality. Generators must cover the full numeric range
  (`Gen.choose`, **not** FsCheck's size-bounded default `int`) and must include the empty string,
  the null string, and non-ASCII names — a narrowed alphabet is exactly what hides an
  encoder-and-decoder-agree bug. Note the float generators inherited from the fixed-arg tests are
  `NormalFloat`, so NaN payloads are invisible; generating floats from raw bits and comparing
  bitwise would close that. Pre-existing pattern, so not a blocker for this PR, but worth doing
  while the code is open.
* **Grammar totality**: `readFieldOrPropType` decodes each of the five forms, including nested
  `SzArray (SzArray (Primitive …))` and `Enum` with an assembly-qualified name.
* **Enum header, two distinct strings**: a header carrying *both* an enum type-name SerString and a
  member-name SerString, with content assertions on both fields. This is the only test that can kill
  a field-swapped `readNamedArgHeader`: both fields are SerStrings, so a swapped implementation
  decodes cleanly with the values exchanged, and the truncation cases cannot see it. Mutation (v)
  below depends on this case existing.
* Unit cases: field vs property tag; each supported primitive; empty string; null-sentinel string;
  null name; invalid kind byte; invalid type byte; truncation at **every** boundary (before the
  kind tag, before the type byte, mid-`SzArray` element byte, mid-enum-name, mid-name, mid-value).

### 2. Handler (`WoofWare.PawPrint.Test/TestNativeCustomAttribute.fs`)

The existing `Fixture` supplies a corelib + Roslyn guest assembly, a `QCallModule` value,
`ObjectHandleOnStack` values and an `int32` out-slot. Three pieces of real fixture work are needed —
this is *not* pure reuse:

* a `StringHandleOnStack` builder (structurally a near-copy of `objectHandleOnStackValue`, since
  both are a `_ptr` field);
* `allocateBlobArrays` currently hard-codes the 14-byte ctor blob and a cursor of 0
  (`TestNativeCustomAttribute.fs:164-223`) — it must take the bytes and the starting cursor index as
  parameters, since the whole point of several cases below is a non-zero cursor;
* `findQCallStub` hard-codes the entry-point name (lines 245-264) and must be parameterised.

**Every out-slot (type, name, value) is seeded with a non-null sentinel before each call**, per D5,
so that "the handler wrote null" is distinguishable from "the handler did nothing".

Then assert, for hand-built blobs:

* property named arg with a string value: `isProperty = 1`, name string contents, value is that
  string object, `type` slot null, cursor advanced to exactly the end of the named arg;
* field named arg with an `Int32` value: `isProperty = 0`, value is a **box of `System.Int32`**
  (assert the box's concrete type, not just its payload), `type` slot null;
* **box-type matrix** — `U4`, `Boolean`, `Char`, `R4`, `U8` box as `System.UInt32`,
  `System.Boolean`, `System.Char`, `System.Single`, `System.UInt64`, asserting both the box's
  concrete type *and* its payload. Each row is a distinct way for "take the box type from the
  decoded F# value rather than the blob byte" to survive an Int32-only test, and they are not
  redundant with one another: `U4`/`U8` are the cases CLI eval-stack rules normalise to
  `Int32`/`Int64` (`CustomAttribValueLowering.fs:43,45`); `Boolean`/`Char` are sub-word narrowings;
  and `R4` is the float width, where the eval stack has a *single* float type, so
  `toCliTypeCoerced` narrowing to `float32` inside a `System.Single` box is a failure mode no
  integer row exercises;
* null-string value: `value` null **and** `type` = `typeof(string)`;
* empty-string value: routes through the canonical interned empty string;
* null name sentinel: `name` slot null;
* **two named args back-to-back**: feeding the first call's written cursor into a second call
  decodes the second arg — the property a cursor-arithmetic bug breaks and a single-arg test cannot
  see. (The real guest gets this check for free from `blobStart != blobEnd`; the unit test is what
  we have until the guest test un-parks.)
* each out-of-scope construct (`SzArray`, `Enum`, `Type`, `TaggedObject`) fails with *its own*
  message rather than mis-parsing or falling into a neighbouring arm.

**Mutation check before claiming coverage** — break each of these one at a time and confirm a
*named* test fails for each: (i) the cursor write-back (leave `*ppBlobStart` unchanged), (ii) the
`isProperty` polarity, (iii) the box type taken from the value instead of the blob byte, (iv) the
`type`-slot write for the null-string case, (v) the header field order (type-name before member
name — needs the two-distinct-strings case above to be killable), and (vi)–(viii) the three
*leave-untouched* mutants: delete the null write to the type slot, to the name slot, and to the
value slot in turn. (vi)–(viii) are the ones that only fail if the sentinel seeding above is
actually in place, so they double as the check that the seeding works.

### 3. Parked guest test

`WoofWare.PawPrint.Test/sourcesPure/CustomAttributeNamedArgs.cs` — an attribute with one string
property and one int field, read via `typeof(D).GetCustomAttributes(typeof(MarkerAttribute), false)`
— added to `TestPureCases.unimplemented` with a comment naming **both measured blockers**
(`RuntimeMethodHandle::GetMethodDef` InternalCall for the property route; the
`RuntimeFieldHandle_SetValue` QCall for the field route) and the fact that this QCall itself is
implemented.

The parked file must **assert the values**, not merely that the call returns, or un-parking is
meaningless. Two things to verify rather than assume, both cheap:

* run it on real .NET through the parked fixture and confirm the expected exit code is what the
  oracle actually produces (the `unimplemented` fixture runs real .NET only, so it validates the
  expectation even while parked);
* run it un-parked once, by hand, and confirm it fails at one of the two named blockers and not
  somewhere else.

### 4. One known divergence to comment, not to fix

CoreCLR boxes a `BOOLEAN` named arg's **raw blob byte**, so a hand-crafted blob can produce a
`System.Boolean` box containing 2. `readPrimitiveValue` normalises to a `bool` and hence to 0/1.
The fixed-arg path already behaves this way, so the named-arg path does not make it worse, and only
hand-written IL can reach it. Leave a comment at the decode site so nobody rediscovers it and
mistakes it for a new bug.

## Out of scope (each its own change)

1. **`SzArray` named args** — needs `CustomAttribValueLowering` to allocate a managed array; the
   fixed-arg path needs the same thing, so fix both together. That change must also make the
   shape-resolution step **reject a nested `SzArray`**, which CoreCLR refuses on this path (see the
   caveat in D3); the decoder's recursion will happily hand one over.
2. **`Enum` and `Type` named args** — needs the custom-attribute type-name resolver
   (`TypeName::GetTypeReferencedByCustomAttribute`), which does not exist in the codebase at all
   today, and is the only consumer of the `pModule` argument. May reintroduce a class-init
   suspension and hence a two-phase handler (D7).
3. **`TaggedObject` (`object`-typed) named args** — recursive; depends on 1 and 2.
4. **`RuntimeMethodHandle::GetMethodDef`** and **`RuntimeFieldHandle_SetValue`** — the two measured
   downstream blockers. Landing both un-parks the guest test.
5. Converting blob-format failures to a guest `CustomAttributeFormatException` (D6), for both
   handlers.

## Validation

* `nix develop -c dotnet test … --filter "FullyQualifiedName~TestCustomAttributeBlob"` and
  `…~TestNativeCustomAttribute`.
* Full suite (the parked guest file must not disturb `TestPureCases`).
* `nix develop -c dotnet fantomas .` **from the worktree root** — `.fantomasignore` is
  root-relative, so running it from the main checkout mangles files in every worktree.
* Re-run the `GetCustomAttributes(attrType, false)` playground program and confirm the failure has
  moved *past* this QCall to `RuntimeMethodHandle::GetMethodDef` (property) or
  `RuntimeFieldHandle_SetValue` (field) — the concrete proof the handler ran, and the same probe
  the reachability table above was measured with.
* `codex review --base origin/main`.
