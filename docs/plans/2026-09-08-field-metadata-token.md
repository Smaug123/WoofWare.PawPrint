# `RuntimeFieldHandle::GetToken`, a field's `mdFieldDef` row

Rung J of the ASP.NET ladder (`docs/plans/2026-08-17-aspnet-critical-path.md`, local branch
`aspnet-spike`) binds its route parameter, runs the handler and closes System.Text.Json's
converter type. This is the plan for the stop after that.

## What is measured

Rung J under the linux CoreLib flavour, on `main` at `0fca2af6` (so with #1415, #1417 and
#1418 in), stops 38 frames out from `Main` at

```
Unimplemented native method (InternalCall):
  System.Private.CoreLib System.RuntimeFieldHandle::GetToken(System.IntPtr) -> System.Int32
```

The route is `RtFieldInfo.MetadataToken` (RtFieldInfo.cs:62) called from
`CustomAttribute.GetCustomAttributes` on a field, while System.Text.Json's
`DefaultJsonTypeInfoResolver.AddMembersDeclaredBySuperType` looks for attributes on the fields
of the handler's result type.

## What CoreCLR does

`RuntimeFieldHandle.GetToken(IntPtr fieldDesc)` (RuntimeHandles.cs:1571) is an `InternalCall`
whose native side is one line (runtimehandles.cpp:2205):

```cpp
INT32 tkFieldDef = (INT32)pField->GetMemberDef();
_ASSERTE(!IsNilToken(tkFieldDef) || tkFieldDef == mdFieldDefNil);
return tkFieldDef;
```

`GetMemberDef` reads a token stored on the `FieldDesc` at construction; it looks nothing up.
The argument is the raw `IntPtr` that `RtFieldInfo` holds in `m_fieldHandle` (RtFieldInfo.cs:16),
which under PawPrint is a `NativeIntSource.FieldHandlePtr` — the same value every other
`IntPtr fieldDesc` native in `NativeRuntimeFieldHandle.fs` already unpacks through
`fieldHandleOfRuntimeFieldHandleInternal`.

Measured on real .NET 10 (`scratchpad/fieldtoken`, throwaway):

| field | token |
| --- | --- |
| `Point.X`, `Point.Y` (guest struct, declared in that order) | `0x04000001`, `0x04000002` |
| `Gen<>.Value`, `Gen<int>.Value`, `Gen<string>.Value` | `0x04000003` for all three |
| `Gen<int>.Count` | `0x04000004` |
| `Holder.Instance` / `Secret` / `Static` / `Constant` | `0x04000005`–`0x04000008` |
| `string.Empty` | `0x04000313` |
| `DayOfWeek.value__` then its seven members | `0x04000390`–`0x04000397` |

Two facts from that table matter: the token is `mdtFieldDef | rid`, and **it does not depend on
the instantiation** — the open definition and two closed instantiations of `Gen<T>` all report
one token, because in CoreCLR the FieldDescs live on the canonical (`__Canon`) MethodTable.

## Decisions

### 1. Does the declaring type participate in the answer?

PawPrint's `FieldHandle` (FieldHandleRegistry.fs:7) is keyed on *both* the FieldDef row and the
`RuntimeTypeHandleTarget` observed at allocation, deliberately: `typeof(G<int>).GetField(..)
.FieldHandle` is observably not interchangeable with `typeof(G<>).GetField(..).FieldHandle`, and
the two get distinct registry ids. So a handler has a choice.

**A. Answer from the FieldDef row alone, ignoring `DeclaringType`.** Matches the measurement
above exactly. The risk is that it throws away a distinction the registry went to trouble to
keep — but that distinction is about *handle identity*, not about which metadata row the field
is, and this native asks the second question.

**B. Refuse for `OpenGenericTypeDefinition` until a guest needs it.** Conservative in the
crash-over-divergence sense, and wrong: the measurement shows the open definition has an answer,
and `typeof(Gen<>).GetField("Value").MetadataToken` is a thing a guest can write today.

**Chosen: A.** B would refuse a shape real .NET answers, which is a divergence dressed as
caution.

### 2. Is there a nil-token arm?

`NativeRuntimeMethodHandle.methodDefToken` has one, because `MethodHandle` has a `FromDynamic`
case that no MethodDef row names, and `RuntimeParameterInfo.GetParameters` relies on the nil
answer. `FieldHandle` has no such case: it is a private record whose `FieldHandle` field is a
`ComparableFieldDefinitionHandle`, so a rowless field is unrepresentable.

**Chosen: no nil arm and no `mdFieldDefNil` constant.** Adding one would be a constant with no
reachable caller, and the type system already enforces what it would be guarding.

### 3. Placement

A named `fieldDefinitionToken : FieldHandle -> int32` in `NativeRuntimeFieldHandle`, beside the
existing `fieldHandleOfRuntimeFieldHandleInternal`, mirroring
`NativeRuntimeTypeHelpers.typeDefinitionToken` and `NativeRuntimeMethodHandle.methodDefToken`.
The alternative — inlining two lines into the match arm — puts the CoreCLR contract and the
instantiation-independence measurement inside a `match` arm where the next reader of
`FieldHandle` will not find them.

## Tests

`sourcesPure/FieldMetadataToken.cs` (differential; every check is a relation between tokens, so
the guest says the same thing on both runtimes without depending on a literal row number):

1. the tag byte is `mdtFieldDef` for instance, private, static and literal fields;
2. those four are four distinct rows (an answer derived from the declaring type would collapse
   them);
3. `Gen<>`, `Gen<int>` and `Gen<string>` report one token for `Value` — decision 1's measured
   fact, and the check that a registry-id-shaped answer fails, since those are three ids;
4. `Gen<int>.Count` is still a different row from `Gen<int>.Value`;
5. the token is stable across calls and across two separately-obtained `FieldInfo`s;
6. corelib fields answer too, and `int.MaxValue` differs from `int.MinValue`;
7. `Point.X` and `Point.Y` occupy *consecutive* rows. This is the one image-dependent check and
   it earns its place: everything above passes for any injective function of the field, and only
   this pins the answer to the metadata row. Roslyn emits a type's FieldDef rows in declaration
   order; if that ever changes the check fails on both runtimes at once, visibly;
8. the consumer that reaches the InternalCall: two adjacent fields carry `[Marker]` with
   different arguments and the field between them carries none, so `GetCustomAttributes` —
   which looks the token up in the CustomAttribute table by `Parent` — returns the *wrong
   field's* attributes rather than failing if the row is off by one.

## Outcome

Checks 1-7 above are as planned. Check 8 is new, and came out of the battery rather than the
plan: mutating the handler to `token + 1` died at check 2, which a uniform shift cannot cause,
and the reason turned out to be that `Constant`'s token never went through the handler at all.
A literal field has no FieldDesc, so CoreCLR reflects it as `MdFieldInfo`, whose `MetadataToken`
is the token it read straight out of metadata (MdFieldInfo.cs:44). Declaring the literal
immediately after `Static` therefore gives the guest an oracle for the *absolute* row number
rather than only for relations between tokens the native itself produced, so that accident was
made deliberate: check 8 asserts the adjacency, and mutant m5 exists to exercise it.

Mutants, each run against the 123 tests matching
`TestCategory=Guest&(Name~FieldMetadataToken|Name~Attribute|Name~Field|Name~Reflect)`, with
`FieldMetadataToken.cs` the only failure in every case:

| mutant | outcome | died at |
| --- | --- | --- |
| m1: the declaring type's token wearing the field tag | killed | check 2 |
| m2: an injective function of the handle (the shape a registry id has) | killed | check 3 |
| m3: `token + 1` | killed | check 2 |
| m4: the right row under the `mdtMethodDef` tag | killed | check 1 |
| m5: `token + 0x1000`, far enough not to collide with the literal's row | killed | check 8 |

## Ladder

Rung J advances 7 frames. It now stops 45 frames out from `Main` in `Utf8.FromUtf16`:
"refusing to subtract array element pointer from incompatible pointer,
`<element 15 of array <object #70560>>` vs `<<element 13 of array <object #70560>> as
System.Byte>`" -- the JSON writer transcoding the response body. That is a byref-model question
rather than a native, and is recorded as the next stop.
