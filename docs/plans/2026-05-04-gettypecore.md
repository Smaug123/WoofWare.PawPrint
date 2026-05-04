# Implement AssemblyNative_GetTypeCore QCall

## Background

The C++ implementation in dotnet/src/runtime/src/coreclr/vm/assemblynative.cpp:337-403 works like this: split szTypeName at the last . into namespace + simple name, look up that top-level type in the assembly's
class loader, then for each entry in rgszNestedTypeNames walk one nesting level deeper. The result type (or null) is returned via retType.Set(...).

Managed entry point: RuntimeAssembly.cs:189-194 declares the QCall with LibraryImport(... StringMarshalling = StringMarshalling.Utf8), so the parameters arriving at the QCall are:

┌─────┬─────────────────────┬─────────────────────────────────────┐
│  #  │        Type         │               Meaning               │
├─────┼─────────────────────┼─────────────────────────────────────┤
│ 0   │ QCallAssembly       │ byref the RuntimeAssembly           │
├─────┼─────────────────────┼─────────────────────────────────────┤
│ 1   │ byte*               │ UTF-8 NUL-terminated typeName       │
├─────┼─────────────────────┼─────────────────────────────────────┤
│ 2   │ IntPtr*             │ array of byte*, one per nested name │
├─────┼─────────────────────┼─────────────────────────────────────┤
│ 3   │ int32               │ length of (2)                       │
├─────┼─────────────────────┼─────────────────────────────────────┤
│ 4   │ ObjectHandleOnStack │ out Type?                           │
└─────┴─────────────────────┴─────────────────────────────────────┘

The PawPrint test (InitializeArrayBoxedFieldHandle.cs) only exercises the case nestedCount = 0 looking up <PrivateImplementationDetails> (empty namespace, top-level), but the implementation should be general
enough to handle nested chains — it falls out naturally from the loop.

Files to change

1. WoofWare.PawPrint/Native/NativeCall.fs — add a readNullTerminatedUtf8 helper that mirrors readNullTerminatedUtf16 (line 166) but reads bytes via IlMachineState.readManagedByrefBytesAs … (UInt8 0uy) and
ManagedPointerByteView.addByteOffset with baseClassTypes.Byte, terminating on 0uy. Reuse the same defensive scan limit. UTF-8-decode at the end with System.Text.Encoding.UTF8.GetString.
2. WoofWare.PawPrint/Native/NativeRuntimeAssembly.fs — extend tryExecuteQCall with a new "AssemblyNative_GetTypeCore" arm, parameter shape:

[ ConcreteType … "QCallAssembly" qCallAssemblyGenerics
ConcretePointer (ConcretePrimitive … PrimitiveType.Byte)
ConcretePointer (ConcretePrimitive … PrimitiveType.IntPtr)
ConcretePrimitive … PrimitiveType.Int32
ConcreteType … "ObjectHandleOnStack" objectHandleGenerics ]
2. returning Void. Guard with qCallAssemblyGenerics.IsEmpty && objectHandleGenerics.IsEmpty.

## Step-by-step body

1. Pull the five args off instruction.Arguments and decode using existing helpers:
- qCallAssemblyToAssemblyFullName → assemblyFullName
- managedPointerOfPointerArgument for typeNamePtr and nestedNamesPtr (both must be non-null when nestedCount > 0; nestedNamesPtr may be null when nestedCount = 0)
- int32Argument for nestedCount
- objectHandleOnStackTarget for retType
2. Read typeName as UTF-8 via the new helper.
3. Build a string list of fully-qualified names: head = typeName, tail = nestedCount names read at successive IntPtr-sized offsets from nestedNamesPtr. Each offset uses ManagedPointerByteView.addByteOffset with
BaseClassTypes.IntPtr (or just compute i * sizeof<nativeint>); read each entry as IntPtr (i.e., CliType.Numeric (NativeInt …)), unwrap to a ManagedPointerSource, then UTF-8-read the pointee.
4. Resolve the top-level type:
- Split typeName at the last . → (ns, simple). If no ., ns = "".
- state.LoadedAssembly' assemblyFullName (fail loudly if not loaded — should always be, since m_assembly came from a loaded handle).
- assembly.TryGetTopLevelTypeDef ns simple. On None push nothing — leave retType untouched (caller's local was preinitialized to null).
- Note: do not silently fall back to TryGetTopLevelExportedType here. Type forwarding in the manifest is genuinely a separate path (CoreCLR's LoadTypeHandleThrowing follows it). Mark with an explicit failwith
$"TODO: AssemblyNative_GetTypeCore type forwarding for %s{name}" in a follow-up arm if TryGetTopLevelTypeDef misses but TryGetTopLevelExportedType hits — for this PR, the test doesn't exercise it, so just None →
leave retType null is acceptable.
5. Walk nested chain: for each subsequent name, split at . likewise, then assembly.TryGetNestedTypeDef parent.TypeDefHandle simple. (CoreCLR also re-roots at the forwarded assembly's loader after a forward, but
again skip for now.)
6. Allocate the RuntimeType:
- If the resolved TypeInfo<_,_>.Generics is empty: concretize via IlMachineTypeResolution.concretizeType … assembly.Name ImmutableArray.Empty ImmutableArray.Empty (TypeDefn.FromDefinition
(ResolvedTypeIdentity.ofTypeDefinition assembly.Name typeInfo.TypeDefHandle, SignatureTypeKind.Class|ValueType)) (kind picked from typeInfo.IsValueType if that exists; otherwise Class is fine for our test). Then
IlMachineState.getOrAllocateType with RuntimeTypeHandleTarget.Closed handle.
- If Generics is non-empty: use RuntimeTypeHandleTarget.OpenGenericTypeDefinition (ResolvedTypeIdentity.ofTypeDefinition assembly.Name typeInfo.TypeDefHandle) straight into getOrAllocateType. (Matches
typeof(List<>) semantics — generic type definition, not a constructed type.)
7. IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state retType (CliType.ObjectRef (Some addr)). Push nothing else; the QCall is void.
8. Return (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some.

Edge-cases / explicit TODOs

- Null typeNamePtr: failwith "TODO: AssemblyNative_GetTypeCore with null typeName should throw ArgumentNullException". (CoreCLR's PRECONDITION(CheckPointer(szTypeName)) is a debug assert; managed callers always
pass non-null.)
- nestedCount > 0 && nestedNamesPtr = Null: failwith — invariant violation by caller.
- Type-forwarder hit (exported types): leave a failwith $"TODO: ..." branch for the case where TryGetTopLevelTypeDef returns None but TryGetTopLevelExportedType returns Some. Not exercised by the test.
- AssemblyNative_GetTypeCoreIgnoreCase: matching QCall but UTF-16 + lowercase, called only when ignoreCase: true. Not needed for this test; can be added later as a sibling arm reusing readNullTerminatedUtf16.
- Case-sensitive . split: use the last ., matching ns::FindSep. CoreCLR's FindSep walks back from end.

Verification

1. nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "Name~InitializeArrayBoxedFieldHandle" should now pass (and exercise RuntimeHelpers.InitializeArray end-to-end via the
boxed RuntimeFieldHandle path the original test was set up for).
2. Run the full pure-test suite to confirm no regressions, since RuntimeAssembly.GetType(string) is on a hot path that other tests may now silently start hitting.
3. nix develop -c dotnet fantomas . to format.

Out of scope (deliberately)

- The IgnoreCase variant.
- Following type forwarders / re-rooting pClassLoader after a forward.
- Returning the constructed Type for already-instantiated generics (none of the BCL paths I saw call GetTypeCore with constructed-generic syntax — generic instantiation goes via Type.MakeGenericType).

