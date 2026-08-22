namespace WoofWare.Pawprint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPureCases =
    let assy = typeof<RunResult>.Assembly

    let unimplemented =
        [
            "ValueTypeHashCodeOverlappingReferenceFields.cs" // Explicit layout may put two *reference* fields at the same offset -- the GC sees one pointer slot, so the type loads, and real .NET hashes whichever object the slot holds (measured: exits 0). PawPrint cannot reach it, and not because of anything in `ValueType_GetHashCodeStrategy`: an explicit-layout struct with overlapping reference fields has no access route at all. `CliValueType.DereferenceFieldById` sees more than one field covering the range and falls back to rendering the bytes, and `CliType.ToBytes` refuses a non-null reference because PawPrint models one as an opaque handle rather than an address. That is the same dead end as `RuntimeHelpersBoxReferenceContainingStruct.cs`, `ReinterpretCellUnderAliasedAncestor.cs` and `BulkMoveAcrossOverlappedStructPadding.cs`. Note that answering the strategy's nullness question some other way would not be enough: the guest's own `Unsafe.As<byte, object>(ref rawData + fieldOffset)` read then hits the same wall, because the two aliased cells make `tryReadHeapValueFieldPrecise`'s uniqueness gate fail and the byte walk refuses a reference-containing payload. Un-park when a reference cell can be named through an aliased explicit-layout offset. Verified to exit 0 on real .NET.
            "ReflectionFieldSetValueFailingCctor.cs" // A reflective field set whose declaring type's initialiser throws. `InvokeUtil::SetValidField` runs the initialiser inside an `EX_TRY` and rethrows the failure wrapped in a *fresh* `TargetInvocationException` (`CreateTargetExcept`, invokeutil.cpp:803), so the guest's `catch (TargetInvocationException)` fires and the inner exception is the `TypeInitializationException`. Note this is the opposite of the sibling `ReflectionInvocation_RunClassConstructor` QCall, which deliberately lets the TIE through unwrapped (reflectioninvocation.cpp:1226) — the suspension plumbing is shared between the two handlers but the exception contract is not. Measured rather than assumed: real .NET exits 0, and PawPrint reports "threw unhandled exception" because the bare `TypeInitializationException` does not match the guest's `catch`. Not fixable inside the handler as it stands. `RuntimeFieldHandle_SetValue` returns `suspendedForClassInit`, the initialiser frame runs and throws, and the exception propagates through the native frame without the handler ever being re-entered, so there is no point at which it could wrap. (The *other* half — a declaring type already in `TypeInitState.Failed` on entry — the handler does catch, and refuses loudly with a TODO naming this, because `ensureTypeInitialised` dispatches that cached exception itself.) Un-park when a native frame can intercept an exception propagating through it. See docs/divergences.md.
            "ReflectionOverloadedIndexer.cs" // Two properties on one type sharing a name — for C#, overloaded indexers, both called `Item`. `RuntimeType.PopulateProperties` compares their signatures to decide whether the second is a duplicate, via `RuntimePropertyInfo.EqualsSig` and so the `Signature_AreEqual` QCall, which is unimplemented. This shape became reachable only once PropertySig decoding landed: before that, any property reflection died earlier in `Signature_Init`. `Signature_AreEqual` is a separate primitive from `Signature_Init` — CoreCLR implements it with `MetaSig::CompareMethodSigs` over two blobs under two type contexts — so it is its own change. A hidden inherited property of the same name reaches the same comparison, so this file stands in for that shape too. Not satisfiable the wrong way: an implementation that always answered "not equal" would still report two properties, so the file also pins which overload each `GetValue` reached. Verified to exit 0 on real .NET.
            "ReflectionPropertyTypeGenericDeclaringType.cs" // A property declared *on* a generic type, which is the shape that makes `Signature_Init` resolve a PropertySig against the declaring type's instantiation. Blocked well before signature decoding: `Type.GetProperty` on a generic instantiation populates the property list, which resolves the accessors' MethodDef tokens, and that fails with "ModuleHandle.ResolveMethod: MethodDef token ... declared on generic type Holder`1; CoreCLR returns the open metadata definition without consuming the caller's typeInstantiation, but the MethodHandle registry only supports fully concretised methods". So this is parked on the MethodHandle registry, not on `PropertySignatureDecoding`. The reachable half — a generic *instantiation* appearing as a property type — is covered by the active `ReflectionPropertyType.cs`. Verified to exit 0 on real .NET.
            "DelegateOverNullInstanceReceiver.cs" // A delegate over an instance method with a null receiver. CoreCLR refuses to build one at all -- `MulticastDelegate.CtorClosed` throws `ArgumentException(Arg_DlgtNullInst)` (MulticastDelegate.CoreCLR.cs:552-556), and the *open* instance delegate that C# cannot spell is made only by `Delegate.CreateDelegate`, which records its target in `_methodPtrAux`. `IlMachineRuntimeMetadata.executeDelegateConstructor` performs no such check, so PawPrint builds the delegate instead. Measured before `Delegate_FindMethodHandle` grew its null-`_target` guard: PawPrint returned 24 where real .NET returns 0 -- the non-generic shape constructed silently, and the generic one then faulted with a NullReferenceException inside `Delegate.GetMethodImpl`, which dereferences `_target` to walk the base chain (Delegate.CoreCLR.cs:189). That second half is now a named refusal from the QCall instead of a guest-visible NRE, so the file stops there rather than returning a code; the first half is still silently wrong either way. Fixing it means teaching delegate construction which of CoreCLR's ctor bodies (`CtorClosed`, `CtorClosedStatic`, `CtorOpened`) the `newobj` selects, which reaches every delegate and so is its own slice. Un-park then. Verified to exit 0 on real .NET.
            "ReflectionParameterMarshalAsPresent.cs" // A parameter carrying a real `[MarshalAs]`, so `PseudoCustomAttribute.GetCustomAttributes` gets a non-empty MarshalSpec blob (ECMA-335 II.23.4) out of `MetadataImport.GetFieldMarshal` and goes on to parse it. Measured on top of that handler: the guest stops at the next InternalCall along, `System.Reflection.MetadataImport::GetMarshalAs(IntPtr, Int32, &Int32, &Int32, &byte*, &Int32, &Int32, &Int32, &byte*, &byte*, &Int32) -> Boolean`, which is CoreCLR's `ParseNativeTypeInfo` (mlinfo.cpp:135) behind an FCall. The *absent* half — every shape whose blob is empty, including the nil ParamDef token that a return value with no Param row produces — is active in `ReflectionParameterMarshalAsAbsent.cs`, so what is parked here is the parsing, not the lookup. One thing for whoever implements `GetMarshalAs`: its two string outputs are not faithfully reproducible. The FCall hands back raw pointers into the blob (managedmdimport.cpp:62-64) and the managed wrapper reads them with `CreateReadOnlySpanFromNullTerminated` (MdImport.cs:265-270), but MarshalSpec strings are length-prefixed rather than NUL-terminated, so real .NET over-reads into whatever `#Blob` bytes follow — measured as a `MarshalType` of "Some.Marshaller" being reported as "Some.MarshallerckM". This file therefore asserts only the numeric properties, which are unaffected. Verified to exit 0 on real .NET.
            "DelegateCombine.cs" // Multicast delegates (issue #959). `Delegate.Combine` reaches `MulticastDelegate.NewMulticastDelegate` (MulticastDelegate.CoreCLR.cs:168), which needs four things. The first is implemented: `RuntimeTypeHandle.InternalAllocNoChecks`, which allocates the new multicast instance (`TestInternalAllocNoChecks` covers it directly, because no guest can reach it without immediately hitting the next blocker). Measured by un-parking on top of that: the guest stops at `System.Delegate::GetMulticastInvoke(MethodTable*)`, an unimplemented InternalCall with a `Delegate_GetMulticastInvokeSlow` QCall fallback, which supplies the new delegate's `_methodPtr`; `System.Delegate::GetInvokeMethod(MethodTable*)` supplies its `_methodPtrAux` and is one instruction further on. Both need a decision about what a "multicast invoke stub" even is in PawPrint's `NativeIntSource.FunctionPointer` model, which has no such shape today. The fourth is dispatch: `AbstractMachine.dispatchDelegateInvoke` reads `_target` and `_methodPtr` and performs exactly one call, so it must learn to walk `_invocationList[0 .. _invocationCount-1]` — note that the array is longer than the count, since `CombineImpl` grows it by doubling, so honouring `_invocationCount` rather than the array length is the thing to get right. That needs a frame surviving N sequential calls, which is the same shape as the limitation `DelegateToActivatorCreateInstance.cs` documents. Un-park when the stub pointers and multicast dispatch land; they interact, since what dispatch needs to see in `_methodPtr` determines what the stub pointers should be.
            "EnumQueriesOpenGenericSharedParent.cs" // `IsEnum` and `IsActualEnum` on an open generic *definition* whose base type still mentions the type parameter (`class Derived<T> : Base<T>`). Both read `MethodTable::ParentMethodTable`, whose projection goes through `resolveBaseRuntimeTypeHandleTarget`; that walk refuses precisely this shape (IlMachineRuntimeMetadata.fs:420, "base type ... references generic parameters (shared/canonical parent); only closed parents are supported today"), because naming the parent needs the definition's formals substituted into the base signature. That is the "a parent whose base type mentions a parameter" exclusion #899 called out as deliberately not done when it added open constructed types, and re-measuring on top of #899 shows it unchanged. Nothing here is about `IsActualEnum` or about the safe-intrinsic allowlist: plain `typeof(Derived<>).IsEnum` — virtual, overridden whole by `RuntimeType`, so it consults no allowlist entry — aborts identically, which is how this was measured. The two parent shapes that *do* resolve (`System.Object`, and a closed non-generic base) are controls at the top of this file and are also asserted by the active `TypeIsActualEnum.cs`. The last two checks are what stop the file being satisfiable the wrong way: answering "no parent at all" would make every enum query here return false correctly — a null parent is how CoreCLR spells `System.Object` — so `BaseType.Name` and `IsSubclassOf(typeof(object))` pin that the real base was found. Un-park when `resolveBaseRuntimeTypeHandleTarget` can name an open constructed parent.
            "InterfaceSlotHiddenByDerivedMethod.cs" // PawPrint does not model interface slot ownership — which type's method implements a given interface-map entry's slot. `findClassImplementation` starts at the receiver and takes the first name/signature match, and `methodMatches` skips its non-virtual/`newslot` guard whenever the call target is an interface, so any same-signature method on the way down wins. The file covers both directions: a derived type that must *not* take an inherited slot (reproducible with no variance at all, and failing identically on `main`), and one that *must* take a slot it re-declares. Fixing it needs a real slot-to-implementation dispatch map, which changes ordinary non-variant interface dispatch too and so wants its own change; `VariantInterfaceSlotOwnership.cs` covers the cases the interface map alone can get right.
            "BulkMoveAcrossOverlappedStructPadding.cs" // A bulk move across padding that *two* fields cover, which explicit layout produces by overlaying identical reference-containing structs. `CliType.TryPaddingRunAt` refuses such a byte — with two fields over it there is no single one to descend through, so it cannot say whose padding it is — and that refusal is the one gap left in the padding step that `BulkMoveAcrossStructPadding.cs` covers. Parked because the refusal is currently unreachable, and measured rather than assumed: an explicit-layout struct with any overlap is stored byte-backed, a byte-backed value holding references cannot be field-accessed, and so the plain `src[i].First.N = i + 1` that *builds* the array stops in `CliType.OfBytesLike` with "non-primitive template ObjectRef None" before any copy happens. Allocating the array alone succeeds; it is the first field write that fails. Same blocker as `ReinterpretCellUnderAliasedAncestor.cs`; un-park when that lands, at which point `TryPaddingRunAt`'s two-fields-cover-it branch is first exercised and will need to learn that padding shared by fields which are padding there too is still padding.
            "ReinterpretCellUnderAliasedAncestor.cs" // A named cell under an explicit-layout ancestor that an unrelated sibling overlaps. Parked on a gap well below cell naming: an explicit-layout struct with any overlap is stored byte-backed, so `CliValueType.DereferenceFieldById` rebuilds a field via `OfBytesLike`, which refuses non-primitive templates — so reference-containing explicit-layout structs cannot be field-accessed at all. The program fails at a plain `outer.Whole.R = ...` before reaching any reinterpret. Not a regression: that path is untouched by the cell resolver.
            "ReinterpretReadNestedFieldThroughIndex.cs" // Reading a field of a nested struct directly through an inline-array index (`buf[k].I.P`), one step deeper than the `buf[k].Field` shape that works. Not the cell resolver: `CliType.CellPathsExactlyCovering` descends to any depth and `TestCliTypeCellPaths` covers depth 3. Nor the projection walk: `walkProjectionByteOffset` folds `ByteOffset` followed by `Field`, so the peeled chain `[ByteOffset k*sizeof(Elem); Field I; Field P]` resolves fine. The one remaining blocker is routing in `readManagedByrefField`, whose reinterpret-aware arms only fire when `ReinterpretAs` is last (or last-but-a-`ByteOffset`); with a trailing `Field` the chain falls through to `readProjectedValue`, which cannot cross a reinterpret. Un-park when that dispatcher learns to route a chain that contains but does not end at a `ReinterpretAs` to the byte-view reader.
            "ActivatorCreateInstanceStructCtor.cs" // `Activator.CreateInstance` on a value type declaring an explicit parameterless ctor. `RuntimeTypeHandle_GetActivationInfo` is implemented and covered by `ActivatorCreateInstance.cs`, but CoreCLR returns that ctor's *boxed* entry point in `ppfnRefCtor` (reflectioninvocation.cpp:1665, `forceBoxedEntryPoint = isValueType`) and `CreateInstanceDefaultCtor` calls exactly that one. `NativeIntSource.FunctionPointer` carries a target with no entry-point flavour, so the boxed entry point is unrepresentable and the QCall fails loudly instead of invoking the ctor with an ObjectRef receiver — which would risk constructing into a copy of the box's payload. Un-park when function pointers can name a boxed entry point.
            "MarshalStructureToPtrDecimalField.cs" // `StructMarshalStub.isBlittableField` rejects Decimal fields (CoreCLR routes them through `NFT_DECIMAL` stub synthesis because native `DECIMAL` is 8-byte aligned while managed `Decimal` is 4-byte aligned), so the struct reaches the has-layout-non-blittable arm and `tryComputePlan` then declines it. The stub machinery a Decimal field would run on now exists — it is what marshals a `DateTime` field — so what remains is Decimal-specific. (1) `Marshal.SizeOf&lt;{int; decimal}&gt;()` returns 20 instead of 24 because the marshal-layout walk in `CliValueType.TryComputeMarshalLayout` does not bump Decimal's field alignment to 8, so the placement handed to the stub is already wrong. (2) There is no `StructMarshalFieldKind` for it. Native `DECIMAL` is one contiguous 16-byte range like any other struct member, so the step's one-range-per-field shape is fine; what differs is the *interior*, since `DECIMAL` orders its members (`wReserved`, `scale`, `sign`, `Hi32`, `Lo64`) differently from managed `System.Decimal`'s `flags`/`hi`/`lo`/`mid`. That is a richer `Kind`, not a richer step.
            "ReflectionOpenGenericDefinitionSharedParent.cs" // `typeof(D<>).GetMethods()` where the definition's base type mentions its own parameter (`SharedDerived<T> : SharedBase<T>`). The layout question is the one the active `ReflectionOpenGenericDefinitionMethods.cs` asks and PawPrint answers; what fails is naming the parent at all, before any of it: `RuntimeType.GetParentType` reaches `resolveBaseRuntimeTypeHandleTarget`, which refuses a base type that references generic parameters -- CoreCLR's shared/canonical parent. Measured: "TODO: resolveBaseRuntimeTypeHandleTarget for open generic typedef ... base type ...[<type param 0>] references generic parameters". That is the same refusal `EnumQueriesOpenGenericSharedParent.cs` parks, reached by a different query, so the two un-park together. The file is not satisfiable by approximating the parent as the *definition* `SharedBase<>`: checks 5 to 8 pin that the parent is the open construction, carrying the deriving definition's own type variable as its argument. Verified to exit 0 on real .NET.
            "MakeGenericMethodOnOpenDefinition.cs" // `MakeGenericMethod` on a generic method of an open generic type *definition*, which reaches `RuntimeMethodHandle_GetStubIfNeededSlow`'s definition-level rebind and stops at the one thing that rebind cannot do: validate the method's constraints. CoreCLR checks them against the declaring type's *unbound formals* (genmeth.cpp:1256-1270), and PawPrint's `validateConstraintsOn` wants each formal as a `ConcreteTypeHandle`, which is closed by construction. Measured: "TODO: RuntimeMethodHandle.GetStubIfNeededSlow: rebinding onto open generic definition ...; constraint validation under an open declaring context is unimplemented". The file exists to rule out the cheap version of the fix: a blanket "no closed argument satisfies a constraint mentioning a formal" is false, because `IComparer<in T>` is contravariant and `T : class` bounds `T` above by `object`, so real .NET accepts `IComparer<object>` for `U : IComparer<T>` while rejecting `IComparer<string>` -- both measured, and checks 9 to 11 are exactly that pair. Un-park when constraint validation can run against a declaring type's formals. Verified to exit 0 on real .NET.
            "ReflectionOpenGenericDefinitionElementTypes.cs" // A parameter or return typed `T[]` or `ref T` on an open generic type *definition*, where `T` is the definition's own variable. `Signature_Init` decodes such a signature against the definition's formals since sourcesPure/ReflectionOpenGenericDefinitionParameterTypes.cs's change, and every shape that file covers works; what is refused is the element type here. A reflected type is a `RuntimeTypeHandleTarget`, whose cases are a closed runtime type, a definition, a variable of a type or method, an open construction of a definition over targets, and the dynamic-methods class -- none of which is an array of, byref to or pointer to a target, and `ConcreteTypeHandle` carries those shapes only over closed element types. Measured: "TODO: Signature_Init: the signature of TakesArray names arr[<type param 0>], which embeds a generic parameter beneath an array, pointer, byref or function-pointer shape". Only the array refusal is reached -- `TakesArray` is decoded first, so the `ref T` checks are behind it rather than separately measured. This bites wider than the file suggests, because a candidate-filtering query decodes *every* candidate's signature: one `out T` overload refuses the whole query, which is why `typeof(Dictionary<,>).GetMethod("TryGetValue")` is out of reach. Un-park when a reflected type can name an array, byref or pointer over a type variable; that reaches the 75 match arms across 34 files which enumerate `RuntimeTypeHandleTarget` today. Verified to exit 0 on real .NET.
            "MakeGenericMethodOpenArgument.cs" //`RuntimeMethodHandle_GetStubIfNeededSlow` (issue #743) handles `MakeGenericMethod` with closed type arguments, which is what every reachable path needs, but an argument that still contains generic parameters — `MakeGenericMethod(typeof(G<>))` or `MakeGenericMethod(someTypeParameter)` — cannot be represented. Both are legal: real .NET returns a MethodInfo with `ContainsGenericParameters = true`, inspectable but not invokable. PawPrint's `MethodHandle.MethodGenerics` is a `ConcreteTypeHandle list`, and `ConcreteTypeHandle` indexes `AllConcreteTypes`, whose entries carry only *closed* generic arguments, so the QCall fails with a precise TODO. Widening that representation reaches concretization and every other MethodHandle consumer, so it is its own change rather than part of the QCall.
            "ReflectionInvokeIntrinsicTarget.cs" // `MethodBase.Invoke` on a method PawPrint services as a JIT intrinsic rather than by interpreting IL (`Unsafe.SizeOf<long>()`; real .NET treats it as an ordinary reflectable method and answers 8). The blocker is in the call path, not in the `RuntimeMethodHandle_InvokeMethod` QCall's own bookkeeping: `callMethodWithCommitment` services such a method inline, computing the result and then advancing the *caller's* program counter — right for a `call` opcode, but here the caller is the native QCall frame, which has no IL. It also reports `CallCommitment.Committed`, so the QCall's commitment check cannot catch it; the QCall therefore rejects the shape up front, so the failure names the method instead of aborting inside `advanceProgramCounter`. Un-parking means letting `Intrinsics.call` honour `advanceProgramCounterOfCaller = false`, which reaches every intrinsic's completion path (~70 sites across `Intrinsics.fs` and `IntrinsicHelpers.fs`).
            "ReflectionInvokeConstructorOnInstanceManyArguments.cs" // `ConstructorInfo.Invoke(instance, args)` with more than four arguments, the only route to the five-argument `MethodBaseInvoker.InvokeConstructorWithoutAlloc` (MethodBaseInvoker.Constructor.cs:15). It stops before the QCall, in `MethodBaseInvoker.CopyBack`: "MemoryBlock.readBytes: byte at offset 0 in <stack memory block #0> is uninitialised" (measured). That overload's `shouldCopyBack` is a bare `stackalloc bool[argCount]` with no `NativeMemory.Clear`, unlike its `InvokeWithManyArgs` sibling, which clears a block covering all three regions (MethodBaseInvoker.cs:238-243) and so passes. CoreLib is compiled `[module: SkipLocalsInit]`, so `localloc` really does leave that block uninitialised on CoreCLR too, and `CheckArguments` writes `shouldCopyBack[i]` only on the conversion paths (MethodBaseInvoker.cs:354-386) — an argument already of the signature's type leaves its slot untouched. Real .NET therefore reads the garbage and is saved by the copy-back being a no-op (`copyOfParameters[i]` is the argument it was handed); PawPrint's stack memory refuses to be read before it is written. Un-parking means deciding what an uninitialised `localloc` byte reads as, which is a question about the memory model rather than about reflection. Verified to exit 0 on real .NET.
            "ReflectionInvokePointerSignature.cs" // `MethodBase.Invoke` on a target whose signature mentions a pointer, in both directions. Reflection does not pass the CLR representation through for pointers, so each direction needs its own work in the `RuntimeMethodHandle_InvokeMethod` QCall, and each is rejected loudly there today. Argument side: `MethodInvokerCommon.Initialize` sets `InvokerArgFlags.IsValueType` for a pointer parameter, so its byref-buffer entry addresses the payload of a boxed `IntPtr` rather than an `object?` slot, which the QCall's reference-type read path cannot serve; `argumentIsValueType` also says false for a structural pointer handle, so the shape needs naming rather than inferring. Return side: `InvokeUtil::CreateObjectAfterInvoke` wraps an `ELEMENT_TYPE_PTR` return in a `System.Reflection.Pointer` carrying the pointed-to Type (so `Invoke` never answers null even for a null pointer) and boxes a function-pointer return as an `IntPtr`; PawPrint has no `Pointer` construction yet. Un-park when both land.
            "ReflectionInvokeVirtualMethod.cs" // `MethodBase.Invoke` on a virtual method looked up through the base class that declares it. The `GetMethod` lookup succeeds (`sourcesPure/ReflectionVirtualMethodSlots.cs` covers the slot layout it needs); the guest reaches the invocation itself and stops at an unimplemented primitive: the QCall `CastHelpers::IsInstanceOf_NoCacheLookup` (`System.Private.CoreLib System.Runtime.CompilerServices.CastHelpers::<IsInstanceOf_NoCacheLookup>g____PInvoke|4_0(*(System.Void), System.Int32, System.Runtime.CompilerServices.ObjectHandleOnStack) -> System.Int32`). That is reached because the receiver is a `Derived` while the declaring type is `Base`, so `InvokeUtil`'s type check needs a real hierarchy walk and misses the managed cast cache. Nothing here is about slots or about invocation bookkeeping: the QCall passes `performInterfaceResolution = true` so that a vtable method dispatches virtually, mirroring CoreCLR's `GetSingleCallableAddrOfVirtualizedCode` (reflectioninvocation.cpp:417-424), and this file is what will check that claim once the cast helper lands. Un-park then.
            "LdvirtftnIntrinsicDeclaringType.cs" // A delegate built by `ldvirtftn` whose resolved body is declared on a type carrying a type-level `[Intrinsic]`. `callvirt` gets this right by keying the *type-level* check on the call site's static declaration and only the *method-level* one on the resolved body (`callMethodWithCommitment`, IlMachineStateExecution.fs:1605-1617) — so `callvirt Object::GetHashCode()` on a boxed `Int128` interprets the override, and the file's direct-call control passes. A delegate cannot make that distinction: `ldvirtftn` must bind eagerly (delegate invocation runs with `performInterfaceResolution = false`), so the pointer names `Int128::GetHashCode`, and `dispatchDelegateInvoke` then hands that same method to `callMethod` as both call site and target, whereupon the type-level `[Intrinsic]` on `Int128` fires and it stops at "TODO: implement JIT intrinsic System.Int128.GetHashCode()". Nothing here is about dispatch, which resolves correctly; the missing piece is that `FunctionPointerTarget.Managed` carries only a body, with no room for the declaration the call site named, so the distinction `callvirt` relies on is unrepresentable in a function pointer. Un-park when a function pointer can carry its call-site declaration alongside its target.
            "DelegateToActivatorCreateInstance.cs" // A delegate whose target is `Activator.CreateInstance<T>()` for a `T` whose `.cctor` has not yet run. PawPrint services that method as an intrinsic which runs `T`'s initialiser and then reports `CallCommitment.SuspendedForClassInit`, asking its caller to re-execute once the initialiser returns. A call *opcode* can honour that by leaving its program counter unadvanced; `dispatchDelegateInvoke` cannot, because the delegate's synthetic `Invoke` frame is already popped by the time the target is called, so there is no frame to re-enter. It therefore refuses loudly, naming the situation. Distinct from the type initialisation this file's sibling tests cover: that one is for the *target's declaring type* (`System.Activator`, long since initialised) and runs *before* the frame is popped, which is exactly what lets it suspend safely. Un-parking means giving delegate invocation a frame that survives the call, which is the same shape as `calli`'s save-and-retry and reaches every delegate, so it is its own change.
            "RuntimeHelpersBoxReferenceContainingStruct.cs" // `RuntimeHelpers.Box` of a struct holding a reference — the one shape whose box CoreCLR fills with `Buffer.BulkMoveWithWriteBarrier` rather than `SpanHelpers.Memmove` (RuntimeType.BoxCache.cs:91). Nothing here is about `ReflectionInvocation_GetBoxInfo`, which serves this type fine: the guest never reaches `Box` at all. It dies one step earlier, in its own `Unsafe.As<WithRef, byte>(ref r)`, which is `Intrinsics.call` (Intrinsics.fs:1672) taking a byte view over a *local* — measured as "refusing byte view over value type containing object references in single-cell byref `<variable 0 …> as System.Byte`", from `validateByteAddressableCell` (IlMachineManagedByref.fs:595). That is the same "a reference-containing value type has no byte image" gap as `ReinterpretCellUnderAliasedAncestor.cs` and `BulkMoveAcrossOverlappedStructPadding.cs`, and it forecloses *every* route a guest has to a `ref byte` over such a struct, which is why the reference-free half of this coverage lives in the active `RuntimeHelpersBox.cs` instead. The file is not satisfiable the wrong way: it checks both fields (so moving only the reference, or only the trailing int, still fails) and re-reads the box after mutating the source (so handing back a view onto the source rather than a copy fails). Un-park when a reference-containing value type can be byte-addressed.
            "DelegateBindOpenVirtual.cs" // An *open* delegate over a virtual instance method on a reference type -- the one shape `COMDelegate::BindToMethod` resolves at invocation rather than at binding, putting a virtual call stub in `_methodPtrAux` and the `MethodDesc` in `_invocationCount` (comdelegate.cpp:1236-1245). PawPrint writes neither field, and `AbstractMachine.dispatchDelegateInvoke` calls whatever `_methodPtr` names without virtualising, so `Delegate_BindToMethodInfo` refuses rather than bind the declared method and silently ignore an override; measured, real .NET dispatches per argument, which checks 4 and 5 pin. C# cannot spell this with method-group syntax, so `Delegate.CreateDelegate` is the only route to it, and it became reachable when that QCall learned to bind a metadata method. Two neighbouring shapes *are* served and are the controls: a non-virtual instance method, which needs no stub upstream either, and a `sealed override`, which is `final` in IL and whose slot therefore always resolves to itself. Same `_methodPtrAux`/`_invocationCount` representation gap as issue #959; un-park with it. Verified to exit 0 on real .NET.
            "DelegateBindStaticAbstractInterfaceMethod.cs" // `CreateDelegate` over a static *abstract* interface method: the one shape that is both static and virtual, so `MethodInfo.DispatchesVirtually` (which folds `not IsStatic` in) says false about it while CoreCLR's `pTargetMethod->IsVirtual()` says true. Measured: real .NET *binds* it, taking `BindToMethod`'s virtual-call-stub branch because the declaring type is not a value type, and raises `EntryPointNotFoundException` only when the delegate is invoked. PawPrint refuses the binding, because it writes no `_methodPtrAux` and an abstract target has no body for `_methodPtr` to name, so there is nothing honest to store; the refusal also sits before the compatibility check, which is what lets the open path there use `DispatchesVirtually` without a static virtual slipping through. Un-park when a delegate can hold a bodiless target, at which point the answer is to bind and to raise from the invocation instead. Verified to exit 0 on real .NET.
            "DelegateBindOpenGenericDefinitionMethod.cs" // `CreateDelegate` over a method whose declaring type is an open generic *definition*, `typeof(G<>).GetMethod("M")`. Real .NET answers with an exception in both directions, and from different places: a static target reaches `MethodDesc::TryGetMultiCallableAddrOfCode` (method.cpp:2091-2093) while `BindToMethod` looks for a code address and gets `InvalidOperationException` with `IDS_EE_CODEEXECUTION_CONTAINSGENERICVAR` -- a native throw, not a managed prologue check -- while an instance target fails the compatibility check and returns FALSE, which the managed caller turns into `ArgumentException`. PawPrint refuses both earlier, in `NativeRuntimeMethodHandle.requireClosedDeclaringType`: a target's signature has to be read against an exact instantiation and a definition has none. Un-park when a method handle can carry a formal declaring context, which is the representation `MakeGenericMethodOnOpenDefinition.cs` also waits on. Verified to exit 0 on real .NET.
            "DelegateFindMethodHandleOpenInstanceGeneric.cs" // `Delegate.Method` on an *open* delegate over an instance method whose declaring type is a generic instantiation. Binding and invoking work; only the `Delegate_FindMethodHandle` query stops. `Delegate.GetMethodImpl` branches on `_methodPtrAux` -- nonzero means open, and the target is read off the handle; zero means closed, and a generic declaring type sends it to walk `_target.GetType()`'s base chain (Delegate.CoreCLR.cs:189). PawPrint's aux is always zero, so CoreLib takes the closed branch and dereferences a null `_target`, and the QCall refuses rather than hand back a method CoreLib will then fault on. This became reachable when `Delegate_BindToMethodInfo` learned to bind a metadata method: `Delegate.CreateDelegate(Type, MethodInfo)` is the only route to an open instance delegate, C# having no method-group syntax for one, which is why the guard's own comment used to call it unreachable. The non-generic declaring type is the control and is served. Same gap as issue #959; un-park with it. Verified to exit 0 on real .NET.
            "PointerFieldAliasedWidthStore.cs" // Storing into a pointer-typed field through a byref aliased as `long*`/`double*` rather than `void**`. A pointer slot is a `CliType.RuntimePointer` cell with no byte image, so the byte-scatter writer refuses it; the sibling `PointerFieldIndirectStore.cs` fixes that for the pointer-shaped payloads by replacing the whole cell instead. That route is deliberately *not* taken here, because whole-cell replacement restamps the cell with the payload's shape: on a 64-bit runtime `stind.i8`/`stind.r8` are exact-width stores into a `void*` slot too, and taking them would leave the field holding `Numeric Int64`/`Float64`, so the next read pushes the wrong evaluation-stack kind and fails downstream (measured: `bad ceq: Int64 vs NativeInt(0)`) with a message naming neither the field nor the store. Un-parking needs a pointer cell that can hold a non-pointer bit pattern while still reading back as a pointer — i.e. the same "materialise bits late" question as the rest of the provenance model — not a wider routing predicate.
            "StackTraceCurrentThreadFrames.cs" // A current-thread `new StackTrace()` with real frames in it. The `StackTrace_GetStackFramesInternal` QCall this needs is implemented; the guest stops one InternalCall further on, at `System.RuntimeMethodHandle::IsTypicalMethodDefinition(System.IRuntimeMethodInfo) -> System.Boolean` (measured). That is unavoidable for *any* non-empty capture: `CaptureStackTrace` builds a `StackFrame` for every captured frame before computing skips (StackTrace.CoreCLR.cs:73-85), and that constructor calls `GetMethodBase` unconditionally (StackFrame.CoreCLR.cs:18), which routes through `RuntimeMethodHandle.GetTypicalMethodDefinition` and so through the predicate. Un-park when `IsTypicalMethodDefinition` lands together with its `RuntimeMethodHandle_GetTypicalMethodDefinition` QCall fallback: they belong in one change, because CoreCLR strips only the *method* instantiation from a frame's handle and leaves the class instantiation alone (debugdebugger.cpp:449-452), so a frame on a method of a generic declaring type answers false to the predicate and genuinely reaches the QCall. Beyond un-parking, the file pins the property PawPrint's frame walk relies on: PawPrint does not inline, so its raw capture carries seven `System.Diagnostics` frames where real .NET carries fewer, and that is harmless only because `CalculateFramesToSkip` skips the leading run by *ordinal-equal* namespace rather than by count. Frame counts are therefore not asserted, but the identity and order of the reported frames are — which is what goes red if the walk starts filtering CoreLib frames itself and the skip run then eats real guest frames. Verified to exit 0 on real .NET.
            "StackTraceFromExceptionNeedFileInfo.cs" // `new StackTrace(exception, fNeedFileInfo: true)` on an exception with no captured trace. The blocker is not the frame count, which is zero and correctly reported: `InitializeSourceInfo` calls `CreateStackTraceSymbols()` *before* the loop over frames, gated only on `fNeedFileInfo` (StackFrameHelper.cs:95-113), so an empty capture does not avoid it. Measured: "TODO: dispatch [UnsafeAccessor] is unimplemented for System.Diagnostics.StackFrameHelper::CreateStackTraceSymbols (kind=Constructor)". CoreLib wraps that block in `try { } catch { }`, which is how real .NET copes when `System.Diagnostics.StackTrace.dll` is absent, but that swallows a *guest* exception and a host-level refusal is not one. Un-park with `[UnsafeAccessor]` dispatch, or more cheaply by making an unresolvable `[UnsafeAccessor]` raise a guest exception, which CoreLib's own catch then absorbs. This is the blocker standing between `Exception.StackTrace` (Exception.cs:232) and `ExceptionDispatchInfo.SetCurrentStackTrace` (Exception.cs:247) and working, both of which pass `fNeedFileInfo: true`. Verified to exit 0 on real .NET.
        ]
        |> Set.ofList

    let expectsUnhandledException =
        [
            "UnhandledException.cs"
            "EnumHasFlagMismatchUnhandled.cs"
            "EnumHasFlagNullFlagUnhandled.cs"
            "ArrayGetLengthOutOfRangeUnhandled.cs"
        ]
        |> Set.ofList

    let customExitCodes = [ "ExceptionWithNoOpFinally.cs", 3 ] |> Map.ofList

    let allPure =
        assy.GetManifestResourceNames ()
        |> Seq.choose (fun res ->
            let s = "WoofWare.PawPrint.Test.sourcesPure."

            if res.StartsWith (s, StringComparison.OrdinalIgnoreCase) then
                res.Substring s.Length |> Some
            else
                None
        )
        |> Set.ofSeq

    /// Guests that need a filesystem to look at, with the seed each one wants.
    ///
    /// The *same* seed configures both sides of the differential comparison:
    /// PawPrint realises it into a `VirtualFileSystem` rooted at `/`, and
    /// `RealRuntime.executeWithSeed` materialises it into the scratch directory
    /// the real guest runs in. One description, two interpreters — which is
    /// what makes the agreement worth something, rather than two hand-kept
    /// copies of a tree that might have drifted apart.
    ///
    /// Excluded from `simpleCases` because they need a non-default
    /// `KernelConfig`.
    let seededCases : Map<string, Map<FileName, SeedEntry>> =
        let name (s : string) = FileName.parseOrFail "test seed" s
        let target (s : string) = SymlinkTarget.parseOrFail "test seed" s

        let file (contents : string) =
            SeedEntry.file (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

        let bytes (contents : string) =
            Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange

        let mode (raw : int) =
            PermissionBits.parseOrFail "test seed" raw

        let openSeed =
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                ]

        [
            "FileMetadataSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    // A leading dot, which is the whole of what "hidden" means
                    // on Unix.
                    name ".hidden", file "x"
                ]
            // Every mode here is written as an octal literal rather than
            // assembled from named bits, because the guest asserts the named
            // bits: if both sides were spelled the same way, a wrong bit order
            // would agree with itself.
            "FileModeSeeded.cs",
            Map.ofList
                [
                    name "default", file "hello"
                    name "private", SeedEntry.File (bytes "hello", mode 0o600)
                    name "shared", SeedEntry.File (bytes "hello", mode 0o666)
                    name "readonly", SeedEntry.File (bytes "hello", mode 0o444)
                    name "dir", SeedEntry.directory Map.empty
                    name "narrow", SeedEntry.Directory (Map.empty, mode 0o711)
                    // Non-empty *and* not writable by its owner, which is the
                    // one shape that makes the oracle's materialisation order
                    // observable: the child has to be created before the mode
                    // is applied, or the host cannot create it at all. (A
                    // process running as root bypasses that check, so on a root
                    // CI runner this seed exercises the order without being
                    // able to falsify it — the test still passes either way.)
                    name "locked", SeedEntry.Directory (Map.ofList [ name "inside", file "within" ], mode 0o555)
                ]
            "FileExistsSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "SystemNativeReadLink.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    name "lf", SeedEntry.Symlink (target "f")
                    // Six bytes, so that "exactly the target", "one byte more"
                    // and "one byte less" are three different buffer sizes.
                    // Dangling on purpose: `readlink` reports a target without
                    // resolving it, and a target that existed would let a
                    // handler that answered from the *resolved* file pass.
                    name "five", SeedEntry.Symlink (target "hello5")
                ]
            // Both open-path guests want the same tree, and deliberately share
            // one: the raw guest pins the syscall contract and the managed one
            // pins which exception each errno becomes, so a divergence between
            // them is a divergence about one filesystem rather than two.
            "UnlinkSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "g", file "bye"
                    name "held", file "payload"
                    name "f2", file "two"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lg", SeedEntry.Symlink (target "g")
                    name "dang", SeedEntry.Symlink (target "nx")
                ]
            "RmDirSeeded.cs",
            Map.ofList
                [
                    name "empty", SeedEntry.directory Map.empty
                    name "full", SeedEntry.directory (Map.ofList [ name "x", file "inside" ])
                    name "f", file "hello"
                    // To a directory, so that following it would destroy `full`
                    // rather than merely answering the wrong errno.
                    name "ld", SeedEntry.Symlink (target "full")
                    name "dang", SeedEntry.Symlink (target "nx")
                    // Two levels, so that "nav/kid/." and "nav/kid/.." reach a
                    // directory that is not the root -- the flavours agree there
                    // and diverge at the root itself.
                    name "nav", SeedEntry.directory (Map.ofList [ name "kid", SeedEntry.directory Map.empty ])
                    // Opened before it is removed, which is the row that shows a
                    // descriptor outliving the last name.
                    name "held", SeedEntry.directory Map.empty
                ]
            "SystemNativeOpen.cs", openSeed
            "OpenMissingFile.cs", openSeed
            "LinkTargetSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    // A link to a link, so that following to the final target
                    // has to iterate rather than merely dereference once.
                    name "l2", SeedEntry.Symlink (target "lf")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                    // Longer than the 256-byte stackalloc `Interop.Sys.ReadLink`
                    // starts with, so reading it at all requires the truncating
                    // first call and the grown retry. NAME_MAX does not apply:
                    // this is a link's *target*, not anything's name.
                    name "long", SeedEntry.Symlink (target (String.replicate 300 "a"))
                ]
            "ReadAllBytesSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "empty", file ""
                    // U+00DF then 'x': three UTF-8 bytes for two characters, so
                    // a handler measuring .NET chars rather than bytes differs.
                    name "mb", file "\u00dfx"
                    name "lines", file "one\ntwo\nthree\n"
                    // 10000 bytes, longer than `StreamReader`'s 4096-byte
                    // buffer, so reading it issues several `pread`s at
                    // increasing offsets. The 251-byte cycle is coprime to 4096,
                    // so no chunk boundary lands on a repeat of the previous
                    // one's phase and an off-by-one shifts visible bytes.
                    name "big", file (String.init 10000 (fun i -> string<char> (char (int 'a' + (i % 251) % 26))))
                ]
            "FileStreamHandleSeeded.cs", Map.ofList [ name "f", file "hello" ]
            "CreateSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    name "lf", SeedEntry.Symlink (target "f")
                    // The link the plain-O_CREAT row follows: creating through it
                    // must bind "viadangtarget", not replace the link.
                    name "viadang", SeedEntry.Symlink (target "viadangtarget")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "TruncateSeeded.cs",
            Map.ofList
                [
                    // A separate file per group of rows, so each starts from a
                    // known five bytes rather than from whatever the previous
                    // group left behind.
                    name "f", file "hello"
                    name "g", file "hello"
                    name "h", file "hello"
                    name "d", SeedEntry.directory Map.empty
                    // The symlink O_TRUNC follows: its *target* is what must end
                    // up empty, and the two names are distinct so that following
                    // it can be told from replacing it.
                    name "lf", SeedEntry.Symlink (target "f2")
                    name "f2", file "hello"
                    // The pair the refusal rows read back: a refused open must
                    // leave "keep" exactly as it was, whether it was refused for
                    // EEXIST or (through the link, under O_NOFOLLOW) for ELOOP.
                    name "keep", file "hello"
                    name "lkeep", SeedEntry.Symlink (target "keep")
                    // All four BCL rows need a *non-empty* file to start from:
                    // `SafeFileHandle.Init` swallows EINVAL and EBADF from
                    // FTruncate, so a wrongly-refused truncation shows up only as
                    // bytes that are still there.
                    name "bcl", file "hello"
                    name "bcl2", file "hello"
                    name "bcl3", file "hello"
                    name "bcl4", file "x"
                ]
            "WriteSeeded.cs",
            Map.ofList
                [
                    // Three separate files of the same contents, so that the
                    // syscall-level rows, the O_RDWR rows and the `FileStream`
                    // rows each start from a known five bytes rather than from
                    // whatever an earlier row left behind.
                    name "f", file "hello"
                    name "g", file "hello"
                    name "h", file "hello"
                    name "d", SeedEntry.directory Map.empty
                ]
            "ReadSeekSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    // A directory, because `read` and `lseek` disagree about
                    // one: reading it is EISDIR while seeking it (SEEK_SET and
                    // SEEK_CUR, the portable pair) succeeds.
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                ]
            "EnumerateSeeded.cs",
            Map.ofList
                [
                    name "d",
                    SeedEntry.directory (
                        Map.ofList
                            [
                                name "a", file "aaa"
                                name "sub", SeedEntry.directory (Map.ofList [ name "z", file "zzz" ])
                                name "ls", SeedEntry.Symlink (target "a")
                            ]
                    )
                    name "f", file "hello"
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "gone", SeedEntry.directory Map.empty
                    name "del",
                    SeedEntry.directory (
                        Map.ofList
                            [
                                name "x", file "xxx"
                                name "inner", SeedEntry.directory (Map.ofList [ name "y", file "yyy" ])
                            ]
                    )
                ]
            "MkDirSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    // A dangling link and a cyclic one: `mkdir` answers EEXIST
                    // for both, which is what says it never dereferences the
                    // name it is about to bind.
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "FlockContentionSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    // A second, unrelated file: a lock is per file, so holding
                    // one on `f` must not stop `g` being opened exclusively.
                    name "g", file "other"
                    // Another path to `f`. Locks are keyed on the resolved
                    // inode, not on the path used to reach it.
                    name "lf", SeedEntry.Symlink (target "f")
                ]
        ]
        |> Map.ofList

    let seededCaseNames : string list = seededCases |> Map.toList |> List.map fst

    let simpleCases : string list =
        allPure
        |> Seq.filter (fun s ->
            (customExitCodes.ContainsKey s
             || unimplemented.Contains s
             || expectsUnhandledException.Contains s
             || seededCases.ContainsKey s)
            |> not
        )
        |> Seq.toList

    let runPawPrintSource
        (sourceName : string)
        (source : string)
        (kernelConfig : KernelConfig)
        (assertResult : byte array -> RunOutcome -> unit)
        : unit
        =
        let image = Roslyn.compile [ source ]

        let messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", sourceName ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        try
            let pawPrintResult =
                BoundedRun.run
                    loggerFactory
                    sourceName
                    (Some sourceName)
                    peImage
                    { HostConfig.Default dotnetRuntimes with
                        Guest =
                            { GuestConfig.Default dotnetRuntimes with
                                Kernel = kernelConfig
                            }
                    }

            assertResult image pawPrintResult
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let runTest (case : EndToEndTestCase) : unit =
        if not (AppContextProperties.isEmpty case.AppContext) then
            // The oracle runs the guest out of process under a fixed `runtimeconfig.json`
            // (`RealRuntime.runtimeConfig`) that carries no `configProperties`, so a case's
            // AppContext properties never reach the real runtime. A case with properties
            // would therefore be comparing a seeded PawPrint against an unseeded oracle —
            // a PawPrint-only fact dressed up as a cross-runtime one. Those belong in
            // `sourcesImpure`.
            failwith
                $"%s{case.FileName} sets AppContext properties (%O{case.AppContext}), but it is registered as a *pure* differential case. Move it to sourcesImpure."

        let source = Assembly.getEmbeddedResourceAsString case.FileName assy

        runPawPrintSource
            case.FileName
            source
            case.KernelConfig
            (fun image pawPrintResult ->
                // The case's own seed drives the oracle too, so both runtimes
                // are looking at one description of a filesystem. An unseeded
                // case passes `FileSystemSeed.empty`, which materialises
                // nothing and leaves the oracle exactly as it was.
                let realResult = RealRuntime.executeWithSeed case.KernelConfig.FileSystem [||] image

                // NormalExit and ProcessExit both represent a clean process termination with
                // an exit code on the terminating thread's eval stack; the only difference is
                // whether the guest returned from Main or called Environment.Exit. The real
                // runtime surfaces both as RealRuntimeResult.NormalExit, so normalise here.
                let normalisedPawPrint =
                    match pawPrintResult with
                    | RunOutcome.ProcessExit (s, t) -> RunOutcome.NormalExit (s, t)
                    | other -> other

                match realResult, normalisedPawPrint with
                | RealRuntimeResult.NormalExit exitCode, RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    exitCode |> shouldEqual case.ExpectedReturnCode

                    let pawPrintExitCode =
                        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                        | [] -> failwith "expected program to return a value, but it returned void"
                        | head :: _ ->
                            match head with
                            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
                            | ret -> failwith $"expected program to return an int, but it returned %O{ret}"

                    pawPrintExitCode |> shouldEqual exitCode
                | RealRuntimeResult.UnhandledException _, RunOutcome.GuestUnhandledException _ ->
                    if not case.ExpectsUnhandledException then
                        failwith
                            $"Both runtimes threw unhandled exceptions for %s{case.FileName}, but this test was not expected to throw. Add to expectsUnhandledException if intentional."
                | RealRuntimeResult.NormalExit exitCode, RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith
                        $"Real runtime exited normally with code %d{exitCode}, but PawPrint threw unhandled exception: %O{exn.ExceptionObject}"
                | RealRuntimeResult.Aborted (_code, report), _ ->
                    failwith
                        $"Real runtime called Environment.FailFast for %s{case.FileName}; this fixture does not exercise FailFast:\n%s{report}"
                | RealRuntimeResult.UnhandledException realExn,
                  RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    let pawPrintExitCode =
                        match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                        | [] -> None
                        | EvalStackValue.Int32 (Int32Source.Verbatim i) :: _ -> Some i
                        | _ -> None

                    failwith
                        $"Real runtime terminated with an unhandled exception, but PawPrint exited normally (code: %O{pawPrintExitCode}):\n%s{realExn}"
                | _, RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"

                    failwith $"PawPrint guest aborted (%O{fatal.Code}) for %s{case.FileName}: %s{m}"
                | _, RunOutcome.SignalTerminated (_, signal) ->
                    failwith
                        $"PawPrint guest was terminated by POSIX signal %O{signal} for %s{case.FileName}; this test does not exercise signal-driven termination"
                | _, RunOutcome.ProcessExit _ -> failwith "unreachable: normalised away above"
            )

    /// `calli` through a null function pointer. This cannot be a comparison test in
    /// `sourcesPure`: the real runtime does not raise a catchable NullReferenceException
    /// here, it segfaults (observed as exit 139 on osx-arm64), which would take the test
    /// host down with it. PawPrint instead implements the behaviour ECMA-335 III.3.20
    /// actually specifies. See docs/divergences.md.
    [<Test>]
    let ``calli through a null function pointer throws NullReferenceException`` () =
        let source =
            """
using System;

public class Program
{
    public static unsafe int Main(string[] args)
    {
        // Two spellings of a null function pointer. Both currently reach the interpreter
        // as a verbatim zero, so this does not by itself exercise the other zero-valued
        // `NativeIntSource` shapes that `executeCalli` accepts as null (notably
        // `ManagedPointer ManagedPointerSource.Null`); that handling is deliberately
        // broader than any C# spelling reachable today. (`IntPtr.Zero.ToPointer()` would
        // be a third spelling, but it needs `ldsflda` of a MemberReference, which is a
        // separate unimplemented gap.)
        delegate*<int, int> a = null;
        delegate*<int, int> b = (delegate*<int, int>)(void*)null;

        int caught = 0;
        try { a(1); } catch (NullReferenceException) { caught += 1; }
        try { b(1); } catch (NullReferenceException) { caught += 2; }

        return caught == 3 ? 0 : caught;
    }
}
"""

        runPawPrintSource
            "CalliNullFunctionPointer.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | outcome ->
                    failwith
                        $"Expected the guest to catch a NullReferenceException from the null calli, got %O{outcome}"
            )

    /// ECMA-335 III.3.20 defines `calli`'s marshalling by the call-site StandaloneSignature,
    /// so a guest may legally pun a function pointer to a signature whose types differ from
    /// the target's. PawPrint invokes the target directly, so it is the *target's* types that
    /// drive argument coercion and the return push; until that is fixed (i.e. until arguments
    /// and the result are coerced to the call-site types) such a call must be refused at the
    /// faulting instruction rather than proceeding and failing far away inside
    /// `toCliTypeCoerced` with a message that never mentions `calli`.
    ///
    /// These cannot be `sourcesPure` comparison tests: CoreCLR accepts both (verified
    /// standalone on osx-arm64 — the return case prints 3, the argument case prints 7), so a
    /// comparison test would assert PawPrint reproduces behaviour it deliberately does not.
    /// See docs/divergences.md.
    [<Test>]
    let ``calli refuses a punned return type at the faulting instruction`` () =
        let source =
            """
using System;

public class Program
{
    static int Id(int x) => x;

    public static unsafe int Main(string[] args)
    {
        delegate*<int, int> p = &Id;
        // Same arity, same void-ness, wider return: passes the slot-count and void-ness
        // checks, and would otherwise die in toCliTypeCoerced at the `stloc` of the long.
        long r = ((delegate*<int, long>)p)(3);
        return (int)r;
    }
}
"""

        // `Assert.Catch`, not `Assert.Throws`, here and in the two tests below: `Assert.Throws` is
        // an *exact* type constraint, and a failure raised while interpreting arrives as
        // `GuestFailureException` carrying the guest's position. These tests are about the
        // message, not the type, so the looser assertion is the one that says what they mean.
        let exn =
            Assert.Catch (fun () ->
                runPawPrintSource "CalliPunnedReturn.cs" source KernelConfig.Default (fun _image _result -> ())
            )

        exn.Message |> shouldContainText "calli"
        exn.Message |> shouldContainText "return"
        exn.Message |> shouldContainText "Program"

    [<Test>]
    let ``calli refuses a punned parameter type at the faulting instruction`` () =
        let source =
            """
using System;

public class Program
{
    static long Id(long x) => x;

    public static unsafe int Main(string[] args)
    {
        delegate*<long, long> p = &Id;
        // Arity and return type agree; only the parameter's stack representation differs.
        long r = ((delegate*<int, long>)p)(7);
        return (int)r;
    }
}
"""

        let exn =
            Assert.Catch (fun () ->
                runPawPrintSource "CalliPunnedParameter.cs" source KernelConfig.Default (fun _image _result -> ())
            )

        exn.Message |> shouldContainText "calli"
        exn.Message |> shouldContainText "parameter"
        exn.Message |> shouldContainText "Program"

    /// `float32` and `float64` are the same type (`F`) on the CLI evaluation stack, but a
    /// `calli` marshals across a method boundary, where their ABI footprints differ. Reading a
    /// `float32` return slot as `float64` yields garbage on CoreCLR rather than the target's
    /// value, so this pun must be refused like the integer ones — otherwise PawPrint invokes
    /// the target and silently returns the *plausible* answer where the real runtime returns
    /// nonsense, which is worse than crashing.
    ///
    /// Measured on osx-arm64 with a bitmask probe over five puns (short/byte/uint/float
    /// returns and a signedness-punned parameter): CoreCLR returned 23 and PawPrint 31,
    /// differing on the float bit alone. That is why only `Single`/`Double` are separated and
    /// the integer widths and signedness deliberately are not — conflating those matches the
    /// real runtime, and splitting them would reject calls that work.
    [<Test>]
    let ``calli refuses a punned float width at the faulting instruction`` () =
        let source =
            """
public class Program
{
    static float Id(float x) => x;

    public static unsafe int Main(string[] args)
    {
        delegate*<float, float> p = &Id;
        double r = ((delegate*<float, double>)p)(1.5f);
        return r == 1.5 ? 42 : 7;
    }
}
"""

        let exn =
            Assert.Catch (fun () ->
                runPawPrintSource "CalliPunnedFloatWidth.cs" source KernelConfig.Default (fun _image _result -> ())
            )

        exn.Message |> shouldContainText "calli"
        exn.Message |> shouldContainText "return"
        exn.Message |> shouldContainText "Program"

    [<Test>]
    let ``Unhandled rethrow preserves original throw stack frame`` () =
        let source =
            """
using System;

class StackTraceSentinelException : Exception
{
}

class Program
{
    static void Blow()
    {
        throw new StackTraceSentinelException();
    }

    static int Main(string[] args)
    {
        try
        {
            Blow();
        }
        catch
        {
            throw;
        }

        return 1;
    }
}
"""

        runPawPrintSource
            "RethrowStackTrace.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    match exn.StackTrace with
                    | firstFrame :: _ -> firstFrame.Method.Name |> shouldEqual "Blow"
                    | [] -> failwith "Expected an unhandled rethrow to keep the original throw stack frame"
                | outcome -> failwith $"Expected an unhandled rethrow, got %O{outcome}"
            )

    [<Test>]
    let ``Emulated environment exposes invariant globalization switch`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        return Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") == "1" ? 0 : 1;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentInvariantGlobalization.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Emulated environment returns configured variables and null for missing variables`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("PAWPRINT_TEST_VARIABLE") != "configured")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("DOTNET_SYSTEM_GLOBALIZATION_INVARIANT") != "1")
        {
            return 5;
        }

        string missing = Environment.GetEnvironmentVariable("PAWPRINT_MISSING_VARIABLE");

        if (missing == "configured")
        {
            return 2;
        }

        if (missing == "")
        {
            return 3;
        }

        if (missing != null)
        {
            return 4;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentConfiguredVariables.cs"
            source
            { KernelConfig.Default with
                Environment = [ "PAWPRINT_TEST_VARIABLE", "configured" ] |> Map.ofList
            }
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``GetEnvironmentVariableW lookup is case-sensitive`` () =
        // CoreCLR's Unix PAL implements the `kernel32!GetEnvironmentVariableW`
        // import with exact name comparison (see pal/src/misc/environ.cpp
        // `FindEnvVarValue`), so the QCall shim must do exact-string lookup
        // against the kernel env map even though the *Windows* kernel32 entry
        // would be case-insensitive: PawPrint is baselined against the host
        // runtime, which is the Unix PAL on the hosts this repo runs on.
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        if (Environment.GetEnvironmentVariable("PaWpRiNt_MiXeD_CaSe_KeY") != "found")
        {
            return 1;
        }

        if (Environment.GetEnvironmentVariable("pawprint_mixed_case_key") != null)
        {
            return 2;
        }

        if (Environment.GetEnvironmentVariable("PAWPRINT_MIXED_CASE_KEY") != null)
        {
            return 3;
        }

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentCaseSensitiveLookup.cs"
            source
            { KernelConfig.Default with
                Environment = [ "PaWpRiNt_MiXeD_CaSe_KeY", "found" ] |> Map.ofList
            }
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Emulated environment preserves missing variable last PInvoke error`` () =
        let source =
            """
using System;
using System.Runtime.InteropServices;

class Program
{
    static int Main(string[] args)
    {
        Marshal.SetLastPInvokeError(0);

        string missing = Environment.GetEnvironmentVariable("PAWPRINT_MISSING_VARIABLE");

        if (missing != null)
        {
            return 1;
        }

        return Marshal.GetLastPInvokeError() == 203 ? 0 : 2;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentMissingVariableLastPInvokeError.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.NormalExit (terminalState, terminatingThread) ->
                    match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
                    | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ -> exitCode |> shouldEqual 0
                    | [] -> failwith "expected program to return an int, but it returned void"
                    | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
                | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
                | RunOutcome.Aborted (_, _, fatal) ->
                    let m = fatal.Message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"
            )

    [<Test>]
    let ``Environment.FailFast aborts execution`` () =
        let source =
            """
using System;

class Program
{
    static int Main(string[] args)
    {
        Environment.FailFast("boom");
        return 0;
    }
}
"""

        runPawPrintSource
            "EnvironmentFailFast.cs"
            source
            KernelConfig.Default
            (fun _image pawPrintResult ->
                match pawPrintResult with
                | RunOutcome.Aborted (_, _, fatal) ->
                    fatal.Code |> shouldEqual FatalErrorCode.FailFast
                    fatal.Message |> shouldEqual (Some "boom")
                | RunOutcome.NormalExit _ -> failwith "expected FailFast, got normal exit"
                | RunOutcome.ProcessExit _ -> failwith "expected FailFast, got process exit"
                | RunOutcome.SignalTerminated (_, signal) ->
                    failwith $"expected FailFast, got POSIX signal termination: %O{signal}"
                | RunOutcome.GuestUnhandledException (_, _, exn) ->
                    failwith $"expected FailFast, got guest unhandled exception: %O{exn.ExceptionObject}"
            )

    [<TestCaseSource(nameof simpleCases)>]
    let ``Standard tests`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof seededCaseNames)>]
    let ``Seeded filesystem tests`` (fileName : string) =
        // A seed naming a Unix mode has no Windows equivalent, so the oracle
        // cannot stand in for it and the case is skipped rather than failed.
        // Asked of `RealRuntime` rather than decided here, so that the skip and
        // the validator's refusal cannot disagree about which seeds qualify.
        if not (RealRuntime.canMaterialise seededCases.[fileName]) then
            Assert.Ignore $"%s{fileName}'s seed names Unix permission bits, which this host cannot give a real file."

        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig =
                { KernelConfig.Default with
                    FileSystem = seededCases.[fileName]
                }
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof customExitCodes)>]
    let ``Custom exit code tests`` (KeyValue (fileName : string, exitCode : int)) =
        if unimplemented.Contains fileName then
            Assert.Inconclusive ()

        {
            FileName = fileName
            ExpectedReturnCode = exitCode
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof expectsUnhandledException)>]
    let ``Tests which throw unhandled exceptions`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0 // not checked; both runtimes are expected to throw
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = true
            AssertTerminalState = None
        }
        |> runTest

    [<TestCaseSource(nameof unimplemented)>]
    let ``Unimplemented tests have correct real-runtime behaviour`` (fileName : string) =
        let source = Assembly.getEmbeddedResourceAsString fileName assy
        let image = Roslyn.compile [ source ]

        let expectedExitCode =
            customExitCodes |> Map.tryFind fileName |> Option.defaultValue 0

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual expectedExitCode
        | RealRuntimeResult.UnhandledException report ->
            failwith $"Real runtime terminated with an unhandled exception for %s{fileName}:\n%s{report}"
        | RealRuntimeResult.Aborted (_code, report) ->
            failwith $"Real runtime called Environment.FailFast for %s{fileName}:\n%s{report}"

    [<TestCaseSource(nameof unimplemented)>]
    [<Explicit>]
    let ``Can evaluate C# files, unimplemented`` (fileName : string) =
        {
            FileName = fileName
            ExpectedReturnCode = 0
            KernelConfig = KernelConfig.Default
            AppContext = AppContextProperties.empty
            ExpectsUnhandledException = false
            AssertTerminalState = None
        }
        |> runTest

    /// The one thing every `runPawPrintSource` case below wants from its outcome:
    /// that the guest ran to completion and returned the exit code it promised.
    /// Its own assertions are the interesting part, so a wrong code is reported
    /// with the code the guest actually chose.
    let private expectExitCode (expected : int) (outcome : RunOutcome) : IlMachineState =
        match outcome with
        | RunOutcome.NormalExit (terminalState, terminatingThread) ->
            match terminalState.ThreadState.[terminatingThread].MethodState.EvaluationStack.Values with
            | EvalStackValue.Int32 (Int32Source.Verbatim exitCode) :: _ ->
                exitCode |> shouldEqual expected
                terminalState
            | [] -> failwith "expected program to return an int, but it returned void"
            | ret :: _ -> failwith $"expected program to return an int, but it returned %O{ret}"
        | RunOutcome.ProcessExit _ -> failwith "expected normal exit, got process exit"
        | RunOutcome.Aborted (_, _, fatal) ->
            let m = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"expected normal exit, got an abort (%O{fatal.Code}): %s{m}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"expected normal exit, got POSIX signal termination: %O{signal}"
        | RunOutcome.GuestUnhandledException (_, _, exn) ->
            failwith $"guest threw unhandled exception: %O{exn.ExceptionObject}"

    /// The variables `Environment.GetEnvironmentVariables` is asserted against
    /// below, chosen so that no single mistake in the environment block satisfies
    /// them all: an empty value (which must stay present-and-empty rather than
    /// vanishing, since an entry with no `=` is one CoreLib discards), a value
    /// containing `=` (which must not be split), non-ASCII including an astral
    /// character (two UTF-16 code units, so a byte/code-unit confusion shows), and
    /// two names where one is a prefix of the other.
    let private environmentVariablesSeed : Map<string, string> =
        Map.ofList
            [
                "PAWPRINT_EMPTY", ""
                "PAWPRINT_EQUALS", "a=b=c"
                "PAWPRINT_UNICODE", "\u00e9\u4e2d\U0001F436"
                "PAWPRINT_P", "1"
                "PAWPRINT_PP", "2"
            ]

    /// `environmentVariablesSeed` plus the count the guest should see, derived
    /// from the overlay rule rather than written down: the kernel's table is
    /// `EmulatedKernel.defaultEnvironment` with the seed laid over it, and this
    /// entry is itself one more variable.
    ///
    /// Derived rather than hardcoded because the count is the assertion that
    /// catches a dropped or duplicated entry, and a hand-maintained number would
    /// silently stop matching if the seed or the defaults changed.
    let private environmentVariablesConfig : Map<string, string> =
        let overlaid =
            (EmulatedKernel.defaultEnvironment, environmentVariablesSeed)
            ||> Map.fold (fun acc key value -> Map.add key value acc)

        environmentVariablesSeed
        |> Map.add "PAWPRINT_EXPECTED_COUNT" (string (Map.count overlaid + 1))

    [<Test>]
    let ``GetEnvironmentVariables reports exactly the emulated environment`` () =
        // Impure in spirit — the value asserted is PawPrint's own seeded table,
        // which the real runtime cannot be an oracle for, since it would report
        // whatever environment the test host was started with. The cross-runtime
        // half of the contract lives in sourcesPure/EnvironmentGetVariables.cs.
        //
        // The strongest check here is the round-trip: every variable the
        // environment *block* reported must agree with what
        // `Environment.GetEnvironmentVariable` answers, and that reads
        // `Kernel.Environment` directly through a different QCall. So a block
        // whose entries split at the wrong `=` produces a name the table does not
        // hold, and fails — without this test having to know the table itself.
        let source =
            """
using System;
using System.Collections;

class Program
{
    static int Main(string[] args)
    {
        IDictionary vars = Environment.GetEnvironmentVariables();

        string expectedCount = Environment.GetEnvironmentVariable("PAWPRINT_EXPECTED_COUNT");
        if (expectedCount == null) return 1;

        // Pins that no entry was dropped or duplicated, which a missing or
        // doubled block terminator would cause.
        if (vars.Count != int.Parse(expectedCount)) return 2;

        foreach (DictionaryEntry entry in vars)
        {
            string key = (string)entry.Key;
            string value = (string)entry.Value;

            // CoreLib discards an entry whose first '=' is not after the first
            // character, so a key that is empty or contains '=' cannot come out
            // of a well-formed block at all.
            if (key.Length == 0) return 3;
            if (key.IndexOf('=') >= 0) return 4;

            if (Environment.GetEnvironmentVariable(key) != value) return 5;
        }

        // Present-and-empty, not absent.
        if (!vars.Contains("PAWPRINT_EMPTY")) return 6;
        if ((string)vars["PAWPRINT_EMPTY"] != "") return 7;

        // Only the first '=' separates; the rest belongs to the value.
        if ((string)vars["PAWPRINT_EQUALS"] != "a=b=c") return 8;

        // Three characters, four UTF-16 code units.
        if ((string)vars["PAWPRINT_UNICODE"] != "\u00e9\u4e2d\U0001F436") return 9;

        // A name that is a prefix of another stays distinct from it.
        if ((string)vars["PAWPRINT_P"] != "1") return 10;
        if ((string)vars["PAWPRINT_PP"] != "2") return 11;

        // A name never configured is absent rather than empty.
        if (vars.Contains("PAWPRINT_NEVER_SET")) return 12;

        // A second call must build a fresh dictionary from a fresh block, so the
        // first call cannot have cached or consumed anything.
        IDictionary again = Environment.GetEnvironmentVariables();
        if (ReferenceEquals(again, vars)) return 13;
        if (again.Count != vars.Count) return 14;
        if ((string)again["PAWPRINT_EQUALS"] != "a=b=c") return 15;

        return 0;
    }
}
"""

        runPawPrintSource
            "EmulatedEnvironmentGetVariables.cs"
            source
            { KernelConfig.Default with
                Environment = environmentVariablesConfig
            }
            (fun _image pawPrintResult -> expectExitCode 0 pawPrintResult |> ignore<IlMachineState>)

    [<Test>]
    let ``GetEnvironmentVariables releases every block it allocates`` () =
        // A leaked environment block is invisible to the guest — no double free,
        // no use-after-free, and the next call simply allocates another — so the
        // only way to see whether `FreeEnvironmentStringsW` really frees is to
        // count what the native heap still owns when the process ends.
        //
        // Asserted as "independent of how many blocks were taken" rather than
        // against a fixed number, because the interpreter's own startup may leave
        // native blocks of its own and this test should not have to know how
        // many. If the free were a no-op, the four-call run would end with three
        // more live blocks than the one-call run.
        let source =
            """
using System;
using System.Collections;

class Program
{
    static int Main(string[] args)
    {
        string raw = Environment.GetEnvironmentVariable("PAWPRINT_CALL_COUNT");
        if (raw == null) return 1;

        int calls = int.Parse(raw);
        int total = 0;

        for (int i = 0; i < calls; i++)
        {
            IDictionary vars = Environment.GetEnvironmentVariables();
            total += vars.Count;
        }

        // Every call saw the same table, so a torn or partly-freed block shows up
        // here rather than being averaged away.
        if (total % calls != 0) return 2;

        return 0;
    }
}
"""

        let liveBlocksAfter (calls : int) : int =
            let mutable live = -1

            runPawPrintSource
                "EmulatedEnvironmentBlockLifetime.cs"
                source
                { KernelConfig.Default with
                    Environment = Map.ofList [ "PAWPRINT_CALL_COUNT", string calls ]
                }
                (fun _image pawPrintResult ->
                    let terminalState = expectExitCode 0 pawPrintResult
                    live <- NativeMemoryPool.liveBlockCount terminalState.Kernel.NativeMemoryPool
                )

            live

        let afterOne = liveBlocksAfter 1
        let afterFour = liveBlocksAfter 4

        afterFour |> shouldEqual afterOne
