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
            "DelegateCombine.cs" // Multicast delegates (issue #959). `Delegate.Combine` reaches `MulticastDelegate.NewMulticastDelegate` (MulticastDelegate.CoreCLR.cs:168), which needs four things PawPrint did not have. The first is now implemented: `RuntimeTypeHandle.InternalAllocNoChecks`, which allocates the new multicast instance (`TestInternalAllocNoChecks` covers it directly, because no guest can reach it without immediately hitting the next blocker). Measured by un-parking on top of that: the guest now stops at `System.Delegate::GetMulticastInvoke(MethodTable*)`, an unimplemented InternalCall with a `Delegate_GetMulticastInvokeSlow` QCall fallback, which supplies the new delegate's `_methodPtr`; `System.Delegate::GetInvokeMethod(MethodTable*)` supplies its `_methodPtrAux` and is one instruction further on. Both need a decision about what a "multicast invoke stub" even is in PawPrint's `NativeIntSource.FunctionPointer` model, which has no such shape today. The fourth is dispatch: `AbstractMachine.dispatchDelegateInvoke` reads `_target` and `_methodPtr` and performs exactly one call, so it must learn to walk `_invocationList[0 .. _invocationCount-1]` — note that the array is longer than the count, since `CombineImpl` grows it by doubling, so honouring `_invocationCount` rather than the array length is the thing to get right. That needs a frame surviving N sequential calls, which is the same shape as the limitation `DelegateToActivatorCreateInstance.cs` documents. Un-park when the stub pointers and multicast dispatch land; they interact, since what dispatch needs to see in `_methodPtr` determines what the stub pointers should be.
            "AreSameProjectionCrossesArrayElement.cs" // `Unsafe.AreSame` between a byte view of `a[0].Y` advanced 4 bytes and a byte view of `a[1]`, for `struct Pair { int X; int Y }`. Those are one address and real .NET says so, but the byrefs keep different `ByrefRoot.ArrayElement` roots because the intervening `Field` stops the trailing cursor folding into the element index. Measured returning 1 before the refusal existed, on the reasoning that distinct array elements are disjoint — which is true of the elements and irrelevant once a projection can walk out of the one it started from. PawPrint now refuses. This is the guest that forced the root arm to be stated as "neither byref may have left its root's extent" rather than as a list of root kinds; its siblings all compare byrefs that stay within one root. Un-park with them.
            "AreSameHeapFieldsOverlappingExplicitLayout.cs" // The heap-object counterpart of the struct case below: `[StructLayout(LayoutKind.Explicit)]` on a *class*, so `ldflda` yields two byrefs with different `ByrefRoot.HeapObjectField` roots rather than two `Field` projections over one root. Real .NET says `true`; PawPrint refuses. Measured returning 1 before the refusal existed, when distinct roots were taken to be distinct storage — which is a fact about how each byref was built, not about where it points. Separate from the struct sibling because it exercises the *root* arm rather than the *projection* arm, and fixing one would not cover the other. Un-park with the other two `AreSame*` guests.
            "AreSameExplicitLayoutOverlappingFields.cs" // `Unsafe.AreSame(ref u.A, ref u.B)` where `[FieldOffset(0)]` puts both fields of an explicit-layout struct on one address. Real .NET says `true`; PawPrint refuses, because its byrefs are `[Field A]` and `[Field B]` over one root and only the declaring type's field-offset table separates that from a sequential struct where the same pair of chains means two different addresses. Measured: before the refusal existed this returned 1, i.e. the comparison answered `false`. Worth keeping alongside the sibling below because it is the counterexample to the obvious shortcut — "distinct fields occupy disjoint extents" holds for sequential and auto layout but not for explicit — and because it establishes that such values reach `Field` projections rather than collapsing to a byte range, which the byte-backed explicit-layout parked tests might otherwise suggest. Same root cause as the sibling; un-park both together.
            "AreSameFirstFieldVersusReinterpretedWhole.cs" // `Unsafe.AreSame(ref a.X, ref Unsafe.As<A, int>(ref a))` for a single-field struct. Both byrefs are the same address and real .NET says so. PawPrint now *refuses* the comparison rather than answering it: after the trailing `ReinterpretAs` is stripped, one side is `Byref (local, [Field X])` and the other the bare `Byref (local, [])`, and whether those alias depends on whether `X` sits at offset 0 of its declaring type — field-offset layout that `ceqNormalised` does not carry. It used to answer `false`, which was a silent wrong answer; the refusal names both byrefs. So this guest fails loudly at the first of its two halves rather than returning the 3 it used to. Deciding it needs either the offset-0 predicate or full byte-offset byref identity, and the latter cannot be total while reference- and pointer-containing values remain byte-imageless — so it is its own change. The refusal itself is covered by `TestByrefComparison.fs`, since a parked guest is only ever run against real .NET and so cannot assert PawPrint's side.
            "EnumQueriesOpenGenericSharedParent.cs" // `IsEnum` and `IsActualEnum` on an open generic *definition* whose base type still mentions the type parameter (`class Derived<T> : Base<T>`). Both read `MethodTable::ParentMethodTable`, whose projection goes through `resolveBaseRuntimeTypeHandleTarget`; that walk refuses precisely this shape (IlMachineRuntimeMetadata.fs:420, "base type ... references generic parameters (shared/canonical parent); only closed parents are supported today"), because naming the parent needs the definition's formals substituted into the base signature. That is the "a parent whose base type mentions a parameter" exclusion #899 called out as deliberately not done when it added open constructed types, and re-measuring on top of #899 shows it unchanged. Nothing here is about `IsActualEnum` or about the safe-intrinsic allowlist: plain `typeof(Derived<>).IsEnum` — virtual, overridden whole by `RuntimeType`, so it consults no allowlist entry — aborts identically, which is how this was measured. The two parent shapes that *do* resolve (`System.Object`, and a closed non-generic base) are controls at the top of this file and are also asserted by the active `TypeIsActualEnum.cs`. The last two checks are what stop the file being satisfiable the wrong way: answering "no parent at all" would make every enum query here return false correctly — a null parent is how CoreCLR spells `System.Object` — so `BaseType.Name` and `IsSubclassOf(typeof(object))` pin that the real base was found. Un-park when `resolveBaseRuntimeTypeHandleTarget` can name an open constructed parent.
            "TypeIsValueTypeMethodGenericParameter.cs" // `Type.IsValueType` on a *method*-level generic parameter. The type-level counterpart passes (`TypeIsValueTypeIsEnumGenericParameter.cs`), and by the same route: `RuntimeType.IsValueTypeImpl` sees `IsTypeDesc` for a VAR or an MVAR alike and defers to `IsSubclassOf(typeof(ValueType))`, which walks the parameter's base type. What stops the MVAR case is one step below that walk — `RuntimeTypeHandle_GetConstraints` (Native/NativeRuntimeTypeQCall.fs) serves `RuntimeTypeHandleTarget.GenericParameter` but fails with a precise TODO for `MethodGenericParameter`, because a method-level parameter's constraints hang off the `GenericParamConstraint` rows of the enclosing *method* rather than of the declaring type. Nothing here is about `IsValueType`: `GetGenericArguments()` on the MethodInfo already succeeds, and the same QCall blocks `IsEnum` on an MVAR too. Un-park when that QCall learns the method-level parent.
            "GCMemoryInfoSpanProperties.cs" // `GC.GetGCMemoryInfo()` itself works (see GCGetMemoryInfo.cs), but its two span-valued properties don't past index 0. CoreLib builds them with `MemoryMarshal.CreateReadOnlySpan(ref _generationInfo0, 5)` / `(ref _pauseDuration0, 2)`: a byref to the *first* of a run of sibling fields, walked forward by sizeof(element), relying on `[StructLayout(Sequential)]` having laid them out contiguously. PawPrint models a heap object as named field cells, not a byte block, so `GenerationInfo[1]` becomes a byte-view read at offset 48 into `_generationInfo0`'s own 32-byte cell and fails in `IlMachineManagedByref.resolveCell` (IlMachineManagedByref.fs:1086) with "does not fit in single primitive cell". `Length` and index 0 both work. This is the general "sibling fields are not contiguous" gap rather than anything GC-specific; tracked as issue #729.
            "InterfaceSlotHiddenByDerivedMethod.cs" // PawPrint does not model interface slot ownership — which type's method implements a given interface-map entry's slot. `findClassImplementation` starts at the receiver and takes the first name/signature match, and `methodMatches` skips its non-virtual/`newslot` guard whenever the call target is an interface, so any same-signature method on the way down wins. The file covers both directions: a derived type that must *not* take an inherited slot (reproducible with no variance at all, and failing identically on `main`), and one that *must* take a slot it re-declares. Fixing it needs a real slot-to-implementation dispatch map, which changes ordinary non-variant interface dispatch too and so wants its own change; `VariantInterfaceSlotOwnership.cs` covers the cases the interface map alone can get right.
            "BulkMoveAcrossOverlappedStructPadding.cs" // A bulk move across padding that *two* fields cover, which explicit layout produces by overlaying identical reference-containing structs. `CliType.TryPaddingRunAt` refuses such a byte — with two fields over it there is no single one to descend through, so it cannot say whose padding it is — and that refusal is the one gap left in the padding step that `BulkMoveAcrossStructPadding.cs` covers. Parked because the refusal is currently unreachable, and measured rather than assumed: an explicit-layout struct with any overlap is stored byte-backed, a byte-backed value holding references cannot be field-accessed, and so the plain `src[i].First.N = i + 1` that *builds* the array stops in `CliType.OfBytesLike` with "non-primitive template ObjectRef None" before any copy happens. Allocating the array alone succeeds; it is the first field write that fails. Same blocker as `ReinterpretCellUnderAliasedAncestor.cs`; un-park when that lands, at which point `TryPaddingRunAt`'s two-fields-cover-it branch becomes load-bearing for the first time and will need to learn that padding shared by fields which are padding there too is still padding.
            "ReinterpretCellUnderAliasedAncestor.cs" // A named cell under an explicit-layout ancestor that an unrelated sibling overlaps. Parked on a gap well below cell naming: an explicit-layout struct with any overlap is stored byte-backed, so `CliValueType.DereferenceFieldById` rebuilds a field via `OfBytesLike`, which refuses non-primitive templates — so reference-containing explicit-layout structs cannot be field-accessed at all. The program fails at a plain `outer.Whole.R = ...` before reaching any reinterpret. Not a regression: that path is untouched by the cell resolver.
            "ReinterpretReadNestedFieldThroughIndex.cs" // Reading a field of a nested struct directly through an inline-array index (`buf[k].I.P`), one step deeper than the `buf[k].Field` shape that works. Not the cell resolver: `CliType.CellPathsExactlyCovering` descends to any depth and `TestCliTypeCellPaths` covers depth 3. Nor, any longer, the projection walk: `walkProjectionByteOffset` folds `ByteOffset` followed by `Field` since the guard relaxation, so the peeled chain `[ByteOffset k*sizeof(Elem); Field I; Field P]` resolves fine. The one remaining blocker is routing in `readManagedByrefField`, whose reinterpret-aware arms only fire when `ReinterpretAs` is last (or last-but-a-`ByteOffset`); with a trailing `Field` the chain falls through to `readProjectedValue`, which cannot cross a reinterpret. Un-park when that dispatcher learns to route a chain that contains but does not end at a `ReinterpretAs` to the byte-view reader.
            "ActivatorCreateInstanceStructCtor.cs" // `Activator.CreateInstance` on a value type declaring an explicit parameterless ctor. `RuntimeTypeHandle_GetActivationInfo` is implemented and covered by `ActivatorCreateInstance.cs`, but CoreCLR returns that ctor's *boxed* entry point in `ppfnRefCtor` (reflectioninvocation.cpp:1665, `forceBoxedEntryPoint = isValueType`) and `CreateInstanceDefaultCtor` calls exactly that one. `NativeIntSource.FunctionPointer` carries a target with no entry-point flavour, so the boxed entry point is unrepresentable and the QCall fails loudly instead of invoking the ctor with an ObjectRef receiver — which would risk constructing into a copy of the box's payload. Un-park when function pointers can name a boxed entry point.
            "MarshalStructureToPtrDecimalField.cs" // `StructMarshalStub.isBlittableField` rejects Decimal fields (CoreCLR routes them through `NFT_DECIMAL` stub synthesis because native `DECIMAL` is 8-byte aligned while managed `Decimal` is 4-byte aligned), so the struct reaches the has-layout-non-blittable arm and `tryComputePlan` then declines it. The stub machinery a Decimal field would run on now exists — it is what marshals a `DateTime` field — so what remains is Decimal-specific. (1) `Marshal.SizeOf&lt;{int; decimal}&gt;()` returns 20 instead of 24 because the marshal-layout walk in `CliValueType.TryComputeMarshalLayout` does not bump Decimal's field alignment to 8, so the placement handed to the stub is already wrong. (2) There is no `StructMarshalFieldKind` for it. Native `DECIMAL` is one contiguous 16-byte range like any other struct member, so the step's one-range-per-field shape is fine; what differs is the *interior*, since `DECIMAL` orders its members (`wReserved`, `scale`, `sign`, `Hi32`, `Lo64`) differently from managed `System.Decimal`'s `flags`/`hi`/`lo`/`mid`. That is a richer `Kind`, not a richer step.
            "AutoInlineArrayElementSizeRounding.cs" // `[InlineArray(N)]` on a type that declares `LayoutKind.Auto`, where the element's own size is not already its rounded size. CoreCLR lays such a type out as its *one* declared field and multiplies the resulting instance size by N (`PlaceInstanceFields`, methodtablebuilder.cpp:8612), so the auto route's size rounding applies to one element: three 3-byte elements are 12 bytes, not 9. PawPrint materialises N storage slots (`InlineArrayStorage.expand`) and lays them out together, so the rounding would apply once to the whole run and give 16 for a run of three ints — wrong in the *other* direction, and wrong for the common case. `CliValueType.AutoLayoutGoverns` therefore keeps inline-array expansions on the sequential route, which is exactly right for every element whose size equals its rounded size (all primitives, hence every shape a guest is likely to write) and leaves this one case short. Closing it needs the repeat count to reach the sizing code, which today sees only the expanded slots; the file's other three cases pass and pin what that change must not disturb.
            "MakeGenericMethodOpenArgument.cs" //`RuntimeMethodHandle_GetStubIfNeededSlow` (issue #743) handles `MakeGenericMethod` with closed type arguments, which is what every reachable path needs, but an argument that still contains generic parameters — `MakeGenericMethod(typeof(G<>))` or `MakeGenericMethod(someTypeParameter)` — cannot be represented. Both are legal: real .NET returns a MethodInfo with `ContainsGenericParameters = true`, inspectable but not invokable. PawPrint's `MethodHandle.MethodGenerics` is a `ConcreteTypeHandle list`, and `ConcreteTypeHandle` indexes `AllConcreteTypes`, whose entries carry only *closed* generic arguments, so the QCall fails with a precise TODO. Widening that representation reaches concretization and every other MethodHandle consumer, so it is its own change rather than part of the QCall.
            "ComparerDefaultEnumCompare.cs" // `Comparer<TEnum>.Default` *selection* works and is asserted by the sibling `ComparerDefault.cs`; what is parked here is calling the comparer it returns. `EnumComparer<T>.Compare` delegates to `RuntimeHelpers.EnumCompareTo<T>` (Comparer.CoreCLR.cs:19), a distinct [Intrinsic] which PawPrint has not reviewed for the safe-intrinsic allowlist, so it stops at the `TODO: implement JIT intrinsic` failure in `callMethod` (IlMachineStateExecution.fs:1931). An allowlist entry alone is not enough: its IL body is `ldarga.s 0; ldarg.1; box T; constrained. T; callvirt Enum::CompareTo(object); ret`, so un-parking needs `Enum.CompareTo(object)` to be reachable under a `constrained.` callvirt on a boxed enum.
            "ReflectionInvokeIntrinsicTarget.cs" // `MethodBase.Invoke` on a method PawPrint services as a JIT intrinsic rather than by interpreting IL (`Unsafe.SizeOf<long>()`; real .NET treats it as an ordinary reflectable method and answers 8). The blocker is in the call path, not in the `RuntimeMethodHandle_InvokeMethod` QCall's own bookkeeping: `callMethodWithCommitment` services such a method inline, computing the result and then advancing the *caller's* program counter — right for a `call` opcode, but here the caller is the native QCall frame, which has no IL. It also reports `CallCommitment.Committed`, so the QCall's commitment check cannot catch it; the QCall therefore rejects the shape up front, so the failure names the method instead of aborting inside `advanceProgramCounter`. Un-parking means letting `Intrinsics.call` honour `advanceProgramCounterOfCaller = false`, which reaches every intrinsic's completion path (~70 sites across `Intrinsics.fs` and `IntrinsicHelpers.fs`).
            "ReflectionInvokePointerSignature.cs" // `MethodBase.Invoke` on a target whose signature mentions a pointer, in both directions. Reflection does not pass the CLR representation through for pointers, so each direction needs its own work in the `RuntimeMethodHandle_InvokeMethod` QCall, and each is rejected loudly there today. Argument side: `MethodInvokerCommon.Initialize` sets `InvokerArgFlags.IsValueType` for a pointer parameter, so its byref-buffer entry addresses the payload of a boxed `IntPtr` rather than an `object?` slot, which the QCall's reference-type read path cannot serve; `argumentIsValueType` also says false for a structural pointer handle, so the shape needs naming rather than inferring. Return side: `InvokeUtil::CreateObjectAfterInvoke` wraps an `ELEMENT_TYPE_PTR` return in a `System.Reflection.Pointer` carrying the pointed-to Type (so `Invoke` never answers null even for a null pointer) and boxes a function-pointer return as an `IntPtr`; PawPrint has no `Pointer` construction yet. Un-park when both land.
            "ReflectionVirtualSlotsGenericDefinitionLayout.cs" // Vtable slots are laid out on the *generic definition*, so `A<T>.M(T)` and `B<T>.M(string)` occupy different slots and `C<T>.M(T)` overrides the former; reflecting over `C<string>` must still report `B.M`. PawPrint matches an override against base slots by concretising both signatures, i.e. after substituting the declaring types' generic arguments, so at `T = string` the two inherited signatures have become identical and the candidate matches both. Measured against the real runtime, which reports `C3`/`B3` where matching closed signatures yields `C3`/`A3`. `vtableOfClosed` detects the shape (several matches with generics in play) and fails with a TODO rather than answering wrongly. It also covers the two shapes that show why a closed walk cannot decide the question: a `new virtual` shadow whose signature mentions the parameter (a genuine tie), and `Kb<T> : Ka<string>`, where both inherited signatures are raw `[!0]` and yet denote different things because a raw `!0` is scoped to the type that wrote it. Fixing it means walking the generic definition's base chain with generic parameters kept symbolic — the same capability `RuntimeTypeHandle.GetNumVirtuals` lacks for open generic type definitions, and a change to how PawPrint models vtables rather than to this matcher. Un-park then.
            "ReflectionGenericVirtualMethodOverrideSlots.cs" // Vtable slot layout for a *generic* virtual method that a derived type overrides (`class Gb : Ga` overriding `virtual void M<T>(T)`) — ordinary C#, not hand-written IL. `candidateFillsSlot` compares two signatures by concretising both into a common form, and there is no `ConcreteTypeHandle` standing for "method generic parameter i" (`AllConcreteTypes` entries carry only closed arguments), so `concretiseSignatureForSlotMatch` refuses rather than compare coarsely and risk binding the override to the wrong slot. Because `numVirtualsOfClosed` is now the vtable's *length*, that refusal poisons every reflection query on such a type, not merely one asking after the generic method — measured: `typeof(Gb).GetMethods()` stops in `RuntimeTypeHandle.GetNumVirtuals`. CoreCLR needs no substitution to decide this: `MetaSig::CompareMethodSigs` compares `ELEMENT_TYPE_MVAR` positionally, and this code already knows the arities are equal by the time it gives up, so what is missing is a *symbolic* positional comparison rather than a better closed one. Narrowly a reachability regression against the pre-#856 `numVirtualsOwn`, which summed per-type newslot counts and so never compared signatures at all: on that code a query matching only *non-virtual* methods of such a type answered, while anything touching a virtual died at the then-unimplemented `GetSlot` regardless — so no shape that used to work end-to-end stopped working. The non-generic `Name()` assertions are controls, so a failure here is attributable to the generic method rather than to the walk as a whole. Un-park when the slot matcher can compare method-generic parameters symbolically.
            "ReflectionInvokeVirtualMethod.cs" // `MethodBase.Invoke` on a virtual method looked up through the base class that declares it. The `RuntimeMethodHandle.GetSlot` blocker this was originally parked for has landed, so the `GetMethod` lookup now succeeds (`sourcesPure/ReflectionVirtualMethodSlots.cs` covers the slot layout it needed) and the guest gets as far as the invocation itself, where it stops at the next unimplemented primitive: the QCall `CastHelpers::IsInstanceOf_NoCacheLookup` (`System.Private.CoreLib System.Runtime.CompilerServices.CastHelpers::<IsInstanceOf_NoCacheLookup>g____PInvoke|4_0(*(System.Void), System.Int32, System.Runtime.CompilerServices.ObjectHandleOnStack) -> System.Int32`). That is reached because the receiver is a `Derived` while the declaring type is `Base`, so `InvokeUtil`'s type check needs a real hierarchy walk and misses the managed cast cache. Nothing here is about slots or about invocation bookkeeping: the QCall passes `performInterfaceResolution = true` so that a vtable method dispatches virtually, mirroring CoreCLR's `GetSingleCallableAddrOfVirtualizedCode` (reflectioninvocation.cpp:417-424), and this file is what will check that claim once the cast helper lands. Un-park then.
            "LdvirtftnIntrinsicDeclaringType.cs" // A delegate built by `ldvirtftn` whose resolved body is declared on a type carrying a type-level `[Intrinsic]`. `callvirt` gets this right by keying the *type-level* check on the call site's static declaration and only the *method-level* one on the resolved body (`callMethodWithCommitment`, IlMachineStateExecution.fs:1605-1617) — so `callvirt Object::GetHashCode()` on a boxed `Int128` interprets the override, and the file's direct-call control passes. A delegate cannot make that distinction: `ldvirtftn` must bind eagerly (delegate invocation runs with `performInterfaceResolution = false`), so the pointer names `Int128::GetHashCode`, and `dispatchDelegateInvoke` then hands that same method to `callMethod` as both call site and target, whereupon the type-level `[Intrinsic]` on `Int128` fires and it stops at "TODO: implement JIT intrinsic System.Int128.GetHashCode()". Nothing here is about dispatch, which resolves correctly; the missing piece is that `FunctionPointerTarget.Managed` carries only a body, with no room for the declaration the call site named, so the distinction `callvirt` relies on is unrepresentable in a function pointer. Un-park when a function pointer can carry its call-site declaration alongside its target.
            "DelegateToActivatorCreateInstance.cs" // A delegate whose target is `Activator.CreateInstance<T>()` for a `T` whose `.cctor` has not yet run. PawPrint services that method as an intrinsic which runs `T`'s initialiser and then reports `CallCommitment.SuspendedForClassInit`, asking its caller to re-execute once the initialiser returns. A call *opcode* can honour that by leaving its program counter unadvanced; `dispatchDelegateInvoke` cannot, because the delegate's synthetic `Invoke` frame is already popped by the time the target is called, so there is no frame to re-enter. It therefore refuses loudly, naming the situation. Distinct from the type initialisation this file's sibling tests cover: that one is for the *target's declaring type* (`System.Activator`, long since initialised) and runs *before* the frame is popped, which is exactly what lets it suspend safely. Pre-existing, and previously silent — before the commitment was checked, the suspension was dropped and the guest died downstream with "eval stack was empty on pop instruction", naming neither the delegate nor the activator. Un-parking means giving delegate invocation a frame that survives the call, which is the same shape as `calli`'s save-and-retry and reaches every delegate, so it is its own change.
            "RuntimeHelpersBoxReferenceContainingStruct.cs" // `RuntimeHelpers.Box` of a struct holding a reference — the one shape whose box CoreCLR fills with `Buffer.BulkMoveWithWriteBarrier` rather than `SpanHelpers.Memmove` (RuntimeType.BoxCache.cs:91). Nothing here is about `ReflectionInvocation_GetBoxInfo`, which serves this type fine: the guest never reaches `Box` at all. It dies one step earlier, in its own `Unsafe.As<WithRef, byte>(ref r)`, which is `Intrinsics.call` (Intrinsics.fs:1672) taking a byte view over a *local* — measured as "refusing byte view over value type containing object references in single-cell byref `<variable 0 …> as System.Byte`", from `validateByteAddressableCell` (IlMachineManagedByref.fs:595). That is the same "a reference-containing value type has no byte image" gap as `ReinterpretCellUnderAliasedAncestor.cs` and `BulkMoveAcrossOverlappedStructPadding.cs`, and it forecloses *every* route a guest has to a `ref byte` over such a struct, which is why the reference-free half of this coverage lives in the active `RuntimeHelpersBox.cs` instead. The file is not satisfiable the wrong way: it checks both fields (so moving only the reference, or only the trailing int, still fails) and re-reads the box after mutating the source (so handing back a view onto the source rather than a copy fails). Un-park when a reference-containing value type can be byte-addressed.
            "PointerFieldAliasedWidthStore.cs" // Storing into a pointer-typed field through a byref aliased as `long*`/`double*` rather than `void**`. A pointer slot is a `CliType.RuntimePointer` cell with no byte image, so the byte-scatter writer refuses it; the sibling `PointerFieldIndirectStore.cs` fixes that for the pointer-shaped payloads by replacing the whole cell instead. That route is deliberately *not* taken here, because whole-cell replacement restamps the cell with the payload's shape: on a 64-bit runtime `stind.i8`/`stind.r8` are exact-width stores into a `void*` slot too, and taking them would leave the field holding `Numeric Int64`/`Float64`, so the next read pushes the wrong evaluation-stack kind and fails downstream (measured: `bad ceq: Int64 vs NativeInt(0)`) with a message naming neither the field nor the store. Un-parking needs a pointer cell that can hold a non-pointer bit pattern while still reading back as a pointer — i.e. the same "materialise bits late" question as the rest of the provenance model — not a wider routing predicate.
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
            SeedEntry.File (Text.Encoding.UTF8.GetBytes contents |> ImmutableArray.CreateRange)

        let openSeed =
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.Directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                ]

        [
            "FileMetadataSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.Directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    // A leading dot, which is the whole of what "hidden" means
                    // on Unix.
                    name ".hidden", file "x"
                ]
            "FileExistsSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.Directory (Map.ofList [ name "g", file "nested" ])
                    name "lf", SeedEntry.Symlink (target "f")
                    name "ld", SeedEntry.Symlink (target "d")
                    name "dang", SeedEntry.Symlink (target "nx")
                    name "cyc", SeedEntry.Symlink (target "cyc")
                ]
            "SystemNativeReadLink.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.Directory Map.empty
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
            "SystemNativeOpen.cs", openSeed
            "OpenMissingFile.cs", openSeed
            "LinkTargetSeeded.cs",
            Map.ofList
                [
                    name "f", file "hello"
                    name "d", SeedEntry.Directory Map.empty
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
            // The oracle below loads the guest in-process on the *host* runtime, whose
            // AppContext was seeded by the real host before this test process started and
            // cannot be reseeded. A case with properties would therefore be comparing a
            // seeded PawPrint against an unseeded oracle — a PawPrint-only fact dressed up
            // as a cross-runtime one. Those belong in `sourcesImpure`.
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
                | RealRuntimeResult.FailFast report, _ ->
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
                | _, RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"

                    failwith $"PawPrint guest called Environment.FailFast for %s{case.FileName}: %s{m}"
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
        // an *exact* type constraint, and a failure raised while interpreting now arrives as
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
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
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
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
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
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
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
                | RunOutcome.FailFast (_, _, message) ->
                    let m = message |> Option.defaultValue "<no message>"
                    failwith $"expected normal exit, got Environment.FailFast: %s{m}"
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
                | RunOutcome.FailFast (_, _, message) -> message |> shouldEqual (Some "boom")
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
        | RealRuntimeResult.FailFast report ->
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
