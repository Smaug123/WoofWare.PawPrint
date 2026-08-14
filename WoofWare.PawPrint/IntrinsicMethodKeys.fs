namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IntrinsicMethodKeys =
    type IntrinsicMethodKey =
        {
            AssemblyName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterShapes : string list
        }

    [<RequireQualifiedAccess>]
    type private IntrinsicParameterPattern =
        | Any
        | Exact of string
        | Byref
        | Pointer
        | SzArray
        | Array

    type private IntrinsicMethodPattern =
        {
            AssemblyName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterPatterns : IntrinsicParameterPattern list option
        }

    let private pattern
        (assemblyName : string)
        (declaringTypeFullName : string)
        (methodName : string)
        (parameterPatterns : IntrinsicParameterPattern list)
        : IntrinsicMethodPattern
        =
        {
            AssemblyName = assemblyName
            DeclaringTypeFullName = declaringTypeFullName
            MethodName = methodName
            ParameterPatterns = Some parameterPatterns
        }

    let private anyParams
        (assemblyName : string)
        (declaringTypeFullName : string)
        (methodName : string)
        : IntrinsicMethodPattern
        =
        {
            AssemblyName = assemblyName
            DeclaringTypeFullName = declaringTypeFullName
            MethodName = methodName
            ParameterPatterns = None
        }

    let methodKey
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IntrinsicMethodKey
        =
        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringAssembly with
            | Some assy -> assy
            | None ->
                failwith
                    $"Intrinsic method key requested for method whose declaring assembly is not loaded: %O{methodToCall}"

        let declaringType =
            declaringAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        let concreteTypeShape (handle : ConcreteTypeHandle) : string =
            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | Some ct ->
                    if String.IsNullOrEmpty ct.Namespace then
                        ct.Name
                    else
                        $"%s{ct.Namespace}.%s{ct.Name}"
                | None -> failwith $"Intrinsic method key requested for unknown concrete type handle: %O{handle}"
            | ConcreteTypeHandle.Byref _ -> "&"
            | ConcreteTypeHandle.Pointer _ -> "*"
            | ConcreteTypeHandle.FunctionPointer _ -> "fnptr"
            | ConcreteTypeHandle.OneDimArrayZero _ -> "[]"
            | ConcreteTypeHandle.Array (_, rank) -> $"[%i{rank}]"

        {
            AssemblyName = methodToCall.DeclaringAssembly.Name
            DeclaringTypeFullName = TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) declaringType
            MethodName = methodToCall.Name
            ParameterShapes = methodToCall.Signature.ParameterTypes |> List.map concreteTypeShape
        }

    let formatMethodKey (key : IntrinsicMethodKey) : string =
        let parameters = key.ParameterShapes |> String.concat ", "
        $"%s{key.AssemblyName} %s{key.DeclaringTypeFullName}.%s{key.MethodName}(%s{parameters})"

    let private parameterPatternMatches (pattern : IntrinsicParameterPattern) (actual : string) : bool =
        match pattern with
        | IntrinsicParameterPattern.Any -> true
        | IntrinsicParameterPattern.Exact expected -> expected = actual
        | IntrinsicParameterPattern.Byref -> actual = "&"
        | IntrinsicParameterPattern.Pointer -> actual = "*"
        | IntrinsicParameterPattern.SzArray -> actual = "[]"
        | IntrinsicParameterPattern.Array -> actual.StartsWith ("[", StringComparison.Ordinal)

    let private methodPatternMatches (pattern : IntrinsicMethodPattern) (key : IntrinsicMethodKey) : bool =
        pattern.AssemblyName = key.AssemblyName
        && pattern.DeclaringTypeFullName = key.DeclaringTypeFullName
        && pattern.MethodName = key.MethodName
        && match pattern.ParameterPatterns with
           | None -> true
           | Some patterns ->
               List.length patterns = List.length key.ParameterShapes
               && List.forall2 parameterPatternMatches patterns key.ParameterShapes

    let private safeIntrinsics =
        [
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L739-L750
            pattern "System.Private.CoreLib" "System.String" "get_Length" []
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L728-L737
            pattern
                "System.Private.CoreLib"
                "System.String"
                "get_Chars"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            // IL body is `ldarg.0; ldflda _firstChar; ret`; PawPrint projects `_firstChar`
            // to the string character data side-table.
            pattern "System.Private.CoreLib" "System.String" "GetRawStringData" []
            pattern "System.Private.CoreLib" "System.String" "GetRawStringDataAsUInt16" []
            // IL body constructs a span over the string contents; PawPrint's string field
            // projection handles the `_firstChar` boundary it depends on.
            pattern
                "System.Private.CoreLib"
                "System.String"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.String" ]
            // String overloads bottom out in String.GetRawStringData plus ReadOnlySpan construction.
            anyParams "System.Private.CoreLib" "System.MemoryExtensions" "AsSpan"
            // Managed wrapper over RuntimeHelpers.IsBitwiseEquatable<T> and SpanHelpers.SequenceEqual.
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "SequenceEqual"
                [
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // Same shape as SequenceEqual above, with `value.Length <= span.Length` in place of
            // the equal-lengths check: the IL is `get_Length`, RuntimeHelpers.IsBitwiseEquatable<T>,
            // MemoryMarshal.GetReference, Unsafe.As<T, byte>, `sizeof T`, then
            // SpanHelpers.SequenceEqual(ref byte, ref byte, nuint) — all modelled boundaries.
            // The `[Intrinsic]` marker is only so the JIT can unroll/vectorise half-constant input.
            // As with SequenceEqual, the non-bitwise-equatable fallback bottoms out in the generic
            // SpanHelpers.SequenceEqual<T>, which PawPrint does not yet implement; executing this
            // IL for such a T therefore fails loudly there rather than silently misbehaving.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3561
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "StartsWith"
                [
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // The mirror image of StartsWith above: the same length guard and the same
            // SpanHelpers.SequenceEqual(ref byte, ref byte, nuint) call, but comparing at
            // `span.Length - value.Length` rather than at 0. That offset is applied by
            // `Unsafe.Add<T>(ref T, nint)` (with the count zero-extended through `conv.u`),
            // which is an implemented boundary; every other callee is shared with StartsWith.
            // The same caveat applies: for a T where IsBitwiseEquatable is false, the IL falls
            // through to the generic SpanHelpers.SequenceEqual<T>, which PawPrint does not
            // implement, so such a T fails loudly there rather than silently misbehaving.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3601
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "EndsWith"
                [
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // The Span<T>-receiver siblings of the two overloads above. Each IL body is
            // `ldarg.0; call Span<T>::op_Implicit; ldarg.1; call <the ReadOnlySpan<T> overload>; ret`
            // — both callees are themselves allowlisted, so there is nothing further to review.
            //
            // These are not reachable from C# 13 or later: they carry
            // [OverloadResolutionPriority(-1)], so once the first-class span conversion makes the
            // ReadOnlySpan<T> overload applicable to a Span<T> receiver, the priority pruning
            // removes these from the candidate set — even for an explicit
            // `MemoryExtensions.StartsWith<T>(span, value)` call or a method-group conversion.
            // They are therefore reachable only from assemblies built by an older compiler (or
            // another language), which PawPrint can be pointed at but the pure-source test
            // harness cannot produce; hence no end-to-end coverage for these two specifically.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3553-L3554
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "StartsWith"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3593-L3594
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "EndsWith"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/ArgumentNullException.cs#L54
            anyParams "System.Private.CoreLib" "System.ArgumentNullException" "ThrowIfNull"
            // The instance `String.Equals(string)` overload — the one that implements
            // `IEquatable<string>`, so interface dispatch through `IEquatable<string>::Equals`
            // resolves to it. Its `[Intrinsic]` is a pure codegen hint ("Unrolled and vectorized
            // for half-constant input"), so the managed body is the semantic definition:
            // ReferenceEquals, a null check, a Length compare, then `EqualsHelper`. All of those
            // are already-modelled string primitives. (The *static* two-argument
            // `String.Equals(string, string)` overload is handled explicitly in Intrinsics.fs
            // instead; this pattern is parameter-count-specific so the two do not overlap.)
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L607-L625
            pattern
                "System.Private.CoreLib"
                "System.String"
                "Equals"
                [ IntrinsicParameterPattern.Exact "System.String" ]
            // `String.StartsWith(string, StringComparison)` and its `EndsWith` mirror. As with
            // the `Equals` overload above, the `[Intrinsic]` marker is a pure codegen hint
            // ("Unrolled and vectorized for half-constant input (Ordinal)"), so the managed body
            // is the semantic definition — including the argument validation, which is not
            // reproducible by a native reimplementation without duplicating CoreLib's exact
            // ordering: `ArgumentNullException.ThrowIfNull(value)` first (so a null `value` beats
            // an invalid `comparisonType`), then two short-circuits — reference equality and
            // `value.Length == 0` — that each still run `CheckStringComparison` before returning
            // true, and finally a switch whose default arm throws `ArgumentException`.
            //
            // Every arm of that switch bottoms out in a modelled boundary:
            //  * Ordinal `StartsWith` reads both `_firstChar` fields (projected to the string
            //    character side-table) and then calls
            //    `SpanHelpers.SequenceEqual(ref byte, ref byte, nuint)` over
            //    `GetRawStringDataAsUInt8()`; that helper is intercepted explicitly in
            //    `Intrinsics.fs`.
            //  * Ordinal `EndsWith` instead computes `this.AsSpan(offset).SequenceEqual(value)`,
            //    i.e. `MemoryExtensions.AsSpan`, `String.op_Implicit` and
            //    `MemoryExtensions.SequenceEqual` — all three allowlisted above.
            //  * Both OrdinalIgnoreCase arms call `Ordinal.EqualsIgnoreCase(ref char, ref char,
            //    int)`. Its four vector guards all test `VectorNNN.IsHardwareAccelerated`, which
            //    PawPrint's scalar CPU profile folds to false (see `vectorAccelerationAvailable`),
            //    so the first guard sends every input to `EqualsIgnoreCase_Scalar`: an unrolled
            //    walk of `Unsafe.ReadUnaligned` / `Unsafe.AddByteOffset` over a byte cursor plus
            //    `Utf16Utility` bit-twiddling, with no P/Invoke. Non-ASCII input leaves that fast
            //    path for `Ordinal.CompareStringIgnoreCase`, whose casing tables PawPrint does not
            //    implement, so such input fails loudly there rather than silently misbehaving —
            //    as would ASCII input under a hypothetical SIMD-reporting profile.
            //  * The four culture-sensitive arms delegate to `CompareInfo.IsPrefix`/`IsSuffix`,
            //    an already-working boundary.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L1086-L1135
            pattern
                "System.Private.CoreLib"
                "System.String"
                "StartsWith"
                [
                    IntrinsicParameterPattern.Exact "System.String"
                    IntrinsicParameterPattern.Exact "System.StringComparison"
                ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L517-L557
            pattern
                "System.Private.CoreLib"
                "System.String"
                "EndsWith"
                [
                    IntrinsicParameterPattern.Exact "System.String"
                    IntrinsicParameterPattern.Exact "System.StringComparison"
                ]
            // `SZArrayHelper.GetEnumerator<T>` is where an SZ-array's implicit
            // `IEnumerable<T>::GetEnumerator` lands, so classifying against the resolved method
            // reaches it. It is the only `[Intrinsic]` member of `SZArrayHelper`, and the
            // attribute is purely an exact-return-type hint: the JIT marks the call `isSpecial`
            // ("We may know the exact type these return") and asks the VM for the concrete
            // enumerator class to sharpen devirtualization. It does not replace the body — the
            // VM's `getSZArrayHelperEnumeratorClassHelper` says it "Mirrors the logic in BCL's
            // SZArrayHelper::GetEnumerator", i.e. the managed body is the source of truth.
            //
            // That body is `Unsafe.As<T[]>(this)`, a `Length` read, then either
            // `SZGenericArrayEnumerator<T>.Empty` or a `new SZGenericArrayEnumerator<T>`. The
            // `Unsafe.As` is the documented "`this` is really the array, not an SZArrayHelper"
            // convention, which PawPrint's SZ-array interface dispatch already establishes.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Array.CoreCLR.cs#L398-L407
            pattern "System.Private.CoreLib" "System.SZArrayHelper" "GetEnumerator" []
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/System.Private.CoreLib/src/System/Type.CoreCLR.cs#L82
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "GetTypeFromHandle"
                [ IntrinsicParameterPattern.Exact "System.RuntimeTypeHandle" ]
            // `RuntimeType.TypeHandle`'s getter overrides the [Intrinsic] `Type.TypeHandle`
            // getter and carries its own [Intrinsic] solely "to avoid round-trip
            // handle -> RuntimeType -> handle in JIT" (its own source comment), so the managed
            // body is the semantic definition. That body is
            // `ldarg.0; newobj RuntimeTypeHandle::.ctor(RuntimeType); ret`, and the ctor is
            // `internal RuntimeTypeHandle(RuntimeType? type) { m_type = type; }` — a single store
            // into a field PawPrint already models.
            //
            // Going through the real `newobj` means `UnaryMetadataObjectOps` runs
            // `ensureTypeInitialised` for `RuntimeTypeHandle` on every `.TypeHandle` access.
            // `RuntimeTypeHandle` has no `.cctor`, so that is inert; it is also what a real
            // `newobj` does.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/RuntimeType.cs#L27-L31
            pattern "System.Private.CoreLib" "System.RuntimeType" "get_TypeHandle" []
            // The base `Type.TypeHandle` getter is `[Intrinsic]` with an IL body of
            // `throw new NotSupportedException()`, and that throw is the behaviour we want.
            // Under `callvirt` on any PawPrint-created receiver, virtual resolution selects the
            // `RuntimeType` override above, so this body runs only for a `Type` subclass that
            // does not override it — where throwing is exactly right — or for a non-virtual
            // `call`, where ECMA-335 dispatches statically to this body and .NET throws too.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Type.cs#L467-L471
            pattern "System.Private.CoreLib" "System.Type" "get_TypeHandle" []
            // `Type.IsPrimitive`'s getter is `[Intrinsic]` only so the JIT can constant-fold
            // `typeof(X).IsPrimitive` when the receiver is a literal `ldtoken`
            // (`NI_System_Type_get_IsPrimitive`, importercalls.cpp:4046, alongside `IsEnum` /
            // `IsValueType` / `IsByRefLike` / `IsGenericType`). When the receiver is not a
            // known `typeof`, the JIT emits the ordinary call, so the managed body is the
            // semantic definition rather than a placeholder, and every primitive it bottoms
            // out in is already modelled:
            //
            //   Type::get_IsPrimitive        `ldarg.0; callvirt Type::IsPrimitiveImpl(); ret`
            //   RuntimeType::IsPrimitiveImpl `ldarg.0; call RuntimeTypeHandle::IsPrimitive; ret`
            //   RuntimeTypeHandle::IsPrimitive  `RuntimeHelpers.IsPrimitiveType(type.GetCorElementType())`
            //   RuntimeType::GetCorElementType  the `TypeHandle_GetCorElementType` QCall,
            //                                   implemented in NativeRuntimeTypeQCall.fs
            //
            // and `RuntimeHelpers.IsPrimitiveType` is a plain bit test of the element type
            // against 0x03003FFC — I1,U1,I2,U2,I4,U4,I8,U8,R4,R8,I,U,CHAR,BOOLEAN.
            //
            // Interpreting the body rather than hand-writing an arm in `Intrinsics.call` is
            // what keeps the `callvirt` on line 1 a real virtual dispatch. `IsPrimitiveImpl`
            // is abstract on `Type`, so a receiver that is not a `RuntimeType` — a
            // `TypeDelegator`, or any guest `Type` subclass — answers from its own override.
            // An arm keyed on `Type::get_IsPrimitive` would intercept ahead of that dispatch
            // and then fail trying to read `m_handle` off a type that does not declare it.
            // `Type.get_IsValueType` below is an entry for the same reason.
            //
            // It also keeps `GetCorElementType` the single place that classifies a runtime
            // type handle, so `IsPrimitive` cannot drift from it. In particular an enum is
            // *not* primitive even though its underlying type is: CoreCLR categorises it
            // `PrimitiveValueType`, and `MethodTable::GetSignatureCorElementType`
            // (methodtable.cpp:5113) maps that whole category to ELEMENT_TYPE_VALUETYPE
            // rather than to the underlying element type.
            //
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Type.cs#L129-L134
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/RuntimeType.cs#L272
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L133-L136
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L109-L111
            pattern "System.Private.CoreLib" "System.Type" "get_IsPrimitive" []
            // `Type.IsValueType` is the same shape as `IsPrimitive` above: a non-virtual
            // property whose `[Intrinsic]` getter is `ldarg.0; callvirt Type::IsValueTypeImpl();
            // ret`, with the attribute present only for the JIT's `typeof(X)` constant-fold
            // (`NI_System_Type_get_IsValueType`, importercalls.cpp:4045). The getter is always
            // the call target, so its `callvirt` is the only thing that selects an
            // implementation — a `TypeDelegator` or a guest `Type` subclass must answer from
            // its own override. (`IsEnum` needs no entry at all: it is itself virtual and
            // `RuntimeType` overrides the whole property, so a `callvirt` never lands on an
            // `[Intrinsic]` body.)
            //
            // `RuntimeType.IsValueTypeImpl` is not a plain handle query:
            //
            //   TypeHandle th = GetNativeTypeHandle();
            //   if (th.IsTypeDesc) return IsSubclassOf(typeof(ValueType));  // generic parameters
            //   return th.AsMethodTable()->IsValueType;
            //
            // Both branches are modelled — the MethodTable flag by `MethodTableProjection`,
            // and the TypeDesc branch by ordinary `IsSubclassOf` walking a type variable's
            // base type, which CoreCLR defines as its most specific non-interface class
            // constraint. That walk is what makes `where T : Enum` report true, which reading
            // only the NotNullableValueType/Reference constraint *flags* cannot.
            //
            // Method-level generic parameters reach `RuntimeTypeHandle.GetConstraints`
            // (NativeRuntimeTypeQCall.fs), which does not serve them;
            // `TypeIsValueTypeMethodGenericParameter.cs` is parked on that.
            //
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Type.cs#L135-L140
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/RuntimeType.CoreCLR.cs#L3432-L3443
            pattern "System.Private.CoreLib" "System.Type" "get_IsValueType" []
            // `RuntimeType.IsActualEnum` is the *nominal* enum test — "is this type's immediate
            // base `System.Enum`" — as opposed to the neighbouring `RuntimeType.IsEnum`, which
            // also answers true for a generic parameter constrained `where T : Enum`. That is the
            // whole reason it exists, and it is what every enum reflection entry point gates on
            // (`GetEnumUnderlyingType`, `GetEnumNames`, `GetEnumValuesAsUnderlyingType`,
            // `IsEnumDefined`, `Enum.GetUnderlyingType`, `Type.GetTypeCode`), so an
            // implementation that conflated the two would hand a generic parameter to code that
            // then looks for a `value__` field.
            //
            // Its `[Intrinsic]` is the same JIT constant-fold hint the two `Type` getters above
            // carry — `importercalls.cpp:10438` maps `get_IsActualEnum` to
            // `NI_System_Type_get_IsEnum`, which folds only when the receiver is a literal
            // `ldtoken` — so the managed body is the semantic definition:
            //
            //   TypeHandle th = GetNativeTypeHandle();
            //   bool isEnum = !th.IsTypeDesc
            //                 && th.AsMethodTable()->ParentMethodTable
            //                    == TypeHandle.TypeHandleOf<Enum>().AsMethodTable();
            //   GC.KeepAlive(this);
            //   return isEnum;
            //
            // Every step is an already-modelled boundary:
            //  * `GetNativeTypeHandle` is `new TypeHandle((void*)m_handle)`, and PawPrint
            //    populates `RuntimeType.m_handle` with a `NativeIntSource.TypeHandlePtr`.
            //  * `TypeHandle.IsTypeDesc` is the `& 2` tag test, whose single home is
            //    `TypeHandleTag.forTarget` (NativeIntSource.fs). It reports true for generic
            //    parameters, byrefs, pointers and function pointers, which is what keeps the
            //    short-circuit load-bearing: `MethodTable::ParentMethodTable` deliberately
            //    refuses a TypeDesc target, so a wrong tag would surface as a loud projection
            //    failure rather than a wrong answer.
            //  * `MethodTable::ParentMethodTable` is projected (MethodTableProjection.fs) through
            //    `resolveBaseRuntimeTypeHandleTarget`, the same base-type walk `isEnumValueType`
            //    uses; interpreting the body rather than hand-writing an arm is what keeps those
            //    two from drifting.
            //  * `TypeHandle.TypeHandleOf<Enum>()` is not itself `[Intrinsic]`; it is
            //    `RuntimeTypeHandle.ToIntPtr(typeof(Enum).TypeHandle)`, and both of those are
            //    allowlisted here already.
            //  * The `ceq` between the two `MethodTable*`s is the `MethodTablePtr` comparison
            //    that `NativeIntSource` already has to support for
            //    `RuntimeHelpers.GetMethodTable(obj) == TypeHandleOf<T>().AsMethodTable()`.
            //
            // `RuntimeType` is sealed, so unlike `Type.get_IsPrimitive`/`get_IsValueType` there is
            // no virtual dispatch for an allowlist entry to preserve here; the reason to interpret
            // rather than intercept is the single-classifier one above.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/RuntimeType.CoreCLR.cs#L3474-L3486
            pattern "System.Private.CoreLib" "System.RuntimeType" "get_IsActualEnum" []
            // .NET 10 added [Intrinsic] to RuntimeTypeHandle.ToIntPtr; the IL body delegates
            // to the Value getter which reads RuntimeType.m_handle, a field PawPrint already
            // populates with NativeIntSource.TypeHandlePtr. Executing the IL is safe and
            // round-trips through the existing TypeHandle representation.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L80-L81
            // (the `Value` getter it delegates to is at L103.)
            pattern
                "System.Private.CoreLib"
                "System.RuntimeTypeHandle"
                "ToIntPtr"
                [ IntrinsicParameterPattern.Exact "System.RuntimeTypeHandle" ]
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L703
            // Managed IL bodies with RuntimeType fast paths before Equals; op_Inequality delegates to op_Equality.
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "op_Equality"
                [
                    IntrinsicParameterPattern.Exact "System.Type"
                    IntrinsicParameterPattern.Exact "System.Type"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.Type"
                    IntrinsicParameterPattern.Exact "System.Type"
                ]
            // IL body is `targetType?.IsAssignableFrom(this) ?? false`; safe to execute since
            // the virtual IsAssignableFrom dispatches to RuntimeType.IsAssignableFrom which
            // bottoms out in the RuntimeTypeHandle.CanCastTo InternalCall (modelled in
            // NativeRuntimeType.tryExecute).
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.cs#L143
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "IsAssignableTo"
                [ IntrinsicParameterPattern.Exact "System.Type" ]
            // Virtual IsAssignableFrom; the override on RuntimeType is what carries the cast logic,
            // but the base IL body itself is safe (it handles null, identity, and a few fallbacks
            // that delegate back through normal virtual dispatch).
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.Helpers.cs#L336
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "IsAssignableFrom"
                [ IntrinsicParameterPattern.Exact "System.Type" ]
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L161
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_Length" []
            // IL body is `ldarg.0; ldfld _length; ldc.i4.0; ceq; ret`.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_IsEmpty" []
            // Reviewed constructors initialise `_reference` / `_length` through already-modelled
            // array and byref boundaries. The `(void*, int)` constructor is an explicit
            // intrinsic implementation below because it crosses the unmanaged-pointer boundary.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" ".ctor" [ IntrinsicParameterPattern.SzArray ]
            // IL body delegates to the array-backed constructor above.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "op_Implicit" [ IntrinsicParameterPattern.SzArray ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.SzArray
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" ".ctor" [ IntrinsicParameterPattern.Byref ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            // Managed wrappers over already-modelled span fields, bounds checks, array allocation,
            // and Buffer.Memmove.
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "CopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "TryCopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            // Reviewed IL: bounds checks, Unsafe.Add over the span byref, then byref+length
            // ReadOnlySpan<T> construction. Unsafe.Add and the constructor are implemented
            // boundaries below.
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "Slice"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "Slice"
                [
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "ToArray" []
            // IL body is `Unsafe.NullRef<T>(); if (_length != 0) ret = ref _reference; return ret`.
            // Unsafe.NullRef is implemented as an intrinsic in Intrinsics.fs; the field reads
            // and managed-byref assignment are already-modelled span primitives.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L289
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "GetPinnableReference" []
            // Reviewed IL: `ldfld _length` on each argument, `bne.un.s` to a `ldc.i4.0; ret`,
            // then `ldarga.s; ldfld _reference` on each and `Unsafe.AreSame<T>(ref T, ref T)`.
            // Every step is an already-modelled boundary: the `_length` and `_reference` field
            // reads are the same span primitives `Slice` and `GetPinnableReference` above rely
            // on, and `Unsafe.AreSame` is implemented in `Intrinsics.fs` — where it normalises
            // both byrefs before comparing, so two spans built over the same storage by
            // different routes (implicit conversion, `AsSpan`, `.ctor`, `Slice`) compare equal.
            // Neither operator carries an attribute of its own: what routes them here is the
            // type-level `[Intrinsic]` on ReadOnlySpan<T>, which every member of the type
            // inherits and which is why each one needs allowlisting individually.
            //
            // Note this is deliberately *reference* equality, not content equality: equal-
            // length spans over distinct backing storage are unequal, and a zero-length slice
            // of a live array is unequal to `default` because only the latter is null-backed.
            //
            // The operator is therefore exactly as good as `Unsafe.AreSame`, and inherits its
            // one known gap: a byref to a struct's first field does not compare equal to a
            // byref to the whole struct reinterpreted as that field's type, so two spans built
            // over the same storage that way wrongly report unequal. That is a defect in byref
            // normalisation, not in this operator — it is reachable with no span in sight by
            // calling `Unsafe.AreSame` directly, and predates this entry — and is parked as
            // `AreSameFirstFieldVersusReinterpretedWhole.cs`, whose first assertion uses no
            // span at all.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L346-L348
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "op_Equality"
                [
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // `ldarg.0; ldarg.1; call op_Equality; ldc.i4.0; ceq; ret` — nothing but the
            // operator above, which is itself allowlisted, and a negation. The C# `!=`
            // operator on two spans emits a call to this, so it is inseparable from `==`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L178
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // IL body for both Span<T>.Empty and ReadOnlySpan<T>.Empty is
            // `.locals init (valuetype S V_0) ldloca.s V_0; initobj S; ldloc.0; ret` —
            // i.e. just returning `default(...)`. The `[Intrinsic]` attribute is for
            // JIT inlining; the IL is safe to execute directly.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L214
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_Empty" []
            // IL body is `ldarg.0; ldfld _length; ret`.
            pattern "System.Private.CoreLib" "System.Span`1" "get_Length" []
            // IL body is `ldarg.0; ldfld _length; ldc.i4.0; ceq; ret`.
            pattern "System.Private.CoreLib" "System.Span`1" "get_IsEmpty" []
            // See ReadOnlySpan<T>.get_Empty above; the IL body is the same `default(Span<T>)` shape.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Span.cs#L219
            pattern "System.Private.CoreLib" "System.Span`1" "get_Empty" []
            // The `Span<T>` siblings of the ReadOnlySpan<T> operators above. Both IL bodies are
            // the same instruction for instruction, over `Span<T>`'s own `_length` and
            // `_reference` fields, so the review above applies unchanged — including the caveat
            // that these are exactly as good as `Unsafe.AreSame` and inherit its first-field
            // versus reinterpreted-whole gap.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Span.cs#L364-L366
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "op_Equality"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.Span`1"
                ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Span.cs#L183
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.Span`1"
                ]
            // Same constructor shape as ReadOnlySpan<T>; the `(void*, int)` constructor is
            // handled explicitly below.
            pattern "System.Private.CoreLib" "System.Span`1" ".ctor" [ IntrinsicParameterPattern.SzArray ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.SzArray
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.Span`1" ".ctor" [ IntrinsicParameterPattern.Byref ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            // IL body delegates to the array-backed constructor above.
            pattern "System.Private.CoreLib" "System.Span`1" "op_Implicit" [ IntrinsicParameterPattern.SzArray ]
            // IL body constructs ReadOnlySpan<T> over this span's `_reference` and `_length`.
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            // Managed wrappers over already-modelled span fields, bounds checks, array allocation,
            // and Buffer.Memmove.
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "CopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "TryCopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            // Reviewed IL: bounds checks, Unsafe.Add over the span byref, then byref+length
            // Span<T> construction. Unsafe.Add and the constructor are implemented
            // boundaries below.
            pattern "System.Private.CoreLib" "System.Span`1" "Slice" [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "Slice"
                [
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern "System.Private.CoreLib" "System.Span`1" "ToArray" []
            // IL body is `ldarg.0; ldfld _reference; ldarg.0; ldfld _length; conv.u; ldarg.1;
            // call SpanHelpers::Fill<T>` — pure field reads plus the helper allowlisted below.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Span.cs#L310-L313
            pattern "System.Private.CoreLib" "System.Span`1" "Fill" [ IntrinsicParameterPattern.Any ]
            // `SpanHelpers.Fill<T>(ref T, nuint, T)` opens with a vectorised fast path, but
            // PawPrint emulates a deterministic scalar CPU: `Vector.IsHardwareAccelerated` folds
            // to false (see `vectorAccelerationAvailable`), which is the second of the four
            // guards and jumps straight to `CannotVectorize`. The later guards — and every
            // `Vector<byte>`/`Vector256`/`Vector512` construction — are therefore never
            // evaluated. A reference-containing T leaves even earlier, at the
            // `RuntimeHelpers.IsReferenceOrContainsReferences<T>` guard, which is implemented.
            //
            // `CannotVectorize` is an unrolled scalar loop of `Unsafe.Add(ref refData, i) = value`
            // writes in blocks of 8/4/2/1 — only modelled boundaries, and no P/Invoke. That is
            // what distinguishes this from the sibling `Span<T>.Clear`, whose IL instead bottoms
            // out in `SpanHelpers.ClearWithReferences` / `ClearWithoutReferences`. The latter is
            // itself `[Intrinsic]` and is implemented in `Intrinsics.fs`, but the former is
            // plain managed IL that writes a pointer-width zero through a reinterpreted byref
            // onto object-reference cells — a shape the byref-write model does not yet support.
            // `Span<T>.Clear` therefore stays natively implemented in `Intrinsics.fs`.
            //
            // Should PawPrint ever report SIMD as accelerated, this IL would start walking into
            // the vector path and fail loudly there rather than silently misbehaving.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/SpanHelpers.T.cs#L15-L189
            // (guards at L23-L26; `CannotVectorize:` at L138.)
            pattern
                "System.Private.CoreLib"
                "System.SpanHelpers"
                "Fill"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.UIntPtr"
                    IntrinsicParameterPattern.Any
                ]
            // Same IL body as ReadOnlySpan<T>.GetPinnableReference above.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Span.cs#L282
            pattern "System.Private.CoreLib" "System.Span`1" "GetPinnableReference" []
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            anyParams "System.Private.CoreLib" "System.Runtime.CompilerServices.RuntimeHelpers" "CreateSpan"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L127
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L137
            anyParams "System.Private.CoreLib" "System.Math" "Abs"
            // Single-line delegation to Math.Abs above; the [Intrinsic] marker is for the JIT,
            // but the IL body is just a tail call we can safely execute.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Double.cs#L1041-L1043
            anyParams "System.Private.CoreLib" "System.Double" "Abs"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L965C10-L1062C19
            anyParams "System.Private.CoreLib" "System.Math" "Max"
            // Mirror of Math.Max above: most overloads have a `(val1 <= val2) ? val1 : val2`
            // IL body, and the [Intrinsic]-marked double/float overloads use the IEEE 754:2019
            // `minimum` definition expressed in terms of IsNaN/IsNegative — both already supported.
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L1064-L1187
            anyParams "System.Private.CoreLib" "System.Math" "Min"
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Buffer.cs#L150
            anyParams "System.Private.CoreLib" "System.Buffer" "Memmove"
            // Note: `System.SpanHelpers.Memmove(ref byte, ref byte, nuint)` is intercepted
            // explicitly in `Intrinsics.fs` and routed through `CellAwareMemOps.copy`, so it is
            // deliberately omitted from the safe-intrinsic allowlist: the managed body's
            // `Unsafe.ReadUnaligned<Block16>` walk would lose non-`Verbatim` cell provenance.
            // https://github.com/dotnet/runtime/blob/1c3221b63340d7f81dfd829f3bcd822e582324f6/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L799
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_CurrentThread" []
            // IL body is `ldarg.0; ldfld _managedThreadId; ret` — pure field access.
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_ManagedThreadId" []
            // `ValueTask<TResult>.ConfigureAwait(bool)` — the awaitable-configuring member every
            // `await something.ConfigureAwait(false)` over a `ValueTask<T>` goes through.
            //
            // Its `[Intrinsic]` is not an implementation at all: `lookupNamedIntrinsic` maps
            // `ConfigureAwait` to `NI_System_Threading_Tasks_Task_ConfigureAwait`
            // (importercalls.cpp:11200-11209), and the only place in the whole JIT that ever *reads*
            // that enumerator is `impMatchTaskAwaitPattern` (importer.cpp:6027), which recognises the
            // `call <Method>; ldc.i4.0/1; call <ConfigureAwait>; call <AsyncHelpers.Await>` IL
            // shape a *runtime-async* method compiles to, and rewrites the trio into a single call
            // to the runtime-async counterpart of `Method`. There is no `impIntrinsic` case for it,
            // so outside that peephole the JIT emits the ordinary call and the managed body is the
            // semantic definition. (For this member the peephole cannot fire even in principle: the
            // class-name test upstream compares against "ValuTask`1" — importercalls.cpp:11205, a
            // typo for "ValueTask`1" — so `ValueTask<T>`'s own overload is never recognised at all.
            // We do not rely on that: the reasoning above holds for the whole family.)
            //
            // That body is:
            //   ldarg.0; ldfld _obj; ldarg.0; ldfld _result; ldarg.0; ldfld _token; ldarg.1
            //   newobj ValueTask`1::.ctor(object, !TResult, int16, bool)
            //   stloc.0; ldloca.s 0
            //   newobj ConfiguredValueTaskAwaitable`1::.ctor(ValueTask`1&)
            //   ret
            // i.e. three field reads through the `this` byref, the private "non-verified
            // initialization" constructor — four plain field stores, no validation — and the
            // awaitable's own constructor, whose body is the single `_value = value` copy from the
            // `in` parameter. Every step is ordinary value-type IL over fields PawPrint already
            // models; there is no P/Invoke, no vectorisation, and no `continueOnCapturedContext`
            // interpretation here at all — the flag is merely stored, and only the *awaiter* later
            // reads it to decide how to schedule a continuation.
            //
            // The `ConfigureAwaitOptions`-taking `Task`/`Task<T>` overloads are allowlisted further
            // down, separately: they are `[Intrinsic]` for the same pattern-match reason, but their
            // bodies validate their argument against a per-type mask and so warrant their own
            // review.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/ValueTask.cs#L829-L832
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/ConfiguredValueTaskAwaitable.cs#L127
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.ValueTask`1"
                "ConfigureAwait"
                [ IntrinsicParameterPattern.Exact "System.Boolean" ]
            // The non-generic sibling of the entry above. Every word of that review applies here
            // unchanged — same `[Intrinsic]`-is-only-a-peephole-marker reasoning, same shape of body
            // — with one field fewer, since a `ValueTask` carries no `_result`:
            //   ldarg.0; ldfld _obj; ldarg.0; ldfld _token; ldarg.1
            //   newobj ValueTask::.ctor(object, int16, bool)
            //   stloc.0; ldloca.s 0
            //   newobj ConfiguredValueTaskAwaitable::.ctor(ValueTask&)
            //   ret
            // Unlike `ValueTask<T>`'s overload this one *is* spelled correctly in the JIT's
            // class-name test (importercalls.cpp:11205 lists "ValueTask"), so the runtime-async
            // peephole can recognise it; that changes nothing here, because the peephole only fires
            // inside a runtime-async method and rewrites the whole `call; ConfigureAwait; Await`
            // trio rather than supplying a body for this method.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/ValueTask.cs#L428-L431
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/ConfiguredValueTaskAwaitable.cs#L22
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.ValueTask"
                "ConfigureAwait"
                [ IntrinsicParameterPattern.Exact "System.Boolean" ]
            // The `Task`/`Task<TResult>` members of the same `ConfigureAwait` family. The
            // `[Intrinsic]` reasoning above carries over verbatim — these are in fact the overloads
            // the JIT's class-name test was *written* for — but the body is a different shape, so it
            // gets its own review rather than riding on the ValueTask one:
            //   ldarg.0; ldarg.1; brtrue.s L; ldc.i4.0; br.s M; L: ldc.i4.1
            //   M: newobj ConfiguredTaskAwaitable[`1]::.ctor(Task[`1], ConfigureAwaitOptions)
            //   ret
            // i.e. the `bool` is selected into a `ConfigureAwaitOptions` — `true` to
            // ContinueOnCapturedContext (1), `false` to None (0) — and handed with the task to the
            // awaitable's constructor, whose whole body is `m_configuredTaskAwaiter = new
            // ConfiguredTaskAwaiter(task, options)`: two field stores into a nested struct. No
            // validation, no P/Invoke, and nothing that inspects the options — as with ValueTask,
            // only the *awaiter* later reads them, when it decides how to schedule a continuation
            // and (for SuppressThrowing, which this overload cannot produce) whether to propagate a
            // fault.
            //
            // Both overloads of each type are listed by parameter shape rather than with
            // `anyParams`, so that the `ConfigureAwaitOptions`-taking siblings — whose bodies do
            // validate — are not swept in by accident.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Task.cs#L2450-L2454
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Future.cs#L512-L516
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/TaskAwaiter.cs#L364-L368
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.Task"
                "ConfigureAwait"
                [ IntrinsicParameterPattern.Exact "System.Boolean" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/TaskAwaiter.cs#L447-L450
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.Task`1"
                "ConfigureAwait"
                [ IntrinsicParameterPattern.Exact "System.Boolean" ]
            // The `ConfigureAwaitOptions`-taking siblings of the two entries above. The
            // `[Intrinsic]` reasoning is again unchanged, but these are the only members of the
            // family whose body does more than shuffle values into an awaitable: each validates its
            // argument against a mask, and the two masks differ.
            //
            //   Task:    ldarg.1; ldc.i4.s -8; and; brfalse.s ok
            //            ldc.i4.s 83; call ThrowHelper::ThrowArgumentOutOfRangeException
            //     ok:    ldarg.0; ldarg.1; newobj ConfiguredTaskAwaitable::.ctor(Task, opts); ret
            //
            //   Task<T>: the same with `ldc.i4.s -6`, and a private local function in place of the
            //            ThrowHelper call, which picks between the two ArgumentOutOfRangeException
            //            constructors on `(options & SuppressThrowing) != 0`.
            //
            // `~7` and `~5` are the legal masks: `None | ContinueOnCapturedContext |
            // SuppressThrowing | ForceYielding` for `Task`, the same minus SuppressThrowing (0x2)
            // for `Task<T>`, which cannot both swallow a fault and return a result. Interpreting
            // the bodies is what keeps that asymmetry — and the two distinct exception messages —
            // the guest's CoreLib's business rather than something PawPrint restates and can get
            // out of step with.
            //
            // Every step is modelled: the mask test is `and`/`brfalse`, and both throw paths
            // construct an ordinary `ArgumentOutOfRangeException`. `ThrowHelper` and the SR lookup
            // behind `Task<T>`'s message are already exercised elsewhere; the accompanying
            // `TaskConfigureAwaitOptions.cs` reaches both throws end-to-end, so neither is taken on
            // trust here.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Task.cs#L2460-L2472
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.Task"
                "ConfigureAwait"
                [
                    IntrinsicParameterPattern.Exact "System.Threading.Tasks.ConfigureAwaitOptions"
                ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Future.cs#L522-L536
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.Task`1"
                "ConfigureAwait"
                [
                    IntrinsicParameterPattern.Exact "System.Threading.Tasks.ConfigureAwaitOptions"
                ]
            // IL body is `ldsfld <Default>k__BackingField; ret`; the .cctor constructs the comparer.
            pattern "System.Private.CoreLib" "System.Collections.Generic.EqualityComparer`1" "get_Default" []
            // Same shape as its EqualityComparer sibling above: the IL body is
            // `ldsfld <Default>k__BackingField; ret`, and the .cctor picks the comparer via
            // `ComparerHelpers.CreateDefaultComparer(typeof(T))`. The [Intrinsic] marker exists so
            // the JIT can devirtualise the returned comparer's `Compare`; PawPrint has no JIT, so
            // running the IL yields the same object the JIT would have specialised against.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Collections/Generic/Comparer.CoreCLR.cs#L12
            pattern "System.Private.CoreLib" "System.Collections.Generic.Comparer`1" "get_Default" []
            // The IBinaryNumber<TSelf>.Log2 wrappers on the unsigned primitive types each have
            // an IL body of the form `ldarg.0; call int32 BitOperations::Log2(<T>); ret`
            // (with a `(T)` cast for UInt32/UInt64/UIntPtr's typed return). They are marked
            // [Intrinsic] only so the JIT can elide the wrapper; PawPrint can run the IL
            // unchanged because the BitOperations.Log2 boundary is modelled in Intrinsics.fs.
            pattern "System.Private.CoreLib" "System.UInt32" "Log2" [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            pattern "System.Private.CoreLib" "System.UInt64" "Log2" [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "Log2"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            // `BitOperations.LeadingZeroCount`'s uint32 and uint64 overloads are modelled as
            // arms in Intrinsics.fs, because both bodies bottom out in a De Bruijn table backed
            // by a PE byte range. The `(nuint)` overload needs no such treatment: its body is
            // `ldarg.0; conv.u8; call LeadingZeroCount(uint64); ret`, so running the IL lands on
            // the modelled uint64 arm and the guest's own CoreLib decides the width, rather than
            // PawPrint asserting one. The rule is: model the widths BitOperations implements
            // separately, and run its forwarders.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L252-L267
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            // The IBinaryInteger<TSelf>.LeadingZeroCount wrappers on the primitive integer
            // types are each `ldarg.0; call int32 BitOperations::LeadingZeroCount(<U>); ret`,
            // where U is the unsigned type of the same width (the signed wrappers reinterpret
            // their argument, which needs no IL at all) and a `conv` for the wrapper's own
            // return type follows the call on everything but the 32-bit pair. They are marked
            // [Intrinsic] only so the JIT can elide the wrapper; PawPrint can run the IL
            // unchanged because the BitOperations.LeadingZeroCount boundary is modelled in
            // Intrinsics.fs. The narrower wrappers (SByte/Byte/Int16/UInt16) and the Int128
            // pair carry no [Intrinsic] attribute, so they are never routed to Intrinsics.call
            // and need no entry here.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt32.cs#L294-L296
            pattern
                "System.Private.CoreLib"
                "System.Int32"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.Int64"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            pattern
                "System.Private.CoreLib"
                "System.IntPtr"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt32"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt64"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            // BitOperations.TrailingZeroCount's uint32 overload is modelled as an arm in
            // Intrinsics.fs, because its body bottoms out in a De Bruijn table backed by a PE
            // byte range. Its siblings do not need that: `(long)`, `(nint)` and `(nuint)` are
            // one-instruction forwarders to the uint64 overload, and the uint64 overload's own
            // fallback splits the value into 32-bit halves and calls back into the modelled
            // uint32 one. Every `IsSupported` guard on those paths is either folded to false
            // when CoreLib was built for another architecture or answered false by
            // `scalarOnlyFalseIsSupportedIntrinsics`, so the IL runs to the same fully
            // specified answer. This is the LeadingZeroCount rule above applied here: model the
            // widths BitOperations implements separately, and run its forwarders — which for
            // TrailingZeroCount leaves exactly one modelled width, since its uint64 body reduces
            // to the uint32 one instead of reaching a table PawPrint cannot read.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L579-L668
            //
            // `BitOperations.TrailingZeroCount(int)` carries no [Intrinsic] attribute at all,
            // so it is never routed to `Intrinsics.call` and deliberately has no entry here.
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            // The IBinaryInteger<TSelf>.TrailingZeroCount wrappers, exactly as for
            // LeadingZeroCount above: `ldarg.0; call BitOperations::TrailingZeroCount; [conv];
            // ret`, [Intrinsic] only so the JIT can elide the wrapper. The narrower wrappers
            // (SByte/Byte/Int16/UInt16) and the Int128 pair are not [Intrinsic] and so need no
            // entry.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt32.cs#L310-L312
            pattern
                "System.Private.CoreLib"
                "System.Int32"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.Int64"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            pattern
                "System.Private.CoreLib"
                "System.IntPtr"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt32"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt64"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            // BitOperations.RotateLeft is marked [Intrinsic] only so the JIT can lower it to a
            // single ROL instruction; the IL bodies are pure shift+OR over the existing primitive
            // numeric ops PawPrint already supports:
            //   uint:  (value << offset) | (value >> (32 - offset))
            //   ulong: (value << offset) | (value >> (64 - offset))
            //   nuint: forwards to the uint or ulong overload depending on TARGET_64BIT.
            // Reached through the Marvin string-hash path (Dictionary<string, …> keying).
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L675
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateLeft"
                [
                    IntrinsicParameterPattern.Exact "System.UInt32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateLeft"
                [
                    IntrinsicParameterPattern.Exact "System.UInt64"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateLeft"
                [
                    IntrinsicParameterPattern.Exact "System.UIntPtr"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            // BitOperations.RotateRight is the mirror image of RotateLeft above, and
            // [Intrinsic] for the same reason (a single ROR):
            //   uint:  (value >> offset) | (value << (32 - offset))
            //   ulong: (value >> offset) | (value << (64 - offset))
            //   nuint: forwards to the uint or ulong overload depending on TARGET_64BIT.
            // Reached through BinaryPrimitives.ReverseEndianness(uint) below.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L724
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateRight"
                [
                    IntrinsicParameterPattern.Exact "System.UInt32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateRight"
                [
                    IntrinsicParameterPattern.Exact "System.UInt64"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateRight"
                [
                    IntrinsicParameterPattern.Exact "System.UIntPtr"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            // BinaryPrimitives.ReverseEndianness is [Intrinsic] only so the JIT can emit a
            // single BSWAP/REV; every body is pure managed arithmetic over primitives
            // PawPrint already supports:
            //   ushort: (ushort)((value >> 8) + (value << 8))
            //   uint:   RotateRight(value & 0x00FF00FF, 8) + RotateLeft(value & 0xFF00FF00, 8)
            //   ulong:  the two 32-bit halves reversed and swapped
            //   short/int/long: cast to the unsigned overload of the same width and back.
            // The uint overload is why the RotateRight entries above are needed; RotateLeft
            // was already allowlisted for the Marvin hash. The sbyte/byte/nint/nuint/char
            // and Int128/UInt128 overloads are not [Intrinsic] and so need no entry.
            //
            // Reached by any guest writing big-endian on a little-endian host, and in
            // particular by RuntimeILGenerator.InternalEmit, which writes every opcode
            // wider than one byte with WriteInt16BigEndian — i.e. the whole 0xFE page.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Buffers/Binary/BinaryPrimitives.ReverseEndianness.cs#L41
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.Int16" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.UInt16" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            // RuntimeHelpers.IsKnownConstant overloads (Type?, string?, char, generic struct T)
            // are JIT-only intrinsics: every IL body is literally `ldc.i4.0; ret`. The JIT may
            // rewrite the call to `ldc.i4.1` when the argument is a compile-time constant;
            // PawPrint has no JIT, so executing the IL yields the documented fallback (false).
            // The single Any-shaped pattern subsumes all overloads since the IL body is the same
            // regardless of the argument type.
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L168-L178
            pattern
                "System.Private.CoreLib"
                "System.Runtime.CompilerServices.RuntimeHelpers"
                "IsKnownConstant"
                [ IntrinsicParameterPattern.Any ]
            // Volatile.Read/Write wrappers are managed field accesses through volatile struct
            // views. PawPrint does not currently model memory-ordering effects, but executing
            // the IL is deterministic and preserves the accessed value.
            pattern "System.Private.CoreLib" "System.Threading.Volatile" "Read" [ IntrinsicParameterPattern.Byref ]
            pattern
                "System.Private.CoreLib"
                "System.Threading.Volatile"
                "Write"
                [ IntrinsicParameterPattern.Byref ; IntrinsicParameterPattern.Any ]
            // Unlike its `IsAddressLessThan` / `IsAddressGreaterThan` siblings — whose bodies
            // are a bare `throw new PlatformNotSupportedException()` and so must be intercepted
            // in `Intrinsics.fs` — this one has a real IL body:
            // `ldarg.0; ldarg.1; call Unsafe::IsAddressLessThan<!!T>; ldc.i4.0; ceq; ret`.
            // The `[Intrinsic]` marker is only so the JIT can emit `cge.un` directly. Executing
            // the IL routes through the `IsAddressLessThan` intrinsic and negates it, which is
            // exactly the documented meaning.
            //
            // `IsAddressLessThanOrEqualTo` has the mirror-image body but bottoms out in
            // `IsAddressGreaterThan`, which is not implemented; it is deliberately left off the
            // allowlist so it keeps failing at the intrinsic dispatcher, naming the method that
            // is actually missing rather than failing one frame deeper.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/Unsafe.cs#L414-L421
            pattern
                "System.Private.CoreLib"
                "System.Runtime.CompilerServices.Unsafe"
                "IsAddressGreaterThanOrEqualTo"
                [ IntrinsicParameterPattern.Byref ; IntrinsicParameterPattern.Byref ]
        ]

    let isSafeIntrinsic (key : IntrinsicMethodKey) : bool =
        safeIntrinsics |> List.exists (fun pattern -> methodPatternMatches pattern key)
