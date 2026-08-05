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
            match state.LoadedAssembly methodToCall.DeclaringType.Assembly with
            | Some assy -> assy
            | None ->
                failwith
                    $"Intrinsic method key requested for method whose declaring assembly is not loaded: %O{methodToCall}"

        let declaringType =
            declaringAssy.TypeDefs.[methodToCall.DeclaringType.Definition.Get]

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
            AssemblyName = methodToCall.DeclaringType.Assembly.Name
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
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3561
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
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3601
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
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3553
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "StartsWith"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            // https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3593
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
            // .NET 10 added [Intrinsic] to RuntimeTypeHandle.ToIntPtr; the IL body delegates
            // to the Value getter which reads RuntimeType.m_handle, a field PawPrint already
            // populates with NativeIntSource.TypeHandlePtr. Executing the IL is safe and
            // round-trips through the existing TypeHandle representation.
            // https://github.com/dotnet/runtime/blob/HEAD/src/coreclr/System.Private.CoreLib/src/System/RuntimeHandles.cs#L43-L44
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
            // what distinguishes this from the sibling `Span<T>.Clear`, which is implemented
            // natively in `Intrinsics.fs` precisely because its IL bottoms out in
            // `SpanHelpers.ClearWithoutReferences`' P/Invoke fallback.
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
            // explicitly in `Intrinsics.fs` and routed through `CellAwareCopy.copy`, so it is
            // deliberately omitted from the safe-intrinsic allowlist: the managed body's
            // `Unsafe.ReadUnaligned<Block16>` walk would lose non-`Verbatim` cell provenance.
            // https://github.com/dotnet/runtime/blob/1c3221b63340d7f81dfd829f3bcd822e582324f6/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L799
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_CurrentThread" []
            // IL body is `ldarg.0; ldfld _managedThreadId; ret` — pure field access.
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_ManagedThreadId" []
            // IL body is `ldsfld <Default>k__BackingField; ret`; the .cctor constructs the comparer.
            pattern "System.Private.CoreLib" "System.Collections.Generic.EqualityComparer`1" "get_Default" []
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
        ]

    let isSafeIntrinsic (key : IntrinsicMethodKey) : bool =
        safeIntrinsics |> List.exists (fun pattern -> methodPatternMatches pattern key)
