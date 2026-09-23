namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IntrinsicMethodKeys =
    type IntrinsicMethodKey =
        {
            /// The definition identity of the declaring assembly. A pattern below names an assembly by
            /// its simple name instead, and `methodPatternMatches` bridges the two: keying on the
            /// identity is what keeps building a key off the interpreter's hot path, since the identity
            /// is what a `MethodInfo` already carries.
            DeclaringAssemblyFullName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterShapes : string list

            /// The shape of what the method returns, in the same vocabulary as `ParameterShapes`.
            /// Overloads that differ only here are distinct methods with distinct bodies -- CoreLib's
            /// conversion operators are the reason this is here: `System.Int128` declares sixteen
            /// `op_Explicit` overloads that all take a single `System.Int128`, and eleven of them are
            /// a one-instruction truncation while the rest are real floating-point algorithms.
            ReturnShape : MethodReturnType<string>
        }

    [<RequireQualifiedAccess>]
    type private IntrinsicParameterPattern =
        | Any
        | Exact of string
        | Byref
        | Pointer
        | SzArray
        | Array

    /// How a pattern constrains what a method returns. This mirrors `MethodReturnType` rather than
    /// folding `Void` into the shape vocabulary, so "returns nothing" cannot be confused with
    /// "returns something whose shape happens to be spelled that way".
    [<RequireQualifiedAccess>]
    type private IntrinsicReturnPattern =
        | Void
        | Returns of IntrinsicParameterPattern

    type private IntrinsicMethodPattern =
        {
            AssemblyName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterPatterns : IntrinsicParameterPattern list option
            /// `None` constrains nothing, which is what almost every entry wants: an overload set
            /// whose members already differ in their parameters is fully identified without this.
            ReturnPattern : IntrinsicReturnPattern option
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
            ReturnPattern = None
        }

    /// As `pattern`, but also pinning what the method returns. Reach for this only when the
    /// parameters do not identify the method on their own -- an overload set that differs solely in
    /// its return type, which in CoreLib means the conversion operators.
    let private patternReturning
        (assemblyName : string)
        (declaringTypeFullName : string)
        (methodName : string)
        (parameterPatterns : IntrinsicParameterPattern list)
        (returnPattern : IntrinsicReturnPattern)
        : IntrinsicMethodPattern
        =
        {
            AssemblyName = assemblyName
            DeclaringTypeFullName = declaringTypeFullName
            MethodName = methodName
            ParameterPatterns = Some parameterPatterns
            ReturnPattern = Some returnPattern
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
            ReturnPattern = None
        }

    /// A pattern, with the fingerprints of the IL bodies whose review admitted it.
    ///
    /// The review is of a body, not of a name: a runtime whose body for the same method differs
    /// has not been reviewed, and the gate refuses it rather than interpreting IL nobody read. So a
    /// row lists one fingerprint per distinct body it names in each CoreLib reviewed — one per
    /// overload, and more where CoreLib is compiled differently per architecture or per runtime.
    type private ReviewedIntrinsic =
        {
            Pattern : IntrinsicMethodPattern
            ReviewedBodies : IlBodyFingerprint list
        }

    /// Attach to `pattern` the fingerprints of the bodies its review covered, each as
    /// `IlBodyFingerprint.Hex` renders it.
    let private reviewed (fingerprints : string list) (pattern : IntrinsicMethodPattern) : ReviewedIntrinsic =
        if List.isEmpty fingerprints then
            failwith
                $"safeIntrinsics row for %s{pattern.DeclaringTypeFullName}::%s{pattern.MethodName} lists no reviewed body, so it could admit nothing"

        {
            Pattern = pattern
            ReviewedBodies = fingerprints |> List.map IlBodyFingerprint.OfHex
        }

    let methodKey
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IntrinsicMethodKey
        =
        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringAssemblyFullName with
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
            DeclaringAssemblyFullName = methodToCall.DeclaringAssemblyFullName
            DeclaringTypeFullName = TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) declaringType
            MethodName = methodToCall.Name
            ParameterShapes = methodToCall.Signature.ParameterTypes |> List.map concreteTypeShape
            ReturnShape =
                // Note that `MethodReturnType.Void` is strictly the bare `void` column: a `void`
                // under custom modifiers, which is how C# spells every `init` accessor, decodes as
                // `Returns` (see `TypeMethodSignature`'s own docstring). Mirroring the decoded
                // column rather than re-classifying it keeps this key saying what the blob said.
                match methodToCall.Signature.ReturnType with
                | MethodReturnType.Void -> MethodReturnType.Void
                | MethodReturnType.Returns handle -> MethodReturnType.Returns (concreteTypeShape handle)
        }

    let formatMethodKey (key : IntrinsicMethodKey) : string =
        let parameters = key.ParameterShapes |> String.concat ", "

        let returns =
            match key.ReturnShape with
            | MethodReturnType.Void -> "void"
            | MethodReturnType.Returns shape -> shape

        $"%s{AssemblyDefinitionName.simpleName key.DeclaringAssemblyFullName} %s{key.DeclaringTypeFullName}.%s{key.MethodName}(%s{parameters}) : %s{returns}"

    let private parameterPatternMatches (pattern : IntrinsicParameterPattern) (actual : string) : bool =
        match pattern with
        | IntrinsicParameterPattern.Any -> true
        | IntrinsicParameterPattern.Exact expected -> expected = actual
        | IntrinsicParameterPattern.Byref -> actual = "&"
        | IntrinsicParameterPattern.Pointer -> actual = "*"
        | IntrinsicParameterPattern.SzArray -> actual = "[]"
        | IntrinsicParameterPattern.Array -> actual.StartsWith ("[", StringComparison.Ordinal)

    let private returnPatternMatches (pattern : IntrinsicReturnPattern) (actual : MethodReturnType<string>) : bool =
        match pattern, actual with
        | IntrinsicReturnPattern.Void, MethodReturnType.Void -> true
        | IntrinsicReturnPattern.Returns pattern, MethodReturnType.Returns actual ->
            parameterPatternMatches pattern actual
        | IntrinsicReturnPattern.Void, MethodReturnType.Returns _
        | IntrinsicReturnPattern.Returns _, MethodReturnType.Void -> false

    let private methodPatternMatches (pattern : IntrinsicMethodPattern) (key : IntrinsicMethodKey) : bool =
        AssemblyDefinitionName.isNamed pattern.AssemblyName key.DeclaringAssemblyFullName
        && pattern.DeclaringTypeFullName = key.DeclaringTypeFullName
        && pattern.MethodName = key.MethodName
        && (
            match pattern.ParameterPatterns with
            | None -> true
            | Some patterns ->
                List.length patterns = List.length key.ParameterShapes
                && List.forall2 parameterPatternMatches patterns key.ParameterShapes
        )
        && match pattern.ReturnPattern with
           | None -> true
           | Some pattern -> returnPatternMatches pattern key.ReturnShape

    // Each row ends in the fingerprints of the bodies its review covered. To add a row, or to
    // re-review one whose body changed, read the body's IL (`WoofWare.PawPrint.IlDump`), then take
    // its fingerprint from `TestSafeIntrinsicFingerprints`, whose audit prints every body a row
    // names but has not reviewed.
    let private safeIntrinsics =
        [
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L739-L750
            pattern "System.Private.CoreLib" "System.String" "get_Length" []
            |> reviewed [ "38fd0772174bbb41" ]
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/String.cs#L728-L737
            pattern
                "System.Private.CoreLib"
                "System.String"
                "get_Chars"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "395287e219521deb" ]
            // IL body constructs a span over the string contents; PawPrint's string field
            // projection handles the `_firstChar` boundary it depends on.
            pattern
                "System.Private.CoreLib"
                "System.String"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.String" ]
            |> reviewed [ "bee1a083f49efece" ]
            // String overloads bottom out in String.GetRawStringData plus ReadOnlySpan construction.
            anyParams "System.Private.CoreLib" "System.MemoryExtensions" "AsSpan"
            |> reviewed [ "bee1a083f49efece" ]
            // Managed wrapper over RuntimeHelpers.IsBitwiseEquatable<T> and SpanHelpers.SequenceEqual.
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "SequenceEqual"
                [
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            |> reviewed [ "5f3060105d5b29bd" ]
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
            |> reviewed [ "a08f29df67ec4f46" ]
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
            |> reviewed [ "7b84c45911de5533" ]
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
            |> reviewed [ "2cc2b5dbd81c58f3" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/MemoryExtensions.cs#L3593-L3594
            pattern
                "System.Private.CoreLib"
                "System.MemoryExtensions"
                "EndsWith"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.ReadOnlySpan`1"
                ]
            |> reviewed [ "8e1d9da1a0be6d9f" ]
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/ArgumentNullException.cs#L54
            anyParams "System.Private.CoreLib" "System.ArgumentNullException" "ThrowIfNull"
            |> reviewed [ "44470ee49f19f992" ; "f0653be9d999b198" ]
            // The instance `String.Equals(string)` overload — the one that implements
            // `IEquatable<string>`, so interface dispatch through `IEquatable<string>::Equals`
            // resolves to it. Its `[Intrinsic]` is a pure codegen hint ("Unrolled and vectorized
            // for half-constant input"), so the managed body is the semantic definition:
            // ReferenceEquals, a null check, a Length compare, then `EqualsHelper`. All of those
            // are already-modelled string primitives.
            //
            // Four `String.Equals` overloads can reach this allowlist, and they are served three
            // different ways, which is why every pattern here is parameter-shape-specific rather
            // than `anyParams`. This one and the two `StringComparison`-taking overloads below
            // run their own IL. The *static* two-argument `String.Equals(string, string)` is
            // served by a hand-written arm in `Intrinsics.fs` instead, so it is deliberately
            // absent. The `Equals(object)` override carries no `[Intrinsic]` at all, so it never
            // consults this list. No two of those parameter shapes overlap.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L607-L625
            pattern
                "System.Private.CoreLib"
                "System.String"
                "Equals"
                [ IntrinsicParameterPattern.Exact "System.String" ]
            |> reviewed [ "db46097de4eb6aa4" ]
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
            //    `Utf16Utility` bit-twiddling, with no P/Invoke. (No CPU profile PawPrint offers
            //    reports vector acceleration, so `EqualsIgnoreCase_Vector` is unreachable, and
            //    no test can cover it.) Non-ASCII input leaves that fast path for
            //    `Ordinal.CompareStringIgnoreCase`, which under the guest's
            //    `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` bottoms out in `InvariantModeCasing`'s
            //    managed `CharUnicodeInfo` tables. PawPrint runs those, so non-ASCII input is
            //    answered rather than refused — measured on Latin-1, Greek and Cyrillic case
            //    pairs, and on the `U+017F`/`S` and `U+0130`/`i` folds. No test asserts those
            //    answers, because the differential oracle runs *without* that variable and so
            //    collates with the host's ICU; agreement would then be a fact about two casing
            //    tables rather than about the method under test.
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
            |> reviewed [ "8828ec92d24ed72b" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L517-L557
            pattern
                "System.Private.CoreLib"
                "System.String"
                "EndsWith"
                [
                    IntrinsicParameterPattern.Exact "System.String"
                    IntrinsicParameterPattern.Exact "System.StringComparison"
                ]
            |> reviewed [ "b7bc9ec03ab544fb" ]
            // `String.Equals(string, StringComparison)` and the static
            // `String.Equals(string, string, StringComparison)`. The same shape as the two
            // entries above and for the same reason: the `[Intrinsic]` is the codegen hint
            // "Unrolled and vectorized for half-constant input (Ordinal)", so the managed body is
            // the semantic definition — and here too the body is mostly argument validation whose
            // *ordering* a native reimplementation would have to duplicate. `CheckStringComparison`
            // runs on both short-circuit paths before either returns, so a reference-equal pair
            // and a null argument each throw `ArgumentException` when `comparisonType` is out of
            // range rather than returning their answer. (Unlike `StartsWith`, neither overload has
            // an `ArgumentNullException.ThrowIfNull`: null is a legal argument to both, and for
            // the static overload two nulls compare equal. So `Equals(null, null, invalid)` throws
            // by the reference-equality route, not by a null check.)
            //
            // The switch arms bottom out one step earlier than `StartsWith`'s, because equality
            // needs no offset arithmetic:
            //  * Ordinal compares `Length`, then `EqualsHelper` — i.e.
            //    `SpanHelpers.SequenceEqual(ref byte, ref byte, nuint)` over
            //    `GetRawStringDataAsUInt8()`, intercepted explicitly in `Intrinsics.fs`.
            //  * OrdinalIgnoreCase compares `Length`, then `EqualsOrdinalIgnoreCaseNoLengthCheck`
            //    — the same `Ordinal.EqualsIgnoreCase(ref char, ref char, int)` the
            //    `StartsWith` entry above describes, so the same scalar walk and the same
            //    non-ASCII tail.
            //  * The four culture-sensitive arms are the one genuinely new callee:
            //    `CompareInfo.Compare(a, b, options) == 0`, where `StartsWith` used
            //    `IsPrefix`/`IsSuffix`. Under the guest's
            //    `DOTNET_SYSTEM_GLOBALIZATION_INVARIANT=1` the span overload short-circuits ahead
            //    of any ICU call, to `ReadOnlySpan<char>.SequenceCompareTo` for the case-sensitive
            //    options and to `Ordinal.CompareStringIgnoreCase` for the IgnoreCase ones. Both
            //    are ordinary IL over the same `Unsafe.ReadUnaligned` byte-cursor shape the
            //    OrdinalIgnoreCase arm already relies on; `MemoryExtensions.SequenceCompareTo` is
            //    not itself `[Intrinsic]`, so it needs no entry of its own.
            //
            // Because the API exposes only `Compare(...) == 0`, a comparer that were wrong in
            // *sign* alone would be invisible from here; `sourcesPure/StringEqualsComparison.cs`
            // pins what is observable, which is the equality verdict.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L626-L664
            pattern
                "System.Private.CoreLib"
                "System.String"
                "Equals"
                [
                    IntrinsicParameterPattern.Exact "System.String"
                    IntrinsicParameterPattern.Exact "System.StringComparison"
                ]
            |> reviewed [ "9bdde7fa8d0a2f99" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/String.Comparison.cs#L684-L725
            pattern
                "System.Private.CoreLib"
                "System.String"
                "Equals"
                [
                    IntrinsicParameterPattern.Exact "System.String"
                    IntrinsicParameterPattern.Exact "System.String"
                    IntrinsicParameterPattern.Exact "System.StringComparison"
                ]
            |> reviewed [ "fb25db27d8ca4f2b" ]
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
            |> reviewed [ "e12bee258bf174ba" ]
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/coreclr/System.Private.CoreLib/src/System/Type.CoreCLR.cs#L82
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "GetTypeFromHandle"
                [ IntrinsicParameterPattern.Exact "System.RuntimeTypeHandle" ]
            |> reviewed [ "8d35749525955904" ]
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
            |> reviewed [ "e55fa69b07ad55ba" ]
            // The base `Type.TypeHandle` getter is `[Intrinsic]` with an IL body of
            // `throw new NotSupportedException()`, and that throw is the behaviour we want.
            // Under `callvirt` on any PawPrint-created receiver, virtual resolution selects the
            // `RuntimeType` override above, so this body runs only for a `Type` subclass that
            // does not override it — where throwing is exactly right — or for a non-virtual
            // `call`, where ECMA-335 dispatches statically to this body and .NET throws too.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Type.cs#L467-L471
            pattern "System.Private.CoreLib" "System.Type" "get_TypeHandle" []
            |> reviewed [ "f71fcaa21189fd8c" ]
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
            |> reviewed [ "e361800dd434bf34" ]
            // `Type.IsValueType` is the same shape as `IsPrimitive` above: a non-virtual
            // property whose `[Intrinsic]` getter is `ldarg.0; callvirt Type::IsValueTypeImpl();
            // ret`, with the attribute present only for the JIT's `typeof(X)` constant-fold
            // (`NI_System_Type_get_IsValueType`, importercalls.cpp:4045). PawPrint performs that
            // fold too, at the `ldtoken` that begins `typeof(X).IsValueType`
            // (`TypeofIntrinsicFold`), so this entry is what runs for every other receiver. The
            // getter is always the call target, so its `callvirt` is the only thing that selects
            // an implementation — a `TypeDelegator` or a guest `Type` subclass must answer from
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
            |> reviewed [ "0f480743c0d9dcb1" ]
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
            //    parameters, byrefs, pointers and function pointers, which the short-circuit
            //    depends on: `MethodTable::ParentMethodTable` refuses a TypeDesc target, so a
            //    wrong tag would surface as a loud projection failure rather than a wrong
            //    answer.
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
            |> reviewed [ "b659e1dac527cb4e" ]
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
            |> reviewed [ "002a3013b9d4ad08" ]
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
            |> reviewed [ "4835ccf731b7fe12" ]
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.Type"
                    IntrinsicParameterPattern.Exact "System.Type"
                ]
            |> reviewed [ "48a86daf5f0fdf68" ]
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
            |> reviewed [ "0c77c32e3fd5ae99" ]
            // Virtual IsAssignableFrom; the override on RuntimeType is what carries the cast logic,
            // but the base IL body itself is safe (it handles null, identity, and a few fallbacks
            // that delegate back through normal virtual dispatch).
            // https://github.com/dotnet/runtime/blob/ec11903827fc28847d775ba17e0cd1ff56cfbc2e/src/libraries/System.Private.CoreLib/src/System/Type.Helpers.cs#L336
            pattern
                "System.Private.CoreLib"
                "System.Type"
                "IsAssignableFrom"
                [ IntrinsicParameterPattern.Exact "System.Type" ]
            |> reviewed [ "7778c6cb88c7cf2c" ]
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L161
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_Length" []
            |> reviewed [ "15d6b5958f957dae" ]
            // IL body is `ldarg.0; ldfld _length; ldc.i4.0; ceq; ret`.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_IsEmpty" []
            |> reviewed [ "24fe3aa069451107" ]
            // Reviewed constructors initialise `_reference` / `_length` through already-modelled
            // array and byref boundaries. The `(void*, int)` constructor is an explicit
            // intrinsic implementation below because it crosses the unmanaged-pointer boundary.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" ".ctor" [ IntrinsicParameterPattern.SzArray ]
            |> reviewed [ "22d28eac7f58f533" ]
            // IL body delegates to the array-backed constructor above.
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "op_Implicit" [ IntrinsicParameterPattern.SzArray ]
            |> reviewed [ "e84bc450f73748c4" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.SzArray
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "4b99228380ab2ef2" ]
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" ".ctor" [ IntrinsicParameterPattern.Byref ]
            |> reviewed [ "a27e6d9f46e1eea8" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "a50d95ac73e4dad8" ]
            // Managed wrappers over already-modelled span fields, bounds checks, array allocation,
            // and Buffer.Memmove.
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "CopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            |> reviewed [ "942d11d3cb4e81b9" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "TryCopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            |> reviewed [ "f79215942a886d4e" ]
            // Reviewed IL: bounds checks, Unsafe.Add over the span byref, then byref+length
            // ReadOnlySpan<T> construction. Unsafe.Add and the constructor are implemented
            // boundaries below.
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "Slice"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "728ad5d03c2782ca" ]
            pattern
                "System.Private.CoreLib"
                "System.ReadOnlySpan`1"
                "Slice"
                [
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "6941a7dbc8479a50" ]
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "ToArray" []
            |> reviewed [ "9d482ee41f7b781b" ]
            // IL body is `Unsafe.NullRef<T>(); if (_length != 0) ret = ref _reference; return ret`.
            // Unsafe.NullRef is implemented as an intrinsic in Intrinsics.fs; the field reads
            // and managed-byref assignment are already-modelled span primitives.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L289
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "GetPinnableReference" []
            |> reviewed [ "6af394479d82d3f8" ]
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
            // This is deliberately *reference* equality, not content equality: equal-
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
            |> reviewed [ "eda5cd2cad4c672b" ]
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
            |> reviewed [ "5084867d33a7f076" ]
            // IL body for both Span<T>.Empty and ReadOnlySpan<T>.Empty is
            // `.locals init (valuetype S V_0) ldloca.s V_0; initobj S; ldloc.0; ret` —
            // i.e. just returning `default(...)`. The `[Intrinsic]` attribute is for
            // JIT inlining; the IL is safe to execute directly.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/ReadOnlySpan.cs#L214
            pattern "System.Private.CoreLib" "System.ReadOnlySpan`1" "get_Empty" []
            |> reviewed [ "15d76781d54bed34" ]
            // IL body is `ldarg.0; ldfld _length; ret`.
            pattern "System.Private.CoreLib" "System.Span`1" "get_Length" []
            |> reviewed [ "ea013f93d5fc97c9" ]
            // IL body is `ldarg.0; ldfld _length; ldc.i4.0; ceq; ret`.
            pattern "System.Private.CoreLib" "System.Span`1" "get_IsEmpty" []
            |> reviewed [ "181fa97488e2e538" ]
            // See ReadOnlySpan<T>.get_Empty above; the IL body is the same `default(Span<T>)` shape.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Span.cs#L219
            pattern "System.Private.CoreLib" "System.Span`1" "get_Empty" []
            |> reviewed [ "50aba32b3d546a25" ]
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
            |> reviewed [ "cd031528095dd57f" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Span.cs#L183
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.Span`1"
                    IntrinsicParameterPattern.Exact "System.Span`1"
                ]
            |> reviewed [ "0ab4beee79659389" ]
            // Same constructor shape as ReadOnlySpan<T>; the `(void*, int)` constructor is
            // handled explicitly below.
            pattern "System.Private.CoreLib" "System.Span`1" ".ctor" [ IntrinsicParameterPattern.SzArray ]
            |> reviewed [ "cd487f3c94052afc" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.SzArray
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "52a4965e732f163b" ]
            pattern "System.Private.CoreLib" "System.Span`1" ".ctor" [ IntrinsicParameterPattern.Byref ]
            |> reviewed [ "40bed4d169ce9696" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                ".ctor"
                [
                    IntrinsicParameterPattern.Byref
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "e91445b3df56ee93" ]
            // IL body delegates to the array-backed constructor above.
            pattern "System.Private.CoreLib" "System.Span`1" "op_Implicit" [ IntrinsicParameterPattern.SzArray ]
            |> reviewed [ "512f7ca88469124c" ]
            // IL body constructs ReadOnlySpan<T> over this span's `_reference` and `_length`.
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            |> reviewed [ "b67aaf27a837d367" ]
            // Managed wrappers over already-modelled span fields, bounds checks, array allocation,
            // and Buffer.Memmove.
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "CopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            |> reviewed [ "61deffc1e2a12687" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "TryCopyTo"
                [ IntrinsicParameterPattern.Exact "System.Span`1" ]
            |> reviewed [ "0ba317dd3f2636d6" ]
            // Reviewed IL: bounds checks, Unsafe.Add over the span byref, then byref+length
            // Span<T> construction. Unsafe.Add and the constructor are implemented
            // boundaries below.
            pattern "System.Private.CoreLib" "System.Span`1" "Slice" [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "c4e5452099e94073" ]
            pattern
                "System.Private.CoreLib"
                "System.Span`1"
                "Slice"
                [
                    IntrinsicParameterPattern.Exact "System.Int32"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "7c435a2fc71b8220" ]
            pattern "System.Private.CoreLib" "System.Span`1" "ToArray" []
            |> reviewed [ "192194b00fde4ed0" ]
            // IL body is `ldarg.0; ldfld _reference; ldarg.0; ldfld _length; conv.u; ldarg.1;
            // call SpanHelpers::Fill<T>` — pure field reads plus the helper allowlisted below.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Span.cs#L310-L313
            pattern "System.Private.CoreLib" "System.Span`1" "Fill" [ IntrinsicParameterPattern.Any ]
            |> reviewed [ "5f10a3bf0f0700ab" ]
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
            |> reviewed [ "595d78a78e48c6a9" ]
            // Same IL body as ReadOnlySpan<T>.GetPinnableReference above.
            // https://github.com/dotnet/runtime/blob/108fa7856efcfd39bc991c2d849eabbf7ba5989c/src/libraries/System.Private.CoreLib/src/System/Span.cs#L282
            pattern "System.Private.CoreLib" "System.Span`1" "GetPinnableReference" []
            |> reviewed [ "778f8e72c038aac4" ]
            // https://github.com/dotnet/runtime/blob/9e5e6aa7bc36aeb2a154709a9d1192030c30a2ef/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.cs#L153
            anyParams "System.Private.CoreLib" "System.Runtime.CompilerServices.RuntimeHelpers" "CreateSpan"
            |> reviewed [ "4dfba8a1067d2b65" ]
            // Unusual among these entries: CoreCLR does not merely *recognise* this method, it
            // swaps a different IL body in at JIT time (`getILIntrinsicImplementationForRuntimeHelpers`,
            // jitinterface.cpp:7383). Two consequences. First, the swap is conditional on the
            // underlying type being one of I1/U1/I2/U2/I4/U4/I8/U8, so for any other underlying type
            // the shipped body below is what real .NET runs too — this is live code, not a placeholder
            // like `CopyConstruct`'s `throw`. Second, where the swap does fire it emits
            // `ldarga.s 0; ldarg.1; call <underlying>::CompareTo(<underlying>)`, and the shipped body
            // reaches that same primitive `CompareTo` the long way round, so the returned int is equal
            // by construction rather than merely equal in sign.
            //
            // The shipped body is `ldarga.s 0; ldarg.1; box T; constrained. T;
            // callvirt Enum::CompareTo(object); ret`. All three of `Enum.CompareTo(object)`'s
            // preambles are unreachable for this call shape, which is what makes running it safe:
            // `ReferenceEquals(this, target)` is false for two fresh boxes (and where it could fire,
            // the operands are equal and 0 is the answer anyway); `target is null` cannot hold, since
            // boxing a struct never yields null; and the `ArgumentException` arm needs two *different*
            // runtime types, which the `(T, T)` signature forecloses.
            //
            // Divergence, in the conservative direction: the shipped body allocates two boxes per
            // comparison that the swapped-in body does not. No guest PawPrint supports today can see
            // that; a guest reading `GC.GetTotalAllocatedBytes` could in principle.
            //
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Runtime/CompilerServices/RuntimeHelpers.CoreCLR.cs#L371-L377
            pattern
                "System.Private.CoreLib"
                "System.Runtime.CompilerServices.RuntimeHelpers"
                "EnumCompareTo"
                [ IntrinsicParameterPattern.Any ; IntrinsicParameterPattern.Any ]
            |> reviewed [ "35fc80ab0662b27f" ]
            // Sibling of `EnumCompareTo` above, and the EE swaps its body on the same terms: the same
            // eight underlying types, from the same function, in the branch immediately before it
            // (jitinterface.cpp:7342-7382). The two differ only in what the swapped-in body is and in
            // which `Enum` member the shipped one reaches.
            //
            // Here the swap emits `ldarg.0; ldarg.1; ceq; ret`, and the shipped body is
            // `ldarga.s 0; ldarg.1; box T; constrained. T; callvirt Object::Equals(object); ret`.
            // (C# emitted the base-most declaration, but `constrained.` on an enum resolves it to
            // `Enum::Equals(object)`, which overrides it; the enum itself declares no `Equals`, so the
            // receiver is boxed and dispatched normally.) `Enum.Equals(object)` has three preambles,
            // all unreachable for this call shape: `obj is null` cannot hold, since boxing a struct
            // never yields null; `this == obj` is reference equality between two boxes `box` has just
            // allocated separately; and `GetType() != obj.GetType()` is foreclosed by the `(T, T)`
            // signature. So control always reaches the switch, which compares the two boxes' raw bytes
            // at the underlying type's width — the same answer `ceq` gives, since the widening `ldarg`
            // applies to a 1- or 2-byte enum is injective on those bytes whichever way it signs them.
            //
            // Same divergence as above, in the same conservative direction: two boxes per comparison
            // that the swapped-in body does not allocate.
            //
            // Where the swap does *not* fire the shipped body is what real .NET runs too, so admitting
            // it is faithful by construction rather than by any argument about what it computes. That
            // switch does answer for bool, char, float, double, nint and nuint, under the `RARE_ENUMS`
            // its own file defines (Enum.cs:5-7); only genuinely unknown metadata reaches the default
            // arm. None of it is reachable from C#, which admits only those eight as an underlying
            // type, and a precompiled assembly carrying such an enum does not get this far under
            // PawPrint anyway — measured, it stops earlier, when one is passed as a call argument.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Enum.cs#L1190-L1248
            pattern
                "System.Private.CoreLib"
                "System.Runtime.CompilerServices.RuntimeHelpers"
                "EnumEquals"
                [ IntrinsicParameterPattern.Any ; IntrinsicParameterPattern.Any ]
            |> reviewed [ "c2e248e8214d7e47" ]
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L127
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L137
            anyParams "System.Private.CoreLib" "System.Math" "Abs"
            |> reviewed [ "3084f333b6783eea" ; "2b420246dbc80983" ]
            // Single-line delegation to Math.Abs above; the [Intrinsic] marker is for the JIT,
            // but the IL body is just a tail call we can safely execute.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Double.cs#L1041-L1043
            anyParams "System.Private.CoreLib" "System.Double" "Abs"
            |> reviewed [ "70f1bdc577ed6546" ]
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L965C10-L1062C19
            anyParams "System.Private.CoreLib" "System.Math" "Max"
            |> reviewed
                [
                    "c5c8b96f8131c044"
                    "bc94219deb8a477b"
                    "4758250f793477a5"
                    "232812ea47f49d6c"
                    "ab097a35af259b1a"
                    "732834ac71768b9c"
                    "849ce65dad3e4c69"
                    "e4aad8039181320d"
                    "6341981a1568e70b"
                    "e0e9650953a24c38"
                    "fa49d3eb24a13a6d"
                    "69976df605e9c467"
                ]
            // Mirror of Math.Max above: most overloads have a `(val1 <= val2) ? val1 : val2`
            // IL body, and the [Intrinsic]-marked double/float overloads use the IEEE 754:2019
            // `minimum` definition expressed in terms of IsNaN/IsNegative — both already supported.
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Math.cs#L1064-L1187
            anyParams "System.Private.CoreLib" "System.Math" "Min"
            |> reviewed
                [
                    "ee85a54ee9b5109b"
                    "553b060d95ed40f8"
                    "7ac845014eb63c24"
                    "03fa3c3301eccd8b"
                    "7157f58129739a94"
                    "df55946e411ae51a"
                    "4d295f08dc0e615a"
                    "0a1a422bf6bb6c25"
                    "82c3fc5df0e10963"
                    "6842c11eac026469"
                    "17e9b61c5a1f9c1a"
                    "ec5e215ba0865d28"
                ]
            // https://github.com/dotnet/runtime/blob/d258af50034c192bf7f0a18856bf83d2903d98ae/src/libraries/System.Private.CoreLib/src/System/Buffer.cs#L150
            anyParams "System.Private.CoreLib" "System.Buffer" "Memmove"
            |> reviewed [ "83d1b1c25c9dd67e" ]
            // Note: `System.SpanHelpers.Memmove(ref byte, ref byte, nuint)` is intercepted
            // explicitly in `Intrinsics.fs` and routed through `CellAwareMemOps.copy`, so it is
            // deliberately omitted from the safe-intrinsic allowlist: the managed body's
            // `Unsafe.ReadUnaligned<Block16>` walk would lose non-`Verbatim` cell provenance.
            // https://github.com/dotnet/runtime/blob/1c3221b63340d7f81dfd829f3bcd822e582324f6/src/libraries/System.Private.CoreLib/src/System/Threading/Thread.cs#L799
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_CurrentThread" []
            |> reviewed [ "732b73d11652fbfd" ]
            // IL body is `ldarg.0; ldfld _managedThreadId; ret` — pure field access.
            pattern "System.Private.CoreLib" "System.Threading.Thread" "get_ManagedThreadId" []
            |> reviewed [ "c11b0f7a76bc414c" ]
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
            |> reviewed [ "5e2c808c73bf2fd6" ]
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
            |> reviewed [ "a30a4044f92e90ce" ]
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
            |> reviewed [ "1abd5f84a1d51288" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/CompilerServices/TaskAwaiter.cs#L447-L450
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.Task`1"
                "ConfigureAwait"
                [ IntrinsicParameterPattern.Exact "System.Boolean" ]
            |> reviewed [ "0e322ffcc5b0b89d" ]
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
            |> reviewed [ "88b782c0cef3b733" ]
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Threading/Tasks/Future.cs#L522-L536
            pattern
                "System.Private.CoreLib"
                "System.Threading.Tasks.Task`1"
                "ConfigureAwait"
                [
                    IntrinsicParameterPattern.Exact "System.Threading.Tasks.ConfigureAwaitOptions"
                ]
            |> reviewed [ "82ae502751710f3b" ]
            // IL body is `ldsfld <Default>k__BackingField; ret`; the .cctor constructs the comparer.
            pattern "System.Private.CoreLib" "System.Collections.Generic.EqualityComparer`1" "get_Default" []
            |> reviewed [ "557df9bb6a2553b3" ]
            // Same shape as its EqualityComparer sibling above: the IL body is
            // `ldsfld <Default>k__BackingField; ret`, and the .cctor picks the comparer via
            // `ComparerHelpers.CreateDefaultComparer(typeof(T))`. The [Intrinsic] marker exists so
            // the JIT can devirtualise the returned comparer's `Compare`; PawPrint has no JIT, so
            // running the IL yields the same object the JIT would have specialised against.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/coreclr/System.Private.CoreLib/src/System/Collections/Generic/Comparer.CoreCLR.cs#L12
            pattern "System.Private.CoreLib" "System.Collections.Generic.Comparer`1" "get_Default" []
            |> reviewed [ "ac6b85880ff04a9c" ]
            // The IBinaryNumber<TSelf>.Log2 wrappers on the unsigned primitive types each have
            // an IL body of the form `ldarg.0; call int32 BitOperations::Log2(<T>); ret`
            // (with a `(T)` cast for UInt32/UInt64/UIntPtr's typed return). They are marked
            // [Intrinsic] only so the JIT can elide the wrapper; PawPrint can run the IL
            // unchanged because the BitOperations.Log2 boundary is modelled in Intrinsics.fs.
            pattern "System.Private.CoreLib" "System.UInt32" "Log2" [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "019c382c9ef65870" ]
            pattern "System.Private.CoreLib" "System.UInt64" "Log2" [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "8ea11f141b762614" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "Log2"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "3005697690a2d090" ]
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
            |> reviewed [ "c34c552e5cbaf2f4" ]
            // The IBinaryInteger<TSelf>.LeadingZeroCount wrappers on the primitive integer
            // types are each `ldarg.0; call int32 BitOperations::LeadingZeroCount(<U>); ret`,
            // where U is the unsigned type of the same width (the signed wrappers reinterpret
            // their argument, which needs no IL at all) and a `conv` for the wrapper's own
            // return type follows the call on everything but the 32-bit pair. They are marked
            // [Intrinsic] only so the JIT can elide the wrapper; PawPrint can run the IL
            // unchanged because the BitOperations.LeadingZeroCount boundary is modelled in
            // Intrinsics.fs. The narrower wrappers (SByte/Byte/Int16/UInt16) carry no [Intrinsic]
            // attribute, so they are never routed to Intrinsics.call and need no entry here. The
            // Int128 pair are a different case: the *methods* are unmarked, but `Int128` and
            // `UInt128` carry a type-level [Intrinsic], which routes every one of their members.
            // Their absence here is a gap, not an argument -- see the PopCount block below.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt32.cs#L294-L296
            pattern
                "System.Private.CoreLib"
                "System.Int32"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "05a0111838ea8daf" ]
            pattern
                "System.Private.CoreLib"
                "System.Int64"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            |> reviewed [ "d2977244f93447dc" ]
            pattern
                "System.Private.CoreLib"
                "System.IntPtr"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            |> reviewed [ "3bd4b7ae168022d7" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt32"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "b8b07ceefeed6127" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt64"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "1f1dac92e877a9c9" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "LeadingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "63cedc660d046607" ]
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
            |> reviewed [ "566746e95341b8ae" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "ed52e167a9911294" ; "6912ce739ec4067b" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            |> reviewed [ "ba9b2ce0e5eb1d24" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "dbbe96f66cf2b519" ]
            // The IBinaryInteger<TSelf>.TrailingZeroCount wrappers, exactly as for
            // LeadingZeroCount above: `ldarg.0; call BitOperations::TrailingZeroCount; [conv];
            // ret`, [Intrinsic] only so the JIT can elide the wrapper. The narrower wrappers
            // (SByte/Byte/Int16/UInt16) are not [Intrinsic] and so need no entry; the Int128 pair
            // are unmarked as methods but are routed anyway by their type-level [Intrinsic], as
            // the PopCount block below describes.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt32.cs#L310-L312
            pattern
                "System.Private.CoreLib"
                "System.Int32"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "25d67ae2f5518c9e" ]
            pattern
                "System.Private.CoreLib"
                "System.Int64"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            |> reviewed [ "3bb635f799ce81a7" ]
            pattern
                "System.Private.CoreLib"
                "System.IntPtr"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            |> reviewed [ "0667ef35167597b1" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt32"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "71e0737e98dc4ed2" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt64"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "fa6a742171fb122a" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "TrailingZeroCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "8e475a18c9ae8313" ]
            // BitOperations.PopCount is marked [Intrinsic] only so the JIT can lower it to POPCNT
            // on x86 or CNT+ADDV on Arm. Unlike its LeadingZeroCount, TrailingZeroCount and Log2
            // siblings above, no width of it needs an arm in Intrinsics.fs: its software fallback
            // is pure shift/mask/add/multiply arithmetic on the operand itself,
            //   value -= (value >> 1) & 0x5555_5555;
            //   value = (value & 0x3333_3333) + ((value >> 2) & 0x3333_3333);
            //   value = (((value + (value >> 4)) & 0x0F0F_0F0F) * 0x0101_0101) >> 24;
            // (the uint64 body is the same over 64-bit constants and a final shift of 56), with no
            // De Bruijn lookup table backed by a PE byte range -- which is the only reason those
            // siblings are modelled. The `(nuint)` overload is `ldarg.0; conv.u8; call
            // PopCount(uint64); ret` on a 64-bit build, so it too just forwards.
            //
            // Both hardware guards ahead of the fallback answer false: `Popcnt`, `Popcnt+X64` and
            // `AdvSimd+Arm64` are all in `scalarOnlyFalseIsSupportedIntrinsics`, and whichever of
            // them belongs to a foreign architecture was already folded to `ldc.i4.0` when CoreLib
            // was compiled. The two flavours fold opposite guards, so the IL differs between them:
            // TestLinuxCoreLibFlavour.fs runs the x64 shape, which a macOS/arm64 box never
            // interprets.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/BitOperations.cs#L427-L523
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "98901095dbbbdfb5" ; "ab5f6fb06e73729e" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "7d865354181f50e2" ; "d4044bed748cec8e" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "16ea51d789017d18" ]
            // The IBinaryInteger<TSelf>.PopCount wrappers, exactly as for LeadingZeroCount above:
            // `ldarg.0; call int32 BitOperations::PopCount(<U>); [conv]; ret`, [Intrinsic] only so
            // the JIT can elide the wrapper. The narrower wrappers (SByte/Byte/Int16/UInt16) and
            // `char`'s explicit interface implementation carry no [Intrinsic] attribute, so they
            // are never routed to Intrinsics.call and need no entry here.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt32.cs#L298-L300
            pattern
                "System.Private.CoreLib"
                "System.Int32"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "510aeb677dba7bf8" ]
            pattern
                "System.Private.CoreLib"
                "System.Int64"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            |> reviewed [ "cc3dedc48eff09c9" ]
            pattern
                "System.Private.CoreLib"
                "System.IntPtr"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            |> reviewed [ "6b9ee4707969d7fc" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt32"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "fdb6e36afff429de" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt64"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "9062e04e58775177" ]
            pattern
                "System.Private.CoreLib"
                "System.UIntPtr"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "93576026ab104616" ]
            // `Int128.PopCount` and `UInt128.PopCount` carry no *method*-level [Intrinsic], but
            // their declaring types do, and a type-level marker routes every member — so unlike
            // the narrower wrappers above they do reach Intrinsics.call and do need entries. Their
            // bodies are `ulong.PopCount(value._lower) + ulong.PopCount(value._upper)`, which
            // bottoms out in the UInt64 wrapper allowlisted above and is widened back to the
            // return type through `op_Implicit(UInt64)`. No conversion is needed to *reach*
            // either method, mind: `default(Int128)` is a legal argument that constructs nothing.
            // Everything a guest needs in order to build a 128-bit value and inspect the result
            // is allowlisted together at the end of this list.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt128.cs#L800-L802
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.UInt128" ]
            |> reviewed [ "79ac34263b45dcec" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "PopCount"
                [ IntrinsicParameterPattern.Exact "System.Int128" ]
            |> reviewed [ "dbc41180b21c8d32" ]
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
            |> reviewed [ "e121f1770834cf5c" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateLeft"
                [
                    IntrinsicParameterPattern.Exact "System.UInt64"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "2f28a5c040675f9c" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateLeft"
                [
                    IntrinsicParameterPattern.Exact "System.UIntPtr"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "d8de50ba779e9381" ]
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
            |> reviewed [ "1c80a27ebb085920" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateRight"
                [
                    IntrinsicParameterPattern.Exact "System.UInt64"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "efcd2e6f8451642f" ]
            pattern
                "System.Private.CoreLib"
                "System.Numerics.BitOperations"
                "RotateRight"
                [
                    IntrinsicParameterPattern.Exact "System.UIntPtr"
                    IntrinsicParameterPattern.Exact "System.Int32"
                ]
            |> reviewed [ "5733cf41371ebf8e" ]
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
            |> reviewed [ "cc5c93b393792bb8" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.UInt16" ]
            |> reviewed [ "ba8e3536750582bc" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "3f8e13f5908f0502" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "9fe17e077753de25" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            |> reviewed [ "442770ce955b2425" ]
            pattern
                "System.Private.CoreLib"
                "System.Buffers.Binary.BinaryPrimitives"
                "ReverseEndianness"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "e9bdfdbc4eb8ad83" ]
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
            |> reviewed
                [
                    "bc31b1dffc7a8be1"
                    "4e116e223e13e3e2"
                    "ab1b688621d87123"
                    "29967aa3d40687e0"
                ]
            // Volatile.Read/Write wrappers are managed field accesses through volatile struct
            // views. PawPrint does not currently model memory-ordering effects, but executing
            // the IL is deterministic and preserves the accessed value.
            pattern "System.Private.CoreLib" "System.Threading.Volatile" "Read" [ IntrinsicParameterPattern.Byref ]
            |> reviewed
                [
                    "80edddb425fee093"
                    "8b39318d03dfd6d3"
                    "f3fd5caef16c6ab0"
                    "f777b925d8ee93a0"
                    "eb53b3a29b7cb1f0"
                    "68edcb2a0a44d7f6"
                    "6cdf3192f836645b"
                    "43ad1d82a75e5671"
                    "ba4c73808fe3925a"
                    "5902f8c0a09db997"
                    "949166c1089e884b"
                    "df6f935333fac579"
                    "0989d2760153fcc3"
                    "3169bf1791836545"
                ]
            pattern
                "System.Private.CoreLib"
                "System.Threading.Volatile"
                "Write"
                [ IntrinsicParameterPattern.Byref ; IntrinsicParameterPattern.Any ]
            |> reviewed
                [
                    "887c998984df0cb8"
                    "70d4a58bbd13484a"
                    "95e224d36212393e"
                    "c5ec7866650b9d28"
                    "4aa4973ae40cbf1a"
                    "e215892aea2c87e1"
                    "1fff2ee36fa97343"
                    "f3e8af7791fee3a6"
                    "c891dc7468ae0898"
                    "6858eeeb027b1294"
                    "1e707a75ba7e7cfc"
                    "1c12e517989f182a"
                    "797a6944f72b5117"
                    "f97ec9eeb2f6a058"
                ]
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
            |> reviewed [ "7fb39ae4ec04dc0b" ]
            // Vector{64,128,256,512}<T>.IsSupported and System.Numerics.Vector<T>.IsSupported
            // ask whether T is a valid vector *element type*, not whether the hardware can
            // accelerate the width: real .NET answers true for the twelve primitive element
            // types even on hardware with no SIMD at all, and
            // ThrowHelper.ThrowForUnsupportedIntrinsicsVectorNNNBaseType /
            // ThrowForUnsupportedNumericsVectorBaseType rely on the true answer to no-op on
            // paths that are live under a scalar profile (folding these to false raises
            // NotSupportedException where real .NET proceeds). The body is an honest
            // terminating chain of twelve `typeof(T) == typeof(X)` checks — ldtoken /
            // GetTypeFromHandle / op_Equality throughout, all modelled boundaries — which the
            // JIT merely constant-folds. All five types share the same body shape.
            //
            // A true answer commits PawPrint to nothing further. All five types carry a
            // type-level [Intrinsic], and `isIntrinsic` (IlMachineStateExecution.fs) reads that
            // attribute off the call site's declaring type, so a direct call to any other member
            // — including the ones with no [Intrinsic] of their own, such as the constructors,
            // `ToString` and `CopyTo` — is routed to the intrinsic dispatcher and stops at the
            // unimplemented-intrinsic gate rather than running IL the scalar profile cannot
            // honour.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Runtime/Intrinsics/Vector256_1.cs#L78-L97
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Numerics/Vector_1.cs#L169-L183
            pattern "System.Private.CoreLib" "System.Runtime.Intrinsics.Vector64`1" "get_IsSupported" []
            |> reviewed [ "b818f682686492ce" ]
            pattern "System.Private.CoreLib" "System.Runtime.Intrinsics.Vector128`1" "get_IsSupported" []
            |> reviewed [ "b818f682686492ce" ]
            pattern "System.Private.CoreLib" "System.Runtime.Intrinsics.Vector256`1" "get_IsSupported" []
            |> reviewed [ "b818f682686492ce" ]
            pattern "System.Private.CoreLib" "System.Runtime.Intrinsics.Vector512`1" "get_IsSupported" []
            |> reviewed [ "b818f682686492ce" ]
            pattern "System.Private.CoreLib" "System.Numerics.Vector`1" "get_IsSupported" []
            |> reviewed [ "4914b11f5ed9017d" ]
            // `System.Int128` and `System.UInt128` each carry a *type-level* [Intrinsic], which
            // is why every one of their members reaches the intrinsic dispatcher. That marker is
            // not about body substitution: CoreCLR consumes it in MethodTableBuilder
            // (methodtablebuilder.cpp:11176, `SetIsIntrinsicType`) to give the type the ABI and
            // alignment of `__int128` — the fact `DeclaredTypeFacts.nominalAlignment` already
            // models — and this runtime's JIT carries no `NI_System_Int128_*` or
            // `NI_System_UInt128_*` entry at all. The shipped IL below is therefore what real
            // .NET runs, modulo ordinary inlining. Every body in this block is byte-identical
            // between the macOS-arm64 and the pinned linux-x64 CoreLib, so unlike the
            // `BitOperations` intrinsics above none of it needs a flavour-specific test.
            //
            // The two clusters are listed member by member rather than type by type, so that
            // the one place they genuinely differ — how many widening conversions each
            // declares — is visible as an asymmetry rather than buried in two long lists.
            //
            // op_Equality: `ldarg.0; ldfld _lower; ldarg.1; ldfld _lower; bne.un L; ldarg.0;
            // ldfld _upper; ldarg.1; ldfld _upper; ceq; ret; L: ldc.i4.0; ret`. Two `ldfld`s of a
            // `ulong` field on a by-value struct argument, then integer comparison — every
            // boundary is modelled. op_Inequality is that same body with `ldc.i4.0; ceq`
            // appended to negate the result. Both types spell both operators identically.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt128.cs#L1300
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Equality"
                [
                    IntrinsicParameterPattern.Exact "System.Int128"
                    IntrinsicParameterPattern.Exact "System.Int128"
                ]
            |> reviewed [ "4980b78af80f90ec" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Equality"
                [
                    IntrinsicParameterPattern.Exact "System.UInt128"
                    IntrinsicParameterPattern.Exact "System.UInt128"
                ]
            |> reviewed [ "293562e644a15db0" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.Int128"
                    IntrinsicParameterPattern.Exact "System.Int128"
                ]
            |> reviewed [ "539850b511a3a5f5" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Inequality"
                [
                    IntrinsicParameterPattern.Exact "System.UInt128"
                    IntrinsicParameterPattern.Exact "System.UInt128"
                ]
            |> reviewed [ "d1c3254cc99fa2bd" ]
            // The value-construction members equality is useless without, and which every one of
            // the entries below bottoms out in: `.ctor(ulong upper, ulong lower)` is
            // `ldarg.0; ldarg.2; stfld _lower; ldarg.0; ldarg.1; stfld _upper; ret`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Int128.cs#L37-L41
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                ".ctor"
                [
                    IntrinsicParameterPattern.Exact "System.UInt64"
                    IntrinsicParameterPattern.Exact "System.UInt64"
                ]
            |> reviewed [ "0d4253a7322a01db" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                ".ctor"
                [
                    IntrinsicParameterPattern.Exact "System.UInt64"
                    IntrinsicParameterPattern.Exact "System.UInt64"
                ]
            |> reviewed [ "6f81d735642fc930" ]
            // `UInt128.MinValue`/`MaxValue` are `ldc.i4.0/m1; conv.i8` twice, i.e. (0, 0) and
            // (ulong.MaxValue, ulong.MaxValue). `Int128` instead loads its upper half with
            // `ldc.i8` of long.MinValue/long.MaxValue and its lower half with `ldc.i4.0/m1;
            // conv.i8`, so its MinValue is the sign bit alone and is *not* the default value:
            // the one place where copying the UInt128 shape would give a wrong answer.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Int128.cs#L1123-L1126
            pattern "System.Private.CoreLib" "System.Int128" "get_MinValue" []
            |> reviewed [ "343a1a70c3ac9545" ]
            pattern "System.Private.CoreLib" "System.Int128" "get_MaxValue" []
            |> reviewed [ "8b85e5fe513caf83" ]
            pattern "System.Private.CoreLib" "System.UInt128" "get_MinValue" []
            |> reviewed [ "743ffbf8d808da86" ]
            pattern "System.Private.CoreLib" "System.UInt128" "get_MaxValue" []
            |> reviewed [ "bb8afd13f38ca5de" ]
            // The widening conversions. Both types take the same six from unsigned sources,
            // each `ldc.i4.0; conv.i8; ldarg.0; conv.u8; newobj .ctor; ret` — a zero upper half
            // and a zero-extended lower half, with the `UInt64` overload the one that needs no
            // `conv.u8`. They are enumerated rather than matched with `anyParams` so that an
            // overload added by a future runtime bump has to be reviewed rather than inherited.
            // Neither type declares an `op_Implicit` *from* itself; the narrowing direction is
            // `op_Explicit`, which is not allowlisted.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/UInt128.cs#L1712
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Byte" ]
            |> reviewed [ "32118c525130ae47" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Char" ]
            |> reviewed [ "bcbf62722924949c" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UInt16" ]
            |> reviewed [ "5cf55aedcda8db03" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "f134bf74f73d9ed4" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "2a786a78f874a9a7" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "c9051c8f25cb421d" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Byte" ]
            |> reviewed [ "c1eb63058f365259" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Char" ]
            |> reviewed [ "bc6fbc218459052a" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UInt16" ]
            |> reviewed [ "5216420741e63b45" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UInt32" ]
            |> reviewed [ "d08495cc5d22864b" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UInt64" ]
            |> reviewed [ "71787329954f86dd" ]
            pattern
                "System.Private.CoreLib"
                "System.UInt128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.UIntPtr" ]
            |> reviewed [ "bbada647f0a9d952" ]
            // `Int128` additionally takes five conversions from signed sources, which `UInt128`
            // cannot: it is those five that make the two clusters different sizes. Their body is
            // `ldarg.0; conv.i8; stloc.0; ldloc.0; ldc.i4.s 63; shr; ldloc.0; newobj .ctor; ret`
            // — widen to int64, then broadcast the sign bit across the upper half with an
            // *arithmetic* shift. (The `Int64` overload is the one that needs no `conv.i8`.)
            // That `shr` is the only place in either cluster where a signed and an unsigned
            // reading of the same bits diverge, so `Int128Conversions.cs` pairs each of these
            // with the unsigned source of the same width: `(Int128)(-1L)` must not equal
            // `(Int128)ulong.MaxValue`.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Int128.cs#L595-L640
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.SByte" ]
            |> reviewed [ "f57471e7cd0cba89" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Int16" ]
            |> reviewed [ "a248fbfc4055b51e" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Int32" ]
            |> reviewed [ "01a0955131c9bc5a" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.Int64" ]
            |> reviewed [ "2679e8eec3cb4714" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Implicit"
                [ IntrinsicParameterPattern.Exact "System.IntPtr" ]
            |> reviewed [ "590a0d20d04abc88" ]
            // The unchecked 128-bit addition, which `TimeSpan.FromMilliseconds(long, long)`
            // reaches through `Math.BigMul(long, long) + microseconds`. Its body is
            // `ldarg.0; ldfld _lower; ldarg.1; ldfld _lower; add; stloc.0; ldloc.0; ldarg.0;
            // ldfld _lower; clt.un; conv.i8; stloc.1; ldarg.0; ldfld _upper; ldarg.1;
            // ldfld _upper; add; ldloc.1; add; ldloc.0; newobj .ctor; ret` — a wrapping 64-bit
            // add of the low halves, a carry recovered from the wrap by the *unsigned* compare
            // `sum <u left._lower`, then a wrapping add of the high halves plus that carry. Both
            // `add`s are the unwidened wrapping opcode, not `add.ovf`: this is `op_Addition`, and
            // the operator C# emits under `checked` is the separate `op_CheckedAddition`, which
            // is not allowlisted. Every boundary is already modelled — `ldfld` of a `ulong` field
            // on a by-value struct argument as `op_Equality` does it, int64 `add` and `clt.un`,
            // and the `.ctor` allowlisted above.
            //
            // The signed `Int128` and the unsigned `UInt128` spell this body identically, since
            // two's complement addition does not depend on the reading; `UInt128.op_Addition` is
            // nonetheless left off until something exercises it.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Int128.cs#L667-L676
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_Addition"
                [
                    IntrinsicParameterPattern.Exact "System.Int128"
                    IntrinsicParameterPattern.Exact "System.Int128"
                ]
            |> reviewed [ "36c9ca96a86e7e42" ]
            // The two strict orderings, which `TimeSpan.FromMicroseconds(Int128)` reaches in its
            // bound check. `op_LessThan` is
            // `ldarg.0; ldfld _upper; ldarg.1; ldfld _upper; blt TRUE; ldarg.0; ldfld _upper;
            // ldarg.1; ldfld _upper; bne.un FALSE; ldarg.0; ldfld _lower; ldarg.1; ldfld _lower;
            // clt.un; ret; FALSE: ldc.i4.0; ret; TRUE: ldc.i4.1; ret`, and `op_GreaterThan` is
            // that with `bgt` and `cgt.un`. So the high halves are compared *signed* -- in two's
            // complement the high half carries the sign of the whole value -- and the low halves
            // *unsigned*, and the low halves are consulted only when the high halves are equal.
            // Every boundary is already modelled: `ldfld` of a `ulong` field on a by-value struct
            // argument, and int64 `blt`/`bgt`/`bne.un`/`clt.un`/`cgt.un`.
            //
            // `op_LessThanOrEqual` and `op_GreaterThanOrEqual` are the same body with the last
            // comparison inverted (`cgt.un; ldc.i4.0; ceq`) and the branch target shifted, but
            // nothing exercises them, so they are left off; `Int128Comparison.cs` therefore may
            // not spell `<=` or `>=` between two `Int128`s.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Int128.cs#L1008-L1036
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_LessThan"
                [
                    IntrinsicParameterPattern.Exact "System.Int128"
                    IntrinsicParameterPattern.Exact "System.Int128"
                ]
            |> reviewed [ "9f09012f5785ff06" ]
            pattern
                "System.Private.CoreLib"
                "System.Int128"
                "op_GreaterThan"
                [
                    IntrinsicParameterPattern.Exact "System.Int128"
                    IntrinsicParameterPattern.Exact "System.Int128"
                ]
            |> reviewed [ "4743e86893f8f724" ]
            // The narrowing conversion to `Int64`, which is how `TimeSpan.FromMicroseconds(Int128)`
            // gets back to ticks. Its whole body is `ldarg.0; ldfld _lower; ret` -- the low half
            // reinterpreted as signed, which costs no instruction -- and it discards the high half
            // without inspecting it, so it is `unchecked` by construction. The operator C# emits
            // under `checked` is the separate `op_CheckedExplicit`, which is not allowlisted.
            //
            // This is the one entry in the file that has to name its return type. `Int128` declares
            // sixteen `op_Explicit` overloads that all take a single `System.Int128` and differ only
            // in what they return, so without the discriminator this line would equally admit the
            // conversions to `double`, `single`, `System.Half`, `System.Decimal` and `System.UInt128`
            // -- four of which are real conversion algorithms rather than a field read, and none of
            // which has been reviewed. `patternReturning` is what makes the entry say only what was
            // reviewed.
            // https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Int128.cs#L295
            patternReturning
                "System.Private.CoreLib"
                "System.Int128"
                "op_Explicit"
                [ IntrinsicParameterPattern.Exact "System.Int128" ]
                (IntrinsicReturnPattern.Returns (IntrinsicParameterPattern.Exact "System.Int64"))
            |> reviewed [ "4e2f35014da46956" ]
        ]

    /// The fingerprints of the bodies reviewed for the method `key` names, across every row that
    /// names it; `None` when no row names it.
    let listedBodies (key : IntrinsicMethodKey) : IlBodyFingerprint list option =
        match safeIntrinsics |> List.filter (fun row -> methodPatternMatches row.Pattern key) with
        | [] -> None
        | rows -> rows |> List.collect _.ReviewedBodies |> List.distinct |> Some

    /// Whether some row names the method `key` identifies, whatever its body.
    let isListed (key : IntrinsicMethodKey) : bool = (listedBodies key).IsSome

    /// Whether the gate may interpret an `[Intrinsic]` method's IL as it stands.
    [<RequireQualifiedAccess>]
    type SafeIntrinsicVerdict =
        /// No row names the method.
        | NotListed
        /// A row names the method, and its body is one that row's review covered.
        | Reviewed
        /// A row names the method, but its body is not one that row's review covered.
        | UnreviewedBody of found : IlBodyFingerprint * reviewed : IlBodyFingerprint list
        /// A row names the method, but it has no IL body, so there is nothing a review could have
        /// covered.
        | ListedWithoutIlBody

    /// The verdict for a method whose rows list `listed` (`None`: no row names it) and whose own
    /// body has fingerprint `found` (`None`: it has no IL body).
    let verdict (listed : IlBodyFingerprint list option) (found : IlBodyFingerprint option) : SafeIntrinsicVerdict =
        match listed, found with
        | None, _ -> SafeIntrinsicVerdict.NotListed
        | Some _, None -> SafeIntrinsicVerdict.ListedWithoutIlBody
        | Some reviewed, Some found ->
            if List.contains found reviewed then
                SafeIntrinsicVerdict.Reviewed
            else
                SafeIntrinsicVerdict.UnreviewedBody (found, reviewed)

    /// The fingerprint of `methodToCall`'s own IL body, memoised on the state.
    let private bodyFingerprint
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IlMachineState * IlBodyFingerprint option
        =
        match methodToCall with
        | MethodInfo.Synthesised _ -> state, None
        | MethodInfo.Metadata (_, facts) ->
            let memoKey =
                methodToCall.DeclaringAssemblyFullName, ComparableMethodDefinitionHandle.Make facts.Handle

            match Map.tryFind memoKey state._IlBodyFingerprints with
            | Some fingerprint -> state, Some fingerprint
            | None ->
                let assembly =
                    match state.LoadedAssembly methodToCall.DeclaringAssemblyFullName with
                    | Some assembly -> assembly
                    | None ->
                        failwith
                            $"IL body fingerprint requested for a method whose declaring assembly is not loaded: %O{methodToCall}"

                match IlBodyFingerprint.ofMethod assembly assembly.Methods.[facts.Handle] with
                | Some fingerprint -> state.WithIlBodyFingerprint memoKey fingerprint, Some fingerprint
                | None -> state, None

    /// The gate's verdict on `methodToCall`, whose key is `key`. Fingerprints the body only when a
    /// row names the method.
    let safeIntrinsicVerdict
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (key : IntrinsicMethodKey)
        : IlMachineState * SafeIntrinsicVerdict
        =
        match listedBodies key with
        | None -> state, SafeIntrinsicVerdict.NotListed
        | Some reviewed ->
            let state, found = bodyFingerprint state methodToCall
            state, verdict (Some reviewed) found

    /// One `[Intrinsic]` method with an IL body that a row names in an image, and whether its body
    /// is one that row reviewed.
    type IntrinsicRowAudit =
        {
            /// The row's position in the table, counting from zero.
            RowIndex : int
            /// The row, as its pattern reads.
            Row : string
            /// The method named, as `Namespace.Type::Name(parameters) : return` over its definition.
            Method : string
            Fingerprint : IlBodyFingerprint
            Reviewed : bool
        }

    let private describePattern (pattern : IntrinsicMethodPattern) : string =
        let parameters =
            match pattern.ParameterPatterns with
            | None -> "*"
            | Some patterns -> patterns |> List.map string<IntrinsicParameterPattern> |> String.concat ", "

        let returns =
            match pattern.ReturnPattern with
            | None -> ""
            | Some returns -> $" : %O{returns}"

        $"%s{pattern.AssemblyName} %s{pattern.DeclaringTypeFullName}::%s{pattern.MethodName}(%s{parameters})%s{returns}"

    /// The shape a key would give a parameter or return of this declared type, or `ValueNone` for a
    /// generic parameter, whose shape depends on the instantiation.
    let rec private definitionShape (assembly : DumpedAssembly) (typeDefn : TypeDefn) : string voption =
        let qualified (ns : string) (name : string) : string =
            if String.IsNullOrEmpty ns then name else $"%s{ns}.%s{name}"

        match typeDefn with
        | TypeDefn.PrimitiveType primitive ->
            match primitive with
            | PrimitiveType.Boolean -> "System.Boolean"
            | PrimitiveType.Char -> "System.Char"
            | PrimitiveType.SByte -> "System.SByte"
            | PrimitiveType.Byte -> "System.Byte"
            | PrimitiveType.Int16 -> "System.Int16"
            | PrimitiveType.UInt16 -> "System.UInt16"
            | PrimitiveType.Int32 -> "System.Int32"
            | PrimitiveType.UInt32 -> "System.UInt32"
            | PrimitiveType.Int64 -> "System.Int64"
            | PrimitiveType.UInt64 -> "System.UInt64"
            | PrimitiveType.Single -> "System.Single"
            | PrimitiveType.Double -> "System.Double"
            | PrimitiveType.String -> "System.String"
            | PrimitiveType.TypedReference -> "System.TypedReference"
            | PrimitiveType.IntPtr -> "System.IntPtr"
            | PrimitiveType.UIntPtr -> "System.UIntPtr"
            | PrimitiveType.Object -> "System.Object"
            |> ValueSome
        | TypeDefn.Byref _ -> ValueSome "&"
        | TypeDefn.Pointer _ -> ValueSome "*"
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> ValueSome "[]"
        | TypeDefn.Array (_, rank) -> ValueSome $"[%i{rank}]"
        | TypeDefn.FunctionPointer _ -> ValueSome "fnptr"
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> ValueNone
        | TypeDefn.GenericInstantiation (generic, _) -> definitionShape assembly generic
        | TypeDefn.Modified modified -> definitionShape assembly modified.Unmodified
        | TypeDefn.Pinned inner -> definitionShape assembly inner
        | TypeDefn.FromReference (typeRef, _) -> ValueSome (qualified typeRef.Namespace typeRef.Name)
        | TypeDefn.FromDefinition (identity, _) ->
            match assembly.TypeDefs.TryGetValue identity.TypeDefinition.Get with
            | true, typeInfo when assembly.DefinitionFullName = identity.AssemblyFullName ->
                ValueSome (qualified typeInfo.Namespace typeInfo.Name)
            | _ -> failwith $"IntrinsicMethodKeys.audit: %O{identity} is not defined in %s{assembly.DefinitionFullName}"
        | TypeDefn.Void -> ValueSome "System.Void"

    let private definitionParameterMatches (pattern : IntrinsicParameterPattern) (shape : string voption) : bool =
        match shape with
        | ValueNone -> true
        | ValueSome actual -> parameterPatternMatches pattern actual

    /// Whether some instantiation of a definition with these shapes could produce a key `pattern`
    /// matches.
    let private patternNamesDefinition
        (pattern : IntrinsicMethodPattern)
        (assemblyFullName : string)
        (declaringTypeFullName : string)
        (methodName : string)
        (parameterShapes : string voption list)
        (returnShape : MethodReturnType<string voption>)
        : bool
        =
        AssemblyDefinitionName.isNamed pattern.AssemblyName assemblyFullName
        && pattern.DeclaringTypeFullName = declaringTypeFullName
        && pattern.MethodName = methodName
        && (
            match pattern.ParameterPatterns with
            | None -> true
            | Some patterns ->
                List.length patterns = List.length parameterShapes
                && List.forall2 definitionParameterMatches patterns parameterShapes
        )
        && match pattern.ReturnPattern, returnShape with
           | None, _ -> true
           | Some IntrinsicReturnPattern.Void, MethodReturnType.Void -> true
           | Some (IntrinsicReturnPattern.Returns pattern), MethodReturnType.Returns shape ->
               definitionParameterMatches pattern shape
           | Some IntrinsicReturnPattern.Void, MethodReturnType.Returns _
           | Some (IntrinsicReturnPattern.Returns _), MethodReturnType.Void -> false

    /// Every `[Intrinsic]` method with an IL body in `assembly` that some row names — taking a
    /// generic parameter to match whatever a row asks of it, since every instantiation shares the
    /// one body — with its fingerprint and whether the naming row reviewed it. A method several
    /// rows name appears once per row.
    ///
    /// For checking the table against an image: every entry reviewed means the gate will
    /// interpret every body the table names there.
    let audit (assembly : DumpedAssembly) : IntrinsicRowAudit list =
        let getMemberRefParentType (handle : System.Reflection.Metadata.MemberReferenceHandle) : TypeRef =
            match assembly.Members.[handle].Parent with
            | MetadataToken.TypeReference r -> assembly.TypeRefs.[r]
            | other -> failwith $"IntrinsicMethodKeys.audit: attribute constructor parent %O{other} is not a TypeRef"

        let rows = List.indexed safeIntrinsics

        [
            for KeyValue (_, definition) in assembly.Methods do
                match IlBodyFingerprint.ofMethod assembly definition with
                | None -> ()
                | Some fingerprint ->
                    let declaringType =
                        assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

                    let isIntrinsic =
                        MethodInfo.isJITIntrinsic getMemberRefParentType assembly.Methods definition
                        || MethodInfo.hasIntrinsicAttribute
                            getMemberRefParentType
                            assembly.Methods
                            declaringType.Attributes

                    if isIntrinsic then
                        let declaringTypeFullName =
                            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) declaringType

                        let parameterShapes =
                            definition.Signature.ParameterTypes |> List.map (definitionShape assembly)

                        let returnShape =
                            match definition.Signature.ReturnType with
                            | MethodReturnType.Void -> MethodReturnType.Void
                            | MethodReturnType.Returns returns ->
                                MethodReturnType.Returns (definitionShape assembly returns)

                        let scope = GenericScope.ofMethod definition

                        let methodText =
                            let parameters =
                                definition.Signature.ParameterTypes
                                |> List.map (IlFormatting.renderTypeDefn assembly scope)
                                |> String.concat ", "

                            let returns =
                                IlFormatting.renderMethodReturnType assembly scope definition.Signature.ReturnType

                            $"%s{declaringTypeFullName}::%s{definition.Name}(%s{parameters}) : %s{returns}"

                        for index, row in rows do
                            if
                                patternNamesDefinition
                                    row.Pattern
                                    assembly.DefinitionFullName
                                    declaringTypeFullName
                                    definition.Name
                                    parameterShapes
                                    returnShape
                            then
                                yield
                                    {
                                        RowIndex = index
                                        Row = describePattern row.Pattern
                                        Method = methodText
                                        Fingerprint = fingerprint
                                        Reviewed = List.contains fingerprint row.ReviewedBodies
                                    }
        ]
