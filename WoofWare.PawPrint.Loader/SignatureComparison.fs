namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// Signature equality as CoreCLR's <c>MetaSig::CompareMethodSigs</c> decides it with both
/// <c>Substitution</c>s null, which is the comparison the <c>Signature_AreEqual</c> QCall
/// performs: structural, <i>symbolic</i> in generic parameters (a <c>VAR</c> equals only the
/// same-indexed <c>VAR</c>, never the type an instantiation would put there), nominal in type
/// references, and sensitive to custom modifiers.
/// </summary>
/// <remarks>
/// Everything here works from decoded signatures and <c>LoadedAssemblies</c> alone. Following a
/// type reference can load the assembly it names, which is why the assemblies are threaded through
/// and handed back; nothing here needs the machine state, so a consumer that never executes IL
/// can ask the same question.
/// </remarks>
[<RequireQualifiedAccess>]
module SignatureComparison =

    /// A metadata nesting depth no real image reaches. The enclosing walks below follow links that
    /// malformed metadata could make cyclic, and a cycle would otherwise hang the interpreter.
    let private maxNestingDepth : int = 256

    /// One nominal type as `CompareTypeTokens` sees it: a token in a module. A reference is a row
    /// of the module that spells it; a definition is a row of the module that declares it.
    [<RequireQualifiedAccess>]
    type private NominalToken =
        | Reference of TypeRef
        | Definition of TypeInfo<GenericParamFromMetadata, TypeDefn>

    type private Nominal =
        {
            Module : DumpedAssembly
            Token : NominalToken
        }

    /// The nominal leaf of a decoded signature, as a token in the module that holds it. For a
    /// reference that is the module the blob came from; a definition is looked up in its own.
    let private nominalOfLeaf
        (operation : string)
        (assemblies : LoadedAssemblies)
        (blobModule : DumpedAssembly)
        (defn : TypeDefn)
        : Nominal
        =
        match defn with
        | TypeDefn.FromReference (typeRef, _) ->
            {
                Module = blobModule
                Token = NominalToken.Reference typeRef
            }
        | TypeDefn.FromDefinition (identity, _) ->
            let definingAssembly =
                assemblies.TryByDefinitionName identity.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: type definition %O{identity.TypeDefinition.Get} names unloaded assembly %s{identity.AssemblyFullName}"
                )

            {
                Module = definingAssembly
                Token = NominalToken.Definition definingAssembly.TypeDefs.[identity.TypeDefinition.Get]
            }
        | other -> failwith $"%s{operation}: %O{other} is not a nominal type reference"

    /// The namespace and name the token's own row carries. A nested type's row has an empty
    /// namespace and only its own leaf name, with its enclosing type reachable separately.
    let private nameOf (nominal : Nominal) : string * string =
        match nominal.Token with
        | NominalToken.Reference typeRef -> typeRef.Namespace, typeRef.Name
        | NominalToken.Definition typeDef -> typeDef.Namespace, typeDef.Name

    /// A failure to bind that PawPrint does not model, with what was being done when it arose.
    let private unmodelled (operation : string) (what : string) : Result<'a, TypeBindingFailure> =
        Error (
            TypeBindingFailure.Unmodelled
                {
                    Diagnostic = $"%s{operation}: %s{what}"
                }
        )

    /// The enclosing type's token in the same module, or None for a top-level type. Metadata whose
    /// enclosing row is not in the module is unmodelled.
    let private enclosingOf (operation : string) (nominal : Nominal) : Result<Nominal option, TypeBindingFailure> =
        match nominal.Token with
        | NominalToken.Reference typeRef ->
            match typeRef.ResolutionScope with
            | TypeRefResolutionScope.TypeRef enclosing ->
                match nominal.Module.TypeRefs.TryGetValue enclosing with
                | true, enclosing ->
                    Ok (
                        Some
                            { nominal with
                                Token = NominalToken.Reference enclosing
                            }
                    )
                | false, _ ->
                    unmodelled
                        operation
                        $"type reference %s{typeRef.Name} in %s{nominal.Module.DefinitionFullName} is scoped to a TypeRef row that assembly does not contain"
            | TypeRefResolutionScope.Assembly _
            | TypeRefResolutionScope.ModuleDef _
            | TypeRefResolutionScope.ModuleRef _ -> Ok None
        | NominalToken.Definition typeDef ->
            if typeDef.IsNested then
                match nominal.Module.TypeDefs.TryGetValue typeDef.DeclaringType with
                | true, enclosing ->
                    Ok (
                        Some
                            { nominal with
                                Token = NominalToken.Definition enclosing
                            }
                    )
                | false, _ ->
                    unmodelled
                        operation
                        $"type definition %s{typeDef.Name} in %s{nominal.Module.DefinitionFullName} names an enclosing type that assembly does not contain"
            else
                Ok None

    /// Resolve a token to the TypeDef that defines it, which is the identity `CompareTypeTokens`
    /// ultimately compares. Following a reference can load the assembly it names, hence the
    /// assemblies in and out.
    ///
    /// Deliberately the identity-only resolver: this comparison never looks at the resolved type
    /// beyond its identity, and priming a base chain would let an assembly reachable only from
    /// some base type decide whether two signatures can be compared at all. CoreCLR's
    /// `ClassLoader::ResolveTokenToTypeDefThrowing` likewise reads metadata and loads no types.
    ///
    /// A miss is a bound assembly that does not declare the name. CoreCLR's resolver returns FALSE
    /// for that and `CompareTypeTokens` answers FALSE, so the caller answers the same way. An
    /// assembly that `loadAssembly` cannot bind at all is a failure, as CoreCLR throws from its own.
    let rec private resolveNominal
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (nominal : Nominal)
        : LoadedAssemblies * Result<ResolvedTypeIdentity option, TypeBindingFailure>
        =
        match nominal.Token with
        | NominalToken.Definition typeDef -> assemblies, Ok (Some typeDef.Identity)
        | NominalToken.Reference typeRef ->
            match LoadedTypeResolution.resolveTypeRef assemblies nominal.Module ImmutableArray.Empty typeRef with
            | TypeResolutionResult.Resolved (_, identity, _) -> assemblies, Ok (Some identity)
            | TypeResolutionResult.NotFound _ -> assemblies, Ok None
            | TypeResolutionResult.Unmodelled unmodelled -> assemblies, Error (TypeBindingFailure.Unmodelled unmodelled)
            | TypeResolutionResult.FirstLoadAssy loadFirst ->
                let handle, referencedIn = loadFirst.Handle

                match loadAssembly.TryLoadAssembly assemblies referencedIn handle with
                | Error (AssemblyLoadFailure.NoSuchAssembly reference) ->
                    assemblies, Error (TypeBindingFailure.AssemblyUnavailable reference)
                | Error (AssemblyLoadFailure.LoadingNotPermitted _ as failure) ->
                    failwith (string<AssemblyLoadFailure> failure)
                | Ok (assemblies, _) ->

                let assemblies =
                    LoadedAssemblies.assertReferenceBound $"type reference %s{typeRef.Name}" loadFirst assemblies

                resolveNominal loadAssembly assemblies nominal

    /// Whether `compare` finds each pair equal, in order, stopping at the first it does not or that
    /// fails to bind, as CoreCLR's comparison returns at the first difference.
    let private allEqual
        (compare : LoadedAssemblies -> 'a -> 'a -> LoadedAssemblies * Result<bool, TypeBindingFailure>)
        (assemblies : LoadedAssemblies)
        (pairs : ('a * 'a) seq)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        let mutable assemblies = assemblies
        let mutable answer = Ok true
        use pairs = pairs.GetEnumerator ()

        while answer = Ok true && pairs.MoveNext () do
            let l, r = pairs.Current
            let loaded, compared = compare assemblies l r
            assemblies <- loaded
            answer <- compared

        assemblies, answer

    /// `CompareTypeTokens` (siginfo.cpp), step for step: the same row of one module, then names,
    /// then the enclosing types by the same steps, then each side resolved in turn. The order is
    /// the point, not a saving: it decides which assemblies are loaded, or fail to bind, before an
    /// answer is reached, and a failure to bind is reported where CoreCLR throws.
    let rec private compareNominal
        (loadAssembly : IAssemblyLoad)
        (operation : string)
        (depth : int)
        (assemblies : LoadedAssemblies)
        (left : Nominal)
        (right : Nominal)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        if depth > maxNestingDepth then
            assemblies,
            unmodelled
                operation
                $"%O{nameOf left} in %s{left.Module.DefinitionFullName} is nested more than %d{maxNestingDepth} deep; its enclosing chain is cyclic"
        else

        match left.Token, right.Token with
        | NominalToken.Reference lRef, NominalToken.Reference rRef when
            left.Module.DefinitionFullName = right.Module.DefinitionFullName
            && lRef.Handle = rRef.Handle
            ->
            // `tk1 == tk2` within one module, answered before anything is resolved. That step
            // earns its place rather than merely saving work — `Signature_Init` strips custom
            // modifiers without loading their types, so two signatures can carry the same
            // modifier naming an assembly nothing has loaded, and resolving it would fail where
            // CoreCLR answers.
            //
            // The row, not the description: two rows of one module may describe the same type,
            // and CoreCLR does not take this shortcut for them. The module check is what makes
            // the comparison meaningful, since a handle indexes its own module's tables.
            assemblies, Ok true
        | NominalToken.Definition lDef, NominalToken.Definition rDef ->
            // Two definitions are the same type only if they are the same definition. CoreCLR has
            // one escape from that — CLR type equivalence, which makes separately embedded
            // `[TypeIdentifier]` interop types compare equal — but `CompareTypeTokens` reaches it
            // only under `FEATURE_TYPEEQUIVALENCE`, and `clrfeatures.cmake` sets that solely for
            // `CLR_CMAKE_TARGET_WIN32`. Everywhere else the arm reads `return FALSE`, commented
            // "two type defs can't be the same unless they are identical". PawPrint models a
            // Linux guest (`SimulatedUnixPlatform`), so equivalence is not a behaviour it should
            // reproduce; a build for a Windows guest would have to revisit it.
            assemblies, Ok (lDef.Identity = rDef.Identity)
        | _, _ ->

        if nameOf left <> nameOf right then
            assemblies, Ok false
        else

        // A nested type's row carries an empty namespace and only its own leaf name, so the
        // enclosing types are what tell `A+X` from `B+X` — compared by these same steps, so their
        // resolution happens, on both sides, before either leaf's.
        let assemblies, enclosingMatch =
            match enclosingOf operation left, enclosingOf operation right with
            | Error failure, _
            | _, Error failure -> assemblies, Error failure
            | Ok (Some lEnclosing), Ok (Some rEnclosing) ->
                compareNominal loadAssembly operation (depth + 1) assemblies lEnclosing rEnclosing
            | Ok None, Ok None -> assemblies, Ok true
            | Ok (Some _), Ok None
            | Ok None, Ok (Some _) -> assemblies, Ok false

        match enclosingMatch with
        | Error failure -> assemblies, Error failure
        | Ok false -> assemblies, Ok false
        | Ok true ->

        match resolveNominal loadAssembly assemblies left with
        | assemblies, Error failure -> assemblies, Error failure
        | assemblies, Ok None -> assemblies, Ok false
        | assemblies, Ok (Some lIdentity) ->

        match resolveNominal loadAssembly assemblies right with
        | assemblies, Error failure -> assemblies, Error failure
        | assemblies, Ok None -> assemblies, Ok false
        | assemblies, Ok (Some rIdentity) -> assemblies, Ok (lIdentity = rIdentity)

    /// `CompareTypeTokens` (siginfo.cpp) on two type references or definitions of signatures, `left`
    /// spelled in `leftModule` and `right` in `rightModule`, binding assemblies with `loadAssembly`:
    /// whether they name one type definition. It compares names before it resolves either, and a
    /// reference into an assembly that binds but does not declare the type names no type, so it is
    /// unequal to every other; one into an assembly `loadAssembly` cannot bind is a failure, where
    /// CoreCLR throws. The element types' kinds, class or value type, are not compared.
    let compareTypeTokens
        (loadAssembly : IAssemblyLoad)
        (operation : string)
        (assemblies : LoadedAssemblies)
        (leftModule : DumpedAssembly)
        (left : TypeDefn)
        (rightModule : DumpedAssembly)
        (right : TypeDefn)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        compareNominal
            loadAssembly
            operation
            0
            assemblies
            (nominalOfLeaf operation assemblies leftModule left)
            (nominalOfLeaf operation assemblies rightModule right)

    /// Compare two decoded signature types the way `MetaSig::CompareElementType` compares two
    /// blobs with both `Substitution`s null.
    ///
    /// Custom modifiers participate: `CompareState.IgnoreCustomModifiers` defaults to false
    /// (siginfo.hpp) and the `Signature_AreEqual` path never sets it, so `ref readonly int` and
    /// `ref int` — which differ only by a `modreq(InAttribute)` — are different signatures.
    let rec compareSignatureTypes
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (operation : string)
        (assemblies : LoadedAssemblies)
        (leftAssembly : DumpedAssembly)
        (left : TypeDefn)
        (rightAssembly : DumpedAssembly)
        (right : TypeDefn)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        let recurse assemblies l r =
            compareSignatureTypes loggerFactory dotnetRuntimeDirs operation assemblies leftAssembly l rightAssembly r

        // `first`, and if it finds them equal, `next`.
        let andThen
            (next : LoadedAssemblies -> LoadedAssemblies * Result<bool, TypeBindingFailure>)
            (assemblies, first)
            =
            match first with
            | Ok true -> next assemblies
            | Ok false
            | Error _ -> assemblies, first

        match left, right with
        | TypeDefn.PrimitiveType l, TypeDefn.PrimitiveType r -> assemblies, Ok (l = r)
        | TypeDefn.Void, TypeDefn.Void -> assemblies, Ok true
        // Symbolic, as CoreCLR is: with a null Substitution the VAR/MVAR arms of
        // `CompareElementType` compare the index and nothing else. This is what stops a type
        // parameter comparing equal to whatever an instantiation would substitute for it.
        | TypeDefn.GenericTypeParameter l, TypeDefn.GenericTypeParameter r -> assemblies, Ok (l = r)
        | TypeDefn.GenericMethodParameter l, TypeDefn.GenericMethodParameter r -> assemblies, Ok (l = r)
        | TypeDefn.Byref l, TypeDefn.Byref r
        | TypeDefn.Pointer l, TypeDefn.Pointer r
        | TypeDefn.Pinned l, TypeDefn.Pinned r
        | TypeDefn.OneDimensionalArrayLowerBoundZero l, TypeDefn.OneDimensionalArrayLowerBoundZero r ->
            recurse assemblies l r
        | TypeDefn.Array (lElt, lRank), TypeDefn.Array (rElt, rRank) ->
            // Element before rank, in the order `CompareElementType` reads the blob: the element is
            // resolved even when the ranks then differ, so the assemblies this loads, or fails to
            // bind, are the ones CoreCLR's comparison touches.
            //
            // Rank alone then decides the shape: the decoder refuses a non-canonical `ArrayShape`,
            // and canonical shapes of one rank carry identical sizes and lower bounds.
            recurse assemblies lElt rElt
            |> andThen (fun assemblies -> assemblies, Ok (lRank = rRank))
        | TypeDefn.Modified l, TypeDefn.Modified r ->
            // A modreq never equals a modopt, and the modifier itself is compared nominally.
            // Nesting order is preserved by the decoder, so recursing through `Unmodified`
            // compares multiple modifiers pairwise in order, as `CompareElementType`'s `goto redo`
            // does.
            if l.IsRequired <> r.IsRequired then
                assemblies, Ok false
            else
                recurse assemblies l.Modifier r.Modifier
                |> andThen (fun assemblies -> recurse assemblies l.Unmodified r.Unmodified)
        | TypeDefn.GenericInstantiation (lGeneric, lArgs), TypeDefn.GenericInstantiation (rGeneric, rArgs) ->
            // The generic definition before the argument count, as `CompareElementType` reads
            // the blob, for the same reason as the array arm above. Well-formed metadata cannot
            // reach a count mismatch with one definition, since the arity is part of its name,
            // but the order is kept so that a malformed pair loads what CoreCLR would load.
            recurse assemblies lGeneric rGeneric
            |> andThen (fun assemblies ->
                if lArgs.Length <> rArgs.Length then
                    assemblies, Ok false
                else
                    allEqual recurse assemblies (Seq.zip lArgs rArgs)
            )
        | TypeDefn.FunctionPointer l, TypeDefn.FunctionPointer r ->
            if l.Header.Get.RawValue <> r.Header.Get.RawValue then
                assemblies, Ok false
            elif l.GenericParameterCount <> r.GenericParameterCount then
                // CoreCLR reads one compressed integer from each blob after the calling-convention
                // bytes and compares it as `argCnt` (siginfo.cpp, the FNPTR arm). For a GENERIC
                // signature that integer is the generic-parameter count, since the blob spells
                // CallConv | GenParamCount | ParamCount | RetType | Params — so differing counts are
                // rejected there, before any element is parsed. For a non-GENERIC signature the count
                // is zero on both sides and this branch cannot fire.
                assemblies, Ok false
            elif l.Header.Get.IsGeneric then
                // The calling-convention bytes and the generic-parameter counts are equal by the
                // branches above, and those are the last two things CoreCLR decides without
                // reinterpreting a byte: both are integers read as integers
                // (`CorSigUncompressData_EndPtr`).
                //
                // Past that point it compares `GenParamCount + 1` elements of a stream misaligned by one
                // integer, so the *parameter count* byte is handed to `CompareElementType` as a
                // `CorElementType` — 0x01 read as ELEMENT_TYPE_VOID and compared as such, other values
                // able to fail the signature outright — and it answers from the real return type as
                // though it were a parameter, ignoring the rest. Which answer comes back is a fact about
                // the numeric count values reinterpreted as element types, which a correctly decoded
                // signature no longer knows. So refuse every pair that reaches here, including one whose
                // parameter counts differ, rather than trying to predict it.
                assemblies,
                unmodelled
                    operation
                    $"comparing two function pointer signatures that spell the same GENERIC calling convention and the same generic-parameter count (in %s{leftAssembly.DefinitionFullName} against %s{rightAssembly.DefinitionFullName}); from here CoreCLR compares elements read at a one-integer offset into each blob, reinterpreting the parameter-count byte as an element type, which cannot be reproduced from a decoded signature"
            elif List.length l.ParameterTypes <> List.length r.ParameterTypes then
                // For a non-GENERIC signature this *is* CoreCLR's `argCnt` comparison: the integer it
                // reads after the calling-convention byte is the parameter count. A GENERIC one never
                // reaches here, having been refused above.
                assemblies, Ok false
            else
                let returnsMatch =
                    match l.ReturnType, r.ReturnType with
                    | MethodReturnType.Void, MethodReturnType.Void -> assemblies, Ok true
                    | MethodReturnType.Returns lReturn, MethodReturnType.Returns rReturn ->
                        recurse assemblies lReturn rReturn
                    | _, _ -> assemblies, Ok false

                returnsMatch
                |> andThen (fun assemblies -> allEqual recurse assemblies (List.zip l.ParameterTypes r.ParameterTypes))
        | (TypeDefn.FromDefinition (_, lKind) | TypeDefn.FromReference (_, lKind)),
          (TypeDefn.FromDefinition (_, rKind) | TypeDefn.FromReference (_, rKind)) ->
            // CoreCLR fails a CLASS against a VALUETYPE on the element-type byte alone, before it
            // reads a token, so two spellings of one type with different kinds are unequal.
            if lKind <> rKind then
                assemblies, Ok false
            else
                compareNominal
                    (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                    operation
                    0
                    assemblies
                    (nominalOfLeaf operation assemblies leftAssembly left)
                    (nominalOfLeaf operation assemblies rightAssembly right)
        | _, _ -> assemblies, Ok false

    /// Compare two decoded signatures as `MetaSig::CompareMethodSigs` does once its same-module
    /// byte-equality fast path has failed: calling convention, then argument count, then the
    /// return type and each parameter in order.
    let compareDecodedSignatures
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (operation : string)
        (assemblies : LoadedAssemblies)
        (leftAssembly : DumpedAssembly)
        (left : MethodSignature<TypeDefn>)
        (rightAssembly : DumpedAssembly)
        (right : MethodSignature<TypeDefn>)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        // CoreCLR reads one argument count from the blob and uses it for both the comparison and
        // the walk. Only a call site's MemberRef can carry a VARARG sentinel, so for the signatures
        // that reach here the decoded parameter list is always exactly that long. Assert it rather
        // than branch on it: the fold below zips the two lists, which would silently compare a
        // prefix if a count and its list ever disagreed.
        if left.RequiredParameterCount <> left.ParameterTypes.Length then
            failwith
                $"%s{operation}: the first signature declares %d{left.RequiredParameterCount} parameters but decoded %d{left.ParameterTypes.Length} of them; a signature reaching this comparison was not expected to carry a VARARG sentinel"

        if right.RequiredParameterCount <> right.ParameterTypes.Length then
            failwith
                $"%s{operation}: the second signature declares %d{right.RequiredParameterCount} parameters but decoded %d{right.ParameterTypes.Length} of them; a signature reaching this comparison was not expected to carry a VARARG sentinel"

        // CoreCLR masks out `CORINFO_CALLCONV_PARAMTYPE`, a bit the JIT sets on internal
        // signatures; it cannot appear in a signature read from PE metadata, so the raw bytes are
        // compared. This is what makes a static property differ from an instance one (HASTHIS) and
        // a PROPERTY blob differ from a METHOD one.
        if left.Header.RawValue <> right.Header.RawValue then
            assemblies, Ok false
        elif left.GenericParameterCount <> right.GenericParameterCount then
            assemblies, Ok false
        elif left.ParameterTypes.Length <> right.ParameterTypes.Length then
            assemblies, Ok false
        else

        let compare assemblies l r =
            compareSignatureTypes loggerFactory dotnetRuntimeDirs operation assemblies leftAssembly l rightAssembly r

        // The return type first, then each parameter, as `MetaSig::CompareMethodSigs` walks them.
        allEqual
            compare
            assemblies
            (Seq.append [ left.ReturnType, right.ReturnType ] (Seq.zip left.ParameterTypes right.ParameterTypes))

    /// Compare two signatures exactly as `Signature_AreEqual` does: CoreCLR's same-module
    /// byte-equality fast path, then the structural comparison.
    ///
    /// The fast path must stay gated on the assembly. A signature blob's type tokens are indices
    /// into its *own* module's tables, so two byte-identical blobs in different assemblies can name
    /// entirely different types — which is exactly what happens when two assemblies of the same
    /// shape each declare a property of their own local type.
    let signaturesAreEqual
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (operation : string)
        (assemblies : LoadedAssemblies)
        (leftAssembly : DumpedAssembly)
        (leftBytes : byte[])
        (left : MethodSignature<TypeDefn>)
        (rightAssembly : DumpedAssembly)
        (rightBytes : byte[])
        (right : MethodSignature<TypeDefn>)
        : LoadedAssemblies * Result<bool, TypeBindingFailure>
        =
        if
            leftAssembly.DefinitionFullName = rightAssembly.DefinitionFullName
            && leftBytes = rightBytes
        then
            assemblies, Ok true
        else
            compareDecodedSignatures
                loggerFactory
                dotnetRuntimeDirs
                operation
                assemblies
                leftAssembly
                left
                rightAssembly
                right
