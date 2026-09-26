namespace WoofWare.PawPrint

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

    /// The enclosing type's token in the same module, or None for a top-level type.
    let private enclosingOf (operation : string) (nominal : Nominal) : Nominal option =
        match nominal.Token with
        | NominalToken.Reference typeRef ->
            match typeRef.ResolutionScope with
            | TypeRefResolutionScope.TypeRef enclosing ->
                match nominal.Module.TypeRefs.TryGetValue enclosing with
                | true, enclosing ->
                    Some
                        { nominal with
                            Token = NominalToken.Reference enclosing
                        }
                | false, _ ->
                    failwith
                        $"%s{operation}: type reference %s{typeRef.Name} in %s{nominal.Module.DefinitionFullName} is scoped to a TypeRef row that assembly does not contain"
            | TypeRefResolutionScope.Assembly _
            | TypeRefResolutionScope.ModuleDef _
            | TypeRefResolutionScope.ModuleRef _ -> None
        | NominalToken.Definition typeDef ->
            if typeDef.IsNested then
                match nominal.Module.TypeDefs.TryGetValue typeDef.DeclaringType with
                | true, enclosing ->
                    Some
                        { nominal with
                            Token = NominalToken.Definition enclosing
                        }
                | false, _ ->
                    failwith
                        $"%s{operation}: type definition %s{typeDef.Name} in %s{nominal.Module.DefinitionFullName} names an enclosing type that assembly does not contain"
            else
                None

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
    /// for that and `CompareTypeTokens` answers FALSE, so the miss is handed back for the caller
    /// to answer the same way. An assembly that cannot be bound at all fails loudly in the loader,
    /// as CoreCLR throws from its own.
    let private resolveNominal
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (assemblies : LoadedAssemblies)
        (nominal : Nominal)
        : LoadedAssemblies * Result<ResolvedTypeIdentity, TypeResolutionMiss>
        =
        match nominal.Token with
        | NominalToken.Definition typeDef -> assemblies, Ok typeDef.Identity
        | NominalToken.Reference typeRef ->
            TypeResolution.resolveTypeRefIdentity loggerFactory dotnetRuntimeDirs nominal.Module typeRef assemblies

    /// `CompareTypeTokens` (siginfo.cpp), step for step: the same row of one module, then names,
    /// then the enclosing types by the same steps, then each side resolved in turn. The order is
    /// the point, not a saving: it decides which assemblies are loaded, or fail to bind, before an
    /// answer is reached, and PawPrint fails loudly where CoreCLR throws.
    let rec private compareNominal
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (operation : string)
        (depth : int)
        (assemblies : LoadedAssemblies)
        (left : Nominal)
        (right : Nominal)
        : LoadedAssemblies * bool
        =
        if depth > maxNestingDepth then
            failwith
                $"%s{operation}: %O{nameOf left} in %s{left.Module.DefinitionFullName} is nested more than %d{maxNestingDepth} deep; its enclosing chain is cyclic"

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
            assemblies, true
        | NominalToken.Definition lDef, NominalToken.Definition rDef ->
            // Two definitions are the same type only if they are the same definition. CoreCLR has
            // one escape from that — CLR type equivalence, which makes separately embedded
            // `[TypeIdentifier]` interop types compare equal — but `CompareTypeTokens` reaches it
            // only under `FEATURE_TYPEEQUIVALENCE`, and `clrfeatures.cmake` sets that solely for
            // `CLR_CMAKE_TARGET_WIN32`. Everywhere else the arm reads `return FALSE`, commented
            // "two type defs can't be the same unless they are identical". PawPrint models a
            // Linux guest (`SimulatedUnixPlatform`), so equivalence is not a behaviour it should
            // reproduce; a build for a Windows guest would have to revisit it.
            assemblies, lDef.Identity = rDef.Identity
        | _, _ ->

        if nameOf left <> nameOf right then
            assemblies, false
        else

        // A nested type's row carries an empty namespace and only its own leaf name, so the
        // enclosing types are what tell `A+X` from `B+X` — compared by these same steps, so their
        // resolution happens, on both sides, before either leaf's.
        let assemblies, enclosingMatch =
            match enclosingOf operation left, enclosingOf operation right with
            | Some lEnclosing, Some rEnclosing ->
                compareNominal loggerFactory dotnetRuntimeDirs operation (depth + 1) assemblies lEnclosing rEnclosing
            | None, None -> assemblies, true
            | Some _, None
            | None, Some _ -> assemblies, false

        if not enclosingMatch then
            assemblies, false
        else

        let assemblies, lIdentity =
            resolveNominal loggerFactory dotnetRuntimeDirs assemblies left

        match lIdentity with
        | Error _ -> assemblies, false
        | Ok lIdentity ->

        let assemblies, rIdentity =
            resolveNominal loggerFactory dotnetRuntimeDirs assemblies right

        match rIdentity with
        | Error _ -> assemblies, false
        | Ok rIdentity -> assemblies, lIdentity = rIdentity

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
        : LoadedAssemblies * bool
        =
        let recurse assemblies l r =
            compareSignatureTypes loggerFactory dotnetRuntimeDirs operation assemblies leftAssembly l rightAssembly r

        match left, right with
        | TypeDefn.PrimitiveType l, TypeDefn.PrimitiveType r -> assemblies, l = r
        | TypeDefn.Void, TypeDefn.Void -> assemblies, true
        // Symbolic, as CoreCLR is: with a null Substitution the VAR/MVAR arms of
        // `CompareElementType` compare the index and nothing else. This is what stops a type
        // parameter comparing equal to whatever an instantiation would substitute for it.
        | TypeDefn.GenericTypeParameter l, TypeDefn.GenericTypeParameter r -> assemblies, l = r
        | TypeDefn.GenericMethodParameter l, TypeDefn.GenericMethodParameter r -> assemblies, l = r
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
            let assemblies, elementsMatch = recurse assemblies lElt rElt

            if not elementsMatch then
                assemblies, false
            else
                assemblies, lRank = rRank
        | TypeDefn.Modified l, TypeDefn.Modified r ->
            // A modreq never equals a modopt, and the modifier itself is compared nominally.
            // Nesting order is preserved by the decoder, so recursing through `Unmodified`
            // compares multiple modifiers pairwise in order, as `CompareElementType`'s `goto redo`
            // does.
            if l.IsRequired <> r.IsRequired then
                assemblies, false
            else
                let assemblies, modifiersMatch = recurse assemblies l.Modifier r.Modifier

                if not modifiersMatch then
                    assemblies, false
                else
                    recurse assemblies l.Unmodified r.Unmodified
        | TypeDefn.GenericInstantiation (lGeneric, lArgs), TypeDefn.GenericInstantiation (rGeneric, rArgs) ->
            // The generic definition before the argument count, as `CompareElementType` reads
            // the blob, for the same reason as the array arm above. Well-formed metadata cannot
            // reach a count mismatch with one definition, since the arity is part of its name,
            // but the order is kept so that a malformed pair loads what CoreCLR would load.
            let assemblies, genericMatches = recurse assemblies lGeneric rGeneric

            if not genericMatches then
                assemblies, false
            elif lArgs.Length <> rArgs.Length then
                assemblies, false
            else
                ((assemblies, true), Seq.zip lArgs rArgs)
                ||> Seq.fold (fun (assemblies, soFar) (l, r) ->
                    if not soFar then
                        (assemblies, false)
                    else
                        recurse assemblies l r
                )
        | TypeDefn.FunctionPointer l, TypeDefn.FunctionPointer r ->
            if l.Header.Get.RawValue <> r.Header.Get.RawValue then
                assemblies, false
            elif l.GenericParameterCount <> r.GenericParameterCount then
                // CoreCLR reads one compressed integer from each blob after the calling-convention
                // bytes and compares it as `argCnt` (siginfo.cpp, the FNPTR arm). For a GENERIC
                // signature that integer is the generic-parameter count, since the blob spells
                // CallConv | GenParamCount | ParamCount | RetType | Params — so differing counts are
                // rejected there, before any element is parsed. For a non-GENERIC signature the count
                // is zero on both sides and this branch cannot fire.
                assemblies, false
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
                failwith
                    $"%s{operation}: comparing two function pointer signatures that spell the same GENERIC calling convention and the same generic-parameter count (in %s{leftAssembly.DefinitionFullName} against %s{rightAssembly.DefinitionFullName}); from here CoreCLR compares elements read at a one-integer offset into each blob, reinterpreting the parameter-count byte as an element type, which cannot be reproduced from a decoded signature"
            elif List.length l.ParameterTypes <> List.length r.ParameterTypes then
                // For a non-GENERIC signature this *is* CoreCLR's `argCnt` comparison: the integer it
                // reads after the calling-convention byte is the parameter count. A GENERIC one never
                // reaches here, having been refused above.
                assemblies, false
            else
                let assemblies, returnMatches =
                    match l.ReturnType, r.ReturnType with
                    | MethodReturnType.Void, MethodReturnType.Void -> assemblies, true
                    | MethodReturnType.Returns l, MethodReturnType.Returns r -> recurse assemblies l r
                    | _, _ -> assemblies, false

                if not returnMatches then
                    assemblies, false
                else
                    ((assemblies, true), List.zip l.ParameterTypes r.ParameterTypes)
                    ||> List.fold (fun (assemblies, soFar) (l, r) ->
                        if not soFar then
                            (assemblies, false)
                        else
                            recurse assemblies l r
                    )
        | (TypeDefn.FromDefinition (_, lKind) | TypeDefn.FromReference (_, lKind)),
          (TypeDefn.FromDefinition (_, rKind) | TypeDefn.FromReference (_, rKind)) ->
            // CoreCLR fails a CLASS against a VALUETYPE on the element-type byte alone, before it
            // reads a token, so two spellings of one type with different kinds are unequal.
            if lKind <> rKind then
                assemblies, false
            else
                compareNominal
                    loggerFactory
                    dotnetRuntimeDirs
                    operation
                    0
                    assemblies
                    (nominalOfLeaf operation assemblies leftAssembly left)
                    (nominalOfLeaf operation assemblies rightAssembly right)
        | _, _ -> assemblies, false

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
        : LoadedAssemblies * bool
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
            assemblies, false
        elif left.GenericParameterCount <> right.GenericParameterCount then
            assemblies, false
        elif left.ParameterTypes.Length <> right.ParameterTypes.Length then
            assemblies, false
        else

        let assemblies, returnMatches =
            compareSignatureTypes
                loggerFactory
                dotnetRuntimeDirs
                operation
                assemblies
                leftAssembly
                left.ReturnType
                rightAssembly
                right.ReturnType

        if not returnMatches then
            assemblies, false
        else

        ((assemblies, true), Seq.zip left.ParameterTypes right.ParameterTypes)
        ||> Seq.fold (fun (assemblies, soFar) (l, r) ->
            if not soFar then
                (assemblies, false)
            else
                compareSignatureTypes
                    loggerFactory
                    dotnetRuntimeDirs
                    operation
                    assemblies
                    leftAssembly
                    l
                    rightAssembly
                    r
        )

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
        : LoadedAssemblies * bool
        =
        if
            leftAssembly.DefinitionFullName = rightAssembly.DefinitionFullName
            && leftBytes = rightBytes
        then
            assemblies, true
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
