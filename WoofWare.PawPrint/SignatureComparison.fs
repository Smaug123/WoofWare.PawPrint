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

    /// The name a nominal `TypeDefn` leaf carries in its *own* metadata, outermost enclosing type
    /// first, without resolving it. `CompareTypeTokens` (siginfo.cpp) compares these strings before
    /// it resolves anything, so a comparison that resolved eagerly would fail on images CoreCLR
    /// answers cleanly.
    let private nominalName
        (operation : string)
        (assemblies : LoadedAssemblies)
        (assembly : DumpedAssembly)
        (defn : TypeDefn)
        : (string * string) list option
        =
        match defn with
        | TypeDefn.FromReference (typeRef, _) ->
            // A nested type's row carries an empty namespace and only its own leaf name, with its
            // enclosing type in the resolution scope, so the leaf alone does not tell `A+X` from
            // `B+X`. Walk out to the enclosing type that names an assembly or module, innermost
            // first, then reverse.
            let rec walk (depth : int) (acc : (string * string) list) (current : TypeRef) =
                if depth > maxNestingDepth then
                    failwith
                        $"%s{operation}: type reference %s{current.Name} in %s{assembly.Name.FullName} is nested more than %d{maxNestingDepth} deep; its enclosing chain is cyclic"

                let acc = (current.Namespace, current.Name) :: acc

                match current.ResolutionScope with
                | TypeRefResolutionScope.TypeRef enclosing ->
                    match assembly.TypeRefs.TryGetValue enclosing with
                    | true, enclosing -> walk (depth + 1) acc enclosing
                    | false, _ ->
                        failwith
                            $"%s{operation}: type reference %s{current.Name} in %s{assembly.Name.FullName} is scoped to a TypeRef row that assembly does not contain"
                | TypeRefResolutionScope.Assembly _
                | TypeRefResolutionScope.ModuleDef _
                | TypeRefResolutionScope.ModuleRef _ -> acc

            Some (walk 0 [] typeRef)
        | TypeDefn.FromDefinition (identity, _) ->
            let definingAssembly =
                assemblies.TryByDefinitionName identity.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: type definition %O{identity.TypeDefinition.Get} names unloaded assembly %s{identity.AssemblyFullName}"
                )

            let rec walk
                (depth : int)
                (acc : (string * string) list)
                (current : TypeInfo<GenericParamFromMetadata, TypeDefn>)
                =
                if depth > maxNestingDepth then
                    failwith
                        $"%s{operation}: type definition %s{current.Name} in %s{identity.AssemblyFullName} is nested more than %d{maxNestingDepth} deep; its enclosing chain is cyclic"

                let acc = (current.Namespace, current.Name) :: acc

                if current.IsNested then
                    match definingAssembly.TypeDefs.TryGetValue current.DeclaringType with
                    | true, enclosing -> walk (depth + 1) acc enclosing
                    | false, _ ->
                        failwith
                            $"%s{operation}: type definition %s{current.Name} in %s{identity.AssemblyFullName} names an enclosing type that assembly does not contain"
                else
                    acc

            Some (walk 0 [] definingAssembly.TypeDefs.[identity.TypeDefinition.Get])
        | _ ->
            ignore<DumpedAssembly> assembly
            None

    /// Resolve a nominal `TypeDefn` leaf to the TypeDef that defines it, which is the identity
    /// `CompareTypeTokens` ultimately compares. Following a reference can load the assembly it
    /// names, hence the assemblies in and out.
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
    let private resolveNominalIdentity
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (operation : string)
        (assemblies : LoadedAssemblies)
        (assembly : DumpedAssembly)
        (defn : TypeDefn)
        : LoadedAssemblies * Result<ResolvedTypeIdentity, TypeResolutionMiss>
        =
        match defn with
        | TypeDefn.FromDefinition (identity, _) -> assemblies, Ok identity
        | TypeDefn.FromReference (typeRef, _) ->
            TypeResolution.resolveTypeRefIdentity loggerFactory dotnetRuntimeDirs assembly typeRef assemblies
        | other -> failwith $"%s{operation}: %O{other} is not a nominal type reference"

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
            // Rank alone decides the shape: the decoder refuses a non-canonical `ArrayShape`, and
            // canonical shapes of one rank carry identical sizes and lower bounds.
            if lRank <> rRank then
                assemblies, false
            else
                recurse assemblies lElt rElt
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
            if lArgs.Length <> rArgs.Length then
                assemblies, false
            else
                let assemblies, genericMatches = recurse assemblies lGeneric rGeneric

                if not genericMatches then
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
                    $"%s{operation}: comparing two function pointer signatures that spell the same GENERIC calling convention and the same generic-parameter count (in %s{leftAssembly.Name.FullName} against %s{rightAssembly.Name.FullName}); from here CoreCLR compares elements read at a one-integer offset into each blob, reinterpreting the parameter-count byte as an element type, which cannot be reproduced from a decoded signature"
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
        | TypeDefn.FromReference (lRef, lKind), TypeDefn.FromReference (rRef, rKind) when
            leftAssembly.Name.FullName = rightAssembly.Name.FullName
            && lRef.Handle = rRef.Handle
            ->
            // `CompareTypeTokens`'s first step, `tk1 == tk2` within one module, answered before
            // anything is resolved. That step earns its place rather than merely saving work —
            // `Signature_Init` strips custom modifiers without loading their types, so two
            // signatures can carry the same modifier naming an assembly nothing has loaded, and
            // resolving it would fail where CoreCLR answers.
            //
            // The row, not the description: two rows of one module may describe the same type, and
            // CoreCLR does not take this shortcut for them. The assembly check is what makes the
            // comparison meaningful, since a handle indexes its own module's tables.
            //
            // The kind still has to agree: `CompareElementType` compares the element-type byte
            // before it reads either token.
            assemblies, lKind = rKind
        | (TypeDefn.FromDefinition (_, lKind) | TypeDefn.FromReference (_, lKind)),
          (TypeDefn.FromDefinition (_, rKind) | TypeDefn.FromReference (_, rKind)) ->
            // CoreCLR fails a CLASS against a VALUETYPE on the element-type byte alone, before it
            // reads a token, so two spellings of one type with different kinds are unequal.
            if lKind <> rKind then
                assemblies, false
            else

            // Names before resolution, as `CompareTypeTokens` does — and enclosing names too,
            // which it compares by recursing on the scope. This is not an optimisation: an
            // assembly that cannot be bound makes PawPrint fail loudly, so every pair separated
            // here is a pair whose comparison cannot be decided by whether some assembly happens
            // to be loadable. Comparing only the leaf would leave every nested type to
            // resolution, since a nested row carries an empty namespace and its own name alone.
            let lName = nominalName operation assemblies leftAssembly left
            let rName = nominalName operation assemblies rightAssembly right

            if lName <> rName then
                assemblies, false
            else

            // Resolved one side at a time, answering as soon as one misses, so that no assembly
            // is loaded which CoreCLR would not have loaded either.
            let assemblies, lIdentity =
                resolveNominalIdentity loggerFactory dotnetRuntimeDirs operation assemblies leftAssembly left

            match lIdentity with
            | Error _ -> assemblies, false
            | Ok lIdentity ->

            let assemblies, rIdentity =
                resolveNominalIdentity loggerFactory dotnetRuntimeDirs operation assemblies rightAssembly right

            match rIdentity with
            | Error _ -> assemblies, false
            | Ok rIdentity ->

            // Two definitions are the same type only if they are the same definition. CoreCLR has
            // one escape from that — CLR type equivalence, which makes separately embedded
            // `[TypeIdentifier]` interop types compare equal — but `CompareTypeTokens` reaches it
            // only under `FEATURE_TYPEEQUIVALENCE`, and `clrfeatures.cmake` sets that solely for
            // `CLR_CMAKE_TARGET_WIN32`. Everywhere else the arm reads `return FALSE`, commented
            // "two type defs can't be the same unless they are identical". PawPrint models a
            // Linux guest (`SimulatedUnixPlatform`), so equivalence is not a behaviour it should
            // reproduce; a build for a Windows guest would have to revisit it.
            assemblies, lIdentity = rIdentity
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
            leftAssembly.Name.FullName = rightAssembly.Name.FullName
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
