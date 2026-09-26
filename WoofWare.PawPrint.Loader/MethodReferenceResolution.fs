namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// One of the methods the runtime supplies on every array type (array.cpp,
/// <c>ArrayClass::GenerateArrayAccessorCallSig</c>).
[<RequireQualifiedAccess>]
type ArrayAccessor =
    /// <c>E Get(int32, ...)</c>, one index per dimension.
    | Get
    /// <c>void Set(int32, ..., E)</c>.
    | Set
    /// <c>E&amp; Address(int32, ...)</c>.
    | Address
    /// A constructor taking this many <c>int32</c>s: a multidimensional array's lengths, or its
    /// lower bounds and lengths; or, for a vector, the lengths of that many levels of nested vectors.
    | Constructor of arity : int

/// <summary>
/// The method a MemberRef row names, as CoreCLR's <c>MemberLoader::GetDescFromMemberRef</c> binds it.
/// </summary>
[<RequireQualifiedAccess>]
type MethodReferenceTarget =
    /// A method with a definition in metadata: the assembly that declares it, and its row there.
    /// For a reference whose parent is an instantiation, this is the generic definition's method.
    | Defined of declaringAssembly : DumpedAssembly * method : MethodDefinitionHandle

    /// One of the methods the runtime itself supplies on an array type. There is no definition to
    /// point at.
    | ArrayMethod of arrayType : TypeDefn * accessor : ArrayAccessor

    /// Nothing of this name and signature where CoreCLR looks, so binding the reference throws
    /// <c>MissingMethodException</c>.
    | Missing

    /// The answer turns on how a type variable of the context using the reference is instantiated,
    /// which a reading of the definitions alone cannot know: the parent is itself a type variable,
    /// or it is a vector of one and the reference could be a constructor for a deeper nesting of
    /// vectors than the spelling shows.
    | DependsOnInstantiation

/// <summary>
/// Which method a MemberRef names, answered at the level of generic definitions: no type is
/// instantiated, and a reference to a member of <c>List&lt;int&gt;</c> resolves to the method of
/// <c>List&lt;T&gt;</c> it names.
/// </summary>
[<RequireQualifiedAccess>]
module MethodReferenceResolution =

    /// A type definition whose method table is being searched, with its own `!i` read as the
    /// reference's parent sees them: the parent definition's own variables at the start of the
    /// search, and each ancestor's through the extends clauses between (<c>GetSubstitutionForParent</c>).
    type private SearchedType =
        {
            Identity : ResolvedTypeIdentity
            Context : TypeConcretization.SubstitutionContext
        }

    /// The definition a type spelling in `spellingAssembly` names, whether nominally or as one of
    /// the primitive element types a TypeSpec may carry.
    let private identityOfSpelling
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * ResolvedTypeIdentity
        =
        // A custom modifier annotates a signature; the type it annotates is the one being named.
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.GenericInstantiation (root, _) ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromDefinition (identity, _) -> assemblies, identity
            | TypeDefn.FromReference (typeRef, _) ->
                match
                    TypeResolution.resolveTypeRefIdentity
                        loggerFactory
                        dotnetRuntimeDirs
                        spellingAssembly
                        typeRef
                        assemblies
                with
                | assemblies, Ok identity -> assemblies, identity
                | _, Error miss ->
                    failwith
                        $"Type reference %s{typeRef.Namespace}.%s{typeRef.Name} from %s{spellingAssembly.DefinitionFullName} does not resolve: %O{miss}"
            | other ->
                failwith
                    $"An instantiation in %s{spellingAssembly.DefinitionFullName} applies arguments to %O{other}, which names no generic definition"
        | TypeDefn.FromDefinition (identity, _) -> assemblies, identity
        | TypeDefn.FromReference (typeRef, _) ->
            match
                TypeResolution.resolveTypeRefIdentity
                    loggerFactory
                    dotnetRuntimeDirs
                    spellingAssembly
                    typeRef
                    assemblies
            with
            | assemblies, Ok identity -> assemblies, identity
            | _, Error miss ->
                failwith
                    $"Type reference %s{typeRef.Namespace}.%s{typeRef.Name} from %s{spellingAssembly.DefinitionFullName} does not resolve: %O{miss}"
        | TypeDefn.PrimitiveType primitive -> assemblies, (BaseClassTypes.ofPrimitive baseClassTypes primitive).Identity
        | other ->
            failwith
                $"Expected a type with a method table in %s{spellingAssembly.DefinitionFullName}, but got %O{other}"

    /// The methods the runtime synthesises on an array type (array.cpp, <c>ArrayClass::
    /// GenerateArrayAccessorCallSig</c>), each signature spelled with the array's element type where
    /// the runtime's has its formal <c>!0</c>: <c>Get</c>, <c>Set</c> and <c>Address</c> of the
    /// array's rank, and its constructors. A multidimensional array has one taking lengths and one
    /// taking lower bounds and lengths; a vector has one per level of vector nesting in its element
    /// type, each allocating that many levels.
    ///
    /// The flag is whether a vector may have more constructors than these: its innermost element,
    /// past every level of vector nesting, is a type variable, which an instantiation could make a
    /// vector in turn.
    let private arrayMethods (arrayType : TypeDefn) : (ArrayAccessor * TypeMethodSignature<TypeDefn>) list * bool =
        let element, rank, constructorArities, openEnded =
            match arrayType with
            | TypeDefn.Array (element, rank) -> element, rank, [ rank ; 2 * rank ], false
            | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
                // `ptr.GetInternalCorElementType() == ELEMENT_TYPE_SZARRAY` counts the levels of a
                // type handle, which has no custom modifiers to stop at.
                let rec nesting (ty : TypeDefn) : int * bool =
                    match TypeDefn.stripCustomModifiers ty with
                    | TypeDefn.OneDimensionalArrayLowerBoundZero inner ->
                        let depth, openEnded = nesting inner
                        1 + depth, openEnded
                    | TypeDefn.GenericTypeParameter _
                    | TypeDefn.GenericMethodParameter _ -> 0, true
                    | _ -> 0, false

                let depth, openEnded = nesting element
                element, 1, [ 1 .. 1 + depth ], openEnded
            | other -> failwith $"not an array type: %O{other}"

        let instance (ret : MethodReturnType<TypeDefn>) (parameters : TypeDefn list) : TypeMethodSignature<TypeDefn> =
            {
                Header =
                    ComparableSignatureHeader.Make (
                        SignatureHeader (
                            SignatureKind.Method,
                            SignatureCallingConvention.Default,
                            SignatureAttributes.Instance
                        )
                    )
                ParameterTypes = parameters
                GenericParameterCount = 0
                RequiredParameterCount = parameters.Length
                ReturnType = ret
            }

        let indices (count : int) : TypeDefn list =
            List.replicate count (TypeDefn.PrimitiveType PrimitiveType.Int32)

        [
            ArrayAccessor.Get, instance (MethodReturnType.Returns element) (indices rank)
            ArrayAccessor.Set, instance MethodReturnType.Void (indices rank @ [ element ])
            ArrayAccessor.Address, instance (MethodReturnType.Returns (TypeDefn.Byref element)) (indices rank)
        ]
        @ (constructorArities
           |> List.map (fun arity -> ArrayAccessor.Constructor arity, instance MethodReturnType.Void (indices arity))),
        openEnded

    /// <summary>
    /// `MemberLoader::GetDescFromMemberRef` for a method-shaped MemberRef of `referencingAssembly`,
    /// comparing signatures as `MetaSig::CompareMethodSigs` does with the reference's type variables
    /// left standing.
    /// </summary>
    /// <remarks>
    /// <c>MemberLoader::FindMethod</c> walks the parent's method table from the end, and then, for a
    /// class, recurses into the base class. A method table holds the type's own slots beyond the
    /// vtable and then *every* vtable slot, inherited ones included, each slot holding the most
    /// derived declaration placed there; <c>MethodTableLayout</c> lays out exactly that. So a struct
    /// finds <c>ToString</c> but not <c>GetType</c>, an interface finds only its own methods, two
    /// methods whose signatures coincide once an extends clause is substituted are told apart by
    /// their order in the table, and a constructor is never found on an ancestor. A reference to an
    /// array type finds the runtime's accessors, then carries on into <c>System.Array</c>.
    ///
    /// Loads whatever assemblies the parent, its base chain and the signatures name.
    /// </remarks>
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        (referencingAssembly : DumpedAssembly)
        (reference : MemberReferenceHandle)
        : TypeConcretization.ConcretizationContext<DumpedAssembly> * MethodReferenceTarget
        =
        let row = referencingAssembly.Members.[reference]
        let name = referencingAssembly.Strings row.Name

        let signature =
            match row.Signature with
            | MemberSignature.Method signature -> signature
            | MemberSignature.Field _ ->
                failwith $"MemberRef %s{name} in %s{referencingAssembly.DefinitionFullName} names a field, not a method"

        let operation =
            $"resolving the reference to %s{name} from %s{referencingAssembly.DefinitionFullName}"

        let loader = TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs

        let withAssemblies
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (assemblies : LoadedAssemblies)
            =
            { ctx with
                LoadedAssemblies = assemblies
            }

        let unsubstituted (signature : TypeMethodSignature<TypeDefn>) : TypeConcretization.UnsubstitutedComparand =
            {
                Signature = signature
                AssemblyFullName = referencingAssembly.DefinitionFullName
            }

        let searchedDefinition (identity : ResolvedTypeIdentity) (arity : int) : SearchedType =
            {
                Identity = identity
                Context = TypeConcretization.SubstitutionContext.forDefinition identity arity
            }

        /// The first of `entries`, all from `searched`'s method table, that the reference names.
        let firstMatch
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (referenceContext : TypeConcretization.SubstitutionContext)
            (searched : SearchedType)
            (entries : VtableSlot list)
            : TypeConcretization.ConcretizationContext<DumpedAssembly> * VtableSlot option
            =
            let referenceComparand : TypeConcretization.SignatureComparand =
                {
                    Signature = signature
                    AssemblyFullName = referencingAssembly.DefinitionFullName
                    DeclaringTypeGenerics = referenceContext
                }

            let rec go (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>) (entries : VtableSlot list) =
                match entries with
                | [] -> ctx, None
                | entry :: rest when entry.Method.Name <> name -> go ctx rest
                | entry :: rest ->
                    let candidateComparand : TypeConcretization.SignatureComparand =
                        {
                            Signature = entry.Method.Signature
                            AssemblyFullName = entry.DeclaredBy.AssemblyFullName
                            // The table is laid out in `searched`'s own vocabulary; read it in the
                            // reference parent's.
                            DeclaringTypeGenerics =
                                TypeConcretization.SubstitutionContext.rebase
                                    searched.Identity
                                    searched.Context.Arguments
                                    entry.DeclaredBy.Substitution
                        }

                    match
                        TypeConcretization.signaturesEquivalent ctx loader false referenceComparand candidateComparand
                    with
                    | true, ctx -> ctx, Some entry
                    | false, ctx -> go ctx rest

            go ctx entries

        /// `searched`'s method table in the order `MethodTable::MethodIterator` visits it from the
        /// end: the slots beyond the vtable, then the vtable's, each half last slot first.
        let methodTableFromTheEnd
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (searched : SearchedType)
            : TypeConcretization.ConcretizationContext<DumpedAssembly> * VtableSlot list * VtableSlot list
            =
            let ctx, table =
                MethodTableLayout.slotTableOfDefinition loggerFactory dotnetRuntimeDirs operation ctx searched.Identity

            ctx, List.rev table.BeyondVtable, List.rev table.Vtable

        let definedBy
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (entry : VtableSlot)
            : MethodReferenceTarget
            =
            match entry.Method.TryMetadata with
            | Some facts ->
                MethodReferenceTarget.Defined (
                    ctx.LoadedAssemblies.ByDefinitionName entry.DeclaredBy.AssemblyFullName,
                    facts.Handle
                )
            | None ->
                failwith
                    $"%s{operation}: method %s{entry.Method.Name} of %s{entry.DeclaredBy.Description} occupies a slot but has no metadata row"

        /// `IsMdInstanceInitializer`, which `FindMethod` refuses to return from a base class.
        let isInstanceInitializer (entry : VtableSlot) : bool =
            match entry.Method.TryMetadata with
            | Some facts ->
                entry.Method.Name = ".ctor"
                && facts.MethodAttributes.HasFlag System.Reflection.MethodAttributes.RTSpecialName
            | None -> false

        /// `MemberLoader::FindMethod` on `searched`. `inherited` is whether this is a recursion into
        /// a base class, whose constructors the caller may not have.
        let rec findMethod
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (referenceContext : TypeConcretization.SubstitutionContext)
            (searched : SearchedType)
            (inherited : bool)
            : TypeConcretization.ConcretizationContext<DumpedAssembly> * MethodReferenceTarget
            =
            let ctx, beyondVtable, vtable = methodTableFromTheEnd ctx searched

            match firstMatch ctx referenceContext searched (beyondVtable @ vtable) with
            | ctx, Some entry ->
                if inherited && isInstanceInitializer entry then
                    ctx, MethodReferenceTarget.Missing
                else
                    ctx, definedBy ctx entry
            | ctx, None ->
                let assembly, typeInfo =
                    MethodTableLayout.definitionMetadata operation ctx.LoadedAssemblies searched.Identity

                // "No inheritance on value types or interfaces": their inherited virtuals were in
                // the table already.
                if
                    typeInfo.IsInterface
                    || LoadedTypeInfo.isValueType ctx.BaseTypes ctx.LoadedAssemblies typeInfo
                then
                    ctx, MethodReferenceTarget.Missing
                else

                let owner : SlotOwner =
                    {
                        AssemblyFullName = searched.Identity.AssemblyFullName
                        Identity = searched.Identity
                        Substitution = searched.Context
                        Description = TypeInfo.fullName (fun handle -> assembly.TypeDefs.[handle]) typeInfo
                    }

                match
                    MethodTableLayout.baseOfDefinition loggerFactory dotnetRuntimeDirs operation ctx owner typeInfo
                with
                | ctx, None -> ctx, MethodReferenceTarget.Missing
                | ctx, Some (baseIdentity, arguments) ->
                    let parent =
                        {
                            Identity = baseIdentity
                            Context =
                                {
                                    TypeConcretization.SubstitutionContext.Arguments = arguments
                                }
                        }

                    findMethod ctx referenceContext parent true

        /// Search the definition `identity`, whose own type variables the reference's `!i` name.
        let searchFrom
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (identity : ResolvedTypeIdentity)
            =
            let assembly, ty =
                MethodTableLayout.definitionMetadata operation ctx.LoadedAssemblies identity

            // Classifying each type on the way walks its base types, so they must all be loaded.
            let assemblies =
                match
                    TypeResolution.tryPrimeBaseChain loggerFactory dotnetRuntimeDirs ctx.LoadedAssemblies assembly ty
                with
                | assemblies, None -> assemblies
                | _, Some failure -> failwith $"%s{operation}: the base chain of the parent does not load: %O{failure}"

            let root = searchedDefinition identity ty.Generics.Length
            findMethod (withAssemblies ctx assemblies) root.Context root false

        /// Search an array type: the runtime's accessors, then `System.Array`. The array's own
        /// method table also holds every vtable slot of `System.Array`, but those are exactly what
        /// the search of `System.Array` meets next, and no type holds two methods of one name and
        /// signature for the order between them to decide anything.
        let searchArray (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>) (arrayType : TypeDefn) =
            let accessorName (accessor : ArrayAccessor) : string =
                match accessor with
                | ArrayAccessor.Get -> "Get"
                | ArrayAccessor.Set -> "Set"
                | ArrayAccessor.Address -> "Address"
                | ArrayAccessor.Constructor _ -> ".ctor"

            let accessors, openEnded = arrayMethods arrayType

            let accessor =
                accessors
                |> List.filter (fun (accessor, _) -> accessorName accessor = name)
                |> List.fold
                    (fun (ctx, found) (accessor, candidate) ->
                        match found with
                        | Some _ -> ctx, found
                        | None ->
                            let equivalent, ctx =
                                TypeConcretization.signaturesEquivalentWithoutSubstitution
                                    ctx
                                    loader
                                    false
                                    (unsubstituted signature)
                                    (unsubstituted candidate)

                            ctx,
                            (if equivalent then
                                 Some (MethodReferenceTarget.ArrayMethod (arrayType, accessor))
                             else
                                 None)
                    )
                    (ctx, None)

            // A constructor for more levels of nesting than the spelling shows, which only an
            // instantiation making the innermost element a vector would supply.
            let couldBeDeeperConstructor =
                openEnded
                && name = ".ctor"
                && signature.Header.Get.IsInstance
                && signature.GenericParameterCount = 0
                && signature.ReturnType = MethodReturnType.Void
                && signature.ParameterTypes
                   |> List.forall (fun ty -> ty = TypeDefn.PrimitiveType PrimitiveType.Int32)

            match accessor with
            | ctx, Some found -> ctx, found
            | ctx, None when couldBeDeeperConstructor -> ctx, MethodReferenceTarget.DependsOnInstantiation
            | ctx, None ->
                let systemArray = searchedDefinition ctx.BaseTypes.Array.Identity 0
                findMethod ctx systemArray.Context systemArray true

        match row.Parent with
        | MetadataToken.MethodDef handle ->
            // A vararg call site: the parent is the definition itself, in this module, and
            // CoreCLR only checks the signatures agree.
            let method = referencingAssembly.Methods.[handle]

            let equivalent, ctx =
                TypeConcretization.signaturesEquivalentWithoutSubstitution
                    ctx
                    loader
                    false
                    (unsubstituted signature)
                    (unsubstituted method.Signature)

            // Varargs methods may not be generic, so a generic one is not what such a reference
            // names.
            if equivalent && method.Generics.IsEmpty && method.DeclaringTypeGenerics.IsEmpty then
                ctx, MethodReferenceTarget.Defined (referencingAssembly, handle)
            else
                ctx, MethodReferenceTarget.Missing
        | MetadataToken.TypeDefinition handle -> searchFrom ctx referencingAssembly.TypeDefs.[handle].Identity
        | MetadataToken.TypeReference handle ->
            let assemblies, identity =
                identityOfSpelling
                    loggerFactory
                    dotnetRuntimeDirs
                    ctx.BaseTypes
                    referencingAssembly
                    (TypeDefn.FromReference (referencingAssembly.TypeRefs.[handle], SignatureTypeKind.Class))
                    ctx.LoadedAssemblies

            searchFrom (withAssemblies ctx assemblies) identity
        | MetadataToken.TypeSpecification handle ->
            let spelling = referencingAssembly.TypeSpecs.[handle].Signature

            match TypeDefn.stripCustomModifiers spelling with
            | TypeDefn.OneDimensionalArrayLowerBoundZero _
            | TypeDefn.Array _ as arrayType -> searchArray ctx arrayType
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _ -> ctx, MethodReferenceTarget.DependsOnInstantiation
            | _ ->
                let assemblies, identity =
                    identityOfSpelling
                        loggerFactory
                        dotnetRuntimeDirs
                        ctx.BaseTypes
                        referencingAssembly
                        spelling
                        ctx.LoadedAssemblies

                searchFrom (withAssemblies ctx assemblies) identity
        | MetadataToken.ModuleReference _ ->
            failwith
                $"TODO: MemberRef %s{name} in %s{referencingAssembly.DefinitionFullName} names a global function of another module, which is not modelled"
        | other ->
            failwith
                $"MemberRef %s{name} in %s{referencingAssembly.DefinitionFullName} has parent %O{other}, which ECMA-335 does not permit"
