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

/// <summary>
/// Which method a MemberRef names, answered at the level of generic definitions: no type is
/// instantiated, and a reference to a member of <c>List&lt;int&gt;</c> resolves to the method of
/// <c>List&lt;T&gt;</c> it names.
/// </summary>
[<RequireQualifiedAccess>]
module MethodReferenceResolution =

    /// A type whose methods are being searched, and the reading of its signatures' `!i` that makes
    /// them comparable with the reference's.
    type private SearchedType =
        {
            Assembly : DumpedAssembly
            Type : TypeInfo<GenericParamFromMetadata, TypeDefn>
            Context : TypeConcretization.SubstitutionContext
        }

    let private definitionOf
        (assemblies : LoadedAssemblies)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let assembly = assemblies.ByDefinitionName identity.AssemblyFullName
        assembly, assembly.TypeDefs.[identity.TypeDefinition.Get]

    /// The definition a type spelling in `spellingAssembly` names, with the type arguments the
    /// spelling applies to it (empty for a non-generic spelling).
    let private resolveNominal
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (spellingAssembly : DumpedAssembly)
        (spelling : TypeDefn)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * ResolvedTypeIdentity * ImmutableArray<TypeDefn>
        =
        let identityOf (root : TypeDefn) : LoadedAssemblies * ResolvedTypeIdentity =
            match root with
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
                    $"Expected a named type or an instantiation of one, in %s{spellingAssembly.DefinitionFullName}, but got %O{other}"

        match spelling with
        | TypeDefn.GenericInstantiation (root, arguments) ->
            let assemblies, identity = identityOf root
            assemblies, identity, arguments
        | root ->
            let assemblies, identity = identityOf root
            assemblies, identity, ImmutableArray.Empty

    /// `searched`'s base class, with the base's `!i` read through `searched`'s extends clause
    /// (<c>GetSubstitutionForParent</c>), or `None` at the root of the hierarchy.
    let private parentOf
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (searched : SearchedType)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * SearchedType option
        =
        let spelling =
            match searched.Type.BaseType with
            | None -> None
            | Some (BaseTypeInfo.TypeDef handle) ->
                Some (TypeDefn.FromDefinition (searched.Assembly.TypeDefs.[handle].Identity, SignatureTypeKind.Class))
            | Some (BaseTypeInfo.TypeRef handle) ->
                Some (TypeDefn.FromReference (searched.Assembly.TypeRefs.[handle], SignatureTypeKind.Class))
            | Some (BaseTypeInfo.TypeSpec handle) -> Some searched.Assembly.TypeSpecs.[handle].Signature

        match spelling with
        | None -> assemblies, None
        | Some spelling ->
            let assemblies, identity, arguments =
                resolveNominal loggerFactory dotnetRuntimeDirs searched.Assembly spelling assemblies

            let assembly, ty = definitionOf assemblies identity

            let parent =
                {
                    Assembly = assembly
                    Type = ty
                    Context =
                        TypeConcretization.SubstitutionContext.forBase
                            searched.Assembly.DefinitionFullName
                            arguments
                            searched.Context
                }

            assemblies, Some parent

    /// Every strict ancestor of `searched`, nearest first.
    let private ancestorsOf
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (searched : SearchedType)
        (assemblies : LoadedAssemblies)
        : LoadedAssemblies * SearchedType list
        =
        let rec go (assemblies : LoadedAssemblies) (current : SearchedType) (acc : SearchedType list) =
            match parentOf loggerFactory dotnetRuntimeDirs current assemblies with
            | assemblies, None -> assemblies, List.rev acc
            | assemblies, Some parent -> go assemblies parent (parent :: acc)

        go assemblies searched []

    /// The methods the runtime synthesises on an array type (array.cpp, <c>ArrayClass::
    /// GenerateArrayAccessorCallSig</c>), each signature spelled with the array's element type where
    /// the runtime's has its formal <c>!0</c>: <c>Get</c>, <c>Set</c> and <c>Address</c> of the
    /// array's rank, and its constructors. A multidimensional array has one taking lengths and one
    /// taking lower bounds and lengths; a vector has one per level of vector nesting in its element
    /// type, each allocating that many levels.
    let private arrayMethods (arrayType : TypeDefn) : (ArrayAccessor * TypeMethodSignature<TypeDefn>) list =
        let element, rank, constructorArities =
            match arrayType with
            | TypeDefn.Array (element, rank) -> element, rank, [ rank ; 2 * rank ]
            | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
                let rec nesting (ty : TypeDefn) : int =
                    match ty with
                    | TypeDefn.OneDimensionalArrayLowerBoundZero inner -> 1 + nesting inner
                    | _ -> 0

                element, 1, [ 1 .. 1 + nesting element ]
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
           |> List.map (fun arity -> ArrayAccessor.Constructor arity, instance MethodReturnType.Void (indices arity)))

    /// <summary>
    /// `MemberLoader::GetDescFromMemberRef` for a method-shaped MemberRef of `referencingAssembly`,
    /// comparing signatures as `MetaSig::CompareMethodSigs` does with the reference's type variables
    /// left standing.
    /// </summary>
    /// <remarks>
    /// <c>MemberLoader::FindMethod</c> walks the parent's whole method table from the end, and then,
    /// for a class, recurses into the base class. A method table holds the type's own non-virtual
    /// methods and *every* vtable slot, inherited ones included, so the order that makes is:
    /// the parent's own non-virtual methods; then every virtual method visible from the parent, the
    /// most derived first; then, for a class only, each ancestor's non-virtual methods in turn. A
    /// struct therefore finds <c>ToString</c> but not <c>GetType</c>, an interface finds only its
    /// own methods, and a constructor is never found on an ancestor. A reference to an array type
    /// finds the runtime's accessors, and then carries on at <c>System.Array</c>.
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

        /// Search `levels` in order for a method passing `keep` whose signature is the reference's,
        /// the reference's `!i` being read as `referenceContext` says.
        let firstMatch
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (referenceContext : TypeConcretization.SubstitutionContext)
            (keep : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> -> bool)
            (levels : SearchedType list)
            : TypeConcretization.ConcretizationContext<DumpedAssembly> * MethodReferenceTarget option
            =
            let referenceComparand : TypeConcretization.SignatureComparand =
                {
                    Signature = signature
                    AssemblyFullName = referencingAssembly.DefinitionFullName
                    DeclaringTypeGenerics = referenceContext
                }

            let rec go (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>) (levels : SearchedType list) =
                match levels with
                | [] -> ctx, None
                | searched :: rest ->
                    let ctx, matches =
                        ((ctx, []), searched.Type.Methods)
                        ||> List.fold (fun (ctx, acc) candidate ->
                            if candidate.Name <> name || not (keep candidate) then
                                ctx, acc
                            else
                                let candidateComparand : TypeConcretization.SignatureComparand =
                                    {
                                        Signature = candidate.Signature
                                        AssemblyFullName = searched.Assembly.DefinitionFullName
                                        DeclaringTypeGenerics = searched.Context
                                    }

                                let equivalent, ctx =
                                    TypeConcretization.signaturesEquivalent
                                        ctx
                                        loader
                                        false
                                        referenceComparand
                                        candidateComparand

                                if equivalent then ctx, candidate :: acc else ctx, acc
                        )

                    match matches with
                    | [] -> go ctx rest
                    | [ found ] ->
                        match found.TryMetadata with
                        | Some facts -> ctx, Some (MethodReferenceTarget.Defined (searched.Assembly, facts.Handle))
                        | None ->
                            failwith
                                $"%s{searched.Type.Namespace}.%s{searched.Type.Name}::%s{name} in %s{searched.Assembly.DefinitionFullName} is a TypeDef's method with no metadata row"
                    | _ ->
                        // CoreCLR takes whichever it meets first searching the method table from
                        // the end, an order this does not model. Two methods of one type with one
                        // name and one signature, custom modifiers included, is not something a
                        // compiler emits.
                        failwith
                            $"%d{matches.Length} methods of %s{searched.Type.Namespace}.%s{searched.Type.Name} in %s{searched.Assembly.DefinitionFullName} match the reference to %s{name} from %s{referencingAssembly.DefinitionFullName}"

            go ctx levels

        let orElse
            (next :
                TypeConcretization.ConcretizationContext<DumpedAssembly>
                    -> TypeConcretization.ConcretizationContext<DumpedAssembly> * MethodReferenceTarget option)
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>, found : MethodReferenceTarget option)
            =
            match found with
            | Some _ -> ctx, found
            | None -> next ctx

        let isVirtual (m : MethodInfo<_, _, _>) : bool = m.IsVirtual

        /// Steps two and three of the search, for the ancestors of a parent whose own non-virtual
        /// methods did not match. `ownVirtuals` is the parent itself, when it is a type with
        /// virtual methods of its own; `searchNonVirtualAncestors` is whether the parent is a class.
        let searchAncestors
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (referenceContext : TypeConcretization.SubstitutionContext)
            (ownVirtuals : SearchedType list)
            (ancestors : SearchedType list)
            (searchNonVirtualAncestors : bool)
            : TypeConcretization.ConcretizationContext<DumpedAssembly> * MethodReferenceTarget
            =
            let ctx, found =
                firstMatch ctx referenceContext isVirtual (ownVirtuals @ ancestors)
                |> orElse (fun ctx ->
                    // `IsMdInstanceInitializer`: constructors are not inherited.
                    if searchNonVirtualAncestors && name <> ".ctor" then
                        firstMatch ctx referenceContext (isVirtual >> not) ancestors
                    else
                        ctx, None
                )

            ctx, Option.defaultValue MethodReferenceTarget.Missing found

        /// Search the definition `identity`, whose own type variables the reference's `!i` name.
        let searchFrom
            (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
            (identity : ResolvedTypeIdentity)
            =
            let assembly, ty = definitionOf ctx.LoadedAssemblies identity

            // Classifying the parent walks its base types, so they must all be loaded.
            let assemblies =
                match
                    TypeResolution.tryPrimeBaseChain loggerFactory dotnetRuntimeDirs ctx.LoadedAssemblies assembly ty
                with
                | assemblies, None -> assemblies
                | _, Some failure ->
                    failwith
                        $"The base chain of %s{ty.Namespace}.%s{ty.Name}, parent of a reference to %s{name} from %s{referencingAssembly.DefinitionFullName}, does not load: %O{failure}"

            let root =
                {
                    Assembly = assembly
                    Type = ty
                    Context = TypeConcretization.SubstitutionContext.forDefinition identity ty.Generics.Length
                }

            let isClass =
                not ty.IsInterface
                && not (LoadedTypeInfo.isValueType ctx.BaseTypes assemblies ty)

            // An interface's method table holds its own methods and nothing inherited.
            let assemblies, ancestors =
                if ty.IsInterface then
                    assemblies, []
                else
                    ancestorsOf loggerFactory dotnetRuntimeDirs root assemblies

            let ctx = withAssemblies ctx assemblies

            firstMatch ctx root.Context (isVirtual >> not) [ root ]
            |> function
                | ctx, Some found -> ctx, found
                | ctx, None -> searchAncestors ctx root.Context [ root ] ancestors isClass

        /// Search an array type: the runtime's accessors, then `System.Array` and what it inherits.
        let searchArray (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>) (arrayType : TypeDefn) =
            let accessorName (accessor : ArrayAccessor) : string =
                match accessor with
                | ArrayAccessor.Get -> "Get"
                | ArrayAccessor.Set -> "Set"
                | ArrayAccessor.Address -> "Address"
                | ArrayAccessor.Constructor _ -> ".ctor"

            let accessor =
                arrayMethods arrayType
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

            match accessor with
            | ctx, Some found -> ctx, found
            | ctx, None ->
                let systemArray =
                    {
                        Assembly = ctx.BaseTypes.Corelib
                        Type = ctx.BaseTypes.Array
                        Context = TypeConcretization.SubstitutionContext.forDefinition ctx.BaseTypes.Array.Identity 0
                    }

                let assemblies, ancestors =
                    ancestorsOf loggerFactory dotnetRuntimeDirs systemArray ctx.LoadedAssemblies

                let ctx = withAssemblies ctx assemblies
                // The array's own method table holds no virtual methods of its own, but every one
                // `System.Array` has; the non-virtual ones are found by recursing into it.
                let levels = systemArray :: ancestors

                let ctx, found =
                    firstMatch ctx systemArray.Context isVirtual levels
                    |> orElse (fun ctx ->
                        if name <> ".ctor" then
                            firstMatch ctx systemArray.Context (isVirtual >> not) levels
                        else
                            ctx, None
                    )

                ctx, Option.defaultValue MethodReferenceTarget.Missing found

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
            let assemblies, identity, _ =
                resolveNominal
                    loggerFactory
                    dotnetRuntimeDirs
                    referencingAssembly
                    (TypeDefn.FromReference (referencingAssembly.TypeRefs.[handle], SignatureTypeKind.Class))
                    ctx.LoadedAssemblies

            searchFrom (withAssemblies ctx assemblies) identity
        | MetadataToken.TypeSpecification handle ->
            match referencingAssembly.TypeSpecs.[handle].Signature with
            | TypeDefn.OneDimensionalArrayLowerBoundZero _
            | TypeDefn.Array _ as arrayType -> searchArray ctx arrayType
            | spelling ->
                let assemblies, identity, _ =
                    resolveNominal loggerFactory dotnetRuntimeDirs referencingAssembly spelling ctx.LoadedAssemblies

                searchFrom (withAssemblies ctx assemblies) identity
        | MetadataToken.ModuleReference _ ->
            failwith
                $"TODO: MemberRef %s{name} in %s{referencingAssembly.DefinitionFullName} names a global function of another module, which is not modelled"
        | other ->
            failwith
                $"MemberRef %s{name} in %s{referencingAssembly.DefinitionFullName} has parent %O{other}, which ECMA-335 does not permit"
