namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type ConcreteTypeHandle =
    | Concrete of int
    | Byref of ConcreteTypeHandle
    | Pointer of ConcreteTypeHandle
    /// A zero-lower-bound single-dimensional array (szarray in IL), e.g. int[].
    | OneDimArrayZero of element : ConcreteTypeHandle
    /// A general array with explicit rank (potentially multi-dimensional), e.g. int[,] (rank=2).
    /// Rank is tracked so that int[,] and int[,,] are distinct types.
    | Array of element : ConcreteTypeHandle * rank : int
    /// A function pointer type (e.g. `delegate*<int, int>` in C#). Distinct fnptr types
    /// have distinct signatures, so the signature is the type identity.
    | FunctionPointer of TypeMethodSignature<ConcreteTypeHandle>

    override this.ToString () =
        match this with
        | ConcreteTypeHandle.Byref b -> "&" + b.ToString ()
        | ConcreteTypeHandle.Concrete i -> i.ToString ()
        | ConcreteTypeHandle.Pointer i -> "*" + i.ToString ()
        | ConcreteTypeHandle.OneDimArrayZero e -> e.ToString () + "[]"
        | ConcreteTypeHandle.Array (e, rank) ->
            let inside = if rank <= 1 then "*" else String.replicate (rank - 1) ","

            e.ToString () + "[" + inside + "]"
        | ConcreteTypeHandle.FunctionPointer signature ->
            let args =
                signature.ParameterTypes
                |> List.map (fun h -> h.ToString ())
                |> String.concat " -> "

            let returnStr =
                match signature.ReturnType with
                | MethodReturnType.Void -> "void"
                | MethodReturnType.Returns ty -> ty.ToString ()

            $"*({args} -> {returnStr})"

type AllConcreteTypes =
    private
        {
            Mapping : Map<int, ConcreteType<ConcreteTypeHandle>>
            /// Reverse index from (identity, generics) to handle, for O(1) deduplication lookups.
            ReverseIndex : Map<ResolvedTypeIdentity * ConcreteTypeHandle list, ConcreteTypeHandle>
            NextHandle : int
        }

    static member Empty =
        {
            Mapping = Map.empty
            ReverseIndex = Map.empty
            NextHandle = 0
        }

[<RequireQualifiedAccess>]
module AllConcreteTypes =
    let lookup (cth : ConcreteTypeHandle) (this : AllConcreteTypes) : ConcreteType<ConcreteTypeHandle> option =
        match cth with
        | ConcreteTypeHandle.Concrete id -> this.Mapping |> Map.tryFind id
        | ConcreteTypeHandle.Byref _ -> None // Byref types are not stored in the mapping
        | ConcreteTypeHandle.Pointer _ -> None // Pointer types are not stored in the mapping
        | ConcreteTypeHandle.OneDimArrayZero _ -> None // Array types are structural wrappers
        | ConcreteTypeHandle.Array _ -> None // Array types are structural wrappers
        | ConcreteTypeHandle.FunctionPointer _ -> None // FunctionPointer types are structural wrappers

    /// The metadata behind a handle: `lookup`, then the row's assembly, then its TypeDef. This
    /// is the chain every nominal-type question starts with, and it is spelled out at some
    /// forty sites.
    ///
    /// `None` means "not a registered nominal type" — either a structural handle (byref,
    /// pointer, array, function pointer), which by design has no row and no TypeDef, or a
    /// `Concrete` handle whose row is absent. Callers keep their own reaction to that, because
    /// the right answer genuinely differs by site: `Box` calls a pointer token invalid IL and
    /// names the ECMA rule, `Type.get_IsValueType` answers `false` to match CoreCLR's
    /// `IsValueTypeImpl` over TypeDescs, and `zeroOf` builds a null of the right shape.
    /// Collapsing those into one answer here would erase diagnostics that name the offending
    /// construct.
    ///
    /// A row that names an unloaded assembly, or a TypeDef row that is missing from it, stays a
    /// hard failure rather than a `None`: those are broken invariants inside the interpreter,
    /// not shapes a caller can meaningfully handle, and every call site today treats them so.
    let tryTypeInfo
        (assemblies : LoadedAssemblies)
        (concreteTypes : AllConcreteTypes)
        (handle : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>) option
        =
        // Look the assembly up by the identity's own string rather than through
        // `assemblies.[ct.Assembly]`, which reconstitutes an `AssemblyName` from that string and
        // asks for `.FullName` back. Both spellings are in use across the call sites this
        // replaces; this is the one that cannot be perturbed by `AssemblyName` normalising what
        // it round-trips.
        lookup handle concreteTypes
        |> Option.map (fun ct ->
            ct, assemblies.ByDefinitionName(ct.Identity.AssemblyFullName).TypeDefs.[ct.Identity.TypeDefinition.Get]
        )

    /// Whether a handle denotes a value type, for the handles that have a TypeDef to ask.
    ///
    /// `None` carries exactly the meaning it has in `tryTypeInfo`: the handle names no
    /// registered nominal type, so the question has no metadata-backed answer and the caller
    /// must supply its own.
    let tryIsValueType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblies : LoadedAssemblies)
        (concreteTypes : AllConcreteTypes)
        (handle : ConcreteTypeHandle)
        : bool option
        =
        tryTypeInfo assemblies concreteTypes handle
        |> Option.map (fun (_, typeInfo) -> DumpedAssembly.isValueType baseClassTypes assemblies typeInfo)

    /// How deep `describe` recurses through *generic* nesting before truncating, and how far it
    /// walks a declaring-type chain. A stack overflow cannot be caught, so these bounds are what
    /// make `describe`'s never-throws contract hold on a graph that no well-formed program
    /// produces -- including a `NestedClass` table whose declaring-type chain is cyclic.
    [<Literal>]
    let private describeDepthBudget = 16

    /// Render a handle for diagnostics, as `Namespace.Name<args>#handle [AssemblyShortName]`.
    ///
    /// Every nominal type carries `#handle`, and that -- not the name -- is what makes two
    /// renderings comparable: a full name is shared by nested types under different parents, by
    /// types in different assemblies, and by types in different versions of one assembly, so a name
    /// alone cannot say whether two types are the same.
    ///
    /// The one exception is generic nesting deeper than `describeDepthBudget`, which is truncated.
    /// A truncated nominal type still shows its handle; two function pointers differing only below
    /// the cut do render alike.
    ///
    /// Unlike `tryTypeInfo` this never throws and never returns an option: it is called from
    /// failure paths that must not fail a second time and so hide the original error, so every
    /// broken link in the chain renders as its own placeholder instead.
    let describe
        (assemblies : LoadedAssemblies)
        (concreteTypes : AllConcreteTypes)
        (handle : ConcreteTypeHandle)
        : string
        =
        // Byref, pointer and array wrappers form a chain rather than a tree, so they are peeled with
        // a loop. Recursing would spend the depth budget on them, and truncating a chain would
        // render `int[]..[]` and `string[]..[]` alike. `ConcreteTypeHandle` is an immutable tree, so
        // this terminates.
        let peelStructure (handle : ConcreteTypeHandle) : ConcreteTypeHandle * string =
            let decorations = ResizeArray<string> ()
            let mutable current = handle
            let mutable peeling = true

            while peeling do
                match current with
                | ConcreteTypeHandle.Byref inner ->
                    decorations.Add "&"
                    current <- inner
                | ConcreteTypeHandle.Pointer inner ->
                    decorations.Add "*"
                    current <- inner
                | ConcreteTypeHandle.OneDimArrayZero inner ->
                    decorations.Add "[]"
                    current <- inner
                | ConcreteTypeHandle.Array (inner, rank) ->
                    decorations.Add (
                        if rank <= 1 then
                            "[*]"
                        else
                            "[" + String (',', rank - 1) + "]"
                    )

                    current <- inner
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.FunctionPointer _ -> peeling <- false

            // The innermost wrapper binds tightest, so decorations apply in reverse of peel order.
            decorations.Reverse ()
            current, String.Concat decorations

        // `TypeInfo.fullName` is the CLR's own nesting rule, but it recurses over `DeclaringType`
        // and is shared with guest-visible reflection output, so it cannot be bounded in place.
        // This walks the same chain iteratively, refusing to revisit a row, so corrupt metadata
        // costs a marker rather than the process.
        let nestedName (assembly : DumpedAssembly) (typeDef : TypeInfo<GenericParamFromMetadata, TypeDefn>) : string =
            let mutable current = typeDef
            let mutable parts = [ typeDef.Name ]
            let mutable seen = Set.singleton typeDef.Identity.TypeDefinition
            let mutable truncated = false
            let mutable walking = true

            while walking do
                if not current.IsNested then
                    walking <- false
                elif List.length parts > describeDepthBudget then
                    truncated <- true
                    walking <- false
                else

                match assembly.TypeDefs.TryGetValue current.DeclaringType with
                | false, _ ->
                    truncated <- true
                    walking <- false
                | true, parent ->
                    if seen.Contains parent.Identity.TypeDefinition then
                        truncated <- true
                        walking <- false
                    else
                        seen <- seen.Add parent.Identity.TypeDefinition
                        parts <- parent.Name :: parts
                        current <- parent

            let joined = String.Join ("+", parts)

            // The namespace belongs to the outermost type of the chain.
            let qualified =
                if String.IsNullOrEmpty current.Namespace then
                    joined
                else
                    $"%s{current.Namespace}.%s{joined}"

            if truncated then "..+" + qualified else qualified

        let rec go (qualified : bool) (depth : int) (handle : ConcreteTypeHandle) : string =
            let leaf, decorations = peelStructure handle

            let rendered =
                if depth > describeDepthBudget then
                    match leaf with
                    | ConcreteTypeHandle.Concrete id -> $"<#%d{id} nested deeper than %d{describeDepthBudget}>"
                    | _ -> $"<function pointer nested deeper than %d{describeDepthBudget}>"
                else

                match leaf with
                | ConcreteTypeHandle.FunctionPointer signature ->
                    let parameters = signature.ParameterTypes |> List.map (go false (depth + 1))

                    // The calling convention, the generic arity and the vararg boundary are all
                    // fields of `TypeMethodSignature`, so two function pointers differing only in
                    // one of them are different types. ECMA-335 II.23.2.1 spells the vararg
                    // boundary `...`.
                    let args =
                        if
                            signature.RequiredParameterCount >= 0
                            && signature.RequiredParameterCount < List.length parameters
                        then
                            let required, optional = List.splitAt signature.RequiredParameterCount parameters

                            required @ [ "..." ] @ optional |> String.concat ", "
                        else
                            parameters |> String.concat ", "

                    let ret =
                        match signature.ReturnType with
                        | MethodReturnType.Void -> "void"
                        | MethodReturnType.Returns ret -> go false (depth + 1) ret

                    let header = signature.Header.Get

                    let attributes =
                        if header.Attributes = SignatureAttributes.None then
                            ""
                        else
                            $" %O{header.Attributes}"

                    let arity =
                        if signature.GenericParameterCount = 0 then
                            ""
                        else
                            $"<%d{signature.GenericParameterCount}>"

                    $"%O{header.CallingConvention}%s{attributes} %s{ret}%s{arity}(%s{args})*"
                | ConcreteTypeHandle.Concrete id ->
                    match lookup leaf concreteTypes with
                    | None -> $"<unregistered concrete type #%d{id}>"
                    | Some concrete ->

                    let generics =
                        if concrete.Generics.IsEmpty then
                            ""
                        else
                            concrete.Generics
                            |> Seq.map (go false (depth + 1))
                            |> String.concat ", "
                            |> sprintf "<%s>"

                    match assemblies.TryByDefinitionName concrete.Identity.AssemblyFullName with
                    | None -> $"<unloaded assembly %O{concrete.Assembly} for concrete type #%d{id}>"
                    | Some assembly ->

                    match assembly.TypeDefs.TryGetValue concrete.Definition.Get with
                    | true, typeDef ->
                        let name = nestedName assembly typeDef

                        if qualified then
                            $"%s{name}%s{generics}#%d{id} [%s{assembly.Name.Name}]"
                        else
                            $"%s{name}%s{generics}#%d{id}"
                    | false, _ -> $"<missing TypeDef %O{concrete.Definition.Get} in %s{assembly.Name.Name}> (#%d{id})"
                | other ->
                    // `peelStructure` returns only `Concrete` or `FunctionPointer`. Rendering rather
                    // than failing keeps the never-throws contract if that ever stops being true.
                    ignore other
                    "<unexpected structural handle>"

            rendered + decorations

        go true 0 handle


    let findExistingConcreteType
        (concreteTypes : AllConcreteTypes)
        (identity : ResolvedTypeIdentity)
        (generics : ConcreteTypeHandle ImmutableArray)
        : ConcreteTypeHandle option
        =
        let key = (identity, Seq.toList generics)
        concreteTypes.ReverseIndex |> Map.tryFind key

    let findExistingNonGenericConcreteType
        (concreteTypes : AllConcreteTypes)
        (identity : ResolvedTypeIdentity)
        : ConcreteTypeHandle option
        =
        findExistingConcreteType concreteTypes identity ImmutableArray.Empty

    let getRequiredNonGenericHandle (allConcreteTypes : AllConcreteTypes) (ty : TypeInfo<'a, 'b>) : ConcreteTypeHandle =
        findExistingNonGenericConcreteType allConcreteTypes ty.Identity |> Option.get

    let add (ct : ConcreteType<ConcreteTypeHandle>) (this : AllConcreteTypes) : ConcreteTypeHandle * AllConcreteTypes =
        let id = this.NextHandle
        let toRet = ConcreteTypeHandle.Concrete id
        let key = (ct.Identity, Seq.toList ct.Generics)

        let newState =
            {
                NextHandle = this.NextHandle + 1
                Mapping = this.Mapping |> Map.add id ct
                ReverseIndex = this.ReverseIndex |> Map.add key toRet
            }

        toRet, newState

// Active patterns for matching concrete types

[<AutoOpen>]
module ConcreteActivePatterns =
    /// Active pattern to match primitive types from concrete type handles
    let (|ConcretePrimitive|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct when ct.Namespace = "System" && ct.Generics.IsEmpty ->
                match ct.Name with
                | "Int32" -> Some PrimitiveType.Int32
                | "Int64" -> Some PrimitiveType.Int64
                | "Int16" -> Some PrimitiveType.Int16
                | "UInt32" -> Some PrimitiveType.UInt32
                | "UInt64" -> Some PrimitiveType.UInt64
                | "UInt16" -> Some PrimitiveType.UInt16
                | "Byte" -> Some PrimitiveType.Byte
                | "SByte" -> Some PrimitiveType.SByte
                | "Single" -> Some PrimitiveType.Single
                | "Double" -> Some PrimitiveType.Double
                | "String" -> Some PrimitiveType.String
                | "Boolean" -> Some PrimitiveType.Boolean
                | "Char" -> Some PrimitiveType.Char
                | "Object" -> Some PrimitiveType.Object
                | "IntPtr" -> Some PrimitiveType.IntPtr
                | "UIntPtr" -> Some PrimitiveType.UIntPtr
                | "TypedReference" -> Some PrimitiveType.TypedReference
                | _ -> None
            | _ -> None
        | _ -> None

    /// Active pattern to match void type
    let (|ConcreteVoid|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct when
                ct.Assembly.Name = "System.Private.CoreLib"
                && ct.Namespace = "System"
                && ct.Name = "Void"
                && ct.Generics.IsEmpty
                ->
                Some ()
            | _ -> None
        | _ -> None

    /// Active pattern to match any concrete type by assembly/namespace/name and generics
    let (|ConcreteType|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct -> Some (ct.Assembly.Name, ct.Namespace, ct.Name, ct.Generics)
            | None -> None
        | _ -> None

    let (|ConcreteChar|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Char"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteRuntimeFieldHandle|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct when
                ct.Assembly.Name = "System.Private.CoreLib"
                && ct.Namespace = "System"
                && ct.Name = "RuntimeFieldHandle"
                && ct.Generics.IsEmpty
                ->
                Some ()
            | _ -> None
        | _ -> None

    /// Matches the System.Array metadata type exactly.
    let (|ConcreteSystemArray|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct when
                ct.Assembly.Name = "System.Private.CoreLib"
                && ct.Namespace = "System"
                && ct.Name = "Array"
                && ct.Generics.IsEmpty
                ->
                Some ()
            | _ -> None
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None

    /// Matches an array type whose element type is the given handle.
    let (|ConcreteGenericArray|_|)
        (_concreteTypes : AllConcreteTypes)
        (eltType : ConcreteTypeHandle)
        (handle : ConcreteTypeHandle)
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero e when e = eltType -> Some ()
        | ConcreteTypeHandle.Array (e, _) when e = eltType -> Some ()
        | _ -> None

    let (|ConcreteObj|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Object"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteValueType|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "ValueType"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteBool|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Boolean"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteString|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "String"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteDouble|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Double"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteInt64|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Int64"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteInt32|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Int32"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteUInt32|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "UInt32"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteUInt64|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "UInt64"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteSingle|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "Single"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteIntPtr|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "IntPtr"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    let (|ConcreteUIntPtr|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    ct.Assembly.Name = "System.Private.CoreLib"
                    && ct.Namespace = "System"
                    && ct.Name = "UIntPtr"
                    && ct.Generics.IsEmpty
                then
                    Some ()
                else
                    None
            | None -> None
        | _ -> None

    /// Active pattern to match byref types
    let (|ConcreteByref|_|) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Byref inner -> Some inner
        | _ -> None

    /// Active pattern to match pointer types
    let (|ConcretePointer|_|) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Pointer inner -> Some inner
        | _ -> None

    /// Active pattern to match szarray types (zero-lower-bound one-dimensional arrays)
    let (|ConcreteOneDimArrayZero|_|) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero inner -> Some inner
        | _ -> None

    /// Active pattern to match general array types, returning (element, rank).
    let (|ConcreteArray|_|) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Array (inner, rank) -> Some (inner, rank)
        | _ -> None

    /// Active pattern to match function pointer types, returning the concretized signature.
    let (|ConcreteFunctionPointer|_|) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.FunctionPointer signature -> Some signature
        | _ -> None

type IAssemblyLoad =
    /// <param name="referencedIn">
    /// The <em>definition</em> identity of the assembly whose AssemblyReference table
    /// <c>handle</c> indexes. AssemblyReferenceHandles are only meaningful relative to the
    /// assembly that declares them.
    /// </param>
    abstract LoadAssembly :
        loadedAssemblies : LoadedAssemblies ->
        referencedIn : AssemblyName ->
        handle : AssemblyReferenceHandle ->
            LoadedAssemblies * DumpedAssembly

[<RequireQualifiedAccess>]
module IAssemblyLoad =
    /// <summary>
    /// An <c>IAssemblyLoad</c> which refuses to go to disk: it binds an AssemblyReference only if
    /// the load context already holds the assembly. Use it where everything that could possibly
    /// be needed has provably been loaded already, so that a miss is a bug rather than a cue to
    /// read a file.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The proof must be evident *at the call site* — typically because every type reachable from
    /// the inputs lives in an assembly you are holding, as in <c>Corelib.concretizeAll</c>, which
    /// touches only corelib types. Do not use it to encode "some earlier sweep primed this": that
    /// is a claim about the whole interpreter, it cannot be checked here, and it is exactly the
    /// claim that rotted in issue #868, where <c>CliType.zeroOf</c> asserted it and a struct's
    /// field type turned out to live in an assembly the guest never named.
    /// </para>
    /// <para>
    /// The remaining uses that do rest on an upstream sweep are the handful of layout helpers
    /// which return a bare value with nowhere to put an updated load context or concrete-type
    /// registry (<c>MethodState.Empty</c>, <c>IlMachineManagedByref.zeroForConcreteType</c>,
    /// <c>ManagedPointerByteView.arrayElementSize</c>). Each says so at its call site. They keep
    /// this loader on purpose: failing loudly beats silently re-reading an assembly and
    /// discarding the handles minted from it.
    /// </para>
    /// </remarks>
    let alreadyLoadedOnly : IAssemblyLoad =
        { new IAssemblyLoad with
            member _.LoadAssembly loaded referencedIn handle =
                let targetRef = loaded.[referencedIn].AssemblyReferences.[handle]

                match loaded.TryResolveReference targetRef with
                | Some target -> loaded, target
                | None ->
                    failwithf
                        "Assembly %s, referenced by %s, is not loaded, and this context is not permitted to load it."
                        targetRef.Name.FullName
                        referencedIn.FullName
        }

[<RequireQualifiedAccess>]
module TypeConcretization =
    type ConcretizationContext<'corelib> =
        {
            /// All concrete types created so far
            ConcreteTypes : AllConcreteTypes
            /// For resolving type references
            LoadedAssemblies : LoadedAssemblies
            BaseTypes : BaseClassTypes<'corelib>
        }

    // Helper function to find existing types by canonical nominal identity and generics
    let private findExistingType
        (concreteTypes : AllConcreteTypes)
        (identity : ResolvedTypeIdentity)
        (generics : ConcreteTypeHandle ImmutableArray)
        : ConcreteTypeHandle option
        =
        AllConcreteTypes.findExistingConcreteType concreteTypes identity generics

    // Helper function for primitive types (convenience wrapper)
    let private findExistingPrimitiveType
        (concreteTypes : AllConcreteTypes)
        (identity : ResolvedTypeIdentity)
        : ConcreteTypeHandle option
        =
        findExistingType concreteTypes identity ImmutableArray.Empty

    // Helper function to create and add a ConcreteType to the context
    let private createAndAddConcreteType
        (ctx : ConcretizationContext<'corelib>)
        (identity : ResolvedTypeIdentity)
        (ns : string)
        (name : string)
        (generics : ConcreteTypeHandle ImmutableArray)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =
        let concreteType = ConcreteType.makeFromIdentity identity ns name generics

        let handle, newConcreteTypes = AllConcreteTypes.add concreteType ctx.ConcreteTypes

        let newCtx =
            { ctx with
                ConcreteTypes = newConcreteTypes
            }

        handle, newCtx

    // Helper function for assembly loading with retry pattern
    let private loadAssemblyAndResolveTypeRef
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<'corelib>)
        (currentAssembly : AssemblyName)
        (typeRef : TypeRef)
        : (DumpedAssembly * ResolvedTypeIdentity * WoofWare.PawPrint.TypeInfo<_, _>) * ConcretizationContext<'corelib>
        =
        let rec go
            (ctx : ConcretizationContext<'corelib>)
            : (DumpedAssembly * ResolvedTypeIdentity * WoofWare.PawPrint.TypeInfo<_, _>) *
              ConcretizationContext<'corelib>
            =
            let currentAssy = ctx.LoadedAssemblies.[currentAssembly]

            match Assembly.resolveTypeRef ctx.LoadedAssemblies currentAssy ImmutableArray.Empty typeRef with
            | TypeResolutionResult.Resolved (targetAssy, identity, typeInfo) -> (targetAssy, identity, typeInfo), ctx
            | TypeResolutionResult.FirstLoadAssy assemblyRef ->
                let handle, referencedIn = assemblyRef.Handle

                let newAssemblies, _ =
                    loadAssembly.LoadAssembly ctx.LoadedAssemblies referencedIn handle

                let newCtx =
                    { ctx with
                        LoadedAssemblies =
                            LoadedAssemblies.assertReferenceBound
                                $"type reference %s{typeRef.Name}"
                                assemblyRef
                                newAssemblies
                    }

                go newCtx

        go ctx

    let private concretizePrimitive
        (ctx : ConcretizationContext<'corelib>)
        (prim : PrimitiveType)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =

        // Get the TypeInfo for this primitive from BaseClassTypes
        let typeInfo =
            match prim with
            | PrimitiveType.Boolean -> ctx.BaseTypes.Boolean
            | PrimitiveType.Char -> ctx.BaseTypes.Char
            | PrimitiveType.SByte -> ctx.BaseTypes.SByte
            | PrimitiveType.Byte -> ctx.BaseTypes.Byte
            | PrimitiveType.Int16 -> ctx.BaseTypes.Int16
            | PrimitiveType.UInt16 -> ctx.BaseTypes.UInt16
            | PrimitiveType.Int32 -> ctx.BaseTypes.Int32
            | PrimitiveType.UInt32 -> ctx.BaseTypes.UInt32
            | PrimitiveType.Int64 -> ctx.BaseTypes.Int64
            | PrimitiveType.UInt64 -> ctx.BaseTypes.UInt64
            | PrimitiveType.Single -> ctx.BaseTypes.Single
            | PrimitiveType.Double -> ctx.BaseTypes.Double
            | PrimitiveType.String -> ctx.BaseTypes.String
            | PrimitiveType.Object -> ctx.BaseTypes.Object
            | PrimitiveType.TypedReference -> ctx.BaseTypes.TypedReference
            | PrimitiveType.IntPtr -> ctx.BaseTypes.IntPtr
            | PrimitiveType.UIntPtr -> ctx.BaseTypes.UIntPtr

        // Check if we've already concretized this primitive type
        let identity =
            ResolvedTypeIdentity.ofTypeDefinition typeInfo.Assembly typeInfo.TypeDefHandle

        match findExistingPrimitiveType ctx.ConcreteTypes identity with
        | Some handle -> handle, ctx
        | None ->
            // Create and add the concrete type (primitives have no generic arguments)
            createAndAddConcreteType ctx identity typeInfo.Namespace typeInfo.Name ImmutableArray.Empty // Primitives have no generic parameters

    let private concretizeArray
        (ctx : ConcretizationContext<'corelib>)
        (elementHandle : ConcreteTypeHandle)
        (rank : int)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =
        ConcreteTypeHandle.Array (elementHandle, rank), ctx

    let private concretizeOneDimArray
        (ctx : ConcretizationContext<'corelib>)
        (elementHandle : ConcreteTypeHandle)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =
        ConcreteTypeHandle.OneDimArrayZero elementHandle, ctx

    let concretizeTypeDefinition
        (ctx : ConcretizationContext<'corelib>)
        (identity : ResolvedTypeIdentity)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =

        let assembly = ctx.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName

        let typeInfo = Assembly.resolveTypeIdentityDefinition assembly identity

        if not typeInfo.Generics.IsEmpty then
            failwithf
                "Cannot concretize open generic type %s.%s - it has %d generic parameters"
                typeInfo.Namespace
                typeInfo.Name
                typeInfo.Generics.Length

        match findExistingType ctx.ConcreteTypes identity ImmutableArray.Empty with
        | Some handle -> handle, ctx
        | None ->
            // Create and add the concrete type (no generic arguments since it's not generic)
            createAndAddConcreteType ctx identity typeInfo.Namespace typeInfo.Name ImmutableArray.Empty // No generic parameters

    let private concretizeTypeReference
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<'corelib>)
        (currentAssembly : AssemblyName)
        (typeRef : TypeRef)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =
        // Use the helper to load assembly and resolve the type reference
        let (targetAssy, identity, typeInfo), ctx =
            loadAssemblyAndResolveTypeRef loadAssembly ctx currentAssembly typeRef

        // Check if this type has generic parameters
        if not typeInfo.Generics.IsEmpty then
            failwithf
                "Cannot concretize type reference to open generic type %s.%s - it has %d generic parameters"
                typeInfo.Namespace
                typeInfo.Name
                typeInfo.Generics.Length

        // Create or find the concrete type
        match findExistingType ctx.ConcreteTypes identity ImmutableArray.Empty with
        | Some handle -> handle, ctx
        | None -> createAndAddConcreteType ctx identity typeInfo.Namespace typeInfo.Name ImmutableArray.Empty

    /// Does this signature type carry an ECMA-335 custom modifier (`modreq`/`modopt`) anywhere
    /// inside it? Used only to refuse the one position where dropping a modifier is unrecoverable:
    /// see `concretizeGenericInstantiation`.
    let rec private carriesCustomModifier (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | TypeDefn.Modified _ -> true
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> carriesCustomModifier element
        | TypeDefn.GenericInstantiation (generic, args) ->
            carriesCustomModifier generic || (args |> Seq.exists carriesCustomModifier)
        | TypeDefn.FunctionPointer signature ->
            (match signature.ReturnType with
             | MethodReturnType.Void -> false
             | MethodReturnType.Returns ty -> carriesCustomModifier ty)
            || (signature.ParameterTypes |> List.exists carriesCustomModifier)
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _
        | TypeDefn.Void -> false

    /// Concretize a type in a specific generic context
    let rec concretizeType
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (typeDefn : TypeDefn)
        : ConcreteTypeHandle * ConcretizationContext<DumpedAssembly>
        =
        match typeDefn with
        | TypeDefn.PrimitiveType prim -> concretizePrimitive ctx prim

        | TypeDefn.Array (elementType, rank) ->
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            concretizeArray ctx elementHandle rank

        | TypeDefn.OneDimensionalArrayLowerBoundZero elementType ->
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            concretizeOneDimArray ctx elementHandle

        | TypeDefn.GenericTypeParameter index ->
            if index < typeGenerics.Length then
                typeGenerics.[index], ctx
            else
                raise (IndexOutOfRangeException $"Generic type parameter %i{index}")

        | TypeDefn.GenericMethodParameter index ->
            if index < methodGenerics.Length then
                methodGenerics.[index], ctx
            else
                raise (IndexOutOfRangeException $"Generic method parameter %i{index}")

        | TypeDefn.GenericInstantiation (genericDef, args) ->
            concretizeGenericInstantiation ctx loadAssembly assembly typeGenerics methodGenerics genericDef args

        | TypeDefn.FromDefinition (identity, _) -> concretizeTypeDefinition ctx identity

        | TypeDefn.FromReference (typeRef, _) -> concretizeTypeReference loadAssembly ctx assembly typeRef

        | TypeDefn.Byref elementType ->
            // Byref types are managed references to other types
            // First concretize the element type
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            // Return a Byref constructor wrapping the element type
            ConcreteTypeHandle.Byref elementHandle, ctx

        | TypeDefn.Pointer elementType ->
            // Pointer types are unmanaged pointers to other types
            // First concretize the element type
            let elementHandle, ctx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

            // Return a Pointer constructor wrapping the element type
            ConcreteTypeHandle.Pointer elementHandle, ctx

        | TypeDefn.Pinned elementType ->
            // `pinned` is a GC-pinning annotation that appears on local-variable signatures emitted
            // by the C# `fixed` statement. In the real CLR it tells the GC not to relocate the
            // referent; we have no moving GC, so pinning is semantically a no-op and the element
            // type's own concretization handle is the right representation.
            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics elementType

        | TypeDefn.Modified m ->
            // Custom modifiers are metadata annotations on the signature. Runtime type
            // identity and storage shape follow the unmodified type.
            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics m.Unmodified

        | TypeDefn.Void ->
            // Method return signatures represent `void` separately from runtime types.
            // Type signatures can still mention `void`, for example as the element type of `void*`.
            // In those positions, System.Void is the canonical concrete type identity.
            let voidTypeInfo = ctx.BaseTypes.Void

            match
                findExistingType
                    ctx.ConcreteTypes
                    (ResolvedTypeIdentity.ofTypeDefinition voidTypeInfo.Assembly voidTypeInfo.TypeDefHandle)
                    ImmutableArray.Empty
            with
            | Some handle -> handle, ctx
            | None ->
                // Create and add the concrete Void type
                createAndAddConcreteType
                    ctx
                    (ResolvedTypeIdentity.ofTypeDefinition voidTypeInfo.Assembly voidTypeInfo.TypeDefHandle)
                    voidTypeInfo.Namespace
                    voidTypeInfo.Name
                    ImmutableArray.Empty // Void has no generic parameters

        | TypeDefn.FunctionPointer signature ->
            // Function pointer types are structural: the signature is the type identity.
            // Concretize each parameter and the return type under the current generic context.
            let concretized, ctx =
                concretizeMethodSignature ctx loadAssembly assembly typeGenerics methodGenerics signature

            ConcreteTypeHandle.FunctionPointer concretized, ctx

    and concretizeMethodSignature
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (signature : TypeMethodSignature<TypeDefn>)
        : TypeMethodSignature<ConcreteTypeHandle> * ConcretizationContext<DumpedAssembly>
        =
        // Concretize return type only when the method actually returns a value.
        let ctx, returnType =
            MethodReturnType.map
                ctx
                (fun ctx ty ->
                    let handle, ctx =
                        concretizeType ctx loadAssembly assembly typeGenerics methodGenerics ty

                    ctx, handle
                )
                signature.ReturnType

        let paramHandles = ResizeArray<ConcreteTypeHandle> signature.ParameterTypes.Length
        let mutable ctx = ctx

        for paramType in signature.ParameterTypes do
            let handle, newCtx =
                concretizeType ctx loadAssembly assembly typeGenerics methodGenerics paramType

            paramHandles.Add handle
            ctx <- newCtx

        let concretized =
            {
                Header = signature.Header
                ReturnType = returnType
                ParameterTypes = paramHandles |> List.ofSeq
                GenericParameterCount = signature.GenericParameterCount
                RequiredParameterCount = signature.RequiredParameterCount
            }

        concretized, ctx

    and private concretizeGenericInstantiation
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (genericDef : TypeDefn)
        (args : ImmutableArray<TypeDefn>)
        : ConcreteTypeHandle * ConcretizationContext<DumpedAssembly>
        =
        // A custom modifier on a type *argument* is unrecoverable once substitution has happened.
        // Elsewhere, dropping `TypeDefn.Modified` is correct and deliberate: runtime type identity
        // follows the unmodified type, and the places that must compare modifiers -- vtable slot
        // matching in `NativeRuntimeTypeHelpers` -- walk the unsubstituted signature alongside the
        // handle to recover them. That parallel channel cannot reach a modifier that arrived *via*
        // a generic argument: for `Base<int modopt(X)>.M(!0)`, the signature the walk sees is the
        // bare `!0` and the `modopt` lives only in the instantiation, so it silently vanishes.
        // CoreCLR compares the substituted blob and does see it (`MetaSig::CompareElementType`), so
        // conflating the two would let a derived `M(int)` take over a slot it does not override.
        //
        // Refuse instead. This is unreachable from any real compiler: measured over the linux-x64
        // runtime pack, FSharp.Core, the Roslyn assemblies and this repo's own test binaries -- 208
        // assemblies -- not one generic instantiation carries a modified type argument, nested or
        // otherwise. It takes C++/CLI or hand-written IL to produce one.
        args
        |> Seq.iter (fun arg ->
            if carriesCustomModifier arg then
                failwithf
                    "TODO: generic instantiation in %s has a type argument carrying a custom modifier (%O); ConcreteTypeHandle cannot represent it, and unlike a modifier written directly in a signature it cannot be recovered by walking the unsubstituted signature either, so accepting it would make this instantiation compare equal to the unmodified one"
                    assembly.FullName
                    arg
        )

        // First, concretize all type arguments
        let argHandles, ctxAfterArgs =
            args
            |> Seq.fold
                (fun (handles, ctx) arg ->
                    let handle, ctx =
                        concretizeType ctx loadAssembly assembly typeGenerics methodGenerics arg

                    handle :: handles, ctx
                )
                ([], ctx)

        let argHandles = argHandles |> Seq.rev |> ImmutableArray.CreateRange

        // Get the base type definition
        let baseIdentity, baseNamespace, baseName, ctxAfterArgs =
            match genericDef with
            | FromDefinition (identity, _) ->
                let currentAssy =
                    ctxAfterArgs.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName

                let typeDef = Assembly.resolveTypeIdentityDefinition currentAssy identity
                identity, typeDef.Namespace, typeDef.Name, ctxAfterArgs
            | FromReference (typeRef, _) ->
                let (_, identity, typeInfo), ctxWithResolvedType =
                    loadAssemblyAndResolveTypeRef loadAssembly ctxAfterArgs assembly typeRef

                identity, typeInfo.Namespace, typeInfo.Name, ctxWithResolvedType
            | _ -> failwithf "Generic instantiation of %A not supported" genericDef

        // Check if this exact generic instantiation already exists
        match findExistingType ctxAfterArgs.ConcreteTypes baseIdentity argHandles with
        | Some existingHandle ->
            // Type already exists, return it
            existingHandle, ctxAfterArgs
        | None ->
            let concreteType =
                ConcreteType.makeFromIdentity baseIdentity baseNamespace baseName argHandles

            let handle, newConcreteTypes =
                AllConcreteTypes.add concreteType ctxAfterArgs.ConcreteTypes

            handle,
            { ctxAfterArgs with
                ConcreteTypes = newConcreteTypes
            }

/// High-level API for concretizing types
[<RequireQualifiedAccess>]
module Concretization =

    /// Helper to concretize an array of types
    let private concretizeTypeArray
        (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        (types : ImmutableArray<TypeDefn>)
        : ImmutableArray<ConcreteTypeHandle> * TypeConcretization.ConcretizationContext<DumpedAssembly>
        =

        let handles = ImmutableArray.CreateBuilder types.Length
        let mutable ctx = ctx

        for i = 0 to types.Length - 1 do
            let handle, newCtx =
                TypeConcretization.concretizeType ctx loadAssembly assembly typeArgs methodArgs types.[i]

            handles.Add handle
            ctx <- newCtx

        handles.ToImmutable (), ctx

    /// Helper to concretize a method signature
    let private concretizeMethodSignature
        (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        (signature : TypeMethodSignature<TypeDefn>)
        : TypeMethodSignature<ConcreteTypeHandle> * TypeConcretization.ConcretizationContext<DumpedAssembly>
        =
        TypeConcretization.concretizeMethodSignature ctx loadAssembly assembly typeArgs methodArgs signature

    let rec private ensureTypeRefResolved
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (sourceAssembly : DumpedAssembly)
        (typeRef : TypeRef)
        : LoadedAssemblies * DumpedAssembly * TypeDefinitionHandle
        =
        match Assembly.resolveTypeRef assemblies sourceAssembly ImmutableArray.Empty typeRef with
        | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
            assemblies, resolvedAssembly, resolvedType.TypeDefHandle
        | TypeResolutionResult.FirstLoadAssy assemblyRef ->
            let handle, referencedIn = assemblyRef.Handle
            let newAssemblies, _ = loadAssembly.LoadAssembly assemblies referencedIn handle

            let newAssemblies =
                LoadedAssemblies.assertReferenceBound $"base type reference %s{typeRef.Name}" assemblyRef newAssemblies

            let refreshedSourceAssembly = newAssemblies.[sourceAssembly.Name]
            ensureTypeRefResolved loadAssembly newAssemblies refreshedSourceAssembly typeRef

    let rec private ensureTypeDefnResolved
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (sourceAssembly : DumpedAssembly)
        (ty : TypeDefn)
        : LoadedAssemblies * DumpedAssembly * TypeDefinitionHandle
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, _) ->
            ensureTypeDefnResolved loadAssembly assemblies sourceAssembly generic
        // A custom modifier annotates the signature; the type definition being named is the
        // unmodified one. Stepping into `Modifier` would resolve `InAttribute`/`IsVolatile`/etc.
        | TypeDefn.Modified m -> ensureTypeDefnResolved loadAssembly assemblies sourceAssembly m.Unmodified
        | TypeDefn.FromDefinition (identity, _) ->
            let resolvedAssembly = assemblies.ByDefinitionName identity.AssemblyFullName
            assemblies, resolvedAssembly, identity.TypeDefinition.Get
        | TypeDefn.FromReference (typeRef, _) -> ensureTypeRefResolved loadAssembly assemblies sourceAssembly typeRef
        | unexpected ->
            failwithf
                "Unexpected TypeDefn shape while resolving base type from %s: %O"
                sourceAssembly.Name.FullName
                unexpected

    /// <remarks>
    /// This threads the <c>DumpedAssembly</c> itself rather than its <c>AssemblyName</c>, and
    /// deliberately so: <c>LoadedAssemblies</c> is keyed by definition <em>full name</em>, and
    /// <c>AssemblyName.FullName</c> re-formats that string from its components on every single
    /// access. This walk runs on the type-resolution hot path, so a lookup per link is not free.
    /// Each step already holds the assembly it needs — for a TypeDef link it is the same one, and
    /// for a TypeRef/TypeSpec link the resolver hands back the canonical instance.
    /// </remarks>
    let rec private ensureBaseTypeAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (assy : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo option)
        : LoadedAssemblies
        =
        match baseTypeInfo with
        | None -> assemblies
        | Some (BaseTypeInfo.TypeDef handle) ->
            let baseType = assy.TypeDefs.[handle]
            ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy baseType.BaseType
        | Some (BaseTypeInfo.TypeRef handle) ->
            let typeRef = assy.TypeRefs.[handle]

            let newAssemblies, resolvedAssembly, resolvedHandle =
                ensureTypeRefResolved loadAssembly assemblies assy typeRef

            let resolvedType = resolvedAssembly.TypeDefs.[resolvedHandle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies resolvedAssembly resolvedType.BaseType
        | Some (BaseTypeInfo.TypeSpec handle) ->
            let typeSpec = assy.TypeSpecs.[handle].Signature

            let newAssemblies, resolvedAssembly, resolvedHandle =
                ensureTypeDefnResolved loadAssembly assemblies assy typeSpec

            let resolvedType = resolvedAssembly.TypeDefs.[resolvedHandle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies resolvedAssembly resolvedType.BaseType

    /// Load every assembly reachable from the base-type chain of the given type definition.
    /// <paramref name="assy"/> must be the canonical instance for the assembly which defines it.
    let ensureTypeDefinitionBaseAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (assy : DumpedAssembly)
        (typeDefinitionHandle : TypeDefinitionHandle)
        : LoadedAssemblies
        =
        let typeDef = assy.TypeDefs.[typeDefinitionHandle]
        ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy typeDef.BaseType

    /// Force-load every assembly needed for CliType.zeroOf to zero-initialise the
    /// given concrete handle. zeroOf calls DumpedAssembly.isValueType on the top
    /// type to decide between a zeroed value-type layout and a null reference; if
    /// the type turns out to be a value type, zeroOf recursively zeros each
    /// non-static field, which repeats the same isValueType decision on the field
    /// type. Every one of those isValueType walks fails hard if a TypeRef along
    /// its base chain points at an assembly which has not yet been loaded.
    ///
    /// This helper mirrors zeroOf's traversal exactly:
    ///   * Byref/Pointer/Array/OneDimArrayZero/FunctionPointer wrapper handles
    ///     terminate in zeroOf without inspecting their component types, so we
    ///     don't recurse into them here either. Recursing would follow paths
    ///     zeroOf never takes, and — for recursively constructed but legal types
    ///     such as `struct S<T> { S<S<T>>[] Items; }` — that expansion would
    ///     stack-overflow because every synthesised instantiation is a distinct
    ///     handle the visited-set can't collapse.
    ///   * A nominal reference type also terminates in zeroOf (as `ObjectRef
    ///     None`), so once we've loaded its own base chain we stop; we do NOT
    ///     recurse into its generic arguments or fields.
    ///   * A nominal value type is the only case where zeroOf recurses into
    ///     fields. Each non-static field's TypeDefn is concretized under the
    ///     outer type's generic context (that's how generic-parameter
    ///     substitution happens) and then walked. Generic arguments only need
    ///     priming to the extent they surface as field types, so we don't visit
    ///     them separately.
    ///
    /// concretizeMethod calls this on every ConcreteTypeHandle a subsequent
    /// zeroOf could encounter for a given method — locals, parameter and return
    /// types, plus the method's own and declaring type's generic arguments,
    /// which some intrinsics feed directly into cliTypeZeroOfHandle. A single
    /// `visited` set is shared across the sweep so no handle is walked twice.
    let rec ensureBaseAssembliesLoadedForConcreteHandle
        (loadAssembly : IAssemblyLoad)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (visited : System.Collections.Generic.HashSet<ConcreteTypeHandle>)
        (assemblies : LoadedAssemblies)
        (concreteTypes : AllConcreteTypes)
        (handle : ConcreteTypeHandle)
        : LoadedAssemblies * AllConcreteTypes
        =
        if not (visited.Add handle) then
            assemblies, concreteTypes
        else
            match handle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // Terminal in zeroOf — see the doc comment above.
                assemblies, concreteTypes
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle concreteTypes with
                | None -> assemblies, concreteTypes
                | Some concreteType ->
                    let assemblies =
                        ensureTypeDefinitionBaseAssembliesLoaded
                            loadAssembly
                            assemblies
                            assemblies.[concreteType.Assembly]
                            concreteType.Definition.Get

                    let outerAssembly = assemblies.[concreteType.Assembly]
                    let outerTypeDef = outerAssembly.TypeDefs.[concreteType.Definition.Get]

                    // Reference types terminate in zeroOf as null; fields (and,
                    // by extension, generic arguments only reachable via fields)
                    // are never inspected. Do NOT descend — descent into a
                    // reference type's generics or fields can loop forever on
                    // legal shapes such as `class Box<T> {}` used inside
                    // `struct S<T> { Box<S<S<T>>> F; }`.
                    if not (DumpedAssembly.isValueType baseTypes assemblies outerTypeDef) then
                        assemblies, concreteTypes
                    else
                        // Value type: zeroOf recurses into every non-static
                        // instance field, so each field type's own base-chain
                        // assemblies must also be loaded. Concretize each
                        // field's TypeDefn under the outer type's generic
                        // context (this covers any generic-parameter uses
                        // inside the field type), then recurse.
                        outerTypeDef.Fields
                        |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                        |> List.fold
                            (fun (assemblies, concreteTypes) field ->
                                let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                                    {
                                        TypeConcretization.ConcretizationContext.ConcreteTypes = concreteTypes
                                        TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                                        TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
                                    }

                                // Fields never carry method-level generics; the
                                // outer type's already-concretized generic
                                // arguments cover the field's substitution.
                                let fieldHandle, ctx =
                                    TypeConcretization.concretizeType
                                        ctx
                                        loadAssembly
                                        concreteType.Assembly
                                        concreteType.Generics
                                        ImmutableArray.Empty
                                        field.Signature

                                ensureBaseAssembliesLoadedForConcreteHandle
                                    loadAssembly
                                    baseTypes
                                    visited
                                    ctx.LoadedAssemblies
                                    ctx.ConcreteTypes
                                    fieldHandle
                            )
                            (assemblies, concreteTypes)

    /// Concretize a method's signature and body
    let concretizeMethod
        (ctx : AllConcreteTypes)
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          AllConcreteTypes *
          LoadedAssemblies
        =

        // Concretization walks the declaring type's metadata throughout -- its base-type closure, its
        // TypeDef row, its instantiation -- so it cannot proceed without one. A method minted by
        // `Reflection.Emit` never arrives here: nothing concretizes it, because its signature and
        // body are concretized directly from the registry at the point it is invoked.
        let declaringType =
            MethodOwner.requireDeclaringType "concretizing a method" method.Owner

        // Ensure base type assemblies are loaded for the declaring type
        let assemblies =
            ensureTypeDefinitionBaseAssembliesLoaded
                loadAssembly
                assemblies
                assemblies.[declaringType.Assembly]
                declaringType.Definition.Get

        let concCtx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = ctx
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
            }

        // First, we need to create a TypeDefn for the declaring type with its generics instantiated
        let declaringTypeDefn =
            if declaringType._Generics.IsEmpty then
                // Non-generic type - determine the SignatureTypeKind
                let assy = concCtx.LoadedAssemblies.[declaringType.Assembly]
                let arg = assy.TypeDefs.[declaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseTypes concCtx.LoadedAssemblies arg

                TypeDefn.FromDefinition (declaringType.Identity, signatureTypeKind)
            else
                // Generic type - create a GenericInstantiation
                let assy = concCtx.LoadedAssemblies.[declaringType.Assembly]
                let arg = assy.TypeDefs.[declaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseTypes concCtx.LoadedAssemblies arg

                let baseType = TypeDefn.FromDefinition (declaringType.Identity, signatureTypeKind)

                let genericArgsLength = declaringType.Generics.Length

                if genericArgsLength > typeArgs.Length then
                    failwithf
                        "Method declaring type expects %d generic arguments but only %d provided"
                        genericArgsLength
                        typeArgs.Length

                let genericArgs =
                    typeArgs.Slice (0, genericArgsLength)
                    |> Seq.mapi (fun i _ -> TypeDefn.GenericTypeParameter i)
                    |> ImmutableArray.CreateRange

                TypeDefn.GenericInstantiation (baseType, genericArgs)

        // Concretize the declaring type
        let declaringHandle, concCtx =
            TypeConcretization.concretizeType
                concCtx
                loadAssembly
                declaringType.Assembly
                typeArgs
                methodArgs
                declaringTypeDefn

        // Look up the concretized declaring type
        let concretizedDeclaringType =
            AllConcreteTypes.lookup declaringHandle concCtx.ConcreteTypes |> Option.get

        // Concretize signature
        let signature, concCtx =
            concretizeMethodSignature concCtx loadAssembly declaringType.Assembly typeArgs methodArgs method.Signature

        // Concretize local variables (only IL bodies carry them).
        let body, concCtx2 =
            match method.Body with
            | MethodBody.Il instr ->
                let locals, updatedCtx =
                    match instr.LocalVars with
                    | None -> None, concCtx
                    | Some vars ->
                        let handles, ctx =
                            concretizeTypeArray concCtx loadAssembly declaringType.Assembly typeArgs methodArgs vars

                        Some handles, ctx

                MethodBody.Il (MethodInstructions.setLocalVars locals instr), updatedCtx
            | MethodBody.InternalCall -> MethodBody.InternalCall, concCtx
            | MethodBody.PInvoke -> MethodBody.PInvoke, concCtx
            | MethodBody.RuntimeProvided rb -> MethodBody.RuntimeProvided rb, concCtx
            | MethodBody.Abstract -> MethodBody.Abstract, concCtx

        // Map generics to handles
        let genericHandles =
            method.Generics
            |> ImmutableArray.map (fun (gp, md) -> methodArgs.[gp.SequenceNumber])

        // Concretization rewrites only the universal facts — declaring type, body, generics,
        // signature — so whichever tail the method carries passes through untouched.
        let concretizedMethod : MethodInfo<_, _, _> =
            method
            |> MethodInfo.mapCore (fun core ->
                {
                    Owner = MethodOwner.DeclaredOn concretizedDeclaringType
                    Name = core.Name
                    Body = body
                    Generics = genericHandles
                    Signature = signature
                    IsStatic = core.IsStatic
                }
            )

        // Every ConcreteTypeHandle this method emits is subsequently fed to
        // CliType.zeroOf: locals when the frame is set up (MethodState.Empty),
        // each parameter handle when the caller coerces arguments before invoke
        // (IlMachineStateExecution.callMethod), and the return handle when a
        // non-void method returns (IlMachineThreadState.ret). Some intrinsics
        // — Unsafe.SizeOf<T>, Span<T>.Clear, and their siblings — also feed
        // handles from MethodInfo.Generics and DeclaringType.Generics directly
        // into zeroOf without those handles ever appearing in the signature.
        // zeroOf's base-type walk crashes if a TypeRef along the chain points
        // at an unloaded assembly, so we make sure every such assembly is
        // loaded now — while we still hold the IAssemblyLoad capability —
        // rather than deferring to the effectful edge that only sees a strict
        // assembly-dictionary.
        let visited = System.Collections.Generic.HashSet<ConcreteTypeHandle> ()
        let assemblies = concCtx2.LoadedAssemblies
        let concreteTypes = concCtx2.ConcreteTypes

        let primeHandle (assemblies, concreteTypes) h =
            ensureBaseAssembliesLoadedForConcreteHandle loadAssembly baseTypes visited assemblies concreteTypes h

        let assemblies, concreteTypes =
            signature.ParameterTypes |> List.fold primeHandle (assemblies, concreteTypes)

        let assemblies, concreteTypes =
            match signature.ReturnType with
            | MethodReturnType.Void -> assemblies, concreteTypes
            | MethodReturnType.Returns h -> primeHandle (assemblies, concreteTypes) h

        let assemblies, concreteTypes =
            match body with
            | MethodBody.Il instr ->
                match instr.LocalVars with
                | None -> assemblies, concreteTypes
                | Some vars -> vars |> Seq.fold primeHandle (assemblies, concreteTypes)
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _
            | MethodBody.Abstract -> assemblies, concreteTypes

        // Method-level generic arguments (Unsafe.SizeOf<T> etc read T here).
        let assemblies, concreteTypes =
            genericHandles |> Seq.fold primeHandle (assemblies, concreteTypes)

        // Declaring-type generic arguments (Span<T>.Clear etc read T here).
        let assemblies, concreteTypes =
            concretizedDeclaringType.Generics
            |> Seq.fold primeHandle (assemblies, concreteTypes)

        concretizedMethod, concreteTypes, assemblies

    let rec concreteHandleToTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (concreteTypes : AllConcreteTypes)
        (assemblies : LoadedAssemblies)
        : TypeDefn
        =
        match handle with
        | ConcreteTypeHandle.Byref elementHandle ->
            let elementType =
                concreteHandleToTypeDefn baseClassTypes elementHandle concreteTypes assemblies

            TypeDefn.Byref elementType
        | ConcreteTypeHandle.Pointer elementHandle ->
            let elementType =
                concreteHandleToTypeDefn baseClassTypes elementHandle concreteTypes assemblies

            TypeDefn.Pointer elementType
        | ConcreteTypeHandle.OneDimArrayZero elementHandle ->
            let elementType =
                concreteHandleToTypeDefn baseClassTypes elementHandle concreteTypes assemblies

            TypeDefn.OneDimensionalArrayLowerBoundZero elementType
        | ConcreteTypeHandle.Array (elementHandle, rank) ->
            let elementType =
                concreteHandleToTypeDefn baseClassTypes elementHandle concreteTypes assemblies

            TypeDefn.Array (elementType, rank)
        | ConcreteTypeHandle.FunctionPointer signature ->
            let _, mapped =
                TypeMethodSignature.map
                    ()
                    (fun () h -> (), concreteHandleToTypeDefn baseClassTypes h concreteTypes assemblies)
                    signature

            TypeDefn.FunctionPointer mapped
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.tryTypeInfo assemblies concreteTypes handle with
            | None -> failwith "Logic error: handle not found"
            | Some (concreteType, typeDef) ->

            // Determine SignatureTypeKind
            let signatureTypeKind =
                DumpedAssembly.signatureTypeKind baseClassTypes assemblies typeDef

            if concreteType.Generics.IsEmpty then
                TypeDefn.FromDefinition (concreteType.Identity, signatureTypeKind)
            else
                // Recursively convert generic arguments
                let genericArgs =
                    concreteType.Generics
                    |> ImmutableArray.map (fun h -> concreteHandleToTypeDefn baseClassTypes h concreteTypes assemblies)

                let baseDef = TypeDefn.FromDefinition (concreteType.Identity, signatureTypeKind)

                TypeDefn.GenericInstantiation (baseDef, genericArgs)
