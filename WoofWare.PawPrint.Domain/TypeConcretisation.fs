namespace WoofWare.PawPrint

open System
open System.Collections.Generic
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

    /// Matches a concrete type by namespace, name and generic arguments, in whatever assembly
    /// declares it. Prefer `CorelibType` where the declaring assembly is known: a namespace and
    /// name alone do not identify a type, and two assemblies may spell the same one.
    let (|NamedType|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct -> Some (ct.Namespace, ct.Name, ct.Generics)
            | None -> None
        | _ -> None

    /// Matches a concrete type declared by CoreLib, by namespace, name and generic arguments.
    ///
    /// Yields no assembly name: every caller of the general form spelled CoreLib, so the name
    /// went straight back into a comparison against the literal below.
    let (|CorelibType|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct when ct.Assembly.Name = "System.Private.CoreLib" -> Some (ct.Namespace, ct.Name, ct.Generics)
            | _ -> None
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
            // A GENERIC calling convention on a *function pointer* is refused here, the one place a
            // function-pointer handle is minted, so that no such handle can exist further in. Comparing
            // two of them is a question CoreCLR answers by reading `argCnt` where the blob holds the
            // generic-parameter count and then comparing `GenParamCount + 1` elements of the resulting
            // misaligned stream (siginfo.cpp:4135-4168), which cannot be reproduced from a decoded
            // signature — see the FNPTR arm of `compareElements`, which refuses the spelled form.
            //
            // No compiler emits this and no reflection API can name it, so this is an assertion of that
            // belief rather than a case to handle: if something does produce one, it should say so here,
            // at the point of construction and with the assembly in hand, rather than be discovered
            // later by whatever consumes the handle.
            //
            // Note this is a function-pointer *type*, not a generic method: `concretizeMethodSignature`
            // serves ordinary generic methods, whose GENERIC calling convention is entirely normal.
            if signature.Header.Get.IsGeneric then
                failwithf
                    "TODO: concretising a function pointer type whose signature carries the GENERIC calling convention, in %s. No compiler emits such a type and no reflection API can name one, so this indicates either hand-authored metadata or a decoder fault; signature comparison could not answer for it in any case."
                    assembly.FullName

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
        let ctx, returnType =
            concretizeReturnColumn ctx loadAssembly assembly typeGenerics methodGenerics signature.ReturnType

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

    /// Concretise a method's return column, which is where a `void` under custom modifiers becomes
    /// <c>MethodReturnType.Void</c>. Every consumer of a concretised return shape must go through
    /// this, or two of them can disagree about whether such a method returns a value.
    and concretizeReturnColumn
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (returnType : MethodReturnType<TypeDefn>)
        : ConcretizationContext<DumpedAssembly> * MethodReturnType<ConcreteTypeHandle>
        =
        // A decoded signature mirrors its blob, so `void modreq(IsExternalInit)` -- how C# spells
        // every `init` accessor -- arrives as `Returns (Modified ...)`; but the modifier annotates a
        // return that does not exist at runtime, so no value reaches the caller's evaluation stack.
        // Concretisation is already the place custom modifiers are looked through (see the
        // `TypeDefn.Modified` case of `concretizeType`), and this is the one position where doing so
        // changes the return *shape* rather than just the type named.
        //
        // The translations that must NOT do this read the decoded signature instead, and so are
        // unaffected: `signaturesEquivalent` below, which compares what the blobs say, and
        // `concreteHandleToTypeDefn`, which runs the other way.
        match returnType with
        | MethodReturnType.Void -> ctx, MethodReturnType.Void
        | MethodReturnType.Returns ty ->
            match TypeDefn.stripCustomModifiers ty with
            | TypeDefn.Void -> ctx, MethodReturnType.Void
            | _ ->
                let handle, ctx =
                    concretizeType ctx loadAssembly assembly typeGenerics methodGenerics ty

                ctx, MethodReturnType.Returns handle

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
        // follows the unmodified type, and the questions that must compare modifiers read the
        // decoded signature instead, through `signaturesEquivalent`. That does not help here: for
        // `Base<int modopt(X)>.M(!0)`, the decoded signature is the bare `!0` and the `modopt` lives
        // only in the instantiation, which reaches the comparison as a closed handle. CoreCLR
        // compares the substituted blob and does see it (`MetaSig::CompareElementType`), so
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

    /// One side of a method-signature comparison: a signature as its own blob spells it, the
    /// assembly whose token space those spellings live in, and the closed instantiation of the type
    /// that declared it — which is what ECMA-335 `!0` denotes in this signature.
    type SignatureComparand =
        {
            Signature : TypeMethodSignature<TypeDefn>
            Assembly : AssemblyName
            DeclaringTypeGenerics : ImmutableArray<ConcreteTypeHandle>
        }

    /// The token space and generic context one signature element is spelled in.
    type private ElementContext =
        {
            Assembly : AssemblyName
            TypeGenerics : ImmutableArray<ConcreteTypeHandle>
        }

    /// A signature element under comparison: either still spelled in a blob, or a closed runtime
    /// type that a substitution supplied in place of a generic type parameter.
    type private Element =
        | Spelled of ElementContext * TypeDefn
        | Substituted of ConcreteTypeHandle

    /// Does this element mention a method generic parameter, so that it does not denote a single
    /// closed runtime type? Substitution never reaches an `ELEMENT_TYPE_MVAR`, so such an element
    /// cannot be compared against one that a substitution supplied.
    let rec private mentionsMethodGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> mentionsMethodGenericParameter element
        | TypeDefn.Modified m ->
            mentionsMethodGenericParameter m.Modifier
            || mentionsMethodGenericParameter m.Unmodified
        | TypeDefn.GenericInstantiation (generic, args) ->
            mentionsMethodGenericParameter generic
            || (args |> Seq.exists mentionsMethodGenericParameter)
        | TypeDefn.FunctionPointer signature ->
            (match signature.ReturnType with
             | MethodReturnType.Void -> false
             | MethodReturnType.Returns ty -> mentionsMethodGenericParameter ty)
            || (signature.ParameterTypes |> List.exists mentionsMethodGenericParameter)
        | TypeDefn.PrimitiveType _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.FromDefinition _
        | TypeDefn.FromReference _
        | TypeDefn.Void -> false

    /// The TypeDef a nominal signature element names, or `None` if the element is not nominal. This
    /// is `CompareTypeTokens` (siginfo.cpp:3545) reduced to what a resolved identity already
    /// answers: the AssemblyRef/forwarder walk it performs is what resolution does here.
    let private nominalIdentity
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assembly : AssemblyName)
        (ty : TypeDefn)
        : ResolvedTypeIdentity option * ConcretizationContext<DumpedAssembly>
        =
        match ty with
        | TypeDefn.FromDefinition (identity, _) -> Some identity, ctx
        | TypeDefn.FromReference (typeRef, _) ->
            let (_, identity, _), ctx =
                loadAssemblyAndResolveTypeRef loadAssembly ctx assembly typeRef

            Some identity, ctx
        | _ -> None, ctx

    /// `MetaSig::CompareElementType` (siginfo.cpp:3781), which is a comparison of *blobs*: two
    /// signature elements are the same only if they are spelled with the same element types, so
    /// `M(object)` and `M(class System.Object)` are different signatures, and a custom modifier is
    /// part of the element rather than an annotation on it.
    let rec private compareElements
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (left : Element)
        (right : Element)
        : bool * ConcretizationContext<DumpedAssembly>
        =
        match left, right with
        // A generic type parameter is resolved through the declaring type's instantiation before
        // anything else, exactly as `CompareElementType` consumes ELEMENT_TYPE_VAR ahead of the
        // modifiers (:3820-3866). CoreCLR then keeps comparing the instantiation's *blob*; PawPrint
        // holds instantiations as closed handles, so from here down the comparison is by runtime
        // type identity. What that gives up is the encoding distinction above, inside a substituted
        // argument only — a distinction PawPrint cannot represent anywhere, since a
        // `ConcreteTypeHandle` records no spelling. Custom modifiers it does not give up, but that
        // takes an explicit answer rather than following from the substitution: see the
        // `carriesCustomModifier` arm below, which is what stops concretising the *other* side from
        // stripping its modifiers.
        | Element.Spelled (leftCtx, TypeDefn.GenericTypeParameter index), _ ->
            if index >= leftCtx.TypeGenerics.Length then
                failwithf
                    "Signature comparison in %s reached generic type parameter !%d, but the declaring type's instantiation supplies only %d argument(s); the comparand was built with the wrong instantiation"
                    leftCtx.Assembly.FullName
                    index
                    leftCtx.TypeGenerics.Length

            compareElements ctx loadAssembly (Element.Substituted leftCtx.TypeGenerics.[index]) right
        | _, Element.Spelled (rightCtx, TypeDefn.GenericTypeParameter index) ->
            if index >= rightCtx.TypeGenerics.Length then
                failwithf
                    "Signature comparison in %s reached generic type parameter !%d, but the declaring type's instantiation supplies only %d argument(s); the comparand was built with the wrong instantiation"
                    rightCtx.Assembly.FullName
                    index
                    rightCtx.TypeGenerics.Length

            compareElements ctx loadAssembly left (Element.Substituted rightCtx.TypeGenerics.[index])

        | Element.Substituted leftHandle, Element.Substituted rightHandle -> leftHandle = rightHandle, ctx

        | Element.Substituted handle, Element.Spelled (spelledCtx, ty)
        | Element.Spelled (spelledCtx, ty), Element.Substituted handle ->
            // One side is a closed runtime type, so the other can only match if it denotes one too.
            if mentionsMethodGenericParameter ty then
                false, ctx
            elif carriesCustomModifier ty then
                // Answered here rather than by concretising, because `concretizeType` strips
                // `TypeDefn.Modified` and the two would then compare equal. CoreCLR substitutes the
                // instantiation's blob and goes on comparing modifiers, so a derived
                // `M(int32 modreq(X))` does not override `Base<int32>.M(!0)` and takes a fresh slot.
                //
                // "Unequal" is the whole answer, not an approximation: the handle on the other side
                // came from a type argument, and `concretizeGenericInstantiation` refuses a type
                // argument carrying a custom modifier anywhere inside it, so no substituted element
                // can have had one to match this against.
                false, ctx
            else

            let spelledHandle, ctx =
                concretizeType ctx loadAssembly spelledCtx.Assembly spelledCtx.TypeGenerics ImmutableArray.Empty ty

            spelledHandle = handle, ctx

        | Element.Spelled (leftCtx, leftTy), Element.Spelled (rightCtx, rightTy) ->

        let recurse (ctx : ConcretizationContext<DumpedAssembly>) (l : TypeDefn) (r : TypeDefn) =
            compareElements ctx loadAssembly (Element.Spelled (leftCtx, l)) (Element.Spelled (rightCtx, r))

        match leftTy, rightTy with
        // A modifier is compared before what it modifies, in blob order, with `modreq` and `modopt`
        // distinguished (:4082-4100). So `modopt(A) modopt(B) int32` and `modopt(B) modopt(A) int32`
        // are different signatures, and an unmodified type never matches a modified one.
        | TypeDefn.Modified leftMod, TypeDefn.Modified rightMod ->
            if leftMod.IsRequired <> rightMod.IsRequired then
                false, ctx
            else
                let modifiersMatch, ctx = recurse ctx leftMod.Modifier rightMod.Modifier

                if not modifiersMatch then
                    false, ctx
                else
                    recurse ctx leftMod.Unmodified rightMod.Unmodified
        | TypeDefn.Modified _, _
        | _, TypeDefn.Modified _ -> false, ctx

        | TypeDefn.PrimitiveType leftPrim, TypeDefn.PrimitiveType rightPrim -> leftPrim = rightPrim, ctx

        | TypeDefn.Void, TypeDefn.Void -> true, ctx

        // Compared positionally and symbolically: `varNum1 == varNum2` (:4068-4077). No
        // substitution is ever applied to a method generic parameter, which is what lets two
        // generic methods' signatures be compared without an instantiation for either.
        | TypeDefn.GenericMethodParameter leftIndex, TypeDefn.GenericMethodParameter rightIndex ->
            leftIndex = rightIndex, ctx

        | TypeDefn.Byref leftInner, TypeDefn.Byref rightInner
        | TypeDefn.Pointer leftInner, TypeDefn.Pointer rightInner
        | TypeDefn.OneDimensionalArrayLowerBoundZero leftInner, TypeDefn.OneDimensionalArrayLowerBoundZero rightInner ->
            recurse ctx leftInner rightInner

        // `pinned` cannot appear in a method signature at all — it annotates a local variable — so
        // this arm exists to keep the match total rather than to answer a question anyone asks.
        | TypeDefn.Pinned leftInner, TypeDefn.Pinned rightInner -> recurse ctx leftInner rightInner

        | TypeDefn.Array (leftElement, leftRank), TypeDefn.Array (rightElement, rightRank) ->
            if leftRank <> rightRank then
                // CoreCLR compares the sizes and lower bounds too; the decoder accepts exactly one
                // canonical array shape (see `TypeDefn.Array`), so rank is the whole of it here.
                false, ctx
            else
                recurse ctx leftElement rightElement

        | TypeDefn.GenericInstantiation (leftGeneric, leftArgs), TypeDefn.GenericInstantiation (rightGeneric, rightArgs) ->
            if leftArgs.Length <> rightArgs.Length then
                false, ctx
            else

            let genericMatches, ctx = recurse ctx leftGeneric rightGeneric

            if not genericMatches then
                false, ctx
            else

            let mutable ctx = ctx
            let mutable matches = true
            let mutable i = 0

            while matches && i < leftArgs.Length do
                let argMatches, newCtx = recurse ctx leftArgs.[i] rightArgs.[i]
                matches <- argMatches
                ctx <- newCtx
                i <- i + 1

            matches, ctx

        | TypeDefn.FunctionPointer leftSignature, TypeDefn.FunctionPointer rightSignature ->
            // The whole of the function pointer's signature is compared, return type included
            // (:4137-4200) — including its CallKind byte, which is where a *single* nameable
            // unmanaged convention lives. A combination of conventions is spelled as modifiers on
            // the inner return type instead, so both halves of the encoding are compared here.
            //
            // At exact arity: a function pointer is a *type*, and CoreCLR's FNPTR arm requires
            // `argCnt1 == argCnt2` before comparing anything. The vararg sentinel rule is about
            // matching a call site against a callee, which is not a question one can ask of a type,
            // so applying it here would let `void(int32, ..., string)` and `void(int32)` name the
            // same type.
            //
            // Two function pointers spelling the same GENERIC calling convention and the same
            // generic-parameter count are refused rather than compared.
            //
            // The line is where CoreCLR starts reinterpreting bytes. Its FNPTR arm compares the two
            // calling-convention bytes, then reads one compressed *integer* from each blob with
            // `CorSigUncompressData_EndPtr` and compares it as `argCnt` (siginfo.cpp:4157-4163). For a
            // GENERIC signature that integer is the generic-parameter count, since the blob spells
            // CallConv | GenParamCount | ParamCount | RetType | Params. Both of those comparisons are
            // over data read as what it is, so both are reproducible here, and differing counts fall
            // through to `compareSignatureTypes`' header and `GenericParameterCount` comparisons.
            //
            // Past that point nothing is. `argCnt1++` (:4168) and CoreCLR compares `GenParamCount + 1`
            // elements of a stream misaligned by one integer, so the *parameter count* byte is handed to
            // `CompareElementType` as a `CorElementType`: 0x01 is read as ELEMENT_TYPE_VOID and compared
            // as such, other values can fail the signature outright, and the walk then answers from the
            // real return type as though it were a parameter, ignoring the rest. Which answer comes back
            // depends on the numeric values of counts reinterpreted as element types — not on anything a
            // correctly decoded signature still knows. So the refusal covers every pair that reaches
            // there, rather than trying to predict it.
            if
                leftSignature.Header = rightSignature.Header
                && leftSignature.Header.Get.IsGeneric
                && leftSignature.GenericParameterCount = rightSignature.GenericParameterCount
            then
                failwithf
                    "TODO: comparing two function pointer signatures that spell the same GENERIC calling convention and the same generic-parameter count (in %s against %s); from here CoreCLR compares elements read at a one-integer offset into each blob, reinterpreting the parameter-count byte as an element type, which cannot be reproduced from a decoded signature"
                    leftCtx.Assembly.FullName
                    rightCtx.Assembly.FullName

            compareSignatureTypes ctx loadAssembly leftCtx rightCtx false false leftSignature rightSignature

        | (TypeDefn.FromDefinition _ | TypeDefn.FromReference _), (TypeDefn.FromDefinition _ | TypeDefn.FromReference _) ->
            // Identity, not spelling: a TypeDef in the assembly that declares the type and a
            // TypeRef everywhere else name the same type. Unlike CoreCLR, which reaches
            // `CompareTypeTokens` only once the ELEMENT_TYPE_CLASS/VALUETYPE bytes already agree,
            // this does not also compare `SignatureTypeKind`. For well-formed metadata the kind is
            // a fact about the resolved type rather than about the spelling, so it cannot
            // distinguish two references that resolve to one TypeDef; and PawPrint synthesises
            // nominal `TypeDefn`s in places (`SignatureTypeKind.Unknown` among them) where the
            // kind is not recovered from a blob at all.
            let leftIdentity, ctx = nominalIdentity ctx loadAssembly leftCtx.Assembly leftTy
            let rightIdentity, ctx = nominalIdentity ctx loadAssembly rightCtx.Assembly rightTy
            leftIdentity = rightIdentity, ctx

        // Unreachable rather than merely unequal: the enclosing match on `Element` resolves a
        // generic type parameter on either side through its instantiation before this one is
        // reached, so arriving here means that resolution was bypassed.
        | TypeDefn.GenericTypeParameter _, _
        | _, TypeDefn.GenericTypeParameter _ ->
            failwith
                "logic error: a generic type parameter reached the structural signature comparison; it should have been resolved through the declaring type's instantiation by compareElements"

        | TypeDefn.PrimitiveType _, _
        | TypeDefn.Void, _
        | TypeDefn.GenericMethodParameter _, _
        | TypeDefn.Byref _, _
        | TypeDefn.Pointer _, _
        | TypeDefn.Pinned _, _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _, _
        | TypeDefn.Array _, _
        | TypeDefn.GenericInstantiation _, _
        | TypeDefn.FunctionPointer _, _
        | TypeDefn.FromDefinition _, _
        | TypeDefn.FromReference _, _ -> false, ctx

    /// `MetaSig::CompareMethodSigs` (siginfo.cpp:4549) over decoded signatures. `left` is the
    /// *caller's* side: where the two differ in parameter count, it is `left`'s vararg sentinel that
    /// bounds the comparison.
    and private compareSignatureTypes
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (leftCtx : ElementContext)
        (rightCtx : ElementContext)
        (varargSentinelApplies : bool)
        (skipReturnType : bool)
        (left : TypeMethodSignature<TypeDefn>)
        (right : TypeMethodSignature<TypeDefn>)
        : bool * ConcretizationContext<DumpedAssembly>
        =
        // Calling convention and `hasThis` (:4589). CoreCLR masks out CORINFO_CALLCONV_PARAMTYPE,
        // which it sets on signatures it builds itself; no metadata blob carries that bit, so
        // comparing the header byte as decoded is the same comparison here.
        if left.Header <> right.Header then
            false, ctx
        elif left.GenericParameterCount <> right.GenericParameterCount then
            false, ctx
        else

        let recurse (ctx : ConcretizationContext<DumpedAssembly>) (l : TypeDefn) (r : TypeDefn) =
            compareElements ctx loadAssembly (Element.Spelled (leftCtx, l)) (Element.Spelled (rightCtx, r))

        // A vararg call site's blob carries an ELEMENT_TYPE_SENTINEL before the `...` part, and
        // `RequiredParameterCount` is where it sits (for a signature without one it is the whole
        // parameter list). The parameters past it take no part in matching, and the callee has to end
        // exactly where the sentinel is, so that overloads like `m(int, ...)` and `m(int, int, ...)`
        // stay distinguishable (:4613-4685).
        //
        // The counts alone cannot decide this. A caller `m(int, __arglist(string))` and a callee
        // `m(int, string)` both have two parameters, and CoreCLR rejects that pair because it meets
        // the sentinel where the callee has a real type; the decoded form has no sentinel element to
        // meet, so the sentinel's *position* is what has to be read.
        let callerSentinel =
            varargSentinelApplies
            && left.RequiredParameterCount <> left.ParameterTypes.Length

        let calleeSentinel =
            varargSentinelApplies
            && right.RequiredParameterCount <> right.ParameterTypes.Length

        let comparableCount =
            if
                not varargSentinelApplies
                && left.RequiredParameterCount <> right.RequiredParameterCount
            then
                // Comparing two function pointer *types*, where a sentinel is part of the type
                // rather than a call site's `...`, so it has to sit in the same place on both.
                None
            elif calleeSentinel then
                // Illegal in a callee's signature; CoreCLR asserts rather than checks.
                None
            elif callerSentinel then
                if left.RequiredParameterCount = right.ParameterTypes.Length then
                    Some left.RequiredParameterCount
                else
                    None
            elif left.ParameterTypes.Length = right.ParameterTypes.Length then
                Some left.ParameterTypes.Length
            else
                None

        match comparableCount with
        | None -> false, ctx
        | Some comparableCount ->

        let returnMatches, ctx =
            if skipReturnType then
                // CoreCLR spells "allow a covariant return" as skipping the return type entirely
                // (`SignaturesEquivalent` passes `allowCovariantReturn` straight into
                // `skipReturnTypeSig`), leaving the caller to decide what return types it accepts.
                true, ctx
            else
                match left.ReturnType, right.ReturnType with
                | MethodReturnType.Void, MethodReturnType.Void -> true, ctx
                | MethodReturnType.Returns leftTy, MethodReturnType.Returns rightTy -> recurse ctx leftTy rightTy
                | MethodReturnType.Void, MethodReturnType.Returns _
                | MethodReturnType.Returns _, MethodReturnType.Void -> false, ctx

        if not returnMatches then
            false, ctx
        else

        let leftParams = List.toArray left.ParameterTypes
        let rightParams = List.toArray right.ParameterTypes
        let mutable ctx = ctx
        let mutable matches = true
        let mutable i = 0

        while matches && i < comparableCount do
            let paramMatches, newCtx = recurse ctx leftParams.[i] rightParams.[i]
            matches <- paramMatches
            ctx <- newCtx
            i <- i + 1

        matches, ctx

    /// One side of a generic method's constraint comparison: what its type parameters are
    /// constrained to, and the token space and declaring-type instantiation those constraints are
    /// written in.
    type ConstraintComparand =
        {
            Parameters : GenericParamMetadata list
            Assembly : AssemblyName
            DeclaringTypeGenerics : ImmutableArray<ConcreteTypeHandle>
        }

    /// `MetaSig::CompareMethodConstraints` (siginfo.cpp:5108) and the per-parameter rule it
    /// delegates to, `CompareVariableConstraints` (:5007). CoreCLR runs this *after* the signatures
    /// match, and treats a mismatch as a failure to load the type rather than as a reason to give the
    /// method a slot of its own (methodtablebuilder.cpp:5449-5459).
    ///
    /// The rules are one-directional, so `impl` must be the overriding side: an override may not
    /// *add* a requirement its base did not have, but it may drop one.
    ///
    /// Roslyn copies a base method's constraints verbatim onto an override — C# forbids restating
    /// them, and the metadata carries them all the same — so ordinary C# reaches only the case where
    /// the two sides are identical.
    let methodConstraintsMatch
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (impl : ConstraintComparand)
        (decl : ConstraintComparand)
        : bool * ConcretizationContext<DumpedAssembly>
        =
        if impl.Parameters.Length <> decl.Parameters.Length then
            false, ctx
        else

        let implCtx : ElementContext =
            {
                Assembly = impl.Assembly
                TypeGenerics = impl.DeclaringTypeGenerics
            }

        let declCtx : ElementContext =
            {
                Assembly = decl.Assembly
                TypeGenerics = decl.DeclaringTypeGenerics
            }

        // A constraint naming `System.Object` says nothing, and neither does one naming
        // `System.ValueType` on a parameter already constrained to a non-nullable value type.
        // CoreCLR skips both rather than looking for a match, because the overridden parameter is
        // entitled to leave them implicit (:5069-5079).
        let isVacuous
            (ctx : ConcretizationContext<DumpedAssembly>)
            (isNotNullableValueType : bool)
            (constraintType : TypeDefn)
            : bool * ConcretizationContext<DumpedAssembly>
            =
            // `System.Object` has a primitive spelling as well as a nominal one, and a constraint
            // may use either: the GenericParamConstraint column is a TypeDefOrRefOrSpec, and a
            // TypeSpec may hold a bare `ELEMENT_TYPE_OBJECT`. CoreCLR resolves that spelling to
            // System.Object's TypeDef before comparing (`CompareElementTypeToToken`,
            // siginfo.cpp:4915), so it is just as vacuous as the nominal form.
            match constraintType with
            | TypeDefn.PrimitiveType PrimitiveType.Object -> true, ctx
            | _ ->

            let identity, ctx = nominalIdentity ctx loadAssembly impl.Assembly constraintType

            match identity with
            | None -> false, ctx
            | Some identity ->

            let isBaseType (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
                identity = ResolvedTypeIdentity.ofTypeDefinition ty.Assembly ty.TypeDefHandle

            isBaseType ctx.BaseTypes.Object
            || (isNotNullableValueType && isBaseType ctx.BaseTypes.ValueType),
            ctx

        let mutable ctx = ctx
        let mutable matches = true

        for implParam, declParam in List.zip impl.Parameters decl.Parameters do
            if matches then
                let isNotNullableValueType =
                    implParam.Constraint = Some GenericConstraint.NonNullableValue

                let specialsMatch =
                    // Each of these says the override must not demand more of a type argument than
                    // the method it overrides already demanded.
                    (match implParam.Constraint with
                     | Some GenericConstraint.NonNullableValue ->
                         declParam.Constraint = Some GenericConstraint.NonNullableValue
                     | Some GenericConstraint.Reference -> declParam.Constraint = Some GenericConstraint.Reference
                     | None -> true)
                    && (not implParam.RequiresParameterlessConstructor
                        || declParam.RequiresParameterlessConstructor
                        // A non-nullable value type always has a parameterless constructor.
                        || declParam.Constraint = Some GenericConstraint.NonNullableValue)
                    // `allows ref struct` runs the other way, because it *widens* what the parameter
                    // accepts: the override has to keep accepting what the base accepted.
                    && (not declParam.AllowsByRefLike || implParam.AllowsByRefLike)

                if not specialsMatch then
                    matches <- false
                else

                for implConstraint in implParam.Constraints do
                    if matches then
                        let vacuous, newCtx = isVacuous ctx isNotNullableValueType implConstraint
                        ctx <- newCtx

                        if not vacuous then
                            let mutable found = false

                            for declConstraint in declParam.Constraints do
                                if not found then
                                    let equal, newCtx =
                                        compareElements
                                            ctx
                                            loadAssembly
                                            (Element.Spelled (implCtx, implConstraint))
                                            (Element.Spelled (declCtx, declConstraint))

                                    ctx <- newCtx
                                    found <- equal

                            if not found then
                                matches <- false

        matches, ctx

    /// Do these two method signatures name the same signature, in the sense of
    /// `MetaSig::CompareMethodSigs`? This is a comparison of what the blobs *say*, so it separates
    /// signatures that concretisation deliberately conflates: custom modifiers are compared in every
    /// position, and so is the choice of encoding.
    ///
    /// Generic type parameters are resolved through each side's declaring-type instantiation;
    /// generic *method* parameters are compared positionally, so two generic methods can be compared
    /// without an instantiation for either.
    ///
    /// `skipReturnType` omits the return column, which is how CoreCLR expresses "a covariant return
    /// is acceptable" — the caller then applies whatever rule it has for return types.
    ///
    /// `caller` is the side whose vararg sentinel bounds the comparison, where the two differ in
    /// parameter count.
    let signaturesEquivalent
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (skipReturnType : bool)
        (caller : SignatureComparand)
        (callee : SignatureComparand)
        : bool * ConcretizationContext<DumpedAssembly>
        =
        let toElementContext (comparand : SignatureComparand) : ElementContext =
            {
                Assembly = comparand.Assembly
                TypeGenerics = comparand.DeclaringTypeGenerics
            }

        compareSignatureTypes
            ctx
            loadAssembly
            (toElementContext caller)
            (toElementContext callee)
            true
            skipReturnType
            caller.Signature
            callee.Signature

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
