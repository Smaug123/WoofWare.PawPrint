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
        |> Option.map (fun (_, typeInfo) -> LoadedTypeInfo.isValueType baseClassTypes assemblies typeInfo)

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
                    | None -> $"<unloaded assembly %s{concrete.AssemblyFullName} for concrete type #%d{id}>"
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

    let private intern
        (ct : ConcreteType<ConcreteTypeHandle>)
        (this : AllConcreteTypes)
        : ConcreteTypeHandle * AllConcreteTypes
        =
        let key = (ct.Identity, Seq.toList ct.Generics)

        if this.ReverseIndex.ContainsKey key then
            failwith $"BUG: %O{ct} is already interned; a caller must look it up before adding it"

        let id = this.NextHandle
        let toRet = ConcreteTypeHandle.Concrete id

        let newState =
            {
                NextHandle = this.NextHandle + 1
                Mapping = this.Mapping |> Map.add id ct
                ReverseIndex = this.ReverseIndex |> Map.add key toRet
            }

        toRet, newState

    /// The first of an instantiation's arguments, by index, that CoreCLR admits in no instantiation
    /// (`Generics::CheckInstantiation`, generics.cpp), and why. CoreCLR stops at the first type
    /// variable among the arguments; a handle is closed, so never is one.
    let private malformedArgument
        (baseClassTypes : BaseClassTypes<'corelib>)
        (this : AllConcreteTypes)
        (arguments : ConcreteTypeHandle seq)
        : (int * MalformedArgument) option
        =
        arguments
        |> Seq.indexed
        |> Seq.tryPick (fun (index, argument) ->
            match argument with
            | ConcreteTypeHandle.Byref _ -> Some (index, MalformedArgument.Byref)
            | ConcreteTypeHandle.Pointer _ -> Some (index, MalformedArgument.Pointer)
            | ConcreteTypeHandle.FunctionPointer _ -> Some (index, MalformedArgument.FunctionPointer)
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> None
            | ConcreteTypeHandle.Concrete id ->
                match this.Mapping |> Map.tryFind id with
                | None -> failwith $"BUG: the argument %O{argument} of an instantiation is not interned"
                | Some concrete when concrete.Identity = baseClassTypes.TypedReference.Identity ->
                    Some (index, MalformedArgument.TypedReference)
                | Some concrete when concrete.Identity = baseClassTypes.Void.Identity ->
                    Some (index, MalformedArgument.Void)
                | Some _ -> None
        )

    /// Intern `ct`, which must not already be interned, as a type CoreCLR's class loader admits.
    ///
    /// This and `addNonGeneric` are the only ways to mint a `Concrete` handle, so every handle in the
    /// table is an instantiation CoreCLR could create: none of its arguments is a byref,
    /// `TypedReference`, `void`, a pointer or a function pointer. An instantiation with one is
    /// refused, as `ClassLoader::LoadTypeHandleForTypeKey` refuses it, and nothing is interned. Its
    /// arguments must already be interned.
    let add
        (baseClassTypes : BaseClassTypes<'corelib>)
        (ct : ConcreteType<ConcreteTypeHandle>)
        (this : AllConcreteTypes)
        : Result<ConcreteTypeHandle * AllConcreteTypes, TypeLoadRefusal>
        =
        match malformedArgument baseClassTypes this ct.Generics with
        | Some (index, malformed) -> Error (TypeLoadRefusal.InvalidInstantiation (ct.Identity, index, malformed))
        | None -> Ok (intern ct this)

    /// Intern `ct`, a non-generic type, which must not already be interned. No rule `add` checks
    /// can refuse it.
    let addNonGeneric
        (ct : ConcreteType<ConcreteTypeHandle>)
        (this : AllConcreteTypes)
        : ConcreteTypeHandle * AllConcreteTypes
        =
        if not ct.Generics.IsEmpty then
            failwith $"BUG: %O{ct} is an instantiation; intern it with `add`, which checks its arguments"

        intern ct this

// Active patterns for matching concrete types

[<AutoOpen>]
module ConcreteActivePatterns =
    /// Matches a concrete type handle that is one of CoreLib's primitive types, yielding which.
    ///
    /// Only a type CoreLib declares is a primitive: a guest assembly may declare its own
    /// `System.Int32`, and that is an ordinary value type, whatever its name.
    let (|ConcretePrimitive|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct when
                ct.Namespace = "System"
                && ct.Generics.IsEmpty
                && AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
                ->
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
                AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
            | Some ct when AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName ->
                Some (ct.Namespace, ct.Name, ct.Generics)
            | _ -> None
        | _ -> None

    let (|ConcreteChar|_|) (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : unit option =
        match handle with
        | ConcreteTypeHandle.Concrete id ->
            match concreteTypes.Mapping |> Map.tryFind id with
            | Some ct ->
                if
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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
                    AssemblyDefinitionName.isNamed "System.Private.CoreLib" ct.AssemblyFullName
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

    // Intern a non-generic type, which nothing can refuse.
    let private createAndAddNonGenericType
        (ctx : ConcretizationContext<'corelib>)
        (identity : ResolvedTypeIdentity)
        (ns : string)
        (name : string)
        : ConcreteTypeHandle * ConcretizationContext<'corelib>
        =
        let concreteType =
            ConcreteType.makeFromIdentity identity ns name ImmutableArray.Empty

        let handle, newConcreteTypes =
            AllConcreteTypes.addNonGeneric concreteType ctx.ConcreteTypes

        let newCtx =
            { ctx with
                ConcreteTypes = newConcreteTypes
            }

        handle, newCtx

    /// Register the nominal type `identity` instantiated at `generics`, having first loaded every
    /// assembly its base-type chain runs through: CoreCLR loads a type's parent before the type, so a
    /// type whose base chain does not bind has no runtime type. The failure is the chain's, or an
    /// instantiation `AllConcreteTypes.add` refuses, with what loaded before it in the context.
    let private registerNominal
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<'corelib>)
        (identity : ResolvedTypeIdentity)
        (ns : string)
        (name : string)
        (generics : ConcreteTypeHandle ImmutableArray)
        : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<'corelib>
        =
        match
            BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
                loadAssembly
                ctx.LoadedAssemblies
                (ctx.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName)
                identity.TypeDefinition.Get
        with
        | assemblies, Some failure ->
            Error (TypeBindingFailure.ofBaseChain failure),
            { ctx with
                LoadedAssemblies = assemblies
            }
        | assemblies, None ->
            let ctx =
                { ctx with
                    LoadedAssemblies = assemblies
                }

            match
                AllConcreteTypes.add
                    ctx.BaseTypes
                    (ConcreteType.makeFromIdentity identity ns name generics)
                    ctx.ConcreteTypes
            with
            | Error refusal -> Error (TypeBindingFailure.Refused refusal), ctx
            | Ok (handle, concreteTypes) ->
                Ok handle,
                { ctx with
                    ConcreteTypes = concreteTypes
                }

    /// What `f` makes of a value that bound, or the failure that stopped it, with the context either
    /// way.
    let private andThen
        (f : ConcretizationContext<'corelib> -> 'a -> Result<'b, TypeBindingFailure> * ConcretizationContext<'corelib>)
        (result : Result<'a, TypeBindingFailure>, ctx : ConcretizationContext<'corelib>)
        : Result<'b, TypeBindingFailure> * ConcretizationContext<'corelib>
        =
        match result with
        | Ok bound -> f ctx bound
        | Error failure -> Error failure, ctx

    /// Each of `items` concretised in order, stopping at the first that fails.
    let private concretizeEach
        (concretize :
            ConcretizationContext<'corelib> -> 'a -> Result<'b, TypeBindingFailure> * ConcretizationContext<'corelib>)
        (ctx : ConcretizationContext<'corelib>)
        (items : 'a seq)
        : Result<'b list, TypeBindingFailure> * ConcretizationContext<'corelib>
        =
        let rec go (ctx : ConcretizationContext<'corelib>) (acc : 'b list) (items : 'a list) =
            match items with
            | [] -> Ok (List.rev acc), ctx
            | item :: rest ->
                match concretize ctx item with
                | Ok concretized, ctx -> go ctx (concretized :: acc) rest
                | Error failure, ctx -> Error failure, ctx

        go ctx [] (List.ofSeq items)

    let private unmodelled (diagnostic : string) : Result<'a, TypeBindingFailure> =
        Error (
            TypeBindingFailure.Unmodelled
                {
                    Diagnostic = diagnostic
                }
        )

    let private refused (refusal : TypeLoadRefusal) : Result<'a, TypeBindingFailure> =
        Error (TypeBindingFailure.Refused refusal)

    /// Whether a signature's `CLASS`/`VALUETYPE` spelling of the definition `identity` matches what
    /// the definition is, as `SigPointer::GetTypeHandleThrowing` checks once it has loaded the type;
    /// a kind no signature spelled (`Unknown`, from a bare token) is not checked. The definition's
    /// base-chain assemblies must be loaded.
    let private checkSpelling
        (ctx : ConcretizationContext<DumpedAssembly>)
        (identity : ResolvedTypeIdentity)
        (spelled : SignatureTypeKind)
        : Result<unit, TypeBindingFailure>
        =
        match spelled with
        | SignatureTypeKind.Unknown -> Ok ()
        | SignatureTypeKind.Class
        | SignatureTypeKind.ValueType ->
            let definition =
                Assembly.resolveTypeIdentityDefinition
                    (ctx.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName)
                    identity

            if LoadedTypeInfo.signatureTypeKind ctx.BaseTypes ctx.LoadedAssemblies definition = spelled then
                Ok ()
            else
                refused (TypeLoadRefusal.KindMismatch (identity, spelled))
        | other -> failwith $"BUG: a signature's nominal type has the kind %O{other}"

    // Helper function for assembly loading with retry pattern
    let private loadAssemblyAndResolveTypeRef
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<'corelib>)
        (currentAssemblyFullName : string)
        (typeRef : TypeRef)
        : Result<DumpedAssembly * ResolvedTypeIdentity * WoofWare.PawPrint.TypeInfo<_, _>, TypeBindingFailure> *
          ConcretizationContext<'corelib>
        =
        let rec go
            (ctx : ConcretizationContext<'corelib>)
            : Result<DumpedAssembly * ResolvedTypeIdentity * WoofWare.PawPrint.TypeInfo<_, _>, TypeBindingFailure> *
              ConcretizationContext<'corelib>
            =
            let currentAssy = ctx.LoadedAssemblies.ByDefinitionName currentAssemblyFullName

            match LoadedTypeResolution.resolveTypeRef ctx.LoadedAssemblies currentAssy ImmutableArray.Empty typeRef with
            | TypeResolutionResult.Resolved (targetAssy, identity, typeInfo) -> Ok (targetAssy, identity, typeInfo), ctx
            | TypeResolutionResult.NotFound miss -> Error (TypeBindingFailure.TypeAbsent miss), ctx
            | TypeResolutionResult.Unmodelled unmodelled -> Error (TypeBindingFailure.Unmodelled unmodelled), ctx
            | TypeResolutionResult.FirstLoadAssy assemblyRef ->
                let handle, referencedIn = assemblyRef.Handle

                match loadAssembly.TryLoadAssembly ctx.LoadedAssemblies referencedIn handle with
                | Error (AssemblyLoadFailure.NoSuchAssembly reference) ->
                    Error (TypeBindingFailure.AssemblyUnavailable reference), ctx
                | Error (AssemblyLoadFailure.LoadingNotPermitted _ as failure) ->
                    failwith (string<AssemblyLoadFailure> failure)
                | Ok (newAssemblies, _) ->

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

        let typeInfo = BaseClassTypes.ofPrimitive ctx.BaseTypes prim

        // Check if we've already concretized this primitive type
        let identity =
            ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

        match findExistingPrimitiveType ctx.ConcreteTypes identity with
        | Some handle -> handle, ctx
        | None -> createAndAddNonGenericType ctx identity typeInfo.Namespace typeInfo.Name

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

    /// The concrete type of the non-generic definition `identity`. Its base-type chain is loaded
    /// first, and a type on it that does not bind is the failure.
    let concretizeTypeDefinition
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<'corelib>)
        (identity : ResolvedTypeIdentity)
        : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<'corelib>
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
        | Some handle -> Ok handle, ctx
        | None -> registerNominal loadAssembly ctx identity typeInfo.Namespace typeInfo.Name ImmutableArray.Empty

    /// The non-generic type a signature names by the definition `identity`, spelled `spelled`: a
    /// generic definition named without an instantiation is refused (CoreCLR loads such a token with
    /// `FailIfUninstDefOrRef`), then the type is loaded, then its spelling checked.
    let private concretizeNamedDefinition
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<DumpedAssembly>)
        (identity : ResolvedTypeIdentity)
        (spelled : SignatureTypeKind)
        : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        let typeInfo =
            Assembly.resolveTypeIdentityDefinition
                (ctx.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName)
                identity

        if not typeInfo.Generics.IsEmpty then
            refused (TypeLoadRefusal.UninstantiatedDefinition identity), ctx
        else

        let loaded =
            match findExistingType ctx.ConcreteTypes identity ImmutableArray.Empty with
            | Some handle -> Ok handle, ctx
            | None -> registerNominal loadAssembly ctx identity typeInfo.Namespace typeInfo.Name ImmutableArray.Empty

        loaded
        |> andThen (fun ctx handle -> checkSpelling ctx identity spelled |> Result.map (fun () -> handle), ctx)

    let private concretizeTypeReference
        (loadAssembly : IAssemblyLoad)
        (ctx : ConcretizationContext<DumpedAssembly>)
        (currentAssemblyFullName : string)
        (typeRef : TypeRef)
        (spelled : SignatureTypeKind)
        : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        loadAssemblyAndResolveTypeRef loadAssembly ctx currentAssemblyFullName typeRef
        |> andThen (fun ctx (_, identity, _) -> concretizeNamedDefinition loadAssembly ctx identity spelled)

    /// Does this signature type carry an ECMA-335 custom modifier (`modreq`/`modopt`) anywhere
    /// inside it? Concretisation refuses a generic instantiation with a type argument that does,
    /// the one position where dropping a modifier is unrecoverable (see
    /// `concretizeGenericInstantiation`), so a caller that must not fail can ask this first.
    let rec carriesCustomModifier (typeDefn : TypeDefn) : bool =
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

    /// Concretize a type in a specific generic context.
    ///
    /// The first part of the type, in the order CoreCLR's `SigPointer::GetTypeHandleThrowing` loads
    /// it, that does not bind is the result's failure, with what was loaded and registered before it
    /// in the context: a type or assembly the metadata names that is not there; a type CoreCLR's
    /// class loader refuses (`TypeBindingFailure.Refused`: a type variable out of the context's
    /// scope, a `CLASS`/`VALUETYPE` spelling the definition does not match, a generic definition
    /// named without an instantiation or instantiated at the wrong number of arguments, an
    /// instantiation with an argument no instantiation may have); or a shape PawPrint does not
    /// implement (a generic function pointer, a type argument carrying a custom modifier).
    let rec concretizeType
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (typeDefn : TypeDefn)
        : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        let concretizeElement (ctx : ConcretizationContext<DumpedAssembly>) (elementType : TypeDefn) =
            concretizeType ctx loadAssembly assemblyFullName typeGenerics methodGenerics elementType

        match typeDefn with
        | TypeDefn.PrimitiveType prim ->
            let handle, ctx = concretizePrimitive ctx prim
            Ok handle, ctx

        | TypeDefn.Array (elementType, rank) ->
            concretizeElement ctx elementType
            |> andThen (fun ctx elementHandle ->
                let handle, ctx = concretizeArray ctx elementHandle rank
                Ok handle, ctx
            )

        | TypeDefn.OneDimensionalArrayLowerBoundZero elementType ->
            concretizeElement ctx elementType
            |> andThen (fun ctx elementHandle ->
                let handle, ctx = concretizeOneDimArray ctx elementHandle
                Ok handle, ctx
            )

        | TypeDefn.GenericTypeParameter index ->
            if 0 <= index && index < typeGenerics.Length then
                Ok typeGenerics.[index], ctx
            else
                refused (TypeLoadRefusal.TypeParameterOutOfScope (index, typeGenerics.Length)), ctx

        | TypeDefn.GenericMethodParameter index ->
            if 0 <= index && index < methodGenerics.Length then
                Ok methodGenerics.[index], ctx
            else
                refused (TypeLoadRefusal.MethodParameterOutOfScope (index, methodGenerics.Length)), ctx

        | TypeDefn.GenericInstantiation (genericDef, args) ->
            concretizeGenericInstantiation ctx loadAssembly assemblyFullName typeGenerics methodGenerics genericDef args

        | TypeDefn.FromDefinition (identity, spelled) -> concretizeNamedDefinition loadAssembly ctx identity spelled

        | TypeDefn.FromReference (typeRef, spelled) ->
            concretizeTypeReference loadAssembly ctx assemblyFullName typeRef spelled

        | TypeDefn.Byref elementType ->
            // Byref types are managed references to other types
            concretizeElement ctx elementType
            |> andThen (fun ctx elementHandle -> Ok (ConcreteTypeHandle.Byref elementHandle), ctx)

        | TypeDefn.Pointer elementType ->
            // Pointer types are unmanaged pointers to other types
            concretizeElement ctx elementType
            |> andThen (fun ctx elementHandle -> Ok (ConcreteTypeHandle.Pointer elementHandle), ctx)

        | TypeDefn.Pinned elementType ->
            // `pinned` is a GC-pinning annotation that appears on local-variable signatures emitted
            // by the C# `fixed` statement. In the real CLR it tells the GC not to relocate the
            // referent; we have no moving GC, so pinning is semantically a no-op and the element
            // type's own concretization handle is the right representation.
            concretizeElement ctx elementType

        | TypeDefn.Modified m ->
            // Custom modifiers are metadata annotations on the signature. Runtime type
            // identity and storage shape follow the unmodified type.
            concretizeElement ctx m.Unmodified

        | TypeDefn.Void ->
            // Method return signatures represent `void` separately from runtime types.
            // Type signatures can still mention `void`, for example as the element type of `void*`.
            // In those positions, System.Void is the canonical concrete type identity.
            let voidTypeInfo = ctx.BaseTypes.Void

            match
                findExistingType
                    ctx.ConcreteTypes
                    (ResolvedTypeIdentity.ofDefinitionInAssembly
                        voidTypeInfo.AssemblyFullName
                        voidTypeInfo.TypeDefHandle)
                    ImmutableArray.Empty
            with
            | Some handle -> Ok handle, ctx
            | None ->
                let handle, ctx =
                    createAndAddNonGenericType
                        ctx
                        (ResolvedTypeIdentity.ofDefinitionInAssembly
                            voidTypeInfo.AssemblyFullName
                            voidTypeInfo.TypeDefHandle)
                        voidTypeInfo.Namespace
                        voidTypeInfo.Name

                Ok handle, ctx

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
                unmodelled
                    $"TODO: concretising a function pointer type whose signature carries the GENERIC calling convention, in %s{assemblyFullName}. No compiler emits such a type and no reflection API can name one, so this indicates either hand-authored metadata or a decoder fault; signature comparison could not answer for it in any case.",
                ctx
            else

            // Function pointer types are structural: the signature is the type identity.
            // Concretize each parameter and the return type under the current generic context.
            concretizeMethodSignature ctx loadAssembly assemblyFullName typeGenerics methodGenerics signature
            |> andThen (fun ctx concretized -> Ok (ConcreteTypeHandle.FunctionPointer concretized), ctx)

    and concretizeMethodSignature
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (signature : TypeMethodSignature<TypeDefn>)
        : Result<TypeMethodSignature<ConcreteTypeHandle>, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        match
            concretizeReturnColumn ctx loadAssembly assemblyFullName typeGenerics methodGenerics signature.ReturnType
        with
        | ctx, Error failure -> Error failure, ctx
        | ctx, Ok returnType ->

        signature.ParameterTypes
        |> concretizeEach
            (fun ctx paramType ->
                concretizeType ctx loadAssembly assemblyFullName typeGenerics methodGenerics paramType
            )
            ctx
        |> andThen (fun ctx paramHandles ->
            Ok
                {
                    Header = signature.Header
                    ReturnType = returnType
                    ParameterTypes = paramHandles
                    GenericParameterCount = signature.GenericParameterCount
                    RequiredParameterCount = signature.RequiredParameterCount
                },
            ctx
        )

    /// Concretise a method's return column, which is where a `void` under custom modifiers becomes
    /// <c>MethodReturnType.Void</c>. Every consumer of a concretised return shape must go through
    /// this, or two of them can disagree about whether such a method returns a value.
    and concretizeReturnColumn
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (returnType : MethodReturnType<TypeDefn>)
        : ConcretizationContext<DumpedAssembly> * Result<MethodReturnType<ConcreteTypeHandle>, TypeBindingFailure>
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
        | MethodReturnType.Void -> ctx, Ok MethodReturnType.Void
        | MethodReturnType.Returns ty ->
            match TypeDefn.stripCustomModifiers ty with
            | TypeDefn.Void -> ctx, Ok MethodReturnType.Void
            | _ ->
                let handle, ctx =
                    concretizeType ctx loadAssembly assemblyFullName typeGenerics methodGenerics ty

                ctx, handle |> Result.map MethodReturnType.Returns

    and private concretizeGenericInstantiation
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assemblyFullName : string)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (genericDef : TypeDefn)
        (args : ImmutableArray<TypeDefn>)
        : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
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
        // Refuse instead. Measured over the linux-x64 runtime pack, FSharp.Core, the Roslyn
        // assemblies and this repo's own test binaries -- 208 assemblies -- not one generic
        // instantiation carries a modified type argument, nested or otherwise. C# does write one,
        // though, for a function pointer type in a type argument whose signature carries a modifier:
        // `List<delegate*<out int, void>[]>`, whose `out` parameter is `modreq(OutAttribute)`.
        match args |> Seq.tryFind carriesCustomModifier with
        | Some arg ->
            unmodelled
                $"TODO: generic instantiation in %s{assemblyFullName} has a type argument carrying a custom modifier (%O{arg}); ConcreteTypeHandle cannot represent it, and unlike a modifier written directly in a signature it cannot be recovered by walking the unsubstituted signature either, so accepting it would make this instantiation compare equal to the unmodified one",
            ctx
        | None ->

        // A signature cannot spell an instantiation with no arguments at all.
        if args.IsEmpty then
            unmodelled $"TODO: generic instantiation of %O{genericDef} in %s{assemblyFullName} names no arguments", ctx
        else

        // `SigPointer::GetTypeHandleThrowing` reads the head first, loading the definition (and so its
        // parent) and checking its spelling (`GetGenericInstType`); then loads each argument in full,
        // in order; then checks the arity (`IDS_CLASSLOAD_TYPEWRONGNUMGENERICARGS`), for any count
        // other than the definition's, a non-generic definition's included; and checks the arguments'
        // kinds only as it creates the instantiation, which `registerNominal` does.
        let head =
            match genericDef with
            | FromDefinition (identity, spelled) -> Ok (identity, spelled), ctx
            | FromReference (typeRef, spelled) ->
                loadAssemblyAndResolveTypeRef loadAssembly ctx assemblyFullName typeRef
                |> andThen (fun ctx (_, identity, _) -> Ok (identity, spelled), ctx)
            | _ -> unmodelled $"Generic instantiation of %A{genericDef} not supported", ctx

        head
        |> andThen (fun ctx (identity, spelled) ->
            match
                BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
                    loadAssembly
                    ctx.LoadedAssemblies
                    (ctx.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName)
                    identity.TypeDefinition.Get
            with
            | assemblies, Some failure ->
                Error (TypeBindingFailure.ofBaseChain failure),
                { ctx with
                    LoadedAssemblies = assemblies
                }
            | assemblies, None ->
                let ctx =
                    { ctx with
                        LoadedAssemblies = assemblies
                    }

                checkSpelling ctx identity spelled |> Result.map (fun () -> identity), ctx
        )
        |> andThen (fun ctx identity ->
            args
            |> concretizeEach
                (fun ctx arg -> concretizeType ctx loadAssembly assemblyFullName typeGenerics methodGenerics arg)
                ctx
            |> andThen (fun ctx argHandles ->
                let argHandles = ImmutableArray.CreateRange argHandles

                let definition =
                    Assembly.resolveTypeIdentityDefinition
                        (ctx.LoadedAssemblies.ByDefinitionName identity.AssemblyFullName)
                        identity

                if args.Length <> definition.Generics.Length then
                    refused (TypeLoadRefusal.WrongArity (identity, definition.Generics.Length, args.Length)), ctx
                else

                match findExistingType ctx.ConcreteTypes identity argHandles with
                | Some existingHandle -> Ok existingHandle, ctx
                | None -> registerNominal loadAssembly ctx identity definition.Namespace definition.Name argHandles
            )
        )

    /// What one ECMA-335 `!i` denotes in a signature under comparison.
    ///
    /// `Spelled` is a substitution that stays in blob form, carrying the token space and context its
    /// own `!i` are read in. That is CoreCLR's `Substitution` — a module, a signature pointer, and a
    /// `pNext` for the context the pointer's own variables belong to (methodtablebuilder.cpp:1330-1337)
    /// — and it is what lets a base class's signatures be read in a derived type's vocabulary while
    /// neither is closed.
    ///
    /// Build these through `SubstitutionContext`, never directly: its functions are exactly the shapes
    /// that arise, and a hand-built chain is how a signature comes to be read against the wrong image.
    [<RequireQualifiedAccess>]
    type SubstitutionArgument =
        /// A runtime type, which a closed instantiation supplied.
        | Closed of ConcreteTypeHandle
        /// The `index`th type variable of `owner` itself, denoting no particular type. Equal only to
        /// the same variable of the same owner: this is the positional comparison
        /// `MetaSig::CompareElementType` falls back on for an `ELEMENT_TYPE_VAR` that no substitution
        /// replaced (`varNum1 == varNum2`, siginfo.cpp:4067-4076).
        | Formal of owner : ResolvedTypeIdentity * index : int
        /// A type as `assembly` spells it, whose own `!i` are resolved by `context`.
        | Spelled of assembly : string * spelling : TypeDefn * context : ImmutableArray<SubstitutionArgument>

    /// The instantiation of a declaring type that a signature's `!i` are read against: one argument
    /// per generic parameter the declaring type has.
    ///
    /// A comparison is read in one world throughout. Either every argument reachable from it is
    /// `Closed`, which is what any question about a live object needs, or the whole comparison is
    /// rooted at a generic *definition* and unsubstituted variables survive to be compared
    /// positionally, which is how a method table is laid out. Mixing the two is a bug rather than a
    /// third case, and the comparison says so rather than answering. (A comparison that substitutes
    /// nothing at all takes no context: that is `signaturesEquivalentWithoutSubstitution`.)
    type SubstitutionContext =
        {
            Arguments : ImmutableArray<SubstitutionArgument>
        }

    [<RequireQualifiedAccess>]
    module SubstitutionContext =
        /// The declaring type is closed, so each `!i` denotes a runtime type.
        let ofClosed (generics : ImmutableArray<ConcreteTypeHandle>) : SubstitutionContext =
            {
                Arguments = generics |> ImmutableArray.map SubstitutionArgument.Closed
            }

        /// The declaring type is the generic definition itself — CoreCLR's typical instantiation, which
        /// the class loader represents as the definition rather than as an instantiation of it
        /// (`ClassLoader::LoadGenericInstantiationThrowing`, clsload.cpp:1426). Each `!i` denotes the
        /// definition's own `i`th type variable.
        let forDefinition (definition : ResolvedTypeIdentity) (parameterCount : int) : SubstitutionContext =
            {
                Arguments =
                    Seq.init parameterCount (fun i -> SubstitutionArgument.Formal (definition, i))
                    |> ImmutableArray.CreateRange
            }

        /// The context an ancestor's signatures are read in, given the type arguments its `extends`
        /// clause supplies and the context of the type that wrote that clause.
        ///
        /// `spellingAssembly` is the assembly of the type that *names* the ancestor, not the ancestor's
        /// own: an extends clause lives in the token space of the type that writes it, and CoreCLR
        /// chains substitutions the same way (`MethodTableBuilder::GetSubstitutionForParent`,
        /// methodtablebuilder.cpp:788). Passing the ancestor's assembly instead resolves the clause's
        /// TypeRefs against the wrong image — which corelib cannot detect, having no TypeRefs at all.
        let forBase
            (spellingAssembly : string)
            (arguments : ImmutableArray<TypeDefn>)
            (spellingContext : SubstitutionContext)
            : SubstitutionContext
            =
            {
                Arguments =
                    arguments
                    |> ImmutableArray.map (fun ty ->
                        SubstitutionArgument.Spelled (spellingAssembly, ty, spellingContext.Arguments)
                    )
            }

        /// Re-read a substitution built in `definition`'s own vocabulary as one in the vocabulary of a
        /// type that instantiates it: every `Formal (definition, i)` becomes `arguments.[i]`.
        ///
        /// Which slot a method occupies is decided once per definition, in that definition's own
        /// context, and inherited unchanged -- CoreCLR builds each method table once and
        /// `CopyParentVtable` copies it (methodtablebuilder.cpp:1143). *Comparing* an inherited
        /// signature against a derived type's own is the separate question, and that needs both read in
        /// the derived type's world, which is what this supplies. Re-running the parent's placement
        /// under the child's arguments instead would fold two of the parent's slots together whenever
        /// the child's arguments make two of its distinct declarations coincide.
        let rebase
            (definition : ResolvedTypeIdentity)
            (arguments : ImmutableArray<SubstitutionArgument>)
            (context : SubstitutionContext)
            : SubstitutionContext
            =
            let rec rebaseArgument (argument : SubstitutionArgument) : SubstitutionArgument =
                match argument with
                | SubstitutionArgument.Closed _ -> argument
                | SubstitutionArgument.Formal (owner, index) ->
                    if owner <> definition then
                        // A definition-rooted layout mentions only the variables of the one definition
                        // it was laid out on, so another owner here means the slot came from a walk
                        // rooted elsewhere.
                        failwith
                            $"Cannot re-read a substitution against %s{definition.AssemblyFullName}/%O{definition.TypeDefinition.Get}: it mentions variable !%d{index} of %s{owner.AssemblyFullName}/%O{owner.TypeDefinition.Get}"

                    if index >= arguments.Length then
                        failwith
                            $"Cannot re-read a substitution against %s{definition.AssemblyFullName}/%O{definition.TypeDefinition.Get}: it mentions variable !%d{index} but only %d{arguments.Length} argument(s) were supplied"

                    arguments.[index]
                | SubstitutionArgument.Spelled (assembly, spelling, context) ->
                    SubstitutionArgument.Spelled (assembly, spelling, context |> ImmutableArray.map rebaseArgument)

            {
                Arguments = context.Arguments |> ImmutableArray.map rebaseArgument
            }

    /// The runtime types a closed substitution denotes, one per argument: a `Closed` argument is
    /// its handle, and a `Spelled` one is its spelling instantiated in its own assembly with its own
    /// context closed first.
    ///
    /// Fails on a `Formal` argument, which denotes no particular type: rebase a definition-rooted
    /// context onto closed arguments (`SubstitutionContext.rebase`) before asking.
    let concretizeSubstitution
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (context : SubstitutionContext)
        : Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        let rec close
            (ctx : ConcretizationContext<DumpedAssembly>)
            (argument : SubstitutionArgument)
            : Result<ConcreteTypeHandle, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
            =
            match argument with
            | SubstitutionArgument.Closed handle -> Ok handle, ctx
            | SubstitutionArgument.Formal (owner, index) ->
                failwith
                    $"Cannot concretise a substitution that mentions type variable !%d{index} of %s{owner.AssemblyFullName}/%O{owner.TypeDefinition.Get}, which denotes no particular type; rebase it onto closed arguments first"
            | SubstitutionArgument.Spelled (assembly, spelling, inner) ->
                closeAll ctx inner
                |> andThen (fun ctx typeGenerics ->
                    concretizeType ctx loadAssembly assembly typeGenerics ImmutableArray.Empty spelling
                )

        and closeAll
            (ctx : ConcretizationContext<DumpedAssembly>)
            (arguments : ImmutableArray<SubstitutionArgument>)
            : Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
            =
            concretizeEach close ctx arguments
            |> andThen (fun ctx handles -> Ok (ImmutableArray.CreateRange handles), ctx)

        closeAll ctx context.Arguments

    /// One side of a method-signature comparison: a signature as its own blob spells it, the
    /// assembly whose token space those spellings live in, and the instantiation of the type
    /// that declared it — which is what ECMA-335 `!0` denotes in this signature.
    type SignatureComparand =
        {
            Signature : TypeMethodSignature<TypeDefn>
            AssemblyFullName : string
            DeclaringTypeGenerics : SubstitutionContext
        }

    /// One side of a comparison that applies no substitution to either signature: a signature as its
    /// own blob spells it, and the assembly whose token space those spellings live in.
    ///
    /// Nothing says which type declared the signature, because nothing needs to: with no
    /// substitution, a type variable is identified by its index alone.
    type UnsubstitutedComparand =
        {
            Signature : TypeMethodSignature<TypeDefn>
            AssemblyFullName : string
        }

    /// How the `!i` of one element context are read.
    type private TypeVariables =
        /// Through the declaring type's instantiation: `!i` denotes the `i`th argument.
        | Substituted of ImmutableArray<SubstitutionArgument>
        /// Not at all: `!i` is left standing and compared by index alone, whichever type it belongs
        /// to. This is `MetaSig::CompareElementType` given a null `Substitution`.
        | Unsubstituted

    /// The token space and generic context one signature element is spelled in.
    type private ElementContext =
        {
            AssemblyFullName : string
            TypeVariables : TypeVariables
        }

    /// A signature element under comparison: still spelled in a blob, a closed runtime type that a
    /// substitution supplied in place of a generic type parameter, a type variable of a definition
    /// that no substitution replaced, or a type variable in a comparison that substitutes nothing.
    type private Element =
        | Spelled of ElementContext * TypeDefn
        | Substituted of ConcreteTypeHandle
        | Formal of owner : ResolvedTypeIdentity * index : int
        | Positional of index : int

    /// What a substitution supplied for one `!i`, as an element of a comparison.
    let private elementOfArgument (argument : SubstitutionArgument) : Element =
        match argument with
        | SubstitutionArgument.Closed handle -> Element.Substituted handle
        | SubstitutionArgument.Formal (owner, index) -> Element.Formal (owner, index)
        | SubstitutionArgument.Spelled (assembly, spelling, context) ->
            Element.Spelled (
                {
                    ElementContext.AssemblyFullName = assembly
                    ElementContext.TypeVariables = TypeVariables.Substituted context
                },
                spelling
            )

    /// The runtime types a context supplies, for the one comparison step that has to concretise a
    /// spelled element rather than compare it structurally.
    ///
    /// Fails rather than approximating: reaching it with an unsubstituted variable in the context means
    /// a closed element and a definition-rooted one met, and the two describe different worlds. A
    /// `false` here would silently lay out a method table wrongly, which is the failure this
    /// representation exists to prevent.
    let private requireClosedArguments
        (assembly : string)
        (variables : TypeVariables)
        : ImmutableArray<ConcreteTypeHandle>
        =
        match variables with
        | TypeVariables.Unsubstituted ->
            failwith
                $"Signature comparison in %s{assembly} met a closed runtime type on one side and a signature read with no substitution on the other; a comparison either substitutes on both sides or on neither"
        | TypeVariables.Substituted arguments ->

        arguments
        |> ImmutableArray.map (fun argument ->
            match argument with
            | SubstitutionArgument.Closed handle -> handle
            | SubstitutionArgument.Formal (owner, index) ->
                failwith
                    $"Signature comparison in %s{assembly} met a closed runtime type on one side and type variable !%d{index} of %s{owner.AssemblyFullName}/%O{owner.TypeDefinition.Get} on the other; a comparison is rooted either at closed instantiations or at a generic definition, so the two comparands were built against different declaring contexts"
            | SubstitutionArgument.Spelled (spelledAssembly, spelling, _) ->
                failwith
                    $"Signature comparison in %s{assembly} needs the runtime type of a substituted argument, but the substitution supplied %O{spelling} still spelled in %s{spelledAssembly}; only a definition-rooted comparison carries those, and it never concretises"
        )

    /// What one context's `!index` is, as an element of a comparison.
    let private readTypeVariable (context : ElementContext) (index : int) : Element =
        match context.TypeVariables with
        | TypeVariables.Unsubstituted -> Element.Positional index
        | TypeVariables.Substituted arguments ->
            if index >= arguments.Length then
                failwithf
                    "Signature comparison in %s reached generic type parameter !%d, but the declaring type's instantiation supplies only %d argument(s); the comparand was built with the wrong instantiation"
                    context.AssemblyFullName
                    index
                    arguments.Length

            elementOfArgument arguments.[index]

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
        (assemblyFullName : string)
        (ty : TypeDefn)
        : Result<ResolvedTypeIdentity option, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        match ty with
        | TypeDefn.FromDefinition (identity, _) -> Ok (Some identity), ctx
        | TypeDefn.FromReference (typeRef, _) ->
            loadAssemblyAndResolveTypeRef loadAssembly ctx assemblyFullName typeRef
            |> andThen (fun ctx (_, identity, _) -> Ok (Some identity), ctx)
        | _ -> Ok None, ctx

    /// Each comparison in order, answering unequal at the first that does, or failing at the first
    /// that fails; the later ones are not made.
    let rec private allEqual
        (compare :
            ConcretizationContext<DumpedAssembly>
                -> 'a
                -> Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>)
        (ctx : ConcretizationContext<DumpedAssembly>)
        (items : 'a list)
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        match items with
        | [] -> Ok true, ctx
        | item :: rest ->
            match compare ctx item with
            | Ok true, ctx -> allEqual compare ctx rest
            | answered -> answered

    /// `MetaSig::CompareElementType` (siginfo.cpp:3781), which is a comparison of *blobs*: two
    /// signature elements are the same only if they are spelled with the same element types, so
    /// `M(object)` and `M(class System.Object)` are different signatures, and a custom modifier is
    /// part of the element rather than an annotation on it.
    let rec private compareElements
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (left : Element)
        (right : Element)
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
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
            compareElements ctx loadAssembly (readTypeVariable leftCtx index) right
        | _, Element.Spelled (rightCtx, TypeDefn.GenericTypeParameter index) ->
            compareElements ctx loadAssembly left (readTypeVariable rightCtx index)

        | Element.Substituted leftHandle, Element.Substituted rightHandle -> Ok (leftHandle = rightHandle), ctx

        // Two variables no substitution replaced. CoreCLR compares such a pair positionally
        // (`varNum1 == varNum2`, siginfo.cpp:4067-4076) and needs no owner, because the chain it builds
        // guarantees every surviving variable belongs to the type being laid out. The owner is carried
        // and checked here instead of trusted: a mismatch means two chains rooted at different
        // definitions met, and comparing their indices would silently equate unrelated variables.
        | Element.Formal (leftOwner, leftIndex), Element.Formal (rightOwner, rightIndex) ->
            if leftOwner <> rightOwner then
                failwith
                    $"Signature comparison compared type variable !%d{leftIndex} of %s{leftOwner.AssemblyFullName}/%O{leftOwner.TypeDefinition.Get} against !%d{rightIndex} of %s{rightOwner.AssemblyFullName}/%O{rightOwner.TypeDefinition.Get}; a definition-rooted comparison resolves every ancestor's variables into the vocabulary of the one definition it is laid out on, so two owners meeting means the substitution chain was built wrong"

            Ok (leftIndex = rightIndex), ctx

        // A variable that stands for no particular type cannot equal one that does. This is the
        // mixed-world case `requireClosedArguments` describes, reached when the variable is the whole
        // element rather than inside a spelled one.
        | Element.Formal (owner, index), Element.Substituted handle
        | Element.Substituted handle, Element.Formal (owner, index) ->
            failwith
                $"Signature comparison compared runtime type %O{handle} against type variable !%d{index} of %s{owner.AssemblyFullName}/%O{owner.TypeDefinition.Get}; a comparison is rooted either at closed instantiations or at a generic definition, so the two comparands were built against different declaring contexts"

        // `varNum1 == varNum2` with no owner to check, because there is none to have: the
        // comparison substitutes nothing on either side, so each variable is whichever of its own
        // declaring type's variables sits at that index.
        | Element.Positional leftIndex, Element.Positional rightIndex -> Ok (leftIndex = rightIndex), ctx

        // Only a comparison that substitutes on neither side produces a `Positional`, and only one
        // that substitutes produces anything else but `Spelled`; `signaturesEquivalentWithoutSubstitution`
        // is the sole entry to the first and builds both of its sides the same way.
        | Element.Positional index, (Element.Substituted _ | Element.Formal _)
        | (Element.Substituted _ | Element.Formal _), Element.Positional index ->
            failwith
                $"Signature comparison compared type variable !%d{index}, read with no substitution, against an element that a substitution supplied; a comparison either substitutes on both sides or on neither"

        // As for `Formal` against `Spelled`, below: the spelled side is not a bare
        // `ELEMENT_TYPE_VAR`, so it is a different element type.
        | Element.Positional _, Element.Spelled _
        | Element.Spelled _, Element.Positional _ -> Ok false, ctx

        // The spelled side is not a bare `ELEMENT_TYPE_VAR`: the arms above resolve those through the
        // substitution before anything else, exactly as `CompareElementType` consumes VAR ahead of the
        // modifiers (:3820-3866). So whatever it names -- a nominal type, a primitive, an array, a
        // method variable -- is a different element type from an unsubstituted type variable, and
        // CoreCLR's comparison of the two element types fails.
        | Element.Formal _, Element.Spelled _
        | Element.Spelled _, Element.Formal _ -> Ok false, ctx

        | Element.Substituted handle, Element.Spelled (spelledCtx, ty)
        | Element.Spelled (spelledCtx, ty), Element.Substituted handle ->
            // One side is a closed runtime type, so the other can only match if it denotes one too.
            if mentionsMethodGenericParameter ty then
                Ok false, ctx
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
                Ok false, ctx
            else

            concretizeType
                ctx
                loadAssembly
                spelledCtx.AssemblyFullName
                (requireClosedArguments spelledCtx.AssemblyFullName spelledCtx.TypeVariables)
                ImmutableArray.Empty
                ty
            |> andThen (fun ctx spelledHandle -> Ok (spelledHandle = handle), ctx)

        | Element.Spelled (leftCtx, leftTy), Element.Spelled (rightCtx, rightTy) ->

        let recurse (ctx : ConcretizationContext<DumpedAssembly>) (l : TypeDefn) (r : TypeDefn) =
            compareElements ctx loadAssembly (Element.Spelled (leftCtx, l)) (Element.Spelled (rightCtx, r))

        let recursePair (ctx : ConcretizationContext<DumpedAssembly>) (l : TypeDefn, r : TypeDefn) = recurse ctx l r

        match leftTy, rightTy with
        // A modifier is compared before what it modifies, in blob order, with `modreq` and `modopt`
        // distinguished (:4082-4100). So `modopt(A) modopt(B) int32` and `modopt(B) modopt(A) int32`
        // are different signatures, and an unmodified type never matches a modified one.
        | TypeDefn.Modified leftMod, TypeDefn.Modified rightMod ->
            if leftMod.IsRequired <> rightMod.IsRequired then
                Ok false, ctx
            else
                allEqual
                    recursePair
                    ctx
                    [
                        leftMod.Modifier, rightMod.Modifier
                        leftMod.Unmodified, rightMod.Unmodified
                    ]
        | TypeDefn.Modified _, _
        | _, TypeDefn.Modified _ -> Ok false, ctx

        | TypeDefn.PrimitiveType leftPrim, TypeDefn.PrimitiveType rightPrim -> Ok (leftPrim = rightPrim), ctx

        | TypeDefn.Void, TypeDefn.Void -> Ok true, ctx

        // Compared positionally and symbolically: `varNum1 == varNum2` (:4068-4077). No
        // substitution is ever applied to a method generic parameter, which is what lets two
        // generic methods' signatures be compared without an instantiation for either.
        | TypeDefn.GenericMethodParameter leftIndex, TypeDefn.GenericMethodParameter rightIndex ->
            Ok (leftIndex = rightIndex), ctx

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
                Ok false, ctx
            else
                recurse ctx leftElement rightElement

        | TypeDefn.GenericInstantiation (leftGeneric, leftArgs), TypeDefn.GenericInstantiation (rightGeneric, rightArgs) ->
            if leftArgs.Length <> rightArgs.Length then
                Ok false, ctx
            else

            allEqual
                recursePair
                ctx
                ((leftGeneric, rightGeneric)
                 :: List.zip (List.ofSeq leftArgs) (List.ofSeq rightArgs))

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
                unmodelled
                    $"TODO: comparing two function pointer signatures that spell the same GENERIC calling convention and the same generic-parameter count (in %s{leftCtx.AssemblyFullName} against %s{rightCtx.AssemblyFullName}); from here CoreCLR compares elements read at a one-integer offset into each blob, reinterpreting the parameter-count byte as an element type, which cannot be reproduced from a decoded signature",
                ctx
            else

            compareSignatureTypes ctx loadAssembly leftCtx rightCtx false false leftSignature rightSignature

        | (TypeDefn.FromDefinition _ | TypeDefn.FromReference _), (TypeDefn.FromDefinition _ | TypeDefn.FromReference _) ->
            // Identity, not spelling: a TypeDef in the assembly that declares the type and a
            // TypeRef everywhere else name the same type, by `CompareTypeTokens`' own steps. Unlike CoreCLR, which reaches
            // `CompareTypeTokens` only once the ELEMENT_TYPE_CLASS/VALUETYPE bytes already agree,
            // this does not also compare `SignatureTypeKind`. For well-formed metadata the kind is
            // a fact about the resolved type rather than about the spelling, so it cannot
            // distinguish two references that resolve to one TypeDef; and PawPrint synthesises
            // nominal `TypeDefn`s in places (`SignatureTypeKind.Unknown` among them) where the
            // kind is not recovered from a blob at all.
            let assemblies, same =
                SignatureComparison.compareTypeTokens
                    loadAssembly
                    $"comparing a signature of %s{leftCtx.AssemblyFullName} with one of %s{rightCtx.AssemblyFullName}"
                    ctx.LoadedAssemblies
                    (ctx.LoadedAssemblies.ByDefinitionName leftCtx.AssemblyFullName)
                    leftTy
                    (ctx.LoadedAssemblies.ByDefinitionName rightCtx.AssemblyFullName)
                    rightTy

            same,
            { ctx with
                LoadedAssemblies = assemblies
            }

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
        | TypeDefn.FromReference _, _ -> Ok false, ctx

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
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        // Calling convention and `hasThis` (:4589). CoreCLR masks out CORINFO_CALLCONV_PARAMTYPE,
        // which it sets on signatures it builds itself; no metadata blob carries that bit, so
        // comparing the header byte as decoded is the same comparison here.
        if left.Header <> right.Header then
            Ok false, ctx
        elif left.GenericParameterCount <> right.GenericParameterCount then
            Ok false, ctx
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
        | None -> Ok false, ctx
        | Some comparableCount ->

        let returnComparison =
            if skipReturnType then
                // CoreCLR spells "allow a covariant return" as skipping the return type entirely
                // (`SignaturesEquivalent` passes `allowCovariantReturn` straight into
                // `skipReturnTypeSig`), leaving the caller to decide what return types it accepts.
                Ok true, ctx
            else
                match left.ReturnType, right.ReturnType with
                | MethodReturnType.Void, MethodReturnType.Void -> Ok true, ctx
                | MethodReturnType.Returns leftTy, MethodReturnType.Returns rightTy -> recurse ctx leftTy rightTy
                | MethodReturnType.Void, MethodReturnType.Returns _
                | MethodReturnType.Returns _, MethodReturnType.Void -> Ok false, ctx

        match returnComparison with
        | Ok true, ctx ->
            List.zip
                (List.truncate comparableCount left.ParameterTypes)
                (List.truncate comparableCount right.ParameterTypes)
            |> allEqual (fun ctx (l, r) -> recurse ctx l r) ctx
        | answered -> answered

    /// One side of a generic method's constraint comparison: what its type parameters are
    /// constrained to, and the token space and declaring-type instantiation those constraints are
    /// written in.
    type ConstraintComparand =
        {
            Parameters : GenericParamMetadata list
            AssemblyFullName : string
            DeclaringTypeGenerics : SubstitutionContext
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
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        if impl.Parameters.Length <> decl.Parameters.Length then
            Ok false, ctx
        else

        let implCtx : ElementContext =
            {
                AssemblyFullName = impl.AssemblyFullName
                TypeVariables = TypeVariables.Substituted impl.DeclaringTypeGenerics.Arguments
            }

        let declCtx : ElementContext =
            {
                AssemblyFullName = decl.AssemblyFullName
                TypeVariables = TypeVariables.Substituted decl.DeclaringTypeGenerics.Arguments
            }

        // A constraint naming `System.Object` says nothing, and neither does one naming
        // `System.ValueType` on a parameter already constrained to a non-nullable value type.
        // CoreCLR skips both rather than looking for a match, because the overridden parameter is
        // entitled to leave them implicit (:5069-5079).
        let isVacuous
            (ctx : ConcretizationContext<DumpedAssembly>)
            (isNotNullableValueType : bool)
            (constraintType : TypeDefn)
            : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
            =
            // `System.Object` has a primitive spelling as well as a nominal one, and a constraint
            // may use either: the GenericParamConstraint column is a TypeDefOrRefOrSpec, and a
            // TypeSpec may hold a bare `ELEMENT_TYPE_OBJECT`. CoreCLR resolves that spelling to
            // System.Object's TypeDef before comparing (`CompareElementTypeToToken`,
            // siginfo.cpp:4915), so it is just as vacuous as the nominal form.
            match constraintType with
            | TypeDefn.PrimitiveType PrimitiveType.Object -> Ok true, ctx
            | _ ->

            nominalIdentity ctx loadAssembly impl.AssemblyFullName constraintType
            |> andThen (fun ctx identity ->
                match identity with
                | None -> Ok false, ctx
                | Some identity ->

                let isBaseType (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) =
                    identity = ResolvedTypeIdentity.ofDefinitionInAssembly ty.AssemblyFullName ty.TypeDefHandle

                Ok (
                    isBaseType ctx.BaseTypes.Object
                    || (isNotNullableValueType && isBaseType ctx.BaseTypes.ValueType)
                ),
                ctx
            )

        // Whether one of `declConstraints` equals `implConstraint`, comparing no further than the
        // first that does.
        let rec anyEqual
            (ctx : ConcretizationContext<DumpedAssembly>)
            (implConstraint : TypeDefn)
            (declConstraints : TypeDefn list)
            : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
            =
            match declConstraints with
            | [] -> Ok false, ctx
            | declConstraint :: rest ->
                match
                    compareElements
                        ctx
                        loadAssembly
                        (Element.Spelled (implCtx, implConstraint))
                        (Element.Spelled (declCtx, declConstraint))
                with
                | Ok false, ctx -> anyEqual ctx implConstraint rest
                | answered -> answered

        let parameterMatches
            (ctx : ConcretizationContext<DumpedAssembly>)
            (implParam : GenericParamMetadata, declParam : GenericParamMetadata)
            : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
            =
            let isNotNullableValueType = implParam.RequiresNonNullableValueType

            let specialsMatch =
                // Each of these says the override must not demand more of a type argument than
                // the method it overrides already demanded.
                (not implParam.RequiresNonNullableValueType
                 || declParam.RequiresNonNullableValueType)
                && (not implParam.RequiresReferenceType || declParam.RequiresReferenceType)
                && (not implParam.RequiresParameterlessConstructor
                    || declParam.RequiresParameterlessConstructor
                    // A non-nullable value type always has a parameterless constructor.
                    || declParam.RequiresNonNullableValueType)
                // `allows ref struct` runs the other way, because it *widens* what the parameter
                // accepts: the override has to keep accepting what the base accepted.
                && (not declParam.AllowsByRefLike || implParam.AllowsByRefLike)

            if not specialsMatch then
                Ok false, ctx
            else

            List.ofSeq implParam.Constraints
            |> allEqual
                (fun ctx implConstraint ->
                    match isVacuous ctx isNotNullableValueType implConstraint with
                    | Ok false, ctx -> anyEqual ctx implConstraint (List.ofSeq declParam.Constraints)
                    | answered -> answered
                )
                ctx

        allEqual parameterMatches ctx (List.zip impl.Parameters decl.Parameters)

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
    ///
    /// A type the comparison must bind and cannot is the result's failure, as is a pair of generic
    /// function pointers whose comparison CoreCLR reads misaligned.
    let signaturesEquivalent
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (skipReturnType : bool)
        (caller : SignatureComparand)
        (callee : SignatureComparand)
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        let toElementContext (comparand : SignatureComparand) : ElementContext =
            {
                AssemblyFullName = comparand.AssemblyFullName
                TypeVariables = TypeVariables.Substituted comparand.DeclaringTypeGenerics.Arguments
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

    /// `signaturesEquivalent` with no substitution applied to either side: `MetaSig::CompareElementType`
    /// handed a null `Substitution` for both blobs, which is how `[UnsafeAccessor]` matching compares
    /// a declaration against a candidate (unsafeaccessors.cpp:401 and :408).
    ///
    /// A generic type parameter is then compared by its index alone, like a generic method
    /// parameter, so `!0` in one signature equals `!0` in the other whichever types declared them,
    /// and equals nothing else: not a type an instantiation might supply for it, and not `!!0`.
    let signaturesEquivalentWithoutSubstitution
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (skipReturnType : bool)
        (caller : UnsubstitutedComparand)
        (callee : UnsubstitutedComparand)
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        let toElementContext (comparand : UnsubstitutedComparand) : ElementContext =
            {
                AssemblyFullName = comparand.AssemblyFullName
                TypeVariables = TypeVariables.Unsubstituted
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

    /// Do two instantiations of one generic definition name the same type, in the sense of
    /// `MetaSig::CompareTypeDefsUnderSubstitutions`? Each argument is compared as a signature element
    /// would be, so two instantiations spelled in different assemblies agree when they denote the same
    /// types, while a type variable left standing equals only itself.
    ///
    /// The caller is responsible for both contexts belonging to the same generic definition.
    let substitutionsEquivalent
        (ctx : ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (left : SubstitutionContext)
        (right : SubstitutionContext)
        : Result<bool, TypeBindingFailure> * ConcretizationContext<DumpedAssembly>
        =
        if left.Arguments.Length <> right.Arguments.Length then
            Ok false, ctx
        else
            List.zip (List.ofSeq left.Arguments) (List.ofSeq right.Arguments)
            |> allEqual
                (fun ctx (l, r) -> compareElements ctx loadAssembly (elementOfArgument l) (elementOfArgument r))
                ctx

/// High-level API for concretizing types
[<RequireQualifiedAccess>]
module Concretization =

    /// Helper to concretize an array of types, stopping at the first that fails
    let private concretizeTypeArray
        (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>)
        (loadAssembly : IAssemblyLoad)
        (assemblyFullName : string)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        (types : ImmutableArray<TypeDefn>)
        : Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure> *
          TypeConcretization.ConcretizationContext<DumpedAssembly>
        =
        let handles = ImmutableArray.CreateBuilder types.Length

        let rec go (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>) (i : int) =
            if i >= types.Length then
                Ok (handles.ToImmutable ()), ctx
            else
                match
                    TypeConcretization.concretizeType ctx loadAssembly assemblyFullName typeArgs methodArgs types.[i]
                with
                | Ok handle, ctx ->
                    handles.Add handle
                    go ctx (i + 1)
                | Error failure, ctx -> Error failure, ctx

        go ctx 0

    /// Force-load every assembly needed for CliType.zeroOf to zero-initialise the
    /// given concrete handle. zeroOf calls LoadedTypeInfo.isValueType on the top
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
    ///
    /// Stops at the first type that does not bind, and returns why, with what was loaded and
    /// registered before it.
    let rec ensureBaseAssembliesLoadedForConcreteHandle
        (loadAssembly : IAssemblyLoad)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (visited : System.Collections.Generic.HashSet<ConcreteTypeHandle>)
        (assemblies : LoadedAssemblies)
        (concreteTypes : AllConcreteTypes)
        (handle : ConcreteTypeHandle)
        : LoadedAssemblies * AllConcreteTypes * TypeBindingFailure option
        =
        if not (visited.Add handle) then
            assemblies, concreteTypes, None
        else
            match handle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.FunctionPointer _ ->
                // Terminal in zeroOf — see the doc comment above.
                assemblies, concreteTypes, None
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle concreteTypes with
                | None -> assemblies, concreteTypes, None
                | Some concreteType ->
                    match
                        BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
                            loadAssembly
                            assemblies
                            (assemblies.ByDefinitionName concreteType.AssemblyFullName)
                            concreteType.Definition.Get
                    with
                    | assemblies, Some failure ->
                        assemblies, concreteTypes, Some (TypeBindingFailure.ofBaseChain failure)
                    | assemblies, None ->

                    let outerAssembly = assemblies.ByDefinitionName concreteType.AssemblyFullName
                    let outerTypeDef = outerAssembly.TypeDefs.[concreteType.Definition.Get]

                    // Reference types terminate in zeroOf as null; fields (and,
                    // by extension, generic arguments only reachable via fields)
                    // are never inspected. Do NOT descend — descent into a
                    // reference type's generics or fields can loop forever on
                    // legal shapes such as `class Box<T> {}` used inside
                    // `struct S<T> { Box<S<S<T>>> F; }`.
                    if not (LoadedTypeInfo.isValueType baseTypes assemblies outerTypeDef) then
                        assemblies, concreteTypes, None
                    else
                        // Value type: zeroOf recurses into every non-static
                        // instance field, so each field type's own base-chain
                        // assemblies must also be loaded. Concretize each
                        // field's TypeDefn under the outer type's generic
                        // context (this covers any generic-parameter uses
                        // inside the field type), then recurse.
                        let rec primeFields
                            (assemblies : LoadedAssemblies)
                            (concreteTypes : AllConcreteTypes)
                            (fields : FieldInfo<GenericParamFromMetadata, TypeDefn> list)
                            =
                            match fields with
                            | [] -> assemblies, concreteTypes, None
                            | field :: rest ->
                                let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
                                    {
                                        TypeConcretization.ConcretizationContext.ConcreteTypes = concreteTypes
                                        TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                                        TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
                                    }

                                // Fields never carry method-level generics; the
                                // outer type's already-concretized generic
                                // arguments cover the field's substitution.
                                match
                                    TypeConcretization.concretizeType
                                        ctx
                                        loadAssembly
                                        concreteType.AssemblyFullName
                                        concreteType.Generics
                                        ImmutableArray.Empty
                                        field.Signature
                                with
                                | Error failure, ctx -> ctx.LoadedAssemblies, ctx.ConcreteTypes, Some failure
                                | Ok fieldHandle, ctx ->
                                    match
                                        ensureBaseAssembliesLoadedForConcreteHandle
                                            loadAssembly
                                            baseTypes
                                            visited
                                            ctx.LoadedAssemblies
                                            ctx.ConcreteTypes
                                            fieldHandle
                                    with
                                    | assemblies, concreteTypes, None -> primeFields assemblies concreteTypes rest
                                    | stopped -> stopped

                        outerTypeDef.Fields
                        |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                        |> primeFields assemblies concreteTypes

    /// Concretize a method's signature and body.
    ///
    /// `typeArgs` instantiates the declaring type and must have exactly one handle per generic
    /// parameter it declares (counting those a nested type inherits from its declaring types);
    /// `methodArgs` likewise must have exactly one handle per generic parameter of the method.
    /// Any other length fails. A non-generic declaring type reads nothing from `typeArgs`.
    ///
    /// A type the method names that does not bind is the result's failure, with what was loaded
    /// and registered before it.
    let concretizeMethod
        (ctx : AllConcreteTypes)
        (loadAssembly : IAssemblyLoad)
        (assemblies : LoadedAssemblies)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        : Result<
              WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>,
              TypeBindingFailure
           > *
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
        match
            BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
                loadAssembly
                assemblies
                (assemblies.ByDefinitionName declaringType.AssemblyFullName)
                declaringType.Definition.Get
        with
        | assemblies, Some failure -> Error (TypeBindingFailure.ofBaseChain failure), ctx, assemblies
        | assemblies, None ->

        let concCtx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = ctx
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
            }

        // Checked up front, before the signature is concretized against `methodArgs`: a short list
        // would otherwise surface as "generic method parameter out of range" from whichever
        // signature position first mentions the missing parameter, and a long one would not
        // surface at all.
        if method.Generics.Length <> methodArgs.Length then
            failwith
                $"Method %s{declaringType.Namespace}.%s{declaringType.Name}.%s{method.Name} in %s{declaringType.AssemblyFullName} declares %d{method.Generics.Length} generic parameter(s), but %d{methodArgs.Length} method argument(s) were supplied"

        // First, we need to create a TypeDefn for the declaring type with its generics instantiated
        let declaringTypeDefn =
            if declaringType.Generics.IsEmpty then
                // Non-generic type - determine the SignatureTypeKind
                let assy = concCtx.LoadedAssemblies.ByDefinitionName declaringType.AssemblyFullName
                let arg = assy.TypeDefs.[declaringType.Definition.Get]

                let signatureTypeKind =
                    LoadedTypeInfo.signatureTypeKind baseTypes concCtx.LoadedAssemblies arg

                TypeDefn.FromDefinition (declaringType.Identity, signatureTypeKind)
            else
                // Generic type - create a GenericInstantiation
                let assy = concCtx.LoadedAssemblies.ByDefinitionName declaringType.AssemblyFullName
                let arg = assy.TypeDefs.[declaringType.Definition.Get]

                let signatureTypeKind =
                    LoadedTypeInfo.signatureTypeKind baseTypes concCtx.LoadedAssemblies arg

                let baseType = TypeDefn.FromDefinition (declaringType.Identity, signatureTypeKind)

                let genericArgsLength = declaringType.Generics.Length

                // Exactly one argument per parameter. A longer list is refused rather than
                // truncated: its prefix would be the wrong instantiation whenever the caller had a
                // derived type's arguments in hand, and nothing downstream could tell.
                if genericArgsLength <> typeArgs.Length then
                    failwith
                        $"Method %s{method.Name} is declared on %s{declaringType.Namespace}.%s{declaringType.Name} in %s{declaringType.AssemblyFullName}, which declares %d{genericArgsLength} generic parameter(s), but %d{typeArgs.Length} type argument(s) were supplied"

                let genericArgs =
                    Array.init genericArgsLength TypeDefn.GenericTypeParameter
                    |> ImmutableArray.CreateRange

                TypeDefn.GenericInstantiation (baseType, genericArgs)

        let stopped (failure : TypeBindingFailure) (ctx : TypeConcretization.ConcretizationContext<DumpedAssembly>) =
            Error failure, ctx.ConcreteTypes, ctx.LoadedAssemblies

        // Concretize the declaring type
        match
            TypeConcretization.concretizeType
                concCtx
                loadAssembly
                declaringType.AssemblyFullName
                typeArgs
                methodArgs
                declaringTypeDefn
        with
        | Error failure, concCtx -> stopped failure concCtx
        | Ok declaringHandle, concCtx ->

        // Look up the concretized declaring type
        let concretizedDeclaringType =
            AllConcreteTypes.lookup declaringHandle concCtx.ConcreteTypes |> Option.get

        // Concretize signature
        match
            TypeConcretization.concretizeMethodSignature
                concCtx
                loadAssembly
                declaringType.AssemblyFullName
                typeArgs
                methodArgs
                method.Signature
        with
        | Error failure, concCtx -> stopped failure concCtx
        | Ok signature, concCtx ->

        // Concretize local variables (only IL bodies carry them).
        let body =
            match method.Body with
            | MethodBody.Il instr ->
                match instr.LocalVars with
                | None -> Ok (MethodBody.Il (MethodInstructions.setLocalVars None instr)), concCtx
                | Some vars ->
                    match
                        concretizeTypeArray concCtx loadAssembly declaringType.AssemblyFullName typeArgs methodArgs vars
                    with
                    | Ok handles, ctx -> Ok (MethodBody.Il (MethodInstructions.setLocalVars (Some handles) instr)), ctx
                    | Error failure, ctx -> Error failure, ctx
            | MethodBody.InternalCall -> Ok MethodBody.InternalCall, concCtx
            | MethodBody.PInvoke -> Ok MethodBody.PInvoke, concCtx
            | MethodBody.RuntimeProvided rb -> Ok (MethodBody.RuntimeProvided rb), concCtx
            | MethodBody.Abstract -> Ok MethodBody.Abstract, concCtx

        match body with
        | Error failure, concCtx2 -> stopped failure concCtx2
        | Ok body, concCtx2 ->

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

        let rec prime
            (assemblies : LoadedAssemblies)
            (concreteTypes : AllConcreteTypes)
            (handles : ConcreteTypeHandle list)
            =
            match handles with
            | [] -> Ok concretizedMethod, concreteTypes, assemblies
            | h :: rest ->
                match
                    ensureBaseAssembliesLoadedForConcreteHandle
                        loadAssembly
                        baseTypes
                        visited
                        assemblies
                        concreteTypes
                        h
                with
                | assemblies, concreteTypes, None -> prime assemblies concreteTypes rest
                | assemblies, concreteTypes, Some failure -> Error failure, concreteTypes, assemblies

        let returnHandles =
            match signature.ReturnType with
            | MethodReturnType.Void -> []
            | MethodReturnType.Returns h -> [ h ]

        let localHandles =
            match body with
            | MethodBody.Il instr ->
                match instr.LocalVars with
                | None -> []
                | Some vars -> List.ofSeq vars
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _
            | MethodBody.Abstract -> []

        // Parameters, the return, locals; then the method-level generic arguments (Unsafe.SizeOf<T>
        // etc read T here) and the declaring type's (Span<T>.Clear etc read T here).
        signature.ParameterTypes
        @ returnHandles
        @ localHandles
        @ List.ofSeq genericHandles
        @ List.ofSeq concretizedDeclaringType.Generics
        |> prime concCtx2.LoadedAssemblies concCtx2.ConcreteTypes

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
                LoadedTypeInfo.signatureTypeKind baseClassTypes assemblies typeDef

            if concreteType.Generics.IsEmpty then
                TypeDefn.FromDefinition (concreteType.Identity, signatureTypeKind)
            else
                // Recursively convert generic arguments
                let genericArgs =
                    concreteType.Generics
                    |> ImmutableArray.map (fun h -> concreteHandleToTypeDefn baseClassTypes h concreteTypes assemblies)

                let baseDef = TypeDefn.FromDefinition (concreteType.Identity, signatureTypeKind)

                TypeDefn.GenericInstantiation (baseDef, genericArgs)
