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
    /// A function-pointer type — C#'s `delegate*<...>` (managed calling convention) or
    /// `delegate* unmanaged<...>` (unmanaged calling convention). Both compile to ECMA-335
    /// FNPTR signatures and share this case; the calling convention lives in the signature's
    /// `Header` byte. Storage shape is a native-int-sized address, but signature identity must
    /// distinguish a function pointer from `IntPtr` — otherwise overload resolution would
    /// conflate `M(IntPtr)` with `M(delegate*<...>)`.
    | FunctionPointer of ConcreteFunctionPointerSignature

    override this.ToString () =
        match this with
        | ConcreteTypeHandle.Byref b -> "&" + b.ToString ()
        | ConcreteTypeHandle.Concrete i -> i.ToString ()
        | ConcreteTypeHandle.Pointer i -> "*" + i.ToString ()
        | ConcreteTypeHandle.OneDimArrayZero e -> e.ToString () + "[]"
        | ConcreteTypeHandle.Array (e, rank) ->
            let inside = if rank <= 1 then "*" else String.replicate (rank - 1) ","

            e.ToString () + "[" + inside + "]"
        | ConcreteTypeHandle.FunctionPointer sig' ->
            let parameters =
                sig'.ParameterTypes |> List.map (fun p -> p.ToString ()) |> String.concat ","

            let returnType =
                match sig'.ReturnType with
                | ConcreteFunctionPointerReturnType.Void -> "void"
                | ConcreteFunctionPointerReturnType.Returns ty -> ty.ToString ()

            "delegate*<" + parameters + "->" + returnType + ">"

/// A `ConcreteTypeHandle` adorned with optional ECMA-335 custom modifiers (modreq/modopt).
/// Custom modifiers form part of the *signature identity*: e.g. `delegate* unmanaged[Cdecl]<...>`
/// and `delegate* unmanaged[Stdcall]<...>` differ only in modopts on the return type, but are
/// distinct overloads. Storing the modifier chain alongside the underlying type lets the
/// concrete handle preserve that identity through round-trips.
///
/// The underlying type is itself a `ConcreteSignatureType`, which mirrors `ConcreteTypeHandle`
/// but uses `ConcreteTypeWithModifiers` for nested element positions. This is needed because
/// ECMA-335 permits custom modifiers below the outermost position (e.g. `int modopt(A)*`
/// applies modopt to the pointer's element type), and those modifiers participate in
/// signature identity just like outermost ones.
///
/// `Modifiers` is in outermost-first order — the first element of the list is the modifier
/// closest to the reading-order start of the signature, e.g. for `T modopt(M1) modopt(M2)`
/// it is `[(M1, false); (M2, false)]`.
and ConcreteTypeWithModifiers =
    {
        UnderlyingType : ConcreteSignatureType
        Modifiers : (ConcreteTypeHandle * bool) list
    }

    override this.ToString () =
        match this.Modifiers with
        | [] -> this.UnderlyingType.ToString ()
        | mods ->
            let modStr =
                mods
                |> List.map (fun (m, isReq) ->
                    let kw = if isReq then "modreq" else "modopt"
                    kw + "(" + m.ToString () + ")"
                )
                |> String.concat " "

            this.UnderlyingType.ToString () + " " + modStr

/// A type appearing inside a function-pointer signature element. Mirrors `ConcreteTypeHandle`
/// but composite variants reference `ConcreteTypeWithModifiers` so that custom modifiers on
/// nested element positions participate in signature identity. For example, `int modopt(A)*`
/// and `int modopt(B)*` differ only in a modifier below the outermost position; the only way
/// to keep them distinct as FP-signature components is to recursively wrap composite element
/// types alongside their modifier chains.
and [<RequireQualifiedAccess>] ConcreteSignatureType =
    /// A nominal type as registered in `AllConcreteTypes`. The wrapped handle is always a
    /// non-composite leaf and never a generic instantiation — those use the dedicated
    /// variants below so nested modifiers can be tracked per element / per generic argument.
    | Concrete of ConcreteTypeHandle
    | Byref of element : ConcreteTypeWithModifiers
    | Pointer of element : ConcreteTypeWithModifiers
    | OneDimArrayZero of element : ConcreteTypeWithModifiers
    | Array of element : ConcreteTypeWithModifiers * rank : int
    | FunctionPointer of ConcreteFunctionPointerSignature
    /// A generic instantiation appearing inside an FP signature. `ResolvedHandle` is the
    /// modifier-blind concrete handle (i.e. the result of `concretizeType` on the whole
    /// instantiation, with modifiers stripped from the args) so callers that only care
    /// about runtime identity can flatten via `toHandle` cheaply. `Args` carries the
    /// modifier-preserving versions of the generic arguments — necessary because
    /// ECMA-335 lets each generic arg carry its own custom modifiers, and those modifiers
    /// participate in signature identity (e.g. `delegate*<G<int modopt(A)>, void>` vs
    /// `delegate*<G<int modopt(B)>, void>`).
    | GenericInstantiation of resolvedHandle : ConcreteTypeHandle * args : ConcreteTypeWithModifiers list

    override this.ToString () =
        match this with
        | ConcreteSignatureType.Concrete h -> h.ToString ()
        | ConcreteSignatureType.Byref e -> "&" + e.ToString ()
        | ConcreteSignatureType.Pointer e -> "*" + e.ToString ()
        | ConcreteSignatureType.OneDimArrayZero e -> e.ToString () + "[]"
        | ConcreteSignatureType.Array (e, rank) ->
            let inside = if rank <= 1 then "*" else String.replicate (rank - 1) ","
            e.ToString () + "[" + inside + "]"
        | ConcreteSignatureType.FunctionPointer sig' ->
            let parameters =
                sig'.ParameterTypes |> List.map (fun p -> p.ToString ()) |> String.concat ","

            let returnType =
                match sig'.ReturnType with
                | ConcreteFunctionPointerReturnType.Void -> "void"
                | ConcreteFunctionPointerReturnType.Returns ty -> ty.ToString ()

            "delegate*<" + parameters + "->" + returnType + ">"
        | ConcreteSignatureType.GenericInstantiation (resolvedHandle, args) ->
            let argStr = args |> List.map (fun a -> a.ToString ()) |> String.concat ","
            resolvedHandle.ToString () + "<" + argStr + ">"

/// The return shape of a concrete function-pointer signature. `Void` is not a runtime type;
/// it indicates the callee returns nothing. `Returns` carries the underlying type plus any
/// custom modifiers that decorated the return position (notably calling-convention modopts).
and [<RequireQualifiedAccess>] ConcreteFunctionPointerReturnType =
    | Void
    | Returns of ConcreteTypeWithModifiers

/// The fully concretized form of a function-pointer signature (`TypeDefn.FunctionPointer`),
/// covering both managed (`delegate*<...>`) and unmanaged (`delegate* unmanaged<...>`)
/// flavours. Differs from `TypeMethodSignature<ConcreteTypeHandle>` in that each
/// return/parameter position retains its custom-modifier chain so that e.g.
/// `delegate* unmanaged[Cdecl]<...>` and `delegate* unmanaged[Stdcall]<...>` remain distinct
/// concrete types (calling convention is encoded as a modopt on the return type).
and ConcreteFunctionPointerSignature =
    {
        Header : ComparableSignatureHeader
        ReturnType : ConcreteFunctionPointerReturnType
        ParameterTypes : ConcreteTypeWithModifiers list
        GenericParameterCount : int
        RequiredParameterCount : int
    }

[<RequireQualifiedAccess>]
module ConcreteSignatureType =
    /// Lift a `ConcreteTypeHandle` into a `ConcreteSignatureType`, recursively breaking out
    /// composite handles into the dedicated variants so subsequent additions of nested
    /// modifiers can attach at any level. Composite element handles are wrapped in a
    /// `ConcreteTypeWithModifiers` with an empty modifier list.
    let rec ofHandle (h : ConcreteTypeHandle) : ConcreteSignatureType =
        match h with
        | ConcreteTypeHandle.Concrete _ -> ConcreteSignatureType.Concrete h
        | ConcreteTypeHandle.Byref inner ->
            ConcreteSignatureType.Byref
                {
                    UnderlyingType = ofHandle inner
                    Modifiers = []
                }
        | ConcreteTypeHandle.Pointer inner ->
            ConcreteSignatureType.Pointer
                {
                    UnderlyingType = ofHandle inner
                    Modifiers = []
                }
        | ConcreteTypeHandle.OneDimArrayZero inner ->
            ConcreteSignatureType.OneDimArrayZero
                {
                    UnderlyingType = ofHandle inner
                    Modifiers = []
                }
        | ConcreteTypeHandle.Array (inner, rank) ->
            ConcreteSignatureType.Array (
                {
                    UnderlyingType = ofHandle inner
                    Modifiers = []
                },
                rank
            )
        | ConcreteTypeHandle.FunctionPointer signature -> ConcreteSignatureType.FunctionPointer signature

    /// Flatten a `ConcreteSignatureType` to a `ConcreteTypeHandle`, dropping any nested
    /// modifier chains. The resulting handle reflects runtime type identity (the storage
    /// shape) but loses signature-identity distinctions that depend on nested modifiers.
    let rec toHandle (s : ConcreteSignatureType) : ConcreteTypeHandle =
        match s with
        | ConcreteSignatureType.Concrete h -> h
        | ConcreteSignatureType.Byref e -> ConcreteTypeHandle.Byref (toHandle e.UnderlyingType)
        | ConcreteSignatureType.Pointer e -> ConcreteTypeHandle.Pointer (toHandle e.UnderlyingType)
        | ConcreteSignatureType.OneDimArrayZero e -> ConcreteTypeHandle.OneDimArrayZero (toHandle e.UnderlyingType)
        | ConcreteSignatureType.Array (e, rank) -> ConcreteTypeHandle.Array (toHandle e.UnderlyingType, rank)
        | ConcreteSignatureType.FunctionPointer signature -> ConcreteTypeHandle.FunctionPointer signature
        // The resolved handle was built by passing the whole instantiation through the
        // standard concretizer, which strips per-arg modifiers; that is exactly the
        // modifier-blind handle we want here.
        | ConcreteSignatureType.GenericInstantiation (resolvedHandle, _) -> resolvedHandle

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
        | ConcreteTypeHandle.FunctionPointer _ -> None // Function pointer types are structural wrappers

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
    abstract LoadAssembly :
        loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> ->
        referencedIn : AssemblyName ->
        handle : AssemblyReferenceHandle ->
            ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly

[<RequireQualifiedAccess>]
module TypeConcretization =
    type ConcretizationContext<'corelib> =
        {
            /// All concrete types created so far
            ConcreteTypes : AllConcreteTypes
            /// For resolving type references
            LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
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
            let currentAssy =
                match ctx.LoadedAssemblies.TryGetValue currentAssembly.FullName with
                | false, _ -> failwithf "Current assembly %s not loaded" currentAssembly.FullName
                | true, assy -> assy

            match Assembly.resolveTypeRef ctx.LoadedAssemblies currentAssy ImmutableArray.Empty typeRef with
            | TypeResolutionResult.Resolved (targetAssy, identity, typeInfo) -> (targetAssy, identity, typeInfo), ctx
            | TypeResolutionResult.FirstLoadAssy assemblyRef ->
                let handle, referencedIn = assemblyRef.Handle

                let newAssemblies, _ =
                    loadAssembly.LoadAssembly ctx.LoadedAssemblies referencedIn handle

                let newCtx =
                    { ctx with
                        LoadedAssemblies = newAssemblies
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

        let assembly =
            match ctx.LoadedAssemblies.TryGetValue identity.AssemblyFullName with
            | false, _ ->
                failwithf "Cannot concretize type definition - assembly %s not loaded" identity.AssemblyFullName
            | true, assy -> assy

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

        | TypeDefn.Modified (unmodifiedType, _modifierType, _modificationRequired) ->
            // Custom modifiers are metadata annotations on the signature. Runtime type
            // identity and storage shape follow the unmodified type.
            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics unmodifiedType

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
            // Function pointers — both C#'s managed `delegate*<...>` and unmanaged
            // `delegate* unmanaged<...>`, which share the same FNPTR shape in ECMA-335 and
            // differ only in the calling-convention bits of the signature header. They carry
            // their full method signature so that `M(IntPtr)` and `M(delegate*<void>)` remain
            // distinct overloads. Storage shape is still a native-int-sized address, but
            // signature identity has to flow through concretization or member resolution
            // conflates the two arities. PawPrint does not currently invoke these pointers; if
            // it ever does, the signature recorded here is the contract to honour.
            //
            // Custom modifiers (modreq/modopt) on the return type or any parameter position
            // also participate in identity: C# encodes calling conventions such as
            // `delegate* unmanaged[Cdecl]<...>` vs `delegate* unmanaged[Stdcall]<...>` as
            // modopts on the return type, so we must preserve the chain rather than letting
            // the generic `TypeDefn.Modified` arm strip it.
            // Walk outward through `Modified` wrappers, collecting (modifier, isRequired)
            // pairs in outermost-first order; what remains is the underlying type. Also
            // looks through `Pinned`, which is transparent for FP-signature identity and
            // appears only in local-variable signatures in valid metadata anyway.
            let rec strip (acc : (TypeDefn * bool) list) (t : TypeDefn) : (TypeDefn * bool) list * TypeDefn =
                match t with
                | TypeDefn.Modified (inner, modifierType, isRequired) ->
                    // `acc` is built innermost-first as we descend; we reverse below.
                    strip ((modifierType, isRequired) :: acc) inner
                | TypeDefn.Pinned inner -> strip acc inner
                | other -> List.rev acc, other

            // Recursively concretize a parameter/return-type body, preserving custom
            // modifiers at every nesting level. Composite types (Pointer/Byref/Array/...)
            // re-enter this helper for their element so that nested `Modified` wrappers are
            // captured rather than stripped — without that, valid metadata such as
            // `delegate*<int modopt(A)*, void>` and `delegate*<int modopt(B)*, void>`
            // would collapse to the same concrete handle and silently conflate signature
            // identity for overload resolution.
            let rec concretizeTypeWithModifiers
                (ctx : ConcretizationContext<DumpedAssembly>)
                (ty : TypeDefn)
                : ConcretizationContext<DumpedAssembly> * ConcreteTypeWithModifiers
                =
                let modifiersOuterFirst, underlying = strip [] ty

                let ctx, underlyingSigType =
                    match underlying with
                    | TypeDefn.Pointer elem ->
                        let ctx, e = concretizeTypeWithModifiers ctx elem
                        ctx, ConcreteSignatureType.Pointer e
                    | TypeDefn.Byref elem ->
                        let ctx, e = concretizeTypeWithModifiers ctx elem
                        ctx, ConcreteSignatureType.Byref e
                    | TypeDefn.OneDimensionalArrayLowerBoundZero elem ->
                        let ctx, e = concretizeTypeWithModifiers ctx elem
                        ctx, ConcreteSignatureType.OneDimArrayZero e
                    | TypeDefn.Array (elem, rank) ->
                        let ctx, e = concretizeTypeWithModifiers ctx elem
                        ctx, ConcreteSignatureType.Array (e, rank)
                    | TypeDefn.FunctionPointer _ ->
                        // A nested FP signature has already preserved its own nested modifiers
                        // through this same code path; just unwrap the FP handle to get the
                        // already-built `ConcreteFunctionPointerSignature`.
                        let handle, ctx =
                            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics underlying

                        match handle with
                        | ConcreteTypeHandle.FunctionPointer fp -> ctx, ConcreteSignatureType.FunctionPointer fp
                        | _ ->
                            failwith
                                "Logic error: TypeDefn.FunctionPointer did not concretize to FunctionPointer handle"
                    | TypeDefn.GenericInstantiation (_, genericArgs) ->
                        // A `G<int modopt(A)>` and `G<int modopt(B)>` differ only in custom
                        // modifiers on a generic argument; if we let `concretizeType` resolve the
                        // whole instantiation it would strip those per-arg modifiers and the two
                        // signatures would collide. So preserve the modifier-bearing args here,
                        // and also keep the modifier-blind resolved handle for cheap flattening.
                        let ctx, argsReversed =
                            ((ctx, []), genericArgs)
                            ||> Seq.fold (fun (ctx, acc) arg ->
                                let ctx, withMods = concretizeTypeWithModifiers ctx arg
                                ctx, withMods :: acc
                            )

                        let args = List.rev argsReversed

                        let resolvedHandle, ctx =
                            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics underlying

                        ctx, ConcreteSignatureType.GenericInstantiation (resolvedHandle, args)
                    | _ ->
                        // Leaf nominal type, generic parameter, void, etc. The standard
                        // concretizer is correct for these (it strips modifiers, but any
                        // modifiers at this position were already harvested above).
                        let handle, ctx =
                            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics underlying

                        ctx, ConcreteSignatureType.ofHandle handle

                let ctx, modifierHandlesReversed =
                    ((ctx, []), modifiersOuterFirst)
                    ||> List.fold (fun (ctx, acc) (modTy, isRequired) ->
                        let handle, ctx =
                            concretizeType ctx loadAssembly assembly typeGenerics methodGenerics modTy

                        ctx, (handle, isRequired) :: acc
                    )

                let modifierHandles = List.rev modifierHandlesReversed

                let result : ConcreteTypeWithModifiers =
                    {
                        UnderlyingType = underlyingSigType
                        Modifiers = modifierHandles
                    }

                ctx, result

            let ctx, retType =
                match signature.ReturnType with
                | MethodReturnType.Void -> ctx, ConcreteFunctionPointerReturnType.Void
                | MethodReturnType.Returns ty ->
                    let ctx, withMods = concretizeTypeWithModifiers ctx ty
                    ctx, ConcreteFunctionPointerReturnType.Returns withMods

            let ctx, paramHandlesReversed =
                ((ctx, []), signature.ParameterTypes)
                ||> List.fold (fun (ctx, acc) paramTy ->
                    let ctx, withMods = concretizeTypeWithModifiers ctx paramTy
                    ctx, withMods :: acc
                )

            let concreteSignature : ConcreteFunctionPointerSignature =
                {
                    Header = signature.Header
                    ReturnType = retType
                    ParameterTypes = List.rev paramHandlesReversed
                    GenericParameterCount = signature.GenericParameterCount
                    RequiredParameterCount = signature.RequiredParameterCount
                }

            ConcreteTypeHandle.FunctionPointer concreteSignature, ctx

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
                let currentAssy = ctxAfterArgs.LoadedAssemblies.[identity.AssemblyFullName]
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

        // Concretize return type only when the method actually returns a value.
        let ctx, returnType =
            MethodReturnType.map
                ctx
                (fun ctx ty ->
                    let handle, ctx =
                        TypeConcretization.concretizeType ctx loadAssembly assembly typeArgs methodArgs ty

                    ctx, handle
                )
                signature.ReturnType

        // Concretize parameter types
        let paramHandles = ResizeArray<ConcreteTypeHandle> ()
        let mutable ctx = ctx

        for paramType in signature.ParameterTypes do
            let handle, newCtx =
                TypeConcretization.concretizeType ctx loadAssembly assembly typeArgs methodArgs paramType

            paramHandles.Add handle
            ctx <- newCtx

        let newSignature : TypeMethodSignature<ConcreteTypeHandle> =
            {
                Header = signature.Header
                ReturnType = returnType
                ParameterTypes = paramHandles |> Seq.toList
                GenericParameterCount = signature.GenericParameterCount
                RequiredParameterCount = signature.RequiredParameterCount
            }

        newSignature, ctx

    /// Helper to ensure base type assembly is loaded
    let private loadAssemblyReferenceByName
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (referencedInAssembly : DumpedAssembly)
        (targetAssemblyName : AssemblyName)
        : ImmutableDictionary<string, DumpedAssembly>
        =
        match assemblies.TryGetValue targetAssemblyName.FullName with
        | true, _ -> assemblies
        | false, _ ->
            let handle =
                referencedInAssembly.AssemblyReferences
                |> Seq.tryPick (fun (KeyValue (assemblyRefHandle, assemblyRef)) ->
                    if assemblyRef.Name.FullName = targetAssemblyName.FullName then
                        Some assemblyRefHandle
                    else
                        None
                )
                |> Option.defaultWith (fun () ->
                    failwithf
                        "Assembly %s references base assembly %s, but no AssemblyReferenceHandle was found"
                        referencedInAssembly.Name.FullName
                        targetAssemblyName.FullName
                )

            let newAssemblies, _ =
                loadAssembly.LoadAssembly assemblies referencedInAssembly.Name handle

            newAssemblies

    let rec private ensureTypeRefResolved
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (sourceAssembly : DumpedAssembly)
        (typeRef : TypeRef)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * TypeDefinitionHandle
        =
        match Assembly.resolveTypeRef assemblies sourceAssembly ImmutableArray.Empty typeRef with
        | TypeResolutionResult.Resolved (resolvedAssembly, _, resolvedType) ->
            assemblies, resolvedAssembly, resolvedType.TypeDefHandle
        | TypeResolutionResult.FirstLoadAssy assemblyRef ->
            let handle, referencedIn = assemblyRef.Handle
            let newAssemblies, _ = loadAssembly.LoadAssembly assemblies referencedIn handle
            let refreshedSourceAssembly = newAssemblies.[sourceAssembly.Name.FullName]
            ensureTypeRefResolved loadAssembly newAssemblies refreshedSourceAssembly typeRef

    let rec private ensureTypeDefnResolved
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (sourceAssembly : DumpedAssembly)
        (ty : TypeDefn)
        : ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly * TypeDefinitionHandle
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, _) ->
            ensureTypeDefnResolved loadAssembly assemblies sourceAssembly generic
        | TypeDefn.Modified (_, afterMod, _) -> ensureTypeDefnResolved loadAssembly assemblies sourceAssembly afterMod
        | TypeDefn.FromDefinition (identity, _) ->
            let resolvedAssembly = assemblies.[identity.AssemblyFullName]
            assemblies, resolvedAssembly, identity.TypeDefinition.Get
        | TypeDefn.FromReference (typeRef, _) -> ensureTypeRefResolved loadAssembly assemblies sourceAssembly typeRef
        | unexpected ->
            failwithf
                "Unexpected TypeDefn shape while resolving base type from %s: %O"
                sourceAssembly.Name.FullName
                unexpected

    let rec private ensureBaseTypeAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (assyName : AssemblyName)
        (baseTypeInfo : BaseTypeInfo option)
        : ImmutableDictionary<string, DumpedAssembly>
        =
        match baseTypeInfo with
        | None -> assemblies
        | Some (BaseTypeInfo.TypeDef handle) ->
            let assy = assemblies.[assyName.FullName]
            let baseType = assy.TypeDefs.[handle]
            ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy.Name baseType.BaseType
        | Some (BaseTypeInfo.TypeRef handle) ->
            let assy = assemblies.[assyName.FullName]
            let typeRef = assy.TypeRefs.[handle]

            let newAssemblies, resolvedAssembly, resolvedHandle =
                ensureTypeRefResolved loadAssembly assemblies assy typeRef

            let resolvedType = resolvedAssembly.TypeDefs.[resolvedHandle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies resolvedAssembly.Name resolvedType.BaseType
        | Some (BaseTypeInfo.ForeignAssemblyType (assemblyName, handle)) ->
            let assy = assemblies.[assyName.FullName]

            let newAssemblies =
                loadAssemblyReferenceByName loadAssembly assemblies assy assemblyName

            let targetAssembly = newAssemblies.[assemblyName.FullName]
            let targetType = targetAssembly.TypeDefs.[handle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies targetAssembly.Name targetType.BaseType
        | Some (BaseTypeInfo.TypeSpec handle) ->
            let assy = assemblies.[assyName.FullName]
            let typeSpec = assy.TypeSpecs.[handle].Signature

            let newAssemblies, resolvedAssembly, resolvedHandle =
                ensureTypeDefnResolved loadAssembly assemblies assy typeSpec

            let resolvedType = resolvedAssembly.TypeDefs.[resolvedHandle]
            ensureBaseTypeAssembliesLoaded loadAssembly newAssemblies resolvedAssembly.Name resolvedType.BaseType

    let ensureTypeDefinitionBaseAssembliesLoaded
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (assemblyName : AssemblyName)
        (typeDefinitionHandle : TypeDefinitionHandle)
        : ImmutableDictionary<string, DumpedAssembly>
        =
        let assy = assemblies.[assemblyName.FullName]
        let typeDef = assy.TypeDefs.[typeDefinitionHandle]
        ensureBaseTypeAssembliesLoaded loadAssembly assemblies assy.Name typeDef.BaseType

    /// Concretize a method's signature and body
    let concretizeMethod
        (ctx : AllConcreteTypes)
        (loadAssembly : IAssemblyLoad)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (baseTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (typeArgs : ImmutableArray<ConcreteTypeHandle>)
        (methodArgs : ImmutableArray<ConcreteTypeHandle>)
        : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
          AllConcreteTypes *
          ImmutableDictionary<string, DumpedAssembly>
        =

        // Ensure base type assemblies are loaded for the declaring type
        let assemblies =
            ensureTypeDefinitionBaseAssembliesLoaded
                loadAssembly
                assemblies
                method.DeclaringType.Assembly
                method.DeclaringType.Definition.Get

        let concCtx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = ctx
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = baseTypes
            }

        // First, we need to create a TypeDefn for the declaring type with its generics instantiated
        let declaringTypeDefn =
            if method.DeclaringType._Generics.IsEmpty then
                // Non-generic type - determine the SignatureTypeKind
                let assy = concCtx.LoadedAssemblies.[method.DeclaringType.Assembly.FullName]
                let arg = assy.TypeDefs.[method.DeclaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseTypes concCtx.LoadedAssemblies arg

                TypeDefn.FromDefinition (method.DeclaringType.Identity, signatureTypeKind)
            else
                // Generic type - create a GenericInstantiation
                let assy = concCtx.LoadedAssemblies.[method.DeclaringType.Assembly.FullName]
                let arg = assy.TypeDefs.[method.DeclaringType.Definition.Get]

                let signatureTypeKind =
                    DumpedAssembly.signatureTypeKind baseTypes concCtx.LoadedAssemblies arg

                let baseType =
                    TypeDefn.FromDefinition (method.DeclaringType.Identity, signatureTypeKind)

                let genericArgsLength = method.DeclaringType.Generics.Length

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
                method.DeclaringType.Assembly
                typeArgs
                methodArgs
                declaringTypeDefn

        // Look up the concretized declaring type
        let concretizedDeclaringType =
            AllConcreteTypes.lookup declaringHandle concCtx.ConcreteTypes |> Option.get

        // Concretize signature
        let signature, concCtx =
            concretizeMethodSignature
                concCtx
                loadAssembly
                method.DeclaringType.Assembly
                typeArgs
                methodArgs
                method.Signature

        // Concretize local variables
        let instructions, concCtx2 =
            match method.Instructions with
            | None -> None, concCtx
            | Some instr ->
                let locals, updatedCtx =
                    match instr.LocalVars with
                    | None -> None, concCtx
                    | Some vars ->
                        let handles, ctx =
                            concretizeTypeArray
                                concCtx
                                loadAssembly
                                method.DeclaringType.Assembly
                                typeArgs
                                methodArgs
                                vars

                        Some handles, ctx

                Some (MethodInstructions.setLocalVars locals instr), updatedCtx

        // Map generics to handles
        let genericHandles =
            method.Generics
            |> ImmutableArray.map (fun (gp, md) -> methodArgs.[gp.SequenceNumber])

        let concretizedMethod : MethodInfo<_, _, _> =
            {
                DeclaringType = concretizedDeclaringType
                Handle = method.Handle
                Name = method.Name
                Instructions = instructions
                Parameters = method.Parameters
                Generics = genericHandles
                Signature = signature
                RawSignature = method.RawSignature
                CustomAttributes = method.CustomAttributes
                MethodAttributes = method.MethodAttributes
                ImplAttributes = method.ImplAttributes
                NativeImport = method.NativeImport
                IsStatic = method.IsStatic
            }

        concretizedMethod, concCtx2.ConcreteTypes, concCtx2.LoadedAssemblies

    let rec concreteHandleToTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
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
            // Rebuild the `TypeDefn.Modified` wrapper chain from outermost to innermost so
            // that the round-trip (TypeDefn → ConcreteTypeHandle → TypeDefn) preserves any
            // calling-convention modopts and other custom modifiers we captured — at every
            // nesting level, since the underlying type can itself contain composites whose
            // elements carry modifiers.
            let rec sigTypeToTypeDefn (s : ConcreteSignatureType) : TypeDefn =
                match s with
                | ConcreteSignatureType.Concrete h -> concreteHandleToTypeDefn baseClassTypes h concreteTypes assemblies
                | ConcreteSignatureType.Byref e -> TypeDefn.Byref (rebuildModified e)
                | ConcreteSignatureType.Pointer e -> TypeDefn.Pointer (rebuildModified e)
                | ConcreteSignatureType.OneDimArrayZero e ->
                    TypeDefn.OneDimensionalArrayLowerBoundZero (rebuildModified e)
                | ConcreteSignatureType.Array (e, rank) -> TypeDefn.Array (rebuildModified e, rank)
                | ConcreteSignatureType.FunctionPointer fp ->
                    concreteHandleToTypeDefn
                        baseClassTypes
                        (ConcreteTypeHandle.FunctionPointer fp)
                        concreteTypes
                        assemblies
                | ConcreteSignatureType.GenericInstantiation (resolvedHandle, args) ->
                    // The resolved handle gives us the (modifier-blind) base generic definition;
                    // the per-arg modifier chains live in `args` and are rebuilt here so that
                    // round-tripping `delegate*<G<int modopt(A)>, void>` reproduces the same
                    // `Modified` wrappers on each argument rather than collapsing them.
                    let baseDef =
                        match AllConcreteTypes.lookup resolvedHandle concreteTypes with
                        | None -> failwith "Logic error: GenericInstantiation handle not found"
                        | Some concreteType ->
                            let assy = assemblies.[concreteType.Assembly.FullName]
                            let typeDef = assy.TypeDefs.[concreteType.Definition.Get]

                            let signatureTypeKind =
                                DumpedAssembly.signatureTypeKind baseClassTypes assemblies typeDef

                            TypeDefn.FromDefinition (concreteType.Identity, signatureTypeKind)

                    let argDefns = args |> List.map rebuildModified |> ImmutableArray.CreateRange

                    TypeDefn.GenericInstantiation (baseDef, argDefns)

            and rebuildModified (withMods : ConcreteTypeWithModifiers) : TypeDefn =
                let underlying = sigTypeToTypeDefn withMods.UnderlyingType

                // `Modifiers` is outermost-first; we wrap from innermost outward to match
                // the encoding produced by the `TypeProvider.GetModifiedType` callback.
                (underlying, List.rev withMods.Modifiers)
                ||> List.fold (fun acc (modHandle, isRequired) ->
                    let modifierType =
                        concreteHandleToTypeDefn baseClassTypes modHandle concreteTypes assemblies

                    TypeDefn.Modified (acc, modifierType, isRequired)
                )

            let recoveredSignature : TypeMethodSignature<TypeDefn> =
                let returnType =
                    match signature.ReturnType with
                    | ConcreteFunctionPointerReturnType.Void -> MethodReturnType.Void
                    | ConcreteFunctionPointerReturnType.Returns withMods ->
                        rebuildModified withMods |> MethodReturnType.Returns

                let parameterTypes = signature.ParameterTypes |> List.map rebuildModified

                {
                    Header = signature.Header
                    ReturnType = returnType
                    ParameterTypes = parameterTypes
                    GenericParameterCount = signature.GenericParameterCount
                    RequiredParameterCount = signature.RequiredParameterCount
                }

            TypeDefn.FunctionPointer recoveredSignature
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup handle concreteTypes with
            | None -> failwith "Logic error: handle not found"
            | Some concreteType ->

            // Determine SignatureTypeKind
            let assy = assemblies.[concreteType.Assembly.FullName]
            let typeDef = assy.TypeDefs.[concreteType.Definition.Get]

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
