namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

/// What CoreCLR's type loader records about a value type of sequential or explicit layout: the
/// flags `MethodTableBuilder::PlaceInstanceFields` (methodtablebuilder.cpp) stores in its
/// `EEClassLayoutInfo`, and whether its instances hold GC pointers.
type LaidOutValueTypeFacts =
    {
        /// `EEClassLayoutInfo::IsBlittable`: every instance field passes `IsFieldBlittable`
        /// (fieldmarshaler.cpp), judged by the leading byte of its `[MarshalAs]` and the type's own
        /// charset, an `Auto` charset meaning ANSI as it does off Windows. A field of an enum or of
        /// a CoreLib primitive is judged as the primitive; a `bool` field never passes; a field of
        /// another value type passes if that type is blittable and is not `System.Decimal`.
        IsBlittable : bool
        /// `EEClassLayoutInfo::HasAutoLayoutField`: some instance field is of a value type that has
        /// no layout, or that has such a field itself. An enum field does not count: CoreCLR judges
        /// it as its underlying primitive.
        HasAutoLayoutField : bool
        /// `EEClassLayoutInfo::IsInt128OrHasInt128Fields`: the type is CoreLib's `Int128` or
        /// `UInt128`, or some instance field is of a value type for which this holds.
        IsInt128OrHasInt128Fields : bool
        /// `EEClassLayoutInfo::IsDecimalFloatingPointOrHasDecimalFloatingPointFields`: the type is
        /// one of CoreLib's `System.Numerics.Decimal32`, `Decimal64` or `Decimal128`, or some
        /// instance field is of a value type for which this holds. Only .NET 11's CoreLib declares
        /// those types, and only .NET 11's CoreCLR records the flag, so this is false wherever
        /// .NET 10 would not have it.
        IsDecimalFloatingPointOrHasDecimalFloatingPointFields : bool
        /// `MethodTable::ContainsGCPointers`: see `ValueTypeLayoutFacts.ContainsGcPointers`.
        ContainsGcPointers : bool
    }

/// What CoreCLR's type loader records about one value type that marshalling and the JIT read:
/// whether it has layout, and if so the flags `MethodTableBuilder::PlaceInstanceFields`
/// (methodtablebuilder.cpp) stores. The members answer as the `MethodTable` accessors of the same
/// names do (methodtable.inl, class.h), which gate each flag on `HasLayout`.
///
/// The facts are about one concrete type: an open generic definition has no layout in CoreCLR,
/// and each instantiation's facts are its own (`Wrap<int>` is blittable, `Wrap<bool>` is not).
[<RequireQualifiedAccess>]
type ValueTypeLayoutFacts =
    /// An enum, which `IsEnum` (`MethodTable::IsEnum`) answers. CoreCLR gives an enum no layout: its
    /// parent, `System.Enum`, has none.
    | Enum
    /// A value type of auto layout, which has no layout and no layout flags.
    | WithoutLayout of containsGcPointers : bool
    /// A value type of sequential or explicit layout. A sequential type that CoreCLR places as auto
    /// layout because it holds GC pointers still has layout.
    | WithLayout of LaidOutValueTypeFacts

    /// `MethodTable::HasLayout`.
    member this.HasLayout : bool =
        match this with
        | ValueTypeLayoutFacts.Enum
        | ValueTypeLayoutFacts.WithoutLayout _ -> false
        | ValueTypeLayoutFacts.WithLayout _ -> true

    /// `MethodTable::IsBlittable`: false without layout.
    member this.IsBlittable : bool =
        match this with
        | ValueTypeLayoutFacts.Enum
        | ValueTypeLayoutFacts.WithoutLayout _ -> false
        | ValueTypeLayoutFacts.WithLayout facts -> facts.IsBlittable

    /// `MethodTable::IsAutoLayoutOrHasAutoLayoutField`: true without layout, so true for an enum.
    member this.IsAutoLayoutOrHasAutoLayoutField : bool =
        match this with
        | ValueTypeLayoutFacts.Enum
        | ValueTypeLayoutFacts.WithoutLayout _ -> true
        | ValueTypeLayoutFacts.WithLayout facts -> facts.HasAutoLayoutField

    /// `MethodTable::IsInt128OrHasInt128Fields`: false without layout, even for an auto-layout
    /// type holding an `Int128`.
    member this.IsInt128OrHasInt128Fields : bool =
        match this with
        | ValueTypeLayoutFacts.Enum
        | ValueTypeLayoutFacts.WithoutLayout _ -> false
        | ValueTypeLayoutFacts.WithLayout facts -> facts.IsInt128OrHasInt128Fields

    /// `MethodTable::IsDecimalFloatingPointOrHasDecimalFloatingPointFields` (.NET 11): false
    /// without layout.
    member this.IsDecimalFloatingPointOrHasDecimalFloatingPointFields : bool =
        match this with
        | ValueTypeLayoutFacts.Enum
        | ValueTypeLayoutFacts.WithoutLayout _ -> false
        | ValueTypeLayoutFacts.WithLayout facts -> facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields

    /// `MethodTable::ContainsGCPointers`: whether an instance holds an object reference, directly
    /// or in a field of value type, so that the GC has a series to scan in it.
    ///
    /// This is not `GcReferences.containsReferences`, which answers
    /// `RuntimeHelpers.IsReferenceOrContainsReferences` and so counts a byref field too. A byref
    /// field, which only a byref-like type may have, is in no GC series: such a value is never on
    /// the heap, and the stack walk reports its byrefs by other means. Nor does a `TypedReference`
    /// field count, as its own fields are a byref and a pointer.
    member this.ContainsGcPointers : bool =
        match this with
        | ValueTypeLayoutFacts.Enum -> false
        | ValueTypeLayoutFacts.WithoutLayout containsGcPointers -> containsGcPointers
        | ValueTypeLayoutFacts.WithLayout facts -> facts.ContainsGcPointers

/// The layout facts of a concrete value type, as CoreCLR's type loader computes them when it loads
/// the type.
[<RequireQualifiedAccess>]
module LayoutFacts =

    type private Status =
        | InProgress
        | Completed of StructLayoutFacts

    /// The element type `CheckForSystemTypes` gives a value type of CoreLib's `System` namespace,
    /// by name: those `CorTypeInfo::FindPrimitiveType` knows, and the three handle types it gives
    /// `ELEMENT_TYPE_I`.
    let private systemPrimitive (name : string) : FieldBlittability.Element<StructLayoutFacts> option =
        match name with
        | "Boolean" -> Some FieldBlittability.Element.Boolean
        | "Char" -> Some FieldBlittability.Element.Char
        | "SByte"
        | "Byte" -> Some FieldBlittability.Element.OneByte
        | "Int16"
        | "UInt16" -> Some FieldBlittability.Element.TwoBytes
        | "Int32"
        | "UInt32" -> Some FieldBlittability.Element.FourBytes
        | "Int64"
        | "UInt64" -> Some FieldBlittability.Element.EightBytes
        | "Single" -> Some FieldBlittability.Element.Single
        | "Double" -> Some FieldBlittability.Element.Double
        | "IntPtr"
        | "UIntPtr"
        | "RuntimeArgumentHandle"
        | "RuntimeMethodHandleInternal"
        | "RuntimeFieldHandleInternal" -> Some FieldBlittability.Element.NativeInteger
        | "Void" -> Some FieldBlittability.Element.Void
        | "TypedReference" -> Some FieldBlittability.Element.TypedReference
        | _ -> None

    /// The public form of a struct's facts.
    let private ofStructFacts (facts : StructLayoutFacts) : ValueTypeLayoutFacts =
        match facts with
        | StructLayoutFacts.WithoutLayout containsGcPointers -> ValueTypeLayoutFacts.WithoutLayout containsGcPointers
        | StructLayoutFacts.WithLayout facts ->
            ValueTypeLayoutFacts.WithLayout
                {
                    IsBlittable = facts.IsBlittable
                    HasAutoLayoutField = facts.HasAutoLayoutField
                    IsInt128OrHasInt128Fields = facts.IsInt128OrHasInt128Fields
                    IsDecimalFloatingPointOrHasDecimalFloatingPointFields =
                        facts.IsDecimalFloatingPointOrHasDecimalFloatingPointFields
                    ContainsGcPointers = facts.ContainsGcPointers
                }

    let private unmodelled (diagnostic : string) : TypeBindingFailure =
        TypeBindingFailure.Unmodelled
            {
                Diagnostic = diagnostic
            }

    /// What a nominal type is, for the walk.
    [<RequireQualifiedAccess>]
    type private Category =
        | Reference
        | Enum
        | Struct

    /// What `concrete`, whose row is `typeInfo`, is. Its base chain decides, so the assemblies on
    /// that chain are loaded first; one that does not bind is the failure.
    let private categoryOf
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * Result<Category, TypeBindingFailure>
        =
        let declaring =
            state.LoadedAssembly concrete.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"LayoutFacts: the assembly of the concrete type %O{concrete} is not loaded"
            )

        let assemblies, failure =
            BaseChainLoading.tryEnsureTypeDefinitionBaseAssembliesLoaded
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                state._LoadedAssemblies
                declaring
                concrete.Definition.Get

        let state =
            { state with
                _LoadedAssemblies = assemblies
            }

        match failure with
        | Some failure -> state, Error (TypeBindingFailure.ofBaseChain failure)
        | None ->
            if not (LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies typeInfo) then
                state, Ok Category.Reference
            elif LoadedTypeInfo.isEnum baseClassTypes state._LoadedAssemblies typeInfo then
                state, Ok Category.Enum
            else
                state, Ok Category.Struct

    /// The most value types the walk follows, each held by value in the one before, or the most
    /// enums it strips in a chain. CoreCLR refuses a type that holds itself by value, and one whose
    /// instantiations grow without end along such a chain (`S<T>` holding `S<S<T>>`). The walk
    /// recognises the first exactly; for the second it stops at this depth, which no type CoreCLR
    /// loads has been seen to approach, and says that PawPrint does not model what lies beyond.
    [<Literal>]
    let private MaxNesting = 64

    let private tooDeep (concrete : ConcreteType<ConcreteTypeHandle>) : TypeBindingFailure =
        unmodelled
            $"%O{concrete} is held by value %d{MaxNesting} types deep; PawPrint does not follow a nesting that deep, as an instantiation growing at each level would make it"

    /// The internal element type of `handle`, an enum whose row is `typeInfo`: that of its one
    /// instance field, which must be of a CoreLib primitive once an enum-typed field is stripped
    /// down to that enum's own (`#FieldDescTypeMorph`, methodtablebuilder.cpp). `stripping` holds the
    /// enums already being stripped, whose recurrence is a cycle CoreCLR refuses to load.
    let rec private enumElement
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (stripping : Set<ConcreteTypeHandle>)
        (handle : ConcreteTypeHandle)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * Result<FieldBlittability.Element<StructLayoutFacts>, TypeBindingFailure>
        =
        let refused (state : TypeSystemState) =
            state,
            Error (
                unmodelled
                    $"the enum %O{concrete} has an instance field of no primitive type, which CoreCLR refuses to load"
            )

        if
            (typeInfo.TypeAttributes &&& TypeAttributes.LayoutMask)
            <> TypeAttributes.AutoLayout
        then
            // `HasLayoutMetadata` refuses layout on a type whose parent has none.
            state,
            Error (
                unmodelled
                    $"the enum %O{concrete} declares sequential or explicit layout, which CoreCLR refuses to load"
            )
        elif stripping.Contains handle then
            refused state
        elif stripping.Count >= MaxNesting then
            state, Error (tooDeep concrete)
        else

        match typeInfo.Fields |> List.filter (fun field -> not field.IsStatic) with
        | [ value ] ->
            match
                TypeSystemState.concretizeType
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    concrete.AssemblyFullName
                    concrete.Generics
                    ImmutableArray.Empty
                    value.Signature
            with
            | state, Error failure -> state, Error failure
            | state, Ok underlying ->

            match TypeSystemState.tryGetConcreteTypeInfo state underlying with
            | None -> refused state
            | Some (underlyingType, underlyingInfo) ->

            let primitive =
                if
                    underlyingType.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
                    && underlyingType.Namespace = "System"
                    && not underlyingInfo.IsNested
                then
                    systemPrimitive underlyingType.Name
                else
                    None

            match primitive with
            | Some FieldBlittability.Element.Void
            | Some FieldBlittability.Element.TypedReference -> refused state
            | Some element -> state, Ok element
            | None ->

            match categoryOf loggerFactory dotnetRuntimeDirs baseClassTypes state underlyingType underlyingInfo with
            | state, Error failure -> state, Error failure
            | state, Ok Category.Enum ->
                enumElement
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    state
                    (stripping.Add handle)
                    underlying
                    underlyingType
                    underlyingInfo
            | state, Ok Category.Reference
            | state, Ok Category.Struct -> refused state
        | fields ->
            state,
            Error (
                unmodelled
                    $"the enum %O{concrete} has %d{fields.Length} instance fields, not one, which CoreCLR refuses to load"
            )

    /// The facts of `handle`, a value type other than an enum, whose row is `typeInfo`, reached
    /// `depth` value types deep. The handles `InProgress` in `seen` are those whose fields the walk
    /// is inside.
    let rec private factsIn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (seen : ImmutableDictionary<ConcreteTypeHandle, Status>)
        (depth : int)
        (handle : ConcreteTypeHandle)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState *
          Result<ImmutableDictionary<ConcreteTypeHandle, Status> * StructLayoutFacts, TypeBindingFailure>
        =
        match seen.TryGetValue handle with
        | true, Completed facts -> state, Ok (seen, facts)
        | true, InProgress ->
            state,
            Error (unmodelled $"the value type %O{concrete} holds itself by value, which CoreCLR refuses to load")
        | false, _ ->

        if depth >= MaxNesting then
            state, Error (tooDeep concrete)
        else

        let layoutMask = typeInfo.TypeAttributes &&& TypeAttributes.LayoutMask

        let isCoreLib =
            concrete.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName

        let seen = seen.Add (handle, InProgress)

        // Each instance field's signature is concretised against this instantiation's generic
        // arguments, in the declaring type's assembly; a field signature names no method generics.
        let state, fields =
            (state, (seen, []), typeInfo.Fields)
            |||> StateResult.fold (fun state (seen, acc) field ->
                if field.IsStatic then
                    state, Ok (seen, acc)
                else

                match
                    TypeSystemState.concretizeType
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        concrete.AssemblyFullName
                        concrete.Generics
                        ImmutableArray.Empty
                        field.Signature
                with
                | state, Error failure -> state, Error failure
                | state, Ok fieldHandle ->

                match
                    fieldElementIn loggerFactory dotnetRuntimeDirs baseClassTypes state seen (depth + 1) fieldHandle
                with
                | state, Error failure -> state, Error failure
                | state, Ok (seen, element) -> state, Ok (seen, (field.MarshallingDescriptor, element) :: acc)
            )

        match fields with
        | Error failure -> state, Error failure
        | Ok (seen, fields) ->

        let fields = List.rev fields

        let layoutAndAnsi =
            match layoutMask with
            // `ofStruct` reads no charset for a type without layout.
            | TypeAttributes.AutoLayout -> Ok (TypeLayoutKind.Auto, true)
            | TypeAttributes.SequentialLayout
            | TypeAttributes.ExplicitLayout ->
                let layout =
                    if layoutMask = TypeAttributes.SequentialLayout then
                        TypeLayoutKind.Sequential
                    else
                        TypeLayoutKind.Explicit

                // `HasLayoutMetadata`: the charset off Windows, where `Auto` means ANSI.
                match typeInfo.TypeAttributes &&& TypeAttributes.StringFormatMask with
                | TypeAttributes.AnsiClass
                | TypeAttributes.AutoClass -> Ok (layout, true)
                | TypeAttributes.UnicodeClass -> Ok (layout, false)
                | _ ->
                    Error (
                        unmodelled
                            $"the value type %O{concrete} declares a custom string format, which CoreCLR refuses to load"
                    )
            | _ ->
                // .NET 11 reads this pattern as extended layout (`CStruct`, `CUnion`); .NET 10
                // refuses it.
                Error (
                    unmodelled $"the value type %O{concrete} declares extended layout, which PawPrint does not model"
                )

        // `CheckForSystemTypes`, which runs after the fields are placed, marks these CoreLib types
        // by name, and only a top-level one.
        let named (``namespace`` : string) (names : string list) : bool =
            isCoreLib
            && not typeInfo.IsNested
            && concrete.Namespace = ``namespace``
            && List.contains concrete.Name names

        let selfKind =
            if named "System" [ "Int128" ; "UInt128" ] then
                StructLayoutFacts.SelfKind.Int128
            elif named "System.Numerics" [ "Decimal32" ; "Decimal64" ; "Decimal128" ] then
                StructLayoutFacts.SelfKind.DecimalFloatingPoint
            else
                StructLayoutFacts.SelfKind.Ordinary

        let facts =
            layoutAndAnsi
            |> Result.map (fun (layout, ansi) -> StructLayoutFacts.ofStruct layout ansi selfKind fields)

        match facts with
        | Error failure -> state, Error failure
        | Ok facts -> state, Ok (seen.SetItem (handle, Completed facts), facts)

    and private fieldElementIn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (seen : ImmutableDictionary<ConcreteTypeHandle, Status>)
        (depth : int)
        (handle : ConcreteTypeHandle)
        : TypeSystemState *
          Result<
              ImmutableDictionary<ConcreteTypeHandle, Status> * FieldBlittability.Element<StructLayoutFacts>,
              TypeBindingFailure
           >
        =
        match handle with
        | ConcreteTypeHandle.Pointer _ -> state, Ok (seen, FieldBlittability.Element.Pointer)
        | ConcreteTypeHandle.FunctionPointer _ -> state, Ok (seen, FieldBlittability.Element.FunctionPointer)
        | ConcreteTypeHandle.Byref _ -> state, Ok (seen, FieldBlittability.Element.Byref)
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> state, Ok (seen, FieldBlittability.Element.Reference)
        | ConcreteTypeHandle.Concrete _ ->

        match TypeSystemState.tryGetConcreteTypeInfo state handle with
        | None -> failwith $"LayoutFacts: the concrete handle %O{handle} has no TypeDef row"
        | Some (concrete, typeInfo) ->

        let primitive =
            if
                concrete.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
                && concrete.Namespace = "System"
                && not typeInfo.IsNested
            then
                systemPrimitive concrete.Name
            else
                None

        match primitive with
        | Some element -> state, Ok (seen, element)
        | None ->

        match categoryOf loggerFactory dotnetRuntimeDirs baseClassTypes state concrete typeInfo with
        | state, Error failure -> state, Error failure
        | state, Ok Category.Reference -> state, Ok (seen, FieldBlittability.Element.Reference)
        | state, Ok Category.Enum ->
            match
                enumElement loggerFactory dotnetRuntimeDirs baseClassTypes state Set.empty handle concrete typeInfo
            with
            | state, Error failure -> state, Error failure
            | state, Ok element -> state, Ok (seen, element)
        | state, Ok Category.Struct ->

        match factsIn loggerFactory dotnetRuntimeDirs baseClassTypes state seen depth handle concrete typeInfo with
        | state, Error failure -> state, Error failure
        | state, Ok (seen, facts) ->
            let isDecimal =
                concrete.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
                && concrete.Namespace = "System"
                && concrete.Name = "Decimal"

            state, Ok (seen, FieldBlittability.Element.ValueType (isDecimal, facts))

    /// The layout facts of `handle`, or `None` where it is not a value type: a class, an
    /// interface, an array, a pointer or a byref. A type on the way that does not bind is the
    /// failure, as is a value type CoreCLR would refuse to load for a reason these facts read (such
    /// as one holding itself by value) and a nesting of value types deeper than the walk follows,
    /// returned with what loaded before it.
    let ofValueType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * Result<ValueTypeLayoutFacts option, TypeBindingFailure>
        =
        match TypeSystemState.tryGetConcreteTypeInfo state handle with
        | None -> state, Ok None
        | Some (concrete, typeInfo) ->

        match categoryOf loggerFactory dotnetRuntimeDirs baseClassTypes state concrete typeInfo with
        | state, Error failure -> state, Error failure
        | state, Ok Category.Reference -> state, Ok None
        | state, Ok Category.Enum ->
            match
                enumElement loggerFactory dotnetRuntimeDirs baseClassTypes state Set.empty handle concrete typeInfo
            with
            | state, Error failure -> state, Error failure
            | state, Ok _ -> state, Ok (Some ValueTypeLayoutFacts.Enum)
        | state, Ok Category.Struct ->

        let state, result =
            factsIn
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                state
                ImmutableDictionary.Empty
                0
                handle
                concrete
                typeInfo

        state, result |> Result.map (snd >> ofStructFacts >> Some)
