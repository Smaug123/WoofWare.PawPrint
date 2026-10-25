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
        | Completed of ValueTypeLayoutFacts

    /// The element type CoreCLR judges a field by. A field of value type takes the value type's
    /// `GetInternalCorElementType`: the primitive for an enum, or for one of the CoreLib value
    /// types `CheckForSystemTypes` (methodtablebuilder.cpp) marks as one.
    [<RequireQualifiedAccess>]
    type private FieldElement =
        | Boolean
        | Char
        | OneByte
        | TwoBytes
        | FourBytes
        | EightBytes
        | Single
        | Double
        | NativeInteger
        | Void
        | TypedReference
        | Pointer
        | FunctionPointer
        | Byref
        /// An object reference: a class, an interface, an array, `string` or `object`.
        | Reference
        | ValueType of isDecimal : bool * ValueTypeLayoutFacts

    /// The element type `CheckForSystemTypes` gives a value type of CoreLib's `System` namespace,
    /// by name: those `CorTypeInfo::FindPrimitiveType` knows, and the three handle types it gives
    /// `ELEMENT_TYPE_I`.
    let private systemPrimitive (name : string) : FieldElement option =
        match name with
        | "Boolean" -> Some FieldElement.Boolean
        | "Char" -> Some FieldElement.Char
        | "SByte"
        | "Byte" -> Some FieldElement.OneByte
        | "Int16"
        | "UInt16" -> Some FieldElement.TwoBytes
        | "Int32"
        | "UInt32" -> Some FieldElement.FourBytes
        | "Int64"
        | "UInt64" -> Some FieldElement.EightBytes
        | "Single" -> Some FieldElement.Single
        | "Double" -> Some FieldElement.Double
        | "IntPtr"
        | "UIntPtr"
        | "RuntimeArgumentHandle"
        | "RuntimeMethodHandleInternal"
        | "RuntimeFieldHandleInternal" -> Some FieldElement.NativeInteger
        | "Void" -> Some FieldElement.Void
        | "TypedReference" -> Some FieldElement.TypedReference
        | _ -> None

    // `NATIVE_TYPE_*` (corhdr.h): the leading byte of a field's marshalling blob.
    [<Literal>]
    let private NativeTypeI1 = 0x03uy

    [<Literal>]
    let private NativeTypeU1 = 0x04uy

    [<Literal>]
    let private NativeTypeI2 = 0x05uy

    [<Literal>]
    let private NativeTypeU2 = 0x06uy

    [<Literal>]
    let private NativeTypeI4 = 0x07uy

    [<Literal>]
    let private NativeTypeU4 = 0x08uy

    [<Literal>]
    let private NativeTypeI8 = 0x09uy

    [<Literal>]
    let private NativeTypeU8 = 0x0Auy

    [<Literal>]
    let private NativeTypeR4 = 0x0Buy

    [<Literal>]
    let private NativeTypeR8 = 0x0Cuy

    [<Literal>]
    let private NativeTypeStruct = 0x1Buy

    [<Literal>]
    let private NativeTypeInt = 0x1Fuy

    [<Literal>]
    let private NativeTypeUInt = 0x20uy

    [<Literal>]
    let private NativeTypeFunc = 0x26uy

    [<Literal>]
    let private NativeTypeError = 0x2Duy

    /// The leading byte of a field's marshalling blob, which is all `IsFieldBlittable` reads:
    /// `NATIVE_TYPE_DEFAULT` for no blob, and for an empty one.
    let private leadingNativeType (descriptor : FieldMarshalDescriptor option) : byte =
        match descriptor with
        | None
        | Some FieldMarshalDescriptor.Empty -> NativeTypeParamInfo.NativeTypeDefault
        | Some (FieldMarshalDescriptor.ByValTStr _) -> NativeTypeParamInfo.NativeTypeFixedSysString
        | Some (FieldMarshalDescriptor.ByValArray _) -> NativeTypeParamInfo.NativeTypeFixedArray
        | Some (FieldMarshalDescriptor.Other unmanagedType) -> byte unmanagedType
        | Some (FieldMarshalDescriptor.Malformed nativeType) -> nativeType

    /// `IsFieldBlittable` (fieldmarshaler.cpp), for a field of `element` whose marshalling blob
    /// leads with `nativeType`, declared in a type of ANSI charset or not.
    let private isFieldBlittable (ansi : bool) (nativeType : byte) (element : FieldElement) : bool =
        let isDefault = nativeType = NativeTypeParamInfo.NativeTypeDefault

        match element with
        | FieldElement.Char ->
            (isDefault && not ansi)
            || nativeType = NativeTypeI2
            || nativeType = NativeTypeU2
        | FieldElement.OneByte -> isDefault || nativeType = NativeTypeI1 || nativeType = NativeTypeU1
        | FieldElement.TwoBytes -> isDefault || nativeType = NativeTypeI2 || nativeType = NativeTypeU2
        | FieldElement.FourBytes ->
            isDefault
            || nativeType = NativeTypeI4
            || nativeType = NativeTypeU4
            || nativeType = NativeTypeError
        | FieldElement.EightBytes -> isDefault || nativeType = NativeTypeI8 || nativeType = NativeTypeU8
        | FieldElement.Single -> isDefault || nativeType = NativeTypeR4
        | FieldElement.Double -> isDefault || nativeType = NativeTypeR8
        | FieldElement.NativeInteger -> isDefault || nativeType = NativeTypeInt || nativeType = NativeTypeUInt
        | FieldElement.Pointer -> isDefault
        | FieldElement.FunctionPointer -> isDefault || nativeType = NativeTypeFunc
        | FieldElement.ValueType (isDecimal, facts) ->
            (isDefault || nativeType = NativeTypeStruct)
            && not isDecimal
            && facts.IsBlittable
        | FieldElement.Boolean
        | FieldElement.Void
        | FieldElement.TypedReference
        | FieldElement.Byref
        | FieldElement.Reference -> false

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

    /// Whether `needle` is `haystack`, or occurs in it: as a generic argument, an element type, or a
    /// function pointer's parameter or return type, at any depth.
    let rec private occursIn
        (state : TypeSystemState)
        (needle : ConcreteTypeHandle)
        (haystack : ConcreteTypeHandle)
        : bool
        =
        needle = haystack
        || match haystack with
           | ConcreteTypeHandle.Concrete _ ->
               match AllConcreteTypes.lookup haystack state.ConcreteTypes with
               | Some concrete -> concrete.Generics |> Seq.exists (occursIn state needle)
               | None -> failwith $"LayoutFacts: the concrete handle %O{haystack} is not registered"
           | ConcreteTypeHandle.Byref element
           | ConcreteTypeHandle.Pointer element
           | ConcreteTypeHandle.OneDimArrayZero element
           | ConcreteTypeHandle.Array (element, _) -> occursIn state needle element
           | ConcreteTypeHandle.FunctionPointer signature ->
               signature.ParameterTypes |> List.exists (occursIn state needle)
               || match signature.ReturnType with
                  | MethodReturnType.Void -> false
                  | MethodReturnType.Returns returned -> occursIn state needle returned

    /// Whether `concrete` is an instantiation of the same definition as one of `outers`, the types
    /// the walk is already inside, over arguments that contain all of that one's. Then `D<A>`
    /// reaches `D<B>`, with `B` built from `A`, and reaches by the same route a `D` built from `B`
    /// and so on without end: CoreCLR refuses such a type. A nesting such as `Wrap<Wrap<int>>`
    /// reaches a `D` over less, and is not caught.
    let private expands
        (state : TypeSystemState)
        (outers : ConcreteTypeHandle seq)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        : bool
        =
        outers
        |> Seq.exists (fun outer ->
            match AllConcreteTypes.lookup outer state.ConcreteTypes with
            | Some outerType ->
                outerType.Identity = concrete.Identity
                && outerType.Generics
                   |> Seq.forall (fun argument -> concrete.Generics |> Seq.exists (occursIn state argument))
            | None -> failwith $"LayoutFacts: the concrete handle %O{outer} is not registered"
        )

    /// The internal element type of `handle`, an enum whose row is `typeInfo`: that of its one
    /// instance field, which must be of a CoreLib primitive once an enum-typed field is stripped
    /// down to that enum's own (`#FieldDescTypeMorph`, methodtablebuilder.cpp). `stripping` holds the
    /// enums already being stripped, whose recurrence, or expansion (see `expands`), CoreCLR refuses
    /// to load.
    let rec private enumElement
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (stripping : Set<ConcreteTypeHandle>)
        (handle : ConcreteTypeHandle)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * Result<FieldElement, TypeBindingFailure>
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
        elif stripping.Contains handle || expands state stripping concrete then
            refused state
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
            | Some FieldElement.Void
            | Some FieldElement.TypedReference -> refused state
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

    /// The facts of `handle`, a value type other than an enum, whose row is `typeInfo`. The
    /// handles `InProgress` in `seen` are those whose fields the walk is inside.
    let rec private factsIn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (seen : ImmutableDictionary<ConcreteTypeHandle, Status>)
        (handle : ConcreteTypeHandle)
        (concrete : ConcreteType<ConcreteTypeHandle>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState *
          Result<ImmutableDictionary<ConcreteTypeHandle, Status> * ValueTypeLayoutFacts, TypeBindingFailure>
        =
        match seen.TryGetValue handle with
        | true, Completed facts -> state, Ok (seen, facts)
        | true, InProgress ->
            state,
            Error (unmodelled $"the value type %O{concrete} holds itself by value, which CoreCLR refuses to load")
        | false, _ ->

        let inside =
            seen
            |> Seq.choose (fun (KeyValue (outer, status)) ->
                match status with
                | Completed _ -> None
                | InProgress -> Some outer
            )

        if expands state inside concrete then
            state,
            Error (
                unmodelled
                    $"the value type %O{concrete} is held by value inside an instantiation of its own definition over less, which CoreCLR refuses to load"
            )
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

                match fieldElementIn loggerFactory dotnetRuntimeDirs baseClassTypes state seen fieldHandle with
                | state, Error failure -> state, Error failure
                | state, Ok (seen, element) -> state, Ok (seen, (field.MarshallingDescriptor, element) :: acc)
            )

        match fields with
        | Error failure -> state, Error failure
        | Ok (seen, fields) ->

        let fields = List.rev fields

        let containsGcPointers =
            fields
            |> List.exists (fun (_, element) ->
                match element with
                | FieldElement.Reference -> true
                | FieldElement.ValueType (_, facts) -> facts.ContainsGcPointers
                | FieldElement.Boolean
                | FieldElement.Char
                | FieldElement.OneByte
                | FieldElement.TwoBytes
                | FieldElement.FourBytes
                | FieldElement.EightBytes
                | FieldElement.Single
                | FieldElement.Double
                | FieldElement.NativeInteger
                | FieldElement.Void
                | FieldElement.TypedReference
                | FieldElement.Pointer
                | FieldElement.FunctionPointer
                | FieldElement.Byref -> false
            )

        let facts =
            match layoutMask with
            | TypeAttributes.AutoLayout -> Ok (ValueTypeLayoutFacts.WithoutLayout containsGcPointers)
            | TypeAttributes.SequentialLayout
            | TypeAttributes.ExplicitLayout ->
                // `HasLayoutMetadata`: the charset off Windows, where `Auto` means ANSI.
                let ansi =
                    match typeInfo.TypeAttributes &&& TypeAttributes.StringFormatMask with
                    | TypeAttributes.AnsiClass
                    | TypeAttributes.AutoClass -> Ok true
                    | TypeAttributes.UnicodeClass -> Ok false
                    | _ ->
                        Error (
                            unmodelled
                                $"the value type %O{concrete} declares a custom string format, which CoreCLR refuses to load"
                        )

                ansi
                |> Result.map (fun ansi ->
                    // `TypeHasAutoLayoutField`, `TypeHasInt128Field` and their .NET 11 sibling pass
                    // on the flags of a field's type only for `ELEMENT_TYPE_VALUETYPE`, after the
                    // normalisation to primitives (classlayoutinfo.cpp).
                    let fieldFacts =
                        fields
                        |> List.choose (fun (_, element) ->
                            match element with
                            | FieldElement.ValueType (_, facts) -> Some facts
                            | FieldElement.Boolean
                            | FieldElement.Char
                            | FieldElement.OneByte
                            | FieldElement.TwoBytes
                            | FieldElement.FourBytes
                            | FieldElement.EightBytes
                            | FieldElement.Single
                            | FieldElement.Double
                            | FieldElement.NativeInteger
                            | FieldElement.Void
                            | FieldElement.TypedReference
                            | FieldElement.Pointer
                            | FieldElement.FunctionPointer
                            | FieldElement.Byref
                            | FieldElement.Reference -> None
                        )

                    // `CheckForSystemTypes`, which runs after the fields are placed, marks these
                    // CoreLib types by name, and only a top-level one.
                    let named (``namespace`` : string) (names : string list) : bool =
                        isCoreLib
                        && not typeInfo.IsNested
                        && concrete.Namespace = ``namespace``
                        && List.contains concrete.Name names

                    ValueTypeLayoutFacts.WithLayout
                        {
                            IsBlittable =
                                fields
                                |> List.forall (fun (descriptor, element) ->
                                    isFieldBlittable ansi (leadingNativeType descriptor) element
                                )
                            HasAutoLayoutField = fieldFacts |> List.exists _.IsAutoLayoutOrHasAutoLayoutField
                            IsInt128OrHasInt128Fields =
                                named "System" [ "Int128" ; "UInt128" ]
                                || fieldFacts |> List.exists _.IsInt128OrHasInt128Fields
                            IsDecimalFloatingPointOrHasDecimalFloatingPointFields =
                                named "System.Numerics" [ "Decimal32" ; "Decimal64" ; "Decimal128" ]
                                || fieldFacts
                                   |> List.exists _.IsDecimalFloatingPointOrHasDecimalFloatingPointFields
                            ContainsGcPointers = containsGcPointers
                        }
                )
            | _ ->
                // .NET 11 reads this pattern as extended layout (`CStruct`, `CUnion`); .NET 10
                // refuses it.
                Error (
                    unmodelled $"the value type %O{concrete} declares extended layout, which PawPrint does not model"
                )

        match facts with
        | Error failure -> state, Error failure
        | Ok facts -> state, Ok (seen.SetItem (handle, Completed facts), facts)

    and private fieldElementIn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (seen : ImmutableDictionary<ConcreteTypeHandle, Status>)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * Result<ImmutableDictionary<ConcreteTypeHandle, Status> * FieldElement, TypeBindingFailure>
        =
        match handle with
        | ConcreteTypeHandle.Pointer _ -> state, Ok (seen, FieldElement.Pointer)
        | ConcreteTypeHandle.FunctionPointer _ -> state, Ok (seen, FieldElement.FunctionPointer)
        | ConcreteTypeHandle.Byref _ -> state, Ok (seen, FieldElement.Byref)
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> state, Ok (seen, FieldElement.Reference)
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
        | state, Ok Category.Reference -> state, Ok (seen, FieldElement.Reference)
        | state, Ok Category.Enum ->
            match
                enumElement loggerFactory dotnetRuntimeDirs baseClassTypes state Set.empty handle concrete typeInfo
            with
            | state, Error failure -> state, Error failure
            | state, Ok element -> state, Ok (seen, element)
        | state, Ok Category.Struct ->

        match factsIn loggerFactory dotnetRuntimeDirs baseClassTypes state seen handle concrete typeInfo with
        | state, Error failure -> state, Error failure
        | state, Ok (seen, facts) ->
            let isDecimal =
                concrete.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName
                && concrete.Namespace = "System"
                && concrete.Name = "Decimal"

            state, Ok (seen, FieldElement.ValueType (isDecimal, facts))

    /// The layout facts of `handle`, or `None` where it is not a value type: a class, an
    /// interface, an array, a pointer or a byref. A type on the way that does not bind is the
    /// failure, as is a value type CoreCLR would refuse to load for a reason these facts read (such
    /// as one holding itself by value), returned with what loaded before it.
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
                handle
                concrete
                typeInfo

        state, result |> Result.map (snd >> Some)
