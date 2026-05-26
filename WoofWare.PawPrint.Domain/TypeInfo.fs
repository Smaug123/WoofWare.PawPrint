namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
module CharSetMetadata =
    /// Project a `TypeAttributes` value's `StringFormatMask` bits onto the marshalling
    /// `CharSet` enum. Mirrors the CLR's StringFormat → CharSet mapping used when no
    /// explicit `[StructLayout(CharSet=...)]` is provided: `CustomFormatClass` is rare and
    /// has no direct `CharSet` analogue, so we surface it as `CharSet.None` (callers should
    /// treat that as "unspecified" rather than as a real choice).
    let ofTypeAttributes (attrs : TypeAttributes) : CharSet =
        match attrs &&& TypeAttributes.StringFormatMask with
        | TypeAttributes.AnsiClass -> CharSet.Ansi
        | TypeAttributes.UnicodeClass -> CharSet.Unicode
        | TypeAttributes.AutoClass -> CharSet.Auto
        | _ -> CharSet.None

[<RequireQualifiedAccess>]
type BaseTypeInfo =
    | TypeDef of TypeDefinitionHandle
    | TypeRef of TypeReferenceHandle
    | TypeSpec of TypeSpecificationHandle
    | ForeignAssemblyType of assemblyName : AssemblyName * TypeDefinitionHandle

type MethodImplParsed =
    {
        Declaration : MetadataToken
        Body : MetadataToken
    }

type InterfaceImplementation =
    {
        /// TypeDefinition, TypeReference, or TypeSpecification
        InterfaceHandle : MetadataToken

        /// The assembly which InterfaceHandle is relative to
        RelativeToAssembly : AssemblyName
    }

type Layout =
    | Default
    | Custom of size : int * packingSize : int

/// <summary>
/// Represents detailed information about a type definition in a .NET assembly.
/// This is a strongly-typed representation of TypeDefinition from System.Reflection.Metadata.
/// </summary>
type TypeInfo<'generic, 'fieldGeneric> =
    {
        /// <summary>The namespace containing the type.</summary>
        Namespace : string

        /// <summary>The name of the type.</summary>
        Name : string

        /// <summary>
        /// All methods defined within this type.
        /// </summary>
        Methods : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list

        /// <summary>
        /// Method implementation mappings for this type, often used for interface implementations
        /// or overriding virtual methods from base classes.
        /// </summary>
        MethodImpls : ImmutableDictionary<MethodImplementationHandle, MethodImplParsed>

        /// <summary>
        /// Fields defined in this type.
        /// </summary>
        Fields : WoofWare.PawPrint.FieldInfo<GenericParamFromMetadata, 'fieldGeneric> list

        /// <summary>
        /// The base type that this type inherits from, or None for types that don't have a base type
        /// (like System.Object).
        ///
        /// Value types inherit *directly* from System.ValueType; enums directly from System.Enum.
        /// </summary>
        BaseType : BaseTypeInfo option

        /// <summary>
        /// Attributes applied to this type, such as visibility, inheritance characteristics,
        /// special handling, and other flags.
        /// </summary>
        TypeAttributes : TypeAttributes

        /// <summary>
        /// Custom attributes applied to this type.
        /// </summary>
        Attributes : WoofWare.PawPrint.CustomAttribute list

        /// <summary>
        /// The metadata token handle that uniquely identifies this type in the assembly.
        /// </summary>
        TypeDefHandle : TypeDefinitionHandle

        DeclaringType : TypeDefinitionHandle

        /// <summary>
        /// The assembly in which this type is defined.
        /// </summary>
        Assembly : AssemblyName

        Generics : 'generic ImmutableArray

        Events : EventDefn ImmutableArray

        ImplementedInterfaces : InterfaceImplementation ImmutableArray

        Layout : Layout
    }

    member this.IsInterface = this.TypeAttributes.HasFlag TypeAttributes.Interface

    member this.IsNested =
        [
            TypeAttributes.NestedPublic
            TypeAttributes.NestedPrivate
            TypeAttributes.NestedFamily
            TypeAttributes.NestedAssembly
            TypeAttributes.NestedFamANDAssem
            TypeAttributes.NestedFamORAssem
        ]
        |> List.exists this.TypeAttributes.HasFlag

    member this.Identity : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofTypeDefinition this.Assembly this.TypeDefHandle

    override this.ToString () =
        $"%s{this.Assembly.Name}.%s{this.Namespace}.%s{this.Name}"

    static member NominallyEqual
        (a : TypeInfo<'generic, 'fieldGeneric>)
        (b : TypeInfo<'generic, 'fieldGeneric>)
        : bool
        =
        a.Assembly.FullName = b.Assembly.FullName
        && a.TypeDefHandle = b.TypeDefHandle
        && a.Generics = b.Generics

type TypeInfoEval<'ret> =
    abstract Eval<'a, 'field> : TypeInfo<'a, 'field> -> 'ret

type TypeInfoCrate =
    abstract Apply<'ret> : TypeInfoEval<'ret> -> 'ret
    abstract ToString : unit -> string
    abstract BaseType : BaseTypeInfo option
    abstract Assembly : AssemblyName
    abstract Namespace : string
    abstract Name : string

[<RequireQualifiedAccess>]
module TypeInfoCrate =
    let make<'a, 'field> (t : TypeInfo<'a, 'field>) : TypeInfoCrate =
        { new TypeInfoCrate with
            member _.Apply e = e.Eval t

            member this.ToString () =
                { new TypeInfoEval<_> with
                    member _.Eval this = string<TypeInfo<_, _>> this
                }
                |> this.Apply

            member this.BaseType = t.BaseType

            member this.Assembly = t.Assembly

            member this.Namespace = t.Namespace

            member this.Name = t.Name
        }

type BaseClassTypes<'corelib> =
    {
        Corelib : 'corelib
        String : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Boolean : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Char : TypeInfo<GenericParamFromMetadata, TypeDefn>
        SByte : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Byte : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Int16 : TypeInfo<GenericParamFromMetadata, TypeDefn>
        UInt16 : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Int32 : TypeInfo<GenericParamFromMetadata, TypeDefn>
        UInt32 : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Int64 : TypeInfo<GenericParamFromMetadata, TypeDefn>
        UInt64 : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Single : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Double : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Array : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Enum : TypeInfo<GenericParamFromMetadata, TypeDefn>
        ValueType : TypeInfo<GenericParamFromMetadata, TypeDefn>
        DelegateType : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Object : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeMethodHandle : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeMethodInfoStub : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeMethodHandleInternal : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeFieldHandle : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeTypeHandle : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeFieldInfoStub : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeFieldHandleInternal : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeType : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Void : TypeInfo<GenericParamFromMetadata, TypeDefn>
        TypedReference : TypeInfo<GenericParamFromMetadata, TypeDefn>
        IntPtr : TypeInfo<GenericParamFromMetadata, TypeDefn>
        UIntPtr : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.ByReference` (non-generic in modern corelibs) or `System.ByReference<T>` (older).
        /// Optional because not every supported corelib exposes it.
        ByReference : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        /// `System.Nullable\`1`. The open-generic TypeDef row; used to detect any
        /// `Nullable<T>` instantiation by comparing `.Identity` against this field, since
        /// `ConcreteType.Identity` ignores generic arguments. ECMA-335 III.4.16 mandates
        /// special box/unbox semantics for this type.
        Nullable : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Exception : TypeInfo<GenericParamFromMetadata, TypeDefn>
        ArithmeticException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        DivideByZeroException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        OverflowException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        StackOverflowException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        TypeLoadException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        TypeInitializationException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        IndexOutOfRangeException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        InvalidCastException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        ArrayTypeMismatchException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        MissingFieldException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        MissingMethodException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        NotSupportedException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        NullReferenceException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        OutOfMemoryException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        ArgumentException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        ArgumentNullException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Reflection.TargetInvocationException`. Used to wrap the inner exception
        /// when a reflection-style invocation (e.g. `Activator.CreateInstance<T>()`) propagates
        /// an exception thrown by user code through the runtime's invocation seam.
        TargetInvocationException : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.DateTime`. Host-known because CoreCLR's `MarshalInfo` short-circuits a
        /// DateTime field to `MARSHAL_TYPE_DATE` (8 bytes) before the AutoLayout rejection
        /// triggers; reproducing that requires identifying DateTime nominally at marshal time.
        DateTime : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Decimal`. Host-known because CoreCLR's `MarshalInfo` routes Decimal fields
        /// through marshal-stub synthesis (`NFT_DECIMAL` in `fieldmarshaler.cpp`) rather than
        /// the memmove fast path: managed `Decimal` is 16 bytes with 4-byte field alignment,
        /// but native `DECIMAL` is 16 bytes with 8-byte alignment (its `Lo64` union member is
        /// `ULONGLONG`), so a sequential outer struct containing a `Decimal` field has a
        /// different byte image managed vs native. Identifying Decimal nominally at marshal
        /// time lets the classifier reject these fields before quietly emitting wrong bytes.
        Decimal : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Collections.Generic.IList<T>`. The open generic definition. Used by the
        /// SZ-array → implicit-generic-interface carve-out (CoreCLR's
        /// `IsImplicitInterfaceOfSZArray`): a single-dimensional zero-bound array
        /// implements `IList<T>` for any element-compatible `T`, even though the metadata
        /// of `System.Array` does not list it as an implemented interface.
        IListGeneric : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Collections.Generic.IEnumerable<T>`. One of the five interfaces in the
        /// SZ-array implicit-interface set.
        IEnumerableGeneric : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Collections.Generic.ICollection<T>`. One of the five interfaces in the
        /// SZ-array implicit-interface set.
        ICollectionGeneric : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Collections.Generic.IReadOnlyList<T>`. One of the five interfaces in
        /// the SZ-array implicit-interface set.
        IReadOnlyListGeneric : TypeInfo<GenericParamFromMetadata, TypeDefn>
        /// `System.Collections.Generic.IReadOnlyCollection<T>`. One of the five interfaces
        /// in the SZ-array implicit-interface set.
        IReadOnlyCollectionGeneric : TypeInfo<GenericParamFromMetadata, TypeDefn>
    }

[<RequireQualifiedAccess>]
module TypeInfo =
    let rec fullName (get : TypeDefinitionHandle -> TypeInfo<_, _>) (ty : TypeInfo<'a, 'b>) =
        if ty.IsNested then
            let parent = get ty.DeclaringType |> fullName get
            $"%s{parent}.{ty.Name}"
        else if not (String.IsNullOrEmpty ty.Namespace) then
            $"{ty.Namespace}.{ty.Name}"
        else
            ty.Name

    let withGenerics<'a, 'b, 'field> (gen : 'b ImmutableArray) (t : TypeInfo<'a, 'field>) : TypeInfo<'b, 'field> =
        {
            Namespace = t.Namespace
            Name = t.Name
            Methods = t.Methods
            MethodImpls = t.MethodImpls
            Fields = t.Fields
            BaseType = t.BaseType
            TypeAttributes = t.TypeAttributes
            Attributes = t.Attributes
            TypeDefHandle = t.TypeDefHandle
            DeclaringType = t.DeclaringType
            Assembly = t.Assembly
            Generics = gen
            Events = t.Events
            ImplementedInterfaces = t.ImplementedInterfaces
            Layout = t.Layout
        }

    let mapGeneric<'a, 'b, 'field> (f : 'a -> 'b) (t : TypeInfo<'a, 'field>) : TypeInfo<'b, 'field> =
        withGenerics (t.Generics |> ImmutableArray.map f) t

    let internal read
        (peReader : PEReader)
        (thisAssembly : AssemblyName)
        (metadataReader : MetadataReader)
        (typeHandle : TypeDefinitionHandle)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let typeDef = metadataReader.GetTypeDefinition typeHandle
        let declaringType = typeDef.GetDeclaringType ()
        let methods = typeDef.GetMethods ()

        let methodImpls =
            typeDef.GetMethodImplementations ()
            |> Seq.map (fun handle ->
                let m = metadataReader.GetMethodImplementation handle

                let impl : MethodImplParsed =
                    {
                        Declaration = MetadataToken.ofEntityHandle m.MethodDeclaration
                        Body = MetadataToken.ofEntityHandle m.MethodBody
                    }

                KeyValuePair (handle, impl)
            )
            |> ImmutableDictionary.CreateRange

        let fields =
            typeDef.GetFields ()
            |> Seq.map (fun h -> FieldInfo.make metadataReader thisAssembly h (metadataReader.GetFieldDefinition h))
            |> Seq.toList

        let name = metadataReader.GetString typeDef.Name
        let ns = metadataReader.GetString typeDef.Namespace
        let typeAttrs = typeDef.Attributes

        let attrs =
            typeDef.GetCustomAttributes ()
            |> Seq.map (fun h -> CustomAttribute.make metadataReader h (metadataReader.GetCustomAttribute h))
            |> Seq.toList

        let genericParams =
            GenericParameter.readAll thisAssembly metadataReader (typeDef.GetGenericParameters ())

        let methods =
            methods
            |> Seq.map (fun m -> MethodInfo.read peReader metadataReader m)
            |> Seq.toList

        let baseType =
            if typeDef.BaseType.IsNil then
                None
            else
                match MetadataToken.ofEntityHandle typeDef.BaseType with
                | TypeReference typeReferenceHandle -> Some (BaseTypeInfo.TypeRef typeReferenceHandle)
                | TypeDefinition typeDefinitionHandle -> Some (BaseTypeInfo.TypeDef typeDefinitionHandle)
                | TypeSpecification typeSpecHandle -> Some (BaseTypeInfo.TypeSpec typeSpecHandle)
                | t -> failwith $"Unrecognised base-type entity identifier: %O{t}"

        let events =
            let result = ImmutableArray.CreateBuilder ()

            for evt in typeDef.GetEvents () do
                metadataReader.GetEventDefinition evt
                |> EventDefn.make metadataReader evt
                |> result.Add

            result.ToImmutable ()

        let interfaces =
            let result = ImmutableArray.CreateBuilder ()

            for i in typeDef.GetInterfaceImplementations () do
                let impl = metadataReader.GetInterfaceImplementation i

                {
                    InterfaceHandle = MetadataToken.ofEntityHandle impl.Interface
                    RelativeToAssembly = thisAssembly
                }
                |> result.Add

            result.ToImmutable ()

        let layout =
            let l = typeDef.GetLayout ()

            if l.IsDefault then
                Layout.Default
            else
                Layout.Custom (size = l.Size, packingSize = l.PackingSize)

        {
            Namespace = ns
            Name = name
            Methods = methods
            MethodImpls = methodImpls
            Fields = fields
            BaseType = baseType
            TypeAttributes = typeAttrs
            Attributes = attrs
            TypeDefHandle = typeHandle
            Assembly = thisAssembly
            Generics = genericParams
            Events = events
            ImplementedInterfaces = interfaces
            DeclaringType = declaringType
            Layout = layout
        }

    let isBaseType<'corelib>
        (baseClassTypes : BaseClassTypes<'corelib>)
        (getName : 'corelib -> AssemblyName)
        (typeAssy : AssemblyName)
        (typeDefinitionHandle : TypeDefinitionHandle)
        : ResolvedBaseType option
        =
        if typeAssy = getName baseClassTypes.Corelib then
            if typeDefinitionHandle = baseClassTypes.Enum.TypeDefHandle then
                Some ResolvedBaseType.Enum
            elif typeDefinitionHandle = baseClassTypes.ValueType.TypeDefHandle then
                Some ResolvedBaseType.ValueType
            elif typeDefinitionHandle = baseClassTypes.DelegateType.TypeDefHandle then
                Some ResolvedBaseType.Delegate
            elif typeDefinitionHandle = baseClassTypes.Object.TypeDefHandle then
                Some ResolvedBaseType.Object
            else
                None
        else
            None

    let rec private resolveBaseType<'corelib, 'generic, 'field>
        (baseClassTypes : BaseClassTypes<'corelib>)
        (assemblies : AssemblyName -> 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (getTypeSpec : 'corelib -> TypeSpecificationHandle -> 'corelib * TypeDefinitionHandle)
        (sourceAssy : 'corelib)
        (value : BaseTypeInfo option)
        : ResolvedBaseType
        =
        match value with
        | None -> ResolvedBaseType.Object
        | Some value ->

        match value with
        | BaseTypeInfo.TypeDef typeDefinitionHandle ->
            // A TypeDef BaseType lives in the same assembly as the type we're walking from.
            match isBaseType baseClassTypes getName (getName sourceAssy) typeDefinitionHandle with
            | Some x -> x
            | None ->
                let baseType = getTypeDef sourceAssy typeDefinitionHandle

                resolveBaseType
                    baseClassTypes
                    assemblies
                    getName
                    getTypeDef
                    getTypeRef
                    getTypeSpec
                    sourceAssy
                    baseType.BaseType
        | BaseTypeInfo.TypeRef typeReferenceHandle ->
            let targetAssy, typeRef = getTypeRef sourceAssy typeReferenceHandle

            match isBaseType baseClassTypes getName (getName targetAssy) typeRef.TypeDefHandle with
            | Some x -> x
            | None ->
                let baseType = getTypeDef targetAssy typeRef.TypeDefHandle

                resolveBaseType
                    baseClassTypes
                    assemblies
                    getName
                    getTypeDef
                    getTypeRef
                    getTypeSpec
                    targetAssy
                    baseType.BaseType
        | BaseTypeInfo.TypeSpec typeSpecificationHandle ->
            let resolvedAssy, resolvedHandle = getTypeSpec sourceAssy typeSpecificationHandle

            match isBaseType baseClassTypes getName (getName resolvedAssy) resolvedHandle with
            | Some x -> x
            | None ->
                let baseType = getTypeDef resolvedAssy resolvedHandle

                resolveBaseType
                    baseClassTypes
                    assemblies
                    getName
                    getTypeDef
                    getTypeRef
                    getTypeSpec
                    resolvedAssy
                    baseType.BaseType
        | BaseTypeInfo.ForeignAssemblyType (assemblyName, typeDefinitionHandle) ->
            let targetAssy = assemblies assemblyName

            resolveBaseType
                baseClassTypes
                assemblies
                getName
                getTypeDef
                getTypeRef
                getTypeSpec
                targetAssy
                (Some (BaseTypeInfo.TypeDef typeDefinitionHandle))

    /// ECMA "value type": transitively inherits from System.ValueType (possibly via System.Enum),
    /// but is NOT exactly System.ValueType or System.Enum themselves.
    let isValueType
        (baseClassTypes : BaseClassTypes<'corelib>)
        (assemblies : AssemblyName -> 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (getTypeSpec : 'corelib -> TypeSpecificationHandle -> 'corelib * TypeDefinitionHandle)
        (ty : TypeInfo<'g, 'f>)
        : bool
        =
        match isBaseType baseClassTypes getName ty.Assembly ty.TypeDefHandle with
        | Some ResolvedBaseType.Enum
        | Some ResolvedBaseType.ValueType -> false
        | Some ResolvedBaseType.Object
        | Some ResolvedBaseType.Delegate
        | None ->
            match
                resolveBaseType
                    baseClassTypes
                    assemblies
                    getName
                    getTypeDef
                    getTypeRef
                    getTypeSpec
                    (assemblies ty.Assembly)
                    ty.BaseType
            with
            | ResolvedBaseType.Enum
            | ResolvedBaseType.ValueType -> true
            | ResolvedBaseType.Object
            | ResolvedBaseType.Delegate -> false

    /// Convenience: not a value type.
    let isReferenceType
        (baseClassTypes : BaseClassTypes<'corelib>)
        (assemblies : AssemblyName -> 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (getTypeSpec : 'corelib -> TypeSpecificationHandle -> 'corelib * TypeDefinitionHandle)
        (ty : TypeInfo<'g, 'f>)
        : bool
        =
        not (isValueType baseClassTypes assemblies getName getTypeDef getTypeRef getTypeSpec ty)

    /// Metadata layout kind: ValueType for value types, Class otherwise. Note that System.Enum and
    /// System.ValueType themselves encode as Class, matching real CLR signature encoding.
    let signatureTypeKind
        (baseClassTypes : BaseClassTypes<'corelib>)
        (assemblies : AssemblyName -> 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (getTypeSpec : 'corelib -> TypeSpecificationHandle -> 'corelib * TypeDefinitionHandle)
        (ty : TypeInfo<'g, 'f>)
        : SignatureTypeKind
        =
        if isValueType baseClassTypes assemblies getName getTypeDef getTypeRef getTypeSpec ty then
            SignatureTypeKind.ValueType
        else
            SignatureTypeKind.Class

    let toTypeDefn
        (baseClassTypes : BaseClassTypes<'corelib>)
        (assemblies : AssemblyName -> 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (getTypeSpec : 'corelib -> TypeSpecificationHandle -> 'corelib * TypeDefinitionHandle)
        (ty : TypeInfo<TypeDefn, TypeDefn>)
        : TypeDefn
        =
        let stk =
            signatureTypeKind baseClassTypes assemblies getName getTypeDef getTypeRef getTypeSpec ty

        let defn =
            // The only allowed construction of FromDefinition!
            // All other constructions should use DumpedAssembly.typeInfoToTypeDefn.
            TypeDefn.FromDefinition (ty.Identity, stk)

        if ty.Generics.IsEmpty then
            defn
        else
            let generics = ty.Generics
            TypeDefn.GenericInstantiation (defn, generics)
