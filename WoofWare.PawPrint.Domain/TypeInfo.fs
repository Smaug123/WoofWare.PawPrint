namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
type BaseTypeInfo =
    | TypeDef of TypeDefinitionHandle
    | TypeRef of TypeReferenceHandle
    | TypeSpec of TypeSpecificationHandle
    | ForeignAssemblyType of assemblyName : AssemblyName * TypeDefinitionHandle

type MethodImplParsed =
    | MethodImplementation of MethodImplementationHandle
    | MethodDefinition of MethodDefinitionHandle

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

    override this.ToString () =
        $"%s{this.Assembly.Name}.%s{this.Namespace}.%s{this.Name}"

    static member NominallyEqual
        (a : TypeInfo<'generic, 'fieldGeneric>)
        (b : TypeInfo<'generic, 'fieldGeneric>)
        : bool
        =
        a.Assembly.FullName = b.Assembly.FullName
        && a.Namespace = b.Namespace
        && a.Name = b.Name
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
        RuntimeFieldHandle : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeTypeHandle : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeFieldInfoStub : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeFieldHandleInternal : TypeInfo<GenericParamFromMetadata, TypeDefn>
        RuntimeType : TypeInfo<GenericParamFromMetadata, TypeDefn>
        Void : TypeInfo<GenericParamFromMetadata, TypeDefn>
        TypedReference : TypeInfo<GenericParamFromMetadata, TypeDefn>
        IntPtr : TypeInfo<GenericParamFromMetadata, TypeDefn>
        UIntPtr : TypeInfo<GenericParamFromMetadata, TypeDefn>
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
        (loggerFactory : ILoggerFactory)
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
                let methodBody = MetadataToken.ofEntityHandle m.MethodBody

                match methodBody with
                | MetadataToken.MethodImplementation t ->
                    KeyValuePair (handle, MethodImplParsed.MethodImplementation t)
                | MetadataToken.MethodDef t -> KeyValuePair (handle, MethodImplParsed.MethodDefinition t)
                | k -> failwith $"unexpected kind: {k}"

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
            |> Seq.map (fun h -> CustomAttribute.make h (metadataReader.GetCustomAttribute h))
            |> Seq.toList

        let genericParams =
            GenericParameter.readAll metadataReader (typeDef.GetGenericParameters ())

        let methods =
            methods
            |> Seq.choose (fun m ->
                let result = MethodInfo.read loggerFactory peReader metadataReader m

                match result with
                | None -> None
                | Some x -> Some x
            )
            |> Seq.toList

        let baseType =
            match MetadataToken.ofEntityHandle typeDef.BaseType with
            | TypeReference typeReferenceHandle -> Some (BaseTypeInfo.TypeRef typeReferenceHandle)
            | TypeDefinition typeDefinitionHandle ->
                if typeDefinitionHandle.IsNil then
                    None
                else
                    Some (BaseTypeInfo.TypeDef typeDefinitionHandle)
            | TypeSpecification typeSpecHandle -> Some (BaseTypeInfo.TypeSpec typeSpecHandle)
            | t -> failwith $"Unrecognised base-type entity identifier: %O{t}"

        let events =
            let result = ImmutableArray.CreateBuilder ()

            for evt in typeDef.GetEvents () do
                metadataReader.GetEventDefinition evt
                |> EventDefn.make metadataReader
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

    let rec resolveBaseType<'corelib, 'generic, 'field>
        (baseClassTypes : BaseClassTypes<'corelib>)
        (sourceAssy : 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (value : BaseTypeInfo option)
        : ResolvedBaseType
        =
        match value with
        | None -> ResolvedBaseType.Object
        | Some value ->

        match value with
        | BaseTypeInfo.TypeDef typeDefinitionHandle ->
            match isBaseType baseClassTypes getName (getName sourceAssy) typeDefinitionHandle with
            | Some x -> x
            | None ->
                let baseType = getTypeDef baseClassTypes.Corelib typeDefinitionHandle
                resolveBaseType baseClassTypes sourceAssy getName getTypeDef getTypeRef baseType.BaseType
        | BaseTypeInfo.TypeRef typeReferenceHandle ->
            let targetAssy, typeRef = getTypeRef sourceAssy typeReferenceHandle

            match isBaseType baseClassTypes getName (getName targetAssy) typeRef.TypeDefHandle with
            | Some x -> x
            | None ->
                let baseType = getTypeDef baseClassTypes.Corelib typeRef.TypeDefHandle
                resolveBaseType baseClassTypes sourceAssy getName getTypeDef getTypeRef baseType.BaseType
        | BaseTypeInfo.TypeSpec typeSpecificationHandle -> failwith "todo"
        | BaseTypeInfo.ForeignAssemblyType (assemblyName, typeDefinitionHandle) ->
            resolveBaseType
                baseClassTypes
                sourceAssy
                getName
                getTypeDef
                getTypeRef
                (Some (BaseTypeInfo.TypeDef typeDefinitionHandle))

    let toTypeDefn
        (baseClassTypes : BaseClassTypes<'corelib>)
        (assemblies : AssemblyName -> 'corelib)
        (getName : 'corelib -> AssemblyName)
        (getTypeDef : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic, 'field>)
        (getTypeRef : 'corelib -> TypeReferenceHandle -> 'corelib * TypeInfo<'generic, 'field>)
        (ty : TypeInfo<TypeDefn, TypeDefn>)
        : TypeDefn
        =
        let stk =
            match resolveBaseType baseClassTypes (assemblies ty.Assembly) getName getTypeDef getTypeRef ty.BaseType with
            | ResolvedBaseType.Enum
            | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
            | ResolvedBaseType.Object
            | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

        let defn =
            // The only allowed construction of FromDefinition!
            // All other constructions should use DumpedAssembly.typeInfoToTypeDefn.
            TypeDefn.FromDefinition (ComparableTypeDefinitionHandle.Make ty.TypeDefHandle, ty.Assembly.FullName, stk)

        if ty.Generics.IsEmpty then
            defn
        else
            let generics = ty.Generics
            TypeDefn.GenericInstantiation (defn, generics)
