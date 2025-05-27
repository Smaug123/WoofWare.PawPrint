namespace WoofWare.PawPrint

open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type BaseTypeInfo =
    | TypeDef of TypeDefinitionHandle
    | TypeRef of TypeReferenceHandle
    | TypeSpec of TypeSpecificationHandle
    | ForeignAssemblyType of assemblyName : AssemblyName * TypeDefinitionHandle

type ResolvedBaseType =
    | Enum
    | ValueType
    | Object
    | Delegate

type MethodImplParsed =
    | MethodImplementation of MethodImplementationHandle
    | MethodDefinition of MethodDefinitionHandle

/// <summary>
/// Represents detailed information about a type definition in a .NET assembly.
/// This is a strongly-typed representation of TypeDefinition from System.Reflection.Metadata.
/// </summary>
type TypeInfo<'generic> =
    {
        /// <summary>The namespace containing the type.</summary>
        Namespace : string

        /// <summary>The name of the type.</summary>
        Name : string

        /// <summary>
        /// All methods defined within this type.
        /// </summary>
        Methods : WoofWare.PawPrint.MethodInfo list

        /// <summary>
        /// Method implementation mappings for this type, often used for interface implementations
        /// or overriding virtual methods from base classes.
        /// </summary>
        MethodImpls : ImmutableDictionary<MethodImplementationHandle, MethodImplParsed>

        /// <summary>
        /// Fields defined in this type.
        /// </summary>
        Fields : WoofWare.PawPrint.FieldInfo list

        /// <summary>
        /// The base type that this type inherits from, or None for types that don't have a base type
        /// (like System.Object).
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

        /// <summary>
        /// The assembly in which this type is defined.
        /// </summary>
        Assembly : AssemblyName

        Generics : 'generic ImmutableArray

        Events : EventDefn ImmutableArray
    }

type TypeInfoEval<'ret> =
    abstract Eval<'a> : TypeInfo<'a> -> 'ret

type TypeInfoCrate =
    abstract Apply<'ret> : TypeInfoEval<'ret> -> 'ret

[<RequireQualifiedAccess>]
module TypeInfoCrate =
    let make<'a> (t : TypeInfo<'a>) =
        { new TypeInfoCrate with
            member _.Apply e = e.Eval t
        }

type BaseClassTypes<'corelib> =
    {
        Corelib : 'corelib
        String : TypeInfo<WoofWare.PawPrint.GenericParameter>
        Array : TypeInfo<WoofWare.PawPrint.GenericParameter>
        Enum : TypeInfo<WoofWare.PawPrint.GenericParameter>
        ValueType : TypeInfo<WoofWare.PawPrint.GenericParameter>
        Object : TypeInfo<WoofWare.PawPrint.GenericParameter>
    }

[<RequireQualifiedAccess>]
module TypeInfo =
    let withGenerics<'a, 'b> (gen : 'b ImmutableArray) (t : TypeInfo<'a>) : TypeInfo<'b> =
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
            Assembly = t.Assembly
            Generics = gen
            Events = t.Events
        }

    let mapGeneric<'a, 'b> (f : 'a -> 'b) (t : TypeInfo<'a>) : TypeInfo<'b> =
        withGenerics (t.Generics |> Seq.map f |> ImmutableArray.CreateRange) t

    let internal read
        (loggerFactory : ILoggerFactory)
        (peReader : PEReader)
        (thisAssembly : AssemblyName)
        (metadataReader : MetadataReader)
        (typeHandle : TypeDefinitionHandle)
        : TypeInfo<WoofWare.PawPrint.GenericParameter>
        =
        let typeDef = metadataReader.GetTypeDefinition typeHandle
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
            |> Seq.map (fun h -> FieldInfo.make metadataReader.GetString h (metadataReader.GetFieldDefinition h))
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
        }

    let rec resolveBaseType<'corelib, 'generic>
        (getName : 'corelib -> AssemblyName)
        (getType : 'corelib -> TypeDefinitionHandle -> TypeInfo<'generic>)
        (baseClassTypes : BaseClassTypes<'corelib>)
        (sourceAssembly : AssemblyName)
        (value : BaseTypeInfo option)
        : ResolvedBaseType
        =
        match value with
        | None -> ResolvedBaseType.Object
        | Some value ->

        match value with
        | BaseTypeInfo.TypeDef typeDefinitionHandle ->
            if sourceAssembly = getName baseClassTypes.Corelib then
                //if typeDefinitionHandle = baseClassTypes.Enum.TypeDefHandle then
                //    ResolvedBaseType.Enum
                //elif typeDefinitionHandle = baseClassTypes.ValueType.TypeDefHandle then
                //    ResolvedBaseType.ValueType
                //else
                let baseType = getType baseClassTypes.Corelib typeDefinitionHandle
                resolveBaseType getName getType baseClassTypes sourceAssembly baseType.BaseType
            else
                failwith "unexpected base type not in corelib"
        | BaseTypeInfo.TypeRef typeReferenceHandle -> failwith "todo"
        | BaseTypeInfo.TypeSpec typeSpecificationHandle -> failwith "todo"
        | BaseTypeInfo.ForeignAssemblyType (assemblyName, typeDefinitionHandle) ->
            resolveBaseType
                getName
                getType
                baseClassTypes
                assemblyName
                (Some (BaseTypeInfo.TypeDef typeDefinitionHandle))
