namespace WoofWare.PawPrint

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

type MetadataToken =
    | MethodDef of MethodDefinitionHandle
    | MethodSpecification of MethodSpecificationHandle
    | MemberReference of MemberReferenceHandle
    | TypeReference of TypeReferenceHandle
    | ModuleDefinition of ModuleDefinitionHandle
    | AssemblyReference of AssemblyReferenceHandle
    | TypeSpecification of TypeSpecificationHandle
    | TypeDefinition of TypeDefinitionHandle
    | FieldDefinition of FieldDefinitionHandle
    | Parameter of ParameterHandle
    | InterfaceImplementation of InterfaceImplementationHandle
    | ExportedType of ExportedTypeHandle

[<RequireQualifiedAccess>]
module MetadataToken =
    let ofInt (value : int32) : MetadataToken =
        let asRowNum = value &&& 0x00FFFFFF

        match LanguagePrimitives.EnumOfValue<byte, HandleKind> (byte (value &&& 0xFF000000 >>> 24)) with
        | HandleKind.ModuleDefinition -> MetadataToken.ModuleDefinition (failwith "TODO")
        | HandleKind.TypeReference -> MetadataToken.TypeReference (MetadataTokens.TypeReferenceHandle asRowNum)
        | HandleKind.TypeDefinition -> MetadataToken.TypeDefinition (MetadataTokens.TypeDefinitionHandle asRowNum)
        | HandleKind.FieldDefinition -> MetadataToken.FieldDefinition (MetadataTokens.FieldDefinitionHandle asRowNum)
        | HandleKind.MethodDefinition -> MetadataToken.MethodDef (MetadataTokens.MethodDefinitionHandle asRowNum)
        | HandleKind.Parameter -> MetadataToken.Parameter (MetadataTokens.ParameterHandle asRowNum)
        | HandleKind.InterfaceImplementation ->
            MetadataToken.InterfaceImplementation (MetadataTokens.InterfaceImplementationHandle asRowNum)
        | HandleKind.MemberReference -> MetadataToken.MemberReference (MetadataTokens.MemberReferenceHandle asRowNum)
        | HandleKind.Constant -> failwith "todo"
        | HandleKind.CustomAttribute -> failwith "todo"
        | HandleKind.DeclarativeSecurityAttribute -> failwith "todo"
        | HandleKind.StandaloneSignature -> failwith "todo"
        | HandleKind.EventDefinition -> failwith "todo"
        | HandleKind.PropertyDefinition -> failwith "todo"
        | HandleKind.MethodImplementation -> failwith "todo"
        | HandleKind.ModuleReference -> failwith "todo"
        | HandleKind.TypeSpecification ->
            MetadataToken.TypeSpecification (MetadataTokens.TypeSpecificationHandle asRowNum)
        | HandleKind.AssemblyDefinition -> failwith "todo"
        | HandleKind.AssemblyReference ->
            MetadataToken.AssemblyReference (MetadataTokens.AssemblyReferenceHandle asRowNum)
        | HandleKind.AssemblyFile -> failwith "todo"
        | HandleKind.ExportedType -> MetadataToken.ExportedType (MetadataTokens.ExportedTypeHandle asRowNum)
        | HandleKind.ManifestResource -> failwith "todo"
        | HandleKind.GenericParameter -> failwith "todo"
        | HandleKind.MethodSpecification ->
            MetadataToken.MethodSpecification (MetadataTokens.MethodSpecificationHandle asRowNum)
        | HandleKind.GenericParameterConstraint -> failwith "todo"
        | HandleKind.Document -> failwith "todo"
        | HandleKind.MethodDebugInformation -> failwith "todo"
        | HandleKind.LocalScope -> failwith "todo"
        | HandleKind.LocalVariable -> failwith "todo"
        | HandleKind.LocalConstant -> failwith "todo"
        | HandleKind.ImportScope -> failwith "todo"
        | HandleKind.CustomDebugInformation -> failwith "todo"
        | HandleKind.UserString -> failwith "todo"
        | HandleKind.Blob -> failwith "todo"
        | HandleKind.Guid -> failwith "todo"
        | HandleKind.String -> failwith "todo"
        | HandleKind.NamespaceDefinition -> failwith "todo"
        | h -> failwith $"Unrecognised kind: {h}"

    let ofEntityHandle (eh : EntityHandle) : MetadataToken = ofInt (eh.GetHashCode ())
