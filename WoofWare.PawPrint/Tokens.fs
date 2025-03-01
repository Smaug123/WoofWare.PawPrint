namespace WoofWare.PawPrint

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

type MetadataToken =
    | MethodImplementation of MethodImplementationHandle
    | MethodDef of MethodDefinitionHandle
    | MethodSpecification of MethodSpecificationHandle
    | MemberReference of MemberReferenceHandle
    | TypeReference of TypeReferenceHandle
    | AssemblyReference of AssemblyReferenceHandle
    | TypeSpecification of TypeSpecificationHandle
    | TypeDefinition of TypeDefinitionHandle
    | FieldDefinition of FieldDefinitionHandle
    | Parameter of ParameterHandle
    | InterfaceImplementation of InterfaceImplementationHandle
    | ExportedType of ExportedTypeHandle
    | StandaloneSignature of StandaloneSignatureHandle
    | EventDefinition of EventDefinitionHandle
    | Constant of ConstantHandle
    | CustomAttribute of CustomAttributeHandle
    | DeclarativeSecurityAttribute of DeclarativeSecurityAttributeHandle
    | PropertyDefinition of PropertyDefinitionHandle
    | ModuleReference of ModuleReferenceHandle
    | AssemblyFile of AssemblyFileHandle
    | ManifestResource of ManifestResourceHandle
    | GenericParameter of GenericParameterHandle
    | GenericParameterConstraint of GenericParameterConstraintHandle
    | Document of DocumentHandle
    | MethodDebugInformation of MethodDebugInformationHandle
    | LocalScope of LocalScopeHandle
    | LocalVariable of LocalVariableHandle
    | LocalConstant of LocalConstantHandle
    | ImportScope of ImportScopeHandle
    | CustomDebugInformation of CustomDebugInformationHandle

[<RequireQualifiedAccess>]
module MetadataToken =
    let ofInt (value : int32) : MetadataToken =
        let asRowNum = value &&& 0x00FFFFFF

        match LanguagePrimitives.EnumOfValue<byte, HandleKind> (byte (value &&& 0xFF000000 >>> 24)) with
        | HandleKind.ModuleDefinition -> failwith "TODO"
        | HandleKind.TypeReference -> MetadataToken.TypeReference (MetadataTokens.TypeReferenceHandle asRowNum)
        | HandleKind.TypeDefinition -> MetadataToken.TypeDefinition (MetadataTokens.TypeDefinitionHandle asRowNum)
        | HandleKind.FieldDefinition -> MetadataToken.FieldDefinition (MetadataTokens.FieldDefinitionHandle asRowNum)
        | HandleKind.MethodDefinition -> MetadataToken.MethodDef (MetadataTokens.MethodDefinitionHandle asRowNum)
        | HandleKind.Parameter -> MetadataToken.Parameter (MetadataTokens.ParameterHandle asRowNum)
        | HandleKind.InterfaceImplementation ->
            MetadataToken.InterfaceImplementation (MetadataTokens.InterfaceImplementationHandle asRowNum)
        | HandleKind.MemberReference -> MetadataToken.MemberReference (MetadataTokens.MemberReferenceHandle asRowNum)
        | HandleKind.Constant -> MetadataToken.Constant (MetadataTokens.ConstantHandle asRowNum)
        | HandleKind.CustomAttribute -> MetadataToken.CustomAttribute (MetadataTokens.CustomAttributeHandle asRowNum)
        | HandleKind.DeclarativeSecurityAttribute ->
            MetadataToken.DeclarativeSecurityAttribute (MetadataTokens.DeclarativeSecurityAttributeHandle asRowNum)
        | HandleKind.StandaloneSignature ->
            MetadataToken.StandaloneSignature (MetadataTokens.StandaloneSignatureHandle asRowNum)
        | HandleKind.EventDefinition -> MetadataToken.EventDefinition (MetadataTokens.EventDefinitionHandle asRowNum)
        | HandleKind.PropertyDefinition -> MetadataToken.PropertyDefinition (MetadataTokens.PropertyDefinitionHandle asRowNum)
        | HandleKind.MethodImplementation ->
            MetadataToken.MethodImplementation (MetadataTokens.MethodImplementationHandle asRowNum)
        | HandleKind.ModuleReference -> MetadataToken.ModuleReference (MetadataTokens.ModuleReferenceHandle asRowNum)
        | HandleKind.TypeSpecification ->
            MetadataToken.TypeSpecification (MetadataTokens.TypeSpecificationHandle asRowNum)
        | HandleKind.AssemblyDefinition -> failwith "TODO"
        | HandleKind.AssemblyReference ->
            MetadataToken.AssemblyReference (MetadataTokens.AssemblyReferenceHandle asRowNum)
        | HandleKind.AssemblyFile -> MetadataToken.AssemblyFile (MetadataTokens.AssemblyFileHandle asRowNum)
        | HandleKind.ExportedType -> MetadataToken.ExportedType (MetadataTokens.ExportedTypeHandle asRowNum)
        | HandleKind.ManifestResource -> MetadataToken.ManifestResource (MetadataTokens.ManifestResourceHandle asRowNum)
        | HandleKind.GenericParameter -> MetadataToken.GenericParameter (MetadataTokens.GenericParameterHandle asRowNum)
        | HandleKind.MethodSpecification ->
            MetadataToken.MethodSpecification (MetadataTokens.MethodSpecificationHandle asRowNum)
        | HandleKind.GenericParameterConstraint ->
            MetadataToken.GenericParameterConstraint (MetadataTokens.GenericParameterConstraintHandle asRowNum)
        | HandleKind.Document -> MetadataToken.Document (MetadataTokens.DocumentHandle asRowNum)
        | HandleKind.MethodDebugInformation ->
            MetadataToken.MethodDebugInformation (MetadataTokens.MethodDebugInformationHandle asRowNum)
        | HandleKind.LocalScope -> MetadataToken.LocalScope (MetadataTokens.LocalScopeHandle asRowNum)
        | HandleKind.LocalVariable -> MetadataToken.LocalVariable (MetadataTokens.LocalVariableHandle asRowNum)
        | HandleKind.LocalConstant -> MetadataToken.LocalConstant (MetadataTokens.LocalConstantHandle asRowNum)
        | HandleKind.ImportScope -> MetadataToken.ImportScope (MetadataTokens.ImportScopeHandle asRowNum)
        | HandleKind.CustomDebugInformation ->
            MetadataToken.CustomDebugInformation (MetadataTokens.CustomDebugInformationHandle asRowNum)
        | HandleKind.UserString -> failwith "TODO"
        | HandleKind.Blob -> failwith "TODO"
        | HandleKind.Guid -> failwith "TODO"
        | HandleKind.String -> failwith "TODO"
        | HandleKind.NamespaceDefinition -> failwith "TODO"
        | h -> failwith $"Unrecognised kind: {h}"

    let ofEntityHandle (eh : EntityHandle) : MetadataToken = ofInt (eh.GetHashCode ())
