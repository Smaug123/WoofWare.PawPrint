namespace WoofWare.PawPrint

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// <summary>
/// Represents a strongly-typed metadata token which can reference various elements in the assembly metadata.
/// This discriminated union provides type-safe handling of metadata tokens with specific handle types.
/// </summary>
type MetadataToken =
    /// <summary>Method implementation token, specifying how a virtual method is implemented.</summary>
    | MethodImplementation of MethodImplementationHandle
    /// <summary>Method definition token, identifying a method defined in this assembly.</summary>
    | MethodDef of MethodDefinitionHandle
    /// <summary>Method specification token, typically for generic method instantiations.</summary>
    | MethodSpecification of MethodSpecificationHandle
    /// <summary>Member reference token, for references to fields or methods in other modules/assemblies.</summary>
    | MemberReference of MemberReferenceHandle
    /// <summary>Type reference token, for references to types in other modules/assemblies.</summary>
    | TypeReference of TypeReferenceHandle
    /// <summary>Assembly reference token, identifying an external assembly.</summary>
    | AssemblyReference of AssemblyReferenceHandle
    /// <summary>Type specification token, for representing complex types like generic instantiations.</summary>
    | TypeSpecification of TypeSpecificationHandle
    /// <summary>Type definition token, identifying a type defined in this assembly.</summary>
    | TypeDefinition of TypeDefinitionHandle
    /// <summary>Field definition token, identifying a field defined in this assembly.</summary>
    | FieldDefinition of FieldDefinitionHandle
    /// <summary>Parameter token, identifying a parameter of a method.</summary>
    | Parameter of ParameterHandle
    /// <summary>Interface implementation token, mapping an implementation to an interface method.</summary>
    | InterfaceImplementation of InterfaceImplementationHandle
    /// <summary>Exported type token, identifying a type exported from this assembly.</summary>
    | ExportedType of ExportedTypeHandle
    /// <summary>Standalone signature token, for method signatures not attached to any method.</summary>
    | StandaloneSignature of StandaloneSignatureHandle
    /// <summary>Event definition token, identifying an event defined in this assembly.</summary>
    | EventDefinition of EventDefinitionHandle
    /// <summary>Constant token, representing a constant value stored in metadata.</summary>
    | Constant of ConstantHandle
    /// <summary>Custom attribute token, identifying an attribute applied to a metadata element.</summary>
    | CustomAttribute of CustomAttributeHandle
    /// <summary>Security attribute token, for declarative security attributes.</summary>
    | DeclarativeSecurityAttribute of DeclarativeSecurityAttributeHandle
    /// <summary>Property definition token, identifying a property defined in this assembly.</summary>
    | PropertyDefinition of PropertyDefinitionHandle
    /// <summary>Module reference token, for references to other modules in the same assembly.</summary>
    | ModuleReference of ModuleReferenceHandle
    /// <summary>Assembly file token, identifying a file that is part of this assembly.</summary>
    | AssemblyFile of AssemblyFileHandle
    /// <summary>Manifest resource token, identifying a resource embedded in this assembly.</summary>
    | ManifestResource of ManifestResourceHandle
    /// <summary>Generic parameter token, identifying a generic type or method parameter.</summary>
    | GenericParameter of GenericParameterHandle
    /// <summary>Generic parameter constraint token, identifying a constraint on a generic parameter.</summary>
    | GenericParameterConstraint of GenericParameterConstraintHandle
    /// <summary>Document token, used in debugging information.</summary>
    | Document of DocumentHandle
    /// <summary>Method debug information token, for debugging metadata about a method.</summary>
    | MethodDebugInformation of MethodDebugInformationHandle
    /// <summary>Local scope token, identifying a scope within a method body.</summary>
    | LocalScope of LocalScopeHandle
    /// <summary>Local variable token, identifying a local variable in a method.</summary>
    | LocalVariable of LocalVariableHandle
    /// <summary>Local constant token, identifying a local constant in a method.</summary>
    | LocalConstant of LocalConstantHandle
    /// <summary>Import scope token, used in debugging information for namespace imports.</summary>
    | ImportScope of ImportScopeHandle
    /// <summary>Custom debug information token, for user-defined debugging metadata.</summary>
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
        | HandleKind.PropertyDefinition ->
            MetadataToken.PropertyDefinition (MetadataTokens.PropertyDefinitionHandle asRowNum)
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
