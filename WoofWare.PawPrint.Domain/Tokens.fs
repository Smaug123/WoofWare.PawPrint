namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// <summary>
/// Represents a strongly-typed metadata token which can reference various elements in the assembly metadata.
/// This discriminated union provides type-safe handling of metadata tokens with specific handle types.
/// </summary>
type MetadataToken =
    /// <summary>Module definition token, identifying the current module.</summary>
    | ModuleDefinition of ModuleDefinitionHandle
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
    /// <summary>Assembly definition token, identifying the current assembly's manifest metadata row.</summary>
    | AssemblyDefinition of AssemblyDefinitionHandle
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
        | HandleKind.ModuleDefinition ->
            if asRowNum = 0 then
                failwith "Nil ModuleDefinition token (row 0)"
            elif asRowNum <> 1 then
                failwith $"Invalid ModuleDefinition row number: {asRowNum} (only row 1 is valid)"
            else
                MetadataToken.ModuleDefinition EntityHandle.ModuleDefinition
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
        | HandleKind.AssemblyDefinition ->
            if asRowNum = 0 then
                failwith "Nil AssemblyDefinition token (row 0)"
            elif asRowNum <> 1 then
                failwith $"Invalid AssemblyDefinition row number: {asRowNum} (only row 1 is valid)"
            else
                MetadataToken.AssemblyDefinition EntityHandle.AssemblyDefinition
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

    let ofEntityHandle (eh : EntityHandle) : MetadataToken =
        if eh.IsNil then
            failwith $"Nil EntityHandle (kind {eh.Kind})"
        else
            ofInt (MetadataTokens.GetToken eh)

    /// <summary>
    /// Converts a MetadataToken back to its raw int32 metadata token representation.
    /// </summary>
    let toInt (token : MetadataToken) : int32 =
        let handle : Handle =
            match token with
            | MetadataToken.ModuleDefinition h -> ModuleDefinitionHandle.op_Implicit h
            | MetadataToken.MethodImplementation h -> MethodImplementationHandle.op_Implicit h
            | MetadataToken.MethodDef h -> MethodDefinitionHandle.op_Implicit h
            | MetadataToken.MethodSpecification h -> MethodSpecificationHandle.op_Implicit h
            | MetadataToken.MemberReference h -> MemberReferenceHandle.op_Implicit h
            | MetadataToken.TypeReference h -> TypeReferenceHandle.op_Implicit h
            | MetadataToken.AssemblyReference h -> AssemblyReferenceHandle.op_Implicit h
            | MetadataToken.TypeSpecification h -> TypeSpecificationHandle.op_Implicit h
            | MetadataToken.TypeDefinition h -> TypeDefinitionHandle.op_Implicit h
            | MetadataToken.FieldDefinition h -> FieldDefinitionHandle.op_Implicit h
            | MetadataToken.Parameter h -> ParameterHandle.op_Implicit h
            | MetadataToken.InterfaceImplementation h -> InterfaceImplementationHandle.op_Implicit h
            | MetadataToken.ExportedType h -> ExportedTypeHandle.op_Implicit h
            | MetadataToken.StandaloneSignature h -> StandaloneSignatureHandle.op_Implicit h
            | MetadataToken.EventDefinition h -> EventDefinitionHandle.op_Implicit h
            | MetadataToken.Constant h -> ConstantHandle.op_Implicit h
            | MetadataToken.CustomAttribute h -> CustomAttributeHandle.op_Implicit h
            | MetadataToken.DeclarativeSecurityAttribute h -> DeclarativeSecurityAttributeHandle.op_Implicit h
            | MetadataToken.PropertyDefinition h -> PropertyDefinitionHandle.op_Implicit h
            | MetadataToken.ModuleReference h -> ModuleReferenceHandle.op_Implicit h
            | MetadataToken.AssemblyFile h -> AssemblyFileHandle.op_Implicit h
            | MetadataToken.ManifestResource h -> ManifestResourceHandle.op_Implicit h
            | MetadataToken.GenericParameter h -> GenericParameterHandle.op_Implicit h
            | MetadataToken.AssemblyDefinition h -> AssemblyDefinitionHandle.op_Implicit h
            | MetadataToken.GenericParameterConstraint h -> GenericParameterConstraintHandle.op_Implicit h
            | MetadataToken.Document h -> DocumentHandle.op_Implicit h
            | MetadataToken.MethodDebugInformation h -> MethodDebugInformationHandle.op_Implicit h
            | MetadataToken.LocalScope h -> LocalScopeHandle.op_Implicit h
            | MetadataToken.LocalVariable h -> LocalVariableHandle.op_Implicit h
            | MetadataToken.LocalConstant h -> LocalConstantHandle.op_Implicit h
            | MetadataToken.ImportScope h -> ImportScopeHandle.op_Implicit h
            | MetadataToken.CustomDebugInformation h -> CustomDebugInformationHandle.op_Implicit h

        MetadataTokens.GetToken handle

/// A metadata token operand together with the assembly whose metadata tables own it.
/// CLI metadata tokens are only meaningful relative to a module, so executable IL
/// operands should carry this context rather than consulting ambient thread state.
[<NoEquality ; NoComparison>]
type SourcedMetadataToken =
    {
        SourceAssembly : AssemblyName
        Token : MetadataToken
    }

[<RequireQualifiedAccess>]
module SourcedMetadataToken =
    let make (sourceAssembly : AssemblyName) (token : MetadataToken) : SourcedMetadataToken =
        {
            SourceAssembly = sourceAssembly
            Token = token
        }

    let ofInt (sourceAssembly : AssemblyName) (value : int32) : SourcedMetadataToken =
        MetadataToken.ofInt value |> make sourceAssembly

/// The operand of an instruction that names a type, method or field. A body read from a PE image
/// draws it from its module's metadata tables; a body minted by `Reflection.Emit` draws it from the
/// `DynamicScope` attached to the method's `DynamicResolver`, which is a `List&lt;object&gt;` of
/// already-resolved runtime handles and not metadata at all.
///
/// Two cases rather than one for the same reason `StringOperand` has two: the token forms are
/// *indistinguishable*. A scope operand is `index ||| someTag`, which is a perfectly well-formed
/// TypeDef/MethodDef/FieldDef token naming some unrelated real row in whatever module the dynamic
/// method is scoped to, and `DynamicScope`'s own indexer masks the tag off and ignores it entirely
/// (`DynamicILGenerator.cs:976-987`), so not even the tag is authoritative about what the entry is.
/// The universe is therefore settled when the body is decoded and recorded here.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type MetadataOperand =
    /// A token into the metadata tables of the assembly that owns it.
    | FromMetadata of SourcedMetadataToken
    /// Entry <paramref name="scopeIndex"/> of the emitting method's `DynamicScope`.
    ///
    /// The index and nothing else, as for `StringOperand.FromDynamicScope`: an `IlOp` is a
    /// description of code, so it must not carry a heap address, and the entry's contents are read
    /// when the instruction executes rather than when the body is decoded. That deferral is
    /// measured, not stylistic — CoreCLR's `DynamicResolver.ResolveToken`
    /// (`DynamicILGenerator.cs:772`) reads `m_scope[token]` at JIT, and a guest that replaces the
    /// entry between minting the method and first invoking it sees the *new* type.
    | FromDynamicScope of scopeIndex : int

/// A string token operand together with the assembly whose string heap owns it.
/// CLI string tokens are only meaningful relative to a module, so executable IL
/// operands should carry this context rather than consulting ambient thread state.
[<NoEquality ; NoComparison>]
type SourcedStringToken =
    {
        SourceAssembly : AssemblyName
        Token : StringToken
    }

[<RequireQualifiedAccess>]
module SourcedStringToken =
    let make (sourceAssembly : AssemblyName) (token : StringToken) : SourcedStringToken =
        {
            SourceAssembly = sourceAssembly
            Token = token
        }

    let ofInt (sourceAssembly : AssemblyName) (value : int32) : SourcedStringToken =
        StringToken.ofInt value |> make sourceAssembly

/// The operand of an `ldstr`. A body read from a PE image draws it from its module's user-string
/// heap; a body minted by `Reflection.Emit` draws it from the `DynamicScope` attached to the
/// method's `DynamicResolver`, which is a `List&lt;object&gt;` and not metadata at all.
///
/// These are two cases rather than one because the token forms are *indistinguishable*: a scope
/// operand is `index ||| 0x70000000`, which is a perfectly well-formed UserString token naming some
/// unrelated real row in whatever module the dynamic method is scoped to. Nothing about the bits
/// says which universe they belong to, so the universe has to be settled when the body is decoded
/// and recorded here, rather than guessed at by whoever executes the instruction.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type StringOperand =
    /// A token into the user-string heap of the assembly that owns it.
    | FromMetadata of SourcedStringToken
    /// Entry <paramref name="scopeIndex"/> of the emitting method's `DynamicScope`.
    ///
    /// The index and nothing else. Not the guest `string` object that holds the characters: an
    /// `IlOp` is a description of code, and a heap address would tie it to one machine's heap. And
    /// not the characters either, though they are known when the body is decoded — a guest can
    /// mutate a `System.String`'s data in place through an unsafe pointer after emitting it, and
    /// real .NET reads the contents at first JIT rather than at emit, so a value captured here
    /// would be a snapshot that can go stale. Both the characters and the object are resolved when
    /// the instruction executes, by reading the entry out of the executing method's live
    /// `DynamicScope` — nothing about it is captured when the method is minted, because a guest can
    /// replace the whole slot as well as mutate what is in it.
    | FromDynamicScope of scopeIndex : int

/// What a `DynamicScope` entry is, so far as decoding a method body needs to care.
///
/// `Unsupported` is not a decoding failure and must not be treated as one. `DynamicILGenerator`'s
/// constructor calls `m_scope.GetTokenFor(methodSignature)` before any user code runs, so *every*
/// dynamic method's scope has a signature blob at index 1 that no instruction ever names —
/// `GetCallableMethod` reads it out directly. A reader that insisted every entry be of a supported
/// kind would therefore refuse every dynamic method in existence. Entries are classified totally
/// here and only rejected if an instruction actually names one.
[<RequireQualifiedAccess>]
type DynamicScopeEntry =
    | String of string
    /// A boxed `System.RuntimeTypeHandle`, which is what `DynamicILGenerator.Emit(OpCode, Type)`
    /// stores: `GetTokenFor(RuntimeType)` is `m_scope.GetTokenFor(rtType.TypeHandle)`
    /// (`DynamicILGenerator.cs:496`).
    ///
    /// Carries no payload on purpose. Which type it names is read from the guest heap when the
    /// instruction executes, not now — see `MetadataOperand.FromDynamicScope`.
    | TypeHandle
    /// Some entry kind whose resolution is not yet implemented — a signature blob, a
    /// `RuntimeMethodHandle`, a nested `DynamicMethod`. The description names the kind, for the
    /// refusal message an instruction naming it would produce.
    | Unsupported of description : string

/// Which token universe a method body's operands index into.
[<RequireQualifiedAccess>]
[<NoEquality ; NoComparison>]
type IlTokenUniverse =
    /// The metadata tables and heaps of the named assembly, as for any body read from a PE image.
    | Metadata of AssemblyName
    /// The `DynamicScope` of a method minted by `Reflection.Emit`, keyed by the index a token's low
    /// 24 bits carry.
    | DynamicScope of entries : Map<int, DynamicScopeEntry>
