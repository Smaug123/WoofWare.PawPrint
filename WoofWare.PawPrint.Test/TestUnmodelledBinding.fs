namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The Loader reports what a guest's metadata names and it cannot bind as a result: a miss it can
/// name (a type absent from its assembly, an assembly no runtime directory supplies), or a shape
/// PawPrint does not implement binding at all, with the diagnostic that used to be a `failwith`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnmodelledBinding =

    /// The handles of an emitted image's shapes.
    type private Shapes =
        {
            /// `System.Gone`, scoped to CoreLib, which declares no such type.
            Absent : TypeReferenceHandle
            /// `System.Gone`, scoped to an assembly named `Nowhere`.
            Unbound : TypeReferenceHandle
            /// `System.Collections.Generic.List`1`, scoped to CoreLib.
            List : TypeReferenceHandle
            /// `N.Inner`, scoped to a module reference.
            ModuleScoped : TypeReferenceHandle
            /// A field reference whose parent is a module reference.
            ModuleParent : MemberReferenceHandle
            /// A field reference whose parent is a method definition.
            MethodParent : MemberReferenceHandle
            /// A reference to `Object.ToString`, a method, read as a field.
            MethodSignature : MemberReferenceHandle
            /// `N.Exported`, exported from another file of the assembly.
            FileExport : ExportedTypeHandle
        }

    let private emit () : byte[] * Shapes =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Unmodelled.dll",
            metadata.GetOrAddGuid (Guid "0d4b9e61-2a7c-4e3f-9b8d-5c6e7f8a9b0c"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "Unmodelled",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let nowhereRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString "Nowhere",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (scope : EntityHandle) (ns : string) (name : string) : TypeReferenceHandle =
            metadata.AddTypeReference (scope, metadata.GetOrAddString ns, metadata.GetOrAddString name)

        let objectRef =
            typeRef (AssemblyReferenceHandle.op_Implicit corelibRef) "System" "Object"

        let absent =
            typeRef (AssemblyReferenceHandle.op_Implicit corelibRef) "System" "Gone"

        let unbound =
            typeRef (AssemblyReferenceHandle.op_Implicit nowhereRef) "System" "Gone"

        let list =
            typeRef (AssemblyReferenceHandle.op_Implicit corelibRef) "System.Collections.Generic" "List`1"

        let otherModule =
            metadata.AddModuleReference (metadata.GetOrAddString "Other.netmodule")

        let moduleScoped =
            typeRef (ModuleReferenceHandle.op_Implicit otherModule) "N" "Inner"

        let int32Field =
            let blob = BlobBuilder ()
            BlobEncoder(blob).Field().Type().Int32 ()
            metadata.GetOrAddBlob blob

        let toStringSignature =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (0, (fun returnType -> returnType.Type().String ()), (fun _ -> ()))

            metadata.GetOrAddBlob blob

        let voidSignature =
            let blob = BlobBuilder ()

            BlobEncoder(blob).MethodSignature().Parameters (0, (fun returnType -> returnType.Void ()), (fun _ -> ()))

            metadata.GetOrAddBlob blob

        let body =
            let il = InstructionEncoder (BlobBuilder ())
            il.OpCode ILOpCode.Ret
            MethodBodyStreamEncoder(ilStream).AddMethodBody il

        // `<Module>` is row 1 and `Holder` row 2, which declares `M`, MethodDef row 1.
        metadata.AddTypeDefinition (
            TypeAttributes.NotPublic,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "N",
            metadata.GetOrAddString "Holder",
            TypeReferenceHandle.op_Implicit objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let m =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString "M",
                voidSignature,
                body,
                MetadataTokens.ParameterHandle 1
            )

        let moduleParent =
            metadata.AddMemberReference (
                ModuleReferenceHandle.op_Implicit otherModule,
                metadata.GetOrAddString "F",
                int32Field
            )

        let methodParent =
            metadata.AddMemberReference (MethodDefinitionHandle.op_Implicit m, metadata.GetOrAddString "F", int32Field)

        let methodSignature =
            metadata.AddMemberReference (
                TypeReferenceHandle.op_Implicit objectRef,
                metadata.GetOrAddString "ToString",
                toStringSignature
            )

        let file =
            metadata.AddAssemblyFile (
                metadata.GetOrAddString "Other.netmodule",
                metadata.GetOrAddBlob [| 0uy |],
                containsMetadata = true
            )

        let fileExport =
            metadata.AddExportedType (
                TypeAttributes.Public,
                metadata.GetOrAddString "N",
                metadata.GetOrAddString "Exported",
                AssemblyFileHandle.op_Implicit file,
                0
            )

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        image.ToArray (),
        {
            Absent = absent
            Unbound = unbound
            List = list
            ModuleScoped = moduleScoped
            ModuleParent = moduleParent
            MethodParent = methodParent
            MethodSignature = methodSignature
            FileExport = fileExport
        }

    /// The emitted image read, CoreLib, and everything needed to resolve against them.
    type private Fixture =
        {
            LoggerFactory : Microsoft.Extensions.Logging.ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assemblies : LoadedAssemblies
            Analysed : DumpedAssembly
            Shapes : Shapes
        }

    let private fixture () : Fixture =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (HostImage.frameworkDirectory, "System.Private.CoreLib.dll"))

        let image, shapes = emit ()

        let analysed =
            Assembly.read loggerFactory (Some "Unmodelled.dll") (new MemoryStream (image))

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = BaseClassTypes.ofCorelib corelib
            Assemblies = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]
            Analysed = analysed
            Shapes = shapes
        }

    let private unmodelledSaying (fragment : string) (unmodelled : UnmodelledBinding) : unit =
        Assert.That (unmodelled.Diagnostic, Does.Contain fragment)

    [<Test>]
    let ``a type reference scoped to a module reference is unmodelled`` () : unit =
        let f = fixture ()
        let target = f.Analysed.TypeRefs.[f.Shapes.ModuleScoped]

        match LoadedTypeResolution.resolveTypeRef f.Assemblies f.Analysed ImmutableArray.Empty target with
        | TypeResolutionResult.Unmodelled unmodelled ->
            unmodelledSaying "ModuleRef type resolution is not yet supported for type N.Inner" unmodelled
        | other -> failwith $"expected Unmodelled, got %O{other}"

        match
            TypeResolution.resolveTypeFromRef
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                target
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, Error (TypeBindingFailure.Unmodelled unmodelled) ->
            unmodelledSaying "ModuleRef type resolution is not yet supported for type N.Inner" unmodelled
        | _, other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``a type reference naming nothing is a named miss`` () : unit =
        let f = fixture ()

        match
            TypeResolution.resolveTypeFromRef
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                f.Analysed.TypeRefs.[f.Shapes.Absent]
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
            ()
        | _, other -> failwith $"expected TypeAbsent, got %A{other}"

        match
            TypeResolution.resolveTypeFromRef
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                f.Analysed.TypeRefs.[f.Shapes.Unbound]
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, Error (TypeBindingFailure.AssemblyUnavailable reference) -> reference.Name.Name |> shouldEqual "Nowhere"
        | _, other -> failwith $"expected AssemblyUnavailable, got %A{other}"

    [<Test>]
    let ``a type exported from another file of the assembly is unmodelled`` () : unit =
        let f = fixture ()

        match
            TypeResolution.tryResolveTypeFromExport
                f.LoggerFactory
                HostImage.runtimeDirs
                f.Analysed
                f.Analysed.ExportedTypes.[f.Shapes.FileExport]
                ImmutableArray.Empty
                f.Assemblies
        with
        | _, ExportedTypeResolution.Unmodelled unmodelled ->
            unmodelledSaying "AssemblyFile exported types are not yet supported" unmodelled
        | _, other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``a member reference whose parent is a module reference is unmodelled`` () : unit =
        let f = fixture ()

        match
            MemberReferenceParent.resolve
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                f.Assemblies
                f.Analysed
                f.Shapes.ModuleParent
        with
        | _, MemberReferenceParent.Unmodelled unmodelled ->
            unmodelledSaying "names a global member of another module, which is not modelled" unmodelled
        | _, other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``a field reference ECMA-335 forbids is unmodelled`` () : unit =
        let f = fixture ()

        let resolve (reference : MemberReferenceHandle) =
            FieldReferenceResolution.resolve
                f.LoggerFactory
                HostImage.runtimeDirs
                f.BaseClassTypes
                f.Assemblies
                f.Analysed
                reference
            |> snd

        match resolve f.Shapes.MethodParent with
        | FieldReferenceTarget.Unmodelled unmodelled ->
            unmodelledSaying "a field reference's parent is a MethodDef, which ECMA-335 does not permit" unmodelled
        | other -> failwith $"expected Unmodelled, got %A{other}"

        match resolve f.Shapes.MethodSignature with
        | FieldReferenceTarget.Unmodelled unmodelled -> unmodelledSaying "names a method, not a field" unmodelled
        | other -> failwith $"expected Unmodelled, got %A{other}"

    [<Test>]
    let ``an argument of an instantiation that names nothing is the instantiation's miss`` () : unit =
        let f = fixture ()

        let spelled (handle : TypeReferenceHandle) =
            TypeDefn.FromReference (f.Analysed.TypeRefs.[handle], SignatureTypeKind.Class)

        // The arguments are resolved in order, stopping at the first that fails, so the second
        // argument's unavailable assembly is never looked for.
        for arguments in [ [ f.Shapes.Absent ] ; [ f.Shapes.Absent ; f.Shapes.Unbound ] ] do
            let instantiation =
                TypeDefn.GenericInstantiation (
                    spelled f.Shapes.List,
                    arguments |> List.map spelled |> ImmutableArray.CreateRange
                )

            match
                TypeResolution.resolveTypeFromDefn
                    f.LoggerFactory
                    HostImage.runtimeDirs
                    f.BaseClassTypes
                    instantiation
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    f.Analysed
                    f.Assemblies
            with
            | _,
              Error (TypeBindingFailure.TypeAbsent (TypeResolutionMiss.TopLevelTypeAbsent (_, Some "System", "Gone"))) ->
                ()
            | _, other -> failwith $"expected System.Gone to be absent, got %A{other}"
