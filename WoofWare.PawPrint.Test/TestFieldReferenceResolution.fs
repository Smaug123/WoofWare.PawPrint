namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.Loader
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `FieldReferenceResolution` against the real runtime's own answer, `Module.ResolveField` on the
/// same bytes: over every field MemberRef in a set of shared-framework assemblies, and over a
/// hand-emitted image holding the shapes no compiler writes.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFieldReferenceResolution =

    [<RequireQualifiedAccess>]
    type private Oracle =
        | Field of FieldInfo
        | Missing
        | NoAnswer

    let private agrees (oracle : Oracle) (ours : FieldReferenceTarget) : bool =
        match oracle, ours with
        | Oracle.Field field, FieldReferenceTarget.Defined (assembly, handle) ->
            field.Module.Assembly.GetName().Name = assembly.Name.Name
            && field.MetadataToken = MetadataTokens.GetToken (FieldDefinitionHandle.op_Implicit handle : EntityHandle)
        | Oracle.Missing, FieldReferenceTarget.Missing -> true
        | _ -> false

    let private placeholder : Type[] = Array.create 16 typeof<obj>

    /// `Module.ResolveField`, read as the binder's answer. The binder's `MissingFieldException`
    /// does not reach the caller: `RuntimeModule.ResolveField` catches it and retries the token as a
    /// literal FieldDef, which a MemberRef is not, so it surfaces as `ArgumentOutOfRangeException`.
    /// `context` is a method whose IL uses the reference, whose type parameters satisfy whatever
    /// constraints the parent places on them.
    let private askReflection (m : Module) (context : MethodDefinitionHandle option) (token : int) : Oracle =
        let resolve (typeArgs : Type[]) (methodArgs : Type[]) =
            try
                Oracle.Field (m.ResolveField (token, typeArgs, methodArgs))
            with :? ArgumentOutOfRangeException ->
                Oracle.Missing

        try
            resolve null null
        with
        | :? ArgumentException
        | :? BadImageFormatException
        | :? TypeLoadException ->
            let inContext =
                context
                |> Option.bind (fun user ->
                    let user =
                        m.ResolveMethod (
                            MetadataTokens.GetToken (MethodDefinitionHandle.op_Implicit user : EntityHandle)
                        )

                    let methodArgs =
                        if user.IsGenericMethodDefinition then
                            user.GetGenericArguments ()
                        else
                            [||]

                    try
                        Some (resolve (user.DeclaringType.GetGenericArguments ()) methodArgs)
                    with _ ->
                        None
                )

            match inContext with
            | Some answer -> answer
            | None ->
                try
                    resolve placeholder placeholder
                with _ ->
                    Oracle.NoAnswer

    let assemblyNames : string list = TestMethodReferenceResolution.assemblyNames

    [<TestCaseSource(nameof assemblyNames)>]
    let ``every field MemberRef resolves where the real runtime binds it`` (assemblyName : string) : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()
        let reflected = System.Reflection.Assembly.Load (AssemblyName assemblyName)

        Path.GetDirectoryName reflected.Location
        |> shouldEqual (Path.GetFullPath frameworkDir)

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let analysed = Assembly.readFile loggerFactory reflected.Location
        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let mutable assemblies = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]
        let contexts = TestMethodReferenceResolution.contextsOfUse analysed
        let failures = ResizeArray<string> ()
        let mutable agreed = 0
        let mutable noAnswer = 0

        for KeyValue (handle, reference) in analysed.Members do
            match reference.Signature with
            | MemberSignature.Method _ -> ()
            | MemberSignature.Field _ ->
                let token =
                    MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let assemblies', ours =
                    FieldReferenceResolution.resolve
                        loggerFactory
                        runtimeDirs
                        baseClassTypes
                        assemblies
                        (assemblies.ByDefinitionName analysed.DefinitionFullName)
                        handle

                assemblies <- assemblies'

                let context =
                    match contexts.TryGetValue handle with
                    | true, user -> Some user
                    | false, _ -> None

                match askReflection reflected.ManifestModule context token with
                | Oracle.NoAnswer -> noAnswer <- noAnswer + 1
                | oracle when agrees oracle ours -> agreed <- agreed + 1
                | oracle ->
                    failures.Add $"%s{reference.PrettyName} (0x%08x{token}): runtime %A{oracle}, resolver %A{ours}"

        TestContext.Progress.WriteLine
            $"%s{assemblyName}: %d{agreed} field MemberRefs agree; %d{noAnswer} the runtime could not be asked about"

        if failures.Count > 0 then
            failures |> Seq.truncate 30 |> String.concat Environment.NewLine |> failwith

        // An oracle that cannot be asked is no oracle.
        noAnswer * 20 |> shouldBeSmallerThan (agreed + 1)

    /// The parent a hand-emitted reference names.
    [<RequireQualifiedAccess>]
    type private Parent =
        /// `Base<int32>`, which declares `f : !0`, `s : int32` (static) and `k : int32` (literal).
        | BaseOfInt
        /// `Derived : Base<int32>`, which declares nothing.
        | Derived
        /// `int32[]`, which as an array has no fields.
        | Vector

    [<RequireQualifiedAccess>]
    type private FieldType =
        | Var0
        | Int32
        | ModifiedVar0

    let private cases : (Parent * string * FieldType) list =
        [
            Parent.BaseOfInt, "f", FieldType.Var0
            // Symbolic: `!0` is not the `int32` an instantiation would put there.
            Parent.BaseOfInt, "f", FieldType.Int32
            // Custom modifiers are compared.
            Parent.BaseOfInt, "f", FieldType.ModifiedVar0
            Parent.BaseOfInt, "s", FieldType.Int32
            // A literal field has no FieldDesc for a reference to find.
            Parent.BaseOfInt, "k", FieldType.Int32
            // Fields are not inherited.
            Parent.Derived, "f", FieldType.Var0
            Parent.Derived, "s", FieldType.Int32
            Parent.Vector, "Length", FieldType.Int32
        ]

    let private emit () : byte[] * MemberReferenceHandle list =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "FieldRefs.dll",
            metadata.GetOrAddGuid (Guid "7a1c2e33-5f4b-4d6e-8a90-1b2c3d4e5f60"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "FieldRefs",
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

        let corelibType (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = corelibType "System" "Object"
        let isConstRef = corelibType "System.Runtime.CompilerServices" "IsConst"

        let fieldSignature (fieldType : FieldType) : BlobHandle =
            let blob = BlobBuilder ()
            let encoder = BlobEncoder(blob).Field().Type ()

            match fieldType with
            | FieldType.Var0 -> encoder.GenericTypeParameter 0
            | FieldType.Int32 -> encoder.Int32 ()
            | FieldType.ModifiedVar0 ->
                encoder.CustomModifiers().AddModifier (isConstRef, true)
                |> ignore<CustomModifiersEncoder>

                encoder.GenericTypeParameter 0

            metadata.GetOrAddBlob blob

        // `<Module>` is row 1, `Base`1` row 2, `Derived` row 3.
        let baseHandle : EntityHandle =
            MetadataTokens.TypeDefinitionHandle 2 |> TypeDefinitionHandle.op_Implicit

        let derivedHandle : EntityHandle =
            MetadataTokens.TypeDefinitionHandle 3 |> TypeDefinitionHandle.op_Implicit

        let typeSpec (encode : SignatureTypeEncoder -> unit) : EntityHandle =
            let blob = BlobBuilder ()
            encode (BlobEncoder(blob).TypeSpecificationSignature ())
            TypeSpecificationHandle.op_Implicit (metadata.AddTypeSpecification (metadata.GetOrAddBlob blob))

        let baseOfInt =
            typeSpec (fun encoder ->
                let args = encoder.GenericInstantiation (baseHandle, 1, false)
                args.AddArgument().Int32 ()
            )

        let addField (attributes : FieldAttributes) (name : string) (fieldType : FieldType) =
            metadata.AddFieldDefinition (attributes, metadata.GetOrAddString name, fieldSignature fieldType)

        let f = addField FieldAttributes.Public "f" FieldType.Var0

        addField (FieldAttributes.Public ||| FieldAttributes.Static) "s" FieldType.Int32
        |> ignore<FieldDefinitionHandle>

        let k =
            addField
                (FieldAttributes.Public
                 ||| FieldAttributes.Static
                 ||| FieldAttributes.Literal
                 ||| FieldAttributes.HasDefault)
                "k"
                FieldType.Int32

        metadata.AddConstant ((FieldDefinitionHandle.op_Implicit k : EntityHandle), box 7)
        |> ignore<ConstantHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Base`1",
            objectRef,
            f,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public ||| TypeAttributes.Class,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Derived",
            baseOfInt,
            MetadataTokens.FieldDefinitionHandle 4,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddGenericParameter (
            (TypeDefinitionHandle.op_Implicit (MetadataTokens.TypeDefinitionHandle 2) : EntityHandle),
            GenericParameterAttributes.None,
            metadata.GetOrAddString "T",
            0
        )
        |> ignore<GenericParameterHandle>

        let vector = typeSpec (fun encoder -> encoder.SZArray().Int32 ())

        let references =
            cases
            |> List.map (fun (parent, name, fieldType) ->
                let parent =
                    match parent with
                    | Parent.BaseOfInt -> baseOfInt
                    | Parent.Derived -> derivedHandle
                    | Parent.Vector -> vector

                metadata.AddMemberReference (parent, metadata.GetOrAddString name, fieldSignature fieldType)
            )

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray (), references

    [<Test>]
    let ``field MemberRefs in shapes no compiler emits resolve where the real runtime binds them`` () : unit =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let image, references = emit ()
        let context = AssemblyLoadContext ("FieldRefs", isCollectible = true)

        try
            let reflected = context.LoadFromStream (new MemoryStream (image))

            let analysed =
                Assembly.read loggerFactory (Some "FieldRefs.dll") (new MemoryStream (image))

            let baseClassTypes = BaseClassTypes.ofCorelib corelib
            let mutable assemblies = LoadedAssemblies.ofAssemblies [ corelib ; analysed ]
            let failures = ResizeArray<string> ()
            let mutable defined = 0
            let mutable missing = 0

            for case, handle in List.zip cases references do
                let token =
                    MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)

                let assemblies', ours =
                    FieldReferenceResolution.resolve
                        loggerFactory
                        runtimeDirs
                        baseClassTypes
                        assemblies
                        (assemblies.ByDefinitionName analysed.DefinitionFullName)
                        handle

                assemblies <- assemblies'

                match askReflection reflected.ManifestModule None token with
                | oracle when agrees oracle ours ->
                    match ours with
                    | FieldReferenceTarget.Defined _ -> defined <- defined + 1
                    | _ -> missing <- missing + 1
                | oracle -> failures.Add $"%A{case}: runtime %A{oracle}, resolver %A{ours}"

            if failures.Count > 0 then
                failures |> String.concat Environment.NewLine |> failwith

            // Both answers must have been compared: two references bind, the rest do not.
            defined |> shouldEqual 2
            missing |> shouldEqual (cases.Length - 2)
        finally
            context.Unload ()
