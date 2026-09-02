namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// A TypeDef says it is nested in two independent places: its own <c>Flags.Visibility</c> (one of
/// the <c>NestedXxx</c> values, ECMA-335 II.22.37) and a row in the NestedClass table naming its
/// encloser (II.22.32). Compilers keep the two in step; hand-written metadata need not. CoreCLR
/// resolves names by the NestedClass table (<c>ClassLoader::AddAvailableClassHaveLock</c>,
/// clsload.cpp) and refuses to load a type whose visibility disagrees with it
/// (<c>ClassLoader::CreateTypeHandleForTypeDefThrowing</c>, methodtablebuilder.cpp: a row under a
/// non-nested visibility is <c>VLDTR_E_TD_ENCLNOTNESTED</c>, a nested visibility with no row is
/// <c>IDS_CLASSLOAD_BADFORMAT</c>).
/// </summary>
/// <remarks>
/// PawPrint reads every TypeDef when it reads the assembly, so it refuses the image there rather
/// than at first use of the type; that costs a program which carries such a type and never loads
/// it, and no program that runs.
/// </remarks>
[<TestFixture>]
module TestNestedClassifier =

    /// An image with a top-level `Probe.Outer` and a second type `Inner` whose nesting metadata
    /// is exactly what the caller asks for: `innerVisibility` goes into its `Flags`, and
    /// `withNestedRow` decides whether the NestedClass table names `Outer` as its encloser.
    let private image (assemblyName : string) (innerVisibility : TypeAttributes) (withNestedRow : bool) : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString (assemblyName + ".dll"),
            metadata.GetOrAddGuid (Guid "7b1c2d3e-4f50-4617-8293-a4b5c6d7e8f9"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString assemblyName,
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString "System.Private.CoreLib",
                Version (10, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        // The real runtime declines an image with no `<Module>` row, and it is the oracle here.
        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let outer =
            metadata.AddTypeDefinition (
                TypeAttributes.Public ||| TypeAttributes.Class,
                metadata.GetOrAddString "Probe",
                metadata.GetOrAddString "Outer",
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                MetadataTokens.FieldDefinitionHandle 1,
                MetadataTokens.MethodDefinitionHandle 1
            )

        let inner =
            metadata.AddTypeDefinition (
                innerVisibility ||| TypeAttributes.Class,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "Inner",
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                MetadataTokens.FieldDefinitionHandle 1,
                MetadataTokens.MethodDefinitionHandle 1
            )

        if withNestedRow then
            metadata.AddNestedType (inner, outer)

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                BlobBuilder (),
                null,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    /// Both facts agree: this is what a compiler emits.
    let private agreeing : byte[] =
        image "NestedAgreeing" TypeAttributes.NestedPublic true

    /// The NestedClass table names an encloser, but the visibility is a top-level one.
    let private rowWithoutNestedVisibility : byte[] =
        image "NestedRowOnly" TypeAttributes.Public true

    /// The visibility is a nested one, but no NestedClass row names an encloser.
    let private nestedVisibilityWithoutRow : byte[] =
        image "NestedFlagOnly" TypeAttributes.NestedPublic false

    /// Runs `f` against the image as the host CLR sees it. Nothing executes, and the context is
    /// collectible so the test host is not permanently populated with the fixture.
    let private withHostAssembly (image : byte[]) (f : Reflection.Assembly -> 'a) : 'a =
        let context =
            Runtime.Loader.AssemblyLoadContext ("nestedClassifierOracle", isCollectible = true)

        try
            use stream = new MemoryStream (image)
            f (context.LoadFromStream stream)
        finally
            context.Unload ()

    let private readImage (image : byte[]) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image)
        Assembly.read loggerFactory None stream

    let private nilTypeDef : TypeDefinitionHandle =
        Unchecked.defaultof<TypeDefinitionHandle>

    /// The verdict the refusals below are modelled on, asserted rather than asserted-in-a-comment:
    /// the agreeing image loads, and each disagreeing one is found by name the way the NestedClass
    /// table says and then refused at class load.
    [<Test>]
    let ``host CLR loads the agreeing image and refuses both disagreeing ones`` () : unit =
        withHostAssembly
            agreeing
            (fun assembly ->
                let inner = assembly.GetType ("Probe.Outer+Inner", true)
                inner.IsNested |> shouldEqual true
                inner.DeclaringType.FullName |> shouldEqual "Probe.Outer"
                assembly.GetType ("Inner", false) |> shouldEqual null
            )

        withHostAssembly
            rowWithoutNestedVisibility
            (fun assembly ->
                // Found through its encloser, so the class hash keyed on the NestedClass row; then
                // refused at class load.
                assembly.GetType ("Inner", false) |> shouldEqual null

                Assert.Throws<BadImageFormatException> (fun () ->
                    assembly.GetType ("Probe.Outer+Inner", true) |> ignore<Type>
                )
                |> ignore<BadImageFormatException>
            )

        withHostAssembly
            nestedVisibilityWithoutRow
            (fun assembly ->
                // Found at top level, so the class hash ignored the visibility bits; then refused at
                // class load.
                assembly.GetType ("Probe.Outer+Inner", false) |> shouldEqual null

                Assert.Throws<TypeLoadException> (fun () -> assembly.GetType ("Inner", true) |> ignore<Type>)
                |> ignore<TypeLoadException>
            )

    [<Test>]
    let ``agreeing image: the nested type is reachable through its encloser and not at top level`` () : unit =
        let assembly = readImage agreeing

        let outer =
            assembly.TryGetTopLevelTypeDef "Probe" "Outer"
            |> Option.defaultWith (fun () -> failwith "Probe.Outer should be a top-level type")

        let inner =
            assembly.TryGetNestedTypeDef outer.TypeDefHandle "Inner"
            |> Option.defaultWith (fun () -> failwith "Inner should be nested in Probe.Outer")

        inner.IsNested |> shouldEqual true
        inner.DeclaringType |> shouldEqual outer.TypeDefHandle
        outer.IsNested |> shouldEqual false
        assembly.TryGetTopLevelTypeDef "" "Inner" |> Option.isNone |> shouldEqual true

        TypeInfo.fullName (fun handle -> assembly.TypeDefs.[handle]) inner
        |> shouldEqual "Probe.Outer+Inner"

        assembly.NestedTypeDefsByEnclosing.[ComparableTypeDefinitionHandle.Make outer.TypeDefHandle]
        |> Seq.toList
        |> shouldEqual [ inner.TypeDefHandle ]

    [<Test>]
    let ``a NestedClass row under a non-nested visibility is refused at read`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> readImage rowWithoutNestedVisibility |> ignore<DumpedAssembly>)

        exn.Message |> shouldContainText "Inner"
        exn.Message |> shouldContainText "NestedClass"
        exn.Message |> shouldContainText "Public"

    [<Test>]
    let ``a nested visibility with no NestedClass row is refused at read`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> readImage nestedVisibilityWithoutRow |> ignore<DumpedAssembly>)

        exn.Message |> shouldContainText "Inner"
        exn.Message |> shouldContainText "NestedClass"
        exn.Message |> shouldContainText "NestedPublic"

    /// `IsNested` is the NestedClass-table fact and nothing else: the visibility bits do not enter
    /// into it, so `fullName`'s walk over `DeclaringType` never dereferences a nil handle.
    [<Test>]
    let ``IsNested reads the NestedClass table, not the visibility bits`` () : unit =
        let assembly = readImage agreeing

        let outer =
            assembly.TryGetTopLevelTypeDef "Probe" "Outer"
            |> Option.defaultWith (fun () -> failwith "Probe.Outer should be a top-level type")

        let inner =
            assembly.TryGetNestedTypeDef outer.TypeDefHandle "Inner"
            |> Option.defaultWith (fun () -> failwith "Inner should be nested in Probe.Outer")

        let get (handle : TypeDefinitionHandle) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            assembly.TypeDefs.[handle]

        let innerWithoutRow =
            { inner with
                DeclaringType = nilTypeDef
            }

        innerWithoutRow.IsNested |> shouldEqual false
        TypeInfo.fullName get innerWithoutRow |> shouldEqual "Inner"

        let outerWithNestedBits =
            { outer with
                TypeAttributes = TypeAttributes.NestedPublic ||| TypeAttributes.Class
            }

        outerWithNestedBits.IsNested |> shouldEqual false
        TypeInfo.fullName get outerWithNestedBits |> shouldEqual "Probe.Outer"

    /// On compiler output the two facts agree, and the host CLR's `Type.IsNested` and
    /// `Type.DeclaringType` are the outside oracle for what PawPrint reads off the table. The test
    /// assembly is F# output, whose closures and nested modules give the sweep some shapes C# does
    /// not emit.
    [<Test>]
    let ``IsNested and DeclaringType agree with host reflection over the test assembly`` () : unit =
        let location = Reflection.Assembly.GetExecutingAssembly().Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        let assembly = Assembly.readFile loggerFactory location
        let hostModule = Reflection.Assembly.GetExecutingAssembly().ManifestModule

        let disagreements =
            assembly.TypeDefs.Values
            // Row 1 is `<Module>`, which host reflection declines to resolve as a type.
            |> Seq.filter (fun ty ->
                MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit ty.TypeDefHandle : EntityHandle)
                <> 1
            )
            |> Seq.choose (fun ty ->
                let hostType =
                    hostModule.ResolveType (
                        MetadataTokens.GetToken (TypeDefinitionHandle.op_Implicit ty.TypeDefHandle : EntityHandle)
                    )

                let hostDeclaring =
                    if isNull hostType.DeclaringType then
                        nilTypeDef
                    else
                        MetadataTokens.TypeDefinitionHandle (hostType.DeclaringType.MetadataToken &&& 0x00FFFFFF)

                if ty.IsNested <> hostType.IsNested || ty.DeclaringType <> hostDeclaring then
                    Some
                        $"%s{hostType.FullName}: PawPrint IsNested=%b{ty.IsNested} DeclaringType=%O{ty.DeclaringType}, host IsNested=%b{hostType.IsNested} DeclaringType=%O{hostDeclaring}"
                else
                    None
            )
            |> Seq.toList

        disagreements |> shouldEqual []
        assembly.TypeDefs.Count |> shouldBeGreaterThan 100
