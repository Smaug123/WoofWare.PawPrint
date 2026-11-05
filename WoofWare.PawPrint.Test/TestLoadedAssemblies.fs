namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `LoadedAssemblies` is the one place that knows an assembly's definition identity is not its
/// reference identity. These tests pin its laws directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestLoadedAssemblies =

    let private corelibPath : string = HostImage.coreLibPath
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    let private readAssembly (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory path

    /// Bypasses the process-lifetime parse cache, so this really does produce a fresh instance.
    let private readUncached (path : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (File.ReadAllBytes path)
        AssemblyApi.read loggerFactory (Some path) stream

    /// Real assemblies from the pinned shared framework, which is where mismatching
    /// (reference, definition) identity pairs actually occur. Synthesising `DumpedAssembly`
    /// values is not practical, so the generator draws from this pool.
    let private pool : DumpedAssembly array Lazy =
        System.Lazy<_>
            .Create (fun () ->
                [|
                    "System.Private.CoreLib"
                    "mscorlib"
                    "System"
                    "System.Runtime"
                    "netstandard"
                    "System.Console"
                    "System.Collections"
                |]
                |> Array.choose (fun name ->
                    let path = Path.Combine (runtimeDir, name + ".dll")
                    if File.Exists path then Some (readAssembly path) else None
                )
            )

    /// Every AssemblyReference declared by anything in the pool. This is the only honest source of
    /// reference identities: half of these disagree with their referent's definition identity.
    let private references : (DumpedAssembly * AssemblyReference) array Lazy =
        System.Lazy<_>
            .Create (fun () ->
                pool.Value
                |> Array.collect (fun assy ->
                    assy.AssemblyReferences.Values |> Seq.map (fun r -> assy, r) |> Seq.toArray
                )
            )

    /// The operations that can be performed on a load context.
    type private Op =
        /// Register this pool assembly under its own definition identity.
        | Load of int
        /// Bind reference `refIndex` to pool assembly `assyIndex`.
        | Bind of refIndex : int * assyIndex : int

    /// Reference implementation: an association list of definition identity -> assembly (first
    /// write wins), and one of reference identity -> definition identity (last write wins). Kept
    /// deliberately naive so it is obviously correct by inspection.
    type private Model =
        {
            ByDefinition : (string * DumpedAssembly) list
            Bindings : (string * string) list
        }

    let private modelEmpty =
        {
            ByDefinition = []
            Bindings = []
        }

    let private modelLoad (assy : DumpedAssembly) (m : Model) : Model =
        if m.ByDefinition |> List.exists (fun (k, _) -> k = assy.Name.FullName) then
            m
        else
            { m with
                ByDefinition = m.ByDefinition @ [ assy.Name.FullName, assy ]
            }

    let private modelBind (reference : AssemblyReference) (assy : DumpedAssembly) (m : Model) : Model =
        let m = modelLoad assy m

        { m with
            Bindings =
                (m.Bindings |> List.filter (fun (k, _) -> k <> reference.Name.FullName))
                @ [ reference.Name.FullName, assy.Name.FullName ]
        }

    let private modelResolveReference (reference : AssemblyReference) (m : Model) : DumpedAssembly option =
        let byDefinition (name : string) =
            m.ByDefinition |> List.tryPick (fun (k, v) -> if k = name then Some v else None)

        match
            m.Bindings
            |> List.tryPick (fun (k, v) -> if k = reference.Name.FullName then Some v else None)
        with
        | Some definitionName -> byDefinition definitionName
        | None -> byDefinition reference.Name.FullName

    let private opsGen : Gen<Op list> =
        gen {
            let assemblyCount = pool.Value.Length
            let referenceCount = references.Value.Length

            return!
                Gen.listOf (
                    Gen.oneof
                        [
                            Gen.choose (0, assemblyCount - 1) |> Gen.map Op.Load
                            Gen.zip (Gen.choose (0, referenceCount - 1)) (Gen.choose (0, assemblyCount - 1))
                            |> Gen.map Op.Bind
                        ]
                )
        }

    /// The central invariant, checked after every single operation.
    let private assertEveryAssemblyFindableByItsOwnName (real : LoadedAssemblies) (model : Model) : unit =
        for _, assy in model.ByDefinition do
            match real.TryByDefinition assy.Name with
            | None ->
                Assert.Fail
                    $"Assembly %s{assy.Name.FullName} is in the load context but is not findable by its own definition identity"
            | Some found -> found.Name.FullName |> shouldEqual assy.Name.FullName

    /// <summary>
    /// The model's <c>ByDefinition</c> is an append-only association list with first-write-wins,
    /// so it already <em>is</em> the load order — no separate model state is needed to check it.
    /// </summary>
    /// <remarks>
    /// Load order is guest-visible: it is the order <c>AppDomain.GetAssemblies()</c> reports, so a
    /// path that appended twice, appended on re-registration, or moved an identity on rebinding
    /// would be a wrong answer to a guest rather than an internal untidiness.
    /// </remarks>
    let private assertLoadOrderMatches (real : LoadedAssemblies) (model : Model) : unit =
        (real.DefinitionNamesInLoadOrder |> List.ofSeq)
        |> shouldEqual (model.ByDefinition |> List.map fst)

    [<Test>]
    let ``LoadedAssemblies agrees with a naive reference implementation`` () : unit =
        if pool.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let property (ops : Op list) : bool =
            let mutable real = LoadedAssemblies.empty
            let mutable model = modelEmpty

            for op in ops do
                match op with
                | Op.Load i ->
                    let assy = pool.Value.[i]
                    real <- real.WithLoadedAssembly assy
                    model <- modelLoad assy model
                | Op.Bind (refIndex, assyIndex) ->
                    let _, reference = references.Value.[refIndex]
                    let assy = pool.Value.[assyIndex]
                    let next, canonical = real.WithBoundReference reference assy
                    real <- next
                    model <- modelBind reference assy model

                    // WithBoundReference must hand back the instance the context actually holds.
                    canonical.Name.FullName |> shouldEqual assy.Name.FullName

                assertEveryAssemblyFindableByItsOwnName real model
                assertLoadOrderMatches real model

            // Definition-identity lookups agree.
            for assy in pool.Value do
                let expected =
                    model.ByDefinition
                    |> List.tryPick (fun (k, v) -> if k = assy.Name.FullName then Some v else None)

                (real.TryByDefinition assy.Name |> Option.isSome)
                |> shouldEqual (expected |> Option.isSome)

            // Reference lookups agree, including the exact-identity fallback.
            for _, reference in references.Value do
                let expected = modelResolveReference reference model
                let actual = real.TryResolveReference reference

                match expected, actual with
                | None, None -> ()
                | Some e, Some a -> a.Name.FullName |> shouldEqual e.Name.FullName
                | _ ->
                    Assert.Fail
                        $"Disagreement resolving reference %s{reference.Name.FullName}: model %b{expected.IsSome}, real %b{actual.IsSome}"

            true

        Prop.forAll (Arb.fromGen opsGen) property |> Check.QuickThrowOnFailure

    /// Loading an assembly is idempotent, and never displaces what is already there.
    [<Test>]
    let ``WithLoadedAssembly is idempotent`` () : unit =
        if pool.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let assy = pool.Value.[0]
        let once = LoadedAssemblies.empty.WithLoadedAssembly assy
        let twice = once.WithLoadedAssembly assy

        (twice.DefinitionNames |> Seq.length)
        |> shouldEqual (once.DefinitionNames |> Seq.length)

        // Re-registration must not append a second entry: load order is what a guest sees, so a
        // duplicate here is an assembly appearing twice in `AppDomain.GetAssemblies()`.
        (twice.DefinitionNamesInLoadOrder |> List.ofSeq)
        |> shouldEqual [ assy.Name.FullName ]

        twice.ContainsDefinition assy.Name |> shouldEqual true

    /// Binding the same reference to the same assembly twice changes nothing, and a second
    /// reference to an already-loaded assembly reuses the instance already held.
    [<Test>]
    let ``WithBoundReference is idempotent and preserves the canonical instance`` () : unit =
        if references.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let _, reference = references.Value.[0]
        let assy = pool.Value.[0]

        let first, canonicalFirst = LoadedAssemblies.empty.WithBoundReference reference assy
        let second, canonicalSecond = first.WithBoundReference reference assy

        canonicalFirst.Name.FullName |> shouldEqual assy.Name.FullName
        canonicalSecond.Name.FullName |> shouldEqual assy.Name.FullName

        (second.DefinitionNames |> Seq.length)
        |> shouldEqual (first.DefinitionNames |> Seq.length)

        // A distinct instance of the same assembly must not displace the one already
        // held. `Assembly.readFile` memoises by path, so it would hand back a reference-equal
        // value and never exercise the branch; read uncached to get a real second instance.
        let reread = readUncached assy.OriginalPath.Value
        Object.ReferenceEquals (reread, assy) |> shouldEqual false
        reread.Name.FullName |> shouldEqual assy.Name.FullName

        let third, canonicalThird = second.WithBoundReference reference reread

        // The instance already held wins: exactly one DumpedAssembly per definition identity.
        Object.ReferenceEquals (canonicalThird, assy) |> shouldEqual true

        (third.DefinitionNames |> Seq.length)
        |> shouldEqual (second.DefinitionNames |> Seq.length)

        // A *distinct instance* of an identity already held is the one shape the property test's
        // pool cannot produce, because `Assembly.readFile` memoises by path. It must not append.
        (third.DefinitionNamesInLoadOrder |> List.ofSeq)
        |> shouldEqual [ assy.Name.FullName ]

        match third.TryByDefinition assy.Name with
        | None -> Assert.Fail "Expected the assembly to remain findable by its definition identity"
        | Some held -> Object.ReferenceEquals (held, assy) |> shouldEqual true

    /// Two *different* builds can make the identical identity claim — trivially so for unsigned
    /// assemblies, whose entire identity is `Foo, Version=0.0.0.0, Culture=neutral,
    /// PublicKeyToken=null`. Silently keeping one and discarding the other would mean resolving
    /// and executing metadata the caller did not ask for, so this must crash.
    ///
    /// A coarser fingerprint than the module version ID (type counts, say) would let exactly this
    /// case through: both assemblies below define one type.
    let private conflictingBuildBytes () : (DumpedAssembly * byte[]) * (DumpedAssembly * byte[]) =
        let corelibReference =
            TypeIdentityTestHelpers.metadataReferenceFromImage (File.ReadAllBytes corelibPath)

        let compile (source : string) : DumpedAssembly * byte[] =
            let bytes =
                TypeIdentityTestHelpers.compileLibrary "Ambiguous" [ corelibReference ] [ source ]

            TypeIdentityTestHelpers.dumpedAssembly None bytes, bytes

        let first = compile "namespace N { public class OnlyType { public int A; } }"
        let second = compile "namespace N { public class OnlyType { public string B; } }"

        // Same declared identity, same type count, different builds.
        (fst second).Name.FullName |> shouldEqual (fst first).Name.FullName
        (fst second).TypeDefs.Count |> shouldEqual (fst first).TypeDefs.Count
        (fst second).ModuleVersionId |> shouldNotEqual (fst first).ModuleVersionId

        first, second

    let private conflictingBuilds () : DumpedAssembly * DumpedAssembly =
        let first, second = conflictingBuildBytes ()
        fst first, fst second

    let private expectCollisionRejected (act : unit -> unit) : unit =
        let thrown =
            try
                act ()
                None
            with e ->
                Some e.Message

        match thrown with
        | None -> Assert.Fail "Expected two different builds claiming one identity to be rejected"
        | Some msg -> msg |> shouldContainText "module version IDs"

    [<Test>]
    let ``two different builds claiming one identity is a hard error when binding a reference`` () : unit =
        if references.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let first, second = conflictingBuilds ()
        let _, reference = references.Value.[0]
        let loaded, _ = LoadedAssemblies.empty.WithBoundReference reference first

        expectCollisionRejected (fun () -> loaded.WithBoundReference reference second |> ignore)

    /// Direct registration must enforce the same rule, or it is a way around the guard.
    [<Test>]
    let ``two different builds claiming one identity is a hard error when registering directly`` () : unit =
        let first, second = conflictingBuilds ()
        let loaded = LoadedAssemblies.empty.WithLoadedAssembly first

        expectCollisionRejected (fun () -> loaded.WithLoadedAssembly second |> ignore)
        expectCollisionRejected (fun () -> LoadedAssemblies.ofAssemblies [ first ; second ] |> ignore)

    /// ...while staying idempotent for the same build, which is the common case: the entry
    /// assembly is registered directly and is also discoverable on disk.
    [<Test>]
    let ``registering the same build twice is idempotent`` () : unit =
        let first, _ = conflictingBuilds ()

        let once = LoadedAssemblies.empty.WithLoadedAssembly first
        let twice = once.WithLoadedAssembly first

        (twice.DefinitionNames |> Seq.length)
        |> shouldEqual (once.DefinitionNames |> Seq.length)

        match twice.TryByDefinition first.Name with
        | None -> Assert.Fail "Expected the assembly to remain findable by its definition identity"
        | Some held -> Object.ReferenceEquals (held, first) |> shouldEqual true

    /// Two distinct reads of one file are byte-identical, so they must be accepted as the same
    /// assembly — otherwise every entry assembly that is also discoverable on disk would crash.
    [<Test>]
    let ``distinct instances of one image compare as the same assembly`` () : unit =
        if pool.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let assy = pool.Value.[0]
        let reread = readUncached assy.OriginalPath.Value

        Object.ReferenceEquals (reread, assy) |> shouldEqual false
        assy.HasSameContentAs reread |> shouldEqual true

    /// The MVID is an assertion stamped into the image, not a digest of it: an IL rewriter that
    /// preserves it, or crafted metadata, gives two different images claiming to be one build.
    /// Sameness must therefore be decided on content, so that such a pair is still rejected.
    [<Test>]
    let ``images sharing an MVID but differing in content are not the same assembly`` () : unit =
        let (first, _), (second, originalSecondBytes) = conflictingBuildBytes ()

        // Splice `first`'s MVID GUID bytes over `second`'s, leaving the rest of its metadata
        // alone, to synthesise the MVID collision an equality-on-MVID check would wave through.
        let secondBytes = Array.copy originalSecondBytes
        let firstMvid = first.ModuleVersionId.ToByteArray ()
        let secondMvid = second.ModuleVersionId.ToByteArray ()

        let mvidOffset =
            let rec find (i : int) =
                if i + secondMvid.Length > secondBytes.Length then
                    None
                elif Span(secondBytes, i, secondMvid.Length).SequenceEqual (Span secondMvid) then
                    Some i
                else
                    find (i + 1)

            find 0

        match mvidOffset with
        | None -> Assert.Ignore "Could not locate the MVID bytes in the compiled image"
        | Some offset ->
            Array.blit firstMvid 0 secondBytes offset firstMvid.Length
            let spliced = TypeIdentityTestHelpers.dumpedAssembly None secondBytes

            // Same declared identity, and now the same MVID — but different metadata.
            spliced.Name.FullName |> shouldEqual first.Name.FullName
            spliced.ModuleVersionId |> shouldEqual first.ModuleVersionId
            first.HasSameContentAs spliced |> shouldEqual false

            let loaded = LoadedAssemblies.empty.WithLoadedAssembly first
            expectCollisionRejected (fun () -> loaded.WithLoadedAssembly spliced |> ignore)

    /// IL method bodies live outside the metadata block, and PawPrint reads them through
    /// `PEReader.GetMethodBody`. Two images can therefore agree on every byte of metadata — same
    /// MVID, same type and method rows, same signatures — and still execute differently. Comparing
    /// only metadata would call such a pair the same assembly and silently run the wrong one.
    [<Test>]
    let ``images differing only in an IL method body are not the same assembly`` () : unit =
        let corelibReference =
            TypeIdentityTestHelpers.metadataReferenceFromImage (File.ReadAllBytes corelibPath)

        // Two constants of equal encoded width, so the two `ldc.i4` bodies are the same length and
        // the metadata — which records only the body's RVA and size — is unaffected.
        let compile (returned : string) : byte[] =
            let source =
                $"namespace N {{ public static class C {{ public static int M() {{ return %s{returned}; }} }} }}"

            TypeIdentityTestHelpers.compileLibrary "SameMetadata" [ corelibReference ] [ source ]

        let firstBytes = compile "1122867"
        let secondBytes = compile "1146447" |> Array.copy

        firstBytes.Length |> shouldEqual secondBytes.Length

        let first = TypeIdentityTestHelpers.dumpedAssembly None firstBytes
        let unspliced = TypeIdentityTestHelpers.dumpedAssembly None secondBytes

        // Roslyn stamps a fresh MVID per build and the MVID lives in metadata, so splice it across
        // to leave the IL bodies as the only difference between the two images.
        let firstMvid = first.ModuleVersionId.ToByteArray ()
        let secondMvid = unspliced.ModuleVersionId.ToByteArray ()

        let mvidOffset =
            let rec find (i : int) =
                if i + secondMvid.Length > secondBytes.Length then
                    None
                elif Span(secondBytes, i, secondMvid.Length).SequenceEqual (Span secondMvid) then
                    Some i
                else
                    find (i + 1)

            find 0

        match mvidOffset with
        | None -> Assert.Ignore "Could not locate the MVID bytes in the compiled image"
        | Some offset ->

        Array.blit firstMvid 0 secondBytes offset firstMvid.Length
        let second = TypeIdentityTestHelpers.dumpedAssembly None secondBytes

        let metadataOf (assy : DumpedAssembly) : byte array =
            assy.PeReader.GetMetadata().GetContent () |> Seq.toArray

        // If the compiler varied metadata beyond the MVID, this pair does not isolate an
        // IL-body-only difference and so cannot make the point.
        if metadataOf first <> metadataOf second then
            Assert.Ignore
                "Compiler produced metadata differing beyond the MVID; cannot isolate an IL-body-only difference."

        // Byte-identical metadata — so a metadata-only comparison would call these the same
        // assembly — yet the images differ, and they differ precisely in the IL.
        first.ModuleVersionId |> shouldEqual second.ModuleVersionId
        firstBytes |> shouldNotEqual secondBytes
        first.HasSameContentAs second |> shouldEqual false

        let loaded = LoadedAssemblies.empty.WithLoadedAssembly first
        expectCollisionRejected (fun () -> loaded.WithLoadedAssembly second |> ignore)

    let private dynamicParts (name : string) : DynamicAssemblyName =
        {
            SimpleName = name
            Version = Version (0, 0, 0, 0)
            Culture = ""
            PublicKey = System.Collections.Immutable.ImmutableArray<byte>.Empty
            Flags = enum<AssemblyFlags> 0
            HashAlgorithm = AssemblyHashAlgorithm.Sha1
        }

    /// The scope of a dynamic assembly named `name`, with no public key, as
    /// `AppDomain_CreateDynamicAssembly` would make it.
    let private dynamicScope (name : string) : EmitScope =
        DynamicAssemblyImage.scope (dynamicParts name) Guid.Empty

    /// The image of `dynamicScope name`, read as an ordinary assembly.
    let private dynamicImage (name : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        use stream =
            new MemoryStream (DynamicAssemblyImage.build (dynamicParts name) Guid.Empty)

        AssemblyApi.read loggerFactory None stream

    let private registerDynamic (scope : EmitScope) (loaded : LoadedAssemblies) : LoadedAssemblies * DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        match loaded.WithDynamicAssembly loggerFactory scope with
        | Ok registered -> registered
        | Error existing -> failwith $"unexpected collision with %s{existing.DefinitionFullName}"

    /// A reference whose identity is `target`'s definition identity: the shape the exact-identity
    /// fallback in `TryResolveReference` answers.
    let private referenceNaming (target : DumpedAssembly) : AssemblyReference =
        let _, reference = references.Value.[0]

        { reference with
            Name = target.Name
            FullName = target.DefinitionFullName
        }

    [<Test>]
    let ``a dynamic assembly is found by its own name and never by a reference`` () : unit =
        if references.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let loaded, dynamic =
            registerDynamic (dynamicScope "Dynamic") LoadedAssemblies.empty

        loaded.IsDynamic dynamic.DefinitionFullName |> shouldEqual true

        (loaded.TryByDefinitionName dynamic.DefinitionFullName).IsSome
        |> shouldEqual true

        (loaded.DefinitionNamesInLoadOrder |> List.ofSeq)
        |> shouldEqual [ dynamic.DefinitionFullName ]

        // An image registered the ordinary way under the same kind of identity is found by the same
        // reference; that is the fallback the dynamic one must not take.
        let image = dynamicImage "Image"
        let withImage = loaded.WithLoadedAssembly image
        withImage.IsDynamic image.DefinitionFullName |> shouldEqual false

        (withImage.TryResolveReference (referenceNaming image)).IsSome
        |> shouldEqual true

        (withImage.TryResolveReference (referenceNaming dynamic)).IsNone
        |> shouldEqual true

    [<Test>]
    let ``a dynamic assembly holds the scope its image was read from`` () : unit =
        let scope = dynamicScope "Scoped"
        let loaded, dynamic = registerDynamic scope LoadedAssemblies.empty

        match loaded.TryDynamicScope dynamic.DefinitionFullName with
        | None -> Assert.Fail "Expected the dynamic assembly's scope"
        | Some held -> Object.ReferenceEquals (held, scope) |> shouldEqual true

        Object.ReferenceEquals (loaded.ByDefinitionName dynamic.DefinitionFullName, dynamic)
        |> shouldEqual true

        dynamic.Name.Name |> shouldEqual "Scoped"
        dynamic.ScopeName |> shouldEqual DynamicAssemblyImage.manifestModuleName

        // Only a dynamic assembly has a scope.
        let image = dynamicImage "Image"
        let withImage = loaded.WithLoadedAssembly image
        withImage.TryDynamicScope image.DefinitionFullName |> shouldEqual None

    [<Test>]
    let ``a scope with no Assembly row is refused`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let scope = EmitScope.create DynamicAssemblyImage.manifestModuleName Guid.Empty

        let thrown =
            try
                LoadedAssemblies.empty.WithDynamicAssembly loggerFactory scope |> ignore
                None
            with e ->
                Some e.Message

        match thrown with
        | None -> Assert.Fail "Expected a scope with no Assembly row to be refused"
        | Some message -> message |> shouldContainText "Assembly row"

    [<Test>]
    let ``a dynamic assembly cannot share an identity already loaded`` () : unit =
        if pool.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let _, loggerFactory = LoggerFactory.makeTest ()
        let loaded, dynamic = registerDynamic (dynamicScope "Twin") LoadedAssemblies.empty

        match loaded.WithDynamicAssembly loggerFactory (dynamicScope "Twin") with
        | Ok _ -> Assert.Fail "Expected a second dynamic assembly named Twin to be refused"
        | Error existing -> Object.ReferenceEquals (existing, dynamic) |> shouldEqual true

        let image = pool.Value.[0]
        let loaded = loaded.WithLoadedAssembly image

        // Identical to the image in everything but its module version ID and contents.
        let shadow =
            DynamicAssemblyImage.scope
                {
                    SimpleName = image.Name.Name
                    Version = image.Name.Version
                    Culture = image.Name.CultureName
                    PublicKey = image.PublicKey
                    Flags = image.Flags
                    HashAlgorithm = image.HashAlgorithm
                }
                Guid.Empty

        match loaded.WithDynamicAssembly loggerFactory shadow with
        | Ok _ -> Assert.Fail $"Expected a dynamic assembly named %s{image.DefinitionFullName} to be refused"
        | Error existing -> Object.ReferenceEquals (existing, image) |> shouldEqual true

    [<Test>]
    let ``no reference binds to a dynamic assembly`` () : unit =
        if references.Value.Length = 0 then
            Assert.Ignore "No shared-framework assemblies found next to corelib"

        let loaded, dynamic =
            registerDynamic (dynamicScope "Unbindable") LoadedAssemblies.empty

        let thrown =
            try
                loaded.WithBoundReference (referenceNaming dynamic) dynamic |> ignore
                None
            with e ->
                Some e.Message

        match thrown with
        | None -> Assert.Fail "Expected binding a reference to a dynamic assembly to be refused"
        | Some message -> message |> shouldContainText "never binds a reference to a dynamic assembly"

    let private thrownBy (f : unit -> 'a) : string option =
        try
            f () |> ignore<'a>
            None
        with e ->
            Some e.Message

    [<Test>]
    let ``replacing a dynamic assembly's scope by an extension of it swaps in the new image`` () : unit =
        let property (mvid : Guid) (ops : EmitScopeGen.Op list) (extra : EmitScopeGen.Op list) : unit =
            let _, loggerFactory = LoggerFactory.makeTest ()
            let before = EmitScopeGen.apply mvid ops
            let after = EmitScopeGen.apply mvid (ops @ extra)

            let other = dynamicImage "Other"
            let loaded, original = registerDynamic before LoadedAssemblies.empty
            let loaded = loaded.WithLoadedAssembly other
            let name = original.DefinitionFullName

            let replaced, assembly = loaded.WithEmitScope loggerFactory name after

            assembly.DefinitionFullName |> shouldEqual name

            Object.ReferenceEquals (replaced.ByDefinitionName name, assembly)
            |> shouldEqual true

            replaced.IsDynamic name |> shouldEqual true

            match replaced.TryDynamicScope name with
            | None -> Assert.Fail "Expected the replaced assembly's scope"
            | Some held -> Object.ReferenceEquals (held, after) |> shouldEqual true

            replaced.DefinitionNamesInLoadOrder
            |> List.ofSeq
            |> shouldEqual (List.ofSeq loaded.DefinitionNamesInLoadOrder)

            assembly.TypeDefs.Count |> shouldEqual (EmitScope.typeDefs after).Count
            assembly.Methods.Count |> shouldEqual (EmitScope.methodDefs after).Count
            assembly.Fields.Count |> shouldEqual (EmitScope.fields after).Count
            assembly.TypeRefs.Count |> shouldEqual (EmitScope.typeRefs after).Count
            assembly.Members.Count |> shouldEqual (EmitScope.memberRefs after).Count

            assembly.AssemblyReferences.Count
            |> shouldEqual (EmitScope.assemblyRefs after).Count

            // The other assembly, and the old registration, are untouched.
            Object.ReferenceEquals (replaced.ByDefinitionName other.DefinitionFullName, other)
            |> shouldEqual true

            Object.ReferenceEquals (loaded.ByDefinitionName name, original)
            |> shouldEqual true

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 50,
            Prop.forAll
                (Arb.fromGen (Gen.zip3 EmitScopeGen.genGuid EmitScopeGen.genOps EmitScopeGen.genOps))
                (fun (mvid, ops, extra) -> property mvid ops extra)
        )

    [<Test>]
    let ``replacing a dynamic assembly's scope by one that rewrites a row is refused`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let loaded, dynamic =
            registerDynamic (dynamicScope "Rewritten") LoadedAssemblies.empty

        let rewritten =
            DynamicAssemblyImage.scope
                { dynamicParts "Rewritten" with
                    Flags = AssemblyFlags.Retargetable
                }
                Guid.Empty

        match thrownBy (fun () -> loaded.WithEmitScope loggerFactory dynamic.DefinitionFullName rewritten) with
        | None -> Assert.Fail "Expected a scope that rewrites the Assembly row to be refused"
        | Some message -> message |> shouldContainText "RowChanged"

    [<Test>]
    let ``only a dynamic assembly's scope can be replaced`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let image = dynamicImage "Image"
        let loaded = LoadedAssemblies.empty.WithLoadedAssembly image

        match
            thrownBy (fun () -> loaded.WithEmitScope loggerFactory image.DefinitionFullName (dynamicScope "Image"))
        with
        | None -> Assert.Fail "Expected replacing the scope of an image from disk to be refused"
        | Some message -> message |> shouldContainText "not a dynamic assembly"

    /// A public class `name` with no members, in the global namespace.
    let private withType (name : string) (scope : EmitScope) : TypeDefRid * EmitScope =
        let name, scope = EmitScope.internString name scope
        let ns, scope = EmitScope.internString "" scope

        EmitScope.addTypeDef
            {
                Flags = TypeAttributes.Public ||| TypeAttributes.Class
                Name = name
                Namespace = ns
                Extends = None
            }
            scope

    /// An instance method `void M()` of `owner`, whose body is `ret`.
    let private withMethod (owner : TypeDefRid) (scope : EmitScope) : EmitScope =
        let name, scope = EmitScope.internString "M" scope

        let signature, scope =
            EmitScope.internBlob (ImmutableArray.Create<byte> [| 0x20uy ; 0uy ; 0x01uy |]) scope

        EmitScope.addMethodDef
            {
                Owner = owner
                // A tiny header (ECMA-335 II.25.4.2) for one byte of code, then `ret`.
                Body = Some (ImmutableArray.Create<byte> [| 0x06uy ; 0x2Auy |])
                ImplFlags = MethodImplAttributes.IL
                Flags = MethodAttributes.Public ||| MethodAttributes.HideBySig
                Name = name
                Signature = signature
            }
            scope
        |> snd

    [<Test>]
    let ``a created type gains no members`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let first, scope = dynamicScope "Creating" |> withType "First"
        let second, scope = withType "Second" scope
        let loaded, dynamic = registerDynamic scope LoadedAssemblies.empty
        let name = dynamic.DefinitionFullName

        loaded.IsCreatedType name first |> shouldEqual false
        let loaded = loaded.WithCreatedType name first
        loaded.IsCreatedType name first |> shouldEqual true
        loaded.IsCreatedType name second |> shouldEqual false

        // A type not yet created still gains members.
        let withSecondMethod = withMethod second scope
        let loaded, assembly = loaded.WithEmitScope loggerFactory name withSecondMethod
        assembly.Methods.Count |> shouldEqual 1
        loaded.IsCreatedType name first |> shouldEqual true

        match thrownBy (fun () -> loaded.WithEmitScope loggerFactory name (withMethod first withSecondMethod)) with
        | None -> Assert.Fail "Expected a method added to a created type to be refused"
        | Some message -> message |> shouldContainText "AddedToCreatedType"

    [<Test>]
    let ``a type is created once, and only a type the scope has`` () : unit =
        let first, scope = dynamicScope "CreatedOnce" |> withType "First"
        let loaded, dynamic = registerDynamic scope LoadedAssemblies.empty
        let name = dynamic.DefinitionFullName
        let loaded = loaded.WithCreatedType name first

        match thrownBy (fun () -> loaded.WithCreatedType name first) with
        | None -> Assert.Fail "Expected creating a type twice to be refused"
        | Some message -> message |> shouldContainText "already created"

        // `<Module>` is RID 1, so the scope's types are RIDs 1 and 2.
        let missing, _ = withType "Missing" scope

        match thrownBy (fun () -> loaded.WithCreatedType name missing) with
        | None -> Assert.Fail "Expected creating a type the scope does not have to be refused"
        | Some message -> message |> shouldContainText "not in the scope"

        let image = dynamicImage "Image"

        match thrownBy (fun () -> (loaded.WithLoadedAssembly image).WithCreatedType image.DefinitionFullName first) with
        | None -> Assert.Fail "Expected creating a type of an image from disk to be refused"
        | Some message -> message |> shouldContainText "not a dynamic assembly"
