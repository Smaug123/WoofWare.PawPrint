namespace WoofWare.PawPrint.Test

open System
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
module TestLoadedAssemblies =

    let private corelibPath : string = typeof<obj>.Assembly.Location
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
