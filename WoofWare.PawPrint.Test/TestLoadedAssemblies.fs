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
/// reference identity. These tests pin its laws directly, so the whole class of ref/def confusion
/// is covered rather than the single facade chain that happened to be reported.
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

    /// Real assemblies from the pinned shared framework, which is where genuinely mismatching
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

    /// The invariant the fix exists to guarantee, checked after every single operation.
    let private assertEveryAssemblyFindableByItsOwnName (real : LoadedAssemblies) (model : Model) : unit =
        for _, assy in model.ByDefinition do
            match real.TryByDefinition assy.Name with
            | None ->
                Assert.Fail
                    $"Assembly %s{assy.Name.FullName} is in the load context but is not findable by its own definition identity"
            | Some found -> found.Name.FullName |> shouldEqual assy.Name.FullName

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

        // A genuinely distinct instance of the same assembly must not displace the one already
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

        match third.TryByDefinition assy.Name with
        | None -> Assert.Fail "Expected the assembly to remain findable by its definition identity"
        | Some held -> Object.ReferenceEquals (held, assy) |> shouldEqual true
