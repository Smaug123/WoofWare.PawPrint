namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
module TestProcessId =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    /// Every `int32`, with the boundaries weighted in: a uniform draw almost
    /// never lands on 0 or 1, which are where an off-by-one in `parse` would be.
    let private candidateGen : Gen<int32> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<int32>
                Gen.elements [ Int32.MinValue ; -1 ; 0 ; 1 ; 2 ; Int32.MaxValue ]
            ]

    [<Test>]
    let ``parse accepts exactly the positive numbers, and gives them back unchanged`` () : unit =
        let property (candidate : int32) : unit =
            match ProcessId.parse candidate with
            | Some pid ->
                candidate |> shouldBeGreaterThan 0
                ProcessId.toInt32 pid |> shouldEqual candidate
            | None -> candidate |> shouldBeSmallerThan 1

        Check.One (config, Prop.forAll (Arb.fromGen candidateGen) property)

    [<Test>]
    let ``parseOrFail agrees with parse, naming its context when it refuses`` () : unit =
        let property (candidate : int32) : unit =
            match ProcessId.parse candidate with
            | Some pid -> ProcessId.parseOrFail "ctx" candidate |> shouldEqual pid
            | None ->
                let exn =
                    Assert.Throws<Exception> (fun () -> ProcessId.parseOrFail "ctx" candidate |> ignore<ProcessId>)

                exn.Message.StartsWith ("ctx: ", StringComparison.Ordinal) |> shouldEqual true

        Check.One (config, Prop.forAll (Arb.fromGen candidateGen) property)

    [<Test>]
    let ``assertValid refuses the forged default and passes every parsed ID through`` () : unit =
        // A struct DU has a default value that no smart constructor produced, and
        // `Array.zeroCreate` or an uninitialised record field will hand one out.
        Assert.Throws<Exception> (fun () ->
            ProcessId.assertValid "ctx" Unchecked.defaultof<ProcessId> |> ignore<ProcessId>
        )
        |> ignore<Exception>

        let property (candidate : int32) : unit =
            match ProcessId.parse candidate with
            | None -> ()
            | Some pid -> ProcessId.assertValid "ctx" pid |> shouldEqual pid

        Check.One (config, Prop.forAll (Arb.fromGen candidateGen) property)

    [<Test>]
    let ``the default process ID is not init, and is not any other default ID`` () : unit =
        let pid = ProcessId.toInt32 UnixSystem.defaultProcessId
        pid |> shouldNotEqual 1

        // So that a caller reading the uid or gid where it meant the pid cannot
        // pass a test that uses the defaults.
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            int64 pid
            |> shouldNotEqual (int64 (UserId.toUInt32 (UnixSystem.defaultUserId flavour)))

            int64 pid
            |> shouldNotEqual (int64 (GroupId.toUInt32 (UnixSystem.defaultGroupId flavour)))
