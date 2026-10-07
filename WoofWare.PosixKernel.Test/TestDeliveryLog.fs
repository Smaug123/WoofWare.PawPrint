namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `DeliveryLog` answers every question a client can ask of it exactly as the
/// array it replaced did, appended to one delivery at a time: the count, the
/// deliveries after a given count, the whole log oldest first, and equality.
///
/// The oracle is an `ImmutableArray<Delivery>` built by `Add`, which is what the
/// machine held before appending was made constant-time.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDeliveryLog =

    /// Few endpoints and short payloads over a small alphabet, so that two
    /// generated deliveries are often equal and the equality property sees both
    /// verdicts.
    let private deliveryGen : Gen<Delivery> =
        gen {
            let! fd = Gen.elements [ 1 ; 2 ]
            let! bytes = Gen.listOf (Gen.elements [ 0uy ; 1uy ]) |> Gen.resize 3

            return
                {
                    Endpoint = ExternalEndpoint (UnixSystem.defaultProcessId, fd)
                    Bytes = ImmutableArray.CreateRange bytes
                }
        }

    /// The log and the reference agree on every question, given that both were
    /// built from the same appends.
    let private agree (log : DeliveryLog) (reference : ImmutableArray<Delivery>) : unit =
        DeliveryLog.count log |> shouldEqual reference.Length
        DeliveryLog.toList log |> shouldEqual (List.ofSeq reference)

        for count in 0 .. reference.Length do
            DeliveryLog.since count log
            |> shouldEqual (reference |> Seq.skip count |> List.ofSeq)

    [<Test>]
    let ``the empty log has no deliveries`` () : unit =
        agree DeliveryLog.empty ImmutableArray.Empty

    [<Test>]
    let ``since refuses a count the log does not reach`` () : unit =
        let log =
            DeliveryLog.empty
            |> DeliveryLog.append
                {
                    Endpoint = ExternalEndpoint (UnixSystem.defaultProcessId, 1)
                    Bytes = ImmutableArray.Create<byte> 0x41uy
                }

        Assert.Throws<exn> (fun () -> DeliveryLog.since 2 log |> ignore<Delivery list>)
        |> ignore<exn>

        Assert.Throws<exn> (fun () -> DeliveryLog.since -1 log |> ignore<Delivery list>)
        |> ignore<exn>

    [<Test>]
    let ``a log answers as the array appended to does, after every append`` () : unit =
        let property (deliveries : Delivery list) : unit =
            ((DeliveryLog.empty, ImmutableArray<Delivery>.Empty), deliveries)
            ||> List.fold (fun (log, reference) delivery ->
                let log = DeliveryLog.append delivery log
                let reference = reference.Add delivery
                agree log reference
                log, reference
            )
            |> ignore<DeliveryLog * ImmutableArray<Delivery>>

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll (Arb.fromGen (Gen.listOf deliveryGen |> Gen.resize 40)) property
        )

    [<Test>]
    let ``two logs are equal exactly when their deliveries are, oldest first`` () : unit =
        let build (deliveries : Delivery list) : DeliveryLog =
            (DeliveryLog.empty, deliveries)
            ||> List.fold (fun log d -> DeliveryLog.append d log)

        let property (left : Delivery list, right : Delivery list) : unit =
            let leftLog = build left
            let rightLog = build right
            let leftReference = ImmutableArray.CreateRange left
            let rightReference = ImmutableArray.CreateRange right

            (leftLog = rightLog) |> shouldEqual (leftReference = rightReference)

            if leftLog = rightLog then
                hash leftLog |> shouldEqual (hash rightLog)

        let deliveries = Gen.listOf deliveryGen |> Gen.resize 3

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll (Arb.fromGen (Gen.zip deliveries deliveries)) property
        )

    [<Test>]
    let ``the machine's log after any sequence of writes is the reference's`` () : unit =
        let property (platform : SimulatedUnixPlatform, writes : (int * int) list) : unit =
            let initial : UnixSystem<int, string> =
                UnixSystem.initial platform
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let final, reference =
                ((initial, ImmutableArray<Delivery>.Empty), List.indexed writes)
                ||> List.fold (fun (system, reference) (index, (fd, count)) ->
                    let bytes =
                        ImmutableArray.Create<byte> (Array.init count (fun i -> byte (index + i)))

                    match WriteOutcomes.write fd bytes system with
                    | Ok (WriteAnswer.Completed written, after) ->
                        written |> shouldEqual (int64 count)

                        // A write that moved no bytes delivers nothing.
                        let reference =
                            if count = 0 then
                                reference
                            else
                                reference.Add
                                    {
                                        Endpoint = ExternalEndpoint (UnixSystem.processId after, fd)
                                        Bytes = bytes
                                    }

                        agree after.Machine.Delivered reference
                        after, reference
                    | other -> failwith $"write(%d{fd}, %d{count} bytes) answered %A{Result.map fst other}"
                )

            agree final.Machine.Delivered reference

        let gen =
            Gen.zip
                (Gen.elements
                    [
                        SimulatedUnixPlatform.linuxX64
                        SimulatedUnixPlatform.linuxArm64
                        SimulatedUnixPlatform.macOsArm64
                    ])
                (Gen.listOf (Gen.zip (Gen.elements [ 1 ; 2 ]) (Gen.choose (0, 300)))
                 |> Gen.resize 30)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)
