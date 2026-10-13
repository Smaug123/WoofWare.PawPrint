namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The paused-call protocol as a client meets it: which bytes a paused call
/// asks for, what resuming it with the wrong bytes does, and the calls that
/// answer without stopping because they copy nothing.
///
/// What each call answers is held to the internal steps it is made of by the
/// helpers every other test writes through (`WriteOutcomes`, `CopyIn`,
/// `SockOptCalls`); this fixture is about the protocol's own promises.
[<TestFixture>]
module TestCallProgress =

    /// What a write under the property below came to.
    [<RequireQualifiedAccess>]
    type private Reached =
        /// Answered before the copy.
        | Answered
        /// Stopped at the copy, asking for every byte.
        | PausedWhole
        /// Stopped at the copy, asking for fewer bytes than the write was of.
        | PausedShort

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private payload (count : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init count (fun i -> byte (i % 251)))

    let private nonBlocking (fd : int) (value : bool) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        snd (UnixDescriptor.setNonBlocking fd value system)

    /// A pipe on `platform`, `prefill` bytes into it as far as it takes them
    /// without blocking, and its write end blocking or not as `writeNonBlocking`
    /// says: the write end and the system.
    let private pipeWith
        (platform : SimulatedUnixPlatform)
        (prefill : int)
        (writeNonBlocking : bool)
        : int * UnixSystem<int, string>
        =
        let writeFd, system =
            match UnixPipe.tryPipe2 0 UserBuffer.Mapped (systemOn platform) with
            | Ok (Pipe2Answer.Created (_, writeFd), system) -> writeFd, system
            | other -> failwith $"pipe2 did not make a pipe: %A{other}"

        let filled =
            match
                WriteOutcomes.admitThenWrite
                    0
                    writeFd
                    UserBuffer.Mapped
                    (payload prefill)
                    (nonBlocking writeFd true system)
            with
            | Ok (WriteStep.Returns (_, system)) -> system
            | other -> failwith $"filling the pipe with %d{prefill} bytes answered %A{other}"

        writeFd, nonBlocking writeFd writeNonBlocking filled

    [<Test>]
    let ``a paused write asks for bytes from the start of its buffer, never more than it was given, and resumes as a value``
        ()
        : unit
        =
        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! prefill = Gen.frequency [ 2, Gen.constant 0 ; 3, Gen.choose (0, 80000) ]
                let! writeNonBlocking = Gen.elements [ true ; false ]

                let! count = Gen.frequency [ 1, Gen.constant 0 ; 3, Gen.choose (1, 100) ; 3, Gen.choose (1, 140000) ]

                return platform, prefill, writeNonBlocking, count
            }

        let property
            (cover : Reached -> unit)
            ((platform, prefill, writeNonBlocking, count) : SimulatedUnixPlatform * int * bool * int)
            : unit
            =
            let writeFd, system = pipeWith platform prefill writeNonBlocking

            match UnixReadWrite.write 0 writeFd UserBuffer.Mapped (uint64 count) system with
            | CallProgress.Answered _ -> cover Reached.Answered
            | CallProgress.NeedsCopyIn paused ->
                let range = PausedWrite.copyRange paused
                range.FirstByte |> shouldEqual 0
                (range.ByteCount > 0 && range.ByteCount <= count) |> shouldEqual true

                cover (
                    if range.ByteCount < count then
                        Reached.PausedShort
                    else
                        Reached.PausedWhole
                )

                for wrong in [ range.ByteCount - 1 ; range.ByteCount + 1 ] do
                    Assert.Throws<InvalidOperationException> (fun () ->
                        UnixReadWrite.writeWithBytes (payload wrong) paused |> ignore
                    )
                    |> ignore

                let bytes = payload range.ByteCount
                let first = UnixReadWrite.writeWithBytes bytes paused
                let second = UnixReadWrite.writeWithBytes bytes paused

                match first, second with
                | ProcessFate.Continues (outcome, after), ProcessFate.Continues (outcome', after') ->
                    outcome' |> shouldEqual outcome
                    (after' = after) |> shouldEqual true
                | first, second -> failwith $"a write into a pipe ended the process: %A{first}, %A{second}"

            // And the whole write, against the internal steps it is made of.
            WriteOutcomes.admitThenWrite 0 writeFd UserBuffer.Mapped (payload count) system
            |> ignore

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 300) (Arb.fromGen gen) property

        for reached in [ Reached.Answered ; Reached.PausedWhole ; Reached.PausedShort ] do
            (coverage.Count reached >= 20) |> shouldEqual true

    let private socketOn (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        // AF_INET and SOCK_STREAM are 2 and 1 under both flavours.
        match UnixSocket.socket SimulatedUnixPlatform.internetAddressFamily 1 0 (systemOn platform) with
        | Ok (fd, system) -> fd, system
        | Error error -> failwith $"socket failed with %O{error}"

    [<Test>]
    let ``a bind that copies no bytes of its sockaddr answers without stopping`` () : unit =
        let fd, system = socketOn SimulatedUnixPlatform.linuxX64

        match UnixSocket.bind fd UserBuffer.Mapped 0u system with
        | CallProgress.Answered (BindAnswer.Failed _, after) -> (after = system) |> shouldEqual true
        | other -> failwith $"a bind of a zero-length sockaddr answered %A{other}"

    [<Test>]
    let ``a bind resumed with other than the bytes it asked for fails loudly`` () : unit =
        let fd, system = socketOn SimulatedUnixPlatform.linuxX64

        match UnixSocket.bind fd UserBuffer.Mapped 16u system with
        | CallProgress.NeedsCopyIn paused ->
            let range = PausedBind.copyRange paused

            range
            |> shouldEqual
                {
                    FirstByte = 0
                    ByteCount = 16
                }

            for wrong in [ 0 ; 15 ; 17 ] do
                Assert.Throws<InvalidOperationException> (fun () ->
                    UnixSocket.bindWithAddress (payload wrong) paused |> ignore
                )
                |> ignore
        | other -> failwith $"a bind of a 16-byte sockaddr answered %A{other} before the copy"

    [<Test>]
    let ``a Darwin getsockopt through a null value buffer answers without reading the length cell`` () : unit =
        let platform = SimulatedUnixPlatform.macOsArm64
        let fd, system = socketOn platform

        match
            UnixSocket.getsockopt
                fd
                (SimulatedUnixPlatform.socketOptionLevel platform)
                (SimulatedUnixPlatform.reuseAddressOption platform)
                (UserBuffer.Unmapped 0UL)
                UserBuffer.Mapped
                system
        with
        | CallProgress.Answered _ -> ()
        | CallProgress.NeedsCopyIn _ -> failwith "Darwin's getsockopt stopped to read a length cell it never reads"
