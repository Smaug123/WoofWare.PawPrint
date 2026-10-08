namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What `getsockname(2)` and `accept(2)` copy out to a caller's buffer, and the
/// length they write back to its cell.
///
/// `sockaddr-decoding.c` (section T) measured both on Linux 6.18.5 and Darwin
/// 27.0.0 at every declared length 0..20: the bytes written are the first
/// `min(declared, 16)` bytes of the address, and the cell gets 16. Every row is
/// replayed here, and a property states the rule over the whole domain.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCopyOutMeasured =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform
        |> UnixBootImage.withLocalAddresses UnixSystem.defaultLocalAddresses []
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private stream (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

    /// A listener at 127.0.0.1:5000 and a client connected to it, with the
    /// connection queued: the listener's descriptor, the client's, and the
    /// system.
    let private connected (platform : SimulatedUnixPlatform) : int * int * UnixSystem<int, string> =
        let listener, system = stream (systemOn platform)

        let system =
            match CopyIn.bind listener UserBuffer.Mapped 16u (CopyIn.inet platform (loopback 5000us)) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"bind answered %A{other}"

        let system =
            match UnixSocket.listen listener 8 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listen answered %A{other}"

        let client, system = stream system

        match CopyIn.connect client UserBuffer.Mapped 16u (CopyIn.inet platform (loopback 5000us)) system with
        | Ok (ConnectOutcome.Completed, system) -> listener, client, system
        | other -> failwith $"connect answered %A{other}"

    /// One call's copy-out under this kernel, made on the client for
    /// `getsockname` and on the listener for `accept`: the bytes written, the
    /// length reported, and the client's address, which both calls report.
    let private copyOut
        (platform : SimulatedUnixPlatform)
        (call : string)
        (declared : uint32)
        : (ImmutableArray<byte> * int * InternetEndpoint) option
        =
        let listener, client, system = connected platform

        match call with
        | "getsockname" ->
            match UnixSocket.getsockname client UserBuffer.Mapped declared system with
            | Ok (GetSockNameAnswer.Reported (copiedOut, reported)) ->
                Some (copiedOut, reported, local (socketOf client system))
            | Ok (GetSockNameAnswer.Failed _) -> None
            | Error refusal -> failwith $"getsockname refused: %O{refusal}"
        | "accept" ->
            match UnixConnection.accept 0 listener UserBuffer.Mapped declared system with
            | Ok (AcceptOutcome.Accepted (_, copiedOut, reported), _) ->
                Some (copiedOut, reported, local (socketOf client system))
            | Ok _ -> None
            | Error refusal -> failwith $"accept refused: %O{refusal}"
        | other -> failwith $"no such call %s{other}"

    let private replay (flavour : string) (platform : SimulatedUnixPlatform) : unit =
        let row =
            Regex
                @"^T (?<call>\w+)\s+declared=(?<declared>\d+)\s+OK cell=(?<cell>\d+)\s+bytes=(?<bytes>[0-9a-f]+) written=(?<written>\d+) prefix-of-full=yes$"

        let rows =
            resource $"sockaddr-decoding.%s{flavour}.txt"
            |> Array.toList
            |> List.filter (fun line -> line.StartsWith "T ")

        // getsockname, getpeername and accept at 0..20 each.
        rows |> List.length |> shouldEqual 63

        let mutable replayed = 0

        for line in rows do
            let m = row.Match line

            if not m.Success then
                failwith $"unparsed probe line: %s{line}"

            let call = m.Groups.["call"].Value
            let declared = uint32 m.Groups.["declared"].Value

            // This library has no `getpeername`.
            if call <> "getpeername" then
                match copyOut platform call declared with
                | None -> failwith $"%s{line}: this kernel failed the call"
                | Some (copiedOut, reported, _) ->
                    copiedOut.Length |> shouldEqual (int m.Groups.["written"].Value)

                    reported |> shouldEqual (int m.Groups.["cell"].Value)

                    // Every byte the probe saw written, but the port's, which is
                    // the ephemeral one each kernel happened to choose.
                    let measured = Convert.FromHexString m.Groups.["bytes"].Value

                    for i in 0 .. copiedOut.Length - 1 do
                        if i <> 2 && i <> 3 then
                            if copiedOut.[i] <> measured.[i] then
                                failwith
                                    $"%s{line}: byte %d{i} is 0x%02x{copiedOut.[i]} here and 0x%02x{measured.[i]} measured"

                    replayed <- replayed + 1

        replayed |> shouldEqual 42

    [<Test>]
    let ``every Linux copy-out row is answered as measured`` () : unit =
        replay "linux" SimulatedUnixPlatform.linuxX64

    [<Test>]
    let ``every Darwin copy-out row is answered as measured`` () : unit =
        replay "darwin" SimulatedUnixPlatform.macOsArm64

    /// For every platform, call and declared length: the bytes written are the
    /// first `min(declared, 16)` bytes of the socket's `struct sockaddr_in`, laid
    /// out independently of this kernel's encoder, and 16 is reported. Linux
    /// fails a negative length instead.
    [<Test>]
    let ``what is copied out is a prefix of the address, and 16 is reported`` () : unit =
        let gen =
            gen {
                let! platform = Gen.elements platforms
                let! call = Gen.elements [ "getsockname" ; "accept" ]

                let! declared =
                    Gen.frequency
                        [
                            8, Gen.choose (0, 300) |> Gen.map uint32
                            1, Gen.elements [ 0x7FFFFFFFu ; 0x80000000u ; 0xFFFFFFFFu ]
                        ]

                return platform, call, declared
            }

        let property (platform : SimulatedUnixPlatform, call : string, declared : uint32) : unit =
            let negativeOnLinux =
                SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
                && int declared < 0

            match copyOut platform call declared with
            | None -> negativeOnLinux |> shouldEqual true
            | Some (copiedOut, reported, endpoint) ->
                negativeOnLinux |> shouldEqual false
                copiedOut |> shouldEqual (CopyOut.expected platform endpoint declared)
                reported |> shouldEqual 16

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)
