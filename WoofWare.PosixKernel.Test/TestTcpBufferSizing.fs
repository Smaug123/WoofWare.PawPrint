namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The buffer sizes a new loopback TCP connection gets, held to the C section
/// of `tcp-transfer.c` (docs/plans/2026-10-07-tcp-byte-transfer), which
/// filled a connection under several buffer configurations on Linux 6.18.5
/// and Darwin 27.0 and read `SO_SNDBUF` and `SO_RCVBUF` before and after.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestTcpBufferSizing =

    /// One C row: the configuration's name, and the writer's `SO_SNDBUF` and
    /// the accepted reader's `SO_RCVBUF`, each before and after the fill.
    type private Row =
        {
            Config : string
            SendBefore : int
            SendAfter : int
            ReceiveBefore : int
            ReceiveAfter : int
        }

    let private sizesPattern : Regex =
        Regex @"writer SO_SNDBUF (\d+)->(\d+), reader SO_RCVBUF (\d+)->(\d+)"

    let private rowsOf (flavour : SimulatedUnixFlavour) : Row list =
        let resource =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "WoofWare.PosixKernel.Test.tcpTransfer.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "WoofWare.PosixKernel.Test.tcpTransfer.darwin.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split '\n'
        |> Array.toList
        |> List.choose (fun line ->
            match line.TrimEnd('\r').Split '\t' |> Array.toList with
            | "C" :: config :: _ ->
                let m = sizesPattern.Match line

                if not m.Success then
                    failwith $"no buffer sizes in %s{line}"

                Some
                    {
                        Config = config
                        SendBefore = int m.Groups.[1].Value
                        SendAfter = int m.Groups.[2].Value
                        ReceiveBefore = int m.Groups.[3].Value
                        ReceiveAfter = int m.Groups.[4].Value
                    }
            | _ -> None
        )

    /// The listener's `SO_RCVBUF` the configuration named, if it set one.
    let private listenerReceiveBuffer (config : string) : int option =
        config.Split ','
        |> Array.tryPick (fun part ->
            if part.StartsWith ("rcvbuf=", StringComparison.Ordinal) then
                Some (int (part.Substring 7))
            else
                None
        )

    let private machine (platform : SimulatedUnixPlatform) : UnixMachineState =
        (UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
         |> UnixBootImage.boot)
            .Machine

    [<Test>]
    let ``Darwin's handshake leaves the accepted end the receive buffer XNU's rule derives, in every configuration measured``
        ()
        =
        let rows = rowsOf SimulatedUnixFlavour.Darwin

        rows
        |> List.map (fun row -> row.Config)
        |> List.distinct
        |> List.length
        |> shouldEqual 12

        let disagreements =
            rows
            |> List.choose (fun row ->
                let initial =
                    listenerReceiveBuffer row.Config
                    |> Option.defaultValue (UnixMachineState.defaultTcpReceiveSpace SimulatedUnixFlavour.Darwin)

                let derived = TcpBufferSizing.darwinHandshakeReceiveBuffer initial SocketDomain.Inet

                if derived = row.ReceiveBefore then
                    None
                else
                    Some $"%s{row.Config}: derived %d{derived}, measured %d{row.ReceiveBefore}"
            )

        disagreements |> shouldEqual []

    [<Test>]
    let ``a default Darwin connection's buffers are 146988 to send and 408300 to receive over IPv4`` () =
        let transfer =
            TcpBufferSizing.newTransfer SocketDomain.Inet (machine SimulatedUnixPlatform.macOsArm64)

        for e in [ ConnectionEnd.Client ; ConnectionEnd.Server ] do
            let direction = TcpTransfer.towards e transfer
            direction.SendCapacity |> shouldEqual 146988
            direction.ReceiveCapacity |> shouldEqual 408300

        // The measured default rows, whose buffers did not grow during the fill.
        rowsOf SimulatedUnixFlavour.Darwin
        |> List.filter (fun row -> row.Config = "default")
        |> List.iter (fun row ->
            row.SendBefore |> shouldEqual 146988
            row.ReceiveBefore |> shouldEqual 408300
            row.ReceiveAfter |> shouldEqual 408300
        )

        let ipv6 =
            TcpBufferSizing.newTransfer SocketDomain.Inet6 (machine SimulatedUnixPlatform.macOsArm64)

        (TcpTransfer.towards ConnectionEnd.Client ipv6).SendCapacity
        |> shouldEqual 146808

        (TcpTransfer.towards ConnectionEnd.Client ipv6).ReceiveCapacity
        |> shouldEqual 407800

    [<Test>]
    let ``a default Linux connection's buffers are tcp_wmem's maximum to send and tcp_rmem's default to receive`` () =
        let transfer =
            TcpBufferSizing.newTransfer SocketDomain.Inet (machine SimulatedUnixPlatform.linuxX64)

        let rows =
            rowsOf SimulatedUnixFlavour.Linux
            |> List.filter (fun row -> row.Config = "default")

        rows |> List.isEmpty |> shouldEqual false

        for e in [ ConnectionEnd.Client ; ConnectionEnd.Server ] do
            let direction = TcpTransfer.towards e transfer
            // What every fill but the one-byte writes autotuned the send buffer to.
            direction.SendCapacity
            |> shouldEqual (rows |> List.map (fun row -> row.SendAfter) |> List.max)

            for row in rows do
                direction.ReceiveCapacity |> shouldEqual row.ReceiveBefore

    [<Test>]
    let ``withTcpReceiveSpace admits on Darwin exactly the sizes whose buffer the handshake settles`` () =
        let darwin =
            UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let admitted (value : int) : bool =
            match darwin |> UnixBootImage.withTcpReceiveSpace (Some value) with
            | Ok image ->
                (UnixBootImage.boot image).Machine.TcpReceiveSpace |> shouldEqual value
                true
            | Error (TcpReceiveSpaceRefusal.GrowsAfterHandshake (refused, _)) ->
                refused |> shouldEqual value
                false
            | Error refusal -> failwith $"unexpected refusal: %s{TcpReceiveSpaceRefusal.describe refusal}"

        for value in [ 49152 ; 65536 ; 131072 ; 244679 ] do
            admitted value |> shouldEqual true

        // Below the route's receive pipe; above fifteen segments; whole
        // numbers of an IPv4 and of an IPv6 segment.
        for value in [ 49151 ; 244681 ; 262144 ; 9 * 16332 ; 9 * 16312 ; 15 * 16312 ] do
            admitted value |> shouldEqual false

        // Every admitted size gives a buffer that does not grow again.
        for value in 49152..244680 do
            if TcpBufferSizing.darwinReceiveSpaceRefusal value |> Option.isNone then
                for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ] do
                    TcpBufferSizing.darwinReceiveBuffer value domain |> ignore<int>

    [<Test>]
    let ``the TCP buffer sysctls default per flavour and refuse what nothing reads`` () =
        let image (platform : SimulatedUnixPlatform) =
            UnixSystem.initial<int, string> platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let linux = image SimulatedUnixPlatform.linuxX64
        let darwin = image SimulatedUnixPlatform.macOsArm64

        (UnixBootImage.boot linux).Machine.TcpReceiveSpace |> shouldEqual 131072
        (UnixBootImage.boot linux).Machine.TcpSendSpaceMax |> shouldEqual 4194304
        (UnixBootImage.boot darwin).Machine.TcpReceiveSpace |> shouldEqual 131072

        (linux
         |> UnixBootImage.withTcpReceiveSpace (Some 1)
         |> Configured.expectOk TcpReceiveSpaceRefusal.describe
         |> UnixBootImage.boot)
            .Machine.TcpReceiveSpace
        |> shouldEqual 1

        (linux
         |> UnixBootImage.withTcpSendSpaceMax (Some 65536)
         |> Configured.expectOk TcpSendSpaceMaxRefusal.describe
         |> UnixBootImage.boot)
            .Machine.TcpSendSpaceMax
        |> shouldEqual 65536

        (linux
         |> UnixBootImage.withTcpSendSpaceMax (Some 65536)
         |> Configured.expectOk TcpSendSpaceMaxRefusal.describe
         |> UnixBootImage.withTcpSendSpaceMax None
         |> Configured.expectOk TcpSendSpaceMaxRefusal.describe
         |> UnixBootImage.boot)
            .Machine.TcpSendSpaceMax
        |> shouldEqual 4194304

        match UnixBootImage.withTcpReceiveSpace (Some 0) linux with
        | Error refusal -> refusal |> shouldEqual (TcpReceiveSpaceRefusal.NotPositive 0)
        | Ok _ -> failwith "a non-positive Linux receive space was admitted"

        match UnixBootImage.withTcpSendSpaceMax (Some 0) linux with
        | Error refusal -> refusal |> shouldEqual (TcpSendSpaceMaxRefusal.NotPositive 0)
        | Ok _ -> failwith "a non-positive Linux send-space maximum was admitted"

        match UnixBootImage.withTcpSendSpaceMax (Some 4194304) darwin with
        | Error refusal ->
            refusal
            |> shouldEqual (TcpSendSpaceMaxRefusal.NotReadOn (SimulatedUnixFlavour.Darwin, 4194304))
        | Ok _ -> failwith "a Darwin send-space maximum, which nothing reads, was admitted"
