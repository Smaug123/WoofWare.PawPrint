namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSocket.setNonBlocking`, `UnixSocket.isNonBlocking`, and what
/// `UnixSocket.socket` creates.
///
/// The flag's whole subtlety is *where it lives* and *what each target does
/// with it*: it is a property of the open file description rather than of the
/// descriptor, a standard stream stores it and refuses the one write it would
/// shorten, and an event port stores it while reporting a
/// failure — which is a flavour split no guest can reach, since a guest runs one
/// flavour and the managed surface never sets the flag on an event port.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNonBlocking =

    let private context : string = "TestNonBlocking"

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    /// A simulated process on the flavour asked for, before anything has
    /// happened to it.
    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)

        { system with
            Machine =
                { system.Machine with
                    LocalRoutes = []
                }
        }


    let private linux : UnixSystem<int, string> =
        systemOn SimulatedUnixPlatform.linuxX64

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private setOrFail
        (fd : int)
        (value : bool)
        (system : UnixSystem<int, string>)
        : SetNonBlockingAnswer * UnixSystem<int, string>
        =
        UnixSocket.setNonBlocking fd value system

    /// What `poll` would report for `fd` right now.
    let private readinessOf (fd : int) (system : UnixSystem<int, string>) : uint32 =
        match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
        | Some id -> LinuxReadiness.ofDescription id system
        | None -> failwith $"fd %d{fd} is not open"

    let private set (fd : int) (value : bool) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match setOrFail fd value system with
        | SetNonBlockingAnswer.Set, system -> system
        | SetNonBlockingAnswer.Failed error, _ -> failwith $"expected the flag to be set, got %O{error}"

    // ------------------------------------------------------------------
    // socket
    // ------------------------------------------------------------------

    /// The socket and its descriptor are minted together, and the identity the
    /// one mints is the identity the other names.
    [<TestCaseSource(nameof platforms)>]
    let ``a created socket and its descriptor agree`` (platform : SimulatedUnixPlatform) : unit =
        let system = systemOn platform

        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let socketId =
            match FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors with
            | Some (OpenFileTarget.Socket socketId) -> socketId
            | other -> failwith $"expected a socket target, got %A{other}"

        let socket = UnixMachineState.socket socketId system.Machine
        socket.Domain |> shouldEqual SocketDomain.Inet
        socket.Kind |> shouldEqual SocketKind.Stream
        socket.Protocol |> shouldEqual SocketProtocol.Tcp

        // A fresh socket is unbound, idle, and carries no reuse flag; all three
        // are what every later screen keys on.
        socket.Binding |> shouldEqual None
        socket.Phase |> shouldEqual SocketPhase.Idle
        socket.ReuseAddress |> shouldEqual false

        // ...and it is not born non-blocking.
        UnixSocket.isNonBlocking fd system |> shouldEqual (Some false)

    /// Each socket gets its own identity: the counter advances, so a second
    /// socket cannot overwrite the first in the table.
    [<TestCaseSource(nameof platforms)>]
    let ``each created socket gets a fresh identity`` (platform : SimulatedUnixPlatform) : unit =
        let first, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp (systemOn platform)

        let second, system =
            NewSocket.create SocketDomain.Inet SocketKind.Datagram SocketProtocol.Udp system

        first |> shouldNotEqual second
        system.Machine.Sockets |> Map.count |> shouldEqual 2

        let targetOf fd =
            FileDescriptorRegistry.tryFindTarget fd system.Process.FileDescriptors

        targetOf first |> shouldNotEqual (targetOf second)

    // ------------------------------------------------------------------
    // Where the flag lives
    // ------------------------------------------------------------------

    /// The flag is a property of the open file *description*, where POSIX keeps
    /// the status flags — so a `dup` sees it, and setting it through either
    /// number is the same act.
    [<Test>]
    let ``the flag lives on the description, so a dup shares it`` () : unit =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp linux

        let duplicate, registry =
            match FileDescriptorRegistry.dup fd system.Process.FileDescriptors with
            | Ok result -> result
            | Error error -> failwith $"could not dup: %A{error}"

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        let system = set fd true system
        UnixSocket.isNonBlocking duplicate system |> shouldEqual (Some true)

        // ...and clearing it through the *other* number clears it for both.
        let system = set duplicate false system
        UnixSocket.isNonBlocking fd system |> shouldEqual (Some false)

    [<Test>]
    let ``a descriptor that is not open has no flag and cannot be set`` () : unit =
        UnixSocket.isNonBlocking 99 linux |> shouldEqual None

        setOrFail 99 true linux
        |> fst
        |> shouldEqual (SetNonBlockingAnswer.Failed UnixError.EBADF)

    // ------------------------------------------------------------------
    // Which targets may carry it
    // ------------------------------------------------------------------

    /// A regular file and a socket both take it. Both kernels give `O_NONBLOCK`
    /// no effect on a regular file, so an operation that never looks at it is
    /// right not to — storing it is still what a real `fcntl` does.
    [<Test>]
    let ``a file and a socket both take the flag`` () : unit =
        let socketFd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp linux

        let fileFd, registry =
            FileDescriptorRegistry.openFile (InodeNumber 1L) FileAccessMode.ReadOnly system.Process.FileDescriptors

        let system =
            { system with
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }

        for fd in [ socketFd ; fileFd ] do
            let after = set fd true system
            UnixSocket.isNonBlocking fd after |> shouldEqual (Some true)

    // ------------------------------------------------------------------
    // The standard streams
    // ------------------------------------------------------------------

    // Every row in this section was measured by
    // docs/plans/2026-08-23-posix-kernel-extraction/stdio-nonblock.c, under the
    // launch shape `UnixSystem.pipedStandardStreams` describes: three distinct
    // pipes, standard input's writer closed before the process runs, and the
    // output streams read by the launcher as fast as it can. Linux 6.18.5
    // aarch64 and Darwin 27.0.0 arm64 answered every row identically.

    let private streamPlatforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// One `fcntl(F_SETFL)` on a standard stream, or on a `dup` of one: the
    /// index is into `[0; 1; 2; dup 0; dup 1; dup 2]`.
    type private StreamFlagOp =
        {
            Index : int
            Value : bool
        }

    /// A system whose descriptors 3, 4 and 5 are `dup`s of 0, 1 and 2.
    let private withStreamDuplicates (system : UnixSystem<int, string>) : int list * UnixSystem<int, string> =
        let fds, registry =
            [ 0 ; 1 ; 2 ]
            |> List.mapFold
                (fun registry fd ->
                    match FileDescriptorRegistry.dup fd registry with
                    | Ok (duplicate, registry) -> duplicate, registry
                    | Error error -> failwith $"could not dup %d{fd}: %A{error}"
                )
                system.Process.FileDescriptors

        [ 0 ; 1 ; 2 ] @ fds,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    /// Measured: `F_SETFL` takes `O_NONBLOCK` on each of the three streams and
    /// answers 0, `F_GETFL` reads it back, and it lives on the description, so a
    /// `dup` sees it and clearing it through the `dup` clears it for the
    /// original. Checked against a model holding one flag per description, over
    /// every sequence of sets and clears through either number.
    [<Test>]
    let ``the flag on a standard stream is stored on its description`` () : unit =
        let property (platform : SimulatedUnixPlatform, ops : StreamFlagOp list) : unit =
            let fds, system = withStreamDuplicates (systemOn platform)
            let readiness = fds |> List.map (fun fd -> readinessOf fd system)
            let mutable system = system
            // One flag per description: index mod 3 names it.
            let mutable model = [| false ; false ; false |]

            for op in ops do
                let answer, after = setOrFail fds.[op.Index] op.Value system
                answer |> shouldEqual SetNonBlockingAnswer.Set
                system <- after
                model.[op.Index % 3] <- op.Value

                fds
                |> List.mapi (fun index fd -> UnixSocket.isNonBlocking fd system, Some model.[index % 3])
                |> List.iter (fun (actual, expected) -> actual |> shouldEqual expected)

                UnixSystem.checkInvariants system |> shouldEqual []

                // Measured: `poll` answers the same with the flag set as clear.
                fds |> List.map (fun fd -> readinessOf fd system) |> shouldEqual readiness

        let opGen =
            Gen.zip (Gen.choose (0, 5)) (Gen.elements [ true ; false ])
            |> Gen.map (fun (index, value) ->
                {
                    Index = index
                    Value = value
                }
            )

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll
                (Arb.fromGen (Gen.zip (Gen.elements streamPlatforms) (Gen.listOf opGen |> Gen.map (List.truncate 30))))
                property
        )

    /// The rows of the property above as literals, so that it cannot agree with
    /// a store that answers something other than what was measured.
    [<Test>]
    let ``setting the flag on each standard stream answers and reads back`` () : unit =
        for platform in streamPlatforms do
            for fd in [ 0 ; 1 ; 2 ] do
                let answer, after = setOrFail fd true (systemOn platform)
                answer |> shouldEqual SetNonBlockingAnswer.Set
                UnixSocket.isNonBlocking fd after |> shouldEqual (Some true)

                // Only the description `fd` names carries it: the launch
                // shape's three streams are three descriptions.
                for other in [ 0 ; 1 ; 2 ] |> List.filter ((<>) fd) do
                    UnixSocket.isNonBlocking other after |> shouldEqual (Some false)

                let answer, after = setOrFail fd false after
                answer |> shouldEqual SetNonBlockingAnswer.Set
                UnixSocket.isNonBlocking fd after |> shouldEqual (Some false)

    let private bufferGen : Gen<UserBuffer> =
        Gen.oneof
            [
                Gen.constant UserBuffer.Mapped
                Gen.constant UserBuffer.Opaque
                Gen.constant UserBuffer.Addressless
                Gen.elements [ 0UL ; 1UL ; 0x7FFF_FFFF_F000UL ; 0xFFFF_FFFF_FFFF_F000UL ]
                |> Gen.map UserBuffer.Unmapped
            ]

    let private streamCountGen : Gen<int> =
        Gen.frequency
            [
                3, Gen.choose (0, 64)
                3,
                Gen.elements
                    [
                        0
                        1
                        16
                        511
                        512
                        513
                        4095
                        4096
                        4097
                        65535
                        65536
                        65537
                        70000
                    ]
                2, Gen.choose (0, 200_000)
            ]

    /// Measured: standard input's writer is gone, so a non-blocking read answers
    /// end-of-file exactly as a blocking one does (`read(0, buf, 16)`,
    /// `read(0, NULL, 16)` and `read(0, buf, 0)` are all 0). So the flag changes
    /// no answer a read of standard input gives, whatever the count and buffer,
    /// and the read changes nothing either way.
    [<Test>]
    let ``the flag changes no read of standard input`` () : unit =
        let property (platform : SimulatedUnixPlatform, buffer : UserBuffer, count : int) : unit =
            let clear = systemOn platform
            let flagged = set 0 true clear

            let answerOf (system : UnixSystem<int, string>) =
                match ReadOutcomes.read 0 buffer (uint64 count) system with
                | Ok (answer, after) ->
                    after |> shouldEqual system
                    Ok answer
                | Error refusal -> Error refusal

            answerOf flagged |> shouldEqual (answerOf clear)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip3 (Gen.elements streamPlatforms) bufferGen streamCountGen)) property
        )

    /// The measured rows themselves, as literals.
    [<Test>]
    let ``a non-blocking read of standard input is end of file`` () : unit =
        for platform in streamPlatforms do
            let flagged = set 0 true (systemOn platform)

            for buffer, count in
                [
                    UserBuffer.Mapped, 16UL
                    UserBuffer.Unmapped 0UL, 16UL
                    UserBuffer.Mapped, 0UL
                ] do
                match ReadOutcomes.read 0 buffer count flagged with
                | Ok (ReadAnswer.Completed bytes, _) -> bytes.IsEmpty |> shouldEqual true
                | other -> failwith $"%O{platform}: read(0, %A{buffer}, %d{count}) answered %A{other}"

    /// Measured: with the far reader draining as fast as it can, a non-blocking
    /// write to stdout or stderr of at most 65536 bytes is taken whole, and a
    /// longer one takes 65536 and comes back short -- 20 writes to each stream
    /// of each of sixteen sizes from 1 to 1 MiB, each after the pipe had
    /// drained, on both flavours. That
    /// is what a write into an empty pipe takes. The whole writes are answered
    /// exactly as a blocking write is; the short ones deliver the bytes they
    /// took, and no more.
    [<Test>]
    let ``a non-blocking write to an output stream is whole up to what an empty pipe takes`` () : unit =
        let emptyPipeTakes = 65536

        let property (platform : SimulatedUnixPlatform, fd : int, count : int) : unit =
            let clear = systemOn platform
            let flagged = set fd true clear
            let bytes = ImmutableArray.Create<byte> (Array.init count byte)

            let unflag (system : UnixSystem<int, string>) =
                match UnixSocket.setNonBlocking fd false system with
                | SetNonBlockingAnswer.Set, system -> system
                | SetNonBlockingAnswer.Failed error, _ -> failwith $"could not clear the flag: %O{error}"

            let deliveries (system : UnixSystem<int, string>) =
                DeliveryLog.toList system.Machine.Delivered
                |> List.map (fun delivery -> delivery.Endpoint, List.ofSeq delivery.Bytes)

            match WriteOutcomes.write fd bytes flagged, WriteOutcomes.write fd bytes clear with
            | Ok (answer, after), Ok (blockingAnswer, blockingAfter) when count <= emptyPipeTakes ->
                answer |> shouldEqual (WriteAnswer.Completed (int64 count))
                answer |> shouldEqual blockingAnswer

                // The same bytes reach the same client, delivery for delivery,
                // and the flag is the only other difference.
                deliveries after |> shouldEqual (deliveries blockingAfter)
                unflag after |> shouldEqual blockingAfter
            | Ok (answer, after), Ok (WriteAnswer.Completed written, blockingAfter) ->
                written |> shouldEqual (int64 count)

                deliveries blockingAfter
                |> shouldEqual [ ExternalEndpoint fd, List.ofSeq bytes ]

                answer |> shouldEqual (WriteAnswer.Completed (int64 emptyPipeTakes))

                deliveries after
                |> shouldEqual [ ExternalEndpoint fd, List.ofSeq bytes |> List.take emptyPipeTakes ]
            | flaggedResult, clearResult ->
                failwith
                    $"%O{platform}: write(%d{fd}, %d{count} bytes) answered %A{Result.map fst flaggedResult} with the flag set and %A{Result.map fst clearResult} without it"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll
                (Arb.fromGen (
                    Gen.zip3 (Gen.elements streamPlatforms) (Gen.elements [ 1 ; 2 ]) (streamCountGen |> Gen.map (max 1))
                ))
                property
        )

    /// The boundary of the property above, as literals.
    [<Test>]
    let ``a non-blocking write one byte longer than an empty pipe takes is short`` () : unit =
        for platform in streamPlatforms do
            let flagged = set 1 true (systemOn platform)

            match WriteOutcomes.write 1 (ImmutableArray.Create<byte> (Array.zeroCreate 65536)) flagged with
            | Ok (WriteAnswer.Completed 65536L, _) -> ()
            | other -> failwith $"%O{platform}: a 65536-byte write answered %A{Result.map fst other}"

            match WriteOutcomes.write 1 (ImmutableArray.Create<byte> (Array.zeroCreate 65537)) flagged with
            | Ok (WriteAnswer.Completed 65536L, after) ->
                DeliveryLog.toList after.Machine.Delivered
                |> List.map (fun delivery -> delivery.Bytes.Length)
                |> shouldEqual [ 65536 ]
            | other -> failwith $"%O{platform}: a 65537-byte write answered %A{Result.map fst other}"

    // ------------------------------------------------------------------
    // The event port, where store and answer come apart
    // ------------------------------------------------------------------

    /// The flavour's event port: an epoll instance or a kqueue, and the
    /// descriptor onto it.
    let private withPort (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match SimulatedUnixPlatform.flavour system.Machine.UnixPlatform with
        | SimulatedUnixFlavour.Linux ->
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok (fd, system)) -> fd, system
            | other -> failwith $"expected an epoll instance, got %A{other}"
        | SimulatedUnixFlavour.Darwin ->
            match UnixKqueue.kqueue system with
            | Ok (fd, system) -> fd, system
            | Error refusal -> failwith $"expected a kqueue, got %A{refusal}"

    /// Measured: the bit toggles on both an epoll instance and a kqueue, and the
    /// answers differ — Linux succeeds where Darwin reports ENOTTY **with the bit
    /// toggled anyway**, in both directions. That is why the answer and the
    /// stored flag are checked separately, and why the failing arm still hands
    /// back a system. Literals, so that the rows cannot agree with any rule at
    /// all.
    [<Test>]
    let ``an event port stores the flag whatever it answers`` () : unit =
        let rows =
            [
                SimulatedUnixPlatform.linuxX64, SetNonBlockingAnswer.Set
                SimulatedUnixPlatform.linuxArm64, SetNonBlockingAnswer.Set
                SimulatedUnixPlatform.macOsArm64, SetNonBlockingAnswer.Failed UnixError.ENOTTY
            ]

        for platform, expected in rows do
            let portFd, system = withPort (systemOn platform)

            for value in [ true ; false ; true ] do
                let answer, after = setOrFail portFd value system
                UnixSocket.isNonBlocking portFd after |> shouldEqual (Some value)
                answer |> shouldEqual expected
