namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSocket.socket` against what real kernels answered, over every
/// argument the sweeps in `socketSyscall/` asked about: 392,736 calls per
/// flavour, taken with
/// `docs/plans/2026-08-23-posix-kernel-extraction/socket-arguments.c`.
///
/// Where the library answers, the answer must be the measured one. Where it
/// refuses, the refusal must be consistent with the measurement it claims to
/// stand for, and the number of refusals of each kind is pinned, so a
/// library that refused more than it must would fail.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketSyscall =

    let private systemOn (platform : SimulatedUnixPlatform) : UnixSystem<int, string> = UnixSystem.initial platform

    let private numbering (platform : SimulatedUnixPlatform) : RawErrnoNumbering =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> RawErrnoNumbering.Linux
        | SimulatedUnixFlavour.Darwin -> RawErrnoNumbering.Darwin

    let private errno (platform : SimulatedUnixPlatform) (error : UnixError) : int =
        UnixError.toRawErrnoUnder (numbering platform) error

    let private soType (kind : SocketKind) : int =
        match kind with
        | SocketKind.Stream -> 1
        | SocketKind.Datagram -> 2
        | SocketKind.SeqPacket -> 5

    let private domainNumber (platform : SimulatedUnixPlatform) (domain : SocketDomain) : int =
        match domain with
        | SocketDomain.Unix -> 1
        | SocketDomain.Inet -> SimulatedUnixPlatform.internetAddressFamily
        | SocketDomain.Inet6 -> SimulatedUnixPlatform.internetV6AddressFamily platform

    /// The kinds of refusal, for counting.
    let private refusalKind (refusal : SocketRefusal) : string =
        match refusal with
        | SocketRefusal.UnmodelledDomain _ -> "UnmodelledDomain"
        | SocketRefusal.RawSocket _ -> "RawSocket"
        | SocketRefusal.IcmpDatagram _ -> "IcmpDatagram"
        | SocketRefusal.BuildDependentProtocol _ -> "BuildDependentProtocol"

    /// Every disagreement between the library and the measurement, and how many
    /// refusals of each kind it made.
    let private compare
        (platform : SimulatedUnixPlatform)
        (measured : ((int * int * int) * MeasuredSocketAnswer) list)
        (measuredAsRoot : Map<int * int * int, MeasuredSocketAnswer> option)
        : string list * Map<string, int>
        =
        let system = systemOn platform
        let errno = errno platform
        let flavour = SimulatedUnixPlatform.flavour platform

        let disagreements = ResizeArray ()
        let mutable refusals = Map.empty

        for (domain, socketType, protocol) as arguments, answer in measured do
            let describe (message : string) : unit =
                disagreements.Add
                    $"socket(%d{domain}, 0x%x{socketType}, %d{protocol}): measured %A{answer}, %s{message}"

            match UnixSocket.socket domain socketType protocol system, answer with
            | Ok (Error error), MeasuredSocketAnswer.Failed measured ->
                if errno error <> measured then
                    describe $"library %O{error}"
            | Ok (Ok (fd, after)), MeasuredSocketAnswer.Created (measuredType, measuredProtocol, measuredNonBlocking, _) ->
                let socket =
                    match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
                    | Some (OpenFileTarget.Socket socketId) -> UnixMachineState.socket socketId after.Machine
                    | other -> failwith $"socket(%A{arguments}) made descriptor %d{fd} onto %A{other}"

                if soType socket.Kind <> measuredType then
                    describe $"library kind %O{socket.Kind}"

                if domainNumber platform socket.Domain <> domain then
                    describe $"library domain %O{socket.Domain}"

                if UnixSocket.isNonBlocking fd after <> Some measuredNonBlocking then
                    describe $"library non-blocking %A{UnixSocket.isNonBlocking fd after}"

                // Linux reports the protocol it resolved; a protocol the caller
                // named must be that one.
                match socket.Protocol, measuredProtocol with
                | SocketProtocol.Tcp, Some measured when measured <> 6 -> describe "library TCP"
                | SocketProtocol.Udp, Some measured when measured <> 17 -> describe "library UDP"
                | _ -> ()
            | Error refusal, _ ->
                let kind = refusalKind refusal

                refusals <-
                    refusals
                    |> Map.change kind (fun count -> Some (1 + Option.defaultValue 0 count))

                // Each refusal must stand for what it says it does.
                let consistent =
                    match refusal, answer with
                    | SocketRefusal.UnmodelledDomain refused, answer ->
                        refused = domain
                        && match flavour, answer with
                           // Darwin refuses only a family it has.
                           | SimulatedUnixFlavour.Darwin, MeasuredSocketAnswer.Failed measured ->
                               measured <> errno UnixError.EAFNOSUPPORT
                           | _ -> true
                    | SocketRefusal.RawSocket (d, t, p), answer ->
                        (d, t, p) = arguments
                        && match answer with
                           | MeasuredSocketAnswer.Failed measured ->
                               measured = errno UnixError.EPERM
                               || (flavour = SimulatedUnixFlavour.Darwin && measured = errno UnixError.EOPNOTSUPP)
                           | MeasuredSocketAnswer.Created _ -> false
                        // Linux's privileged answer, where it was measured,
                        // is a socket.
                        && match measuredAsRoot with
                           | None -> true
                           | Some root ->
                               match root.[arguments] with
                               | MeasuredSocketAnswer.Created _ -> true
                               | MeasuredSocketAnswer.Failed _ -> false
                    | SocketRefusal.IcmpDatagram (d, p), answer ->
                        (d, p) = (domain, protocol)
                        && match flavour, answer with
                           | SimulatedUnixFlavour.Darwin, MeasuredSocketAnswer.Created _ -> true
                           | SimulatedUnixFlavour.Linux, MeasuredSocketAnswer.Failed measured ->
                               measured = errno UnixError.EACCES
                           | _ -> false
                    | SocketRefusal.BuildDependentProtocol (d, t, p), answer ->
                        (d, t, p) = arguments
                        && flavour = SimulatedUnixFlavour.Linux
                        && match answer with
                           | MeasuredSocketAnswer.Created _ -> true
                           | MeasuredSocketAnswer.Failed measured ->
                               measured = errno UnixError.EPROTONOSUPPORT
                               || measured = errno UnixError.ESOCKTNOSUPPORT

                if not consistent then
                    describe $"library refused: %s{SocketRefusal.describe refusal}"
            | Ok (Ok _), MeasuredSocketAnswer.Failed _ -> describe "library created a socket"
            | Ok (Error error), MeasuredSocketAnswer.Created _ -> describe $"library %O{error}"

        List.ofSeq disagreements, refusals

    [<Test>]
    let ``Linux answers every measured call as Linux did`` () : unit =
        let measured = SocketSweep.load "linux.tsv"
        measured |> List.length |> shouldEqual 392_736

        let root = SocketSweep.load "linux-root.tsv" |> Map.ofList
        root |> Map.count |> shouldEqual 392_736

        let disagreements, refusals =
            compare SimulatedUnixPlatform.linuxX64 measured (Some root)

        disagreements |> List.truncate 20 |> shouldEqual []

        // Counted from the sweep's shape, where the flag sweep crosses three
        // valid flag sets with types 0..10 and nine protocols:
        // - UnmodelledDomain: the 42 families in 3..45 other than 10, times
        //   types 0..10, times 310 protocols; and family 30 in the flag sweep,
        //   3 * 11 * 9.
        // - RawSocket: SOCK_RAW with protocols 1..262 in each internet domain,
        //   and AF_INET's SOCK_PACKET with all 310; in the flag sweep, SOCK_RAW
        //   with the six in-range non-zero protocols in each domain, 3 * 2 * 6,
        //   and AF_INET's SOCK_PACKET, 3 * 9.
        // - IcmpDatagram: one per domain, and AF_INET's in the flag sweep three
        //   times (58 is not among its protocols).
        // - BuildDependentProtocol: five stream and datagram protocols and the
        //   263 in-range seqpacket ones per domain; in the flag sweep, two
        //   stream and seven seqpacket protocols, 3 * 2 * 9.
        refusals
        |> shouldEqual (
            Map.ofList
                [
                    "UnmodelledDomain", 143_517
                    "RawSocket", 897
                    "IcmpDatagram", 5
                    "BuildDependentProtocol", 590
                ]
        )

    [<Test>]
    let ``Darwin answers every measured call as Darwin did`` () : unit =
        let measured = SocketSweep.load "darwin.tsv"
        measured |> List.length |> shouldEqual 392_736

        let disagreements, refusals = compare SimulatedUnixPlatform.macOsArm64 measured None

        disagreements |> List.truncate 20 |> shouldEqual []

        // UnmodelledDomain: the seven other families, times 16 types, times 310
        // protocols. RawSocket: SOCK_RAW with all 310 in each internet domain.
        // IcmpDatagram: one per domain. The flag sweep adds none: no Darwin
        // type has a flag bit.
        refusals
        |> shouldEqual (Map.ofList [ "UnmodelledDomain", 34_720 ; "RawSocket", 620 ; "IcmpDatagram", 2 ])

    /// The measurements were taken at euid 1000 (Linux) and 501 (Darwin), and
    /// the library answers without reading the caller's identity: every answer
    /// it gives must be the privileged kernel's answer too.
    [<Test>]
    let ``Linux's answers do not depend on privilege`` () : unit =
        let root = SocketSweep.load "linux-root.tsv"

        let disagreements, _ = compare SimulatedUnixPlatform.linuxX64 root None

        disagreements
        |> List.filter (fun message -> not (message.Contains "library refused"))
        |> List.truncate 20
        |> shouldEqual []

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    /// Arguments drawn from the whole int range, weighted towards the numbers
    /// that mean something.
    let private arguments : Gen<int * int * int> =
        let interesting (values : int list) : Gen<int> =
            Gen.frequency [ 3, Gen.elements values ; 1, ArbMap.defaults |> ArbMap.generate<int> ]

        let domains =
            interesting [ -1 ; 0 ; 1 ; 2 ; 10 ; 16 ; 17 ; 30 ; 32 ; 45 ; 46 ; System.Int32.MinValue ]

        let types =
            interesting
                [
                    0
                    1
                    2
                    3
                    4
                    5
                    10
                    11
                    0x801
                    0x80001
                    0x80802
                    0x10
                    System.Int32.MinValue
                ]

        let protocols = interesting [ -1 ; 0 ; 1 ; 6 ; 17 ; 58 ; 132 ; 255 ; 262 ; 263 ]
        Gen.zip3 domains types protocols

    /// A created socket is the lowest free descriptor onto a fresh, idle,
    /// unbound socket, and nothing else changes; a failure or a refusal leaves
    /// nothing to change.
    [<TestCaseSource(nameof platforms)>]
    let ``socket creates exactly one socket and one descriptor`` (platform : SimulatedUnixPlatform) : unit =
        // A hole below a live descriptor, so the lowest free one is neither
        // the next nor the first ever handed out.
        let hole, before =
            let first, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp (systemOn platform)

            let second, system =
                NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

            second |> shouldEqual (first + 1)

            match UnixDescriptor.close first system with
            | Ok (SyscallAnswer.Completed _, system) -> first, system
            | Ok (answer, _) -> failwith $"closing descriptor %d{first} answered %A{answer}"
            | Error _ -> failwith $"closing descriptor %d{first} was refused"

        let property (domain : int, socketType : int, protocol : int) : unit =
            match UnixSocket.socket domain socketType protocol before with
            | Ok (Ok (fd, after)) ->
                fd |> shouldEqual hole

                let socketId = before.Machine.NextSocketId

                FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors
                |> shouldEqual (Some (OpenFileTarget.Socket socketId))

                let socket = UnixMachineState.socket socketId after.Machine
                socket.Binding |> shouldEqual None
                socket.Phase |> shouldEqual SocketPhase.Idle
                socket.ReuseAddress |> shouldEqual false

                Map.remove socketId after.Machine.Sockets |> shouldEqual before.Machine.Sockets
            | Ok (Error _)
            | Error _ -> ()

        Prop.forAll (Arb.fromGen arguments) property |> Check.QuickThrowOnFailure

    /// Linux's `SOCK_CLOEXEC` changes nothing this library models, and
    /// `SOCK_NONBLOCK` changes only the new description's flag.
    [<Test>]
    let ``Linux's type flags change nothing but the non-blocking flag`` () : unit =
        let system = systemOn SimulatedUnixPlatform.linuxX64

        let property (domain : int, baseType : int, protocol : int) : unit =
            let baseType = baseType &&& 0xf

            let answer (socketType : int) =
                match UnixSocket.socket domain socketType protocol system with
                | Ok (Ok (fd, after)) -> Ok (Ok (UnixSocket.isNonBlocking fd after))
                | Ok (Error error) -> Ok (Error error)
                | Error refusal -> Error (refusalKind refusal)

            let plain = answer baseType
            answer (baseType ||| 0x80000) |> shouldEqual plain

            let expectedNonBlocking =
                match plain with
                | Ok (Ok (Some false)) -> Ok (Ok (Some true))
                | other -> other

            answer (baseType ||| 0x800) |> shouldEqual expectedNonBlocking
            answer (baseType ||| 0x80800) |> shouldEqual expectedNonBlocking

        Prop.forAll (Arb.fromGen arguments) property |> Check.QuickThrowOnFailure
