namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PosixKernel

/// What a socket's domain, its `IPV6_V6ONLY` and its local address allow of
/// one another, as a sequence of calls changes them: random sequences of
/// `socket`, `setsockopt` and `getsockopt` of `IPV6_V6ONLY`, `bind` and
/// `listen`, against a reference model written out here, with the system's
/// invariants checked after every call.
///
/// The reference model holds per socket only what decides these answers: its
/// domain and kind, its `IPV6_V6ONLY`, and whether it has an address.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketAddressing =

    [<RequireQualifiedAccess>]
    type Call =
        | Socket of domain : SocketDomain * kind : SocketKind
        | SetIpv6Only of socket : int * value : int
        | GetIpv6Only of socket : int
        | Bind of socket : int
        | Listen of socket : int

    /// What the reference model holds of one socket.
    type Expected =
        {
            Domain : SocketDomain
            Kind : SocketKind
            Ipv6Only : bool
            HasAddress : bool
        }

    /// A call's answer, reduced to what the reference model predicts.
    [<RequireQualifiedAccess>]
    type Answer =
        | Succeeded
        | Failed of UnixError
        | Read of value : int
        | Refused

    let private flavourColumn (platform : SimulatedUnixPlatform) (onLinux : 'a) (onDarwin : 'a) : 'a =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> onLinux
        | SimulatedUnixFlavour.Darwin -> onDarwin

    /// The answer to a set of `IPV6_V6ONLY` on `socket`, and the socket after
    /// it. Measured (`docs/probes/sockopt-options/`): an option for a socket it
    /// does not apply to is the flavour's errno, and an IPv6 socket with an
    /// address answers EINVAL.
    let private expectedSet (platform : SimulatedUnixPlatform) (value : int) (socket : Expected) : Answer * Expected =
        match socket.Domain with
        | SocketDomain.Inet6 when socket.HasAddress -> Answer.Failed UnixError.EINVAL, socket
        | SocketDomain.Inet6 ->
            Answer.Succeeded,
            { socket with
                Ipv6Only = value <> 0
            }
        | SocketDomain.Inet -> Answer.Failed (flavourColumn platform UnixError.ENOPROTOOPT UnixError.EINVAL), socket
        | SocketDomain.Unix -> Answer.Failed UnixError.EOPNOTSUPP, socket

    let private expectedGet (platform : SimulatedUnixPlatform) (socket : Expected) : Answer =
        match socket.Domain with
        | SocketDomain.Inet6 -> Answer.Read (if socket.Ipv6Only then 1 else 0)
        | SocketDomain.Inet -> Answer.Failed (flavourColumn platform UnixError.EOPNOTSUPP UnixError.EINVAL)
        | SocketDomain.Unix -> Answer.Failed UnixError.EOPNOTSUPP

    /// A bind to loopback and port 0, in the socket's own family: on an IPv6
    /// socket, `::ffff:127.0.0.1`. Measured (`docs/probes/dual-mode/`, B and
    /// F): an IPv6 socket with `IPV6_V6ONLY` on takes no v4-mapped address,
    /// and judges that before whether it is bound.
    let private expectedBind (platform : SimulatedUnixPlatform) (socket : Expected) : Answer * Expected =
        let bindable =
            socket.Domain = SocketDomain.Inet
            || (socket.Domain = SocketDomain.Inet6 && socket.Kind = SocketKind.Stream)

        if not bindable then
            Answer.Refused, socket
        elif socket.Domain = SocketDomain.Inet6 && socket.Ipv6Only then
            Answer.Failed (flavourColumn platform UnixError.EINVAL UnixError.EADDRNOTAVAIL), socket
        elif socket.HasAddress then
            Answer.Failed UnixError.EINVAL, socket
        else
            Answer.Succeeded,
            { socket with
                HasAddress = true
            }

    let private expectedListen (socket : Expected) : Answer * Expected =
        match socket.Domain with
        | SocketDomain.Inet ->
            Answer.Succeeded,
            { socket with
                HasAddress = true
            }
        | SocketDomain.Inet6
        | SocketDomain.Unix -> Answer.Refused, socket

    let private ipv6OnlyOption (platform : SimulatedUnixPlatform) : int * int =
        SimulatedUnixPlatform.ipv6OptionLevel platform, SimulatedUnixPlatform.ipv6OnlyOption platform

    let private set (fd : int) (value : int) (system : UnixSystem<int, string>) : Answer * UnixSystem<int, string> =
        let level, name = ipv6OnlyOption system.Machine.UnixPlatform
        let bytes = OptionValue.ofInt value

        let supplied =
            match UnixSocket.admitSetSockOpt fd level name UserBuffer.Mapped 4u system with
            | Ok (SetSockOptAdmission.Transfer count) -> Some (ImmutableArray.Create (bytes, 0, count))
            | Ok SetSockOptAdmission.NoCopy
            | Ok (SetSockOptAdmission.Answered _)
            | Error _ -> None

        match UnixSocket.setsockopt fd level name UserBuffer.Mapped 4u supplied system with
        | Ok (SetSockOptAnswer.Set, system) -> Answer.Succeeded, system
        | Ok (SetSockOptAnswer.Failed error, system) -> Answer.Failed error, system
        | Error _ -> Answer.Refused, system

    let private get (fd : int) (system : UnixSystem<int, string>) : Answer =
        let level, name = ipv6OnlyOption system.Machine.UnixPlatform

        let read =
            match UnixSocket.admitGetSockOpt fd level name UserBuffer.Mapped UserBuffer.Mapped system with
            | Ok GetSockOptAdmission.ReadLength -> Some 4u
            | _ -> None

        match UnixSocket.getsockopt fd level name UserBuffer.Mapped UserBuffer.Mapped read system with
        | Ok (GetSockOptAnswer.Reported bytes, _) when bytes.Length = 4 ->
            Answer.Read (SimulatedUnixPlatform.decodeCInt system.Machine.UnixPlatform bytes 0)
        | Ok (GetSockOptAnswer.Reported bytes, _) -> failwith $"getsockopt reported %d{bytes.Length} bytes"
        | Ok (GetSockOptAnswer.Failed (error, _), _) -> Answer.Failed error
        | Error _ -> Answer.Refused

    let private bind
        (fd : int)
        (domain : SocketDomain)
        (system : UnixSystem<int, string>)
        : Answer * UnixSystem<int, string>
        =
        let platform = system.Machine.UnixPlatform
        let loopback = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 0us

        let blob, length =
            match domain with
            | SocketDomain.Inet6 -> CopyIn.inet6Mapped platform loopback, 28u
            | SocketDomain.Inet
            | SocketDomain.Unix -> CopyIn.inet platform loopback, 16u

        match CopyIn.bind fd UserBuffer.Mapped length blob system with
        | Ok (BindAnswer.Bound _, system) -> Answer.Succeeded, system
        | Ok (BindAnswer.Failed error, system) -> Answer.Failed error, system
        | Error _ -> Answer.Refused, system

    let private listen (fd : int) (system : UnixSystem<int, string>) : Answer * UnixSystem<int, string> =
        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> Answer.Succeeded, system
        | Ok (ListenAnswer.Failed error, system) -> Answer.Failed error, system
        | Error _ -> Answer.Refused, system

    let private callGen : Gen<Call> =
        let index = Gen.choose (0, 3)

        Gen.oneof
            [
                Gen.zip
                    (Gen.elements [ SocketDomain.Inet ; SocketDomain.Inet6 ; SocketDomain.Unix ])
                    (Gen.elements [ SocketKind.Stream ; SocketKind.Datagram ])
                |> Gen.map Call.Socket
                Gen.zip index (Gen.elements [ 0 ; 1 ; 2 ; -1 ]) |> Gen.map Call.SetIpv6Only
                index |> Gen.map Call.GetIpv6Only
                index |> Gen.map Call.Bind
                index |> Gen.map Call.Listen
            ]

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    [<Test>]
    let ``a socket's domain, IPV6_V6ONLY and address move together as the reference model says`` () : unit =
        let gen =
            Gen.zip
                (Gen.zip (Gen.elements platforms) (Gen.elements [ false ; true ]))
                (Gen.listOf callGen |> Gen.resize 24)

        let property ((platform : SimulatedUnixPlatform, ipv6OnlyByDefault : bool), calls : Call list) : unit =
            let system =
                UnixSystem.initial platform
                |> UnixBootImage.withIpv6OnlyByDefault ipv6OnlyByDefault
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let check (describe : string) (system : UnixSystem<int, string>) : unit =
                match UnixSystem.checkInvariants system with
                | [] -> ()
                | defects -> failwith $"%s{describe}: the system broke its invariants: %A{defects}"

            // The sockets made so far, oldest first: descriptor and model.
            let rec go
                (index : int)
                (calls : Call list)
                (sockets : (int * Expected) list)
                (system : UnixSystem<int, string>)
                : unit
                =
                match calls with
                | [] -> ()
                | call :: rest ->

                let describe =
                    $"%O{platform}, default %b{ipv6OnlyByDefault}, call %d{index} (%A{call})"

                let target (i : int) : (int * (int * Expected)) option =
                    if List.isEmpty sockets then
                        None
                    else
                        let i = i % List.length sockets
                        Some (i, List.item i sockets)

                let replace (i : int) (expected : Expected) =
                    sockets
                    |> List.mapi (fun j (fd, old) -> if i = j then fd, expected else fd, old)

                let compare (actual : Answer) (expected : Answer) =
                    if actual <> expected then
                        failwith $"%s{describe}: answered %A{actual}, where the model says %A{expected}"

                match call with
                | Call.Socket (domain, kind) ->
                    let fd, system = NewSocket.create domain kind SocketProtocol.Default system
                    check describe system

                    let expected =
                        {
                            Domain = domain
                            Kind = kind
                            Ipv6Only = domain = SocketDomain.Inet6 && ipv6OnlyByDefault
                            HasAddress = false
                        }

                    go (index + 1) rest (sockets @ [ fd, expected ]) system
                | Call.SetIpv6Only (i, value) ->
                    match target i with
                    | None -> go (index + 1) rest sockets system
                    | Some (i, (fd, socket)) ->
                        let actual, system = set fd value system
                        let expected, socket = expectedSet platform value socket
                        compare actual expected
                        check describe system
                        go (index + 1) rest (replace i socket) system
                | Call.GetIpv6Only i ->
                    match target i with
                    | None -> go (index + 1) rest sockets system
                    | Some (_, (fd, socket)) ->
                        compare (get fd system) (expectedGet platform socket)
                        go (index + 1) rest sockets system
                | Call.Bind i ->
                    match target i with
                    | None -> go (index + 1) rest sockets system
                    | Some (i, (fd, socket)) ->
                        let actual, system = bind fd socket.Domain system
                        let expected, socket = expectedBind platform socket
                        compare actual expected
                        check describe system
                        go (index + 1) rest (replace i socket) system
                | Call.Listen i ->
                    match target i with
                    | Some (i, (fd, socket)) when socket.Kind = SocketKind.Stream ->
                        let actual, system = listen fd system
                        let expected, socket = expectedListen socket
                        compare actual expected
                        check describe system
                        go (index + 1) rest (replace i socket) system
                    | Some _
                    | None -> go (index + 1) rest sockets system

            go 0 calls [] system

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)
