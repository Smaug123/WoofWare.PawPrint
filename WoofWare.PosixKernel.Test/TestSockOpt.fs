namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSocket.setsockopt` and `UnixSocket.getsockopt`, driven directly on a
/// constructed system.
///
/// The literal rows are the measurements, written out per flavour: Darwin
/// 25.6.0 natively and Linux 6.18.5 (arm64, under Apple's `container`), with
/// `docs/plans/2026-08-23-posix-kernel-extraction/sockopt.c`. They are literals
/// rather than a restatement of the rule so that a change to the rule has
/// something independent to disagree with. `TestSockOptAgainstHost` puts the
/// same questions to the kernel running the suite.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSockOpt =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64
    let private platforms : SimulatedUnixPlatform list = [ linux ; darwin ]

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private wildcard (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.WildcardAddress port

    /// A descriptor number nothing in a fresh system has open.
    let private closedFd : int = 99

    /// Standard input, which this kernel models as a pipe.
    let private pipeFd : int = 0

    /// A fresh system on `platform` holding one IPv4 stream socket in `phase`,
    /// bound where the phase needs it, and one socket event port.
    let private systemWith
        (platform : SimulatedUnixPlatform)
        (phase : SocketPhase)
        : int * int * UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        let socketFd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let binding =
            match phase with
            | SocketPhase.Idle -> None
            | _ ->
                Some
                    {
                        Endpoint = loopback 40000us
                        LockedAddress = None
                        LockedPort = false
                    }

        let system =
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            system.Machine.Sockets
                            |> Map.change
                                (SocketId 0L)
                                (Option.map (fun socket ->
                                    { socket with
                                        Phase = phase
                                        Binding = binding
                                    }
                                ))
                    }
            }

        let portFd, registry =
            FileDescriptorRegistry.createSocketEventPort system.Process.FileDescriptors

        socketFd,
        portFd,
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let private reuseFlag (system : UnixSystem<int, string>) : bool =
        (UnixMachineState.socket (SocketId 0L) system.Machine).ReuseAddress

    /// The whole `setsockopt(2)` a client makes: ask the admission, read the
    /// value only if the kernel would, then make the call.
    let private setWith
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (optionLength : uint32)
        (supplied : int)
        (system : UnixSystem<int, string>)
        : Result<SetSockOptAnswer * UnixSystem<int, string>, SocketOptionRefusal>
        =
        let read =
            match UnixSocket.admitSetSockOpt fd level optionName value optionLength system with
            | Ok (SetSockOptAdmission.Transfer length) ->
                length |> shouldEqual 4
                Some supplied
            | Ok (SetSockOptAdmission.Answered _)
            | Error _ -> None

        UnixSocket.setsockopt fd level optionName value optionLength read system

    /// The whole `getsockopt(2)` a client makes: ask the admission, read the
    /// length cell only if the kernel would, then make the call.
    let private getWith
        (fd : int)
        (level : int)
        (optionName : int)
        (value : UserBuffer)
        (length : UserBuffer)
        (declaredLength : uint32)
        (system : UnixSystem<int, string>)
        : Result<GetSockOptAnswer, SocketOptionRefusal>
        =
        let read =
            match UnixSocket.admitGetSockOpt fd level optionName length system with
            | Ok GetSockOptAdmission.ReadLength -> Some declaredLength
            | Ok (GetSockOptAdmission.Answered _)
            | Error _ -> None

        UnixSocket.getsockopt fd level optionName value length read system

    /// Which option a row asks about, named so that each flavour can number it.
    [<RequireQualifiedAccess>]
    type private Option =
        | ReuseAddress
        /// `SO_ERROR`: measured numbers, 4 on Linux and 0x1007 on Darwin.
        | SocketError
        /// A `SOL_SOCKET` option number neither kernel defines.
        | UnknownAtSocketLevel
        /// A level neither kernel defines, with an option number to match.
        | UnknownLevel

    let private numbered (platform : SimulatedUnixPlatform) (option : Option) : int * int =
        let socketLevel = SimulatedUnixPlatform.socketOptionLevel platform

        match option with
        | Option.ReuseAddress -> socketLevel, SimulatedUnixPlatform.reuseAddressOption platform
        | Option.SocketError ->
            socketLevel,
            (match SimulatedUnixPlatform.flavour platform with
             | SimulatedUnixFlavour.Linux -> 4
             | SimulatedUnixFlavour.Darwin -> 0x1007)
        | Option.UnknownAtSocketLevel -> socketLevel, 9999
        | Option.UnknownLevel -> 999, 999

    /// What a row expects of one flavour.
    [<RequireQualifiedAccess>]
    type private Expect =
        | Fails of UnixError
        | Succeeds
        | Refused

    /// Which descriptor a row calls on.
    [<RequireQualifiedAccess>]
    type private Target =
        | Closed
        | Pipe
        | EventPort
        | Socket

    let private null' : UserBuffer = UserBuffer.Unmapped 0UL
    let private unmapped : UserBuffer = UserBuffer.Unmapped 0x1000UL

    let private ebadf : Expect = Expect.Fails UnixError.EBADF
    let private enotsock : Expect = Expect.Fails UnixError.ENOTSOCK
    let private efault : Expect = Expect.Fails UnixError.EFAULT
    let private einval : Expect = Expect.Fails UnixError.EINVAL
    let private ok : Expect = Expect.Succeeds
    let private refused : Expect = Expect.Refused
    let private real : UserBuffer = UserBuffer.Mapped

    /// `(socklen_t)-1`.
    let private minus1 : uint32 = System.UInt32.MaxValue

    type private SetRow =
        {
            Label : string
            Target : Target
            Option : Option
            Value : UserBuffer
            Length : uint32
            Linux : Expect
            Darwin : Expect
        }

    let private setRow
        (label : string)
        (target : Target)
        (option : Option)
        (value : UserBuffer)
        (length : uint32)
        (onLinux : Expect)
        (onDarwin : Expect)
        : SetRow
        =
        {
            Label = label
            Target = target
            Option = option
            Value = value
            Length = length
            Linux = onLinux
            Darwin = onDarwin
        }

    /// Every `setsockopt` row measured, on a fresh stream socket unless the
    /// target says otherwise: label, target, option, value, length, then the
    /// Linux and the Darwin answer.
    let private setRows : SetRow list =
        let closed, pipe, port, sock =
            Target.Closed, Target.Pipe, Target.EventPort, Target.Socket

        let reuse = Option.ReuseAddress
        let unknownLevel, unknownOption = Option.UnknownLevel, Option.UnknownAtSocketLevel

        [
            setRow "closed" closed reuse real 4u ebadf ebadf
            // Darwin screens a null value before it looks at the descriptor.
            setRow "closed, null value" closed reuse null' 4u ebadf efault
            setRow "closed, null value, length 0" closed reuse null' 0u ebadf ebadf
            setRow "closed, unmapped value" closed reuse unmapped 4u ebadf ebadf
            setRow "closed, length 0" closed reuse real 0u ebadf ebadf
            setRow "closed, length -1" closed reuse real minus1 ebadf ebadf
            setRow "closed, unknown level, null value" closed unknownLevel null' 4u ebadf efault
            setRow "pipe" pipe reuse real 4u enotsock enotsock
            setRow "pipe, null value" pipe reuse null' 4u enotsock efault
            setRow "pipe, unmapped value" pipe reuse unmapped 4u enotsock enotsock
            setRow "pipe, length 0" pipe reuse real 0u enotsock enotsock
            setRow "pipe, length -1" pipe reuse real minus1 enotsock enotsock
            setRow "event port" port reuse real 4u enotsock enotsock
            setRow "socket, length 0" sock reuse real 0u einval einval
            setRow "socket, length 1" sock reuse real 1u einval einval
            setRow "socket, length 3" sock reuse real 3u einval einval
            setRow "socket, null value, length 0" sock reuse null' 0u einval einval
            setRow "socket, null value, length 3" sock reuse null' 3u einval efault
            setRow "socket, null value, length 4" sock reuse null' 4u efault efault
            setRow "socket, unmapped value, length 3" sock reuse unmapped 3u einval einval
            setRow "socket, unmapped value, length 4" sock reuse unmapped 4u efault efault
            // Linux reads the length as an `int`; Darwin as a `socklen_t`.
            setRow "socket, unmapped value, length -1" sock reuse unmapped minus1 einval efault
            setRow "socket, null value, length -1" sock reuse null' minus1 einval efault
            setRow "socket, length -1" sock reuse real minus1 einval ok
            setRow "socket, length 2^31" sock reuse real 0x8000_0000u einval ok
            setRow "socket, length 2^31 - 1" sock reuse real 0x7fff_ffffu ok ok
            setRow "socket, unmapped value, length 2^31 - 1" sock reuse unmapped 0x7fff_ffffu efault efault
            setRow "socket, length 4" sock reuse real 4u ok ok
            setRow "socket, length 8" sock reuse real 8u ok ok
            setRow "socket, length 16" sock reuse real 16u ok ok
            // Linux screens a negative length whatever the option; Darwin's
            // screen for an unknown level is the protocol's, and unmodelled.
            setRow "socket, unknown level, length -1" sock unknownLevel real minus1 einval refused
            setRow "socket, unknown level, null value" sock unknownLevel null' 4u refused efault
            setRow "socket, unknown SOL_SOCKET option" sock unknownOption real 4u refused refused
            setRow "socket, unknown SOL_SOCKET option, length 0" sock unknownOption real 0u refused refused
            setRow "socket, SO_ERROR" sock Option.SocketError real 4u refused refused
        ]

    let private targetFd (target : Target) (socketFd : int) (portFd : int) : int =
        match target with
        | Target.Closed -> closedFd
        | Target.Pipe -> pipeFd
        | Target.EventPort -> portFd
        | Target.Socket -> socketFd

    let private flavourColumn (platform : SimulatedUnixPlatform) (onLinux : 'a) (onDarwin : 'a) : 'a =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> onLinux
        | SimulatedUnixFlavour.Darwin -> onDarwin

    [<Test>]
    let ``SOL_SOCKET and SO_REUSEADDR are numbered per flavour`` () : unit =
        (SimulatedUnixPlatform.socketOptionLevel linux, SimulatedUnixPlatform.reuseAddressOption linux)
        |> shouldEqual (1, 2)

        (SimulatedUnixPlatform.socketOptionLevel darwin, SimulatedUnixPlatform.reuseAddressOption darwin)
        |> shouldEqual (0xffff, 4)

    [<Test>]
    let ``setsockopt answers every measured row`` () : unit =
        for platform in platforms do
            for row in setRows do
                let socketFd, portFd, system = systemWith platform SocketPhase.Idle
                let level, optionName = numbered platform row.Option
                let fd = targetFd row.Target socketFd portFd
                let expected = flavourColumn platform row.Linux row.Darwin

                let actual =
                    match setWith fd level optionName row.Value row.Length 1 system with
                    | Ok (SetSockOptAnswer.Set, after) ->
                        reuseFlag after |> shouldEqual true
                        Expect.Succeeds
                    | Ok (SetSockOptAnswer.Failed error, after) ->
                        after |> shouldEqual system
                        Expect.Fails error
                    | Error (SocketOptionRefusal.UnmodelledOption (SocketId 0L, l, n)) ->
                        (l, n) |> shouldEqual (level, optionName)
                        Expect.Refused
                    | Error refusal -> failwith $"%O{platform} %s{row.Label}: unexpected refusal %A{refusal}"

                if actual <> expected then
                    failwith $"%O{platform} %s{row.Label}: expected %A{expected}, got %A{actual}"

    /// A buffer this kernel cannot see through is refused exactly where the
    /// kernel would first look at it, and answered wherever an earlier screen
    /// decides first.
    [<Test>]
    let ``an opaque or addressless value is refused only where it is read`` () : unit =
        for platform in platforms do
            let socketFd, _, system = systemWith platform SocketPhase.Idle
            let level, optionName = numbered platform Option.ReuseAddress

            let attempt fd buffer length =
                UnixSocket.admitSetSockOpt fd level optionName buffer length system

            attempt socketFd UserBuffer.Opaque 4u
            |> shouldEqual (Error (SocketOptionRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

            attempt socketFd UserBuffer.Opaque 3u
            |> shouldEqual (Ok (SetSockOptAdmission.Answered UnixError.EINVAL))

            attempt closedFd UserBuffer.Opaque 4u
            |> shouldEqual (Ok (SetSockOptAdmission.Answered UnixError.EBADF))

            // Darwin compares the pointer with NULL before anything else, so an
            // address it has no number for is unanswerable there even on a
            // closed descriptor; Linux decides on the descriptor first.
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux ->
                attempt closedFd UserBuffer.Addressless 4u
                |> shouldEqual (Ok (SetSockOptAdmission.Answered UnixError.EBADF))

                attempt socketFd UserBuffer.Addressless 4u
                |> shouldEqual (Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtTransfer))
            | SimulatedUnixFlavour.Darwin ->
                attempt closedFd UserBuffer.Addressless 4u
                |> shouldEqual (Error (SocketOptionRefusal.Buffer BufferRefusal.AddresslessAtScreen))

                attempt closedFd UserBuffer.Addressless 0u
                |> shouldEqual (Ok (SetSockOptAdmission.Answered UnixError.EBADF))

    /// The same for `getsockopt`'s two buffers. The length cell is read on both
    /// flavours; the value buffer only when something is copied, and Darwin
    /// asks whether it is null first.
    [<Test>]
    let ``an opaque or addressless getsockopt buffer is refused only where it is read`` () : unit =
        for platform in platforms do
            let socketFd, _, system = systemWith platform SocketPhase.Idle
            let level, optionName = numbered platform Option.ReuseAddress

            for buffer, refusal in
                [
                    UserBuffer.Opaque, BufferRefusal.OpaqueAtTransfer
                    UserBuffer.Addressless, BufferRefusal.AddresslessAtTransfer
                ] do
                UnixSocket.admitGetSockOpt socketFd level optionName buffer system
                |> shouldEqual (Error (SocketOptionRefusal.Buffer refusal))

                UnixSocket.admitGetSockOpt closedFd level optionName buffer system
                |> shouldEqual (Ok (GetSockOptAdmission.Answered UnixError.EBADF))

            let get value declaredLength =
                UnixSocket.getsockopt socketFd level optionName value UserBuffer.Mapped (Some declaredLength) system

            get UserBuffer.Opaque 4u
            |> shouldEqual (Error (SocketOptionRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

            get UserBuffer.Opaque 0u |> shouldEqual (Ok (GetSockOptAnswer.Reported (0, 0u)))

            get UserBuffer.Addressless 0u
            |> shouldEqual (Ok (GetSockOptAnswer.Reported (0, 0u)))

            get UserBuffer.Addressless 4u
            |> shouldEqual (
                Error (
                    SocketOptionRefusal.Buffer (
                        flavourColumn platform BufferRefusal.AddresslessAtTransfer BufferRefusal.AddresslessAtScreen
                    )
                )
            )

    /// Every phase this kernel models, measured through a `setsockopt` of 1 and
    /// then of 0. Darwin refuses both on a socket whose connection was refused,
    /// because that socket can neither send nor receive any more; Linux takes
    /// the option in every phase.
    [<Test>]
    let ``setsockopt is refused on Darwin once a connection has been refused`` () : unit =
        let phases =
            [
                SocketPhase.Idle, false
                SocketPhase.Listening
                    {
                        Backlog = 5
                        Queue = []
                    },
                false
                SocketPhase.Established (ConnectionId 0L), false
                SocketPhase.EstablishedPendingReport (ConnectionId 0L), false
                SocketPhase.DatagramPeer (loopback 5000us), false
                SocketPhase.RefusedPendingDelivery, true
                SocketPhase.Dead, true
            ]

        for platform in platforms do
            for phase, shutDown in phases do
                let socketFd, _, system = systemWith platform phase
                let level, optionName = numbered platform Option.ReuseAddress

                let expected =
                    match SimulatedUnixPlatform.flavour platform, shutDown with
                    | SimulatedUnixFlavour.Darwin, true -> SetSockOptAnswer.Failed UnixError.EINVAL
                    | _ -> SetSockOptAnswer.Set

                for value in [ 1 ; 0 ] do
                    match setWith socketFd level optionName UserBuffer.Mapped 4u value system with
                    | Ok (answer, _) ->
                        if answer <> expected then
                            failwith $"%O{platform} %A{phase} value %d{value}: expected %A{expected}, got %A{answer}"
                    | Error refusal -> failwith $"%O{platform} %A{phase}: refused %A{refusal}"

                // Measured on Darwin: that refusal comes before the copy, so an
                // unmapped value is EINVAL there. The null screen is earlier
                // still, on both.
                let fault = setWith socketFd level optionName unmapped 4u 1 system |> Result.map fst

                match SimulatedUnixPlatform.flavour platform, shutDown with
                | SimulatedUnixFlavour.Darwin, true ->
                    fault |> shouldEqual (Ok (SetSockOptAnswer.Failed UnixError.EINVAL))
                | _ -> fault |> shouldEqual (Ok (SetSockOptAnswer.Failed UnixError.EFAULT))

                setWith socketFd level optionName null' 4u 1 system
                |> Result.map fst
                |> shouldEqual (Ok (SetSockOptAnswer.Failed UnixError.EFAULT))

    /// Measured on both: a connection that completes while the listener has
    /// the option keeps it through a later clear, and one that completes
    /// without it stays without it through a later set. This kernel copies the
    /// option at accept rather than at completion, so it refuses a change while
    /// any connection is queued, and allows one that changes nothing.
    [<Test>]
    let ``a listener with queued connections refuses a change to SO_REUSEADDR`` () : unit =
        let listening (queue : ConnectionId list) : SocketPhase =
            SocketPhase.Listening
                {
                    Backlog = 5
                    Queue = queue
                }

        for platform in platforms do
            let level, optionName = numbered platform Option.ReuseAddress

            for initially in [ false ; true ] do
                let socketFd, _, system = systemWith platform (listening [])
                let system = ReuseAddress.set initially socketFd system

                let queued =
                    { system with
                        Machine =
                            { system.Machine with
                                Sockets =
                                    system.Machine.Sockets
                                    |> Map.change
                                        (SocketId 0L)
                                        (Option.map (fun socket ->
                                            { socket with
                                                Phase = listening [ ConnectionId 0L ]
                                            }
                                        ))
                            }
                    }

                let change = if initially then 0 else 1
                let keep = if initially then 1 else 0

                setWith socketFd level optionName UserBuffer.Mapped 4u change queued
                |> shouldEqual (Error (SocketOptionRefusal.ListenerWithQueuedConnections (SocketId 0L)))

                match setWith socketFd level optionName UserBuffer.Mapped 4u keep queued with
                | Ok (SetSockOptAnswer.Set, after) -> after |> shouldEqual queued
                | other -> failwith $"%O{platform}: re-setting the same value answered %A{other}"

                match setWith socketFd level optionName UserBuffer.Mapped 4u change system with
                | Ok (SetSockOptAnswer.Set, after) -> reuseFlag after |> shouldEqual (not initially)
                | other -> failwith $"%O{platform}: an empty queue answered %A{other}"

    /// Every socket shape this kernel creates takes the option. Measured on
    /// both flavours for every shape `UnixSocket.socket` creates.
    [<Test>]
    let ``every creatable socket takes SO_REUSEADDR`` () : unit =
        let mutable created = 0

        for platform in platforms do
            for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ; SocketDomain.Unix ] do
                for kind in [ SocketKind.Stream ; SocketKind.Datagram ; SocketKind.SeqPacket ] do
                    for protocol in [ SocketProtocol.Default ; SocketProtocol.Tcp ; SocketProtocol.Udp ] do
                        let system : UnixSystem<int, string> = UnixSystem.initial platform

                        let rawDomain, rawKind, rawProtocol =
                            NewSocket.arguments platform domain kind protocol

                        match UnixSocket.socket rawDomain rawKind rawProtocol system with
                        | Ok (Ok (fd, system)) ->
                            created <- created + 1
                            let system = ReuseAddress.set true fd system
                            reuseFlag system |> shouldEqual true
                        | Ok (Error _)
                        | Error _ -> ()

        // Linux's ten and its seqpacket socket, and Darwin's ten.
        created |> shouldEqual 21

    // ------------------------------------------------------------------
    // What the value means, and what reads back
    // ------------------------------------------------------------------

    /// What `getsockopt` reads back for a set flag: Linux answers 1 and Darwin
    /// the option's own bit in `so_options`, which is its number.
    let private readBack (platform : SimulatedUnixPlatform) : int = flavourColumn platform 1 4

    /// Any non-zero value sets the option and zero clears it, whatever was
    /// there before, and it reads back as 0 or as the flavour's one set value.
    /// Measured for 0, 1, 2, 4, -1, 0x100, 0x10000 and 0x1000000.
    [<Test>]
    let ``any non-zero value sets the option, and it reads back as the flavour's set value`` () : unit =
        let property (values : int list) : unit =
            for platform in platforms do
                let socketFd, _, system = systemWith platform SocketPhase.Idle
                let level, optionName = numbered platform Option.ReuseAddress

                values
                |> List.fold
                    (fun system value ->
                        let system =
                            match setWith socketFd level optionName UserBuffer.Mapped 4u value system with
                            | Ok (SetSockOptAnswer.Set, system) -> system
                            | other -> failwith $"%O{platform}: setting %d{value} answered %A{other}"

                        getWith socketFd level optionName UserBuffer.Mapped UserBuffer.Mapped 4u system
                        |> shouldEqual (
                            Ok (GetSockOptAnswer.Reported ((if value = 0 then 0 else readBack platform), 4u))
                        )

                        system
                    )
                    system
                |> ignore<UnixSystem<int, string>>

        Check.One (propertyConfig, property)

    /// A fresh socket has the option clear.
    [<Test>]
    let ``a fresh socket reads back zero`` () : unit =
        for platform in platforms do
            let socketFd, _, system = systemWith platform SocketPhase.Idle
            let level, optionName = numbered platform Option.ReuseAddress

            getWith socketFd level optionName UserBuffer.Mapped UserBuffer.Mapped 4u system
            |> shouldEqual (Ok (GetSockOptAnswer.Reported (0, 4u)))

    type private GetRow =
        {
            Label : string
            Target : Target
            Option : Option
            Value : UserBuffer
            LengthCell : UserBuffer
            Length : uint32
            Linux : Expect
            Darwin : Expect
        }

    let private getRow
        (label : string)
        (target : Target)
        (option : Option)
        (value : UserBuffer)
        (lengthCell : UserBuffer)
        (length : uint32)
        (onLinux : Expect)
        (onDarwin : Expect)
        : GetRow
        =
        {
            Label = label
            Target = target
            Option = option
            Value = value
            LengthCell = lengthCell
            Length = length
            Linux = onLinux
            Darwin = onDarwin
        }

    /// Every `getsockopt` row measured, on a stream socket whose option is set:
    /// label, target, option, value buffer, length cell, the length it holds,
    /// then the Linux and the Darwin answer.
    let private getRows : GetRow list =
        let closed, pipe, port, sock =
            Target.Closed, Target.Pipe, Target.EventPort, Target.Socket

        let reuse = Option.ReuseAddress
        let unknownLevel, unknownOption = Option.UnknownLevel, Option.UnknownAtSocketLevel

        [
            getRow "closed" closed reuse real real 4u ebadf ebadf
            getRow "closed, null length" closed reuse real null' 4u ebadf ebadf
            getRow "pipe" pipe reuse real real 4u enotsock enotsock
            getRow "pipe, null length" pipe reuse real null' 4u enotsock enotsock
            getRow "event port" port reuse real real 4u enotsock enotsock
            getRow "socket, null length" sock reuse real null' 4u efault efault
            getRow "socket, unmapped length" sock reuse real unmapped 4u efault efault
            getRow "socket, null value, null length" sock reuse null' null' 4u efault efault
            getRow "socket, length -1" sock reuse real real minus1 einval ok
            getRow "socket, length 2^31" sock reuse real real 0x8000_0000u einval ok
            getRow "socket, null value, length 4" sock reuse null' real 4u efault ok
            getRow "socket, null value, length 2" sock reuse null' real 2u efault ok
            getRow "socket, null value, length 0" sock reuse null' real 0u ok ok
            getRow "socket, null value, length -1" sock reuse null' real minus1 einval ok
            getRow "socket, unmapped value, length 4" sock reuse unmapped real 4u efault efault
            getRow "socket, unmapped value, length 0" sock reuse unmapped real 0u ok ok
            getRow "socket, unmapped value, length -1" sock reuse unmapped real minus1 einval efault
            getRow "socket, unknown level" sock unknownLevel real real 4u refused refused
            getRow "socket, unknown SOL_SOCKET option" sock unknownOption real real 4u refused refused
            getRow "socket, SO_ERROR" sock Option.SocketError real real 4u refused refused
        ]

    [<Test>]
    let ``getsockopt answers every measured row`` () : unit =
        for platform in platforms do
            for row in getRows do
                let socketFd, portFd, system = systemWith platform SocketPhase.Idle
                let system = ReuseAddress.set true socketFd system
                let level, optionName = numbered platform row.Option
                let fd = targetFd row.Target socketFd portFd
                let expected = flavourColumn platform row.Linux row.Darwin

                let actual =
                    match getWith fd level optionName row.Value row.LengthCell row.Length system with
                    | Ok (GetSockOptAnswer.Reported _) -> Expect.Succeeds
                    | Ok (GetSockOptAnswer.Failed error) -> Expect.Fails error
                    | Error (SocketOptionRefusal.UnmodelledOption (SocketId 0L, l, n)) ->
                        (l, n) |> shouldEqual (level, optionName)
                        Expect.Refused
                    | Error refusal -> failwith $"%O{platform} %s{row.Label}: unexpected refusal %A{refusal}"

                if actual <> expected then
                    failwith $"%O{platform} %s{row.Label}: expected %A{expected}, got %A{actual}"

    /// How much a successful `getsockopt` writes, and what it leaves in the
    /// length cell: the smaller of the declared length and `sizeof(int)`, on
    /// both -- except for a null value buffer on Darwin, which writes nothing and
    /// reports 0 whatever was declared. Measured at 0, 1, 2, 3, 4, 16, 2^31 and
    /// -1 through real storage, and at 0, 2, 4 and -1 through a null value.
    [<Test>]
    let ``a successful getsockopt reports the smaller of the declared length and an int`` () : unit =
        let property (declaredLength : uint32) (viaNull : bool) : unit =
            for platform in platforms do
                let socketFd, _, system = systemWith platform SocketPhase.Idle
                let system = ReuseAddress.set true socketFd system
                let level, optionName = numbered platform Option.ReuseAddress
                let value = if viaNull then null' else UserBuffer.Mapped

                let answer =
                    getWith socketFd level optionName value UserBuffer.Mapped declaredLength system

                let negativeOnLinux =
                    SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux
                    && int declaredLength < 0

                let expected =
                    if negativeOnLinux then
                        GetSockOptAnswer.Failed UnixError.EINVAL
                    elif viaNull then
                        match SimulatedUnixPlatform.flavour platform with
                        | SimulatedUnixFlavour.Darwin -> GetSockOptAnswer.Reported (readBack platform, 0u)
                        | SimulatedUnixFlavour.Linux ->
                            if declaredLength = 0u then
                                GetSockOptAnswer.Reported (readBack platform, 0u)
                            else
                                GetSockOptAnswer.Failed UnixError.EFAULT
                    else
                        GetSockOptAnswer.Reported (readBack platform, min declaredLength 4u)

                answer |> shouldEqual (Ok expected)

        Check.One (propertyConfig, property)

        // The boundaries a generator may not hit.
        for declaredLength in [ 0u ; 3u ; 4u ; 5u ; 0x7fff_ffffu ; 0x8000_0000u ; System.UInt32.MaxValue ] do
            for viaNull in [ false ; true ] do
                property declaredLength viaNull

    /// Darwin refuses to *set* the option on a socket whose connection was
    /// refused, but still reads it.
    [<Test>]
    let ``getsockopt reads the option in every phase`` () : unit =
        for platform in platforms do
            for phase in [ SocketPhase.RefusedPendingDelivery ; SocketPhase.Dead ] do
                let socketFd, _, system = systemWith platform phase
                let level, optionName = numbered platform Option.ReuseAddress

                getWith socketFd level optionName UserBuffer.Mapped UserBuffer.Mapped 4u system
                |> shouldEqual (Ok (GetSockOptAnswer.Reported (0, 4u)))

    // ------------------------------------------------------------------
    // The contract with the caller
    // ------------------------------------------------------------------

    [<Test>]
    let ``supplying a value the admission did not ask for is a caller bug`` () : unit =
        for platform in platforms do
            let socketFd, _, system = systemWith platform SocketPhase.Idle
            let level, optionName = numbered platform Option.ReuseAddress

            let tooMuch =
                Assert.Throws<exn> (fun () ->
                    UnixSocket.setsockopt socketFd level optionName UserBuffer.Mapped 3u (Some 1) system
                    |> ignore<_>
                )

            tooMuch.Message |> shouldContainText "this is a bug in the caller"

            let tooLittle =
                Assert.Throws<exn> (fun () ->
                    UnixSocket.setsockopt socketFd level optionName UserBuffer.Mapped 4u None system
                    |> ignore<_>
                )

            tooLittle.Message |> shouldContainText "this is a bug in the caller"

            let lengthNotRead =
                Assert.Throws<exn> (fun () ->
                    UnixSocket.getsockopt socketFd level optionName UserBuffer.Mapped null' (Some 4u) system
                    |> ignore<_>
                )

            lengthNotRead.Message |> shouldContainText "this is a bug in the caller"

            let lengthRead =
                Assert.Throws<exn> (fun () ->
                    UnixSocket.getsockopt socketFd level optionName UserBuffer.Mapped UserBuffer.Mapped None system
                    |> ignore<_>
                )

            lengthRead.Message |> shouldContainText "this is a bug in the caller"

    // ------------------------------------------------------------------
    // What the option does to bind and listen
    // ------------------------------------------------------------------

    let private inetFamily : int option =
        Some SimulatedUnixPlatform.internetAddressFamily

    let private bindAt
        (fd : int)
        (endpoint : InternetEndpoint)
        (system : UnixSystem<int, string>)
        : BindAnswer * UnixSystem<int, string>
        =
        match UnixSocket.bind fd UserBuffer.Mapped 16 inetFamily (Some endpoint) system with
        | Ok result -> result
        | Error refusal -> failwith $"expected an answer, got a refusal: %s{BindRefusal.describe refusal}"

    let private boundAt (fd : int) (endpoint : InternetEndpoint) (system : UnixSystem<int, string>) =
        match bindAt fd endpoint system with
        | BindAnswer.Bound _, system -> system
        | BindAnswer.Failed error, _ -> failwith $"expected the bind to succeed, got %O{error}"

    let private stream (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

    let private bindsOk (answer : BindAnswer) : bool =
        match answer with
        | BindAnswer.Bound _ -> true
        | BindAnswer.Failed UnixError.EADDRINUSE -> false
        | BindAnswer.Failed other -> failwith $"expected success or EADDRINUSE, got %O{other}"

    /// A bind reads every socket's flag as it stands now, not as it stood when
    /// that socket was bound: clearing it on a bound socket is seen by the next
    /// bind, and so is setting it after the bind. Each row measured.
    [<Test>]
    let ``bind reads SO_REUSEADDR as setsockopt last left it`` () : unit =
        let port = 40000us

        // Two reuse-carrying sockets share the exact endpoint (Linux only);
        // then the first clears the flag, and a third reuse-carrying socket
        // may no longer join them.
        for platform in platforms do
            let system : UnixSystem<int, string> = UnixSystem.initial platform
            let a, system = stream system
            let system = ReuseAddress.set true a system |> boundAt a (loopback port)
            let b, system = stream system
            let system = ReuseAddress.set true b system
            let answer, system = bindAt b (loopback port) system
            bindsOk answer |> shouldEqual (flavourColumn platform true false)
            let system = ReuseAddress.set false a system
            let c, system = stream system
            let system = ReuseAddress.set true c system
            bindAt c (loopback port) system |> fst |> bindsOk |> shouldEqual false

        // A bound without the flag, then setting it: a reuse-carrying
        // candidate may now share the endpoint on Linux.
        for platform in platforms do
            let system : UnixSystem<int, string> = UnixSystem.initial platform
            let a, system = stream system
            let system = boundAt a (loopback port) system
            let system = ReuseAddress.set true a system
            let c, system = stream system
            let system = ReuseAddress.set true c system

            bindAt c (loopback port) system
            |> fst
            |> bindsOk
            |> shouldEqual (flavourColumn platform true false)

        // A reuse-carrying wildcard, cleared, and a reuse-carrying specific
        // candidate on its port: Linux reads the cleared flag and refuses,
        // Darwin keys on the candidate's flag alone and admits.
        for platform in platforms do
            let system : UnixSystem<int, string> = UnixSystem.initial platform
            let a, system = stream system
            let system = ReuseAddress.set true a system |> boundAt a (wildcard port)
            let c, system = stream system
            let system = ReuseAddress.set true c system
            bindAt c (loopback port) system |> fst |> bindsOk |> shouldEqual true
            let system = ReuseAddress.set false a system

            bindAt c (loopback port) system
            |> fst
            |> bindsOk
            |> shouldEqual (flavourColumn platform false true)

    /// Linux's `listen(2)` asks the conflict question again, with the flags as
    /// they stand: two reuse-carrying sockets share an endpoint, one clears the
    /// flag, and the other can no longer listen. Measured; Darwin's pair could
    /// not share the endpoint in the first place.
    [<Test>]
    let ``Linux's listen reads SO_REUSEADDR as setsockopt last left it`` () : unit =
        let system : UnixSystem<int, string> = UnixSystem.initial linux
        let a, system = stream system
        let system = ReuseAddress.set true a system |> boundAt a (loopback 40000us)
        let b, system = stream system
        let system = ReuseAddress.set true b system |> boundAt b (loopback 40000us)
        let system = ReuseAddress.set false b system

        match UnixSocket.listen a 5 system with
        | Ok (answer, _) -> answer |> shouldEqual (ListenAnswer.Failed UnixError.EADDRINUSE)
        | Error refusal -> failwith $"expected an answer, got %s{ListenRefusal.describe refusal}"
