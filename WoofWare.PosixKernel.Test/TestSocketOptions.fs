namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `setsockopt(2)` and `getsockopt(2)` of `TCP_NODELAY`, `IPV6_V6ONLY`,
/// `SO_LINGER` and Darwin's `SO_LINGER_SEC`: the numbering, the defaults, what
/// a set stores and a get reads back, the errno for a socket the option does
/// not apply to, and the length and buffer rules each kernel's protocol layers
/// add to the ones `TestSockOpt` carries.
///
/// The rows are literals of the measurements, per flavour: Linux 6.18.5 (arm64,
/// under Apple's `container`) and Darwin 27.0.0, with
/// `docs/probes/sockopt-options/`, outputs beside the probes.
/// `TestSockOptAgainstHost` puts random inputs to the kernel running the suite
/// and to the model.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketOptions =

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64
    let private platforms : SimulatedUnixPlatform list = [ linux ; darwin ]

    let private flavourColumn (platform : SimulatedUnixPlatform) (onLinux : 'a) (onDarwin : 'a) : 'a =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> onLinux
        | SimulatedUnixFlavour.Darwin -> onDarwin

    [<RequireQualifiedAccess>]
    type Option =
        | NoDelay
        | Ipv6Only
        | Linger
        | LingerSeconds

    let private numbered (platform : SimulatedUnixPlatform) (option : Option) : int * int =
        match option with
        | Option.NoDelay -> SimulatedUnixPlatform.tcpOptionLevel platform, SimulatedUnixPlatform.noDelayOption platform
        | Option.Ipv6Only ->
            SimulatedUnixPlatform.ipv6OptionLevel platform, SimulatedUnixPlatform.ipv6OnlyOption platform
        | Option.Linger -> SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.lingerOption platform
        | Option.LingerSeconds ->
            SimulatedUnixPlatform.socketOptionLevel platform,
            Option.get (SimulatedUnixPlatform.lingerSecondsOption platform)

    let private fresh (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot

    let private socketOf
        (domain : SocketDomain)
        (kind : SocketKind)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        NewSocket.create domain kind SocketProtocol.Default system

    /// A set as a client makes it: the admission, then the bytes the copy takes
    /// from `value`.
    let private set
        (option : Option)
        (fd : int)
        (buffer : UserBuffer)
        (length : uint32)
        (value : ImmutableArray<byte>)
        (system : UnixSystem<int, string>)
        : Result<SetSockOptAnswer * UnixSystem<int, string>, SocketOptionRefusal>
        =
        let level, name = numbered system.Machine.UnixPlatform option

        let supplied =
            match UnixSocket.admitSetSockOpt fd level name buffer length system with
            | Ok (SetSockOptAdmission.Transfer count) -> Some (ImmutableArray.Create (value, 0, count))
            | Ok SetSockOptAdmission.NoCopy
            | Ok (SetSockOptAdmission.Answered _)
            | Error _ -> None

        UnixSocket.setsockopt fd level name buffer length supplied system

    /// A set through real storage at the option's own size, which must succeed.
    let private setOk (option : Option) (fd : int) (value : ImmutableArray<byte>) (system : UnixSystem<int, string>) =
        match set option fd UserBuffer.Mapped (uint32 value.Length) value system with
        | Ok (SetSockOptAnswer.Set, system) -> system
        | other -> failwith $"setting %A{option} on fd %d{fd} to %A{value} answered %A{other}"

    let private get
        (option : Option)
        (fd : int)
        (value : UserBuffer)
        (length : UserBuffer)
        (declared : uint32)
        (system : UnixSystem<int, string>)
        : GetSockOptAnswer
        =
        let level, name = numbered system.Machine.UnixPlatform option

        let read =
            match UnixSocket.admitGetSockOpt fd level name value length system with
            | Ok GetSockOptAdmission.ReadLength -> Some declared
            | _ -> None

        match UnixSocket.getsockopt fd level name value length read system with
        | Ok (answer, _) -> answer
        | Error refusal -> failwith $"getsockopt refused: %s{SocketOptionRefusal.describe refusal}"

    /// A read through real storage of eight bytes.
    let private read (option : Option) (fd : int) (system : UnixSystem<int, string>) : GetSockOptAnswer =
        get option fd UserBuffer.Mapped UserBuffer.Mapped 8u system

    let private int' (value : int) : GetSockOptAnswer = OptionValue.reported value 4u

    let private linger (onOff : int) (time : int) : GetSockOptAnswer =
        GetSockOptAnswer.Reported (OptionValue.ofLinger onOff time)

    let private failed (error : UnixError) : GetSockOptAnswer = GetSockOptAnswer.Failed (error, None)

    // ------------------------------------------------------------------
    // Numbering and defaults
    // ------------------------------------------------------------------

    [<Test>]
    let ``the options are numbered as each kernel numbers them`` () : unit =
        numbered linux Option.NoDelay |> shouldEqual (6, 1)
        numbered darwin Option.NoDelay |> shouldEqual (6, 1)
        numbered linux Option.Ipv6Only |> shouldEqual (41, 26)
        numbered darwin Option.Ipv6Only |> shouldEqual (41, 27)
        numbered linux Option.Linger |> shouldEqual (1, 13)
        numbered darwin Option.Linger |> shouldEqual (0xffff, 0x80)
        numbered darwin Option.LingerSeconds |> shouldEqual (0xffff, 0x1080)
        SimulatedUnixPlatform.lingerSecondsOption linux |> shouldEqual None

    [<TestCaseSource(nameof platforms)>]
    let ``every option starts off, with a linger time of zero`` (platform : SimulatedUnixPlatform) : unit =
        let system = fresh platform
        let tcp, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let tcp6, system = socketOf SocketDomain.Inet6 SocketKind.Stream system
        let udp, system = socketOf SocketDomain.Inet SocketKind.Datagram system
        let unix, system = socketOf SocketDomain.Unix SocketKind.Stream system

        read Option.NoDelay tcp system |> shouldEqual (int' 0)
        read Option.NoDelay tcp6 system |> shouldEqual (int' 0)
        read Option.Ipv6Only tcp6 system |> shouldEqual (int' 0)

        for fd in [ tcp ; tcp6 ; udp ; unix ] do
            read Option.Linger fd system |> shouldEqual (linger 0 0)

    [<TestCaseSource(nameof platforms)>]
    let ``a new IPv6 socket takes IPV6_V6ONLY from the machine's sysctl`` (platform : SimulatedUnixPlatform) : unit =
        let system =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withIpv6OnlyByDefault true
            |> UnixBootImage.boot

        let tcp6, system = socketOf SocketDomain.Inet6 SocketKind.Stream system
        let udp6, system = socketOf SocketDomain.Inet6 SocketKind.Datagram system
        read Option.Ipv6Only tcp6 system |> shouldEqual (int' 1)
        read Option.Ipv6Only udp6 system |> shouldEqual (int' 1)

    // ------------------------------------------------------------------
    // Values
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``any non-zero int turns TCP_NODELAY and IPV6_V6ONLY on, and each reads back its kernel's own one``
        (platform : SimulatedUnixPlatform)
        =
        for value in [ 1 ; 2 ; 4 ; -1 ; 0x100 ; 0x10000 ; System.Int32.MinValue ] do
            let system = fresh platform
            let tcp, system = socketOf SocketDomain.Inet SocketKind.Stream system
            let tcp6, system = socketOf SocketDomain.Inet6 SocketKind.Stream system
            let system = setOk Option.NoDelay tcp (OptionValue.ofInt value) system
            let system = setOk Option.Ipv6Only tcp6 (OptionValue.ofInt value) system

            // Darwin reports TCP_NODELAY as its flag's bit.
            read Option.NoDelay tcp system
            |> shouldEqual (int' (flavourColumn platform 1 4))

            read Option.Ipv6Only tcp6 system |> shouldEqual (int' 1)

            let system = setOk Option.NoDelay tcp (OptionValue.ofInt 0) system
            let system = setOk Option.Ipv6Only tcp6 (OptionValue.ofInt 0) system
            read Option.NoDelay tcp system |> shouldEqual (int' 0)
            read Option.Ipv6Only tcp6 system |> shouldEqual (int' 0)

    /// Each row: the `struct linger` set, then what SO_LINGER reads back.
    let private lingerRows : (int * int * GetSockOptAnswer * GetSockOptAnswer) list =
        [
            0, 0, linger 0 0, linger 0 0
            1, 0, linger 1 0, linger 1 0
            1, 5, linger 1 5, linger 1 5
            2, 7, linger 1 7, linger 1 7
            -1, 3, linger 1 3, linger 1 3
            // Linux drops the time when lingering is off; Darwin keeps it.
            0, 5, linger 0 0, linger 0 5
            0, -1, linger 0 0, linger 0 -1
            1, 32767, linger 1 32767, linger 1 32767
            1, System.Int32.MaxValue, linger 1 System.Int32.MaxValue, linger 0 0
            // Darwin stores sixteen bits.
            0, 40000, linger 0 0, linger 0 -25536
            0, 70000, linger 0 0, linger 0 4464
        ]

    [<TestCaseSource(nameof platforms)>]
    let ``SO_LINGER reads back what each kernel stored`` (platform : SimulatedUnixPlatform) : unit =
        for onOff, time, onLinux, onDarwin in lingerRows do
            let system = fresh platform
            let fd, system = socketOf SocketDomain.Inet SocketKind.Stream system

            let system =
                match set Option.Linger fd UserBuffer.Mapped 8u (OptionValue.ofLinger onOff time) system with
                | Ok (_, system) -> system
                | Error refusal -> failwith $"{{%d{onOff}, %d{time}}}: %s{SocketOptionRefusal.describe refusal}"

            let actual = read Option.Linger fd system
            let expected = flavourColumn platform onLinux onDarwin

            if actual <> expected then
                failwith $"%O{platform} {{%d{onOff}, %d{time}}}: expected %A{expected}, got %A{actual}"

    [<Test>]
    let ``Linux keeps the earlier linger time across a set that turns lingering off`` () : unit =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh linux)
        let system = setOk Option.Linger fd (OptionValue.ofLinger 1 9) system
        let system = setOk Option.Linger fd (OptionValue.ofLinger 0 4) system
        read Option.Linger fd system |> shouldEqual (linger 0 9)

    [<Test>]
    let ``Linux refuses to say what a negative linger time reads back as`` () : unit =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh linux)

        match set Option.Linger fd UserBuffer.Mapped 8u (OptionValue.ofLinger 1 -1) system with
        | Error (SocketOptionRefusal.NegativeLingerTime (_, -1)) -> ()
        | other -> failwith $"expected a refusal, got %A{other}"

    [<Test>]
    let ``Darwin's SO_LINGER is hundredths of a second, and SO_LINGER_SEC whole seconds of it`` () : unit =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh darwin)

        let system = setOk Option.LingerSeconds fd (OptionValue.ofLinger 1 5) system
        read Option.Linger fd system |> shouldEqual (linger 1 500)
        read Option.LingerSeconds fd system |> shouldEqual (linger 1 5)

        let system = setOk Option.Linger fd (OptionValue.ofLinger 1 327) system
        read Option.Linger fd system |> shouldEqual (linger 1 327)
        read Option.LingerSeconds fd system |> shouldEqual (linger 1 3)

        // The product wraps as a 32-bit `int` and is cut to sixteen bits; the
        // quotient rounds toward zero.
        let system = setOk Option.LingerSeconds fd (OptionValue.ofLinger 0 400) system
        read Option.Linger fd system |> shouldEqual (linger 0 -25536)
        read Option.LingerSeconds fd system |> shouldEqual (linger 0 -255)

        let system =
            setOk Option.LingerSeconds fd (OptionValue.ofLinger 0 System.Int32.MaxValue) system

        read Option.Linger fd system |> shouldEqual (linger 0 -100)
        read Option.LingerSeconds fd system |> shouldEqual (linger 0 -1)

    [<Test>]
    let ``Darwin answers EDOM for a time outside what fits, when turning lingering on, and changes nothing`` () : unit =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh darwin)
        let system = setOk Option.Linger fd (OptionValue.ofLinger 1 9) system

        let rows =
            [
                Option.Linger, -1
                Option.Linger, 32768
                Option.Linger, System.Int32.MaxValue
                Option.LingerSeconds, -1
                Option.LingerSeconds, 328
                // 42949673 * 100 wraps to 4, which would fit.
                Option.LingerSeconds, 42949673
            ]

        for option, time in rows do
            match set option fd UserBuffer.Mapped 8u (OptionValue.ofLinger 1 time) system with
            | Ok (SetSockOptAnswer.Failed UnixError.EDOM, after) -> after |> shouldEqual system
            | other -> failwith $"%A{option} {{1, %d{time}}}: expected EDOM, got %A{other}"

        match set Option.LingerSeconds fd UserBuffer.Mapped 8u (OptionValue.ofLinger 1 327) system with
        | Ok (SetSockOptAnswer.Set, system) -> read Option.Linger fd system |> shouldEqual (linger 1 32700)
        | other -> failwith $"327 seconds: expected a set, got %A{other}"

    // ------------------------------------------------------------------
    // Sockets the option does not apply to
    // ------------------------------------------------------------------

    /// Each row: the socket, the option, then the set's and the get's errno on
    /// Linux and on Darwin.
    let private kindRows : (SocketDomain * SocketKind * Option * (UnixError * UnixError) * (UnixError * UnixError)) list =
        [
            SocketDomain.Inet,
            SocketKind.Datagram,
            Option.NoDelay,
            (UnixError.ENOPROTOOPT, UnixError.EOPNOTSUPP),
            (UnixError.EINVAL, UnixError.EINVAL)
            SocketDomain.Inet6,
            SocketKind.Datagram,
            Option.NoDelay,
            (UnixError.ENOPROTOOPT, UnixError.ENOPROTOOPT),
            (UnixError.EINVAL, UnixError.EINVAL)
            SocketDomain.Unix,
            SocketKind.Stream,
            Option.NoDelay,
            (UnixError.EOPNOTSUPP, UnixError.EOPNOTSUPP),
            (UnixError.EOPNOTSUPP, UnixError.ENOTCONN)
            SocketDomain.Unix,
            SocketKind.Datagram,
            Option.NoDelay,
            (UnixError.EOPNOTSUPP, UnixError.EOPNOTSUPP),
            (UnixError.EOPNOTSUPP, UnixError.EINVAL)
            SocketDomain.Inet,
            SocketKind.Stream,
            Option.Ipv6Only,
            (UnixError.ENOPROTOOPT, UnixError.EOPNOTSUPP),
            (UnixError.EINVAL, UnixError.EINVAL)
            SocketDomain.Inet,
            SocketKind.Datagram,
            Option.Ipv6Only,
            (UnixError.ENOPROTOOPT, UnixError.EOPNOTSUPP),
            (UnixError.EINVAL, UnixError.EINVAL)
            SocketDomain.Unix,
            SocketKind.Stream,
            Option.Ipv6Only,
            (UnixError.EOPNOTSUPP, UnixError.EOPNOTSUPP),
            (UnixError.EOPNOTSUPP, UnixError.EOPNOTSUPP)
            SocketDomain.Unix,
            SocketKind.Datagram,
            Option.Ipv6Only,
            (UnixError.EOPNOTSUPP, UnixError.EOPNOTSUPP),
            (UnixError.EOPNOTSUPP, UnixError.EOPNOTSUPP)
        ]

    [<TestCaseSource(nameof platforms)>]
    let ``an option the socket does not have answers its kernel's errno, ahead of the length and the buffers``
        (platform : SimulatedUnixPlatform)
        =
        for domain, kind, option, onLinux, onDarwin in kindRows do
            let fd, system = socketOf domain kind (fresh platform)
            let setErrno, getErrno = flavourColumn platform onLinux onDarwin
            let describe = $"%O{platform} %A{domain} %A{kind} %A{option}"

            // A short length and an unmapped value do not get a look in.
            for buffer, length in
                [
                    UserBuffer.Mapped, 4u
                    UserBuffer.Mapped, 1u
                    UserBuffer.Unmapped 0x1000UL, 4u
                ] do
                match set option fd buffer length (OptionValue.ofInt 1) system with
                | Ok (SetSockOptAnswer.Failed error, _) when error = setErrno -> ()
                | other -> failwith $"%s{describe}: set %A{buffer} %d{length}: expected %O{setErrno}, got %A{other}"

            get option fd UserBuffer.Mapped UserBuffer.Mapped 4u system
            |> shouldEqual (failed getErrno)

            // Linux answers before it reads the cell; so does Darwin through a
            // null value buffer, but through any other it reads the cell first.
            get option fd UserBuffer.Mapped (UserBuffer.Unmapped 0UL) 4u system
            |> shouldEqual (failed (flavourColumn platform getErrno UnixError.EFAULT))

            get option fd (UserBuffer.Unmapped 0UL) (UserBuffer.Unmapped 0UL) 4u system
            |> shouldEqual (failed getErrno)

    [<Test>]
    let ``Linux's Unix-domain SOCK_SEQPACKET socket has neither TCP_NODELAY nor IPV6_V6ONLY`` () : unit =
        let fd, system = socketOf SocketDomain.Unix SocketKind.SeqPacket (fresh linux)

        for option in [ Option.NoDelay ; Option.Ipv6Only ] do
            match set option fd UserBuffer.Mapped 4u (OptionValue.ofInt 1) system with
            | Ok (SetSockOptAnswer.Failed UnixError.EOPNOTSUPP, _) -> ()
            | other -> failwith $"%A{option}: expected EOPNOTSUPP, got %A{other}"

            read option fd system |> shouldEqual (failed UnixError.EOPNOTSUPP)

        read Option.Linger fd system |> shouldEqual (linger 0 0)

    // ------------------------------------------------------------------
    // Lengths and buffers
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``a set takes at least the option's size, and Darwin's IPV6_V6ONLY exactly it``
        (platform : SimulatedUnixPlatform)
        =
        let system = fresh platform
        let tcp, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let tcp6, system = socketOf SocketDomain.Inet6 SocketKind.Stream system

        let value =
            ImmutableArray.CreateRange (Array.append (OptionValue.ofLinger 1 3 |> Seq.toArray) (Array.zeroCreate 8))

        let answer option fd length =
            match set option fd UserBuffer.Mapped length value system with
            | Ok (SetSockOptAnswer.Set, _) -> None
            | Ok (SetSockOptAnswer.Failed error, _) -> Some error
            | Error refusal -> failwith (SocketOptionRefusal.describe refusal)

        for length in [ 0u ; 1u ; 3u ] do
            answer Option.NoDelay tcp length |> shouldEqual (Some UnixError.EINVAL)
            answer Option.Ipv6Only tcp6 length |> shouldEqual (Some UnixError.EINVAL)

        for length in [ 0u ; 4u ; 7u ] do
            answer Option.Linger tcp length |> shouldEqual (Some UnixError.EINVAL)

        for length in [ 4u ; 5u ; 16u ; 0x7fff_ffffu ] do
            answer Option.NoDelay tcp length |> shouldEqual None

        for length in [ 8u ; 9u ; 16u ; 0x7fff_ffffu ] do
            answer Option.Linger tcp length |> shouldEqual None

        answer Option.Ipv6Only tcp6 4u |> shouldEqual None

        for length in [ 5u ; 8u ; 0x7fff_ffffu ] do
            answer Option.Ipv6Only tcp6 length
            |> shouldEqual (flavourColumn platform None (Some UnixError.EINVAL))

        // Linux reads the length as an `int`; Darwin does not.
        answer Option.NoDelay tcp 0x8000_0000u
        |> shouldEqual (flavourColumn platform (Some UnixError.EINVAL) None)

    [<Test>]
    let ``Linux asks SO_LINGER for an int, copies it, and only then for the rest`` () : unit =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh linux)

        for length in [ 4u ; 7u ] do
            match set Option.Linger fd (UserBuffer.Unmapped 0x1000UL) length (OptionValue.ofLinger 1 1) system with
            | Ok (SetSockOptAnswer.Failed UnixError.EFAULT, _) -> ()
            | other -> failwith $"length %d{length}, unmapped: expected EFAULT, got %A{other}"

        match set Option.Linger fd (UserBuffer.Unmapped 0x1000UL) 3u (OptionValue.ofLinger 1 1) system with
        | Ok (SetSockOptAnswer.Failed UnixError.EINVAL, _) -> ()
        | other -> failwith $"length 3, unmapped: expected EINVAL, got %A{other}"

    [<Test>]
    let ``Darwin answers SO_LINGER's short length before it copies`` () : unit =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh darwin)

        match set Option.Linger fd (UserBuffer.Unmapped 0x1000UL) 4u (OptionValue.ofLinger 1 1) system with
        | Ok (SetSockOptAnswer.Failed UnixError.EINVAL, _) -> ()
        | other -> failwith $"expected EINVAL, got %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``Linux sets IPV6_V6ONLY to 0 through a null value; Darwin answers EFAULT`` (platform : SimulatedUnixPlatform) =
        let fd, system = socketOf SocketDomain.Inet6 SocketKind.Stream (fresh platform)
        let system = setOk Option.Ipv6Only fd (OptionValue.ofInt 1) system

        match set Option.Ipv6Only fd (UserBuffer.Unmapped 0UL) 4u (OptionValue.ofInt 1) system, platform with
        | Ok (SetSockOptAnswer.Set, after), _ when platform = linux ->
            read Option.Ipv6Only fd after |> shouldEqual (int' 0)
        | Ok (SetSockOptAnswer.Failed UnixError.EFAULT, after), _ when platform = darwin -> after |> shouldEqual system
        | other, _ -> failwith $"%O{platform}: got %A{other}"

        // A short length still comes first on Linux.
        match set Option.Ipv6Only fd (UserBuffer.Unmapped 0UL) 1u (OptionValue.ofInt 1) system with
        | Ok (SetSockOptAnswer.Failed UnixError.EINVAL, _) when platform = linux -> ()
        | Ok (SetSockOptAnswer.Failed UnixError.EFAULT, _) when platform = darwin -> ()
        | other -> failwith $"%O{platform}, length 1: got %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``a get copies the smaller of the declared length and the option's size`` (platform : SimulatedUnixPlatform) =
        let system = fresh platform
        let tcp, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let tcp6, system = socketOf SocketDomain.Inet6 SocketKind.Stream system
        let system = setOk Option.NoDelay tcp (OptionValue.ofInt 1) system
        let system = setOk Option.Ipv6Only tcp6 (OptionValue.ofInt 1) system
        let system = setOk Option.Linger tcp (OptionValue.ofLinger 1 5) system
        let noDelay = flavourColumn platform 1 4

        for declared in [ 0u ; 1u ; 3u ; 4u ; 5u ; 8u ; 16u ; 0x7fff_ffffu ] do
            get Option.NoDelay tcp UserBuffer.Mapped UserBuffer.Mapped declared system
            |> shouldEqual (OptionValue.reported noDelay (min declared 4u))

            get Option.Ipv6Only tcp6 UserBuffer.Mapped UserBuffer.Mapped declared system
            |> shouldEqual (OptionValue.reported 1 (min declared 4u))

            get Option.Linger tcp UserBuffer.Mapped UserBuffer.Mapped declared system
            |> shouldEqual (
                GetSockOptAnswer.Reported (ImmutableArray.Create (OptionValue.ofLinger 1 5, 0, int (min declared 8u)))
            )

        // Linux's socket layer and TCP refuse a negative length; its IPv6 layer
        // reads it as unsigned, as Darwin does everywhere.
        for declared in [ 0x8000_0000u ; System.UInt32.MaxValue ] do
            get Option.NoDelay tcp UserBuffer.Mapped UserBuffer.Mapped declared system
            |> shouldEqual (flavourColumn platform (failed UnixError.EINVAL) (int' noDelay))

            get Option.Linger tcp UserBuffer.Mapped UserBuffer.Mapped declared system
            |> shouldEqual (flavourColumn platform (failed UnixError.EINVAL) (linger 1 5))

            get Option.Ipv6Only tcp6 UserBuffer.Mapped UserBuffer.Mapped declared system
            |> shouldEqual (int' 1)

    [<TestCaseSource(nameof platforms)>]
    let ``a faulting value leaves the length Linux's TCP and IPv6 layers wrote first, and nothing else``
        (platform : SimulatedUnixPlatform)
        =
        let system = fresh platform
        let tcp, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let tcp6, system = socketOf SocketDomain.Inet6 SocketKind.Stream system
        let unmapped = UserBuffer.Unmapped 0x1000UL

        for declared in [ 1u ; 4u ; 16u ] do
            let written = flavourColumn platform (Some (min declared 4u)) None

            get Option.NoDelay tcp unmapped UserBuffer.Mapped declared system
            |> shouldEqual (GetSockOptAnswer.Failed (UnixError.EFAULT, written))

            get Option.Ipv6Only tcp6 unmapped UserBuffer.Mapped declared system
            |> shouldEqual (GetSockOptAnswer.Failed (UnixError.EFAULT, written))

            get Option.Linger tcp unmapped UserBuffer.Mapped declared system
            |> shouldEqual (GetSockOptAnswer.Failed (UnixError.EFAULT, None))

        // Darwin reads nothing through a null value and writes back 0.
        get Option.NoDelay tcp (UserBuffer.Unmapped 0UL) UserBuffer.Mapped 4u system
        |> shouldEqual (
            flavourColumn
                platform
                (GetSockOptAnswer.Failed (UnixError.EFAULT, Some 4u))
                (GetSockOptAnswer.Reported ImmutableArray.Empty)
        )

    // ------------------------------------------------------------------
    // Phases
    // ------------------------------------------------------------------

    let private listening (platform : SimulatedUnixPlatform) =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh platform)

        let system =
            match
                CopyIn.bind
                    fd
                    UserBuffer.Mapped
                    16u
                    (CopyIn.inet platform (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 5000us))
                    system
            with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"bind answered %A{other}"

        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> fd, system
        | other -> failwith $"listen answered %A{other}"

    let private connectTo (port : uint16) (fd : int) (system : UnixSystem<int, string>) =
        let platform = system.Machine.UnixPlatform

        match
            CopyIn.connect
                fd
                UserBuffer.Mapped
                16u
                (CopyIn.inet platform (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port))
                system
        with
        | Ok (_, system) -> system
        | Error refusal -> failwith (ConnectRefusal.describe refusal)

    [<TestCaseSource(nameof platforms)>]
    let ``an accepted socket has its listener's options`` (platform : SimulatedUnixPlatform) : unit =
        let listener, system = listening platform
        let system = setOk Option.NoDelay listener (OptionValue.ofInt 1) system
        let system = setOk Option.Linger listener (OptionValue.ofLinger 1 5) system
        let client, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let system = connectTo 5000us client system

        let accepted, system =
            match UnixConnection.accept 0 listener UserBuffer.Mapped 16u system with
            | Ok (AcceptOutcome.Accepted (fd, _, _), system) -> fd, system
            | other -> failwith $"accept answered %A{other}"

        read Option.NoDelay accepted system
        |> shouldEqual (int' (flavourColumn platform 1 4))

        read Option.Linger accepted system |> shouldEqual (linger 1 5)
        // The client's own are its own.
        read Option.NoDelay client system |> shouldEqual (int' 0)

    [<TestCaseSource(nameof platforms)>]
    let ``changing an option on a listener with connections queued is refused`` (platform : SimulatedUnixPlatform) =
        let listener, system = listening platform
        let client, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let system = connectTo 5000us client system

        for option, value in
            [
                Option.NoDelay, OptionValue.ofInt 1
                Option.Linger, OptionValue.ofLinger 1 5
            ] do
            match set option listener UserBuffer.Mapped (uint32 value.Length) value system with
            | Error (SocketOptionRefusal.ListenerWithQueuedConnections _) -> ()
            | other -> failwith $"%A{option}: expected a refusal, got %A{other}"

        // Setting what it already holds changes nothing a connection copied.
        match set Option.NoDelay listener UserBuffer.Mapped 4u (OptionValue.ofInt 0) system with
        | Ok (SetSockOptAnswer.Set, after) -> after |> shouldEqual system
        | other -> failwith $"expected a set, got %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``after a refused connect Darwin refuses every set, and Linux takes them`` (platform : SimulatedUnixPlatform) =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh platform)
        let system = connectTo 5999us fd system

        for option, value in
            [
                Option.NoDelay, OptionValue.ofInt 1
                Option.Linger, OptionValue.ofLinger 1 5
            ] do
            match set option fd UserBuffer.Mapped (uint32 value.Length) value system with
            | Ok (SetSockOptAnswer.Set, _) when platform = linux -> ()
            | Ok (SetSockOptAnswer.Failed UnixError.EINVAL, _) when platform = darwin -> ()
            | other -> failwith $"%O{platform} %A{option}: got %A{other}"

            read option fd system
            |> shouldEqual (
                match option with
                | Option.Linger -> linger 0 0
                | _ -> int' 0
            )

    /// No IPv6 socket here can take an address yet, so the socket's state is
    /// written by hand: bound, listening, and connected, as the probe made them.
    [<TestCaseSource(nameof platforms)>]
    let ``IPV6_V6ONLY changes only while the socket has no address`` (platform : SimulatedUnixPlatform) : unit =
        let fd, system = socketOf SocketDomain.Inet6 SocketKind.Stream (fresh platform)
        let socketId = SocketId 0L

        let withState (binding : SocketBinding option) (phase : SocketPhase) =
            { system with
                Machine =
                    { system.Machine with
                        Sockets =
                            Map.add
                                socketId
                                { UnixMachineState.socket socketId system.Machine with
                                    Binding = binding
                                    Phase = phase
                                }
                                system.Machine.Sockets
                    }
            }

        let bound =
            Some
                {
                    Endpoint = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 5000us
                    LockedAddress = None
                    LockedPort = true
                }

        let states =
            [
                "bound", withState bound SocketPhase.Idle
                "listening",
                withState
                    bound
                    (SocketPhase.Listening
                        {
                            Backlog = 4
                            Queue = []
                            Drained = false
                        })
            ]

        for label, system in states do
            for value in [ 0 ; 1 ] do
                match set Option.Ipv6Only fd UserBuffer.Mapped 4u (OptionValue.ofInt value) system with
                | Ok (SetSockOptAnswer.Failed UnixError.EINVAL, after) -> after |> shouldEqual system
                | other -> failwith $"%O{platform} %s{label}, set %d{value}: expected EINVAL, got %A{other}"

            // After the copy: a faulting value is EFAULT first.
            match set Option.Ipv6Only fd (UserBuffer.Unmapped 0x1000UL) 4u (OptionValue.ofInt 1) system with
            | Ok (SetSockOptAnswer.Failed UnixError.EFAULT, _) -> ()
            | other -> failwith $"%O{platform} %s{label}, unmapped: expected EFAULT, got %A{other}"

    [<TestCaseSource(nameof platforms)>]
    let ``a close that would reset a connection is refused, and every other close is orderly``
        (platform : SimulatedUnixPlatform)
        =
        let listener, system = listening platform
        let client, system = socketOf SocketDomain.Inet SocketKind.Stream system
        let system = connectTo 5000us client system

        let accepted, system =
            match UnixConnection.accept 0 listener UserBuffer.Mapped 16u system with
            | Ok (AcceptOutcome.Accepted (fd, _, _), system) -> fd, system
            | other -> failwith $"accept answered %A{other}"

        let close (fd : int) (system : UnixSystem<int, string>) =
            match UnixDescriptor.close fd system with
            | Ok (SyscallAnswer.Completed _, system) -> Ok system
            | Ok (SyscallAnswer.Failed error, _) -> failwith $"close answered %O{error}"
            | Error refusal -> Error refusal

        let refusedClose (fd : int) (system : UnixSystem<int, string>) =
            match close fd system with
            | Error (CloseRefusal.Release (DescriptionReleaseRefusal.AbortiveClose _)) -> ()
            | other -> failwith $"%O{platform} fd %d{fd}: expected a refusal, got %A{other}"

        // Lingering for no time resets the peer, connected or still queued.
        let zero = setOk Option.Linger client (OptionValue.ofLinger 1 0) system
        refusedClose client zero

        let queuedClient, queued = socketOf SocketDomain.Inet SocketKind.Stream zero
        let queued = connectTo 5000us queuedClient queued
        let queued = setOk Option.Linger queuedClient (OptionValue.ofLinger 1 0) queued
        refusedClose queuedClient queued

        // Lingering for some time, or not at all, is an orderly close.
        for onOff, time in [ 1, 1 ; 0, 0 ] do
            let system = setOk Option.Linger client (OptionValue.ofLinger onOff time) system

            match close client system with
            | Ok _ -> ()
            | Error refusal -> failwith $"{{%d{onOff}, %d{time}}}: %s{CloseRefusal.describe refusal}"

        // Once the peer has gone, there is nothing to reset.
        match close accepted zero with
        | Ok system ->
            match close client system with
            | Ok _ -> ()
            | Error refusal -> failwith $"after the peer closed: %s{CloseRefusal.describe refusal}"
        | Error refusal -> failwith $"closing the peer: %s{CloseRefusal.describe refusal}"

    [<Test>]
    let ``a Linux accept that drops a connection whose end would reset it is refused`` () : unit =
        let withLinger (time : int) =
            let listener, system = listening linux
            let system = setOk Option.Linger listener (OptionValue.ofLinger 1 time) system
            let client, system = socketOf SocketDomain.Inet SocketKind.Stream system
            let system = connectTo 5000us client system
            // A negative length: Linux takes the connection, then answers
            // EINVAL and drops it.
            UnixConnection.accept 0 listener UserBuffer.Mapped System.UInt32.MaxValue system

        match withLinger 0 with
        | Error (AcceptRefusal.AbortiveDrop _) -> ()
        | other -> failwith $"expected a refusal, got %A{other}"

        // Lingering for some time drops it in order, as before.
        match withLinger 1 with
        | Ok (AcceptOutcome.DroppedConnection UnixError.EINVAL, _) -> ()
        | other -> failwith $"expected a dropped connection, got %A{other}"

    // ------------------------------------------------------------------
    // Caller bugs
    // ------------------------------------------------------------------

    [<TestCaseSource(nameof platforms)>]
    let ``supplying other than the bytes the copy takes is a caller bug`` (platform : SimulatedUnixPlatform) =
        let fd, system = socketOf SocketDomain.Inet SocketKind.Stream (fresh platform)
        let level, name = numbered platform Option.Linger

        let short =
            Assert.Throws<exn> (fun () ->
                UnixSocket.setsockopt fd level name UserBuffer.Mapped 8u (Some (OptionValue.ofInt 1)) system
                |> ignore<_>
            )

        short.Message |> shouldContainText "this is a bug in the caller"
