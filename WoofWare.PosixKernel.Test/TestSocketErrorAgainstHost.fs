namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PosixKernel

/// `getsockopt(SO_ERROR)` interleaved with the connects that make a refusal
/// pending and report it, put to the kernel running the suite and to the model
/// of the same flavour, on the same random sequences.
///
/// Each host falsifies its own column: macOS locally, Linux in CI. A sequence
/// runs on one TCP socket, blocking or not, and mixes `SO_ERROR` reads through
/// every kind of buffer and length with connects to a port nothing listens on,
/// connects to a listener, and `listen(2)`; each answer, and every byte a read
/// leaves in real storage, must agree. A sequence ends early where the model
/// declines to answer (`listen(2)` on a refused socket, for one), since there
/// is nothing to compare from there on.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketErrorAgainstHost =

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "bind", SetLastError = true)>]
    extern int private hostBind(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "listen", SetLastError = true)>]
    extern int private hostListen(int fd, int backlog)

    [<DllImport("libc", EntryPoint = "connect", SetLastError = true)>]
    extern int private hostConnect(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "getsockname", SetLastError = true)>]
    extern int private hostGetSockName(int fd, byte[] address, uint32& length)

    [<DllImport("libc", EntryPoint = "getsockopt", SetLastError = true)>]
    extern int private hostGetSockOpt(int fd, int level, int optionName, nativeint value, nativeint optionLength)

    // `fcntl(2)` is variadic, which a P/Invoke cannot call portably (Apple's
    // arm64 ABI passes variadic arguments on the stack), so this goes through
    // the runtime's own fixed-arity wrapper of it.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll(int64[] fds, unativeint count, int timeoutMs)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "mmap", SetLastError = true)>]
    extern nativeint private hostMmap(
        nativeint address,
        unativeint length,
        int protection,
        int flags,
        int fd,
        int64 offset
    )

    [<Literal>]
    let private AF_INET = 2

    [<Literal>]
    let private SOCK_STREAM = 1

    [<Literal>]
    let private POLLOUT = 4s

    let private mapPrivateAnonymous (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x02 ||| 0x20
        | SimulatedUnixFlavour.Darwin -> 0x02 ||| 0x1000

    /// One reserved page nothing can be mapped over, whose address faults on
    /// every access. Never unmapped: it is a process-lifetime fixture.
    let private faultingPage : Lazy<uint64> =
        lazy
            match HostPlatform.flavour () with
            | None -> failwith "TestSocketErrorAgainstHost: no Unix host, so nothing to reserve"
            | Some flavour ->
                let page = hostMmap (0n, 4096un, 0, mapPrivateAnonymous flavour, -1, 0L)

                if page = -1n then
                    failwith $"TestSocketErrorAgainstHost: mmap failed with errno %d{Marshal.GetLastPInvokeError ()}"

                uint64 (int64 page)

    [<RequireQualifiedAccess>]
    type private Buffer =
        | Real
        | Null
        | Faulting

    [<RequireQualifiedAccess>]
    type private Operation =
        /// `getsockopt(SO_ERROR)` with these buffers and this declared length.
        | ReadError of value : Buffer * lengthCell : Buffer * declaredLength : uint32
        /// `connect(2)` to a loopback port nothing listens on.
        | ConnectToNobody
        /// `connect(2)` to a loopback listener.
        | ConnectToListener
        | Listen

    let private lengths : uint32 list =
        [
            0u
            1u
            2u
            3u
            4u
            5u
            8u
            0x7fff_ffffu
            0x8000_0000u
            UInt32.MaxValue
        ]

    let private operationGen : Gen<Operation> =
        let buffer = Gen.elements [ Buffer.Real ; Buffer.Null ; Buffer.Faulting ]

        let read =
            Gen.map3
                (fun value lengthCell length -> Operation.ReadError (value, lengthCell, length))
                (Gen.frequency [ 2, Gen.constant Buffer.Real ; 1, buffer ])
                (Gen.frequency [ 2, Gen.constant Buffer.Real ; 1, buffer ])
                (Gen.frequency [ 3, Gen.constant 4u ; 2, Gen.elements lengths ])

        Gen.frequency
            [
                5, read
                4, Gen.constant Operation.ConnectToNobody
                1, Gen.constant Operation.ConnectToListener
                1, Gen.constant Operation.Listen
            ]

    let private caseGen : Gen<bool * Operation list> =
        Gen.zip (Gen.elements [ false ; true ]) (Gen.listOf operationGen |> Gen.resize 8)

    let private userBuffer (buffer : Buffer) : UserBuffer =
        match buffer with
        | Buffer.Real -> UserBuffer.Mapped
        | Buffer.Null -> UserBuffer.Unmapped 0UL
        | Buffer.Faulting -> UserBuffer.Unmapped faultingPage.Value

    let private withBuffer (buffer : Buffer) (bytes : byte[]) (action : nativeint -> 'a) : 'a =
        match buffer with
        | Buffer.Null -> action 0n
        | Buffer.Faulting -> action (nativeint (int64 faultingPage.Value))
        | Buffer.Real ->
            let storage = Marshal.AllocHGlobal bytes.Length

            try
                Marshal.Copy (bytes, 0, storage, bytes.Length)
                action storage
            finally
                Marshal.FreeHGlobal storage

    let private errnoOf (platform : SimulatedUnixPlatform) (error : UnixError) : int =
        UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let private hostSockaddr (platform : SimulatedUnixPlatform) (port : uint16) : byte[] =
        SimulatedUnixPlatform.encodeInternetSockaddr platform (loopback port)

    /// A host socket bound to an ephemeral loopback port, and that port.
    let private hostBound (platform : SimulatedUnixPlatform) : int * uint16 =
        let fd = hostSocket (AF_INET, SOCK_STREAM, 0)

        if fd < 0 then
            failwith $"socket failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let address = hostSockaddr platform 0us

        if hostBind (fd, address, uint32 address.Length) <> 0 then
            failwith $"bind failed with errno %d{Marshal.GetLastPInvokeError ()}"

        let name = Array.zeroCreate<byte> 16
        let mutable length = 16u

        if hostGetSockName (fd, name, &length) <> 0 then
            failwith $"getsockname failed with errno %d{Marshal.GetLastPInvokeError ()}"

        fd, (uint16 name.[2] <<< 8) ||| uint16 name.[3]

    /// Whether some other socket on the host holds `port`, which turns a
    /// connect meant to be refused into one that is not.
    let private portTaken (platform : SimulatedUnixPlatform) (port : uint16) : bool =
        let fd = hostSocket (AF_INET, SOCK_STREAM, 0)

        try
            let address = hostSockaddr platform port
            hostBind (fd, address, uint32 address.Length) <> 0
        finally
            hostClose fd |> ignore<int>

    /// Wait until a non-blocking connect in flight on the host has an outcome,
    /// as the model's connect has one at once.
    let private settle (fd : int) : unit =
        // struct pollfd { int fd; short events; short revents; }, little-endian.
        let entry = [| int64 (uint32 fd) ||| (int64 POLLOUT <<< 32) |]

        if hostPoll (entry, 1un, 5000) <> 1 then
            failwith $"the host's connect on fd %d{fd} had no outcome after five seconds"

    let private modelConnect
        (fd : int)
        (port : uint16)
        (system : UnixSystem<int, string>)
        : (int * UnixSystem<int, string>) option
        =
        match
            UnixConnection.connect
                fd
                UserBuffer.Mapped
                16u
                (Some SimulatedUnixPlatform.internetAddressFamily)
                (Some (loopback port))
                system
        with
        | Ok (ConnectOutcome.Completed, system) -> Some (0, system)
        | Ok (ConnectOutcome.Failed error, system) -> Some (errnoOf system.Machine.UnixPlatform error, system)
        | Error _ -> None

    /// A model holding a listener at `listenerPort` and a fresh TCP socket,
    /// and that socket's descriptor.
    let private modelSystem
        (platform : SimulatedUnixPlatform)
        (listenerPort : uint16)
        (nonBlocking : bool)
        : int * UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.boot

        let listener, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match
                UnixSocket.bind
                    listener
                    UserBuffer.Mapped
                    16u
                    (Some SimulatedUnixPlatform.internetAddressFamily)
                    (Some (loopback listenerPort))
                    system
            with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"the model's listener did not bind: %A{other}"

        let system =
            match UnixSocket.listen listener 16 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"the model's listener did not listen: %A{other}"

        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            if nonBlocking then
                match UnixSocket.setNonBlocking fd true system with
                | SetNonBlockingAnswer.Set, system -> system
                | other, _ -> failwith $"the model would not set O_NONBLOCK: %A{other}"
            else
                system

        fd, system

    /// What one operation answered: an errno (0 for success), and for a read
    /// through real storage, the bytes it left in the value buffer and the
    /// length cell.
    type private Observed =
        {
            Errno : int
            Value : byte[] option
            Length : uint32 option
        }

    let private sentinel : byte = 0x5auy

    let private hostRead
        (platform : SimulatedUnixPlatform)
        (fd : int)
        (value : Buffer)
        (lengthCell : Buffer)
        (declaredLength : uint32)
        : Observed
        =
        let level = SimulatedUnixPlatform.socketOptionLevel platform
        let optionName = SimulatedUnixPlatform.socketErrorOption platform

        withBuffer
            value
            (Array.create 8 sentinel)
            (fun valueAddress ->
                withBuffer
                    lengthCell
                    (BitConverter.GetBytes declaredLength)
                    (fun lengthAddress ->
                        Marshal.SetLastPInvokeError 0

                        let errno =
                            if hostGetSockOpt (fd, level, optionName, valueAddress, lengthAddress) = 0 then
                                0
                            else
                                Marshal.GetLastPInvokeError ()

                        {
                            Errno = errno
                            Value =
                                match value with
                                | Buffer.Real ->
                                    let bytes = Array.zeroCreate<byte> 8
                                    Marshal.Copy (valueAddress, bytes, 0, 8)
                                    Some bytes
                                | Buffer.Null
                                | Buffer.Faulting -> None
                            Length =
                                match lengthCell with
                                | Buffer.Real -> Some (uint32 (Marshal.ReadInt32 lengthAddress))
                                | Buffer.Null
                                | Buffer.Faulting -> None
                        }
                    )
            )

    let private modelRead
        (fd : int)
        (value : Buffer)
        (lengthCell : Buffer)
        (declaredLength : uint32)
        (system : UnixSystem<int, string>)
        : Observed * UnixSystem<int, string>
        =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.socketOptionLevel platform
        let optionName = SimulatedUnixPlatform.socketErrorOption platform
        let lengthBuffer = userBuffer lengthCell

        let read =
            match UnixSocket.admitGetSockOpt fd level optionName (userBuffer value) lengthBuffer system with
            | Ok GetSockOptAdmission.ReadLength -> Some declaredLength
            | Ok GetSockOptAdmission.SkipLength
            | Ok (GetSockOptAdmission.Answered _)
            | Error _ -> None

        let untouched (buffer : Buffer) (contents : 'a) : 'a option =
            match buffer with
            | Buffer.Real -> Some contents
            | Buffer.Null
            | Buffer.Faulting -> None

        match UnixSocket.getsockopt fd level optionName (userBuffer value) lengthBuffer read system with
        | Error refusal -> failwith $"the model refused SO_ERROR: %s{SocketOptionRefusal.describe refusal}"
        | Ok (GetSockOptAnswer.Failed error, system) ->
            {
                Errno = errnoOf platform error
                Value = untouched value (Array.create 8 sentinel)
                Length = untouched lengthCell declaredLength
            },
            system
        | Ok (GetSockOptAnswer.Reported (reported, length), system) ->
            let bytes = Array.create 8 sentinel
            Array.blit (BitConverter.GetBytes reported) 0 bytes 0 (int length)

            {
                Errno = 0
                Value = untouched value bytes
                Length = untouched lengthCell length
            },
            system

    /// Run `operations` on a fresh socket on the host and in the model, and
    /// answer the first disagreement, if any.
    let private run
        (platform : SimulatedUnixPlatform)
        (nobody : uint16)
        (nonBlocking : bool)
        (operations : Operation list)
        : string option
        =
        let listener, listenerPort = hostBound platform

        try
            if hostListen (listener, 16) <> 0 then
                failwith $"listen failed with errno %d{Marshal.GetLastPInvokeError ()}"

            let fd = hostSocket (AF_INET, SOCK_STREAM, 0)

            try
                if nonBlocking && hostSetNonBlocking (nativeint fd, 1) <> 0 then
                    failwith "setting O_NONBLOCK on the host's socket failed"

                let modelFd, system = modelSystem platform listenerPort nonBlocking

                let hostConnectTo (port : uint16) : int =
                    let address = hostSockaddr platform port
                    Marshal.SetLastPInvokeError 0

                    if hostConnect (fd, address, uint32 address.Length) = 0 then
                        0
                    else
                        let errno = Marshal.GetLastPInvokeError ()

                        if errno = errnoOf platform UnixError.EINPROGRESS then
                            settle fd

                        errno

                let rec go (index : int) (operations : Operation list) (system : UnixSystem<int, string>) =
                    match operations with
                    | [] -> None
                    | operation :: rest ->

                    let step =
                        match operation with
                        | Operation.ReadError (value, lengthCell, declaredLength) ->
                            let host = hostRead platform fd value lengthCell declaredLength
                            let model, system = modelRead modelFd value lengthCell declaredLength system
                            Some (host, model, system)
                        | Operation.ConnectToNobody
                        | Operation.ConnectToListener ->
                            let port =
                                match operation with
                                | Operation.ConnectToNobody -> nobody
                                | _ -> listenerPort

                            match modelConnect modelFd port system with
                            | None -> None
                            | Some (model, system) ->

                            let host = hostConnectTo port

                            let observed (errno : int) =
                                {
                                    Errno = errno
                                    Value = None
                                    Length = None
                                }

                            Some (observed host, observed model, system)
                        | Operation.Listen ->
                            match UnixSocket.listen modelFd 4 system with
                            | Error _ -> None
                            | Ok (answer, system) ->

                            Marshal.SetLastPInvokeError 0

                            let host =
                                if hostListen (fd, 4) = 0 then
                                    0
                                else
                                    Marshal.GetLastPInvokeError ()

                            let model =
                                match answer with
                                | ListenAnswer.Listening _ -> 0
                                | ListenAnswer.Failed error -> errnoOf platform error

                            let observed (errno : int) =
                                {
                                    Errno = errno
                                    Value = None
                                    Length = None
                                }

                            Some (observed host, observed model, system)

                    match step with
                    | None -> None
                    | Some (host, model, system) ->

                    if host <> model then
                        Some
                            $"operation %d{index} (%A{operation}) of %A{operations}: the host observed %A{host}, the model %A{model}"
                    else
                        go (index + 1) rest system

                go 0 operations system
            finally
                hostClose fd |> ignore<int>
        finally
            hostClose listener |> ignore<int>

    [<Test>]
    let ``SO_ERROR reads and connects answer as this kernel does`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            if not BitConverter.IsLittleEndian then
                Assert.Ignore "the model's presets are little-endian machines"

            let platform = HostPlatform.platformOf flavour

            // A port briefly bound and released, so nothing listens there;
            // re-chosen whenever another socket on the host takes it.
            let freshNobody () : uint16 =
                let fd, port = hostBound platform
                hostClose fd |> ignore<int>
                port

            let nobody = ref (freshNobody ())

            let property ((nonBlocking : bool, operations : Operation list)) : unit =
                let rec attempt (remaining : int) =
                    match run platform nobody.Value nonBlocking operations with
                    | None -> ()
                    | Some disagreement ->
                        if remaining > 0 && portTaken platform nobody.Value then
                            nobody.Value <- freshNobody ()
                            attempt (remaining - 1)
                        else
                            failwith $"%O{platform}, non-blocking %b{nonBlocking}: %s{disagreement}"

                attempt 3

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen caseGen) property)
        )
