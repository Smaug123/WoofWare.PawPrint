namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PosixKernel

/// `setsockopt(2)` and `getsockopt(2)` of every option the model holds --
/// `SO_REUSEADDR`, `TCP_NODELAY`, `IPV6_V6ONLY`, `SO_LINGER` and Darwin's
/// `SO_LINGER_SEC` -- put to the kernel running the suite and to the model of
/// the same flavour, on the same random inputs.
///
/// Each host falsifies its own column: macOS locally, Linux in CI. The inputs
/// cover the descriptor kinds, value buffers and lengths the model
/// distinguishes -- a closed descriptor, a pipe, a fresh socket of every
/// domain and kind the flavour creates; real storage, a null pointer, and a
/// reserved `PROT_NONE` page; every length at the boundaries of `sizeof(int)`,
/// of `sizeof(struct linger)`, and of the signed and unsigned readings of a
/// `socklen_t`; and values at the edges of what each kernel stores. Only a
/// fresh socket's phases are reachable from here, which is why `TestSockOpt`
/// and `TestSocketOptions` carry the others as measured literals.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSockOptAgainstHost =

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "setsockopt", SetLastError = true)>]
    extern int private hostSetSockOpt(int fd, int level, int optionName, nativeint value, uint32 optionLength)

    [<DllImport("libc", EntryPoint = "getsockopt", SetLastError = true)>]
    extern int private hostGetSockOpt(int fd, int level, int optionName, nativeint value, nativeint optionLength)

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

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
    let private PROT_NONE = 0

    let private mapPrivateAnonymous (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x02 ||| 0x20
        | SimulatedUnixFlavour.Darwin -> 0x02 ||| 0x1000

    /// A descriptor number far above anything this process opens, so that it is
    /// closed on the host without racing another test's `open` for the number.
    [<Literal>]
    let private ClosedFd = 1_000_000

    /// One reserved page nothing can be mapped over, whose address faults on
    /// every access. Never unmapped: it is a process-lifetime fixture.
    let private faultingPage : Lazy<uint64> =
        lazy
            match HostPlatform.flavour () with
            | None -> failwith "TestSockOptAgainstHost: no Unix host, so nothing to reserve"
            | Some flavour ->
                let page = hostMmap (0n, 4096un, PROT_NONE, mapPrivateAnonymous flavour, -1, 0L)

                if page = -1n then
                    failwith $"TestSockOptAgainstHost: mmap failed with errno %d{Marshal.GetLastPInvokeError ()}"

                uint64 (int64 page)

    [<RequireQualifiedAccess>]
    type private Target =
        | Closed
        | Pipe
        | Socket of SocketDomain * SocketKind

    [<RequireQualifiedAccess>]
    type private Option =
        | ReuseAddress
        | NoDelay
        | Ipv6Only
        | Linger
        | LingerSeconds

    [<RequireQualifiedAccess>]
    type private Buffer =
        | Real
        | Null
        | Faulting

    let private numbered (platform : SimulatedUnixPlatform) (option : Option) : int * int =
        match option with
        | Option.ReuseAddress ->
            SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.reuseAddressOption platform
        | Option.NoDelay -> SimulatedUnixPlatform.tcpOptionLevel platform, SimulatedUnixPlatform.noDelayOption platform
        | Option.Ipv6Only ->
            SimulatedUnixPlatform.ipv6OptionLevel platform, SimulatedUnixPlatform.ipv6OnlyOption platform
        | Option.Linger -> SimulatedUnixPlatform.socketOptionLevel platform, SimulatedUnixPlatform.lingerOption platform
        | Option.LingerSeconds ->
            match SimulatedUnixPlatform.lingerSecondsOption platform with
            | Some name -> SimulatedUnixPlatform.socketOptionLevel platform, name
            | None -> failwith $"%O{platform} has no SO_LINGER_SEC"

    let private valueSize (option : Option) : int =
        match option with
        | Option.Linger
        | Option.LingerSeconds -> 8
        | Option.ReuseAddress
        | Option.NoDelay
        | Option.Ipv6Only -> 4

    let private options (platform : SimulatedUnixPlatform) : Option list =
        [
            yield Option.ReuseAddress
            yield Option.NoDelay
            yield Option.Ipv6Only
            yield Option.Linger
            if (SimulatedUnixPlatform.lingerSecondsOption platform).IsSome then
                yield Option.LingerSeconds
        ]

    let private targets (platform : SimulatedUnixPlatform) : Target list =
        [
            yield Target.Closed
            yield Target.Pipe
            for domain in [ SocketDomain.Inet ; SocketDomain.Inet6 ; SocketDomain.Unix ] do
                for kind in [ SocketKind.Stream ; SocketKind.Datagram ] do
                    yield Target.Socket (domain, kind)
            if SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux then
                yield Target.Socket (SocketDomain.Unix, SocketKind.SeqPacket)
        ]

    let private lengths : uint32 list =
        [
            0u
            1u
            2u
            3u
            4u
            5u
            7u
            8u
            9u
            16u
            0x7fff_ffffu
            0x8000_0000u
            0xffff_fffeu
            UInt32.MaxValue
        ]

    let private bufferGen : Gen<Buffer> =
        Gen.elements [ Buffer.Real ; Buffer.Null ; Buffer.Faulting ]

    let private lengthGen : Gen<uint32> =
        Gen.oneof [ Gen.elements lengths ; ArbMap.defaults |> ArbMap.generate<uint32> ]

    let private intGen : Gen<int> =
        Gen.oneof
            [
                Gen.elements
                    [
                        0
                        1
                        2
                        4
                        -1
                        0x100
                        5
                        327
                        328
                        32767
                        32768
                        65535
                        65536
                        21474836
                        21474837
                        42949673
                        Int32.MaxValue
                        Int32.MinValue
                    ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    /// The bytes a caller's value buffer holds: two `int`s, of which an
    /// `int`-sized option reads the first.
    let private valueGen : Gen<byte[]> =
        Gen.map2
            (fun (a : int) (b : int) -> Array.append (BitConverter.GetBytes a) (BitConverter.GetBytes b))
            intGen
            intGen

    let private userBuffer (buffer : Buffer) : UserBuffer =
        match buffer with
        | Buffer.Real -> UserBuffer.Mapped
        | Buffer.Null -> UserBuffer.Unmapped 0UL
        | Buffer.Faulting -> UserBuffer.Unmapped faultingPage.Value

    let private fresh (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// A host descriptor for `target`, and a matching model descriptor on
    /// `system`. The model's pipe is standard input, which it models as one.
    let private withTarget
        (target : Target)
        (system : UnixSystem<int, string>)
        (action : int -> int -> UnixSystem<int, string> -> 'a)
        : 'a
        =
        match target with
        | Target.Closed -> action ClosedFd ClosedFd system
        | Target.Pipe ->
            let fds = Array.zeroCreate<int> 2

            if hostPipe fds <> 0 then
                failwith $"pipe failed with errno %d{Marshal.GetLastPInvokeError ()}"

            try
                action fds.[0] 0 system
            finally
                hostClose fds.[0] |> ignore<int>
                hostClose fds.[1] |> ignore<int>
        | Target.Socket (domain, kind) ->
            let rawDomain, rawKind, _ =
                NewSocket.arguments system.Machine.UnixPlatform domain kind SocketProtocol.Default

            let fd = hostSocket (rawDomain, rawKind, 0)

            if fd < 0 then
                failwith $"socket failed with errno %d{Marshal.GetLastPInvokeError ()}"

            try
                let modelFd, system = NewSocket.create domain kind SocketProtocol.Default system
                action fd modelFd system
            finally
                hostClose fd |> ignore<int>

    /// Real storage holding `bytes`, or the address `buffer` names, for the
    /// duration of `action`.
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

    let private hostSet (fd : int) (level : int) (name : int) (buffer : Buffer) (length : uint32) (value : byte[]) =
        withBuffer
            buffer
            value
            (fun address ->
                Marshal.SetLastPInvokeError 0

                if hostSetSockOpt (fd, level, name, address, length) = 0 then
                    0
                else
                    Marshal.GetLastPInvokeError ()
            )

    /// The model's set, as a client makes it: the admission, then the bytes the
    /// copy takes.
    let private modelSet
        (fd : int)
        (level : int)
        (name : int)
        (buffer : Buffer)
        (length : uint32)
        (value : byte[])
        (system : UnixSystem<int, string>)
        =
        let supplied =
            match UnixSocket.admitSetSockOpt fd level name (userBuffer buffer) length system with
            | Ok (SetSockOptAdmission.Transfer count) -> Some (ImmutableArray.Create (value, 0, count))
            | Ok SetSockOptAdmission.NoCopy
            | Ok (SetSockOptAdmission.Answered _)
            | Error _ -> None

        UnixSocket.setsockopt fd level name (userBuffer buffer) length supplied system

    /// What a read through real buffers of the option's size answers, on the
    /// host: the errno, and the bytes and length on success.
    let private hostReadBack (fd : int) (level : int) (name : int) : int * byte[] =
        let storage = Marshal.AllocHGlobal 12

        try
            for i in 0..7 do
                Marshal.WriteByte (storage, i, 0x5auy)

            Marshal.WriteInt32 (storage, 8, 8)
            Marshal.SetLastPInvokeError 0

            if hostGetSockOpt (fd, level, name, storage, storage + 8n) <> 0 then
                Marshal.GetLastPInvokeError (), [||]
            else
                let length = Marshal.ReadInt32 (storage, 8)
                let bytes = Array.zeroCreate<byte> length
                Marshal.Copy (storage, bytes, 0, length)
                0, bytes
        finally
            Marshal.FreeHGlobal storage

    let private modelReadBack (fd : int) (level : int) (name : int) (system : UnixSystem<int, string>) : int * byte[] =
        let platform = system.Machine.UnixPlatform

        let read =
            match UnixSocket.admitGetSockOpt fd level name UserBuffer.Mapped UserBuffer.Mapped system with
            | Ok GetSockOptAdmission.ReadLength -> Some 8u
            | _ -> None

        match UnixSocket.getsockopt fd level name UserBuffer.Mapped UserBuffer.Mapped read system with
        | Ok (GetSockOptAnswer.Reported copied, _) -> 0, Seq.toArray copied
        | Ok (GetSockOptAnswer.Failed (error, _), _) -> errnoOf platform error, [||]
        | Error refusal -> failwith $"reading the model back was refused: %s{SocketOptionRefusal.describe refusal}"

    let private onHost (test : SimulatedUnixPlatform -> unit) : unit =
        HostPlatform.onUnixHost (fun flavour ->
            if not BitConverter.IsLittleEndian then
                Assert.Ignore "the model's presets are little-endian machines"

            test (HostPlatform.platformOf flavour)
        )

    /// Whether the model's refusal is one it states for this input, rather than
    /// a disagreement: Linux's negative linger time, which this library will
    /// not answer.
    let private isStatedRefusal (refusal : SocketOptionRefusal) : bool =
        match refusal with
        | SocketOptionRefusal.NegativeLingerTime _ -> true
        | SocketOptionRefusal.UnmodelledOption _
        | SocketOptionRefusal.Buffer _ -> false

    [<Test>]
    let ``a sequence of setsockopt calls answers and reads back as this kernel does`` () : unit =
        onHost (fun platform ->
            let step = Gen.zip (Gen.zip bufferGen lengthGen) valueGen

            let gen =
                Gen.zip
                    (Gen.zip (Gen.elements (targets platform)) (Gen.elements (options platform)))
                    (Gen.listOf step |> Gen.resize 4)

            let property ((target : Target, option : Option), steps : ((Buffer * uint32) * byte[]) list) : unit =
                let level, name = numbered platform option

                withTarget
                    target
                    (fresh platform)
                    (fun hostFd modelFd system ->
                        let rec go (index : int) steps system =
                            match steps with
                            | [] -> ()
                            | ((buffer, length), value) :: rest ->

                            let describe =
                                $"%A{target} %A{option}, step %d{index} of %A{steps}: %A{buffer} length %d{length} value %A{value}"

                            match modelSet modelFd level name buffer length value system with
                            | Error refusal when isStatedRefusal refusal -> ()
                            | Error refusal ->
                                failwith
                                    $"%s{describe}: the model refused (%s{SocketOptionRefusal.describe refusal}) where this kernel answered errno %d{hostSet hostFd level name buffer length value}"
                            | Ok (answer, system) ->

                            let hostErrno = hostSet hostFd level name buffer length value

                            let modelErrno =
                                match answer with
                                | SetSockOptAnswer.Set -> 0
                                | SetSockOptAnswer.Failed error -> errnoOf platform error

                            if modelErrno <> hostErrno then
                                failwith $"%s{describe}: the model answered %d{modelErrno}, this kernel %d{hostErrno}"

                            let modelRead = modelReadBack modelFd level name system
                            let hostRead = hostReadBack hostFd level name

                            if modelRead <> hostRead then
                                failwith $"%s{describe}: the model reads back %A{modelRead}, this kernel %A{hostRead}"

                            go (index + 1) rest system

                        go 0 steps system
                    )

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)
        )

    [<Test>]
    let ``getsockopt answers as this kernel does`` () : unit =
        onHost (fun platform ->
            let sentinel = 0x5auy

            let gen =
                Gen.zip
                    (Gen.zip (Gen.elements (targets platform)) (Gen.elements (options platform)))
                    (Gen.zip (Gen.zip bufferGen bufferGen) (Gen.zip lengthGen (Gen.optionOf valueGen)))

            let property
                (
                    (target : Target, option : Option),
                    ((valueBuffer : Buffer, lengthBuffer : Buffer), (declaredLength : uint32, prior : byte[] option))
                )
                : unit
                =
                let level, name = numbered platform option

                withTarget
                    target
                    (fresh platform)
                    (fun hostFd modelFd system ->
                        // Set the option first through real storage at its size,
                        // where both sides take it, so the read has something
                        // other than the default to report.
                        let system =
                            match prior with
                            | None -> Some system
                            | Some value ->
                                let size = uint32 (valueSize option)

                                match modelSet modelFd level name Buffer.Real size value system with
                                | Ok (SetSockOptAnswer.Set, system) ->
                                    if hostSet hostFd level name Buffer.Real size value <> 0 then
                                        failwith
                                            $"%A{target} %A{option}: the model took %A{value}, this kernel did not"

                                    Some system
                                | Ok (SetSockOptAnswer.Failed _, _)
                                | Error _ -> None

                        match system with
                        | None -> ()
                        | Some system ->

                        let hostErrno, hostValue, hostLength =
                            withBuffer
                                valueBuffer
                                (Array.create 16 sentinel)
                                (fun valueAddress ->
                                    withBuffer
                                        lengthBuffer
                                        (BitConverter.GetBytes declaredLength)
                                        (fun lengthAddress ->
                                            Marshal.SetLastPInvokeError 0

                                            let errno =
                                                if
                                                    hostGetSockOpt (
                                                        hostFd,
                                                        level,
                                                        name,
                                                        valueAddress,
                                                        lengthAddress
                                                    ) = 0
                                                then
                                                    0
                                                else
                                                    Marshal.GetLastPInvokeError ()

                                            let value =
                                                match valueBuffer with
                                                | Buffer.Real ->
                                                    let bytes = Array.zeroCreate<byte> 16
                                                    Marshal.Copy (valueAddress, bytes, 0, 16)
                                                    Some bytes
                                                | Buffer.Null
                                                | Buffer.Faulting -> None

                                            let length =
                                                match lengthBuffer with
                                                | Buffer.Real -> Some (uint32 (Marshal.ReadInt32 lengthAddress))
                                                | Buffer.Null
                                                | Buffer.Faulting -> None

                                            errno, value, length
                                        )
                                )

                        let modelLength = userBuffer lengthBuffer

                        let read =
                            match
                                UnixSocket.admitGetSockOpt
                                    modelFd
                                    level
                                    name
                                    (userBuffer valueBuffer)
                                    modelLength
                                    system
                            with
                            | Ok GetSockOptAdmission.ReadLength -> Some declaredLength
                            | _ -> None

                        let describe =
                            $"%A{target} %A{option} prior %A{prior} value %A{valueBuffer} length %A{lengthBuffer} = %d{declaredLength}"

                        let untouched (buffer : Buffer) (contents : 'a) : 'a option =
                            match buffer with
                            | Buffer.Real -> Some contents
                            | Buffer.Null
                            | Buffer.Faulting -> None

                        let model =
                            match
                                UnixSocket.getsockopt
                                    modelFd
                                    level
                                    name
                                    (userBuffer valueBuffer)
                                    modelLength
                                    read
                                    system
                            with
                            | Error refusal ->
                                failwith
                                    $"%s{describe}: the model refused (%s{SocketOptionRefusal.describe refusal}) where this kernel answered errno %d{hostErrno}"
                            | Ok (GetSockOptAnswer.Failed (error, overwritten), _) ->
                                errnoOf platform error,
                                untouched valueBuffer (Array.create 16 sentinel),
                                untouched lengthBuffer (Option.defaultValue declaredLength overwritten)
                            | Ok (GetSockOptAnswer.Reported copied, _) ->
                                let bytes = Array.create 16 sentinel
                                copied.CopyTo bytes
                                0, untouched valueBuffer bytes, untouched lengthBuffer (uint32 copied.Length)

                        let host = hostErrno, hostValue, hostLength

                        if model <> host then
                            failwith $"%s{describe}: the model predicts %A{model}, this kernel did %A{host}"
                    )

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)
        )
