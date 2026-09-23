namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PosixKernel

/// `setsockopt(2)` and `getsockopt(2)` of `SO_REUSEADDR`, put to the kernel
/// running the suite and to the model of the same flavour, on the same random
/// inputs.
///
/// Each host falsifies its own column: macOS locally, Linux in CI. The inputs
/// cover the descriptor kinds, value buffers and lengths the model
/// distinguishes -- a closed descriptor, a pipe, both socket kinds; real
/// storage, a null pointer, and a reserved `PROT_NONE` page; every length at
/// the boundaries of `sizeof(int)` and of the signed and unsigned readings of a
/// `socklen_t`. Only a fresh socket's phases are reachable from here, which is
/// why `TestSockOpt` carries the others as measured literals.
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
    let private AF_INET = 2

    [<Literal>]
    let private SOCK_STREAM = 1

    [<Literal>]
    let private SOCK_DGRAM = 2

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
        | Stream
        | Datagram

    [<RequireQualifiedAccess>]
    type private Buffer =
        | Real
        | Null
        | Faulting

    let private lengths : uint32 list =
        [
            0u
            1u
            2u
            3u
            4u
            5u
            8u
            16u
            0x7fff_ffffu
            0x8000_0000u
            0xffff_fffeu
            UInt32.MaxValue
        ]

    let private targetGen : Gen<Target> =
        Gen.elements [ Target.Closed ; Target.Pipe ; Target.Stream ; Target.Datagram ]

    let private bufferGen : Gen<Buffer> =
        Gen.elements [ Buffer.Real ; Buffer.Null ; Buffer.Faulting ]

    let private lengthGen : Gen<uint32> =
        Gen.oneof [ Gen.elements lengths ; ArbMap.defaults |> ArbMap.generate<uint32> ]

    let private valueGen : Gen<int> =
        Gen.oneof
            [
                Gen.elements [ 0 ; 1 ; 2 ; 4 ; -1 ; 0x100 ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    let private userBuffer (buffer : Buffer) : UserBuffer =
        match buffer with
        | Buffer.Real -> UserBuffer.Mapped
        | Buffer.Null -> UserBuffer.Unmapped 0UL
        | Buffer.Faulting -> UserBuffer.Unmapped faultingPage.Value

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
        | Target.Stream
        | Target.Datagram ->
            let kind, modelKind, protocol =
                match target with
                | Target.Stream -> SOCK_STREAM, SocketKind.Stream, SocketProtocol.Tcp
                | _ -> SOCK_DGRAM, SocketKind.Datagram, SocketProtocol.Udp

            let fd = hostSocket (AF_INET, kind, 0)

            if fd < 0 then
                failwith $"socket failed with errno %d{Marshal.GetLastPInvokeError ()}"

            try
                let modelFd, system =
                    UnixSocket.createSocket SocketDomain.InterNetwork modelKind protocol system

                action fd modelFd system
            finally
                hostClose fd |> ignore<int>

    /// Four bytes of real storage holding `value`, or the address `buffer`
    /// names, for the duration of `action`.
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

    /// The host's `SO_REUSEADDR` on `fd` read through real storage, which is
    /// the one read both kernels answer without condition.
    let private hostReadBack (platform : SimulatedUnixPlatform) (fd : int) : int =
        let storage = Marshal.AllocHGlobal 8

        try
            Marshal.WriteInt32 (storage, 0, 0x5a5a5a5a)
            Marshal.WriteInt32 (storage, 4, 4)

            let level = SimulatedUnixPlatform.socketOptionLevel platform
            let optionName = SimulatedUnixPlatform.reuseAddressOption platform

            if hostGetSockOpt (fd, level, optionName, storage, storage + 4n) <> 0 then
                failwith $"reading SO_REUSEADDR back failed with errno %d{Marshal.GetLastPInvokeError ()}"

            Marshal.ReadInt32 storage
        finally
            Marshal.FreeHGlobal storage

    let private onHost (test : SimulatedUnixPlatform -> unit) : unit =
        HostPlatform.onUnixHost (fun flavour ->
            if not BitConverter.IsLittleEndian then
                Assert.Ignore "the model's presets are little-endian machines"

            test (HostPlatform.platformOf flavour)
        )

    [<Test>]
    let ``setsockopt of SO_REUSEADDR answers as this kernel does`` () : unit =
        onHost (fun platform ->
            let level = SimulatedUnixPlatform.socketOptionLevel platform
            let optionName = SimulatedUnixPlatform.reuseAddressOption platform

            let gen = Gen.zip (Gen.zip targetGen bufferGen) (Gen.zip lengthGen valueGen)

            let property ((target : Target, buffer : Buffer), (optionLength : uint32, value : int)) : unit =
                withTarget
                    target
                    (UnixSystem.initial platform)
                    (fun hostFd modelFd system ->
                        let hostErrno =
                            withBuffer
                                buffer
                                (BitConverter.GetBytes value)
                                (fun address ->
                                    Marshal.SetLastPInvokeError 0

                                    if hostSetSockOpt (hostFd, level, optionName, address, optionLength) = 0 then
                                        0
                                    else
                                        Marshal.GetLastPInvokeError ()
                                )

                        let modelValue = userBuffer buffer

                        let supplied =
                            match
                                UnixSocket.admitSetSockOpt modelFd level optionName modelValue optionLength system
                            with
                            | Ok (SetSockOptAdmission.Transfer _) -> Some value
                            | _ -> None

                        match
                            UnixSocket.setsockopt modelFd level optionName modelValue optionLength supplied system
                        with
                        | Error refusal ->
                            failwith
                                $"%A{target} %A{buffer} length %d{optionLength}: the model refused (%s{SocketOptionRefusal.describe refusal}) where this kernel answered errno %d{hostErrno}"
                        | Ok (SetSockOptAnswer.Failed error, _) ->
                            if errnoOf platform error <> hostErrno then
                                failwith
                                    $"%A{target} %A{buffer} length %d{optionLength}: the model answered %O{error}, this kernel errno %d{hostErrno}"
                        | Ok (SetSockOptAnswer.Set, system) ->
                            if hostErrno <> 0 then
                                failwith
                                    $"%A{target} %A{buffer} length %d{optionLength}: the model succeeded, this kernel answered errno %d{hostErrno}"

                            let modelReadBack =
                                match
                                    UnixSocket.getsockopt
                                        modelFd
                                        level
                                        optionName
                                        UserBuffer.Mapped
                                        UserBuffer.Mapped
                                        (Some 4u)
                                        system
                                with
                                | Ok (GetSockOptAnswer.Reported (read, 4u)) -> read
                                | other -> failwith $"reading the model back answered %A{other}"

                            let hostRead = hostReadBack platform hostFd

                            if modelReadBack <> hostRead then
                                failwith
                                    $"%A{target} set to %d{value}: the model reads back %d{modelReadBack}, this kernel %d{hostRead}"
                    )

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)
        )

    [<Test>]
    let ``getsockopt of SO_REUSEADDR answers as this kernel does`` () : unit =
        onHost (fun platform ->
            let level = SimulatedUnixPlatform.socketOptionLevel platform
            let optionName = SimulatedUnixPlatform.reuseAddressOption platform
            let sentinel = 0x5auy

            let gen =
                Gen.zip
                    (Gen.zip targetGen (Gen.zip bufferGen bufferGen))
                    (Gen.zip lengthGen (Gen.elements [ false ; true ]))

            let property
                (
                    (target : Target, (valueBuffer : Buffer, lengthBuffer : Buffer)),
                    (declaredLength : uint32, isSet : bool)
                )
                : unit
                =
                withTarget
                    target
                    (UnixSystem.initial platform)
                    (fun hostFd modelFd system ->
                        let isSocket =
                            match target with
                            | Target.Stream
                            | Target.Datagram -> true
                            | Target.Closed
                            | Target.Pipe -> false

                        let system =
                            if isSet && isSocket then
                                let setValue = BitConverter.GetBytes 1

                                withBuffer
                                    Buffer.Real
                                    setValue
                                    (fun address ->
                                        if hostSetSockOpt (hostFd, level, optionName, address, 4u) <> 0 then
                                            failwith
                                                $"setting the host's flag failed: %d{Marshal.GetLastPInvokeError ()}"
                                    )

                                ReuseAddress.set true modelFd system
                            else
                                system

                        // What the host leaves in each buffer, where it is real.
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
                                                        optionName,
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
                            match UnixSocket.admitGetSockOpt modelFd level optionName modelLength system with
                            | Ok GetSockOptAdmission.ReadLength -> Some declaredLength
                            | _ -> None

                        let describe =
                            $"%A{target} set=%b{isSet} value %A{valueBuffer} length %A{lengthBuffer} = %d{declaredLength}"

                        match
                            UnixSocket.getsockopt
                                modelFd
                                level
                                optionName
                                (userBuffer valueBuffer)
                                modelLength
                                read
                                system
                        with
                        | Error refusal ->
                            failwith
                                $"%s{describe}: the model refused (%s{SocketOptionRefusal.describe refusal}) where this kernel answered errno %d{hostErrno}"
                        | Ok (GetSockOptAnswer.Failed error) ->
                            if errnoOf platform error <> hostErrno then
                                failwith
                                    $"%s{describe}: the model answered %O{error}, this kernel errno %d{hostErrno}"

                            // Neither buffer is written on a failure.
                            match hostValue with
                            | Some bytes when bytes <> Array.create 16 sentinel ->
                                failwith $"%s{describe}: this kernel failed yet wrote %A{bytes}"
                            | _ -> ()

                            match hostLength with
                            | Some length when length <> declaredLength ->
                                failwith $"%s{describe}: this kernel failed yet left %d{length} in the length cell"
                            | _ -> ()
                        | Ok (GetSockOptAnswer.Reported (value, length)) ->
                            if hostErrno <> 0 then
                                failwith
                                    $"%s{describe}: the model succeeded, this kernel answered errno %d{hostErrno}"

                            match hostLength with
                            | Some hostLength when hostLength <> length ->
                                failwith
                                    $"%s{describe}: the model reports length %d{length}, this kernel %d{hostLength}"
                            | _ -> ()

                            match hostValue with
                            | Some bytes ->
                                let expected = Array.create 16 sentinel
                                let encoded = BitConverter.GetBytes value
                                Array.blit encoded 0 expected 0 (int length)

                                if bytes <> expected then
                                    failwith
                                        $"%s{describe}: the model predicts %A{expected}, this kernel wrote %A{bytes}"
                            | None -> ()
                    )

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)
        )
