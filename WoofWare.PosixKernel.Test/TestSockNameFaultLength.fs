namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// What a `getsockname(2)` whose address copy faults leaves in the caller's
/// length cell, which changed between Linux 6.17 and 6.18.
///
/// Every row of `docs/probes/sockname-fault-length` is replayed against a
/// platform running the kernel that measured it, so the version boundary is
/// held by measurements on both sides of it, on both architectures.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSockNameFaultLength =

    /// One row of a probe output.
    type private Row =
        {
            Call : string
            /// `null` or `unmapped`.
            Destination : string
            Declared : uint32
            Errno : int
            Cell : uint32
        }

    /// One probe output: the kernel it ran on, and its rows.
    type private Measurement =
        {
            Resource : string
            Platform : SimulatedUnixPlatform
            Rows : Row list
        }

    let private resources : string list =
        [
            "darwin-27.0.0-arm64.txt"
            "linux-6.12.111-x86_64.txt"
            "linux-6.17.13-x86_64.txt"
            "linux-6.18.5-x86_64.txt"
            "linux-6.18.5-aarch64.txt"
        ]

    let private platformOf (resource : string) (header : string) : SimulatedUnixPlatform =
        match header.Split ' ' |> List.ofArray with
        | [ "uname:" ; "Darwin" ; release ; "arm64" ] ->
            SimulatedUnixPlatform.createOrFail
                resource
                SimulatedUnixKernel.Darwin
                SimulatedUnixArchitecture.Arm64
                SimulatedPageSize.SixteenKiB
                release
        | [ "uname:" ; "Linux" ; release ; machine ] ->
            let architecture =
                match machine with
                | "x86_64" -> SimulatedUnixArchitecture.X64
                | "aarch64" -> SimulatedUnixArchitecture.Arm64
                | other -> failwith $"%s{resource}: unexpected machine %s{other}"

            let version =
                match LinuxRelease.version release with
                | Some version -> version
                | None -> failwith $"%s{resource}: release %s{release} names no version"

            SimulatedUnixPlatform.createOrFail
                resource
                (SimulatedUnixKernel.Linux version)
                architecture
                SimulatedPageSize.FourKiB
                release
        | _ -> failwith $"%s{resource}: unexpected header %s{header}"

    let private rowPattern : Regex =
        Regex
            @"^(getsockname|getpeername)\s+(null|unmapped)\s+declared=([0-9]+)\s+->\s+r=-1\s+errno=([0-9]+)\s+cell=([0-9]+)$"

    let private measurement (resource : string) : Measurement =
        let name = $"WoofWare.PosixKernel.Test.sockname-fault-length.%s{resource}"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream name with
            | null -> failwith $"no embedded resource %s{name}"
            | stream -> stream

        use reader = new StreamReader (stream)

        let lines =
            reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
            |> List.ofArray

        match lines with
        | [] -> failwith $"%s{resource} is empty"
        | header :: rows ->
            {
                Resource = resource
                Platform = platformOf resource header
                Rows =
                    rows
                    |> List.map (fun line ->
                        let m = rowPattern.Match line

                        if not m.Success then
                            failwith $"%s{resource}: unparsed row %s{line}"

                        {
                            Call = m.Groups.[1].Value
                            Destination = m.Groups.[2].Value
                            Declared = UInt32.Parse m.Groups.[3].Value
                            Errno = Int32.Parse m.Groups.[4].Value
                            Cell = UInt32.Parse m.Groups.[5].Value
                        }
                    )
            }

    let private measurements : Lazy<Measurement list> =
        lazy (resources |> List.map measurement)

    /// A socket bound to loopback on `platform`, and its descriptor.
    let private boundSocket (platform : SimulatedUnixPlatform) : int * UnixSystem<int, string> =
        let system =
            UnixSystem.initial platform
            |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let endpoint = InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 6000us

        match CopyIn.bind fd UserBuffer.Mapped 16u (CopyIn.inet platform endpoint) system with
        | Ok (BindAnswer.Bound _, system) -> fd, system
        | other -> failwith $"binding: %A{other}"

    [<Test>]
    let ``getsockname leaves the length cell as each measured kernel did`` () : unit =
        for m in measurements.Force () do
            let fd, system = boundSocket m.Platform

            let rows = m.Rows |> List.filter (fun row -> row.Call = "getsockname")
            rows |> shouldNotEqual []

            for row in rows do
                let destination =
                    match row.Destination with
                    | "null" -> UserBuffer.Unmapped 0UL
                    | _ -> UserBuffer.Unmapped 0x1000UL

                match UnixSocket.getsockname fd destination row.Declared system with
                | Ok (GetSockNameAnswer.Failed (error, overwritten)) ->
                    let errno =
                        UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering m.Platform) error

                    let cell =
                        match overwritten with
                        | Some length -> uint32 length
                        | None -> row.Declared

                    if (errno, cell) <> (row.Errno, row.Cell) then
                        failwith $"%s{m.Resource} %A{row}: the model answered errno %d{errno}, cell %d{cell}"
                | other -> failwith $"%s{m.Resource} %A{row}: the model answered %A{other}"

    /// `getpeername` copies out through the same kernel routine, and every
    /// kernel measured agrees: the two calls' rows are the same.
    [<Test>]
    let ``getpeername's rows are getsockname's on every measured kernel`` () : unit =
        for m in measurements.Force () do
            let answers (call : string) =
                m.Rows
                |> List.filter (fun row -> row.Call = call)
                |> List.map (fun row -> row.Destination, row.Declared, row.Errno, row.Cell)

            answers "getpeername" |> shouldNotEqual []
            answers "getpeername" |> shouldEqual (answers "getsockname")

    /// The Linux measurements reach both answers, so the replay above cannot
    /// pass with Linux's rule fixed at either one: some kernel left every cell
    /// as declared, and some wrote 16 into every one.
    [<Test>]
    let ``the Linux measurements reach both answers`` () : unit =
        let linuxCells =
            measurements.Force ()
            |> List.filter (fun m -> SimulatedUnixPlatform.flavour m.Platform = SimulatedUnixFlavour.Linux)
            |> List.map (fun m -> m.Rows)

        linuxCells
        |> List.exists (List.forall (fun row -> row.Cell = row.Declared))
        |> shouldEqual true

        linuxCells
        |> List.exists (List.forall (fun row -> row.Cell = 16u))
        |> shouldEqual true

    /// 6.18.0 is the first release containing commit 1fb0e471611d. These rows
    /// are its edges, including two that a comparison of the wrong component
    /// first would get wrong.
    [<Test>]
    let ``Linux stores the length first from 6.18.0`` () : unit =
        let at (major : uint32) (minor : uint32) (patch : uint32) =
            SimulatedUnixPlatform.createOrFail
                "test"
                (SimulatedUnixKernel.Linux
                    {
                        Major = major
                        Minor = minor
                        Patch = patch
                    })
                SimulatedUnixArchitecture.X64
                SimulatedPageSize.FourKiB
                "test"
            |> SimulatedUnixPlatform.getSockNameFaultLength

        at 6u 17u UInt32.MaxValue |> shouldEqual GetSockNameFaultLength.Untouched
        at 6u 18u 0u |> shouldEqual GetSockNameFaultLength.AlreadyReported
        at 5u 99u 0u |> shouldEqual GetSockNameFaultLength.Untouched
        at 7u 0u 0u |> shouldEqual GetSockNameFaultLength.AlreadyReported

        SimulatedUnixPlatform.getSockNameFaultLength SimulatedUnixPlatform.linuxX64
        |> shouldEqual GetSockNameFaultLength.Untouched

        SimulatedUnixPlatform.getSockNameFaultLength SimulatedUnixPlatform.linuxArm64
        |> shouldEqual GetSockNameFaultLength.AlreadyReported

        SimulatedUnixPlatform.getSockNameFaultLength SimulatedUnixPlatform.macOsArm64
        |> shouldEqual GetSockNameFaultLength.Untouched

/// `getsockname(2)`'s fault path put to the kernel running the suite and to a
/// model of that same kernel: its flavour, its architecture and, on Linux, its
/// own version, read from `uname`. So it holds on whichever kernel it runs on,
/// on either side of the 6.18 change.
[<TestFixture>]
module TestSockNameFaultLengthAgainstHost =

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "bind", SetLastError = true)>]
    extern int private hostBind(int fd, byte[] address, uint32 length)

    [<DllImport("libc", EntryPoint = "getsockname", SetLastError = true)>]
    extern int private hostGetSockName(int fd, nativeint address, nativeint length)

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
    let private PROT_NONE = 0

    let private mapPrivateAnonymous (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0x02 ||| 0x20
        | SimulatedUnixFlavour.Darwin -> 0x02 ||| 0x1000

    /// One reserved page nothing can be mapped over, whose address faults on
    /// every access. Never unmapped: it is a process-lifetime fixture.
    let private faultingPage : Lazy<uint64> =
        lazy
            match HostPlatform.flavour () with
            | None -> failwith "TestSockNameFaultLengthAgainstHost: no Unix host, so nothing to reserve"
            | Some flavour ->
                let page =
                    hostMmap (0n, unativeint Environment.SystemPageSize, PROT_NONE, mapPrivateAnonymous flavour, -1, 0L)

                if page = -1n then
                    failwith
                        $"TestSockNameFaultLengthAgainstHost: mmap failed with errno %d{Marshal.GetLastPInvokeError ()}"

                uint64 (int64 page)

    let private lengths : uint32 list =
        [
            0u
            1u
            2u
            4u
            7u
            8u
            13u
            15u
            16u
            17u
            100u
            128u
            4096u
            0x7fff_ffffu
            0x8000_0000u
            UInt32.MaxValue
        ]

    /// `sockaddr_in` for 127.0.0.1, port 0, as `platform` lays it out.
    let private loopbackAnyPort (platform : SimulatedUnixPlatform) : byte[] =
        CopyIn.inet platform (InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress 0us)

    /// The errno and the length cell after one host call.
    let private hostCall (fd : int) (address : nativeint) (declared : uint32) : int * uint32 =
        let cell = Marshal.AllocHGlobal 4

        try
            Marshal.WriteInt32 (cell, int declared)
            Marshal.SetLastPInvokeError 0

            let errno =
                if hostGetSockName (fd, address, cell) = 0 then
                    0
                else
                    Marshal.GetLastPInvokeError ()

            errno, uint32 (Marshal.ReadInt32 cell)
        finally
            Marshal.FreeHGlobal cell

    let private modelCall
        (fd : int)
        (destination : UserBuffer)
        (declared : uint32)
        (system : UnixSystem<int, string>)
        : int * uint32
        =
        let platform = system.Machine.UnixPlatform

        match UnixSocket.getsockname fd destination declared system with
        | Ok (GetSockNameAnswer.Reported (_, reported)) -> 0, uint32 reported
        | Ok (GetSockNameAnswer.Failed (error, overwritten)) ->
            UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) error,
            (match overwritten with
             | Some length -> uint32 length
             | None -> declared)
        | Error refusal -> failwith $"the model refused: %s{GetSockNameRefusal.describe refusal}"

    [<Test>]
    let ``getsockname's fault path answers as this kernel does`` () : unit =
        HostPlatform.onUnixHostKernel (fun platform ->
            if not BitConverter.IsLittleEndian then
                Assert.Ignore "the model's presets are little-endian machines"

            let fd = hostSocket (AF_INET, SOCK_STREAM, 0)

            if fd < 0 then
                failwith $"socket failed with errno %d{Marshal.GetLastPInvokeError ()}"

            try
                let address = loopbackAnyPort platform

                if hostBind (fd, address, 16u) <> 0 then
                    failwith $"bind failed with errno %d{Marshal.GetLastPInvokeError ()}"

                let system =
                    UnixSystem.initial platform
                    |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

                let modelFd, system =
                    NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

                let system =
                    match CopyIn.bind modelFd UserBuffer.Mapped 16u address system with
                    | Ok (BindAnswer.Bound _, system) -> system
                    | other -> failwith $"the model's bind: %A{other}"

                let destinations =
                    [
                        "null", 0n, UserBuffer.Unmapped 0UL
                        "unmapped", nativeint (int64 faultingPage.Value), UserBuffer.Unmapped faultingPage.Value
                    ]

                let disagreements =
                    [
                        for name, hostAddress, modelDestination in destinations do
                            for declared in lengths do
                                let host = hostCall fd hostAddress declared
                                let model = modelCall modelFd modelDestination declared system

                                if host <> model then
                                    $"%s{name} at %d{declared}: the host answered (errno, cell) %A{host}, the model %A{model}"
                    ]

                if not disagreements.IsEmpty then
                    failwith $"on %O{platform}:\n%s{String.Join ('\n', disagreements)}"
            finally
                hostClose fd |> ignore<int>
        )
