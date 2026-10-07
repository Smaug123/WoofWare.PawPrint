namespace WoofWare.PosixKernel.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixPoll.epollCreate1`: its flag screen, and the epoll instance it creates.
///
/// Measured by `docs/plans/2026-08-23-posix-kernel-extraction/epoll-wait.c`
/// (section A) on Linux 6.18.5 aarch64, 2026-09-27: flags 0 and
/// `EPOLL_CLOEXEC` (0x80000) create an epoll instance on the lowest free
/// descriptor; every other single bit, `EPOLL_CLOEXEC` beside any other bit, -1
/// and `INT_MIN` are EINVAL. The host test below repeats the sweep on whatever
/// Linux runs the suite, which in CI is x86-64.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEpollCreate1 =

    let private linux : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private darwin : UnixSystem<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.macOsArm64
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private cloExec : int = 0x80000

    /// The flags the probe swept, each with whether it created an epoll instance.
    let private measured : (int * bool) list =
        let singleBits = [ 0..31 ] |> List.map (fun bit -> 1 <<< bit)

        [
            yield 0, true
            for bit in singleBits do
                yield bit, (bit = cloExec)
            for bit in singleBits do
                if bit <> cloExec then
                    yield cloExec ||| bit, false
            yield -1, false
            yield System.Int32.MinValue, false
        ]

    let private created (flags : int) (system : UnixSystem<int, string>) : bool =
        match UnixPoll.epollCreate1 flags system with
        | Ok (Ok _) -> true
        | Ok (Error UnixError.EINVAL) -> false
        | other -> failwith $"flags 0x%x{flags}: expected an epoll instance or EINVAL, got %A{other}"

    [<Test>]
    let ``the flag constant is the measured EPOLL_CLOEXEC`` () : unit =
        EpollCreateFlags.CloseOnExec |> shouldEqual cloExec

    [<Test>]
    let ``every measured flag value is answered as Linux answered it`` () : unit =
        measured.Length |> shouldEqual 66

        for flags, expected in measured do
            if created flags linux <> expected then
                failwith $"flags 0x%x{flags}: measured %b{expected}, modelled %b{not expected}"

    /// Values that stress the screen: the two accepted ones, single bits, and
    /// arbitrary words.
    let private flagsGen : Gen<int> =
        Gen.oneof
            [
                Gen.elements [ 0 ; cloExec ]
                Gen.choose (0, 31) |> Gen.map (fun bit -> 1 <<< bit)
                Gen.choose (0, 31) |> Gen.map (fun bit -> cloExec ||| (1 <<< bit))
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    [<Test>]
    let ``an epoll instance is created exactly for 0 and EPOLL_CLOEXEC, on the lowest free descriptor`` () : unit =
        // A descriptor table with holes, so "lowest free" is not "next".
        let epoll, queueSystem =
            FileDescriptorRegistry.createEpoll (UnixSystemState.fileDescriptors linux)

        let _, withTwo = FileDescriptorRegistry.createEpoll queueSystem

        let holed =
            UnixSystemState.withFileDescriptors
                (match FileDescriptorRegistry.dropDescriptor linux.Process.ProcessId epoll withTwo with
                 | Ok (registry, _) -> registry
                 | Error error -> failwith $"expected the close to succeed, got %O{error}")
                linux

        let property (flags : int) : unit =
            match UnixPoll.epollCreate1 flags holed with
            | Ok (Ok (fd, after)) ->
                (flags = 0 || flags = cloExec) |> shouldEqual true
                fd |> shouldEqual epoll

                let expectedFd, expectedRegistry =
                    FileDescriptorRegistry.createEpoll (UnixSystemState.fileDescriptors holed)

                fd |> shouldEqual expectedFd

                // EPOLL_CLOEXEC gives the descriptor FD_CLOEXEC, and that is
                // all it changes.
                let expectedRegistry =
                    FileDescriptorRegistry.setFlags
                        fd
                        { DescriptorFlags.none with
                            CloseOnExec = (flags = cloExec)
                        }
                        expectedRegistry

                after
                |> shouldEqual (UnixSystemState.withFileDescriptors expectedRegistry holed)
            | Ok (Error error) ->
                error |> shouldEqual UnixError.EINVAL
                (flags = 0 || flags = cloExec) |> shouldEqual false
            | Error refusal -> failwith $"unexpected refusal %s{EpollCreateRefusal.describe refusal}"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen flagsGen) property)

    [<Test>]
    let ``Darwin has no epoll, so every call is refused`` () : unit =
        let property (flags : int) : unit =
            UnixPoll.epollCreate1 flags darwin
            |> shouldEqual (Error (EpollCreateRefusal.UnmodelledFlavour SimulatedUnixFlavour.Darwin))

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen flagsGen) property)

    [<DllImport("libc", EntryPoint = "epoll_create1", SetLastError = true)>]
    extern int private hostEpollCreate1(int flags)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<Test>]
    let ``the flag screen is this Linux host's`` () : unit =
        HostPlatform.onUnixHostPreset (fun platform ->
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> Assert.Ignore "epoll is a Linux interface"
            | SimulatedUnixFlavour.Linux -> ()

            let modelled =
                UnixSystem.initial<int, string> platform
                |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

            for flags, _ in measured do
                let fd = hostEpollCreate1 flags

                let hostCreated =
                    if fd >= 0 then
                        hostClose fd |> ignore<int>
                        true
                    else
                        let errno = Marshal.GetLastPInvokeError ()

                        if errno <> 22 then
                            failwith $"epoll_create1(0x%x{flags}) on this host failed with errno %d{errno}, not EINVAL"

                        false

                if hostCreated <> created flags modelled then
                    failwith
                        $"epoll_create1(0x%x{flags}): this host created an epoll instance: %b{hostCreated}; the model: %b{not hostCreated}."
        )
