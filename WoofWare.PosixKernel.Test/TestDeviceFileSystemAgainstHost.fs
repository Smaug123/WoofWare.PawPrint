namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Measures the host's `/dev` and checks the model's device filesystem against
/// it: the device nodes' stat fields, the filesystem type `/dev` reports, and
/// `EXDEV` for a rename across it.
///
/// Only a Linux host can falsify anything here: Darwin's devfs is not modelled,
/// and on a macOS host each test checks only that the model refuses.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDeviceFileSystemAgainstHost =

    let private context : string = "TestDeviceFileSystemAgainstHost"

    [<DllImport("libc", EntryPoint = "rename", SetLastError = true)>]
    extern int private hostRename(string source, string destination)

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    let private booted (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour) UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot

    let private modelStatus (system : UnixSystem<int, string>) (path : string) : Result<FileStatusAnswer, StatRefusal> =
        UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofText path) system

    [<Test>]
    let ``each device node's link count, device number and mode are the host's`` () : unit =
        HostStat.onMeasuredHost (fun flavour ->
            let system = booted flavour

            for path in [ "/dev/null" ; "/dev/urandom" ] do
                match flavour, modelStatus system path with
                | SimulatedUnixFlavour.Darwin, Error (StatRefusal.Path (PathRefusal.UnmodelledFileSystem _)) -> ()
                | SimulatedUnixFlavour.Darwin, other ->
                    failwith $"%s{path} on Darwin: expected a refusal, got %A{other}"
                | SimulatedUnixFlavour.Linux, Ok (FileStatusAnswer.Reported status) ->
                    HostStat.ofModel status |> shouldEqual (HostStat.lstat flavour path)

                    let hostMode = int (File.GetUnixFileMode path)

                    if status.Mode &&& 0o7777 <> hostMode then
                        failwith
                            $"%s{path}: the model's permission bits are 0o%o{status.Mode &&& 0o7777}, the host's 0o%o{hostMode}"
                | SimulatedUnixFlavour.Linux, other -> failwith $"%s{path} on Linux: expected a status, got %A{other}"
        )

    [<Test>]
    let ``dev reports the host's filesystem type`` () : unit =
        HostStat.onMeasuredHost (fun flavour ->
            let system = booted flavour

            match flavour, UnixPathResolution.statfs (PathArg.ofText "/dev/null") system with
            | SimulatedUnixFlavour.Darwin, Error (PathRefusal.UnmodelledFileSystem _) -> ()
            | SimulatedUnixFlavour.Darwin, other -> failwith $"statfs on Darwin: expected a refusal, got %A{other}"
            | SimulatedUnixFlavour.Linux, Ok (FileSystemStatisticsAnswer.Reported statistics) ->
                // O_RDONLY: the host's /dev/null is readable by everyone.
                let fd = hostOpen ("/dev/null", 0, 0)

                if fd < 0 then
                    failwith $"opening the host's /dev/null failed: errno %d{Marshal.GetLastPInvokeError ()}"

                try
                    HostFileSystemType.typeFieldsFor flavour fd
                    |> shouldEqual (Ok (FileSystemStatistics.typeFields statistics))
                finally
                    hostClose fd |> ignore<int>
            | SimulatedUnixFlavour.Linux, other -> failwith $"statfs on Linux: expected an answer, got %A{other}"
        )

    [<Test>]
    let ``a rename into dev is EXDEV here exactly when the model says so`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let system = booted flavour

            // A name no real /dev holds, so that the host's rename cannot
            // succeed and the model's walk must answer without knowing it.
            let unique () = Guid.NewGuid().ToString "N"
            let destination = $"/dev/posixkernel-%s{unique ()}"

            let model =
                UnixNamespace.rename (PathArg.ofText "/nonexistent") (PathArg.ofText destination) system

            match flavour with
            | SimulatedUnixFlavour.Darwin ->
                // Darwin looks the source up first, so a missing one is ENOENT
                // before the walk reaches /dev (measured, `devices.c` NS rows).
                match model with
                | Ok (SyscallAnswer.Failed UnixError.ENOENT, _) -> ()
                | other -> failwith $"rename on Darwin: expected ENOENT, got %A{other}"
            | SimulatedUnixFlavour.Linux ->

            let source = Path.Combine (Path.GetTempPath (), $"posixkernel-%s{unique ()}")

            File.WriteAllBytes (source, [||])

            try
                Marshal.SetLastPInvokeError 0
                let result = hostRename (source, destination)
                let errno = Marshal.GetLastPInvokeError ()

                if result = 0 then
                    failwith $"the host renamed %s{source} into its /dev; this test assumes it cannot"

                UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux errno
                |> shouldEqual (Some UnixError.EXDEV)

                match model with
                | Ok (SyscallAnswer.Failed UnixError.EXDEV, _) -> ()
                | other -> failwith $"rename on Linux: expected EXDEV, got %A{other}"
            finally
                File.Delete source
        )
