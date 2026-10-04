namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Opens the host's `/dev/null` and `/dev/urandom` and checks that every
/// syscall through those descriptors answers what the model's descriptors onto
/// its own nodes answer: one table of rows per device, each a return value or
/// an errno.
///
/// Only a Linux host can falsify anything here: Darwin's devfs is not modelled,
/// and on a macOS host the test checks only that the model refuses the path.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDeviceDescriptorsAgainstHost =

    let private context : string = "TestDeviceDescriptorsAgainstHost"

    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private PollFd =
        val mutable Fd : int
        val mutable Events : int16
        val mutable Revents : int16

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostReadAt(int fd, nativeint buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWriteAt(int fd, nativeint buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "pread", SetLastError = true)>]
    extern nativeint private hostPRead(int fd, byte[] buffer, unativeint count, int64 offset)

    [<DllImport("libc", EntryPoint = "pwrite", SetLastError = true)>]
    extern nativeint private hostPWrite(int fd, byte[] buffer, unativeint count, int64 offset)

    [<DllImport("libc", EntryPoint = "lseek", SetLastError = true)>]
    extern int64 private hostLSeek(int fd, int64 offset, int whence)

    [<DllImport("libc", EntryPoint = "ftruncate", SetLastError = true)>]
    extern int private hostFTruncate(int fd, int64 length)

    /// Returns the errno rather than setting it.
    [<DllImport("libc", EntryPoint = "posix_fadvise")>]
    extern int private hostFadvise(int fd, int64 offset, int64 length, int advice)

    [<DllImport("libc", EntryPoint = "ioctl", SetLastError = true)>]
    extern int private hostIoctlInt(int fd, unativeint request, int& value)

    [<DllImport("libc", EntryPoint = "tcgetattr", SetLastError = true)>]
    extern int private hostTcGetAttr(int fd, byte[] termios)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll([<In ; Out>] PollFd[] fds, unativeint count, int timeout)

    [<DllImport("libc", EntryPoint = "epoll_create1", SetLastError = true)>]
    extern int private hostEpollCreate1(int flags)

    [<DllImport("libc", EntryPoint = "epoll_ctl", SetLastError = true)>]
    extern int private hostEpollCtl(int epfd, int op, int fd, byte[] event)

    [<DllImport("libc", EntryPoint = "flock", SetLastError = true)>]
    extern int private hostFlock(int fd, int operation)

    /// What a call answered: its return value, or the errno it failed with.
    type private Answer = Result<int64, UnixError>

    let private hostAnswer (result : int64) : Answer =
        if result < 0L then
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux errno with
            | Some error -> Error error
            | None -> failwith $"the host answered errno %d{errno}, which UnixError does not name"
        else
            Ok result

    let private fromErrno (errno : int) : Answer =
        if errno = 0 then
            Ok 0L
        else

        match UnixError.ofRawErrnoUnder RawErrnoNumbering.Linux errno with
        | Some error -> Error error
        | None -> failwith $"the host answered errno %d{errno}, which UnixError does not name"

    let private fionread : unativeint = 0x541Bun

    let private hostFionread (fd : int) : Answer =
        let mutable available = -12345
        hostAnswer (int64 (hostIoctlInt (fd, fionread, &available)))

    let private hostPollOnce (fd : int) (events : int16) : Answer =
        let mutable entry = PollFd ()
        entry.Fd <- fd
        entry.Events <- events
        let entries = [| entry |]

        hostAnswer (int64 (hostPoll (entries, 1un, 0)))
        |> Result.map (fun _ -> int64 entries.[0].Revents)

    let private rdwr : int = 2
    let private wronly : int = 1

    let private reading : OpenFlags =
        {
            Access = FileAccessMode.ReadWrite
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    let private booted (flavour : SimulatedUnixFlavour) : UnixSystem<int, string> =
        UnixSystem.initial (HostPlatform.platformOf flavour) UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.boot

    let private modelOpen (access : FileAccessMode) (path : string) (system : UnixSystem<int, string>) =
        match
            OpenFlagWords.openPath
                { reading with
                    Access = access
                }
                (PathArg.ofText path)
                0
                system
        with
        | Ok (SyscallAnswer.Completed fd, system) -> int fd, system
        | other -> failwith $"the model's open(%s{path}): %A{other}"

    let private ofSyscall (answer : SyscallAnswer) : Answer =
        match answer with
        | SyscallAnswer.Completed value -> Ok value
        | SyscallAnswer.Failed error -> Error error

    let private ofRead (answer : ReadAnswer) : Answer =
        match answer with
        | ReadAnswer.Completed bytes -> Ok (int64 bytes.Length)
        | ReadAnswer.Drawn draw -> Ok (int64 (EntropyDraw.count draw))
        | ReadAnswer.Failed error -> Error error

    let private ofWrite (answer : WriteAnswer) : Answer =
        match answer with
        | WriteAnswer.Completed count -> Ok count
        | WriteAnswer.Failed error -> Error error

    /// Every row, as (description, the host's answer, the model's answer), for
    /// the device at `path`.
    let private rows (flavour : SimulatedUnixFlavour) (path : string) : (string * Answer * Answer) list =
        let host = hostOpen (path, rdwr, 0)
        let hostWriteOnly = hostOpen (path, wronly, 0)

        if host < 0 || hostWriteOnly < 0 then
            failwith $"opening the host's %s{path} failed: errno %d{Marshal.GetLastPInvokeError ()}"

        let fd, system = modelOpen FileAccessMode.ReadWrite path (booted flavour)
        let writeOnly, system = modelOpen FileAccessMode.WriteOnly path system

        let read (fd : int) (buffer : UserBuffer) (count : uint64) : Answer =
            match UnixReadWrite.read 0 fd buffer count system with
            | Ok (ReadOutcome.Answered answer, _) -> ofRead answer
            | other -> failwith $"the model's read: %A{other}"

        let admitWrite (buffer : UserBuffer) (count : uint64) : Answer =
            match UnixReadWrite.admitWrite 0 fd buffer count system with
            | Ok (WriteOutcome.Returns (WriteAdmission.Answered answer, _)) -> ofWrite answer
            | Ok (WriteOutcome.Returns (WriteAdmission.Transfer count, after)) ->
                match UnixReadWrite.write 0 fd (ImmutableArray.CreateRange (Array.zeroCreate<byte> count)) after with
                | Ok (WriteOutcome.Returns (answer, _)) -> ofWrite answer
                | other -> failwith $"the model's write: %A{other}"
            | other -> failwith $"the model's admitWrite: %A{other}"

        try
            [
                for whence in -1 .. 6 do
                    for offset in [ 0L ; -5L ; System.Int64.MaxValue ] do
                        $"lseek %d{offset} %d{whence}",
                        hostAnswer (hostLSeek (host, offset, whence)),
                        (match UnixDescriptor.lseek fd offset whence system with
                         | Ok (answer, _) -> ofSyscall answer
                         | Error refusal -> failwith $"the model's lseek: %A{refusal}")

                for count in [ 0 ; 1 ; 7 ; 4096 ; 65536 ] do
                    $"read %d{count}",
                    hostAnswer (int64 (hostRead (host, Array.zeroCreate (max count 1), unativeint count))),
                    read fd UserBuffer.Mapped (uint64 count)

                for count in [ 0 ; 1 ; 100 ] do
                    $"read NULL %d{count}",
                    hostAnswer (int64 (hostReadAt (host, 0n, unativeint count))),
                    read fd (UserBuffer.Unmapped 0UL) (uint64 count)

                "read through O_WRONLY",
                hostAnswer (int64 (hostRead (hostWriteOnly, Array.zeroCreate 16, 16un))),
                read writeOnly UserBuffer.Mapped 16UL

                for count in [ 0 ; 1 ; 4096 ] do
                    $"write %d{count}",
                    hostAnswer (int64 (hostWrite (host, Array.zeroCreate (max count 1), unativeint count))),
                    admitWrite UserBuffer.Mapped (uint64 count)

                for count in [ 0 ; 1 ; 16 ] do
                    $"write NULL %d{count}",
                    hostAnswer (int64 (hostWriteAt (host, 0n, unativeint count))),
                    admitWrite (UserBuffer.Unmapped 0UL) (uint64 count)

                for offset in [ 0L ; 1L <<< 40 ; -1L ; System.Int64.MaxValue ] do
                    $"pread 16@%d{offset}",
                    hostAnswer (int64 (hostPRead (host, Array.zeroCreate 16, 16un, offset))),
                    (match UnixReadWrite.pread 0 fd UserBuffer.Mapped 16UL offset system with
                     | Ok (answer, _) -> ofRead answer
                     | Error refusal -> failwith $"the model's pread: %A{refusal}")

                    $"pwrite 16@%d{offset}",
                    hostAnswer (int64 (hostPWrite (host, Array.zeroCreate 16, 16un, offset))),
                    (match UnixReadWrite.admitPWrite 0 fd UserBuffer.Mapped 16UL offset system with
                     | Ok (PWriteAdmission.Answered answer) -> ofWrite answer
                     | Ok (PWriteAdmission.Transfer count) ->
                         match
                             UnixReadWrite.pwrite
                                 0
                                 fd
                                 (ImmutableArray.CreateRange (Array.zeroCreate<byte> count))
                                 offset
                                 system
                         with
                         | Ok (answer, _) -> ofWrite answer
                         | Error refusal -> failwith $"the model's pwrite: %A{refusal}"
                     | Error refusal -> failwith $"the model's admitPWrite: %A{refusal}")

                for length in [ 0L ; 10L ; -1L ] do
                    $"ftruncate %d{length}",
                    hostAnswer (int64 (hostFTruncate (host, length))),
                    (match UnixDescriptor.ftruncate fd length system with
                     | Ok (answer, _) -> ofSyscall answer
                     | Error refusal -> failwith $"the model's ftruncate: %A{refusal}")

                for offset, length, advice in
                    [
                        for advice in -1 .. 7 -> 0L, 0L, advice
                        yield -1L, 0L, 0
                        yield 0L, -1L, 0
                    ] do
                    $"posix_fadvise %d{offset} %d{length} %d{advice}",
                    fromErrno (hostFadvise (host, offset, length, advice)),
                    (match UnixDescriptor.posixFadvise fd offset length advice system with
                     | Ok FileAdviceAnswer.Completed -> Ok 0L
                     | Ok (FileAdviceAnswer.Failed error) -> Error error
                     | Error refusal -> failwith $"the model's posix_fadvise: %A{refusal}")

                "FIONREAD",
                hostFionread host,
                (match UnixDescriptor.bytesAvailable fd UserBuffer.Mapped system with
                 | Ok (BytesAvailableAnswer.Reported count) -> Ok (int64 count)
                 | Ok (BytesAvailableAnswer.Failed error) -> Error error
                 | Error refusal -> failwith $"the model's FIONREAD: %A{refusal}")

                "tcgetattr",
                hostAnswer (int64 (hostTcGetAttr (host, Array.zeroCreate 256))),
                (match UnixDescriptor.terminalAttributes fd system with
                 | TerminalAttributesAnswer.NotATerminal error -> Error error)

                for events in [ 0s ; 1s ; 4s ; 0x7FFFs ] do
                    $"poll %d{events}",
                    hostPollOnce host events,
                    (match
                        UnixPoll.poll
                            0
                            [
                                {
                                    Fd = fd
                                    Events = events
                                }
                            ]
                            0
                            system
                     with
                     | Ok (PollOutcome.Answered ([ revents ], _), _) -> Ok (int64 revents)
                     | other -> failwith $"the model's poll: %A{other}")

                "epoll_ctl ADD",
                (let port = hostEpollCreate1 0

                 try
                     // struct epoll_event, EPOLLIN, laid out wide enough for either
                     // architecture's packing.
                     let event = Array.zeroCreate<byte> 16
                     event.[0] <- 1uy
                     hostAnswer (int64 (hostEpollCtl (port, 1, host, event)))
                 finally
                     hostClose port |> ignore<int>),
                (let queueFd, registry =
                    FileDescriptorRegistry.createEpoll system.Process.FileDescriptors

                 let system =
                     { system with
                         Process =
                             { system.Process with
                                 FileDescriptors = registry
                             }
                     }

                 match UnixPoll.epollCtl queueFd 1 fd (EpollEventArgument.Readable (1u, 0UL)) system with
                 | Ok (EpollCtlAnswer.Changed, _) -> Ok 0L
                 | Ok (EpollCtlAnswer.Failed EpollCtlError.TargetNotPollable, _) -> Error UnixError.EPERM
                 | other -> failwith $"the model's epoll_ctl: %A{other}")

                "flock LOCK_EX|LOCK_NB, then through another description",
                (let first = hostAnswer (int64 (hostFlock (host, 6)))
                 let second = hostAnswer (int64 (hostFlock (hostWriteOnly, 6)))
                 hostFlock (host, 8) |> ignore<int>
                 first |> Result.bind (fun _ -> second)),
                (match UnixDescriptor.flock 0 fd 6 system with
                 | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed _), locked) ->
                     match UnixDescriptor.flock 0 writeOnly 6 locked with
                     | Ok (SyscallOutcome.Answered answer, _) -> ofSyscall answer
                     | other -> failwith $"the model's second flock: %A{other}"
                 | other -> failwith $"the model's first flock: %A{other}")
            ]
        finally
            hostClose host |> ignore<int>
            hostClose hostWriteOnly |> ignore<int>

    [<Test>]
    let ``every syscall through a device's descriptor answers what the host's does`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            match flavour with
            | SimulatedUnixFlavour.Darwin ->
                for path in [ "/dev/null" ; "/dev/urandom" ] do
                    match OpenFlagWords.openPath reading (PathArg.ofText path) 0 (booted flavour) with
                    | Error (OpenRefusal.Path (PathRefusal.UnmodelledFileSystem _)) -> ()
                    | other -> failwith $"open %s{path} on Darwin: expected a refusal, got %A{other}"
            | SimulatedUnixFlavour.Linux ->

            for path in [ "/dev/null" ; "/dev/urandom" ] do
                let mismatches =
                    rows flavour path
                    |> List.filter (fun (_, host, model) -> host <> model)
                    |> List.map (fun (row, host, model) -> $"%s{path} %s{row}: host %A{host}, model %A{model}")

                mismatches |> shouldEqual []
        )
