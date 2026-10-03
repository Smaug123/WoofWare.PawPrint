namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Every syscall on a pipe, made on a real pipe on the kernel running the suite
/// and on the model, with the same random arguments: what each call answered,
/// the bytes moved, what `poll`, `FIONREAD` and `fstat` then report of each end,
/// and which timestamps moved.
///
/// Each host falsifies its own column: macOS locally, Linux in CI, which is
/// x86-64 and so the one place the x86-64 `pipe2` flags are measured.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipeAgainstHost =

    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private PollFd =
        val mutable Fd : int
        val mutable Events : int16
        val mutable Revents : int16

    /// `SystemNative_FStat`'s `FileStatus` (`pal_io.h`), which states every field
    /// in fixed-width types whatever the platform's `struct stat` is.
    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private HostStatus =
        val mutable Flags : int
        val mutable Mode : int
        val mutable Uid : uint32
        val mutable Gid : uint32
        val mutable Size : int64
        val mutable ATime : int64
        val mutable ATimeNsec : int64
        val mutable MTime : int64
        val mutable MTimeNsec : int64
        val mutable CTime : int64
        val mutable CTimeNsec : int64
        val mutable BirthTime : int64
        val mutable BirthTimeNsec : int64
        val mutable Dev : int64
        val mutable RDev : int64
        val mutable Ino : int64
        val mutable UserFlags : uint32

    // Absent from Darwin's libc before 27, so only the test of `pipe2`'s own
    // flags calls it, and skips where it is missing.
    [<DllImport("libc", EntryPoint = "pipe2", SetLastError = true)>]
    extern int private hostPipe2(int[] fds, int flags)

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "dup", SetLastError = true)>]
    extern int private hostDup(int fd)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWriteAt(int fd, nativeint buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostReadAt(int fd, nativeint buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll([<In ; Out>] PollFd[] fds, unativeint count, int timeout)

    [<DllImport("libc", EntryPoint = "lseek", SetLastError = true)>]
    extern int64 private hostLSeek(int fd, int64 offset, int whence)

    [<DllImport("libc", EntryPoint = "isatty", SetLastError = true)>]
    extern int private hostIsATty(int fd)

    [<DllImport("libc", EntryPoint = "geteuid")>]
    extern uint32 private hostGetEUid()

    [<DllImport("libc", EntryPoint = "getegid")>]
    extern uint32 private hostGetEGid()

    // `fcntl(2)` and `ioctl(2)` are variadic, which a P/Invoke cannot call
    // portably, so these go through the runtime's own fixed-arity wrappers.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")>]
    extern int private hostBytesAvailable(nativeint fd, int& available)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FStat", SetLastError = true)>]
    extern int private hostFStat(nativeint fd, HostStatus& output)

    /// One call, naming a descriptor by its slot in the list the run keeps.
    [<RequireQualifiedAccess>]
    type HostPipeOp =
        | Write of slot : int * count : int * mapped : bool
        | Read of slot : int * count : int * mapped : bool
        | SetNonBlocking of slot : int * value : bool
        | Dup of slot : int
        | Close of slot : int

    let private opGen : Gen<HostPipeOp> =
        let slot = Gen.choose (0, 3)

        let count =
            Gen.frequency
                [
                    3, Gen.choose (0, 20)
                    2, Gen.elements [ 511 ; 512 ; 513 ; 4095 ; 4096 ; 4097 ; 16383 ; 16384 ; 65536 ; 70000 ]
                    2, Gen.choose (1, 9000)
                ]

        let mapped = Gen.frequency [ 6, Gen.constant true ; 1, Gen.constant false ]

        Gen.frequency
            [
                6, Gen.map3 (fun s c m -> HostPipeOp.Write (s, c, m)) slot count mapped
                5, Gen.map3 (fun s c m -> HostPipeOp.Read (s, c, m)) slot count mapped
                2, Gen.map2 (fun s v -> HostPipeOp.SetNonBlocking (s, v)) slot (Gen.elements [ true ; false ])
                1, Gen.map HostPipeOp.Dup slot
                1, Gen.map HostPipeOp.Close slot
            ]

    let private payload (start : int) (count : int) : byte[] =
        Array.init count (fun i -> byte ((start + i) % 251))

    /// What the host answered: the count, or the model's name for the errno.
    let private hostAnswer (numbering : RawErrnoNumbering) (result : nativeint) : Result<int, UnixError> =
        if result >= 0n then
            Ok (int result)
        else
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrnoUnder numbering errno with
            | Some error -> Error error
            | None -> failwith $"the host answered errno %d{errno}, which the model has no name for"

    let private hostStatus (fd : int) : HostStatus =
        let mutable status = HostStatus ()

        if hostFStat (nativeint fd, &status) <> 0 then
            failwith $"host fstat of %d{fd} failed: errno %d{Marshal.GetLastPInvokeError ()}"

        status

    let private hostTimes (fd : int) : (int64 * int64) * (int64 * int64) * (int64 * int64) =
        let s = hostStatus fd
        (s.ATime, s.ATimeNsec), (s.MTime, s.MTimeNsec), (s.CTime, s.CTimeNsec)

    let private modelTimes
        (fd : int)
        (system : UnixSystem<int, string>)
        : UnixTimestamp * UnixTimestamp * UnixTimestamp
        =
        match UnixPathResolution.fstat fd system with
        | Ok (FileStatusAnswer.Reported s) -> s.AccessTime, s.ModificationTime, s.StatusChangeTime
        | other -> failwith $"model fstat of %d{fd}: %A{other}"

    let private hostLevel (fd : int) : int16 =
        // Every readiness bit either flavour's `<poll.h>` names below 0x200.
        let mutable entry = PollFd ()
        entry.Fd <- fd
        entry.Events <- 0x1c7s
        let fds = [| entry |]

        if hostPoll (fds, 1un, 0) < 0 then
            failwith $"poll failed: errno %d{Marshal.GetLastPInvokeError ()}"

        fds.[0].Revents

    /// Make `call` on `fd` with `O_NONBLOCK` set, restoring the description's
    /// own flag afterwards: a host call that would sleep, because the model
    /// was wrong to answer it, then fails the comparison rather than hanging the
    /// suite.
    let private withNonBlockingForOneCall (fd : int) (wasNonBlocking : bool) (call : unit -> 'a) : 'a =
        if wasNonBlocking then
            call ()
        else
            hostSetNonBlocking (nativeint fd, 1) |> ignore

            try
                call ()
            finally
                hostSetNonBlocking (nativeint fd, 0) |> ignore

    [<Test>]
    let ``a real pipe and the model answer every call on a pipe alike`` () : unit =
        HostPlatform.onUnixHostPreset (fun platform ->
            // A fresh Linux pipe with fewer slots than the model's sixteen is
            // this machine's configuration, not the rule under test.
            TestPipeBufferAgainstHost.requireDefaultLinuxCapacity platform
            let flavour = SimulatedUnixPlatform.flavour platform
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering platform

            let initial =
                let uid = UserId.parseOrFail "TestPipeAgainstHost" (hostGetEUid ())
                let gid = GroupId.parseOrFail "TestPipeAgainstHost" (hostGetEGid ())

                let system : UnixSystem<int, string> =
                    UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                    |> UnixBootImage.withCredentials "TestPipeAgainstHost" (Credentials.ofIds uid gid [])
                    |> UnixBootImage.boot

                // The test host's runtime ignores SIGPIPE, so a write with no
                // reader answers EPIPE there rather than ending it.
                let system =
                    { system with
                        Process =
                            { system.Process with
                                Signals =
                                    SignalState.setDisposition
                                        Signal.SIGPIPE
                                        SignalDisposition.Ignore
                                        system.Process.Signals
                            }
                    }

                // Up a second before the pipe is made, so that its creation is
                // not the epoch that Darwin reports as its birth.
                { system with
                    Machine = UnixMachineState.advanceClock 1_000_000_000L system.Machine
                }

            let property (startNonBlocking : bool, ops : HostPipeOp list) : unit =
                let nonBlockFlag =
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> 0x800
                    | SimulatedUnixFlavour.Darwin -> 0x4

                let flags = if startNonBlocking then nonBlockFlag else 0
                let hostFds = Array.zeroCreate<int> 2

                // `pipe` and then O_NONBLOCK on each end: what `pipe2` with the
                // flag makes, on a host whose libc may have no `pipe2`.
                if hostPipe hostFds <> 0 then
                    failwith $"pipe failed: errno %d{Marshal.GetLastPInvokeError ()}"

                if startNonBlocking then
                    for fd in hostFds do
                        hostSetNonBlocking (nativeint fd, 1) |> shouldEqual 0

                let created, modelFds =
                    match UnixPipe.pipe2 flags UserBuffer.Mapped initial with
                    | Ok (Pipe2Answer.Created (r, w), system) -> system, (r, w)
                    | other -> failwith $"model pipe2: %A{other}"

                let mutable system = created

                // Slot -> (model fd, host fd, end, description group).
                let mutable slots =
                    [
                        fst modelFds, hostFds.[0], PipeEnd.Read, 0
                        snd modelFds, hostFds.[1], PipeEnd.Write, 1
                    ]

                let mutable nonBlocking = Map.ofList [ 0, startNonBlocking ; 1, startNonBlocking ]
                let mutable offered = 0

                // The two ends' fstat fields that are not timestamps agree from
                // the start: the file type and permissions, the owner, and
                // whether the ends share an inode number.
                let hostRead0 = hostStatus hostFds.[0]
                let hostWrite0 = hostStatus hostFds.[1]

                let modelStatus fd =
                    match UnixPathResolution.fstat fd system with
                    | Ok (FileStatusAnswer.Reported s) -> s
                    | other -> failwith $"%A{other}"

                let modelRead0 = modelStatus (fst modelFds)
                let modelWrite0 = modelStatus (snd modelFds)

                (modelRead0.Mode, modelWrite0.Mode)
                |> shouldEqual (hostRead0.Mode, hostWrite0.Mode)

                (UserId.toUInt32 modelRead0.UserId, GroupId.toUInt32 modelRead0.GroupId)
                |> shouldEqual (hostRead0.Uid, hostRead0.Gid)

                (modelRead0.Inode = modelWrite0.Inode)
                |> shouldEqual (hostRead0.Ino = hostWrite0.Ino)

                match flavour with
                | SimulatedUnixFlavour.Darwin ->
                    (modelRead0.DeviceId, hostRead0.Dev, hostWrite0.Dev) |> shouldEqual (0L, 0L, 0L)

                    (modelRead0.BirthTime, hostRead0.BirthTime, hostRead0.BirthTimeNsec)
                    |> shouldEqual (Some UnixTimestamp.epoch, 0L, 0L)
                | SimulatedUnixFlavour.Linux ->
                    // The device is this boot's; only that both ends report one.
                    hostRead0.Dev |> shouldEqual hostWrite0.Dev

                try
                    for i, op in List.indexed ops do
                        let where = $"%O{platform}, call %d{i} (%A{op})"

                        system <-
                            { system with
                                Machine = UnixMachineState.advanceClock 1000L system.Machine
                            }

                        let liveSlots = List.toArray slots

                        let before =
                            liveSlots |> Array.map (fun (m, h, _, _) -> hostTimes h, modelTimes m system)

                        // Set where the model refused a call that would sleep and
                        // the host was asked a non-blocking stand-in for it, which
                        // on Darwin moves a timestamp the refused call never did.
                        let mutable substituted = false

                        let slotOf (index : int) =
                            if liveSlots.Length = 0 then
                                None
                            else
                                Some liveSlots.[index % liveSlots.Length]

                        match op with
                        | HostPipeOp.Write (index, count, mapped) ->
                            match slotOf index with
                            | None -> ()
                            | Some (modelFd, hostFd, _, group) ->

                            let buffer =
                                if mapped then
                                    UserBuffer.Mapped
                                else
                                    UserBuffer.Unmapped 8UL

                            let bytes = payload offered count

                            // `None` for a write that sleeps.
                            let model =
                                match UnixReadWrite.admitWrite system.Leader modelFd buffer (uint64 count) system with
                                | Error refusal -> Error refusal
                                | Ok (WriteOutcome.WouldBlock _) -> Ok None
                                | Ok (WriteOutcome.Returns (WriteAdmission.Answered answer, after)) ->
                                    Ok (Some (answer, after))
                                | Ok (WriteOutcome.ReturnsRaising (WriteAdmission.Answered answer, raised, after)) ->
                                    // The one signal a write raises, discarded
                                    // as the ignored signal it is.
                                    (answer, raised.Signal)
                                    |> shouldEqual (WriteAnswer.Failed UnixError.EPIPE, Signal.SIGPIPE)

                                    after.Process.Signals |> shouldEqual system.Process.Signals
                                    Ok (Some (answer, after))
                                | Ok (WriteOutcome.Returns (WriteAdmission.Transfer n, admitted)) ->
                                    match
                                        UnixReadWrite.write
                                            system.Leader
                                            modelFd
                                            (ImmutableArray.Create<byte> (Array.sub bytes 0 n))
                                            admitted
                                    with
                                    | Error refusal -> Error refusal
                                    | Ok (WriteOutcome.WouldBlock _) -> Ok None
                                    | Ok (WriteOutcome.Returns (answer, after)) -> Ok (Some (answer, after))
                                    | Ok other -> failwith $"%s{where}: model %A{other}"
                                // The host would sleep with part of it in.
                                | Ok (WriteOutcome.Returns (WriteAdmission.TransferThenSleep _, _)) -> Ok None
                                | Ok other -> failwith $"%s{where}: model %A{other}"

                            let hostCall () =
                                if mapped then
                                    hostWrite (hostFd, bytes, unativeint count)
                                else
                                    hostWriteAt (hostFd, 8n, unativeint count)

                            match model with
                            | Ok None ->
                                // The host would sleep; a partial write through a
                                // temporary O_NONBLOCK would leave it holding
                                // what the model does not, so it is not asked.
                                ()
                            | Error refusal -> failwith $"%s{where}: model refused %A{refusal}"
                            | Ok (Some (answer, after)) ->
                                let host =
                                    hostAnswer
                                        numbering
                                        (withNonBlockingForOneCall hostFd nonBlocking.[group] hostCall)

                                match answer, host with
                                | WriteAnswer.Completed n, Ok h when int n = h -> offered <- offered + h
                                | WriteAnswer.Failed e, Error h when e = h -> ()
                                | _ -> failwith $"%s{where}: model %A{answer}, host %A{host}"

                                system <- after
                        | HostPipeOp.Read (index, count, mapped) ->
                            match slotOf index with
                            | None -> ()
                            | Some (modelFd, hostFd, _, group) ->

                            let buffer =
                                if mapped then
                                    UserBuffer.Mapped
                                else
                                    UserBuffer.Unmapped 8UL

                            let target = Array.zeroCreate<byte> (max count 1)

                            let hostCall () =
                                if mapped then
                                    hostRead (hostFd, target, unativeint count)
                                else
                                    hostReadAt (hostFd, 8n, unativeint count)

                            match UnixReadWrite.read system.Leader modelFd buffer (uint64 count) system with
                            | Ok (ReadOutcome.WouldBlock _, _) ->
                                // The host would sleep: asked without blocking,
                                // it must find nothing to take.
                                hostAnswer numbering (withNonBlockingForOneCall hostFd false hostCall)
                                |> shouldEqual (Error UnixError.EAGAIN)

                                substituted <- true
                            | Ok (ReadOutcome.Restarts, _) -> failwith $"%s{where}: a read that never slept restarted"
                            | Error refusal -> failwith $"%s{where}: model refused %A{refusal}"
                            | Ok (ReadOutcome.Answered answer, after) ->
                                let host =
                                    hostAnswer
                                        numbering
                                        (withNonBlockingForOneCall hostFd nonBlocking.[group] hostCall)

                                match answer, host with
                                | ReadAnswer.Completed bytes, Ok h when bytes.Length = h ->
                                    Array.sub target 0 h |> shouldEqual (Seq.toArray bytes)
                                | ReadAnswer.Failed e, Error h when e = h -> ()
                                | _ -> failwith $"%s{where}: model %A{answer}, host %A{host}"

                                system <- after
                        | HostPipeOp.SetNonBlocking (index, value) ->
                            match slotOf index with
                            | None -> ()
                            | Some (modelFd, hostFd, _, group) ->
                                hostSetNonBlocking (nativeint hostFd, (if value then 1 else 0)) |> shouldEqual 0

                                let answer, after = UnixDescriptor.setNonBlocking modelFd value system
                                answer |> shouldEqual SetNonBlockingAnswer.Set
                                system <- after
                                nonBlocking <- Map.add group value nonBlocking
                        | HostPipeOp.Dup index ->
                            match slotOf index with
                            | None -> ()
                            | Some (modelFd, hostFd, pipeEnd, group) ->
                                let hostNew = hostDup hostFd

                                if hostNew < 0 then
                                    failwith $"%s{where}: host dup failed"

                                match UnixDescriptor.dup modelFd system with
                                | SyscallAnswer.Completed modelNew, after ->
                                    slots <- slots @ [ int modelNew, hostNew, pipeEnd, group ]
                                    system <- after
                                | other -> failwith $"%s{where}: model dup %A{other}"
                        | HostPipeOp.Close index ->
                            match slotOf index with
                            | None -> ()
                            | Some (modelFd, hostFd, _, _ as closing) ->
                                hostClose hostFd |> shouldEqual 0

                                match UnixDescriptor.close modelFd system with
                                | Ok (SyscallAnswer.Completed _, after) -> system <- after
                                | other -> failwith $"%s{where}: model close %A{other}"

                                slots <- slots |> List.filter (fun s -> s <> closing)

                        // What every descriptor still open reports now.
                        for modelFd, hostFd, pipeEnd, _ in slots do
                            let mutable available = 0

                            if hostBytesAvailable (nativeint hostFd, &available) <> 0 then
                                failwith $"%s{where}: host FIONREAD failed"

                            UnixDescriptor.bytesAvailable modelFd UserBuffer.Mapped system
                            |> shouldEqual (Ok (BytesAvailableAnswer.Reported available))

                            let h = hostStatus hostFd
                            let m = modelStatus modelFd

                            if (m.Mode, m.Size) <> (h.Mode, h.Size) then
                                failwith
                                    $"%s{where}: fd %O{pipeEnd} end: model mode 0o%o{m.Mode} size %d{m.Size}, host 0o%o{h.Mode} size %d{h.Size}"

                            match flavour with
                            | SimulatedUnixFlavour.Darwin ->
                                // Darwin's poll is not modelled, but while both
                                // ends are open its IN and OUT bits are the
                                // buffer's own readiness.
                                let bothOpen =
                                    List.exists (fun (_, _, e, _) -> e = PipeEnd.Read) slots
                                    && List.exists (fun (_, _, e, _) -> e = PipeEnd.Write) slots

                                if bothOpen then
                                    let pipeId =
                                        match
                                            FileDescriptorRegistry.tryFindTarget modelFd system.Process.FileDescriptors
                                        with
                                        | Some (OpenFileTarget.Pipe (pipeId, _)) -> pipeId
                                        | other -> failwith $"%s{where}: fd %d{modelFd} is %A{other}"

                                    let buffer = (UnixMachineState.pipe pipeId system.Machine).Buffer

                                    let modelReady =
                                        match pipeEnd with
                                        | PipeEnd.Read -> PipeBuffer.readable buffer
                                        | PipeEnd.Write -> PipeBuffer.writable buffer

                                    let bit =
                                        match pipeEnd with
                                        | PipeEnd.Read -> 0x1s
                                        | PipeEnd.Write -> 0x4s

                                    let hostReady = hostLevel hostFd &&& bit <> 0s

                                    if modelReady <> hostReady then
                                        failwith
                                            $"%s{where}: %O{pipeEnd} end: model ready %b{modelReady}, host %b{hostReady}"
                            | SimulatedUnixFlavour.Linux ->
                                let id =
                                    FileDescriptorRegistry.tryFindId modelFd system.Process.FileDescriptors
                                    |> Option.get

                                let model = int16 (LinuxReadiness.ofDescription id system) &&& (0x1c7s ||| 0x18s)
                                let host = hostLevel hostFd

                                if model <> host then
                                    failwith $"%s{where}: %O{pipeEnd} end: model polls 0x%x{model}, host 0x%x{host}"

                            // lseek and isatty: ESPIPE and ENOTTY on every pipe end.
                            if hostLSeek (hostFd, 0L, 1) <> -1L then
                                failwith $"%s{where}: host lseek succeeded on a pipe"

                            UnixError.ofRawErrnoUnder numbering (Marshal.GetLastPInvokeError ())
                            |> shouldEqual (Some UnixError.ESPIPE)

                            match UnixDescriptor.lseek modelFd 0L 1 system with
                            | Ok (SyscallAnswer.Failed UnixError.ESPIPE, _) -> ()
                            | other -> failwith $"%s{where}: model lseek %A{other}"

                            hostIsATty hostFd |> shouldEqual 0

                            UnixError.ofRawErrnoUnder numbering (Marshal.GetLastPInvokeError ())
                            |> shouldEqual (Some UnixError.ENOTTY)

                            UnixDescriptor.terminalAttributes modelFd system
                            |> shouldEqual (TerminalAttributesAnswer.NotATerminal UnixError.ENOTTY)

                        // Which timestamps the call moved, on the descriptors that
                        // were open before it and still are.
                        for (modelFd, hostFd, pipeEnd, _), (hostBefore, modelBefore) in Array.zip liveSlots before do
                            if not substituted && List.exists (fun (m, _, _, _) -> m = modelFd) slots then
                                let ha, hm, hc = hostBefore
                                let ha', hm', hc' = hostTimes hostFd
                                let ma, mm, mc = modelBefore
                                let ma', mm', mc' = modelTimes modelFd system

                                let hostMoved = ha <> ha', hm <> hm', hc <> hc'
                                let modelMoved = ma <> ma', mm <> mm', mc <> mc'

                                // Linux stamps a pipe from its coarse clock, so a
                                // move within one tick is invisible to the host;
                                // but Linux moves nothing anyway.
                                if hostMoved <> modelMoved then
                                    failwith
                                        $"%s{where}: %O{pipeEnd} end: host moved (atime, mtime, ctime) %A{hostMoved}, model %A{modelMoved}"

                        match UnixSystem.checkInvariants system with
                        | [] -> ()
                        | defects -> failwith $"%s{where}: %A{defects}"
                finally
                    for _, hostFd, _, _ in slots do
                        hostClose hostFd |> ignore

            Check.One (
                Config.QuickThrowOnFailure.WithMaxTest 200,
                Prop.forAll
                    (Arb.fromGen (
                        Gen.zip (Gen.elements [ true ; false ]) (Gen.listOf opGen |> Gen.map (List.truncate 40))
                    ))
                    property
            )
        )

    [<Test>]
    let ``pipe2 answers each single flag bit as the host does`` () : unit =
        HostPlatform.onUnixHostPreset (fun platform ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering platform

            let available =
                try
                    let fds = [| -1 ; -1 |]

                    if hostPipe2 (fds, 0) = 0 then
                        hostClose fds.[0] |> ignore
                        hostClose fds.[1] |> ignore

                    true
                with :? System.EntryPointNotFoundException ->
                    false

            if not available then
                Assert.Ignore "this host's libc has no pipe2 (Darwin's has one from 27)"

            for bit in 0..31 do
                let flags = 1 <<< bit
                let fds = [| -1 ; -1 |]
                let rc = hostPipe2 (fds, flags)
                let errno = Marshal.GetLastPInvokeError ()

                if rc = 0 then
                    hostClose fds.[0] |> ignore
                    hostClose fds.[1] |> ignore

                let host =
                    if rc = 0 then
                        Ok ()
                    else
                        Error (UnixError.ofRawErrnoUnder numbering errno)

                let model : Result<Pipe2Answer * UnixSystem<int, string>, Pipe2Refusal> =
                    UnixPipe.pipe2
                        flags
                        UserBuffer.Mapped
                        (UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                         |> UnixBootImage.boot)

                match model, host with
                | Ok (Pipe2Answer.Created _, _), Ok () -> ()
                | Ok (Pipe2Answer.Failed error, _), Error (Some hostError) when error = hostError -> ()
                // Refused where the real kernel does something this one does
                // not model; the refusal is honest only if the host did not
                // simply reject the bit.
                | Error (Pipe2Refusal.PacketMode _), Ok () -> ()
                | Error (Pipe2Refusal.NotificationPipe _), _ when host <> Error (Some UnixError.EINVAL) -> ()
                | model, host ->
                    failwith $"%O{platform}, flag 0x%x{flags}: model %A{model}, host %A{host} (errno %d{errno})"
        )
