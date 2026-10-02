namespace WoofWare.PosixKernel

/// What `pipe2(2)` answered, for a request this kernel could answer.
[<RequireQualifiedAccess>]
type Pipe2Answer =
    /// A pipe now exists. The call returned 0, having stored `readFd` and
    /// `writeFd` in the caller's two-`int` array, in that order.
    | Created of readFd : int * writeFd : int
    /// The call returned -1 with this errno. No pipe exists, and no descriptor
    /// was taken.
    | Failed of error : UnixError

/// Why this kernel will not answer a `pipe2(2)`.
[<RequireQualifiedAccess>]
type Pipe2Refusal =
    /// Linux's `O_DIRECT`, which makes the pipe a packet pipe: each write is a
    /// packet that a read takes whole or truncates. This kernel's pipes carry
    /// a byte stream only.
    | PacketMode of flags : int
    /// Linux's `O_NOTIFICATION_PIPE`. Whether a kernel accepts it depends on
    /// how it was built (`CONFIG_WATCH_QUEUE`): measured, the Linux 6.18.5
    /// build probed answers ENOPKG, and one built with watch queues would make a
    /// pipe that carries kernel notifications, which this kernel does not model.
    | NotificationPipe of flags : int
    /// The destination has no answer at the copy.
    | Buffer of BufferRefusal
    /// The destination names no storage, on a platform whose C library stores
    /// the two descriptors itself rather than having the kernel copy them out:
    /// measured on Darwin 27.0.0, `pipe2(NULL, 0)` and `pipe2((int *)8, 0)` kill
    /// the process with SIGSEGV. A dead process is not an errno.
    | FatalToTheProcess

[<RequireQualifiedAccess>]
module Pipe2Refusal =
    /// What this kernel knows about why it cannot answer. The client supplies
    /// its own half -- which entry point asked, and what the destination was.
    let describe (refusal : Pipe2Refusal) : string =
        match refusal with
        | Pipe2Refusal.PacketMode flags ->
            $"flags 0x%x{flags} carry O_DIRECT, which Linux accepts and answers with a packet pipe: a write is one packet of at most PIPE_BUF bytes and a read takes one packet, truncating it to the count. This kernel's pipes carry a byte stream only; model packets before answering."
        | Pipe2Refusal.NotificationPipe flags ->
            $"flags 0x%x{flags} carry O_NOTIFICATION_PIPE. A Linux kernel built without watch queues answers ENOPKG (measured on 6.18.5) and one built with them makes a pipe that carries kernel notifications, so the answer is a fact of the kernel's build, which this kernel does not model."
        | Pipe2Refusal.Buffer refusal -> BufferRefusal.describe refusal
        | Pipe2Refusal.FatalToTheProcess ->
            "the destination names no storage, and this platform's C library stores the two descriptors into it itself, after the kernel has made the pipe: measured on Darwin 27.0.0, pipe2 through NULL or a small bad pointer kills the process with SIGSEGV, where Linux's kernel copies them out and answers EFAULT. A dead process is not an errno, so this kernel will not answer one."

/// `pipe2(2)`: making a pipe.
///
/// A pipe carries bytes from its write end to its read end in the order they
/// were written. Its buffer is the flavour's (see `PipeBuffer`); an end is open
/// while some descriptor names it; and the pipe is freed when neither is. What
/// each syscall answers of a pipe is that syscall's: `UnixReadWrite.read` and
/// `UnixReadWrite.write`, `UnixPoll.poll`, `UnixPathResolution.fstat`,
/// `UnixDescriptor.bytesAvailable`, and `UnixDescriptor.close`, which frees it.
///
/// A pipe's timestamps are its flavour's. On Linux none of them ever moves.
/// On Darwin every read that reaches the pipe moves the read end's
/// `st_atime`, and every write that reaches it moves `st_mtime` and `st_ctime`
/// of both ends, whatever the call answers: measured on 27.0.0, a transfer of
/// no bytes, an `EAGAIN` and an `EFAULT` each move them.
[<RequireQualifiedAccess>]
module UnixPipe =

    /// `O_NOTIFICATION_PIPE`, which is `O_EXCL`.
    [<Literal>]
    let private LinuxNotificationPipe = OpenFlagNumbering.LinuxExclusive

    /// What the flag word asks for, once every bit in it is known to be one the
    /// flavour accepts.
    [<RequireQualifiedAccess>]
    type private Pipe2Flags =
        | Creates of nonBlocking : bool
        | Fails of error : UnixError
        | Refused of Pipe2Refusal

    // Measured by pipe-syscalls.c, every one of the 32 single bits on each
    // flavour (Linux 6.18.5 aarch64, Darwin 27.0.0 arm64), and the combination
    // of O_CLOEXEC with O_NONBLOCK:
    //
    //   Linux   O_NONBLOCK, O_CLOEXEC and O_DIRECT make a pipe;
    //           O_NOTIFICATION_PIPE is ENOPKG; every other bit is EINVAL.
    //   Darwin  O_NONBLOCK, O_CLOEXEC and O_CLOFORK make a pipe; every other
    //           bit is EINVAL.
    //
    // O_NONBLOCK lands on both descriptions. O_CLOEXEC and O_CLOFORK set a
    // per-descriptor flag that matters only across exec and fork, which this
    // kernel models neither of, so they are accepted and change nothing here.
    let private decode (platform : SimulatedUnixPlatform) (flags : int) : Pipe2Flags =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            let direct =
                OpenFlagNumbering.linuxDirect (SimulatedUnixPlatform.architecture platform)

            let accepted =
                OpenFlagNumbering.LinuxNonBlock
                ||| OpenFlagNumbering.LinuxCloseOnExec
                ||| direct
                ||| LinuxNotificationPipe

            if flags &&& ~~~accepted <> 0 then
                Pipe2Flags.Fails UnixError.EINVAL
            elif flags &&& LinuxNotificationPipe <> 0 then
                Pipe2Flags.Refused (Pipe2Refusal.NotificationPipe flags)
            elif flags &&& direct <> 0 then
                Pipe2Flags.Refused (Pipe2Refusal.PacketMode flags)
            else
                Pipe2Flags.Creates (flags &&& OpenFlagNumbering.LinuxNonBlock <> 0)
        | SimulatedUnixFlavour.Darwin ->
            let accepted =
                OpenFlagNumbering.DarwinNonBlock
                ||| OpenFlagNumbering.DarwinCloseOnExec
                ||| OpenFlagNumbering.DarwinCloseOnFork

            if flags &&& ~~~accepted <> 0 then
                Pipe2Flags.Fails UnixError.EINVAL
            else
                Pipe2Flags.Creates (flags &&& OpenFlagNumbering.DarwinNonBlock <> 0)

    /// `pipe2(2)`: make a pipe, with a descriptor onto each end, and store the
    /// two in the caller's array at `destination`.
    ///
    /// `flags` is raw, in the simulated flavour's own `<fcntl.h>` numbering.
    /// `pipe(2)` is `pipe2` with flags 0.
    ///
    /// The flags are screened first, on both flavours: a flag word the flavour
    /// rejects is EINVAL whatever the destination. Then Linux copies the two
    /// descriptors out, answering EFAULT for an unmapped destination and
    /// leaving no pipe and no descriptor behind (measured: the next descriptor
    /// allocated is the one the pipe would have taken). Darwin's C library
    /// stores them itself, so an unmapped destination there is refused (see
    /// `Pipe2Refusal.FatalToTheProcess`).
    ///
    /// The read end takes the lowest descriptor not in use and the write end
    /// the next (see `FileDescriptorRegistry.createPipe`). Both are owned by
    /// the process's effective user and group, and stamped with the machine's
    /// realtime clock.
    let pipe2<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : int)
        (destination : UserBuffer)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<Pipe2Answer * UnixSystem<'Task, 'Handler>, Pipe2Refusal>
        =
        let platform = system.Machine.UnixPlatform

        match decode platform flags with
        | Pipe2Flags.Fails error -> Ok (Pipe2Answer.Failed error, system)
        | Pipe2Flags.Refused refusal -> Error refusal
        | Pipe2Flags.Creates nonBlocking ->

        match destination with
        | UserBuffer.Opaque -> Error (Pipe2Refusal.Buffer BufferRefusal.OpaqueAtTransfer)
        | UserBuffer.Addressless -> Error (Pipe2Refusal.Buffer BufferRefusal.AddresslessAtTransfer)
        | UserBuffer.Unmapped _ ->
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> Ok (Pipe2Answer.Failed UnixError.EFAULT, system)
            | SimulatedUnixFlavour.Darwin -> Error Pipe2Refusal.FatalToTheProcess
        | UserBuffer.Mapped ->

        let machine = system.Machine
        let pipeId = machine.NextPipeId
        let (PipeId rawPipe) = pipeId
        let (InodeNumber rawInode) = machine.NextPipeInode

        let inodes, nextInode =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> PipeInodes.Shared (InodeNumber rawInode), InodeNumber (rawInode + 1L)
            | SimulatedUnixFlavour.Darwin ->
                PipeInodes.PerEnd (InodeNumber rawInode, InodeNumber (rawInode + 1L)), InodeNumber (rawInode + 2L)

        let now = UnixMachineState.realtime machine

        // Measured by pipe-syscalls.c and pipe-states.c, and held to the host
        // by `TestPipeAgainstHost`. Neither flavour applies the umask: the bits
        // are the same under umask 0 and 0777.
        let permissions =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> PermissionBits.parseOrFail "UnixPipe.pipe2" 0o600
            | SimulatedUnixFlavour.Darwin -> PermissionBits.parseOrFail "UnixPipe.pipe2" 0o660

        let pipe =
            {
                Buffer = PipeBuffer.empty platform
                Reads = 0L
                Origin =
                    PipeOrigin.Made
                        {
                            Inodes = inodes
                            Owner = InodeOwner.ofProcess system.Process.Credentials
                            Permissions = permissions
                            Times =
                                {
                                    Created = now
                                    ReadEndAccess = now
                                    Modification = now
                                    StatusChange = now
                                }
                        }
            }

        let (readFd, writeFd), registry =
            FileDescriptorRegistry.createPipe pipeId nonBlocking system.Process.FileDescriptors

        Ok (
            Pipe2Answer.Created (readFd, writeFd),
            { system with
                Machine =
                    { machine with
                        Pipes = Map.add pipeId pipe machine.Pipes
                        NextPipeId = PipeId (rawPipe + 1L)
                        NextPipeInode = nextInode
                    }
                Process =
                    { system.Process with
                        FileDescriptors = registry
                    }
            }
        )
