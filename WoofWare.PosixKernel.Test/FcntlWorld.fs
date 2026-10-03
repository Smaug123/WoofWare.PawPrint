namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open WoofWare.PosixKernel

/// The descriptors `fcntl-dup.c` measured, built through the syscalls on a
/// system shaped like the probe's, and the probe's output.
///
/// `docs/plans/2026-08-23-posix-kernel-extraction/fcntl-dup.c`, measured on
/// Darwin 27.0.0 arm64 (uid 501) and Linux 6.18.5 aarch64 (root and uid 1000),
/// and embedded.
[<RequireQualifiedAccess>]
module internal FcntlWorld =

    let private context : string = "FcntlWorld"

    /// One run of the probe: the platform that answers for it, who made it,
    /// and what it printed.
    type Run =
        {
            Name : string
            Platform : SimulatedUnixPlatform
            Caller : Credentials
            Lines : string list
        }

    let private lines (resource : string) : string list =
        let name = $"WoofWare.PosixKernel.Test.%s{resource}"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream name with
            | null -> failwith $"no embedded resource %s{name}"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    /// The three runs, each with its own platform and caller.
    let runs : Run list =
        [
            {
                Name = "darwin"
                Platform = SimulatedUnixPlatform.macOsArm64
                Caller = Credentials.ofIds (UserId.parseOrFail context 501u) (GroupId.parseOrFail context 20u) []
                Lines = lines "fcntlDup.darwin.txt"
            }
            {
                Name = "linux root"
                Platform = SimulatedUnixPlatform.linuxArm64
                Caller = Owners.root
                Lines = lines "fcntlDup.linuxRoot.txt"
            }
            {
                Name = "linux uid 1000"
                Platform = SimulatedUnixPlatform.linuxArm64
                Caller = Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
                Lines = lines "fcntlDup.linuxUser.txt"
            }
        ]

    /// The tab-separated fields of every line of `run` in `section`, without
    /// the section's name.
    let rows (section : string) (run : Run) : string list list =
        run.Lines
        |> List.filter (fun line -> line.StartsWith (section + "\t"))
        |> List.map (fun line -> line.Split '\t' |> List.ofArray |> List.tail)

    /// `F_DUPFD`, `F_GETFD`, `F_SETFD`, `F_GETFL` and `F_SETFL`, which both
    /// flavours number alike.
    [<Literal>]
    let DupFd = 0

    [<Literal>]
    let GetFd = 1

    [<Literal>]
    let SetFd = 2

    [<Literal>]
    let GetFl = 3

    [<Literal>]
    let SetFl = 4

    /// `F_DUPFD_CLOEXEC` in `platform`'s numbering.
    let dupFdCloexec (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 1030
        | SimulatedUnixFlavour.Darwin -> 67

    /// `O_NONBLOCK` in `platform`'s numbering.
    let nonBlock (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> OpenFlagNumbering.LinuxNonBlock
        | SimulatedUnixFlavour.Darwin -> OpenFlagNumbering.DarwinNonBlock

    /// `O_CLOEXEC` in `platform`'s numbering.
    let closeOnExec (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> OpenFlagNumbering.LinuxCloseOnExec
        | SimulatedUnixFlavour.Darwin -> OpenFlagNumbering.DarwinCloseOnExec

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    /// A system on `run`'s platform, made by `run`'s caller, whose working
    /// directory holds the probe's `f` (eight bytes, the caller's, 0644), `d`
    /// (the caller's, 0755) and `foreign` (root's, 0644), with tasks 1 to 3
    /// beside the leader 0.
    let system (run : Run) : UnixSystem<int, string> =
        let caller = InodeOwner.ofProcess run.Caller

        let root : InodeOwner =
            {
                User = UserId.root
                Group = GroupId.parseOrFail context 0u
            }

        let bits (mode : int) = PermissionBits.parseOrFail context mode

        let seed =
            Map.ofList
                [
                    name "w",
                    SeedEntry.Directory (
                        Map.ofList
                            [
                                name "f",
                                SeedEntry.File (ImmutableArray.CreateRange "abcdefgh"B, bits 0o644, Some caller)
                                name "d", SeedEntry.Directory (Map.empty, bits 0o755, Some caller)
                                name "foreign", SeedEntry.File (ImmutableArray.CreateRange "x"B, bits 0o644, Some root)
                            ],
                        bits 0o755,
                        Some caller
                    )
                ]

        let image : UnixBootImage<int, string> =
            UnixSystem.initial run.Platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context run.Caller

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
                root
                seed
                (AbsoluteUnixPath.parseOrFail context "/w")
                image
        with
        | Ok image ->
            UnixBootImage.boot image
            |> fun system -> ([ 1..3 ], system) ||> List.foldBack Tasks.ensure
        | Error fault -> failwith $"could not build the system: %A{fault}"

    /// `fcntl`, which must not be refused.
    let fcntl
        (fd : int)
        (command : int)
        (argument : int)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixDescriptor.fcntl fd command argument system with
        | Ok answered -> answered
        | Error refusal ->
            failwith $"fcntl(%d{fd}, %d{command}, 0x%x{argument}) was refused: %s{FcntlRefusal.describe refusal}"

    /// An answer as the probe prints a flag word: "ok 0x..." or the errno.
    let word (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed value -> $"ok 0x%x{value}"
        | SyscallAnswer.Failed error -> $"%A{error}"

    /// An answer as the probe prints a number: "ok <n>" or the errno.
    let number (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed value -> $"ok %d{value}"
        | SyscallAnswer.Failed error -> $"%A{error}"

    /// `F_GETFL` of `fd`, as the probe prints it.
    let statusFlags (fd : int) (system : UnixSystem<int, string>) : string = word (fst (fcntl fd GetFl 0 system))

    /// `F_GETFD` of `fd`, as the probe prints it.
    let descriptorFlags (fd : int) (system : UnixSystem<int, string>) : string = word (fst (fcntl fd GetFd 0 system))

    /// `open(2)` of `path` with the raw word `flags`, which must succeed.
    let openRaw (flags : int) (path : string) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixNamespace.openPath flags (PathArg.ofText path) 0o644 system with
        | Ok (SyscallAnswer.Completed fd, system) -> int fd, system
        | other -> failwith $"open(%s{path}, 0x%x{flags}): %A{other}"

    /// `open(2)` of `path` with `flags`, which must succeed.
    let openWith
        (flags : OpenFlags)
        (path : string)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        openRaw (OpenFlagWords.encode system.Machine.UnixPlatform flags) path system

    /// The request for `access` and nothing else.
    let opening (access : FileAccessMode) : OpenFlags =
        {
            Access = access
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    /// A pipe made with the raw `pipe2` word `flags`: its read and write ends.
    let pipe (flags : int) (system : UnixSystem<int, string>) : (int * int) * UnixSystem<int, string> =
        match UnixPipe.pipe2 flags UserBuffer.Mapped system with
        | Ok (Pipe2Answer.Created (r, w), system) -> (r, w), system
        | other -> failwith $"pipe2(0x%x{flags}): %A{other}"

    let private inet : int option = Some SimulatedUnixPlatform.internetAddressFamily

    let private loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    /// A listening IPv4 stream socket at `port`, non-blocking when
    /// `nonBlocking`.
    let listener
        (port : uint16)
        (nonBlocking : bool)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match UnixSocket.bind fd UserBuffer.Mapped 16u inet (Some (loopback port)) system with
            | Ok (BindAnswer.Bound _, system) -> system
            | other -> failwith $"bind: %A{other}"

        let system =
            match UnixSocket.listen fd 16 system with
            | Ok (ListenAnswer.Listening _, system) -> system
            | other -> failwith $"listen: %A{other}"

        if nonBlocking then
            fd, snd (UnixDescriptor.setNonBlocking fd true system)
        else
            fd, system

    /// A socket accepted from a listener at `port`, after a client connected
    /// to it: the accepted descriptor.
    let accepted
        (port : uint16)
        (nonBlocking : bool)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        let listenerFd, system = listener port nonBlocking system

        let client, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        let system =
            match UnixConnection.connect client UserBuffer.Mapped 16u inet (Some (loopback port)) system with
            | Ok (_, system) -> system
            | Error refusal -> failwith $"connect: %s{ConnectRefusal.describe refusal}"

        match UnixConnection.accept 0 listenerFd UserBuffer.Mapped 16u system with
        | Ok (AcceptOutcome.Accepted (fd, _, _), system) -> fd, system
        | other -> failwith $"accept: %A{other}"

    /// A fresh descriptor of the probe's kind `kind`, or `None` for a kind
    /// this kernel does not make on `system`'s flavour (`accept4`, which it
    /// does not model, and every kind the flavour lacks).
    let make (kind : string) (system : UnixSystem<int, string>) : (int * UnixSystem<int, string>) option =
        let platform = system.Machine.UnixPlatform
        let linux = SimulatedUnixPlatform.flavour platform = SimulatedUnixFlavour.Linux

        let socketWith (extra : int) =
            let domain, kind, protocol =
                NewSocket.arguments platform SocketDomain.Inet SocketKind.Stream SocketProtocol.Default

            match UnixSocket.socket domain (kind ||| extra) protocol system with
            | Ok (Ok created) -> Some created
            | other -> failwith $"socket with 0x%x{extra}: %A{other}"

        let socket domain kind =
            Some (NewSocket.create domain kind SocketProtocol.Default system)

        match kind with
        | "file-rdonly" -> Some (openWith (opening FileAccessMode.ReadOnly) "f" system)
        | "file-wronly" -> Some (openWith (opening FileAccessMode.WriteOnly) "f" system)
        | "file-rdwr" -> Some (openWith (opening FileAccessMode.ReadWrite) "f" system)
        | "file-rdonly-cloexec" ->
            Some (
                openWith
                    { opening FileAccessMode.ReadOnly with
                        CloseOnExec = true
                    }
                    "f"
                    system
            )
        | "file-rdwr-cloexec" ->
            Some (
                openWith
                    { opening FileAccessMode.ReadWrite with
                        CloseOnExec = true
                    }
                    "f"
                    system
            )
        | "file-rdwr-sync" ->
            Some (
                openWith
                    { opening FileAccessMode.ReadWrite with
                        Synchronous = true
                        DataSynchronous = linux
                    }
                    "f"
                    system
            )
        | "dir" -> Some (openWith (opening FileAccessMode.ReadOnly) "d" system)
        | "dir-o_directory" ->
            Some (
                openWith
                    { opening FileAccessMode.ReadOnly with
                        Directory = true
                    }
                    "d"
                    system
            )
        | "foreign-rdonly" -> Some (openWith (opening FileAccessMode.ReadOnly) "foreign" system)
        | "pipe-r" -> pipe 0 system |> fun ((r, _), system) -> Some (r, system)
        | "pipe-w" -> pipe 0 system |> fun ((_, w), system) -> Some (w, system)
        | "pipe2-nonblock-r" -> pipe (nonBlock platform) system |> fun ((r, _), system) -> Some (r, system)
        | "pipe2-nonblock-w" -> pipe (nonBlock platform) system |> fun ((_, w), system) -> Some (w, system)
        | "pipe2-cloexec-r" -> pipe (closeOnExec platform) system |> fun ((r, _), system) -> Some (r, system)
        | "pipe2-cloexec-w" -> pipe (closeOnExec platform) system |> fun ((_, w), system) -> Some (w, system)
        | "inet-stream" -> socket SocketDomain.Inet SocketKind.Stream
        | "inet-dgram" -> socket SocketDomain.Inet SocketKind.Datagram
        | "inet6-stream" -> socket SocketDomain.Inet6 SocketKind.Stream
        | "unix-stream" -> socket SocketDomain.Unix SocketKind.Stream
        | "unix-dgram" -> socket SocketDomain.Unix SocketKind.Datagram
        | "inet-stream-sock_nonblock" when linux -> socketWith OpenFlagNumbering.LinuxNonBlock
        | "inet-stream-sock_cloexec" when linux -> socketWith OpenFlagNumbering.LinuxCloseOnExec
        | "accepted" -> Some (accepted 5000us false system)
        | "accepted-from-nonblocking" -> Some (accepted 5000us true system)
        | "port" when linux ->
            match UnixPoll.epollCreate1 0 system with
            | Ok (Ok created) -> Some created
            | other -> failwith $"epoll_create1(0): %A{other}"
        | "port-cloexec" when linux ->
            match UnixPoll.epollCreate1 EpollCreateFlags.CloseOnExec system with
            | Ok (Ok created) -> Some created
            | other -> failwith $"epoll_create1(EPOLL_CLOEXEC): %A{other}"
        | "port" ->
            match UnixKqueue.kqueue system with
            | Ok created -> Some created
            | Error refusal -> failwith $"kqueue: %s{KqueueRefusal.describe refusal}"
        | "null-rdonly" when linux -> Some (openWith (opening FileAccessMode.ReadOnly) "/dev/null" system)
        | "null-wronly" when linux -> Some (openWith (opening FileAccessMode.WriteOnly) "/dev/null" system)
        | "null-rdwr" when linux -> Some (openWith (opening FileAccessMode.ReadWrite) "/dev/null" system)
        | "urandom-rdonly" when linux -> Some (openWith (opening FileAccessMode.ReadOnly) "/dev/urandom" system)
        | "urandom-rdwr" when linux -> Some (openWith (opening FileAccessMode.ReadWrite) "/dev/urandom" system)
        | _ -> None
