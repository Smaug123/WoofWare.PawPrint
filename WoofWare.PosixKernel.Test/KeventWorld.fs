namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open System.Text.RegularExpressions
open WoofWare.PosixKernel

/// A Darwin-flavoured system for driving `kevent` registrations, built through the
/// syscalls themselves, and the probe output those registrations are held to:
/// `docs/plans/2026-08-23-posix-kernel-extraction/kevent-register.c`'s, measured on
/// Darwin 27.0.0 arm64 and embedded.
[<RequireQualifiedAccess>]
module KeventWorld =

    /// A Darwin system with tasks 1 to 4.
    let darwin : UnixSystem<int, string> =
        UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> fun system -> ([ 1..4 ], system) ||> List.foldBack Tasks.ensure

    let private inet : int option = Some SimulatedUnixPlatform.internetAddressFamily

    let loopback (port : uint16) : InternetEndpoint =
        InternetEndpoint.ofParts InternetEndpoint.LoopbackAddress port

    let withRegistry (registry : FileDescriptorRegistry) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        { system with
            Process =
                { system.Process with
                    FileDescriptors = registry
                }
        }

    let idOf (fd : int) (system : UnixSystem<int, string>) : OpenFileDescriptionId =
        match FileDescriptorRegistry.tryFindId fd system.Process.FileDescriptors with
        | Some id -> id
        | None -> failwith $"fd %d{fd} names no description"

    let kqueue (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixKqueue.kqueue system with
        | Ok created -> created
        | Error refusal -> failwith $"kqueue: %s{KqueueRefusal.describe refusal}"

    /// A new stream socket, non-blocking when `nonBlocking`.
    let stream (nonBlocking : bool) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system =
            NewSocket.create SocketDomain.Inet SocketKind.Stream SocketProtocol.Tcp system

        if nonBlocking then
            fd, UnixSocket.setNonBlocking fd true system |> snd
        else
            fd, system

    let bind (fd : int) (port : uint16) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixSocket.bind fd UserBuffer.Mapped 16u inet (Some (loopback port)) system with
        | Ok (BindAnswer.Bound _, system) -> system
        | other -> failwith $"binding fd %d{fd} at port %d{port}: %A{other}"

    let listen (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixSocket.listen fd 8 system with
        | Ok (ListenAnswer.Listening _, system) -> system
        | other -> failwith $"listening on fd %d{fd}: %A{other}"

    /// A new listener at `port`.
    let listenerAt (port : uint16) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = stream false system
        fd, system |> bind fd port |> listen fd

    let connect
        (fd : int)
        (port : uint16)
        (system : UnixSystem<int, string>)
        : ConnectOutcome * UnixSystem<int, string>
        =
        match UnixConnection.connect fd UserBuffer.Mapped 16u inet (Some (loopback port)) system with
        | Ok answered -> answered
        | Error refusal -> failwith $"connecting fd %d{fd} to port %d{port}: %s{ConnectRefusal.describe refusal}"

    /// A new non-blocking socket connecting to `port`, as the probe's `client` makes.
    let client (port : uint16) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        let fd, system = stream true system
        let _, system = connect fd port system
        fd, system

    /// Accept one connection through `fd`, which must hold one.
    let accept (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixConnection.accept 1 fd UserBuffer.Mapped 16u system with
        | Ok (AcceptOutcome.Accepted (accepted, _, _), system) -> accepted, system
        | other -> failwith $"accepting on fd %d{fd}: %A{other}"

    let close (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed 0L, closed) -> closed
        | other -> failwith $"closing fd %d{fd}: %A{other}"

    let dup (fd : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixDescriptor.dup fd system with
        | SyscallAnswer.Completed copy, system -> int copy, system
        | other, _ -> failwith $"dup of fd %d{fd}: %A{other}"

    /// `getsockopt(SO_ERROR)` on `fd`.
    let readSocketError (fd : int) (system : UnixSystem<int, string>) : GetSockOptAnswer * UnixSystem<int, string> =
        let platform = system.Machine.UnixPlatform
        let level = SimulatedUnixPlatform.socketOptionLevel platform
        let optionName = SimulatedUnixPlatform.socketErrorOption platform

        let read =
            match UnixSocket.admitGetSockOpt fd level optionName UserBuffer.Mapped UserBuffer.Mapped system with
            | Ok GetSockOptAdmission.ReadLength -> Some 4u
            | Ok GetSockOptAdmission.SkipLength
            | Ok (GetSockOptAdmission.Answered _)
            | Error _ -> None

        match UnixSocket.getsockopt fd level optionName UserBuffer.Mapped UserBuffer.Mapped read system with
        | Ok answer -> answer
        | Error refusal -> failwith $"getsockopt refused: %s{SocketOptionRefusal.describe refusal}"

    let change (fd : int) (filter : int16) (flags : uint16) (userData : uint64) : Kevent =
        {
            Ident = uint64 (int64 fd)
            Filter = filter
            Flags = flags
            FilterFlags = 0u
            Data = 0L
            UserData = userData
        }

    /// `kevent(kq, changes, n, eventlist, nevents, {0,0})`, by task 4, which no test
    /// parks.
    let apply
        (kq : int)
        (changes : Kevent list)
        (nevents : int)
        (system : UnixSystem<int, string>)
        : KeventOutcome * UnixSystem<int, string>
        =
        match
            UnixKqueue.kevent
                4
                kq
                (List.length changes)
                changes
                nevents
                UserBuffer.Mapped
                (KeventTimeout.Readable (0L, 0L))
                system
        with
        | Ok answered -> answered
        | Error refusal -> failwith $"kevent refused: %s{KeventRefusal.describe refusal}"

    /// One change and no room, which must succeed: the probe's `reg`.
    let register
        (kq : int)
        (fd : int)
        (filter : int16)
        (flags : uint16)
        (userData : uint64)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match apply kq [ change fd filter flags userData ] 0 system with
        | KeventOutcome.Answered [], system -> system
        | other, _ -> failwith $"registering fd %d{fd}: %A{other}"

    /// The registrations and the queue of the kqueue `kq` names.
    let stateOf (kq : int) (system : UnixSystem<int, string>) : KqueueState =
        match FileDescriptorRegistry.tryFindTarget kq system.Process.FileDescriptors with
        | Some (OpenFileTarget.Kqueue state) -> state
        | other -> failwith $"fd %d{kq} names %A{other}, not a kqueue"

    // ------------------------------------------------------------------
    // The probe's output
    // ------------------------------------------------------------------

    /// One entry of an eventlist as the probe printed it: the name it gave the ident,
    /// the filter's name, flags, fflags, data (`None` for a send buffer's free space,
    /// which this kernel does not model) and udata.
    type Entry =
        {
            Name : string
            Filter : string
            Flags : uint16
            FilterFlags : uint32
            Data : int64 option
            UserData : uint64
        }

    /// What one `kevent` the probe printed returned: its result, its errno's name
    /// ("-" for none) and the entries it wrote.
    type Observed =
        {
            Result : int
            Errno : string
            Entries : Entry list
        }

    let private probeText : Lazy<string list> =
        lazy
            (let assembly = Assembly.GetExecutingAssembly ()
             let resource = "WoofWare.PosixKernel.Test.keventRegister.darwin.txt"

             use stream =
                 match assembly.GetManifestResourceStream resource with
                 | null -> failwith $"embedded resource %s{resource} not found"
                 | stream -> stream

             use reader = new StreamReader (stream)

             reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
             |> Array.toList)

    let private entryPattern : Regex =
        Regex
            @"^(?<name>.+):(?<filter>READ|WRITE|OTHER) flags=0x(?<flags>[0-9a-f]+) fflags=(?<fflags>\d+) data=(?<data>-?\d+) udata=0x(?<udata>[0-9a-f]+)$"

    let private parseEntries (text : string) : Entry list =
        if
            not (
                text.StartsWith ("[", StringComparison.Ordinal)
                && text.EndsWith ("]", StringComparison.Ordinal)
            )
        then
            failwith $"not an entry list: %s{text}"

        match text.Substring (1, text.Length - 2) with
        | "" -> []
        | inner ->
            inner.Split "; "
            |> Array.toList
            |> List.map (fun entry ->
                let m = entryPattern.Match entry

                if not m.Success then
                    failwith $"unparseable entry: %s{entry}"

                {
                    Name = m.Groups.["name"].Value
                    Filter = m.Groups.["filter"].Value
                    Flags = Convert.ToUInt16 (m.Groups.["flags"].Value, 16)
                    FilterFlags = UInt32.Parse m.Groups.["fflags"].Value
                    Data = Some (Int64.Parse m.Groups.["data"].Value)
                    UserData = Convert.ToUInt64 (m.Groups.["udata"].Value, 16)
                }
            )

    /// The columns after `prefix` of every line of the probe's output whose leading
    /// tab-separated columns are exactly `prefix` (the section, the mode where there is
    /// one, and the label), in the order the probe printed them.
    let private linesAfter (prefix : string list) : string list list =
        probeText.Force ()
        |> List.map (fun line -> line.Split '\t' |> Array.toList)
        |> List.filter (fun columns -> List.truncate (List.length prefix) columns = prefix)
        |> List.map (List.skip (List.length prefix))

    /// The columns after `prefix` of the one line of the probe's output that carries
    /// it.
    let probeColumns (prefix : string list) : string list =
        match linesAfter prefix with
        | [ columns ] -> columns
        | lines -> failwith $"the probe printed %d{List.length lines} lines for %A{prefix}, expected one"

    /// Every `kevent` the probe printed under `prefix`, parsed, in order.
    let observedAll (prefix : string list) : Observed list =
        linesAfter prefix
        |> List.map (fun columns ->
            match columns with
            | [ rv ; errno ; entries ] when rv.StartsWith ("rv=", StringComparison.Ordinal) ->
                {
                    Result = Int32.Parse (rv.Substring 3)
                    Errno = errno
                    Entries = parseEntries entries
                }
            | other -> failwith $"the probe's line for %A{prefix} has columns %A{other}"
        )

    let private filterName (filter : int16) : string =
        match filter with
        | -1s -> "READ"
        | -2s -> "WRITE"
        | _ -> "OTHER"

    let private errnoName (error : UnixError) : string =
        match error with
        | UnixError.EBADF -> "EBADF"
        | UnixError.EINVAL -> "EINVAL"
        | UnixError.EFAULT -> "EFAULT"
        | UnixError.ENOENT -> "ENOENT"
        | other -> $"%O{other}"

    /// What `outcome` would have printed, naming each ident through `names`.
    let render (names : Map<int, string>) (outcome : KeventOutcome) : Observed =
        let name (ident : uint64) =
            match Map.tryFind (int ident) names with
            | Some name when ident <= uint64 Int32.MaxValue -> name
            | _ -> $"ident%d{ident}"

        match outcome with
        | KeventOutcome.Failed error ->
            {
                Result = -1
                Errno = errnoName error
                Entries = []
            }
        | KeventOutcome.Answered events ->
            {
                Result = List.length events
                Errno = "-"
                Entries =
                    events
                    |> List.map (fun event ->
                        {
                            Name = name event.Ident
                            Filter = filterName event.Filter
                            Flags = event.Flags
                            FilterFlags = event.FilterFlags
                            Data =
                                match event.Data with
                                | KqueueEventData.Exactly data -> Some data
                                | KqueueEventData.SendBufferSpace -> None
                            UserData = event.UserData
                        }
                    )
            }
        | KeventOutcome.Echoed changes ->
            {
                Result = List.length changes
                Errno = "-"
                Entries =
                    changes
                    |> List.map (fun change ->
                        {
                            Name = name change.Ident
                            Filter = filterName change.Filter
                            Flags = change.Flags
                            FilterFlags = change.FilterFlags
                            Data = Some change.Data
                            UserData = change.UserData
                        }
                    )
            }
        | KeventOutcome.WouldBlock _ -> failwith "a probe row never waited"

    /// Whether `modelled` agrees with what the probe printed: everything exactly, but a
    /// send buffer's free space, which the probe printed as a positive number.
    let agrees (measured : Observed) (modelled : Observed) : bool =
        measured.Result = modelled.Result
        && measured.Errno = modelled.Errno
        && List.length measured.Entries = List.length modelled.Entries
        && List.forall2
            (fun (m : Entry) (o : Entry) ->
                m.Name = o.Name
                && m.Filter = o.Filter
                && m.Flags = o.Flags
                && m.FilterFlags = o.FilterFlags
                && m.UserData = o.UserData
                && (
                    match o.Data with
                    | Some data -> m.Data = Some data
                    | None -> m.Data |> Option.exists (fun data -> data > 0L)
                )
            )
            measured.Entries
            modelled.Entries
