namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open WoofWare.PosixKernel

/// One record `read(2)` returned from an inotify instance, as the probes in
/// docs/plans/2026-10-11-inotify print it.
type internal ReadRecord =
    {
        Wd : int
        Mask : uint32
        Cookie : uint32
        Len : int
        Name : string
        /// Every byte of the name field after the name is zero.
        ZeroPadded : bool
    }

/// The syscalls the inotify probes make, on a model booted like the machine
/// they measured, and their output rendered as the probes print it: for the
/// tests that replay a probe's rows against the model, which open it.
module internal InotifyReplay =

    let private context : string = "InotifyReplay"

    /// The rows of an embedded probe output, each `section`, `case` and
    /// what it reported.
    let recorded (logicalName : string) : (string * string * string) list =
        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream logicalName with
            | null -> failwith $"no embedded resource %s{logicalName}"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
        |> List.choose (fun line ->
            match line.Split '\t' with
            | [| section ; case ; result |] -> Some (section, case, result)
            | _ -> None
        )

    /// The rows of `section`, but those whose case is in `skipped`, each a
    /// case and what it reported. A skip must name a row that exists.
    let rows
        (file : Lazy<(string * string * string) list>)
        (section : string)
        (skipped : string list)
        : (string * string) list
        =
        let all =
            file.Value
            |> List.filter (fun (s, _, _) -> s = section)
            |> List.map (fun (_, case, result) -> case, result)

        for case in skipped do
            if not (List.exists (fun (c, _) -> c = case) all) then
                failwith $"section %s{section} has no row %s{case} to skip"

        all |> List.filter (fun (case, _) -> not (List.contains case skipped))

    let name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let bits (mode : int) : PermissionBits = PermissionBits.parseOrFail context mode
    let group (gid : uint32) : GroupId = GroupId.parseOrFail context gid
    let user (uid : uint32) : UserId = UserId.parseOrFail context uid

    let owner (uid : uint32) : InodeOwner =
        {
            User = user uid
            Group = group uid
        }

    let directory (mode : int) (entries : (string * SeedEntry) list) : SeedEntry =
        SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, bits mode, None)

    let directoryOwnedBy (uid : uint32) (mode : int) (entries : (string * SeedEntry) list) : SeedEntry =
        SeedEntry.Directory (entries |> List.map (fun (n, e) -> name n, e) |> Map.ofList, bits mode, Some (owner uid))

    let file (contents : string) : SeedEntry =
        SeedEntry.File (ImmutableArray.CreateRange (Encoding.ASCII.GetBytes contents), bits 0o644, None)

    let fileOwnedBy (uid : uint32) (contents : string) : SeedEntry =
        SeedEntry.File (ImmutableArray.CreateRange (Encoding.ASCII.GetBytes contents), bits 0o644, Some (owner uid))

    let symlink (target : string) : SeedEntry =
        SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None)

    let root : Credentials = Credentials.ofIds UserId.root (group 0u) []

    /// uid and gid 1000, in no other group.
    let unprivileged : Credentials = Credentials.ofIds (user 1000u) (group 1000u) []

    /// A machine like the one measured: Linux 6.18.5 aarch64, the measuring
    /// VM's `fs.inotify` sysctls, `seed` at the root, and a process of
    /// `credentials` with tasks 0 to 2 standing at the root.
    let boot (credentials : Credentials) (seed : (string * SeedEntry) list) : UnixSystem<int, string> =
        UnixSystem.initial<int, string> SimulatedUnixPlatform.linuxArm64
        |> UnixBootImage.tryWithInotifyLimits
            {
                MaxUserInstances = 128
                MaxUserWatches = 8982
                MaxQueuedEvents = 16384
            }
        |> Configured.expectOk InotifyLimitsRefusal.describe
        |> Launched.fileSystem
            (UnixTimestamp.ofMillisecondsSinceEpoch 0L)
            (owner 0u)
            (seed |> List.map (fun (n, e) -> name n, e) |> Map.ofList)
        |> Launched.bootWith (Launched.credentials credentials) UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> fun system -> ([ 1 ; 2 ], system) ||> List.foldBack Tasks.ensure

    /// `IN_NONBLOCK`.
    let nonblock : int = 0x800

    let modify : uint32 = 0x2u
    let attrib : uint32 = 0x4u
    let movedFrom : uint32 = 0x40u
    let movedTo : uint32 = 0x80u
    let create : uint32 = 0x100u
    let delete : uint32 = 0x200u

    /// `IN_ALL_EVENTS` without the events this kernel does not raise: those
    /// opening, reading and releasing a file raise.
    let modelledEvents : uint32 = 0xFC6u

    /// The mask .NET's `FileSystemWatcher` asks for.
    let dotnetMask : uint32 =
        create
        ||| delete
        ||| modify
        ||| attrib
        ||| movedFrom
        ||| movedTo
        ||| 0x01000000u
        ||| 0x04000000u

    /// The probe's `rc`.
    let rc (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed value -> $"%d{value}"
        | SyscallAnswer.Failed error -> $"-1 %O{error}"

    let init1 (flags : int) (system : UnixSystem<int, string>) : int * UnixSystem<int, string> =
        match UnixInotify.tryInotifyInit1 flags system with
        | Ok (Ok created) -> created
        | other -> failwith $"inotify_init1(0x%x{flags}): %A{other}"

    let addWatch
        (fd : int)
        (path : string)
        (mask : uint32)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match UnixInotify.tryInotifyAddWatchEager fd (PathArg.ofText path) mask system with
        | Ok answered -> answered
        | Error refusal ->
            failwith
                $"inotify_add_watch(%d{fd}, %s{path}, 0x%x{mask}) refused: %s{InotifyAddWatchRefusal.describe refusal}"

    /// `addWatch`, which must succeed: the watch descriptor.
    let watch
        (fd : int)
        (path : string)
        (mask : uint32)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match addWatch fd path mask system with
        | SyscallAnswer.Completed wd, system -> int wd, system
        | SyscallAnswer.Failed error, _ -> failwith $"inotify_add_watch(%d{fd}, %s{path}): %O{error}"

    let rmWatch (fd : int) (wd : int) (system : UnixSystem<int, string>) : SyscallAnswer * UnixSystem<int, string> =
        match UnixInotify.tryInotifyRmWatch fd wd system with
        | Ok answered -> answered
        | Error refusal -> failwith $"inotify_rm_watch(%d{fd}, %d{wd}) refused: %s{InotifyRefusal.describe refusal}"

    let close (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.tryClose fd system with
        | Ok (SyscallAnswer.Completed 0L, system) -> system
        | other -> failwith $"close(%d{fd}): %A{other}"

    let private completed (call : string) (result : Result<SyscallAnswer * UnixSystem<int, string>, 'r>) =
        match result with
        | Ok (SyscallAnswer.Completed _, system) -> system
        | Ok (SyscallAnswer.Failed error, _) -> failwith $"%s{call}: %O{error}"
        | Error refusal -> failwith $"%s{call} refused: %A{refusal}"

    let openFlags (access : FileAccessMode) (creating : bool) (exclusive : bool) (truncate : bool) : OpenFlags =
        {
            Access = access
            Create = creating
            Exclusive = exclusive
            Truncate = truncate
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    let openAnswer
        (flags : OpenFlags)
        (path : string)
        (mode : int)
        (system : UnixSystem<int, string>)
        : SyscallAnswer * UnixSystem<int, string>
        =
        match OpenFlagWords.openPath flags (PathArg.ofText path) mode system with
        | Ok answered -> answered
        | Error refusal -> failwith $"open(%s{path}) refused: %s{OpenRefusal.describe refusal}"

    let openFd
        (flags : OpenFlags)
        (path : string)
        (mode : int)
        (system : UnixSystem<int, string>)
        : int * UnixSystem<int, string>
        =
        match openAnswer flags path mode system with
        | SyscallAnswer.Completed fd, system -> int fd, system
        | SyscallAnswer.Failed error, _ -> failwith $"open(%s{path}): %O{error}"

    let write (fd : int) (text : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let bytes = ImmutableArray.CreateRange (Encoding.ASCII.GetBytes text)

        match
            WriteOutcomes.admitThenWrite 0 fd UserBuffer.Mapped bytes system
            |> WriteOutcomes.returned
        with
        | Ok (WriteAnswer.Completed _, system) -> system
        | other -> failwith $"write(%d{fd}): %A{other}"

    let pwrite
        (fd : int)
        (text : string)
        (offset : int64)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        let bytes = ImmutableArray.CreateRange (Encoding.ASCII.GetBytes text)

        match UnixReadWrite.tryPwriteBytes 0 fd bytes offset system with
        | Ok (WriteAnswer.Completed _, system) -> system
        | other -> failwith $"pwrite(%d{fd}): %A{other}"

    let ftruncate (fd : int) (length : int64) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixDescriptor.tryFtruncate fd length system |> completed $"ftruncate(%d{fd})"

    let fchmod (fd : int) (mode : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixPathResolution.tryFchmod fd mode system |> completed $"fchmod(%d{fd})"

    let fchown
        (fd : int)
        (uid : uint32 option)
        (gid : uint32 option)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        UnixPathResolution.tryFchown fd (Option.map user uid) (Option.map group gid) system
        |> completed $"fchown(%d{fd})"

    /// `futimens(fd, times)`: `utimensat` of a null path.
    let futimens (fd : int) (times : TimesArgument) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixPathResolution.tryUtimensat fd NullablePathArgument.Null times 0 system
        |> completed $"futimens(%d{fd})"

    /// Linux's `AT_EMPTY_PATH`.
    let atEmptyPath : int = 0x1000

    /// `copy_file_range(source, NULL, destination, NULL, count, 0)`.
    let copyFileRange
        (source : int)
        (destination : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        match UnixReadWrite.tryCopyFileRange source destination (uint64 count) 0 system with
        | Ok (SyscallAnswer.Completed _, system) -> system
        | other -> failwith $"copy_file_range(%d{source}, %d{destination}): %A{other}"

    let readFile (fd : int) (count : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixReadWrite.tryRead 0 fd UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed _), system) -> system
        | other -> failwith $"read(%d{fd}): %A{other}"

    /// The refusal of a change an inotify watch could hear of through a name
    /// this kernel does not keep, which `result` answers: `choose` picks it out
    /// of the call's own refusal. Fails the test for an answer, or another
    /// refusal.
    let unplaced
        (call : string)
        (choose : 'r -> UnplacedChangeRefusal option)
        (result : Result<'a, 'r>)
        : UnplacedChangeRefusal
        =
        match result with
        | Ok answer -> failwith $"%s{call} answered %A{answer}, where it should have been refused"
        | Error refusal ->
            match choose refusal with
            | Some unplaced -> unplaced
            | None -> failwith $"%s{call} was refused, but for another reason: %A{refusal}"

    /// That `refusal` is of a change through a removed directory, whose
    /// missing input is the name it had in its old parent.
    let throughRemovedDirectory (refusal : UnplacedChangeRefusal) : unit =
        match refusal with
        | UnplacedChangeRefusal.RemovedDirectoryName _ -> ()

    /// The refusal of `chmod(path, mode)`, which must be of a change an
    /// inotify watch could hear of through a name this kernel does not keep.
    let chmodRefusal (path : string) (mode : int) (system : UnixSystem<int, string>) : UnplacedChangeRefusal =
        UnixPathResolution.tryChmod (PathArg.ofText path) mode system
        |> unplaced
            $"chmod(%s{path})"
            (fun refusal ->
                match refusal with
                | ChModRefusal.UnplacedChange refusal -> Some refusal
                | _ -> None
            )

    /// The probe's `writeFile`: `open(O_WRONLY|O_CREAT|O_TRUNC, 0644)`, a
    /// write of `text`, and a close.
    let writeFile (path : string) (text : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        let fd, system =
            openFd (openFlags FileAccessMode.WriteOnly true false true) path 0o644 system

        system |> write fd text |> close fd

    let chmod (path : string) (mode : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixPathResolution.tryChmod (PathArg.ofText path) mode system
        |> completed $"chmod(%s{path})"

    let chown
        (path : string)
        (uid : uint32 option)
        (gid : uint32 option)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        UnixPathResolution.tryChown (PathArg.ofText path) (Option.map user uid) (Option.map group gid) system
        |> completed $"chown(%s{path})"

    /// Linux's `AT_FDCWD`.
    let private atFdCwd : int = -100

    /// Linux's `UTIME_NOW`.
    let utimeNow : int64 = 0x3FFFFFFFL

    /// Linux's `UTIME_OMIT`.
    let utimeOmit : int64 = 0x3FFFFFFEL

    /// `utimensat(AT_FDCWD, path, times, 0)`.
    let utimensat (path : string) (times : TimesArgument) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixPathResolution.tryUtimensat atFdCwd (NullablePathArgument.NotNull (PathArg.ofText path)) times 0 system
        |> completed $"utimensat(%s{path})"

    /// A `struct timespec` of `seconds`, or of a marker when `nanoseconds` is one.
    let timespec (seconds : int64) (nanoseconds : int64) : TimespecFields =
        {
            Seconds = seconds
            Nanoseconds = nanoseconds
        }

    let mkdir (path : string) (mode : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.tryMkdir (PathArg.ofText path) mode system
        |> completed $"mkdir(%s{path})"

    let rmdir (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.tryRmdir (PathArg.ofText path) system
        |> completed $"rmdir(%s{path})"

    let unlink (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.tryUnlink (PathArg.ofText path) system
        |> completed $"unlink(%s{path})"

    let link (existing : string) (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.tryLinkEager (PathArg.ofText existing) (PathArg.ofText path) system
        |> completed $"link(%s{existing}, %s{path})"

    let symlinkAt (target : string) (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.trySymlinkEager (PathArg.ofText target) (PathArg.ofText path) system
        |> completed $"symlink(%s{target}, %s{path})"

    let rename (source : string) (destination : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixNamespace.tryRenameEager (PathArg.ofText source) (PathArg.ofText destination) system
        |> completed $"rename(%s{source}, %s{destination})"

    let chdir (path : string) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        UnixPathResolution.tryChdir (PathArg.ofText path) system
        |> completed $"chdir(%s{path})"

    /// The number of the inode `path` names, following a final symbolic link.
    let numberOf (path : string) (system : UnixSystem<int, string>) : InodeNumber =
        match
            UnixPathResolution.resolvePath
                AtDirectory.CurrentDirectory
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail context path)
                system
        with
        | Ok inode -> inode
        | Error failure -> failwith $"%s{path}: %A{failure}"

    /// The inode `path` names, following a final symbolic link.
    let inodeOf (path : string) (system : UnixSystem<int, string>) : Inode =
        match VirtualFileSystem.tryGet (numberOf path system) (UnixSystem.fileSystem system) with
        | Some inode -> inode
        | None -> failwith $"%s{path} resolved to an inode the filesystem does not hold"

    /// The permission bits of what `path` names, as `stat` reports them.
    let modeOf (path : string) (system : UnixSystem<int, string>) : int =
        Inode.permissions (inodeOf path system) |> PermissionBits.toInt

    /// The probe's `maskNames`.
    let maskNames (mask : uint32) : string =
        let names =
            [
                0x1u, "ACCESS"
                0x2u, "MODIFY"
                0x4u, "ATTRIB"
                0x8u, "CLOSE_WRITE"
                0x10u, "CLOSE_NOWRITE"
                0x20u, "OPEN"
                0x40u, "MOVED_FROM"
                0x80u, "MOVED_TO"
                0x100u, "CREATE"
                0x200u, "DELETE"
                0x400u, "DELETE_SELF"
                0x800u, "MOVE_SELF"
                0x2000u, "UNMOUNT"
                0x4000u, "Q_OVERFLOW"
                0x8000u, "IGNORED"
                0x40000000u, "ISDIR"
            ]

        let named = names |> List.filter (fun (bit, _) -> mask &&& bit <> 0u)
        let seen = named |> List.fold (fun acc (bit, _) -> acc ||| bit) 0u
        let text = named |> List.map snd |> String.concat "+"

        if mask &&& ~~~seen <> 0u then
            failwith $"mask 0x%x{mask} has bits the probe would print raw"
        elif text = "" then
            "0"
        else
            text

    /// Every record in `bytes`, which must hold whole records only.
    let parse (bytes : ImmutableArray<byte>) : ReadRecord list =
        let bytes = bytes.AsSpan().ToArray ()

        let rec go (pos : int) (acc : ReadRecord list) =
            if pos + 16 > bytes.Length then
                if pos <> bytes.Length then
                    failwith $"%d{bytes.Length - pos} trailing bytes"

                List.rev acc
            else
                let len =
                    int (BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos + 12, 4)))

                let raw = bytes.[pos + 16 .. pos + 16 + len - 1]

                let nul =
                    match Array.IndexOf (raw, 0uy) with
                    | -1 -> raw.Length
                    | nul -> nul

                let record =
                    {
                        Wd = BinaryPrimitives.ReadInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos, 4))
                        Mask = BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos + 4, 4))
                        Cookie = BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, pos + 8, 4))
                        Len = len
                        Name = Encoding.UTF8.GetString (raw, 0, nul)
                        ZeroPadded = raw.[nul..] |> Array.forall ((=) 0uy)
                    }

                go (pos + 16 + len) (record :: acc)

        go 0 []

    /// A record as the probe prints it. A cookie is printed as its value,
    /// for `normaliseCookies` to number.
    let render (record : ReadRecord) : string =
        let cookie = if record.Cookie = 0u then "0" else $"k%d{record.Cookie}"

        $"wd=%d{record.Wd} mask=%s{maskNames record.Mask} cookie=%s{cookie} len=%d{record.Len} name=\"%s{record.Name}\""

    /// `rows` with each cookie renamed by the order it first appears in them,
    /// so that the probe's numbering, which counts every cookie its run saw,
    /// and the model's values compare by which records share a cookie.
    let normaliseCookies (rows : (string * string) list) : (string * string) list =
        let seen = Dictionary<string, int> ()

        rows
        |> List.map (fun (case, text) ->
            let renamed =
                Regex.Replace (
                    text,
                    @"cookie=(k\d+)",
                    fun (m : Match) ->
                        let cookie = m.Groups.[1].Value

                        if not (seen.ContainsKey cookie) then
                            seen.[cookie] <- seen.Count + 1

                        $"cookie=c%d{seen.[cookie]}"
                )

            case, renamed
        )

    /// What a row of records would have been under a watch asking for no
    /// event this kernel does not raise: the records of `IN_ACCESS`,
    /// `IN_OPEN` and the two `IN_CLOSE_*` events dropped.
    let withoutUnmodelled (text : string) : string =
        if text = "(none)" then
            text
        else

        let unmodelled = set [ "ACCESS" ; "OPEN" ; "CLOSE_WRITE" ; "CLOSE_NOWRITE" ]

        let kept =
            text.Split " | "
            |> List.ofArray
            |> List.filter (fun record ->
                let names =
                    (Regex.Match (record, @"mask=(\S+)")).Groups.[1].Value.Split '+' |> Set.ofArray

                let dropped = Set.intersect names unmodelled

                if dropped.IsEmpty then
                    true
                elif dropped = names then
                    false
                else
                    failwith $"record %s{record} mixes unmodelled and modelled events"
            )

        if kept.IsEmpty then "(none)" else String.concat " | " kept

    /// One read of up to `count` bytes, which must return records.
    let readRecords
        (fd : int)
        (count : int)
        (system : UnixSystem<int, string>)
        : ReadRecord list * UnixSystem<int, string>
        =
        match UnixReadWrite.tryRead 0 fd UserBuffer.Mapped (uint64 count) system with
        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) -> parse bytes, system
        | other -> failwith $"read(%d{fd}, %d{count}): %A{other}"

    /// Every record the non-blocking instance `fd` holds, read until `EAGAIN`.
    let drainRecords (fd : int) (system : UnixSystem<int, string>) : ReadRecord list * UnixSystem<int, string> =
        let rec go (acc : ReadRecord list) (system : UnixSystem<int, string>) =
            match UnixReadWrite.tryRead 0 fd UserBuffer.Mapped (uint64 (1 <<< 20)) system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), system) -> go (acc @ parse bytes) system
            | Ok (ReadOutcome.Answered (ReadAnswer.Failed UnixError.EAGAIN), system) -> acc, system
            | other -> failwith $"read(%d{fd}): %A{other}"

        go [] system

    /// The probe's `drain`.
    let drain (fd : int) (system : UnixSystem<int, string>) : string * UnixSystem<int, string> =
        match drainRecords fd system with
        | [], system -> "(none)", system
        | records, system -> records |> List.map render |> String.concat " | ", system
