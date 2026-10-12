namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open System.Text
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PosixKernel

/// Which records a filesystem change queues on the inotify instances watching
/// it, against a real Linux kernel: random sequences of path syscalls
/// (creating, truncating, linking, unlinking, renaming, changing modes, owners
/// and times), of descriptors opened, written through, truncated, `fchmod`ed
/// and closed, and of watches added and removed, some with `IN_EXCL_UNLINK`,
/// on two instances, run over a fresh real temporary directory and over the
/// model holding the same tree.
/// Every call must answer alike, and after each, the records each instance
/// holds must be alike, cookies compared by which records share one.
///
/// Watches ask for the self events too (`IN_DELETE_SELF`, `IN_MOVE_SELF`),
/// and every `IN_IGNORED` is compared, that of a watched inode's going as
/// well as that of an `inotify_rm_watch`. Linux only; elsewhere the test is
/// ignored.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyDeliveryAgainstHost =

    [<DllImport("libc", EntryPoint = "inotify_init1", SetLastError = true)>]
    extern int private hostInit1(int flags)

    [<DllImport("libc", EntryPoint = "inotify_add_watch", SetLastError = true)>]
    extern int private hostAddWatch(int fd, byte[] path, uint32 mask)

    [<DllImport("libc", EntryPoint = "inotify_rm_watch", SetLastError = true)>]
    extern int private hostRmWatch(int fd, int wd)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(byte[] path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "mkdir", SetLastError = true)>]
    extern int private hostMkdir(byte[] path, int mode)

    [<DllImport("libc", EntryPoint = "rmdir", SetLastError = true)>]
    extern int private hostRmdir(byte[] path)

    [<DllImport("libc", EntryPoint = "unlink", SetLastError = true)>]
    extern int private hostUnlink(byte[] path)

    [<DllImport("libc", EntryPoint = "link", SetLastError = true)>]
    extern int private hostLink(byte[] existing, byte[] path)

    [<DllImport("libc", EntryPoint = "symlink", SetLastError = true)>]
    extern int private hostSymlink(byte[] target, byte[] path)

    [<DllImport("libc", EntryPoint = "rename", SetLastError = true)>]
    extern int private hostRename(byte[] source, byte[] destination)

    [<DllImport("libc", EntryPoint = "chmod", SetLastError = true)>]
    extern int private hostChmod(byte[] path, int mode)

    [<DllImport("libc", EntryPoint = "chown", SetLastError = true)>]
    extern int private hostChown(byte[] path, uint32 user, uint32 group)

    /// `times` is two `struct timespec`s: seconds and nanoseconds, twice.
    [<DllImport("libc", EntryPoint = "utimensat", SetLastError = true)>]
    extern int private hostUtimensat(int dirfd, byte[] path, int64[] times, int flags)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "ftruncate", SetLastError = true)>]
    extern int private hostFtruncate(int fd, int64 length)

    [<DllImport("libc", EntryPoint = "fchmod", SetLastError = true)>]
    extern int private hostFchmod(int fd, int mode)

    [<DllImport("libc", EntryPoint = "geteuid")>]
    extern uint32 private hostGetEUid()

    [<DllImport("libc", EntryPoint = "getegid")>]
    extern uint32 private hostGetEGid()

    let private context : string = "TestInotifyDeliveryAgainstHost"

    let private onLinux (action : SimulatedUnixPlatform -> unit) : unit =
        HostPlatform.onUnixHostKernel (fun platform ->
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> Assert.Ignore "inotify is Linux's"
            | SimulatedUnixFlavour.Linux -> action platform
        )

    let private dirs : string list = [ "." ; "a" ; "a/s" ; "b" ]
    let private names : string list = [ "x" ; "y" ; "s" ; "a" ]

    let private join (dir : string) (entry : string) : string =
        if dir = "." then entry else $"%s{dir}/%s{entry}"

    let private paths : string list =
        dirs
        @ [
            for dir in dirs do
                for entry in names -> join dir entry
        ]
        |> List.distinct

    /// Symbolic links' targets, relative and without "..", so that each side's
    /// resolves within its own tree: the host's tree is not at its root.
    let private targets : string list = [ "x" ; "a" ; "s" ; "s/x" ; "missing" ]

    [<RequireQualifiedAccess>]
    type private Form =
        | Plain
        | Dot
        | Slash

    [<RequireQualifiedAccess>]
    type private Times =
        | Both
        | AccessOnly
        | ModificationOnly

    [<RequireQualifiedAccess>]
    type private Op =
        | Create of path : int * exclusive : bool
        | OpenTruncate of path : int
        | MkDir of path : int
        | RmDir of path : int
        | Unlink of path : int
        | Link of existing : int * path : int
        | Symlink of target : int * path : int
        | Rename of source : int * destination : int
        | Chmod of path : int * form : Form * mode : int
        | Chown of path : int * neither : bool
        | Utimes of path : int * times : Times
        | AddWatch of instance : int * path : int * mask : uint32
        | RmWatch of instance : int * wd : int
        /// `open(2)` of the path, kept: read-only, or for reading and writing.
        | Open of path : int * readOnly : bool
        /// Through the kept descriptor `slot` (modulo how many are kept): a
        /// write of `count` bytes.
        | Write of slot : int * count : int
        | Ftruncate of slot : int
        | Fchmod of slot : int * mode : int
        | Close of slot : int

    /// Every event this kernel raises.
    let private eventBits : uint32 list =
        [ 0x2u ; 0x4u ; 0x40u ; 0x80u ; 0x100u ; 0x200u ; 0x400u ; 0x800u ]

    let private opGen : Gen<Op> =
        let path = Gen.choose (0, paths.Length - 1)

        let mask =
            gen {
                let! events =
                    Gen.subListOf eventBits
                    |> Gen.map (List.fold (|||) 0u)
                    |> Gen.filter (fun mask -> mask <> 0u)

                // `IN_EXCL_UNLINK`.
                let! excl = Gen.elements [ 0u ; 0x04000000u ]
                return events ||| excl
            }

        let slot = Gen.choose (0, 7)

        Gen.frequency
            [
                4, Gen.map2 (fun p e -> Op.Create (p, e)) path (Gen.elements [ true ; false ])
                2, Gen.map Op.OpenTruncate path
                3, Gen.map Op.MkDir path
                2, Gen.map Op.RmDir path
                3, Gen.map Op.Unlink path
                2, Gen.map2 (fun e p -> Op.Link (e, p)) path path
                1, Gen.map2 (fun t p -> Op.Symlink (t, p)) (Gen.choose (0, targets.Length - 1)) path
                4, Gen.map2 (fun s d -> Op.Rename (s, d)) path path
                4,
                Gen.map3
                    (fun p f m -> Op.Chmod (p, f, m))
                    path
                    (Gen.elements [ Form.Plain ; Form.Plain ; Form.Dot ; Form.Slash ])
                    (Gen.elements [ 0o755 ; 0o700 ])
                2, Gen.map2 (fun p n -> Op.Chown (p, n)) path (Gen.elements [ false ; false ; true ])
                2,
                Gen.map2
                    (fun p t -> Op.Utimes (p, t))
                    path
                    (Gen.elements [ Times.Both ; Times.AccessOnly ; Times.ModificationOnly ])
                5, Gen.map3 (fun i p m -> Op.AddWatch (i, p, m)) (Gen.choose (0, 1)) path mask
                1, Gen.map2 (fun i w -> Op.RmWatch (i, w)) (Gen.choose (0, 1)) (Gen.choose (1, 6))
                4, Gen.map2 (fun p r -> Op.Open (p, r)) path (Gen.elements [ false ; false ; true ])
                5, Gen.map2 (fun s c -> Op.Write (s, c)) slot (Gen.elements [ 0 ; 1 ; 1 ; 3 ])
                2, Gen.map Op.Ftruncate slot
                2, Gen.map2 (fun s m -> Op.Fchmod (s, m)) slot (Gen.elements [ 0o755 ; 0o700 ])
                2, Gen.map Op.Close slot
            ]

    /// Half the sequences open by watching every directory of the pool, on
    /// both instances, for every event, so that most changes reach a watch.
    /// A file with two names, watched itself, opened through one or both or
    /// one twice, then its names unlinked or renamed over in some order and
    /// its descriptors closed; or a directory, watched, opened, removed and
    /// closed: when each goes for inotify depends on what holds its names.
    let private selfEpisodeGen : Gen<Op list> =
        gen {
            let path = Gen.choose (0, paths.Length - 1)
            let! p = path
            let! q = path
            let! other = path
            let! instance = Gen.choose (0, 1)
            let! events = Gen.elements [ 0xC00u ; 0x400u ; 0x4u ; List.fold (|||) 0u eventBits ]

            let! opens =
                Gen.elements [ [] ; [ p ] ; [ q ] ; [ p ; q ] ; [ p ; p ] ; [ q ; p ; q ] ]
                |> Gen.map (List.map (fun opened -> Op.Open (opened, false)))

            let! removals =
                Gen.elements
                    [
                        [ Op.Unlink p ; Op.Unlink q ]
                        [ Op.Unlink q ; Op.Unlink p ]
                        [ Op.Unlink p ; Op.Rename (other, q) ]
                        [
                            Op.Unlink q
                            Op.Link (p, q)
                            Op.Open (q, false)
                            Op.Unlink p
                            Op.Unlink q
                        ]
                    ]

            let! closes = Gen.listOfLength 4 (Gen.map Op.Close (Gen.choose (0, 3)))
            let! directory = Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ]

            if directory then
                return
                    [
                        Op.MkDir p
                        Op.AddWatch (instance, p, events)
                        Op.Open (p, true)
                        Op.RmDir p
                        Op.Close 0
                    ]
            else
                return
                    [ Op.Create (p, false) ; Op.Link (p, q) ; Op.AddWatch (instance, p, events) ]
                    @ opens
                    @ removals
                    @ closes
        }

    let private caseGen : Gen<Op list> =
        gen {
            let! opening = Gen.elements [ true ; false ]

            let! ops =
                Gen.frequency [ 6, Gen.map List.singleton opGen ; 1, selfEpisodeGen ]
                |> Gen.listOf
                |> Gen.resize 40
                |> Gen.map List.concat

            let watchAll =
                [
                    for instance in 0..1 do
                        for dir in dirs do
                            Op.AddWatch (instance, List.findIndex ((=) dir) paths, List.fold (|||) 0u eventBits)
                ]

            return (if opening then watchAll else []) @ ops
        }

    [<RequireQualifiedAccess>]
    type private Label =
        | Created
        | Deleted
        | Moved
        | AttribNamed
        | AttribSelf
        | Modified
        | DeletedSelf
        | MovedSelf
        | Ignored
        /// A call through a kept descriptor that answered.
        | ThroughDescriptor

    /// Numbers each cookie by the order it first appears in.
    type private Cookies () =
        let seen = Dictionary<uint32, uint32> ()

        member _.Ordinal (cookie : uint32) : uint32 =
            if cookie = 0u then
                0u
            else
                match seen.TryGetValue cookie with
                | true, ordinal -> ordinal
                | false, _ ->
                    let ordinal = uint32 seen.Count + 1u
                    seen.[cookie] <- ordinal
                    ordinal

    /// A record as compared: the watch descriptor, the mask, the cookie's
    /// ordinal, the name field's length and the name.
    type private Compared = int * uint32 * uint32 * int * string

    let private compared (cookies : Cookies) (record : ReadRecord) : Compared =
        record.Wd, record.Mask, cookies.Ordinal record.Cookie, record.Len, record.Name

    [<Test>]
    let ``a real inotify and the model queue alike for every path and descriptor change`` () : unit =
        onLinux (fun platform ->
            let numbering = SimulatedUnixPlatform.rawErrnoNumbering platform
            let uid = hostGetEUid ()
            let gid = hostGetEGid ()

            let credentials =
                Credentials.ofIds (UserId.parseOrFail context uid) (GroupId.parseOrFail context gid) []

            let owner : InodeOwner =
                {
                    User = UserId.parseOrFail context uid
                    Group = GroupId.parseOrFail context gid
                }

            let name (text : string) =
                DirectoryEntryName.parseOrFail context text

            let bits (mode : int) = PermissionBits.parseOrFail context mode

            let seed =
                Map.ofList
                    [
                        name "a",
                        SeedEntry.Directory (
                            Map.ofList
                                [
                                    name "s", SeedEntry.Directory (Map.empty, bits 0o755, None)
                                    name "x", SeedEntry.File (ImmutableArray.CreateRange "x"B, bits 0o644, None)
                                ],
                            bits 0o755,
                            None
                        )
                        name "b",
                        SeedEntry.Directory (
                            Map.ofList [ name "y", SeedEntry.File (ImmutableArray.CreateRange "y"B, bits 0o644, None) ],
                            bits 0o755,
                            None
                        )
                    ]

            let initial : UnixSystem<int, string> =
                UnixSystem.initial platform
                |> Launched.fileSystem (UnixTimestamp.ofMillisecondsSinceEpoch 0L) owner seed
                |> Launched.bootWith (Launched.credentials credentials) UnixSystem.pipedStandardStreams 0 (CpuId 0)

            let hostAnswer (result : int) : Result<int64, UnixError> =
                if result >= 0 then
                    Ok (int64 result)
                else
                    let errno = Marshal.GetLastPInvokeError ()

                    match UnixError.ofRawErrnoUnder numbering errno with
                    | Some error -> Error error
                    | None -> failwith $"the host answered errno %d{errno}, which the model has no name for"

            let property (cover : Label -> unit) (ops : Op list) : unit =
                let root =
                    Path.Combine (Path.GetTempPath (), "inotify-delivery-" + Guid.NewGuid().ToString "N")

                Directory.CreateDirectory (Path.Combine (root, "a", "s"))
                |> ignore<DirectoryInfo>

                Directory.CreateDirectory (Path.Combine (root, "b")) |> ignore<DirectoryInfo>
                File.WriteAllText (Path.Combine (root, "a", "x"), "x")
                File.WriteAllText (Path.Combine (root, "b", "y"), "y")

                let onHost (relative : string) : byte[] =
                    Array.append (Encoding.UTF8.GetBytes (root + "/" + relative)) [| 0uy |]

                let bytes (text : string) : byte[] =
                    Array.append (Encoding.UTF8.GetBytes text) [| 0uy |]

                let hostFds = [| hostInit1 0x800 ; hostInit1 0x800 |]

                if hostFds |> Array.exists (fun fd -> fd < 0) then
                    failwith "inotify_init1 failed on the host"

                let system = ref initial
                let modelFds = Array.zeroCreate<int> 2

                for i in 0..1 do
                    let fd, after = InotifyReplay.init1 0x800 system.Value
                    system.Value <- after
                    modelFds.[i] <- fd

                let hostCookies = Cookies ()
                let modelCookies = Cookies ()
                // The kept descriptors, oldest first: the host's and the model's.
                let kept = ResizeArray<int * int> ()

                let answered
                    (op : Op)
                    (host : Result<int64, UnixError>)
                    (model : Result<SyscallAnswer * UnixSystem<int, string>, 'r>)
                    =
                    match model with
                    | Error refusal -> failwith $"%A{op}: refused: %A{refusal}"
                    | Ok (answer, after) ->
                        system.Value <- after

                        let model =
                            match answer with
                            | SyscallAnswer.Completed value -> Ok value
                            | SyscallAnswer.Failed error -> Error error

                        if host <> model then
                            failwith $"%A{op}: the host answered %A{host}, the model %A{model}"

                let opened (op : Op) (flags : OpenFlags) (raw : int) (path : int) =
                    let fd = hostOpen (onHost paths.[path], raw, 0o644)
                    let host = hostAnswer fd

                    if fd >= 0 then
                        hostClose fd |> ignore<int>

                    let model =
                        match OpenFlagWords.openPath flags (PathArg.ofText paths.[path]) 0o644 system.Value with
                        | Ok (SyscallAnswer.Completed fd, after) ->
                            Ok (SyscallAnswer.Completed 0L, InotifyReplay.close (int fd) after)
                        | other -> other

                    answered op (Result.map (fun _ -> 0L) host) model

                try
                    for step, op in List.indexed ops do
                        match op with
                        | Op.Create (path, exclusive) ->
                            // O_WRONLY|O_CREAT, and O_EXCL.
                            let raw = 0x1 ||| 0x40 ||| (if exclusive then 0x80 else 0)
                            opened op (InotifyReplay.openFlags FileAccessMode.WriteOnly true exclusive false) raw path
                        | Op.OpenTruncate path ->
                            // O_WRONLY|O_TRUNC.
                            opened
                                op
                                (InotifyReplay.openFlags FileAccessMode.WriteOnly false false true)
                                (0x1 ||| 0x200)
                                path
                        | Op.MkDir path ->
                            answered
                                op
                                (hostAnswer (hostMkdir (onHost paths.[path], 0o755)))
                                (UnixNamespace.tryMkdir (PathArg.ofText paths.[path]) 0o755 system.Value)
                        | Op.RmDir path ->
                            answered
                                op
                                (hostAnswer (hostRmdir (onHost paths.[path])))
                                (UnixNamespace.tryRmdir (PathArg.ofText paths.[path]) system.Value)
                        | Op.Unlink path ->
                            answered
                                op
                                (hostAnswer (hostUnlink (onHost paths.[path])))
                                (UnixNamespace.tryUnlink (PathArg.ofText paths.[path]) system.Value)
                        | Op.Link (existing, path) ->
                            answered
                                op
                                (hostAnswer (hostLink (onHost paths.[existing], onHost paths.[path])))
                                (UnixNamespace.tryLink
                                    (PathArg.ofText paths.[existing])
                                    (PathArg.ofText paths.[path])
                                    system.Value)
                        | Op.Symlink (target, path) ->
                            answered
                                op
                                (hostAnswer (hostSymlink (bytes targets.[target], onHost paths.[path])))
                                (UnixNamespace.trySymlink
                                    (PathArg.ofText targets.[target])
                                    (PathArg.ofText paths.[path])
                                    system.Value)
                        | Op.Rename (source, destination) ->
                            answered
                                op
                                (hostAnswer (hostRename (onHost paths.[source], onHost paths.[destination])))
                                (UnixNamespace.tryRename
                                    (PathArg.ofText paths.[source])
                                    (PathArg.ofText paths.[destination])
                                    system.Value)
                        | Op.Chmod (path, form, mode) ->
                            let relative =
                                match form with
                                | Form.Plain -> paths.[path]
                                | Form.Dot -> paths.[path] + "/."
                                | Form.Slash -> paths.[path] + "/"

                            answered
                                op
                                (hostAnswer (hostChmod (onHost relative, mode)))
                                (UnixPathResolution.tryChmod (PathArg.ofText relative) mode system.Value)
                        | Op.Chown (path, neither) ->
                            let user, group =
                                if neither then
                                    UInt32.MaxValue, UInt32.MaxValue
                                else
                                    uid, gid

                            answered
                                op
                                (hostAnswer (hostChown (onHost paths.[path], user, group)))
                                (UnixPathResolution.tryChown
                                    (PathArg.ofText paths.[path])
                                    (if neither then None else Some owner.User)
                                    (if neither then None else Some owner.Group)
                                    system.Value)
                        | Op.Utimes (path, times) ->
                            let access, modification =
                                match times with
                                | Times.Both -> (1L, 0L), (2L, 0L)
                                | Times.AccessOnly -> (1L, 0L), (0L, InotifyReplay.utimeOmit)
                                | Times.ModificationOnly -> (0L, InotifyReplay.utimeOmit), (2L, 0L)

                            answered
                                op
                                (hostAnswer (
                                    hostUtimensat (
                                        -100,
                                        onHost paths.[path],
                                        [| fst access ; snd access ; fst modification ; snd modification |],
                                        0
                                    )
                                ))
                                (UnixPathResolution.tryUtimensat
                                    -100
                                    (NullablePathArgument.NotNull (PathArg.ofText paths.[path]))
                                    (TimesArgument.Fields (
                                        InotifyReplay.timespec (fst access) (snd access),
                                        InotifyReplay.timespec (fst modification) (snd modification)
                                    ))
                                    0
                                    system.Value)
                        | Op.AddWatch (instance, path, mask) ->
                            answered
                                op
                                (hostAnswer (hostAddWatch (hostFds.[instance], onHost paths.[path], mask)))
                                (UnixInotify.tryInotifyAddWatch
                                    modelFds.[instance]
                                    (PathArg.ofText paths.[path])
                                    mask
                                    system.Value
                                 |> Result.mapError InotifyAddWatchRefusal.describe)
                        | Op.Open (path, readOnly) ->
                            // O_RDONLY or O_RDWR.
                            let hostFd = hostOpen (onHost paths.[path], (if readOnly then 0x0 else 0x2), 0)
                            let host = hostAnswer hostFd

                            let access =
                                if readOnly then
                                    FileAccessMode.ReadOnly
                                else
                                    FileAccessMode.ReadWrite

                            match
                                OpenFlagWords.openPath
                                    (InotifyReplay.openFlags access false false false)
                                    (PathArg.ofText paths.[path])
                                    0
                                    system.Value
                            with
                            | Ok (SyscallAnswer.Completed modelFd, after) when hostFd >= 0 ->
                                system.Value <- after
                                kept.Add (hostFd, int modelFd)
                            | other ->
                                if hostFd >= 0 then
                                    hostClose hostFd |> ignore<int>

                                answered op host other
                        | Op.Write _
                        | Op.Ftruncate _
                        | Op.Fchmod _
                        | Op.Close _ when kept.Count = 0 -> ()
                        | Op.Write (slot, count) ->
                            let hostFd, modelFd = kept.[slot % kept.Count]
                            let payload = Array.create count (byte 'w')
                            let host = hostWrite (hostFd, payload, unativeint count)

                            let host = if host >= 0n then Ok (int64 host) else hostAnswer -1

                            let model =
                                WriteOutcomes.admitThenWrite
                                    0
                                    modelFd
                                    UserBuffer.Mapped
                                    (ImmutableArray.CreateRange payload)
                                    system.Value
                                |> WriteOutcomes.returned
                                |> Result.map (fun (answer, after) ->
                                    match answer with
                                    | WriteAnswer.Completed written -> SyscallAnswer.Completed written, after
                                    | WriteAnswer.Failed error -> SyscallAnswer.Failed error, after
                                )

                            answered op host model
                        | Op.Ftruncate slot ->
                            let hostFd, modelFd = kept.[slot % kept.Count]

                            answered
                                op
                                (hostAnswer (hostFtruncate (hostFd, 0L)))
                                (UnixDescriptor.tryFtruncate modelFd 0L system.Value)
                        | Op.Fchmod (slot, mode) ->
                            let hostFd, modelFd = kept.[slot % kept.Count]

                            answered
                                op
                                (hostAnswer (hostFchmod (hostFd, mode)))
                                (UnixPathResolution.tryFchmod modelFd mode system.Value)
                        | Op.Close slot ->
                            let index = slot % kept.Count
                            let hostFd, modelFd = kept.[index]
                            kept.RemoveAt index

                            answered op (hostAnswer (hostClose hostFd)) (UnixDescriptor.tryClose modelFd system.Value)
                        | Op.RmWatch (instance, wd) ->
                            answered
                                op
                                (hostAnswer (hostRmWatch (hostFds.[instance], wd)))
                                (UnixInotify.tryInotifyRmWatch modelFds.[instance] wd system.Value
                                 |> Result.mapError InotifyRefusal.describe)

                        for instance in 0..1 do
                            let buffer = Array.zeroCreate<byte> 65536

                            let rec drainHost (acc : ReadRecord list) =
                                let n = hostRead (hostFds.[instance], buffer, unativeint buffer.Length)

                                if n < 0n then
                                    acc
                                else
                                    drainHost (acc @ InotifyReplay.parse (ImmutableArray.Create (buffer, 0, int n)))

                            let host = drainHost [] |> List.map (compared hostCookies)

                            let records, after = InotifyReplay.drainRecords modelFds.[instance] system.Value
                            system.Value <- after
                            let model = records |> List.map (compared modelCookies)

                            if host <> model then
                                failwith
                                    $"step %d{step}, %A{op}: instance %d{instance}: the host queued %A{host}, the model %A{model}"

                            for _, mask, _, _, entry in host do
                                if mask &&& 0x100u <> 0u then
                                    cover Label.Created

                                if mask &&& 0x200u <> 0u then
                                    cover Label.Deleted

                                if mask &&& 0x80u <> 0u then
                                    cover Label.Moved

                                if mask &&& 0x4u <> 0u && entry <> "" then
                                    cover Label.AttribNamed

                                if mask &&& 0x4u <> 0u && entry = "" then
                                    cover Label.AttribSelf

                                if mask &&& 0x2u <> 0u then
                                    cover Label.Modified

                                if mask &&& 0x400u <> 0u then
                                    cover Label.DeletedSelf

                                if mask &&& 0x800u <> 0u then
                                    cover Label.MovedSelf

                                if mask &&& 0x8000u <> 0u then
                                    cover Label.Ignored

                        match op with
                        | Op.Write _
                        | Op.Ftruncate _
                        | Op.Fchmod _ when kept.Count > 0 -> cover Label.ThroughDescriptor
                        | _ -> ()
                finally
                    for fd in hostFds do
                        hostClose fd |> ignore<int>

                    for hostFd, _ in kept do
                        hostClose hostFd |> ignore<int>

                    Directory.Delete (root, true)

            let coverage =
                CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 200) (Arb.fromGen caseGen) property

            for label in
                [
                    Label.Created
                    Label.Deleted
                    Label.Moved
                    Label.AttribNamed
                    Label.AttribSelf
                    Label.Modified
                    Label.DeletedSelf
                    Label.MovedSelf
                    Label.Ignored
                    Label.ThroughDescriptor
                ] do
                if coverage.Count label < 20 then
                    failwith $"the fixed sample reached %A{label} only %d{coverage.Count label} times"
        )
