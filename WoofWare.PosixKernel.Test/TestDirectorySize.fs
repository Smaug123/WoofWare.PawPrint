namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of a directory's history, stated as indices into whatever the
/// directory holds when the step runs rather than as names, so that every
/// generated history is valid by construction and no step's success depends on
/// the model under test.
type DirectorySizeStep =
    | CreateFile of nameLength : int
    | MakeDirectory of nameLength : int
    /// Put files *inside* one of the directory's subdirectories, which must not
    /// move the directory's own size.
    | FillSubdirectory of victim : int * count : int
    | Remove of victim : int
    | RenameWithin of victim : int * nameLength : int
    | RenameOut of victim : int
    | RenameIn of nameLength : int
    /// Rename one entry over another of the same kind, which removes a name.
    | RenameOver of source : int * destination : int

/// A namespace operation on a path relative to the history's root: `d` is the
/// directory whose size is watched and `o` is a sibling for renames to cross.
[<RequireQualifiedAccess>]
type DirectorySizeOp =
    | Create of path : string
    | MakeDirectory of path : string
    | Unlink of path : string
    | RemoveDirectory of path : string
    | Rename of source : string * destination : string

/// `st_size` for a directory, driven through histories of `open(O_CREAT)`,
/// `mkdir`, `unlink`, `rmdir` and `rename`.
///
/// The model tests below check the library against the rule measured on a real
/// kernel of each filesystem type; the host test checks the same histories
/// against the kernel this test runs on, where that kernel's filesystem is one
/// the library models.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectorySize =

    let private context : string = "TestDirectorySize"

    // ------------------------------------------------------ resolving a history

    [<RequireQualifiedAccess>]
    type private EntryKind =
        | File
        | Directory of children : string list

    type private Oracle =
        {
            Entries : Map<string, EntryKind>
            NextId : int
        }

    /// A name no earlier step has used, padded to `length` bytes where the id
    /// leaves room. Lower case throughout, so that a case-insensitive host
    /// filesystem cannot see two of them as one.
    let private freshName (length : int) (oracle : Oracle) : string * Oracle =
        let stem = $"n%d{oracle.NextId}"

        let name =
            if length > stem.Length then
                stem.PadRight (length, 'x')
            else
                stem

        name,
        { oracle with
            NextId = oracle.NextId + 1
        }

    let private pick (victim : int) (candidates : 'a list) : 'a option =
        match candidates with
        | [] -> None
        | _ -> Some (List.item (victim % List.length candidates) candidates)

    /// The operations `step` stands for, each paired with how many names `d`
    /// holds once it has run.
    let private resolveStep (oracle : Oracle) (step : DirectorySizeStep) : (DirectorySizeOp * int) list * Oracle =
        let names = oracle.Entries |> Map.toList |> List.map fst
        let count (entries : Map<string, EntryKind>) = Map.count entries

        match step with
        | DirectorySizeStep.CreateFile length ->
            let name, oracle = freshName length oracle
            let entries = Map.add name EntryKind.File oracle.Entries

            [ DirectorySizeOp.Create $"d/%s{name}", count entries ],
            { oracle with
                Entries = entries
            }
        | DirectorySizeStep.MakeDirectory length ->
            let name, oracle = freshName length oracle
            let entries = Map.add name (EntryKind.Directory []) oracle.Entries

            [ DirectorySizeOp.MakeDirectory $"d/%s{name}", count entries ],
            { oracle with
                Entries = entries
            }
        | DirectorySizeStep.FillSubdirectory (victim, children) ->
            let directories =
                oracle.Entries
                |> Map.toList
                |> List.choose (fun (name, kind) ->
                    match kind with
                    | EntryKind.Directory existing -> Some (name, existing)
                    | EntryKind.File -> None
                )

            match pick victim directories with
            | None -> [], oracle
            | Some (directory, existing) ->
                let rec go (remaining : int) (oracle : Oracle) (made : string list) =
                    if remaining = 0 then
                        List.rev made, oracle
                    else
                        let name, oracle = freshName 8 oracle
                        go (remaining - 1) oracle (name :: made)

                let made, oracle = go children oracle []

                let entries =
                    Map.add directory (EntryKind.Directory (existing @ made)) oracle.Entries

                made
                |> List.map (fun child -> DirectorySizeOp.Create $"d/%s{directory}/%s{child}", count entries),
                { oracle with
                    Entries = entries
                }
        | DirectorySizeStep.Remove victim ->
            match pick victim names with
            | None -> [], oracle
            | Some name ->
                let entries = Map.remove name oracle.Entries

                let ops =
                    match oracle.Entries.[name] with
                    | EntryKind.File -> [ DirectorySizeOp.Unlink $"d/%s{name}", count entries ]
                    | EntryKind.Directory children ->
                        let before = count oracle.Entries

                        (children
                         |> List.map (fun child -> DirectorySizeOp.Unlink $"d/%s{name}/%s{child}", before))
                        @ [ DirectorySizeOp.RemoveDirectory $"d/%s{name}", count entries ]

                ops,
                { oracle with
                    Entries = entries
                }
        | DirectorySizeStep.RenameWithin (victim, length) ->
            match pick victim names with
            | None -> [], oracle
            | Some name ->
                let fresh, oracle = freshName length oracle

                let entries =
                    oracle.Entries |> Map.remove name |> Map.add fresh oracle.Entries.[name]

                [ DirectorySizeOp.Rename ($"d/%s{name}", $"d/%s{fresh}"), count entries ],
                { oracle with
                    Entries = entries
                }
        | DirectorySizeStep.RenameOut victim ->
            match pick victim names with
            | None -> [], oracle
            | Some name ->
                let fresh, oracle = freshName 8 oracle
                let entries = Map.remove name oracle.Entries

                [ DirectorySizeOp.Rename ($"d/%s{name}", $"o/%s{fresh}"), count entries ],
                { oracle with
                    Entries = entries
                }
        | DirectorySizeStep.RenameIn length ->
            let outside, oracle = freshName 8 oracle
            let name, oracle = freshName length oracle
            let entries = Map.add name EntryKind.File oracle.Entries

            [
                DirectorySizeOp.Create $"o/%s{outside}", count oracle.Entries
                DirectorySizeOp.Rename ($"o/%s{outside}", $"d/%s{name}"), count entries
            ],
            { oracle with
                Entries = entries
            }
        | DirectorySizeStep.RenameOver (source, destination) ->
            match pick source names, pick destination names with
            | Some source, Some destination when source <> destination ->
                // Only between two files, or onto an empty directory: every
                // other pairing is an errno rather than a removal.
                let permitted =
                    match oracle.Entries.[source], oracle.Entries.[destination] with
                    | EntryKind.File, EntryKind.File -> true
                    | EntryKind.Directory _, EntryKind.Directory [] -> true
                    | _ -> false

                if not permitted then
                    [], oracle
                else
                    let entries =
                        oracle.Entries
                        |> Map.remove source
                        |> Map.add destination oracle.Entries.[source]

                    [
                        DirectorySizeOp.Rename ($"d/%s{source}", $"d/%s{destination}"), count entries
                    ],
                    { oracle with
                        Entries = entries
                    }
            | _ -> [], oracle

    /// A whole history, flattened into the operations it stands for.
    let private resolve (steps : DirectorySizeStep list) : (DirectorySizeOp * int) list =
        let initial =
            {
                Entries = Map.empty
                NextId = 0
            }

        steps |> List.mapFold resolveStep initial |> fst |> List.concat

    let private stepGen : Gen<DirectorySizeStep> =
        let length = Gen.choose (1, 255)
        let index = Gen.choose (0, 1000)

        Gen.frequency
            [
                6, Gen.map DirectorySizeStep.CreateFile length
                2, Gen.map DirectorySizeStep.MakeDirectory length
                1, Gen.zip index (Gen.choose (1, 4)) |> Gen.map DirectorySizeStep.FillSubdirectory
                5, Gen.map DirectorySizeStep.Remove index
                2, Gen.zip index length |> Gen.map DirectorySizeStep.RenameWithin
                1, Gen.map DirectorySizeStep.RenameOut index
                1, Gen.map DirectorySizeStep.RenameIn length
                2, Gen.zip index index |> Gen.map DirectorySizeStep.RenameOver
            ]

    let private historyGen : Gen<DirectorySizeStep list> =
        Gen.choose (0, 150) |> Gen.bind (fun n -> Gen.listOfLength n stepGen)

    // ----------------------------------------------------------- the oracle rule

    /// What a real kernel reports as a directory's `st_size`, as a function of
    /// the names it holds besides `.` and `..`. Measured, not derived: see the
    /// comment on `EmulatedFileSystemType.directorySize` for the sweep.
    let private measuredSize (fsType : EmulatedFileSystemType) (entries : int) : int64 =
        match fsType with
        | EmulatedFileSystemType.Tmpfs -> 40L + 20L * int64 entries
        | EmulatedFileSystemType.Apfs -> 64L + 32L * int64 entries
        | EmulatedFileSystemType.Nfs -> failwith "test bug: NFS has no measured directory size"

    // ------------------------------------------------------------- the model

    let private creating : OpenFlags =
        {
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
        }

    let private reading : OpenFlags =
        { creating with
            Access = FileAccessMode.ReadOnly
            Create = false
        }

    let private rooted (relative : string) : UnixPath =
        UnixPath.parseOrFail context $"/%s{relative}"

    let private argument (relative : string) : PathArgumentBytes =
        PathArgumentBytes.Bytes (ImmutableArray.CreateRange (Text.Encoding.UTF8.GetBytes $"/%s{relative}"))

    let private completed
        (what : string)
        (answer : SyscallAnswer, system : UnixSystem<int, string>)
        : int64 * UnixSystem<int, string>
        =
        match answer with
        | SyscallAnswer.Completed value -> value, system
        | SyscallAnswer.Failed error ->
            failwith $"%s{what} failed with %O{error}, but the history is valid by construction"

    let private closeFd (fd : int) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match UnixDescriptor.close fd system with
        | Ok (SyscallAnswer.Completed _, system) -> system
        | other -> failwith $"close %d{fd}: %A{other}"

    let private applyToModel (op : DirectorySizeOp) (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match op with
        | DirectorySizeOp.Create path ->
            let fd, system =
                UnixNamespace.openPath creating (rooted path) 0o644 system
                |> completed $"creat %s{path}"

            closeFd (int fd) system
        | DirectorySizeOp.MakeDirectory path ->
            UnixNamespace.mkdir (rooted path) 0o755 system
            |> completed $"mkdir %s{path}"
            |> snd
        | DirectorySizeOp.Unlink path ->
            UnixNamespace.unlink (rooted path) system |> completed $"unlink %s{path}" |> snd
        | DirectorySizeOp.RemoveDirectory path ->
            UnixNamespace.rmdir (rooted path) system |> completed $"rmdir %s{path}" |> snd
        | DirectorySizeOp.Rename (source, destination) ->
            match UnixNamespace.rename (argument source) (argument destination) system with
            | Ok result -> result |> completed $"rename %s{source} %s{destination}" |> snd
            | Error refusal -> failwith $"rename %s{source} %s{destination} was refused: %A{refusal}"

    /// A fresh system on `platform`, mounted as `fsType`, holding the empty
    /// directories `/d` and `/o`, with a read-only descriptor onto `/d`.
    let private modelWith
        (platform : SimulatedUnixPlatform)
        (fsType : EmulatedFileSystemType)
        : int * UnixSystem<int, string>
        =
        let system : UnixSystem<int, string> = UnixSystem.initial platform

        let system =
            { system with
                Machine = UnixMachineState.withFileSystemType (Some fsType) system.Machine
            }

        let system =
            system
            |> applyToModel (DirectorySizeOp.MakeDirectory "d")
            |> applyToModel (DirectorySizeOp.MakeDirectory "o")

        let fd, system =
            UnixNamespace.openPath reading (rooted "d") 0 system |> completed "open d"

        int fd, system

    let private modelSize (system : UnixSystem<int, string>) : int64 =
        match UnixPathResolution.stat SymlinkPolicy.Follow (rooted "d") system with
        | Ok (FileStatusAnswer.Reported status) -> status.Size
        | Ok (FileStatusAnswer.Failed error) -> failwith $"stat d failed with %O{error}"
        | Error refusal -> failwith $"stat d was refused: %s{StatRefusal.describe refusal}"

    let private modelFStatSize (fd : int) (system : UnixSystem<int, string>) : int64 =
        match UnixPathResolution.fstat fd system with
        | Ok (FileStatusAnswer.Reported status) -> status.Size
        | other -> failwith $"fstat %d{fd}: %A{other}"

    /// `lseek(fd, offset, SEEK_END)` as the model answers it: the position, or
    /// the errno.
    let private modelSeekEnd (fd : int) (offset : int64) (system : UnixSystem<int, string>) : Result<int64, UnixError> =
        match UnixDescriptor.lseek fd offset 2 system with
        | Ok (SyscallAnswer.Completed position, _) -> Ok position
        | Ok (SyscallAnswer.Failed error, _) -> Error error
        | Error refusal ->
            failwith $"lseek(%d{fd}, %d{offset}, SEEK_END) was refused: %s{LSeekRefusal.describe refusal}"

    /// What `lseek(dir, 0, SEEK_END)` is measured to answer on each filesystem.
    let private measuredSeekEnd (fsType : EmulatedFileSystemType) (entries : int) : Result<int64, UnixError> =
        match fsType with
        // Measured on every step of every history below: tmpfs's directories
        // take no `SEEK_END` at all.
        | EmulatedFileSystemType.Tmpfs -> Error UnixError.EINVAL
        // And APFS's seek relative to the size `stat` reports.
        | EmulatedFileSystemType.Apfs -> Ok (measuredSize fsType entries)
        | EmulatedFileSystemType.Nfs -> failwith "test bug: NFS has no measured directory seek"

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    /// The (platform, filesystem) pairs whose directory size is a function of
    /// the directory's contents.
    let private derivable : (SimulatedUnixPlatform * EmulatedFileSystemType) list =
        [
            SimulatedUnixPlatform.linuxX64, EmulatedFileSystemType.Tmpfs
            SimulatedUnixPlatform.macOsArm64, EmulatedFileSystemType.Apfs
        ]

    [<Test>]
    let ``a directory's size follows its entry count through any history`` () : unit =
        for platform, fsType in derivable do
            let property (steps : DirectorySizeStep list) : unit =
                let fd, system = modelWith platform fsType

                let check (entries : int) (system : UnixSystem<int, string>) =
                    let expected = measuredSize fsType entries
                    let viaStat = modelSize system
                    let viaFStat = modelFStatSize fd system

                    if viaStat <> expected || viaFStat <> expected then
                        failwith
                            $"%O{fsType}: /d holds %d{entries} names, so should report %d{expected}, but stat says %d{viaStat} and fstat says %d{viaFStat}"

                    let seek = modelSeekEnd fd 0L system
                    let expectedSeek = measuredSeekEnd fsType entries

                    if seek <> expectedSeek then
                        failwith
                            $"%O{fsType}: /d holds %d{entries} names, so lseek(d, 0, SEEK_END) should be %A{expectedSeek}, but was %A{seek}"

                check 0 system

                (system, resolve steps)
                ||> List.fold (fun system (op, entries) ->
                    let system = applyToModel op system
                    check entries system
                    system
                )
                |> ignore<UnixSystem<int, string>>

            Check.One (propertyConfig, Prop.forAll (Arb.fromGen historyGen) property)

    [<Test>]
    let ``a directory's size follows its entry count out to 5000 names`` () : unit =
        // The histories above rarely hold more than a few dozen names at once;
        // this is the other half of the measurement, which created and then
        // removed 5000 files one at a time.
        for platform, fsType in derivable do
            let _, system = modelWith platform fsType

            let names = [ 0..4999 ] |> List.map (fun i -> $"d/f%d{i}")

            let check (entries : int) (system : UnixSystem<int, string>) =
                let expected = measuredSize fsType entries
                let actual = modelSize system

                if actual <> expected then
                    failwith
                        $"%O{fsType}: /d holds %d{entries} names, so should report %d{expected}, but reports %d{actual}"

            let system =
                (system, List.indexed names)
                ||> List.fold (fun system (i, name) ->
                    let system = applyToModel (DirectorySizeOp.Create name) system
                    check (i + 1) system
                    system
                )

            (system, List.indexed names)
            ||> List.fold (fun system (i, name) ->
                let system = applyToModel (DirectorySizeOp.Unlink name) system
                check (List.length names - i - 1) system
                system
            )
            |> ignore<UnixSystem<int, string>>

    [<Test>]
    let ``SEEK_END on an APFS directory is the regular-file arithmetic over its size`` () : unit =
        // Measured on macOS 26.6 at 0, 1, 5 and 37 entries, for offsets
        // INT64_MIN, -10000, -size-1, -size, -size+1, -1, 0, 1, 7, 2^40 and
        // INT64_MAX-size-1 .. INT64_MAX: a negative result is EINVAL and one
        // past INT64_MAX is EOVERFLOW, exactly as for a regular file.
        let fd, system =
            modelWith SimulatedUnixPlatform.macOsArm64 EmulatedFileSystemType.Apfs

        let system =
            [ 1..5 ]
            |> List.fold (fun system i -> applyToModel (DirectorySizeOp.Create $"d/e%d{i}") system) system

        let size = 64L + 32L * 5L

        let rows =
            [
                Int64.MinValue, Error UnixError.EINVAL
                -10000L, Error UnixError.EINVAL
                -size - 1L, Error UnixError.EINVAL
                -size, Ok 0L
                -1L, Ok (size - 1L)
                0L, Ok size
                7L, Ok (size + 7L)
                Int64.MaxValue - size, Ok Int64.MaxValue
                Int64.MaxValue - size + 1L, Error UnixError.EOVERFLOW
                Int64.MaxValue, Error UnixError.EOVERFLOW
            ]

        for offset, expected in rows do
            modelSeekEnd fd offset system |> shouldEqual expected

    [<Test>]
    let ``SEEK_END on a tmpfs directory is EINVAL whatever the offset`` () : unit =
        // Measured on Linux 6.18.5 at 0, 1, 5 and 37 entries for the offsets
        // listed above: every one is EINVAL, and the position does not move.
        let property (offset : int64) (entries : int) : unit =
            let fd, system =
                modelWith SimulatedUnixPlatform.linuxX64 EmulatedFileSystemType.Tmpfs

            let system =
                [ 1 .. (abs entries % 20) ]
                |> List.fold (fun system i -> applyToModel (DirectorySizeOp.Create $"d/e%d{i}") system) system

            // Away from 0 first, so that a failure which reset the position
            // would be visible.
            let system =
                match UnixDescriptor.lseek fd 7L 0 system with
                | Ok (SyscallAnswer.Completed 7L, system) -> system
                | other -> failwith $"lseek(d, 7, SEEK_SET): %A{other}"

            match UnixDescriptor.lseek fd offset 2 system with
            | Ok (SyscallAnswer.Failed UnixError.EINVAL, after) -> after |> shouldEqual system
            | other -> failwith $"lseek(d, %d{offset}, SEEK_END): %A{other}"

        Check.One (propertyConfig, property)

    // ----------------------------------------------------------------- NFS

    [<Test>]
    let ``an NFS directory's size is refused rather than invented`` () : unit =
        // An NFS client reports whatever the server's own filesystem says, which
        // nothing in this machine determines.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let fd, system = modelWith platform EmulatedFileSystemType.Nfs

            let inodeOf (relative : string) : InodeNumber =
                match
                    UnixPathResolution.resolvePath SymlinkPolicy.Follow (UnixPath.parseOrFail context relative) system
                with
                | Ok inode -> inode
                | Error error -> failwith $"resolving %s{relative}: %O{error}"

            // The root is a directory on the same mount, so it is refused too.
            for path in [ "/d" ; "/" ; "/o" ] do
                for policy in [ SymlinkPolicy.Follow ; SymlinkPolicy.NoFollowFinal ] do
                    UnixPathResolution.stat policy (UnixPath.parseOrFail context path) system
                    |> shouldEqual (Error (StatRefusal.NfsDirectorySize (inodeOf path)))

            UnixPathResolution.fstat fd system
            |> shouldEqual (Error (FStatRefusal.NfsDirectorySize (inodeOf "/d")))

            match UnixDescriptor.lseek fd 0L 2 system with
            | Error (LSeekRefusal.DirectoryEnd _) -> ()
            | other -> failwith $"expected SEEK_END on an NFS directory to be refused, got %A{other}"

    [<Test>]
    let ``an NFS refusal names the server as the reason`` () : unit =
        let inode = InodeNumber 7L

        for text in
            [
                StatRefusal.describe (StatRefusal.NfsDirectorySize inode)
                FStatRefusal.describe (FStatRefusal.NfsDirectorySize inode)
            ] do
            text |> shouldContainText "NFS server"
            text |> shouldContainText (string<InodeNumber> inode)

    [<Test>]
    let ``an NFS mount still reports a regular file's size`` () : unit =
        // The refusal is about directories alone: a file's size is its
        // contents' length on every filesystem.
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let _, system = modelWith platform EmulatedFileSystemType.Nfs
            let system = applyToModel (DirectorySizeOp.Create "d/f") system

            match UnixPathResolution.stat SymlinkPolicy.Follow (rooted "d/f") system with
            | Ok (FileStatusAnswer.Reported status) -> status.Size |> shouldEqual 0L
            | other -> failwith $"stat d/f: %A{other}"

    // ------------------------------------------------------------ the host

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_Stat", SetLastError = true)>]
    extern int private hostStat(string path, byte[] output)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetFileSystemType", SetLastError = true)>]
    extern uint32 private hostGetFileSystemType(nativeint fd)

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "lseek", SetLastError = true)>]
    extern int64 private hostLSeek(int fd, int64 offset, int whence)

    [<DllImport("libc", EntryPoint = "mkdir", SetLastError = true)>]
    extern int private hostMkdir(string path, uint32 mode)

    [<DllImport("libc", EntryPoint = "unlink", SetLastError = true)>]
    extern int private hostUnlink(string path)

    [<DllImport("libc", EntryPoint = "rmdir", SetLastError = true)>]
    extern int private hostRmdir(string path)

    [<DllImport("libc", EntryPoint = "rename", SetLastError = true)>]
    extern int private hostRename(string source, string destination)

    let private hostSucceeded (what : string) (result : int) : unit =
        if result <> 0 then
            failwith $"host %s{what} failed: errno %d{Marshal.GetLastPInvokeError ()}"

    let private applyToHost (root : string) (op : DirectorySizeOp) : unit =
        let under (relative : string) = Path.Combine (root, relative)

        match op with
        | DirectorySizeOp.Create path -> File.Create(under path).Dispose ()
        | DirectorySizeOp.MakeDirectory path -> hostMkdir (under path, 0o755u) |> hostSucceeded $"mkdir %s{path}"
        | DirectorySizeOp.Unlink path -> hostUnlink (under path) |> hostSucceeded $"unlink %s{path}"
        | DirectorySizeOp.RemoveDirectory path -> hostRmdir (under path) |> hostSucceeded $"rmdir %s{path}"
        | DirectorySizeOp.Rename (source, destination) ->
            hostRename (under source, under destination)
            |> hostSucceeded $"rename %s{source} %s{destination}"

    /// `st_size` through the shim's own `SystemNative_Stat`, whose output
    /// struct has one layout on every platform (`Size` at byte 16), rather
    /// than through a `struct stat` that would have to fork by platform.
    let private hostSize (path : string) : int64 =
        let output = Array.zeroCreate<byte> 256
        hostStat (path, output) |> hostSucceeded $"stat %s{path}"
        BitConverter.ToInt64 (output, 16)

    let private hostSeekEnd (fd : int) : Result<int64, UnixError> =
        Marshal.SetLastPInvokeError 0
        let position = hostLSeek (fd, 0L, 2)

        if position >= 0L then
            Ok position
        else
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrno errno with
            | Some error -> Error error
            | None -> failwith $"host lseek failed with errno %d{errno}, which has no portable name"

    /// A directory on this host whose filesystem is the one the library
    /// defaults to for `flavour`, or `None` if this host has none: `/dev/shm`
    /// for tmpfs on Linux, and the temporary directory for APFS on macOS.
    let private hostDirectoryFor (flavour : SimulatedUnixFlavour) : string option =
        let candidate =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "/dev/shm"
            | SimulatedUnixFlavour.Darwin -> Path.GetTempPath ()

        let wanted =
            EmulatedFileSystemType.magic (EmulatedFileSystemType.defaultFor flavour)

        if not (Directory.Exists candidate) then
            None
        else

        let fd = hostOpen (candidate, 0, 0)

        if fd < 0 then
            None
        else

        try
            if hostGetFileSystemType (nativeint fd) = wanted then
                Some candidate
            else
                None
        finally
            hostClose fd |> ignore<int>

    [<Test>]
    let ``a directory's size and SEEK_END agree with this host's through any history`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let fsType = EmulatedFileSystemType.defaultFor flavour

            let hostBase =
                match hostDirectoryFor flavour with
                | Some directory -> directory
                | None ->
                    Assert.Ignore $"this %O{flavour} host has no %O{fsType} directory to measure"
                    failwith "unreachable: Assert.Ignore throws"

            let platform = HostPlatform.platformOf flavour

            let property (steps : DirectorySizeStep list) : unit =
                let unique = Guid.NewGuid().ToString "N"
                let root = Path.Combine (hostBase, $"pawprint-dirsize-%s{unique}")
                hostMkdir (root, 0o755u) |> hostSucceeded "mkdir root"

                try
                    applyToHost root (DirectorySizeOp.MakeDirectory "d")
                    applyToHost root (DirectorySizeOp.MakeDirectory "o")
                    let hostFd = hostOpen (Path.Combine (root, "d"), 0, 0)

                    if hostFd < 0 then
                        failwith $"host open d failed: errno %d{Marshal.GetLastPInvokeError ()}"

                    try
                        let modelFd, system = modelWith platform fsType

                        let check (op : DirectorySizeOp option) (system : UnixSystem<int, string>) =
                            let host = hostSize (Path.Combine (root, "d"))
                            let model = modelSize system

                            if host <> model then
                                failwith
                                    $"after %A{op}, this %O{flavour} host's %O{fsType} reports /d as %d{host} bytes, but the model says %d{model}"

                            let host = hostSeekEnd hostFd
                            let model = modelSeekEnd modelFd 0L system

                            if host <> model then
                                failwith
                                    $"after %A{op}, this %O{flavour} host's lseek(d, 0, SEEK_END) is %A{host}, but the model says %A{model}"

                        check None system

                        (system, resolve steps)
                        ||> List.fold (fun system (op, _) ->
                            applyToHost root op
                            let system = applyToModel op system
                            check (Some op) system
                            system
                        )
                        |> ignore<UnixSystem<int, string>>
                    finally
                        hostClose hostFd |> ignore<int>
                finally
                    Directory.Delete (root, true)

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 30, Prop.forAll (Arb.fromGen historyGen) property)
        )
