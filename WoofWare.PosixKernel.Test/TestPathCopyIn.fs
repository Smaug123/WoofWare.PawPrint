namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Every path-taking syscall copies its pathname in itself, from the bytes the
/// caller passed. Two kinds of test hold that:
///
/// - the probe's rows (`path-copyin-order.c`, both flavours), replayed cell by
///   cell, which pin *where* each call copies its path in among its other
///   checks: EFAULT, ENAMETOOLONG and the empty path's ENOENT against each
///   other argument's screen;
/// - a property per family that on a path it copies in, each entry point
///   answers exactly what it answered when it took a parsed path (its
///   `*Parsed` core, the reference), and that on one it cannot copy in it
///   answers that errno and changes nothing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPathCopyIn =

    let private context : string = "TestPathCopyIn"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L
    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private linuxPlatform : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64

    let private darwinPlatform : SimulatedUnixPlatform =
        SimulatedUnixPlatform.macOsArm64

    /// A process on `platform` as `credentials`, its current directory the
    /// root of a filesystem built from `seed`, every entry of which it owns
    /// unless the seed says otherwise.
    let private systemWith
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (seed : Map<DirectoryEntryName, SeedEntry>)
        : UnixSystem<int, string>
        =
        let system : UnixBootImage<int, string> =
            UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withCredentials context credentials

        match
            UnixBootImage.withFileSystemAndCurrentDirectory
                epoch
                (InodeOwner.ofProcess credentials)
                seed
                (AbsoluteUnixPath.parseOrFail context "/")
                system
        with
        | Ok image -> UnixBootImage.boot image
        | Error fault -> failwith $"could not build the system: %A{fault}"

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    /// `n` bytes of "a/a/a...", ending in a name: every component is short, so
    /// only the argument's whole length can refuse it.
    let private slashed (n : int) : string =
        let s = String.init n (fun i -> if i % 2 = 0 then "a" else "/")
        if s.EndsWith '/' then s.Substring (0, n - 1) + "a" else s

    // ------------------------------------------------------------ the probe's rows

    /// The pathnames the probe passed, by the name its output gives them.
    let private argumentNamed (platform : SimulatedUnixPlatform) (column : string) : PathArgumentBytes =
        let pathMax = PathLimits.pathMaxBytes (SimulatedUnixPlatform.pathLimits platform)

        match column with
        | "NULL"
        | "PROT_NONE" -> PathArgumentBytes.Unreadable
        | "overlong" -> PathArg.ofText (slashed pathMax)
        | "inside" -> PathArg.ofText (slashed (pathMax - 1))
        | "empty" -> PathArg.ofText ""
        | "nx"
        | "f"
        | "l" -> PathArg.ofText column
        | other -> failwith $"the probe has no pathname called %s{other}"

    /// The probe's directory: a regular file `f` the caller owns, mode 0644,
    /// and `l -> f`.
    let private probeSeed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList
            [
                name "f", SeedEntry.File (ImmutableArray<byte>.Empty, PermissionBits.parseOrFail context 0o644, None)
                name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail context "f", None)
            ]

    /// Who ran the probe: root in the Linux container, uid 501 (group 20) on
    /// Darwin.
    let private probeSystem (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        let credentials =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux -> Owners.root
            | SimulatedUnixFlavour.Darwin ->
                Credentials.ofIds (UserId.parseOrFail context 501u) (GroupId.parseOrFail context 20u) []

        systemWith platform credentials probeSeed

    let private rendered (error : UnixError option) : string =
        match error with
        | None -> "ok"
        | Some error -> $"%A{error}"

    let private ofAnswer (answer : SyscallAnswer) : UnixError option =
        match answer with
        | SyscallAnswer.Completed _ -> None
        | SyscallAnswer.Failed error -> Some error

    let private flags (access : FileAccessMode) : OpenFlags =
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

    let private uid (n : uint32) : UserId = UserId.parseOrFail context n

    /// What this library answers for one of the probe's rows: the errno, or
    /// `None` for success. `None` for the row itself where the library cannot
    /// express the call; `rowsNotModelled` names each of those.
    let private modelled
        (call : string)
        (arguments : string)
        : (PathArgumentBytes -> UnixSystem<int, string> -> UnixError option) option
        =
        let opened (flags : OpenFlags) =
            Some (fun path system ->
                match OpenFlagWords.openPath flags path 0o644 system with
                | Ok (answer, _) -> ofAnswer answer
                | Error refusal -> failwith $"open refused: %s{OpenRefusal.describe refusal}"
            )

        // A flag word no parsed request stands for, exactly as the probe
        // passed it.
        let openedRaw (word : int) =
            Some (fun path system ->
                match UnixNamespace.openPath word path 0o644 system with
                | Ok (answer, _) -> ofAnswer answer
                | Error refusal -> failwith $"open refused: %s{OpenRefusal.describe refusal}"
            )

        let ro = flags FileAccessMode.ReadOnly
        let wo = flags FileAccessMode.WriteOnly

        let answered
            (f : PathArgumentBytes -> UnixSystem<int, string> -> Result<SyscallAnswer * UnixSystem<int, string>, 'r>)
            =
            Some (fun path system ->
                match f path system with
                | Ok (answer, _) -> ofAnswer answer
                | Error refusal -> failwith $"%s{call} %s{arguments} refused: %A{refusal}"
            )

        let stat (policy : SymlinkPolicy) (nullBuffer : bool) =
            Some (fun path system ->
                match UnixPathResolution.stat policy path system with
                | Ok (FileStatusAnswer.Failed error) -> Some error
                // The buffer is the caller's to write: the walk has answered by
                // now, so an unwritable one is EFAULT exactly where it succeeded.
                | Ok (FileStatusAnswer.Reported _) -> if nullBuffer then Some UnixError.EFAULT else None
                | Error refusal -> failwith $"stat refused: %s{StatRefusal.describe refusal}"
            )

        let readlink (destination : UserBuffer) (capacity : int) =
            Some (fun path system ->
                match UnixNamespace.readlink path destination capacity system with
                | Ok (ReadLinkAnswer.Failed error) -> Some error
                | Ok (ReadLinkAnswer.Reported _) -> None
                | Error refusal -> failwith $"readlink refused: %s{ReadLinkRefusal.describe refusal}"
            )

        let statfs (nullBuffer : bool) =
            Some (fun path system ->
                match Answered.statfs path system with
                | FileSystemStatisticsAnswer.Failed error -> Some error
                | FileSystemStatisticsAnswer.Reported _ -> if nullBuffer then Some UnixError.EFAULT else None
            )

        match call, arguments with
        | "open", "O_RDONLY" -> opened ro
        | "open", "O_WRONLY|O_CREAT" ->
            opened
                { wo with
                    Create = true
                }
        | "open", "O_RDONLY|O_DIRECTORY" ->
            opened
                { ro with
                    Directory = true
                }
        | "open", "O_RDONLY|O_CREAT|O_DIRECTORY" ->
            opened
                { ro with
                    Create = true
                    Directory = true
                }
        | "open", "O_ACCMODE" -> openedRaw 3
        | "open", "all bits" -> openedRaw 0x7fffffff
        | "open", "O_RDONLY|O_TRUNC" ->
            opened
                { ro with
                    Truncate = true
                }
        | "open", "O_WRONLY|O_CREAT|O_EXCL" ->
            opened
                { wo with
                    Create = true
                    Exclusive = true
                }
        | "mkdir", "mode 0777" -> Some (fun path system -> ofAnswer (fst (Answered.mkdir path 0o777 system)))
        | "mkdir", "mode all bits" -> Some (fun path system -> ofAnswer (fst (Answered.mkdir path 0xffff system)))
        | "unlink", "-" -> answered UnixNamespace.unlink
        | "rmdir", "-" -> answered UnixNamespace.rmdir
        | "chdir", "-" -> Some (fun path system -> ofAnswer (fst (Answered.chdir path system)))
        | "chmod", "mode 0644" -> answered (fun path -> UnixPathResolution.chmod path 0o644)
        | "chmod", "mode all bits" -> answered (fun path -> UnixPathResolution.chmod path 0xffff)
        | "chown", "uid -1" -> answered (fun path -> UnixPathResolution.chown path None None)
        | "chown", "uid own" ->
            Some (fun path system ->
                match UnixPathResolution.chown path (Some system.Process.Credentials.EffectiveUser) None system with
                | Ok (answer, _) -> ofAnswer answer
                | Error refusal -> failwith $"chown refused: %A{refusal}"
            )
        | "chown", "uid 4242" -> answered (fun path -> UnixPathResolution.chown path (Some (uid 4242u)) None)
        | "lchown", "uid -1" -> answered (fun path -> UnixPathResolution.lchown path None None)
        | "lchown", "uid 4242" -> answered (fun path -> UnixPathResolution.lchown path (Some (uid 4242u)) None)
        | "stat", "buffer" -> stat SymlinkPolicy.Follow false
        | "stat", "NULL buffer" -> stat SymlinkPolicy.Follow true
        | "lstat", "buffer" -> stat SymlinkPolicy.NoFollowFinal false
        | "lstat", "NULL buffer" -> stat SymlinkPolicy.NoFollowFinal true
        | "readlink", "size 16" -> readlink UserBuffer.Mapped 16
        | "readlink", "size 0" -> readlink UserBuffer.Mapped 0
        | "readlink", "size -1" -> readlink UserBuffer.Mapped -1
        | "readlink", "NULL buffer size 16" -> readlink (UserBuffer.Unmapped 0UL) 16
        | "statfs", "buffer" -> statfs false
        | "statfs", "NULL buffer" -> statfs true
        // `opendir(3)` is this open, as the client issues it.
        | "opendir", "-" ->
            opened
                { ro with
                    Directory = true
                    CloseOnExec = true
                }
        | _ -> None

    /// The probe's rows this library refuses outright, by flavour, and why.
    let private refusedRows : Set<string * string * string> =
        Set.ofList
            [
                // Linux opens access mode 3 as a descriptor that can neither
                // read nor write, which the library does not model.
                "linux", "open", "O_ACCMODE"
                // Every bit: each flavour defines flags among them that the
                // library does not model.
                "linux", "open", "all bits"
                "darwin", "open", "all bits"
            ]

    /// Cells whose answer is the C library's rather than the kernel's: glibc's
    /// `opendir(3)` reads the name's first byte itself, so a NULL or
    /// unreadable one dies with SIGSEGV before any syscall. The client models
    /// that; the kernel's `open` answers EFAULT.
    let private libcCells : Set<string * string * string * string> =
        Set.ofList [ "linux", "opendir", "-", "NULL" ; "linux", "opendir", "-", "PROT_NONE" ]

    let private probeRows (flavour : string) : (string * string * (string * string) list) list =
        let resource = $"WoofWare.PosixKernel.Test.pathCopyInOrder.%s{flavour}.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream resource with
            | null -> failwith $"embedded resource %s{resource} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith "KERNEL") && not (line.StartsWith "CALL"))
        |> List.map (fun line ->
            match line.Split '\t' |> Array.toList with
            | call :: arguments :: cells ->
                let cells =
                    cells
                    |> List.map (fun cell ->
                        match cell.Split ('=', 2) with
                        | [| column ; answer |] -> column, answer
                        | _ -> failwith $"malformed cell %s{cell} in %s{line}"
                    )

                call, arguments, cells
            | _ -> failwith $"malformed row %s{line}"
        )

    let private replay (flavour : string) (platform : SimulatedUnixPlatform) : unit =
        let rows = probeRows flavour
        rows.Length |> shouldBeGreaterThan 25

        let mismatches =
            [
                for call, arguments, cells in rows do
                    match modelled call arguments with
                    | None -> yield $"%s{call} %s{arguments}: the probe measured this row and nothing replays it"
                    | Some run ->
                        for column, expected in cells do
                            if not (libcCells.Contains (flavour, call, arguments, column)) then
                                let actual =
                                    try
                                        rendered (run (argumentNamed platform column) (probeSystem platform))
                                    with e ->
                                        $"refused (%s{e.Message})"

                                // A refusal must come before the path is read, whatever
                                // the path.
                                let expected =
                                    if refusedRows.Contains (flavour, call, arguments) then
                                        "refused"
                                    else
                                        expected

                                if not (actual = expected || (expected = "refused" && actual.StartsWith "refused")) then
                                    yield
                                        $"%s{call} %s{arguments} %s{column}: measured %s{expected}, modelled %s{actual}"
            ]

        mismatches |> shouldEqual []

    [<Test>]
    let ``every Linux row the probe measured is answered as measured`` () : unit = replay "linux" linuxPlatform

    [<Test>]
    let ``every Darwin row the probe measured is answered as measured`` () : unit = replay "darwin" darwinPlatform

    [<Test>]
    let ``the probe's C-library cells really are the C library's`` () : unit =
        // Guards `libcCells`: what the kernel's open answers there is EFAULT,
        // and the probe measured a death, so nothing else is being excused.
        let rows = probeRows "linux"

        for _, call, arguments, column in libcCells do
            let _, _, cells = rows |> List.find (fun (c, a, _) -> c = call && a = arguments)
            cells |> List.find (fst >> (=) column) |> snd |> shouldEqual "signal 11"

    // ------------------------------------------------------- the reference oracle

    let private names : string list = [ "a" ; "b" ; "c" ; "l" ; "m" ]

    let rec private seedGen (depth : int) : Gen<Map<DirectoryEntryName, SeedEntry>> =
        let permissionsGen =
            Gen.elements [ 0o755 ; 0o700 ; 0o555 ; 0o000 ; 0o1777 ; 0o644 ]
            |> Gen.map (PermissionBits.parseOrFail context)

        let symlinkGen =
            Gen.elements [ "a" ; "../a" ; "/a" ; "b/c" ; "l" ; "nx" ; "." ; "a/" ]
            |> Gen.map (fun target -> SeedEntry.Symlink (SymlinkTarget.parseOrFail context target, None))

        let fileGen =
            permissionsGen
            |> Gen.map (fun bits -> SeedEntry.File (ImmutableArray.Create<byte> 1uy, bits, None))

        let entryGen : Gen<SeedEntry> =
            if depth <= 0 then
                Gen.oneof [ fileGen ; symlinkGen ]
            else
                Gen.frequency
                    [
                        2, fileGen
                        2, symlinkGen
                        3,
                        Gen.zip (seedGen (depth - 1)) permissionsGen
                        |> Gen.map (fun (entries, bits) -> SeedEntry.Directory (entries, bits, None))
                    ]

        Gen.choose (0, 4)
        |> Gen.bind (fun count -> Gen.listOfLength count (Gen.zip (Gen.elements names) entryGen))
        |> Gen.map (fun pairs -> pairs |> List.map (fun (n, e) -> name n, e) |> Map.ofList)

    let private systemGen : Gen<UnixSystem<int, string>> =
        gen {
            let! platform = Gen.elements [ linuxPlatform ; darwinPlatform ]
            let! privileged = Gen.elements [ false ; true ]
            let! seed = seedGen 2

            let credentials =
                match SimulatedUnixPlatform.flavour platform, privileged with
                | _, true -> Owners.root
                | SimulatedUnixFlavour.Linux, false -> Owners.linuxDefaultCaller
                | SimulatedUnixFlavour.Darwin, false -> UnixSystem.defaultCredentials SimulatedUnixFlavour.Darwin

            return systemWith platform credentials seed
        }

    /// A path a test can copy in: within `PATH_MAX` on both flavours.
    let private pathGen : Gen<UnixPath> =
        gen {
            let! rooted = Gen.elements [ false ; true ]

            let! components =
                Gen.listOf (Gen.elements (names @ [ "." ; ".." ; "nx" ; "" ]))
                |> Gen.map (List.truncate 5)

            let! trailing = Gen.elements [ false ; true ]
            let text = String.concat "/" components
            let text = (if rooted then "/" else "") + text + (if trailing then "/" else "")
            return UnixPath.parseOrFail context text
        }

    /// An argument this kernel cannot copy in: unreadable, or no NUL within
    /// `PATH_MAX` bytes on either flavour.
    let private badArgumentGen : Gen<PathArgumentBytes * UnixError> =
        Gen.elements
            [
                PathArgumentBytes.Unreadable, UnixError.EFAULT
                PathArg.ofText (slashed 4096), UnixError.ENAMETOOLONG
                PathArg.ofText (String.replicate 5000 "a"), UnixError.ENAMETOOLONG
            ]

    /// What `f` does, an exception included, so that a refusal by `failwith`
    /// compares too.
    let private outcome (f : unit -> 'a) : Result<'a, string> =
        try
            Ok (f ())
        with e ->
            Error e.Message

    /// One entry point, as the property sees it: run it on bytes, run its
    /// reference on the parsed path, and say whether an answer is the given
    /// errno with the system unchanged.
    type private EntryPoint<'a when 'a : equality> =
        {
            Name : string
            Bytes : PathArgumentBytes -> UnixSystem<int, string> -> 'a
            Parsed : UnixPath -> UnixSystem<int, string> -> 'a
            FailedWithoutChange : UnixError -> UnixSystem<int, string> -> 'a -> bool
        }

    let private holds (entry : EntryPoint<'a>) : unit =
        let agrees (system : UnixSystem<int, string>, path : UnixPath) : unit =
            let viaBytes = outcome (fun () -> entry.Bytes (PathArg.ofPath path) system)
            let reference = outcome (fun () -> entry.Parsed path system)

            if viaBytes <> reference then
                failwith $"%s{entry.Name}(%O{path}): via bytes %A{viaBytes}, parsed %A{reference}"

        let refuses (system : UnixSystem<int, string>, (argument : PathArgumentBytes, error : UnixError)) : unit =
            match outcome (fun () -> entry.Bytes argument system) with
            | Ok answer when entry.FailedWithoutChange error system answer -> ()
            | other -> failwith $"%s{entry.Name}: expected %O{error} and no change, got %A{other}"

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip systemGen pathGen)) agrees)
        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip systemGen badArgumentGen)) refuses)

    let private changing
        (name : string)
        (bytes : PathArgumentBytes -> UnixSystem<int, string> -> Result<SyscallAnswer * UnixSystem<int, string>, 'r>)
        (parsed : UnixPath -> UnixSystem<int, string> -> Result<SyscallAnswer * UnixSystem<int, string>, 'r>)
        : EntryPoint<Result<SyscallAnswer * UnixSystem<int, string>, 'r>>
        =
        {
            Name = name
            Bytes = bytes
            Parsed = parsed
            FailedWithoutChange = fun error system answer -> answer = Ok (SyscallAnswer.Failed error, system)
        }

    [<Test>]
    let ``creating calls copy their path in and then answer as before`` () : unit =
        for mode in [ 0o777 ; 0o7755 ] do
            holds (
                changing
                    $"mkdir 0o%o{mode}"
                    (fun path -> UnixNamespace.mkdir path mode)
                    (fun path -> UnixNamespace.mkdirParsed path mode)
            )

        let flagsGen =
            [
                flags FileAccessMode.ReadOnly
                { flags FileAccessMode.WriteOnly with
                    Create = true
                }
                { flags FileAccessMode.ReadWrite with
                    Create = true
                    Exclusive = true
                }
                { flags FileAccessMode.WriteOnly with
                    Truncate = true
                }
                { flags FileAccessMode.ReadOnly with
                    NoFollow = true
                }
                { flags FileAccessMode.ReadOnly with
                    Directory = true
                    CloseOnExec = true
                }
            ]

        for openFlags in flagsGen do
            holds (
                changing
                    $"open %A{openFlags}"
                    (fun path -> OpenFlagWords.openPath openFlags path 0o640)
                    (fun path -> UnixNamespace.openPathParsed openFlags path 0o640)
            )

    [<Test>]
    let ``removing calls copy their path in and then answer as before`` () : unit =
        holds (changing "unlink" UnixNamespace.unlink UnixNamespace.unlinkParsed)
        holds (changing "rmdir" UnixNamespace.rmdir UnixNamespace.rmdirParsed)

    [<Test>]
    let ``attribute-changing calls copy their path in and then answer as before`` () : unit =
        for mode in [ 0o644 ; 0o7777 ; 0o1700 ] do
            holds (
                changing
                    $"chmod 0o%o{mode}"
                    (fun path -> UnixPathResolution.chmod path mode)
                    (fun path -> UnixPathResolution.chmodParsed path mode)
            )

        for user, group in [ None, None ; Some UserId.root, None ; Some (uid 4242u), None ] do
            holds (
                changing
                    $"chown %A{user}"
                    (fun path -> UnixPathResolution.chown path user group)
                    (fun path -> UnixPathResolution.chownParsed path user group)
            )

            holds (
                changing
                    $"lchown %A{user}"
                    (fun path -> UnixPathResolution.lchown path user group)
                    (fun path -> UnixPathResolution.lchownParsed path user group)
            )

        holds (changing "chdir" UnixPathResolution.chdir UnixPathResolution.chdirParsed)

    [<Test>]
    let ``querying calls copy their path in and then answer as before`` () : unit =
        for policy in [ SymlinkPolicy.Follow ; SymlinkPolicy.NoFollowFinal ] do
            holds
                {
                    Name = $"stat %A{policy}"
                    Bytes = UnixPathResolution.stat policy
                    Parsed = UnixPathResolution.statParsed policy
                    FailedWithoutChange = fun error _ answer -> answer = Ok (FileStatusAnswer.Failed error)
                }

        holds
            {
                Name = "statfs"
                Bytes = UnixPathResolution.statfs
                Parsed = UnixPathResolution.statfsParsed
                FailedWithoutChange = fun error _ answer -> answer = Ok (FileSystemStatisticsAnswer.Failed error)
            }

    [<Test>]
    let ``readlink copies its path in after its size, and then answers as before`` () : unit =
        for destination in [ UserBuffer.Mapped ; UserBuffer.Unmapped 0UL ] do
            // Only sizes the flavour admits: a refused size is answered before
            // the path is copied in, which `the probe's rows` pin.
            for capacity in [ 1 ; 4 ; 64 ] do
                holds
                    {
                        Name = $"readlink %A{destination} %d{capacity}"
                        Bytes = fun path -> UnixNamespace.readlink path destination capacity
                        Parsed = fun path -> UnixNamespace.readlinkParsed path destination capacity
                        FailedWithoutChange = fun error _ answer -> answer = Ok (ReadLinkAnswer.Failed error)
                    }

    [<Test>]
    let ``every path-taking Syscall copies its path in`` () : unit =
        // The `step` layer hands the bytes on unchanged: an argument it cannot
        // copy in is that errno, for every case that takes a path.
        let calls (path : PathArgumentBytes) : Syscall list =
            [
                Syscall.MkDir (path, 0o777)
                Syscall.Unlink path
                Syscall.RmDir path
                Syscall.ChDir path
                Syscall.ChMod (path, 0o644)
                Syscall.ChOwn (path, None, None)
                Syscall.LChOwn (path, None, None)
                Syscall.Access (path, 0)
                // AT_FDCWD on Linux.
                Syscall.FAccessAt (-100, path, 0, 0)
                Syscall.CloneFile (path, path, 0)
            ]

        let system = probeSystem linuxPlatform

        for argument, error in
            [
                PathArgumentBytes.Unreadable, UnixError.EFAULT
                PathArg.ofText (slashed 4096), UnixError.ENAMETOOLONG
            ] do
            for call in calls argument do
                match call with
                | Syscall.CloneFile _ ->
                    // Darwin's alone; Linux refuses it as a flavour it lacks.
                    let darwin = probeSystem darwinPlatform

                    let argument =
                        if error = UnixError.EFAULT then
                            argument
                        else
                            PathArg.ofText (slashed 1024)

                    UnixSystem.step 0 (Syscall.CloneFile (argument, argument, 0)) darwin
                    |> shouldEqual (Ok (SyscallOutcome.Answered (SyscallAnswer.Failed error), darwin))
                | call ->
                    UnixSystem.step 0 call system
                    |> shouldEqual (Ok (SyscallOutcome.Answered (SyscallAnswer.Failed error), system))
