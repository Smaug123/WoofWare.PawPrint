namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Which of `UnixSystem.withFileSystemAndCurrentDirectory`'s outcomes are
/// answered to the caller and which crash.
///
/// The split is the point of `CurrentDirectoryFault`: a host that named a
/// directory its own seed does not contain has made a mistake it can fix, and
/// gets told which; a walk that answers an inode the filesystem does not hold
/// has found a bug in this library, which no caller could act on. So the three
/// cases below must each be *reachable* through the public API — a fault case
/// nothing can produce is a case a caller must handle and never will.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCurrentDirectoryFault =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    /// `outer/inner/` beside `outer/file`, so a row can ask to start in a
    /// directory, in a file, and in something absent.
    let private seed : Map<FileName, SeedEntry> =
        Map.ofList
            [
                name "outer",
                SeedEntry.directory (
                    Map.ofList
                        [
                            name "inner", SeedEntry.directory FileSystemSeed.empty
                            name "file", SeedEntry.file noBytes
                        ]
                )
            ]

    let private startAt
        (platform : SimulatedUnixPlatform)
        (entries : Map<FileName, SeedEntry>)
        (dir : string)
        : Result<UnixSystem<int, string>, CurrentDirectoryFault>
        =
        UnixSystem.initial<int, string> platform
        |> UnixSystem.withFileSystemAndCurrentDirectory platform createdAt entries (absolute dir)

    [<Test>]
    let ``a directory the seed contains is accepted`` () : unit =
        // The control. Without it every row below would pass against a function
        // that refused everything.
        match startAt SimulatedUnixPlatform.linuxX64 seed "/outer/inner" with
        | Ok system ->
            UnixSystem.currentDirectoryPath system
            |> shouldEqual (Some (absolute "/outer/inner"))
        | Error fault -> failwith $"expected /outer/inner to be accepted, but it answered %O{fault}."

    [<Test>]
    let ``a directory the seed does not contain answers DoesNotResolve`` () : unit =
        startAt SimulatedUnixPlatform.linuxX64 seed "/outer/nope"
        |> shouldEqual (Error (CurrentDirectoryFault.DoesNotResolve UnixError.ENOENT))

    [<Test>]
    let ``a path that names a file answers NotADirectory`` () : unit =
        // Distinguished from `DoesNotResolve`: a host pointing at a real file
        // has made a different mistake, and the two remedies differ.
        startAt SimulatedUnixPlatform.linuxX64 seed "/outer/file"
        |> shouldEqual (Error CurrentDirectoryFault.NotADirectory)

    [<Test>]
    let ``a component past NAME_MAX answers TooLong, on the flavour that says so`` () : unit =
        // The same seed and the same path under both flavours, because that is
        // what makes this a claim about `NAME_MAX` rather than about the seed:
        // `NAME_MAX` counts UTF-16 code units on Darwin and bytes on Linux, so
        // 255 CJK characters name a directory a Darwin process can start in and
        // a Linux one cannot.
        let wide = String.replicate 255 "中"
        let entries = Map.ofList [ name wide, SeedEntry.directory FileSystemSeed.empty ]
        let path = "/" + wide

        startAt SimulatedUnixPlatform.linuxX64 entries path
        |> shouldEqual (Error (CurrentDirectoryFault.TooLong SimulatedUnixFlavour.Linux))

        match startAt SimulatedUnixPlatform.macOsArm64 entries path with
        | Ok system -> UnixSystem.currentDirectoryPath system |> shouldEqual (Some (absolute path))
        | Error fault -> failwith $"Darwin's NAME_MAX admits this name, but it answered %O{fault}."

    [<Test>]
    let ``the platform argument decides, not the one the system carries`` () : unit =
        // The two are separable, and a reading of the system's own field would
        // pass every row above: they all boot the system on the flavour they
        // then pass. Here the two disagree, and the argument must win.
        let wide = String.replicate 255 "中"
        let entries = Map.ofList [ name wide, SeedEntry.directory FileSystemSeed.empty ]
        let path = "/" + wide

        UnixSystem.initial<int, string> SimulatedUnixPlatform.macOsArm64
        |> UnixSystem.withFileSystemAndCurrentDirectory SimulatedUnixPlatform.linuxX64 createdAt entries (absolute path)
        |> shouldEqual (Error (CurrentDirectoryFault.TooLong SimulatedUnixFlavour.Linux))

    [<Test>]
    let ``every fault case is reachable`` () : unit =
        // The claim the fixture exists to make. Written as a comparison against
        // the union's own case list rather than as a count, so that adding a
        // case fails here by name instead of by arithmetic.
        let reached =
            [
                startAt SimulatedUnixPlatform.linuxX64 seed "/outer/nope"
                startAt SimulatedUnixPlatform.linuxX64 seed "/outer/file"
                startAt
                    SimulatedUnixPlatform.linuxX64
                    (Map.ofList [ name (String.replicate 255 "中"), SeedEntry.directory FileSystemSeed.empty ])
                    ("/" + String.replicate 255 "中")
            ]
            |> List.choose (fun result ->
                match result with
                | Ok _ -> None
                | Error fault ->
                    Some (
                        Reflection.FSharpValue.GetUnionFields (fault, typeof<CurrentDirectoryFault>)
                        |> fst
                    )
            )
            |> List.map (fun case -> case.Name)
            |> Set.ofList

        let declared =
            Reflection.FSharpType.GetUnionCases typeof<CurrentDirectoryFault>
            |> Array.map (fun case -> case.Name)
            |> Set.ofArray

        Set.difference declared reached |> shouldEqual Set.empty

    [<Test>]
    let ``a symlink expansion past PATH_MAX is the same fault, and Linux does not have it`` () : unit =
        // Darwin re-checks the total length when it splices a symlink target in,
        // and reports the same `ENAMETOOLONG` a component past `NAME_MAX` earns.
        // So `TooLong` covers both, and must not be read as the NAME_MAX story:
        // nothing about `/l` here is too long, and shortening it cannot help --
        // it is the seed's symlink target that no path can accommodate.
        // Four hundred components of two bytes each: every one of them is
        // comfortably inside `NAME_MAX`, so the only limit this can reach is
        // the total length. A single 1100-byte component would take the
        // `NAME_MAX` path instead and prove nothing about the splice.
        let deep = String.replicate 400 "/ab"

        let entries =
            Map.ofList [ name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail "test" deep) ]

        startAt SimulatedUnixPlatform.macOsArm64 entries "/l"
        |> shouldEqual (Error (CurrentDirectoryFault.TooLong SimulatedUnixFlavour.Darwin))

        // Linux does not re-check a splice at all, so the same seed gets all the
        // way to the target and finds nothing there. This is what says the row
        // above measures the re-check rather than the length of anything.
        startAt SimulatedUnixPlatform.linuxX64 entries "/l"
        |> shouldEqual (Error (CurrentDirectoryFault.DoesNotResolve UnixError.ENOENT))

    /// A system booted on `seed`, standing at the root.
    let private booted () : UnixSystem<int, string> =
        match startAt SimulatedUnixPlatform.linuxX64 seed "/" with
        | Ok system -> system
        | Error fault -> failwith $"the fixture's own seed did not boot: %O{fault}."

    let private plainOpen : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
        }

    let private replaceFileSystem (system : UnixSystem<int, string>) : UnixSystem<int, string> =
        match
            system
            |> UnixSystem.withFileSystemAndCurrentDirectory
                SimulatedUnixPlatform.linuxX64
                createdAt
                (Map.ofList [ name "other", SeedEntry.directory FileSystemSeed.empty ])
                (absolute "/")
        with
        | Ok replaced -> replaced
        | Error fault -> failwith $"the replacement seed did not boot: %O{fault}."

    [<Test>]
    let ``replacing the filesystem under an open descriptor is refused`` () : unit =
        // The precondition publishing this created. In PawPrint it was called
        // once, at configuration time, on a kernel nothing had opened anything
        // on; as library API any client can call it at any moment. A new
        // filesystem hands out its own inode numbers, so the descriptions would
        // afterwards name a graph that no longer exists -- measured before the
        // guard existed, `checkInvariants` reported `DanglingOpenInode`.
        let _, withHandle =
            UnixSystem.openPath plainOpen (UnixPath.ofAbsolute (absolute "/outer/file")) 0 (booted ())

        // The handle really is filesystem-backed, so the guard below has
        // something to see. Without this the row would pass against a system
        // whose `open` had quietly failed.
        UnixSystem.checkInvariants withHandle |> shouldBeEmpty

        let thrown =
            Assert.Throws<exn> (fun () -> replaceFileSystem withHandle |> ignore<UnixSystem<int, string>>)

        thrown.Message |> shouldContainText "still holds"

    [<Test>]
    let ``replacing the filesystem under a descriptor-less directory stream is refused`` () : unit =
        // The holder a guard written over the descriptions alone would miss.
        // `heldInodes` counts a directory stream separately for exactly this
        // case, which its own comment describes: a guest may close the
        // descriptor `opendir` took out from under the stream, and the stream
        // still names the directory afterwards.
        let answer, withStream =
            UnixSystem.opendir (UnixPath.ofAbsolute (absolute "/outer")) (booted ())

        match answer with
        | OpenDirAnswer.Failed error -> failwith $"opendir on /outer failed: %O{error}."
        | OpenDirAnswer.Opened _ ->

        let closed =
            match UnixSystem.close 3 withStream with
            | Ok (SyscallAnswer.Completed 0L, system) -> system
            | other -> failwith $"closing the stream's descriptor did not succeed: %A{other}."

        // The point of the row: no description remains, and the stream does. So
        // a guard reading only descriptions would let this system through.
        closed.Process.FileDescriptors
        |> FileDescriptorRegistry.descriptions
        |> Map.toList
        |> List.filter (fun (_, description) ->
            match description.Target with
            | OpenFileTarget.File _ -> true
            | _ -> false
        )
        |> shouldBeEmpty

        closed.Process.DirectoryStreams |> Map.isEmpty |> shouldEqual false

        // Asserted on the guard firing rather than on the wording: the message
        // names both holders whichever one triggered it, so matching "directory
        // stream" would pass for the descriptor row too. The setup above is
        // what makes this row specific.
        let thrown =
            Assert.Throws<exn> (fun () -> replaceFileSystem closed |> ignore<UnixSystem<int, string>>)

        thrown.Message |> shouldContainText "still holds"

    [<Test>]
    let ``a handle onto the current directory itself is refused`` () : unit =
        // The case a set of inode *values* cannot answer. Standing at `/` and
        // opening `/` gives a descriptor and a stream that both name the very
        // inode the current directory names, so exempting the current directory
        // by value erases them from the reckoning and the guard sees nothing
        // held at all. They would then carry into the replacement filesystem
        // and silently retarget, because the new graph reissues the root's
        // number -- `checkInvariants` cannot see that.
        let answer, withStream =
            UnixSystem.opendir (UnixPath.ofAbsolute (absolute "/")) (booted ())

        match answer with
        | OpenDirAnswer.Failed error -> failwith $"opendir on / failed: %O{error}."
        | OpenDirAnswer.Opened _ ->

        withStream.Process.DirectoryStreams
        |> Map.toList
        |> List.map (fun (_, stream) -> stream.Inode)
        |> shouldEqual [ withStream.Process.CurrentDirectoryInode ]

        let thrown =
            Assert.Throws<exn> (fun () -> replaceFileSystem withStream |> ignore<UnixSystem<int, string>>)

        thrown.Message |> shouldContainText "still holds"

    [<Test>]
    let ``a standard stream does not block replacement`` () : unit =
        // The guard must not be "any open descriptor at all": a freshly booted
        // process already holds stdin, stdout and stderr, and they are not on
        // the filesystem. Without this row the two above would pass against a
        // guard that refused every system, including the one PawPrint actually
        // configures.
        booted () |> replaceFileSystem |> UnixSystem.checkInvariants |> shouldBeEmpty
