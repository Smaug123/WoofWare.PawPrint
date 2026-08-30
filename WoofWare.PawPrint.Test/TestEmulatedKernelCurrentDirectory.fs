namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The simulated process holds its current directory as an *inode*, the way a
/// real process holds it, rather than as a name re-walked on every relative
/// lookup. These are the claims about that pair: what maintains it, and what a
/// host that misconfigures it is told.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedKernelCurrentDirectory =

    let private name (s : string) : FileName = FileName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    /// `outer/inner/`, plus `outer/file` and a link `outer/lnk -> inner`, so a
    /// row can ask for a current directory that is a file, that is reached
    /// through a symlink, and that does not exist at all.
    let private seed : Map<FileName, SeedEntry> =
        Map.ofList
            [
                name "outer",
                SeedEntry.directory (
                    Map.ofList
                        [
                            name "inner", SeedEntry.directory Map.empty
                            name "file", SeedEntry.file noBytes
                            name "lnk", SeedEntry.Symlink (target "inner")
                        ]
                )
            ]

    /// The inode a path names, resolved independently of the kernel — so a row
    /// asserting "the kernel held *this* inode" is checked against the graph
    /// rather than against the kernel's own answer.
    let private inodeOf (kernel : EmulatedKernel) (path : string) : InodeNumber =
        let vfs = kernel.FileSystem

        match
            VirtualFileSystem.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.UnixPlatform)
                CallerPrivilege.Privileged
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" path)
                vfs
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}"

    /// A kernel seeded with the tree above, whose current directory is `dir`.
    let private seededAt (dir : string) : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.withFileSystemAndCurrentDirectory SimulatedUnixPlatform.linuxX64 createdAt seed (absolute dir)

    let private message (body : unit -> unit) : string =
        let thrown = Assert.Throws<exn> (fun () -> body ())
        thrown.Message

    // -------------------------------------------------- what the kernel holds

    [<Test>]
    let ``a freshly minted kernel holds the root`` () : unit =
        EmulatedKernel.currentDirectoryPath EmulatedKernel.initial
        |> shouldEqual (Some AbsoluteUnixPath.root)

        EmulatedKernel.initial.CurrentDirectoryInode
        |> shouldEqual (VirtualFileSystem.root EmulatedKernel.initial.FileSystem)

        EmulatedKernel.checkInvariants EmulatedKernel.initial |> shouldEqual []

    [<Test>]
    let ``the held inode is the one the configured path names`` () : unit =
        let kernel = seededAt "/outer/inner"

        kernel.CurrentDirectoryInode |> shouldEqual (inodeOf kernel "/outer/inner")

        // ...and not merely *some* inode: the two directories in the seed are
        // distinct, so a resolver that stopped at the first component would
        // pass every other assertion here.
        kernel.CurrentDirectoryInode |> shouldNotEqual (inodeOf kernel "/outer")
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    [<Test>]
    let ``a symlinked current directory is canonicalised`` () : unit =
        // A process launched into `outer/lnk` is launched into the directory
        // that link names, and afterwards nothing can tell it went through a
        // link: `getcwd(3)` reports the *physical* path. Measured on both
        // kernels -- `chdir(".../outer/lnk")` with `lnk -> inner` is followed by
        // `getcwd() == ".../outer/inner"`.
        let kernel = seededAt "/outer/lnk"

        kernel.CurrentDirectoryInode |> shouldEqual (inodeOf kernel "/outer/inner")

        EmulatedKernel.currentDirectoryPath kernel
        |> shouldEqual (Some (absolute "/outer/inner"))

        EmulatedKernel.checkInvariants kernel |> shouldEqual []

    // ------------------------------------------------------ order-independence

    [<Test>]
    let ``the platform argument decides, not the one the kernel carries`` () : unit =
        // The sharp case, and the reason the platform is an argument rather
        // than a field read: `NAME_MAX` is 255 *UTF-16 code units* on Darwin and
        // 255 *bytes* on Linux, so this one name is a directory a macOS process
        // could be started in and a Linux one could not.
        let wide = String.replicate 255 "\u4e2d"
        let seed = Map.ofList [ name wide, SeedEntry.directory FileSystemSeed.empty ]

        let darwin =
            EmulatedKernel.initial
            |> EmulatedKernel.withFileSystemAndCurrentDirectory
                SimulatedUnixPlatform.macOsArm64
                createdAt
                seed
                (absolute $"/%s{wide}")

        darwin.CurrentDirectoryInode
        |> shouldNotEqual (VirtualFileSystem.root darwin.FileSystem)

        EmulatedKernel.checkInvariants darwin |> shouldEqual []

        // The kernel this ran against is still Linux-flavoured -- nothing set
        // its platform -- so a setter reading `kernel.UnixPlatform` instead of
        // its argument would have refused the name above. That is what makes
        // the pair setter order-independent with respect to
        // `withUnixPlatformAndFileSystemType`.
        darwin.UnixPlatform |> shouldEqual SimulatedUnixPlatform.linuxX64

        // ...and the other flavour really does refuse it, so the row above is
        // not passing because the limit is never consulted.
        let text =
            message (fun () ->
                EmulatedKernel.initial
                |> EmulatedKernel.withFileSystemAndCurrentDirectory
                    SimulatedUnixPlatform.linuxX64
                    createdAt
                    seed
                    (absolute $"/%s{wide}")
                |> ignore<EmulatedKernel>
            )

        text |> shouldContainText "NAME_MAX"
        text |> shouldContainText "Linux"

    [<Test>]
    let ``an overlong symlink expansion is not blamed on the configured path`` () : unit =
        // Darwin re-checks the total length when it splices a symlink target in,
        // and reports the same `ENAMETOOLONG` a component past `NAME_MAX` earns.
        // A message that told this host only the `NAME_MAX` story would be wrong
        // twice over: `/l` has no overlong component, and shortening it cannot
        // help -- the seed's symlink target is what no path can accommodate.
        //
        // Four hundred two-byte components, each comfortably legal, so the only
        // limit this can reach is the total length.
        let deep = String.replicate 400 "/ab"

        let seed =
            Map.ofList [ name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail "test" deep) ]

        let text =
            message (fun () ->
                EmulatedKernel.initial
                |> EmulatedKernel.withFileSystemAndCurrentDirectory
                    SimulatedUnixPlatform.macOsArm64
                    createdAt
                    seed
                    (absolute "/l")
                |> ignore<EmulatedKernel>
            )

        text |> shouldContainText "KernelConfig.FileSystem"

    [<Test>]
    let ``replacing the filesystem re-resolves the current directory`` () : unit =
        // Inode numbers are meaningless across graphs, so carrying the old one
        // over would leave the kernel holding whatever happened to land on that
        // number in the new seed -- or nothing at all.
        let deeper =
            Map.ofList
                [
                    name "pad", SeedEntry.directory (Map.ofList [ name "a", SeedEntry.directory Map.empty ])
                    name "outer", SeedEntry.directory (Map.ofList [ name "inner", SeedEntry.directory Map.empty ])
                ]

        let kernel = seededAt "/outer/inner"

        let replaced =
            kernel
            |> EmulatedKernel.withFileSystemAndCurrentDirectory
                SimulatedUnixPlatform.linuxX64
                createdAt
                deeper
                (absolute "/outer/inner")

        replaced.CurrentDirectoryInode |> shouldEqual (inodeOf replaced "/outer/inner")
        EmulatedKernel.checkInvariants replaced |> shouldEqual []

        // The seeds mint inodes in a different order, so the two graphs really
        // do disagree about which number `/outer/inner` is -- which is what
        // makes "it was re-resolved" distinguishable from "it was carried over".
        replaced.CurrentDirectoryInode |> shouldNotEqual kernel.CurrentDirectoryInode

    // -------------------------------------------------- what a host is told

    [<Test>]
    let ``a current directory the seed does not contain names both knobs`` () : unit =
        let text = message (fun () -> seededAt "/outer/nope" |> ignore<EmulatedKernel>)

        text |> shouldContainText "/outer/nope"
        text |> shouldContainText "KernelConfig.FileSystem"
        text |> shouldContainText "KernelConfig.CurrentDirectory"

    [<Test>]
    let ``a current directory that is a file is refused as such`` () : unit =
        // Distinguished from "does not resolve": a host pointing at a real file
        // has made a different mistake, and ENOTDIR-flavoured advice would send
        // it looking for a typo.
        let text = message (fun () -> seededAt "/outer/file" |> ignore<EmulatedKernel>)

        text |> shouldContainText "not to a directory"

    // ------------------------------------------------------------- invariants

    [<Test>]
    let ``checkInvariants rejects a held inode that is not a directory`` () : unit =
        let kernel = seededAt "/outer/inner"
        let file = inodeOf kernel "/outer/file"

        { kernel with
            Process =
                { kernel.Process with
                    CurrentDirectoryInode = file
                }
        }
        |> EmulatedKernel.checkInvariants
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.CurrentDirectoryIsNotADirectory file)
            ]

    [<Test>]
    let ``checkInvariants rejects a held inode the filesystem does not contain`` () : unit =
        let kernel = seededAt "/outer/inner"
        let absent = VirtualFileSystem.nextInode kernel.FileSystem

        { kernel with
            Process =
                { kernel.Process with
                    CurrentDirectoryInode = absent
                }
        }
        |> EmulatedKernel.checkInvariants
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.CurrentDirectoryIsNotADirectory absent)
            ]

    // ------------------------------------------------------------ through the config

    [<Test>]
    let ``KernelConfig applies the current directory whatever else it sets`` () : unit =
        let config =
            { KernelConfig.Default with
                FileSystem = seed
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
                CurrentDirectory = absolute "/outer/inner"
            }

        let kernel = KernelConfig.applyTo config EmulatedKernel.initial

        kernel.CurrentDirectoryInode |> shouldEqual (inodeOf kernel "/outer/inner")
        EmulatedKernel.checkInvariants kernel |> shouldEqual []
