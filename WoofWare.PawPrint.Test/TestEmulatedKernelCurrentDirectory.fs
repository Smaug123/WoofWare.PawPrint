namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `EmulatedKernel.withFileSystemAndCurrentDirectory`, which is the wrapper
/// around `UnixSystem.withFileSystemAndCurrentDirectory`: that it threads its
/// arguments through rather than reading the kernel's own fields, and what a
/// host that misconfigures the two knobs is told.
///
/// The messages are the reason these rows are here rather than in the library.
/// The library answers a `CurrentDirectoryFault`, deliberately saying nothing
/// about `KernelConfig`, which it cannot see; turning each case into advice
/// naming the field the host actually set is this wrapper's whole job, and
/// `TestWithFileSystemAndCurrentDirectory` covers everything underneath it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmulatedKernelCurrentDirectory =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private target (s : string) : SymlinkTarget = SymlinkTarget.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    /// `outer/inner/`, plus `outer/file` and a link `outer/lnk -> inner`, so a
    /// row can ask for a current directory that is a file, that is reached
    /// through a symlink, and that does not exist at all.
    let private seed : Map<DirectoryEntryName, SeedEntry> =
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

    /// A kernel seeded with the tree above, whose current directory is `dir`.
    let private seededAt (dir : string) : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.withFileSystemAndCurrentDirectory SimulatedUnixPlatform.linuxX64 createdAt seed (absolute dir)

    let private message (body : unit -> unit) : string =
        let thrown = Assert.Throws<exn> (fun () -> body ())
        thrown.Message

    // ------------------------------------------------ what a fresh kernel holds

    [<Test>]
    let ``a freshly minted kernel holds the root`` () : unit =
        EmulatedKernel.currentDirectoryPath EmulatedKernel.initial
        |> shouldEqual (Some AbsoluteUnixPath.root)

        EmulatedKernel.initial.CurrentDirectoryInode
        |> shouldEqual (VirtualFileSystem.root EmulatedKernel.initial.FileSystem)

        EmulatedKernel.checkInvariants EmulatedKernel.initial |> shouldEqual []

    // ------------------------------- the arguments, not the kernel's own fields

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
        // its platform -- so a wrapper reading `kernel.UnixPlatform` instead of
        // passing its argument down would have refused the name above. That is
        // what makes the pair setter order-independent with respect to
        // `withUnixPlatformAndFileSystemType`. The library function's own half
        // of this claim is in `TestWithFileSystemAndCurrentDirectory`; what is
        // tested here is that the wrapper hands the argument over.
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

    // -------------------------------------------------- what a host is told

    [<Test>]
    let ``The kernel rejects a forged current directory at configuration time`` () : unit =
        // The boundary that matters: without this, a defaulted value would sail
        // into kernel state and fail as a null reference inside the first
        // SystemNative_GetCwd instead of naming the knob.
        let text =
            message (fun () ->
                EmulatedKernel.initial
                |> EmulatedKernel.withFileSystemAndCurrentDirectory
                    SimulatedUnixPlatform.linuxX64
                    (UnixTimestamp.ofSeconds 0L)
                    FileSystemSeed.empty
                    Unchecked.defaultof<AbsoluteUnixPath>
                |> ignore<EmulatedKernel>
            )

        text |> shouldContainText "EmulatedKernel.CurrentDirectory"

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
