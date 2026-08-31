namespace WoofWare.PawPrint.Test

open FsCheck
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// How an open directory stream is named.
///
/// A guest holds a `DIR*`, which here is the address of the native block whose
/// bytes are also the `d_name` buffer. The kernel holds a stream under a minted
/// `DirectoryStreamId`, and `DirectoryStreamBlocks` is the mapping between the
/// two. Splitting them is what lets the stream table move to a library that has
/// never heard of a native block; the price is that two maps must agree, and
/// these are the rows that hold them to it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryStreamId =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList [ name "dir", SeedEntry.directory Map.empty ]

    let private kernel () : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.withFileSystemAndCurrentDirectory SimulatedUnixPlatform.linuxX64 createdAt seed (absolute "/")

    let private dirInode (kernel : EmulatedKernel) : InodeNumber =
        match
            VirtualFileSystem.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.UnixPlatform)
                CallerPrivilege.Privileged
                (VirtualFileSystem.root kernel.FileSystem)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" "/dir")
                kernel.FileSystem
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve /dir in the test seed: %O{error}"

    /// Everything `SystemNative_OpenDir` does to kernel state: the library mints
    /// the stream and takes the descriptor, and the client allocates the native
    /// block standing in for the `DIR*` and binds it to that identity.
    ///
    /// Two steps rather than one, which is the split these rows exist to hold:
    /// the identity is the library's and the address is PawPrint's.
    let private openDir (kernel : EmulatedKernel) : NativeMemoryBlockId * EmulatedKernel =
        let id, system =
            match UnixNamespace.opendir (UnixPath.parseOrFail "test" "/dir") (EmulatedKernel.unix kernel) with
            | OpenDirAnswer.Opened id, system -> id, system
            | other -> failwith $"could not open the directory: %O{other}"

        let kernel = EmulatedKernel.withUnix system kernel

        let block, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized 1024 kernel.NativeMemoryPool

        block,
        { kernel with
            NativeMemoryPool = pool
        }
        |> EmulatedKernel.withDirectoryStreamBlock block id

    let private closeDir (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : EmulatedKernel =
        let stream = EmulatedKernel.directoryStream block kernel
        let kernel = EmulatedKernel.withoutDirectoryStream block kernel

        match KernelSyscall.close stream.Fd kernel with
        | Ok kernel -> kernel
        | Error error -> failwith $"could not close the stream's descriptor: %O{error}"

    [<Test>]
    let ``a fresh kernel holds no streams`` () : unit =
        let kernel = kernel ()

        kernel.DirectoryStreams |> shouldBeEmpty
        kernel.DirectoryStreamBlocks |> shouldBeEmpty
        kernel.NextDirectoryStreamId |> shouldEqual (DirectoryStreamId 0L)

    [<Test>]
    let ``opening binds the block to a minted id and advances the counter`` () : unit =
        let block, kernel = kernel () |> openDir

        EmulatedKernel.directoryStreamId block kernel
        |> shouldEqual (DirectoryStreamId 0L)

        kernel.NextDirectoryStreamId |> shouldEqual (DirectoryStreamId 1L)

        (EmulatedKernel.directoryStream block kernel).Cursor
        |> shouldEqual DirectoryCursor.Start

    [<Test>]
    let ``two streams of one directory get distinct ids`` () : unit =
        // Two `opendir`s of the same directory advance independently on a real
        // libc, which is only expressible if they are two streams rather than one.
        let kernel = kernel ()
        let first, kernel = openDir kernel
        let second, kernel = openDir kernel

        first |> shouldNotEqual second

        EmulatedKernel.directoryStreamId first kernel
        |> shouldNotEqual (EmulatedKernel.directoryStreamId second kernel)

        kernel.DirectoryStreams.Count |> shouldEqual 2

    [<Test>]
    let ``advancing a cursor does not mint a second id`` () : unit =
        // The mistake this rules out: `readdir` going through the open path and
        // leaving the old stream in the table under its own id, unreachable and
        // pinning its directory for the rest of the run.
        let block, kernel = kernel () |> openDir
        let before = EmulatedKernel.directoryStreamId block kernel

        let kernel =
            EmulatedKernel.mapUnix (fun system -> snd (UnixNamespace.readdir before system)) kernel

        EmulatedKernel.directoryStreamId block kernel |> shouldEqual before
        kernel.NextDirectoryStreamId |> shouldEqual (DirectoryStreamId 1L)
        kernel.DirectoryStreams.Count |> shouldEqual 1

        // `dir` binds no names, so the first entry is `..` and the cursor lands
        // there. Asserted as a value rather than as "it moved": a cursor that
        // advanced to the wrong state would pass the weaker check.
        (EmulatedKernel.directoryStream block kernel).Cursor
        |> shouldEqual DirectoryCursor.ReturnedDotDot

    [<Test>]
    let ``closing clears both maps`` () : unit =
        let block, kernel = kernel () |> openDir
        let kernel = closeDir block kernel

        kernel.DirectoryStreams |> shouldBeEmpty
        kernel.DirectoryStreamBlocks |> shouldBeEmpty

    [<Test>]
    let ``ids are not reused after a close`` () : unit =
        let kernel = kernel ()
        let first, kernel = openDir kernel
        let kernel = closeDir first kernel
        let second, kernel = openDir kernel

        EmulatedKernel.directoryStreamId second kernel
        |> shouldEqual (DirectoryStreamId 1L)

    [<Test>]
    let ``closing one stream leaves the other readable`` () : unit =
        let kernel = kernel ()
        let first, kernel = openDir kernel
        let second, kernel = openDir kernel
        let survivor = EmulatedKernel.directoryStreamId second kernel
        let kernel = closeDir first kernel

        EmulatedKernel.directoryStreamId second kernel |> shouldEqual survivor
        kernel.DirectoryStreams.Count |> shouldEqual 1

    [<Test>]
    let ``a DIR* this kernel never issued is refused loudly`` () : unit =
        let kernel = kernel ()

        let block, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized 8 kernel.NativeMemoryPool

        let kernel =
            { kernel with
                NativeMemoryPool = pool
            }

        let exn =
            Assert.Throws<exn> (fun () -> EmulatedKernel.directoryStreamId block kernel |> ignore<DirectoryStreamId>)

        exn.Message |> shouldContainText "names no open directory stream"

    [<Test>]
    let ``directoryStream refuses a DIR* whose stream has been closed`` () : unit =
        // The other provenance, and the other function. Above, a block this
        // kernel never bound, asked through `directoryStreamId`; here one it
        // bound and then released, asked through `directoryStream`. Both are
        // undefined behaviour on a real libc rather than an error it reports,
        // so there is no errno to invent -- but a lookup that answered a stale
        // stream for either would be worse than a crash.
        let block, kernel = openDir (kernel ())
        let released = EmulatedKernel.withoutDirectoryStream block kernel

        let exn =
            Assert.Throws (fun () -> EmulatedKernel.directoryStream block released |> ignore<DirectoryStream>)

        exn.Message |> shouldContainText "names no open directory stream"

    [<Test>]
    let ``checkInvariants accepts an open stream`` () : unit =
        let _, kernel = kernel () |> openDir

        EmulatedKernel.checkInvariants kernel |> shouldBeEmpty

    [<Test>]
    let ``checkInvariants catches a counter that did not advance`` () : unit =
        let block, kernel = kernel () |> openDir
        let id = EmulatedKernel.directoryStreamId block kernel

        let kernel =
            { kernel with
                Process =
                    { kernel.Process with
                        NextDirectoryStreamId = DirectoryStreamId 0L
                    }
            }

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.System (UnixSystemDefect.NextDirectoryStreamIdNotFresh (DirectoryStreamId 0L, id))
            ]

    [<Test>]
    let ``checkInvariants catches a DIR* whose stream is gone`` () : unit =
        let block, kernel = kernel () |> openDir
        let id = EmulatedKernel.directoryStreamId block kernel

        // Only the stream table forgotten: the shape a `withoutDirectoryStream`
        // that dropped one map and not the other would leave.
        let kernel =
            { kernel with
                Process =
                    { kernel.Process with
                        DirectoryStreams = Map.remove id kernel.DirectoryStreams
                    }
            }

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual [ EmulatedKernelDefect.DirectoryStreamBlockDangling (block, id) ]

    [<Test>]
    let ``checkInvariants catches a stream no DIR* names`` () : unit =
        let block, kernel = kernel () |> openDir
        let id = EmulatedKernel.directoryStreamId block kernel

        let kernel =
            { kernel with
                DirectoryStreamBlocks = Map.remove block kernel.DirectoryStreamBlocks
            }

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual [ EmulatedKernelDefect.UnreachableDirectoryStream id ]

    [<Test>]
    let ``checkInvariants catches two DIR*s naming one stream`` () : unit =
        // Not reachable through `withNewDirectoryStream`, which always mints —
        // but the mapping has to be injective for `withoutDirectoryStream` to be
        // able to do its job. Closing either block would remove the one stream
        // and leave the other `DIR*` naming nothing, and two `opendir`s owe the
        // guest independent cursors in any case.
        let kernel = kernel ()
        let first, kernel = openDir kernel
        let id = EmulatedKernel.directoryStreamId first kernel

        let second, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized 1024 kernel.NativeMemoryPool

        let kernel =
            { kernel with
                NativeMemoryPool = pool
                DirectoryStreamBlocks = Map.add second id kernel.DirectoryStreamBlocks
            }

        EmulatedKernel.checkInvariants kernel
        |> shouldEqual
            [
                EmulatedKernelDefect.DirectoryStreamNamedTwice (id, [ first ; second ] |> List.sort)
            ]

    [<Test>]
    let ``the two maps agree under any sequence of opens and closes`` () : unit =
        // The invariant the rekey introduces, driven rather than asserted at one
        // state: `false` opens a stream, `true` closes the oldest one still open.
        let property (operations : bool list) : unit =
            let mutable kernel = kernel ()
            let mutable live : NativeMemoryBlockId list = []

            for close in operations do
                match close, live with
                | true, block :: rest ->
                    kernel <- closeDir block kernel
                    live <- rest
                | true, [] -> ()
                | false, _ ->
                    let block, next = openDir kernel
                    kernel <- next
                    live <- live @ [ block ]

            EmulatedKernel.checkInvariants kernel |> shouldBeEmpty
            kernel.DirectoryStreams.Count |> shouldEqual live.Length
            kernel.DirectoryStreamBlocks.Count |> shouldEqual live.Length

            for block in live do
                EmulatedKernel.directoryStream block kernel |> ignore<DirectoryStream>

        Check.One (config, property)
