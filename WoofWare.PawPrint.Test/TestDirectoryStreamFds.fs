namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// How a guest's `DIR*` is named.
///
/// A guest holds a `DIR*`, which here is the address of the native block whose
/// bytes are also the `d_name` buffer. The kernel knows nothing of streams: a
/// `DIR*` reads through a descriptor onto a directory, whose open file
/// description holds the position. `DirectoryStreamFds` is the mapping from the
/// one to the other. These rows hold `DirectoryStream`, which is all the
/// `SystemNative_OpenDir` and `SystemNative_CloseDir` handlers do to the
/// kernel, to what the shim's `opendir`, `readdir` and `closedir` do with it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryStreamFds =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList
            [
                name "dir", SeedEntry.directory Map.empty
                name "file", SeedEntry.file ImmutableArray.Empty
            ]

    let private kernel () : EmulatedKernel =
        EmulatedKernel.initialImage
        |> EmulatedKernel.withFileSystemAndCurrentDirectory
            createdAt
            (InodeOwner.ofProcess (UnixSystem.defaultCredentials SimulatedUnixFlavour.Linux))
            seed
            (absolute "/")
        |> EmulatedKernel.boot

    let private tryOpenDir (path : string) (kernel : EmulatedKernel) : DirectoryStreamOpen * EmulatedKernel =
        DirectoryStream.opendir
            (UnixSystem.leader kernel.System)
            (PathArgumentBytes.Bytes (UnixPath.toByteString (UnixPath.parseOrFail "test" path)))
            kernel

    let private openDir (kernel : EmulatedKernel) : NativeMemoryBlockId * EmulatedKernel =
        match tryOpenDir "/dir" kernel with
        | DirectoryStreamOpen.Opened block, kernel -> block, kernel
        | DirectoryStreamOpen.Failed error, _ -> failwith $"could not open the directory: %O{error}"

    let private closeDir (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : EmulatedKernel =
        match DirectoryStream.closedir block kernel with
        | Ok (), kernel -> kernel
        | Error error, _ -> failwith $"could not close the stream: %O{error}"

    /// The number `open(2)` would hand out next.
    let private lowestFree (kernel : EmulatedKernel) : int =
        let live = FileDescriptorRegistry.fds (UnixSystem.fileDescriptors kernel.System)
        Seq.initInfinite id |> Seq.find (fun fd -> not (Map.containsKey fd live))

    /// Where the stream `block` names is positioned, which is its descriptor's
    /// open file description's position.
    let private positionOf (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : DirectoryPosition =
        let fd = EmulatedKernel.directoryStreamFd block kernel

        match FileDescriptorRegistry.tryFindTarget fd (UnixSystem.fileDescriptors kernel.System) with
        | Some (OpenFileTarget.Directory (_, position)) -> position
        | other -> failwith $"the stream's fd %d{fd} names %O{other}, not a directory"

    [<Test>]
    let ``a fresh kernel names no streams`` () : unit =
        (kernel ()).DirectoryStreamFds |> shouldBeEmpty

    [<Test>]
    let ``opening binds the block to a directory descriptor at its start`` () : unit =
        let block, kernel = kernel () |> openDir

        positionOf block kernel
        |> shouldEqual (DirectoryPosition.Cursor DirectoryCursor.Start)

    [<Test>]
    let ``two streams of one directory read through distinct descriptions`` () : unit =
        // Two `opendir`s of the same directory advance independently on a real
        // libc, because each opens its own description.
        let kernel = kernel ()
        let first, kernel = openDir kernel
        let second, kernel = openDir kernel

        first |> shouldNotEqual second

        let firstFd = EmulatedKernel.directoryStreamFd first kernel
        let secondFd = EmulatedKernel.directoryStreamFd second kernel
        firstFd |> shouldNotEqual secondFd

        let kernel =
            EmulatedKernel.mapUnix
                (fun system ->
                    match UnixNamespace.readDirectoryEntry firstFd system with
                    | _, system -> system
                )
                kernel

        positionOf first kernel
        |> shouldEqual (DirectoryPosition.Cursor DirectoryCursor.ReturnedDotDot)

        positionOf second kernel
        |> shouldEqual (DirectoryPosition.Cursor DirectoryCursor.Start)

    [<Test>]
    let ``opening allocates the block a name fits in`` () : unit =
        let block, kernel = kernel () |> openDir

        NativeMemoryPool.blockSize block kernel.NativeMemoryPool
        |> shouldEqual DirectoryStream.nameBufferBytes

    [<Test>]
    let ``a failed open leaves no stream and no block behind`` () : unit =
        let kernel = kernel ()

        for path, expected in [ "/file", UnixError.ENOTDIR ; "/nx", UnixError.ENOENT ] do
            match tryOpenDir path kernel with
            | DirectoryStreamOpen.Failed error, after ->
                error |> shouldEqual expected
                after.DirectoryStreamFds |> shouldBeEmpty

                NativeMemoryPool.liveBlockCount after.NativeMemoryPool
                |> shouldEqual (NativeMemoryPool.liveBlockCount kernel.NativeMemoryPool)

                lowestFree after |> shouldEqual (lowestFree kernel)
            | DirectoryStreamOpen.Opened block, _ -> failwith $"opening %s{path} gave stream %O{block}"

    [<Test>]
    let ``closing forgets the block, frees it and closes its descriptor`` () : unit =
        let block, kernel = kernel () |> openDir
        let fd = EmulatedKernel.directoryStreamFd block kernel
        let kernel = closeDir block kernel

        kernel.DirectoryStreamFds |> shouldBeEmpty
        NativeMemoryPool.isLive block kernel.NativeMemoryPool |> shouldEqual false

        // `fcntl(F_GETFL)` of a closed descriptor is EBADF.
        UnixDescriptor.isNonBlocking fd kernel.System |> shouldEqual None

    [<Test>]
    let ``closing a stream whose descriptor is already closed answers EBADF and still releases it`` () : unit =
        // `closedir` frees the `DIR` whatever `close` answers, so the stream is
        // gone either way.
        let block, kernel = kernel () |> openDir
        let fd = EmulatedKernel.directoryStreamFd block kernel

        let kernel =
            match KernelSyscall.close fd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"%O{error}"

        match DirectoryStream.closedir block kernel with
        | Error error, kernel ->
            error |> shouldEqual UnixError.EBADF
            kernel.DirectoryStreamFds |> shouldBeEmpty
            NativeMemoryPool.isLive block kernel.NativeMemoryPool |> shouldEqual false
        | Ok (), _ -> failwith "closing a stream whose descriptor was already closed succeeded"

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
            Assert.Throws<exn> (fun () -> EmulatedKernel.directoryStreamFd block kernel |> ignore<int>)

        exn.Message |> shouldContainText "names no open directory stream"

    [<Test>]
    let ``a DIR* whose stream has been closed is refused loudly`` () : unit =
        // Undefined behaviour on a real libc rather than an error it reports,
        // so there is no errno to invent; a lookup that answered the stale
        // descriptor would read whatever that number names by now.
        let block, kernel = openDir (kernel ())
        let released = EmulatedKernel.withoutDirectoryStream block kernel

        let exn =
            Assert.Throws<exn> (fun () -> EmulatedKernel.directoryStreamFd block released |> ignore<int>)

        exn.Message |> shouldContainText "names no open directory stream"

        let exn =
            Assert.Throws<exn> (fun () ->
                EmulatedKernel.withoutDirectoryStream block released |> ignore<EmulatedKernel>
            )

        exn.Message |> shouldContainText "names no open directory stream"

    [<Test>]
    let ``two DIR*s naming one descriptor number is a state a guest can reach`` () : unit =
        // A guest that closes a stream's descriptor behind its back and then
        // opens another directory gets that number back, so two `DIR*`s name
        // one descriptor. Undefined behaviour on a real libc, but not a defect
        // in this kernel's bookkeeping.
        let kernel = kernel ()
        let first, kernel = openDir kernel
        let fd = EmulatedKernel.directoryStreamFd first kernel

        let kernel =
            match KernelSyscall.close fd kernel with
            | Ok kernel -> kernel
            | Error error -> failwith $"%O{error}"

        let second, kernel = openDir kernel

        EmulatedKernel.directoryStreamFd second kernel |> shouldEqual fd
        EmulatedKernel.checkInvariants kernel |> shouldBeEmpty

    [<Test>]
    let ``opening takes the lowest free descriptor, holes included`` () : unit =
        // `opendir(3)` is an `open(2)`, so its descriptor is the lowest number
        // not in use -- including one below the top that an earlier close left.
        // A close names a live stream by its index modulo the count, so closes
        // land anywhere in the table rather than only at its top.
        let property (operations : int option list) : unit =
            let mutable kernel = kernel ()
            let mutable live : NativeMemoryBlockId list = []

            for operation in operations do
                match operation, live with
                | Some index, _ :: _ ->
                    let block = live.[abs (index % live.Length)]
                    kernel <- closeDir block kernel
                    live <- live |> List.filter (fun other -> other <> block)
                | Some _, [] -> ()
                | None, _ ->
                    let expected = lowestFree kernel
                    let block, next = openDir kernel
                    EmulatedKernel.directoryStreamFd block next |> shouldEqual expected
                    kernel <- next
                    live <- live @ [ block ]

        Check.One (config, property)

    [<Test>]
    let ``the map follows any sequence of opens and closes`` () : unit =
        // `false` opens a stream, `true` closes the oldest one still open.
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
            kernel.DirectoryStreamFds.Count |> shouldEqual live.Length

            // Every live stream reads through its own open directory description.
            live
            |> List.map (fun block -> EmulatedKernel.directoryStreamFd block kernel)
            |> List.distinct
            |> List.length
            |> shouldEqual live.Length

            for block in live do
                positionOf block kernel
                |> shouldEqual (DirectoryPosition.Cursor DirectoryCursor.Start)

        Check.One (config, property)
