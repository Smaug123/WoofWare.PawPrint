namespace WoofWare.PawPrint.Test

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
/// one to the other, and these rows hold it to what the shim's `opendir`,
/// `readdir` and `closedir` do with it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryStreamFds =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private createdAt : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 0

    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList [ name "dir", SeedEntry.directory Map.empty ]

    let private kernel () : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.withFileSystemAndCurrentDirectory createdAt seed (absolute "/")

    /// `opendir(3)`'s open.
    let private directoryFlags : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = true
            Synchronous = false
            Directory = true
        }

    /// Everything `SystemNative_OpenDir` does to kernel state: the library opens
    /// the directory, and the client allocates the native block standing in for
    /// the `DIR*` and binds it to the descriptor.
    let private openDir (kernel : EmulatedKernel) : NativeMemoryBlockId * EmulatedKernel =
        let fd, system =
            match
                UnixNamespace.openPath
                    directoryFlags
                    (UnixPath.parseOrFail "test" "/dir")
                    0
                    (EmulatedKernel.unix kernel)
            with
            | SyscallAnswer.Completed fd, system -> int fd, system
            | other -> failwith $"could not open the directory: %O{other}"

        let kernel = EmulatedKernel.withUnix system kernel

        let block, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized 1024 kernel.NativeMemoryPool

        block,
        { kernel with
            NativeMemoryPool = pool
        }
        |> EmulatedKernel.withDirectoryStreamFd block fd

    let private closeDir (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : EmulatedKernel =
        let fd = EmulatedKernel.directoryStreamFd block kernel
        let kernel = EmulatedKernel.withoutDirectoryStream block kernel

        match KernelSyscall.close fd kernel with
        | Ok kernel -> kernel
        | Error error -> failwith $"could not close the stream's descriptor: %O{error}"

    /// Where the stream `block` names is positioned, which is its descriptor's
    /// open file description's position.
    let private positionOf (block : NativeMemoryBlockId) (kernel : EmulatedKernel) : DirectoryPosition =
        let fd = EmulatedKernel.directoryStreamFd block kernel

        match FileDescriptorRegistry.tryFindTarget fd kernel.FileDescriptors with
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
                    | Ok (_, system) -> system
                    | Error refusal -> failwith $"%A{refusal}"
                )
                kernel

        positionOf first kernel
        |> shouldEqual (DirectoryPosition.Cursor DirectoryCursor.ReturnedDotDot)

        positionOf second kernel
        |> shouldEqual (DirectoryPosition.Cursor DirectoryCursor.Start)

    [<Test>]
    let ``closing forgets the block and closes its descriptor`` () : unit =
        let block, kernel = kernel () |> openDir
        let fd = EmulatedKernel.directoryStreamFd block kernel
        let kernel = closeDir block kernel

        kernel.DirectoryStreamFds |> shouldBeEmpty

        FileDescriptorRegistry.tryFindTarget fd kernel.FileDescriptors
        |> shouldEqual None

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
