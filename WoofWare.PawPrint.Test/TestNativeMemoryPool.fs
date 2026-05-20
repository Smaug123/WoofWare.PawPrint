namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeMemoryPool =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private allocateZeroInitialized
        (byteCount : int)
        (pool : NativeMemoryPool)
        : NativeMemoryBlockId * NativeMemoryPool
        =
        NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized byteCount pool

    let private allocateUninitialized
        (byteCount : int)
        (pool : NativeMemoryPool)
        : NativeMemoryBlockId * NativeMemoryPool
        =
        NativeMemoryPool.allocate MemoryBlockInitialization.Uninitialized byteCount pool

    [<Test>]
    let ``Allocated block ids are unique`` () : unit =
        let block1, pool = allocateZeroInitialized 0 NativeMemoryPool.empty
        let block2, pool = allocateZeroInitialized 0 pool
        let block3, _ = allocateZeroInitialized 0 pool

        (block1 = block2) |> shouldEqual false
        (block1 = block3) |> shouldEqual false
        (block2 = block3) |> shouldEqual false

    [<Test>]
    let ``Allocated memory is zero initialised`` () : unit =
        let property (NonNegativeInt blockLength) : unit =
            let blockLength = blockLength % 129
            let block, pool = allocateZeroInitialized blockLength NativeMemoryPool.empty
            let actual = NativeMemoryPool.readBytes block 0 blockLength pool
            actual |> shouldEqual (Array.zeroCreate<byte> blockLength)

        Check.One (config, property)

    [<Test>]
    let ``Round-trip read of a freshly written byte range returns what was written`` () : unit =
        let property (NonNegativeInt rawOffset) (bytes : byte[]) : unit =
            // Avoid the empty-block edge case so we actually exercise read/write.
            let bytes =
                if isNull bytes || bytes.Length = 0 then
                    [| 0uy |]
                else
                    bytes

            let blockLength = bytes.Length + (rawOffset % 32)
            let offset = blockLength - bytes.Length
            let block, pool = allocateZeroInitialized blockLength NativeMemoryPool.empty
            let pool = NativeMemoryPool.writeBytes block offset bytes pool

            NativeMemoryPool.readBytes block offset bytes.Length pool |> shouldEqual bytes

        Check.One (config, property)

    [<Test>]
    let ``Blocks in the same pool are isolated`` () : unit =
        let block1, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let block2, pool = allocateZeroInitialized 4 pool

        let pool = NativeMemoryPool.writeBytes block1 1 [| 1uy ; 2uy |] pool

        NativeMemoryPool.readBytes block1 0 4 pool
        |> shouldEqual [| 0uy ; 1uy ; 2uy ; 0uy |]

        NativeMemoryPool.readBytes block2 0 4 pool
        |> shouldEqual [| 0uy ; 0uy ; 0uy ; 0uy |]

    [<Test>]
    let ``free removes the block from the pool`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        NativeMemoryPool.isLive block pool |> shouldEqual true

        let pool = NativeMemoryPool.free block pool
        NativeMemoryPool.isLive block pool |> shouldEqual false

    [<Test>]
    let ``Use-after-free is caught on read`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let pool = NativeMemoryPool.free block pool

        let exn =
            Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.readBytes block 0 1 pool |> ignore)

        exn.Message |> shouldContainText "Use-after-free"

    [<Test>]
    let ``Use-after-free is caught on write`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let pool = NativeMemoryPool.free block pool

        let exn =
            Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.writeBytes block 0 [| 1uy |] pool |> ignore)

        exn.Message |> shouldContainText "Use-after-free"

    [<Test>]
    let ``Use-after-free is caught on tryReadCell`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let pool = NativeMemoryPool.free block pool

        let exn =
            Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.tryReadCell block 0 pool |> ignore)

        exn.Message |> shouldContainText "Use-after-free"

    [<Test>]
    let ``Use-after-free is caught on writeCell`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let pool = NativeMemoryPool.free block pool

        let cell = CliType.Numeric (CliNumericType.Int32 0x11223344)

        let exn =
            Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.writeCell block 0 cell pool |> ignore)

        exn.Message |> shouldContainText "Use-after-free"

    [<Test>]
    let ``Freeing a never-allocated handle is rejected`` () : unit =
        let bogus = NativeMemoryBlockId.NativeMemoryBlockId 99

        Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.free bogus NativeMemoryPool.empty |> ignore)
        |> ignore

    [<Test>]
    let ``Double free is rejected`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let pool = NativeMemoryPool.free block pool

        Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.free block pool |> ignore)
        |> ignore

    [<Test>]
    let ``Freeing one block leaves siblings live`` () : unit =
        let block1, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let block2, pool = allocateZeroInitialized 4 pool

        let pool =
            NativeMemoryPool.writeBytes block2 0 [| 0xAAuy ; 0xBBuy ; 0xCCuy ; 0xDDuy |] pool

        let pool = NativeMemoryPool.free block1 pool

        NativeMemoryPool.isLive block1 pool |> shouldEqual false
        NativeMemoryPool.isLive block2 pool |> shouldEqual true

        NativeMemoryPool.readBytes block2 0 4 pool
        |> shouldEqual [| 0xAAuy ; 0xBBuy ; 0xCCuy ; 0xDDuy |]

    [<Test>]
    let ``Re-allocation after free yields a fresh handle`` () : unit =
        let block1, pool = allocateZeroInitialized 4 NativeMemoryPool.empty
        let pool = NativeMemoryPool.free block1 pool
        let block2, _ = allocateZeroInitialized 4 pool

        // Deterministic monotonic IDs: a freed handle is never recycled.
        (block1 = block2) |> shouldEqual false

    [<Test>]
    let ``Uninitialised memory cannot be read before write`` () : unit =
        let block, pool = allocateUninitialized 4 NativeMemoryPool.empty

        Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.readBytes block 0 4 pool |> ignore)
        |> ignore

    [<Test>]
    let ``Negative size allocation is rejected`` () : unit =
        Assert.Throws<System.Exception> (fun () -> allocateZeroInitialized -1 NativeMemoryPool.empty |> ignore)
        |> ignore

    [<Test>]
    let ``Out-of-range reads and writes fail visibly`` () : unit =
        let block, pool = allocateZeroInitialized 4 NativeMemoryPool.empty

        Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.readBytes block -1 1 pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.readBytes block 4 1 pool |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> NativeMemoryPool.writeBytes block 3 [| 1uy ; 2uy |] pool |> ignore)
        |> ignore
