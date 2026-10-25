namespace WoofWare.PawPrint.Test

open System.IO
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeAddressSpace =

    type private Op =
        | Allocate of byteCount : int * alignment : int
        | Free of index : int

    let private genOp : Gen<Op> =
        let genAllocate =
            gen {
                let! byteCount =
                    Gen.frequency [ 1, Gen.constant 0 ; 6, Gen.choose (1, 256) ; 1, Gen.choose (257, 1 <<< 20) ]

                let! alignmentLog2 = Gen.choose (3, 16)
                return Op.Allocate (byteCount, 1 <<< alignmentLog2)
            }

        Gen.frequency [ 3, genAllocate ; 2, Gen.choose (0, 1000) |> Gen.map Op.Free ]

    /// A block the reference model holds live: what it asked for, and the base it was first
    /// given, which it must keep.
    type private LiveBlock =
        {
            Id : NativeMemoryBlockId
            ByteCount : int
            Alignment : int
            Base : uint64
        }

    let private baseOf (block : NativeMemoryBlockId) (pool : NativeMemoryPool) : uint64 =
        match NativeMemoryPool.tryAddressOf block 0L pool with
        | Some address -> address
        | None -> failwith $"live %O{block} has no address"

    /// Every promise `NativeAddressSpace` makes, checked by the test's own arithmetic rather
    /// than through the module's.
    let private checkPromises (live : LiveBlock list) (freed : NativeMemoryBlockId list) (pool : NativeMemoryPool) =
        NativeMemoryPool.checkAddressInvariants pool

        for block in live do
            baseOf block.Id pool |> shouldEqual block.Base
            block.Base % uint64 block.Alignment |> shouldEqual 0UL
            (block.Base >= NativeAddressSpace.lowest) |> shouldEqual true

            (block.Base + uint64 (max block.ByteCount 1) <= NativeAddressSpace.limit)
            |> shouldEqual true

            NativeMemoryPool.tryAddressOf block.Id (int64 block.ByteCount) pool
            |> shouldEqual (Some (block.Base + uint64 block.ByteCount))

            NativeMemoryPool.tryAddressOf block.Id (int64 block.ByteCount + 1L) pool
            |> shouldEqual None

            NativeMemoryPool.tryAddressOf block.Id -1L pool |> shouldEqual None

            // The alignment-mask model's claim about a pointer into this block holds of
            // where the block really is.
            let claimed =
                ManagedPointerSource.Byref
                    {
                        Root = ByrefRoot.NativeMemoryByte (block.Id, 0)
                        Projections = []
                    }
                |> ManagedPointerSource.tryContainerBase

            match claimed with
            | Some containerBase -> block.Base % (1UL <<< containerBase.AlignmentBits) |> shouldEqual 0UL
            | None -> failwith "tryContainerBase makes no claim about a native-heap block"

        live
        |> List.sortBy _.Base
        |> List.pairwise
        |> List.iter (fun (lower, upper) ->
            if lower.Base + uint64 (max lower.ByteCount 1) > upper.Base then
                failwith
                    $"%O{lower.Id} at 0x%x{lower.Base} (%d{lower.ByteCount} bytes) overlaps %O{upper.Id} at 0x%x{upper.Base}"
        )

        for block in freed do
            NativeMemoryPool.tryAddressOf block 0L pool |> shouldEqual None

    [<Test>]
    let ``Live blocks are aligned, in bounds, disjoint, and keep their base until freed`` () : unit =
        let property (ops : Op list) : unit =
            let step (live : LiveBlock list, freed : NativeMemoryBlockId list, pool : NativeMemoryPool) (op : Op) =
                let live, freed, pool =
                    match op with
                    | Op.Allocate (byteCount, alignment) ->
                        let block, pool =
                            NativeMemoryPool.allocate
                                MemoryBlockInitialization.ZeroInitialized
                                (NativeBlockAlignment.ofBytes alignment)
                                byteCount
                                pool

                        let live =
                            {
                                Id = block
                                ByteCount = byteCount
                                Alignment = alignment
                                Base = baseOf block pool
                            }
                            :: live

                        live, freed, pool
                    | Op.Free _ when List.isEmpty live -> live, freed, pool
                    | Op.Free index ->
                        let victim = live.[index % live.Length]
                        let live = live |> List.filter (fun block -> block.Id <> victim.Id)
                        live, victim.Id :: freed, NativeMemoryPool.free victim.Id pool

                checkPromises live freed pool
                live, freed, pool

            List.fold step ([], [], NativeMemoryPool.empty) ops |> ignore

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen (Gen.listOf genOp)) property)

    [<Test>]
    let ``Every block lies in user space on every machine the kernel models`` () : unit =
        let checks =
            [
                UnixSystem.defaultUserBufferCheck SimulatedUnixPlatform.linuxX64
                UnixSystem.defaultUserBufferCheck SimulatedUnixPlatform.linuxArm64
                UnixSystem.defaultUserBufferCheck SimulatedUnixPlatform.macOsArm64
                UserBufferCheck.BeforeOperation ObservedUserAddressLimit.X64FourLevelPaging
                UserBufferCheck.BeforeOperation ObservedUserAddressLimit.Arm64FortyEightBit
                UserBufferCheck.BeforeOperation ObservedUserAddressLimit.X64FiveLevelPaging
            ]

        for check in checks do
            UserBufferCheck.faultsBeforeOperation
                check
                NativeAddressSpace.lowest
                (NativeAddressSpace.limit - NativeAddressSpace.lowest)
            |> shouldEqual false

    /// Pins today's strategy, which nothing outside `NativeAddressSpace` may rely on: delete
    /// this test when the strategy changes.
    [<Test>]
    let ``Ascending placement puts each block at the next aligned address above the last`` () : unit =
        let first, pool =
            NativeMemoryPool.allocate
                MemoryBlockInitialization.ZeroInitialized
                NativeBlockAlignment.malloc
                1
                NativeMemoryPool.empty

        let second, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized (NativeBlockAlignment.ofBytes 64) 0 pool

        let third, pool =
            NativeMemoryPool.allocate MemoryBlockInitialization.ZeroInitialized NativeBlockAlignment.malloc 0 pool

        baseOf first pool |> shouldEqual NativeAddressSpace.lowest
        baseOf second pool |> shouldEqual (NativeAddressSpace.lowest + 64UL)
        baseOf third pool |> shouldEqual (NativeAddressSpace.lowest + 80UL)

    [<Test>]
    let ``Live blocks larger in total than the address range are refused loudly`` () : unit =
        let largest = System.Int32.MaxValue

        // One more of the largest block than the range can hold, laid end to end.
        let count =
            int ((NativeAddressSpace.limit - NativeAddressSpace.lowest) / uint64 largest)
            + 1

        let exn =
            Assert.Throws<exn> (fun () ->
                (NativeMemoryPool.empty, [ 1..count ])
                ||> List.fold (fun pool _ ->
                    NativeMemoryPool.allocate
                        MemoryBlockInitialization.Uninitialized
                        NativeBlockAlignment.minimum
                        largest
                        pool
                    |> snd
                )
                |> ignore
            )

        exn.Message |> shouldContainText "address range is exhausted"

    [<Test>]
    let ``Alignment must be a power of two no less than a pointer`` () : unit =
        for bytes in [ 8 ; 16 ; 1 <<< 30 ] do
            (NativeBlockAlignment.ofBytes bytes).Bytes |> shouldEqual bytes

        for bytes in [ 0 ; -1 ; 1 ; 2 ; 4 ; 3 ; 24 ; System.Int32.MinValue ] do
            Assert.Throws<exn> (fun () -> NativeBlockAlignment.ofBytes bytes |> ignore)
            |> ignore

    [<Test>]
    let ``A byte's address is its block's base plus its offset, within the block and one past it`` () : unit =
        let property (offset : int64) : unit =
            let block, pool =
                NativeMemoryPool.allocate
                    MemoryBlockInitialization.ZeroInitialized
                    NativeBlockAlignment.malloc
                    128
                    NativeMemoryPool.empty

            let expected =
                if 0L <= offset && offset <= 128L then
                    Some (baseOf block pool + uint64 offset)
                else
                    None

            NativeMemoryPool.tryAddressOf block offset pool |> shouldEqual expected

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.choose (-64, 192) |> Gen.map int64)) property
        )

    /// Two 4096-aligned blocks of 12288 bytes, each after a 16-byte `malloc` block. CoreLib
    /// rounds an aligned allocation's size up to a multiple of its alignment, and 12288 is the
    /// size no other allocation of this guest's has. The `malloc` blocks are there so that two
    /// blocks placed at `malloc`'s alignment cannot both happen to be 4096-byte aligned: each
    /// would be 16 bytes past a `malloc` block, and the two would be 16 bytes apart modulo
    /// 4096.
    let private alignedAllocGuest : string =
        """
using System.Runtime.InteropServices;

public static unsafe class Program
{
    public static int Main()
    {
        // Leaked, so that the test can find them in the final state.
        NativeMemory.Alloc(16);
        NativeMemory.AlignedAlloc(12288, 4096);
        NativeMemory.Alloc(16);
        NativeMemory.AlignedAlloc(12288, 4096);
        return 0;
    }
}
"""

    [<Test>]
    let ``NativeMemory.AlignedAlloc places its block at the alignment asked for`` () : unit =
        let name = "AlignedAllocPlacement.cs"
        let image = Roslyn.compile [ alignedAllocGuest ]
        let _messages, loggerFactory = LoggerFactory.makeTestWithProperties [ "case", name ]
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let state =
            match
                BoundedRun.run
                    loggerFactory
                    name
                    None
                    peImage
                    (HostConfig.Default (FrameworkUnderTest.runtimeDirsFor KernelConfig.Default.UnixPlatform))
                |> ExpectRun.ended
            with
            | RunOutcome.NormalExit (state, _, _) -> state
            | other -> failwith $"expected the guest to exit normally, got %O{other}"

        let pool = IlMachineState.getNativeMemoryPool state

        // Block ids are handed out from 0 and never reused, so every block the run allocated
        // has an id below the number of allocations, which a guest this small keeps well
        // under this bound.
        let bases =
            [ 0..99_999 ]
            |> List.map NativeMemoryBlockId
            |> List.filter (fun block ->
                NativeMemoryPool.isLive block pool
                && NativeMemoryPool.blockSize block pool = 12288
            )
            |> List.map (fun block -> baseOf block pool)

        bases.Length |> shouldEqual 2

        for blockBase in bases do
            blockBase % 4096UL |> shouldEqual 0UL
