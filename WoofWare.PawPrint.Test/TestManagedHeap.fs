namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestManagedHeap =
    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private state (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// The zero of the element type used by the hand-built allocations in this fixture.
    let private int32Zero : CliType = CliType.Numeric (CliNumericType.Int32 0)

    /// An `ArrayShape` whose element facts are derived from `elementZero`, so a fixture
    /// cannot violate the stride/zero agreement `allocateArray` enforces except on purpose.
    let private shapeOf
        (concreteType : ConcreteTypeHandle)
        (elementZero : CliType)
        (lengths : ImmutableArray<int>)
        : ArrayShape
        =
        {
            ConcreteType = concreteType
            Length = Seq.fold (*) 1 lengths
            Lengths = lengths
            ElementStride = CliType.sizeOf elementZero
            ElementZero = elementZero
        }

    /// A szarray shape of `length` `int32` cells.
    let private int32ShapeOf (concreteType : ConcreteTypeHandle) (length : int) : ArrayShape =
        shapeOf concreteType int32Zero (ImmutableArray.Create length)

    /// The zero of a fieldless value type of exactly `size` bytes — what
    /// `[StructLayout(LayoutKind.Sequential, Size = n)]` produces. Lets a test name a width
    /// corelib has no type for, so that each of `allocateArray`'s checks can be provoked
    /// without tripping one of the others first.
    let private sizedZero (size : int) : CliType =
        CliValueType.OfFields
            baseClassTypes
            concreteTypes
            (ConcreteTypeHandle.Concrete 1)
            (Layout.Custom (size, 1))
            CharSet.Ansi
            []
        |> CliType.ValueType

    [<Test>]
    let ``allocateArray preserves concrete array type for empty arrays`` () : unit =
        let intHandle = ConcreteTypeHandle.Concrete 1
        let stringHandle = ConcreteTypeHandle.Concrete 2
        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero intHandle
        let stringArrayHandle = ConcreteTypeHandle.OneDimArrayZero stringHandle

        let intArray : AllocatedArray =
            {
                Shape = int32ShapeOf intArrayHandle 0
                Elements = ImmutableArray.Empty
            }

        let stringArray : AllocatedArray =
            {
                Shape = shapeOf stringArrayHandle (CliType.ObjectRef None) (ImmutableArray.Create 0)
                Elements = ImmutableArray.Empty
            }

        let intArrayAddr, heap = ManagedHeap.allocateArray intArray ManagedHeap.empty
        let stringArrayAddr, heap = ManagedHeap.allocateArray stringArray heap

        (ManagedHeap.getArrayShape intArrayAddr heap).ConcreteType
        |> shouldEqual intArrayHandle

        (ManagedHeap.getArrayShape stringArrayAddr heap).ConcreteType
        |> shouldEqual stringArrayHandle

    [<Test>]
    let ``getObjectConcreteType returns concrete array type`` () : unit =
        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.OneDimArrayZero elementHandle

        let array : AllocatedArray =
            {
                Shape = int32ShapeOf arrayHandle 0
                Elements = ImmutableArray.Empty
            }

        let arrayAddr, heap = ManagedHeap.allocateArray array ManagedHeap.empty

        ManagedHeap.getObjectConcreteType arrayAddr heap |> shouldEqual arrayHandle

    [<Test>]
    let ``tryGetObjectConcreteType returns None for unknown address`` () : unit =
        ManagedHeap.tryGetObjectConcreteType (ManagedHeapAddress 1) ManagedHeap.empty
        |> shouldEqual None

    [<Test>]
    let ``recordStringContents then getStringContents round-trips`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 42

        ManagedHeap.empty
        |> ManagedHeap.recordStringContents addr "hello"
        |> ManagedHeap.getStringContents addr
        |> shouldEqual (Some "hello")

    [<Test>]
    let ``getStringContents returns None when no contents recorded`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 42
        ManagedHeap.getStringContents addr ManagedHeap.empty |> shouldEqual None

    [<Test>]
    let ``recordStringContents overwrites previous content`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 7

        ManagedHeap.empty
        |> ManagedHeap.recordStringContents addr "first"
        |> ManagedHeap.recordStringContents addr "second"
        |> ManagedHeap.getStringContents addr
        |> shouldEqual (Some "second")

    [<Test>]
    let ``getStringChar and setStringChar require recorded contents`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 42

        let dataOffset, heap = ManagedHeap.allocateString 1 ManagedHeap.empty
        let heap = ManagedHeap.recordStringDataOffset addr dataOffset heap

        Assert.Throws<System.Exception> (fun () -> ManagedHeap.getStringChar addr 0 heap |> ignore)
        |> ignore

        Assert.Throws<System.Exception> (fun () -> ManagedHeap.setStringChar addr 0 'x' heap |> ignore)
        |> ignore

    [<Test>]
    let ``setStringChar updates the canonical views`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let addr, state =
            IlMachineState.allocateManagedString loggerFactory baseClassTypes "ab" state

        let heap = ManagedHeap.setStringChar addr 0 'z' state.ManagedHeap

        // Char 0 is exposed both through the byte-level view (used by byrefs and
        // the synthetic `_firstChar` projection) and through the canonical
        // `StringContents` value (used by structural ops).
        ManagedHeap.getStringChar addr 0 heap |> shouldEqual 'z'
        ManagedHeap.getStringContents addr heap |> shouldEqual (Some "zb")

    [<Test>]
    let ``stringsEqual: same content at different addresses is equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> ManagedHeap.recordStringContents addr1 "hello"
            |> ManagedHeap.recordStringContents addr2 "hello"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual true

    [<Test>]
    let ``stringsEqual: same address is equal`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 1
        let heap = ManagedHeap.empty |> ManagedHeap.recordStringContents addr "hello"
        ManagedHeap.stringsEqual addr addr heap |> shouldEqual true

    [<Test>]
    let ``stringsEqual: different content of same length is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> ManagedHeap.recordStringContents addr1 "hello"
            |> ManagedHeap.recordStringContents addr2 "world"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: different length is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> ManagedHeap.recordStringContents addr1 "hello"
            |> ManagedHeap.recordStringContents addr2 "hell"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: empty strings are equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> ManagedHeap.recordStringContents addr1 ""
            |> ManagedHeap.recordStringContents addr2 ""

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual true

    [<Test>]
    let ``stringsEqual: shared prefix but one is longer is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> ManagedHeap.recordStringContents addr1 "hello world"
            |> ManagedHeap.recordStringContents addr2 "hello"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: differ only in last char is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> ManagedHeap.recordStringContents addr1 "abcdef"
            |> ManagedHeap.recordStringContents addr2 "abcdeg"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``allocateMultiDimArray: 2D zero-init has product Length and verbatim Lengths`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 2)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ 3 ; 4 ]

        let addr, state =
            IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state

        let array = state.ManagedHeap.Arrays.[addr]
        array.Shape.ConcreteType |> shouldEqual arrayHandle
        array.Shape.Length |> shouldEqual 12
        array.Shape.Lengths |> shouldEqual lengths
        array.Elements.Length |> shouldEqual 12

        for i = 0 to 11 do
            array.Elements.[i] |> shouldEqual zero

    [<Test>]
    let ``allocateMultiDimArray: zero dimension yields empty backing store`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 3)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ 5 ; 0 ; 7 ]

        let addr, state =
            IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state

        let array = state.ManagedHeap.Arrays.[addr]
        array.Shape.Length |> shouldEqual 0
        array.Shape.Lengths |> shouldEqual lengths
        array.Elements.Length |> shouldEqual 0

    [<Test>]
    let ``allocateMultiDimArray: rank-4 product overflow is detected before wrapping`` () : unit =
        // 65536^2 = 2^32 already overflows the UInt32 running product, so the guard
        // must fire at dimension 1 — before any silent wrap and well before we'd
        // attempt to allocate a backing store.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 4)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ 65536 ; 65536 ; 65536 ; 65536 ]

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state
                |> ignore
            )

        exn.Message |> shouldContainText "overflows UInt32"

    [<Test>]
    let ``allocateMultiDimArray: trailing zero rescues a prefix that fits in UInt32`` () : unit =
        // Per CoreCLR (vm/gchelpers.cpp): the running product is uint32, and 50000 *
        // 50000 = 2,500,000,000 fits in uint32 even though it exceeds Int32.MaxValue.
        // The trailing 0 then brings the product back to 0 and the array is allocated
        // empty — i.e. a transient prefix overshoot must NOT abort allocation.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 3)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ 50000 ; 50000 ; 0 ]

        let addr, state =
            IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state

        let array = state.ManagedHeap.Arrays.[addr]
        array.Shape.Length |> shouldEqual 0
        array.Shape.Lengths |> shouldEqual lengths
        array.Elements.Length |> shouldEqual 0

    [<Test>]
    let ``allocateMultiDimArray: prefix that overflows UInt32 still throws even if a later dim is zero`` () : unit =
        // 65536 * 65536 = 2^32 overflows UInt32 itself, so codex's "trailing zero rescues"
        // fix must NOT extend to the case where the multiply genuinely overflows. CoreCLR
        // throws OutOfMemoryException at the multiplication step here, regardless of the
        // final 0 dimension.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 4)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ 65536 ; 65536 ; 65536 ; 0 ]

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state
                |> ignore
            )

        exn.Message |> shouldContainText "overflows UInt32"

    [<Test>]
    let ``allocateMultiDimArray: final product exceeding Int32 is rejected even if it fits in UInt32`` () : unit =
        // Int32.MaxValue * 2 = UInt32.MaxValue - 1, which fits in UInt32 (the per-step
        // multiply check passes: Int32.MaxValue == UInt32.MaxValue / 2, not strictly
        // greater). But the final product exceeds Int32.MaxValue, so it can't index our
        // backing store; the post-loop guard must catch it.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 2)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ System.Int32.MaxValue ; 2 ]

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state
                |> ignore
            )

        exn.Message |> shouldContainText "exceeds Int32.MaxValue"

    [<Test>]
    let ``allocateMultiDimArray: negative length is rejected`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 2)
        let zero = CliType.Numeric (CliNumericType.Int32 0)
        let lengths = ImmutableArray.CreateRange [ 3 ; -1 ]

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.allocateMultiDimArray arrayHandle (fun () -> zero) lengths state
                |> ignore
            )

        exn.Message |> shouldContainText "negative length"

    // ---------------------------------------------------------------------
    // Sync blocks (object headers).
    //
    // In CoreCLR every heap object carries an `ObjHeader` immediately before
    // its payload, arrays and strings included (`src/coreclr/vm/object.h`);
    // there is no array-shaped carve-out. So the sync block belongs to the
    // *address*, not to the kind of payload stored there, and every live
    // address must have exactly one.
    // ---------------------------------------------------------------------

    let private stubArray (length : int) : AllocatedArray =
        {
            Shape = int32ShapeOf (ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 1)) length
            Elements = Seq.replicate length int32Zero |> ImmutableArray.CreateRange
        }

    /// A placeholder non-array object whose payload is never inspected by the
    /// sync-block machinery. Constructed as `Unchecked.defaultof<_>` because
    /// minting a real `CliValueType` needs `BaseClassTypes` plumbing that is
    /// irrelevant here; if the sync-block code ever starts reading the payload,
    /// these tests fail loudly with an NRE instead of silently passing.
    let private stubNonArray : AllocatedNonArrayObject =
        {
            Contents = Unchecked.defaultof<CliValueType>
            ConcreteType = ConcreteTypeHandle.Concrete 0
        }

    [<Test>]
    let ``a freshly allocated array has an empty sync block`` () : unit =
        let addr, heap = ManagedHeap.allocateArray (stubArray 3) ManagedHeap.empty

        ManagedHeap.getSyncBlock addr heap |> shouldEqual SyncBlock.Empty

    [<Test>]
    let ``setSyncBlock then getSyncBlock round-trips on an array`` () : unit =
        let addr, heap = ManagedHeap.allocateArray (stubArray 3) ManagedHeap.empty

        let held : SyncBlock =
            {
                Lock =
                    SyncBlockLock.Held
                        {
                            LockingThread = ThreadId 7
                            ReentrancyCount = 2
                            AcquireQueue = [ ThreadId 9, None ]
                        }
                WaitQueue = [ ThreadId 11, 1 ]
            }

        let heap = ManagedHeap.setSyncBlock addr held heap

        ManagedHeap.getSyncBlock addr heap |> shouldEqual held

    [<Test>]
    let ``locking one array does not lock another`` () : unit =
        let addr1, heap = ManagedHeap.allocateArray (stubArray 1) ManagedHeap.empty
        let addr2, heap = ManagedHeap.allocateArray (stubArray 1) heap
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray heap

        let held : SyncBlock =
            {
                Lock =
                    SyncBlockLock.Held
                        {
                            LockingThread = ThreadId 1
                            ReentrancyCount = 1
                            AcquireQueue = []
                        }
                WaitQueue = []
            }

        let heap = ManagedHeap.setSyncBlock addr1 held heap

        ManagedHeap.getSyncBlock addr1 heap |> shouldEqual held
        ManagedHeap.getSyncBlock addr2 heap |> shouldEqual SyncBlock.Empty
        ManagedHeap.getSyncBlock objAddr heap |> shouldEqual SyncBlock.Empty

    [<Test>]
    let ``getSyncBlock fails loudly for an address that was never allocated`` () : unit =
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.getSyncBlock (ManagedHeapAddress 42) ManagedHeap.empty |> ignore
            )

        exn.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``setSyncBlock fails loudly for an address that was never allocated`` () : unit =
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.setSyncBlock (ManagedHeapAddress 42) SyncBlock.Empty ManagedHeap.empty
                |> ignore
            )

        exn.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``cloneArray gives the copy a fresh sync block, not the source's lock state`` () : unit =
        // `Array.Clone` copies the elements, never the object header: the clone is a
        // brand-new object and cannot inherit the source's monitor ownership or its
        // queues. Getting this wrong would let `lock (source)` leak onto the clone.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.OneDimArrayZero elementHandle
        let zero = CliType.Numeric (CliNumericType.Int32 0)

        let source, state =
            IlMachineState.allocateArray arrayHandle (fun () -> zero) 2 state

        let held : SyncBlock =
            {
                Lock =
                    SyncBlockLock.Held
                        {
                            LockingThread = ThreadId 3
                            ReentrancyCount = 4
                            AcquireQueue = [ ThreadId 5, None ]
                        }
                WaitQueue = [ ThreadId 6, 2 ]
            }

        let state = IlMachineState.setSyncBlock source held state

        let clone, state = IlMachineState.cloneArray source state

        clone |> shouldNotEqual source
        IlMachineState.getSyncBlock source state |> shouldEqual held
        IlMachineState.getSyncBlock clone state |> shouldEqual SyncBlock.Empty

    [<Test>]
    let ``every live heap address has exactly one sync block`` () : unit =
        // The invariant that makes `getSyncBlock` total over live addresses: allocation
        // is the only way to mint an address, and every allocation path must register a
        // sync block for it. Anything that adds a new allocation kind without doing so
        // breaks this property rather than silently reintroducing "arrays can't be
        // locked".
        let property (ops : bool list) : bool =
            let heap =
                ops
                |> List.fold
                    (fun heap isArray ->
                        if isArray then
                            ManagedHeap.allocateArray (stubArray 1) heap |> snd
                        else
                            ManagedHeap.allocateNonArray stubNonArray heap |> snd
                    )
                    ManagedHeap.empty

            let live =
                Set.union (heap.NonArrayObjects |> Map.keys |> Set.ofSeq) (heap.Arrays |> Map.keys |> Set.ofSeq)

            let withSyncBlocks = heap.SyncBlocks |> Map.keys |> Set.ofSeq

            live = withSyncBlocks
            && live.Count = ops.Length
            && live
               |> Set.forall (fun addr -> ManagedHeap.getSyncBlock addr heap = SyncBlock.Empty)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property
        )

    // ---------------------------------------------------------------------
    // Array shape.
    //
    // `ArrayShape` is everything about an array *except* its contents: the
    // identity and dimensions fixed at allocation. It deliberately has no
    // `Elements`, so a caller that only wants the rank or the length cannot
    // reach a cell — reading a cell is a guest-visible memory access and has
    // to go through `getArrayValue`.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``getArrayShape reports the allocation's dimensions and type`` () : unit =
        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.Array (elementHandle, 2)
        let lengths = ImmutableArray.CreateRange [ 3 ; 4 ]

        let allocation : AllocatedArray =
            {
                Shape = shapeOf arrayHandle int32Zero lengths
                Elements = Seq.replicate 12 int32Zero |> ImmutableArray.CreateRange
            }

        let addr, heap = ManagedHeap.allocateArray allocation ManagedHeap.empty
        let shape = ManagedHeap.getArrayShape addr heap

        shape.ConcreteType |> shouldEqual arrayHandle
        shape.Length |> shouldEqual 12
        shape.Lengths |> shouldEqual lengths

    [<Test>]
    let ``getArrayShape fails loudly, and distinguishably, for a non-array object`` () : unit =
        // Same discrimination as `setFieldById`, for the same reason: a non-array address
        // means the caller misjudged the type of the reference it holds, whereas an
        // unallocated address means the reference itself is bogus.
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray ManagedHeap.empty

        let exn =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.getArrayShape objAddr heap |> ignore)

        exn.Message |> shouldContainText "is not an array"

    [<Test>]
    let ``getArrayShape fails loudly for an address that was never allocated`` () : unit =
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.getArrayShape (ManagedHeapAddress 42) ManagedHeap.empty |> ignore
            )

        exn.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``tryGetArrayShape and isArray answer only for arrays`` () : unit =
        let arrAddr, heap = ManagedHeap.allocateArray (stubArray 3) ManagedHeap.empty
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray heap
        let danglingAddr = ManagedHeapAddress 99

        ManagedHeap.isArray arrAddr heap |> shouldEqual true
        ManagedHeap.isArray objAddr heap |> shouldEqual false
        ManagedHeap.isArray danglingAddr heap |> shouldEqual false

        (ManagedHeap.tryGetArrayShape arrAddr heap).IsSome |> shouldEqual true
        ManagedHeap.tryGetArrayShape objAddr heap |> shouldEqual None
        ManagedHeap.tryGetArrayShape danglingAddr heap |> shouldEqual None

    [<Test>]
    let ``array shape is exactly the allocation minus its elements`` () : unit =
        // The oracle is the allocation record itself: `getArrayShape` must project it
        // field-for-field, never derive or normalise. `Length` in particular is stored,
        // not recomputed from `Lengths`, and the projection must not start recomputing it.
        //
        // This used to allocate `Length = total` with an *empty* backing store, so that a
        // projection deriving `Length` from `Elements.Length` would be caught. That
        // discrimination has moved: `allocateArray` now rejects a length that disagrees with
        // the cell count outright (see the test below), because `getArrayValue` bounds-checks
        // against the shape and then indexes the cells, which is only sound while the two
        // agree. Distinguishing stored from derived is no longer possible here — and no
        // longer meaningful, since the allocator makes them provably equal.
        //
        // Bounding the rank to 6 dimensions of at most 4 keeps the product well inside Int32
        // and the materialised array small, so no seed can make this test fail for reasons
        // unrelated to the property.
        let property (lengths : int list) : bool =
            let lengths = lengths |> List.truncate 6 |> List.map (fun n -> (abs (n % 5)))

            let total = lengths |> List.fold (*) 1

            let allocation : AllocatedArray =
                {
                    Shape =
                        shapeOf
                            (ConcreteTypeHandle.Array (ConcreteTypeHandle.Concrete 1, lengths.Length))
                            int32Zero
                            (ImmutableArray.CreateRange lengths)
                    Elements = Seq.replicate total int32Zero |> ImmutableArray.CreateRange
                }

            let addr, heap = ManagedHeap.allocateArray allocation ManagedHeap.empty
            let shape = ManagedHeap.getArrayShape addr heap

            shape.ConcreteType = allocation.Shape.ConcreteType
            && shape.Length = allocation.Shape.Length
            && shape.Lengths = allocation.Shape.Lengths
            && shape.ElementStride = allocation.Shape.ElementStride
            && shape.ElementZero = allocation.Shape.ElementZero

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property
        )

    // ---------------------------------------------------------------------
    // Field stores.
    //
    // `setFieldById` is the one read-modify-write primitive for storing to a
    // field of a non-array object. It exists so that every field store is a
    // single identifiable event on the heap: `stfld` and delegate
    // construction used to rebuild `NonArrayObjects` inline instead, which
    // left three separately-maintained copies of "overwrite one field" and
    // two of them invisible to anything watching the heap API.
    // ---------------------------------------------------------------------

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Object

    let private fieldA : FieldId = FieldId.named "A"
    let private fieldB : FieldId = FieldId.named "B"

    /// An object with two Int32 fields, `A` and `B`, holding the given values.
    let private allocateTwoFieldObject
        (a : int)
        (b : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let field (id : FieldId) (name : string) (offset : int) (value : int) : CliField =
            {
                Id = id
                Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 value)
                Offset = Some offset
                Type = int32Handle
                MarshallingDescriptor = None
            }

        let contents =
            [ field fieldA "A" 0 a ; field fieldB "B" 4 b ]
            |> CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                objectHandle
                (Layout.Custom (size = 8, packingSize = 0))
                CharSet.Ansi

        IlMachineState.allocateManagedObject objectHandle contents state

    let private readInt32Field (id : FieldId) (addr : ManagedHeapAddress) (heap : ManagedHeap) : int =
        match ManagedHeap.get addr heap |> AllocatedNonArrayObject.DereferenceFieldById id with
        | CliType.Numeric (CliNumericType.Int32 v) -> v
        | other -> failwith $"expected field %O{id} of %O{addr} to hold an Int32, got %O{other}"

    [<Test>]
    let ``setFieldById overwrites the named field and leaves its siblings alone`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let addr, state = allocateTwoFieldObject 11 22 state

        let heap =
            ManagedHeap.setFieldById addr fieldA (CliType.Numeric (CliNumericType.Int32 99)) state.ManagedHeap

        readInt32Field fieldA addr heap |> shouldEqual 99
        readInt32Field fieldB addr heap |> shouldEqual 22

    [<Test>]
    let ``setFieldById leaves other heap objects untouched`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let target, state = allocateTwoFieldObject 1 2 state
        let bystander, state = allocateTwoFieldObject 3 4 state

        let heap =
            ManagedHeap.setFieldById target fieldA (CliType.Numeric (CliNumericType.Int32 42)) state.ManagedHeap

        readInt32Field fieldA target heap |> shouldEqual 42
        readInt32Field fieldA bystander heap |> shouldEqual 3
        readInt32Field fieldB bystander heap |> shouldEqual 4
        // The store must not disturb the object header either.
        ManagedHeap.getSyncBlock bystander heap |> shouldEqual SyncBlock.Empty

    [<Test>]
    let ``setFieldById fails loudly for an address that was never allocated`` () : unit =
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.setFieldById
                    (ManagedHeapAddress 42)
                    fieldA
                    (CliType.Numeric (CliNumericType.Int32 0))
                    ManagedHeap.empty
                |> ignore
            )

        exn.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``setFieldById fails loudly, and distinguishably, for an array address`` () : unit =
        // An array address reaching a field store means the caller misjudged the type of
        // the reference it was handed; a dangling address means the reference itself is
        // bogus. Those are different bugs, so they must not report the same way.
        let addr, heap = ManagedHeap.allocateArray (stubArray 3) ManagedHeap.empty

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.setFieldById addr fieldA (CliType.Numeric (CliNumericType.Int32 0)) heap
                |> ignore
            )

        exn.Message |> shouldContainText "is an array"

    [<Test>]
    let ``set fails loudly for an address that was never allocated`` () : unit =
        // `set` replaces the payload of an existing object; conjuring one at an
        // unallocated address would mint a live object with no header, breaking the
        // invariant that `getSyncBlock` is total over live addresses.
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.set (ManagedHeapAddress 42) stubNonArray ManagedHeap.empty |> ignore
            )

        exn.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``setFieldById agrees with the open-coded read-modify-write it replaces`` () : unit =
        // The oracle is precisely the code `stfld` and `executeDelegateConstructor` ran
        // before this primitive existed. Any divergence means the refactor changed
        // behaviour rather than merely relocating it.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let initialState = state loggerFactory

        let property (writes : (bool * int) list) : bool =
            let addr, state = allocateTwoFieldObject 0 0 initialState

            let viaPrimitive =
                writes
                |> List.fold
                    (fun heap (toA, value) ->
                        let field = if toA then fieldA else fieldB
                        ManagedHeap.setFieldById addr field (CliType.Numeric (CliNumericType.Int32 value)) heap
                    )
                    state.ManagedHeap

            let viaOracle =
                writes
                |> List.fold
                    (fun (heap : ManagedHeap) (toA, value) ->
                        let field = if toA then fieldA else fieldB

                        let updated =
                            ManagedHeap.get addr heap
                            |> AllocatedNonArrayObject.SetFieldById
                                field
                                (CliType.Numeric (CliNumericType.Int32 value))

                        ManagedHeap.set addr updated heap
                    )
                    state.ManagedHeap

            readInt32Field fieldA addr viaPrimitive = readInt32Field fieldA addr viaOracle
            && readInt32Field fieldB addr viaPrimitive = readInt32Field fieldB addr viaOracle

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property
        )

    // ---------------------------------------------------------------------
    // Element stride.
    //
    // The stride is the one part of `ArrayShape` that duplicates something
    // also recoverable from the cells, so it is the one part that could
    // drift. Callers read it precisely so that they never touch guest memory
    // to learn the stride — which is worthless if the recorded number can be
    // wrong. So it is pinned twice over: against an oracle outside PawPrint
    // entirely (the host CLR's own element sizes), and by the check
    // `allocateArray` runs against cell 0 of every non-empty allocation.
    // ---------------------------------------------------------------------

    /// Allocate a `len`-element array of `elementHandle` and report its recorded stride.
    let private strideOfArray
        (elementHandle : ConcreteTypeHandle)
        (len : int)
        (state : IlMachineState)
        : int * IlMachineState
        =
        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementHandle

        let addr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero elementHandle) (fun () -> zero) len state

        ManagedHeap.getArrayElementStride addr state.ManagedHeap, state

    /// Element types paired with the stride the *host* CLR gives an array of them. These
    /// are facts about .NET, established without consulting any PawPrint code, so they can
    /// witness a wrong answer rather than merely a self-consistent one.
    let private strideOracle
        : (string * (BaseClassTypes<DumpedAssembly> -> TypeInfo<GenericParamFromMetadata, TypeDefn>) * int) list =
        [
            "System.Boolean", (fun bct -> bct.Boolean), sizeof<bool>
            "System.Byte", (fun bct -> bct.Byte), sizeof<byte>
            "System.SByte", (fun bct -> bct.SByte), sizeof<sbyte>
            "System.Char", (fun bct -> bct.Char), sizeof<char>
            "System.Int16", (fun bct -> bct.Int16), sizeof<int16>
            "System.UInt16", (fun bct -> bct.UInt16), sizeof<uint16>
            "System.Int32", (fun bct -> bct.Int32), sizeof<int32>
            "System.UInt32", (fun bct -> bct.UInt32), sizeof<uint32>
            "System.Int64", (fun bct -> bct.Int64), sizeof<int64>
            "System.UInt64", (fun bct -> bct.UInt64), sizeof<uint64>
            "System.Single", (fun bct -> bct.Single), sizeof<single>
            "System.Double", (fun bct -> bct.Double), sizeof<double>
            // A reference-typed element is a pointer-width slot, whatever it points at.
            "System.Object", (fun bct -> bct.Object), System.IntPtr.Size
            "System.String", (fun bct -> bct.String), System.IntPtr.Size
        ]

    [<Test>]
    let ``recorded element stride is the host CLR's element size`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let mutable state = state loggerFactory

        for name, selector, expected in strideOracle do
            let handle =
                AllConcreteTypes.getRequiredNonGenericHandle concreteTypes (selector baseClassTypes)

            let stride, newState = strideOfArray handle 3 state
            state <- newState

            if stride <> expected then
                failwith $"array of %s{name} recorded element stride %d{stride}, but .NET lays it out at %d{expected}"

    [<Test>]
    let ``an empty array records the same stride as a populated one`` () : unit =
        // The whole reason the stride is recorded rather than measured: `Array.Empty<T>()`
        // has no cell to measure, and used to leave callers with no answer at all. Every
        // element type is checked, not just one, because a fallback that happened to return
        // a plausible constant would pass a single-type test.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let mutable state = state loggerFactory

        for name, selector, _ in strideOracle do
            let handle =
                AllConcreteTypes.getRequiredNonGenericHandle concreteTypes (selector baseClassTypes)

            let populated, s1 = strideOfArray handle 3 state
            let empty, s2 = strideOfArray handle 0 s1
            state <- s2

            if populated <> empty then
                failwith
                    $"empty array of %s{name} records element stride %d{empty}, but a populated one records %d{populated}"

    [<Test>]
    let ``allocateArray rejects a stride that disagrees with the cells`` () : unit =
        // The check that makes reading the stride, rather than measuring a cell, safe.
        // Without it the field is an honour-system claim.
        //
        // The element zero is widened to match the bogus stride, so that the zero-versus-
        // stride check below passes and this one is what actually fires. Each of
        // `allocateArray`'s guards has to be provokable on its own, or the later ones are
        // untested and could be deleted without any test noticing.
        let wrong : AllocatedArray =
            { stubArray 3 with
                Shape =
                    { (stubArray 3).Shape with
                        ElementStride = sizeof<int32> + 1
                        ElementZero = sizedZero (sizeof<int32> + 1)
                    }
            }

        let exn =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.allocateArray wrong ManagedHeap.empty |> ignore)

        exn.Message |> shouldContainText "but its first cell measures"

    [<Test>]
    let ``allocateArray rejects a stride that disagrees with the element zero`` () : unit =
        // The stride is *defined* as the size of the element zero, and is stored separately
        // only because recomputing it walks a value type's whole field tree on every
        // byte-view access. That makes the two a denormalisation, and this is what keeps
        // them from drifting. Checked on an empty array, where the cell comparison above has
        // nothing to say and this is the only thing standing between the two fields.
        let wrong : AllocatedArray =
            { stubArray 0 with
                Shape =
                    { (stubArray 0).Shape with
                        ElementStride = sizeof<int32> + 1
                    }
            }

        let exn =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.allocateArray wrong ManagedHeap.empty |> ignore)

        exn.Message |> shouldContainText "but its element zero"

    [<Test>]
    let ``allocateArray rejects a length that disagrees with the cell count`` () : unit =
        // `getArrayValue` bounds-checks the index against `Shape.Length` and then indexes
        // `Elements`. Splitting the shape out of the payload record is what made it possible
        // for those to be different numbers; without this check the two would silently
        // disagree and an in-bounds-by-the-shape index would come back as a raw
        // `IndexOutOfRangeException` from beneath the interpreter.
        let tooFew : AllocatedArray =
            { stubArray 3 with
                Elements = Seq.replicate 2 int32Zero |> ImmutableArray.CreateRange
            }

        let exn =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.allocateArray tooFew ManagedHeap.empty |> ignore)

        exn.Message |> shouldContainText "carries 2 cell(s)"

    [<Test>]
    let ``allocateArray rejects a length that disagrees with the per-dimension lengths`` () : unit =
        // `Length` is documented as the product of `Lengths`, and the multi-dimensional
        // accessors depend on it: array `Get`/`Set`/`Address` bounds-check each index against
        // `Lengths` and then index by the flattened offset, which `getArrayValue` checks
        // against `Length`. A disagreement lets an index that every per-dimension check
        // accepts fall outside the cells.
        let wrong : AllocatedArray =
            {
                Shape =
                    { int32ShapeOf (ConcreteTypeHandle.Array (ConcreteTypeHandle.Concrete 1, 2)) 6 with
                        Lengths = ImmutableArray.CreateRange [ 2 ; 4 ]
                    }
                Elements = Seq.replicate 6 int32Zero |> ImmutableArray.CreateRange
            }

        let exn =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.allocateArray wrong ManagedHeap.empty |> ignore)

        exn.Message |> shouldContainText "multiply to 8"

    [<Test>]
    let ``allocateArray rejects a non-positive stride`` () : unit =
        // The element-zero check subsumes this one — it forces the stride to equal a
        // `CliType.sizeOf`, which is never below 1 — so this fires only because it runs
        // first. It is kept as a direct assertion of what consumers depend on: `floorDivRem`
        // divides by the stride, and that shouldn't rest on a two-step argument through a
        // different check in a different file.
        //
        // Zero is rejected as well as negative, and is the more dangerous of the two: it is
        // the plausible-looking value for "an array with nothing in it", and it fails
        // *quietly* — every index would map to byte offset zero, and a byte-offset walk
        // could never advance a cell. No CLI type is zero-sized (a fieldless struct is
        // padded to one byte, per CoreCLR and `CliValueType.SizeOfFieldStorage`), so no
        // real allocation can want it.
        let rejects (stride : int) : unit =
            let wrong : AllocatedArray =
                { stubArray 0 with
                    Shape =
                        { (stubArray 0).Shape with
                            ElementStride = stride
                        }
                }

            let exn =
                Assert.Throws<System.Exception> (fun () -> ManagedHeap.allocateArray wrong ManagedHeap.empty |> ignore)

            exn.Message |> shouldContainText "non-positive element stride"

        rejects 0
        rejects -4

    [<Test>]
    let ``cloneArray carries the source's stride`` () : unit =
        // `cloneArray` reuses the source `AllocatedArray` verbatim, so this holds by
        // construction today; it is pinned because a clone that rebuilt the shape without
        // the stride would produce an array whose every byte-view access is silently
        // misaligned rather than failing.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let handle =
            AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int64

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

        let source, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero handle) (fun () -> zero) 2 state

        let clone, state = IlMachineState.cloneArray source state

        ManagedHeap.getArrayElementStride clone state.ManagedHeap
        |> shouldEqual (ManagedHeap.getArrayElementStride source state.ManagedHeap)

        ManagedHeap.getArrayElementStride clone state.ManagedHeap
        |> shouldEqual sizeof<int64>

    // ---------------------------------------------------------------------
    // Element zero.
    //
    // The witness for "what shape is a cell of this array". Cell 0 used to
    // serve, which was wrong three ways: it is a guest-visible read performed
    // to answer a question about a type, it does not exist for an empty
    // array, and it is only a *sample* — a store to cell 5 was validated
    // against whatever provenance cell 0 had picked up.
    // ---------------------------------------------------------------------

    /// Allocate a `len`-element array of `elementHandle` and report its recorded element
    /// zero.
    let private elementZeroOfArray
        (elementHandle : ConcreteTypeHandle)
        (len : int)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes elementHandle

        let addr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero elementHandle) (fun () -> zero) len state

        ManagedHeap.getArrayElementZero addr state.ManagedHeap, state

    [<Test>]
    let ``recorded element zero is the element type's zero, for an empty array too`` () : unit =
        // The oracle is `cliTypeZeroOfHandle` — the same thing every cell of a fresh array
        // is initialised to — asked independently of the allocation. An empty array must
        // give the same answer as a populated one, which is the whole reason this is
        // recorded rather than sampled off cell 0.
        let _, loggerFactory = LoggerFactory.makeTest ()

        let mutable state = state loggerFactory

        for name, selector, _ in strideOracle do
            let handle =
                AllConcreteTypes.getRequiredNonGenericHandle concreteTypes (selector baseClassTypes)

            let expected, s0 = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle
            let populated, s1 = elementZeroOfArray handle 3 s0
            let empty, s2 = elementZeroOfArray handle 0 s1
            state <- s2

            if populated <> expected then
                failwith $"array of %s{name} recorded element zero %O{populated}, but the type's zero is %O{expected}"

            if empty <> populated then
                failwith
                    $"empty array of %s{name} records element zero %O{empty}, but a populated one records %O{populated}"

    [<Test>]
    let ``a stored cell may drift from the element zero, and does not change it`` () : unit =
        // Why the element zero cannot simply be read back off cell 0, stated as a property
        // of the heap rather than as a comment: a cell legitimately holds something other
        // than the element type's zero the moment anything is written to it, while the
        // element zero is a fact about the type and never moves.
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let handle =
            AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

        let addr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero handle) (fun () -> zero) 3 state

        let state =
            IlMachineState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 7)) 0 state

        ManagedHeap.getArrayValue addr 0 state.ManagedHeap
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 7))

        ManagedHeap.getArrayElementZero addr state.ManagedHeap |> shouldEqual zero

    [<Test>]
    let ``getArrayElementZero fails loudly for a non-array and for a dangling address`` () : unit =
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray ManagedHeap.empty

        let notAnArray =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.getArrayElementZero objAddr heap |> ignore)

        notAnArray.Message |> shouldContainText "is not an array"

        let dangling =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.getArrayElementZero (ManagedHeapAddress 42) heap |> ignore
            )

        dangling.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``cloneArray distinguishes a non-array from a dangling address`` () : unit =
        // Same discrimination as `getArrayShape` / `get`, for the same reason: a non-array
        // address means the caller misjudged the kind of the reference it holds, whereas an
        // unallocated address means the reference itself is bogus. Before `cloneArray` moved
        // into `ManagedHeap` it reached into `Arrays` directly from the state layer.
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray ManagedHeap.empty

        let notAnArray =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.cloneArray objAddr heap |> ignore)

        notAnArray.Message |> shouldContainText "is a non-array object"

        let dangling =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.cloneArray (ManagedHeapAddress 42) heap |> ignore)

        dangling.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``getArrayElementStride fails loudly for a non-array and for a dangling address`` () : unit =
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray ManagedHeap.empty

        let notAnArray =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.getArrayElementStride objAddr heap |> ignore)

        notAnArray.Message |> shouldContainText "is not an array"

        let dangling =
            Assert.Throws<System.Exception> (fun () ->
                ManagedHeap.getArrayElementStride (ManagedHeapAddress 42) heap |> ignore
            )

        dangling.Message |> shouldContainText "not a live managed heap allocation"

    // ---------------------------------------------------------------------
    // Non-array reads.
    //
    // `tryGet` / `get` / `isLive` are the read half of the non-array seam,
    // the counterparts of `tryGetArrayShape` / `getArrayShape` / `isArray`.
    // Before them, seventeen sites indexed `NonArrayObjects` directly, so a
    // read of a guest object was not an identifiable event and the
    // "array" / "never allocated" distinction was open-coded where it was
    // made at all.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``tryGet answers only for non-array objects`` () : unit =
        // Deliberately `None` for a live array rather than throwing: an array has no
        // `AllocatedNonArrayObject` payload at all, so "not here" is the honest answer, and
        // callers that care which it is have `isArray` next door.
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray ManagedHeap.empty
        let arrAddr, heap = ManagedHeap.allocateArray (stubArray 3) heap

        (ManagedHeap.tryGet objAddr heap).IsSome |> shouldEqual true
        ManagedHeap.tryGet arrAddr heap |> shouldEqual None
        ManagedHeap.tryGet (ManagedHeapAddress 99) heap |> shouldEqual None

    [<Test>]
    let ``get fails loudly, and distinguishably, for an array and for a dangling address`` () : unit =
        // Same discrimination as `getArrayShape`, and for the same reason: an array address
        // means the caller misjudged the kind of reference it holds, whereas an unallocated
        // address means the reference itself is bogus. This used to be a bare
        // `KeyNotFoundException` from indexing the map, which distinguished neither.
        let arrAddr, heap = ManagedHeap.allocateArray (stubArray 1) ManagedHeap.empty

        let isArray =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.get arrAddr heap |> ignore)

        isArray.Message |> shouldContainText "is an array"

        let dangling =
            Assert.Throws<System.Exception> (fun () -> ManagedHeap.get (ManagedHeapAddress 99) heap |> ignore)

        dangling.Message |> shouldContainText "not a live managed heap allocation"

    [<Test>]
    let ``isLive covers both payload kinds, and nothing else`` () : unit =
        let objAddr, heap = ManagedHeap.allocateNonArray stubNonArray ManagedHeap.empty
        let arrAddr, heap = ManagedHeap.allocateArray (stubArray 2) heap

        ManagedHeap.isLive objAddr heap |> shouldEqual true
        ManagedHeap.isLive arrAddr heap |> shouldEqual true
        ManagedHeap.isLive (ManagedHeapAddress 99) heap |> shouldEqual false

    [<Test>]
    let ``isLive agrees with the object-header key set`` () : unit =
        // An independent oracle for `isLive`, which is computed from the two *payload* maps.
        // `SyncBlocks`' documented invariant is that its key set is exactly their union, so
        // the header table answers the same question by a different route. Keeping the two
        // routes separate is deliberate: had `isLive` been implemented on `SyncBlocks`, this
        // would be a tautology instead of a check, and a broken invariant would show up as a
        // wrong liveness answer rather than a failing test.
        let property (ops : bool list) : bool =
            let ops = ops |> List.truncate 12

            let heap =
                ops
                |> List.fold
                    (fun heap isArray ->
                        if isArray then
                            ManagedHeap.allocateArray (stubArray 1) heap |> snd
                        else
                            ManagedHeap.allocateNonArray stubNonArray heap |> snd
                    )
                    ManagedHeap.empty

            // Probe past the end too, so a always-true implementation fails.
            [ 1 .. ops.Length + 3 ]
            |> List.forall (fun addr ->
                let addr = ManagedHeapAddress addr
                ManagedHeap.isLive addr heap = heap.SyncBlocks.ContainsKey addr
            )

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary) property
        )
