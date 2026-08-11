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

    [<Test>]
    let ``allocateArray preserves concrete array type for empty arrays`` () : unit =
        let intHandle = ConcreteTypeHandle.Concrete 1
        let stringHandle = ConcreteTypeHandle.Concrete 2
        let intArrayHandle = ConcreteTypeHandle.OneDimArrayZero intHandle
        let stringArrayHandle = ConcreteTypeHandle.OneDimArrayZero stringHandle

        let intArray : AllocatedArray =
            {
                Shape =
                    {
                        ConcreteType = intArrayHandle
                        Length = 0
                        Lengths = ImmutableArray.Create 0
                    }
                Elements = ImmutableArray.Empty
            }

        let stringArray : AllocatedArray =
            {
                Shape =
                    {
                        ConcreteType = stringArrayHandle
                        Length = 0
                        Lengths = ImmutableArray.Create 0
                    }
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
                Shape =
                    {
                        ConcreteType = arrayHandle
                        Length = 0
                        Lengths = ImmutableArray.Create 0
                    }
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
            Shape =
                {
                    ConcreteType = ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 1)
                    Length = length
                    Lengths = ImmutableArray.Create length
                }
            Elements =
                Seq.replicate length (CliType.Numeric (CliNumericType.Int32 0))
                |> ImmutableArray.CreateRange
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
                Shape =
                    {
                        ConcreteType = arrayHandle
                        Length = 12
                        Lengths = lengths
                    }
                Elements =
                    Seq.replicate 12 (CliType.Numeric (CliNumericType.Int32 0))
                    |> ImmutableArray.CreateRange
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
        // The backing store is deliberately left empty while `Length` is not. That is an
        // inconsistent allocation, which is the point: it distinguishes a projection that
        // reads the stored `Length` from one that derives it from `Elements.Length`, and
        // it keeps the generator from ever materialising a large array. Bounding the rank
        // to 6 dimensions of at most 4 keeps the product well inside Int32 too, so no
        // seed can make this test fail for reasons unrelated to the property.
        let property (lengths : int list) : bool =
            let lengths = lengths |> List.truncate 6 |> List.map (fun n -> (abs (n % 5)))

            let total = lengths |> List.fold (*) 1

            let allocation : AllocatedArray =
                {
                    Shape =
                        {
                            ConcreteType = ConcreteTypeHandle.Array (ConcreteTypeHandle.Concrete 1, lengths.Length)
                            Length = total
                            Lengths = ImmutableArray.CreateRange lengths
                        }
                    Elements = ImmutableArray.Empty
                }

            let addr, heap = ManagedHeap.allocateArray allocation ManagedHeap.empty
            let shape = ManagedHeap.getArrayShape addr heap

            shape.ConcreteType = allocation.Shape.ConcreteType
            && shape.Length = allocation.Shape.Length
            && shape.Lengths = allocation.Shape.Lengths

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
