namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
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

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

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
                ConcreteType = intArrayHandle
                Length = 0
                Lengths = ImmutableArray.Create 0
                Elements = ImmutableArray.Empty
            }

        let stringArray : AllocatedArray =
            {
                ConcreteType = stringArrayHandle
                Length = 0
                Lengths = ImmutableArray.Create 0
                Elements = ImmutableArray.Empty
            }

        let intArrayAddr, heap = ManagedHeap.allocateArray intArray ManagedHeap.empty
        let stringArrayAddr, heap = ManagedHeap.allocateArray stringArray heap

        heap.Arrays.[intArrayAddr].ConcreteType |> shouldEqual intArrayHandle
        heap.Arrays.[stringArrayAddr].ConcreteType |> shouldEqual stringArrayHandle

    [<Test>]
    let ``getObjectConcreteType returns concrete array type`` () : unit =
        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.OneDimArrayZero elementHandle

        let array : AllocatedArray =
            {
                ConcreteType = arrayHandle
                Length = 0
                Lengths = ImmutableArray.Create 0
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
        array.ConcreteType |> shouldEqual arrayHandle
        array.Length |> shouldEqual 12
        array.Lengths |> shouldEqual lengths
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
        array.Length |> shouldEqual 0
        array.Lengths |> shouldEqual lengths
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
        array.Length |> shouldEqual 0
        array.Lengths |> shouldEqual lengths
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
