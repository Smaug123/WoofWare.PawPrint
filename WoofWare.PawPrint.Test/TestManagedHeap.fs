namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestManagedHeap =

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
                Elements = ImmutableArray.Empty
            }

        let stringArray : AllocatedArray =
            {
                ConcreteType = stringArrayHandle
                Length = 0
                Elements = ImmutableArray.Empty
            }

        let intArrayAddr, heap = ManagedHeap.allocateArray intArray ManagedHeap.empty
        let stringArrayAddr, heap = ManagedHeap.allocateArray stringArray heap

        heap.Arrays.[intArrayAddr].ConcreteType |> shouldEqual intArrayHandle
        heap.Arrays.[stringArrayAddr].ConcreteType |> shouldEqual stringArrayHandle

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
