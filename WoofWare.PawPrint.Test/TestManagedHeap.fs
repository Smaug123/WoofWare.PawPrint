namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestManagedHeap =

    /// Register a string at a synthetic String heap address by allocating a null-terminated
    /// sibling `char[]` in the ordinary `Arrays` map and recording the linkage. Mirrors the
    /// shape of `IlMachineState.allocateManagedString` without depending on the rest of the
    /// machine state needed to allocate the String heap object itself.
    let private registerString (strAddr : ManagedHeapAddress) (contents : string) (heap : ManagedHeap) : ManagedHeap =
        let payload =
            let builder = ImmutableArray.CreateBuilder<CliType> (contents.Length + 1)

            for c in contents do
                builder.Add (CliType.ofChar c)

            builder.Add (CliType.ofChar (char 0))
            builder.MoveToImmutable ()

        let arr : AllocatedArray =
            {
                Length = contents.Length + 1
                Elements = payload
            }

        let charArrAddr, heap = ManagedHeap.allocateArray arr heap
        heap |> ManagedHeap.recordStringCharArray strAddr charArrAddr

    [<Test>]
    let ``stringsEqual: same content at different addresses is equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> registerString addr1 "hello"
            |> registerString addr2 "hello"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual true

    [<Test>]
    let ``stringsEqual: same address is equal`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 1
        let heap = ManagedHeap.empty |> registerString addr "hello"
        ManagedHeap.stringsEqual addr addr heap |> shouldEqual true

    [<Test>]
    let ``stringsEqual: different content of same length is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> registerString addr1 "hello"
            |> registerString addr2 "world"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: different length is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty |> registerString addr1 "hello" |> registerString addr2 "hell"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: empty strings are equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap = ManagedHeap.empty |> registerString addr1 "" |> registerString addr2 ""

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual true

    [<Test>]
    let ``stringsEqual: shared prefix but one is longer is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> registerString addr1 "hello world"
            |> registerString addr2 "hello"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: differ only in last char is not equal`` () : unit =
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty
            |> registerString addr1 "abcdef"
            |> registerString addr2 "abcdeg"

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual false

    [<Test>]
    let ``stringsEqual: ignores a mutated null terminator`` () : unit =
        // Logical contents match, but one sibling's terminator has been overwritten — e.g.
        // via a `_firstChar` byref that was advanced past the end. Equality must track the
        // visible string, not the hidden terminator.
        let addr1 = ManagedHeapAddress.ManagedHeapAddress 1
        let addr2 = ManagedHeapAddress.ManagedHeapAddress 2

        let heap =
            ManagedHeap.empty |> registerString addr1 "abc" |> registerString addr2 "abc"

        let siblingAddr2 = ManagedHeap.resolveStringCharArrayAddr addr2 heap
        let terminatorIdx = (ManagedHeap.resolveStringChars addr2 heap).Length - 1

        let heap =
            ManagedHeap.setArrayValue siblingAddr2 terminatorIdx (CliType.ofChar 'Z') heap

        ManagedHeap.stringsEqual addr1 addr2 heap |> shouldEqual true
