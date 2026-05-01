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
        { IlMachineThreadState.initial loggerFactory ImmutableArray.Empty corelib with
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
    let ``getObjectConcreteType returns concrete array type`` () : unit =
        let elementHandle = ConcreteTypeHandle.Concrete 1
        let arrayHandle = ConcreteTypeHandle.OneDimArrayZero elementHandle

        let array : AllocatedArray =
            {
                ConcreteType = arrayHandle
                Length = 0
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
    let ``setStringChar keeps firstChar field in sync`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state = state loggerFactory

        let addr, state =
            IlMachineRuntimeMetadata.allocateManagedString loggerFactory baseClassTypes "ab" state

        let heap = ManagedHeap.setStringChar addr 0 'z' state.ManagedHeap

        let firstCharField =
            FieldIdentity.requiredNonGenericInstanceFieldId state.ConcreteTypes baseClassTypes.String "_firstChar"

        ManagedHeap.get addr heap
        |> AllocatedNonArrayObject.DereferenceFieldById firstCharField
        |> shouldEqual (CliType.ofChar 'z')

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
