namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open Microsoft.Extensions.Logging.Abstractions
open NUnit.Framework
open WoofWare.PawPrint

/// `HostStartupCall`'s `Exception*` out-slot reads back exactly what the callee stored in it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestHostExceptionSlot =

    let private initialState () : IlMachineState =
        let image =
            Roslyn.compile [ "public static class Program { public static void Main() { } }" ]

        use peImage = new MemoryStream (image)
        let entry = Assembly.read NullLoggerFactory.Instance None peImage
        IlMachineState.initial NullLoggerFactory.Instance (FrameworkUnderTest.runtimeDirs ()) entry

    let private store (slot : ExceptionSlot) (value : CliType) (state : IlMachineState) : IlMachineState =
        let block =
            match slot.Pointer with
            | ManagedPointerSource.Byref {
                                             Root = ByrefRoot.NativeMemoryByte (block, 0)
                                             Projections = []
                                         } -> block
            | other -> failwith $"expected the slot to point at byte 0 of a native block, got %O{other}"

        IlMachineState.getNativeMemoryPool state
        |> NativeMemoryPool.writeCell block 0 value
        |> fun pool -> IlMachineState.setNativeMemoryPool pool state

    [<Test>]
    let ``a slot nothing stored into reads back as null`` () : unit =
        let slot, state = HostStartupCall.allocateExceptionSlot (initialState ())
        HostStartupCall.readExceptionSlot slot state |> shouldEqual None

    [<Test>]
    let ``a slot reads back the object stored into it`` () : unit =
        let slot, state = HostStartupCall.allocateExceptionSlot (initialState ())
        let addr = ManagedHeapAddress.ManagedHeapAddress 12345

        let state = store slot (CliType.ObjectRef (Some addr)) state
        HostStartupCall.readExceptionSlot slot state |> shouldEqual (Some addr)

        let state = store slot (CliType.ObjectRef None) state
        HostStartupCall.readExceptionSlot slot state |> shouldEqual None

    [<Test>]
    let ``a slot holding something other than an object reference is refused`` () : unit =
        let slot, state = HostStartupCall.allocateExceptionSlot (initialState ())
        let state = store slot (CliType.Numeric (CliNumericType.Int32 7)) state

        let e =
            Assert.Throws<Exception> (fun () -> HostStartupCall.readExceptionSlot slot state |> ignore)

        e.Message |> shouldContainText "not an object reference"

    [<Test>]
    let ``slots are distinct`` () : unit =
        let first, state = HostStartupCall.allocateExceptionSlot (initialState ())
        let second, state = HostStartupCall.allocateExceptionSlot state
        let addr = ManagedHeapAddress.ManagedHeapAddress 12345

        let state = store first (CliType.ObjectRef (Some addr)) state
        HostStartupCall.readExceptionSlot first state |> shouldEqual (Some addr)
        HostStartupCall.readExceptionSlot second state |> shouldEqual None
