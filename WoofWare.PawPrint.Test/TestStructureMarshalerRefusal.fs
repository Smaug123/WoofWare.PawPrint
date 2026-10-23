namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// On .NET 11, `Marshal` marshals a struct through `StructureMarshaler<T>`, and for a struct that is
/// not blittable CoreCLR's VM replaces the bodies of its `ConvertToUnmanagedCore`,
/// `ConvertToManaged` and `FreeCore` with a stub that marshals the struct field by field. The
/// method's own IL copies the managed image, which for such a struct is the wrong answer, so
/// PawPrint fails the run at each of the three, naming it and the struct.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStructureMarshalerRefusal =

    let private source (call : string) : string =
        $$"""
using System;
using System.Runtime.InteropServices;

public unsafe class Program
{
    // A `bool` field marshals as a four-byte `BOOL`, so this struct is not blittable, though
    // CoreCLR blits its pointer field.
    [StructLayout(LayoutKind.Sequential)]
    struct WithBool
    {
        public int A;
        public bool B;
        public int* P;
    }

    public static int Main(string[] args)
    {
        IntPtr ptr = Marshal.AllocHGlobal(16);
        Marshal.WriteInt64(ptr, 0L);
        Marshal.WriteInt64(ptr, 8, 0L);
        {{call}};
        return 1;
    }
}
"""

    let private runRefused (name : string) (call : string) : GuestFailureException =
        match FrameworkUnderTest.runtime () with
        | EmulatedRuntime.Net10 ->
            Assert.Ignore
                ".NET 10 has no `StructureMarshaler<T>`: its `Marshal` marshals a struct that is not blittable through the stub `MarshalNative_TryGetStructMarshalStub` hands back, which PawPrint runs"
        | EmulatedRuntime.Net11 -> ()

        let image = Roslyn.compile [ source call ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let runtimes = FrameworkUnderTest.runtimeDirs ()

        Assert.Throws<GuestFailureException> (fun () ->
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default runtimes)
            |> ExpectRun.ended
            |> ignore<RunOutcome>
        )

    let private refusalNames (exc : GuestFailureException) (methodName : string) : unit =
        exc.Message
        |> shouldContainText
            $"replaces the body of System.Private.CoreLib System.StubHelpers.StructureMarshaler`1.%s{methodName}("

        exc.Message |> shouldContainText "Program+WithBool"
        exc.Message |> shouldContainText "not blittable"

    [<Test>]
    let ``StructureToPtr refuses at ConvertToUnmanagedCore`` () : unit =
        let exc =
            runRefused
                "StructureToPtrWithBool.cs"
                "Marshal.StructureToPtr(new WithBool { A = 1, B = true }, ptr, false)"

        refusalNames exc "ConvertToUnmanagedCore"

    [<Test>]
    let ``PtrToStructure refuses at ConvertToManaged`` () : unit =
        let exc =
            runRefused "PtrToStructureWithBool.cs" "Marshal.PtrToStructure<WithBool>(ptr)"

        refusalNames exc "ConvertToManaged"

    [<Test>]
    let ``DestroyStructure refuses at FreeCore`` () : unit =
        let exc =
            runRefused "DestroyStructureWithBool.cs" "Marshal.DestroyStructure<WithBool>(ptr)"

        refusalNames exc "FreeCore"
