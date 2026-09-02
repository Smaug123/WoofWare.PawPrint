namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// An entry point whose IL no C# source can spell: a `Main` that returns with the wrong number
/// of values on its evaluation stack.
///
/// CoreCLR's JIT refuses such a method outright (`InvalidProgramException` when `Main` is first
/// called, so an unhandled exception on the real runtime), and PawPrint refuses it on the `ret` as
/// invalid CIL for any *called* method. The bottom frame of a thread reaches `ret` with nothing
/// to return to, so this fixture pins that the check applies there as well: a `void Main` that
/// leaves a value behind must not be reported as a clean exit, and an `int Main` that leaves two
/// must not be reported as exiting with the top one.
///
/// The two valid rows are the control: a fabricated executable loads and runs on both runtimes,
/// and the interpreter reads its exit code, so a refusal of the invalid rows is a refusal of the
/// IL and not of the fabrication.
[<TestFixture>]
module TestFabricatedEntryPoint =

    /// A console application whose only method is `static Main()` with the given return type
    /// and body.
    let private fabricateExe (name : string) (returnType : Type) (body : ILGenerator -> unit) : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule name

        let program =
            modul.DefineType ("Program", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let main =
            program.DefineMethod ("Main", MethodAttributes.Public ||| MethodAttributes.Static, returnType, [||])

        body (main.GetILGenerator ())
        program.CreateType () |> ignore<Type>

        let metadata, ilStream, fieldData = builder.GenerateMetadata ()

        let pe =
            ManagedPEBuilder (
                PEHeaderBuilder.CreateExecutableHeader (),
                MetadataRootBuilder metadata,
                ilStream,
                mappedFieldData = fieldData,
                entryPoint = MetadataTokens.MethodDefinitionHandle main.MetadataToken
            )

        let blob = BlobBuilder ()
        pe.Serialize blob |> ignore<BlobContentId>
        blob.ToArray ()

    let private runOnPawPrint (name : string) (image : byte[]) : RunOutcome =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll typeof<RunResult>.Assembly.Location
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)
        BoundedRun.run loggerFactory name (Some name) peImage (HostConfig.Default dotnetRuntimes)

    /// Both runtimes run the image to a clean exit with `expected`.
    let private expectExit (name : string) (image : byte[]) (expected : int) : unit =
        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit code -> code |> shouldEqual expected
        | other -> failwith $"%s{name}: real runtime did not exit normally: %O{other}"

        match runOnPawPrint name image with
        | RunOutcome.NormalExit (state, _) -> state.LatchedExitCode |> shouldEqual expected
        | other -> failwith $"%s{name}: PawPrint did not exit normally: %O{other}"

    /// The real runtime dies of an unhandled `InvalidProgramException`, and PawPrint refuses the
    /// image as invalid CIL — a host failure, annotated with where the guest was — with a message
    /// containing `refusal`.
    let private expectRefused (name : string) (image : byte[]) (refusal : string) : unit =
        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.UnhandledException report -> report |> shouldContainText "InvalidProgramException"
        | other -> failwith $"%s{name}: real runtime did not throw: %O{other}"

        let exn =
            Assert.Throws<GuestFailureException> (fun () -> runOnPawPrint name image |> ignore<RunOutcome>)

        exn.Message |> shouldContainText refusal

    [<Test>]
    let ``a fabricated void Main exits 0 on both runtimes`` () : unit =
        let image = fabricateExe "VoidMain" typeof<Void> (fun il -> il.Emit OpCodes.Ret)
        expectExit "VoidMain" image 0

    [<Test>]
    let ``a fabricated int Main exits with its return value on both runtimes`` () : unit =
        let image =
            fabricateExe
                "IntMain"
                typeof<int>
                (fun il ->
                    il.Emit (OpCodes.Ldc_I4, 5)
                    il.Emit OpCodes.Ret
                )

        expectExit "IntMain" image 5

    [<Test>]
    let ``a void Main that leaves a value on the stack is refused, not reported as a clean exit`` () : unit =
        let image =
            fabricateExe
                "VoidMainLeavesValue"
                typeof<Void>
                (fun il ->
                    il.Emit (OpCodes.Ldc_I4, 1)
                    il.Emit OpCodes.Ret
                )

        expectRefused
            "VoidMainLeavesValue"
            image
            "Invalid CIL: void method Main returned with a non-empty evaluation stack"

    [<Test>]
    let ``an int Main that leaves two values on the stack is refused, not reported as exiting with the top one``
        ()
        : unit
        =
        let image =
            fabricateExe
                "IntMainLeavesTwoValues"
                typeof<int>
                (fun il ->
                    il.Emit (OpCodes.Ldc_I4, 5)
                    il.Emit (OpCodes.Ldc_I4, 6)
                    il.Emit OpCodes.Ret
                )

        expectRefused
            "IntMainLeavesTwoValues"
            image
            "Invalid CIL: method Main returned with more than one evaluation stack value"
