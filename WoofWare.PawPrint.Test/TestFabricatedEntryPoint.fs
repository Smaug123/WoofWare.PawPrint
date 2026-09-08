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
///
/// Two further valid rows put a custom modifier on `Main`'s return column. CoreCLR's
/// `ValidateMainMethod` reads the return through `MetaSig::GetReturnType`, which skips custom
/// modifiers, so `void modreq(X) Main()` is a `void Main` and `int32 modopt(X) Main()` is an
/// `int Main`; the classifier that decides whether `Main`'s return latches the exit code must
/// look through them too.
[<TestFixture>]
module TestFabricatedEntryPoint =

    /// The custom modifiers on a fabricated `Main`'s return column.
    type private ReturnModifiers =
        {
            Required : Type list
            Optional : Type list
        }

        static member None : ReturnModifiers =
            {
                Required = []
                Optional = []
            }

    /// A console application whose only method is `static Main()` with the given return type,
    /// return custom modifiers, and body.
    let private fabricateExeWithModifiers
        (name : string)
        (returnType : Type)
        (modifiers : ReturnModifiers)
        (body : ILGenerator -> unit)
        : byte[]
        =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule name

        let program =
            modul.DefineType ("Program", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let main =
            program.DefineMethod (
                "Main",
                MethodAttributes.Public ||| MethodAttributes.Static,
                CallingConventions.Standard,
                returnType,
                Array.ofList modifiers.Required,
                Array.ofList modifiers.Optional,
                Type.EmptyTypes,
                null,
                null
            )

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

    /// A console application whose only method is `static Main()` with the given return type
    /// and body.
    let private fabricateExe (name : string) (returnType : Type) (body : ILGenerator -> unit) : byte[] =
        fabricateExeWithModifiers name returnType ReturnModifiers.None body

    /// Vacuity guard for the modified-return rows: the modifier really is on `Main`'s return
    /// column in the emitted metadata, where the classifier under test reads it. Without this a
    /// builder that dropped the modifier would leave the row indistinguishable from its
    /// unmodified control.
    let private assertMainReturnIsModified (name : string) (image : byte[]) : unit =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)
        let dumped = Assembly.read loggerFactory (Some name) peImage

        let entryPoint =
            match dumped.MainMethod with
            | None -> failwith $"%s{name}: fabricated image has no entry point"
            | Some d -> d

        match dumped.Methods.[entryPoint].Signature.ReturnType with
        | MethodReturnType.Returns (TypeDefn.Modified _) -> ()
        | other -> failwith $"%s{name}: Main's return column carries no custom modifier: %O{other}"

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
    let ``a fabricated void Main whose return carries a modreq exits 0 on both runtimes`` () : unit =
        let image =
            fabricateExeWithModifiers
                "VoidModreqMain"
                typeof<Void>
                {
                    Required = [ typeof<System.Runtime.CompilerServices.IsExternalInit> ]
                    Optional = []
                }
                (fun il -> il.Emit OpCodes.Ret)

        assertMainReturnIsModified "VoidModreqMain" image
        expectExit "VoidModreqMain" image 0

    [<Test>]
    let ``a fabricated int Main whose return carries a modopt exits with its return value on both runtimes`` () : unit =
        let image =
            fabricateExeWithModifiers
                "IntModoptMain"
                typeof<int>
                {
                    Required = []
                    Optional = [ typeof<System.Runtime.CompilerServices.IsConst> ]
                }
                (fun il ->
                    il.Emit (OpCodes.Ldc_I4, 7)
                    il.Emit OpCodes.Ret
                )

        assertMainReturnIsModified "IntModoptMain" image
        expectExit "IntModoptMain" image 7

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
