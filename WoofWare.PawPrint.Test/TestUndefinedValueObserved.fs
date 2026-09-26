namespace WoofWare.Pawprint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// Guests that *use* a value read from memory nothing wrote: branch on it, compute with it,
/// return it as the exit code, hand it to the runtime. Real .NET's behaviour is undefined for
/// every one of them, so none can be checked against the real runtime; what is checked instead is
/// that PawPrint ends the run with `RunOutcome.UndefinedValueObserved`, naming the use and the
/// never-written bytes, rather than inventing a value or crashing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
[<Category("Guest")>]
[<Explicit>]
module TestUndefinedValueObserved =

    /// The observation the run ended with, failing on any other outcome.
    let private observed (outcome : RunOutcome) : UndefinedValueObservation =
        match outcome with
        | RunOutcome.UndefinedValueObserved (_, _, observation) -> observation
        | RunOutcome.NormalExit (state, _) ->
            failwith
                $"expected an undefined value to be observed, got a normal exit with code %d{state.LatchedExitCode}"
        | RunOutcome.ProcessExit _ -> failwith "expected an undefined value to be observed, got a process exit"
        | RunOutcome.Aborted (_, _, fatal) ->
            failwith $"expected an undefined value to be observed, got an abort (%O{fatal.Code}): %A{fatal.Message}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"expected an undefined value to be observed, got signal termination %O{signal}"
        | RunOutcome.GuestUnhandledException (state, _, exn) ->
            failwith
                $"expected an undefined value to be observed, got an unhandled exception:\n%s{UnhandledExceptionReport.describe state exn}"

    /// The offsets of the never-written bytes `value` descends from, each of which must be a
    /// byte of a `localloc` block.
    let private stackOrigins (value : UndefinedValue) : int list =
        value.Origins
        |> List.map (fun origin ->
            match origin.Memory with
            | UninitialisedMemory.Stack _ -> origin.Offset
            | UninitialisedMemory.Native block -> failwith $"expected a localloc origin, got %O{block}"
        )

    let private run (name : string) (source : string) (check : UndefinedValueObservation -> unit) : unit =
        TestPureCases.runPawPrintSource name source KernelConfig.Default (fun _ outcome -> check (observed outcome))

    [<Test>]
    let ``Branching on an unwritten stackalloc bool ends the run at the branch`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        bool* flags = stackalloc bool[4];
        flags[0] = true;
        // `MethodBaseInvoker.CopyBack`'s shape: a flag nothing set, branched on.
        if (flags[2]) return 1;
        return 0;
    }
}
"""

        run
            "UndefinedBranch.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 2 ]

                match observation.Use with
                | UndefinedValueUse.Operand (method, _, instruction, fromTop) ->
                    method.Name |> shouldEqual "Main"
                    fromTop |> shouldEqual 0

                    match instruction with
                    | IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s _)
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s _)
                    | IlOp.UnaryConst (UnaryConstIlOp.Brfalse _)
                    | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _) -> ()
                    | other -> failwith $"expected a conditional branch, got %O{other}"
                | other -> failwith $"expected an instruction operand, got %O{other}"
            )

    [<Test>]
    let ``Arithmetic on an unwritten stackalloc int ends the run at the arithmetic`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[2];
        numbers[1] = 5;
        // Pushed first, so the undefined operand is the one beneath the top.
        int sum = numbers[0] + numbers[1];
        return sum == 5 ? 0 : 1;
    }
}
"""

        run
            "UndefinedArithmetic.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                match observation.Use with
                | UndefinedValueUse.Operand (_, _, IlOp.Nullary NullaryIlOp.Add, fromTop) -> fromTop |> shouldEqual 1
                | other -> failwith $"expected the add's operand, got %O{other}"
            )

    [<Test>]
    let ``A partly written value is undefined if any byte of it is`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 1;
        bytes[1] = 2;
        bytes[3] = 4;
        // Three of the four bytes are written; the int they make up is still undefined.
        int whole = *(int*)bytes;
        return whole > 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedPartlyWritten.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 2 ]

                observation.Value.Bytes
                |> shouldEqual
                    [
                        ValueByte.Defined 1uy
                        ValueByte.Defined 2uy
                        observation.Value.Bytes.[2]
                        ValueByte.Defined 4uy
                    ]
            )

    [<Test>]
    let ``Returning an unwritten value from Main ends the run at the exit code`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        return numbers[0];
    }
}
"""

        run
            "UndefinedExitCode.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32

                match observation.Use with
                | UndefinedValueUse.ExitCode -> ()
                | other -> failwith $"expected the exit code, got %O{other}"
            )

    [<Test>]
    let ``Passing an unwritten value to a runtime-implemented method ends the run at the call`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        // Moved through an argument into the managed wrapper, then handed to the runtime.
        Environment.Exit(numbers[0]);
        return 0;
    }
}
"""

        run
            "UndefinedRuntimeArgument.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32

                match observation.Use with
                | UndefinedValueUse.RuntimeArgument (_, index) -> index |> shouldEqual 0
                | other -> failwith $"expected a runtime-implemented method's argument, got %O{other}"
            )

    [<Test>]
    let ``Boxing a Nullable whose hasValue nothing wrote ends the run at the box`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        // `byte?` has no padding, so each of its two bytes is a field's.
        byte?* slots = stackalloc byte?[1];
        // Copying the Nullable moves its undefined fields; boxing it reads hasValue.
        byte? copy = slots[0];
        object boxed = copy;
        return boxed is null ? 0 : 1;
    }
}
"""

        run
            "UndefinedNullableBox.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 0 ]

                match observation.Use with
                | UndefinedValueUse.InstructionDetail (_, _, IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, _), _) ->
                    ()
                | other -> failwith $"expected the box's read of hasValue, got %O{other}"
            )

    [<Test>]
    let ``A constrained call that would box a Nullable whose hasValue nothing wrote ends the run at the call``
        ()
        : unit
        =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    // `value.GetType()` on a `ref T` is `constrained. !!T callvirt Object::GetType`, which boxes
    // the Nullable it points at, and so reads its hasValue.
    static Type TypeOf<T>(ref T value) => value.GetType();

    static int Main(string[] args)
    {
        byte?* slots = stackalloc byte?[1];
        byte? copy = slots[0];
        return TypeOf(ref copy) == typeof(byte) ? 0 : 1;
    }
}
"""

        run
            "UndefinedNullableConstrained.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 0 ]

                match observation.Use with
                | UndefinedValueUse.InstructionDetail (method,
                                                       _,
                                                       IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _),
                                                       _) -> method.Name |> shouldEqual "TypeOf"
                | other -> failwith $"expected the constrained callvirt's read of hasValue, got %O{other}"
            )
