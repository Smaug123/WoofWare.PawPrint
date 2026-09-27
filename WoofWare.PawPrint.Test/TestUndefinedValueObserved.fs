namespace WoofWare.Pawprint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Test

/// Guests that *use* a value read from memory nothing wrote: branch on it, compute with it,
/// return it as the exit code, hand it to the runtime. Real .NET's behaviour is undefined for
/// every one of them, so none can be checked against the real runtime; what is checked instead is
/// that PawPrint stops the run with `RunEnd.StoppedAtUndefinedValue`, naming the use and the
/// never-written bytes, rather than inventing a value or crashing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
[<Category("Guest")>]
[<Explicit>]
module TestUndefinedValueObserved =

    /// The observation PawPrint stopped the run at, failing if the process ended instead.
    let private observed (runEnd : RunEnd) : UndefinedValueObservation =
        match runEnd with
        | RunEnd.StoppedAtUndefinedValue (_, _, observation) -> observation
        | RunEnd.Ended (RunOutcome.NormalExit (state, _, _)) ->
            failwith
                $"expected an undefined value to be observed, got a normal exit with code %d{state.LatchedExitCode}"
        | RunEnd.Ended (RunOutcome.ProcessExit _) ->
            failwith "expected an undefined value to be observed, got a process exit"
        | RunEnd.Ended (RunOutcome.Aborted (_, _, fatal, _)) ->
            failwith $"expected an undefined value to be observed, got an abort (%O{fatal.Code}): %A{fatal.Message}"
        | RunEnd.Ended (RunOutcome.SignalTerminated (_, signal, _)) ->
            failwith $"expected an undefined value to be observed, got signal termination %O{signal}"
        | RunEnd.Ended (RunOutcome.GuestUnhandledException (state, _, exn, _)) ->
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

    /// Which way the process ended, without rendering the machine state an outcome carries.
    let private endedName (outcome : RunOutcome) : string =
        match outcome with
        | RunOutcome.NormalExit _ -> "a normal exit"
        | RunOutcome.ProcessExit _ -> "a process exit"
        | RunOutcome.Aborted _ -> "an abort"
        | RunOutcome.SignalTerminated _ -> "signal termination"
        | RunOutcome.GuestUnhandledException _ -> "an unhandled exception"

    let private cctorBranchSource =
        """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static readonly int Chosen;

    static Program()
    {
        bool* flags = stackalloc bool[1];
        Chosen = flags[0] ? 1 : 2;
    }

    static int Main(string[] args) => Chosen;
}
"""

    let private branchSource =
        """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        bool* flags = stackalloc bool[1];
        if (flags[0]) return 1;
        return 0;
    }
}
"""

    /// `Program.run` hands the stop back as a value, so a caller analysing the guest can inspect
    /// it; it never raises.
    [<Test>]
    let ``Program.run returns a stop at an undefined value`` () : unit =
        let image = Roslyn.compile [ branchSource ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new System.IO.MemoryStream (image)

        match
            Program.run
                loggerFactory
                (Some "UndefinedRun.cs")
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | RunEnd.StoppedAtUndefinedValue (state, thread, observation) ->
            observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
            // The state from before the observing step: the thread is still in `Main`.
            state.ThreadState.[thread].MethodState.ExecutingMethod.Name
            |> shouldEqual "Main"
        | RunEnd.Ended outcome -> failwith $"expected a stop at an undefined value, got %s{endedName outcome}"

    /// A use in the entry type's static constructor, which runs during startup, before `Main` is
    /// installed.
    [<Test>]
    let ``An undefined value used before Main stops the run before Main`` () : unit =
        let source = cctorBranchSource

        let image = Roslyn.compile [ source ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new System.IO.MemoryStream (image)

        match
            Program.prepare
                loggerFactory
                (Some "UndefinedBeforeMain.cs")
                peImage
                (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | Program.ProgramStartResult.CompletedBeforeMain (RunEnd.StoppedAtUndefinedValue (state, thread, observation)) ->
            observation.Value.Origins
            |> List.map (fun origin -> origin.Offset)
            |> shouldEqual [ 0 ]

            state.ThreadState.[thread].MethodState.ExecutingMethod.Name
            |> shouldEqual ".cctor"
        | Program.ProgramStartResult.CompletedBeforeMain (RunEnd.Ended outcome) ->
            failwith $"expected a stop at an undefined value, got %s{endedName outcome}"
        | Program.ProgramStartResult.Ready _ -> failwith "expected the run to stop before Main was installed"

    /// As below, when the stop comes during startup.
    [<Test>]
    let ``A stop at an undefined value during startup is the prefix's outcome`` () : unit =
        let image = Roslyn.compile [ cctorBranchSource ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new System.IO.MemoryStream (image)

        match
            Program.runToFirstFork
                loggerFactory
                (Some "UndefinedStartupPrefix.cs")
                peImage
                (GuestConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | Program.PrefixOutcome.NeverForked (RunEnd.StoppedAtUndefinedValue (state, thread, _)) ->
            state.ThreadState.[thread].MethodState.ExecutingMethod.Name
            |> shouldEqual ".cctor"
        | Program.PrefixOutcome.NeverForked (RunEnd.Ended outcome) ->
            failwith $"expected a stop at an undefined value, got %s{endedName outcome}"
        | Program.PrefixOutcome.ForkedAt _
        | Program.PrefixOutcome.DeadlockedBeforeFork _
        | Program.PrefixOutcome.ForkedDuringStartup _ -> failwith "expected the guest to stop without forking"

    /// A stop needs no scheduling choice, so a guest that never forks reports it as the prefix's
    /// whole outcome.
    [<Test>]
    let ``A stop at an undefined value before any fork is the prefix's outcome`` () : unit =
        let image = Roslyn.compile [ branchSource ]
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new System.IO.MemoryStream (image)

        match
            Program.runToFirstFork
                loggerFactory
                (Some "UndefinedPrefix.cs")
                peImage
                (GuestConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        with
        | Program.PrefixOutcome.NeverForked (RunEnd.StoppedAtUndefinedValue (_, _, observation)) ->
            observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
        | Program.PrefixOutcome.NeverForked (RunEnd.Ended outcome) ->
            failwith $"expected a stop at an undefined value, got %s{endedName outcome}"
        | Program.PrefixOutcome.ForkedAt _
        | Program.PrefixOutcome.DeadlockedBeforeFork _
        | Program.PrefixOutcome.ForkedDuringStartup _ -> failwith "expected the guest to stop without forking"

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
