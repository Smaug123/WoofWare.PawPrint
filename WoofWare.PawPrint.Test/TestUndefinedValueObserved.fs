namespace WoofWare.Pawprint.Test

open System
open FsUnitTyped
open Microsoft.Extensions.Logging
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

    /// The observation PawPrint stops at, run step by step, and checked to be a stop at the state
    /// from before the observing step: stepping that state again stops at the same observation,
    /// so the stop cannot have skipped past the instruction or call that used the value.
    let private stoppedRunWith (pctSeed : uint64 option) (name : string) (source : string) : RunEnd =
        let image = Roslyn.compile [ source ]
        let messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let logger = loggerFactory.CreateLogger "TestUndefinedValueObserved"
        use peImage = new System.IO.MemoryStream (image)

        let sameStop (first : UndefinedValueObservation) (again : UndefinedValueObservation) : unit =
            again.Value |> shouldEqual first.Value
            string again.Use |> shouldEqual (string first.Use)

        let rec goMain (steps : int64) (prepared : Program.PreparedProgram) : RunEnd =
            if steps > BoundedRun.defaultMaxSteps then
                failwith $"%s{name} did not stop within the step budget"

            match Program.stepPrepared loggerFactory logger prepared with
            | Program.ProgramStepOutcome.StoppedAtUndefinedValue (stopped, thread, observation) ->
                match Program.stepPrepared loggerFactory logger stopped with
                | Program.ProgramStepOutcome.StoppedAtUndefinedValue (stoppedAgain, threadAgain, again) ->
                    threadAgain |> shouldEqual thread
                    sameStop observation again
                    // A stop is where stepping it leaves it: nothing of the tick that found it,
                    // not even the step counter its preamble advances, is carried into it.
                    stoppedAgain.State.Kernel.StepCounter
                    |> shouldEqual stopped.State.Kernel.StepCounter
                | _ -> failwith $"%s{name}: stepping the stopped state again did not stop at %O{observation}"

                RunEnd.StoppedAtUndefinedValue (stopped.State, thread, observation)
            | Program.ProgramStepOutcome.Completed outcome -> RunEnd.Ended outcome
            | Program.ProgramStepOutcome.Deadlocked (_, stuck) -> failwith $"%s{name} deadlocked: %s{stuck}"
            | Program.ProgramStepOutcome.InstructionStepped (prepared, _, _, _)
            | Program.ProgramStepOutcome.WorkerTerminated (prepared, _) -> goMain (steps + 1L) prepared

        let rec goStartup (steps : int64) (startup : Program.Startup) : RunEnd =
            match Program.stepStartup loggerFactory logger startup with
            | Program.StartupStepOutcome.StoppedAtUndefinedValue (stopped, thread, observation) ->
                match Program.stepStartup loggerFactory logger stopped with
                | Program.StartupStepOutcome.StoppedAtUndefinedValue (stoppedAgain, threadAgain, again) ->
                    threadAgain |> shouldEqual thread
                    sameStop observation again

                    stoppedAgain.State.Kernel.StepCounter
                    |> shouldEqual stopped.State.Kernel.StepCounter
                | _ -> failwith $"%s{name}: stepping the stopped startup again did not stop at %O{observation}"

                RunEnd.StoppedAtUndefinedValue (stopped.State, thread, observation)
            | Program.StartupStepOutcome.Completed (Program.ProgramStartResult.CompletedBeforeMain runEnd) -> runEnd
            | Program.StartupStepOutcome.Completed (Program.ProgramStartResult.Ready prepared) ->
                goMain (steps + 1L) prepared
            | Program.StartupStepOutcome.Deadlocked (_, stuck) -> failwith $"%s{name} deadlocked: %s{stuck}"
            | Program.StartupStepOutcome.Stepped (startup, _, _, _)
            | Program.StartupStepOutcome.WorkerTerminated (startup, _)
            | Program.StartupStepOutcome.PhaseAdvanced startup -> goStartup (steps + 1L) startup

        try
            Program.beginStartup
                loggerFactory
                (Some name)
                peImage
                { HostConfig.Default (FrameworkUnderTest.runtimeDirs ()) with
                    PctSeed = pctSeed
                }
            |> goStartup 0L
        with _ ->
            for message in messages () do
                System.Console.Error.WriteLine $"{message}"

            reraise ()

    let private run (name : string) (source : string) (check : UndefinedValueObservation -> unit) : unit =
        stoppedRunWith None name source |> observed |> check

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

    /// The array member `op` uses the undefined value as `description` says.
    let private expectArrayMemberUse
        (op : UnaryMetadataTokenIlOp)
        (description : string)
        (observation : UndefinedValueObservation)
        =
        match observation.Use with
        | UndefinedValueUse.InstructionDetail (_, _, IlOp.UnaryMetadataToken (actual, _), actualDescription) ->
            actual |> shouldEqual op
            actualDescription |> shouldEqual description
        | other -> failwith $"expected %O{op} to use the value, got %O{other}"

    [<Test>]
    let ``Constructing a multi-dimensional array of unwritten length stops the run at the newobj`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* lengths = stackalloc int[1];
        int[,] array = new int[*lengths, 2];
        return array.Length == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedMultiDimLength.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                expectArrayMemberUse
                    UnaryMetadataTokenIlOp.Newobj
                    "the lengths an array constructor allocates"
                    observation
            )

    [<Test>]
    let ``Indexing a multi-dimensional array by an unwritten index stops the run at the access`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* indices = stackalloc int[1];
        int[,] array = new int[2, 2];
        array[1, 1] = 7;
        return array[*indices, 0] == 7 ? 1 : 0;
    }
}
"""

        run
            "UndefinedMultiDimIndex.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                expectArrayMemberUse
                    UnaryMetadataTokenIlOp.Call
                    "the array and indices a multi-dimensional array's accessor uses"
                    observation
            )

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

    [<Test>]
    let ``Reading an unwritten byte of a local through a byte pointer gives that byte's undefined value`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 42;
        int local = *(int*)bytes;
        // Byte 0 of `local` is defined; byte 2 is not.
        if (*(byte*)&local != 42) return 1;
        return *((byte*)&local + 2) == 0 ? 0 : 2;
    }
}
"""

        run
            "UndefinedLocalByte.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 2 ]
            )

    [<Test>]
    let ``An int read across byte array elements, one of them undefined, is undefined in that byte`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* unwritten = stackalloc byte[4];
        byte[] bytes = new byte[4];
        bytes[0] = 1;
        bytes[1] = unwritten[3];
        bytes[2] = 3;
        bytes[3] = 4;
        // Four elements read as one int: only the second is undefined.
        int whole = MemoryMarshal.Read<int>(bytes);
        return whole > 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedAcrossArrayElements.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 3 ]

                observation.Value.Bytes
                |> shouldEqual
                    [
                        ValueByte.Defined 1uy
                        observation.Value.Bytes.[1]
                        ValueByte.Defined 3uy
                        ValueByte.Defined 4uy
                    ]
            )

    /// The observation must be the runtime's read through a pointer, of the undefined value
    /// `expected` names; `method` names the runtime-implemented method that read it.
    /// The observation is `methodName`'s read of `what`. A P/Invoke's own name is generated, so
    /// `methodName` is a prefix of it.
    let private expectReadByRuntime
        (methodName : string)
        (what : string)
        (observation : UndefinedValueObservation)
        : unit
        =
        match observation.Use with
        | UndefinedValueUse.ReadByRuntime (method, actualWhat) ->
            method.Name.StartsWith (methodName, StringComparison.Ordinal)
            |> shouldEqual true

            actualWhat |> shouldEqual what
        | other -> failwith $"expected %s{methodName}'s read of %s{what}, got %O{other}"

    [<Test>]
    let ``Interlocked.Add on a location holding an undefined value ends the run at the add`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Threading;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        int location = numbers[0];
        // Only moved so far; the add reads it.
        return Interlocked.Add(ref location, 1) == 1 ? 0 : 1;
    }
}
"""

        run
            "UndefinedInterlockedAdd.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]
                expectReadByRuntime "ExchangeAdd" "the location it operates on" observation
            )

    [<Test>]
    let ``Interlocked.Exchange returns an undefined old value, which is observed where it is used`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Threading;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        int location = numbers[0];
        // The exchange only moves the old value; the comparison uses it.
        int old = Interlocked.Exchange(ref location, 1);
        return old == 7 ? 1 : 0;
    }
}
"""

        run
            "UndefinedInterlockedExchangeResult.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                match observation.Use with
                | UndefinedValueUse.Operand (method, _, _, _) -> method.Name |> shouldEqual "Main"
                | other -> failwith $"expected Main's comparison to use the old value, got %O{other}"
            )

    [<Test>]
    let ``Interlocked.CompareExchange on a location holding an undefined value ends the run at the compare`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Threading;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        long* numbers = stackalloc long[1];
        long location = numbers[0];
        return Interlocked.CompareExchange(ref location, 1, 0) == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedInterlockedCompareExchange.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int64
                expectReadByRuntime "CompareExchange" "the location it operates on" observation
            )

    [<Test>]
    let ``Writing a buffer with an unwritten byte to a stream ends the run at the write`` () : unit =
        let source =
            """
using System;
using System.IO;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[4];
        bytes[0] = 65;
        bytes[1] = 66;
        bytes[3] = 10;
        using Stream output = Console.OpenStandardOutput();
        // The native write reads all four bytes, byte 2 among them.
        output.Write(new ReadOnlySpan<byte>(bytes, 4));
        return 0;
    }
}
"""

        run
            "UndefinedNativeWrite.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 2 ]
                expectReadByRuntime "<Write>g__" "the buffer it writes out" observation
            )

    [<Test>]
    let ``A path with an unwritten byte before its terminator ends the run at the syscall`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_MkDir", SetLastError = true)]
    static extern int MkDir(byte* path, int mode);

    static int Main(string[] args)
    {
        byte* existing = stackalloc byte[2];
        existing[0] = (byte)'e';
        existing[1] = 0;
        // The second fails with EEXIST, which is then the thread's last error.
        if (MkDir(existing, 0x1ff) != 0) return 2;
        if (MkDir(existing, 0x1ff) == 0) return 3;

        byte* path = stackalloc byte[3];
        path[0] = (byte)'d';
        path[2] = 0;
        // The scan for the terminator compares byte 1 with NUL.
        return MkDir(path, 0x1ff) == 0 ? 0 : 1;
    }
}
"""

        TestPureCases.runPawPrintSource
            "UndefinedNativePath.cs"
            source
            KernelConfig.Default
            (fun _ outcome ->
                match outcome with
                | RunEnd.StoppedAtUndefinedValue (state, thread, _) ->
                    // Reported at the state from before the call, so the P/Invoke's clearing of
                    // the last error on entry is not part of it.
                    EmulatedKernel.lastSystemErrorFor thread state.Kernel |> shouldEqual 17
                | _ -> ()

                let observation = observed outcome
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 1 ]
                expectReadByRuntime "MkDir" "the path it reads" observation
            )

    [<Test>]
    let ``Making an array whose lower bound nothing wrote ends the run where the runtime reads it`` () : unit =
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
        // Only moved so far: CoreLib checks the lengths, but hands the lower bounds to the runtime
        // unread.
        int[] lowerBounds = { numbers[0], 0 };
        Array made = Array.CreateInstance(typeof(int), new[] { 1, 1 }, lowerBounds);
        return made.Rank == 2 ? 0 : 1;
    }
}
"""

        run
            "UndefinedArrayLowerBound.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]
                expectReadByRuntime "<InternalCreate>g__" "the lower bounds of the array it makes" observation
            )

    [<Test>]
    let ``A socket address with an unwritten port ends the run at the port's read`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    static int Main(string[] args)
    {
        byte* address = stackalloc byte[16];
        // `sa_family = AF_INET` in Linux's two-byte layout; the port, at bytes 2 and 3, is
        // never written.
        address[0] = 2;
        address[1] = 0;
        ushort port;
        return GetPort(address, 16, &port) == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedNativeSockaddrPort.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 2 ]
                expectReadByRuntime "GetPort" "the socket address's port" observation
            )

    [<Test>]
    let ``A socket address with an unwritten family ends the run at the family's read`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;

[module: SkipLocalsInit]

unsafe class Program
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_GetPort")]
    static extern int GetPort(byte* socketAddress, int socketAddressLen, ushort* port);

    static int Main(string[] args)
    {
        byte* address = stackalloc byte[16];
        // Only the port is written; the family, at bytes 0 and 1 on Linux, is not.
        address[2] = 0;
        address[3] = 80;
        ushort port;
        return GetPort(address, 16, &port) == 0 ? 0 : 1;
    }
}
"""

        run
            "UndefinedNativeSockaddrFamily.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 0 ]
                expectReadByRuntime "GetPort" "the socket address's family" observation
            )

    [<Test>]
    let ``Comparing spans a byte of which is unwritten ends the run at the comparison`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* left = stackalloc byte[3];
        left[0] = 1;
        left[2] = 3;
        byte* right = stackalloc byte[3];
        right[0] = 1;
        right[1] = 2;
        right[2] = 3;
        // Byte 0 is equal, so the comparison goes on to byte 1 of `left`.
        return new ReadOnlySpan<byte>(left, 3).SequenceEqual(new ReadOnlySpan<byte>(right, 3)) ? 0 : 1;
    }
}
"""

        run
            "UndefinedSequenceEqual.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.UInt8
                stackOrigins observation.Value |> shouldEqual [ 1 ]
                expectReadByRuntime "SequenceEqual" "the bytes it compares" observation
            )

    [<Test>]
    let ``Making a string of a span of chars one of which is unwritten ends the run there`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        char* chars = stackalloc char[3];
        chars[0] = 'a';
        chars[2] = 'c';
        return new ReadOnlySpan<char>(chars, 3).ToString().Length == 3 ? 0 : 1;
    }
}
"""

        run
            "UndefinedSpanToString.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Char
                stackOrigins observation.Value |> shouldEqual [ 2 ; 3 ]
                expectReadByRuntime "ToString" "the characters of the span" observation
            )

    [<Test>]
    let ``Comparing char spans one of which holds an unwritten char ends the run at the comparison`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        char* chars = stackalloc char[2];
        chars[0] = 'a';
        return new ReadOnlySpan<char>(chars, 2).Equals("ab".AsSpan(), StringComparison.Ordinal) ? 0 : 1;
    }
}
"""

        run
            "UndefinedMemoryExtensionsEquals.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Char
                stackOrigins observation.Value |> shouldEqual [ 2 ; 3 ]
                expectReadByRuntime "Equals" "the characters of the spans" observation
            )

    [<Test>]
    let ``Reflection boxing a returned Nullable whose hasValue nothing wrote stops the run at the invoke`` () : unit =
        let source =
            """
using System.Reflection;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int? Make()
    {
        byte* bytes = stackalloc byte[8];
        // hasValue, at byte 0, is never written; the padding and the value are.
        for (int i = 1; i < 8; i++) bytes[i] = 0;
        return Unsafe.Read<int?>(bytes);
    }

    static int Main(string[] args)
    {
        object boxed = typeof(Program).GetMethod(nameof(Make), BindingFlags.NonPublic | BindingFlags.Static)!.Invoke(null, null);
        return boxed == null ? 0 : 1;
    }
}
"""

        run
            "UndefinedReflectionReturnHasValue.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 0 ]
                expectReadByRuntime "InvokeMethod" "the hasValue field boxing a Nullable`1 decides by" observation
            )

    [<Test>]
    let ``Reflection boxing a Nullable field whose hasValue nothing wrote stops the run at the read`` () : unit =
        let source =
            """
using System.Reflection;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int? Field;

    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[8];
        // hasValue, at byte 0, is never written; the padding and the value are.
        for (int i = 1; i < 8; i++) bytes[i] = 0;
        Field = Unsafe.Read<int?>(bytes);
        object boxed = typeof(Program).GetField(nameof(Field), BindingFlags.NonPublic | BindingFlags.Static)!.GetValue(null);
        return boxed == null ? 0 : 1;
    }
}
"""

        run
            "UndefinedReflectionFieldHasValue.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Bool
                stackOrigins observation.Value |> shouldEqual [ 0 ]
                expectReadByRuntime "<GetValue>g__" "the hasValue field boxing a Nullable`1 decides by" observation
            )

    [<Test>]
    let ``A delegate handing an unwritten value to an intrinsic target stops the run at the invoke`` () : unit =
        let source =
            """
using System;
using System.Numerics;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        Func<uint, int> popCount = BitOperations.PopCount;
        uint* bits = stackalloc uint[1];
        return popCount(*bits) == 3 ? 1 : 0;
    }
}
"""

        run
            "UndefinedDelegateIntrinsicArgument.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                match observation.Use with
                | UndefinedValueUse.RuntimeArgument (method, 0) -> method.Name |> shouldEqual "PopCount"
                | other -> failwith $"expected PopCount's argument, got %O{other}"
            )

    [<Test>]
    let ``A constrained call through an unwritten byref stops the run at the call`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

ref struct R
{
    public ref int X;
}

unsafe class Program
{
    static string Show<T>(ref T x) => x.ToString();

    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[sizeof(nint)];
        R r = Unsafe.Read<R>(bytes);
        return Show(ref r.X).Length == 0 ? 1 : 0;
    }
}
"""

        run
            "UndefinedConstrainedReceiver.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.RuntimePointer

                match observation.Use with
                | UndefinedValueUse.InstructionDetail (method, _, _, description) ->
                    method.Name |> shouldEqual "Show"

                    description
                    |> shouldEqual "the receiver a callvirt null-checks and dispatches on"
                | other -> failwith $"expected Show's constrained call to use the byref, got %O{other}"
            )

    [<Test>]
    let ``A struct hash code over an unwritten reference field stops the run at the strategy`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

struct S
{
    public object X;
}

unsafe class Program
{
    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[sizeof(nint)];
        S s = Unsafe.Read<S>(bytes);
        return s.GetHashCode() == 7 ? 1 : 0;
    }
}
"""

        run
            "UndefinedValueTypeHashCode.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.ObjectRef
                stackOrigins observation.Value |> shouldEqual [ 0..7 ]

                expectReadByRuntime
                    "<GetHashCodeStrategy>g__"
                    "the reference field the hash-code strategy tests for null"
                    observation
            )

    /// A stop under a scheduler with choices to make: whichever thread the seed runs into the
    /// undefined value, stepping the stopped program again makes the same scheduling decision and
    /// stops at the same place, rather than running another thread past it.
    [<Test>]
    let ``A stop on a worker thread under PCT scheduling replays to the same stop`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;
using System.Threading;

[module: SkipLocalsInit]

unsafe class Program
{
    static int counter;

    static void Busy()
    {
        for (int i = 0; i < 20; i++)
        {
            Interlocked.Increment(ref counter);
            Thread.Yield();
        }
    }

    static void Worker()
    {
        for (int i = 0; i < 10; i++) Thread.Yield();
        int* numbers = stackalloc int[1];
        if (numbers[0] == 3) Environment.Exit(1);
    }

    static int Main(string[] args)
    {
        var busy = new Thread(Busy);
        var worker = new Thread(Worker);
        busy.Start();
        worker.Start();
        Busy();
        worker.Join();
        busy.Join();
        return 0;
    }
}
"""

        for seed in 1UL .. 32UL do
            stoppedRunWith (Some seed) $"UndefinedUnderPct%d{seed}.cs" source
            |> observed
            |> fun observation -> stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

    [<Test>]
    let ``Constructing a span of unwritten length with newobj stops the run at the constructor`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

unsafe class Program
{
    static int LengthOf(Span<int> span) => span.Length;

    static int Main(string[] args)
    {
        int* numbers = stackalloc int[1];
        return LengthOf(new Span<int>(numbers, *numbers)) == 3 ? 1 : 0;
    }
}
"""

        run
            "UndefinedSpanConstructorLength.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.Int32
                stackOrigins observation.Value |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

                match observation.Use with
                | UndefinedValueUse.RuntimeArgument (method, index) ->
                    method.Name |> shouldEqual ".ctor"
                    index |> shouldEqual 2
                | other -> failwith $"expected the span constructor's argument, got %O{other}"
            )

    [<Test>]
    let ``Binding a delegate to a target nothing wrote ends the run at the bind`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;

[module: SkipLocalsInit]

struct S
{
    public object X;
}

unsafe class Program
{
    public static int Answer(object target) => 7;

    static int Main(string[] args)
    {
        byte* bytes = stackalloc byte[sizeof(nint)];
        S s = Unsafe.Read<S>(bytes);
        // CoreLib hands the first argument to the runtime unread.
        Delegate bound =
            Delegate.CreateDelegate(typeof(Func<int>), s.X, typeof(Program).GetMethod(nameof(Answer)), false);
        return bound == null ? 1 : 0;
    }
}
"""

        run
            "UndefinedDelegateTarget.cs"
            source
            (fun observation ->
                observation.Value.Kind |> shouldEqual UndefinedPrimitive.ObjectRef
                stackOrigins observation.Value |> shouldEqual [ 0..7 ]
                expectReadByRuntime "<BindToMethodInfo>g__" "the target it binds the delegate to" observation
            )

    [<Test>]
    let ``Waiting on a handle nothing wrote ends the run at the wait`` () : unit =
        let source =
            """
using System;
using System.Runtime.CompilerServices;
using System.Threading;
using Microsoft.Win32.SafeHandles;

[module: SkipLocalsInit]

unsafe class Program
{
    static int Main(string[] args)
    {
        nint* handles = stackalloc nint[1];
        using ManualResetEvent ev = new ManualResetEvent(false);
        // Moved into the safe handle, and from there into the array CoreLib hands the runtime.
        ev.SafeWaitHandle = new SafeWaitHandle(handles[0], ownsHandle: false);
        return WaitHandle.WaitAny(new WaitHandle[] { ev }, 0) == WaitHandle.WaitTimeout ? 0 : 1;
    }
}
"""

        run
            "UndefinedWaitHandle.cs"
            source
            (fun observation ->
                stackOrigins observation.Value |> shouldEqual [ 0..7 ]

                expectReadByRuntime "<WaitMultipleIgnoringSyncContext>g__" "the handles it waits on" observation
            )
