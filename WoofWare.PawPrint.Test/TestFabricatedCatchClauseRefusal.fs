namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// A `catch` clause in metadata naming a byref or `System.Void`, against the real runtime.
///
/// CoreCLR's JIT resolves a clause's type when it imports the clause's handler, which it does once
/// it imports a block of the protected region, and `CEEInfo::resolveToken` refuses a byref or
/// `System.Void` there: the method is refused with `InvalidProgramException` before it runs, raised
/// in its caller. Measured: refused where the protected block is reachable, even behind a branch on
/// a local, and compiled where nothing reaches it or a branch the JIT folds skips it.
///
/// PawPrint has no compile step, so it cannot raise that where CoreCLR does; it stops the run
/// instead when it enters such a method (see docs/divergences.md). Where only the JIT's folding of
/// a branch decides whether it compiles the protected block, PawPrint stops too, rather than guess.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedCatchClauseRefusal =

    /// What each runtime does with a scenario's method.
    type private Expected =
        /// Both run it.
        | Runs
        /// The real runtime refuses it; PawPrint stops, saying CoreCLR refuses it.
        | Refused
        /// The real runtime runs it, the JIT folding the branch past the protected block; PawPrint
        /// stops, saying CoreCLR may refuse it.
        | Undecided

    /// `try { } catch (catchType) { }`, after whatever `guard` emits, which is handed the label
    /// past the clause.
    let private clause (catchType : Type) (guard : ILGenerator -> Label -> unit) (il : ILGenerator) : unit =
        let past = il.DefineLabel ()
        // `ILGenerator` does not count the exception a handler starts with towards `.maxstack`, so
        // the body pushes a value of its own first.
        il.Emit OpCodes.Ldc_I4_0
        il.Emit OpCodes.Pop
        guard il past
        il.BeginExceptionBlock () |> ignore<Label>
        il.BeginCatchBlock catchType
        il.Emit OpCodes.Pop
        il.EndExceptionBlock ()
        il.MarkLabel past

    let private unguarded (_ : ILGenerator) (_ : Label) : unit = ()

    let private scenarios : (string * (ILGenerator -> unit) * Expected) list =
        let byref = typeof<int>.MakeByRefType ()

        [
            "CatchException", clause typeof<Exception> unguarded, Expected.Runs
            "CatchByref", clause byref unguarded, Expected.Refused
            "CatchVoid", clause typeof<Void> unguarded, Expected.Refused
            // A branch on a local, which the JIT does not fold, leaves the protected block compiled.
            "CatchByrefBehindLocal",
            clause
                byref
                (fun il past ->
                    il.Emit (OpCodes.Ldloc, il.DeclareLocal typeof<int>)
                    il.Emit (OpCodes.Brtrue, past)
                ),
            Expected.Refused
            "CatchByrefUnreachable", clause byref (fun il past -> il.Emit (OpCodes.Br, past)), Expected.Runs
            // The JIT folds a branch on a literal, and never compiles the protected block.
            "CatchByrefBehindLiteral",
            clause
                byref
                (fun il past ->
                    il.Emit OpCodes.Ldc_I4_1
                    il.Emit (OpCodes.Brtrue, past)
                ),
            Expected.Undecided
        ]

    /// `static class Probes`, with the one `static void` method of no arguments that `emit` writes,
    /// named `Probe`.
    let private fabricate (emit : ILGenerator -> unit) : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "CatchClauseRefusal", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "CatchClauseRefusal"

        let probes =
            modul.DefineType ("Probes", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let il =
            probes
                .DefineMethod("Probe", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])
                .GetILGenerator ()

        emit il
        il.Emit OpCodes.Ret
        probes.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// 1 where the probe ran, 2 where calling it raised `InvalidProgramException`.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    public static int Main(string[] args)
    {
        try
        {
            Probes.Probe();
            return 1;
        }
        catch (InvalidProgramException)
        {
            return 2;
        }
    }
}
"""

    let private cases : TestCaseData list =
        scenarios
        |> List.map (fun (name, _, _) ->
            TestCaseData(name).SetName $"a catch clause is treated as on the real runtime: %s{name}"
        )

    [<TestCaseSource(nameof cases)>]
    let ``a catch clause is treated as on the real runtime`` (name : string) : unit =
        let _, emit, expected = scenarios |> List.find (fun (n, _, _) -> n = name)

        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "CatchClauseRefusal" (fabricate emit) "CatchClauseRefusalDriver" driverSource

        let hostCode =
            match expected with
            | Expected.Runs
            | Expected.Undecided -> 1
            | Expected.Refused -> 2

        match onHost with
        | RealRuntimeResult.NormalExit code when code = hostCode -> ()
        | other -> failwith $"%s{name}: the real runtime ended %O{other}, not with exit code %d{hostCode}"

        let stoppedSaying (said : string) =
            match onPawPrint with
            | FabricatedOutcome.Failed e when e.ToString().Contains said -> ()
            | other -> failwith $"%s{name}: PawPrint ended %O{other}, rather than stopping with \"%s{said}\""

        match expected with
        | Expected.Runs ->
            match onPawPrint with
            | FabricatedOutcome.Exited 1 -> ()
            | other -> failwith $"%s{name}: PawPrint ended %O{other}, rather than running the probe"
        | Expected.Refused -> stoppedSaying "CoreCLR refuses"
        | Expected.Undecided -> stoppedSaying "CoreCLR may refuse"
