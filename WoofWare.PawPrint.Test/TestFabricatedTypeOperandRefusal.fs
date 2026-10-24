namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// A metadata type operand naming a byref or `System.Void`, which every opcode that consumes a type
/// refuses with `InvalidProgramException` and `ldtoken` accepts: CoreCLR's `CEEInfo::resolveToken`
/// refuses both shapes for every token kind but `ldtoken`'s, as the JIT resolves the token. A pointer
/// is accepted. No compiler emits either shape; `ILGenerator.Emit` takes any `Type`.
///
/// CoreCLR refuses the method when it compiles it, PawPrint when it reaches the instruction (see
/// docs/divergences.md). Each fabricated method reaches its instruction on every run, with nothing
/// observable before it and no handler of its own, so that difference cannot show.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedTypeOperandRefusal =

    /// What the real runtime does with a fabricated method: return 1, or refuse it.
    type private Expected =
        | Returns
        | Refused

    let private voidType = typeof<Void>
    let private byrefType = typeof<int>.MakeByRefType ()
    let private pointerType = typeof<int>.MakePointerType ()

    /// Each opcode's body, over the type operand it is given. Every body leaves the stack empty, and
    /// the method then returns 1.
    let private shapes : (string * (ILGenerator -> Type -> unit)) list =
        let intLocal (il : ILGenerator) : LocalBuilder = il.DeclareLocal typeof<int>

        let intArray (il : ILGenerator) : unit =
            il.Emit OpCodes.Ldc_I4_1
            il.Emit (OpCodes.Newarr, typeof<int>)

        let boxedInt (il : ILGenerator) : unit =
            il.Emit OpCodes.Ldc_I4_7
            il.Emit (OpCodes.Box, typeof<int>)

        [
            "Isinst",
            fun il t ->
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Isinst, t)
                il.Emit OpCodes.Pop
            "Castclass",
            fun il t ->
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Castclass, t)
                il.Emit OpCodes.Pop
            "Box",
            fun il t ->
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Box, t)
                il.Emit OpCodes.Pop
            "Unbox",
            fun il t ->
                boxedInt il
                il.Emit (OpCodes.Unbox, t)
                il.Emit OpCodes.Pop
            "UnboxAny",
            fun il t ->
                boxedInt il
                il.Emit (OpCodes.Unbox_Any, t)
                il.Emit OpCodes.Pop
            "Newarr",
            fun il t ->
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Newarr, t)
                il.Emit OpCodes.Pop
            "Sizeof",
            fun il t ->
                il.Emit (OpCodes.Sizeof, t)
                il.Emit OpCodes.Pop
            "Initobj",
            fun il t ->
                il.Emit (OpCodes.Ldloca, intLocal il)
                il.Emit (OpCodes.Initobj, t)
            "Ldobj",
            fun il t ->
                il.Emit (OpCodes.Ldloca, intLocal il)
                il.Emit (OpCodes.Ldobj, t)
                il.Emit OpCodes.Pop
            "Stobj",
            fun il t ->
                il.Emit (OpCodes.Ldloca, intLocal il)
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Stobj, t)
            "Ldelema",
            fun il t ->
                intArray il
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Ldelema, t)
                il.Emit OpCodes.Pop
            // The prefix suppresses the element-type check, not the token's resolution.
            "ReadonlyLdelema",
            fun il t ->
                intArray il
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Readonly
                il.Emit (OpCodes.Ldelema, t)
                il.Emit OpCodes.Pop
            "Ldelem",
            fun il t ->
                intArray il
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Ldelem, t)
                il.Emit OpCodes.Pop
            "Stelem",
            fun il t ->
                intArray il
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Stelem, t)
            "Constrained",
            fun il t ->
                il.Emit (OpCodes.Ldloca, intLocal il)
                il.Emit (OpCodes.Constrained, t)
                il.Emit (OpCodes.Callvirt, typeof<obj>.GetMethod "GetHashCode")
                il.Emit OpCodes.Pop
            "Ldtoken",
            fun il t ->
                il.Emit (OpCodes.Ldtoken, t)
                il.Emit OpCodes.Pop
        ]

    /// Every fabricated method, by name, with what the real runtime does with it. `int` is each
    /// opcode's control; a pointer is a closed type that is not a byref, and is accepted where
    /// PawPrint implements the opcode over one.
    let private scenarios : (string * (ILGenerator -> Type -> unit) * Type * Expected) list =
        let pointerAccepted =
            set [ "Isinst" ; "Castclass" ; "Newarr" ; "Sizeof" ; "Ldtoken" ]

        [
            for name, emit in shapes do
                yield $"%s{name}_Int", emit, typeof<int>, Expected.Returns

                if pointerAccepted.Contains name then
                    yield $"%s{name}_Pointer", emit, pointerType, Expected.Returns

                let refusal =
                    if name = "Ldtoken" then
                        Expected.Returns
                    else
                        Expected.Refused

                yield $"%s{name}_Void", emit, voidType, refusal
                yield $"%s{name}_Byref", emit, byrefType, refusal
        ]

    /// `static class Probes`, with one `static int` method of no arguments per scenario.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "TypeOperandRefusal", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "TypeOperandRefusal"

        let probes =
            modul.DefineType ("Probes", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        for name, emit, operand, _ in scenarios do
            let il =
                probes
                    .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<int>, [||])
                    .GetILGenerator ()

            emit il operand
            il.Emit OpCodes.Ldc_I4_1
            il.Emit OpCodes.Ret

        probes.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each scenario returns its own code on failure and 0 on success, so a disagreement names the
    /// scenario: scenario `i` (from 0) answers `2i + 1` where it went the wrong way, and `2i + 2`
    /// where it threw something else, or a refusal whose message is not CoreCLR's.
    let private driverSource : string =
        let checks =
            scenarios
            |> List.mapi (fun i (name, _, _, expected) ->
                let helper =
                    match expected with
                    | Expected.Returns -> "Expect"
                    | Expected.Refused -> "ExpectRefused"

                $"        if ((r = %s{helper}(Probes.%s{name}, %d{2 * i + 1})) != 0) return r;"
            )
            |> String.concat "\n"

        $$"""
using System;

public static class Driver
{
    private static int Expect(Func<int> call, int code)
    {
        try
        {
            return call() == 1 ? 0 : code;
        }
        catch (Exception)
        {
            return code + 1;
        }
    }

    // CoreCLR's message is InvalidProgramException's default.
    private static int ExpectRefused(Func<int> call, int code)
    {
        try
        {
            call();
            return code;
        }
        catch (InvalidProgramException e)
        {
            return e.Message == new InvalidProgramException().Message ? 0 : code + 1;
        }
        catch (Exception)
        {
            return code + 1;
        }
    }

    public static int Main(string[] args)
    {
        int r;
{{checks}}
        return 0;
    }
}
"""

    /// The scenario a nonzero exit code of the driver names.
    let private describe (code : int) : string =
        match List.tryItem ((code - 1) / 2) scenarios with
        | Some (name, _, _, expected) ->
            let how =
                if code % 2 = 1 then
                    "went the wrong way"
                else
                    "threw something else"

            $"%s{name} (expected %A{expected}) %s{how}"
        | None -> $"exit code %d{code} names no scenario"

    [<Test>]
    let ``a byref or System.Void type operand is refused as on the real runtime`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "TypeOperandRefusal" (fabricate ()) "TypeOperandRefusalDriver" driverSource

        match onHost with
        | RealRuntimeResult.NormalExit 0 -> ()
        | RealRuntimeResult.NormalExit code -> failwith $"on the real runtime, %s{describe code}"
        | other -> failwith $"real runtime did not exit normally: %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Exited 0 -> ()
        | FabricatedOutcome.Exited code -> failwith $"on PawPrint, %s{describe code}"
        | FabricatedOutcome.Failed e -> raise (Exception ("PawPrint did not run the guest to completion", e))
