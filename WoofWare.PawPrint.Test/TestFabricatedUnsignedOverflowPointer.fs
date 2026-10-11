namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework

/// `add.ovf.un` and `sub.ovf.un` (ECMA-335 III.3.2, III.3.68) with a byref operand, against the
/// real runtime. C# reaches these through `checked` pointer arithmetic, which it compiles to
/// `conv.i` and then the unsigned checked instruction.
///
/// Only the cases whose outcome does not depend on where the pointer is are run on both runtimes:
/// the difference of two byrefs into one array, which overflows exactly when the first is below
/// the second, and a move by zero. A nonzero move overflows on CoreCLR or not according to the
/// pointer's address, which PawPrint never decides, so PawPrint refuses it; the last test pins
/// that refusal.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedUnsignedOverflowPointer =

    /// On `U`, each over a `long[]`:
    /// * `Diff(a, i, j)`: `&a[i] sub.ovf.un &a[j]`, as a native int.
    /// * `AddThenLoad(a, i, n)`: `&a[i] add.ovf.un n`, then `ldind.i8` through the result.
    /// * `SubThenLoad(a, i, n)`: the same with `sub.ovf.un`.
    /// * `AddInt(n, i)` and `SubInt(n, i)`: a native int and an int32 under `add.ovf.un` and
    ///   `sub.ovf.un`. PawPrint holds a default native int as a null pointer, and that plus an
    ///   offset as a pointer of raw bits, so these reach its bit-pattern arms, where the int32 must
    ///   be zero-extended as CoreCLR does.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "U", typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule "U"

        let u =
            modul.DefineType ("U", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (returns : Type) (parameters : Type[]) : ILGenerator =
            let method =
                u.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.Static, returns, parameters)

            method.GetILGenerator ()

        let elementAddress (il : ILGenerator) (index : int16) : unit =
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Ldarg, index)
            il.Emit (OpCodes.Ldelema, typeof<int64>)

        let il =
            define "Diff" typeof<nativeint> [| typeof<int64[]> ; typeof<int> ; typeof<int> |]

        elementAddress il 1s
        elementAddress il 2s
        il.Emit OpCodes.Sub_Ovf_Un
        il.Emit OpCodes.Ret

        for name, opcode in [ "AddThenLoad", OpCodes.Add_Ovf_Un ; "SubThenLoad", OpCodes.Sub_Ovf_Un ] do
            let il =
                define name typeof<int64> [| typeof<int64[]> ; typeof<int> ; typeof<nativeint> |]

            elementAddress il 1s
            il.Emit OpCodes.Ldarg_2
            il.Emit opcode
            il.Emit OpCodes.Ldind_I8
            il.Emit OpCodes.Ret

        for name, opcode in [ "AddInt", OpCodes.Add_Ovf_Un ; "SubInt", OpCodes.Sub_Ovf_Un ] do
            let il = define name typeof<nativeint> [| typeof<nativeint> ; typeof<int> |]
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit opcode
            il.Emit OpCodes.Ret

        u.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each check returns its own nonzero code on failure, so a disagreement names the check.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    static int DiffRaises(long[] a, int i, int j)
    {
        try
        {
            U.Diff(a, i, j);
            return 0;
        }
        catch (Exception e) when (e.GetType() == typeof(OverflowException))
        {
            return 1;
        }
    }

    static int Raises(Func<nint> f)
    {
        try
        {
            f();
            return 0;
        }
        catch (Exception e) when (e.GetType() == typeof(OverflowException))
        {
            return 1;
        }
    }

    public static int Main(string[] args)
    {
        long[] a = { 10, 11, 12, 13 };
        nint[] defaults = new nint[1];
        nint zero = defaults[0];

        if ((long)U.Diff(a, 2, 0) != 16) return 1;
        if ((long)U.Diff(a, 3, 1) != 16) return 2;
        if ((long)U.Diff(a, 1, 1) != 0) return 3;
        if (DiffRaises(a, 0, 2) != 1) return 4;
        if (DiffRaises(a, 1, 3) != 1) return 5;

        if (U.AddThenLoad(a, 1, 0) != 11) return 6;
        if (U.SubThenLoad(a, 2, 0) != 12) return 7;

        // An int32 is zero-extended: -1 reads as 2^32 - 1.
        if ((long)U.AddInt(zero, -1) != 0xFFFFFFFFL) return 8;
        nint eight = U.AddInt(zero, 8);
        if ((long)U.AddInt(eight, -1) != 0x100000007L) return 9;
        if (Raises(() => U.SubInt(zero, -1)) != 1) return 10;
        if (Raises(() => U.SubInt(eight, 9)) != 1) return 11;
        if ((long)U.SubInt(eight, 8) != 0) return 12;

        return 0;
    }
}
"""

    [<Test>]
    let ``unsigned checked arithmetic on byrefs agrees with the real runtime where the address does not matter``
        ()
        : unit
        =
        FabricatedGuest.run "U" (fabricate ()) "UnsignedOverflowPointerDriver" driverSource 0

    [<TestCase("AddThenLoad", "add.ovf.un")>]
    [<TestCase("SubThenLoad", "sub.ovf.un")>]
    let ``a nonzero unsigned checked move of a byref is refused`` (method : string) (instruction : string) : unit =
        let driver =
            $"""
public static class Driver
{{
    public static int Main(string[] args)
    {{
        long[] a = {{ 10, 11, 12, 13 }};
        return (int)U.%s{method}(a, 1, (nint)8);
    }}
}}
"""

        match
            FabricatedGuest.runOnPawPrintBounded "U" (fabricate ()) "UnsignedOverflowRefusalDriver" driver 10_000_000L
        with
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran an address-dependent move, exiting %d{code}"
        | FabricatedOutcome.Failed e ->
            let rec messages (e : exn) : string list =
                if isNull e then
                    []
                else
                    e.Message :: messages e.InnerException

            messages e
            |> List.exists (fun m ->
                m.Contains $"refusing %s{instruction} of a managed pointer by 8"
                && m.Contains "depends on the pointer's address"
            )
            |> shouldEqual true
