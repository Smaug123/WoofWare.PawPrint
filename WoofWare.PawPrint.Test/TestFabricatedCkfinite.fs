namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `ckfinite` (ECMA-335 III.3.19) on float64 and float32 operands, against the real runtime.
///
/// C# never emits `ckfinite`, so the fabricated assembly holds methods that are literally
/// `ldarg.0; ckfinite; ret`, and one that runs the same inside a `catch (OverflowException)` of its
/// own. The spec names `ArithmeticException`; CoreCLR raises its subclass `OverflowException`, and
/// the driver requires that exact type. A finite operand, including a subnormal, a signed zero
/// and each type's largest value, comes back with the same bits; every NaN and infinity faults,
/// whatever its sign or NaN payload.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedCkfinite =

    /// `Ck::Double(float64)`, `Ck::Single(float32)`, each `ldarg.0; ckfinite; ret`, and
    /// `Ck::Caught(float64)`, which returns its argument through `ckfinite` or `-1.0` from its own
    /// `catch (OverflowException)`.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Ck", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Ck"

        let ck =
            modul.DefineType ("Ck", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (operand : Type) : ILGenerator =
            let method =
                ck.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.Static, operand, [| operand |])

            method.GetILGenerator ()

        for name, operand in [ "Double", typeof<float> ; "Single", typeof<float32> ] do
            let il = define name operand
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ckfinite
            il.Emit OpCodes.Ret

        let il = define "Caught" typeof<float>
        let result = il.DeclareLocal typeof<float>
        il.BeginExceptionBlock () |> ignore<Label>
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ckfinite
        il.Emit (OpCodes.Stloc, result)
        il.BeginCatchBlock typeof<OverflowException>
        il.Emit OpCodes.Pop
        il.Emit (OpCodes.Ldc_R8, -1.0)
        il.Emit (OpCodes.Stloc, result)
        il.EndExceptionBlock ()
        il.Emit (OpCodes.Ldloc, result)
        il.Emit OpCodes.Ret

        ck.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each check returns its own nonzero code on failure and 0 on success, so a disagreement
    /// names the check. The sweep draws bit patterns from a fixed splitmix64 sequence, forcing the
    /// exponent to all ones in half of them, since a uniform draw almost never makes a NaN or an
    /// infinity.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    static ulong state = 0x243F6A8885A308D3UL;

    static ulong Next()
    {
        state += 0x9E3779B97F4A7C15UL;
        ulong z = state;
        z = (z ^ (z >> 30)) * 0xBF58476D1CE4E5B9UL;
        z = (z ^ (z >> 27)) * 0x94D049BB133111EBUL;
        return z ^ (z >> 31);
    }

    // 0 if ckfinite passed the operand through unchanged, 1 if it raised OverflowException;
    // anything else propagates and fails the run.
    static int OnDouble(double x, out double result)
    {
        result = 0.0;
        try
        {
            result = Ck.Double(x);
            return 0;
        }
        catch (Exception e) when (e.GetType() == typeof(OverflowException))
        {
            return 1;
        }
    }

    static int OnSingle(float x, out float result)
    {
        result = 0.0f;
        try
        {
            result = Ck.Single(x);
            return 0;
        }
        catch (Exception e) when (e.GetType() == typeof(OverflowException))
        {
            return 1;
        }
    }

    static int CheckDouble(double x)
    {
        int raised = OnDouble(x, out double result);
        if (double.IsFinite(x))
        {
            if (raised != 0) return 1;
            if (BitConverter.DoubleToInt64Bits(result) != BitConverter.DoubleToInt64Bits(x)) return 2;
            return 0;
        }
        return raised == 1 ? 0 : 3;
    }

    static int CheckSingle(float x)
    {
        int raised = OnSingle(x, out float result);
        if (float.IsFinite(x))
        {
            if (raised != 0) return 1;
            if (BitConverter.SingleToInt32Bits(result) != BitConverter.SingleToInt32Bits(x)) return 2;
            return 0;
        }
        return raised == 1 ? 0 : 3;
    }

    public static int Main(string[] args)
    {
        double[] doubles =
        {
            0.0, -0.0, 1.0, -2.5, double.MaxValue, double.MinValue, double.Epsilon, -double.Epsilon,
            double.NaN, -double.NaN, double.PositiveInfinity, double.NegativeInfinity,
            BitConverter.Int64BitsToDouble(0x7FF0000000000001L),
            BitConverter.Int64BitsToDouble(unchecked((long)0xFFF8000000000001UL)),
        };
        for (int i = 0; i < doubles.Length; i++)
        {
            int r = CheckDouble(doubles[i]);
            if (r != 0) return 100 + 10 * i + r;
        }

        float[] singles =
        {
            0.0f, -0.0f, 1.0f, -2.5f, float.MaxValue, float.MinValue, float.Epsilon, -float.Epsilon,
            float.NaN, -float.NaN, float.PositiveInfinity, float.NegativeInfinity,
            BitConverter.Int32BitsToSingle(0x7F800001),
            BitConverter.Int32BitsToSingle(unchecked((int)0xFFC00001U)),
        };
        for (int i = 0; i < singles.Length; i++)
        {
            int r = CheckSingle(singles[i]);
            if (r != 0) return 300 + 10 * i + r;
        }

        for (int i = 0; i < 64; i++)
        {
            ulong bits = Next();
            if (i % 2 == 0) bits |= 0x7FF0000000000000UL;
            if (CheckDouble(BitConverter.Int64BitsToDouble((long)bits)) != 0) return 500 + i;

            uint singleBits = (uint)(bits >> 32);
            if (i % 2 == 0) singleBits |= 0x7F800000U;
            if (CheckSingle(BitConverter.Int32BitsToSingle((int)singleBits)) != 0) return 600 + i;
        }

        if (Ck.Caught(1.5) != 1.5) return 700;
        if (Ck.Caught(double.NaN) != -1.0) return 701;
        if (Ck.Caught(double.NegativeInfinity) != -1.0) return 702;

        return 0;
    }
}
"""

    [<Test>]
    let ``ckfinite agrees with the real runtime`` () : unit =
        FabricatedGuest.run "Ck" (fabricate ()) "CkfiniteDriver" driverSource 0
