namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `div.un` and `rem.un` (ECMA-335 III.3.32, III.3.56) on one int32 operand and one native-int
/// operand, against the real runtime.
///
/// Neither C# nor F# ever emits that operand pair: both convert the int32 to the native width
/// themselves before the divide, so `sourcesPure/ArithmeticOperations.cs` only ever reaches the
/// same-width arms. The pair is where the interpreter has to widen the int32 itself, and the CLR
/// widens it by *sign* extension for every binary numeric instruction, `.un` suffix or not — the
/// suffix picks the division, not the widening. The fabricated assembly is four methods that are
/// literally `ldarg.0; ldarg.1; div.un; ret` (or `rem.un`), typed so that one argument is an
/// `int` and the other a `nint`, and the driver puts a negative int32 on each side in turn.
///
/// `TestNullaryIlOp` pins the arithmetic against a host `DynamicMethod` over many operand pairs;
/// this is the end-to-end half, which shows a guest's own `int` and `nint` values reach those
/// arms through argument loading and the real dispatch.
[<TestFixture>]
module TestFabricatedUnsignedDivision =

    /// `UDiv::DivUnIntNint(int, nint)`, `UDiv::DivUnNintInt(nint, int)`, and the `RemUn` pair,
    /// each a bare unsigned division over its two arguments, returning `nint`.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "UDiv", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "UDiv"

        let udiv =
            modul.DefineType ("UDiv", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (parameters : Type[]) (opcode : OpCode) : unit =
            let method =
                udiv.DefineMethod (
                    name,
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<nativeint>,
                    parameters
                )

            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_1
            il.Emit opcode
            il.Emit OpCodes.Ret

        let intThenNint = [| typeof<int> ; typeof<nativeint> |]
        let nintThenInt = [| typeof<nativeint> ; typeof<int> |]

        define "DivUnIntNint" intThenNint OpCodes.Div_Un
        define "DivUnNintInt" nintThenInt OpCodes.Div_Un
        define "RemUnIntNint" intThenNint OpCodes.Rem_Un
        define "RemUnNintInt" nintThenInt OpCodes.Rem_Un

        udiv.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each row returns its own index on failure and 0 on success, so a disagreement names the
    /// row. The expected values assume a 64-bit native int, which is the only width PawPrint
    /// models; the real runtime checks them before PawPrint is asked.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    public static int Main(string[] args)
    {
        // A non-negative int32 widens the same way under either extension: these rows say the
        // methods divide at all, and divide unsigned.
        if ((long)UDiv.DivUnIntNint(7, (nint)2) != 3L) return 1;
        if ((long)UDiv.RemUnIntNint(7, (nint)2) != 1L) return 2;
        if ((long)UDiv.DivUnNintInt((nint)(-8L), 2) != 0x7FFFFFFFFFFFFFFCL) return 3;

        // int32 -1 as the dividend: sign-extended it is 2^64 - 1, zero-extended 2^32 - 1.
        if ((long)UDiv.DivUnIntNint(-1, (nint)2) != 0x7FFFFFFFFFFFFFFFL) return 4;
        if ((long)UDiv.RemUnIntNint(-1, (nint)0x100000001L) != 0L) return 5;

        // int32 -1 as the divisor, with a dividend of 2^32 so that the two widenings differ.
        if ((long)UDiv.DivUnNintInt((nint)0x100000000L, -1) != 0L) return 6;
        if ((long)UDiv.RemUnNintInt((nint)0x100000000L, -1) != 0x100000000L) return 7;

        return 0;
    }
}
"""

    [<Test>]
    let ``unsigned division of an int32 by a native int agrees with the real runtime`` () : unit =
        FabricatedGuest.run "UDiv" (fabricate ()) "UnsignedDivisionDriver" driverSource 0
