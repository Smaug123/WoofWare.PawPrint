namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `ldlen` (ECMA-335 III.4.12) against the real runtime, with the raw result handed to consumers
/// that can see its width: the spec types it native unsigned int, CoreCLR's importer types it
/// int32, and the two differ once arithmetic wraps.
///
/// Roslyn mostly follows `ldlen` with `conv.i4`, so a compiled guest usually sees the length as
/// an int32 whatever the instruction itself pushed. The fabricated methods here hand the raw
/// result to a `ret` whose return type is `nuint`, a `stloc` into a `nint` local, comparisons
/// against a `nint` and an int32, and an `add` against an int32.
[<TestFixture>]
module TestFabricatedLdlen =

    /// `Len::AsNativeUInt(int[]) : nuint`, `Len::ThroughNativeIntLocal(int[]) : nint`,
    /// `Len::EqualsNativeInt(int[], nint) : bool`, `Len::LongerThanUnsigned(int[], int) : bool`,
    /// `Len::LongerThanUnsignedBranch(int[], int) : bool`, `Len::PlusInt32(int[], int) : nint` and
    /// `Len::PlusInt32Max(int[]) : nint`, each a bare `ldlen` over its first argument with no
    /// conversion after it; and `Len::LessThanZeroNativeInt(int) : bool` and
    /// `Len::ZeroNativeIntGreaterThan(int) : bool`, which compare an int32 against the zero of an
    /// untouched `nint` local.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Len", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Len"

        let len =
            modul.DefineType ("Len", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        // ldarg.0; ldlen; ret
        do
            let il =
                len.DefineMethod("AsNativeUInt", attributes, typeof<unativeint>, [| typeof<int[]> |]).GetILGenerator ()

            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit OpCodes.Ret

        // ldarg.0; ldlen; stloc.0; ldloc.0; ret, with local 0 a nint
        do
            let il =
                len
                    .DefineMethod("ThroughNativeIntLocal", attributes, typeof<nativeint>, [| typeof<int[]> |])
                    .GetILGenerator ()

            il.DeclareLocal typeof<nativeint> |> ignore<LocalBuilder>
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit OpCodes.Stloc_0
            il.Emit OpCodes.Ldloc_0
            il.Emit OpCodes.Ret

        // ldarg.0; ldlen; ldarg.1; ceq; ret
        do
            let il =
                len
                    .DefineMethod("EqualsNativeInt", attributes, typeof<bool>, [| typeof<int[]> ; typeof<nativeint> |])
                    .GetILGenerator ()

            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Ceq
            il.Emit OpCodes.Ret

        // ldarg.0; ldlen; ldarg.1; cgt.un; ret: the length against an int32, which is the operand
        // pair Roslyn's own `arr.Length != 0` produces, here with a non-constant int32.
        do
            let il =
                len
                    .DefineMethod("LongerThanUnsigned", attributes, typeof<bool>, [| typeof<int[]> ; typeof<int> |])
                    .GetILGenerator ()

            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Cgt_Un
            il.Emit OpCodes.Ret

        // ldarg.0; ldlen; ldarg.1; bgt.un taken; ldc.i4.0; ret; taken: ldc.i4.1; ret
        do
            let il =
                len
                    .DefineMethod(
                        "LongerThanUnsignedBranch",
                        attributes,
                        typeof<bool>,
                        [| typeof<int[]> ; typeof<int> |]
                    )
                    .GetILGenerator ()

            let taken = il.DefineLabel ()
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit OpCodes.Ldarg_1
            il.Emit (OpCodes.Bgt_Un, taken)
            il.Emit OpCodes.Ldc_I4_0
            il.Emit OpCodes.Ret
            il.MarkLabel taken
            il.Emit OpCodes.Ldc_I4_1
            il.Emit OpCodes.Ret

        // ldarg.0; ldlen; ldarg.1; add; conv.i; ret: arithmetic on the raw result, which CoreCLR
        // performs at int32 width because its importer types `ldlen` as TYP_INT.
        do
            let il =
                len
                    .DefineMethod("PlusInt32", attributes, typeof<nativeint>, [| typeof<int[]> ; typeof<int> |])
                    .GetILGenerator ()

            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit OpCodes.Ldarg_1
            il.Emit OpCodes.Add
            il.Emit OpCodes.Conv_I
            il.Emit OpCodes.Ret

        // ldarg.0; ldlen; ldc.i4 0x7fffffff; add; conv.i; ret: as `PlusInt32` with the int32 a
        // constant, so the sum of a non-empty array's length wraps.
        do
            let il =
                len.DefineMethod("PlusInt32Max", attributes, typeof<nativeint>, [| typeof<int[]> |]).GetILGenerator ()

            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldlen
            il.Emit (OpCodes.Ldc_I4, Int32.MaxValue)
            il.Emit OpCodes.Add
            il.Emit OpCodes.Conv_I
            il.Emit OpCodes.Ret

        // ldarg.0; ldloc.0; clt; ret, with local 0 a `nint` that is never stored to, so `ldloc`
        // reads the zero the runtime initialised it with. That zero is the shape a signed
        // comparison against an int32 must accept, not only the one `ldc.i4.0; conv.i` makes.
        do
            let il =
                len.DefineMethod("LessThanZeroNativeInt", attributes, typeof<bool>, [| typeof<int> |]).GetILGenerator ()

            il.DeclareLocal typeof<nativeint> |> ignore<LocalBuilder>
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldloc_0
            il.Emit OpCodes.Clt
            il.Emit OpCodes.Ret

        // ldloc.0; ldarg.0; cgt; ret: as `LessThanZeroNativeInt` with the operands the other way
        // round, so the native int is on the left.
        do
            let il =
                len
                    .DefineMethod("ZeroNativeIntGreaterThan", attributes, typeof<bool>, [| typeof<int> |])
                    .GetILGenerator ()

            il.DeclareLocal typeof<nativeint> |> ignore<LocalBuilder>
            il.Emit OpCodes.Ldloc_0
            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Cgt
            il.Emit OpCodes.Ret

        len.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Returns the index of the first check that fails, and 0 when every check passes, so a
    /// disagreement names the consumer that saw the wrong value.
    let private driverSource : string =
        """
public static class Driver
{
    public static int Main(string[] args)
    {
        int[] seven = new int[7];
        int[] empty = new int[0];

        if (Len.AsNativeUInt(seven) != (nuint)7) return 1;
        if (Len.AsNativeUInt(empty) != (nuint)0) return 2;

        if (Len.ThroughNativeIntLocal(seven) != (nint)7) return 3;
        if (Len.ThroughNativeIntLocal(empty) != (nint)0) return 4;

        if (!Len.EqualsNativeInt(seven, (nint)7)) return 5;
        if (Len.EqualsNativeInt(seven, (nint)6)) return 6;
        if (!Len.EqualsNativeInt(empty, (nint)0)) return 7;
        // The comparison is over the whole native int, so a value that agrees with the length
        // only in its low 32 bits is not equal to it.
        if (Len.EqualsNativeInt(seven, unchecked((nint)(7L | (1L << 32))))) return 8;

        if (!Len.LongerThanUnsigned(seven, 6)) return 9;
        if (Len.LongerThanUnsigned(seven, 7)) return 10;
        // A negative int32 is widened to a native int before the unsigned comparison, so it is
        // far above any length.
        if (Len.LongerThanUnsigned(seven, -1)) return 11;
        if (!Len.LongerThanUnsignedBranch(seven, 6)) return 12;
        if (Len.LongerThanUnsignedBranch(empty, 0)) return 13;
        if (Len.LongerThanUnsignedBranch(seven, -1)) return 14;

        // Roslyn's own shapes for these: `ldlen; ldc.i4.0; ceq`, `ldlen; ldc.i4.0; cgt.un` and
        // `ldlen; brtrue`, with no `conv.i4` in any of them.
        if (seven.Length == 0) return 15;
        if (!(empty.Length == 0)) return 16;
        if (!(seven.Length != 0)) return 17;
        if (empty.Length > 0) return 18;
        if (empty.Length != 0) return 19;
        if (seven.Length == 0) return 20; else if (empty.Length != 0) return 21;

        // Arithmetic on the raw result is 32 bits wide: the sum wraps as an int32 before
        // `conv.i` sign-extends it to a native int.
        int[] one = new int[1];
        if (Len.PlusInt32(one, int.MaxValue) != unchecked((nint)(1 + int.MaxValue))) return 22;
        if (Len.PlusInt32(seven, -1) != (nint)6) return 23;
        if (Len.PlusInt32Max(one) != unchecked((nint)(1 + int.MaxValue))) return 24;
        if (Len.PlusInt32Max(empty) != (nint)int.MaxValue) return 25;

        // An int32 against the zero of an untouched `nint` local.
        if (!Len.LessThanZeroNativeInt(-1)) return 26;
        if (Len.LessThanZeroNativeInt(0)) return 27;
        if (Len.LessThanZeroNativeInt(1)) return 28;
        if (!Len.ZeroNativeIntGreaterThan(-1)) return 29;
        if (Len.ZeroNativeIntGreaterThan(0)) return 30;
        if (Len.ZeroNativeIntGreaterThan(1)) return 31;

        return 0;
    }
}
"""

    [<Test>]
    let ``ldlen's result widens to a native int at a sink and wraps as an int32 in arithmetic`` () : unit =
        FabricatedGuest.run "Len" (fabricate ()) "LdlenDriver" driverSource 0
