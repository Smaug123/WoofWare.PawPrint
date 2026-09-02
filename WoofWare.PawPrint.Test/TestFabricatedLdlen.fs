namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `ldlen` (ECMA-335 III.4.12) against the real runtime, consuming the result as the native
/// unsigned int the spec says it is.
///
/// Roslyn and fsc follow every `ldlen` with `conv.i4`, so a compiled guest only ever sees the
/// length as an int32 and cannot tell what stack slot the instruction itself pushed into. The
/// fabricated methods here hand the raw result to the consumers that can: a `ret` whose return
/// type is `nuint`, a `stloc` into a `nint` local, and a `ceq` against a `nint` operand.
[<TestFixture>]
module TestFabricatedLdlen =

    /// `Len::AsNativeUInt(int[]) : nuint`, `Len::ThroughNativeIntLocal(int[]) : nint` and
    /// `Len::EqualsNativeInt(int[], nint) : bool`, each a bare `ldlen` over its first argument
    /// with no conversion after it.
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

        return 0;
    }
}
"""

    [<Test>]
    let ``ldlen's result is a native int wherever a consumer can see the difference`` () : unit =
        FabricatedGuest.run "Len" (fabricate ()) "LdlenDriver" driverSource 0
