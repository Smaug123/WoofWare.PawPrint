namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework

/// The `unaligned.` prefix (ECMA-335 III.2.3) against the real runtime.
///
/// No C# source can spell this on an instruction PawPrint implements. Roslyn emits `unaligned.`
/// in exactly two places: ahead of the `cpblk` that a non-byte-uniform `stackalloc` initializer
/// lowers to, and inside the `Unsafe.ReadUnaligned` / `WriteUnaligned` bodies — and those bodies
/// never execute, because both are `[Intrinsic]` and `Intrinsics.fs` answers them without
/// interpreting any IL. So the prefix is fabricated here instead.
///
/// What the rows are for. The prefix must leave the evaluation stack exactly as it found it, so
/// `StoreI4` and `InitBlk` have their operands sitting underneath it. It must consume its
/// one-byte alignment operand and no more, so the rows carry every alignment the CLI permits — 1,
/// 2 and 4, the set CoreCLR's importer enforces with `BADCODE("Alignment unaligned. must be 1, 2,
/// or 4")` — and 1 is the dangerous one, because 0x01 is itself the `break` opcode, so a prefix
/// that advanced two bytes rather than three would run it. And it must stack with `volatile.` in
/// either order: CoreCLR reaches the instruction a prefix applies to through
/// `impGetNonPrefixOpcode`, so `unaligned. volatile. ldind.i4` is as legal as the other way round.
///
/// Every byref the driver hands in is genuinely misaligned, since an aligned address would make
/// the prefix beside the point. `LoadI4NoPrefix` and `StoreI4NoPrefix` are the same IL without
/// the prefix, which is what makes "the prefix is inert" a claim this fixture states directly
/// rather than one inferred from agreement with the oracle.
[<TestFixture>]
module TestFabricatedUnaligned =

    /// A static class whose methods are `unaligned.`-prefixed IL over their arguments, plus the
    /// unprefixed twins of the first two.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Unal", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Unal"

        let unal =
            modul.DefineType ("Unal", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let define (name : string) (ret : Type) (args : Type list) (body : ILGenerator -> unit) : unit =
            let method = unal.DefineMethod (name, attributes, ret, List.toArray args)
            body (method.GetILGenerator ())

        // A `ref int` rather than a `ref byte`: `ldind.i4` through a byref whose element type is
        // `byte` reads one byte in PawPrint where the CLI reads four, which is a divergence of
        // `ldind` and has nothing to do with this prefix.
        let byrefInt = typeof<int>.MakeByRefType ()

        // `ldarg.0; unaligned. 1; ldind.i4; ret`
        define
            "LoadI4"
            typeof<int>
            [ byrefInt ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Unaligned, 1uy)
                il.Emit OpCodes.Ldind_I4
                il.Emit OpCodes.Ret
            )

        // The same body with the prefix removed.
        define
            "LoadI4NoPrefix"
            typeof<int>
            [ byrefInt ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldind_I4
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; ldarg.1; unaligned. 4; stind.i4; ret` — two operands sit beneath the prefix,
        // so one that disturbed the evaluation stack would store the wrong value, or store it
        // through the wrong address.
        define
            "StoreI4"
            typeof<Void>
            [ byrefInt ; typeof<int> ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldarg_1
                il.Emit (OpCodes.Unaligned, 4uy)
                il.Emit OpCodes.Stind_I4
                il.Emit OpCodes.Ret
            )

        define
            "StoreI4NoPrefix"
            typeof<Void>
            [ byrefInt ; typeof<int> ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldarg_1
                il.Emit OpCodes.Stind_I4
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; unaligned. 1; ldobj int32; ret` — the prefix ahead of an instruction that
        // carries a metadata token of its own.
        define
            "LoadObjI4"
            typeof<int>
            [ byrefInt ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Unaligned, 1uy)
                il.Emit (OpCodes.Ldobj, typeof<int>)
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; ldarg.1; unaligned. 2; stobj int32; ret`
        define
            "StoreObjI4"
            typeof<Void>
            [ byrefInt ; typeof<int> ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldarg_1
                il.Emit (OpCodes.Unaligned, 2uy)
                il.Emit (OpCodes.Stobj, typeof<int>)
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; volatile.; unaligned. 1; ldind.i4; ret` — the two prefixes stacked, in the
        // order ECMA-335 III.2.3 writes them.
        define
            "LoadI4VolatileFirst"
            typeof<int>
            [ byrefInt ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Volatile
                il.Emit (OpCodes.Unaligned, 1uy)
                il.Emit OpCodes.Ldind_I4
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; unaligned. 4; volatile.; ldind.i4; ret` — and in the other order.
        define
            "LoadI4UnalignedFirst"
            typeof<int>
            [ byrefInt ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Unaligned, 4uy)
                il.Emit OpCodes.Volatile
                il.Emit OpCodes.Ldind_I4
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; ldarg.1; ldarg.2; unaligned. 1; initblk; ret` — `initblk` is one of the eight
        // instructions III.2.3 lists as taking the prefix, and the only one of those PawPrint
        // reaches with real work to do behind it.
        define
            "InitBlk"
            typeof<Void>
            [ typeof<byte>.MakeByRefType () ; typeof<int> ; typeof<uint32> ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldarg_1
                il.Emit OpCodes.Ldarg_2
                il.Emit (OpCodes.Unaligned, 1uy)
                il.Emit OpCodes.Initblk
                il.Emit OpCodes.Ret
            )

        // `ldarg.0; unaligned. 8; ldind.i4; ret`. 8 is not one of the three alignments the CLI
        // permits, so this body is one the real runtime refuses to run: its importer answers
        // `BADCODE("Alignment unaligned. must be 1, 2, or 4")`, which surfaces to the guest as an
        // InvalidProgramException when the method is first jitted. It lives in the same assembly
        // as the lawful methods and is reached only by its own driver, so it costs the lawful
        // rows nothing.
        define
            "LoadI4BadAlignment"
            typeof<int>
            [ byrefInt ]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Unaligned, 8uy)
                il.Emit OpCodes.Ldind_I4
                il.Emit OpCodes.Ret
            )

        unal.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each scenario returns its own index on the first check it fails, and 0 when every check
    /// passes, so a disagreement names the scenario. Byte layouts go through `BitConverter` rather
    /// than being written out, so no row depends on the host's endianness.
    let private driverSource : string =
        """
using System;
using System.Runtime.CompilerServices;

public static class Driver
{
    private const int Sentinel = 0x11223344;

    // A `ref int` one byte into a byte[] is genuinely misaligned: the array's element 0 is at
    // least pointer-aligned, so element 1 is 1 mod 4.
    private static ref int Misaligned(byte[] arr, int offset)
    {
        return ref Unsafe.As<byte, int>(ref arr[offset]);
    }

    private static int LoadMisaligned()
    {
        byte[] arr = new byte[8];
        byte[] image = BitConverter.GetBytes(Sentinel);
        for (int i = 0; i < 4; i++) arr[1 + i] = image[i];

        if (Unal.LoadI4(ref Misaligned(arr, 1)) != Sentinel) return 1;
        // The prefix changed nothing: the same IL without it reads the same value.
        if (Unal.LoadI4NoPrefix(ref Misaligned(arr, 1)) != Sentinel) return 2;
        return 0;
    }

    private static int StoreMisaligned()
    {
        byte[] image = BitConverter.GetBytes(Sentinel);

        byte[] arr = new byte[8];
        Unal.StoreI4(ref Misaligned(arr, 3), Sentinel);
        if (arr[0] != 0 || arr[1] != 0 || arr[2] != 0) return 3;
        for (int i = 0; i < 4; i++)
        {
            if (arr[3 + i] != image[i]) return 4;
        }
        if (arr[7] != 0) return 5;

        byte[] twin = new byte[8];
        Unal.StoreI4NoPrefix(ref Misaligned(twin, 3), Sentinel);
        for (int i = 0; i < 8; i++)
        {
            if (twin[i] != arr[i]) return 6;
        }
        return 0;
    }

    // The same pair through `ldobj`/`stobj`, which carry a metadata token of their own.
    private static int ObjRoundTrip()
    {
        byte[] arr = new byte[8];
        Unal.StoreObjI4(ref Misaligned(arr, 1), Sentinel);
        if (Unal.LoadObjI4(ref Misaligned(arr, 1)) != Sentinel) return 7;
        if (arr[0] != 0 || arr[5] != 0 || arr[6] != 0 || arr[7] != 0) return 8;
        return 0;
    }

    // `volatile. unaligned.` and `unaligned. volatile.` are both legal, and mean the same thing.
    private static int StackedPrefixes()
    {
        byte[] arr = new byte[8];
        byte[] image = BitConverter.GetBytes(Sentinel);
        for (int i = 0; i < 4; i++) arr[2 + i] = image[i];

        if (Unal.LoadI4VolatileFirst(ref Misaligned(arr, 2)) != Sentinel) return 9;
        if (Unal.LoadI4UnalignedFirst(ref Misaligned(arr, 2)) != Sentinel) return 10;
        return 0;
    }

    // `initblk` under the prefix fills exactly the range it was given.
    private static int PrefixedInitBlk()
    {
        byte[] arr = new byte[8];
        Unal.InitBlk(ref arr[1], 0xAB, 3);

        if (arr[0] != 0) return 11;
        if (arr[1] != 0xAB || arr[2] != 0xAB || arr[3] != 0xAB) return 12;
        if (arr[4] != 0 || arr[5] != 0 || arr[6] != 0 || arr[7] != 0) return 13;
        return 0;
    }

    public static int Main(string[] args)
    {
        int r = LoadMisaligned();
        if (r != 0) return r;
        r = StoreMisaligned();
        if (r != 0) return r;
        r = ObjRoundTrip();
        if (r != 0) return r;
        r = StackedPrefixes();
        if (r != 0) return r;
        r = PrefixedInitBlk();
        if (r != 0) return r;
        return 0;
    }
}
"""

    /// Calls the one method whose alignment operand is out of range, and nothing else.
    let private badAlignmentDriverSource : string =
        """
using System;
using System.Runtime.CompilerServices;

public static class Driver
{
    public static int Main(string[] args)
    {
        byte[] arr = new byte[8];
        return Unal.LoadI4BadAlignment(ref Unsafe.As<byte, int>(ref arr[1]));
    }
}
"""

    [<Test>]
    let ``unaligned. agrees with the real runtime`` () : unit =
        FabricatedGuest.run "Unal" (fabricate ()) "UnalignedDriver" driverSource 0

    /// An alignment outside {1, 2, 4} makes the whole method body unrunnable on the real runtime,
    /// so PawPrint must not quietly execute it. PawPrint runs no IL verification pass and cannot
    /// raise the InvalidProgramException the CLI raises here, so it refuses instead — which is a
    /// divergence in *how* the guest dies, not in whether it does.
    [<Test>]
    let ``unaligned. with an alignment the CLI forbids is refused`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "Unal" (fabricate ()) "BadAlignmentDriver" badAlignmentDriverSource

        match onHost with
        | RealRuntimeResult.UnhandledException report -> report |> shouldContainText "System.InvalidProgramException"
        | other -> failwith $"real runtime was expected to refuse the body, but got %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Failed e -> e.ToString () |> shouldContainText "alignment must be 1, 2 or 4"
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran a body the CLI refuses, exiting %d{code}"
