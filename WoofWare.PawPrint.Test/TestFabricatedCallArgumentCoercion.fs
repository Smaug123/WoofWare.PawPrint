namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Emit
open System.Threading
open NUnit.Framework

/// Integer arguments of the wrong width for their parameter, against the real runtime.
///
/// CoreCLR's importer converts an argument to its parameter's width at a call
/// (`impImplicitIorI4Cast`, importer.cpp). On a 64-bit target an int32 is one width and a native
/// int, an int64, an object reference and a byref are all the other: an int32 sign-extends into
/// a wider parameter, a wider value narrows into an int32 one, and `ldnull` is zero. C# never needs this; hand-written IL can
/// rely on it. Each fabricated method is one such call to an `Interlocked` method, whose
/// primitive runs at the method's own call to itself, after its frame has taken the arguments.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedCallArgumentCoercion =

    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Coercion", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Coercion"

        let coercion =
            modul.DefineType ("Coercion", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let intField =
            coercion.DefineField ("IntLocation", typeof<int>, FieldAttributes.Public ||| FieldAttributes.Static)

        let longField =
            coercion.DefineField ("LongLocation", typeof<int64>, FieldAttributes.Public ||| FieldAttributes.Static)

        let exchangeInt =
            typeof<Interlocked>.GetMethod ("Exchange", [| typeof<int>.MakeByRefType () ; typeof<int> |])

        let byteField =
            coercion.DefineField ("ByteLocation", typeof<byte>, FieldAttributes.Public ||| FieldAttributes.Static)

        let ushortField =
            coercion.DefineField ("UShortLocation", typeof<uint16>, FieldAttributes.Public ||| FieldAttributes.Static)

        let exchangeByte =
            typeof<Interlocked>.GetMethod ("Exchange", [| typeof<byte>.MakeByRefType () ; typeof<byte> |])

        let exchangeUShort =
            typeof<Interlocked>.GetMethod ("Exchange", [| typeof<uint16>.MakeByRefType () ; typeof<uint16> |])

        let exchangeLong =
            typeof<Interlocked>.GetMethod ("Exchange", [| typeof<int64>.MakeByRefType () ; typeof<int64> |])

        let addLong =
            typeof<Interlocked>.GetMethod ("Add", [| typeof<int64>.MakeByRefType () ; typeof<int64> |])

        let define (name : string) (returns : Type) (emit : ILGenerator -> unit) : unit =
            let il =
                coercion
                    .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, returns, Type.EmptyTypes)
                    .GetILGenerator ()

            emit il
            il.Emit OpCodes.Ret

        // A native-int 7 into `Exchange(ref int, int)`'s int32 value: the field becomes 7.
        define
            "ExchangeNativeIntValue"
            typeof<int>
            (fun il ->
                il.Emit (OpCodes.Ldsflda, intField)
                il.Emit (OpCodes.Ldc_I8, 7L)
                il.Emit OpCodes.Conv_I
                il.Emit (OpCodes.Call, exchangeInt)
                il.Emit OpCodes.Pop
                il.Emit (OpCodes.Ldsfld, intField)
            )

        // The same with a null location: the narrowing succeeds, and the primitive faults.
        define
            "ExchangeNativeIntValueIntoNull"
            typeof<int>
            (fun il ->
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Ldc_I8, 1L)
                il.Emit OpCodes.Conv_I
                il.Emit (OpCodes.Call, exchangeInt)
            )

        // An int32 -5 into `Add(ref long, long)`'s int64 addend: sign-extended, so the sum is -5.
        define
            "AddInt32Addend"
            typeof<int64>
            (fun il ->
                il.Emit (OpCodes.Ldsflda, longField)
                il.Emit (OpCodes.Ldc_I4, -5)
                il.Emit (OpCodes.Call, addLong)
            )

        // `ldnull` as `Exchange(ref long, long)`'s value: zero is stored, and the old value comes
        // back.
        define
            "ExchangeNullIntoLong"
            typeof<int64>
            (fun il ->
                il.Emit (OpCodes.Ldsflda, longField)
                il.Emit (OpCodes.Ldc_I8, 9L)
                il.Emit (OpCodes.Stind_I8)
                il.Emit (OpCodes.Ldsflda, longField)
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Call, exchangeLong)
                il.Emit OpCodes.Pop
                il.Emit (OpCodes.Ldsfld, longField)
            )

        // `ldnull` as both of its arguments: the value is zero, and the null location faults.
        define
            "ExchangeNullIntoNullLong"
            typeof<int64>
            (fun il ->
                il.Emit OpCodes.Ldnull
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Call, exchangeLong)
            )

        // `ldnull` as `Exchange(ref int, int)`'s value: narrowed to zero.
        define
            "ExchangeNullIntoInt"
            typeof<int>
            (fun il ->
                il.Emit (OpCodes.Ldsflda, intField)
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Call, exchangeInt)
                il.Emit OpCodes.Pop
                il.Emit (OpCodes.Ldsfld, intField)
            )

        // A native-int 0x1FF into `Exchange(ref byte, byte)` and 0x1FFFF into
        // `Exchange(ref ushort, ushort)`: each narrows to the parameter's width.
        for name, field, target, bits in
            [
                "ExchangeNativeIntIntoByte", byteField, exchangeByte, 0x1FFL
                "ExchangeNativeIntIntoUShort", ushortField, exchangeUShort, 0x1FFFFL
            ] do
            define
                name
                typeof<int>
                (fun il ->
                    il.Emit (OpCodes.Ldsflda, field)
                    il.Emit (OpCodes.Ldc_I8, bits)
                    il.Emit OpCodes.Conv_I
                    il.Emit (OpCodes.Call, target)
                    il.Emit OpCodes.Pop
                    il.Emit (OpCodes.Ldsfld, field)
                )

        // The same with a null location, which faults.
        for name, target in
            [
                "ExchangeNativeIntIntoNullByte", exchangeByte
                "ExchangeNativeIntIntoNullUShort", exchangeUShort
            ] do
            define
                name
                typeof<int>
                (fun il ->
                    il.Emit OpCodes.Ldnull
                    il.Emit (OpCodes.Ldc_I8, 1L)
                    il.Emit OpCodes.Conv_I
                    il.Emit (OpCodes.Call, target)
                )

        coercion.CreateType () |> ignore<Type>
        use image = new IO.MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private driver =
        """
using System;

public static class Driver
{
    public static int Main()
    {
        if (Coercion.ExchangeNativeIntValue() != 7) return 1;

        try
        {
            Coercion.ExchangeNativeIntValueIntoNull();
            return 2;
        }
        catch (NullReferenceException)
        {
        }

        if (Coercion.AddInt32Addend() != -5L) return 3;
        if (Coercion.ExchangeNullIntoLong() != 0L) return 4;

        try
        {
            Coercion.ExchangeNullIntoNullLong();
            return 5;
        }
        catch (NullReferenceException)
        {
        }

        if (Coercion.ExchangeNullIntoInt() != 0) return 6;
        if (Coercion.ExchangeNativeIntIntoByte() != 0xFF) return 7;
        if (Coercion.ExchangeNativeIntIntoUShort() != 0xFFFF) return 8;

        try
        {
            Coercion.ExchangeNativeIntIntoNullByte();
            return 9;
        }
        catch (NullReferenceException)
        {
        }

        try
        {
            Coercion.ExchangeNativeIntIntoNullUShort();
            return 10;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }
}
"""

    [<Test>]
    let ``an integer argument is converted to its parameter's width at the call`` () : unit =
        FabricatedGuest.run "Coercion" (fabricate ()) "CoercionDriver" driver 0
