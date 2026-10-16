namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `newobj` of a szarray's constructors, and of a multi-dimensional array's lower-bound
/// constructor, against the real runtime.
///
/// C# constructs a szarray with `newarr`, and only ever calls a multi-dimensional array's
/// lengths-only constructor, so these tokens -- a `MemberReference` to `.ctor` whose parent is the
/// array's `TypeSpec` -- come from fabricated IL. Each fabricated method is `ldarg`s of its
/// `int` parameters, then `newobj` of one array constructor, then `ret`.
///
/// A token names the exact array type, so `newobj string[]::.ctor(int32)` builds a `string[]`,
/// unlike an `[UnsafeAccessor]` bound to the same constructor
/// (`sourcesPure/UnsafeAccessorArrayConstructorShapes.cs`).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedArrayConstructorNewobj =

    /// `NewArr::<name>(int32 × n) : object` for each constructor below.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "NewArr", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "NewArr"

        let newArr =
            modul.DefineType ("NewArr", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (arrayType : Type) (parameterCount : int) : unit =
            let parameters = Array.replicate parameterCount typeof<int>

            let ctor =
                arrayType.GetConstructor parameters
                |> Option.ofObj
                |> Option.defaultWith (fun () ->
                    failwith $"%O{arrayType} has no constructor of %d{parameterCount} int32s"
                )

            let il =
                newArr
                    .DefineMethod(name, MethodAttributes.Public ||| MethodAttributes.Static, typeof<obj>, parameters)
                    .GetILGenerator ()

            for i in 0 .. parameterCount - 1 do
                match i with
                | 0 -> il.Emit OpCodes.Ldarg_0
                | 1 -> il.Emit OpCodes.Ldarg_1
                | 2 -> il.Emit OpCodes.Ldarg_2
                | 3 -> il.Emit OpCodes.Ldarg_3
                | _ -> failwith "no constructor here takes more than four arguments"

            il.Emit (OpCodes.Newobj, ctor)
            il.Emit OpCodes.Ret

        define "IntSz" typeof<int[]> 1
        define "StringSz" typeof<string[]> 1
        define "IntJagged" typeof<int[][]> 2
        define "StringJagged" typeof<string[][]> 2
        define "LongJagged3" typeof<int64[][][]> 3
        define "IntRankTwoBounded" typeof<int[,]> 4
        define "StringRankTwoBounded" typeof<string[,]> 4

        newArr.CreateType () |> ignore<Type>

        use image = new IO.MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each row returns its own number on failure and 0 on success, so a disagreement names the
    /// row.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    private static bool Overflows(Func<object> make)
    {
        try { make(); return false; }
        catch (OverflowException) { return true; }
    }

    public static int Main(string[] args)
    {
        var intSz = (int[])NewArr.IntSz(3);
        if (intSz.Length != 3) return 1;
        if (NewArr.StringSz(2).GetType() != typeof(string[])) return 2;
        if (!Overflows(() => NewArr.IntSz(-1))) return 3;
        try { NewArr.IntSz(0x7FFFFFC8); return 4; }
        catch (OutOfMemoryException e) { if (e.Message != "Array dimensions exceeded supported range.") return 5; }

        var jagged = (int[][])NewArr.IntJagged(2, 3);
        if (jagged.Length != 2 || jagged[0].Length != 3 || jagged[1].Length != 3) return 10;
        if (ReferenceEquals(jagged[0], jagged[1])) return 11;
        var strings = (string[][])NewArr.StringJagged(1, 2);
        if (strings.GetType() != typeof(string[][]) || strings[0].GetType() != typeof(string[])) return 12;
        var deep = (long[][][])NewArr.LongJagged3(2, 2, 5);
        if (deep[1][1].Length != 5) return 13;
        if (((int[][])NewArr.IntJagged(0, -1)).Length != 0) return 14;
        if (!Overflows(() => NewArr.IntJagged(1, -1))) return 15;
        if (((long[][][])NewArr.LongJagged3(1, 0, -1))[0].Length != 0) return 16;

        var bounded = (int[,])NewArr.IntRankTwoBounded(0, 2, 0, 3);
        if (bounded.GetLength(0) != 2 || bounded.GetLength(1) != 3) return 20;
        if (NewArr.StringRankTwoBounded(0, 1, 0, 1).GetType() != typeof(string[,])) return 21;
        try { NewArr.IntRankTwoBounded(int.MaxValue, 2, 0, 1); return 22; }
        catch (ArgumentOutOfRangeException e) { if (e.ParamName != null) return 23; }
        if (!Overflows(() => NewArr.IntRankTwoBounded(0, 1, 5, -1))) return 24;

        return 0;
    }
}
"""

    [<Test>]
    let ``newobj of an array's constructors agrees with the real runtime`` () : unit =
        FabricatedGuest.run "NewArr" (fabricate ()) "NewArrDriver" driverSource 0
