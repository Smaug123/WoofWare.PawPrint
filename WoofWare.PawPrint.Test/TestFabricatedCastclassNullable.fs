namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// `castclass` and `isinst` whose type token is a `Nullable<T>`, against the real runtime.
///
/// ECMA-335 III.4.3 and III.4.6 both say a `Nullable<T>` token is interpreted as a boxed `T`,
/// because a `Nullable<T>` never exists boxed as itself. Roslyn emits `isinst Nullable<T>` for
/// `o is T?` (`sourcesPure/IsinstNullable.cs` covers that route), but it never emits `castclass`
/// with a `Nullable<T>` token: `(T?) o` and `(T) o` for an unconstrained `T` both lower to
/// `unbox.any`. So the `castclass` half of the rule is only reachable from fabricated IL, and this
/// fixture supplies it with a closed token and with a generic-parameter token that the driver
/// instantiates at `Nullable<T>`.
[<TestFixture>]
module TestFabricatedCastclassNullable =

    /// `Cast::ChkNullableInt(object) : object` is `ldarg.0; castclass Nullable<int>; ret`,
    /// `Cast::IsNullableInt(object) : object` is `ldarg.0; isinst Nullable<int>; ret`, and
    /// `Cast::Chk<T>(object) : object` and `Cast::Is<T>(object) : object` are the same two
    /// instructions with `!!T` as the token.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Cast", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Cast"

        let cast =
            modul.DefineType ("Cast", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let body (method : MethodBuilder) (opcode : OpCode) (token : Type) : unit =
            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (opcode, token)
            il.Emit OpCodes.Ret

        let closed (name : string) (opcode : OpCode) : unit =
            let method = cast.DefineMethod (name, attributes, typeof<obj>, [| typeof<obj> |])
            body method opcode typeof<Nullable<int>>

        let generic (name : string) (opcode : OpCode) : unit =
            let method = cast.DefineMethod (name, attributes)
            let typeParameter = (method.DefineGenericParameters [| "T" |]).[0]
            method.SetReturnType typeof<obj>
            method.SetParameters [| typeof<obj> |]
            body method opcode typeParameter

        closed "ChkNullableInt" OpCodes.Castclass
        closed "IsNullableInt" OpCodes.Isinst
        generic "Chk" OpCodes.Castclass
        generic "Is" OpCodes.Isinst

        cast.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each check returns its own index on failure and 0 when every check passes, so a
    /// disagreement names the check.
    let private driverSource : string =
        """
using System;

public enum Colour
{
    Red = 0,
    Green = 1,
}

public struct Pair
{
    public int A;
    public int B;
}

public static class Driver
{
    private static bool Throws(Func<object> cast)
    {
        try
        {
            cast();
            return false;
        }
        catch (InvalidCastException)
        {
            return true;
        }
    }

    public static int Main(string[] args)
    {
        object boxedInt = 5;
        object boxedLong = 5L;
        object boxedEnum = Colour.Green;
        object boxedPair = new Pair { A = 1, B = 2 };
        object str = "hello";

        // castclass Nullable<int> passes a boxed int through unchanged: the very same reference,
        // still a boxed int.
        object chk = Cast.ChkNullableInt(boxedInt);
        if (!ReferenceEquals(chk, boxedInt)) return 1;
        if ((int) chk != 5) return 2;

        // Null passes through castclass.
        if (Cast.ChkNullableInt(null) != null) return 3;

        // Anything that is not a boxed int raises InvalidCastException.
        if (!Throws(() => Cast.ChkNullableInt(boxedLong))) return 4;
        if (!Throws(() => Cast.ChkNullableInt(boxedEnum))) return 5;
        if (!Throws(() => Cast.ChkNullableInt(boxedPair))) return 6;
        if (!Throws(() => Cast.ChkNullableInt(str))) return 7;

        // isinst Nullable<int> with a closed token.
        if (!ReferenceEquals(Cast.IsNullableInt(boxedInt), boxedInt)) return 8;
        if (Cast.IsNullableInt(boxedLong) != null) return 9;
        if (Cast.IsNullableInt(null) != null) return 10;

        // castclass !!T at T = Nullable<...>.
        if (!ReferenceEquals(Cast.Chk<int?>(boxedInt), boxedInt)) return 11;
        if (!ReferenceEquals(Cast.Chk<Colour?>(boxedEnum), boxedEnum)) return 12;
        if (!ReferenceEquals(Cast.Chk<Pair?>(boxedPair), boxedPair)) return 13;
        if (Cast.Chk<int?>(null) != null) return 14;
        if (!Throws(() => Cast.Chk<int?>(boxedLong))) return 15;
        if (!Throws(() => Cast.Chk<long?>(boxedInt))) return 16;
        if (!Throws(() => Cast.Chk<int?>(boxedEnum))) return 17;
        if (!Throws(() => Cast.Chk<Colour?>(boxedInt))) return 18;
        if (!Throws(() => Cast.Chk<int?>(str))) return 19;

        // isinst !!T at T = Nullable<...>.
        if (!ReferenceEquals(Cast.Is<int?>(boxedInt), boxedInt)) return 20;
        if (!ReferenceEquals(Cast.Is<Pair?>(boxedPair), boxedPair)) return 21;
        if (Cast.Is<int?>(boxedLong) != null) return 22;
        if (Cast.Is<Colour?>(boxedInt) != null) return 23;
        if (Cast.Is<int?>(str) != null) return 24;

        return 0;
    }
}
"""

    [<Test>]
    let ``castclass and isinst treat a Nullable token as its boxed payload type`` () : unit =
        FabricatedGuest.run "Cast" (fabricate ()) "CastclassNullableDriver" driverSource 0
