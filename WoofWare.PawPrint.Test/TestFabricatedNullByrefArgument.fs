namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Emit
open System.Threading
open NUnit.Framework

/// `ldnull` passed where a method takes a `ref int`, against the real runtime.
///
/// C# cannot write this: a `ref` argument must be a location. Hand-written IL can, and CoreCLR
/// passes the null's bits, zero, as a null byref, which the callee faults on when it dereferences
/// it. So the call is entered normally, and the callee raises a catchable
/// `NullReferenceException`: here `Interlocked.Exchange`, an intrinsic primitive performed at its
/// own call to itself.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedNullByrefArgument =

    /// `NullByref::<name>() : int32`, each `ldnull` followed by the call `target` names.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "NullByref", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "NullByref"

        let nullByref =
            modul.DefineType ("NullByref", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (target : MethodInfo) (emitRest : ILGenerator -> unit) : unit =
            let il =
                nullByref
                    .DefineMethod(
                        name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<int>,
                        Type.EmptyTypes
                    )
                    .GetILGenerator ()

            il.Emit OpCodes.Ldnull
            emitRest il
            il.Emit (OpCodes.Call, target)
            il.Emit OpCodes.Ret

        let refInt = typeof<int>.MakeByRefType ()

        define
            "Exchange"
            (typeof<Interlocked>.GetMethod ("Exchange", [| refInt ; typeof<int> |]))
            (fun il -> il.Emit OpCodes.Ldc_I4_1)

        nullByref.CreateType () |> ignore<Type>
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
        try
        {
            NullByref.Exchange();
            return 1;
        }
        catch (NullReferenceException)
        {
        }

        return 0;
    }
}
"""

    [<Test>]
    let ``a null object reference passed as a byref faults in the callee`` () : unit =
        FabricatedGuest.run "NullByref" (fabricate ()) "NullByrefDriver" driver 0
