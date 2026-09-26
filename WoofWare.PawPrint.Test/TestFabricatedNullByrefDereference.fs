namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Threading
open NUnit.Framework

/// A read or write through a null managed pointer, against the real runtime.
///
/// CoreCLR turns the hardware fault of a null dereference in managed code into a
/// `NullReferenceException` the guest can catch. Each fabricated method here is a single access
/// through its byref argument, which the driver supplies as `ref Unsafe.NullRef<T>()`, and the
/// driver catches `NullReferenceException` around the call. The `Ldnull` cases instead push a null
/// object reference where the address belongs, which hand-written IL can do and C# cannot.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedNullByrefDereference =

    /// `S`, a struct of two `int` fields `X` and `Y`, and `N`, a static class of
    /// one method per case, each named after the case.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "NullByrefDereference", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "NullByrefDereference"

        let s =
            modul.DefineType (
                "S",
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout,
                typeof<ValueType>
            )

        s.DefineField ("X", typeof<int>, FieldAttributes.Public) |> ignore<FieldBuilder>
        let y = s.DefineField ("Y", typeof<int>, FieldAttributes.Public)
        let sType = s.CreateType ()

        let n =
            modul.DefineType ("N", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let define (name : string) (returnType : Type) (parameters : Type[]) (emit : ILGenerator -> unit) : unit =
            let il = n.DefineMethod(name, attributes, returnType, parameters).GetILGenerator ()
            emit il
            il.Emit OpCodes.Ret

        let refInt = typeof<int>.MakeByRefType ()
        let refObj = typeof<obj>.MakeByRefType ()
        let refS = sType.MakeByRefType ()

        define
            "LdindI4"
            typeof<int>
            [| refInt |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldind_I4
            )

        define
            "StindI4"
            typeof<Void>
            [| refInt |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldc_I4_1
                il.Emit OpCodes.Stind_I4
            )

        define
            "LdindRef"
            typeof<obj>
            [| refObj |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldind_Ref
            )

        define
            "StindRef"
            typeof<Void>
            [| refObj |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldnull
                il.Emit OpCodes.Stind_Ref
            )

        define
            "Ldobj"
            typeof<int>
            [| refS |]
            (fun il ->
                il.DeclareLocal sType |> ignore<LocalBuilder>
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Ldobj, sType)
                il.Emit OpCodes.Stloc_0
                il.Emit (OpCodes.Ldloca_S, 0uy)
                il.Emit (OpCodes.Ldfld, y)
            )

        define
            "Stobj"
            typeof<Void>
            [| refS |]
            (fun il ->
                il.DeclareLocal sType |> ignore<LocalBuilder>
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldloc_0
                il.Emit (OpCodes.Stobj, sType)
            )



        define
            "Initobj"
            typeof<Void>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Initobj, sType)
            )

        define
            "Ldfld"
            typeof<int>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Ldfld, y)
            )

        define
            "Stfld"
            typeof<Void>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldc_I4_1
                il.Emit (OpCodes.Stfld, y)
            )

        define
            "LdfldaThenLdind"
            typeof<int>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Ldflda, y)
                il.Emit OpCodes.Ldind_I4
            )

        // Real .NET faults at the `ldflda` itself, not only at a later dereference of the address it
        // computes: these two never dereference it.
        define
            "LdfldaPop"
            typeof<Void>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Ldflda, y)
                il.Emit OpCodes.Pop
            )

        define
            "LdfldaConvU"
            typeof<unativeint>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Ldflda, y)
                il.Emit OpCodes.Conv_U
            )

        define
            "VolatileRead"
            typeof<int>
            [| refInt |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Call, typeof<Volatile>.GetMethod ("Read", [| refInt |]))
            )

        define
            "VolatileWrite"
            typeof<Void>
            [| refInt |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldc_I4_1
                il.Emit (OpCodes.Call, typeof<Volatile>.GetMethod ("Write", [| refInt ; typeof<int> |]))
            )

        define
            "LdnullLdindI4"
            typeof<int>
            [||]
            (fun il ->
                il.Emit OpCodes.Ldnull
                il.Emit OpCodes.Ldind_I4
            )


        define
            "LdnullVolatileRead"
            typeof<int>
            [||]
            (fun il ->
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Call, typeof<Volatile>.GetMethod ("Read", [| refInt |]))
            )

        define
            "LdnullLdfld"
            typeof<int>
            [||]
            (fun il ->
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Ldfld, y)
            )

        define
            "LdnullInitobj"
            typeof<Void>
            [||]
            (fun il ->
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Initobj, sType)
            )

        // `initobj` also takes an unmanaged address; these are the two ways a null one arrives.
        define
            "ConvUInitobj"
            typeof<Void>
            [| refS |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Conv_U
                il.Emit (OpCodes.Initobj, sType)
            )

        define
            "ZeroInitobj"
            typeof<Void>
            [||]
            (fun il ->
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Conv_U
                il.Emit (OpCodes.Initobj, sType)
            )

        n.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private nullInt = "ref Unsafe.NullRef<int>()"
    let private nullObj = "ref Unsafe.NullRef<object>()"
    let private nullS = "ref Unsafe.NullRef<S>()"

    /// Each case: the fabricated method, and the C# argument list the driver calls it with.
    let private cases : Map<string, string> =
        [
            "LdindI4", nullInt
            "StindI4", nullInt
            "LdindRef", nullObj
            "StindRef", nullObj
            "Ldobj", nullS
            "Stobj", nullS
            "Initobj", nullS
            "Ldfld", nullS
            "Stfld", nullS
            "LdfldaThenLdind", nullS
            "LdfldaPop", nullS
            "LdfldaConvU", nullS
            "VolatileRead", nullInt
            "VolatileWrite", nullInt
            "LdnullLdindI4", ""
            "LdnullVolatileRead", ""
            "LdnullLdfld", ""
            "LdnullInitobj", ""
            "ConvUInitobj", nullS
            "ZeroInitobj", ""
        ]
        |> Map.ofList

    let caseNames : string list = cases |> Map.keys |> List.ofSeq

    /// Exits 0 when the call raised `NullReferenceException`, and 1 when it returned.
    let private driverSource (name : string) (arguments : string) : string =
        $$"""
using System;
using System.Runtime.CompilerServices;

public static class Driver
{
    public static int Main(string[] args)
    {
        try
        {
            N.{{name}}({{arguments}});
        }
        catch (NullReferenceException)
        {
            return 0;
        }

        return 1;
    }
}
"""

    [<TestCaseSource(nameof caseNames)>]
    let ``a null byref dereference raises a catchable NullReferenceException`` (name : string) : unit =
        FabricatedGuest.run
            "NullByrefDereference"
            (fabricate ())
            $"NullByrefDereference%s{name}Driver"
            (driverSource name cases.[name])
            0
