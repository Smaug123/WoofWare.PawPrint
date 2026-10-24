namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// Calls of an interface method on a type that does not implement the interface at all, against
/// the real runtime. No C# source spells any of them.
///
/// An instance method raises `EntryPointNotFoundException` from virtual stub dispatch, which finds
/// neither an entry in the receiver's dispatch map nor a default body among the interfaces the
/// receiver implements (`VirtualCallStubManager::Resolver`, virtualcallstub.cpp): so even an
/// interface method with a body of its own raises, its interface being none of the receiver's.
/// That holds for a `constrained.` call on a value type or a class, for a plain `callvirt` and for
/// `ldvirtftn`.
///
/// A static abstract method raises `AmbiguousImplementationException`, which `getCallInfo`
/// (jitinterface.cpp) plants at a `constrained.` call or `ldftn` of a static virtual that it cannot
/// resolve to a body, unless what it resolves is a reabstraction. A static virtual with a body of
/// its own runs that body instead (`MethodTable::ResolveVirtualStaticMethod`, methodtable.cpp).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedUnimplementedInterface =

    // Named through reflection: F# rejects spelling an interface with static abstract members
    // outside a constraint position (FS3536).
    let private additionOperators : Type =
        typeof<obj>.Assembly.GetType("System.Numerics.IAdditionOperators`3", true).MakeGenericType
            [| typeof<int> ; typeof<int> ; typeof<int> |]

    /// `IAdditionOperators<int, int, int>.op_CheckedAddition`, a static virtual whose own body is
    /// `left + right` through `Int32`'s `op_Addition`, which wraps; `Int32`'s implementation of it
    /// overflows instead.
    let private checkedAddition : MethodInfo =
        additionOperators.GetMethod "op_CheckedAddition"

    /// The image:
    ///
    /// * `interface IAbs { static abstract int SA(); int IA(); }` and
    ///   `interface IDef { int ID() => 8; }`;
    /// * `class Cls` and `struct Str`, which implement neither;
    /// * `static class Calls`, whose methods each make one call on `Cls`, `Str`, or a type variable,
    ///   as their names say: `Static*` call `IAbs.SA`, `StaticPointerOnClass` loads a pointer to it
    ///   and returns 100 without calling it, `OwnBody*` call `op_CheckedAddition` on their two
    ///   arguments, `OwnBodyPointerOnClass` through a pointer, `Instance*` call `IAbs.IA` and
    ///   `DefaultBody*` call `IDef.ID`, each through `constrained.`; `Plain(object)` and
    ///   `PlainDefaultBody(object)` call them with a plain `callvirt` on the argument, and
    ///   `VirtualPointer(object)` loads a pointer to `IAbs.IA` with `ldvirtftn` and returns 100.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Unimplemented", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Unimplemented"

        let interfaceAttributes =
            TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract

        let abs = modul.DefineType ("IAbs", interfaceAttributes)

        let sa =
            abs.DefineMethod (
                "SA",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        let ia =
            abs.DefineMethod (
                "IA",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        abs.CreateType () |> ignore<Type>

        let def = modul.DefineType ("IDef", interfaceAttributes)

        let id =
            def.DefineMethod (
                "ID",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.NewSlot
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        do
            let il = id.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4_8)
            il.Emit OpCodes.Ret

        def.CreateType () |> ignore<Type>

        let cls = modul.DefineType ("Cls", TypeAttributes.Public, typeof<obj>)
        let clsCtor = cls.DefineDefaultConstructor MethodAttributes.Public
        cls.CreateType () |> ignore<Type>

        let str =
            modul.DefineType (
                "Str",
                TypeAttributes.Public
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout,
                typeof<ValueType>
            )

        str.DefineField ("x", typeof<int>, FieldAttributes.Public)
        |> ignore<FieldBuilder>

        str.CreateType () |> ignore<Type>

        let calls =
            modul.DefineType ("Calls", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let define (name : string) (parameters : Type[]) (emit : ILGenerator -> unit) : unit =
            let il =
                calls.DefineMethod(name, attributes, typeof<int>, parameters).GetILGenerator ()

            emit il
            il.Emit OpCodes.Ret

        for name, on in [ "StaticOnClass", (cls :> Type) ; "StaticOnStruct", (str :> Type) ] do
            define
                name
                Type.EmptyTypes
                (fun il ->
                    il.Emit (OpCodes.Constrained, on)
                    il.Emit (OpCodes.Call, sa)
                )

        do
            let method = calls.DefineMethod ("StaticOn", attributes)
            let t = (method.DefineGenericParameters [| "T" |]).[0]
            method.SetReturnType typeof<int>
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Constrained, t)
            il.Emit (OpCodes.Call, sa)
            il.Emit OpCodes.Ret

        define
            "StaticPointerOnClass"
            Type.EmptyTypes
            (fun il ->
                il.Emit (OpCodes.Constrained, cls)
                il.Emit (OpCodes.Ldftn, sa)
                il.Emit OpCodes.Pop
                il.Emit (OpCodes.Ldc_I4, 100)
            )

        for name, on in [ "OwnBodyOnClass", (cls :> Type) ; "OwnBodyOnStruct", (str :> Type) ] do
            define
                name
                [| typeof<int> ; typeof<int> |]
                (fun il ->
                    il.Emit OpCodes.Ldarg_0
                    il.Emit OpCodes.Ldarg_1
                    il.Emit (OpCodes.Constrained, on)
                    il.Emit (OpCodes.Call, checkedAddition)
                )

        define
            "OwnBodyPointerOnClass"
            [| typeof<int> ; typeof<int> |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ldarg_1
                il.Emit (OpCodes.Constrained, cls)
                il.Emit (OpCodes.Ldftn, checkedAddition)

                il.EmitCalli (
                    OpCodes.Calli,
                    CallingConventions.Standard,
                    typeof<int>,
                    [| typeof<int> ; typeof<int> |],
                    null
                )
            )

        // `constrained. T callvirt` through the address of a local holding a `Cls` or a default
        // `Str`.
        let constrainedOn (il : ILGenerator) (on : Type) (called : MethodInfo) : unit =
            let local = il.DeclareLocal on

            if on = (cls :> Type) then
                il.Emit (OpCodes.Newobj, clsCtor)
                il.Emit (OpCodes.Stloc, local)

            il.Emit (OpCodes.Ldloca, local)
            il.Emit (OpCodes.Constrained, on)
            il.Emit (OpCodes.Callvirt, called)

        for name, on, called in
            [
                "InstanceOnClass", (cls :> Type), (ia :> MethodInfo)
                "InstanceOnStruct", (str :> Type), (ia :> MethodInfo)
                "DefaultBodyOnClass", (cls :> Type), (id :> MethodInfo)
                "DefaultBodyOnStruct", (str :> Type), (id :> MethodInfo)
            ] do
            define name Type.EmptyTypes (fun il -> constrainedOn il on called)

        do
            let method = calls.DefineMethod ("InstanceOn", attributes)
            let t = (method.DefineGenericParameters [| "T" |]).[0]
            method.SetReturnType typeof<int>
            method.SetParameters [| t :> Type |]
            let il = method.GetILGenerator ()
            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Constrained, t)
            il.Emit (OpCodes.Callvirt, ia)
            il.Emit OpCodes.Ret

        for name, called in [ "Plain", (ia :> MethodInfo) ; "PlainDefaultBody", (id :> MethodInfo) ] do
            define
                name
                [| typeof<obj> |]
                (fun il ->
                    il.Emit OpCodes.Ldarg_0
                    il.Emit (OpCodes.Callvirt, called)
                )

        define
            "VirtualPointer"
            [| typeof<obj> |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Ldvirtftn, ia)
                il.Emit OpCodes.Pop
                il.Emit (OpCodes.Ldc_I4, 100)
            )

        calls.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Exit with what `call` returns, or 201 where it raises `EntryPointNotFoundException`, or 202
    /// where it raises `AmbiguousImplementationException`. `Dynamic` and `DynamicStruct` implement
    /// `IDynamicInterfaceCastable`, answering every interface with `IAbsImpl`'s implementation.
    let private driver (call : string) : string =
        $$"""
using System;
using System.Runtime;
using System.Runtime.InteropServices;

[DynamicInterfaceCastableImplementation]
public interface IAbsImpl : IAbs
{
    int IAbs.IA() => 77;
}

public sealed class Dynamic : IDynamicInterfaceCastable
{
    public bool IsInterfaceImplemented(RuntimeTypeHandle interfaceType, bool throwIfNotImplemented) => true;

    public RuntimeTypeHandle GetInterfaceImplementation(RuntimeTypeHandle interfaceType) =>
        typeof(IAbsImpl).TypeHandle;
}

public struct DynamicStruct : IDynamicInterfaceCastable
{
    public bool IsInterfaceImplemented(RuntimeTypeHandle interfaceType, bool throwIfNotImplemented) => true;

    public RuntimeTypeHandle GetInterfaceImplementation(RuntimeTypeHandle interfaceType) =>
        typeof(IAbsImpl).TypeHandle;
}

public static class Driver
{
    public static int Main(string[] args)
    {
        try
        {
            return {{call}};
        }
        catch (EntryPointNotFoundException)
        {
            return 201;
        }
        catch (AmbiguousImplementationException)
        {
            return 202;
        }
    }
}
"""

    let private run (call : string) (expected : int) : unit =
        FabricatedGuest.run "Unimplemented" (fabricate ()) "UnimplementedDriver" (driver call) expected

    [<Test>]
    let ``a static abstract method called on a class that does not implement its interface raises`` () : unit =
        run "Calls.StaticOnClass()" 202

    [<Test>]
    let ``a static abstract method called on a value type that does not implement its interface raises`` () : unit =
        run "Calls.StaticOnStruct()" 202

    [<Test>]
    let ``a static abstract method called on a type variable instantiated at such a class raises`` () : unit =
        // CoreCLR runs `StaticOn<Cls>` as shared code, whose stub raises the same at the call.
        run "Calls.StaticOn<Cls>()" 202

    [<Test>]
    let ``a pointer loaded to a static abstract method on such a class raises at the load`` () : unit =
        run "Calls.StaticPointerOnClass()" 202

    // `int.MaxValue + 1` wraps through the interface's own body, where `Int32`'s would overflow.

    [<Test>]
    let ``a static virtual with a body of its own runs it on a class that does not implement its interface`` () : unit =
        run "Calls.OwnBodyOnClass(int.MaxValue, 1) == int.MinValue ? 7 : 3" 7

    [<Test>]
    let ``a static virtual with a body of its own runs it on a value type that does not implement its interface``
        ()
        : unit
        =
        run "Calls.OwnBodyOnStruct(int.MaxValue, 1) == int.MinValue ? 7 : 3" 7

    [<Test>]
    let ``a pointer loaded to a static virtual with a body of its own is to that body`` () : unit =
        run "Calls.OwnBodyPointerOnClass(int.MaxValue, 1) == int.MinValue ? 7 : 3" 7

    [<Test>]
    let ``an instance interface method called through constrained. on a class that does not implement it raises``
        ()
        : unit
        =
        run "Calls.InstanceOnClass()" 201

    [<Test>]
    let ``an instance interface method called through constrained. on a value type that does not implement it raises``
        ()
        : unit
        =
        run "Calls.InstanceOnStruct()" 201

    [<Test>]
    let ``an interface method with a body of its own raises on a class that does not implement its interface``
        ()
        : unit
        =
        run "Calls.DefaultBodyOnClass()" 201

    [<Test>]
    let ``an interface method with a body of its own raises on a value type that does not implement its interface``
        ()
        : unit
        =
        run "Calls.DefaultBodyOnStruct()" 201

    [<Test>]
    let ``an instance interface method called through constrained. on a type variable raises`` () : unit =
        run "Calls.InstanceOn(new Cls())" 201

    [<Test>]
    let ``an instance interface method called through constrained. on a type variable at a value type raises``
        ()
        : unit
        =
        run "Calls.InstanceOn(default(Str))" 201

    [<Test>]
    let ``a plain callvirt of an interface method on an object that does not implement it raises`` () : unit =
        run "Calls.Plain(new Cls())" 201

    [<Test>]
    let ``a plain callvirt of an interface method with a body of its own on such an object raises`` () : unit =
        run "Calls.PlainDefaultBody(new Cls())" 201

    [<Test>]
    let ``ldvirtftn of an interface method on an object that does not implement it raises`` () : unit =
        run "Calls.VirtualPointer(new Cls())" 201

    [<Test>]
    let ``an IDynamicInterfaceCastable receiver is asked for the implementation, which PawPrint refuses`` () : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth
                "Unimplemented"
                (fabricate ())
                "UnimplementedDriver"
                (driver "Calls.Plain(new Dynamic())")

        match onHost with
        | RealRuntimeResult.NormalExit 77 -> ()
        | other -> failwith $"expected the real runtime to run IAbsImpl's IA, returning 77; got %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Failed e when e.ToString().Contains "IDynamicInterfaceCastable" -> ()
        | other -> failwith $"expected PawPrint to refuse to ask an IDynamicInterfaceCastable receiver; got %O{other}"

    [<Test>]
    let ``a value type implementing IDynamicInterfaceCastable is not asked, and raises`` () : unit =
        // The runtime marks only a class as dynamically castable.
        run "Calls.Plain(new DynamicStruct())" 201
