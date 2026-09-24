namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// Every way IL can reach a method with no body without dispatching on a receiver: a `calli`
/// through the entry point `RuntimeMethodHandle.GetFunctionPointer` hands out for it, and a `call`
/// or `ldftn` that names it. CoreCLR refuses them at different times and with different messages,
/// both as a `BadImageFormatException` carrying `COR_E_BADIMAGEFORMAT`:
///
///   - the `calli` is refused by the abstract method's prestub, when it is entered, with the
///     HRESULT's own text;
///   - a `call` or `ldftn` naming it is refused by the JIT, when it compiles the method holding the
///     instruction, with "Bad IL format." (`BFA_BAD_IL`). C# cannot spell either: it emits
///     `callvirt` for an abstract method and cannot take the address of one at all.
///
/// PawPrint raises each at the instruction. For the `calli` that is where the caller observes it
/// too. For `call` and `ldftn` it is later than the JIT: see docs/divergences.md. Each fabricated
/// method here puts the offending instruction first and has no handler of its own, so that
/// difference cannot show.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedAbstractMethodCall =

    let private staticMethod
        (owner : TypeBuilder)
        (name : string)
        (returnType : Type)
        (parameters : Type[])
        (emit : ILGenerator -> unit)
        : unit
        =
        let m =
            owner.DefineMethod (
                name,
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.HideBySig,
                returnType,
                parameters
            )

        let il = m.GetILGenerator ()
        emit il
        il.Emit OpCodes.Ret

    let private abstractAttributes : MethodAttributes =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.Abstract
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    let private overrideAttributes : MethodAttributes =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.Final

    /// `int name()` on `owner`, whose body is `ldc.i4 value; ret`.
    let private defineReturning
        (owner : TypeBuilder)
        (name : string)
        (attributes : MethodAttributes)
        (value : int)
        : MethodBuilder
        =
        let m = owner.DefineMethod (name, attributes, typeof<int>, [||])
        let il = m.GetILGenerator ()
        il.Emit (OpCodes.Ldc_I4, value)
        il.Emit OpCodes.Ret
        m

    /// The image:
    ///   - `abstract class Abs { abstract int A(); }` and `class Conc : Abs` returning 9;
    ///   - `interface I { int M(); static abstract int SA(); }` and `class Impl : I` returning 8 and 7;
    ///   - `static class Calls`, whose methods are described inline.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "AbstractCall", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "AbstractCall"

        let abs =
            modul.DefineType ("Abs", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Class)

        abs.DefineDefaultConstructor MethodAttributes.Family
        |> ignore<ConstructorBuilder>

        let absA = abs.DefineMethod ("A", abstractAttributes, typeof<int>, [||])
        abs.CreateType () |> ignore<Type>

        let conc =
            modul.DefineType ("Conc", TypeAttributes.Public ||| TypeAttributes.Class, abs)

        conc.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        let concA = defineReturning conc "A" overrideAttributes 9
        conc.CreateType () |> ignore<Type>

        let i =
            modul.DefineType ("I", TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        let iM = i.DefineMethod ("M", abstractAttributes, typeof<int>, [||])

        let iSA =
            i.DefineMethod (
                "SA",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                [||]
            )

        i.CreateType () |> ignore<Type>

        let impl = modul.DefineType ("Impl", TypeAttributes.Public ||| TypeAttributes.Class)
        impl.AddInterfaceImplementation i

        impl.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        let implM =
            defineReturning impl "M" (overrideAttributes ||| MethodAttributes.NewSlot) 8

        impl.DefineMethodOverride (implM, iM)

        let implSA =
            defineReturning
                impl
                "SA"
                (MethodAttributes.Public
                 ||| MethodAttributes.Static
                 ||| MethodAttributes.HideBySig)
                7

        impl.DefineMethodOverride (implSA, iSA)
        impl.CreateType () |> ignore<Type>

        let calls =
            modul.DefineType ("Calls", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        // `calli` with the receiver implicit in the call-site signature, as C#'s `delegate*` never
        // spells it.
        staticMethod
            calls
            "CalliHasThis"
            typeof<int>
            [| typeof<IntPtr> ; abs |]
            (fun il ->
                il.Emit OpCodes.Ldarg_1
                il.Emit OpCodes.Ldarg_0
                il.EmitCalli (OpCodes.Calli, CallingConventions.HasThis, typeof<int>, [||], null)
            )

        // The receiver as an explicit leading argument, as C# does spell it. The call site types it
        // `object` because `PersistedAssemblyBuilder`'s `EmitCalli` writes a type from this image
        // as a nil TypeDef token (measured on .NET 10), which PawPrint's signature decoder refuses.
        staticMethod
            calls
            "CalliExplicitReceiver"
            typeof<int>
            [| typeof<IntPtr> ; abs |]
            (fun il ->
                il.Emit OpCodes.Ldarg_1
                il.Emit OpCodes.Ldarg_0
                il.EmitCalli (OpCodes.Calli, CallingConventions.Standard, typeof<int>, [| typeof<obj> |], null)
            )

        staticMethod
            calls
            "CalliInterface"
            typeof<int>
            [| typeof<IntPtr> ; i |]
            (fun il ->
                il.Emit OpCodes.Ldarg_1
                il.Emit OpCodes.Ldarg_0
                il.EmitCalli (OpCodes.Calli, CallingConventions.HasThis, typeof<int>, [||], null)
            )

        staticMethod
            calls
            "CalliStatic"
            typeof<int>
            [| typeof<IntPtr> |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.EmitCalli (OpCodes.Calli, CallingConventions.Standard, typeof<int>, [||], null)
            )

        // The `calli` inside a `try` of its own: the runtime raises at the call, so this frame's
        // handler catches it and returns 45.
        staticMethod
            calls
            "CalliCaught"
            typeof<int>
            [| typeof<IntPtr> ; abs |]
            (fun il ->
                let result = il.DeclareLocal typeof<int>
                il.BeginExceptionBlock () |> ignore<Label>
                il.Emit OpCodes.Ldarg_1
                il.Emit OpCodes.Ldarg_0
                il.EmitCalli (OpCodes.Calli, CallingConventions.HasThis, typeof<int>, [||], null)
                il.Emit (OpCodes.Stloc, result)
                il.BeginCatchBlock typeof<BadImageFormatException>
                il.Emit OpCodes.Pop
                il.Emit (OpCodes.Ldc_I4, 45)
                il.Emit (OpCodes.Stloc, result)
                il.EndExceptionBlock ()
                il.Emit (OpCodes.Ldloc, result)
            )

        // `call` naming each abstract method.
        staticMethod
            calls
            "CallAbstract"
            typeof<int>
            [| abs |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Call, absA)
            )

        staticMethod
            calls
            "CallInterface"
            typeof<int>
            [| i |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Call, iM)
            )

        staticMethod calls "CallStaticAbstract" typeof<int> [||] (fun il -> il.Emit (OpCodes.Call, iSA))

        // `ldftn` naming each abstract method.
        staticMethod calls "LdftnAbstract" typeof<IntPtr> [||] (fun il -> il.Emit (OpCodes.Ldftn, absA))
        staticMethod calls "LdftnInterface" typeof<IntPtr> [||] (fun il -> il.Emit (OpCodes.Ldftn, iM))
        staticMethod calls "LdftnStaticAbstract" typeof<IntPtr> [||] (fun il -> il.Emit (OpCodes.Ldftn, iSA))

        // Controls. A `call` naming an override has a body to run; and a `constrained.` prefix
        // resolves a static abstract member to its implementation before `call` or `ldftn` sees it,
        // so the refusal has to come after that resolution rather than on the token.
        staticMethod
            calls
            "CallConcrete"
            typeof<int>
            [| conc |]
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Call, concA)
            )

        staticMethod
            calls
            "ConstrainedCall"
            typeof<int>
            [||]
            (fun il ->
                il.Emit (OpCodes.Constrained, impl)
                il.Emit (OpCodes.Call, iSA)
            )

        staticMethod
            calls
            "ConstrainedLdftnCalli"
            typeof<int>
            [||]
            (fun il ->
                il.Emit (OpCodes.Constrained, impl)
                il.Emit (OpCodes.Ldftn, iSA)
                il.EmitCalli (OpCodes.Calli, CallingConventions.Standard, typeof<int>, [||], null)
            )

        calls.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each scenario returns its own code on failure and 0 on success, so a disagreement names
    /// the scenario.
    let private driverSource : string =
        """
using System;

public static class Driver
{
    // COR_E_BADIMAGEFORMAT's own text, from the prestub. Only its numeral is machine-independent.
    private static int ExpectEntered(Func<int> call, int ifReturned, int ifWrong)
    {
        try
        {
            call();
            return ifReturned;
        }
        catch (BadImageFormatException e)
        {
            return e.HResult == unchecked((int)0x8007000B) && e.Message.Contains("0x8007000B") ? 0 : ifWrong;
        }
    }

    // BFA_BAD_IL, from the JIT.
    private static int ExpectBadIl(Func<int> call, int ifReturned, int ifWrong)
    {
        try
        {
            call();
            return ifReturned;
        }
        catch (BadImageFormatException e)
        {
            return e.HResult == unchecked((int)0x8007000B) && e.Message == "Bad IL format." ? 0 : ifWrong;
        }
    }

    private static IntPtr Fp(Type t, string name) => t.GetMethod(name).MethodHandle.GetFunctionPointer();

    private static int Run()
    {
        IntPtr a = Fp(typeof(Abs), "A");
        IntPtr m = Fp(typeof(I), "M");
        IntPtr sa = Fp(typeof(I), "SA");
        int r;

        r = ExpectEntered(() => Calls.CalliHasThis(a, new Conc()), 1, 2);
        if (r != 0) return r;
        r = ExpectEntered(() => Calls.CalliHasThis(a, null), 3, 4);
        if (r != 0) return r;
        r = ExpectEntered(() => Calls.CalliExplicitReceiver(a, new Conc()), 5, 6);
        if (r != 0) return r;
        r = ExpectEntered(() => Calls.CalliInterface(m, new Impl()), 7, 8);
        if (r != 0) return r;
        r = ExpectEntered(() => Calls.CalliStatic(sa), 9, 10);
        if (r != 0) return r;
        if (Calls.CalliCaught(a, new Conc()) != 45) return 11;

        r = ExpectBadIl(() => Calls.CallAbstract(new Conc()), 12, 13);
        if (r != 0) return r;
        r = ExpectBadIl(() => Calls.CallInterface(new Impl()), 14, 15);
        if (r != 0) return r;
        r = ExpectBadIl(() => Calls.CallStaticAbstract(), 16, 17);
        if (r != 0) return r;
        r = ExpectBadIl(() => (int)Calls.LdftnAbstract(), 18, 19);
        if (r != 0) return r;
        r = ExpectBadIl(() => (int)Calls.LdftnInterface(), 20, 21);
        if (r != 0) return r;
        r = ExpectBadIl(() => (int)Calls.LdftnStaticAbstract(), 22, 23);
        if (r != 0) return r;

        if (Calls.CallConcrete(new Conc()) != 9) return 24;
        if (Calls.ConstrainedCall() != 7) return 25;
        if (Calls.ConstrainedLdftnCalli() != 7) return 26;
        if (Calls.CalliHasThis(Fp(typeof(Conc), "A"), new Conc()) != 9) return 27;
        if (Calls.CalliInterface(Fp(typeof(Impl), "M"), new Impl()) != 8) return 28;

        return 0;
    }

    public static int Main(string[] args)
    {
        return Run();
    }
}
"""

    [<Test>]
    let ``calling an abstract method without dispatch agrees with the real runtime`` () : unit =
        FabricatedGuest.run "AbstractCall" (fabricate ()) "AbstractCallDriver" driverSource 0
