namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework

/// `callvirt` whose operand is a MethodSpec wrapping a bare MethodDef declared on a generic type:
/// `callvirt !!0 G`1::Foo<string>()`, with nothing in the token saying which `G<X>` is meant.
///
/// Such a token names the declaring type's *typical* form, `G<T>` itself. No compiler emits it
/// (Roslyn and fsc both reference a generic type's method through a MemberReference with a
/// TypeSpec parent, which carries the instantiation), so the fabricated assembly is the only route
/// to it, and the real runtime is the oracle for what it means:
///
///   - where the call dispatches (a virtual, non-final target on a non-sealed type), the body that
///     runs is the receiver's runtime type's override under the receiver's own instantiation of
///     the declaring type -- whatever frame the call is made from;
///   - otherwise the call binds to the typical form itself, and the runtime refuses to run a
///     method that still contains generic variables: `InvalidOperationException`, after the null
///     check.
///
/// Each instantiation of `G` keeps its own `Marker` static and the readers return it, so binding
/// the wrong instantiation is observable as reading the wrong static. `G2<T1, T2>` has more type
/// parameters than its methods have method parameters, so an arm that indexes the *method*
/// generic arguments with a *type* parameter index runs off the end there rather than silently
/// picking a wrong instantiation.
[<TestFixture>]
module TestFabricatedCallvirtMethodSpecMethodDef =

    /// The builders for one fabricated generic type, needed to reference it from another.
    type private Fabricated =
        {
            Type : TypeBuilder
            TypeParameters : Type[]
            Ctor : ConstructorBuilder
            Marker : FieldBuilder
            Bar : MethodBuilder
            Foo : MethodBuilder
        }

    /// `int name<U>(parameters)` on `owner`, reading `owner<its own parameters>::marker` and
    /// ignoring its arguments.
    let private defineReader
        (owner : TypeBuilder)
        (typeParameters : Type[])
        (marker : FieldBuilder)
        (name : string)
        (attributes : MethodAttributes)
        (parameters : Type[])
        : MethodBuilder
        =
        let reader = owner.DefineMethod (name, attributes)

        reader.DefineGenericParameters [| "U" |]
        |> ignore<GenericTypeParameterBuilder[]>

        reader.SetReturnType typeof<int>
        reader.SetParameters parameters

        let il = reader.GetILGenerator ()
        il.Emit (OpCodes.Ldsfld, TypeBuilder.GetField (owner.MakeGenericType typeParameters, marker))
        il.Emit OpCodes.Ret
        reader

    /// `int name()` on `owner`, with a body of `emit` followed by `ret`.
    let private defineCaller (owner : TypeBuilder) (name : string) (emit : ILGenerator -> unit) : unit =
        let caller =
            owner.DefineMethod (name, MethodAttributes.Public ||| MethodAttributes.HideBySig, typeof<int>, [||])

        let il = caller.GetILGenerator ()
        emit il
        il.Emit OpCodes.Ret

    let private virtualAttributes : MethodAttributes =
        MethodAttributes.Public
        ||| MethodAttributes.Virtual
        ||| MethodAttributes.HideBySig
        ||| MethodAttributes.NewSlot

    /// A public generic class `name<typeParameterNames>` with:
    ///   - `public static int Marker`
    ///   - `public int Bar<U>()` and `public virtual int Foo<U>()`, each `ldsfld G<!T>::Marker; ret`
    ///     (a MemberReference with a TypeSpec parent, the shape a compiler would emit)
    ///   - `public int CallBar()` and `public int CallFoo()`, each `ldarg.0; callvirt X<string>; ret`
    ///     where the operand is a MethodSpec over the bare MethodDef of `X`.
    let private defineGeneric
        (modul : ModuleBuilder)
        (name : string)
        (typeParameterNames : string[])
        (typeAttributes : TypeAttributes)
        : Fabricated
        =
        let g =
            modul.DefineType (name, TypeAttributes.Public ||| TypeAttributes.Class ||| typeAttributes)

        let typeParameters =
            g.DefineGenericParameters typeParameterNames |> Array.map (fun p -> p :> Type)

        let ctor = g.DefineDefaultConstructor MethodAttributes.Public

        let marker =
            g.DefineField ("Marker", typeof<int>, FieldAttributes.Public ||| FieldAttributes.Static)

        // Non-virtual: `callvirt` on it is only a null check (ECMA-335 III.4.2), and the method
        // that runs is exactly the one the token resolved to.
        let bar =
            defineReader g typeParameters marker "Bar" (MethodAttributes.Public ||| MethodAttributes.HideBySig) [||]

        let foo = defineReader g typeParameters marker "Foo" virtualAttributes [||]

        defineCaller
            g
            "CallBar"
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Callvirt, bar.MakeGenericMethod [| typeof<string> |])
            )

        defineCaller
            g
            "CallFoo"
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Callvirt, foo.MakeGenericMethod [| typeof<string> |])
            )

        {
            Type = g
            TypeParameters = typeParameters
            Ctor = ctor
            Marker = marker
            Bar = bar
            Foo = foo
        }

    /// The image: `G<T>`, `G2<T1, T2>`, the sealed `S<T>`, `H<T> : G<T>` whose `Foo` is a final
    /// override, and the non-generic `Caller`, with the extra call sites described inline.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Gv", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Gv"

        let g = defineGeneric modul "G" [| "T" |] TypeAttributes.Class
        let gOfString = g.Type.MakeGenericType [| typeof<string> |]

        // `int Len<U>(T value)`: a signature that mentions the declaring type's parameter, which
        // has to be bound before the resolved override can be entered.
        let len =
            defineReader g.Type g.TypeParameters g.Marker "Len" virtualAttributes [| g.TypeParameters.[0] |]

        // The receiver is a `G<string>` made inside `G<T>`'s own frame: the body that runs is
        // the receiver's instantiation's, whatever the frame's is.
        defineCaller
            g.Type
            "CallFooOnStringReceiver"
            (fun il ->
                il.Emit (OpCodes.Newobj, TypeBuilder.GetConstructor (gOfString, g.Ctor))
                il.Emit (OpCodes.Callvirt, g.Foo.MakeGenericMethod [| typeof<string> |])
            )

        defineCaller
            g.Type
            "CallLenOnStringReceiver"
            (fun il ->
                il.Emit (OpCodes.Newobj, TypeBuilder.GetConstructor (gOfString, g.Ctor))
                il.Emit (OpCodes.Ldstr, "x")
                il.Emit (OpCodes.Callvirt, len.MakeGenericMethod [| typeof<string> |])
            )

        // Null receivers, through both a dispatching and a non-dispatching target.
        for name, target in [ "CallBarOnNull", g.Bar ; "CallFooOnNull", g.Foo ] do
            defineCaller
                g.Type
                name
                (fun il ->
                    il.Emit OpCodes.Ldnull
                    il.Emit (OpCodes.Callvirt, target.MakeGenericMethod [| typeof<string> |])
                )

        g.Type.CreateType () |> ignore<Type>

        let g2 = defineGeneric modul "G2" [| "T1" ; "T2" |] TypeAttributes.Class
        g2.Type.CreateType () |> ignore<Type>

        // Sealed: nothing can override `Foo`, so the call does not dispatch even though `Foo` is
        // virtual.
        let s = defineGeneric modul "S" [| "T" |] TypeAttributes.Sealed
        s.Type.CreateType () |> ignore<Type>

        // `H<T> : G<T>` with `Foo` overridden as `final`; `CallFooFinal` names H's own `Foo`.
        let h = modul.DefineType ("H", TypeAttributes.Public ||| TypeAttributes.Class)

        let hParameters =
            h.DefineGenericParameters [| "T" |] |> Array.map (fun p -> p :> Type)

        h.SetParent (g.Type.MakeGenericType hParameters)

        h.DefineDefaultConstructor MethodAttributes.Public |> ignore<ConstructorBuilder>

        let hFoo =
            h.DefineMethod (
                "Foo",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.Final
            )

        hFoo.DefineGenericParameters [| "U" |] |> ignore<GenericTypeParameterBuilder[]>

        hFoo.SetReturnType typeof<int>

        do
            let il = hFoo.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, 77)
            il.Emit OpCodes.Ret

        defineCaller
            h
            "CallFooFinal"
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Callvirt, hFoo.MakeGenericMethod [| typeof<string> |])
            )

        h.CreateType () |> ignore<Type>

        // A frame with no generics of its own, naming `G`'s typical `Foo` on a `G<int>` argument.
        let caller =
            modul.DefineType ("Caller", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        do
            let callFooOn =
                caller.DefineMethod (
                    "CallFooOn",
                    MethodAttributes.Public
                    ||| MethodAttributes.Static
                    ||| MethodAttributes.HideBySig,
                    typeof<int>,
                    [| g.Type.MakeGenericType [| typeof<int> |] |]
                )

            let il = callFooOn.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Callvirt, g.Foo.MakeGenericMethod [| typeof<string> |])
            il.Emit OpCodes.Ret

        caller.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Every MethodSpec in the image wraps a bare MethodDef. Reflection.Emit's choice of token
    /// shape is not something the fabrication above states explicitly, so this pins it: if a
    /// future runtime routed `MakeGenericMethod` on a `MethodBuilder` through a MemberReference
    /// instead, the fixture would be exercising the arm every compiler already exercises and
    /// passing vacuously.
    let private assertMethodSpecsWrapMethodDefs (image : byte[]) : unit =
        use pe = new PEReader (new MemoryStream (image))
        let reader = pe.GetMetadataReader ()
        let count = reader.GetTableRowCount TableIndex.MethodSpec
        count |> shouldBeGreaterThan 0

        for row = 1 to count do
            let spec =
                reader.GetMethodSpecification (MetadataTokens.MethodSpecificationHandle row)

            spec.Method.Kind |> shouldEqual HandleKind.MethodDefinition

    /// Each scenario returns its own code on failure and 0 on success, so a disagreement names
    /// the scenario.
    let private driverSource : string =
        """
using System;

public sealed class D : G<int>
{
    public override int Foo<U>()
    {
        return 100;
    }
}

public static class Driver
{
    // The prestub's refusal, IDS_EE_CODEEXECUTION_CONTAINSGENERICVAR.
    private const string NotInstantiated =
        "Could not execute the method because either the method itself or the containing type is not fully instantiated.";

    private static int ExpectNotInstantiated(Func<int> call, int ifReturned, int ifWrongMessage)
    {
        try
        {
            call();
            return ifReturned;
        }
        catch (InvalidOperationException e)
        {
            return e.Message == NotInstantiated ? 0 : ifWrongMessage;
        }
    }

    private static int ExpectNullReference(Func<int> call, int ifNot)
    {
        try
        {
            call();
            return ifNot;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    private static int Expect(int expected, Func<int> call, int ifNot)
    {
        return call() == expected ? 0 : ifNot;
    }

    private static int Run()
    {
        G<string>.Marker = 7;
        G<int>.Marker = 42;
        G2<int, bool>.Marker = 9;
        G2<string, string>.Marker = 3;
        S<int>.Marker = 55;
        int r;

        // Non-dispatching: the typical form is called, and refused.
        r = ExpectNotInstantiated(() => new G<int>().CallBar(), 1, 2);
        if (r != 0) return r;

        // Dispatching, receiver is `this`.
        r = Expect(42, () => new G<int>().CallFoo(), 3);
        if (r != 0) return r;

        // Dispatching to an override in a subclass.
        r = Expect(100, () => new D().CallFoo(), 4);
        if (r != 0) return r;

        // Dispatching to a receiver whose instantiation is not the calling frame's.
        r = Expect(7, () => new G<int>().CallFooOnStringReceiver(), 5);
        if (r != 0) return r;
        r = Expect(7, () => new G<int>().CallLenOnStringReceiver(), 6);
        if (r != 0) return r;

        // The null check comes before either outcome.
        r = ExpectNullReference(() => new G<int>().CallBarOnNull(), 7);
        if (r != 0) return r;
        r = ExpectNullReference(() => new G<int>().CallFooOnNull(), 8);
        if (r != 0) return r;

        // Virtual but non-dispatching: a sealed declaring type, and a final override.
        r = ExpectNotInstantiated(() => new S<int>().CallFoo(), 9, 10);
        if (r != 0) return r;
        r = ExpectNotInstantiated(() => new H<int>().CallFooFinal(), 11, 12);
        if (r != 0) return r;

        // A frame with no generics of its own.
        r = Expect(42, () => Caller.CallFooOn(new G<int>()), 13);
        if (r != 0) return r;

        // Two type parameters against one method parameter.
        r = ExpectNotInstantiated(() => new G2<int, bool>().CallBar(), 14, 15);
        if (r != 0) return r;
        r = Expect(9, () => new G2<int, bool>().CallFoo(), 16);
        if (r != 0) return r;

        return 0;
    }

    public static int Main(string[] args)
    {
        return Run();
    }
}
"""

    [<Test>]
    let ``callvirt of a MethodSpec over a MethodDef on a generic type agrees with the real runtime`` () : unit =
        let image = fabricate ()
        assertMethodSpecsWrapMethodDefs image
        FabricatedGuest.run "Gv" image "CallvirtMethodSpecMethodDefDriver" driverSource 0

    /// The same token shape over a generic *interface*'s method: `I<T>` with `int Foo<U>()`, the
    /// class `C : I<int>` implementing it, and a non-generic `Caller.CallFooOn(object)` that is
    /// `ldarg.0; callvirt I`1::Foo<string>; ret` with a MethodSpec over `I`'s own MethodDef.
    let private fabricateInterface () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Iv", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Iv"

        let i =
            modul.DefineType ("I", TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        i.DefineGenericParameters [| "T" |] |> ignore<GenericTypeParameterBuilder[]>

        let iFoo =
            i.DefineMethod (
                "Foo",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot
            )

        iFoo.DefineGenericParameters [| "U" |] |> ignore<GenericTypeParameterBuilder[]>

        iFoo.SetReturnType typeof<int>
        i.CreateType () |> ignore<Type>

        let c = modul.DefineType ("C", TypeAttributes.Public ||| TypeAttributes.Class)

        c.DefineDefaultConstructor MethodAttributes.Public |> ignore<ConstructorBuilder>

        let iOfInt = i.MakeGenericType [| typeof<int> |]
        c.AddInterfaceImplementation iOfInt

        let cFoo = c.DefineMethod ("Foo", virtualAttributes ||| MethodAttributes.Final)

        cFoo.DefineGenericParameters [| "U" |] |> ignore<GenericTypeParameterBuilder[]>

        cFoo.SetReturnType typeof<int>

        do
            let il = cFoo.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, 5)
            il.Emit OpCodes.Ret

        c.DefineMethodOverride (cFoo, TypeBuilder.GetMethod (iOfInt, iFoo))
        c.CreateType () |> ignore<Type>

        let caller =
            modul.DefineType ("Caller", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        do
            let callFooOn =
                caller.DefineMethod (
                    "CallFooOn",
                    MethodAttributes.Public
                    ||| MethodAttributes.Static
                    ||| MethodAttributes.HideBySig,
                    typeof<int>,
                    [| typeof<obj> |]
                )

            let il = callFooOn.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Callvirt, iFoo.MakeGenericMethod [| typeof<string> |])
            il.Emit OpCodes.Ret

        caller.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// PawPrint refuses the interface shape rather than answering it. The real runtime's answer
    /// is pinned alongside, so the refusal is measured against what a model of it would have to
    /// produce: a receiver implementing `I<int>` gets `EntryPointNotFoundException` from the
    /// typical `I<T>::Foo`, not a dispatch to `C.Foo`.
    [<Test>]
    let ``callvirt of a MethodSpec over a MethodDef on a generic interface is refused`` () : unit =
        let image = fabricateInterface ()
        assertMethodSpecsWrapMethodDefs image

        let driverSource =
            """
public static class Driver
{
    public static int Main(string[] args)
    {
        return Caller.CallFooOn(new C());
    }
}
"""

        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "Iv" image "CallvirtMethodSpecMethodDefInterfaceDriver" driverSource

        match onHost with
        | RealRuntimeResult.UnhandledException report ->
            report.Contains "System.EntryPointNotFoundException: Entry point was not found."
            |> shouldEqual true
        | other -> failwith $"real runtime did not raise: %O{other}"

        match onPawPrint with
        | FabricatedOutcome.Failed e ->
            let rec messages (e : exn) : string list =
                match e.InnerException with
                | null -> [ e.Message ]
                | inner -> e.Message :: messages inner

            match
                messages e
                |> List.tryFind (fun m -> m.StartsWith "TODO: callvirt of Foo on generic interface ")
            with
            | Some refusal ->
                refusal.Contains "which names the interface's typical instantiation"
                |> shouldEqual true
            | None -> failwith $"PawPrint failed for another reason: %A{messages e}"
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran the guest to completion with exit code %d{code}"
