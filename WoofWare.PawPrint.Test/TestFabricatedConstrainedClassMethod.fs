namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework

/// `constrained. V callvirt C::M`, where `V` is a value type and `M` an instance method of a class
/// `C`, against the real runtime. CoreCLR's `MethodTable::TryResolveConstraintMethodApprox`
/// (methodtable.cpp) resolves a virtual `M` by its slot, whichever class declares it: where `V`
/// itself declares the method in that slot, that method runs on `V`'s address, unboxed; otherwise
/// `V` is boxed, and the call runs on the box what it would run on any receiver, which for an `M`
/// that cannot be overridden is `M` itself, though the box is no instance of `C`. A non-virtual `M`
/// always boxes. No C# source spells any of these calls.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedConstrainedClassMethod =

    /// The classes whose methods the calls name, each with `ToString` returning its own name and
    /// `GetHashCode` returning `hash`, both with the method attributes `attributes`.
    let private classes : (string * TypeAttributes * MethodAttributes * int) list =
        let overriding =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

        [
            // Sealed, so neither method can be overridden.
            "Sealed", TypeAttributes.Sealed, overriding, 7
            "OpenFinal", TypeAttributes.Class, overriding ||| MethodAttributes.Final, 8
            "OpenVirt", TypeAttributes.Class, overriding, 9
            // Not virtual at all: methods of their own, which merely share the names.
            "Plain", TypeAttributes.Class, MethodAttributes.Public ||| MethodAttributes.HideBySig, 6
        ]

    /// `Probe`, a static class whose methods each make one `constrained.` call on their argument:
    ///
    /// * `S1` and `S2` are structs with one `int` field `X`; `S2` overrides `ToString`, returning
    ///   "S2", and `GetHashCode`, incrementing `X` and returning its new value; `ReadX`, not
    ///   virtual, returns `X`. `S1` overrides nothing.
    /// * `Sealed`, `OpenFinal`, `OpenVirt` and `Plain` are the classes `classes` describes.
    /// * `<C>ToString<V>(V)` returns what `C::ToString` makes of the argument, and `StringToString<V>`
    ///   does the same with `String::ToString`.
    /// * `<C>Hash<V>(V)` returns ten times what `C::GetHashCode` makes of the argument, plus the
    ///   argument's `X` afterwards, so that a call on the argument's own storage is told from a call
    ///   on a copy. `StringHashS2` does the same with `String::GetHashCode`.
    /// * `SealedToStringEnum(DayOfWeek)` returns what `Sealed::ToString` makes of the argument.
    /// * `HasFlagEnum(DayOfWeek, DayOfWeek)` calls `Enum::HasFlag`, which is not virtual, on its
    ///   first argument with its second.
    /// * `ReadXS2(S2)` returns what `S2::ReadX` makes of the argument, and `S2ToStringS1(S1)` what
    ///   `S2::ToString` makes of it.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Probe", typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule "Probe"

        let defineStruct (name : string) =
            let ty =
                modul.DefineType (
                    name,
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            ty, ty.DefineField ("X", typeof<int>, FieldAttributes.Public)

        let s1, s1X = defineStruct "S1"
        let s2, s2X = defineStruct "S2"

        let overriding =
            MethodAttributes.Public
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.HideBySig

        let s2ToString =
            s2.DefineMethod ("ToString", overriding, typeof<string>, Type.EmptyTypes)

        do
            let il = s2ToString.GetILGenerator ()

            il.Emit (OpCodes.Ldstr, "S2")
            il.Emit OpCodes.Ret

            let il =
                s2.DefineMethod("GetHashCode", overriding, typeof<int>, Type.EmptyTypes).GetILGenerator ()

            il.Emit OpCodes.Ldarg_0
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Ldfld, s2X)
            il.Emit OpCodes.Ldc_I4_1
            il.Emit OpCodes.Add
            il.Emit (OpCodes.Stfld, s2X)
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Ldfld, s2X)
            il.Emit OpCodes.Ret

        let readX =
            s2.DefineMethod (
                "ReadX",
                MethodAttributes.Public ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        do
            let il = readX.GetILGenerator ()
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Ldfld, s2X)
            il.Emit OpCodes.Ret

        let classes =
            classes
            |> List.map (fun (name, typeAttributes, methodAttributes, hash) ->
                let ty =
                    modul.DefineType (name, TypeAttributes.Public ||| typeAttributes, typeof<obj>)

                let toString =
                    ty.DefineMethod ("ToString", methodAttributes, typeof<string>, Type.EmptyTypes)

                let il = toString.GetILGenerator ()
                il.Emit (OpCodes.Ldstr, name)
                il.Emit OpCodes.Ret

                let getHashCode =
                    ty.DefineMethod ("GetHashCode", methodAttributes, typeof<int>, Type.EmptyTypes)

                let il = getHashCode.GetILGenerator ()
                il.Emit (OpCodes.Ldc_I4_S, sbyte hash)
                il.Emit OpCodes.Ret

                ty.DefineDefaultConstructor MethodAttributes.Public
                |> ignore<ConstructorBuilder>

                name, ty, (toString :> MethodInfo), (getHashCode :> MethodInfo)
            )

        let probe =
            modul.DefineType ("Probe", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let defineToString (name : string) (on : Type) (called : MethodInfo) : unit =
            let il =
                probe.DefineMethod(name, attributes, typeof<string>, [| on |]).GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Constrained, on)
            il.Emit (OpCodes.Callvirt, called)
            il.Emit OpCodes.Ret

        let defineHash (name : string) (on : Type) (x : FieldInfo) (called : MethodInfo) : unit =
            let il =
                probe.DefineMethod(name, attributes, typeof<int>, [| on |]).GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Constrained, on)
            il.Emit (OpCodes.Callvirt, called)
            il.Emit (OpCodes.Ldc_I4_S, 10y)
            il.Emit OpCodes.Mul
            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Ldfld, x)
            il.Emit OpCodes.Add
            il.Emit OpCodes.Ret

        let stringToString = typeof<string>.GetMethod ("ToString", Type.EmptyTypes)

        for name, _, toString, getHashCode in classes do
            for valueTypeName, valueType, x in [ "S1", s1, s1X ; "S2", s2, s2X ] do
                defineToString $"%s{name}ToString%s{valueTypeName}" valueType toString
                defineHash $"%s{name}Hash%s{valueTypeName}" valueType x getHashCode

        defineToString "StringToStringS1" s1 stringToString
        defineToString "StringToStringS2" s2 stringToString
        defineHash "StringHashS2" s2 s2X (typeof<string>.GetMethod ("GetHashCode", Type.EmptyTypes))

        do
            let _, _, sealedToString, _ =
                classes |> List.find (fun (name, _, _, _) -> name = "Sealed")

            defineToString "SealedToStringEnum" typeof<DayOfWeek> sealedToString

        do
            let il =
                probe
                    .DefineMethod("HasFlagEnum", attributes, typeof<bool>, [| typeof<DayOfWeek> ; typeof<DayOfWeek> |])
                    .GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit OpCodes.Ldarg_1
            il.Emit (OpCodes.Box, typeof<DayOfWeek>)
            il.Emit (OpCodes.Constrained, typeof<DayOfWeek>)
            il.Emit (OpCodes.Callvirt, typeof<Enum>.GetMethod "HasFlag")
            il.Emit OpCodes.Ret

        do
            let il =
                probe.DefineMethod("ReadXS2", attributes, typeof<int>, [| s2 :> Type |]).GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Constrained, s2)
            il.Emit (OpCodes.Callvirt, readX)
            il.Emit OpCodes.Ret

        defineToString "S2ToStringS1" s1 s2ToString

        s1.CreateType () |> ignore<Type>
        s2.CreateType () |> ignore<Type>

        for _, ty, _, _ in classes do
            ty.CreateType () |> ignore<Type>

        probe.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Exit with `call`, an `int`, or with a code for the `string` `call` returns, where
    /// `returnsString`: 1 to 6 for "S1", "S2", "Sealed", "OpenFinal", "OpenVirt" and "Plain"; 7
    /// where the "string" is in fact a boxed `S1`; 8 for "Friday"; 100 for anything else.
    let private driver (returnsString : bool) (call : string) : string =
        let body =
            if returnsString then
                $$"""
        object result = {{call}};
        if (result is S1) return 7;
        switch ((string)result)
        {
            case "S1": return 1;
            case "S2": return 2;
            case "Sealed": return 3;
            case "OpenFinal": return 4;
            case "OpenVirt": return 5;
            case "Plain": return 6;
            case "Friday": return 8;
            default: return 100;
        }
"""
            else
                $"        return %s{call};"

        $$"""
using System;

public static class Driver
{
    public static int Main(string[] args)
    {
{{body}}
    }
}
"""

    let private runToString (call : string) (expected : int) : unit =
        FabricatedGuest.run "Probe" (fabricate ()) "ProbeDriver" (driver true call) expected

    let private runHash (call : string) (expected : int) : unit =
        FabricatedGuest.run "Probe" (fabricate ()) "ProbeDriver" (driver false call) expected

    /// The reason PawPrint gives for refusing a call that runs a method on a receiver that is no
    /// instance of its class.
    let private mistyped : string = "which is no instance of"

    /// The reason PawPrint gives for refusing a call that runs a value type's method on a box of it.
    let private boxAsThis : string = "which reads it as the value type's address"

    let private shouldRefuse (reason : string) (onPawPrint : FabricatedOutcome) : unit =
        match onPawPrint with
        | FabricatedOutcome.Failed e ->
            let rec messages (e : exn) : string list =
                if isNull e then
                    []
                else
                    e.Message :: messages e.InnerException

            messages e
            |> List.exists (fun message -> message.Contains reason)
            |> shouldEqual true
        | FabricatedOutcome.Exited code -> failwith $"PawPrint ran the guest to exit code %d{code}"

    /// Run the driver on both runtimes, requiring the real runtime to exit with `expectedOnHost`,
    /// and PawPrint to refuse the call, whose body reads its receiver as what it is not.
    let private refused (reason : string) (returnsString : bool) (call : string) (expectedOnHost : int) : unit =
        let onHost, onPawPrint =
            FabricatedGuest.runOnBoth "Probe" (fabricate ()) "ProbeDriver" (driver returnsString call)

        onHost |> shouldEqual (RealRuntimeResult.NormalExit expectedOnHost)
        shouldRefuse reason onPawPrint

    // The value type declares the method in the slot of the method named, so that runs, unboxed.

    [<Test>]
    let ``a sealed class's override runs the value type's own method in its slot`` () : unit =
        runToString "Probe.SealedToStringS2(new S2())" 2

    [<Test>]
    let ``a sealed class's override runs the value type's own method on its own storage`` () : unit =
        runHash "Probe.SealedHashS2(new S2())" 11

    [<Test>]
    let ``a final override runs the value type's own method in its slot`` () : unit =
        runToString "Probe.OpenFinalToStringS2(new S2())" 2

    [<Test>]
    let ``a final override runs the value type's own method on its own storage`` () : unit =
        runHash "Probe.OpenFinalHashS2(new S2())" 11

    [<Test>]
    let ``an overridable method of an unrelated class runs the value type's own method in its slot`` () : unit =
        runToString "Probe.OpenVirtToStringS2(new S2())" 2

    [<Test>]
    let ``an overridable method of an unrelated class runs the value type's own method on its own storage`` () : unit =
        runHash "Probe.OpenVirtHashS2(new S2())" 11

    [<Test>]
    let ``String's ToString runs the value type's own method in its slot`` () : unit =
        runToString "Probe.StringToStringS2(new S2())" 2

    [<Test>]
    let ``String's GetHashCode runs the value type's own method on its own storage`` () : unit =
        runHash "Probe.StringHashS2(new S2())" 11

    // The value type inherits the method in that slot, so the box runs it.

    [<Test>]
    let ``an overridable method of an unrelated class runs what the box inherits`` () : unit =
        // `ValueType.ToString` gives the type's name.
        runToString "Probe.OpenVirtToStringS1(new S1())" 1

    [<Test>]
    let ``a method of a class the value type derives from that is not virtual runs on the box`` () : unit =
        runHash "Probe.HasFlagEnum(DayOfWeek.Friday, DayOfWeek.Monday) ? 1 : 2" 1

    // The box runs the method named, though it is no instance of that method's class, which
    // PawPrint refuses.

    [<Test>]
    let ``a sealed class's override runs itself on a box of a value type that inherits its slot`` () : unit =
        refused mistyped true "Probe.SealedToStringS1(new S1())" 3

    [<Test>]
    let ``a sealed class's override runs itself on a copy of a value type that inherits its slot`` () : unit =
        refused mistyped false "Probe.SealedHashS1(new S1())" 70

    [<Test>]
    let ``a final override runs itself on a box of a value type that inherits its slot`` () : unit =
        refused mistyped true "Probe.OpenFinalToStringS1(new S1())" 4

    [<Test>]
    let ``a final override runs itself on a copy of a value type that inherits its slot`` () : unit =
        refused mistyped false "Probe.OpenFinalHashS1(new S1())" 80

    [<Test>]
    let ``String's ToString runs itself on a box of a value type that inherits its slot, returning the box`` () : unit =
        refused mistyped true "Probe.StringToStringS1(new S1())" 7

    [<Test>]
    let ``a sealed class's override runs itself on a box of an enum, which inherits its slot from Enum`` () : unit =
        refused mistyped true "Probe.SealedToStringEnum(DayOfWeek.Friday)" 3

    [<Test>]
    let ``a non-virtual method runs itself on a box of a value type with no method of that name`` () : unit =
        refused mistyped true "Probe.PlainToStringS1(new S1())" 6

    [<Test>]
    let ``a non-virtual method runs itself on a box of a value type overriding a method of that name`` () : unit =
        refused mistyped true "Probe.PlainToStringS2(new S2())" 6

    [<Test>]
    let ``a non-virtual method runs itself on a copy of a value type overriding a method of that name`` () : unit =
        refused mistyped false "Probe.PlainHashS2(new S2())" 60

    [<Test>]
    let ``another value type's method is refused`` () : unit =
        // CoreCLR runs `S2.ToString` on the address of an `S1`. Measured on .NET 10, the process
        // then crashes (exit 139), so the real runtime is no oracle here.
        FabricatedGuest.runOnPawPrintBounded
            "Probe"
            (fabricate ())
            "ProbeDriver"
            (driver true "Probe.S2ToStringS1(new S1())")
            1_000_000L
        |> shouldRefuse mistyped

    [<Test>]
    let ``a value type's own non-virtual method runs on its box as this`` () : unit =
        // CoreCLR passes the box, where the method expects the value's address, so `X` reads the
        // box's method table pointer, which is not 0.
        refused boxAsThis false "Probe.ReadXS2(new S2()) == 0 ? 1 : 2" 2
