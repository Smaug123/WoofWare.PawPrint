namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework

/// The underlying primitive on the evaluation stack, stored where an enum over it is expected,
/// against the real runtime. ECMA-335 III.1.8.1.2.3 makes an enum and its underlying type
/// interchangeable on the stack, so IL stores the primitive straight into the enum's storage with
/// no conversion.
///
/// The enums are over `nint`, `nuint`, `bool` and `char`: PawPrint keeps their storage as a wrapper
/// around `value__` rather than flattening it to the primitive. No C# source declares such an enum.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedWrappedEnumStore =

    /// An underlying primitive: the name its enum and probes are spelled with, its type, and the
    /// instruction reading a value of it through an address, widened to an `int32`.
    type private Underlying =
        {
            Name : string
            Type : Type
            Load : ILGenerator -> unit
        }

    let private underlyings : Underlying list =
        [
            {
                Name = "NativeInt"
                Type = typeof<nativeint>
                Load =
                    fun il ->
                        il.Emit OpCodes.Ldind_I
                        il.Emit OpCodes.Conv_I4
            }
            {
                Name = "UNativeInt"
                Type = typeof<unativeint>
                Load =
                    fun il ->
                        il.Emit OpCodes.Ldind_I
                        il.Emit OpCodes.Conv_I4
            }
            {
                Name = "Boolean"
                Type = typeof<bool>
                Load = fun il -> il.Emit OpCodes.Ldind_U1
            }
            {
                Name = "Char"
                Type = typeof<char>
                Load = fun il -> il.Emit OpCodes.Ldind_U2
            }
        ]

    /// `Store`, a static class with, for each of `underlyings`, an enum `E_<name>` over it and these
    /// methods, each taking a value of the primitive, storing it as the enum, and returning what
    /// the enum's storage then holds, read through its address:
    ///
    /// * `<name>Local` stores it with `stloc` into a local of the enum.
    /// * `<name>Argument` passes it to a method whose parameter is the enum.
    /// * `<name>Return` gets it back from a method returning the enum, which returns the primitive.
    /// * `<name>Static` stores it with `stsfld` into a static field of the enum.
    /// * `<name>Element` stores it with `stelem` into an element of an array of the enum.
    let private fabricate () : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName "Store", typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule "Store"

        let store =
            modul.DefineType ("Store", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        for underlying in underlyings do
            let enumType =
                (modul.DefineEnum ("E_" + underlying.Name, TypeAttributes.Public, underlying.Type)).CreateType ()

            let probe (site : string) : ILGenerator =
                store
                    .DefineMethod(underlying.Name + site, attributes, typeof<int>, [| underlying.Type |])
                    .GetILGenerator ()

            // Read the enum in `local` through its address.
            let readLocal (il : ILGenerator) (local : LocalBuilder) : unit =
                il.Emit (OpCodes.Ldloca, local)
                underlying.Load il
                il.Emit OpCodes.Ret

            do
                let il = probe "Local"
                let local = il.DeclareLocal enumType
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Stloc, local)
                readLocal il local

            do
                let takesEnum =
                    store.DefineMethod (underlying.Name + "TakesEnum", attributes, typeof<int>, [| enumType |])

                let il = takesEnum.GetILGenerator ()
                il.Emit (OpCodes.Ldarga_S, 0uy)
                underlying.Load il
                il.Emit OpCodes.Ret

                let il = probe "Argument"
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Call, takesEnum)
                il.Emit OpCodes.Ret

            do
                let returnsEnum =
                    store.DefineMethod (underlying.Name + "ReturnsEnum", attributes, enumType, [| underlying.Type |])

                let il = returnsEnum.GetILGenerator ()
                il.Emit OpCodes.Ldarg_0
                il.Emit OpCodes.Ret

                let il = probe "Return"
                let local = il.DeclareLocal enumType
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Call, returnsEnum)
                il.Emit (OpCodes.Stloc, local)
                readLocal il local

            do
                let field =
                    store.DefineField (
                        underlying.Name + "Field",
                        enumType,
                        FieldAttributes.Public ||| FieldAttributes.Static
                    )

                let il = probe "Static"
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Stsfld, field)
                il.Emit (OpCodes.Ldsflda, field)
                underlying.Load il
                il.Emit OpCodes.Ret

            do
                let il = probe "Element"
                let array = il.DeclareLocal (enumType.MakeArrayType ())
                il.Emit OpCodes.Ldc_I4_1
                il.Emit (OpCodes.Newarr, enumType)
                il.Emit (OpCodes.Stloc, array)
                il.Emit (OpCodes.Ldloc, array)
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Stelem, enumType)
                il.Emit (OpCodes.Ldloc, array)
                il.Emit OpCodes.Ldc_I4_0
                il.Emit (OpCodes.Ldelema, enumType)
                underlying.Load il
                il.Emit OpCodes.Ret

        store.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Each underlying primitive's probe argument, as C# spells it, and what reading it back gives.
    let private argumentOf (name : string) : string * int =
        match name with
        | "NativeInt" -> "(nint)5", 5
        | "UNativeInt" -> "(nuint)5", 5
        | "Boolean" -> "true", 1
        | "Char" -> "'A'", 65
        | other -> failwith $"no probe argument for %s{other}"

    [<TestCase("NativeInt", "Local")>]
    [<TestCase("NativeInt", "Argument")>]
    [<TestCase("NativeInt", "Return")>]
    [<TestCase("NativeInt", "Static")>]
    [<TestCase("NativeInt", "Element")>]
    [<TestCase("UNativeInt", "Local")>]
    [<TestCase("UNativeInt", "Argument")>]
    [<TestCase("UNativeInt", "Return")>]
    [<TestCase("UNativeInt", "Static")>]
    [<TestCase("UNativeInt", "Element")>]
    [<TestCase("Boolean", "Local")>]
    [<TestCase("Boolean", "Argument")>]
    [<TestCase("Boolean", "Return")>]
    [<TestCase("Boolean", "Static")>]
    [<TestCase("Boolean", "Element")>]
    [<TestCase("Char", "Local")>]
    [<TestCase("Char", "Argument")>]
    [<TestCase("Char", "Return")>]
    [<TestCase("Char", "Static")>]
    [<TestCase("Char", "Element")>]
    let ``the underlying primitive stored as an enum PawPrint keeps wrapped is the enum's value``
        (underlying : string)
        (site : string)
        : unit
        =
        let argument, expected = argumentOf underlying

        let driver =
            $"public static class Driver {{ public static int Main(string[] args) => Store.%s{underlying}%s{site}(%s{argument}); }}"

        FabricatedGuest.run "Store" (fabricate ()) "StoreDriver" driver expected
