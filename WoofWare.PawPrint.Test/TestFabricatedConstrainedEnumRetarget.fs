namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open NUnit.Framework
open WoofWare.PawPrint

/// `constrained.` calls and loads on the enum `DayOfWeek`, which CoreCLR resolves against `Int32`,
/// the enum's underlying primitive, wherever the method named holds the slot number that
/// `Object.GetHashCode` holds in `Object` (`CEEInfo::getCallInfo`, `ConstrainedType.resolve`),
/// against the real runtime.
///
/// `IBinaryInteger<int>.TryWriteLittleEndian` and `INumberBase<int>.get_Zero` hold that slot
/// number, so the call runs `Int32`'s method with the enum's address as `this`; their neighbours
/// `TryWriteBigEndian` and `get_One` do not, so the call is resolved on `DayOfWeek`, which
/// implements neither interface. `IFormattable.ToString` does not either, and `Enum` implements it,
/// formatting the enum's name where `Int32` would format its number. `String.GetHashCode` is a
/// class's method in that slot, which CoreCLR resolves by slot on `Int32`, giving
/// `Int32.GetHashCode`, even where the method is a sealed class's final override, which a call on a
/// box would run. A body overriding `Object.GetHashCode` by a MethodImpl under another name holds a
/// slot of its own, so a call naming it is not retargeted, and runs it on the box. No C# source
/// spells any of these calls on an enum.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFabricatedConstrainedEnumRetarget =

    // Named through reflection: F# rejects spelling an interface with static abstract members
    // outside a constraint position (FS3536).
    let private numericInterface (name : string) : Type =
        typeof<obj>.Assembly.GetType($"System.Numerics.%s{name}`1", true).MakeGenericType [| typeof<int> |]

    let private binaryInteger : Type = numericInterface "IBinaryInteger"
    let private numberBase : Type = numericInterface "INumberBase"

    // `Span<byte>`, which F# cannot name in `typeof`, a byref-like type being no type argument.
    let private spanOfBytes : Type =
        typeof<obj>.Assembly.GetType("System.Span`1", true).MakeGenericType [| typeof<byte> |]

    /// `Retarget`, a static class whose methods each make one `constrained.` call or load on
    /// `DayOfWeek`, or on a type variable the driver instantiates at `DayOfWeek`:
    ///
    /// * `WriteLittle(DayOfWeek)` and `WriteLittleOn<T>(T)` write the argument with
    ///   `IBinaryInteger<int>.TryWriteLittleEndian` into four bytes, and return ten times the first
    ///   byte plus the count written; `WriteBig(DayOfWeek)` does the same with `TryWriteBigEndian`.
    /// * `Zero()` and `One()` return `INumberBase<int>.Zero` and `.One` plus 11.
    /// * `ZeroPointer()` and `OnePointer()` load a pointer to the same methods and call it, returning
    ///   what that returns plus 13.
    /// * `FormatLength(DayOfWeek)` returns the length of what `IFormattable.ToString` makes of the
    ///   argument.
    /// * `StringHash(DayOfWeek)` calls `String.GetHashCode` on the argument, and `SealedHash(DayOfWeek)`
    ///   calls `Sealed.GetHashCode`, a final override returning 42 on a sealed class.
    ///   `RenamedHash(DayOfWeek)` calls `Renamed.Hash`, which overrides `Object.GetHashCode` by a
    ///   MethodImpl, returning 43.
    /// * `NativeIntHash(nint)`, `UNativeIntHash(nuint)`, `BooleanHash(bool)` and `CharHash(char)`
    ///   store the argument through the address of a local of an enum over its type, and call
    ///   `Object.GetHashCode` on that local.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Retarget", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Retarget"

        let retarget =
            modul.DefineType ("Retarget", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let attributes = MethodAttributes.Public ||| MethodAttributes.Static

        let write (il : ILGenerator) (on : Type) (called : MethodInfo) : unit =
            let bytes = il.DeclareLocal typeof<byte[]>
            let written = il.DeclareLocal typeof<int>
            il.Emit (OpCodes.Ldc_I4_4)
            il.Emit (OpCodes.Newarr, typeof<byte>)
            il.Emit (OpCodes.Stloc, bytes)
            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Ldloc, bytes)
            il.Emit (OpCodes.Newobj, spanOfBytes.GetConstructor [| typeof<byte[]> |])
            il.Emit (OpCodes.Ldloca, written)
            il.Emit (OpCodes.Constrained, on)
            il.Emit (OpCodes.Callvirt, called)
            il.Emit OpCodes.Pop
            il.Emit (OpCodes.Ldloc, bytes)
            il.Emit OpCodes.Ldc_I4_0
            il.Emit OpCodes.Ldelem_U1
            il.Emit (OpCodes.Ldc_I4_S, 10y)
            il.Emit OpCodes.Mul
            il.Emit (OpCodes.Ldloc, written)
            il.Emit OpCodes.Add
            il.Emit OpCodes.Ret

        let tryWriteLittleEndian = binaryInteger.GetMethod "TryWriteLittleEndian"
        let tryWriteBigEndian = binaryInteger.GetMethod "TryWriteBigEndian"

        for name, called in [ "WriteLittle", tryWriteLittleEndian ; "WriteBig", tryWriteBigEndian ] do
            let method =
                retarget.DefineMethod (name, attributes, typeof<int>, [| typeof<DayOfWeek> |])

            write (method.GetILGenerator ()) typeof<DayOfWeek> called

        do
            let method = retarget.DefineMethod ("WriteLittleOn", attributes)
            let t = (method.DefineGenericParameters [| "T" |]).[0]
            method.SetReturnType typeof<int>
            method.SetParameters [| t :> Type |]
            write (method.GetILGenerator ()) t tryWriteLittleEndian

        for name, property in [ "Zero", "Zero" ; "One", "One" ] do
            let getter = numberBase.GetProperty(property).GetMethod

            let il =
                retarget.DefineMethod(name, attributes, typeof<int>, Type.EmptyTypes).GetILGenerator ()

            il.Emit (OpCodes.Constrained, typeof<DayOfWeek>)
            il.Emit (OpCodes.Call, getter)
            il.Emit (OpCodes.Ldc_I4_S, 11y)
            il.Emit OpCodes.Add
            il.Emit OpCodes.Ret

            let il =
                retarget.DefineMethod(name + "Pointer", attributes, typeof<int>, Type.EmptyTypes).GetILGenerator ()

            il.Emit (OpCodes.Constrained, typeof<DayOfWeek>)
            il.Emit (OpCodes.Ldftn, getter)
            il.EmitCalli (OpCodes.Calli, CallingConventions.Standard, typeof<int>, Type.EmptyTypes, null)
            il.Emit (OpCodes.Ldc_I4_S, 13y)
            il.Emit OpCodes.Add
            il.Emit OpCodes.Ret

        do
            let il =
                retarget
                    .DefineMethod("FormatLength", attributes, typeof<int>, [| typeof<DayOfWeek> |])
                    .GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit OpCodes.Ldnull
            il.Emit OpCodes.Ldnull
            il.Emit (OpCodes.Constrained, typeof<DayOfWeek>)
            il.Emit (OpCodes.Callvirt, typeof<IFormattable>.GetMethod "ToString")
            il.Emit (OpCodes.Callvirt, typeof<string>.GetProperty("Length").GetMethod)
            il.Emit OpCodes.Ret

        let sealedClass =
            modul.DefineType ("Sealed", TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<obj>)

        let sealedHash =
            sealedClass.DefineMethod (
                "GetHashCode",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Final
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        do
            let il = sealedHash.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4_S, 42y)
            il.Emit OpCodes.Ret

        let renamedClass =
            modul.DefineType ("Renamed", TypeAttributes.Public ||| TypeAttributes.Sealed, typeof<obj>)

        // `Hash`, overriding `Object.GetHashCode` by a MethodImpl rather than by name.
        let renamedHash =
            renamedClass.DefineMethod (
                "Hash",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Final
                ||| MethodAttributes.HideBySig,
                typeof<int>,
                Type.EmptyTypes
            )

        do
            let il = renamedHash.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4_S, 43y)
            il.Emit OpCodes.Ret

        renamedClass.DefineMethodOverride (renamedHash, typeof<obj>.GetMethod ("GetHashCode", Type.EmptyTypes))

        for name, getHashCode in
            [
                "StringHash", typeof<string>.GetMethod ("GetHashCode", Type.EmptyTypes)
                "SealedHash", (sealedHash :> MethodInfo)
                "RenamedHash", (renamedHash :> MethodInfo)
            ] do
            let il =
                retarget.DefineMethod(name, attributes, typeof<int>, [| typeof<DayOfWeek> |]).GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Constrained, typeof<DayOfWeek>)
            il.Emit (OpCodes.Callvirt, getHashCode)
            il.Emit OpCodes.Ret

        // An enum over each primitive whose enums PawPrint keeps as a wrapper around `value__`
        // rather than flattening to the primitive.
        for name, underlying, store in
            [
                "NativeInt", typeof<nativeint>, OpCodes.Stind_I
                "UNativeInt", typeof<unativeint>, OpCodes.Stind_I
                "Boolean", typeof<bool>, OpCodes.Stind_I1
                "Char", typeof<char>, OpCodes.Stind_I2
            ] do
            let enumType = modul.DefineEnum ("E_" + name, TypeAttributes.Public, underlying)
            let enumType = enumType.CreateType ()

            let il =
                retarget.DefineMethod(name + "Hash", attributes, typeof<int>, [| underlying |]).GetILGenerator ()

            let local = il.DeclareLocal enumType
            il.Emit (OpCodes.Ldloca, local)
            il.Emit OpCodes.Ldarg_0
            il.Emit store
            il.Emit (OpCodes.Ldloca, local)
            il.Emit (OpCodes.Constrained, enumType)
            il.Emit (OpCodes.Callvirt, typeof<obj>.GetMethod ("GetHashCode", Type.EmptyTypes))
            il.Emit OpCodes.Ret

        sealedClass.CreateType () |> ignore<Type>
        renamedClass.CreateType () |> ignore<Type>
        retarget.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Exit with what `call` returns, or 201 where it raises `EntryPointNotFoundException`, or 202
    /// where it raises `AmbiguousImplementationException`: the two ways dispatch reports that the
    /// type a `constrained.` call resolves against supplies no method.
    let private driver (call : string) : string =
        $$"""
using System;
using System.Runtime;

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
        FabricatedGuest.run "Retarget" (fabricate ()) "RetargetDriver" (driver call) expected

    [<Test>]
    let ``an interface instance method in GetHashCode's slot runs the underlying primitive's, on the enum's own storage``
        ()
        : unit
        =
        // `Int32.TryWriteLittleEndian` writes Friday's 5 into four bytes.
        run "Retarget.WriteLittle(DayOfWeek.Friday)" 54

    [<Test>]
    let ``a type variable instantiated at an enum is retargeted as the enum is`` () : unit =
        run "Retarget.WriteLittleOn(DayOfWeek.Friday)" 54

    [<Test>]
    let ``an interface instance method in another slot is resolved on the enum, which does not implement it``
        ()
        : unit
        =
        run "Retarget.WriteBig(DayOfWeek.Friday)" 201

    [<Test>]
    let ``a static virtual numbered GetHashCode's slot runs the underlying primitive's`` () : unit =
        run "Retarget.Zero()" 11

    [<Test>]
    let ``a static virtual numbered another slot is resolved on the enum, which does not implement it`` () : unit =
        run "Retarget.One()" 202

    [<Test>]
    let ``a pointer loaded to a static virtual numbered GetHashCode's slot is the underlying primitive's`` () : unit =
        run "Retarget.ZeroPointer()" 13

    [<Test>]
    let ``a pointer loaded to a static virtual numbered another slot is resolved on the enum`` () : unit =
        run "Retarget.OnePointer()" 202

    [<Test>]
    let ``an interface method in another slot which the enum implements is the enum's`` () : unit =
        // `Enum`'s formats "Friday", where `Int32`'s would format "5".
        run "Retarget.FormatLength(DayOfWeek.Friday)" 6

    [<Test>]
    let ``a class method in GetHashCode's slot runs the underlying primitive's GetHashCode`` () : unit =
        run "Retarget.StringHash(DayOfWeek.Friday)" 5

    [<Test>]
    let ``a sealed class's final override in GetHashCode's slot runs the underlying primitive's GetHashCode``
        ()
        : unit
        =
        run "Retarget.SealedHash(DayOfWeek.Friday)" 5

    [<Test>]
    let ``a final override of GetHashCode by a MethodImpl holds a slot of its own, and runs on the box`` () : unit =
        run "Retarget.RenamedHash(DayOfWeek.Friday)" 43

    // `Int32`'s storage is an enum's own, but an enum over these primitives keeps its `value__`
    // apart from the primitive, so the primitive's method must read the enum's storage as its own.
    // Each hash is reduced modulo 251 to fit an exit code.

    [<Test>]
    let ``an enum over nint is retargeted to IntPtr's GetHashCode`` () : unit =
        run "(int)((uint)Retarget.NativeIntHash((nint)5) % 251)" 5

    [<Test>]
    let ``an enum over nuint is retargeted to UIntPtr's GetHashCode`` () : unit =
        run "(int)((uint)Retarget.UNativeIntHash((nuint)5) % 251)" 5

    [<Test>]
    let ``an enum over bool is retargeted to Boolean's GetHashCode`` () : unit =
        run "(int)((uint)Retarget.BooleanHash(true) % 251)" 1

    [<Test>]
    let ``an enum over char is retargeted to Char's GetHashCode`` () : unit =
        // 'A' | ('A' << 16) is 4259905.
        run "(int)((uint)Retarget.CharHash('A') % 251)" 184
