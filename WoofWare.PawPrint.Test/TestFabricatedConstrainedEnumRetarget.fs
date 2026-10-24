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
/// `Int32.GetHashCode`. No C# source spells any of these calls on an enum.
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
    /// * `StringHash(DayOfWeek)` calls `String.GetHashCode` on the argument.
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

        do
            let il =
                retarget.DefineMethod("StringHash", attributes, typeof<int>, [| typeof<DayOfWeek> |]).GetILGenerator ()

            il.Emit (OpCodes.Ldarga_S, 0uy)
            il.Emit (OpCodes.Constrained, typeof<DayOfWeek>)
            il.Emit (OpCodes.Callvirt, typeof<string>.GetMethod ("GetHashCode", Type.EmptyTypes))
            il.Emit OpCodes.Ret

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

    /// PawPrint does not raise where the type a `constrained.` call resolves against does not
    /// implement the interface at all: a static virtual is refused ("could not find static
    /// implementation"), and an instance method runs the interface's abstract declaration on the
    /// box. Un-park these once both raise as CoreCLR does.
    let private parkedUntilUnimplementedInterfaceRaises () : unit =
        ParkedOn.check
            EmulatedRuntime.supported
            "PawPrint does not raise where a constrained. call's type does not implement the interface at all"

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
        parkedUntilUnimplementedInterfaceRaises ()
        run "Retarget.WriteBig(DayOfWeek.Friday)" 201

    [<Test>]
    let ``a static virtual numbered GetHashCode's slot runs the underlying primitive's`` () : unit =
        run "Retarget.Zero()" 11

    [<Test>]
    let ``a static virtual numbered another slot is resolved on the enum, which does not implement it`` () : unit =
        parkedUntilUnimplementedInterfaceRaises ()
        run "Retarget.One()" 202

    [<Test>]
    let ``a pointer loaded to a static virtual numbered GetHashCode's slot is the underlying primitive's`` () : unit =
        run "Retarget.ZeroPointer()" 13

    [<Test>]
    let ``a pointer loaded to a static virtual numbered another slot is resolved on the enum`` () : unit =
        parkedUntilUnimplementedInterfaceRaises ()
        run "Retarget.OnePointer()" 202

    [<Test>]
    let ``an interface method in another slot which the enum implements is the enum's`` () : unit =
        // `Enum`'s formats "Friday", where `Int32`'s would format "5".
        run "Retarget.FormatLength(DayOfWeek.Friday)" 6

    [<Test>]
    let ``a class method in GetHashCode's slot runs the underlying primitive's GetHashCode`` () : unit =
        run "Retarget.StringHash(DayOfWeek.Friday)" 5
