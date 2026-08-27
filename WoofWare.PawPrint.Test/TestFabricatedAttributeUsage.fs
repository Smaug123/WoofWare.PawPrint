namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Emit
open System.Text
open NUnit.Framework

/// `[AttributeUsage]` applications whose blob no compiler will emit.
///
/// `sourcesPure/AttributeUsageInheritance.cs` covers what Roslyn can spell, which is only
/// well-formed blobs — so it exercises the parser's success path and nothing else. The refusal
/// path has no observer there at all: a guest cannot reach it, because a guest cannot contain a
/// malformed blob. `TypeBuilder.SetCustomAttribute(ConstructorInfo, byte[])` writes the bytes to
/// metadata without validating them, which is what makes the refusal reachable here.
[<TestFixture>]
module TestFabricatedAttributeUsage =

    let private serString (s : string) : byte array =
        let utf8 = Encoding.UTF8.GetBytes s
        Array.append [| byte utf8.Length |] utf8

    /// A well-formed `[AttributeUsage(AttributeTargets.All, Inherited = false)]` blob.
    let private inheritedFalse : byte array =
        Array.concat
            [
                [| 0x01uy ; 0x00uy |] // prolog
                [| 0xFFuy ; 0x7Fuy ; 0x00uy ; 0x00uy |] // AttributeTargets.All
                [| 0x01uy ; 0x00uy |] // one named arg
                [| 0x54uy ; 0x02uy |] // PROPERTY, BOOLEAN
                serString "Inherited"
                [| 0x00uy |]
            ]

    /// The same, but with a prolog of 0x0002. CoreCLR's parser requires exactly 0x0001, so this is
    /// refused, and the managed caller turns the refusal into a `CustomAttributeFormatException`.
    let private wrongProlog : byte array =
        Array.append [| 0x02uy ; 0x00uy |] (Array.sub inheritedFalse 2 (inheritedFalse.Length - 2))

    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "RawUsage", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "RawUsage"

        let usageCtor =
            typeof<AttributeUsageAttribute>.GetConstructor [| typeof<AttributeTargets> |]

        let defineAttribute (name : string) (rawBlob : byte array) : Type =
            let attr =
                modul.DefineType (
                    name,
                    TypeAttributes.Public ||| TypeAttributes.Sealed ||| TypeAttributes.Class,
                    typeof<Attribute>
                )

            attr.DefineDefaultConstructor MethodAttributes.Public
            |> ignore<ConstructorBuilder>

            attr.SetCustomAttribute (usageCtor, rawBlob)
            attr.CreateType ()

        let goodBlob = defineAttribute "GoodBlobAttribute" inheritedFalse
        let badBlob = defineAttribute "BadBlobAttribute" wrongProlog

        let applied (t : Type) =
            CustomAttributeBuilder (t.GetConstructor Type.EmptyTypes, Array.empty)

        let baseType =
            modul.DefineType ("Base", TypeAttributes.Public ||| TypeAttributes.Class, typeof<obj>)

        baseType.SetCustomAttribute (applied goodBlob)
        baseType.SetCustomAttribute (applied badBlob)
        let baseType = baseType.CreateType ()

        let derived =
            modul.DefineType ("DerivedBare", TypeAttributes.Public ||| TypeAttributes.Class, baseType)

        derived.CreateType () |> ignore<Type>

        use image = new IO.MemoryStream ()
        builder.Save image
        image.ToArray ()

    let private driver =
        """
using System;
using System.Reflection;

public static class Driver
{
    public static int Main()
    {
        // A blob the parser refuses reaches the guest as a CustomAttributeFormatException, thrown
        // by CoreLib in managed code and so catchable by the guest's own handler.
        try
        {
            typeof(DerivedBare).GetCustomAttributes(typeof(BadBlobAttribute), true);
            return 1;
        }
        catch (CustomAttributeFormatException)
        {
        }

        // The well-formed blob beside it still parses, and its Inherited=false is honoured: the
        // base's application is not visible from the derived type.
        if (typeof(DerivedBare).GetCustomAttributes(typeof(GoodBlobAttribute), true).Length != 0)
        {
            return 2;
        }

        // ... though it is visible on the type that carries it.
        if (typeof(Base).GetCustomAttributes(typeof(GoodBlobAttribute), true).Length != 1)
        {
            return 3;
        }

        return 0;
    }
}
"""

    [<Test>]
    let ``a malformed AttributeUsage blob throws CustomAttributeFormatException`` () : unit =
        FabricatedGuest.run "RawUsage" (fabricate ()) "RawUsageDriver" driver 0
