namespace WoofWare.PawPrint.Test

open NUnit.Framework

[<TestFixture>]
module TestCrossAssemblyEnumAttribute =

    /// `sourcesPure/CustomAttributeEnumArg.cs` covers an enum-valued attribute argument whose enum
    /// is declared in the same assembly as the attribute's constructor, so the constructor's
    /// signature names it as a `TypeDefn.FromDefinition`. When the enum lives in a *different*
    /// assembly the signature names it as a `TypeDefn.FromReference` instead, which is a separate
    /// arm of the shape resolver; nothing else in the suite would exercise it.
    [<Test>]
    let ``enum-valued attribute argument whose enum is defined in another assembly`` () : unit =
        {
            Assemblies =
                [
                    CrossAssemblySpec.library
                        "EnumAttrCross.EnumLib"
                        []
                        [
                            """
namespace EnumAttrCross;

// Deliberately not int32-underlying: a decoder that assumed a 4-byte width would read
// the wrong bytes and then desynchronise the cursor for the trailing argument below.
public enum Level : short
{
    Low = 1,
    High = -3000,
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "EnumAttrCross.Entry"
                        [ "EnumAttrCross.EnumLib" ]
                        [
                            """
using System;
using EnumAttrCross;

[AttributeUsage(AttributeTargets.Class)]
public class LevelAttribute : Attribute
{
    public LevelAttribute(Level level, int tail)
    {
        Level = level;
        Tail = tail;
    }

    public Level Level { get; }
    public int Tail { get; }
}

[Level(Level.High, 99)]
public class Decorated
{
}

class Program
{
    static int Main(string[] argv)
    {
        // The ctor's parameter type is a TypeRef into EnumAttrCross.EnumLib, so decoding this
        // blob requires resolving the enum across the assembly boundary to learn its width.
        var attr = (LevelAttribute)Attribute.GetCustomAttribute(typeof(Decorated), typeof(LevelAttribute));
        if (attr == null) return 1;
        if (attr.Level != Level.High) return 2;
        if (attr.Tail != 99) return 3;
        return 0;
    }
}
"""
                        ]
                ]
            EntryAssemblyName = "EnumAttrCross.Entry"
            ExpectedReturnCode = 0
        }
        |> CrossAssemblyHarness.runTest
