using System;
using System.Collections.Generic;

class Enclosing
{
    public class Inner { }
}

class Top { }

public class Program
{
    public static int Main(string[] args)
    {
        // `FullName` and `AssemblyQualifiedName` ask `TypeString::AppendType` for FormatFullInst,
        // under which `AppendInst` renders every argument that is not a type variable with its
        // namespace and its assembly's display name, bracketed so that the display name's own
        // commas are not read as argument separators. `ToString()` asks for neither.
        string coreLib = typeof(object).Assembly.FullName;
        string here = typeof(Program).Assembly.FullName;

        // The CoreLib half comes from the running framework; pin its shape, since it is the
        // part a renderer is most likely to get wrong.
        if (!coreLib.StartsWith("System.Private.CoreLib, Version=")) return 1;
        if (!coreLib.EndsWith(", Culture=neutral, PublicKeyToken=7cec85d7bea7798e")) return 2;

        // A guest type, nested so that its own '+' must survive inside the brackets.
        if (typeof(List<Enclosing.Inner>).FullName
            != $"System.Collections.Generic.List`1[[Enclosing+Inner, {here}]]") return 10;
        if (typeof(List<Enclosing.Inner>).AssemblyQualifiedName
            != $"System.Collections.Generic.List`1[[Enclosing+Inner, {here}]], {coreLib}") return 11;
        // ToString() and Name are untouched.
        if (typeof(List<Enclosing.Inner>).ToString()
            != "System.Collections.Generic.List`1[Enclosing+Inner]") return 12;
        if (typeof(List<Enclosing.Inner>).Name != "List`1") return 13;

        // A CoreLib argument.
        if (typeof(List<string>).FullName
            != $"System.Collections.Generic.List`1[[System.String, {coreLib}]]") return 20;

        // Two arguments from two assemblies: each bracketed, joined by a bare comma.
        if (typeof(Dictionary<string, Top>).FullName
            != $"System.Collections.Generic.Dictionary`2[[System.String, {coreLib}],[Top, {here}]]") return 30;

        // An argument that is itself an instantiation renders its own arguments in full too:
        // FormatFullInst stays set as `AppendInst` recurses.
        if (typeof(List<List<int>>).FullName
            != $"System.Collections.Generic.List`1[[System.Collections.Generic.List`1[[System.Int32, {coreLib}]], {coreLib}]]") return 40;
        if (typeof(List<List<int>>).ToString()
            != "System.Collections.Generic.List`1[System.Collections.Generic.List`1[System.Int32]]") return 41;

        // An array argument: the element is rendered without an assembly, the array's suffix
        // follows, and the array then names its element's assembly.
        if (typeof(List<Top[]>).FullName
            != $"System.Collections.Generic.List`1[[Top[], {here}]]") return 50;
        if (typeof(List<int[,]>).FullName
            != $"System.Collections.Generic.List`1[[System.Int32[,], {coreLib}]]") return 51;

        // The same rule at the top level: an array's assembly comes after its suffix.
        if (typeof(int[]).AssemblyQualifiedName != $"System.Int32[], {coreLib}") return 60;
        if (typeof(Top[,]).AssemblyQualifiedName != $"Top[,], {here}") return 61;
        if (typeof(Enclosing.Inner).MakeByRefType().AssemblyQualifiedName != $"Enclosing+Inner&, {here}") return 62;
        unsafe
        {
            if (typeof(int*).AssemblyQualifiedName != $"System.Int32*, {coreLib}") return 63;
        }
        // An array over an instantiation: the instantiation in full, then the suffix, then the
        // assembly of the instantiation's definition.
        if (typeof(List<Top>[]).FullName
            != $"System.Collections.Generic.List`1[[Top, {here}]][]") return 64;
        if (typeof(List<Top>[]).AssemblyQualifiedName
            != $"System.Collections.Generic.List`1[[Top, {here}]][], {coreLib}") return 65;

        // A generic definition has no instantiation under FormatFullInst, and an array over one
        // is round-trip compatible, so it gets an assembly-qualified name too.
        if (typeof(List<>).FullName != "System.Collections.Generic.List`1") return 70;
        if (typeof(List<>).AssemblyQualifiedName != $"System.Collections.Generic.List`1, {coreLib}") return 71;
        if (typeof(List<>).MakeArrayType().AssemblyQualifiedName
            != $"System.Collections.Generic.List`1[], {coreLib}") return 72;

        // The point of the brackets: an assembly-qualified name parses back to the same type.
        if (Type.GetType(typeof(Dictionary<string, Top>).AssemblyQualifiedName) != typeof(Dictionary<string, Top>)) return 80;
        if (Type.GetType(typeof(List<Top[]>).AssemblyQualifiedName) != typeof(List<Top[]>)) return 81;

        return 0;
    }
}
