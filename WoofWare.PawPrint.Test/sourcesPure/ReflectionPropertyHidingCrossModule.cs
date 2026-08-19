using System;
using System.IO;
using System.Reflection;

// The same `new`-hiding dedup as `ReflectionPropertyHiding.cs`, but hiding properties declared in
// *another assembly* — which is what makes the signature comparison do real work.
//
// `Signature_AreEqual` is `MetaSig::CompareMethodSigs(sig1, .., module1, NULL, sig2, .., module2,
// NULL, FALSE)`. Its fast path is "same module && same length && memcmp equal"; across modules that
// path cannot fire, so every comparison here goes through the element-by-element walk. Two
// properties exercise the two things that walk has to get right:
//
//   BufferSize        int/int              — the blobs are byte-identical, so an implementation
//                                            that compared bytes without checking the module would
//                                            get this right by luck, but one that required the same
//                                            module to answer "equal" at all would get it wrong.
//   AttributesToSkip  FileAttributes/same  — the blobs *differ*: CoreLib spells `FileAttributes`
//                                            with a TypeDef token, this assembly spells it with a
//                                            TypeRef. Same type, different bytes, different
//                                            modules — so answering it needs the reference resolved
//                                            to what it names, not compared as bytes.
//
//   MaxRecursionDepth string vs int        — the unequal answer, so "always equal" fails here.
//
// `System.IO.EnumerationOptions` is the base because it is one of the few inheritable BCL types
// with no virtual property accessor anywhere in its chain: an inherited *virtual* accessor sends
// `Associates.AssignAssociates` to `RuntimeTypeHandle.GetMethodAt`, an unimplemented QCall, while
// merely listing properties — so almost every other BCL base aborts the query before any signature
// is compared.

public class MyOptions : EnumerationOptions
{
    public new int BufferSize { get; set; }
    public new FileAttributes AttributesToSkip { get; set; }
    public new string MaxRecursionDepth { get; set; }
}

public class Program
{
    static int CountNamed(Type t, string name)
    {
        int n = 0;
        foreach (PropertyInfo p in t.GetProperties())
        {
            if (p.Name == name) n++;
        }
        return n;
    }

    static PropertyInfo Find(Type t, string name, Type propertyType)
    {
        foreach (PropertyInfo p in t.GetProperties())
        {
            if (p.Name == name && p.PropertyType == propertyType) return p;
        }
        return null;
    }

    public static int Main()
    {
        // Control: the base type, in its own assembly, reports its own eight properties.
        if (typeof(EnumerationOptions).GetProperties().Length != 8) return 1;

        PropertyInfo[] mine = typeof(MyOptions).GetProperties();

        // Three declared here, plus six inherited: eight from the base less the two whose
        // signatures matched a derived property and were dropped.
        if (mine.Length != 9) return 2;

        // Equal across modules, identical blob bytes.
        if (CountNamed(typeof(MyOptions), "BufferSize") != 1) return 3;
        PropertyInfo buffer = Find(typeof(MyOptions), "BufferSize", typeof(int));
        if (buffer == null) return 4;
        if (buffer.DeclaringType != typeof(MyOptions)) return 5;

        // Equal across modules, but the blobs differ: TypeDef in CoreLib vs TypeRef here.
        if (CountNamed(typeof(MyOptions), "AttributesToSkip") != 1) return 6;
        PropertyInfo attrs = Find(typeof(MyOptions), "AttributesToSkip", typeof(FileAttributes));
        if (attrs == null) return 7;
        if (attrs.DeclaringType != typeof(MyOptions)) return 8;

        // Unequal: both survive, and the inherited one keeps the base's type.
        if (CountNamed(typeof(MyOptions), "MaxRecursionDepth") != 2) return 9;
        PropertyInfo hidden = Find(typeof(MyOptions), "MaxRecursionDepth", typeof(string));
        if (hidden == null) return 10;
        if (hidden.DeclaringType != typeof(MyOptions)) return 11;
        PropertyInfo inherited = Find(typeof(MyOptions), "MaxRecursionDepth", typeof(int));
        if (inherited == null) return 12;
        if (inherited.DeclaringType != typeof(EnumerationOptions)) return 13;

        // A property the derived type does not touch is inherited untouched.
        if (CountNamed(typeof(MyOptions), "MatchCasing") != 1) return 14;
        if (Find(typeof(MyOptions), "MatchCasing", typeof(MatchCasing)).DeclaringType
            != typeof(EnumerationOptions)) return 15;

        // The surviving properties address the derived storage, so dropping the wrong one shows up.
        MyOptions o = new MyOptions();
        o.BufferSize = 17;
        ((EnumerationOptions)o).BufferSize = 4096;
        if ((int)buffer.GetValue(o) != 17) return 16;

        o.AttributesToSkip = FileAttributes.ReadOnly;
        ((EnumerationOptions)o).AttributesToSkip = FileAttributes.Hidden;
        if ((FileAttributes)attrs.GetValue(o) != FileAttributes.ReadOnly) return 17;

        o.MaxRecursionDepth = "deep";
        ((EnumerationOptions)o).MaxRecursionDepth = 5;
        if ((string)hidden.GetValue(o) != "deep") return 18;
        if ((int)inherited.GetValue(o) != 5) return 19;

        return 0;
    }
}
