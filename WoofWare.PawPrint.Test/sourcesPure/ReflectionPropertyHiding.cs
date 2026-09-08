using System;
using System.Reflection;

// `RuntimeType.PopulateProperties` deduplicates inherited properties two ways, and only the second
// consults a signature comparison. First it drops a base property whose accessor occupies the same
// vtable slot as the derived one (an `override`). Then, "for backward compatibility", it drops a
// base property that merely shares the derived one's *name and signature*, via
// `RuntimePropertyInfo.EqualsSig` and so the `Signature_AreEqual` QCall.
//
// So `new` hiding is the shape that reaches the comparison, and it reaches both of its answers:
//   D  hides `int Value` with `int Value`    -> signatures equal   -> the base property is dropped
//   D2 hides `int Value` with `string Value` -> signatures differ  -> both properties survive
// A comparison stuck at "always equal" fails on D2; one stuck at "never equal" fails on D. Those
// two are what make this file worth having — `ReflectionOverloadedIndexer.cs` reaches the unequal
// answer too, but nothing there distinguishes it from a comparison that never returns true.
//
// Deliberately no `virtual` property here, which would otherwise be the control for the vtable-slot
// dedup path. That path is unreachable today for an unrelated reason: `Associates.AssignAssociates`
// re-resolves an *inherited* accessor that is virtual through
// `RuntimeTypeHandle.GetMethodAt(reflectedType, slot)`, an unimplemented QCall, and it does so
// while merely listing the properties — so a single inherited virtual property anywhere in the
// hierarchy aborts the whole query before any signature is compared.

public class B
{
    public int Value { get; set; }
    public int Other { get; set; }

    private int _byref;
    // `ref readonly` puts a modreq(InAttribute) on the property's type in the PropertySig blob.
    public ref readonly int ByRef => ref _byref;
}

public class D : B
{
    public new int Value { get; set; }

    private int _byref;
    // Hides `B.ByRef` with a signature differing *only* in that modifier. CoreCLR compares custom
    // modifiers (`CompareState.IgnoreCustomModifiers` defaults false and this path never sets it),
    // so these are unequal and both survive.
    public new ref int ByRef => ref _byref;
}

public class D2 : B
{
    public new string Value { get; set; }
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

    static PropertyInfo Single(Type t, string name, Type propertyType)
    {
        foreach (PropertyInfo p in t.GetProperties())
        {
            if (p.Name == name && p.PropertyType == propertyType) return p;
        }
        return null;
    }

    static int CountDeclaredBy(Type t, string name, Type declaringType)
    {
        int n = 0;
        foreach (PropertyInfo p in t.GetProperties())
        {
            if (p.Name == name && p.DeclaringType == declaringType) n++;
        }
        return n;
    }

    public static int Main()
    {
        // Control: the base type itself reports all three of its own properties.
        if (typeof(B).GetProperties().Length != 3) return 1;

        // Equal signatures: `D.Value` hides `B.Value`, so only the derived one survives, alongside
        // the inherited `Other` and both `ByRef`s.
        if (typeof(D).GetProperties().Length != 4) return 2;
        if (CountNamed(typeof(D), "Value") != 1) return 3;
        if (CountNamed(typeof(D), "Other") != 1) return 4;

        PropertyInfo dValue = Single(typeof(D), "Value", typeof(int));
        if (dValue == null) return 5;
        // The survivor must be the *derived* one; keeping the base property instead would also
        // leave exactly one `Value` of type int.
        if (dValue.DeclaringType != typeof(D)) return 6;

        // Unequal by custom modifier alone: `D.ByRef` is `ref int`, `B.ByRef` is `ref readonly int`,
        // which differ only by a modreq(InAttribute) on the property's type. Both survive. A
        // comparison that stripped modifiers — as concretization does — would see two identical
        // `int&` signatures and silently drop the base one.
        //
        // Checked by declaring type rather than by `GetRequiredCustomModifiers`, which is not
        // reachable: it goes through `Signature.GetParameterOffset`, whose InternalCall refuses a
        // non-FIELD calling convention. Both properties have the same `PropertyType` (`int&`), so
        // the declaring types are what distinguishes them.
        if (CountNamed(typeof(D), "ByRef") != 2) return 20;
        if (CountDeclaredBy(typeof(D), "ByRef", typeof(D)) != 1) return 21;
        if (CountDeclaredBy(typeof(D), "ByRef", typeof(B)) != 1) return 22;

        // Unequal signatures: `D2.Value` is a different type, so both properties survive.
        if (typeof(D2).GetProperties().Length != 4) return 7;
        if (CountNamed(typeof(D2), "Value") != 2) return 8;

        PropertyInfo d2New = Single(typeof(D2), "Value", typeof(string));
        if (d2New == null) return 9;
        if (d2New.DeclaringType != typeof(D2)) return 10;

        PropertyInfo d2Base = Single(typeof(D2), "Value", typeof(int));
        if (d2Base == null) return 11;
        if (d2Base.DeclaringType != typeof(B)) return 12;

        // The properties still work, so a comparison that dropped the wrong one is visible.
        D d = new D();
        d.Value = 7;
        ((B)d).Value = 9;
        if ((int)dValue.GetValue(d) != 7) return 13;

        D2 d2 = new D2();
        d2.Value = "abc";
        ((B)d2).Value = 4;
        if ((string)d2New.GetValue(d2) != "abc") return 14;
        if ((int)d2Base.GetValue(d2) != 4) return 15;

        return 0;
    }
}
