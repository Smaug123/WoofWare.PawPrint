using System;
using System.Reflection;

// Reflection over virtual methods: `RuntimeType.RuntimeTypeCache.PopulateMethods` asks
// `RuntimeMethodHandle.GetSlot` for every virtual method it enumerates, and uses the answer twice —
// `isVirtual = slot < GetNumVirtuals(declaringType)`, and `overrides[slot]` to suppress a base
// declaration that something further down already overrode (RuntimeType.CoreCLR.cs:685-716). So the
// slot has to be a real vtable *layout*: equal, transitively, for a method and the base-chain method
// it overrides.
//
// Everything here is asserted through `Name` / `DeclaringType` / `ReturnType`, never
// `GetParameters()` — that is blocked on the unimplemented `RuntimeMethodHandle::GetMethodDef`
// InternalCall, which has nothing to do with slots. Overloads are therefore given *different return
// types* purely so they can be told apart.

public interface IExplicit
{
    string E ();
}

public abstract class Reabstracted
{
    public virtual string R ()
    {
        return "r1";
    }
}

public abstract class ReabstractedMiddle : Reabstracted
{
    public abstract override string R ();
}

public class ReabstractedLeaf : ReabstractedMiddle
{
    public override string R ()
    {
        return "r3";
    }
}

public class Program
{
    // --- overloaded virtuals, only one overridden: the matcher test ---------------------------
    // A matcher that compares only name and argument *count* binds `DerivedO.M(string)` to
    // `BaseO.M(int)`'s slot, which dedupes `M(int)` away and leaves `M(string)` in its place. The
    // return types are what make that visible.
    public class BaseO
    {
        public virtual string M (int x)
        {
            return "base-int";
        }

        public virtual object M (string x)
        {
            return "base-string";
        }
    }

    public class DerivedO : BaseO
    {
        public override object M (string x)
        {
            return "derived-string";
        }
    }

    // --- covariant return override -------------------------------------------------------------
    // The one class-level MethodImpl Roslyn emits. It is *newslot* + `.override`, so CoreCLR gives
    // it a fresh slot and the base declaration keeps its own: both must be listed. An implementation
    // that let the MethodImpl move the slot number would report one.
    public class Animal
    {
    }

    public class Dog : Animal
    {
    }

    public class BaseC
    {
        public virtual Animal Get ()
        {
            return null;
        }
    }

    public class DerivedC : BaseC
    {
        public override Dog Get ()
        {
            return null;
        }
    }

    // --- `new virtual` shadow, itself overridden further down -----------------------------------
    // S2.V is newslot despite matching S1.V's name and signature shape, so it must NOT take S1's
    // slot; S3.V overrides S2.V. Both S3.V and S1.V survive.
    public class S1
    {
        public virtual string V ()
        {
            return "s1";
        }
    }

    public class S2 : S1
    {
        public new virtual object V ()
        {
            return "s2";
        }
    }

    public class S3 : S2
    {
        public override object V ()
        {
            return "s3";
        }
    }

    // --- three-level chain where the middle level declares nothing ------------------------------
    // Transitivity: C3.T must land on the slot C1.T introduced, across a level that says nothing.
    public class C1
    {
        public virtual string T ()
        {
            return "c1";
        }
    }

    public class C2 : C1
    {
    }

    public class C3 : C2
    {
        public sealed override string T ()
        {
            return "c3";
        }
    }

    // --- override under generic substitution ----------------------------------------------------
    // G1<string>.Id has signature `!0 Id(!0)`; G2.Id has `string Id(string)`. Matching them needs
    // the base's signature substituted through the base instantiation.
    public class G1<T>
    {
        public virtual T Id (T x)
        {
            return x;
        }
    }

    public class G2 : G1<string>
    {
        public override string Id (string x)
        {
            return x;
        }
    }

    // --- explicit interface implementation, seen from a derived type ----------------------------
    // `string IExplicit.E()` is private/final/virtual/newslot: Virtual is set, so GetSlot is called,
    // and the "inherited private" filter keeps it precisely because it is virtual.
    public class E1 : IExplicit
    {
        string IExplicit.E ()
        {
            return "e1";
        }
    }

    public class E2 : E1
    {
    }

    // --- value type overriding cross-assembly virtuals -------------------------------------------
    public struct Val
    {
        public int X;

        public override string ToString ()
        {
            return "val";
        }
    }

    private const BindingFlags All =
        BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic | BindingFlags.Static;

    private static int Count (Type t, string name)
    {
        int n = 0;

        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name == name)
                n++;
        }

        return n;
    }

    /// Return type of the unique method named `name` whose DeclaringType is `owner`, or a marker.
    private static string RetOf (Type t, string name, string owner)
    {
        string found = "<none>";

        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name != name || m.DeclaringType.Name != owner)
                continue;

            if (found != "<none>")
                return "<dup>";

            found = m.ReturnType.Name;
        }

        return found;
    }

    private static string OwnerOf (Type t, string name)
    {
        string found = "<none>";

        foreach (MethodInfo m in t.GetMethods (All))
        {
            if (m.Name != name)
                continue;

            if (found != "<none>")
                return "<dup>";

            found = m.DeclaringType.Name;
        }

        return found;
    }

    public static int Main (string[] args)
    {
        // Overloads: exactly two survive, and the one still owned by BaseO is the *int* overload —
        // i.e. the one whose sibling was overridden got deduped, not this one.
        if (Count (typeof (DerivedO), "M") != 2)
            return 1;

        if (RetOf (typeof (DerivedO), "M", "DerivedO") != "Object")
            return 2;

        if (RetOf (typeof (DerivedO), "M", "BaseO") != "String")
            return 3;

        // Covariant return: both listed, base keeps its own slot.
        if (Count (typeof (DerivedC), "Get") != 2)
            return 4;

        if (RetOf (typeof (DerivedC), "Get", "DerivedC") != "Dog")
            return 5;

        if (RetOf (typeof (DerivedC), "Get", "BaseC") != "Animal")
            return 6;

        // `new virtual` shadow does not take the shadowed slot.
        if (Count (typeof (S3), "V") != 2)
            return 7;

        if (RetOf (typeof (S3), "V", "S3") != "Object")
            return 8;

        if (RetOf (typeof (S3), "V", "S1") != "String")
            return 9;

        // Transitive override across a silent middle level.
        if (Count (typeof (C3), "T") != 1)
            return 10;

        if (OwnerOf (typeof (C3), "T") != "C3")
            return 11;

        // Reabstraction: one entry, owned by the leaf.
        if (Count (typeof (ReabstractedLeaf), "R") != 1)
            return 12;

        if (OwnerOf (typeof (ReabstractedLeaf), "R") != "ReabstractedLeaf")
            return 13;

        // Override under generic substitution.
        if (Count (typeof (G2), "Id") != 1)
            return 14;

        if (OwnerOf (typeof (G2), "Id") != "G2")
            return 15;

        if (RetOf (typeof (G2), "Id", "G2") != "String")
            return 16;

        // Explicit interface implementation, inherited and private but virtual, so still listed.
        if (Count (typeof (E2), "IExplicit.E") != 1)
            return 17;

        if (OwnerOf (typeof (E2), "IExplicit.E") != "E1")
            return 18;

        // Cross-assembly override on a value type: ToString is owned by the struct, and the
        // Object/ValueType declarations it overrides are deduped away.
        if (Count (typeof (Val), "ToString") != 1)
            return 19;

        if (OwnerOf (typeof (Val), "ToString") != "Val")
            return 20;

        // ... whereas a virtual it does *not* override is still owned by its base.
        if (Count (typeof (Val), "GetHashCode") != 1)
            return 21;

        if (OwnerOf (typeof (Val), "GetHashCode") != "ValueType")
            return 22;

        // Control: an interface's own methods never reach GetSlot (PopulateMethods routes
        // interfaces down a separate branch), so this must be unaffected either way.
        if (Count (typeof (IExplicit), "E") != 1)
            return 23;

        return 0;
    }
}
