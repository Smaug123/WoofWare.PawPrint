using System;

public class Program
{
    class WithExplicitCtor
    {
        public int Value;
        public WithExplicitCtor() { Value = 42; }
    }

    class WithFieldInitializer
    {
        // Roslyn folds field initializers into the synthesized parameterless ctor,
        // so a successful Activator path must run the ctor for this to be "init".
        public string S = "init";
    }

    struct PlainStruct
    {
        public int X;
        public int Y;
    }

    struct StructWithRefField
    {
        public string S;
        public int N;
    }

    // Tracker for the struct-cctor-must-not-run check. Holding the flag on a *separate*
    // class means reading it does not itself trigger StructWithStaticCctor's cctor.
    static class CctorTracker
    {
        public static bool StructCctorRan;
    }

    struct StructWithStaticCctor
    {
        public int X;
        static StructWithStaticCctor()
        {
            // CoreCLR's `Activator.CreateInstance<T>()` for a value type with no explicit
            // parameterless instance ctor returns `default(T)` and does NOT run this cctor,
            // even though the explicit static ctor makes the type non-beforefieldinit.
            // Verified empirically against .NET 10.
            CctorTracker.StructCctorRan = true;
        }
    }

    public static int Main(string[] args)
    {
        // Reference type with an explicit parameterless ctor: ctor must run.
        WithExplicitCtor a = Activator.CreateInstance<WithExplicitCtor>();
        if (a == null) return 1;
        if (a.Value != 42) return 2;

        // Reference type whose only "ctor work" is the field initializer.
        WithFieldInitializer b = Activator.CreateInstance<WithFieldInitializer>();
        if (b == null) return 3;
        if (b.S != "init") return 4;

        // Value type: result is default(T).
        PlainStruct c = Activator.CreateInstance<PlainStruct>();
        if (c.X != 0) return 5;
        if (c.Y != 0) return 6;

        // Value type with a managed reference field: ref field is null.
        StructWithRefField d = Activator.CreateInstance<StructWithRefField>();
        if (d.S != null) return 7;
        if (d.N != 0) return 8;

        // Value-type Activator path must NOT trigger T's static constructor.
        StructWithStaticCctor e = Activator.CreateInstance<StructWithStaticCctor>();
        if (e.X != 0) return 9;
        if (CctorTracker.StructCctorRan) return 10;

        return 0;
    }
}
