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

        return 0;
    }
}
