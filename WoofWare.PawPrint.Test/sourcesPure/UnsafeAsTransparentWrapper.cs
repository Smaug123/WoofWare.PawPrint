using System;
using System.Runtime.CompilerServices;

class Holder
{
    public object Field;
}

struct ObjectWrapper
{
    public object Value;
}

struct FloatWrapper
{
    public float Value;
}

class Program
{
    static int Main(string[] args)
    {
        // Custom single-field reference wrapper: the transparent-wrapper
        // classifier must elide reads and writes straight through to the
        // ObjectRef storage cell. This exercises the same path that
        // `Volatile.Write<T> where T : class?` uses with the BCL's
        // VolatileObject, but with a user-defined wrapper to confirm the
        // classifier matches on structural shape rather than the specific BCL
        // type.
        Holder h = new Holder();
        object initial = new object();
        h.Field = initial;

        ref ObjectWrapper wrapper = ref Unsafe.As<object, ObjectWrapper>(ref h.Field);

        if (!ReferenceEquals(wrapper.Value, initial)) return 1;

        object replacement = new object();
        wrapper.Value = replacement;
        if (!ReferenceEquals(h.Field, replacement)) return 2;
        if (ReferenceEquals(h.Field, initial)) return 3;
        if (!ReferenceEquals(wrapper.Value, replacement)) return 4;

        wrapper.Value = null;
        if (h.Field != null) return 5;

        // Bit-reinterpret an int as a float through a single-field wrapper.
        // The classifier must refuse to elide here (Phase A only allows
        // ref↔ref); the bytewise reinterpret path must produce the IEEE 754
        // bit pattern. This is the regression guard against the classifier
        // accidentally widening to cross-type primitives.
        int intStorage = 0x3F800000; // 1.0f bit pattern
        ref FloatWrapper floatView = ref Unsafe.As<int, FloatWrapper>(ref intStorage);

        if (floatView.Value != 1.0f) return 10;

        floatView.Value = 2.0f; // 0x40000000
        if (intStorage != 0x40000000) return 11;

        return 0;
    }
}
