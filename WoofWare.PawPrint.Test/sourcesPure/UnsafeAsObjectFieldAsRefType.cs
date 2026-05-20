using System.Runtime.CompilerServices;

class Holder
{
    public object Field;
}

class Boxed
{
    public int Value;
}

class Program
{
    // Mirrors CoreLib's Task.Context motif: a field typed `object` is reinterpreted
    // through `Unsafe.As<object, TRef>` (TRef itself a reference type) to obtain a
    // `ref TRef`, then dereferenced. The resulting byref shape is
    // `[HeapObjectField root; ReinterpretAs TRef]` and the subsequent Ldind.ref
    // must return the stored ObjectRef unchanged. The byref machinery cannot
    // byte-view an ObjectRef cell, so `readManagedByref` short-circuits when the
    // peeled byte offset is exactly zero and both storage and reinterpret target
    // are reference-typed. Without that, the read would fall into the bytewise
    // fallback and refuse with "refusing byte view over object reference".
    static int Main(string[] args)
    {
        Holder h = new Holder();
        Boxed stored = new Boxed { Value = 42 };
        h.Field = stored;

        ref Boxed view = ref Unsafe.As<object, Boxed>(ref h.Field);
        Boxed observed = view;

        if (!ReferenceEquals(observed, stored)) return 1;
        if (observed.Value != 42) return 2;

        return 0;
    }
}
