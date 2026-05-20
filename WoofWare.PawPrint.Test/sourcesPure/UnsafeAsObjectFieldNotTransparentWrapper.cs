using System.Runtime.CompilerServices;

class Holder
{
    public object Field;
}

struct ObjectAndInt
{
    public object Obj;
    public int Padding;
}

class Program
{
    // Regression guard for the dispatcher's Phase B re-routing in
    // `writeManagedByrefCore`: when the chain shape is `[ReinterpretAs T,
    // Field f]` but `T` is *not* a transparent single-field wrapper (here
    // `ObjectAndInt` has two fields), the transparent-wrapper classifier
    // must return `NotTransparent` and the dispatcher must keep the write
    // on the bytes-or-typed-cell path so the precise-write helper
    // (`tryWriteHeapValueFieldPrecise`) updates the underlying `object`
    // field of `Holder`. Routing this through the structural writer would
    // fall through to `reinterpretStorageBytes` on an ObjectRef-typed
    // storage cell and throw.
    static int Main(string[] args)
    {
        Holder h = new Holder();
        object replacement = new object();
        ref ObjectAndInt view = ref Unsafe.As<object, ObjectAndInt>(ref h.Field);
        view.Obj = replacement;

        if (!ReferenceEquals(h.Field, replacement))
            return 1;

        return 0;
    }
}
