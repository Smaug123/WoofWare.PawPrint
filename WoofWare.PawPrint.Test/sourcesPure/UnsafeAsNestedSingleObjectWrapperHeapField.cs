using System.Runtime.CompilerServices;

struct ObjectWrapperGeneric<T>
{
    public object Field;
}

struct ObjectWrapperPlain
{
    public object Field;
}

class Container
{
    public ObjectWrapperGeneric<int> Wrapper;
}

class Program
{
    // Phase B write through a `HeapObjectField` byref root.
    //
    // Counterpart to `UnsafeAsNestedSingleObjectWrapperArray.cs` for the
    // class-field anchor: `ref c.Wrapper` is a byref over the field slot of
    // a class instance, which the dispatcher classifies as `HeapObjectField`.
    // The non-byte-renderable empty-prefix dispatch must route the trailing
    // `[ReinterpretAs ObjectWrapperPlain, Field Field]` write through the
    // structural projection writer so the ObjectRef payload survives the
    // transparent-wrapper elision; otherwise the bytes-or-typed-cell
    // writer's `tryWriteHeapValueFieldPrecise` rejects the cross-
    // constructor write and the byte-scatter fallback hits
    // `CliType.ToBytes` on the ObjectRef.
    static int Main(string[] args)
    {
        Container c = new Container();
        object initial = new object();
        c.Wrapper.Field = initial;

        ref ObjectWrapperPlain view = ref Unsafe.As<ObjectWrapperGeneric<int>, ObjectWrapperPlain>(ref c.Wrapper);

        if (!ReferenceEquals(view.Field, initial)) return 1;

        object replacement = new object();
        view.Field = replacement;
        if (!ReferenceEquals(c.Wrapper.Field, replacement)) return 2;
        if (ReferenceEquals(c.Wrapper.Field, initial)) return 3;
        if (!ReferenceEquals(view.Field, replacement)) return 4;

        view.Field = null;
        if (c.Wrapper.Field != null) return 5;
        if (view.Field != null) return 6;

        return 0;
    }
}
