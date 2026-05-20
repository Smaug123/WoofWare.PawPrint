using System.Runtime.CompilerServices;

struct ObjectWrapperGeneric<T>
{
    public object Field;
}

struct ObjectWrapperPlain
{
    public object Field;
}

class Program
{
    // Mirrors the BCL motif used in `AsyncTaskMethodBuilder<T>.AwaitUnsafeOnCompleted`:
    //
    //     ref TaskAwaiter ta = ref Unsafe.As<TAwaiter, TaskAwaiter>(ref awaiter);
    //     ... ta.m_task ...
    //
    // Phase A's transparent-wrapper classifier only elides when the byref's
    // storage is a bare `ObjectRef` (e.g. an `object` field on a class). Here
    // both the underlying storage and the reinterpret target are themselves
    // single-`object`-field structs, so the classifier must additionally
    // recognise that pattern and elide via the storage's inner field for both
    // reads and writes.
    static int Main(string[] args)
    {
        object initial = new object();
        ObjectWrapperGeneric<int> outer = default;
        outer.Field = initial;

        ref ObjectWrapperPlain view = ref Unsafe.As<ObjectWrapperGeneric<int>, ObjectWrapperPlain>(ref outer);

        if (!ReferenceEquals(view.Field, initial)) return 1;

        object replacement = new object();
        view.Field = replacement;
        if (!ReferenceEquals(outer.Field, replacement)) return 2;
        if (ReferenceEquals(outer.Field, initial)) return 3;
        if (!ReferenceEquals(view.Field, replacement)) return 4;

        view.Field = null;
        if (outer.Field != null) return 5;
        if (view.Field != null) return 6;

        return 0;
    }
}
