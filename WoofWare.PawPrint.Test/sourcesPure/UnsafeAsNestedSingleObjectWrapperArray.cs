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
    // Phase B write through an `ArrayElement` byref root.
    //
    // Counterpart to `UnsafeAsNestedSingleObjectWrapper.cs`, which exercises
    // the local-variable root. Here the byref originates from an array
    // element, which routes the store through the dispatcher's empty-prefix
    // peel + non-byte-renderable branch. That branch must recognise the
    // trailing `[ReinterpretAs ObjectWrapperPlain, Field Field]` shape as a
    // transparent-wrapper write so the ObjectRef payload survives;
    // otherwise the bytes-or-typed-cell writer's precise-write helpers
    // reject the cross-constructor write (`ValueType` storage, `ObjectRef`
    // payload) and fall through to `CliType.ToBytes` on the ObjectRef.
    static int Main(string[] args)
    {
        ObjectWrapperGeneric<int>[] arr = new ObjectWrapperGeneric<int>[3];
        object initial = new object();
        arr[1].Field = initial;

        ref ObjectWrapperPlain view = ref Unsafe.As<ObjectWrapperGeneric<int>, ObjectWrapperPlain>(ref arr[1]);

        if (!ReferenceEquals(view.Field, initial)) return 1;

        object replacement = new object();
        view.Field = replacement;
        if (!ReferenceEquals(arr[1].Field, replacement)) return 2;
        if (ReferenceEquals(arr[1].Field, initial)) return 3;
        if (!ReferenceEquals(view.Field, replacement)) return 4;

        // Sibling array elements must remain untouched by the write.
        if (arr[0].Field != null) return 5;
        if (arr[2].Field != null) return 6;

        view.Field = null;
        if (arr[1].Field != null) return 7;
        if (view.Field != null) return 8;

        return 0;
    }
}
