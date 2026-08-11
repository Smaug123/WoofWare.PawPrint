using System;
using System.Runtime.CompilerServices;

public class Program
{
    private struct WithRef
    {
        public string S;
        public int N;
    }

    public static int Main(string[] args)
    {
        // A struct holding a reference is the one shape whose box CoreCLR fills with
        // Buffer.BulkMoveWithWriteBarrier rather than SpanHelpers.Memmove
        // (RuntimeType.BoxCache.cs:91). Its sibling RuntimeHelpersBox.cs covers every
        // reference-free shape.
        WithRef r = new WithRef { S = "hello", N = 3 };
        object boxed = RuntimeHelpers.Box(ref Unsafe.As<WithRef, byte>(ref r), typeof(WithRef).TypeHandle);

        if (boxed == null) return 1;
        if (!(boxed is WithRef)) return 2;

        WithRef copy = (WithRef)boxed;
        // Both fields, so an implementation that moved the reference but not the trailing
        // int (or vice versa) still fails.
        if (copy.S != "hello") return 3;
        if (copy.N != 3) return 4;

        // The box must be a copy, not an alias: a Box that handed back a view onto the
        // source would pass every check above.
        r.S = "goodbye";
        r.N = 4;
        if (((WithRef)boxed).S != "hello") return 5;
        if (((WithRef)boxed).N != 3) return 6;

        return 0;
    }
}
