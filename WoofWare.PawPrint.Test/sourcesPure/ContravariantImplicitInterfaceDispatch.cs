// Contravariant (`in`) interface dispatch onto an IMPLICIT interface implementation.
//
// `ContravariantExplicitMethodImpl.cs` covers the explicit-MethodImpl form of this
// (`void IContravariant<object>.Set(object value)`). That form works because a MethodImpl row
// identifies its virtual slot by declaration, so `findMatchingMethodImplBodies` can match the
// slot under variance without consulting the signature at all. This file is the same shape
// with an ordinary public method satisfying the interface instead, which has no such row: the
// only thing tying `ObjectSink.Accept(object, ...)` to `ISink<string>::Accept(string, ...)` is
// the signature, and those differ under `in`-variance. Without a retarget, dispatch misses
// entirely and the interpreter reaches the abstract interface method:
//
//   BUG: reached executeOneStep for abstract method ISink`1::Accept;
//   virtual dispatch should have resolved to a concrete override
//
// `IlMachineStateExecution.tryRetargetToVariantInterfaceMapEntry` serves this: when
// ordinary resolution misses, it retargets the call from the call site's instantiation to the
// receiver's own variance-compatible interface-map entry (`ISink<object>`) and resolves against
// that, exactly as CoreCLR does. No signature comparison is loosened.

using System;

interface ISink<in T>
{
    long Accept(T value, int count, long width, object tag);
}

// Implemented at `object`, so dispatching through `ISink<string>` (legal under `in`
// variance) must reach a body whose first parameter is `object`, not `string`.
sealed class ObjectSink : ISink<object>
{
    public object Last;
    public int LastCount;
    public long LastWidth;
    public object LastTag;

    public long Accept(object value, int count, long width, object tag)
    {
        Last = value;
        LastCount = count;
        LastWidth = width;
        LastTag = tag;
        return width + count;
    }
}

class Program
{
    static int Main(string[] args)
    {
        ObjectSink sink = new ObjectSink();
        ISink<string> asStringSink = sink;

        long result = asStringSink.Accept("payload", 7, 1000L, "tag-a");

        if (result != 1007L) return 1;
        if (!(sink.Last is string s) || s != "payload") return 2;
        if (sink.LastCount != 7) return 3;
        if (sink.LastWidth != 1000L) return 4;
        if (!(sink.LastTag is string t) || t != "tag-a") return 5;

        // A null reference must survive coercion to the body's `object` parameter too.
        result = asStringSink.Accept(null, -3, 5L, null);
        if (result != 2L) return 6;
        if (sink.Last != null) return 7;
        if (sink.LastCount != -3) return 8;
        if (sink.LastTag != null) return 9;

        return 0;
    }
}
