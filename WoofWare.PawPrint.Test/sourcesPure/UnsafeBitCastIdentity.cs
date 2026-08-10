using System;
using System.Runtime.CompilerServices;

// `Unsafe.BitCast<T, T>` — the *same* concrete type on both sides — is the identity.
//
// The managed body is `ReadUnaligned<TTo>(ref As<TFrom, byte>(ref source))`, and the JIT expands
// the call to nothing at all when the two layouts agree ("Handle matching handles, compatible
// struct layouts or integrals where we can simply return op1", importercalls.cpp), which is
// exactly the equal-type case. So no reinterpretation happens and the value moves across intact,
// references and all.
//
// That matters for PawPrint because it models the general case as a byte reinterpretation, and
// refuses value types whose contents carry provenance its byte model cannot render: object
// references, managed pointers, runtime handles. None of those are laundered when the type does
// not change, so the equal-type case can be served where the general one still cannot.
//
// The shape is pervasive in CoreLib's `TChar`-generic formatting and parsing code, which reaches
// `Unsafe.BitCast<ReadOnlySpan<char>, ReadOnlySpan<TChar>>(...)` with `TChar == char` (e.g.
// `NumberFormatInfo.PositiveSignTChar<TChar>`, `DateTimeFormatInfo.AMDesignatorTChar<TChar>`) —
// a `ReadOnlySpan` is a value type containing a managed pointer.
public class TestUnsafeBitCastIdentity
{
    private struct WithReference
    {
        public object Obj;
        public int Tag;
    }

    private struct WithRuntimeHandle
    {
        public IntPtr Handle;
        public int Tag;
    }

    // Test 1: a value type holding an object reference. The identity must move the reference
    // itself; there is no byte rendering of it to move instead.
    public static int Test1()
    {
        object payload = new object();
        WithReference w;
        w.Obj = payload;
        w.Tag = 17;

        WithReference copy = Unsafe.BitCast<WithReference, WithReference>(w);
        if (!ReferenceEquals(copy.Obj, payload)) return 1;
        if (copy.Tag != 17) return 2;

        // A null reference travels too.
        WithReference empty = Unsafe.BitCast<WithReference, WithReference>(default);
        if (empty.Obj != null) return 3;
        if (empty.Tag != 0) return 4;

        // The copy is a copy: mutating it does not disturb the source.
        copy.Tag = 99;
        if (w.Tag != 17) return 5;

        return 0;
    }

    // Test 2: a bare native int carrying runtime-handle provenance. A `RuntimeTypeHandle.Value`
    // has no byte image under PawPrint (there is no real address behind it), so this is the
    // simplest input the general byte path cannot serve.
    public static int Test2()
    {
        IntPtr handle = typeof(int).TypeHandle.Value;

        IntPtr copy = Unsafe.BitCast<IntPtr, IntPtr>(handle);
        if (copy != handle) return 1;

        // Distinct handles stay distinct: the identity is not collapsing everything to one value.
        if (copy == typeof(long).TypeHandle.Value) return 2;

        return 0;
    }

    // Test 3: the same provenance, one level down, inside a value type.
    public static int Test3()
    {
        WithRuntimeHandle w;
        w.Handle = typeof(int).TypeHandle.Value;
        w.Tag = 5;

        WithRuntimeHandle copy = Unsafe.BitCast<WithRuntimeHandle, WithRuntimeHandle>(w);
        if (copy.Handle != w.Handle) return 1;
        if (copy.Tag != 5) return 2;
        if (copy.Handle == typeof(long).TypeHandle.Value) return 3;

        return 0;
    }

    private static ReadOnlySpan<TChar> AsTChar<TChar>(ReadOnlySpan<char> s)
        where TChar : struct
        => Unsafe.BitCast<ReadOnlySpan<char>, ReadOnlySpan<TChar>>(s);

    // Test 4: `ReadOnlySpan<char>` to itself — the CoreLib shape, both spelled directly and
    // reached through a generic method whose type parameter happens to be instantiated at `char`.
    // The two must behave the same: what makes this the identity is that the two type arguments
    // name the same concrete type, not that the IL spells them the same way.
    public static int Test4()
    {
        ReadOnlySpan<char> source = "hello".AsSpan();

        ReadOnlySpan<char> direct = Unsafe.BitCast<ReadOnlySpan<char>, ReadOnlySpan<char>>(source);
        if (direct.Length != 5) return 1;
        if (direct[0] != 'h') return 2;
        if (direct[4] != 'o') return 3;

        ReadOnlySpan<char> viaGeneric = AsTChar<char>(source);
        if (viaGeneric.Length != 5) return 4;
        if (viaGeneric[1] != 'e') return 5;

        // A slice's offset survives, so the managed pointer travelled as it stood rather than
        // being rebuilt from the string's base.
        ReadOnlySpan<char> tail = Unsafe.BitCast<ReadOnlySpan<char>, ReadOnlySpan<char>>(source.Slice(2));
        if (tail.Length != 3) return 6;
        if (tail[0] != 'l') return 7;
        if (tail[2] != 'o') return 8;

        return 0;
    }

    // Test 5: the identity does not bypass the BCL's guard. `BitCast` throws
    // `NotSupportedException` unless *both* type arguments are value types, and equal reference
    // types are still reference types.
    public static int Test5()
    {
        try
        {
            string s = Unsafe.BitCast<string, string>("x");
            return 1;
        }
        catch (NotSupportedException)
        {
        }

        try
        {
            object o = Unsafe.BitCast<object, object>(new object());
            return 2;
        }
        catch (NotSupportedException)
        {
        }

        // Including the null reference, which is the one input a byte model could plausibly
        // render and so the one most at risk of sneaking past the guard.
        try
        {
            string s = Unsafe.BitCast<string, string>(null);
            return 3;
        }
        catch (NotSupportedException)
        {
        }

        return 0;
    }

    public static int Main(string[] argv)
    {
        var result = Test1();
        if (result != 0) return result;

        result = Test2();
        if (result != 0) return 10 + result;

        result = Test3();
        if (result != 0) return 20 + result;

        result = Test4();
        if (result != 0) return 30 + result;

        result = Test5();
        if (result != 0) return 40 + result;

        return 0;
    }
}
