using System;
using System.Runtime.CompilerServices;

public class NullAddressDereferenceTests
{
    // `ldarg.0; ldobj !!T; ret`, driven through a null managed byref by
    // `Unsafe.NullRef<T>()`. Dereferencing null must raise a
    // `NullReferenceException` the guest can catch, not kill the interpreter.
    private static T Read<T>(ref T r) => r;

    private struct Pair
    {
        public int A;
        public int B;
    }

    // A nominal value type: the copy-and-coerce arm.
    public static int TestValueType()
    {
        try
        {
            int v = Read(ref Unsafe.NullRef<int>());
            return v == 0 ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A multi-field value type, so the read is wider than one cell.
    public static int TestStruct()
    {
        try
        {
            Pair p = Read(ref Unsafe.NullRef<Pair>());
            return p.A == 0 ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A nominal reference type: the `ldind.ref` arm.
    public static int TestReferenceType()
    {
        try
        {
            string s = Read(ref Unsafe.NullRef<string>());
            return s == null ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // An array-typed token, which concretizes to a structural handle with no
    // TypeDef behind it. The null check must happen before anything tries to
    // interrogate the token's metadata.
    public static int TestArrayTypedToken()
    {
        try
        {
            int[] arr = Read(ref Unsafe.NullRef<int[]>());
            return arr == null ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // The exception must unwind out of a nested frame to a handler further up,
    // rather than only being catchable in the frame that faulted.
    private static int Inner() => Read(ref Unsafe.NullRef<int>());

    public static int TestUnwindsToOuterFrame()
    {
        try
        {
            return Inner() == 0 ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // A `finally` between the fault and the handler must still run.
    public static int TestRunsFinally()
    {
        int ran = 0;
        try
        {
            try
            {
                int v = Read(ref Unsafe.NullRef<int>());
                return v == 0 ? 1 : 2;
            }
            finally
            {
                ran = 1;
            }
        }
        catch (NullReferenceException)
        {
            return ran == 1 ? 0 : 3;
        }
    }

    // The same fault reached through an unmanaged pointer rather than a managed
    // byref: `*(Pair*)null` is also `ldobj Pair`, but the address arrives on the
    // eval stack in a different shape.
    public static unsafe int TestNullUnmanagedPointer()
    {
        try
        {
            Pair* p = null;
            Pair v = *p;
            return v.A == 0 ? 1 : 2;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // The store direction through the same null unmanaged pointer: `stobj Pair`.
    // `stobj` already raises for the managed-byref spellings of null, but an
    // unmanaged null pointer reaches it in the same shape `ldobj` sees above.
    public static unsafe int TestNullUnmanagedPointerStore()
    {
        try
        {
            Pair* p = null;
            Pair q;
            q.A = 1;
            q.B = 2;
            *p = q;
            return 1;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    // The store direction through a null *managed* byref.
    public static int TestNullByrefStore()
    {
        try
        {
            Write(ref Unsafe.NullRef<Pair>(), new Pair { A = 1, B = 2 });
            return 1;
        }
        catch (NullReferenceException)
        {
            return 0;
        }
    }

    private static void Write<T>(ref T dest, T value) => dest = value;

    // Control: the same dereference through a non-null unmanaged pointer.
    public static unsafe int TestNonNullUnmanagedPointer()
    {
        Pair q;
        q.A = 5;
        q.B = 6;
        Pair* p = &q;
        Pair v = *p;
        if (v.A != 5 || v.B != 6) return 1;
        return 0;
    }

    // Control: the same generic read through a non-null byref keeps working, so
    // a fix that raised unconditionally would not pass.
    public static int TestNonNullControl()
    {
        int i = 42;
        if (Read(ref i) != 42) return 1;

        int[] arr = { 1, 2 };
        if (!ReferenceEquals(Read(ref arr), arr)) return 2;

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = NullAddressDereferenceTests.TestValueType();
        if (result != 0) return 100 + result;

        result = NullAddressDereferenceTests.TestStruct();
        if (result != 0) return 200 + result;

        result = NullAddressDereferenceTests.TestReferenceType();
        if (result != 0) return 300 + result;

        result = NullAddressDereferenceTests.TestArrayTypedToken();
        if (result != 0) return 400 + result;

        result = NullAddressDereferenceTests.TestUnwindsToOuterFrame();
        if (result != 0) return 500 + result;

        result = NullAddressDereferenceTests.TestRunsFinally();
        if (result != 0) return 600 + result;

        result = NullAddressDereferenceTests.TestNullUnmanagedPointer();
        if (result != 0) return 700 + result;

        result = NullAddressDereferenceTests.TestNullUnmanagedPointerStore();
        if (result != 0) return 800 + result;

        result = NullAddressDereferenceTests.TestNullByrefStore();
        if (result != 0) return 900 + result;

        result = NullAddressDereferenceTests.TestNonNullUnmanagedPointer();
        if (result != 0) return 1000 + result;

        result = NullAddressDereferenceTests.TestNonNullControl();
        if (result != 0) return 1100 + result;

        return 0;
    }
}
