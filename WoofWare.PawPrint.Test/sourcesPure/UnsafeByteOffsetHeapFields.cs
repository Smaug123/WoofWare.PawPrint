using System.Runtime.CompilerServices;

// `Unsafe.ByteOffset` between two byrefs into the same heap object. Both are interior pointers
// into one object, so their difference is a stable fact about that object's field layout even
// though neither address is.
//
// This is the only route by which a guest can observe a reference type's field layout at all.
// The other one -- reinterpreting the reference as another class and reading its first field,
// CoreLib's own `Unsafe.As<RawData>(obj).Data` idiom -- is still unsupported, so until this
// worked nothing in `sourcesPure` could see a class's offsets. That mattered: issue #994's
// base-chain layout fix (#1006) had to be pinned by an in-process fixture comparing against real
// .NET, because no guest could reach the behaviour it changed.
//
// The layout facts asserted below are real .NET's, and PawPrint models them as of #1006:
// a base field keeps the offset its own type gives it, and the derived type's fields start after
// the base's instance size.
public class TestUnsafeByteOffsetHeapFields
{
    public class Base { public int BaseInt; }
    public class Derived : Base { public object DerivedRef; public byte DerivedByte; }

    // A single-level class, laid out by auto layout's size-class bucketing.
    public class Bucketed { public byte B; public int I; public long L; }

    public static int Test1()
    {
        Derived d = new Derived();

        ref int baseField = ref d.BaseInt;
        ref int derivedRef = ref Unsafe.As<object, int>(ref d.DerivedRef);

        // Base is laid out first: BaseInt@0, and Derived's own fields start at 8 (Base's instance
        // size is 4, rounded up to the reference's 8-byte alignment). Before the base-chain fix
        // PawPrint bucketed the flattened list and put the reference at 0 with BaseInt at 8, so
        // this delta was -8.
        if (Unsafe.ByteOffset(ref baseField, ref derivedRef) != 8)
            return 1;

        // Reversing the arguments negates the answer.
        if (Unsafe.ByteOffset(ref derivedRef, ref baseField) != -8)
            return 2;

        // A byref to a field is at zero distance from itself.
        if (Unsafe.ByteOffset(ref baseField, ref baseField) != 0)
            return 3;

        return 0;
    }

    public static int Test2()
    {
        Derived d = new Derived();

        ref int baseField = ref d.BaseInt;
        ref int derivedRef = ref Unsafe.As<object, int>(ref d.DerivedRef);
        ref int derivedByte = ref Unsafe.As<byte, int>(ref d.DerivedByte);

        // `DerivedByte` sits *below* `DerivedRef`, at 4. Base ends at 4, which is not 8-aligned,
        // so auto layout back-fills the derived type's one-byte field into the gap before
        // starting its largest-first regions; the reference then lands at 8.
        if (Unsafe.ByteOffset(ref baseField, ref derivedByte) != 4)
            return 4;

        if (Unsafe.ByteOffset(ref derivedRef, ref derivedByte) != -4)
            return 5;

        return 0;
    }

    public static int Test3()
    {
        Bucketed b = new Bucketed();

        ref byte first = ref b.B;
        ref byte viaInt = ref Unsafe.As<int, byte>(ref b.I);
        ref byte viaLong = ref Unsafe.As<long, byte>(ref b.L);

        // Auto layout buckets by size class, largest first: L@0, I@8, B@12. Declared order would
        // have given B@0, I@4, L@8.
        if (Unsafe.ByteOffset(ref viaLong, ref viaInt) != 8)
            return 6;
        if (Unsafe.ByteOffset(ref viaLong, ref first) != 12)
            return 7;

        return 0;
    }

    public static int Main(string[] argv)
    {
        int r = Test1();
        if (r != 0) return r;
        r = Test2();
        if (r != 0) return r;
        r = Test3();
        if (r != 0) return r;
        return 0;
    }
}
