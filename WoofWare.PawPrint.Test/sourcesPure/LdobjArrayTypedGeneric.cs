using System;
using System.Collections.Generic;

public class LdobjArrayTypedGenericTests
{
    // `Read<T>` compiles to exactly `ldarg.0; ldobj !!T; ret`, so instantiating it at an
    // array type is the most direct way to drive `ldobj` with a type token that
    // concretizes to a structural `ConcreteTypeHandle` (one with no row in
    // `AllConcreteTypes` and no TypeDef behind it) rather than a nominal one.
    private static T Read<T>(ref T r) => r;

    // The store direction, `stobj !!T`. It is a control, not a new claim: `stobj` goes
    // through `CliType.zeroOf`, which already knows every structural shape.
    private static void Write<T>(ref T dest, T value) => dest = value;

    private struct Pair
    {
        public int A;
        public int B;
    }

    private sealed class Holder<T>
    {
        public T Field;
    }

    // A one-dimensional array of a primitive: `ConcreteTypeHandle.OneDimArrayZero` over a
    // nominal value type.
    public static int TestOneDimPrimitive()
    {
        int[] arr = { 3, 5, 7 };
        int[] read = Read(ref arr);
        if (read == null) return 1;
        if (read.Length != 3) return 2;
        if (read[0] != 3 || read[1] != 5 || read[2] != 7) return 3;

        // `ldobj` on a reference type is `ldind.ref`: it copies the reference, so the
        // value pushed must alias the original array rather than be a rebuilt copy.
        if (!ReferenceEquals(read, arr)) return 4;
        read[1] = 11;
        if (arr[1] != 11) return 5;

        int[] other = { 1 };
        Write(ref arr, other);
        if (!ReferenceEquals(arr, other)) return 6;

        int[] nullArr = null;
        if (Read(ref nullArr) != null) return 7;

        return 0;
    }

    // A one-dimensional array of a reference type.
    public static int TestOneDimReference()
    {
        string[] arr = { "a", "b" };
        string[] read = Read(ref arr);
        if (read == null) return 1;
        if (read.Length != 2) return 2;
        if (read[0] != "a" || read[1] != "b") return 3;
        if (!ReferenceEquals(read, arr)) return 4;
        return 0;
    }

    // A one-dimensional array whose element is a value type with fields.
    public static int TestOneDimStruct()
    {
        Pair[] arr = new Pair[2];
        arr[0].A = 1;
        arr[0].B = 2;
        arr[1].A = 3;
        arr[1].B = 4;

        Pair[] read = Read(ref arr);
        if (read == null) return 1;
        if (read.Length != 2) return 2;
        if (read[0].A != 1 || read[0].B != 2) return 3;
        if (read[1].A != 3 || read[1].B != 4) return 4;
        if (!ReferenceEquals(read, arr)) return 5;
        return 0;
    }

    // Rank 2, which concretizes to `ConcreteTypeHandle.Array` rather than
    // `OneDimArrayZero` — a distinct arm.
    public static int TestMultiDim()
    {
        int[,] arr = new int[2, 3];
        arr[0, 0] = 1;
        arr[1, 2] = 6;

        int[,] read = Read(ref arr);
        if (read == null) return 1;
        if (read.GetLength(0) != 2 || read.GetLength(1) != 3) return 2;
        if (read[0, 0] != 1 || read[1, 2] != 6) return 3;
        if (!ReferenceEquals(read, arr)) return 4;

        read[0, 1] = 9;
        if (arr[0, 1] != 9) return 5;
        return 0;
    }

    // Jagged: `OneDimArrayZero` nested inside `OneDimArrayZero`.
    public static int TestJagged()
    {
        int[][] arr = new int[2][];
        arr[0] = new int[] { 1, 2 };
        arr[1] = new int[] { 3 };

        int[][] read = Read(ref arr);
        if (read == null) return 1;
        if (read.Length != 2) return 2;
        if (read[0][1] != 2 || read[1][0] != 3) return 3;
        if (!ReferenceEquals(read, arr)) return 4;

        // And one level in, where T is itself `int[]`.
        int[] inner = Read(ref arr[0]);
        if (!ReferenceEquals(inner, arr[0])) return 5;
        return 0;
    }

    // The byref the token is read through has a different root in each of these, and the
    // read happens before the type token is classified, so cover all three.
    public static int TestByrefRoots()
    {
        // Root: a local.
        int[] local = { 1 };
        if (!ReferenceEquals(Read(ref local), local)) return 1;

        // Root: a field of a heap object.
        Holder<int[]> holder = new Holder<int[]> { Field = new int[] { 2 } };
        if (!ReferenceEquals(Read(ref holder.Field), holder.Field)) return 2;

        // Root: an element of an array.
        int[][] outer = new int[1][];
        outer[0] = new int[] { 3 };
        if (!ReferenceEquals(Read(ref outer[0]), outer[0])) return 3;

        // Root: a static field.
        s_static = new int[] { 4 };
        if (!ReferenceEquals(Read(ref s_static), s_static)) return 4;

        return 0;
    }

    private static int[] s_static;

    // Controls: the nominal arms, which must keep working. A value type exercises the
    // copy-and-coerce path; a reference type exercises the existing `ldind.ref` path.
    public static int TestNominalControls()
    {
        int i = 42;
        if (Read(ref i) != 42) return 1;

        Pair p = new Pair { A = 8, B = 9 };
        Pair readPair = Read(ref p);
        if (readPair.A != 8 || readPair.B != 9) return 2;

        // A copy, not an alias: mutating the read-back value must not touch the source.
        readPair.A = 100;
        if (p.A != 8) return 3;

        string s = "hello";
        if (!ReferenceEquals(Read(ref s), s)) return 4;

        Write(ref i, 7);
        if (i != 7) return 5;

        return 0;
    }

    // The shape that found this: a generic dictionary whose value type is an array.
    // `TryGetValue` assigns `value = valRef` on a hit, which is `ldobj !!TValue`, so the
    // miss path alone does not reach it.
    public static int TestDictionaryArrayValued()
    {
        Dictionary<string, int[]> d = new Dictionary<string, int[]>();
        d["x"] = new int[] { 1, 2, 3 };
        d["y"] = new int[] { 4 };

        int[] got;
        if (!d.TryGetValue("x", out got)) return 1;
        if (got == null || got.Length != 3) return 2;
        if (got[0] != 1 || got[2] != 3) return 3;
        if (!ReferenceEquals(got, d["x"])) return 4;

        if (!d.TryGetValue("y", out got)) return 5;
        if (got.Length != 1 || got[0] != 4) return 6;

        if (d.TryGetValue("absent", out got)) return 7;
        if (got != null) return 8;

        return 0;
    }
}

class Program
{
    static int Main(string[] args)
    {
        int result;

        result = LdobjArrayTypedGenericTests.TestOneDimPrimitive();
        if (result != 0) return 100 + result;

        result = LdobjArrayTypedGenericTests.TestOneDimReference();
        if (result != 0) return 200 + result;

        result = LdobjArrayTypedGenericTests.TestOneDimStruct();
        if (result != 0) return 300 + result;

        result = LdobjArrayTypedGenericTests.TestMultiDim();
        if (result != 0) return 400 + result;

        result = LdobjArrayTypedGenericTests.TestJagged();
        if (result != 0) return 500 + result;

        result = LdobjArrayTypedGenericTests.TestByrefRoots();
        if (result != 0) return 600 + result;

        result = LdobjArrayTypedGenericTests.TestNominalControls();
        if (result != 0) return 700 + result;

        result = LdobjArrayTypedGenericTests.TestDictionaryArrayValued();
        if (result != 0) return 800 + result;

        return 0;
    }
}
