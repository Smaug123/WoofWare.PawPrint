using System;
using System.Reflection;

// `MethodBase.Invoke` on targets with byref (`ref`/`out`/`in`) parameters, down to the
// `RuntimeMethodHandle_InvokeMethod` QCall.
//
// CoreCLR's QCall does exactly one thing with a byref parameter: `InvokeUtil::CopyArg`'s
// `ELEMENT_TYPE_BYREF` case passes `args[i]` through as the argument itself. Everything else —
// copying a boxed value-type argument so the caller's box is never mutated (`TryByRefFastPath`),
// filling a null `out` slot with a default box (`TryChangeType`), and writing the private copy back
// into the caller's `object[]` after a normal return (`CopyBack`) — is managed code in
// `MethodBaseInvoker`, which PawPrint interprets. The checks below pin the visible consequences of
// that split, so a QCall that dereferenced the byref, or copied in and out on its own, would fail
// them.
//
// EVERY DISTINCT MethodInfo BELOW IS INVOKED EXACTLY ONCE: after the first invocation the host
// switches to a Reflection.Emit invoker and stops exercising the QCall (see
// `ReflectionInvokeMethod.cs`).
//
// Returns 0 on success, or the number of the first check that failed.
// Reference-free on purpose: the managed layer copies a boxed value-type argument with
// `RuntimeHelpers.Box` over the box's raw data, and a struct holding a reference stops there on a
// pre-existing gap (see the parked `RuntimeHelpersBoxReferenceContainingStruct.cs`, and
// `ReflectionInvokeByRefReferenceStruct.cs` for this file's counterpart).
public struct Pair
{
    public int A;
    public int N;
}

public enum Colour : short
{
    Red = 1,
    Blue = 2,
}

public sealed class Counter
{
    public int Count;

    // A constructor with a `ref` parameter: the QCall allocates the instance itself
    // (`isConstructor`) and still passes the byref through.
    public Counter (ref int seed)
    {
        seed = seed + 3;
        Count = seed;
    }
}

public class Program
{
    private static void Increment (ref int x)
    {
        x = x + 1;
    }

    private static void SetString (ref string s)
    {
        s = "set";
    }

    private static void AppendBang (ref string s)
    {
        s = s + "!";
    }

    // A struct, written field by field through the byref: the callee's `stfld` needs the byref
    // re-viewed as `Pair`, since a field cannot be selected under the byte view the buffer
    // carries.
    private static void FillPair (ref Pair p)
    {
        p.A = 30;
        p.N = p.N + 5;
    }

    // Five parameters, so `InvokeWithManyArgs` builds the buffer, with byrefs at index 0 (the
    // bare address) and index 4 (a cursor).
    private static int Five (ref int a, int b, string c, int d, ref string e)
    {
        a = a + b + d;
        e = c + e;
        return b * d;
    }

    // Four parameters, so `InvokeDirectByRefWithFewArgs` builds the buffer, with byrefs at
    // index 1 and 3 (both cursors into the inline array).
    private static void Mixed (int a, ref int b, string c, ref string d)
    {
        b = a + b;
        d = c + d;
    }

    private static void WriteThenThrow (ref int x)
    {
        x = 99;
        throw new InvalidOperationException ("after the write");
    }

    private static void Twice (ref int x)
    {
        x = x * 2;
    }

    private static int ReadIn (in int x)
    {
        return x * 3;
    }

    // A byref to an array: a reference-type element with no nominal type of its own, so the
    // byref can only be handed over viewed as `System.Object`.
    private static void Grow (ref int[] a)
    {
        int[] grown = new int[a.Length + 1];
        Array.Copy (a, grown, a.Length);
        grown[a.Length] = 4;
        a = grown;
    }

    // A whole-struct store (`stobj`) through the byref, rather than field by field.
    private static void ReplacePair (ref Pair p)
    {
        p = new Pair
        {
            A = 7,
            N = 8,
        };
    }

    private static void Promote (ref Colour c)
    {
        c = Colour.Blue;
    }

    // A byref whose element is a method type parameter, instantiated at a reference type with
    // no nominal type of its own.
    private static void Assign<T> (ref T x, T y)
    {
        x = y;
    }

    private static MethodInfo Get (string name)
    {
        MethodInfo m = typeof (Program).GetMethod (name, BindingFlags.Static | BindingFlags.NonPublic);

        if (m == null)
            throw new Exception ("could not find " + name);

        return m;
    }

    public static int Main (string[] args)
    {
        // 1: `int.TryParse(string, out int)` with a boxed default in the `out` slot — the shape
        // ASP.NET's route parameter binding reaches through the LINQ expression interpreter, whose
        // `ByRefUpdater` passes the interpreter variable's current (boxed) value. A *null* `out`
        // slot is a different primitive: `TryChangeType` fills it with
        // `RuntimeHelpers.GetUninitializedObject`, whose QCall is not implemented; see the parked
        // `ReflectionInvokeOutNullSlot.cs`.
        MethodInfo tryParse =
            typeof (int).GetMethod ("TryParse", new[] { typeof (string), typeof (int).MakeByRefType () });

        if (tryParse == null)
            return 1;

        object[] parseArgs = new object[] { "5", 0 };
        object parsed = tryParse.Invoke (null, parseArgs);

        if (!(parsed is bool ok) || !ok || !(parseArgs[1] is int parsedValue) || parsedValue != 5)
            return 1;

        // 2: a boxed value-type argument is copied before the call: the array slot holds the
        // updated value, in a different box, and the caller's original box is untouched.
        object original = 41;
        object[] incArgs = new object[] { original };
        Get ("Increment").Invoke (null, incArgs);

        if (!(incArgs[0] is int incremented) || incremented != 42)
            return 2;

        if ((int) original != 41 || ReferenceEquals (incArgs[0], original))
            return 2;

        // 3: a reference-type element, from a null slot and from a non-null one.
        object[] setArgs = new object[] { null };
        Get ("SetString").Invoke (null, setArgs);

        if (!(setArgs[0] is string set) || set != "set")
            return 3;

        object[] bangArgs = new object[] { "a" };
        Get ("AppendBang").Invoke (null, bangArgs);

        if (!(bangArgs[0] is string banged) || banged != "a!")
            return 3;

        // 4: a struct, read and written field by field through the byref; the caller's box is
        // again a different object from the one the callee wrote.
        Pair pair = new Pair
        {
            A = 0,
            N = 1,
        };
        object pairBox = pair;
        object[] pairArgs = new object[] { pairBox };
        Get ("FillPair").Invoke (null, pairArgs);

        if (!(pairArgs[0] is Pair filled) || filled.A != 30 || filled.N != 6)
            return 4;

        if (((Pair) pairBox).A != 0 || ((Pair) pairBox).N != 1)
            return 4;

        // 5: the many-arguments buffer, byrefs at both ends.
        object[] fiveArgs = new object[] { 1, 2, "c", 3, "e" };
        object fiveRet = Get ("Five").Invoke (null, fiveArgs);

        if (!(fiveRet is int fiveValue) || fiveValue != 6)
            return 5;

        if (!(fiveArgs[0] is int fiveA) || fiveA != 6 || !(fiveArgs[4] is string fiveE) || fiveE != "ce")
            return 5;

        // 6: the few-arguments buffer with byrefs at cursor positions only.
        object[] mixedArgs = new object[] { 10, 5, "p", "q" };
        Get ("Mixed").Invoke (null, mixedArgs);

        if (!(mixedArgs[1] is int mixedB) || mixedB != 15 || !(mixedArgs[3] is string mixedD) || mixedD != "pq")
            return 6;

        // 7: a callee that writes and then throws: the exception is wrapped, and the caller's
        // array still holds what it put there, because `CopyBack` runs only after a normal return.
        object[] throwArgs = new object[] { 7 };
        bool threw = false;

        try
        {
            Get ("WriteThenThrow").Invoke (null, throwArgs);
        }
        catch (TargetInvocationException e) when (e.InnerException is InvalidOperationException)
        {
            threw = true;
        }

        if (!threw || !(throwArgs[0] is int afterThrow) || afterThrow != 7)
            return 7;

        // 8: a wrongly-typed argument for a `ref int` is rejected by the managed layer before the
        // QCall, as a bare ArgumentException rather than a TargetInvocationException.
        bool rejected = false;

        try
        {
            Get ("Twice").Invoke (null, new object[] { "x" });
        }
        catch (ArgumentException)
        {
            rejected = true;
        }

        if (!rejected)
            return 8;

        // 9: an `in` parameter is a byref the callee only reads.
        object inRet = Get ("ReadIn").Invoke (null, new object[] { 4 });

        if (!(inRet is int inValue) || inValue != 12)
            return 9;

        // 10: a byref to an array, read (its length) and replaced by the callee.
        object[] growArgs = new object[] { new int[] { 1, 2, 3 } };
        Get ("Grow").Invoke (null, growArgs);

        if (!(growArgs[0] is int[] grown) || grown.Length != 4 || grown[3] != 4 || grown[0] != 1)
            return 10;

        // 11: a whole-struct store through the byref.
        object[] replaceArgs = new object[] { new Pair { A = 1, N = 2 } };
        Get ("ReplacePair").Invoke (null, replaceArgs);

        if (!(replaceArgs[0] is Pair replaced) || replaced.A != 7 || replaced.N != 8)
            return 11;

        // 12: an enum element, whose box payload is two bytes wide.
        object[] colourArgs = new object[] { Colour.Red };
        Get ("Promote").Invoke (null, colourArgs);

        if (!(colourArgs[0] is Colour promoted) || promoted != Colour.Blue)
            return 12;

        // 13: a constructor with a `ref` parameter, through the allocating `ConstructorInfo.Invoke`.
        ConstructorInfo counterCtor = typeof (Counter).GetConstructor (new[] { typeof (int).MakeByRefType () });

        if (counterCtor == null)
            return 13;

        object[] ctorArgs = new object[] { 4 };
        object counter = counterCtor.Invoke (ctorArgs);

        if (!(counter is Counter built) || built.Count != 7 || !(ctorArgs[0] is int seeded) || seeded != 7)
            return 13;

        // 14: `ref T` instantiated at an array type.
        MethodInfo assignArrays = Get ("Assign").MakeGenericMethod (typeof (int[]));
        object[] assignArgs = new object[] { new int[] { 1 }, new int[] { 2, 3 } };
        assignArrays.Invoke (null, assignArgs);

        if (!(assignArgs[0] is int[] assigned) || assigned.Length != 2 || assigned[1] != 3)
            return 14;

        return 0;
    }
}
