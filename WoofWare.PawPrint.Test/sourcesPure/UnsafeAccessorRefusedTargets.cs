using System;
using System.Runtime.CompilerServices;

// Targets an accessor can *name* but whose synthesised body cannot then run them, each raised into
// the guest by the shape of the one instruction CoreCLR emits. All measured on real .NET 10.
//
//   * a `static abstract` interface member reached by `StaticMethod`, whose body is a `call`, and a
//     `call` to an abstract method is not valid IL;
//   * an abstract class's constructor reached by `Constructor`, whose body is a `newobj`;
//   * a `const` field, which has a metadata row and no storage, so it is not a candidate at all.
//
// The abstract-class case also pins the *order*: a missing constructor is reported as a missing
// constructor even when the class is abstract, so the body's refusal comes after the lookup.
public class TestUnsafeAccessorRefusedTargets
{
    private interface IStaticAbstract
    {
        static abstract int F();
    }

    private abstract class Abstract
    {
        private Abstract()
        {
        }

        protected Abstract(int x)
        {
        }
    }

    private abstract class AbstractNoMatchingCtor
    {
        private AbstractNoMatchingCtor(int x)
        {
        }
    }

    private class Literals
    {
        private const int Constant = 5;

        private static int _real = 9;
    }

    [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "F")]
    private static extern int StaticAbstractTarget(IStaticAbstract i);

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern Abstract NewAbstract();

    [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
    private static extern AbstractNoMatchingCtor NewAbstractNoMatch();

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "Constant")]
    private static extern ref int ConstantField(Literals l);

    [UnsafeAccessor(UnsafeAccessorKind.StaticField, Name = "_real")]
    private static extern ref int RealField(Literals l);

    private static int Check<TExpected>(int code, Action a)
        where TExpected : Exception
    {
        try
        {
            a();
            return code;
        }
        catch (TExpected)
        {
            return 0;
        }
    }

    private static int Run()
    {
        int r;

        r = Check<BadImageFormatException>(1, () => StaticAbstractTarget(null));
        if (r != 0) return r;

        // The HResult is checked too: a synthesised exception whose type is right but whose
        // HResult is the fallback `COR_E_EXCEPTION` is a difference a guest can read.
        try
        {
            NewAbstract();
            return 2;
        }
        catch (InvalidOperationException e)
        {
            if (e.HResult != unchecked((int) 0x80131509)) return 20;
        }

        // The class is abstract *and* has no matching constructor; the lookup failure is what is
        // reported, so the abstract check happens after it.
        r = Check<MissingMethodException>(3, () => NewAbstractNoMatch());
        if (r != 0) return r;

        // A `const` has no storage to take the address of.
        r = Check<MissingFieldException>(4, () => ConstantField(null));
        if (r != 0) return r;

        // ... while an ordinary static field beside it is reachable, so the exclusion is about
        // being a literal rather than about the type.
        if (RealField(null) != 9) return 5;

        return 0;
    }

    public static int Main() => Run();
}
