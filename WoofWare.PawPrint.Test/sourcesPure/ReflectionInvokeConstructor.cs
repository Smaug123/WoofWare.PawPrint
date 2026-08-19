using System;
using System.Reflection;

// `ConstructorInfo.Invoke` down to its primitive: the `isConstructor = true` branch of the
// `RuntimeMethodHandle_InvokeMethod` QCall (reflectioninvocation.cpp:311), reached via
// `MethodBaseInvoker.InterpretedInvoke_Constructor`. Unlike the method branch, the QCall allocates
// the instance itself (`gc.retVal = pMT->Allocate()`) and hands it back as the result.
//
// EVERY DISTINCT ConstructorInfo BELOW IS INVOKED EXACTLY ONCE.
// `MethodInvokerCommon.DetermineStrategy_RefArgs` / `_ObjSpanArgs` take the interpreted
// `RuntimeMethodHandle.InvokeMethod` path only on a given `MethodBase`'s *first* invocation and
// build a Reflection.Emit delegate for every invocation after that; the `MethodBaseInvoker` is
// cached on the `RuntimeConstructorInfo`, so re-fetching the ConstructorInfo does not reset it.
// The exception is `RuntimeConstructorInfo.Invoke(obj, ...)`, which runs a constructor against an
// instance the caller already has: both of `InvokeConstructorWithoutAlloc`'s overloads call
// `InterpretedInvoke_Constructor` directly and never consult a strategy, so those are repeatable.
public class Program
{
    // Two fields the constructor never touches. A class constructor does not zero its own
    // storage — that is the allocation's job — so these pin that the QCall allocated a zeroed
    // instance rather than handing back whatever the heap held.
    public class Plain
    {
        public int Set;
        public int Untouched;
        public string AlsoUntouched;

        public Plain ()
        {
            Set = 7;
        }
    }

    // A reference-type parameter (whose argument-buffer byref addresses an `object?` slot) and a
    // value-type one (whose byref addresses a box payload) in one signature.
    public class Mixed
    {
        public string Name;
        public int Value;

        public Mixed (string name, int value)
        {
            Name = name;
            Value = value;
        }
    }

    // Six arguments, which is what routes through `MethodBaseInvoker.InvokeWithManyArgs` and so
    // through the `GCFrameRegistration` pair, rather than the `StackAllocatedByRefs` buffer the
    // shorter signatures use.
    public class ManyArgs
    {
        public int Sum;
        public string Joined;

        public ManyArgs (int a, string b, int c, string d, int e, string f)
        {
            Sum = a + c + e;
            Joined = b + d + f;
        }
    }

    // A value-type constructor. CoreCLR forms `this` as `gc.retVal->GetData()`, a pointer into the
    // payload of the box it allocated, and hands that same box back — so the constructor writes
    // through to the object `Invoke` returns. An implementation that constructed into a detached
    // copy would answer a box of zeroes.
    public struct SPoint
    {
        public int X;
        public int Y;

        public SPoint (int x, int y)
        {
            X = x;
            Y = y;
        }
    }

    public class Base
    {
        public int FromBase;

        public Base (int fromBase)
        {
            FromBase = fromBase;
        }
    }

    // The constructed instance's storage must cover the whole base chain, not just the fields the
    // most-derived type declares.
    public class Derived : Base
    {
        public int FromDerived;

        public Derived (int fromBase, int fromDerived)
            : base (fromBase)
        {
            FromDerived = fromDerived;
        }
    }

    public class Box<T>
    {
        public T Held;

        public Box (T held)
        {
            Held = held;
        }
    }

    // A closed generic *struct*: the receiver coercion has to classify the substituted declaring
    // type as a value type and unbox into the allocation's payload, which the non-generic struct
    // case above and the generic class case do not jointly cover.
    public struct GPoint<T>
    {
        public T First;
        public int Second;

        public GPoint (T first, int second)
        {
            First = first;
            Second = second;
        }
    }

    // A null reference-type argument: `readArgument`'s reference branch reads an `object?` slot
    // holding null, which is a different value from the null byref it refuses.
    public class HoldsReference
    {
        public string Held;
        public bool Ran;

        public HoldsReference (string held)
        {
            Held = held;
            Ran = true;
        }
    }

    private static string trace = "";

    // Not `beforefieldinit` (it declares an explicit static constructor), so the initialiser runs
    // at the moment of the first construction and not before. Nothing else in this file touches
    // the type, so the reflective `Invoke` below is what triggers class initialisation.
    public class LazilyInitialised
    {
        static LazilyInitialised ()
        {
            trace += "cctor;";
        }

        public string TraceAtConstruction;

        // Deliberately reads *Program*'s static rather than one of its own, so the body contains
        // nothing that would trigger this type's initialisation by itself.
        public LazilyInitialised ()
        {
            TraceAtConstruction = trace;
        }
    }

    public class Throws
    {
        public Throws ()
        {
            throw new InvalidOperationException ("boom");
        }
    }

    // A class whose initialiser throws. CoreCLR's constructor path runs the initialiser from the
    // instance constructor's prologue rather than from the QCall; PawPrint runs it from the QCall,
    // so this pins that the guest cannot tell — same exception nesting, and the
    // `TypeInitializationException`'s own trace names no instance constructor on either runtime.
    public class BadCctor
    {
        static BadCctor ()
        {
            throw new InvalidOperationException ("from cctor");
        }

        public BadCctor ()
        {
        }
    }

    // Six arguments through `System.Reflection.ConstructorInvoker`, the modern allocating-construction
    // API. It reaches the same QCall branch, but by way of `ConstructorInvoker.InvokeWithManyArgs`,
    // which registers a `GCFrameRegistration` over its own `stackalloc` — a different
    // argument-buffer shape from the `StackAllocatedByRefs` local the shorter signatures use.
    public class ViaInvoker
    {
        public int Sum;
        public string Joined;

        public ViaInvoker (int a, string b, int c, string d, int e, string f)
        {
            Sum = a + c + e;
            Joined = b + d + f;
        }
    }

    private static int reinitCount;

    // `RuntimeConstructorInfo.Invoke(obj, ...)` runs a constructor against an instance the caller
    // already has, and reaches the QCall with `isConstructor: obj is null` — i.e. false. It must
    // therefore allocate nothing and answer null.
    public class Reinit
    {
        public int Value;

        public Reinit ()
        {
            reinitCount = reinitCount + 1;
            Value = reinitCount;
        }
    }

    private static ConstructorInfo Only (Type t)
    {
        ConstructorInfo[] ctors = t.GetConstructors ();

        if (ctors.Length != 1)
            throw new Exception ("expected exactly one constructor on " + t);

        return ctors[0];
    }

    public static int Main (string[] args)
    {
        // Parameterless constructor of a class.
        object plainResult = Only (typeof (Plain)).Invoke (new object[0]);

        if (plainResult == null)
            return 1;

        if (plainResult.GetType () != typeof (Plain))
            return 2;

        Plain plain = (Plain) plainResult;

        if (plain.Set != 7)
            return 3;

        // Zeroed by the allocation, not by the constructor.
        if (plain.Untouched != 0)
            return 4;

        if (plain.AlsoUntouched != null)
            return 5;

        // Reference-type and value-type arguments through a constructor signature.
        object mixedResult = Only (typeof (Mixed)).Invoke (new object[] { "hi", 42 });

        if (!(mixedResult is Mixed mixed))
            return 6;

        if (mixed.Name != "hi")
            return 7;

        if (mixed.Value != 42)
            return 8;

        // Six arguments: the `InvokeWithManyArgs` buffer shape.
        object manyResult = Only (typeof (ManyArgs))
            .Invoke (new object[] { 1, "a", 2, "b", 3, "c" });

        if (!(manyResult is ManyArgs many))
            return 9;

        if (many.Sum != 6)
            return 10;

        if (many.Joined != "abc")
            return 11;

        // A value type: the result is a box the constructor wrote into.
        object pointResult = Only (typeof (SPoint)).Invoke (new object[] { 3, 4 });

        if (pointResult == null)
            return 12;

        if (pointResult.GetType () != typeof (SPoint))
            return 13;

        SPoint point = (SPoint) pointResult;

        if (point.X != 3)
            return 14;

        if (point.Y != 4)
            return 15;

        // The whole base chain is laid out.
        object derivedResult = Only (typeof (Derived)).Invoke (new object[] { 11, 22 });

        if (!(derivedResult is Derived derived))
            return 16;

        if (derived.FromBase != 11)
            return 17;

        if (derived.FromDerived != 22)
            return 18;

        // A closed generic declaring type, so the declaring type's generic arguments have to be
        // substituted through the allocation as well as through the call.
        object boxResult = Only (typeof (Box<int>)).Invoke (new object[] { 99 });

        if (!(boxResult is Box<int> boxed))
            return 19;

        if (boxed.Held != 99)
            return 20;

        // Class initialisation triggered by the invocation itself, asserted by *ordering*: a
        // handler that skipped initialisation would still answer "cctor;" if something upstream had
        // already run the initialiser, so pin that nothing has.
        ConstructorInfo lazyCtor = Only (typeof (LazilyInitialised));

        if (trace != "")
            return 21;

        object lazyResult = lazyCtor.Invoke (new object[0]);

        if (trace != "cctor;")
            return 22;

        // And it ran *before* the instance constructor's body, not merely at some point during the
        // call.
        if (!(lazyResult is LazilyInitialised lazy))
            return 23;

        if (lazy.TraceAtConstruction != "cctor;")
            return 24;

        // A throwing constructor. `MethodBaseInvoker` wraps in `TargetInvocationException` in
        // *managed* code, so the QCall must let the original propagate unwrapped: if it wrapped as
        // well, `InnerException` would itself be a TargetInvocationException.
        try
        {
            Only (typeof (Throws)).Invoke (new object[0]);
            return 25;
        }
        catch (TargetInvocationException ex)
        {
            if (ex.InnerException is TargetInvocationException)
                return 26;

            if (!(ex.InnerException is InvalidOperationException ioe))
                return 27;

            if (ioe.Message != "boom")
                return 28;
        }

        // `isConstructor = false`: run a constructor against an instance we already have. Nothing
        // is allocated and the answer is null. Without this case an implementation that keyed on
        // "the target is a constructor" rather than on the flag would pass everything above.
        Reinit existing = new Reinit ();

        if (existing.Value != 1)
            return 29;

        object reinitResult = Only (typeof (Reinit)).Invoke (existing, null);

        if (reinitResult != null)
            return 30;

        // The constructor really did run again, against the instance we passed.
        if (existing.Value != 2)
            return 31;

        if (reinitCount != 2)
            return 32;

        // A closed generic value type.
        object gPointResult = Only (typeof (GPoint<string>)).Invoke (new object[] { "s", 8 });

        if (gPointResult == null)
            return 36;

        if (gPointResult.GetType () != typeof (GPoint<string>))
            return 37;

        GPoint<string> gPoint = (GPoint<string>) gPointResult;

        if (gPoint.First != "s")
            return 38;

        if (gPoint.Second != 8)
            return 39;

        // A null reference-type argument.
        object heldResult = Only (typeof (HoldsReference)).Invoke (new object[] { null });

        if (!(heldResult is HoldsReference held))
            return 40;

        if (held.Held != null)
            return 41;

        if (!held.Ran)
            return 42;

        // A throwing class initialiser, reached by the construction itself.
        try
        {
            Only (typeof (BadCctor)).Invoke (new object[0]);
            return 43;
        }
        catch (TargetInvocationException ex)
        {
            if (!(ex.InnerException is TypeInitializationException tie))
                return 44;

            if (!(tie.InnerException is InvalidOperationException fromCctor))
                return 45;

            if (fromCctor.Message != "from cctor")
                return 46;

            // ".." rather than "." so that `BadCctor..cctor()` does not match.
            if (tie.StackTrace == null)
                return 47;

            if (tie.StackTrace.Contains ("..ctor"))
                return 48;
        }

        object viaInvokerResult = ConstructorInvoker
            .Create (Only (typeof (ViaInvoker)))
            .Invoke (new object[] { 4, "x", 5, "y", 6, "z" });

        if (!(viaInvokerResult is ViaInvoker viaInvoker))
            return 33;

        if (viaInvoker.Sum != 15)
            return 34;

        if (viaInvoker.Joined != "xyz")
            return 35;

        return 0;
    }
}
