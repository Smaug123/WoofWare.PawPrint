using System;
using System.Reflection;

// `Delegate.CreateDelegate`/`MethodInfo.CreateDelegate` over a method that has a MethodDef row,
// which is the QCall `Delegate_BindToMethodInfo` reached with a metadata target rather than one
// minted by `Reflection.Emit`.
//
// Returns 0 on success, or the number of the first check that failed. Every code is below 128, so
// that none can be mistaken for the 128+signo a signalled guest reports.

public interface INamed
{
    string Named ();
}

public class Targets : INamed
{
    public int Field;

    public Targets (int f)
    {
        Field = f;
    }

    // One argument wider than `Func<int, int>::Invoke`, so binding it closes over the first.
    public static int AddTo (string prefix, int n)
    {
        return prefix.Length + n;
    }

    // Rung E's own shape: a static generic method, bound as an instantiation, closed over its
    // first argument.
    public static TOut Thunk<TIn, TOut> (Func<TIn, TOut> f, TIn x)
    {
        return f (x);
    }

    // Matching arity, so binding it produces an open delegate.
    public static int Double (int n)
    {
        return n * 2;
    }

    public static int NullTolerant (string s, int n)
    {
        return (s == null ? 100 : 0) + n;
    }

    // First argument is a value type, which a closed static binding must refuse.
    public static int TwoInts (int a, int b)
    {
        return a + b;
    }

    public static string ReturnsString (object o)
    {
        return "s";
    }

    public static int TakesInt (int x)
    {
        return x;
    }

    // Control for `TwoInts`: identical but for the first argument being an object reference,
    // which is what a closed static binding requires.
    public static int ObjThenInt (object a, int b)
    {
        return b;
    }

    public static void GenericDef<T> ()
    {
    }

    public int Plus (int n)
    {
        return Field + n;
    }

    // Two further parameters after the receiver, so an open binding's remaining-argument pairing
    // has a length a wrong pairing would disagree with.
    public int Sum (int a, int b)
    {
        return Field + a + b;
    }

    // Does not touch `this`, so it runs rather than faulting when closed over a null receiver.
    public string NoThis ()
    {
        return "noThis";
    }

    public static void VarArgs (__arglist)
    {
    }

    public virtual string Name ()
    {
        return "Targets";
    }

    public string Named ()
    {
        return "named";
    }
}

public class Derived : Targets
{
    public Derived (int f)
        : base (f)
    {
    }

    public override string Name ()
    {
        return "Derived";
    }
}

// An instance method whose signature mentions the declaring type's own generic parameter, so
// binding it has to read the signature under the declaring type's instantiation.
public class Wrapper<T>
{
    public T V;

    public Wrapper (T v)
    {
        V = v;
    }

    public string Describe (int n)
    {
        return V.ToString () + n.ToString ();
    }
}

// An instance method on a value type. Binding it open promotes the receiver to a byref, so the
// delegate has to declare a `ref` first parameter; binding it closed takes the receiver from the
// boxed object supplied.
public struct Cell
{
    public int X;

    public Cell (int x)
    {
        X = x;
    }

    public int Get (int n)
    {
        return X + n;
    }

    // Roslyn leaves a struct's override non-`final`, so this is `virtual` and not `final`; CoreCLR
    // nonetheless binds it directly rather than through a virtual call stub, because the declaring
    // type is a value type.
    public override string ToString ()
    {
        return "Cell:" + X.ToString ();
    }
}

public delegate int CellByRef (ref Cell c, int n);

public delegate int CellByValue (Cell c, int n);

public delegate string CellToString (ref Cell c);

public class Impl : INamed
{
    public string Named ()
    {
        return "impl";
    }
}

public static class Program
{
    static MethodInfo M (string name)
    {
        return typeof (Targets).GetMethod (name);
    }

    static bool Throws<TExn> (Func<object> f)
        where TExn : Exception
    {
        try
        {
            f ();
            return false;
        }
        catch (TExn)
        {
            return true;
        }
    }

    // Which check is in flight, so that a check which *throws* where it should have succeeded is
    // reported as that check rather than as an unhandled exception. Every bind failure surfaces as
    // an `ArgumentException` from `CreateDelegate`, so without this a regression in any rule below
    // would name no rule at all.
    static int step;

    public static int Main ()
    {
        try
        {
            return Run ();
        }
        catch (Exception e)
        {
            Console.Error.WriteLine ($"check {step} threw {e.GetType ().Name}: {e.Message}");
            return step;
        }
    }

    static int Run ()
    {
        // 1: closed over the first argument of a static method.
        step = 1;
        Func<int, int> closedStatic = (Func<int, int>) M ("AddTo").CreateDelegate (typeof (Func<int, int>), "xy");
        if (closedStatic (5) != 7)
        {
            return 1;
        }

        // 2: the same, where the target is a generic method instantiation.
        step = 2;
        MethodInfo thunk = M ("Thunk").MakeGenericMethod (typeof (int), typeof (int));
        Func<int, int> inner = x => x + 3;
        Func<int, int> closedGeneric = (Func<int, int>) thunk.CreateDelegate (typeof (Func<int, int>), inner);
        if (closedGeneric (4) != 7)
        {
            return 2;
        }

        // 3: open over a static method, through the overload that passes OpenDelegateOnly.
        step = 3;
        Func<int, int> openStatic = (Func<int, int>) M ("Double").CreateDelegate (typeof (Func<int, int>));
        if (openStatic (4) != 8)
        {
            return 3;
        }

        // 4: the same binding through the overload that does not, so only the arity decides.
        step = 4;
        Func<int, int> openStatic2 = (Func<int, int>) M ("Double").CreateDelegate (typeof (Func<int, int>), null);
        if (openStatic2 (4) != 8)
        {
            return 4;
        }

        // 5: closed over a receiver, instance method.
        step = 5;
        Func<int, int> closedInstance = (Func<int, int>) M ("Plus").CreateDelegate (typeof (Func<int, int>), new Targets (10));
        if (closedInstance (5) != 15)
        {
            return 5;
        }

        // 6: open over an instance method, so Invoke's first argument is the receiver. The target's
        // total argument count counts the implicit `this`.
        step = 6;
        Func<Targets, int, int> openInstance =
            (Func<Targets, int, int>) M ("Plus").CreateDelegate (typeof (Func<Targets, int, int>));
        if (openInstance (new Targets (10), 5) != 15)
        {
            return 6;
        }

        // 7: closed over a receiver whose runtime type overrides the target. CoreCLR virtualises at
        // bind time, so the override is what runs.
        step = 7;
        Func<string> virtualOverride = (Func<string>) M ("Name").CreateDelegate (typeof (Func<string>), new Derived (0));
        if (virtualOverride () != "Derived")
        {
            return 7;
        }

        // 8: the same, where the receiver's type is the declaring type, so nothing is virtualised.
        step = 8;
        Func<string> virtualExact = (Func<string>) M ("Name").CreateDelegate (typeof (Func<string>), new Targets (0));
        if (virtualExact () != "Targets")
        {
            return 8;
        }

        // 9: an interface method bound over an implementing receiver.
        step = 9;
        Func<string> viaInterface =
            (Func<string>) typeof (INamed).GetMethod ("Named").CreateDelegate (typeof (Func<string>), new Impl ());
        if (viaInterface () != "impl")
        {
            return 9;
        }

        // 10: closed over null. The shape comes from the arity, not from whether a target was
        // supplied, so this is a closed delegate whose bound argument is null.
        step = 10;
        Func<int, int> closedOverNull =
            (Func<int, int>) M ("NullTolerant").CreateDelegate (typeof (Func<int, int>), null);
        if (closedOverNull (5) != 105)
        {
            return 10;
        }

        // 11: relaxed matching on the return type: a target returning `string` may back a delegate
        // returning `object`.
        step = 11;
        Func<object, object> relaxedReturn =
            (Func<object, object>) M ("ReturnsString").CreateDelegate (typeof (Func<object, object>));
        if ((string) relaxedReturn (null) != "s")
        {
            return 11;
        }

        // 12: relaxed matching sees through an enum to its underlying type.
        step = 12;
        Func<DayOfWeek, int> enumArg = (Func<DayOfWeek, int>) M ("TakesInt").CreateDelegate (typeof (Func<DayOfWeek, int>));
        if (enumArg (DayOfWeek.Thursday) != 4)
        {
            return 12;
        }

        // 13: `Target` is the bound object for a closed delegate and null for an open one.
        step = 13;
        if (!ReferenceEquals (closedGeneric.Target, inner))
        {
            return 13;
        }

        if (openStatic.Target != null)
        {
            return 14;
        }

        if (closedOverNull.Target != null)
        {
            return 15;
        }

        // 16: `Method` names the target, which is read back out of the same field this binding
        // wrote.
        step = 16;
        if (openStatic.Method.Name != "Double")
        {
            return 16;
        }

        if (closedInstance.Method.Name != "Plus")
        {
            return 17;
        }

        if (virtualOverride.Method.DeclaringType != typeof (Derived))
        {
            return 18;
        }

        // 19: a pairing whose arity cannot match is a bind failure, which the managed caller turns
        // into ArgumentException rather than crashing.
        step = 19;
        if (!Throws<ArgumentException> (() => M ("TwoInts").CreateDelegate (typeof (Func<int, int, int, int>), null)))
        {
            return 19;
        }

        // 20: so is a pairing whose argument types do not match.
        step = 20;
        if (!Throws<ArgumentException> (() => M ("Double").CreateDelegate (typeof (Func<string, string>))))
        {
            return 20;
        }

        // 21: OpenDelegateOnly refuses a pairing whose arity makes it closed. Check 1 is the
        // control: the same method and delegate type bind through the overload that does not pass
        // the flag.
        step = 21;
        if (!Throws<ArgumentException> (() => M ("AddTo").CreateDelegate (typeof (Func<int, int>))))
        {
            return 21;
        }

        // 22: a delegate closed over a static method whose first argument is a value type is
        // refused, whatever is supplied for it.
        step = 22;
        if (!Throws<ArgumentException> (() => M ("TwoInts").CreateDelegate (typeof (Func<int, int>), 1)))
        {
            return 22;
        }

        // 23: control for 22. The same shape with an object-reference first argument binds, which
        // is what makes 22 a statement about objref-ness rather than about arity or assignability.
        step = 23;
        Func<int, int> closedOverBoxedInt =
            (Func<int, int>) M ("ObjThenInt").CreateDelegate (typeof (Func<int, int>), 1);
        if (closedOverBoxedInt (7) != 7)
        {
            return 23;
        }

        // 24: a generic method definition cannot be dispatched to, and the QCall *raises* rather
        // than reporting a bind failure. The overload that suppresses bind failures is what
        // distinguishes the two: a FALSE return here would be a null result, where the raise
        // propagates.
        step = 24;
        if (!Throws<ArgumentException> (() =>
                Delegate.CreateDelegate (typeof (Action), M ("GenericDef"), false)))
        {
            return 24;
        }

        // 25: control for 24. Its instantiation binds and runs, so 24 is a statement about the
        // definition rather than about the signature.
        step = 25;
        ((Action) M ("GenericDef").MakeGenericMethod (typeof (int)).CreateDelegate (typeof (Action))) ();

        // 26: closed over a receiver whose declaring type is a generic instantiation, so the
        // target's signature has to be read under that instantiation.
        step = 26;
        Func<int, string> genericDeclaring = (Func<int, string>)
            typeof (Wrapper<string>).GetMethod ("Describe")
                .CreateDelegate (typeof (Func<int, string>), new Wrapper<string> ("q"));
        if (genericDeclaring (3) != "q3")
        {
            return 26;
        }

        // 27: and open over the same method.
        step = 27;
        Func<Wrapper<string>, int, string> genericDeclaringOpen = (Func<Wrapper<string>, int, string>)
            typeof (Wrapper<string>).GetMethod ("Describe").CreateDelegate (typeof (Func<Wrapper<string>, int, string>));
        if (genericDeclaringOpen (new Wrapper<string> ("z"), 8) != "z8")
        {
            return 27;
        }

        // 28: closed over a value type's instance method. The receiver arrives boxed, which is
        // what a closed binding over an instance method of a value type has to accept.
        step = 28;
        Func<int, int> closedStruct = (Func<int, int>)
            typeof (Cell).GetMethod ("Get").CreateDelegate (typeof (Func<int, int>), new Cell (10));
        if (closedStruct (5) != 15)
        {
            return 28;
        }

        // 29: open over the same method. The first argument of the target is promoted to a byref
        // because the declaring type is a value type, so the delegate must declare `ref Cell`.
        step = 29;
        CellByRef openStruct = (CellByRef) typeof (Cell).GetMethod ("Get").CreateDelegate (typeof (CellByRef));
        Cell cell = new Cell (20);
        if (openStruct (ref cell, 5) != 25)
        {
            return 29;
        }

        // 30: control for 29. The same delegate shape taking the receiver *by value* is refused,
        // which is what makes 29 a statement about the promotion rather than about arity.
        step = 30;
        if (!Throws<ArgumentException> (() => typeof (Cell).GetMethod ("Get").CreateDelegate (typeof (CellByValue))))
        {
            return 30;
        }

        // 31: open over a value type's *override*. `virtual` and not `final`, so a refusal keyed on
        // virtualness alone would reject it; CoreCLR exempts value types and binds it directly.
        step = 31;
        CellToString structOverride = (CellToString)
            typeof (Cell).GetMethod ("ToString", Type.EmptyTypes).CreateDelegate (typeof (CellToString));
        Cell named = new Cell (4);
        if (structOverride (ref named) != "Cell:4")
        {
            return 31;
        }

        // 32: closed over a boxed value type, where the declaring type is a reference type and the
        // receiver's runtime type overrides the target — so this is bind-time virtualisation with a
        // value-type receiver.
        step = 32;
        Func<string> boxedVirtual = (Func<string>)
            typeof (object).GetMethod ("ToString").CreateDelegate (typeof (Func<string>), new Cell (7));
        if (boxedVirtual () != "Cell:7")
        {
            return 32;
        }

        // 33: closed over a null receiver. Binding does not virtualise — there is no receiver to
        // virtualise on — and a body that never touches `this` runs normally.
        step = 33;
        Func<string> nullReceiver = (Func<string>) M ("NoThis").CreateDelegate (typeof (Func<string>), null);
        if (nullReceiver () != "noThis")
        {
            return 33;
        }

        if (nullReceiver.Target != null)
        {
            return 34;
        }

        // 35: open over an instance method with two further parameters. A remaining-argument
        // pairing that dropped the target's first parameter — right for a static target, wrong for
        // an instance one — disagrees about the length here, where a one-parameter target hides it.
        step = 35;
        Func<Targets, int, int, int> openInstanceWide =
            (Func<Targets, int, int, int>) M ("Sum").CreateDelegate (typeof (Func<Targets, int, int, int>));
        if (openInstanceWide (new Targets (1), 2, 3) != 6)
        {
            return 35;
        }

        // 36: a vararg target cannot back a non-vararg delegate.
        step = 36;
        if (!Throws<ArgumentException> (() => M ("VarArgs").CreateDelegate (typeof (Action))))
        {
            return 36;
        }

        return 0;
    }
}
