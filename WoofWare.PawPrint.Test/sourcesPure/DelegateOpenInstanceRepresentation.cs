using System;
using System.Reflection;

// Open instance delegates, built by `Delegate.CreateDelegate` (C# has no method-group syntax for
// them), observed through everything CoreLib reads their fields for: invocation, `Target`,
// `Method`, `Equals`, `GetHashCode`, `DynamicInvoke`, and combination into a multicast delegate.
//
// CoreCLR lays an open delegate out with the delegate itself in `_target`, a shuffle thunk in
// `_methodPtr` and the target in `_methodPtrAux`; an open delegate over a virtual method on a
// reference type gets a virtual call stub in `_methodPtrAux` instead, so the receiver is resolved
// at every invocation (comdelegate.cpp:1236-1245). CoreLib's managed code branches on
// `_methodPtrAux` being nonzero, which is why the layout, and not only the invocation, is
// observable.
//
// Returns 0 on success, or the number of the first check that failed.

public class Animal
{
    public virtual string Speak ()
    {
        return "...";
    }

    public virtual string Move ()
    {
        return "walk";
    }

    public string Kind ()
    {
        return "animal";
    }

    // Reads nothing through `this`, so it runs to completion with a null one.
    public string NullTolerant ()
    {
        return "tolerant";
    }
}

public class Dog : Animal
{
    public override string Speak ()
    {
        return "woof";
    }
}

public class Box<T>
{
    public virtual int Size ()
    {
        return 1;
    }
}

public class BigBox<T> : Box<T>
{
    public override int Size ()
    {
        return 2;
    }
}

public interface IShape
{
    int Sides ();
}

public class Triangle : IShape
{
    public int Sides ()
    {
        return 3;
    }
}

public struct Polygon : IShape
{
    public int N;

    public int Sides ()
    {
        return N;
    }
}

public interface IDefaulted
{
    static virtual int Value ()
    {
        return 5;
    }
}

public class UsesDefault : IDefaulted
{
}

public class OverridesDefault : IDefaulted
{
    public static int Value ()
    {
        return 7;
    }
}

public class GenericMethods
{
    public virtual int Virtual<T> (int x)
    {
        return x;
    }

    public int NonVirtual<T> (int x)
    {
        return x + 1;
    }
}

public static class Program
{
    // `constrained. T ldftn IDefaulted::Value`, which resolves the static virtual to an
    // implementation before the delegate is constructed.
    private static Func<int> ValueOf<T> () where T : IDefaulted
    {
        return T.Value;
    }

    public static int Twice (int x)
    {
        return 2 * x;
    }

    public static int Thrice (int x)
    {
        return 3 * x;
    }

    private static Func<Animal, string> Open (string name)
    {
        return (Func<Animal, string>) typeof (Animal).GetMethod (name).CreateDelegate (typeof (Func<Animal, string>));
    }

    public static int Main ()
    {
        Func<Animal, string> speak = Open ("Speak");
        Func<Animal, string> speakAgain = Open ("Speak");
        Func<Animal, string> move = Open ("Move");
        Func<Animal, string> kind = Open ("Kind");

        // 1-2: an open virtual delegate dispatches on each argument's runtime type.
        if (speak (new Dog ()) != "woof")
        {
            return 1;
        }

        if (speak (new Animal ()) != "...")
        {
            return 2;
        }

        // 3: an open delegate has no target, virtual or not.
        if (speak.Target != null || kind.Target != null)
        {
            return 3;
        }

        // 4: `Method` reports the method bound, not whichever override an invocation reached.
        if (speak.Method.DeclaringType != typeof (Animal) || speak.Method.Name != "Speak")
        {
            return 4;
        }

        // 5-6: two open delegates over one method are equal and hash alike; over different
        // methods they are not equal. Neither has a cached `Method` here, which is what sends
        // `Equals` to the runtime for the second comparison (Delegate.CoreCLR.cs:135-138).
        Func<Animal, string> speakFresh = Open ("Speak");
        Func<Animal, string> moveFresh = Open ("Move");

        if (!speakFresh.Equals (Open ("Speak")) || speakFresh.GetHashCode () != Open ("Speak").GetHashCode ())
        {
            return 5;
        }

        if (speakFresh.Equals (moveFresh) || moveFresh.Equals (speakFresh))
        {
            return 6;
        }

        // 7: an open virtual delegate and an open non-virtual one are different kinds of
        // delegate in CoreCLR, and are not equal either way round.
        if (Open ("Kind").Equals (Open ("Speak")) || Open ("Speak").Equals (Open ("Kind")))
        {
            return 7;
        }

        // 8: `DynamicInvoke` goes through `Invoke`, and so dispatches too.
        if ((string) speak.DynamicInvoke (new Dog ()) != "woof")
        {
            return 8;
        }

        // 9-10: a multicast delegate of open virtual delegates dispatches every element, and
        // `Remove` finds an element by the same equality as check 5.
        Func<Animal, string> all = (Func<Animal, string>) Delegate.Combine (speak, move, speakAgain);

        if (all (new Dog ()) != "woof" || all.GetInvocationList ().Length != 3)
        {
            return 9;
        }

        Func<Animal, string> removed = (Func<Animal, string>) Delegate.Remove (all, Open ("Speak"));

        if (removed.GetInvocationList ().Length != 2 || removed (new Dog ()) != "walk")
        {
            return 10;
        }

        // 11: a virtual call stub reads the receiver's type first, so a null receiver faults at
        // once.
        try
        {
            speak (null);
            return 11;
        }
        catch (NullReferenceException)
        {
        }

        // 12: an open delegate over a *non*-virtual method enters it with a null `this`.
        if (Open ("NullTolerant") (null) != "tolerant")
        {
            return 12;
        }

        // 13-14: on a generic declaring type, `Method` names the exact instantiation, which
        // CoreLib reads off `Invoke`'s first parameter for an open delegate.
        Func<Box<string>, int> size = (Func<Box<string>, int>)
            typeof (Box<string>).GetMethod ("Size").CreateDelegate (typeof (Func<Box<string>, int>));

        if (size (new BigBox<string> ()) != 2 || size (new Box<string> ()) != 1)
        {
            return 13;
        }

        if (size.Method.DeclaringType != typeof (Box<string>))
        {
            return 14;
        }

        // 15-16: an interface method dispatches too, including to a value type's
        // implementation through its box.
        Func<IShape, int> sides = (Func<IShape, int>)
            typeof (IShape).GetMethod ("Sides").CreateDelegate (typeof (Func<IShape, int>));

        if (sides (new Triangle ()) != 3 || sides (new Polygon { N = 5 }) != 5)
        {
            return 15;
        }

        if (sides.Method.DeclaringType != typeof (IShape))
        {
            return 16;
        }

        // 17-18: an open static delegate from a method group and one from reflection are the
        // same shape, and so equal in both directions; over different methods they are not, but
        // still hash alike, since an open delegate hashes by its type alone
        // (Delegate.CoreCLR.cs:153-156).
        Func<int, int> twiceGroup = Twice;
        Func<int, int> twiceReflected = (Func<int, int>)
            typeof (Program).GetMethod ("Twice").CreateDelegate (typeof (Func<int, int>));

        if (!twiceGroup.Equals (twiceReflected) || !twiceReflected.Equals (twiceGroup))
        {
            return 17;
        }

        Func<int, int> thriceGroup = Thrice;

        if (twiceGroup.Equals (thriceGroup) || twiceGroup.GetHashCode () != thriceGroup.GetHashCode ())
        {
            return 18;
        }

        // 19: a delegate *closed* over a null receiver is a different thing from an open one:
        // `Target` is its `_target`, and `Method` on a generic declaring type walks that null
        // target's base chain (Delegate.CoreCLR.cs:189) and faults in CoreLib.
        Func<int> closedOverNull = (Func<int>) typeof (Box<string>).GetMethod ("Size").CreateDelegate (typeof (Func<int>), null);

        try
        {
            MethodInfo unused = closedOverNull.Method;
            return 19;
        }
        catch (NullReferenceException)
        {
        }

        // 20: an open delegate's `_target` is itself, and that is observable. `CombineImpl` appends
        // into a shared invocation-list array in place, and `TrySetSlot` treats an occupied slot
        // as already holding the new element when `_methodPtr`, `_target` and `_methodPtrAux` all
        // match (MulticastDelegate.CoreCLR.cs:146-164). Two distinct open delegates over one
        // method agree in `_methodPtr` and `_methodPtrAux`, so only `_target` keeps the second
        // from being silently replaced by the first.
        Func<Animal, string> first = Open ("Speak");
        Func<Animal, string> second = Open ("Speak");
        Func<Animal, string> prefix = (Func<Animal, string>) Delegate.Combine (Delegate.Combine (move, first), move);
        Delegate.Combine (prefix, first);
        Func<Animal, string> withSecond = (Func<Animal, string>) Delegate.Combine (prefix, second);

        if (!ReferenceEquals (withSecond.GetInvocationList ()[3], second))
        {
            return 20;
        }

        // 21: an open delegate over a static virtual, built by `newobj` from a constrained
        // `ldftn`, calls the implementation the constraint resolved to, default or not. Only
        // `CreateDelegate` puts a static virtual behind a virtual call stub.
        Func<int> defaulted = ValueOf<UsesDefault> ();

        if (defaulted () != 5 || ValueOf<OverridesDefault> () () != 7 || defaulted.Method.DeclaringType != typeof (IDefaulted))
        {
            return 21;
        }

        // 22: a virtual call stub cannot be built over a generic method instantiation, and the
        // runtime raises rather than reporting a bind failure, so `throwOnBindFailure: false` does
        // not suppress it.
        MethodInfo genericVirtual = typeof (GenericMethods).GetMethod ("Virtual").MakeGenericMethod (typeof (int));

        try
        {
            Delegate.CreateDelegate (typeof (Func<GenericMethods, int, int>), genericVirtual, false);
            return 22;
        }
        catch (NotSupportedException)
        {
        }

        // 23: the same method closed over a receiver needs no stub, and nor does an open delegate
        // over a non-virtual generic method.
        Func<int, int> closedGeneric = (Func<int, int>) genericVirtual.CreateDelegate (typeof (Func<int, int>), new GenericMethods ());
        Func<GenericMethods, int, int> openGeneric = (Func<GenericMethods, int, int>)
            typeof (GenericMethods).GetMethod ("NonVirtual").MakeGenericMethod (typeof (int))
                .CreateDelegate (typeof (Func<GenericMethods, int, int>));

        if (closedGeneric (3) != 3 || openGeneric (new GenericMethods (), 3) != 4)
        {
            return 23;
        }

        return 0;
    }
}
