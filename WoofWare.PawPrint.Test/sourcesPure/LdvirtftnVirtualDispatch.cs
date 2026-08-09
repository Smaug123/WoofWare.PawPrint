using System;

// `ldvirtftn` must select the body appropriate to the *receiver's runtime type*, not the
// method named at the call site. Roslyn emits `dup; ldvirtftn; newobj Delegate::.ctor` for
// every method-group conversion whose named method is virtual, so each delegate below is a
// distinct `ldvirtftn`; invoking it then runs whatever body the pointer named, because
// delegate invocation does no dispatch of its own.

public abstract class LdvirtftnAnimal
{
    public abstract int Legs();

    public virtual int Eyes()
    {
        return 2;
    }
}

public class LdvirtftnSpider : LdvirtftnAnimal
{
    public override int Legs()
    {
        return 8;
    }

    public override int Eyes()
    {
        return 8;
    }
}

public class LdvirtftnDog : LdvirtftnAnimal
{
    public override int Legs()
    {
        return 4;
    }
}

public sealed class LdvirtftnAnt : LdvirtftnAnimal
{
    public override int Legs()
    {
        return 6;
    }

    public sealed override int Eyes()
    {
        return 3;
    }
}

public interface ILdvirtftnCounter
{
    int Count();
}

public class LdvirtftnImplicitCounter : ILdvirtftnCounter
{
    public int Count()
    {
        return 11;
    }
}

public class LdvirtftnExplicitCounter : ILdvirtftnCounter
{
    int ILdvirtftnCounter.Count()
    {
        return 12;
    }
}

public struct LdvirtftnStructCounter : ILdvirtftnCounter
{
    public int Count()
    {
        return 13;
    }
}

public static class LdvirtftnVirtualDispatch
{
    public static int Main(string[] argv)
    {
        // Abstract method reached through a base-typed reference: the call site names
        // `LdvirtftnAnimal::Legs`, which has no body at all.
        LdvirtftnAnimal spider = new LdvirtftnSpider();
        Func<int> spiderLegs = spider.Legs;
        if (spiderLegs() != 8)
        {
            return 1;
        }

        // Virtual method that the receiver's runtime type overrides. Taking the call-site
        // method here would run `LdvirtftnAnimal::Eyes` and silently answer 2.
        Func<int> spiderEyes = spider.Eyes;
        if (spiderEyes() != 8)
        {
            return 2;
        }

        // Virtual method the receiver's runtime type does *not* override: dispatch must fall
        // back to the inherited body rather than failing to resolve.
        LdvirtftnAnimal dog = new LdvirtftnDog();
        Func<int> dogEyes = dog.Eyes;
        if (dogEyes() != 2)
        {
            return 3;
        }

        // One call-site token, two receivers of different runtime types: the two pointers
        // must differ.
        Func<int> legsOfSpider = ((LdvirtftnAnimal)new LdvirtftnSpider()).Legs;
        Func<int> legsOfDog = ((LdvirtftnAnimal)new LdvirtftnDog()).Legs;
        if (legsOfSpider() != 8)
        {
            return 4;
        }

        if (legsOfDog() != 4)
        {
            return 5;
        }

        // Receiver whose static type is `sealed`, over a `sealed override`. Roslyn still
        // emits `ldvirtftn`, and the token still names the least-derived non-final
        // declaration, so this is a dispatch and not a fallthrough.
        LdvirtftnAnt ant = new LdvirtftnAnt();
        Func<int> antEyes = ant.Eyes;
        if (antEyes() != 3)
        {
            return 6;
        }

        // Interface method with an implicit implementation.
        ILdvirtftnCounter implicitCounter = new LdvirtftnImplicitCounter();
        Func<int> implicitCount = implicitCounter.Count;
        if (implicitCount() != 11)
        {
            return 7;
        }

        // Interface method with an explicit implementation.
        ILdvirtftnCounter explicitCounter = new LdvirtftnExplicitCounter();
        Func<int> explicitCount = explicitCounter.Count;
        if (explicitCount() != 12)
        {
            return 8;
        }

        // Boxed value-type receiver: the delegate's target is the box, but the resolved body
        // is a value-type instance method, so `this` must arrive as a byref into the box.
        ILdvirtftnCounter structCounter = new LdvirtftnStructCounter();
        Func<int> structCount = structCounter.Count;
        if (structCount() != 13)
        {
            return 9;
        }

        return 0;
    }
}
