using System;

// `CreateDelegate` over a default interface method of an open variant interface definition,
// closed over a receiver that implements it by variance through an interface which overrides the
// default. The binding virtualises onto the receiver, and real .NET resolves the slot to the
// definition's own default body, not to the override the closed instantiation would dispatch to.
//
// Returns 0 on success. Measured on real .NET.

public interface IContra<in T> where T : class
{
    int M()
    {
        return 11;
    }
}

public interface IChild : IContra<object>
{
    int IContra<object>.M()
    {
        return 22;
    }
}

public class C : IChild
{
}

public static class Program
{
    public static int Main()
    {
        Func<int> f = (Func<int>)typeof(IContra<>).GetMethod("M").CreateDelegate(typeof(Func<int>), new C());

        if (f() != 11)
        {
            return 1;
        }

        // The closed instantiation dispatches to the override, which is what makes the answer
        // above a property of the open definition rather than of the receiver.
        if (((IContra<object>)new C()).M() != 22)
        {
            return 2;
        }

        return 0;
    }
}
