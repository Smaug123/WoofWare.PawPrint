using System;
using System.Reflection;

// A delegate closed over a *null* receiver whose target is abstract. Real .NET builds it — the
// binding is a legal closed-instance one, and CoreCLR's `BindToMethod` does not virtualise when
// there is no receiver to virtualise on (`*pRefFirstArg != NULL`, comdelegate.cpp:1284) — and fails
// only at invocation, with a catchable `BadImageFormatException`.
//
// This is the only route to an abstract target: a non-null receiver's runtime type is necessarily a
// subclass of the abstract declaring type, so binding virtualises to a concrete override, and the
// open shape is refused (`DelegateBindOpenVirtual.cs`).
//
// The exception is faithful; its *trace* is not. Real .NET names the abstract target as the top
// frame, because the failure happens while it is preparing to enter it; PawPrint pops the
// delegate's synthetic frame first, so the trace starts at the caller. Nothing here asserts the
// trace, because that would pin a known divergence as though it were the intended answer — see
// docs/divergences.md, "A delegate invocation that fails before entering its target names no frame
// for it".
//
// Returns 0 on success, or the number of the first check that failed.

public abstract class AbstractBase
{
    public abstract string Abstract ();

    public virtual string Virtual ()
    {
        return "virtual";
    }
}

public class AbstractDerived : AbstractBase
{
    public override string Abstract ()
    {
        return "derived";
    }
}

public interface IAbstractMember
{
    string Member ();
}

public class AbstractMemberImpl : IAbstractMember
{
    public string Member ()
    {
        return "impl";
    }
}

public static class Program
{
    public static int Main ()
    {
        MethodInfo abstractMethod = typeof (AbstractBase).GetMethod ("Abstract");

        // 1: binding succeeds, and produces a closed delegate over a null receiver.
        Func<string> overNull = (Func<string>) abstractMethod.CreateDelegate (typeof (Func<string>), null);

        if (overNull.Target != null)
        {
            return 1;
        }

        // 2: invocation is what fails, and catchably.
        try
        {
            overNull ();
            return 2;
        }
        catch (BadImageFormatException e)
        {
            // 3: the HResult is COR_E_BADIMAGEFORMAT, which is what distinguishes the runtime's
            // throw from one a guest could have constructed.
            if (e.HResult != unchecked ((int) 0x8007000B))
            {
                return 3;
            }

            // 4: and the message is the CLR's HRESULT text rather than the parameterless
            // constructor's, which is a different string with no HRESULT in it. Only the numeral is
            // checked: the prose around it is localisable, so a machine with a non-English UI
            // culture would report different words for the same failure.
            if (!e.Message.Contains ("0x8007000B"))
            {
                return 4;
            }
        }

        // 5: an interface member behaves identically — it is abstract for the same reason.
        Func<string> interfaceOverNull =
            (Func<string>) typeof (IAbstractMember).GetMethod ("Member").CreateDelegate (typeof (Func<string>), null);

        try
        {
            interfaceOverNull ();
            return 5;
        }
        catch (BadImageFormatException)
        {
        }

        // 6: control. A *non*-abstract virtual method closed over null has a body, and running it
        // with a null receiver is fine so long as the body does not touch it.
        Func<string> virtualOverNull = (Func<string>)
            typeof (AbstractBase).GetMethod ("Virtual").CreateDelegate (typeof (Func<string>), null);

        if (virtualOverNull () != "virtual")
        {
            return 6;
        }

        // 7: control. The same abstract method closed over a real receiver virtualises at binding,
        // so it runs the override rather than reaching the abstract declaration.
        Func<string> overReceiver =
            (Func<string>) abstractMethod.CreateDelegate (typeof (Func<string>), new AbstractDerived ());

        if (overReceiver () != "derived")
        {
            return 7;
        }

        return 0;
    }
}
