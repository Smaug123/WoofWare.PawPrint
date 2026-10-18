using System;
using System.Reflection;

// An open delegate over a static virtual interface method, invoked. Its binding is the one
// `COMDelegate::BindToMethod` decision that differs between the runtimes: .NET 10 points every
// open delegate over a virtual method at a virtual call stub, which a static method has no
// receiver to dispatch on, so invoking one raises `EntryPointNotFoundException`; .NET 11 sends
// only instance methods to the stub, and points one over a static method at the method itself.
// Its default body then runs, and invoking one over a static abstract method raises
// `BadImageFormatException`, as calling a method with no body does. Measured on 10.0.12 and on
// 11.0.0-rc.1.
//
// Returns 0 for .NET 11's outcomes, 10 for .NET 10's, or the number of the first check that saw
// neither.

public interface IStaticVirtual
{
    static virtual string Describe () => "default";
}

public interface IStaticAbstract
{
    static abstract string Describe ();
}

public class Implementation : IStaticAbstract, IStaticVirtual
{
    public static string Describe () => "implementation";
}

public static class Program
{
    static string Invoke (Func<string> f)
    {
        try
        {
            return f ();
        }
        catch (EntryPointNotFoundException)
        {
            return "EntryPointNotFoundException";
        }
        catch (BadImageFormatException)
        {
            return "BadImageFormatException";
        }
    }

    static Func<string> Bind (Type declaring)
    {
        return (Func<string>) declaring.GetMethod ("Describe").CreateDelegate (typeof (Func<string>));
    }

    public static int Main ()
    {
        // 1: control. The implementation is an ordinary static method on either runtime.
        if (Invoke (Bind (typeof (Implementation))) != "implementation")
        {
            return 1;
        }

        string withBody = Invoke (Bind (typeof (IStaticVirtual)));
        string withoutBody = Invoke (Bind (typeof (IStaticAbstract)));

        if (withBody == "default" && withoutBody == "BadImageFormatException")
        {
            return 0;
        }

        if (withBody == "EntryPointNotFoundException" && withoutBody == "EntryPointNotFoundException")
        {
            return 10;
        }

        // 2, 3: something neither runtime does.
        return withBody != "default" && withBody != "EntryPointNotFoundException" ? 2 : 3;
    }
}
