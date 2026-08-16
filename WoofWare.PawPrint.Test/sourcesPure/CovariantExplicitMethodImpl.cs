// Regression for IlMachineStateExecution.findMatchingMethodImplBodies: when a class
// provides an explicit MethodImpl for ICovariant<string>.Get and the call site
// dispatches ICovariant<object>.Get (allowed by `out`-variance), the MethodImpl must
// still be selected. Exact generic-argument equality on the declaration's declaring
// type rejects this case.

using System;

interface ICovariant<out T> { T Get(); }

class CovariantImpl : ICovariant<string>
{
    string ICovariant<string>.Get() => "covariant";
}

class Program
{
    static int Main(string[] args)
    {
        // Dispatch directly through ICovariant<object> without ever materialising an
        // ICovariant<string> reference: the call site only registers the call target
        // (ICovariant<object>), so the MethodImpl's body declaration (ICovariant<string>)
        // is *not* yet in the ConcreteTypes registry when dispatch runs. A cache-only
        // lookup of the declaration would silently skip the MethodImpl.
        ICovariant<object> objCov = new CovariantImpl();
        object result = objCov.Get();
        return (result is string s && s == "covariant") ? 0 : 1;
    }
}
