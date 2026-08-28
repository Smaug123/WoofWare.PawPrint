using System;
using System.Reflection;

// `AppDomain.GetAssemblies()` is the only route a guest has to the `AssemblyNative_GetLoadedAssemblies`
// QCall; `AssemblyLoadContext.Assemblies`, the other managed caller, filters each result through
// `GetLoadContext`, whose QCall is not implemented.
//
// Neither the *length* of the result nor its *order* is asserted here, and that is not caution: the
// answer is a snapshot of how far loading has got, so it is not a property of the program at all.
// CoreCLR says so in its own comments ("there may be assemblies that are still loading"; it stops
// filling early "in case assemblies have been loaded into this appdomain, on another thread"), and
// it is measurable — 60 runs of one unchanged binary whose background thread touched `Regex`,
// `Linq` and `IPAddress` reported 8 assemblies 57 times, 9 twice and 7 once. PawPrint's own set
// differs again, because it interprets rather than jitting and so loads at different moments.
// `docs/divergences.md` records that.
//
// What *is* asserted is everything that holds on both: the element type, membership, the reference
// identity of the elements, and that the array is fresh per call while its contents are not.
public class Program
{
    private static bool Contains (Assembly[] haystack, Assembly needle)
    {
        for (int i = 0; i < haystack.Length; i++)
        {
            if (ReferenceEquals (haystack[i], needle))
                return true;
        }

        return false;
    }

    public static int Main (string[] args)
    {
        Assembly[] assemblies = AppDomain.CurrentDomain.GetAssemblies ();

        if (assemblies.Length == 0)
            return 1;

        // `AllocateObjectArray(..., CoreLibBinder::GetClass(CLASS__ASSEMBLY))`, and `CLASS__ASSEMBLY`
        // is `RuntimeAssembly` — not the `Assembly` that the managed wrapper's local is typed as.
        if (assemblies.GetType ().FullName != "System.Reflection.RuntimeAssembly[]")
            return 2;

        for (int i = 0; i < assemblies.Length; i++)
        {
            if (assemblies[i] == null)
                return 3;

            // No assembly may appear twice: one identity, one exposed object.
            for (int j = i + 1; j < assemblies.Length; j++)
            {
                if (ReferenceEquals (assemblies[i], assemblies[j]))
                    return 4;
            }
        }

        // Every element is the assembly's cached `GetExposedObject()`, so these are the very
        // objects the other routes to an `Assembly` hand back, not equal copies of them.
        if (!Contains (assemblies, typeof (object).Assembly))
            return 5;

        if (!Contains (assemblies, Assembly.GetExecutingAssembly ()))
            return 6;

        Assembly[] again = AppDomain.CurrentDomain.GetAssemblies ();

        // The array, unlike its contents, is allocated afresh on every call.
        if (ReferenceEquals (again, assemblies))
            return 7;

        // A superset rather than an equality, because the first call may itself have caused a load:
        // an assembly can join the set between the two calls, but none can ever leave it.
        for (int i = 0; i < assemblies.Length; i++)
        {
            if (!Contains (again, assemblies[i]))
                return 8;
        }

        return 0;
    }
}
