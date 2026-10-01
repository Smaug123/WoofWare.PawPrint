using System;
using System.Collections.Generic;
using System.Reflection;
using System.Runtime.CompilerServices;

// CoreCLR calls an `AppDomain.AssemblyLoad` handler from `AppDomain::RaiseLoadingAssemblyEvent`
// inside `EX_TRY ... EX_CATCH {}`, so an exception the handler throws is discarded there and never
// reaches the code whose load raised the event.
public class Program
{
    private static int s_calls;

    private static void OnLoad (object sender, AssemblyLoadEventArgs args)
    {
        s_calls++;
        throw new InvalidOperationException ("thrown by an AssemblyLoad handler");
    }

    private static bool IsLoaded (string simpleName)
    {
        Assembly[] loaded = AppDomain.CurrentDomain.GetAssemblies ();

        for (int i = 0; i < loaded.Length; i++)
        {
            if (loaded[i].GetName ().Name == simpleName)
                return true;
        }

        return false;
    }

    // Loads System.Collections; kept out of line so that compiling `Main` cannot bind it early.
    [MethodImpl (MethodImplOptions.NoInlining)]
    private static int TouchStack ()
    {
        var stack = new Stack<int> ();
        stack.Push (1);
        return stack.Count;
    }

    public static int Main (string[] args)
    {
        if (IsLoaded ("System.Collections"))
            return 1;

        AppDomain.CurrentDomain.AssemblyLoad += OnLoad;

        int count;

        try
        {
            count = TouchStack ();
        }
        catch (InvalidOperationException)
        {
            return 2;
        }

        if (count != 1)
            return 3;

        // The handler did run; it is only its exception that went nowhere.
        if (s_calls == 0)
            return 4;

        return 0;
    }
}
