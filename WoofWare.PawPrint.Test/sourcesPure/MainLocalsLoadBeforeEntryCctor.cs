using System;
using System.Collections.Generic;

// The runtime compiles Main before it runs the entry type's static constructor,
// and compiling Main loads every assembly its locals name. Main's only mention
// of System.Collections is a local, so the static constructor already sees it
// loaded. (Debug-compiled, so the local is not optimised into a stack slot.)
//
// Exit codes: 0 when the static constructor saw System.Collections; 1 when not.
class Program
{
    static readonly bool s_loadedInCctor;

    static Program()
    {
        foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
        {
            if (assembly.GetName().Name == "System.Collections")
                s_loadedInCctor = true;
        }
    }

    static int Main()
    {
        Stack<int> local = null;
        GC.KeepAlive(local);
        return s_loadedInCctor ? 0 : 1;
    }
}
