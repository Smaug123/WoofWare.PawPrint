using System;
using System.Collections.Generic;
using System.Reflection;

// Invoking a static method reflectively compiles it, loading what its locals
// name, before its type's static constructor runs: measured on .NET 10, the
// static constructor sees System.Collections, which only the method's local names.
//
// A class whose method has no such local goes first, as a control: its static
// constructor must not see System.Collections loaded either way.
//
// Exit codes: 0 as on real .NET; 1 when the control's static constructor saw
// System.Collections; 2 when the static constructor under test did not see it.
static class Probe
{
    public static bool CollectionsLoaded()
    {
        foreach (var assembly in AppDomain.CurrentDomain.GetAssemblies())
        {
            if (assembly.GetName().Name == "System.Collections")
                return true;
        }

        return false;
    }
}

class Control
{
    public static readonly bool SawCollections;
    static Control() { SawCollections = Probe.CollectionsLoaded(); }
    public static void M() { object local = null; GC.KeepAlive(local); }
}

class Subject
{
    public static readonly bool SawCollections;
    static Subject() { SawCollections = Probe.CollectionsLoaded(); }
    public static void M() { Stack<int> local = null; GC.KeepAlive(local); }
}

class Program
{
    static int Main()
    {
        typeof(Control).GetMethod("M").Invoke(null, null);
        if (Control.SawCollections)
            return 1;

        typeof(Subject).GetMethod("M").Invoke(null, null);
        return Subject.SawCollections ? 0 : 2;
    }
}
