using System;
using System.Collections.Generic;
using System.Reflection;

// Constructing a custom attribute compiles its constructor, loading what its
// locals name, before the attribute type's static constructor runs: measured on
// .NET 10, the static constructor sees System.Collections, which only the
// constructor's local names.
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

class Control : Attribute
{
    public static readonly bool SawCollections;
    static Control() { SawCollections = Probe.CollectionsLoaded(); }
    public Control() { object local = null; GC.KeepAlive(local); }
}

class Subject : Attribute
{
    public static readonly bool SawCollections;
    static Subject() { SawCollections = Probe.CollectionsLoaded(); }
    public Subject() { Stack<int> local = null; GC.KeepAlive(local); }
}

[Control]
class MarkedByControl { }

[Subject]
class MarkedBySubject { }

class Program
{
    static int Main()
    {
        typeof(MarkedByControl).GetCustomAttributes(false);
        if (Control.SawCollections)
            return 1;

        typeof(MarkedBySubject).GetCustomAttributes(false);
        return Subject.SawCollections ? 0 : 2;
    }
}
