using System;
using System.Collections.Generic;
using System.Reflection;

// ConstructorInfo.Invoke runs the type's static constructor before it compiles
// the instance constructor: measured on .NET 10, the static constructor does not
// see System.Collections, which only the instance constructor's local names.
// (Activator.CreateInstance, by contrast, compiles first.)
//
// A class whose method has no such local goes first, as a control: its static
// constructor must not see System.Collections loaded either way.
//
// Exit codes: 0 as on real .NET; 1 when the control's static constructor saw
// System.Collections; 2 when the static constructor under test saw it.
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
    public Control() { object local = null; GC.KeepAlive(local); }
}

class Subject
{
    public static readonly bool SawCollections;
    static Subject() { SawCollections = Probe.CollectionsLoaded(); }
    public Subject() { Stack<int> local = null; GC.KeepAlive(local); }
}

class Program
{
    static int Main()
    {
        typeof(Control).GetConstructor(Type.EmptyTypes).Invoke(null);
        if (Control.SawCollections)
            return 1;

        typeof(Subject).GetConstructor(Type.EmptyTypes).Invoke(null);
        return !Subject.SawCollections ? 0 : 2;
    }
}
