using System;
using System.Reflection;

// `AssemblyName.Name` is the simple name from the manifest's `Assembly` metadata row, so
// every assertion below is a fact both runtimes agree on regardless of where the image
// lives or which framework flavour supplied CoreLib.

public class AssemblyGetNameSimpleName
{
    public static int Main(string[] argv)
    {
        string corelib = typeof(object).Assembly.GetName().Name;
        if (corelib != "System.Private.CoreLib") return 1;

        // The guest's own name is chosen by the test harness, so assert its shape rather
        // than its value: non-empty, and stable across calls.
        AssemblyName self = typeof(AssemblyGetNameSimpleName).Assembly.GetName();
        if (self.Name == null) return 2;
        if (self.Name.Length == 0) return 3;

        string again = typeof(AssemblyGetNameSimpleName).Assembly.GetName().Name;
        if (self.Name != again) return 4;

        // The simple name is not the display name: a display name carries `, Version=`
        // and the rest of the identity after a comma. This is what stops the QCall from
        // being satisfied by handing back the assembly identity it was keyed by — which
        // for PawPrint is exactly a display name. `Assembly.FullName` would be the
        // sharper way to say this; it is asserted in `AssemblyFullNameDisplayName.cs`,
        // which needs a QCall this file does not.
        if (self.Name.IndexOf(',') >= 0) return 5;
        if (corelib.IndexOf(',') >= 0) return 6;

        // A dot in a simple name is part of the name, not a separator.
        if (corelib.IndexOf('.') < 0) return 7;

        return 0;
    }
}
