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
        if (!string.Equals(self.Name, again, StringComparison.Ordinal)) return 4;

        // The simple name is not the display name: `FullName` appends version, culture and
        // public key token. This is what stops the QCall from being satisfied by handing
        // back the assembly identity it was keyed by.
        string full = typeof(AssemblyGetNameSimpleName).Assembly.FullName;
        if (full == null) return 5;
        if (string.Equals(full, self.Name, StringComparison.Ordinal)) return 6;
        if (!full.StartsWith(self.Name + ", ", StringComparison.Ordinal)) return 7;

        // A dot in a simple name is part of the name, not a separator.
        if (corelib.IndexOf('.') < 0) return 8;

        return 0;
    }
}
