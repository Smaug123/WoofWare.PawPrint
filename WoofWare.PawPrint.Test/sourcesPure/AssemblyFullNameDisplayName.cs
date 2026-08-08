using System;
using System.Reflection;

// `Assembly.FullName` is the display name built from the manifest's `Assembly` metadata
// row: the simple name, then version, culture and public key token. Both runtimes agree
// on its relationship to the simple name whatever the image is called, which is all this
// asserts — the name itself is chosen by the test harness.

public class AssemblyFullNameDisplayName
{
    public static int Main(string[] argv)
    {
        AssemblyName self = typeof(AssemblyFullNameDisplayName).Assembly.GetName();
        if (self.Name == null) return 1;
        if (self.Name.Length == 0) return 2;

        string full = typeof(AssemblyFullNameDisplayName).Assembly.FullName;
        if (full == null) return 3;

        // The display name is strictly longer than the simple name it starts with: this is
        // what stops the QCall behind it from being satisfied by returning the simple name,
        // and stops the simple name's own QCall from returning the display name.
        if (full == self.Name) return 4;
        if (!full.StartsWith(self.Name + ", ", StringComparison.Ordinal)) return 5;

        return 0;
    }
}
