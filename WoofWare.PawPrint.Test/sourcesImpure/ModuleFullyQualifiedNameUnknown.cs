using System;
using System.Reflection;

// PawPrint reports every image as having no file backing (see AssemblyLocationEmpty.cs and
// docs/divergences.md), and `RuntimeModule_GetFullyQualifiedName` answers a module with no path
// `<Unknown>`. That is what CoreCLR reports for an `Assembly.Load(byte[])` image, measured on
// .NET 10 to give `<Unknown>` for both `FullyQualifiedName` and `Name`.
//
// PawPrint-only for the same reason as AssemblyLocationEmpty.cs: the real runtime, launched from a
// real .dll, reports that file's path here.
//
// Returns 0 on success, or the number of the first check that failed.

public class ModuleFullyQualifiedNameUnknown
{
    public static int Main(string[] argv)
    {
        Module own = typeof(ModuleFullyQualifiedNameUnknown).Module;

        if (own.FullyQualifiedName != "<Unknown>") return 1;

        // `Name` is the part of `FullyQualifiedName` after its last directory separator, and
        // `<Unknown>` has none.
        if (own.Name != "<Unknown>") return 2;

        // The path is the image's, whichever route reached the module.
        if (typeof(ModuleFullyQualifiedNameUnknown).Assembly.ManifestModule.FullyQualifiedName != "<Unknown>") return 3;

        // A framework module has no path either: reporting the host's would leak the machine that
        // produced the run into the replay.
        Module corelib = typeof(object).Module;
        if (corelib.FullyQualifiedName != "<Unknown>") return 4;
        if (corelib.Name != "<Unknown>") return 5;

        // CoreCLR's QCall allocates its result afresh on each call.
        if (object.ReferenceEquals(own.FullyQualifiedName, own.FullyQualifiedName)) return 6;

        return 0;
    }
}
