using System;
using System.Reflection;

// PawPrint reports every assembly as having no file backing: `Location` is the
// empty string, which is what CoreCLR reports for a byte-array load or a
// single-file-published app. See docs/divergences.md for why that is the honest
// answer rather than a synthesised path.
//
// This is a PawPrint-only test rather than a differential one precisely because
// the real runtime, launched from a real .dll on disk, reports a real path here
// — so there is no cross-runtime fact to assert.

public class AssemblyLocationEmpty
{
    public static int Main(string[] argv)
    {
        Assembly entry = typeof(AssemblyLocationEmpty).Assembly;

        // Never null: CoreCLR's QCall always sets the out-parameter, so the
        // managed `location!` is a genuine string. A handler that left the
        // StringHandleOnStack untouched would surface here.
        string loc = entry.Location;
        if (loc == null) return 1;
        if (loc.Length != 0) return 2;

        // A framework assembly resolves from the host's runtime directories.
        // Reporting the host's real path here would leak the machine that
        // produced the run into the replay contract, so it is empty too.
        string corelib = typeof(object).Assembly.Location;
        if (corelib == null) return 3;
        if (corelib.Length != 0) return 4;

        // CoreCLR's StringObject::NewString hands back the shared empty-string
        // instance for a zero-length string, so these identities hold there
        // too; allocating a fresh empty string per call would break them.
        if (!object.ReferenceEquals(loc, string.Empty)) return 5;
        if (!object.ReferenceEquals(loc, corelib)) return 6;

        // The knock-on effect. `AppContext.BaseDirectory` falls back to
        // `Path.GetDirectoryName(Assembly.GetEntryAssembly()?.Location)` when no
        // host has supplied the APP_CONTEXT_BASE_DIRECTORY property, and
        // `GetDirectoryName("")` is null, so the fallback yields string.Empty
        // rather than throwing. Asserted here so that an empty Location cannot
        // quietly turn into a crash further down this path.
        string baseDir = AppContext.BaseDirectory;
        if (baseDir == null) return 7;
        if (baseDir.Length != 0) return 8;

        return 0;
    }
}
