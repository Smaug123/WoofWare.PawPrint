using System;
using System.Reflection;

// `Assembly.GetModules()`, `GetModules(bool)` and `GetLoadedModules(bool)` all bottom out in the
// single `AssemblyNative_GetModules` QCall, which differ only in the two `bool`s they pass.
//
// On CoreCLR that QCall can only ever produce the manifest module: it appends
// `pAssembly->GetModule()` and then walks the `File` table, and `ModuleBase::LoadModule` has no
// success path for a `File` row — it throws `COR_E_MULTIMODULEASSEMBLIESDIALLOWED`. A guest
// compiled from a single source file has no `File` rows at all, so what this pins is the shape of
// the ordinary answer; the `File`-row refusal has no real-runtime oracle a guest can reach and is
// pinned in `TestAssemblyNativeQCalls.fs` instead.
public class Program
{
    public static int Main (string[] args)
    {
        Assembly assembly = Assembly.GetExecutingAssembly ();

        Module[] modules = assembly.GetModules ();

        if (modules.Length != 1)
            return 1;

        // `GetExposedObject()` is cached on the module, so this is the very object
        // `Assembly.ManifestModule` reports rather than an equal one.
        if (!ReferenceEquals (modules[0], assembly.ManifestModule))
            return 2;

        // The array, unlike its contents, is allocated afresh on every call.
        Module[] again = assembly.GetModules ();

        if (ReferenceEquals (again, modules))
            return 3;

        if (!ReferenceEquals (again[0], modules[0]))
            return 4;

        // `GetLoadedModules` is the same QCall with `loadIfNotFound` false, which changes nothing
        // for an assembly with no `File` rows.
        Module[] loaded = assembly.GetLoadedModules ();

        if (loaded.Length != 1)
            return 5;

        if (!ReferenceEquals (loaded[0], modules[0]))
            return 6;

        // `getResourceModules` is read nowhere in the QCall's body, so passing it makes no
        // difference either.
        if (assembly.GetModules (true).Length != 1)
            return 7;

        if (assembly.GetLoadedModules (true).Length != 1)
            return 8;

        // The array's element type is `RuntimeModule`, not the `Module` its static type says:
        // CoreCLR allocates it as `CLASS__MODULE`, which `corelib.h` defines as `RuntimeModule`.
        // Observable from the guest, so it is a genuine cross-runtime fact rather than an
        // implementation detail.
        Type elementType = modules.GetType ().GetElementType ();

        if (elementType == null)
            return 9;

        if (elementType.FullName != "System.Reflection.RuntimeModule")
            return 10;

        return 0;
    }
}
