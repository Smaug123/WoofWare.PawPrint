namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `Assembly.GetReferencedAssemblies`, which CoreCLR answers from the image's AssemblyRef rows
/// without loading any of them: `AssemblyNative_GetReferencedAssemblies` has CoreLib make each
/// `AssemblyName` from a `NativeAssemblyNameParts` it fills from the row.
///
/// Each client references libraries that are never loaded (nothing runs the methods that name
/// them), and checks what it is handed itself. Each runs on real .NET too, so its expectations are
/// measured before PawPrint is held to them; both runtimes run it with invariant globalization,
/// the only kind PawPrint implements, since an AssemblyRef's culture is made into a `CultureInfo`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGetReferencedAssemblies =

    let private unsignedSource =
        """
[assembly: System.Reflection.AssemblyVersion("1.2.3.4")]

namespace Unsigned
{
    public class Marker { }
}
"""

    let private culturedSource =
        """
[assembly: System.Reflection.AssemblyVersion("5.6.7.8")]
[assembly: System.Reflection.AssemblyCulture("fr")]

namespace Cultured
{
    public class Marker { }
}
"""

    /// References a library with no public key and the framework. Expects, as its argument, the
    /// simple names of its AssemblyRef rows in table order.
    let private namesClientSource =
        """
using System;
using System.Reflection;

public static class Program
{
    // Never called. It makes the compiler emit an AssemblyRef row for the library, and since
    // nothing runs it, neither runtime ever loads the library.
    static object NameUnsigned() => new Unsigned.Marker();

    static bool SameBytes(byte[] a, byte[] b)
    {
        if (a == null || b == null) return a == b;
        if (a.Length != b.Length) return false;
        for (int i = 0; i < a.Length; i++)
        {
            if (a[i] != b[i]) return false;
        }
        return true;
    }

    static AssemblyName Find(AssemblyName[] names, string simpleName)
    {
        foreach (var name in names)
        {
            if (name.Name == simpleName) return name;
        }
        return null;
    }

    public static int Main(string[] args)
    {
        AssemblyName[] refs = typeof(Program).Assembly.GetReferencedAssemblies();

        string[] simpleNames = new string[refs.Length];
        for (int i = 0; i < refs.Length; i++) simpleNames[i] = refs[i].Name;
        if (string.Join(",", simpleNames) != args[0]) return 1;

        // No public key: the VM hands the constructor a non-null pointer and a zero length, so the
        // token is an empty array, not null.
        AssemblyName unsigned = Find(refs, "Unsigned");
        if (unsigned.Version != new Version(1, 2, 3, 4)) return 2;
        if (unsigned.CultureName != "") return 3;
        byte[] unsignedToken = unsigned.GetPublicKeyToken();
        if (unsignedToken == null || unsignedToken.Length != 0) return 4;
        if (unsigned.GetPublicKey() != null) return 5;
        if (unsigned.Flags != AssemblyNameFlags.None) return 6;
        if (unsigned.FullName != "Unsigned, Version=1.2.3.4, Culture=neutral, PublicKeyToken=null") return 7;

        // A framework reference carries the token the framework's own assembly is signed with.
        AssemblyName self = typeof(object).Assembly.GetName();
        AssemblyName corelib = Find(refs, self.Name);
        if (corelib == null) return 10;
        byte[] corelibToken = corelib.GetPublicKeyToken();
        if (corelibToken == null || corelibToken.Length != 8) return 11;
        if (!SameBytes(corelibToken, self.GetPublicKeyToken())) return 12;
        if (corelib.GetPublicKey() != null) return 13;
        if (corelib.CultureName != "") return 14;
        if (corelib.Version == null || corelib.Version.Revision < 0) return 15;
        if (corelib.Flags != AssemblyNameFlags.None) return 16;

        // Each call makes new names.
        AssemblyName[] again = typeof(Program).Assembly.GetReferencedAssemblies();
        if (ReferenceEquals(again, refs)) return 17;
        if (again.Length != refs.Length) return 18;
        if (ReferenceEquals(again[0], refs[0])) return 19;
        if (again[0].FullName != refs[0].FullName) return 20;

        // CoreLib references nothing.
        if (typeof(object).Assembly.GetReferencedAssemblies().Length != 0) return 21;

        // FakeItEasy's bootstrap asks this of every loaded assembly.
        foreach (Assembly loaded in AppDomain.CurrentDomain.GetAssemblies())
        {
            foreach (AssemblyName name in loaded.GetReferencedAssemblies())
            {
                if (string.IsNullOrEmpty(name.Name)) return 22;
            }
        }

        return 0;
    }
}
"""

    /// References a library with a culture. Under invariant globalization the `CultureInfo` of
    /// that culture cannot be made, and the exception CoreLib's constructor throws leaves
    /// `GetReferencedAssemblies`; it names the culture the row gave.
    let private culturedClientSource =
        """
using System;
using System.Globalization;

public static class Program
{
    // Never called; see the other client.
    static object NameCultured() => new Cultured.Marker();

    public static int Main(string[] args)
    {
        try
        {
            typeof(Program).Assembly.GetReferencedAssemblies();
            return 1;
        }
        catch (CultureNotFoundException e)
        {
            if (e.InvalidCultureName != "fr") return 2;
            return 0;
        }
    }
}
"""

    let private library (name : string) (source : string) : MetadataReference =
        let image =
            Roslyn.compileAssembly name OutputKind.DynamicallyLinkedLibrary [] [ source ]

        MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)

    let private client (library : MetadataReference) (source : string) : byte[] =
        Roslyn.compileAssembly "Client" OutputKind.ConsoleApplication [ library ] [ source ]

    /// The simple names of the image's AssemblyRef rows, in table order.
    let private assemblyRefNames (image : byte[]) : string list =
        use peReader = new PEReader (new MemoryStream (image))
        let metadata : MetadataReader = peReader.GetMetadataReader ()

        metadata.AssemblyReferences
        |> Seq.map (fun handle -> metadata.GetString (metadata.GetAssemblyReference(handle).Name))
        |> List.ofSeq

    let private invariantGlobalization : string list =
        [ EnvironmentPal.nameValueEntry "DOTNET_SYSTEM_GLOBALIZATION_INVARIANT" "1" ]

    /// Run `image` with `args` on real .NET and then under PawPrint, requiring each to exit 0.
    let private runOnBoth (name : string) (args : string list) (image : byte[]) : unit =
        match
            RealRuntime.executeWithSeed
                FileSystemSeed.empty
                invariantGlobalization
                StandardStreamsConfig.piped
                (Array.ofList args)
                image
        with
        | RealRuntimeResult.NormalExit code -> code |> shouldEqual 0
        | other -> failwith $"real .NET did not exit normally: %O{other}"

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            FrameworkUnderTest.runtimeDirsFor KernelConfig.Default.UnixPlatform

        let hostConfig = HostConfig.Default dotnetRuntimes

        let hostConfig =
            { hostConfig with
                Guest =
                    { hostConfig.Guest with
                        Argv = args
                    }
            }

        use peImage = new MemoryStream (image)

        match
            BoundedRun.runWith loggerFactory BoundedRun.defaultMaxSteps name (Some name) peImage hostConfig
            |> ExpectRun.ended
        with
        | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 0
        | RunOutcome.GuestUnhandledException (state, _, exn, _) ->
            failwith
                $"expected a normal exit, got an unhandled exception:\n%s{UnhandledExceptionReport.describe state exn}"
        | other -> failwith $"expected a normal exit, got %O{other}"

    [<Test>]
    let ``the referenced assemblies are the AssemblyRef rows, as CoreCLR names them`` () : unit =
        let image = client (library "Unsigned" unsignedSource) namesClientSource
        let names = assemblyRefNames image

        // The library is referenced, or the client checks nothing about it.
        names |> shouldContain "Unsigned"

        runOnBoth "GetReferencedAssemblies.cs" [ String.concat "," names ] image

    [<Test>]
    let ``a referenced culture is made by CoreLib's CultureInfo`` () : unit =
        Net11Frontier.park
            "On .NET 11 the VM makes each AssemblyName through `[UnmanagedCallersOnly] AssemblyName.CreateAssemblyName`, which catches the `CultureNotFoundException` for the VM to rethrow from the QCall (`UnmanagedCallersOnlyCaller::InvokeThrowing`), and PawPrint does not model a native frame throwing an existing exception object: `AssemblyNative_GetReferencedAssemblies` refuses. Un-park when it does."

        let image = client (library "Cultured" culturedSource) culturedClientSource

        assemblyRefNames image |> shouldContain "Cultured"

        runOnBoth "GetReferencedAssembliesCultured.cs" [] image
