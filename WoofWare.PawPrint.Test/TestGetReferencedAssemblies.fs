namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `Assembly.GetReferencedAssemblies`, which CoreCLR answers from the image's AssemblyRef rows
/// without loading any of them: `AssemblyNative_GetReferencedAssemblies` builds each
/// `AssemblyName` by running CoreLib's own `AssemblyName(NativeAssemblyNameParts*)` constructor.
///
/// The client references two libraries that are never loaded (nothing runs the methods that name
/// them), one with no public key and one with a culture, as well as the framework. It checks what
/// it is handed itself; the order it expects is the AssemblyRef table's, read here from the image.
/// The client runs on real .NET too, so its expectations are measured before PawPrint is held to
/// them.
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

    let private clientSource =
        """
using System;
using System.Reflection;

public static class Program
{
    // Never called. They make the compiler emit an AssemblyRef row for each library, and since
    // nothing runs them, neither runtime ever loads one.
    static object NameUnsigned() => new Unsigned.Marker();
    static object NameCultured() => new Cultured.Marker();

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

        // The AssemblyRef table's order.
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

        AssemblyName cultured = Find(refs, "Cultured");
        if (cultured.CultureName != "fr") return 8;
        if (cultured.FullName != "Cultured, Version=5.6.7.8, Culture=fr, PublicKeyToken=null") return 9;

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

        // Each call builds new names.
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

    let private library (name : string) (source : string) : byte[] =
        Roslyn.compileAssembly name OutputKind.DynamicallyLinkedLibrary [] [ source ]

    let private client : Lazy<byte[]> =
        lazy
            (Roslyn.compileAssembly
                "Client"
                OutputKind.ConsoleApplication
                [
                    MetadataReference.CreateFromImage (ImmutableArray.CreateRange (library "Unsigned" unsignedSource))
                    MetadataReference.CreateFromImage (ImmutableArray.CreateRange (library "Cultured" culturedSource))
                ]
                [ clientSource ])

    /// The simple names of the image's AssemblyRef rows, in table order.
    let private assemblyRefNames (image : byte[]) : string =
        use peReader = new PEReader (new MemoryStream (image))
        let metadata : MetadataReader = peReader.GetMetadataReader ()

        metadata.AssemblyReferences
        |> Seq.map (fun handle -> metadata.GetString (metadata.GetAssemblyReference(handle).Name))
        |> String.concat ","

    [<Test>]
    let ``the referenced assemblies are the AssemblyRef rows, as CoreCLR names them`` () : unit =
        let image = client.Force ()
        let expectedNames = assemblyRefNames image

        // The libraries are referenced, or the client checks nothing about them.
        expectedNames.Split ',' |> shouldContain "Unsigned"
        expectedNames.Split ',' |> shouldContain "Cultured"

        match RealRuntime.executeWithRealRuntime [| expectedNames |] image with
        | RealRuntimeResult.NormalExit code -> code |> shouldEqual 0
        | other -> failwith $"real .NET did not exit normally: %O{other}"

        let name = "GetReferencedAssemblies.cs"

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
                        Argv = [ expectedNames ]
                    }
            }

        use peImage = new MemoryStream (image)

        match
            BoundedRun.runWith loggerFactory BoundedRun.defaultMaxSteps name (Some name) peImage hostConfig
            |> ExpectRun.ended
        with
        | RunOutcome.NormalExit (state, _, _) -> state.LatchedExitCode |> shouldEqual 0
        | other -> failwith $"expected a normal exit, got %O{other}"
