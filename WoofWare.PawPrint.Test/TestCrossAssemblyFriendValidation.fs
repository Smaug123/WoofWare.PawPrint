namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestCrossAssemblyFriendValidation =

    /// `sourcesPure/InternalsVisibleToPublicKeyToken.cs` shows that an assembly whose
    /// `InternalsVisibleTo` name carries a `PublicKeyToken=` segment loads and runs. This is the
    /// other half of CoreCLR's timing: the name is validated the first time an access check
    /// consults the assembly's friend list (`Assembly::GetFriendAssemblyInfo`), and *that* throws.
    ///
    /// Reaching the consultation takes two assemblies. The library declares the invalid friend and
    /// a public attribute type with an `internal` constructor; the entry assembly applies the
    /// attribute (Roslyn honours the friend declaration despite the token) and then asks for it
    /// back. `GetCustomAttributes` runs the constructor through `IsCAVisibleFromDecoratedType`,
    /// whose `Assembly`-visibility arm asks the library who its friends are.
    ///
    /// Measured on the real runtime: the guest runs up to that call, and it raises a
    /// `COMException` whose `HResult` is `META_E_CA_BAD_FRIENDS_ARGS` (0x801311E5). PawPrint has
    /// no way yet to raise that from a QCall, so its half of the contract is a host failure at the
    /// same point, naming the assembly and the QCall. When that plumbing exists this test should
    /// collapse into a `CrossAssemblyHarness.runTest` with `ExpectedReturnCode = 3`.
    [<Test>]
    let ``an invalid InternalsVisibleTo surfaces when an access check first consults it`` () : unit =
        let compiled =
            CrossAssemblyHarness.compileAssemblies
                [
                    CrossAssemblySpec.library
                        "FriendValidation.Lib"
                        []
                        [
                            """
using System;
using System.Runtime.CompilerServices;

[assembly: InternalsVisibleTo("FriendValidation.Entry, PublicKeyToken=b77a5c561934e089")]

[AttributeUsage(AttributeTargets.Class)]
public sealed class InternalCtorAttribute : Attribute
{
    internal InternalCtorAttribute()
    {
    }
}
"""
                        ]
                    CrossAssemblySpec.entryPoint
                        "FriendValidation.Entry"
                        [ "FriendValidation.Lib" ]
                        [
                            """
using System;
using System.Runtime.InteropServices;

[InternalCtor]
public static class Program
{
    public static int Main()
    {
        try
        {
            object[] attrs = typeof(Program).GetCustomAttributes(typeof(InternalCtorAttribute), false);
            return attrs.Length == 1 ? 0 : 1;
        }
        catch (COMException e) when (e.HResult == unchecked((int)0x801311E5))
        {
            return 3;
        }
        catch (Exception)
        {
            return 2;
        }
    }
}
"""
                        ]
                ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore

        try
            CrossAssemblyHarness.writeAssemblies tempDir compiled
            let entryPath = Path.Combine (tempDir, "FriendValidation.Entry.dll")

            match RealRuntime.executeAssemblyInPlace [||] entryPath with
            | RealRuntimeResult.NormalExit exitCode -> exitCode |> shouldEqual 3
            | RealRuntimeResult.UnhandledException report ->
                failwith $"Real runtime terminated with an unhandled exception:\n%s{report}"
            | RealRuntimeResult.Aborted (code, report) -> failwith $"Real runtime aborted (%O{code}):\n%s{report}"

            let hostFailure =
                Assert.Throws<GuestFailureException> (fun () ->
                    CrossAssemblyHarness.executeWithPawPrint entryPath compiled.["FriendValidation.Entry"]
                    |> ignore<int>
                )

            // The failure comes from the access check inside the QCall, not from loading the
            // library, and it says which assembly's declarations are at fault.
            hostFailure.Message
            |> shouldContainText "RuntimeMethodHandle.IsCAVisibleFromDecoratedType"

            hostFailure.Message
            |> shouldContainText "friend-assembly declarations on FriendValidation.Lib"

            hostFailure.Message |> shouldContainText "PublicKeyToken"
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()
