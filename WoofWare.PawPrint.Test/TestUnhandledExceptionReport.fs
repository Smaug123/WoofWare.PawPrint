namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `UnhandledExceptionReport.describe` names what a guest threw, read from the heap of the run it
/// ended, so a failing guest can be diagnosed from its report alone.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
[<Category("Guest")>]
[<Explicit>]
module TestUnhandledExceptionReport =

    let private reportOf (outcome : RunOutcome) : string list =
        match outcome with
        | RunOutcome.GuestUnhandledException (state, _, exn) ->
            UnhandledExceptionReport.describe state exn
            |> fun s -> s.Split Environment.NewLine
            |> List.ofArray
        | RunOutcome.NormalExit (state, _)
        | RunOutcome.ProcessExit (state, _) ->
            failwith $"expected an unhandled exception, but the guest exited with %d{state.LatchedExitCode}"
        | RunOutcome.Aborted (_, _, fatal) ->
            let message = fatal.Message |> Option.defaultValue "<no message>"
            failwith $"expected an unhandled exception, but the guest aborted (%O{fatal.Code}): %s{message}"
        | RunOutcome.SignalTerminated (_, signal) ->
            failwith $"expected an unhandled exception, but the guest was terminated by %O{signal}"

    let private runImage (name : string) (image : byte[]) : string list =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        BoundedRun.run loggerFactory name None peImage (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
        |> reportOf

    let private indexOfLineContaining (needle : string) (lines : string list) : int =
        match lines |> List.tryFindIndex (fun line -> line.Contains needle) with
        | Some i -> i
        | None -> failwith $"no line contains %s{needle}; report was:\n%s{String.concat Environment.NewLine lines}"

    [<Test>]
    let ``the report names each exception in the inner chain with its message and frames`` () : unit =
        let source =
            """
using System;

class Program
{
    static void Inner()
    {
        throw new ArgumentException("inner message");
    }

    static void Outer()
    {
        try
        {
            Inner();
        }
        catch (Exception e)
        {
            throw new InvalidOperationException("outer message", e);
        }
    }

    static int Main()
    {
        Outer();
        return 0;
    }
}
"""

        let lines = runImage "UnhandledNested.cs" (Roslyn.compile [ source ])

        // The inner exception's trace runs out to the frame that caught it, as CoreCLR's does,
        // so `Outer` appears on both sides of the inner section's end. Offsets are Roslyn's
        // choice and so are not asserted.
        let expected : (string -> bool) list =
            [
                (=) "System.InvalidOperationException: outer message"
                (=) " ---> System.ArgumentException: inner message"
                fun l -> l.StartsWith "   at " && l.Contains "Program.Inner at IL offset "
                fun l -> l.StartsWith "   at " && l.Contains "Program.Outer at IL offset "
                (=) "   --- End of inner exception stack trace ---"
                fun l -> l.StartsWith "   at " && l.Contains "Program.Outer at IL offset "
                fun l -> l.StartsWith "   at " && l.Contains "Program.Main at IL offset "
            ]

        let report = String.concat Environment.NewLine lines

        if lines.Length <> expected.Length then
            failwith $"expected %d{expected.Length} lines, got:\n%s{report}"

        List.zip expected lines
        |> List.iteri (fun i (matches, line) ->
            if not (matches line) then
                failwith $"line %d{i} is not as expected:\n%s{report}"
        )

    [<Test>]
    let ``the report says when an exception carries no message`` () : unit =
        let source =
            """
using System;

class BareException : Exception
{
    public BareException() : base((string)null) { }
}

class Program
{
    static int Main()
    {
        throw new BareException();
    }
}
"""

        // Compiled with a PDB, so the frame names the `throw` statement's line.
        let lines = runImage "UnhandledBare.cs" (Roslyn.compileWithSymbols [ source ])

        lines.Length |> shouldEqual 2
        lines.[0] |> shouldEqual "BareException (_message is null)"
        indexOfLineContaining "Program.Main at IL offset " lines |> shouldEqual 1
        lines.[1].EndsWith ":13)" |> shouldEqual true

    [<Test>]
    let ``the report marks where a rethrown trace was captured`` () : unit =
        let source =
            """
using System;
using System.Runtime.ExceptionServices;

class Program
{
    static void Thrower()
    {
        throw new InvalidOperationException("captured");
    }

    static int Main()
    {
        ExceptionDispatchInfo captured = null;
        try
        {
            Thrower();
        }
        catch (Exception e)
        {
            captured = ExceptionDispatchInfo.Capture(e);
        }

        captured.Throw();
        return 0;
    }
}
"""

        let lines = runImage "UnhandledRethrown.cs" (Roslyn.compile [ source ])

        let expected : (string -> bool) list =
            [
                (=) "System.InvalidOperationException: captured"
                fun l -> l.StartsWith "   at " && l.Contains "Program.Thrower at IL offset "
                fun l -> l.StartsWith "   at " && l.Contains "Program.Main at IL offset "
                (=) "--- End of stack trace from previous location ---"
            ]

        let report = String.concat Environment.NewLine lines

        // The frames after the boundary are the rethrow's, which pass through CoreLib's
        // `ExceptionDispatchInfo.Throw` on their way out to `Main`, so only their last is pinned.
        if lines.Length <= expected.Length then
            failwith $"expected more than %d{expected.Length} lines, got:\n%s{report}"

        List.zip expected (List.take expected.Length lines)
        |> List.iteri (fun i (matches, line) ->
            if not (matches line) then
                failwith $"line %d{i} is not as expected:\n%s{report}"
        )

        (List.last lines).Contains "Program.Main at IL offset " |> shouldEqual true

    /// `Thrower::Throw()`: `ldstr "not an exception"; throw`. No C# can spell a throw of a
    /// non-exception.
    let private fabricateThrower () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Thrower", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Thrower"

        let thrower =
            modul.DefineType ("Thrower", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let il =
            thrower
                .DefineMethod("Throw", MethodAttributes.Public ||| MethodAttributes.Static, typeof<Void>, [||])
                .GetILGenerator ()

        il.Emit (OpCodes.Ldstr, "not an exception")
        il.Emit OpCodes.Throw

        thrower.CreateType () |> ignore<Type>

        use stream = new MemoryStream ()
        builder.Save stream
        stream.ToArray ()

    [<Test>]
    let ``the report names a thrown object that is not an exception`` () : unit =
        let fabricated = fabricateThrower ()

        let driver =
            Roslyn.compileAssembly
                "ThrowerDriver"
                OutputKind.ConsoleApplication
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange fabricated) ]
                [
                    """
class Program
{
    static int Main()
    {
        Thrower.Throw();
        return 0;
    }
}
"""
                ]

        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>

        try
            File.WriteAllBytes (Path.Combine (tempDir, "Thrower.dll"), fabricated)
            let driverPath = Path.Combine (tempDir, "ThrowerDriver.dll")
            File.WriteAllBytes (driverPath, driver)

            let dotnetRuntimeDirs =
                seq {
                    yield tempDir
                    yield! FrameworkUnderTest.runtimeDirs ()
                }
                |> ImmutableArray.CreateRange

            let _, loggerFactory = LoggerFactory.makeTest ()
            use _loggerFactoryResource = loggerFactory
            use peImage = new MemoryStream (driver)

            let lines =
                BoundedRun.run
                    loggerFactory
                    "ThrowerDriver"
                    (Some driverPath)
                    peImage
                    (HostConfig.Default dotnetRuntimeDirs)
                |> reportOf

            lines.Length |> shouldEqual 3
            lines.[0] |> shouldEqual "System.String (not a System.Exception)"
            indexOfLineContaining "Thrower.Throw at IL offset " lines |> shouldEqual 1
            indexOfLineContaining "Program.Main at IL offset " lines |> shouldEqual 2
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()
