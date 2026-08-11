namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// PawPrint has ~2,400 `failwith` sites and almost none of them can say where the guest was:
/// the most context-free of all live in pure helpers (`convOvfI4Un`, `divUnValues`) that have no
/// `IlMachineState` to consult. Rather than thread state into all of them, `executeOneStep`
/// annotates whatever escapes it — so every one of those sites gains a guest location at once.
///
/// These tests pin the three properties that make the annotation safe to apply that broadly: it
/// must add information without destroying any, it must not fire twice, and it must never
/// replace the failure it is describing.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGuestFailure =

    let private assy = typeof<RunResult>.Assembly

    /// A guest that reaches a native entry point PawPrint does not implement. `failUnimplemented`
    /// is the single most common way PawPrint fails in practice, and its message names the
    /// missing native but nothing about the guest that wanted it.
    let private callsMissingNative =
        """
using System.Runtime.InteropServices;

class CallsMissingNative
{
    [DllImport("libSystem.Native", EntryPoint = "SystemNative_PawPrintDoesNotImplementThis")]
    private static extern int Missing();

    static int Main()
    {
        return Missing(); // CALL SITE
    }
}
"""

    let private lineContaining (marker : string) (source : string) : int =
        let lines = source.Replace("\r\n", "\n").Split '\n'

        match
            lines
            |> Array.mapi (fun i l -> i, l)
            |> Array.filter (fun (_, l) -> l.Contains marker)
        with
        | [| (i, _) |] -> i + 1
        | [||] -> failwith $"guest source contains no line matching %s{marker}; the test's oracle is broken"
        | many ->
            failwith $"guest source has %d{many.Length} lines matching %s{marker}, so the expected line is ambiguous"

    let private runToFailure (name : string) (source : string) : exn =
        let image = Roslyn.compileWithSymbols [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        // `Assert.Catch`, not `Assert.Throws`: the latter is an *exact* type constraint, so it
        // rejects the `GuestFailureException` subclass these tests exist to inspect. Each test
        // below asserts the type it wants for itself.
        Assert.Catch (fun () ->
            BoundedRun.runWith
                loggerFactory
                BoundedRun.defaultMaxSteps
                name
                (Some name)
                peImage
                (HostConfig.Default dotnetRuntimes)
            |> ignore<RunOutcome>
        )

    [<Test>]
    let ``a failure inside a step names the guest code that provoked it`` () : unit =
        let exn = runToFailure "CallsMissingNative.cs" callsMissingNative
        let callSite = lineContaining "// CALL SITE" callsMissingNative

        // The point of the whole exercise: "which native is missing" plus "who asked for it".
        exn.Message |> shouldContainText "SystemNative_PawPrintDoesNotImplementThis"
        exn.Message |> shouldContainText "PawPrintTestAssembly.CallsMissingNative.Main"
        exn.Message |> shouldContainText $"File0.cs:%d{callSite}"

    /// Annotation must be purely additive. A wrapper that swallowed the original text would
    /// trade a message that says what went wrong for one that says only where.
    [<Test>]
    let ``the original failure message survives verbatim`` () : unit =
        let exn = runToFailure "CallsMissingNative.cs" callsMissingNative

        match exn with
        | :? GuestFailureException as failure ->
            failure.InnerException |> shouldNotEqual null
            exn.Message |> shouldContainText failure.InnerException.Message
        | other -> failwith $"expected a GuestFailureException, got %s{other.GetType().FullName}"

    /// The inner exception is the only thing carrying the *host* stack trace — where in PawPrint's
    /// own source the `failwith` was. Losing it would trade a PawPrint-side diagnostic for a
    /// guest-side one, when the whole point is to have both.
    [<Test>]
    let ``the host stack trace is preserved on the inner exception`` () : unit =
        let exn = runToFailure "CallsMissingNative.cs" callsMissingNative

        match exn with
        | :? GuestFailureException as failure ->
            failure.InnerException.StackTrace |> shouldNotEqual null
            failure.InnerException.StackTrace |> shouldContainText "WoofWare.PawPrint"
        | other -> failwith $"expected a GuestFailureException, got %s{other.GetType().FullName}"

    /// `executeOneStep` must not annotate an already-annotated failure. Without the guard, a
    /// re-entrant step would nest wrappers and repeat the thread summary once per level.
    [<Test>]
    let ``the annotation is applied exactly once`` () : unit =
        let exn = runToFailure "CallsMissingNative.cs" callsMissingNative

        match exn with
        | :? GuestFailureException as failure ->
            failure.InnerException :? GuestFailureException |> shouldEqual false

            let occurrences =
                exn.Message.Split ([| "Guest was:" |], StringSplitOptions.None) |> Array.length

            occurrences |> shouldEqual 2
        | other -> failwith $"expected a GuestFailureException, got %s{other.GetType().FullName}"

    /// The structured location is the reason this is an exception type rather than a string
    /// concatenation: the App, the debugger and the harness each want to render it differently.
    [<Test>]
    let ``the location is carried as data, not only as text`` () : unit =
        let exn = runToFailure "CallsMissingNative.cs" callsMissingNative

        match exn with
        | :? GuestFailureException as failure ->
            failure.Guest |> shouldNotEqual []

            // The failing thread is present and located, not merely counted.
            failure.Guest
            |> List.exists (fun t ->
                match t.Position with
                | GuestThreadPosition.AtSource _
                | GuestThreadPosition.CalledFrom _ -> true
                | GuestThreadPosition.NoFrame
                | GuestThreadPosition.Unattributed _ -> false
            )
            |> shouldEqual true
        | other -> failwith $"expected a GuestFailureException, got %s{other.GetType().FullName}"
