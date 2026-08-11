namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `GuestLocation` is what turns "the guest is stuck" into "the guest is stuck *here*". The
/// mechanism has two halves and they fail in different ways, so they are tested apart:
///
/// * the renderer is pure over `GuestThreadPosition`, so every case of that DU is pinned
///   directly, without an interpreter;
/// * the classifier needs a real machine, and its interesting behaviour is which *frame* it
///   reports — so those tests wedge a guest and assert on the line number, with the expected
///   line derived from the source text rather than written out by hand.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGuestLocation =

    let private assy = typeof<RunResult>.Assembly

    // ---------------------------------------------------------------------------------------
    // The pure renderer.
    // ---------------------------------------------------------------------------------------

    let private loc (line : int) : SourceLocation =
        {
            DocumentPath = "/build/Guest.cs"
            StartLine = line
            StartColumn = 9
            EndLine = line
            EndColumn = 24
        }

    let private frame (name : string) (offset : int) : GuestFrame =
        {
            Method = name
            IlOffset = offset
        }

    let private render (status : ThreadStatus) (position : GuestThreadPosition) : string =
        GuestLocation.renderThread
            {
                Thread = ThreadId 3
                Status = status
                Position = position
            }

    [<Test>]
    let ``a thread with no live frame renders its status alone`` () : unit =
        render ThreadStatus.NotStarted GuestThreadPosition.NoFrame
        |> shouldEqual "thread 3 (NotStarted)"

    [<Test>]
    let ``an attributed frame names the source span`` () : unit =
        render ThreadStatus.Runnable (GuestThreadPosition.AtSource (frame "Guest.Spin" 22, loc 17))
        |> shouldEqual "thread 3 (Runnable) in Guest.Spin at IL offset 22 (/build/Guest.cs:17)"

    /// The case the whole design exists for: the innermost frame is framework code, which ships
    /// without a PDB, so naming only that frame would say nothing about the guest.
    [<Test>]
    let ``an unattributed frame names the nearest enclosing frame that has a source span`` () : unit =
        render
            ThreadStatus.Runnable
            (GuestThreadPosition.CalledFrom (frame "SpinWait.SpinOnceCore" 43, 1, frame "Guest.Spin" 22, loc 17))
        |> shouldEqual
            "thread 3 (Runnable) in SpinWait.SpinOnceCore at IL offset 43, called from Guest.Spin at IL offset 22 (/build/Guest.cs:17)"

    /// "called from" alone would read as the immediate caller. A frame buried deep in framework
    /// code is a materially different situation from one the guest called straight into, and the
    /// reader can only tell the two apart if the distance is stated.
    [<Test>]
    let ``a distant ancestor is reported with the distance walked`` () : unit =
        render
            ThreadStatus.Runnable
            (GuestThreadPosition.CalledFrom (frame "Monitor.Wait" 0, 4, frame "Guest.Take" 91, loc 42))
        |> shouldEqual
            "thread 3 (Runnable) in Monitor.Wait at IL offset 0, called 4 frames out from Guest.Take at IL offset 91 (/build/Guest.cs:42)"

    /// No symbols anywhere on the stack — the ordinary case for a guest built without a PDB, and
    /// for any stack that bottoms out in the shared framework. The renderer must degrade to
    /// what it could say before, not omit the frame or fabricate a location.
    [<Test>]
    let ``a stack with no source information at all still names the active frame`` () : unit =
        render ThreadStatus.Runnable (GuestThreadPosition.Unattributed (frame "SpinWait.SpinOnceCore" 43))
        |> shouldEqual "thread 3 (Runnable) in SpinWait.SpinOnceCore at IL offset 43"

    [<Test>]
    let ``a blocked thread renders the status payload`` () : unit =
        render (ThreadStatus.BlockedOnJoin (ThreadId 1, None)) (GuestThreadPosition.Unattributed (frame "Guest.Main" 3))
        |> shouldContainText "thread 3 (BlockedOnJoin"

    // ---------------------------------------------------------------------------------------
    // The classifier, end to end.
    // ---------------------------------------------------------------------------------------

    /// The line of the single source line containing `marker`, 1-based as a PDB counts lines.
    ///
    /// Derived from the source rather than written down, so that editing the guest cannot leave
    /// a test asserting a stale number — and, more to the point, so that the assertion is a
    /// claim about the compiler's own attribution rather than a restatement of it.
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

    /// Run a guest that never terminates, and return the diagnostic the harness gave up with.
    ///
    /// `name` identifies the run in the message; it is *not* the document name a resolved source
    /// location carries. `Roslyn.compileCore` parses each source as `File{index}.cs`, so that is
    /// what the compiler records in the PDB and what the assertions below look for.
    let private runWedged (withSymbols : bool) (maxSteps : int64) (name : string) (source : string) : string =
        let image =
            if withSymbols then
                Roslyn.compileWithSymbols [ source ]
            else
                Roslyn.compile [ source ]

        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let dotnetRuntimes =
            DotnetRuntime.SelectForDll assy.Location |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        let exn =
            Assert.Throws (fun () ->
                BoundedRun.runWith loggerFactory maxSteps name (Some name) peImage (HostConfig.Default dotnetRuntimes)
                |> ignore<RunOutcome>
            )

        exn.Message

    /// A guest whose innermost frame is its own code. The reported line must be the statement the
    /// guest is wedged on, not merely some line of the enclosing method.
    [<Test>]
    let ``a guest wedged in its own code is located at the wedged statement`` () : unit =
        let source =
            """
class SpinsInGuestCode
{
    static int Main(string[] args)
    {
        int i = 0;
        while (true) { i = i + 1; } // WEDGE
    }
}
"""

        let message = runWedged true 50_000L "SpinsInGuestCode.cs" source
        let expected = lineContaining "// WEDGE" source

        message |> shouldContainText "Threads: "
        message |> shouldContainText $"File0.cs:%d{expected}"

    /// The same guest without symbols. It must still report the frame — losing the PDB costs the
    /// line and nothing else — so that a build with no debug information is merely less
    /// informative rather than broken.
    [<Test>]
    let ``a guest built without symbols still reports its frame`` () : unit =
        let source =
            """
class SpinsUnattributed
{
    static int Main(string[] args)
    {
        int i = 0;
        while (true) { i = i + 1; } // WEDGE
    }
}
"""

        let message = runWedged false 50_000L "SpinsUnattributed.cs" source

        message |> shouldContainText "Threads: "
        message |> shouldContainText "Main at IL offset"
        // Nothing may claim a source span: the image carries no debug information to derive one
        // from, and inventing one would be worse than saying nothing.
        message |> shouldNotContainText "File0.cs:"

    /// The motivating case. The guest blocks inside the BCL, so the innermost frame belongs to an
    /// assembly with no PDB; the diagnostic is only useful if it walks out to the guest's own
    /// frame. Without the outward walk this message names a framework method and stops.
    [<Test>]
    let ``a guest blocked inside the BCL is located at its own call site`` () : unit =
        let source =
            """
using System.Threading;

class BlocksInBcl
{
    static int Main(string[] args)
    {
        var gate = new ManualResetEventSlim(false);
        gate.Wait(); // WEDGE
        return 0;
    }
}
"""

        let message = runWedged true 20_000_000L "BlocksInBcl.cs" source
        let expected = lineContaining "// WEDGE" source

        message |> shouldContainText "Threads: "
        // The active frame is framework code, which has no PDB — so the guest line below can
        // only have come from walking out of it. How far out is a fact about the BCL's
        // implementation of `Wait` and deliberately not asserted; that the walk reached the
        // guest's own frame is the claim.
        message |> shouldContainText "in System.Private.CoreLib."

        message
        |> shouldContainText "from PawPrintTestAssembly.BlocksInBcl.Main at IL offset"

        message |> shouldContainText $"File0.cs:%d{expected}"

    /// A thread that exists but has never run has no frame to describe, and reaching for one
    /// throws. The diagnostic must survive that: a guest holding a constructed-but-unstarted
    /// `Thread` while another thread wedges is entirely ordinary.
    [<Test>]
    let ``an unstarted thread is described without dereferencing a frame`` () : unit =
        let source =
            """
using System.Threading;

class HoldsAnUnstartedThread
{
    static int Main(string[] args)
    {
        var idle = new Thread(() => { });
        int i = 0;
        while (true) { i = i + 1; } // WEDGE
    }
}
"""

        let message = runWedged true 200_000L "HoldsAnUnstartedThread.cs" source
        let expected = lineContaining "// WEDGE" source

        // Asserted, not merely survived: without this the test passes for a `GuestLocation` that
        // never met a frameless thread at all, and the guard it is meant to cover goes untested.
        message |> shouldContainText "thread 1 (NotStarted)"
        message |> shouldContainText $"File0.cs:%d{expected}"

    /// The rich summary must appear once, not twice. Before unification the harness printed the
    /// library's thread description under `Stuck:` and its own under `Threads:`, which said the
    /// same thing in two formats.
    [<Test>]
    let ``the thread summary is not printed twice`` () : unit =
        let source =
            """
using System.Threading;

class DeadlocksImmediately
{
    static int Main(string[] args)
    {
        new ManualResetEventSlim(false).Wait();
        return 0;
    }
}
"""

        let message = runWedged true 20_000_000L "DeadlocksImmediately.cs" source

        let occurrences =
            message.Split ([| "thread 0 (" |], StringSplitOptions.None) |> Array.length

        occurrences |> shouldEqual 2
