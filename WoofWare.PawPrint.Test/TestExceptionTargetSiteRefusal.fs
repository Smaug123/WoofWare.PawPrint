namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `Exception.TargetSite` (and `Source`, which reads it) of an exception the runtime raised rather
/// than an IL `throw`. CoreCLR raises most of these from a `[StackTraceHidden]` managed helper that
/// its JIT calls (`ThrowHelpers.ThrowIndexOutOfRangeException`, `CastHelpers`,
/// `InitHelpers.InitClassSlow`, `RuntimeType.CreateInstanceOfT`), which is the first frame of the
/// trace and so what `TargetSite` names; which helper is the JIT's choice. PawPrint runs no frame
/// for it, so it refuses the question rather than name the method beneath.
/// `sourcesPure/ExceptionTargetSite.cs` covers the exceptions an IL `throw` raises, which PawPrint
/// answers.
///
/// Each guest exits 0 on real .NET only if `Source` names CoreLib, which is what makes the refusal
/// warranted: a guest whose real answer were its own method would be one PawPrint should answer.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestExceptionTargetSiteRefusal =

    let private guest (raise : string) : string =
        $$"""
using System;
using System.Runtime.CompilerServices;
using System.Runtime.ExceptionServices;

class BadInit
{
    public static int X = Fail();
    static int Fail() { throw new InvalidOperationException("cctor"); }
}

class BadFieldInit
{
    public static int X = Fail();
    static int Fail() { throw new InvalidOperationException("cctor"); }
}

class ThrowingCtor
{
    public ThrowingCtor() { throw new InvalidOperationException("ctor"); }
}

public static class Program
{
    [MethodImpl(MethodImplOptions.NoInlining)] static int Index(int[] a, int i) => a[i];
    [MethodImpl(MethodImplOptions.NoInlining)] static string Cast(object o) => (string)o;
    [MethodImpl(MethodImplOptions.NoInlining)] static int Touch() => BadInit.X;

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void TouchTwice()
    {
        try { Touch(); } catch (TypeInitializationException) { }
        Touch();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void ReadFieldTwice()
    {
        try { typeof(BadFieldInit).GetField("X").GetValue(null); } catch (Exception) { }
        typeof(BadFieldInit).GetField("X").GetValue(null);
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Rethrow()
    {
        try { Index(new int[1], 2); }
        catch { throw; }
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void Replay()
    {
        ExceptionDispatchInfo edi = null;
        try { Index(new int[1], 2); }
        catch (Exception e) { edi = ExceptionDispatchInfo.Capture(e); }
        edi.Throw();
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static void ThrowAgain(Exception e) { throw e; }

    // The trace a handler's snapshot holds is not the trace the exception holds by the time it is
    // rethrown: `edi.Throw()` here splices the captured runtime raise back onto the same object.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static void SpliceThenRethrow()
    {
        Exception raised = null;
        ExceptionDispatchInfo edi = null;
        try { Index(new int[1], 2); }
        catch (Exception e) { raised = e; edi = ExceptionDispatchInfo.Capture(e); }
        try { ThrowAgain(raised); }
        catch
        {
            try { edi.Throw(); } catch { }
            throw;
        }
    }

    // A filter runs guest code in the middle of a raise, and here it raises the same object again
    // from a capture of its runtime raise, so the raise it interrupted continues that trace.
    [MethodImpl(MethodImplOptions.NoInlining)]
    static bool SpliceInFilter(ExceptionDispatchInfo edi, bool accept)
    {
        try { edi.Throw(); } catch { }
        return accept;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static Exception SpliceWhileFiltering(bool accept)
    {
        Exception raised = null;
        ExceptionDispatchInfo edi = null;
        try { Index(new int[1], 2); }
        catch (Exception e) { raised = e; edi = ExceptionDispatchInfo.Capture(e); }
        try { ThrowAgain(raised); }
        catch (Exception e) when (SpliceInFilter(edi, accept)) { return e; }
        return null;
    }

    [MethodImpl(MethodImplOptions.NoInlining)]
    static Exception Raise()
    {
        try { {{raise}}; }
        catch (Exception e) { return e; }
        return null;
    }

    public static int Main(string[] args)
    {
        Exception e = Raise();
        if (e == null) return 1;
        return e.Source == "System.Private.CoreLib" ? 0 : 2;
    }
}
"""

    /// Each case: its name, the statement that raises, and the `RuntimeRaiseSite` the refusal names.
    let private cases : (string * string * string) list =
        [
            "an index out of range", "Index(new int[1], 2)", "MadeByRuntime"
            "a failing cast", "Cast(1)", "MadeByRuntime"
            "a type initialiser's failure", "Touch()", "TypeInitializerFailed"
            "a type initialiser's failure, raised again", "TouchTwice()", "TypeInitializerFailedEarlier"
            "Activator.CreateInstance<T> over a throwing constructor",
            "Activator.CreateInstance<ThrowingCtor>()",
            "TargetInvocationWrap"
            "a reflective read of a field whose initialiser failed, again", "ReadFieldTwice()", "TargetInvocationWrap"
            "a rethrow of a runtime raise", "Rethrow()", "MadeByRuntime"
            "a captured runtime raise thrown again", "Replay()", "MadeByRuntime"
            "a rethrow after a splice of a captured runtime raise", "SpliceThenRethrow()", "MadeByRuntime"
            "a raise continued after a rejecting filter spliced in a captured runtime raise",
            "SpliceWhileFiltering(false)",
            "MadeByRuntime"
            "a raise caught by a filter that spliced in a captured runtime raise",
            "{ Exception caught = SpliceWhileFiltering(true); if (caught != null) return caught; }",
            "MadeByRuntime"
        ]

    let private caseNames : string list = cases |> List.map (fun (name, _, _) -> name)

    [<TestCaseSource(nameof caseNames)>]
    let ``TargetSite of an exception the runtime raised is refused`` (name : string) : unit =
        let _, raise, site = cases |> List.find (fun (n, _, _) -> n = name)
        let image = Roslyn.compile [ guest raise ]

        match RealRuntime.executeWithRealRuntime [||] image with
        | RealRuntimeResult.NormalExit 0 -> ()
        | other -> failwith $"real .NET did not name CoreLib as the Source of %s{name}: %O{other}"

        let _messages, loggerFactory = LoggerFactory.makeTestWithProperties [ "case", name ]

        use _loggerFactoryResource = loggerFactory
        use peImage = new MemoryStream (image)

        let exn =
            Assert.Catch (fun () ->
                BoundedRun.run loggerFactory name None peImage (HostConfig.Default (FrameworkUnderTest.runtimeDirs ()))
                |> ExpectRun.ended
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "ExceptionNative_GetMethodFromStackTrace"
        exn.Message |> shouldContainText "the runtime raised"
        exn.Message |> shouldContainText $"RuntimeRaiseSite.%s{site}"
