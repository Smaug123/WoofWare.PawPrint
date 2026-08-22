namespace WoofWare.PawPrint.Test

open Microsoft.FSharp.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Pins the `COR_E_*` values PawPrint reports for a fatal error.
///
/// These are not free-choice constants: `EEPolicy::HandleFatalError` is handed one of them, and
/// everything CoreCLR does next is derived from it — which stderr banner it prints
/// (coreclr/vm/eepolicy.cpp:374-383) and, on Windows, the process exit code, since
/// `CrashDumpAndTerminateProcess` passes it straight to `TerminateProcess`. On Unix the process
/// aborts and the shell sees 134 whichever it was, so a wrong value here is invisible on the
/// platforms this suite runs on. That is exactly why it is pinned rather than exercised.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFatalErrorCode =

    [<Test>]
    let ``the HRESULTs are the ones corerror.xml names`` () : unit =
        // corerror.xml:1280-1281 and :1632-1633 in the pinned runtime.
        FatalErrorCode.toHResult FatalErrorCode.FailFast |> shouldEqual 0x80131623

        FatalErrorCode.toHResult FatalErrorCode.ExecutionEngine
        |> shouldEqual 0x80131506

    [<Test>]
    let ``distinct codes have distinct HRESULTs`` () : unit =
        // The HRESULT *is* the identity of a fatal error, so two cases sharing one would make them
        // indistinguishable to every consumer that derives from it — including a Windows host's
        // exit code. A copy-pasted constant on a case added later is the way that happens, and it
        // is not otherwise observable on Unix.
        let codes =
            FSharpType.GetUnionCases typeof<FatalErrorCode>
            |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> FatalErrorCode)

        codes
        |> Array.map FatalErrorCode.toHResult
        |> Array.distinct
        |> Array.length
        |> shouldEqual codes.Length
