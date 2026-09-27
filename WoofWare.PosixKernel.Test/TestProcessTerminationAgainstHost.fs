namespace WoofWare.PosixKernel.Test

open System.Diagnostics
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `ProcessTermination.shellStatus` against this host: children of `/bin/sh` that
/// exit, or kill themselves, as `wait-status.c` swept, read back by .NET's
/// `Process.ExitCode` and by a shell's own `$?`.
[<TestFixture>]
module TestProcessTerminationAgainstHost =

    /// Run `/bin/sh -c script` and return its exit code as .NET reports it, and its
    /// standard output. Standard error is discarded: a shell reports a child that a
    /// signal killed there ("Aborted"), in words that differ between shells.
    let private runShell (script : string) : int * string =
        let info = ProcessStartInfo "/bin/sh"
        info.ArgumentList.Add "-c"
        info.ArgumentList.Add script
        info.RedirectStandardOutput <- true
        info.RedirectStandardError <- true
        info.UseShellExecute <- false

        use proc = Process.Start info
        let stdout = proc.StandardOutput.ReadToEndAsync ()
        let stderr = proc.StandardError.ReadToEndAsync ()

        if not (proc.WaitForExit 60_000) then
            proc.Kill true
            failwith $"/bin/sh -c %s{script} did not finish within a minute"

        stderr.Result |> ignore<string>
        proc.ExitCode, stdout.Result

    /// Each case's script, run once as a child of .NET and once as a child of a shell
    /// that prints `$?`, against what `termination` renders as.
    let private assertEachRendersAsTheHostSays
        (flavour : SimulatedUnixFlavour)
        (cases : (string * ProcessTermination) list)
        : unit
        =
        let numbering =
            SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

        let expected =
            cases
            |> List.map (fun (script, termination) -> script, ProcessTermination.shellStatus numbering termination)

        let viaDotnet = cases |> List.map (fun (script, _) -> script, fst (runShell script))

        viaDotnet |> shouldEqual expected

        // One shell for the lot, each child reported on its own line. A script is
        // single-quoted inside the child's command, so none may contain a quote.
        for script, _ in cases do
            script.Contains '\'' |> shouldEqual false

        let viaShell =
            let outer =
                cases
                |> List.map (fun (script, _) -> $"/bin/sh -c '%s{script}'; echo $?")
                |> String.concat "\n"

            let code, stdout = runShell outer
            code |> shouldEqual 0

            stdout.Split ('\n', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map int
            |> List.ofArray
            |> List.zip (List.map fst cases)

        viaShell |> shouldEqual expected

    [<Test>]
    let ``an exit renders as this host reports it`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            // `wait-status.c`'s sweep, except negative statuses, which Linux's dash
            // refuses as an argument to `exit` before any exit is made; every status
            // up to 300, so each low byte is seen; and the edges of Darwin's 24 bits.
            let statuses =
                [ 0..300 ]
                @ [
                    511
                    65535
                    65543
                    0xffffff
                    0x1000000
                    0x1000007
                    System.Int32.MaxValue
                ]

            statuses
            |> List.map (fun status ->
                $"exit %d{status}", ProcessTermination.Exited (ExitStatus.ofExitArgument flavour status)
            )
            |> assertEachRendersAsTheHostSays flavour
        )

    [<Test>]
    let ``a death by signal renders as this host reports it`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            [ 1 .. Signal.highestSignoUnder numbering ]
            |> List.choose (fun signo ->
                match Signal.ofRawSignoUnder numbering signo with
                | ValueNone -> None
                | ValueSome signal ->

                match flavour with
                // glibc's reserved two, which `wait-status.c` skipped too.
                | SimulatedUnixFlavour.Linux when signo = 32 || signo = 33 -> None
                // The .NET runtime this test runs in ignores SIGPIPE, its children
                // inherit the ignore, and a shell may not undo an ignore it inherited.
                | _ when signal = Signal.SIGPIPE -> None
                // A death that would dump core raises `EXC_CRASH` on Darwin, which
                // writes a crash report for `/bin/sh` into the user's logs.
                | SimulatedUnixFlavour.Darwin when Signal.dumpsCoreUnder numbering signal -> None
                | _ when Signal.defaultDispositionUnder numbering signal <> DefaultDisposition.Terminate -> None
                | _ ->
                    // No core file, wherever the host would put one.
                    Some ($"ulimit -c 0; kill -%d{signo} $$", ProcessTermination.Signaled (signal, false))
            )
            |> assertEachRendersAsTheHostSays flavour
        )
