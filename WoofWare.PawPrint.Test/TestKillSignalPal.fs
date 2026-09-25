namespace WoofWare.PawPrint.Test

open System
open System.Diagnostics
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `KillSignalPal` transcribes a private BCL enum and one screen in
/// `pal_process.c`. The enum is read here from the System.Diagnostics.Process
/// this test host runs, and the screen is measured against the host's own
/// `SystemNative_Kill` for every value that is safe to send to this process:
/// the null signal and the refused ones. SIGKILL and SIGSTOP are not, so for
/// those two the enum's values are the oracle.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKillSignalPal =

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_Kill", SetLastError = true)>]
    extern int private hostKill(int pid, int signal)

    let private everyNumbering : SignalNumbering list =
        [ SignalNumbering.Linux ; SignalNumbering.Darwin ]

    /// `Interop.Sys.Signals`, read from the BCL, as name to value.
    let private enumMembers : Map<string, int> =
        let enumType = typeof<Process>.Assembly.GetType ("Interop+Sys+Signals", true)

        Enum.GetNames enumType
        |> Array.map (fun name -> name, Convert.ToInt32 (Enum.Parse (enumType, name)))
        |> Map.ofArray

    [<Test>]
    let ``the enum has exactly the three members the screen admits`` () : unit =
        enumMembers
        |> shouldEqual (Map.ofList [ "None", 0 ; "SIGKILL", 9 ; "SIGSTOP", 19 ])

    [<Test>]
    let ``each member names the signal its name says, under either numbering`` () : unit =
        for numbering in everyNumbering do
            KillSignalPal.toSigno numbering enumMembers.["None"] |> shouldEqual (Some 0)

            let signalOf (signo : int) : Signal =
                match Signal.ofRawSignoUnder numbering signo with
                | ValueSome signal -> signal
                | ValueNone -> failwith $"%d{signo} is not a signal under %O{numbering}"

            match KillSignalPal.toSigno numbering enumMembers.["SIGKILL"] |> Option.map signalOf with
            | Some signal ->
                // 9 under every numbering: POSIX's XSI option fixes it.
                Signal.toRawSignoUnder numbering signal |> shouldEqual 9
                Signal.isUncatchableUnder numbering signal |> shouldEqual true

                Signal.defaultDispositionUnder numbering signal
                |> shouldEqual DefaultDisposition.Terminate
            | other -> failwith $"SIGKILL under %O{numbering}: %O{other}"

            match KillSignalPal.toSigno numbering enumMembers.["SIGSTOP"] |> Option.map signalOf with
            | Some signal ->
                Signal.isUncatchableUnder numbering signal |> shouldEqual true

                Signal.defaultDispositionUnder numbering signal
                |> shouldEqual DefaultDisposition.Stop
            | other -> failwith $"SIGSTOP under %O{numbering}: %O{other}"

    [<Test>]
    let ``every other value is refused, as the host's shim refuses it`` () : unit =
        let hostNumbering =
            if RuntimeInformation.IsOSPlatform OSPlatform.OSX then
                Some SignalNumbering.Darwin
            elif RuntimeInformation.IsOSPlatform OSPlatform.Linux then
                Some SignalNumbering.Linux
            else
                None

        let self = Environment.ProcessId

        for pal in -3 .. 70 do
            if pal <> 9 && pal <> 19 then
                let modelled = KillSignalPal.toSigno SignalNumbering.Linux pal

                KillSignalPal.toSigno SignalNumbering.Darwin pal |> shouldEqual modelled

                match hostNumbering with
                | None -> ()
                | Some _ ->
                    // Safe to send: 0 is the null signal, and anything else
                    // the shim refuses before calling kill(2).
                    let result = hostKill (self, pal)

                    match modelled with
                    | Some 0 -> result |> shouldEqual 0
                    | Some other -> failwith $"%d{pal} unexpectedly maps to %O{other}"
                    | None -> result |> shouldEqual -1
