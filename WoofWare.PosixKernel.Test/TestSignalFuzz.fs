namespace WoofWare.PosixKernel.Test

open System
open System.Diagnostics
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The signal fuzzer: generated sequences of `sigaction`, `kill` and `raise`,
/// some of them run from inside handlers, on a real kernel (`signalFuzz/harness.c`,
/// built with the host's C compiler) and on this library, whose transcripts --
/// each handler's start with its mask and its signal's disposition, each op's
/// `sigpending`, and how the process ended -- must agree.
///
/// Single-threaded, and every signal is sent by the process to itself, so each
/// kernel's answer is deterministic. The live test runs on a Linux or Darwin
/// host and checks that host's flavour; the corpus replays transcripts measured
/// on both flavours everywhere.
[<TestFixture>]
module TestSignalFuzz =

    let private harnessResource = "WoofWare.PosixKernel.Test.signalFuzz.harness.c"

    let private readResource (name : string) : string =
        let assembly = Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)
        reader.ReadToEnd ()

    let private platformOf (flavour : SimulatedUnixFlavour) : SimulatedUnixPlatform = HostPlatform.platformOf flavour

    /// Compare one sequence's two transcripts, failing with both.
    let private compare (platform : SimulatedUnixPlatform) (line : string) (measured : string) : bool =
        match SignalFuzz.executeEmulated platform (SignalFuzz.parse line) with
        | SignalFuzzRun.Refused _ -> false
        | SignalFuzzRun.Transcript emulated ->
            if emulated <> measured then
                Assert.Fail $"sequence: %s{line}\nreal:     %s{measured}\nemulated: %s{emulated}"

            true

    [<Test>]
    let ``serialising a sequence and parsing it back is the identity`` () : unit =
        for numbering in [ SignalNumbering.Linux ; SignalNumbering.Darwin ] do
            let rng = Random 20260930

            for _ in 1..500 do
                let sequence = SignalFuzz.generate numbering rng
                SignalFuzz.parse (SignalFuzz.serialise sequence) |> shouldEqual sequence

    [<Test>]
    let ``the harness resolves as an embedded resource`` () : unit =
        (readResource harnessResource).Contains "run_sequence" |> shouldEqual true

    /// Every row of the measured corpus, per flavour: `sequence<TAB>transcript`.
    let private corpus (flavour : string) : (string * string) list =
        (readResource $"WoofWare.PosixKernel.Test.signalFuzzCorpus.%s{flavour}.txt")
            .Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))
        |> List.map (fun line ->
            match line.TrimEnd('\r').Split '\t' with
            | [| sequence ; transcript |] -> sequence, transcript
            | _ -> failwith $"corpus %s{flavour}: not sequence<TAB>transcript: %s{line}"
        )

    [<TestCase "linux">]
    [<TestCase "darwin">]
    let ``every measured corpus row agrees with the model`` (file : string) : unit =
        let flavour =
            match file with
            | "linux" -> SimulatedUnixFlavour.Linux
            | "darwin" -> SimulatedUnixFlavour.Darwin
            | other -> failwith $"no such flavour: %s{other}"

        let rows = corpus file
        rows.Length |> shouldBeGreaterThan 100

        let compared =
            rows
            |> List.filter (fun (sequence, transcript) -> compare (platformOf flavour) sequence transcript)
            |> List.length

        // A row the model now refuses was recorded as comparable, so it is a
        // regression, not a skip.
        compared |> shouldEqual rows.Length

    /// Build the harness with the host's C compiler, in `workDir`.
    let private buildHarness (workDir : string) : string =
        Directory.CreateDirectory workDir |> ignore
        let source = Path.Combine (workDir, "harness.c")
        let binary = Path.Combine (workDir, "harness")
        File.WriteAllText (source, readResource harnessResource)

        let psi = ProcessStartInfo "cc"

        for arg in
            [
                "-O2"
                "-Wall"
                "-Wextra"
                "-Werror"
                "-o"
                binary
                source
                "-lpthread"
            ] do
            psi.ArgumentList.Add arg

        psi.RedirectStandardError <- true
        psi.RedirectStandardOutput <- true

        use proc =
            try
                Process.Start psi
            with :? ComponentModel.Win32Exception as e ->
                // Inside the devshell `cc` is always there, and a run without it
                // would silently check nothing.
                if isNull (Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC") then
                    Assert.Ignore $"no C compiler to build the harness with: %s{e.Message}"

                reraise ()

        let stderr = proc.StandardError.ReadToEnd ()
        proc.WaitForExit ()

        if proc.ExitCode <> 0 then
            failwith $"building the harness failed:\n%s{stderr}"

        binary

    /// Run the harness over `sequences`, one transcript per sequence.
    let private runHarness (binary : string) (sequences : string list) : string list =
        let psi = ProcessStartInfo binary
        psi.RedirectStandardInput <- true
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true

        use proc = Process.Start psi
        let stdout = proc.StandardOutput.ReadToEndAsync ()
        let stderr = proc.StandardError.ReadToEndAsync ()

        for sequence in sequences do
            proc.StandardInput.WriteLine sequence

        proc.StandardInput.Close ()

        // Each sequence is bounded by the child's alarm(10); in practice a few
        // milliseconds.
        if not (proc.WaitForExit (10 * 60 * 1000)) then
            proc.Kill true
            failwith "the harness did not finish within ten minutes"

        if proc.ExitCode <> 0 then
            failwith $"the harness failed with exit code %d{proc.ExitCode}\n%s{stderr.Result}"

        let lines =
            stdout.Result.Split ('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> List.map (fun line ->
                if line.StartsWith ("= ", StringComparison.Ordinal) then
                    line.Substring 2
                else
                    failwith $"unparseable harness output line: %s{line}"
            )

        if lines.Length <> sequences.Length then
            failwith $"the harness answered %d{lines.Length} lines for %d{sequences.Length} sequences"

        lines

    [<Test>]
    let ``generated sequences agree with this host's kernel`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = platformOf flavour
            let numbering = SimulatedUnixPlatform.signalNumbering platform

            let seed =
                match Environment.GetEnvironmentVariable "PAWPRINT_SIGNAL_FUZZ_SEED" with
                | null
                | "" -> 20260930
                | s -> int s

            let count =
                match Environment.GetEnvironmentVariable "PAWPRINT_SIGNAL_FUZZ_SEQUENCES" with
                | null
                | "" -> 400
                | s -> int s

            let rng = Random seed

            let sequences =
                List.init count (fun _ -> SignalFuzz.generate numbering rng |> SignalFuzz.serialise)

            let unique = Guid.NewGuid().ToString "N"
            let workDir = Path.Combine (Path.GetTempPath (), $"pawprint-signal-fuzz-%s{unique}")

            try
                let binary = buildHarness workDir
                let measured = runHarness binary sequences

                let compared =
                    List.zip sequences measured
                    |> List.filter (fun (sequence, transcript) -> compare platform sequence transcript)
                    |> List.length

                // Most sequences stay inside the modelled envelope; a generator
                // change that pushed them all out would otherwise pass vacuously.
                compared |> shouldBeGreaterThan (count / 2)
            finally
                try
                    Directory.Delete (workDir, true)
                with _ ->
                    ()
        )
