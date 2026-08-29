namespace WoofWare.PosixKernel.Test

open System
open System.Diagnostics
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework

/// The socket/epoll differential fuzzer
/// (docs/plans/2026-08-22-socket-epoll-fuzzer.md): generated op sequences run
/// against real Linux epoll (the `harness.c` interpreter, in the `gcc:14`
/// container) and against this library's `UnixSystem`, and the transcripts
/// must agree.
///
/// The live test needs the container and so is gated on
/// `PAWPRINT_SOCKET_FUZZ=1`; the corpus test replays previously-measured real
/// transcripts and runs everywhere.
[<TestFixture>]
module TestSocketFuzz =

    let private corpusPrefix = "WoofWare.PosixKernel.Test.socketFuzzCorpus."

    /// One binding, named once: the resolution test below has to guard the
    /// name `runHarness` actually uses, and a second copy of the literal
    /// would guard only itself.
    let private harnessResource = "WoofWare.PosixKernel.Test.socketFuzz.harness.c"

    let private readResource (name : string) : string =
        let assembly = Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)
        reader.ReadToEnd ()

    /// One corpus row: the serialized sequence, tab, the real kernel's
    /// transcript as the harness measured it.
    let private corpusRows () : (string * string * string) list =
        let assembly = Assembly.GetExecutingAssembly ()

        assembly.GetManifestResourceNames ()
        |> Array.filter (fun name -> name.StartsWith (corpusPrefix, StringComparison.Ordinal))
        |> Array.sort
        |> Array.toList
        |> List.collect (fun name ->
            (readResource name).Split ('\n', StringSplitOptions.RemoveEmptyEntries)
            |> Array.toList
            |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))
            |> List.map (fun line ->
                match line.TrimEnd('\r').Split '\t' with
                | [| ops ; transcript |] -> name, ops, transcript
                | _ -> failwith $"corpus %s{name}: line is not ops<TAB>transcript: %s{line}"
            )
        )

    /// Both embedded resource families resolve under this assembly's name.
    ///
    /// A logical resource name is `<assembly>.<directory>.<file>`, so it changes
    /// when the project does. The corpus guards itself -- an empty replay is
    /// refused as vacuous below -- but `harness.c` is read only inside
    /// `runHarness`, which only the container-gated live test reaches, so a wrong
    /// name there would go unnoticed until someone had a container. This is that
    /// half.
    [<Test>]
    let ``the harness and the corpus resolve as embedded resources`` () : unit =
        let harness = readResource harnessResource

        // Named for the function this fuzzer's F# side mirrors, so the row fails
        // if the resource resolves to something that is not the harness.
        harness.Contains "interest_to_epoll" |> shouldEqual true

        corpusRows () |> shouldNotEqual []

    /// A sequence the generator should never have produced is a `Defect`, not a
    /// `Refused`.
    ///
    /// The distinction is what the live fuzzer does with it: a `Refused`
    /// sequence is outside the modelled envelope and gets skipped and counted,
    /// while a `Defect` fails the run. `executeEmulated` tells them apart by
    /// looking for a marker in the `failwith` text, so a generator-bug failure
    /// that forgets the marker is silently downgraded to a skip -- and the fuzzer
    /// goes on reporting agreement it never checked.
    ///
    /// An interest mask outside the five bits the op language defines is the
    /// cheapest such shape to construct: `parse` does not screen it, so it
    /// reaches the conversion exactly as a generator regression would.
    [<Test>]
    let ``an op outside the language is a defect rather than a skip`` () : unit =
        match SocketFuzz.executeEmulated (SocketFuzz.parse "port:0 sock:1 add:0:1:32") with
        | EmulatedRun.Defect (_, message) -> message |> shouldContainText "0x20"
        | EmulatedRun.Refused (_, message) ->
            Assert.Fail
                $"an out-of-range interest mask was classified as a skippable refusal, so the live fuzzer would have counted it rather than failed: %s{message}"
        | EmulatedRun.Transcript transcript ->
            Assert.Fail $"an out-of-range interest mask was accepted outright: %s{transcript}"

    [<Test>]
    let ``SocketFuzzCorpus: every measured real-kernel transcript replays against the emulated kernel`` () : unit =
        let rows = corpusRows ()
        // The corpus is checked in; an empty replay would be vacuous.
        rows |> shouldNotEqual []

        for name, ops, expected in rows do
            match SocketFuzz.executeEmulated (SocketFuzz.parse ops) with
            | EmulatedRun.Transcript actual ->
                if actual <> expected then
                    Assert.Fail $"corpus %s{name}\nsequence: %s{ops}\nreal:     %s{expected}\nemulated: %s{actual}"
            | EmulatedRun.Refused (index, message) ->
                Assert.Fail
                    $"corpus %s{name}\nsequence: %s{ops}\nthe emulated kernel refused op %d{index}, but the row was corpus-recorded as comparable:\n%s{message}"
            | EmulatedRun.Defect (index, message) ->
                Assert.Fail $"corpus %s{name}\nsequence: %s{ops}\ndefect at op %d{index}:\n%s{message}"

    /// Run the harness over `sequences` in the container; answers one result
    /// line per sequence: `Ok transcript`, or `Error` for an unstable one
    /// (the three repetitions disagreed, with the raw line preserved).
    let private runHarness (workDir : string) (sequences : string list) : Result<string, string> list =
        Directory.CreateDirectory workDir |> ignore

        File.WriteAllText (Path.Combine (workDir, "harness.c"), readResource harnessResource)

        File.WriteAllLines (Path.Combine (workDir, "seqs.txt"), sequences)

        let psi = ProcessStartInfo ()
        psi.FileName <- "container"

        for arg in
            [
                "run"
                "--rm"
                "-v"
                $"%s{workDir}:/fuzz"
                "gcc:14"
                "bash"
                "-c"
                "gcc -O2 -Wall -Wextra -Werror -o /tmp/h /fuzz/harness.c && /tmp/h < /fuzz/seqs.txt > /fuzz/out.txt"
            ] do
            psi.ArgumentList.Add arg

        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true

        use proc = Process.Start psi
        let stdout = proc.StandardOutput.ReadToEndAsync ()
        let stderr = proc.StandardError.ReadToEndAsync ()

        // Container start plus compile is seconds; the run itself is bounded
        // by settles: 3 repetitions x ~20 ops x 5ms per sequence.
        if not (proc.WaitForExit (10 * 60 * 1000)) then
            proc.Kill true
            failwith "the container run did not finish within ten minutes"

        if proc.ExitCode <> 0 then
            failwith
                $"the container run failed with exit code %d{proc.ExitCode}\nstdout:\n%s{stdout.Result}\nstderr:\n%s{stderr.Result}"

        let results =
            File.ReadAllLines (Path.Combine (workDir, "out.txt"))
            |> Array.filter (fun line -> line <> "")
            |> Array.map (fun line ->
                if line.StartsWith ("= ", StringComparison.Ordinal) then
                    Ok (line.Substring 2)
                elif line.StartsWith ("! ", StringComparison.Ordinal) then
                    Error line
                else
                    failwith $"unparseable harness output line: %s{line}"
            )
            |> Array.toList

        if results.Length <> sequences.Length then
            failwith
                $"the harness answered %d{results.Length} lines for %d{sequences.Length} sequences\nstderr:\n%s{stderr.Result}"

        results

    let private opKind (op : FuzzOp) : string =
        match op with
        | FuzzOp.NewSocket _ -> "sock"
        | FuzzOp.Listen _ -> "lstn"
        | FuzzOp.Connect _ -> "conn"
        | FuzzOp.ConnectDead _ -> "conndead"
        | FuzzOp.Accept _ -> "acpt"
        | FuzzOp.Close _ -> "close"
        | FuzzOp.Dup _ -> "dup"
        | FuzzOp.NewPort _ -> "port"
        | FuzzOp.Add _ -> "add"
        | FuzzOp.Mod _ -> "mod"
        | FuzzOp.Del _ -> "del"
        | FuzzOp.Wait _ -> "wait"
        | FuzzOp.Poll _ -> "poll"

    let private allOpKinds : string list =
        [
            "sock"
            "lstn"
            "conn"
            "conndead"
            "acpt"
            "close"
            "dup"
            "port"
            "add"
            "mod"
            "del"
            "wait"
            "poll"
        ]

    [<Test>]
    let ``SocketFuzzLive: generated sequences agree with the real kernel`` () : unit =
        if Environment.GetEnvironmentVariable "PAWPRINT_SOCKET_FUZZ" <> "1" then
            Assert.Ignore
                "differential fuzzing needs the `container` CLI; opt in with PAWPRINT_SOCKET_FUZZ=1 (see docs/plans/2026-08-22-socket-epoll-fuzzer.md)"

        let sequenceCount =
            match Environment.GetEnvironmentVariable "PAWPRINT_SOCKET_FUZZ_SEQUENCES" with
            | null
            | "" -> 150
            | s -> int s

        let seed =
            match Environment.GetEnvironmentVariable "PAWPRINT_SOCKET_FUZZ_SEED" with
            | null
            | "" -> 20260822
            | s -> int s

        let rng = Random seed
        let sequences = List.init sequenceCount (fun _ -> SocketFuzz.generate rng)

        // Emulated side first: it decides which sequences are inside the
        // modelled envelope at all.
        let emulated =
            sequences |> List.map (fun ops -> ops, SocketFuzz.executeEmulated ops)

        let defects =
            emulated
            |> List.choose (fun (ops, run) ->
                match run with
                | EmulatedRun.Defect (index, message) ->
                    Some $"sequence: %s{SocketFuzz.serialize ops}\ndefect at op %d{index}: %s{message}"
                | EmulatedRun.Transcript _
                | EmulatedRun.Refused _ -> None
            )

        if not (List.isEmpty defects) then
            Assert.Fail (
                $"seed %d{seed}: %d{defects.Length} sequences hit an interpreter defect:\n\n"
                + String.concat "\n\n" defects
            )

        let refused =
            emulated
            |> List.choose (fun (ops, run) ->
                match run with
                | EmulatedRun.Refused (index, message) -> Some (ops, index, message)
                | EmulatedRun.Transcript _
                | EmulatedRun.Defect _ -> None
            )

        let comparable =
            emulated
            |> List.choose (fun (ops, run) ->
                match run with
                | EmulatedRun.Transcript transcript -> Some (ops, transcript)
                | EmulatedRun.Refused _
                | EmulatedRun.Defect _ -> None
            )

        let workDir =
            Path.Combine (TestContext.CurrentContext.WorkDirectory, $"socketFuzz-seed%d{seed}")

        let real = runHarness workDir (comparable |> List.map (fst >> SocketFuzz.serialize))

        let unstable =
            List.zip comparable real
            |> List.choose (fun ((ops, _), result) ->
                match result with
                | Error line -> Some (ops, line)
                | Ok _ -> None
            )

        let compared =
            List.zip comparable real
            |> List.choose (fun ((ops, emulatedTranscript), result) ->
                match result with
                | Ok realTranscript -> Some (ops, emulatedTranscript, realTranscript)
                | Error _ -> None
            )

        let divergences =
            compared
            |> List.filter (fun (_, emulatedTranscript, realTranscript) -> emulatedTranscript <> realTranscript)

        // A divergence is the fuzzer's whole purpose: report it with a
        // one-line repro before any distribution bookkeeping.
        if not (List.isEmpty divergences) then
            let rendered =
                divergences
                |> List.map (fun (ops, emulatedTranscript, realTranscript) ->
                    $"sequence: %s{SocketFuzz.serialize ops}\nreal:     %s{realTranscript}\nemulated: %s{emulatedTranscript}"
                )

            Assert.Fail (
                $"seed %d{seed}: %d{divergences.Length} of %d{compared.Length} compared sequences diverged:\n\n"
                + String.concat "\n\n" rendered
            )

        match Environment.GetEnvironmentVariable "PAWPRINT_SOCKET_FUZZ_WRITE_CORPUS" with
        | null
        | "" -> ()
        | path ->
            File.WriteAllLines (
                path,
                compared
                |> List.map (fun (ops, _, realTranscript) -> $"%s{SocketFuzz.serialize ops}\t%s{realTranscript}")
            )

        // Distribution assertions, not printouts: the run must actually have
        // explored the space it claims to cover.
        let report =
            let refusalSummary =
                refused
                |> List.truncate 5
                |> List.map (fun (ops, index, message) ->
                    let head = message.Split '\n' |> Array.head
                    $"  op %d{index} of %s{SocketFuzz.serialize ops}: %s{head}"
                )
                |> String.concat "\n"

            $"seed %d{seed}: compared %d{compared.Length}, refusal-skipped %d{refused.Length}, unstable %d{unstable.Length}\nsample refusals:\n%s{refusalSummary}"

        if compared.Length < sequenceCount / 2 then
            Assert.Fail
                $"under half the sequences were compared — the generator is drifting out of the modelled envelope.\n%s{report}"

        if unstable.Length * 10 > sequenceCount then
            Assert.Fail
                $"over a tenth of the sequences were unstable on the real side — the settle is too short.\n%s{report}"

        let comparedKinds =
            compared
            |> List.collect (fun (ops, _, _) -> ops |> List.map opKind)
            |> Set.ofList

        for kind in allOpKinds do
            if not (Set.contains kind comparedKinds) then
                Assert.Fail
                    $"no compared sequence contains a '%s{kind}' op — the generator's coverage collapsed.\n%s{report}"

        let nonEmptyBatches =
            compared
            |> List.sumBy (fun (_, _, realTranscript) ->
                realTranscript.Split ' '
                |> Array.sumBy (fun token ->
                    if token.StartsWith ("[", StringComparison.Ordinal) && token <> "[]" then
                        1
                    else
                        0
                )
            )

        if nonEmptyBatches < sequenceCount / 10 then
            Assert.Fail
                $"only %d{nonEmptyBatches} nonempty delivered batches across the whole run — the sequences are not reaching delivery.\n%s{report}"

        TestContext.Out.WriteLine report
