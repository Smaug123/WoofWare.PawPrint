namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `PipeBuffer`, replayed against every call recorded in `pipeBuffer/linux.txt`
/// and `pipeBuffer/darwin.txt`: what a real pipe answered to random
/// non-blocking reads and writes, with its `FIONREAD` and both ends' `poll`
/// after each. Each file's header says where and how it was measured.
///
/// The corpus holds each flavour's column whichever machine runs the suite;
/// `TestPipeBufferAgainstHost` holds the host's own column live.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipeBufferCorpus =

    type private Row =
        {
            Case : string
            IsWrite : bool
            Count : int
            Returned : int
            Errno : int
            Held : int
            PollRead : int
            PollWrite : int
        }

    let private rows (flavour : string) : Row list =
        let assembly = Assembly.GetExecutingAssembly ()
        let name = $"WoofWare.PosixKernel.Test.pipeBuffer.%s{flavour}.txt"

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))
        |> List.map (fun line ->
            match line.Split ('|') |> Array.map (fun part -> part.Trim ()) with
            | [| call ; held ; polls ; _sizes |] ->
                let call = call.Split (' ', StringSplitOptions.RemoveEmptyEntries)
                let polls = polls.Split (' ', StringSplitOptions.RemoveEmptyEntries)

                {
                    Case = call.[0]
                    IsWrite =
                        match call.[1] with
                        | "W" -> true
                        | "R" -> false
                        | other -> failwith $"unknown operation %s{other} in: %s{line}"
                    Count = int call.[2]
                    Returned = int call.[4]
                    Errno = int call.[5]
                    Held = int held
                    PollRead = Convert.ToInt32 (polls.[0], 16)
                    PollWrite = Convert.ToInt32 (polls.[1], 16)
                }
            | _ -> failwith $"malformed corpus row: %s{line}"
        )

    /// Replay every case from a fresh buffer. `readBits` and `writeBits` are the
    /// `poll` bits each end reported when ready, which these rows show are the
    /// only nonzero answers: readiness is one level per end.
    let private replay
        (platform : SimulatedUnixPlatform)
        (flavour : string)
        (eagain : int)
        (readBits : int)
        (writeBits : int)
        (expectedRows : int)
        : unit
        =
        let all = rows flavour
        all |> List.length |> shouldEqual expectedRows

        for case, rows in all |> List.groupBy (fun row -> row.Case) do
            let mutable buffer = PipeBuffer.empty platform
            let mutable offered = 0
            let mutable delivered = 0

            for i, row in List.indexed rows do
                let where = $"%s{flavour} %s{case} call %d{i}"

                if row.IsWrite then
                    let accepted, b = PipeBuffer.write (TestPipeBuffer.payload offered row.Count) buffer

                    let expected =
                        if row.Returned >= 0 then
                            row.Returned
                        elif row.Errno = eagain then
                            0
                        else
                            failwith $"%s{where}: a write failed with errno %d{row.Errno}, which no row should record"

                    (where, accepted) |> shouldEqual (where, expected)
                    buffer <- b
                    offered <- offered + accepted
                else
                    let got, b = PipeBuffer.read row.Count buffer
                    let expected = if row.Returned >= 0 then row.Returned else 0
                    (where, got.Length) |> shouldEqual (where, expected)

                    got
                    |> Seq.iteri (fun k value ->
                        if value <> byte ((delivered + k) % 256) then
                            failwith $"%s{where}: byte %d{k} came out of order"
                    )

                    buffer <- b
                    delivered <- delivered + got.Length

                (where, PipeBuffer.held buffer) |> shouldEqual (where, row.Held)

                (where, row.PollRead = 0 || row.PollRead = readBits, PipeBuffer.readable buffer)
                |> shouldEqual (where, true, row.PollRead <> 0)

                (where, row.PollWrite = 0 || row.PollWrite = writeBits, PipeBuffer.writable buffer)
                |> shouldEqual (where, true, row.PollWrite <> 0)

    [<Test>]
    let ``every recorded Linux call is answered as the kernel answered it`` () : unit =
        // Measured with 4 KiB pages on aarch64; x86-64's pages are 4 KiB too,
        // and the rule is the page size's, so both presets must replay it.
        for platform in [ SimulatedUnixPlatform.linuxArm64 ; SimulatedUnixPlatform.linuxX64 ] do
            replay platform "linux" 11 0x41 0x104 4800

    [<Test>]
    let ``every recorded Darwin call is answered as the kernel answered it`` () : unit =
        replay SimulatedUnixPlatform.macOsArm64 "darwin" 35 0xc3 0x104 5471
