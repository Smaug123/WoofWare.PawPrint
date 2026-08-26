namespace ExnSpike

open System.Diagnostics
open Microsoft.Extensions.Logging.Abstractions
open WoofWare.PawPrint

module Driver =

    let private pct (n : int) (d : int) =
        if d = 0 then 0.0 else 100.0 * float n / float d

    let private report (label : string) (a : Escape.Analysis) =
        let all = a.Escaping |> Seq.map (fun kv -> kv.Value) |> List.ofSeq
        let total = all.Length
        let unknown = all |> List.filter _.Unknown |> List.length

        let clean =
            all
            |> List.filter (fun s -> not s.Unknown && Set.isEmpty s.Types)
            |> List.length

        let exact =
            all
            |> List.filter (fun s -> not s.Unknown && not (Set.isEmpty s.Types))
            |> List.length

        printfn ""
        printfn "===== %s =====" label
        printfn "fixpoint rounds: %d" a.Rounds
        printfn "methods: %d" total
        printfn "  provably throws nothing:            %6d  %5.1f%%" clean (pct clean total)
        printfn "  exact non-empty escaping set:       %6d  %5.1f%%" exact (pct exact total)
        printfn "  Unknown (analysis envelope hit):    %6d  %5.1f%%" unknown (pct unknown total)

        let sizes =
            all
            |> List.filter (fun s -> not s.Unknown)
            |> List.map (fun s -> Set.count s.Types)

        if not sizes.IsEmpty then
            let sorted = List.sort sizes

            let at (p : float) =
                sorted.[min (sorted.Length - 1) (int (p * float sorted.Length))]

            printfn "  escaping-set size (exact answers): median %d, p90 %d, max %d" (at 0.5) (at 0.9) (List.max sorted)

        printfn "  incompleteness reasons (site counts):"

        for KeyValue (r, c) in a.Reasons |> Seq.sortByDescending (fun kv -> kv.Value) do
            printfn "    %-24O %8d" r c

    let private show (a : Escape.Analysis) (needle : string) (n : int) =
        printfn ""
        printfn "-- sample answers matching %s --" needle

        let mutable shown = 0

        for KeyValue (h, name) in a.Names do
            if shown < n && name.Contains needle then
                match a.Escaping.TryGetValue h with
                | true, s ->
                    let body =
                        if s.Unknown && Set.isEmpty s.Types then
                            "UNKNOWN"
                        elif s.Unknown then
                            (s.Types |> Set.toList |> String.concat ", ") + ", + UNKNOWN"
                        elif Set.isEmpty s.Types then
                            "(nothing)"
                        else
                            s.Types |> Set.toList |> String.concat ", "

                    printfn "  %s" name
                    printfn "      %s" body
                    shown <- shown + 1
                | _ -> ()

    [<EntryPoint>]
    let main (argv : string[]) : int =
        let path = argv.[0]
        let lf = NullLoggerFactory.Instance
        let sw = Stopwatch.StartNew ()
        let assy = Assembly.readFile lf path
        printfn "read %s in %dms" path sw.ElapsedMilliseconds

        Census.run assy

        let sw2 = Stopwatch.StartNew ()
        let withImplicit = Escape.run true assy
        printfn ""
        printfn "escape analysis (implicit on) took %dms" sw2.ElapsedMilliseconds
        report "sound: opcode-raised exceptions included" withImplicit

        let sw3 = Stopwatch.StartNew ()
        let withoutImplicit = Escape.run false assy
        printfn ""
        printfn "escape analysis (implicit off) took %dms" sw3.ElapsedMilliseconds
        report "unsound control: only explicit `throw`s and propagation" withoutImplicit

        if argv.Length > 1 then
            show withoutImplicit argv.[1] 40

        0
