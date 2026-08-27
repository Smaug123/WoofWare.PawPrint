namespace ExnSpike

open System.Collections.Generic
open System.Reflection
open System.Reflection.Metadata
open WoofWare.PawPrint

/// How much of the `TypeInitializationException` load could a static checker discharge?
///
/// Every invoking instruction carries `TypeInitialization`, which after the `.cctor` fix is the
/// largest single entry in the whole table — 121,520 sites in CoreLib. That is sound but nearly
/// content-free, so the question is whether it can be pruned per call site rather than carried
/// everywhere.
///
/// Two prunes are available, and this measures both:
///
/// 1. **The target type has no `.cctor` at all.** Then there is no initializer to fail, exactly.
///    Pure metadata, no analysis, no approximation.
/// 2. **The target type's `.cctor` provably cannot throw.** That is the escaping-exception
///    question again, asked of one particular method, so the fixpoint already answers it.
///
/// A third prune is *not* available here, and it is worth saying why. ECMA-335 I.8.9.5 does not
/// list method invocation as a trigger for a `beforefieldinit` type — only static field access —
/// so in principle a call to such a type could be pruned too. PawPrint deliberately runs `.cctor`s
/// eagerly regardless of the flag (II.10.5.3.2 permits eager schedules; see
/// `IlMachineStateExecution.fs` and `docs/divergences.md`), so an analyser taking that prune would
/// disagree with the interpreter that is meant to validate it. The flag is counted below for
/// information, not used.
module CctorCensus =

    type private Counters () =
        let d = Dictionary<string, int> ()

        member _.Bump (k : string) =
            match d.TryGetValue k with
            | true, v -> d.[k] <- v + 1
            | _ -> d.[k] <- 1

        member _.Items =
            d
            |> Seq.sortByDescending (fun kv -> kv.Value)
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> List.ofSeq

    /// Classification of one invoking instruction, by what its target type's initializer could do.
    [<RequireQualifiedAccess>]
    type private Verdict =
        /// The target type has no `.cctor`. No initializer, so no `TypeInitializationException`.
        | NoInitializer
        /// The target type has a `.cctor` which the fixpoint proves raises nothing.
        | InitializerCannotThrow
        /// The target type has a `.cctor` that can throw, or whose answer is `Unknown`.
        | InitializerMayThrow
        /// The target is not a MethodDef in this assembly, so its declaring type is not reachable
        /// from here. This is the same wall the escape analysis reports, showing up again.
        | TargetNotLocal

    let run (assy : DumpedAssembly) (analysis : Escape.Analysis) : unit =
        // TypeDef -> its `.cctor`, when it has one.
        let cctorOf = Dictionary<TypeDefinitionHandle, MethodDefinitionHandle> ()
        let typeOfMethod = Dictionary<MethodDefinitionHandle, TypeDefinitionHandle> ()
        let beforeFieldInit = HashSet<TypeDefinitionHandle> ()

        for KeyValue (th, ty) in assy.TypeDefs do
            if ty.TypeAttributes.HasFlag TypeAttributes.BeforeFieldInit then
                beforeFieldInit.Add th |> ignore

            for m in ty.Methods do
                match m.TryMetadata with
                | Some facts ->
                    typeOfMethod.[facts.Handle] <- th

                    if m.Name = ".cctor" && m.IsStatic then
                        cctorOf.[th] <- facts.Handle
                | None -> ()

        /// Does this `.cctor` provably raise nothing?
        let cctorIsHarmless (h : MethodDefinitionHandle) : bool =
            match analysis.Escaping.TryGetValue h with
            | true, s -> not s.Unknown && Set.isEmpty s.Types
            | _ -> false

        let verdictFor (token : MetadataToken) : Verdict =
            match token with
            | MetadataToken.MethodDef mh ->
                match typeOfMethod.TryGetValue mh with
                | false, _ -> Verdict.TargetNotLocal
                | true, th ->
                    match cctorOf.TryGetValue th with
                    | false, _ -> Verdict.NoInitializer
                    | true, cctor ->
                        if cctorIsHarmless cctor then
                            Verdict.InitializerCannotThrow
                        else
                            Verdict.InitializerMayThrow
            | _ -> Verdict.TargetNotLocal

        let sites = Counters ()
        let mutable invokingSites = 0

        for KeyValue (_, ty) in assy.TypeDefs do
            for m in ty.Methods do
                match m.Body with
                | MethodBody.Il instrs ->
                    for op, _ in instrs.Instructions do
                        match op with
                        | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt | UnaryMetadataTokenIlOp.Newobj | UnaryMetadataTokenIlOp.Jmp),
                                                   operand) ->
                            invokingSites <- invokingSites + 1

                            let v =
                                match operand with
                                | MetadataOperand.FromMetadata t -> verdictFor t.Token
                                | MetadataOperand.FromDynamicScope _ -> Verdict.TargetNotLocal

                            sites.Bump $"%O{v}"
                        | _ -> ()
                | _ -> ()

        // The initializers themselves.
        let cctorVerdicts = Counters ()

        for KeyValue (th, cctor) in cctorOf do
            let kind =
                match analysis.Escaping.TryGetValue cctor with
                | true, s when not s.Unknown && Set.isEmpty s.Types -> "provably raises nothing"
                | true, s when s.Unknown -> "Unknown (analysis envelope)"
                | true, _ -> "can throw a named type"
                | _ -> "not analysed"

            cctorVerdicts.Bump kind

            cctorVerdicts.Bump (
                if beforeFieldInit.Contains th then
                    "  ...of which beforefieldinit"
                else
                    "  ...of which NOT beforefieldinit"
            )

        let pct (n : int) (d : int) =
            if d = 0 then 0.0 else 100.0 * float n / float d

        printfn ""
        printfn "===== can a static checker discharge the TypeInitializationException load? ====="
        printfn "types: %d, of which %d have a .cctor" assy.TypeDefs.Count cctorOf.Count
        printfn ""
        printfn "-- invoking instructions, by what their target type's initializer could do --"
        printfn "   (%d sites)" invokingSites

        for k, v in sites.Items do
            printfn "  %-28s %8d  %5.1f%%" k v (pct v invokingSites)

        printfn ""
        printfn "-- the .cctors themselves --"

        for k, v in cctorVerdicts.Items do
            printfn "  %-38s %6d" k v

        // What the throwing ones actually carry. If most of them carry only
        // `TypeInitializationException`, the prune is being defeated by the analysis's own
        // over-approximation rather than by initializers that genuinely throw -- a `.cctor` that
        // writes its own type's statics picks up `TypeInitialization` from `stsfld`, even though
        // the initializer it would supposedly trigger is the one already running.
        let shapes = Counters ()

        for KeyValue (_, cctor) in cctorOf do
            match analysis.Escaping.TryGetValue cctor with
            | true, s when not s.Unknown && not (Set.isEmpty s.Types) ->
                s.Types
                |> Set.toList
                |> List.map (Escape.display assy)
                |> List.sort
                |> String.concat ", "
                |> shapes.Bump
            | _ -> ()

        printfn ""
        printfn "-- what the throwing .cctors carry (exact answers only) --"

        for k, v in shapes.Items |> List.truncate 12 do
            printfn "  %6d  %s" v k

        // Named examples, because "23.9% of sites" is not something a reader can picture. These
        // are the types whose initializer the analysis says can throw, so a call to any of their
        // members carries `TypeInitializationException`.
        let named =
            [
                for KeyValue (th, cctor) in cctorOf do
                    match assy.TypeDefs.TryGetValue th, analysis.Escaping.TryGetValue cctor with
                    | (true, ty), (true, s) when not s.Unknown && not (Set.isEmpty s.Types) ->
                        let what =
                            s.Types
                            |> Set.toList
                            |> List.map (Escape.display assy)
                            |> List.sort
                            |> String.concat ", "

                        // A nested type carries an empty namespace and its own short name, so name
                        // it through its declaring type or the reader sees a wall of `.<>c`.
                        let name =
                            if ty.IsNested then
                                match assy.TypeDefs.TryGetValue ty.DeclaringType with
                                | true, outer -> $"{outer.Namespace}.{outer.Name}+{ty.Name}"
                                | _ -> $"?+{ty.Name}"
                            else
                                $"{ty.Namespace}.{ty.Name}"

                        yield name, what
                    | _ -> ()
            ]
            |> List.sortBy fst

        // Compiler-generated closure caches (`<>c`) are the bulk by count and tell a reader
        // nothing, so show the hand-written types, which are what someone picturing "when does
        // this arise?" wants to see.
        let handWritten = named |> List.filter (fun (n, _) -> not (n.Contains "<"))

        printfn ""

        printfn
            "-- types whose initializer can throw: %d in all, %d of them hand-written --"
            (List.length named)
            (List.length handWritten)

        for name, what in handWritten |> List.truncate 16 do
            printfn "  %-56s %s" name what
