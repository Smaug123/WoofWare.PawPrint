namespace WoofWare.PawPrint

/// Where control goes after an instruction, given the stack it leaves.
[<RequireQualifiedAccess>]
type Successors =
    /// Control leaves the method, or the instruction is a rethrow: nothing follows.
    | None
    /// The next instruction in the stream.
    | FallThrough
    /// Branch targets, as byte offsets, plus the next instruction.
    | TargetsAndFallThrough of int list
    /// Branch targets only.
    | Targets of int list
    /// `leave`: the target, with the stack emptied first.
    | Leave of int

[<RequireQualifiedAccess>]
module ControlFlow =

    /// Where control goes after the instruction at `offset`, as byte offsets into the body.
    let successorsOf (offset : int) (instruction : IlOp) : Successors =
        let target (delta : int) : int =
            offset + IlOp.NumberOfBytes instruction + delta

        match instruction with
        | IlOp.Nullary NullaryIlOp.Ret
        | IlOp.Nullary NullaryIlOp.Throw
        | IlOp.Nullary NullaryIlOp.Rethrow
        | IlOp.Nullary NullaryIlOp.Endfinally
        | IlOp.Nullary NullaryIlOp.Endfilter
        | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Jmp, _) -> Successors.None
        // A `br` to the very next instruction is a `nop` to the JIT, which merges the two blocks
        // before importing (`DoEarlyBlockMerging`, Tier-0 included): no block boundary, so a
        // constant of the block survives it.
        | IlOp.UnaryConst (UnaryConstIlOp.Br 0)
        | IlOp.UnaryConst (UnaryConstIlOp.Br_s 0y) -> Successors.FallThrough
        | IlOp.UnaryConst (UnaryConstIlOp.Br delta) -> Successors.Targets [ target delta ]
        | IlOp.UnaryConst (UnaryConstIlOp.Br_s delta) -> Successors.Targets [ target (int delta) ]
        | IlOp.UnaryConst (UnaryConstIlOp.Brfalse delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Brtrue delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Beq delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bne_un delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge_un delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt_un delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble_un delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt_un delta) -> Successors.TargetsAndFallThrough [ target delta ]
        | IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Beq_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bne_un_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge_un_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt_un_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble_un_s delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt_un_s delta) -> Successors.TargetsAndFallThrough [ target (int delta) ]
        | IlOp.UnaryConst (UnaryConstIlOp.Leave delta) -> Successors.Leave (target delta)
        | IlOp.UnaryConst (UnaryConstIlOp.Leave_s delta) -> Successors.Leave (target (int delta))
        | IlOp.Switch targets -> Successors.TargetsAndFallThrough (targets |> Seq.map target |> List.ofSeq)
        | IlOp.Nullary _
        | IlOp.UnaryConst _
        | IlOp.UnaryMetadataToken _
        | IlOp.UnaryStringToken _ -> Successors.FallThrough

    /// A protected block, and the offsets its handler is entered at: the filter's too, for a filter.
    let private entries (region : ExceptionRegion) : ExceptionOffset * int list =
        match region with
        | ExceptionRegion.Filter (filterOffset, o) -> o, [ filterOffset ; o.HandlerOffset ]
        | ExceptionRegion.Catch (_, o)
        | ExceptionRegion.Finally o
        | ExceptionRegion.Fault o -> o, [ o.HandlerOffset ]

    /// Every offset in `body` control can arrive at other than by falling through to it: the
    /// targets of branches, `switch` and `leave`, and the entries of handlers and filters. The
    /// instruction at any other offset is entered only from the one before it.
    let landedOn (body : MethodInstructions<'methodVars>) : Set<int> =
        let fromInstructions =
            body.Instructions
            |> List.collect (fun (instruction, offset) ->
                match successorsOf offset instruction with
                | Successors.None
                | Successors.FallThrough -> []
                | Successors.Targets targets
                | Successors.TargetsAndFallThrough targets -> targets
                | Successors.Leave target -> [ target ]
            )

        let fromHandlers =
            body.ExceptionRegions |> Seq.collect (entries >> snd) |> List.ofSeq

        Set.ofList (fromInstructions @ fromHandlers)

    /// <summary>
    /// The offsets a run of <c>body</c> may execute: those control reaches from the entry by
    /// fall-through, branches and <c>leave</c>, and the entry of every handler (and filter) whose
    /// protected block holds one of them.
    /// </summary>
    /// <remarks>
    /// <c>constants</c> maps the offset of an instruction that falls through to the Boolean it is
    /// known to push. A <c>brtrue</c> or <c>brfalse</c> straight after it, which no branch or
    /// handler entry also lands on, pops that value, so only the successor the value chooses is
    /// followed. Every other conditional branch keeps both successors.
    ///
    /// Unlike <c>StackShape.reachable</c>, which is the graph CoreCLR's importer builds and holds
    /// every handler, this is what a run can execute; it over-approximates that, and never omits
    /// an offset a run executes.
    /// </remarks>
    let mayExecute (body : MethodInstructions<'methodVars>) (constants : Map<int, bool>) : Set<int> =
        let landedOn = landedOn body

        // The value on top of the stack on entry to each offset only the instruction before it
        // leads to, where that instruction pushed a known Boolean.
        let known : Map<int, bool> =
            constants
            |> Map.toSeq
            |> Seq.choose (fun (offset, value) ->
                let instruction =
                    match body.Locations.TryFind offset with
                    | Some instruction -> instruction
                    | None -> invalidArg (nameof constants) $"No instruction starts at offset %d{offset}"

                match successorsOf offset instruction with
                | Successors.FallThrough -> ()
                | _ ->
                    invalidArg
                        (nameof constants)
                        $"The instruction at offset %d{offset}, %O{instruction}, does not fall through"

                let next = offset + IlOp.NumberOfBytes instruction

                if landedOn.Contains next then None else Some (next, value)
            )
            |> Map.ofSeq

        let successors (offset : int) (instruction : IlOp) : int list =
            let fallThrough = offset + IlOp.NumberOfBytes instruction

            let chosen (jumpWhen : bool) (target : int) : int list =
                match Map.tryFind offset known with
                | Some value when value = jumpWhen -> [ target ]
                | Some _ -> [ fallThrough ]
                | None -> [ fallThrough ; target ]

            match instruction, successorsOf offset instruction with
            | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _ | UnaryConstIlOp.Brtrue_s _),
              Successors.TargetsAndFallThrough [ target ] -> chosen true target
            | IlOp.UnaryConst (UnaryConstIlOp.Brfalse _ | UnaryConstIlOp.Brfalse_s _),
              Successors.TargetsAndFallThrough [ target ] -> chosen false target
            | _, Successors.None -> []
            | _, Successors.FallThrough -> [ fallThrough ]
            | _, Successors.Targets targets -> targets
            | _, Successors.TargetsAndFallThrough targets -> fallThrough :: targets
            | _, Successors.Leave target -> [ target ]

        let seen = System.Collections.Generic.HashSet<int> ()
        let worklist = System.Collections.Generic.Queue<int> ()

        let visit (offset : int) : unit =
            if body.Locations.ContainsKey offset && seen.Add offset then
                worklist.Enqueue offset

        visit 0

        while worklist.Count > 0 do
            while worklist.Count > 0 do
                let offset = worklist.Dequeue ()

                for next in successors offset body.Locations.[offset] do
                    visit next

            // A handler runs only when something in its protected block has; entering one can
            // reach another protected block, so repeat until nothing new is entered.
            for region in body.ExceptionRegions do
                let offsets, handlerEntries = entries region

                if
                    seen
                    |> Seq.exists (fun offset ->
                        offsets.TryOffset <= offset && offset < offsets.TryOffset + offsets.TryLength
                    )
                then
                    List.iter visit handlerEntries


        Set.ofSeq seen
