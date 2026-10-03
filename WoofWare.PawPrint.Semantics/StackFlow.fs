namespace WoofWare.PawPrint

open System.Collections.Immutable

/// What a dataflow over the evaluation stack tracks about each slot, and how: the values its
/// slots hold, which every instruction's `Pushed` values become, and how two paths' values meet
/// at a join.
type SlotLattice<'slot> =
    {
        /// How the instruction at an offset computes one value it pushes, `Pushed` saying what
        /// that value is, from the values it popped, top first; or why this lattice cannot say,
        /// which makes the instruction invalid.
        Push : int -> IlOp -> Pushed -> Result<'slot list -> 'slot, StackShapeError>
        /// The value a slot holds where two paths' values meet: at the offset given, in the slot
        /// given counted from the top. An error makes the join unknown, and what follows only
        /// from it.
        Join : int -> int -> 'slot -> 'slot -> Result<'slot, StackShapeError>
        /// The exception a `catch` handler, or a filter and its handler, starts with on the stack.
        Caught : ExceptionRegion -> 'slot
        /// Whether CoreCLR converts a value that a join widens, so that the conversion is part of
        /// what the body computes; a float32 meeting a double is the one such join. A branch
        /// the JIT may fold can remove the path that widens it, so such a join, where such a
        /// branch reaches it, is refused (`StackShapeError.WidthDependsOnFoldedBranch`). A join
        /// that only coarsens the lattice's own account of the values needs no refusal.
        WideningConverts : bool
    }

/// The evaluation stack at the entry of every reachable instruction of a body, as a slot lattice
/// describes it.
type StackFlow<'slot> =
    {
        /// The stack on entry to each reachable offset the analysis could type, top first.
        Entry : Map<int, 'slot list>
        /// The reachable offsets the analysis could not type, and why. An instruction that
        /// underflows, or a join two paths reach with stacks that cannot meet, is recorded here
        /// rather than failing the whole body: CoreCLR's importer refuses it only if it imports
        /// it. What follows only from such an offset, joins it feeds included, is in `Reachable`
        /// but in neither `Entry` nor here.
        Invalid : Map<int, StackShapeError>
        /// Every offset control can reach in the body's graph, typed or not; see
        /// `StackFlow.reachable`.
        Reachable : Set<int>
        /// The offsets at which a value arriving in the listed slots (counted from the top of the
        /// stack, `0` being the top) along some path differs from the slot's value over every
        /// path: where the join widened it.
        Widened : Map<int, int list>
        /// The offsets at which CoreCLR's importer starts a basic block: the entry, every branch
        /// target, every handler entry, and the instruction after a conditional branch or
        /// `switch`. A value on the stack on entry to one arrives through a spill temp, whose
        /// type the importer decides over every path into the temp's clique.
        BlockStarts : Set<int>
    }

[<RequireQualifiedAccess>]
module StackFlow =

    /// What the instruction at `offset` pops, and how it computes each value it pushes, top
    /// first, from what it popped.
    let private stepOf
        (lattice : SlotLattice<'slot>)
        (inputs : StackEffectInputs)
        (locations : Map<int, IlOp>)
        (offset : int)
        (instruction : IlOp)
        : Result<int * ('slot list -> 'slot) list, StackShapeError>
        =
        match StackEffect.ofInstruction inputs locations offset instruction with
        | Error e -> Error e
        | Ok effect ->
            let rec computes (pushed : Pushed list) (acc : ('slot list -> 'slot) list) =
                match pushed with
                | [] -> Ok (List.rev acc)
                | p :: rest ->
                    match lattice.Push offset instruction p with
                    | Error e -> Error e
                    | Ok compute -> computes rest (compute :: acc)

            computes effect.Pushes [] |> Result.map (fun pushes -> effect.Pops, pushes)

    /// The entry stacks CoreCLR gives handler code, which no instruction jumps to. A catch or
    /// filter handler starts with the exception on the stack; so does the filter's own code. A
    /// finally or fault handler starts empty.
    let private handlerEntries
        (lattice : SlotLattice<'slot>)
        (regions : ImmutableArray<ExceptionRegion>)
        : (int * 'slot list) list
        =
        regions
        |> Seq.collect (fun region ->
            match region with
            | ExceptionRegion.Catch (_, offsets) -> [ offsets.HandlerOffset, [ lattice.Caught region ] ]
            | ExceptionRegion.Filter (filterOffset, offsets) ->
                [
                    filterOffset, [ lattice.Caught region ]
                    offsets.HandlerOffset, [ lattice.Caught region ]
                ]
            | ExceptionRegion.Finally offsets
            | ExceptionRegion.Fault offsets -> [ offsets.HandlerOffset, [] ]
        )
        |> List.ofSeq

    /// A slot of the stack on entry to an instruction, or the anchor of a component of the
    /// flow graph, which every member's slot is unioned with so that the whole component shares
    /// one temp per slot.
    [<RequireQualifiedAccess>]
    type private SlotKey =
        | Slot of offset : int * slotFromTop : int
        | Anchor of cluster : int * slotFromTop : int

    /// Union-find over the slots that share one of CoreCLR's spill temps. The importer gives
    /// every block reachable through the predecessor/successor relation the same temp base
    /// (`impWalkSpillCliqueFromPred`), so a slot's type is joined over the whole clique, not
    /// over one block's incoming edges: a predecessor delivering a float32 to two successors,
    /// one of which another predecessor reaches with a double, sees both successors typed
    /// double.
    type private Cliques<'slot when 'slot : equality> (lattice : SlotLattice<'slot>) =
        let parent = System.Collections.Generic.Dictionary<SlotKey, SlotKey> ()
        let shape = System.Collections.Generic.Dictionary<SlotKey, 'slot> ()
        let members = System.Collections.Generic.Dictionary<SlotKey, Set<int>> ()

        member this.Find (key : SlotKey) : SlotKey =
            match parent.TryGetValue key with
            | false, _ ->
                parent.[key] <- key

                members.[key] <-
                    match key with
                    | SlotKey.Slot (offset, _) -> Set.singleton offset
                    | SlotKey.Anchor _ -> Set.empty

                key
            | true, p when p = key -> key
            | true, p ->
                let root = this.Find p
                parent.[key] <- root
                root

        member this.Shape (key : SlotKey) : 'slot option =
            match shape.TryGetValue (this.Find key) with
            | true, s -> Some s
            | false, _ -> None

        /// The offsets whose slot is in this clique.
        member this.Members (key : SlotKey) : Set<int> = members.[this.Find key]

        /// Merge two cliques. Returns the offsets whose entry shape may have changed, with the
        /// error if their shapes cannot meet.
        member this.Union (offset : int) (slot : int) (a : SlotKey) (b : SlotKey) : Result<Set<int>, StackShapeError> =
            let ra = this.Find a
            let rb = this.Find b

            if ra = rb then
                Ok Set.empty
            else
                let sa =
                    shape.TryGetValue ra
                    |> function
                        | true, s -> Some s
                        | false, _ -> None

                let sb =
                    shape.TryGetValue rb
                    |> function
                        | true, s -> Some s
                        | false, _ -> None

                let joined =
                    match sa, sb with
                    | None, s
                    | s, None -> Ok s
                    | Some sa, Some sb -> lattice.Join offset slot sa sb |> Result.map Some

                match joined with
                | Error e -> Error e
                | Ok joined ->
                    let membersA = members.[ra]
                    let membersB = members.[rb]
                    parent.[rb] <- ra
                    members.[ra] <- Set.union membersA membersB
                    members.Remove rb |> ignore

                    match joined with
                    | None -> Ok Set.empty
                    | Some joined ->
                        shape.[ra] <- joined
                        // A member of either clique whose shape moved needs its transfer redone.
                        let changedA = if sa <> Some joined then membersA else Set.empty
                        let changedB = if sb <> Some joined then membersB else Set.empty
                        Ok (Set.union changedA changedB)

        /// Join a delivered shape into the clique. Returns the offsets whose entry shape changed.
        member this.Deliver
            (offset : int)
            (slot : int)
            (key : SlotKey)
            (delivered : 'slot)
            : Result<Set<int>, StackShapeError>
            =
            let root = this.Find key

            match shape.TryGetValue root with
            | false, _ ->
                shape.[root] <- delivered
                Ok members.[root]
            | true, existing ->
                match lattice.Join offset slot existing delivered with
                | Error e -> Error e
                | Ok joined ->
                    if joined = existing then
                        Ok Set.empty
                    else
                        shape.[root] <- joined
                        Ok members.[root]

    /// Where control goes after the instruction at `offset`, as byte offsets into the body.
    let private targetsOf (offset : int) (instruction : IlOp) : int list =
        let fallThrough = offset + IlOp.NumberOfBytes instruction

        match ControlFlow.successorsOf offset instruction with
        | Successors.None -> []
        | Successors.FallThrough -> [ fallThrough ]
        | Successors.Targets targets -> targets
        | Successors.TargetsAndFallThrough targets -> fallThrough :: targets
        | Successors.Leave target -> [ target ]

    /// The successors of every instruction in the body's flow graph, including those of an
    /// instruction control never reaches, which CoreCLR's importer still puts in its graph.
    let private flowGraphOf (body : MethodInstructions<'methodVars>) : Map<int, int list> =
        body.Instructions
        |> List.map (fun (instruction, offset) -> offset, targetsOf offset instruction)
        |> Map.ofList

    /// The offsets at which the importer starts a basic block: the entry, every branch target,
    /// every handler entry, and the instruction after a conditional branch or `switch`. A value
    /// arriving at one of these is a spill temp to the importer, not a constant.
    let private leadersOf (body : MethodInstructions<'methodVars>) : Set<int> =
        let fromInstructions =
            body.Instructions
            |> List.collect (fun (instruction, offset) ->
                match ControlFlow.successorsOf offset instruction with
                | Successors.None
                | Successors.FallThrough -> []
                | Successors.Targets targets -> targets
                | Successors.TargetsAndFallThrough targets -> (offset + IlOp.NumberOfBytes instruction) :: targets
                | Successors.Leave target -> [ target ]
            )

        let handlerOffsets =
            body.ExceptionRegions
            |> Seq.collect (fun region ->
                match region with
                | ExceptionRegion.Catch (_, offsets)
                | ExceptionRegion.Finally offsets
                | ExceptionRegion.Fault offsets -> [ offsets.HandlerOffset ]
                | ExceptionRegion.Filter (filterOffset, offsets) -> [ filterOffset ; offsets.HandlerOffset ]
            )
            |> List.ofSeq

        Set.ofList (0 :: fromInstructions @ handlerOffsets)

    /// How many operands a conditional branch or `switch` reads from the stack to choose its
    /// successor: none for any other instruction.
    let private conditionOperands (instruction : IlOp) : int =
        match instruction with
        | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _)
        | IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Brfalse _)
        | IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s _)
        | IlOp.Switch _ -> 1
        | IlOp.UnaryConst (UnaryConstIlOp.Beq _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bne_un _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge _)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt_un _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge_un _)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt_un _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble_un _)
        | IlOp.UnaryConst (UnaryConstIlOp.Beq_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bne_un_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bgt_un_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Bge_un_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Blt_un_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ble_un_s _) -> 2
        | _ -> 0

    /// Whether the instruction pushes a literal, which the importer holds as a constant it may
    /// fold.
    let private pushesLiteral (instruction : IlOp) : bool =
        match instruction with
        | IlOp.Nullary NullaryIlOp.LdcI4_0
        | IlOp.Nullary NullaryIlOp.LdcI4_1
        | IlOp.Nullary NullaryIlOp.LdcI4_2
        | IlOp.Nullary NullaryIlOp.LdcI4_3
        | IlOp.Nullary NullaryIlOp.LdcI4_4
        | IlOp.Nullary NullaryIlOp.LdcI4_5
        | IlOp.Nullary NullaryIlOp.LdcI4_6
        | IlOp.Nullary NullaryIlOp.LdcI4_7
        | IlOp.Nullary NullaryIlOp.LdcI4_8
        | IlOp.Nullary NullaryIlOp.LdcI4_m1
        | IlOp.Nullary NullaryIlOp.LdNull
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4_s _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_I8 _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 _)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 _)
        | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, _)
        | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Sizeof, _) -> true
        | _ -> false

    /// The index of the argument an instruction loads, if it is an `ldarg`.
    let private loadedArgument (instruction : IlOp) : int option =
        match instruction with
        | IlOp.Nullary NullaryIlOp.LdArg0 -> Some 0
        | IlOp.Nullary NullaryIlOp.LdArg1 -> Some 1
        | IlOp.Nullary NullaryIlOp.LdArg2 -> Some 2
        | IlOp.Nullary NullaryIlOp.LdArg3 -> Some 3
        | IlOp.UnaryConst (UnaryConstIlOp.Ldarg_s i) -> Some (int i)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldarg i) -> Some (int i)
        | _ -> None

    /// The arguments the body stores to or takes the address of, anywhere in its IL. The JIT
    /// substitutes a constant a caller passes into an inlinee only for an argument that is
    /// neither (`impInlineFetchArg`, whose `argHasStargOp` and `argHasLdargaOp` come from a scan
    /// of the whole body).
    let private modifiableArguments (body : MethodInstructions<'methodVars>) : Set<int> =
        body.Instructions
        |> List.choose (fun (instruction, _) ->
            match instruction with
            | IlOp.UnaryConst (UnaryConstIlOp.Starg_s i)
            | IlOp.UnaryConst (UnaryConstIlOp.Ldarga_s i) -> Some (int i)
            | IlOp.UnaryConst (UnaryConstIlOp.Starg i)
            | IlOp.UnaryConst (UnaryConstIlOp.Ldarga i) -> Some (int i)
            | _ -> None
        )
        |> Set.ofList

    /// The conditional branches and `switch`es whose every operand their basic block computes
    /// from values the importer may hold as constants: a literal, a static field (the importer
    /// reads an initialised `static readonly` one), an argument the body never stores to or
    /// takes the address of (the constant a caller passes, when the JIT inlines the body there),
    /// the result of a call on such values or on none (an intrinsic such as `IsSupported`,
    /// `Type.op_Equality` on two `typeof`s), or the result of an operation without a token on
    /// such values. The JIT may fold such a branch
    /// (`gtFoldExpr`), importing only the arm taken, and does so at every tier but not in
    /// debuggable code. A value arriving at a block's first instruction is a spill temp to the
    /// importer, and no constant; a `br` to the very next instruction starts no block.
    let private foldableBranchesOf
        (lattice : SlotLattice<'slot>)
        (inputs : StackEffectInputs)
        (body : MethodInstructions<'methodVars>)
        : Set<int>
        =
        let leaders = leadersOf body
        let locations = body.Locations
        let modifiable = modifiableArguments body

        // Whether each slot the block itself pushed, top first, may be a constant to the importer;
        // the block's entry stack lies below these and is none.
        let _, _, found =
            ((([] : bool list), false, Set.empty), body.Instructions)
            ||> List.fold (fun (stack, fallsIn, found) (instruction, offset) ->
                let stack =
                    if fallsIn && not (leaders.Contains offset) then
                        stack
                    else
                        []

                let constantOperands (count : int) : bool =
                    stack.Length >= count && stack |> List.take count |> List.forall id

                let found =
                    let operands = conditionOperands instruction

                    if operands > 0 && constantOperands operands then
                        Set.add offset found
                    else
                        found

                let stack =
                    match stepOf lattice inputs locations offset instruction with
                    | Error _ -> []
                    | Ok (pops, pushes) ->
                        let pushed = pushes.Length

                        let constant =
                            pushesLiteral instruction
                            || (
                                match loadedArgument instruction with
                                | Some index -> not (modifiable.Contains index)
                                | None -> false
                            )
                            || (
                                match instruction with
                                | IlOp.Nullary _ -> pops > 0 && constantOperands pops
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, _)
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _) -> constantOperands pops
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, _) -> true
                                | _ -> false
                            )

                        let rest = if stack.Length >= pops then List.skip pops stack else []

                        List.replicate pushed constant @ rest

                let fallsOut =
                    match ControlFlow.successorsOf offset instruction with
                    | Successors.FallThrough
                    | Successors.TargetsAndFallThrough _ -> true
                    | Successors.None
                    | Successors.Targets _
                    | Successors.Leave _ -> false

                stack, fallsOut, found
            )

        found

    /// The component of the flow graph each offset belongs to, where two offsets are in one
    /// component when they are successors of the same instruction, transitively: the blocks
    /// `impWalkSpillCliqueFromPred` walks into one spill clique, through instructions the
    /// importer never imports as much as through those it does. A `leave` target is a
    /// successor on its own, and a handler entry is nobody's successor.
    let private componentsOf (successors : Map<int, int list>) : Map<int, int> =
        let parent = System.Collections.Generic.Dictionary<int, int> ()

        let rec find (offset : int) : int =
            match parent.TryGetValue offset with
            | false, _ ->
                parent.[offset] <- offset
                offset
            | true, p when p = offset -> offset
            | true, p ->
                let root = find p
                parent.[offset] <- root
                root

        for KeyValue (_, targets) in successors do
            match List.distinct targets with
            | first :: rest ->
                for target in rest do
                    let ra = find first
                    let rb = find target

                    if ra <> rb then
                        parent.[rb] <- ra
            | [] -> ()

        parent.Keys |> Seq.map (fun offset -> offset, find offset) |> Map.ofSeq

    /// The offsets control can reach from the method's entry or a handler entry, following
    /// branches, fall-through and `leave` but not exceptions. A token on an instruction outside
    /// this set need never be read: CoreCLR's importer does not import such code either.
    let reachable (body : MethodInstructions<'methodVars>) : Set<int> =
        let seen = System.Collections.Generic.HashSet<int> ()
        let worklist = System.Collections.Generic.Queue<int> ()

        let visit (offset : int) : unit =
            if body.Locations.ContainsKey offset && seen.Add offset then
                worklist.Enqueue offset

        visit 0

        for region in body.ExceptionRegions do
            match region with
            | ExceptionRegion.Catch (_, offsets)
            | ExceptionRegion.Finally offsets
            | ExceptionRegion.Fault offsets -> visit offsets.HandlerOffset
            | ExceptionRegion.Filter (filterOffset, offsets) ->
                visit filterOffset
                visit offsets.HandlerOffset

        while worklist.Count > 0 do
            let offset = worklist.Dequeue ()
            let instruction = body.Locations.[offset]
            let fallThrough = offset + IlOp.NumberOfBytes instruction

            match ControlFlow.successorsOf offset instruction with
            | Successors.None -> ()
            | Successors.FallThrough -> visit fallThrough
            | Successors.Targets targets -> List.iter visit targets
            | Successors.TargetsAndFallThrough targets ->
                visit fallThrough
                List.iter visit targets
            | Successors.Leave target -> visit target

        Set.ofSeq seen

    /// What the analysis knows about the stack on entry to an offset while the fixpoint runs.
    [<RequireQualifiedAccess>]
    type private Entry =
        /// Every path seen so far arrives at this depth; the slots' values live in the cliques.
        | Known of int
        /// Some path arrives with a stack the analysis cannot state: through a join two paths
        /// disagree at, through a join it refuses to decide, through an instruction it could not
        /// type, or through a spill temp such a join shares. Nothing past here is typed.
        | Unknown

    /// What an instruction delivers to one successor.
    [<RequireQualifiedAccess>]
    type private Delivery<'slot> =
        /// The stack, top first.
        | Known of 'slot list
        | Unknown

    /// The offsets whose shape can depend on which way the JIT takes the branch whose
    /// successors are `targets`: those successors, closed under succession in `graph` and under
    /// sharing a component of the flow graph, whose spill temps a changed delivery retypes.
    let private dependentOn
        (graph : Map<int, int list>)
        (componentOf : int -> int)
        (membersOf : Map<int, int list>)
        (targets : int list)
        : Set<int>
        =
        let seen = System.Collections.Generic.HashSet<int> ()
        let worklist = System.Collections.Generic.Queue<int> ()

        let visit (offset : int) : unit =
            if graph.ContainsKey offset && seen.Add offset then
                worklist.Enqueue offset

        List.iter visit targets

        while worklist.Count > 0 do
            let offset = worklist.Dequeue ()
            List.iter visit graph.[offset]

            Map.tryFind (componentOf offset) membersOf
            |> Option.defaultValue []
            |> List.iter visit

        Set.ofSeq seen

    /// Compute the stack at the entry of every instruction reachable from the method's entry or
    /// from a handler entry, joining over every path in `lattice`. An instruction that cannot be
    /// typed is recorded in `Invalid` and delivers nothing. A join that two paths reach with
    /// stacks that cannot meet is recorded as a conflict and delivers an unknown stack, which
    /// propagates: what follows only from the join is left untyped, a join it feeds is untyped
    /// too rather than classified from its other arms, and so is every offset sharing one of its
    /// spill temps. Where `lattice.WideningConverts`, a join that widens a value, but which a
    /// branch the importer may fold reaches, is recorded as `WidthDependsOnFoldedBranch` and
    /// propagates as a conflict does.
    let analyse
        (lattice : SlotLattice<'slot>)
        (inputs : StackEffectInputs)
        (body : MethodInstructions<'methodVars>)
        : StackFlow<'slot>
        =
        let locations = body.Locations
        let graph = flowGraphOf body
        let components = componentsOf graph
        let entry = System.Collections.Generic.Dictionary<int, Entry> ()

        let invalid = System.Collections.Generic.Dictionary<int, StackShapeError> ()
        let cliques = Cliques<'slot> lattice

        let componentOf (offset : int) : int =
            Map.tryFind offset components |> Option.defaultValue offset

        let membersOf =
            components
            |> Map.toList
            |> List.groupBy snd
            |> List.map (fun (root, pairs) -> root, pairs |> List.map fst)
            |> Map.ofList

        // A component one member of which is unknown: its temps are undecidable, so every member
        // is unknown, those reached later included.
        let poisoned = System.Collections.Generic.HashSet<int> ()
        let worklist = System.Collections.Generic.Queue<int> ()
        let queued = System.Collections.Generic.HashSet<int> ()

        let enqueue (offset : int) : unit =
            if queued.Add offset then
                worklist.Enqueue offset

        let knownDepth (offset : int) : int option =
            match entry.TryGetValue offset with
            | true, Entry.Known depth -> Some depth
            | _ -> None

        /// `offset` is unknown from here on, `reason` recorded against it if it is a conflict of
        /// its own; whatever its instruction found with the stack it had is no longer a claim
        /// about every path. Its whole component of the flow graph shares its spill temps, which
        /// are now undecidable, so every member is unknown too, whatever depth any of them had.
        let rec markUnknown (offset : int) (reason : StackShapeError option) : unit =
            match entry.TryGetValue offset with
            | true, Entry.Unknown -> ()
            | _ ->
                entry.[offset] <- Entry.Unknown
                invalid.Remove offset |> ignore

                match reason with
                | Some reason -> invalid.[offset] <- reason
                | None -> ()

                enqueue offset

                let root = componentOf offset

                if poisoned.Add root then
                    for shared in Map.tryFind root membersOf |> Option.defaultValue [] do
                        if shared <> offset && entry.ContainsKey shared then
                            markUnknown shared None

        let entryShapes (offset : int) (depth : int) : 'slot list =
            List.init
                depth
                (fun k ->
                    match cliques.Shape (SlotKey.Slot (offset, k)) with
                    | Some s -> s
                    | None ->
                        failwith $"BUG: stack flow: slot %d{k} at offset %d{offset} has a depth but no clique value"
                )

        /// Where `offset` sends control, each successor arriving with `delivery` (a `leave`
        /// empties the stack).
        let successors (offset : int) (delivery : Delivery<'slot>) : (int * Delivery<'slot>) list =
            match ControlFlow.successorsOf offset locations.[offset] with
            | Successors.Leave target -> [ target, Delivery.Known [] ]
            | _ -> graph.[offset] |> List.map (fun target -> target, delivery)

        /// Where `offset` sends control, and what it delivers there: the stack its instruction
        /// leaves.
        let deliveries (offset : int) : Result<(int * Delivery<'slot>) list, StackShapeError> =
            let instruction = locations.[offset]

            match entry.[offset] with
            | Entry.Unknown -> Ok (successors offset Delivery.Unknown)
            | Entry.Known depth ->

            let stack = entryShapes offset depth

            match stepOf lattice inputs locations offset instruction with
            | Error e -> Error e
            | Ok (pops, pushes) ->

            if depth < pops then
                Error (StackShapeError.StackUnderflow (offset, instruction, depth))
            else

            let popped, rest = List.splitAt pops stack
            let pushed = pushes |> List.map (fun compute -> compute popped)
            let targets = successors offset (Delivery.Known (pushed @ rest))

            match targets |> List.tryFind (fun (target, _) -> not (locations.ContainsKey target)) with
            | Some (target, _) -> Error (StackShapeError.BranchOutsideBody (offset, target))
            | None -> Ok targets

        /// Deliver one instruction's out-stack to all its successors. A successor reached with a
        /// known stack joins the delivered value into each slot's clique, which it shares with
        /// its whole component of the flow graph; a successor whose depth or slots cannot meet
        /// the delivery becomes unknown, as does one delivered an unknown stack or one whose
        /// temps are already undecidable.
        let deliverAll (targets : (int * Delivery<'slot>) list) : unit =
            let changed = System.Collections.Generic.HashSet<int> ()

            let stillKnown (offset : int) : bool = (knownDepth offset).IsSome

            let arrivals =
                targets
                |> List.choose (fun (target, delivery) ->
                    if not (locations.ContainsKey target) then
                        None
                    else
                        match delivery with
                        | Delivery.Unknown ->
                            markUnknown target None
                            None
                        | Delivery.Known stack ->
                            match entry.TryGetValue target with
                            | true, Entry.Unknown -> None
                            | true, Entry.Known existing when existing <> stack.Length ->
                                markUnknown
                                    target
                                    (Some (StackShapeError.DepthMismatch (target, existing, stack.Length)))

                                None
                            | true, Entry.Known _ -> Some (target, stack)
                            | false, _ ->
                                entry.[target] <- Entry.Known stack.Length
                                enqueue target

                                // Every slot shares its component's temp for that slot; an offset
                                // in no sibling group is a component of its own.
                                let cluster = componentOf target

                                if poisoned.Contains cluster then
                                    markUnknown target None

                                for k in 0 .. stack.Length - 1 do
                                    if stillKnown target then
                                        let slot = SlotKey.Slot (target, k)
                                        let anchor = SlotKey.Anchor (cluster, k)

                                        match cliques.Union target k anchor slot with
                                        | Ok moved -> changed.UnionWith moved
                                        | Error e -> markUnknown target (Some e)

                                if stillKnown target then Some (target, stack) else None
                )

            for target, stack in arrivals do
                stack
                |> List.iteri (fun k value ->
                    if stillKnown target then
                        match cliques.Deliver target k (SlotKey.Slot (target, k)) value with
                        | Ok moved -> changed.UnionWith moved
                        | Error e -> markUnknown target (Some e)
                )

            for offset in changed do
                enqueue offset

        // An offset is queued when first reached, when a clique one of its slots belongs to
        // changes value, and when it becomes unknown. An instruction found invalid on its own
        // delivers nothing; a call whose token could not be read is no claim about the IL, so
        // what follows it is unknown rather than unreached.
        let drain () : unit =
            while worklist.Count > 0 do
                let offset = worklist.Dequeue ()
                queued.Remove offset |> ignore

                match deliveries offset with
                | Error (StackShapeError.MissingTokenShape _ as e) ->
                    invalid.[offset] <- e
                    deliverAll (successors offset Delivery.Unknown)
                | Error e -> invalid.[offset] <- e
                | Ok targets -> deliverAll targets

        let typed () : Map<int, 'slot list> =
            entry
            |> Seq.choose (fun kv ->
                match kv.Value with
                | Entry.Known depth when not (invalid.ContainsKey kv.Key) -> Some (kv.Key, entryShapes kv.Key depth)
                | _ -> None
            )
            |> Map.ofSeq

        // Widenings come from the settled states alone. During the fixpoint an offset can be
        // delivered a value by a predecessor whose own entry was later widened; CoreCLR
        // re-imports such a predecessor with the widened temp, so it delivers the widened value
        // in the end and inserts no conversion. Only an edge that *still* delivers a value
        // other than the settled one is widened.
        let widenedOf (typed : Map<int, 'slot list>) : Map<int, int list> =
            typed
            |> Map.toList
            |> List.collect (fun (offset, _) ->
                match deliveries offset with
                | Error _ -> []
                | Ok targets ->
                    targets
                    |> List.collect (fun (target, delivery) ->
                        match delivery, Map.tryFind target typed with
                        | Delivery.Known delivered, Some settled ->
                            List.zip delivered settled
                            |> List.indexed
                            |> List.choose (fun (i, (d, s)) -> if d <> s then Some (target, i) else None)
                        | _ -> []
                    )
            )
            |> List.distinct
            |> List.groupBy fst
            |> List.map (fun (target, slots) -> target, slots |> List.map snd |> List.sort)
            |> Map.ofList

        for offset, stack in (0, []) :: handlerEntries lattice body.ExceptionRegions do
            deliverAll [ offset, Delivery.Known stack ]

        drain ()

        let reachableOffsets = reachable body

        // The analysis types the flow graph with every arm imported, as debuggable code imports
        // it. Folding a branch only removes edges and the deliveries of code reached only through
        // them, so a join keeps its value in every compilation unless it widens a value: then
        // the wider value may arrive only through a folded-away arm. Where the widening is a
        // conversion CoreCLR inserts, such a join, where a branch the importer may fold can
        // change what reaches its spill temps, is refused, and what follows only from it is
        // unknown.
        if lattice.WideningConverts then
            let dependent : Map<int, int> =
                foldableBranchesOf lattice inputs body
                |> Set.intersect reachableOffsets
                |> Seq.fold
                    (fun (acc : Map<int, int>) (branch : int) ->
                        dependentOn graph componentOf membersOf graph.[branch]
                        |> Seq.fold
                            (fun (acc : Map<int, int>) (offset : int) ->
                                if acc.ContainsKey offset then
                                    acc
                                else
                                    Map.add offset branch acc
                            )
                            acc
                    )
                    Map.empty

            let refused =
                widenedOf (typed ())
                |> Map.toList
                |> List.choose (fun (offset, _) ->
                    Map.tryFind offset dependent |> Option.map (fun branch -> offset, branch)
                )

            for offset, _ in refused do
                markUnknown offset None

            drain ()

            for offset, branch in refused do
                invalid.[offset] <- StackShapeError.WidthDependsOnFoldedBranch (offset, branch)

        let typed = typed ()

        {
            Entry = typed
            Invalid = invalid |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            Reachable = reachableOffsets
            Widened = widenedOf typed
            BlockStarts = leadersOf body
        }
