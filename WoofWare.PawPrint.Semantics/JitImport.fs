namespace WoofWare.PawPrint

/// What PawPrint knows of a body that CoreCLR's JIT optimises, which decides branches it folds.
type OptimisedBody =
    {
        /// Each `call` whose callee the JIT expands to a constant Boolean on the CPU it compiles
        /// for (a capability query), by its offset, with that constant.
        Capabilities : Map<int, bool>
        /// The `call`s and `callvirt`s, by offset, whose callee the JIT never expands as an
        /// intrinsic, so that what they return is never a constant while it imports the body.
        OrdinaryCalls : Set<int>
    }

/// Which of a body's conditional branches CoreCLR's JIT may fold when it compiles the body.
[<RequireQualifiedAccess>]
type JitFolding =
    /// None: it does not optimise the body. A method marked `NoOptimization`, or in an assembly
    /// whose `DebuggableAttribute` disables optimisation, is compiled so.
    | Nothing
    /// It optimises the body, and folds any conditional branch whose condition it reduces to a
    /// constant, never importing the successor the constant rules out.
    | Optimises of OptimisedBody

/// Bounds on the instructions CoreCLR's JIT imports when it compiles a body, by offset.
type CompiledCode =
    {
        /// Instructions it imports, whatever else it folds. Holds none it does not import.
        Surely : Set<int>
        /// Instructions it may import. Omits none it imports.
        Maybe : Set<int>
    }

/// The order in which CoreCLR's JIT imports a body's instructions, as far as it does not depend on
/// the importer's worklist.
type ImportOrder =
    {
        /// Each instruction it may import other than the entry, by offset, with the last one it
        /// surely imports before it whenever it imports it: its immediate dominator in the flow
        /// graph of the code it may import.
        ImportedJustBefore : Map<int, int>
    }

/// What CoreCLR's JIT compiles of a body, and in what order, as far as PawPrint can state it
/// without modelling the importer's folding.
///
/// The importer (`importer.cpp`) walks a body's blocks from the entry, importing a block only when
/// one it has imported leads there, and the handlers of a protected block once it begins importing a
/// block inside it (`impVerifyEHBlock`). At a conditional branch whose condition `gtFoldExpr`
/// reduces to a constant it keeps only the successor the constant chooses (`COND_JUMP`). That
/// happens only where it optimises the body: `gtFoldExpr` folds nothing unless
/// `Tier0OptimizationEnabled()`.
[<RequireQualifiedAccess>]
module JitImport =

    /// The argument or local an instruction loads, as `(isArgument, index)`.
    let private slotLoaded (instruction : IlOp) : (bool * int) option =
        match instruction with
        | IlOp.Nullary NullaryIlOp.LdArg0 -> Some (true, 0)
        | IlOp.Nullary NullaryIlOp.LdArg1 -> Some (true, 1)
        | IlOp.Nullary NullaryIlOp.LdArg2 -> Some (true, 2)
        | IlOp.Nullary NullaryIlOp.LdArg3 -> Some (true, 3)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldarg_s index) -> Some (true, int index)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldarg index) -> Some (true, int index)
        | IlOp.Nullary NullaryIlOp.Ldloc_0 -> Some (false, 0)
        | IlOp.Nullary NullaryIlOp.Ldloc_1 -> Some (false, 1)
        | IlOp.Nullary NullaryIlOp.Ldloc_2 -> Some (false, 2)
        | IlOp.Nullary NullaryIlOp.Ldloc_3 -> Some (false, 3)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldloc_s index) -> Some (false, int index)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldloc index) -> Some (false, int index)
        | _ -> None

    /// Whether the conditional branch `instruction` pops one value, rather than comparing two.
    [<RequireQualifiedAccess>]
    type private Condition =
        /// `brtrue`, `brfalse` or `switch`.
        | OneValue
        /// A comparison of two values.
        | TwoValues
        /// Not a conditional branch.
        | NotConditional

    let private conditionOf (instruction : IlOp) : Condition =
        match instruction with
        | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _ | UnaryConstIlOp.Brtrue_s _ | UnaryConstIlOp.Brfalse _ | UnaryConstIlOp.Brfalse_s _)
        | IlOp.Switch _ -> Condition.OneValue
        | IlOp.UnaryConst (UnaryConstIlOp.Beq _ | UnaryConstIlOp.Beq_s _ | UnaryConstIlOp.Bne_un _ | UnaryConstIlOp.Bne_un_s _ | UnaryConstIlOp.Blt _ | UnaryConstIlOp.Blt_s _ | UnaryConstIlOp.Blt_un _ | UnaryConstIlOp.Blt_un_s _ | UnaryConstIlOp.Ble _ | UnaryConstIlOp.Ble_s _ | UnaryConstIlOp.Ble_un _ | UnaryConstIlOp.Ble_un_s _ | UnaryConstIlOp.Bgt _ | UnaryConstIlOp.Bgt_s _ | UnaryConstIlOp.Bgt_un _ | UnaryConstIlOp.Bgt_un_s _ | UnaryConstIlOp.Bge _ | UnaryConstIlOp.Bge_s _ | UnaryConstIlOp.Bge_un _ | UnaryConstIlOp.Bge_un_s _) ->
            Condition.TwoValues
        | _ -> Condition.NotConditional

    /// The capability queries' answers that decide branches, by the branch's offset, where the JIT
    /// optimises the body; validating what PawPrint says of it.
    let private decided (body : MethodInstructions<'methodVars>) (folding : JitFolding) : Map<int, bool> =
        match folding with
        | JitFolding.Nothing -> Map.empty
        | JitFolding.Optimises optimised ->
            for offset in optimised.OrdinaryCalls do
                match body.Locations.TryFind offset with
                | Some (IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt), _)) ->
                    if optimised.Capabilities.ContainsKey offset then
                        invalidArg
                            (nameof folding)
                            $"The call at offset %d{offset} is both an ordinary call and a capability query"
                | Some other ->
                    invalidArg (nameof folding) $"The instruction at offset %d{offset}, %O{other}, is not a call"
                | None -> invalidArg (nameof folding) $"No instruction starts at offset %d{offset}"

            ControlFlow.decidedBranches body optimised.Capabilities

    /// Bounds on what the JIT imports of `body` when it folds as `folding` says.
    let compiledCode (body : MethodInstructions<'methodVars>) (folding : JitFolding) : CompiledCode =
        let decided = decided body folding
        let maybe = ControlFlow.reachable body (ControlFlow.successorsDecided decided)

        match folding with
        | JitFolding.Nothing ->
            {
                Surely = maybe
                Maybe = maybe
            }
        | JitFolding.Optimises optimised ->

        let landedOn = ControlFlow.landedOn body

        // The instruction before each one in the stream, where something only falls through from it.
        let previous : Map<int, int * IlOp> =
            body.Instructions
            |> List.pairwise
            |> List.choose (fun ((instruction, offset), (_, next)) ->
                if landedOn.Contains next then
                    None
                else
                    Some (next, (offset, instruction))
            )
            |> Map.ofList

        // Whether the importer's node for the value `instruction` pushes is never a constant: a
        // local (an argument, a local, or the temp `newobj` allocates into) or a call it does not
        // expand, which it inlines, if at all, only after importing.
        let neverConstant (offset : int) (instruction : IlOp) : bool =
            match instruction with
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, _) -> true
            | IlOp.UnaryMetadataToken ((UnaryMetadataTokenIlOp.Call | UnaryMetadataTokenIlOp.Callvirt), _) ->
                optimised.OrdinaryCalls.Contains offset
            | _ -> (slotLoaded instruction).IsSome

        let cannotFold (offset : int) (instruction : IlOp) : bool =
            match conditionOf instruction with
            | Condition.NotConditional -> true
            | Condition.OneValue ->
                match Map.tryFind offset previous with
                | Some (pushOffset, push) -> neverConstant pushOffset push
                | None -> false
            | Condition.TwoValues ->
                // `gtFoldExprCompare` folds a comparison of two identical trees, so the operands
                // must be different arguments or locals.
                match Map.tryFind offset previous with
                | Some (secondOffset, second) ->
                    match Map.tryFind secondOffset previous, slotLoaded second with
                    | Some (_, first), Some secondSlot ->
                        match slotLoaded first with
                        | Some firstSlot -> firstSlot <> secondSlot
                        | None -> false
                    | _ -> false
                | None -> false

        // A capability query's answer decides only a `brtrue` or `brfalse` straight after it.
        let decidedHere (offset : int) (instruction : IlOp) : bool =
            match instruction with
            | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _ | UnaryConstIlOp.Brtrue_s _ | UnaryConstIlOp.Brfalse _ | UnaryConstIlOp.Brfalse_s _) ->
                decided.ContainsKey offset
            | _ -> false

        let surelySuccessors (offset : int) (instruction : IlOp) : int list =
            if decidedHere offset instruction || cannotFold offset instruction then
                ControlFlow.successorsDecided decided offset instruction
            else
                []

        {
            Surely = ControlFlow.reachable body surelySuccessors
            Maybe = maybe
        }

    /// The order in which the JIT imports what it may import of `body`, as far as it is fixed.
    let importOrder (body : MethodInstructions<'methodVars>) (folding : JitFolding) : ImportOrder =
        let decided = decided body folding

        // The flow graph of what the JIT may import, entering a handler from every instruction of
        // its protected block.
        let successors (offset : int) : int list =
            ControlFlow.successorsDecided decided offset body.Locations.[offset]
            @ ControlFlow.handlersEntered body offset
            |> List.filter body.Locations.ContainsKey
            |> List.distinct

        // Reverse postorder from the entry.
        let postorder = ResizeArray<int> ()
        let visited = System.Collections.Generic.HashSet<int> ()
        let stack = System.Collections.Generic.Stack<int * int list> ()

        if body.Locations.ContainsKey 0 then
            visited.Add 0 |> ignore<bool>
            stack.Push (0, successors 0)

        while stack.Count > 0 do
            match stack.Pop () with
            | node, [] -> postorder.Add node
            | node, next :: rest ->
                stack.Push (node, rest)

                if visited.Add next then
                    stack.Push (next, successors next)

        let rank =
            postorder
            |> Seq.indexed
            |> Seq.map (fun (index, node) -> node, index)
            |> Map.ofSeq

        let predecessors =
            postorder
            |> Seq.collect (fun node -> successors node |> Seq.map (fun next -> next, node))
            |> Seq.groupBy fst
            |> Seq.map (fun (node, edges) -> node, edges |> Seq.map snd |> List.ofSeq)
            |> Map.ofSeq

        // Cooper, Harvey and Kennedy, "A Simple, Fast Dominance Algorithm".
        let idom = System.Collections.Generic.Dictionary<int, int> ()

        if postorder.Count > 0 then
            idom.[0] <- 0

        let rec intersect (a : int) (b : int) : int =
            if a = b then a
            elif rank.[a] < rank.[b] then intersect idom.[a] b
            else intersect a idom.[b]

        let mutable changed = true

        while changed do
            changed <- false

            for node in Seq.rev postorder do
                if node <> 0 then
                    let processed =
                        Map.tryFind node predecessors
                        |> Option.defaultValue []
                        |> List.filter idom.ContainsKey

                    match processed with
                    | [] -> failwith $"BUG: offset %d{node} is reachable but no predecessor has been processed"
                    | first :: rest ->
                        let candidate = List.fold intersect first rest

                        match idom.TryGetValue node with
                        | true, existing when existing = candidate -> ()
                        | _ ->
                            idom.[node] <- candidate
                            changed <- true

        {
            ImportedJustBefore =
                idom
                |> Seq.filter (fun (KeyValue (node, _)) -> node <> 0)
                |> Seq.map (fun (KeyValue (node, dominator)) -> node, dominator)
                |> Map.ofSeq
        }

    /// Whether, whenever the JIT imports the instruction at `later`, it has imported the one at
    /// `earlier` first. False for an instruction and itself, and for an instruction the JIT does not
    /// import.
    let importedBefore (order : ImportOrder) (earlier : int) (later : int) : bool =
        let rec walk (node : int) : bool =
            match Map.tryFind node order.ImportedJustBefore with
            | Some dominator -> dominator = earlier || walk dominator
            | None -> false

        walk later
