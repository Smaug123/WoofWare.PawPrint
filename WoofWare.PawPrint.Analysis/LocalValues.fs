namespace WoofWare.PawPrint.Analysis

open WoofWare.PawPrint

/// What a local of an integer type holds on entry to an instruction, over every path to it.
type internal LocalState =
    {
        /// One of these integers, as the stack held them when the body stored them, where only
        /// the body's own stores have stored to it (`Escaped`); `None` for any value of the local's
        /// type.
        Holds : Set<SpelledInteger> option
        /// Whether some path to the instruction takes the local's address.
        Taken : bool
        /// Whether, on some path to the instruction, something ran after the local's address was
        /// taken that `OwnWrites.only` does not accept, which may have kept the address, so that
        /// whatever kept it may store to the local through it at any time after.
        Escaped : bool
    }

/// What a body's locals of integer types whose addresses it takes hold on entry to each of its
/// instructions.
type internal LocalValues =
    {
        /// The integer type of each such local, by index.
        Types : Map<int, PrimitiveType>
        /// Each such local's state on entry to each offset a path reaches, by offset, then by index.
        Entry : Map<int, Map<int, LocalState>>
    }

[<RequireQualifiedAccess>]
module internal LocalValues =

    /// How many integers a local may hold before it holds any value; it bounds the join.
    let private limit : int = 8

    let private stored (op : IlOp) : int option =
        match op with
        | IlOp.Nullary NullaryIlOp.Stloc_0 -> Some 0
        | IlOp.Nullary NullaryIlOp.Stloc_1 -> Some 1
        | IlOp.Nullary NullaryIlOp.Stloc_2 -> Some 2
        | IlOp.Nullary NullaryIlOp.Stloc_3 -> Some 3
        | IlOp.UnaryConst (UnaryConstIlOp.Stloc_s index) -> Some (int index)
        | IlOp.UnaryConst (UnaryConstIlOp.Stloc index) -> Some (int index)
        | _ -> None

    let private addressed (op : IlOp) : int option =
        match op with
        | IlOp.UnaryConst (UnaryConstIlOp.Ldloca_s index) -> Some (int index)
        | IlOp.UnaryConst (UnaryConstIlOp.Ldloca index) -> Some (int index)
        | _ -> None

    let private join (a : LocalState) (b : LocalState) : LocalState =
        {
            Holds =
                match a.Holds, b.Holds with
                | Some a, Some b ->
                    let holds = Set.union a b
                    if holds.Count > limit then None else Some holds
                | _ -> None
            Taken = a.Taken || b.Taken
            Escaped = a.Escaped || b.Escaped
        }

    /// What the locals of `body` whose types `types` gives (one for each local, by index, `None`
    /// for a type that is not an integer type `StackInteger.ofParameter` names) hold, given what
    /// `values` says is on the stack where it stores to them. A local whose address the body never
    /// takes is left out, as is any offset no path reaches.
    let analyse
        (body : MethodInstructions<'methodVars>)
        (types : PrimitiveType option list)
        (values : StackFlow<StackValue>)
        : LocalValues
        =
        let addressedLocals =
            body.Instructions |> List.choose (fst >> addressed) |> Set.ofList

        let tracked =
            types
            |> List.indexed
            |> List.choose (fun (index, ty) ->
                match ty with
                | Some ty when addressedLocals.Contains index -> Some (index, ty)
                | _ -> None
            )
            |> Map.ofList

        if tracked.IsEmpty then
            {
                Types = tracked
                Entry = Map.empty
            }
        else

        let handlerRanges =
            body.ExceptionRegions
            |> Seq.choose (fun region ->
                match region with
                | ExceptionRegion.Finally offsets
                | ExceptionRegion.Fault offsets -> Some (offsets.HandlerOffset, offsets.HandlerLength)
                | ExceptionRegion.Catch _
                | ExceptionRegion.Filter _ -> None
            )
            |> List.ofSeq

        // The locals whose address a `finally` or `fault` handler takes, which a `leave` may run.
        let addressedInHandlers =
            body.Instructions
            |> List.choose (fun (op, offset) ->
                if
                    handlerRanges
                    |> List.exists (fun (start, length) -> start <= offset && offset < start + length)
                then
                    addressed op
                else
                    None
            )
            |> Set.ofList

        let everywhere (state : LocalState) : Map<int, LocalState> = tracked |> Map.map (fun _ _ -> state)

        let after (offset : int) (op : IlOp) (before : Map<int, LocalState>) : Map<int, LocalState> =
            let state =
                match addressed op with
                | Some index when before.ContainsKey index ->
                    Map.add
                        index
                        { before.[index] with
                            Taken = true
                        }
                        before
                | _ -> before

            let state =
                match stored op with
                | Some index when state.ContainsKey index ->
                    let holds =
                        match Map.tryFind offset values.Entry with
                        | Some (StackValue.Integers integers :: _) -> Some integers
                        | _ -> None

                    Map.add
                        index
                        { state.[index] with
                            Holds = holds
                        }
                        state
                | _ -> state

            // What `op` does with an address, or what the code it runs does, may keep it.
            if OwnWrites.only op then
                state
            else
                state
                |> Map.map (fun _ local ->
                    { local with
                        Escaped = local.Escaped || local.Taken
                    }
                )

        let entry = System.Collections.Generic.Dictionary<int, Map<int, LocalState>> ()
        let worklist = System.Collections.Generic.Queue<int> ()

        let arrive (offset : int) (state : Map<int, LocalState>) : unit =
            if body.Locations.ContainsKey offset then
                match entry.TryGetValue offset with
                | true, existing ->
                    let joined = Map.map (fun index local -> join local existing.[index]) state

                    if joined <> existing then
                        entry.[offset] <- joined
                        worklist.Enqueue offset
                | false, _ ->
                    entry.[offset] <- state
                    worklist.Enqueue offset

        arrive
            0
            (everywhere
                {
                    Holds = None
                    Taken = false
                    Escaped = false
                })

        // A handler may be entered from anywhere in its protected block.
        for region in body.ExceptionRegions do
            let unknown =
                everywhere
                    {
                        Holds = None
                        Taken = true
                        Escaped = true
                    }

            match region with
            | ExceptionRegion.Filter (filterOffset, offsets) ->
                arrive filterOffset unknown
                arrive offsets.HandlerOffset unknown
            | ExceptionRegion.Catch (_, offsets)
            | ExceptionRegion.Finally offsets
            | ExceptionRegion.Fault offsets -> arrive offsets.HandlerOffset unknown

        while worklist.Count > 0 do
            let offset = worklist.Dequeue ()
            let op = body.Locations.[offset]
            let state = after offset op entry.[offset]
            let next = offset + IlOp.NumberOfBytes op

            match ControlFlow.successorsOf offset op with
            | Successors.None -> ()
            | Successors.FallThrough -> arrive next state
            | Successors.Targets targets -> targets |> List.iter (fun target -> arrive target state)
            | Successors.TargetsAndFallThrough targets ->
                next :: targets |> List.iter (fun target -> arrive target state)
            | Successors.Leave target ->
                // The `finally` handlers it runs may store to any local, take the address of those
                // they take it of, and run anything.
                state
                |> Map.map (fun index local ->
                    let taken = local.Taken || addressedInHandlers.Contains index

                    {
                        Holds = None
                        Taken = taken
                        Escaped = local.Escaped || taken
                    }
                )
                |> arrive target

        {
            Types = tracked
            Entry = entry |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
        }

    /// The integer type of the locals `addresses` names, and what they hold on entry to the
    /// instruction at `offset`, if that is known: every address is a local's, all of one type,
    /// whose address has not escaped there, so that nothing but the body's own stores can have
    /// stored to it, and nothing else holds its address.
    let held
        (locals : LocalValues)
        (offset : int)
        (addresses : Set<SpelledAddress>)
        : (PrimitiveType * Set<SpelledInteger>) option
        =
        let ofAddress (address : SpelledAddress) : (PrimitiveType * Set<SpelledInteger>) option =
            match address with
            | SpelledAddress.Argument _ -> None
            | SpelledAddress.Local index ->
                let state = Map.tryFind offset locals.Entry |> Option.bind (Map.tryFind index)

                match Map.tryFind index locals.Types, state with
                | Some ty, Some state when not state.Escaped -> state.Holds |> Option.map (fun holds -> ty, holds)
                | _ -> None

        let held = addresses |> Set.toList |> List.map ofAddress

        match held with
        | Some (ty, _) :: _ when
            held
            |> List.forall (fun held ->
                match held with
                | Some (other, _) -> other = ty
                | None -> false
            )
            ->
            Some (ty, held |> List.choose (Option.map snd) |> Set.unionMany)
        | _ -> None
