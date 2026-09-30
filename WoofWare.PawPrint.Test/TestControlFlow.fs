namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for `ControlFlow.mayExecute`: the offsets a run of a body can execute, with the
/// conditional branches a known Boolean decides followed one way only.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestControlFlow =

    /// One instruction of a generated body. Targets are indices into the body's instructions.
    [<RequireQualifiedAccess>]
    type private Step =
        | Nop
        /// Pushes a Boolean known before the body runs.
        | Push of bool
        /// Pushes a Boolean nothing knows in advance.
        | PushUnknown
        | Pop
        | Br of int
        | Brtrue of int
        | Brfalse of int
        | Leave of int
        | Switch of int list
        | Ret
        | Throw
        | Endfinally

    [<RequireQualifiedAccess>]
    type private RegionKind =
        | Catch
        | Filter of filterStart : int
        | Finally
        | Fault

    /// A protected block, as instruction indices: `[TryStart, TryEnd)` and the handler's entry.
    type private Region =
        {
            Kind : RegionKind
            TryStart : int
            TryEnd : int
            HandlerStart : int
        }

    /// The IL for `steps`, in long branch forms so that an instruction's width does not depend on
    /// its target, with the byte offset of each index (and of the end, at index `steps.Length`).
    let private layOut (steps : Step list) (regions : Region list) : MethodInstructions<TypeDefn> * int[] =
        let opWith (delta : int -> int) (step : Step) : IlOp =
            match step with
            | Step.Nop -> IlOp.Nullary NullaryIlOp.Nop
            | Step.Push false -> IlOp.Nullary NullaryIlOp.LdcI4_0
            | Step.Push true -> IlOp.Nullary NullaryIlOp.LdcI4_1
            | Step.PushUnknown -> IlOp.Nullary NullaryIlOp.LdArg0
            | Step.Pop -> IlOp.Nullary NullaryIlOp.Pop
            | Step.Br t -> IlOp.UnaryConst (UnaryConstIlOp.Br (delta t))
            | Step.Brtrue t -> IlOp.UnaryConst (UnaryConstIlOp.Brtrue (delta t))
            | Step.Brfalse t -> IlOp.UnaryConst (UnaryConstIlOp.Brfalse (delta t))
            | Step.Leave t -> IlOp.UnaryConst (UnaryConstIlOp.Leave (delta t))
            | Step.Switch ts -> IlOp.Switch (ts |> List.map delta |> ImmutableArray.CreateRange)
            | Step.Ret -> IlOp.Nullary NullaryIlOp.Ret
            | Step.Throw -> IlOp.Nullary NullaryIlOp.Throw
            | Step.Endfinally -> IlOp.Nullary NullaryIlOp.Endfinally

        let widths = steps |> List.map (opWith (fun _ -> 0) >> IlOp.NumberOfBytes)
        let at = widths |> List.scan (+) 0 |> Array.ofList

        let ops =
            steps
            |> List.mapi (fun index step ->
                let next = at.[index] + widths.[index]
                opWith (fun target -> at.[target] - next) step, at.[index]
            )

        let region (r : Region) : ExceptionRegion =
            let offsets =
                {
                    TryOffset = at.[r.TryStart]
                    TryLength = at.[r.TryEnd] - at.[r.TryStart]
                    HandlerOffset = at.[r.HandlerStart]
                    HandlerLength = 1
                }

            match r.Kind with
            | RegionKind.Catch ->
                ExceptionRegion.Catch (ExceptionCatchType.FromMetadata (MetadataToken.ofInt 0x01000001), offsets)
            | RegionKind.Filter filterStart -> ExceptionRegion.Filter (at.[filterStart], offsets)
            | RegionKind.Finally -> ExceptionRegion.Finally offsets
            | RegionKind.Fault -> ExceptionRegion.Fault offsets

        {
            Instructions = ops
            Locations = ops |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
            LocalsInit = false
            LocalVars = None
            ExceptionRegions = regions |> List.map region |> ImmutableArray.CreateRange
        },
        at

    /// Every offset some run of `steps` executes, found by exploring every choice a run can make:
    /// each Boolean nothing knows goes both ways, a `switch` goes everywhere, and any instruction in
    /// a protected block may raise, entering its handler or filter. A pushed Boolean is tracked
    /// through the stack, so a branch on a known one goes one way only.
    let private executed (steps : Step list) (regions : Region list) : Set<int> =
        let steps = Array.ofList steps
        // Deep enough that a known Boolean is never truncated between its push and its pop in
        // any body where `mayExecute` would use it; below this, a slot reads as unknown.
        let depth = 8
        let seen = System.Collections.Generic.HashSet<int * bool option list> ()
        let pending = System.Collections.Generic.Stack<int * bool option list> ()

        let enter (index : int) (stack : bool option list) =
            if index < steps.Length then
                let state = index, List.truncate depth stack

                if seen.Add state then
                    pending.Push state

        let pop (stack : bool option list) : bool option * bool option list =
            match stack with
            | top :: rest -> top, rest
            | [] -> None, []

        let branch (value : bool option) (jumpWhen : bool) (target : int) (next : int) (rest : bool option list) =
            match value with
            | Some v -> enter (if v = jumpWhen then target else next) rest
            | None ->
                enter target rest
                enter next rest

        enter 0 []

        while pending.Count > 0 do
            let index, stack = pending.Pop ()

            for r in regions do
                if r.TryStart <= index && index < r.TryEnd then
                    match r.Kind with
                    | RegionKind.Catch -> enter r.HandlerStart [ None ]
                    | RegionKind.Filter filterStart ->
                        enter filterStart [ None ]
                        enter r.HandlerStart [ None ]
                    | RegionKind.Finally
                    | RegionKind.Fault -> enter r.HandlerStart []

            let next = index + 1

            match steps.[index] with
            | Step.Nop -> enter next stack
            | Step.Push b -> enter next (Some b :: stack)
            | Step.PushUnknown -> enter next (None :: stack)
            | Step.Pop -> enter next (snd (pop stack))
            | Step.Br t -> enter t stack
            | Step.Brtrue t ->
                let value, rest = pop stack
                branch value true t next rest
            | Step.Brfalse t ->
                let value, rest = pop stack
                branch value false t next rest
            | Step.Leave t -> enter t []
            | Step.Switch ts ->
                let _, rest = pop stack

                for t in next :: ts do
                    enter t rest
            | Step.Ret
            | Step.Throw
            | Step.Endfinally -> ()

        seen |> Seq.map fst |> Set.ofSeq

    let private genBody : Gen<Step list * Region list> =
        gen {
            // Some steps are a known push with a branch straight after it, the shape `mayExecute`
            // decides; the rest are single instructions.
            let! chunkCount = Gen.choose (1, 10)
            let! pairs = Gen.listOfLength chunkCount (Gen.frequency [ 3, Gen.constant false ; 1, Gen.constant true ])
            let length = pairs |> List.sumBy (fun pair -> if pair then 2 else 1)
            let target = Gen.choose (0, length - 1)

            let genStep =
                Gen.frequency
                    [
                        3, Gen.constant Step.Nop
                        6, Gen.elements [ Step.Push false ; Step.Push true ]
                        2, Gen.constant Step.PushUnknown
                        1, Gen.constant Step.Pop
                        2, Gen.map Step.Br target
                        5, Gen.map Step.Brtrue target
                        5, Gen.map Step.Brfalse target
                        1, Gen.map Step.Leave target
                        1, Gen.map Step.Switch (Gen.listOf target |> Gen.map (List.truncate 3))
                        2, Gen.constant Step.Ret
                        1, Gen.constant Step.Throw
                        1, Gen.constant Step.Endfinally
                    ]

            let genPair =
                gen {
                    let! value = Gen.elements [ false ; true ]
                    let! t = target
                    let! branch = Gen.elements [ Step.Brtrue t ; Step.Brfalse t ]
                    return [ Step.Push value ; branch ]
                }

            let! chunks =
                pairs
                |> List.map (fun pair -> if pair then genPair else Gen.map List.singleton genStep)
                |> Gen.sequenceToList

            let steps = List.concat chunks

            let genRegion =
                gen {
                    let! a = Gen.choose (0, length)
                    let! b = Gen.choose (0, length)
                    let! handlerStart = target

                    let! kind =
                        Gen.oneof
                            [
                                Gen.constant RegionKind.Catch
                                Gen.map RegionKind.Filter target
                                Gen.constant RegionKind.Finally
                                Gen.constant RegionKind.Fault
                            ]

                    return
                        {
                            Kind = kind
                            TryStart = min a b
                            TryEnd = max a b
                            HandlerStart = handlerStart
                        }
                }

            let! regionCount = Gen.choose (0, 3)
            let! regions = Gen.listOfLength regionCount genRegion
            return steps, regions
        }

    /// The known Boolean each `Push` pushes, at its offset: those `mayExecute` may use.
    let private pushedConstants (steps : Step list) (at : int[]) : Map<int, bool> =
        steps
        |> List.indexed
        |> List.choose (fun (index, step) ->
            match step with
            | Step.Push b -> Some (at.[index], b)
            | _ -> None
        )
        |> Map.ofList

    [<Test>]
    let ``every offset a run executes is one mayExecute gives`` () : unit =
        let mutable pruned = 0
        let mutable handlersExcluded = 0

        let property (steps : Step list, regions : Region list) : bool =
            let body, at = layOut steps regions
            let constants = pushedConstants steps at
            let may = ControlFlow.mayExecute body constants
            let run = executed steps regions |> Set.map (fun index -> at.[index])

            if Set.isProperSubset may (ControlFlow.mayExecute body Map.empty) then
                pruned <- pruned + 1

            if
                body.ExceptionRegions
                |> Seq.exists (fun region ->
                    match region with
                    | ExceptionRegion.Catch (_, o)
                    | ExceptionRegion.Filter (_, o)
                    | ExceptionRegion.Finally o
                    | ExceptionRegion.Fault o -> not (may.Contains o.HandlerOffset)
                )
            then
                handlersExcluded <- handlersExcluded + 1

            Set.isSubset run may

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen genBody) property)

        // The property holds of any superset, so show that `mayExecute` does leave things out.
        pruned |> shouldBeGreaterThan 130
        handlersExcluded |> shouldBeGreaterThan 90

    [<Test>]
    let ``a branch on a known Boolean goes one way`` () : unit =
        // push; brtrue L; <dead>; L: ret
        for value, jumps in [ true, true ; false, false ] do
            let steps = [ Step.Push value ; Step.Brtrue 4 ; Step.Nop ; Step.Ret ; Step.Ret ]
            let body, at = layOut steps []

            ControlFlow.mayExecute body (pushedConstants steps at)
            |> shouldEqual (
                if jumps then
                    Set.ofList [ at.[0] ; at.[1] ; at.[4] ]
                else
                    Set.ofList [ at.[0] ; at.[1] ; at.[2] ; at.[3] ]
            )

        let steps = [ Step.Push false ; Step.Brfalse 3 ; Step.Ret ; Step.Ret ]
        let body, at = layOut steps []

        ControlFlow.mayExecute body (pushedConstants steps at)
        |> shouldEqual (Set.ofList [ at.[0] ; at.[1] ; at.[3] ])

    [<Test>]
    let ``a branch something else also reaches keeps both successors`` () : unit =
        // unknown; brtrue P; push false; B: brtrue L; ret; L: ret; P: push true; br B
        // `B` is reached from `push false` and from `br B`, so its operand is not that push's.
        let steps =
            [
                Step.PushUnknown
                Step.Brtrue 6
                Step.Push false
                Step.Brtrue 5
                Step.Ret
                Step.Ret
                Step.Push true
                Step.Br 3
            ]

        let body, at = layOut steps []

        ControlFlow.mayExecute body (pushedConstants steps at)
        |> shouldEqual (Set.ofSeq at.[0..7])

    [<Test>]
    let ``a handler runs only if its protected block does`` () : unit =
        // push false; brtrue T; ret; T: nop; leave E; H: endfinally; E: ret
        let steps =
            [
                Step.Push false
                Step.Brtrue 3
                Step.Ret
                Step.Nop
                Step.Leave 6
                Step.Endfinally
                Step.Ret
            ]

        for kind in
            [
                RegionKind.Catch
                RegionKind.Finally
                RegionKind.Fault
                RegionKind.Filter 5
            ] do
            let region =
                {
                    Kind = kind
                    TryStart = 3
                    TryEnd = 5
                    HandlerStart = 5
                }

            let body, at = layOut steps [ region ]

            ControlFlow.mayExecute body (pushedConstants steps at)
            |> shouldEqual (Set.ofList [ at.[0] ; at.[1] ; at.[2] ])

            ControlFlow.mayExecute body Map.empty |> shouldEqual (Set.ofSeq at.[0..6])

    [<Test>]
    let ``an offset just past a protected block does not run its handler`` () : unit =
        // nop; ret; H: endfinally, protecting only the unreachable instruction before `H`.
        let steps = [ Step.Br 2 ; Step.Nop ; Step.Ret ; Step.Endfinally ]

        let region =
            {
                Kind = RegionKind.Finally
                TryStart = 1
                TryEnd = 2
                HandlerStart = 3
            }

        let body, at = layOut steps [ region ]

        ControlFlow.mayExecute body Map.empty
        |> shouldEqual (Set.ofList [ at.[0] ; at.[2] ])

    [<Test>]
    let ``a filter runs if its protected block does`` () : unit =
        // try { nop; leave E } filter F: endfilter handler H: leave E; E: ret
        let steps = [ Step.Nop ; Step.Leave 4 ; Step.Endfinally ; Step.Leave 4 ; Step.Ret ]

        let region =
            {
                Kind = RegionKind.Filter 2
                TryStart = 0
                TryEnd = 2
                HandlerStart = 3
            }

        let body, at = layOut steps [ region ]
        ControlFlow.mayExecute body Map.empty |> shouldEqual (Set.ofSeq at.[0..4])

    [<Test>]
    let ``a constant at an offset that does not fall through is refused`` () : unit =
        let steps = [ Step.Ret ; Step.Ret ]
        let body, at = layOut steps []

        Assert.Throws<System.ArgumentException> (fun () ->
            ControlFlow.mayExecute body (Map.ofList [ at.[0], true ]) |> ignore
        )
        |> ignore

        Assert.Throws<System.ArgumentException> (fun () ->
            ControlFlow.mayExecute body (Map.ofList [ 99, true ]) |> ignore
        )
        |> ignore
