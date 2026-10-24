namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for `JitImport`: bounds on what CoreCLR's JIT imports of a body, and the order it
/// imports it in.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestJitImport =

    /// One instruction of a generated body. Targets are indices into the body's instructions.
    [<RequireQualifiedAccess>]
    type private Step =
        | Nop
        | Pop
        /// `ldarg`, in its short or long form.
        | LdArg of index : int * longForm : bool
        /// `ldloc`, in its short or long form.
        | LdLoc of index : int * longForm : bool
        /// `ldarga.s`, whose node the importer may fold.
        | LdArgAddress
        | Const of bool
        /// `add`, of whatever is on the stack: a constant only if both operands are.
        | Add
        /// A `call` of a method the JIT never expands.
        | CallOrdinary
        /// A `call` of a capability query, answering this.
        | CallCapability of bool
        /// A `call` of a method the JIT may expand to anything.
        | CallOther
        | Newobj
        | Br of int
        | Brtrue of int
        | Brfalse of int
        | BneUn of int
        | Switch of int list
        | Leave of int
        | Ret
        | Throw
        | Endfinally

    [<RequireQualifiedAccess>]
    type private RegionKind =
        | Catch
        | Filter of filterStart : int
        | Finally

    /// A protected block, as instruction indices: `[TryStart, TryEnd)` and the handler's entry.
    type private Region =
        {
            Kind : RegionKind
            TryStart : int
            TryEnd : int
            HandlerStart : int
        }

    let private callTo (op : UnaryMetadataTokenIlOp) (row : int) : IlOp =
        IlOp.UnaryMetadataToken (
            op,
            MetadataOperand.FromMetadata (
                SourcedMetadataToken.make (AssemblyName "Generated") (MetadataToken.ofInt (0x06000000 + row))
            )
        )

    /// The IL for `steps`, in long branch forms so that an instruction's width does not depend on
    /// its target, with the byte offset of each index (and of the end, at index `steps.Length`).
    let private layOut (steps : Step list) (regions : Region list) : MethodInstructions<TypeDefn> * int[] =
        let opWith (delta : int -> int) (step : Step) : IlOp =
            match step with
            | Step.Nop -> IlOp.Nullary NullaryIlOp.Nop
            | Step.Pop -> IlOp.Nullary NullaryIlOp.Pop
            | Step.LdArg (0, false) -> IlOp.Nullary NullaryIlOp.LdArg0
            | Step.LdArg (1, false) -> IlOp.Nullary NullaryIlOp.LdArg1
            | Step.LdArg (index, _) -> IlOp.UnaryConst (UnaryConstIlOp.Ldarg_s (byte index))
            | Step.LdLoc (0, false) -> IlOp.Nullary NullaryIlOp.Ldloc_0
            | Step.LdLoc (1, false) -> IlOp.Nullary NullaryIlOp.Ldloc_1
            | Step.LdLoc (index, _) -> IlOp.UnaryConst (UnaryConstIlOp.Ldloc_s (byte index))
            | Step.LdArgAddress -> IlOp.UnaryConst (UnaryConstIlOp.Ldarga_s 0uy)
            | Step.Const false -> IlOp.Nullary NullaryIlOp.LdcI4_0
            | Step.Const true -> IlOp.Nullary NullaryIlOp.LdcI4_1
            | Step.Add -> IlOp.Nullary NullaryIlOp.Add
            | Step.CallOrdinary -> callTo UnaryMetadataTokenIlOp.Call 1
            | Step.CallCapability _ -> callTo UnaryMetadataTokenIlOp.Call 2
            | Step.CallOther -> callTo UnaryMetadataTokenIlOp.Call 3
            | Step.Newobj -> callTo UnaryMetadataTokenIlOp.Newobj 4
            | Step.Br t -> IlOp.UnaryConst (UnaryConstIlOp.Br (delta t))
            | Step.Brtrue t -> IlOp.UnaryConst (UnaryConstIlOp.Brtrue (delta t))
            | Step.Brfalse t -> IlOp.UnaryConst (UnaryConstIlOp.Brfalse (delta t))
            | Step.BneUn t -> IlOp.UnaryConst (UnaryConstIlOp.Bne_un (delta t))
            | Step.Leave t -> IlOp.UnaryConst (UnaryConstIlOp.Leave (delta t))
            | Step.Switch ts -> IlOp.Switch (ts |> List.map delta |> ImmutableArray.CreateRange)
            | Step.Ret -> IlOp.Nullary NullaryIlOp.Ret
            | Step.Throw -> IlOp.Nullary NullaryIlOp.Throw
            | Step.Endfinally -> IlOp.Nullary NullaryIlOp.Endfinally

        // A `br` to the very next instruction is a `nop` to `ControlFlow`; keep every `br` a block
        // boundary by measuring it with a nonzero displacement.
        let widths = steps |> List.map (opWith (fun _ -> 1) >> IlOp.NumberOfBytes)
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

        {
            Instructions = ops
            Locations = ops |> List.map (fun (op, offset) -> offset, op) |> Map.ofList
            LocalsInit = false
            LocalVars = None
            ExceptionRegions = regions |> List.map region |> ImmutableArray.CreateRange
        },
        at

    /// What PawPrint tells `JitImport` of a generated body the JIT optimises.
    let private optimised (steps : Step list) (at : int[]) : OptimisedBody =
        {
            Capabilities =
                steps
                |> List.indexed
                |> List.choose (fun (index, step) ->
                    match step with
                    | Step.CallCapability value -> Some (at.[index], value)
                    | _ -> None
                )
                |> Map.ofList
            OrdinaryCalls =
                steps
                |> List.indexed
                |> List.choose (fun (index, step) ->
                    match step with
                    | Step.CallOrdinary -> Some at.[index]
                    | _ -> None
                )
                |> Set.ofList
        }

    /// How the reference treats the conditional branch at an index.
    [<RequireQualifiedAccess>]
    type private Branch =
        /// The JIT imports each successor: it cannot fold the condition.
        | Kept
        /// The JIT folds it to the successor at this index.
        | Decided of int
        /// It may fold the condition to any one successor, or not at all.
        | MayFold

    /// The indices some branch or handler lands on, so that their stack is not what the instruction
    /// before left.
    let private landed (steps : Step list) (regions : Region list) : Set<int> =
        let fromSteps =
            steps
            |> List.collect (fun step ->
                match step with
                | Step.Br t
                | Step.Brtrue t
                | Step.Brfalse t
                | Step.BneUn t
                | Step.Leave t -> [ t ]
                | Step.Switch ts -> ts
                | _ -> []
            )

        let fromRegions =
            regions
            |> List.collect (fun r ->
                match r.Kind with
                | RegionKind.Filter f -> [ f ; r.HandlerStart ]
                | RegionKind.Catch
                | RegionKind.Finally -> [ r.HandlerStart ]
            )

        Set.ofList (fromSteps @ fromRegions)

    /// The successors of each index of a body when the JIT folds the conditional branches as
    /// `choose` says, entering a handler from every index of its protected block; and the
    /// reference's rules for which branches it may fold, read off the steps that push the
    /// condition.
    let private branchKinds (optimises : bool) (steps : Step[]) (landedOn : Set<int>) : int -> Branch =
        let isLeaf (step : Step) : bool =
            match step with
            | Step.LdArg _
            | Step.LdLoc _
            | Step.CallOrdinary
            | Step.Newobj -> true
            | _ -> false

        let sameSlot (a : Step) (b : Step) : bool =
            match a, b with
            | Step.LdArg (i, _), Step.LdArg (j, _) -> i = j
            | Step.LdLoc (i, _), Step.LdLoc (j, _) -> i = j
            | _ -> false

        let isSlot (step : Step) : bool =
            match step with
            | Step.LdArg _
            | Step.LdLoc _ -> true
            | _ -> false

        fun (index : int) ->
            if not optimises then
                Branch.Kept
            else

            let before (back : int) : Step option =
                if index - back >= 0 then
                    Some steps.[index - back]
                else
                    None

            match steps.[index] with
            | Step.Brtrue t
            | Step.Brfalse t when not (landedOn.Contains index) ->
                let jumpWhen =
                    match steps.[index] with
                    | Step.Brtrue _ -> true
                    | _ -> false

                match before 1 with
                | Some (Step.CallCapability value) -> Branch.Decided (if value = jumpWhen then t else index + 1)
                | Some step when isLeaf step -> Branch.Kept
                | _ -> Branch.MayFold
            | Step.Switch _ when not (landedOn.Contains index) ->
                match before 1 with
                | Some step when isLeaf step -> Branch.Kept
                | _ -> Branch.MayFold
            | Step.BneUn _ when not (landedOn.Contains index) && not (landedOn.Contains (index - 1)) ->
                match before 2, before 1 with
                | Some a, Some b when isSlot a && isSlot b && not (sameSlot a b) -> Branch.Kept
                | _ -> Branch.MayFold
            | _ -> Branch.MayFold

    /// Each way control leaves the step at `index`, unfolded, as indices.
    let private allSuccessors (steps : Step[]) (index : int) : int list =
        match steps.[index] with
        | Step.Br t
        | Step.Leave t -> [ t ]
        | Step.Brtrue t
        | Step.Brfalse t
        | Step.BneUn t -> [ index + 1 ; t ]
        | Step.Switch ts -> (index + 1) :: ts
        | Step.Ret
        | Step.Throw
        | Step.Endfinally -> []
        | _ -> [ index + 1 ]

    /// The indices reached from 0 when the step at each index goes to `successors index`, and the
    /// handlers of each protected block holding it, never entering `avoiding`.
    let private reach
        (steps : Step[])
        (regions : Region list)
        (successors : int -> int list)
        (avoiding : int option)
        : Set<int>
        =
        let seen = Collections.Generic.HashSet<int> ()
        let pending = Collections.Generic.Stack<int> ()

        let enter (index : int) =
            if index < steps.Length && Some index <> avoiding && seen.Add index then
                pending.Push index

        enter 0

        while pending.Count > 0 do
            let index = pending.Pop ()

            for r in regions do
                if r.TryStart <= index && index < r.TryEnd then
                    match r.Kind with
                    | RegionKind.Filter f ->
                        enter f
                        enter r.HandlerStart
                    | RegionKind.Catch
                    | RegionKind.Finally -> enter r.HandlerStart

            for next in successors index do
                enter next

        Set.ofSeq seen

    /// Every way the JIT may fold the branches the reference says it may fold, each as the
    /// successors it gives every index; `None` where there are too many to enumerate.
    let private foldings (steps : Step[]) (kind : int -> Branch) : (int -> int list) list option =
        let conditional =
            [ 0 .. steps.Length - 1 ]
            |> List.filter (fun index ->
                match steps.[index] with
                | Step.Brtrue _
                | Step.Brfalse _
                | Step.BneUn _
                | Step.Switch _ -> true
                | _ -> false
            )

        let mayFold = conditional |> List.filter (fun index -> kind index = Branch.MayFold)

        if mayFold.Length > 6 then
            None
        else
            // Each may-fold branch is either kept or folded to one of its successors.
            let choices (index : int) : int list option list =
                None
                :: (allSuccessors steps index |> List.distinct |> List.map (fun s -> Some [ s ]))

            let rec assignments (remaining : int list) : Map<int, int list option> list =
                match remaining with
                | [] -> [ Map.empty ]
                | index :: rest ->
                    [
                        for tail in assignments rest do
                            for choice in choices index do
                                Map.add index choice tail
                    ]

            assignments mayFold
            |> List.map (fun assignment index ->
                match kind index with
                | Branch.Decided target -> [ target ]
                | Branch.Kept -> allSuccessors steps index
                | Branch.MayFold ->
                    match Map.tryFind index assignment with
                    | Some (Some folded) -> folded
                    | Some None
                    | None -> allSuccessors steps index
            )
            |> Some

    let private genBody : Gen<Step list * Region list * bool> =
        gen {
            let! length = Gen.choose (2, 16)
            let target = Gen.choose (0, length - 1)

            let genStep =
                Gen.frequency
                    [
                        2, Gen.constant Step.Nop
                        1, Gen.constant Step.Pop
                        4, Gen.map2 (fun i l -> Step.LdArg (i, l)) (Gen.choose (0, 1)) (Gen.elements [ false ; true ])
                        3, Gen.map2 (fun i l -> Step.LdLoc (i, l)) (Gen.choose (0, 1)) (Gen.elements [ false ; true ])
                        1, Gen.constant Step.LdArgAddress
                        2, Gen.elements [ Step.Const false ; Step.Const true ]
                        1, Gen.constant Step.Add
                        2, Gen.constant Step.CallOrdinary
                        2, Gen.elements [ Step.CallCapability false ; Step.CallCapability true ]
                        1, Gen.constant Step.CallOther
                        1, Gen.constant Step.Newobj
                        2, Gen.map Step.Br target
                        5, Gen.map Step.Brtrue target
                        4, Gen.map Step.Brfalse target
                        3, Gen.map Step.BneUn target
                        1, Gen.map Step.Switch (Gen.listOf target |> Gen.map (List.truncate 3))
                        1, Gen.map Step.Leave target
                        2, Gen.constant Step.Ret
                        1, Gen.constant Step.Throw
                        1, Gen.constant Step.Endfinally
                    ]

            let! steps = Gen.listOfLength length genStep

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
                            ]

                    return
                        {
                            Kind = kind
                            TryStart = min a b
                            TryEnd = max a b
                            HandlerStart = handlerStart
                        }
                }

            let! regionCount = Gen.choose (0, 2)
            let! regions = Gen.listOfLength regionCount genRegion
            let! optimises = Gen.frequency [ 4, Gen.constant true ; 1, Gen.constant false ]
            return steps, regions, optimises
        }

    let private foldingOf (steps : Step list) (at : int[]) (optimises : bool) : JitFolding =
        if optimises then
            JitFolding.Optimises (optimised steps at)
        else
            JitFolding.Nothing

    /// What a generated case showed, beyond the property holding.
    [<RequireQualifiedAccess>]
    type private Shown =
        /// The JIT surely imports something past a conditional branch.
        | SurelyPastABranch
        /// Something it may import, it need not.
        | SurelyFallsShort
        /// One instruction is imported before another that is not straight after it.
        | OrderedAcrossBlocks
        /// Every way of folding was enumerated.
        | Enumerated

    [<Test>]
    let ``the bounds hold whichever way the JIT folds what it may`` () : unit =
        let property (cover : Shown -> unit) (steps : Step list, regions : Region list, optimises : bool) : bool =
            let body, at = layOut steps regions
            let folding = foldingOf steps at optimises
            let code = JitImport.compiledCode body folding
            let stepArray = Array.ofList steps
            let kind = branchKinds optimises stepArray (landed steps regions)

            let toOffsets (indices : Set<int>) =
                indices |> Set.map (fun index -> at.[index])

            let expectedMaybe =
                ControlFlow.mayExecute
                    body
                    (if optimises then
                         (optimised steps at).Capabilities
                     else
                         Map.empty)

            if code.Maybe <> expectedMaybe then
                failwith $"Maybe is %O{code.Maybe}, but mayExecute gives %O{expectedMaybe}"

            if not (Set.isSubset code.Surely code.Maybe) then
                failwith $"Surely %O{code.Surely} is not within Maybe %O{code.Maybe}"

            if not optimises && code.Surely <> code.Maybe then
                failwith $"Unoptimised, Surely %O{code.Surely} should be Maybe %O{code.Maybe}"

            if code.Surely.Count < code.Maybe.Count then
                cover Shown.SurelyFallsShort

            if
                code.Surely
                |> Set.exists (fun offset ->
                    let index = Array.findIndex ((=) offset) at

                    [ 0 .. index - 1 ]
                    |> List.exists (fun before ->
                        match stepArray.[before] with
                        | Step.Brtrue _
                        | Step.Brfalse _
                        | Step.BneUn _
                        | Step.Switch _ -> code.Surely.Contains at.[before]
                        | _ -> false
                    )
                )
            then
                cover Shown.SurelyPastABranch

            match foldings stepArray kind with
            | None -> ()
            | Some foldings ->
                cover Shown.Enumerated

                let reached =
                    foldings
                    |> List.map (fun successors -> reach stepArray regions successors None |> toOffsets)

                let everyFolding = reached |> List.reduce Set.intersect
                let someFolding = reached |> List.reduce Set.union

                if not (Set.isSubset code.Surely everyFolding) then
                    failwith
                        $"Surely %O{code.Surely} holds what some folding never imports; every folding imports %O{everyFolding}"

                if not (Set.isSubset someFolding code.Maybe) then
                    failwith $"Maybe %O{code.Maybe} omits what some folding imports: %O{someFolding}"

            true

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 3000) (Arb.fromGen genBody) property

        coverage.Count Shown.Enumerated |> shouldBeGreaterThan 2000
        coverage.Count Shown.SurelyFallsShort |> shouldBeGreaterThan 300
        coverage.Count Shown.SurelyPastABranch |> shouldBeGreaterThan 300

    [<Test>]
    let ``one instruction is imported before another exactly when every path to the other passes it`` () : unit =
        let property (cover : Shown -> unit) (steps : Step list, regions : Region list, optimises : bool) : bool =
            let body, at = layOut steps regions
            let folding = foldingOf steps at optimises
            let code = JitImport.compiledCode body folding
            let order = JitImport.importOrder body folding
            let stepArray = Array.ofList steps
            let kind = branchKinds optimises stepArray (landed steps regions)

            // The flow graph of what the JIT may import: every branch kept, but those it decides.
            let successors (index : int) : int list =
                match kind index with
                | Branch.Decided target -> [ target ]
                | Branch.Kept
                | Branch.MayFold -> allSuccessors stepArray index

            let indexOf (offset : int) = Array.findIndex ((=) offset) at

            for earlier in code.Maybe do
                let avoided =
                    reach stepArray regions successors (Some (indexOf earlier))
                    |> Set.map (fun index -> at.[index])

                for later in code.Maybe do
                    let expected = earlier <> later && not (avoided.Contains later)
                    let actual = JitImport.importedBefore order earlier later

                    if actual <> expected then
                        failwith
                            $"importedBefore %d{earlier} %d{later} is %b{actual}; removing %d{earlier} leaves %O{avoided} reachable"

                    if actual && indexOf later > indexOf earlier + 1 then
                        cover Shown.OrderedAcrossBlocks

            true

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 2000) (Arb.fromGen genBody) property

        coverage.Count Shown.OrderedAcrossBlocks |> shouldBeGreaterThan 500

    [<Test>]
    let ``a branch on a literal may fold, and one on an argument cannot`` () : unit =
        // ldc.i4.0 or ldarg.0; brtrue L; ret; L: nop; ret
        for push, surely in [ Step.Const false, false ; Step.LdArg (0, false), true ] do
            let steps = [ push ; Step.Brtrue 3 ; Step.Ret ; Step.Nop ; Step.Ret ]
            let body, at = layOut steps []
            let code = JitImport.compiledCode body (foldingOf steps at true)
            code.Maybe |> shouldEqual (Set.ofSeq at.[0..4])
            code.Surely.Contains at.[3] |> shouldEqual surely
            code.Surely.Contains at.[2] |> shouldEqual surely

            JitImport.compiledCode body JitFolding.Nothing
            |> shouldEqual
                {
                    Surely = Set.ofSeq at.[0..4]
                    Maybe = Set.ofSeq at.[0..4]
                }

    [<Test>]
    let ``a branch on a capability query goes one way where the JIT optimises`` () : unit =
        // call IsSupported; brtrue L; nop; ret; L: ret
        let steps =
            [ Step.CallCapability false ; Step.Brtrue 4 ; Step.Nop ; Step.Ret ; Step.Ret ]

        let body, at = layOut steps []

        JitImport.compiledCode body (foldingOf steps at true)
        |> shouldEqual
            {
                Surely = Set.ofSeq at.[0..3]
                Maybe = Set.ofSeq at.[0..3]
            }

        (JitImport.compiledCode body JitFolding.Nothing).Surely
        |> shouldEqual (Set.ofSeq at.[0..4])

    [<Test>]
    let ``a comparison of an argument with itself may fold`` () : unit =
        for second, surely in
            [
                Step.LdArg (0, true), false
                Step.LdArg (1, false), true
                Step.LdLoc (0, false), true
            ] do
            let steps = [ Step.LdArg (0, false) ; second ; Step.BneUn 4 ; Step.Ret ; Step.Ret ]
            let body, at = layOut steps []

            (JitImport.compiledCode body (foldingOf steps at true)).Surely.Contains at.[4]
            |> shouldEqual surely

    // ---- Against the real runtime ----

    /// A condition a generated method branches on.
    [<RequireQualifiedAccess>]
    type private Condition =
        | Argument of index : int
        | Local
        | Literal of bool
        /// `ldc.i4.1; ldc.i4.1; sub`.
        | Arithmetic
        | ArgumentAddress
        | OrdinaryCall
        /// `X86Base.IsSupported`.
        | X86
        /// `Vector128.IsHardwareAccelerated`.
        | Vector128
        | NewObject
        /// `ldarg.0; ldarg.0`, for `bne.un`.
        | SameArguments
        /// `ldarg.0; ldarg.1`, for `bne.un`.
        | TwoArguments

    [<RequireQualifiedAccess>]
    type private Site =
        /// `newarr Span<int>`: `TypeLoadException`.
        | NewarrSpan
        /// `call` of an abstract method: `BadImageFormatException`.
        | CallAbstract

    [<RequireQualifiedAccess>]
    type private Chunk =
        | At of Site
        /// Branch past the next `count` chunks of the same sequence when the condition holds (or
        /// does not, with `jumpWhenFalse`).
        | Skip of Condition * jumpWhenFalse : bool * count : int
        | Protected of body : Chunk list * handler : Chunk list
        /// `ldc.i4.0; ret`, outside any protected block.
        | Return

    [<RequireQualifiedAccess>]
    type private Optimisation =
        | Default
        | NoOptimization
        | AggressiveOptimization

    let rec private genChunks (depth : int) (protectedBlock : bool) : Gen<Chunk list> =
        gen {
            let! length = Gen.choose (1, 5)

            let condition =
                Gen.frequency
                    [
                        3, Gen.map Condition.Argument (Gen.choose (0, 1))
                        2, Gen.constant Condition.Local
                        3, Gen.elements [ Condition.Literal false ; Condition.Literal true ]
                        1, Gen.constant Condition.Arithmetic
                        1, Gen.constant Condition.ArgumentAddress
                        2, Gen.constant Condition.OrdinaryCall
                        2, Gen.elements [ Condition.X86 ; Condition.Vector128 ]
                        1, Gen.constant Condition.NewObject
                        1, Gen.constant Condition.SameArguments
                        2, Gen.constant Condition.TwoArguments
                    ]

            let chunk =
                Gen.frequency
                    [
                        yield 3, Gen.elements [ Chunk.At Site.NewarrSpan ; Chunk.At Site.CallAbstract ]
                        yield
                            5,
                            Gen.map3
                                (fun c f n -> Chunk.Skip (c, f, n))
                                condition
                                (Gen.elements [ false ; true ])
                                (Gen.choose (1, 3))
                        if depth < 2 then
                            yield
                                1,
                                Gen.map2
                                    (fun b h -> Chunk.Protected (b, h))
                                    (genChunks (depth + 1) true)
                                    (genChunks (depth + 1) true)
                        if not protectedBlock then
                            yield 1, Gen.constant Chunk.Return
                    ]

            return! Gen.listOfLength length chunk
        }

    let private genMethod : Gen<Chunk list * Optimisation> =
        Gen.map2
            (fun chunks optimisation -> chunks, optimisation)
            (genChunks 0 false)
            (Gen.frequency
                [
                    6, Gen.constant Optimisation.Default
                    2, Gen.constant Optimisation.NoOptimization
                    1, Gen.constant Optimisation.AggressiveOptimization
                ])

    /// An assembly whose `Generated.Runners` holds `static int M<i>(bool, bool)` for each method.
    let private fabricate (methods : (Chunk list * Optimisation) list) : byte[] =
        let span =
            typeof<obj>.Assembly.GetType("System.Span`1", true).MakeGenericType [| typeof<int> |]

        let builder =
            Emit.PersistedAssemblyBuilder (AssemblyName "Generated", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Generated"

        let abs =
            modul.DefineType ("Generated.Abs", TypeAttributes.Public ||| TypeAttributes.Abstract)

        let abstractMethod =
            abs.DefineMethod (
                "A",
                MethodAttributes.Public
                ||| MethodAttributes.Virtual
                ||| MethodAttributes.Abstract
                ||| MethodAttributes.HideBySig
                ||| MethodAttributes.NewSlot,
                typeof<int>,
                [||]
            )

        abs.CreateType () |> ignore<Type>

        let runners =
            modul.DefineType (
                "Generated.Runners",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let ordinary =
            let m =
                runners.DefineMethod (
                    "Ordinary",
                    MethodAttributes.Assembly ||| MethodAttributes.Static,
                    typeof<bool>,
                    [||]
                )

            m.SetImplementationFlags MethodImplAttributes.NoInlining
            let il = m.GetILGenerator ()
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret
            m

        let x86 =
            typeof<Runtime.Intrinsics.X86.X86Base>.GetProperty("IsSupported").GetMethod

        let vector128 =
            typeof<Runtime.Intrinsics.Vector128>.GetProperty("IsHardwareAccelerated").GetMethod

        let objectCtor = typeof<obj>.GetConstructor Type.EmptyTypes

        for index, (chunks, optimisation) in List.indexed methods do
            let m =
                runners.DefineMethod (
                    $"M%d{index}",
                    MethodAttributes.Public ||| MethodAttributes.Static,
                    typeof<int>,
                    [| typeof<bool> ; typeof<bool> |]
                )

            match optimisation with
            | Optimisation.Default -> ()
            | Optimisation.NoOptimization -> m.SetImplementationFlags MethodImplAttributes.NoOptimization
            | Optimisation.AggressiveOptimization ->
                m.SetImplementationFlags MethodImplAttributes.AggressiveOptimization

            let il = m.GetILGenerator ()
            il.DeclareLocal typeof<bool> |> ignore<Emit.LocalBuilder>

            let push (condition : Condition) =
                match condition with
                | Condition.Argument 0 -> il.Emit Emit.OpCodes.Ldarg_0
                | Condition.Argument _ -> il.Emit Emit.OpCodes.Ldarg_1
                | Condition.Local -> il.Emit Emit.OpCodes.Ldloc_0
                | Condition.Literal false -> il.Emit Emit.OpCodes.Ldc_I4_0
                | Condition.Literal true -> il.Emit Emit.OpCodes.Ldc_I4_1
                | Condition.Arithmetic ->
                    il.Emit Emit.OpCodes.Ldc_I4_1
                    il.Emit Emit.OpCodes.Ldc_I4_1
                    il.Emit Emit.OpCodes.Sub
                | Condition.ArgumentAddress -> il.Emit (Emit.OpCodes.Ldarga_S, 0uy)
                | Condition.OrdinaryCall -> il.Emit (Emit.OpCodes.Call, ordinary)
                | Condition.X86 -> il.Emit (Emit.OpCodes.Call, x86)
                | Condition.Vector128 -> il.Emit (Emit.OpCodes.Call, vector128)
                | Condition.NewObject -> il.Emit (Emit.OpCodes.Newobj, objectCtor)
                | Condition.SameArguments ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit Emit.OpCodes.Ldarg_0
                | Condition.TwoArguments ->
                    il.Emit Emit.OpCodes.Ldarg_0
                    il.Emit Emit.OpCodes.Ldarg_1

            let rec emit (chunks : Chunk list) =
                let labels = Array.init (chunks.Length + 1) (fun _ -> il.DefineLabel ())

                for index, chunk in List.indexed chunks do
                    il.MarkLabel labels.[index]

                    match chunk with
                    | Chunk.At Site.NewarrSpan ->
                        il.Emit Emit.OpCodes.Ldc_I4_1
                        il.Emit (Emit.OpCodes.Newarr, span)
                        il.Emit Emit.OpCodes.Pop
                    | Chunk.At Site.CallAbstract ->
                        il.Emit Emit.OpCodes.Ldnull
                        il.Emit (Emit.OpCodes.Call, abstractMethod)
                        il.Emit Emit.OpCodes.Pop
                    | Chunk.Skip (condition, jumpWhenFalse, count) ->
                        push condition
                        let target = labels.[min chunks.Length (index + 1 + count)]

                        match condition with
                        | Condition.SameArguments
                        | Condition.TwoArguments ->
                            il.Emit (
                                (if jumpWhenFalse then
                                     Emit.OpCodes.Beq
                                 else
                                     Emit.OpCodes.Bne_Un),
                                target
                            )
                        | _ ->
                            il.Emit (
                                (if jumpWhenFalse then
                                     Emit.OpCodes.Brfalse
                                 else
                                     Emit.OpCodes.Brtrue),
                                target
                            )
                    | Chunk.Protected (body, handler) ->
                        il.BeginExceptionBlock () |> ignore<Emit.Label>
                        emit body
                        il.BeginCatchBlock typeof<Exception>
                        il.Emit Emit.OpCodes.Pop
                        emit handler
                        il.EndExceptionBlock ()
                    | Chunk.Return ->
                        il.Emit Emit.OpCodes.Ldc_I4_0
                        il.Emit Emit.OpCodes.Ret

                il.MarkLabel labels.[chunks.Length]
                // A label at the very end of a sequence needs an instruction after it.
                il.Emit Emit.OpCodes.Nop

            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Stloc_0
            emit chunks
            il.Emit Emit.OpCodes.Ldc_I4_0
            il.Emit Emit.OpCodes.Ret

        runners.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// Calls each `M<i>` and writes what it did, one line each, to the file its argument names.
    let private driverSource : string =
        """
using System;
using System.IO;
using System.Reflection;

public static class Driver
{
    public static int Main(string[] args)
    {
        var runners = typeof(Generated.Runners);
        using var output = new StreamWriter(args[0]);
        for (var index = 0; ; index++)
        {
            var method = runners.GetMethod("M" + index);
            if (method == null) break;
            string outcome;
            try
            {
                outcome = "Returned";
                method.Invoke(null, new object[] { false, false });
            }
            catch (TargetInvocationException e)
            {
                outcome = e.InnerException.GetType().Name;
            }
            output.WriteLine(outcome);
        }
        return 0;
    }
}
"""

    /// What `JitImport` predicts compiling a generated method does.
    [<RequireQualifiedAccess>]
    type private Prediction =
        | Returns
        /// Raises this. `contested` where a site raising something else is compiled too.
        | Raises of exceptionName : string * contested : bool
        | Undecided

    let private exceptionOf (site : Site) : string =
        match site with
        | Site.NewarrSpan -> "TypeLoadException"
        | Site.CallAbstract -> "BadImageFormatException"

    /// The prediction for a method's body, given what each capability query answers here.
    let private predict
        (fixture : DumpedAssembly)
        (body : MethodInstructions<TypeDefn>)
        (optimisation : Optimisation)
        : Prediction
        =
        let callee (op : IlOp) : string option =
            match op with
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, MetadataOperand.FromMetadata sourced) ->
                match sourced.Token with
                | MetadataToken.MethodDef h -> Some fixture.Methods.[h].Name
                | MetadataToken.MemberReference h -> Some fixture.Members.[h].PrettyName
                | other -> failwith $"unexpected call operand %O{other}"
            | _ -> None

        let calls =
            body.Instructions
            |> List.choose (fun (op, offset) -> callee op |> Option.map (fun name -> offset, name))

        let folding =
            match optimisation with
            | Optimisation.NoOptimization -> JitFolding.Nothing
            | Optimisation.Default
            | Optimisation.AggressiveOptimization ->
                JitFolding.Optimises
                    {
                        Capabilities =
                            calls
                            |> List.choose (fun (offset, name) ->
                                match name with
                                | "get_IsSupported" -> Some (offset, Runtime.Intrinsics.X86.X86Base.IsSupported)
                                | "get_IsHardwareAccelerated" ->
                                    Some (offset, Runtime.Intrinsics.Vector128.IsHardwareAccelerated)
                                | _ -> None
                            )
                            |> Map.ofList
                        OrdinaryCalls =
                            calls
                            |> List.choose (fun (offset, name) -> if name = "Ordinary" then Some offset else None)
                            |> Set.ofList
                    }

        let sites =
            body.Instructions
            |> List.choose (fun (op, offset) ->
                match op with
                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, _) -> Some (offset, Site.NewarrSpan)
                | _ ->
                    match callee op with
                    | Some "A" -> Some (offset, Site.CallAbstract)
                    | _ -> None
            )

        let code = JitImport.compiledCode body folding
        let order = JitImport.importOrder body folding
        let compiled = sites |> List.filter (fun (offset, _) -> code.Maybe.Contains offset)

        if compiled.IsEmpty then
            Prediction.Returns
        else

        let first =
            compiled
            |> List.tryFind (fun (offset, site) ->
                code.Surely.Contains offset
                && compiled
                   |> List.forall (fun (other, otherSite) ->
                       other = offset
                       || exceptionOf otherSite = exceptionOf site
                       || JitImport.importedBefore order offset other
                   )
            )

        match first with
        | Some (_, site) ->
            let contested =
                compiled
                |> List.exists (fun (_, other) -> exceptionOf other <> exceptionOf site)

            Prediction.Raises (exceptionOf site, contested)
        | None -> Prediction.Undecided

    [<RequireQualifiedAccess>]
    type private Decided =
        | Returns
        | Raises
        /// Raises one site's exception where another, with a different exception, is compiled too.
        | Ordered
        | Undecided

    [<Test>]
    let ``the real runtime compiles what JitImport says, in the order it says`` () : unit =
        let methods = Gen.sampleWithSeed (Rnd 20261024UL) 20 600 genMethod |> List.ofArray

        let image = fabricate methods
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "Generated.dll") (new MemoryStream (image))

        let runners = fixture.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = "Runners")

        let driver =
            Roslyn.compileAssembly
                "GeneratedDriver"
                OutputKind.ConsoleApplication
                [ MetadataReference.CreateFromImage (ImmutableArray.CreateRange image) ]
                [ driverSource ]

        let directory = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory directory |> ignore<DirectoryInfo>

        let observed =
            try
                File.WriteAllBytes (Path.Combine (directory, "Generated.dll"), image)
                let driverPath = Path.Combine (directory, "GeneratedDriver.dll")
                File.WriteAllBytes (driverPath, driver)
                let results = Path.Combine (directory, "results.txt")

                match RealRuntime.executeAssemblyInPlace [| results |] driverPath with
                | RealRuntimeResult.NormalExit 0 -> File.ReadAllLines results
                | other -> failwith $"the driver did not run to completion: %O{other}"
            finally
                RealRuntime.deleteScratchTree directory

        observed.Length |> shouldEqual methods.Length

        let decided = Collections.Generic.Dictionary<Decided, int> ()

        let count (d : Decided) =
            decided.[d] <-
                (match decided.TryGetValue d with
                 | true, n -> n
                 | false, _ -> 0)
                + 1

        let mismatches =
            [
                for index, (chunks, optimisation) in List.indexed methods do
                    let method = runners.Methods |> List.find (fun m -> m.Name = $"M%d{index}")

                    let body =
                        MethodInfo.tryIlBody method
                        |> Option.defaultWith (fun () -> failwith $"M%d{index} has no IL body")

                    let prediction = predict fixture body optimisation

                    match prediction with
                    | Prediction.Undecided -> count Decided.Undecided
                    | Prediction.Returns -> count Decided.Returns
                    | Prediction.Raises (_, contested) ->
                        count Decided.Raises

                        if contested then
                            count Decided.Ordered

                    let expected =
                        match prediction with
                        | Prediction.Undecided -> None
                        | Prediction.Returns -> Some "Returned"
                        | Prediction.Raises (e, _) -> Some e

                    match expected with
                    | Some e when e <> observed.[index] ->
                        yield
                            $"M%d{index} (%A{optimisation}): predicted %s{e}, observed %s{observed.[index]}\n%A{chunks}"
                    | _ -> ()
            ]

        if not mismatches.IsEmpty then
            failwith (String.Join ("\n\n", mismatches))

        let counted (d : Decided) =
            match decided.TryGetValue d with
            | true, n -> n
            | false, _ -> 0

        TestContext.Progress.WriteLine
            $"decided: returns %d{counted Decided.Returns}, raises %d{counted Decided.Raises}, ordered %d{counted Decided.Ordered}, undecided %d{counted Decided.Undecided}"

        counted Decided.Returns |> shouldBeGreaterThan 60
        counted Decided.Raises |> shouldBeGreaterThan 150
        counted Decided.Ordered |> shouldBeGreaterThan 20
