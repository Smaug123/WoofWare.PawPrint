namespace WoofWare.PawPrint

open System.Collections.Immutable

/// The width of a floating-point value in an evaluation-stack slot: CoreCLR's `TYP_FLOAT` and
/// `TYP_DOUBLE`.
[<RequireQualifiedAccess>]
type FloatWidth =
    | Single
    | Double

/// What the stack-shape analysis tracks about one evaluation-stack slot.
///
/// Only floating-point slots carry information. Their width at a control-flow join is a fact
/// CoreCLR's importer decides statically over every incoming path, and the one kind of fact an
/// interpreter cannot recover from the path that happened to execute.
[<RequireQualifiedAccess>]
type SlotShape =
    | Float of FloatWidth
    /// Anything that is not a float: an integer, a reference, a pointer, a value type.
    | Other

/// What a token-bearing instruction does to the stack, as read from its token by whoever can
/// read it: a signature blob for a body from a PE image, a `DynamicScope` entry for a body minted
/// by `Reflection.Emit`.
[<RequireQualifiedAccess>]
type TokenShape =
    /// `call`, `callvirt`, `newobj` and `calli`: how many values the callee takes from the stack,
    /// and what it leaves. `arguments` includes `this` for an instance method, and excludes the
    /// function pointer `calli` pops after them; for `newobj` it is the constructor's parameters
    /// alone, since the object is made rather than passed.
    | Callee of arguments : int * returns : SlotShape option
    /// The type of the field a `ldfld` or `ldsfld` pushes.
    | Field of SlotShape
    /// The type an `ldobj`, `unbox.any` or `ldelem` pushes.
    | Type of SlotShape

/// Everything the analysis needs beyond the body itself.
type StackShapeInputs =
    {
        /// The shape `ldarg n` pushes, with `this` at index 0 for an instance method.
        Arguments : ImmutableArray<SlotShape>
        /// The shape `ldloc n` pushes.
        Locals : ImmutableArray<SlotShape>
        /// Whether `ret` pops a value.
        ReturnsValue : bool
        /// For every instruction whose stack effect depends on its token, that effect, keyed by the
        /// instruction's offset. An instruction that needs one and has none is an analysis error.
        Tokens : Map<int, TokenShape>
    }

/// The shape of the evaluation stack at the entry of every reachable instruction.
type StackShape =
    {
        /// The stack on entry to each reachable offset the analysis could type, top first.
        Entry : Map<int, SlotShape list>
        /// The reachable offsets the analysis could not type, and why. An instruction that
        /// underflows, or a join two paths reach with stacks that cannot meet, is recorded here
        /// rather than failing the whole body: CoreCLR's importer refuses it only if it imports
        /// it. What follows only from such an offset, joins it feeds included, is in `Reachable`
        /// but in neither `Entry` nor here.
        Invalid : Map<int, StackShapeError>
        /// Every offset control can reach in the body's graph, typed or not; see
        /// `StackFlow.reachable`.
        Reachable : Set<int>
        /// The offsets at which a float32 arriving in the listed slots (counted from the top of the
        /// stack, `0` being the top) must be widened to double, because the slot's shape over
        /// every incoming path is `Double`. This is the cast CoreCLR's importer inserts on the
        /// float32 predecessors of a spill clique it has typed as double.
        Promotions : Map<int, int list>
        /// The offsets at which CoreCLR's importer starts a basic block: the entry, every branch
        /// target, every handler entry, and the instruction after a conditional branch or
        /// `switch`. A value on the stack on entry to one arrives through a spill temp, whose
        /// width the importer decides over every path into the temp's clique.
        BlockStarts : Set<int>
    }


[<RequireQualifiedAccess>]
module StackShape =

    /// The width CoreCLR gives a binary arithmetic result: single precision only when both
    /// operands are single, otherwise double, with the float32 operand widened first. Anything
    /// that is not a float operand makes the result not a float.
    let private arithmetic (popped : SlotShape list) : SlotShape =
        match popped with
        | [ SlotShape.Float FloatWidth.Single ; SlotShape.Float FloatWidth.Single ] -> SlotShape.Float FloatWidth.Single
        | [ SlotShape.Float _ ; SlotShape.Float _ ] -> SlotShape.Float FloatWidth.Double
        | _ -> SlotShape.Other

    /// The join of two slot shapes, or the reason they cannot meet.
    let private join (offset : int) (slotFromTop : int) (existing : SlotShape) (incoming : SlotShape) =
        match existing, incoming with
        | SlotShape.Other, SlotShape.Other -> Ok SlotShape.Other
        | SlotShape.Float FloatWidth.Single, SlotShape.Float FloatWidth.Single -> Ok existing
        | SlotShape.Float _, SlotShape.Float _ -> Ok (SlotShape.Float FloatWidth.Double)
        | SlotShape.Float _, SlotShape.Other
        | SlotShape.Other, SlotShape.Float _ -> Error (StackShapeError.FloatMergedWithOther (offset, slotFromTop))

    /// Float widths as a slot lattice: a value is a float of the width CoreCLR's importer gives
    /// it, or not a float, and a token-bearing instruction pushes what `inputs.Tokens` says.
    let lattice (inputs : StackShapeInputs) : SlotLattice<SlotShape> =
        let fixedShape (shape : SlotShape) : Result<SlotShape list -> SlotShape, StackShapeError> = Ok (fun _ -> shape)

        let push (offset : int) (instruction : IlOp) (pushed : Pushed) =
            let fromToken (select : TokenShape -> SlotShape option) =
                match Map.tryFind offset inputs.Tokens |> Option.bind select with
                | Some shape -> fixedShape shape
                | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))

            match pushed with
            | Pushed.Argument index -> fixedShape inputs.Arguments.[index]
            | Pushed.Local index -> fixedShape inputs.Locals.[index]
            | Pushed.Operand fromTop -> Ok (fun popped -> popped.[fromTop])
            | Pushed.Arithmetic -> Ok arithmetic
            | Pushed.Number StackNumber.Float32 -> fixedShape (SlotShape.Float FloatWidth.Single)
            | Pushed.Number StackNumber.Float64 -> fixedShape (SlotShape.Float FloatWidth.Double)
            | Pushed.Number StackNumber.Int32
            | Pushed.Number StackNumber.Int64
            | Pushed.Number StackNumber.NativeInt
            | Pushed.Bitwise
            | Pushed.Null
            | Pushed.String
            | Pushed.Address
            | Pushed.Indirect
            | Pushed.Element
            | Pushed.ArgumentHandle
            | Pushed.TypedReferenceType -> fixedShape SlotShape.Other
            | Pushed.FromToken TokenValue.CallResult ->
                fromToken (
                    function
                    | TokenShape.Callee (_, returns) -> returns
                    | TokenShape.Field _
                    | TokenShape.Type _ -> None
                )
            | Pushed.FromToken TokenValue.Field ->
                fromToken (
                    function
                    | TokenShape.Field shape -> Some shape
                    | TokenShape.Callee _
                    | TokenShape.Type _ -> None
                )
            | Pushed.FromToken TokenValue.Loaded ->
                fromToken (
                    function
                    | TokenShape.Type shape -> Some shape
                    | TokenShape.Callee _
                    | TokenShape.Field _ -> None
                )
            | Pushed.FromToken TokenValue.NewObject
            | Pushed.FromToken TokenValue.Cast
            | Pushed.FromToken TokenValue.Boxed
            | Pushed.FromToken TokenValue.NewArray
            | Pushed.FromToken TokenValue.Handle
            | Pushed.FromToken TokenValue.MethodPointer
            | Pushed.FromToken TokenValue.TypedReference -> fixedShape SlotShape.Other

        {
            Push = push
            Join = join
            Caught = fun _ -> SlotShape.Other
            WideningConverts = true
        }

    /// What the stack effects need of `inputs`: the counts, and each callee's arity.
    let effectInputs (inputs : StackShapeInputs) : StackEffectInputs =
        {
            Arguments = inputs.Arguments.Length
            Locals = inputs.Locals.Length
            ReturnsValue = inputs.ReturnsValue
            Callees =
                inputs.Tokens
                |> Map.toSeq
                |> Seq.choose (fun (offset, shape) ->
                    match shape with
                    | TokenShape.Callee (arguments, returns) ->
                        Some (
                            offset,
                            {
                                Arguments = arguments
                                Returns = returns.IsSome
                            }
                        )
                    | TokenShape.Field _
                    | TokenShape.Type _ -> None
                )
                |> Map.ofSeq
        }

    /// Compute the shape of the evaluation stack at the entry of every instruction reachable
    /// from the method's entry or from a handler entry, joining over every path. An instruction
    /// that cannot be typed is recorded in `Invalid` and delivers nothing. A join that two paths
    /// reach with stacks that cannot meet is recorded as a conflict and delivers an unknown
    /// stack, which propagates: what follows only from the join is left untyped, a join it feeds
    /// is untyped too rather than classified from its other arms, and so is every offset sharing
    /// one of its spill temps. A join that would be a promotion, but which a branch the importer
    /// may fold reaches, is recorded as `WidthDependsOnFoldedBranch` and propagates as a conflict
    /// does.
    let analyse (inputs : StackShapeInputs) (body : MethodInstructions<'methodVars>) : StackShape =
        let flow = StackFlow.analyse (lattice inputs) (effectInputs inputs) body

        {
            Entry = flow.Entry
            Invalid = flow.Invalid
            Reachable = flow.Reachable
            // A float join widens only a float32 meeting a double, so every widening is the
            // promotion CoreCLR's importer inserts.
            Promotions = flow.Widened
            BlockStarts = flow.BlockStarts
        }
