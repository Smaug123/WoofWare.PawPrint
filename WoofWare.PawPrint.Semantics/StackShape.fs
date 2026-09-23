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

/// Why an instruction could not be given an entry shape. Each is a claim that CoreCLR's importer
/// would refuse the instruction if it imported it, except where noted. The importer imports only
/// what it reaches, and unless it compiles the body as debuggable code it folds some branches (on
/// literals, on an intrinsic such as `IsSupported`, on a `typeof` comparison), never importing the
/// arm it drops, so an
/// instruction the analysis reaches may never be imported: the analysis therefore records the
/// instruction rather than rejecting the body, and the interpreter refuses only its execution.
[<RequireQualifiedAccess>]
type StackShapeError =
    /// An instruction pops more than the stack holds on entry to it.
    | StackUnderflow of offset : int * instruction : IlOp * depth : int
    /// Two paths reach an offset with stacks of different depths.
    | DepthMismatch of offset : int * existing : int * incoming : int
    /// Two paths reach an offset with a float and a non-float in the same slot, counted from the
    /// top of the stack.
    | FloatMergedWithOther of offset : int * slotFromTop : int
    /// A token-bearing instruction whose effect `StackShapeInputs.Tokens` does not supply. Not a
    /// claim about the IL: the caller could not read the token, and the instruction itself
    /// decides what happens when it runs. What follows it is unknown rather than unreached.
    | MissingTokenShape of offset : int * instruction : IlOp
    /// A branch whose target is not the offset of any instruction.
    | BranchOutsideBody of offset : int * target : int
    /// `ldarg` or `starg` of an index the signature does not have.
    | ArgumentOutOfRange of offset : int * index : int
    /// `ldloc` or `stloc` of an index the locals signature does not have.
    | LocalOutOfRange of offset : int * index : int
    /// A join at which a float32 meets a double, downstream of the conditional branch or
    /// `switch` at `branch`, whose operands its block computes from values the importer may hold
    /// as constants: literals, static fields, and calls on such values or on none. Not a claim
    /// about the IL: the JIT folds such a branch at every tier but not in debuggable code,
    /// importing only the arm taken, so whether CoreCLR widens the join depends on how it
    /// compiled the body, which the analysis does not decide. What follows only from the join is
    /// unknown rather than typed.
    | WidthDependsOnFoldedBranch of offset : int * branch : int

    /// Whether the error is about two paths disagreeing rather than about the instruction on
    /// its own. A disagreement can be an artefact of following an arm CoreCLR's importer would
    /// never import, so it does not prove the join cannot run; an instruction that underflows
    /// on every path cannot.
    member this.IsConflict : bool =
        match this with
        | StackShapeError.DepthMismatch _
        | StackShapeError.FloatMergedWithOther _ -> true
        | StackShapeError.StackUnderflow _
        | StackShapeError.MissingTokenShape _
        | StackShapeError.BranchOutsideBody _
        | StackShapeError.ArgumentOutOfRange _
        | StackShapeError.LocalOutOfRange _
        | StackShapeError.WidthDependsOnFoldedBranch _ -> false

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
        /// `StackShape.reachable`.
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

    /// Where control goes after an instruction, given the stack it leaves.
    [<RequireQualifiedAccess>]
    type private Successors =
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

    /// What an instruction does to the stack: how many values it pops and what it pushes.
    /// `pushes` is a function of the popped values (top first) because an arithmetic result's
    /// width depends on its operands' and `dup` reproduces its operand.
    type private Effect =
        {
            Pops : int
            Pushes : SlotShape list -> SlotShape list
        }

    let private effect (pops : int) (pushes : SlotShape list) : Effect =
        {
            Pops = pops
            Pushes = fun _ -> pushes
        }

    /// Where control goes after the instruction at `offset`, as byte offsets into the body.
    let private successorsOf (offset : int) (instruction : IlOp) : Successors =
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

    /// The width CoreCLR gives a binary arithmetic result: single precision only when both
    /// operands are single, otherwise double, with the float32 operand widened first. Anything
    /// that is not a float operand makes the result not a float.
    let private arithmetic (popped : SlotShape list) : SlotShape list =
        match popped with
        | [ SlotShape.Float FloatWidth.Single ; SlotShape.Float FloatWidth.Single ] ->
            [ SlotShape.Float FloatWidth.Single ]
        | [ SlotShape.Float _ ; SlotShape.Float _ ] -> [ SlotShape.Float FloatWidth.Double ]
        | _ -> [ SlotShape.Other ]

    let private binaryArithmetic : Effect =
        {
            Pops = 2
            Pushes = arithmetic
        }

    /// `neg` and `ckfinite` return their operand's shape.
    let private unaryPreserving : Effect =
        {
            Pops = 1
            Pushes = id
        }

    /// An instruction that only pops: a branch, a `leave`, or one that ends the method.
    let private pops (count : int) : Effect = effect count []

    let private single : SlotShape = SlotShape.Float FloatWidth.Single
    let private double : SlotShape = SlotShape.Float FloatWidth.Double
    let private other : SlotShape = SlotShape.Other

    let private nullaryEffect
        (inputs : StackShapeInputs)
        (offset : int)
        (next : IlOp option)
        (op : NullaryIlOp)
        : Result<Effect, StackShapeError>
        =
        let argument (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments.Length then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 0 [ inputs.Arguments.[index] ])

        let local (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals.Length then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 0 [ inputs.Locals.[index] ])

        let storeLocal (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals.Length then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 1 [])

        match op with
        | NullaryIlOp.Nop
        | NullaryIlOp.Break
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly -> Ok (effect 0 [])
        | NullaryIlOp.LdArg0 -> argument 0
        | NullaryIlOp.LdArg1 -> argument 1
        | NullaryIlOp.LdArg2 -> argument 2
        | NullaryIlOp.LdArg3 -> argument 3
        | NullaryIlOp.Ldloc_0 -> local 0
        | NullaryIlOp.Ldloc_1 -> local 1
        | NullaryIlOp.Ldloc_2 -> local 2
        | NullaryIlOp.Ldloc_3 -> local 3
        | NullaryIlOp.Stloc_0 -> storeLocal 0
        | NullaryIlOp.Stloc_1 -> storeLocal 1
        | NullaryIlOp.Stloc_2 -> storeLocal 2
        | NullaryIlOp.Stloc_3 -> storeLocal 3
        | NullaryIlOp.Pop -> Ok (effect 1 [])
        | NullaryIlOp.Dup ->
            Ok
                {
                    Pops = 1
                    Pushes = fun popped -> popped @ popped
                }
        | NullaryIlOp.Ret -> Ok (pops (if inputs.ReturnsValue then 1 else 0))
        | NullaryIlOp.LdcI4_0
        | NullaryIlOp.LdcI4_1
        | NullaryIlOp.LdcI4_2
        | NullaryIlOp.LdcI4_3
        | NullaryIlOp.LdcI4_4
        | NullaryIlOp.LdcI4_5
        | NullaryIlOp.LdcI4_6
        | NullaryIlOp.LdcI4_7
        | NullaryIlOp.LdcI4_8
        | NullaryIlOp.LdcI4_m1
        | NullaryIlOp.LdNull
        | NullaryIlOp.Arglist -> Ok (effect 0 [ other ])
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un -> Ok (effect 2 [ other ])
        | NullaryIlOp.Add
        | NullaryIlOp.Add_ovf
        | NullaryIlOp.Add_ovf_un
        | NullaryIlOp.Sub
        | NullaryIlOp.Sub_ovf
        | NullaryIlOp.Sub_ovf_un
        | NullaryIlOp.Mul
        | NullaryIlOp.Mul_ovf
        | NullaryIlOp.Mul_ovf_un
        | NullaryIlOp.Div
        | NullaryIlOp.Div_un
        | NullaryIlOp.Rem
        | NullaryIlOp.Rem_un -> Ok binaryArithmetic
        | NullaryIlOp.And
        | NullaryIlOp.Or
        | NullaryIlOp.Xor
        | NullaryIlOp.Shl
        | NullaryIlOp.Shr
        | NullaryIlOp.Shr_un -> Ok (effect 2 [ other ])
        | NullaryIlOp.Neg
        | NullaryIlOp.Ckfinite -> Ok unaryPreserving
        | NullaryIlOp.Not
        | NullaryIlOp.Conv_I
        | NullaryIlOp.Conv_I1
        | NullaryIlOp.Conv_I2
        | NullaryIlOp.Conv_I4
        | NullaryIlOp.Conv_I8
        | NullaryIlOp.Conv_U
        | NullaryIlOp.Conv_U1
        | NullaryIlOp.Conv_U2
        | NullaryIlOp.Conv_U4
        | NullaryIlOp.Conv_U8
        | NullaryIlOp.Conv_ovf_i
        | NullaryIlOp.Conv_ovf_u
        | NullaryIlOp.Conv_ovf_i1
        | NullaryIlOp.Conv_ovf_i2
        | NullaryIlOp.Conv_ovf_i4
        | NullaryIlOp.Conv_ovf_i8
        | NullaryIlOp.Conv_ovf_u1
        | NullaryIlOp.Conv_ovf_u2
        | NullaryIlOp.Conv_ovf_u4
        | NullaryIlOp.Conv_ovf_u8
        | NullaryIlOp.Conv_ovf_i_un
        | NullaryIlOp.Conv_ovf_u_un
        | NullaryIlOp.Conv_ovf_i1_un
        | NullaryIlOp.Conv_ovf_u1_un
        | NullaryIlOp.Conv_ovf_i2_un
        | NullaryIlOp.Conv_ovf_u2_un
        | NullaryIlOp.Conv_ovf_i4_un
        | NullaryIlOp.Conv_ovf_u4_un
        | NullaryIlOp.Conv_ovf_i8_un
        | NullaryIlOp.Conv_ovf_u8_un
        | NullaryIlOp.LdLen
        | NullaryIlOp.Localloc
        | NullaryIlOp.Refanytype -> Ok (effect 1 [ other ])
        | NullaryIlOp.Conv_R4 -> Ok (effect 1 [ single ])
        | NullaryIlOp.Conv_R8 -> Ok (effect 1 [ double ])
        | NullaryIlOp.Conv_r_un ->
            // There is no `conv.r4.un`, so compilers emit `conv.r.un; conv.r4` for an unsigned
            // source cast to float32, and CoreCLR's importer types this result as float32 when
            // the next opcode is `conv.r4` (`CEE_CONV_R_UN` in importer.cpp). The `conv.r4` that
            // follows then finds a float32 and leaves it single.
            match next with
            | Some (IlOp.Nullary NullaryIlOp.Conv_R4) -> Ok (effect 1 [ single ])
            | _ -> Ok (effect 1 [ double ])
        | NullaryIlOp.Endfilter -> Ok (pops 1)
        | NullaryIlOp.Endfinally
        | NullaryIlOp.Rethrow -> Ok (pops 0)
        | NullaryIlOp.Throw -> Ok (pops 1)
        | NullaryIlOp.Ldind_ref
        | NullaryIlOp.Ldind_i
        | NullaryIlOp.Ldind_i1
        | NullaryIlOp.Ldind_i2
        | NullaryIlOp.Ldind_i4
        | NullaryIlOp.Ldind_i8
        | NullaryIlOp.Ldind_u1
        | NullaryIlOp.Ldind_u2
        | NullaryIlOp.Ldind_u4
        | NullaryIlOp.Ldind_u8 -> Ok (effect 1 [ other ])
        | NullaryIlOp.Ldind_r4 -> Ok (effect 1 [ single ])
        | NullaryIlOp.Ldind_r8 -> Ok (effect 1 [ double ])
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8 -> Ok (effect 2 [])
        | NullaryIlOp.Ldelem_i
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_u4
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_u8
        | NullaryIlOp.Ldelem_ref -> Ok (effect 2 [ other ])
        | NullaryIlOp.Ldelem_r4 -> Ok (effect 2 [ single ])
        | NullaryIlOp.Ldelem_r8 -> Ok (effect 2 [ double ])
        | NullaryIlOp.Stelem_i
        | NullaryIlOp.Stelem_i1
        | NullaryIlOp.Stelem_u1
        | NullaryIlOp.Stelem_i2
        | NullaryIlOp.Stelem_u2
        | NullaryIlOp.Stelem_i4
        | NullaryIlOp.Stelem_u4
        | NullaryIlOp.Stelem_i8
        | NullaryIlOp.Stelem_u8
        | NullaryIlOp.Stelem_r4
        | NullaryIlOp.Stelem_r8
        | NullaryIlOp.Stelem_ref
        | NullaryIlOp.Cpblk
        | NullaryIlOp.Initblk -> Ok (effect 3 [])

    let private unaryConstEffect
        (inputs : StackShapeInputs)
        (offset : int)
        (size : int)
        (op : UnaryConstIlOp)
        : Result<Effect, StackShapeError>
        =
        let argument (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments.Length then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 0 [ inputs.Arguments.[index] ])

        let storeArgument (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments.Length then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 1 [])

        let local (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals.Length then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 0 [ inputs.Locals.[index] ])

        let storeLocal (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals.Length then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 1 [])

        match op with
        | UnaryConstIlOp.Stloc i -> storeLocal (int i)
        | UnaryConstIlOp.Stloc_s i -> storeLocal (int i)
        | UnaryConstIlOp.Ldloc i -> local (int i)
        | UnaryConstIlOp.Ldloc_s i -> local (int i)
        | UnaryConstIlOp.Ldarg i -> argument (int i)
        | UnaryConstIlOp.Ldarg_s i -> argument (int i)
        | UnaryConstIlOp.Starg i -> storeArgument (int i)
        | UnaryConstIlOp.Starg_s i -> storeArgument (int i)
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_I4_s _ -> Ok (effect 0 [ other ])
        | UnaryConstIlOp.Ldc_R4 _ -> Ok (effect 0 [ single ])
        | UnaryConstIlOp.Ldc_R8 _ -> Ok (effect 0 [ double ])
        | UnaryConstIlOp.Br delta -> Ok (pops 0)
        | UnaryConstIlOp.Br_s delta -> Ok (pops 0)
        | UnaryConstIlOp.Brfalse delta
        | UnaryConstIlOp.Brtrue delta -> Ok (pops 1)
        | UnaryConstIlOp.Brfalse_s delta
        | UnaryConstIlOp.Brtrue_s delta -> Ok (pops 1)
        | UnaryConstIlOp.Beq delta
        | UnaryConstIlOp.Blt delta
        | UnaryConstIlOp.Ble delta
        | UnaryConstIlOp.Bgt delta
        | UnaryConstIlOp.Bge delta
        | UnaryConstIlOp.Bne_un delta
        | UnaryConstIlOp.Bge_un delta
        | UnaryConstIlOp.Bgt_un delta
        | UnaryConstIlOp.Ble_un delta
        | UnaryConstIlOp.Blt_un delta -> Ok (pops 2)
        | UnaryConstIlOp.Beq_s delta
        | UnaryConstIlOp.Blt_s delta
        | UnaryConstIlOp.Ble_s delta
        | UnaryConstIlOp.Bgt_s delta
        | UnaryConstIlOp.Bge_s delta
        | UnaryConstIlOp.Bne_un_s delta
        | UnaryConstIlOp.Bge_un_s delta
        | UnaryConstIlOp.Bgt_un_s delta
        | UnaryConstIlOp.Ble_un_s delta
        | UnaryConstIlOp.Blt_un_s delta -> Ok (pops 2)
        | UnaryConstIlOp.Leave _ -> Ok (pops 0)
        | UnaryConstIlOp.Leave_s _ -> Ok (pops 0)
        | UnaryConstIlOp.Unaligned _ -> Ok (effect 0 [])

    let private tokenEffect
        (inputs : StackShapeInputs)
        (offset : int)
        (instruction : IlOp)
        (op : UnaryMetadataTokenIlOp)
        : Result<Effect, StackShapeError>
        =
        let token () : Result<TokenShape, StackShapeError> =
            match Map.tryFind offset inputs.Tokens with
            | Some shape -> Ok shape
            | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))

        let callee (extraPops : int) (pushesResult : bool) : Result<Effect, StackShapeError> =
            match token () with
            | Error e -> Error e
            | Ok (TokenShape.Callee (arguments, returns)) ->
                let pushes =
                    match returns with
                    | Some shape when pushesResult -> [ shape ]
                    | _ -> []

                Ok (effect (arguments + extraPops) pushes)
            | Ok other ->
                // The table describes a field or a type where a method was needed: a caller
                // bug, reported as the missing entry it amounts to.
                Error (StackShapeError.MissingTokenShape (offset, instruction))

        let pushesFromToken (pops : int) (select : TokenShape -> SlotShape option) : Result<Effect, StackShapeError> =
            match token () with
            | Error e -> Error e
            | Ok shape ->
                match select shape with
                | Some pushed -> Ok (effect pops [ pushed ])
                | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))

        let fieldShape (shape : TokenShape) : SlotShape option =
            match shape with
            | TokenShape.Field s -> Some s
            | _ -> None

        let typeShape (shape : TokenShape) : SlotShape option =
            match shape with
            | TokenShape.Type s -> Some s
            | _ -> None

        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt -> callee 0 true
        | UnaryMetadataTokenIlOp.Calli -> callee 1 true
        | UnaryMetadataTokenIlOp.Newobj ->
            match token () with
            | Error e -> Error e
            | Ok (TokenShape.Callee (arguments, _)) -> Ok (effect arguments [ other ])
            | Ok _ -> Error (StackShapeError.MissingTokenShape (offset, instruction))
        | UnaryMetadataTokenIlOp.Jmp ->
            // `jmp` transfers to the target with the caller's own arguments; the stack must be
            // empty and nothing follows.
            Ok (pops 0)
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> Ok (effect 1 [ other ])
        | UnaryMetadataTokenIlOp.Ldelema -> Ok (effect 2 [ other ])
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj -> Ok (effect 2 [])
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Initobj -> Ok (effect 1 [])
        | UnaryMetadataTokenIlOp.Stelem -> Ok (effect 3 [])
        | UnaryMetadataTokenIlOp.Ldfld -> pushesFromToken 1 fieldShape
        | UnaryMetadataTokenIlOp.Ldsfld -> pushesFromToken 0 fieldShape
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Unbox_Any -> pushesFromToken 1 typeShape
        | UnaryMetadataTokenIlOp.Ldelem -> pushesFromToken 2 typeShape
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Sizeof -> Ok (effect 0 [ other ])
        | UnaryMetadataTokenIlOp.Constrained -> Ok (effect 0 [])

    let private effectOf
        (inputs : StackShapeInputs)
        (locations : Map<int, IlOp>)
        (offset : int)
        (instruction : IlOp)
        : Result<Effect, StackShapeError>
        =
        let size = IlOp.NumberOfBytes instruction

        match instruction with
        | IlOp.Nullary op -> nullaryEffect inputs offset (Map.tryFind (offset + size) locations) op
        | IlOp.UnaryConst op -> unaryConstEffect inputs offset size op
        | IlOp.UnaryMetadataToken (op, _) -> tokenEffect inputs offset instruction op
        | IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, _) -> Ok (effect 0 [ other ])
        | IlOp.Switch _ -> Ok (pops 1)

    /// The join of two slot shapes, or the reason they cannot meet.
    let private join (offset : int) (slotFromTop : int) (existing : SlotShape) (incoming : SlotShape) =
        match existing, incoming with
        | SlotShape.Other, SlotShape.Other -> Ok SlotShape.Other
        | SlotShape.Float FloatWidth.Single, SlotShape.Float FloatWidth.Single -> Ok existing
        | SlotShape.Float _, SlotShape.Float _ -> Ok (SlotShape.Float FloatWidth.Double)
        | SlotShape.Float _, SlotShape.Other
        | SlotShape.Other, SlotShape.Float _ -> Error (StackShapeError.FloatMergedWithOther (offset, slotFromTop))

    /// The entry stacks CoreCLR gives handler code, which no instruction jumps to. A catch or
    /// filter handler starts with the exception on the stack; so does the filter's own code. A
    /// finally or fault handler starts empty.
    let private handlerEntries (regions : ImmutableArray<ExceptionRegion>) : (int * SlotShape list) list =
        regions
        |> Seq.collect (fun region ->
            match region with
            | ExceptionRegion.Catch (_, offsets) -> [ offsets.HandlerOffset, [ other ] ]
            | ExceptionRegion.Filter (filterOffset, offsets) ->
                [ filterOffset, [ other ] ; offsets.HandlerOffset, [ other ] ]
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
    type private Cliques () =
        let parent = System.Collections.Generic.Dictionary<SlotKey, SlotKey> ()
        let shape = System.Collections.Generic.Dictionary<SlotKey, SlotShape> ()
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

        member this.Shape (key : SlotKey) : SlotShape option =
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
                    | Some sa, Some sb -> join offset slot sa sb |> Result.map Some

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
            (delivered : SlotShape)
            : Result<Set<int>, StackShapeError>
            =
            let root = this.Find key

            match shape.TryGetValue root with
            | false, _ ->
                shape.[root] <- delivered
                Ok members.[root]
            | true, existing ->
                match join offset slot existing delivered with
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

        match successorsOf offset instruction with
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
                match successorsOf offset instruction with
                | Successors.None
                | Successors.FallThrough -> []
                | Successors.Targets targets -> targets
                | Successors.TargetsAndFallThrough targets -> (offset + IlOp.NumberOfBytes instruction) :: targets
                | Successors.Leave target -> [ target ]
            )

        Set.ofList (0 :: fromInstructions @ (handlerEntries body.ExceptionRegions |> List.map fst))

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

    /// The conditional branches and `switch`es whose every operand their basic block computes
    /// from values the importer may hold as constants: a literal, a static field (the importer
    /// reads an initialised `static readonly` one), the result of a call on such values or on
    /// none (an intrinsic such as `IsSupported`, `Type.op_Equality` on two `typeof`s), or the
    /// result of an operation without a token on such values. The JIT may fold such a branch
    /// (`gtFoldExpr`), importing only the arm taken, and does so at every tier but not in
    /// debuggable code. A value arriving at a block's first instruction is a spill temp to the
    /// importer, and no constant; a `br` to the very next instruction starts no block.
    let private foldableBranchesOf (inputs : StackShapeInputs) (body : MethodInstructions<'methodVars>) : Set<int> =
        let leaders = leadersOf body
        let locations = body.Locations

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
                    match effectOf inputs locations offset instruction with
                    | Error _ -> []
                    | Ok effect ->
                        let pushed = (effect.Pushes (List.replicate effect.Pops SlotShape.Other)).Length

                        let constant =
                            pushesLiteral instruction
                            || (
                                match instruction with
                                | IlOp.Nullary _ -> effect.Pops > 0 && constantOperands effect.Pops
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, _)
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _) ->
                                    constantOperands effect.Pops
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, _) -> true
                                | _ -> false
                            )

                        let rest =
                            if stack.Length >= effect.Pops then
                                List.skip effect.Pops stack
                            else
                                []

                        List.replicate pushed constant @ rest

                let fallsOut =
                    match successorsOf offset instruction with
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

        for offset, _ in handlerEntries body.ExceptionRegions do
            visit offset

        while worklist.Count > 0 do
            let offset = worklist.Dequeue ()
            let instruction = body.Locations.[offset]
            let fallThrough = offset + IlOp.NumberOfBytes instruction

            match successorsOf offset instruction with
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
        /// Every path seen so far arrives at this depth; the slots' shapes live in the cliques.
        | Known of int
        /// Some path arrives with a stack the analysis cannot state: through a join two paths
        /// disagree at, through a join whose width it refuses to decide, through an instruction it
        /// could not type, or through a spill temp such a join shares. Nothing past here is typed.
        | Unknown

    /// What an instruction delivers to one successor.
    [<RequireQualifiedAccess>]
    type private Delivery =
        /// The stack, top first.
        | Known of SlotShape list
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
        let locations = body.Locations
        let graph = flowGraphOf body
        let components = componentsOf graph
        let entry = System.Collections.Generic.Dictionary<int, Entry> ()

        let invalid = System.Collections.Generic.Dictionary<int, StackShapeError> ()
        let cliques = Cliques ()

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

        let entryShapes (offset : int) (depth : int) : SlotShape list =
            List.init
                depth
                (fun k ->
                    match cliques.Shape (SlotKey.Slot (offset, k)) with
                    | Some s -> s
                    | None ->
                        failwith $"BUG: stack shape: slot %d{k} at offset %d{offset} has a depth but no clique shape"
                )

        /// Where `offset` sends control, each successor arriving with `delivery` (a `leave`
        /// empties the stack).
        let successors (offset : int) (delivery : Delivery) : (int * Delivery) list =
            match successorsOf offset locations.[offset] with
            | Successors.Leave target -> [ target, Delivery.Known [] ]
            | _ -> graph.[offset] |> List.map (fun target -> target, delivery)

        /// Where `offset` sends control, and what it delivers there: the stack its instruction
        /// leaves.
        let deliveries (offset : int) : Result<(int * Delivery) list, StackShapeError> =
            let instruction = locations.[offset]

            match entry.[offset] with
            | Entry.Unknown -> Ok (successors offset Delivery.Unknown)
            | Entry.Known depth ->

            let stack = entryShapes offset depth

            match effectOf inputs locations offset instruction with
            | Error e -> Error e
            | Ok effect ->

            if depth < effect.Pops then
                Error (StackShapeError.StackUnderflow (offset, instruction, depth))
            else

            let popped, rest = List.splitAt effect.Pops stack
            let targets = successors offset (Delivery.Known (effect.Pushes popped @ rest))

            match targets |> List.tryFind (fun (target, _) -> not (locations.ContainsKey target)) with
            | Some (target, _) -> Error (StackShapeError.BranchOutsideBody (offset, target))
            | None -> Ok targets

        /// Deliver one instruction's out-stack to all its successors. A successor reached with a
        /// known stack joins the delivered shape into each slot's clique, which it shares with
        /// its whole component of the flow graph; a successor whose depth or slots cannot meet
        /// the delivery becomes unknown, as does one delivered an unknown stack or one whose
        /// temps are already undecidable.
        let deliverAll (targets : (int * Delivery) list) : unit =
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
                |> List.iteri (fun k shape ->
                    if stillKnown target then
                        match cliques.Deliver target k (SlotKey.Slot (target, k)) shape with
                        | Ok moved -> changed.UnionWith moved
                        | Error e -> markUnknown target (Some e)
                )

            for offset in changed do
                enqueue offset

        // An offset is queued when first reached, when a clique one of its slots belongs to
        // changes shape, and when it becomes unknown. An instruction found invalid on its own
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

        let typed () : Map<int, SlotShape list> =
            entry
            |> Seq.choose (fun kv ->
                match kv.Value with
                | Entry.Known depth when not (invalid.ContainsKey kv.Key) -> Some (kv.Key, entryShapes kv.Key depth)
                | _ -> None
            )
            |> Map.ofSeq

        // Promotions come from the settled states alone. During the fixpoint an offset can be
        // delivered a float32 by a predecessor whose own entry was later widened; CoreCLR
        // re-imports such a predecessor with the widened temp, so it delivers a double in the
        // end and inserts no cast. Only an edge that *still* delivers a float32 into a slot
        // whose clique settled as double is cast.
        let promotionsOf (typed : Map<int, SlotShape list>) : Map<int, int list> =
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
                            |> List.choose (fun (i, (d, s)) ->
                                if
                                    d = SlotShape.Float FloatWidth.Single && s = SlotShape.Float FloatWidth.Double
                                then
                                    Some (target, i)
                                else
                                    None
                            )
                        | _ -> []
                    )
            )
            |> List.distinct
            |> List.groupBy fst
            |> List.map (fun (target, slots) -> target, slots |> List.map snd |> List.sort)
            |> Map.ofList

        for offset, stack in (0, []) :: handlerEntries body.ExceptionRegions do
            deliverAll [ offset, Delivery.Known stack ]

        drain ()

        // The analysis types the flow graph with every arm imported, as debuggable code imports
        // it. Folding a branch only removes edges and the deliveries of code reached only through
        // them, so a join keeps its width in every compilation unless a float32 meets a double
        // there: then the double may arrive only through a folded-away arm. Such a join, where a
        // branch the importer may fold can change what reaches its spill temps, is refused, and
        // what follows only from it is unknown.
        let reachableOffsets = reachable body

        let dependent : Map<int, int> =
            foldableBranchesOf inputs body
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
            promotionsOf (typed ())
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
            Promotions = promotionsOf typed
            BlockStarts = leadersOf body
        }
