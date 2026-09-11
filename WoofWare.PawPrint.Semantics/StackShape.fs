namespace WoofWare.PawPrint

open System.Collections.Immutable

/// What a token-bearing instruction does to the stack, as read from its token by whoever can
/// read it: a signature blob for a body from a PE image, a `DynamicScope` entry for a body minted
/// by `Reflection.Emit`. Only a call's effect depends on its token; every other token-bearing
/// opcode pops and pushes a fixed number of values.
[<RequireQualifiedAccess>]
type TokenShape =
    /// `call`, `callvirt`, `newobj` and `calli`: how many values the callee takes from the stack,
    /// and whether it leaves one. `arguments` includes `this` for an instance method, and excludes
    /// the function pointer `calli` pops after them; for `newobj` it is the constructor's
    /// parameters alone, since the object is made rather than passed.
    | Callee of arguments : int * returnsValue : bool

/// Everything the analysis needs beyond the body itself.
type StackShapeInputs =
    {
        /// How many arguments `ldarg n` can name, counting `this` for an instance method.
        Arguments : int
        /// How many locals `ldloc n` can name.
        Locals : int
        /// Whether `ret` pops a value.
        ReturnsValue : bool
        /// For every instruction whose stack effect depends on its token, that effect, keyed by the
        /// instruction's offset. An instruction that needs one and has none is an analysis error.
        Tokens : Map<int, TokenShape>
    }

/// Why an instruction could not be given an entry depth. Each is a claim that CoreCLR's importer
/// would refuse the instruction if it imported it, except where noted. The importer imports only
/// what it reaches, and it evaluates the condition of a branch where it can (a literal, an
/// intrinsic such as `IsSupported`) and never imports the arm it drops, so an instruction control
/// can reach in the body's graph may never be imported: the analysis therefore records the
/// instruction rather than rejecting the body, and the interpreter refuses only its execution.
[<RequireQualifiedAccess>]
type StackShapeError =
    /// An instruction pops more than the stack holds on entry to it.
    | StackUnderflow of offset : int * instruction : IlOp * depth : int
    /// Two paths reach an offset with stacks of different depths.
    | DepthMismatch of offset : int * existing : int * incoming : int
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

    /// Whether the error is about two paths disagreeing rather than about the instruction on
    /// its own. A disagreement can be an artefact of following an arm CoreCLR's importer would
    /// never import, so it does not prove the join cannot run; an instruction that underflows
    /// on every path cannot.
    member this.IsConflict : bool =
        match this with
        | StackShapeError.DepthMismatch _ -> true
        | StackShapeError.StackUnderflow _
        | StackShapeError.MissingTokenShape _
        | StackShapeError.BranchOutsideBody _
        | StackShapeError.ArgumentOutOfRange _
        | StackShapeError.LocalOutOfRange _ -> false

/// The depth of the evaluation stack at the entry of every reachable instruction.
type StackShape =
    {
        /// The depth on entry to each reachable offset the analysis could type.
        Entry : Map<int, int>
        /// The reachable offsets the analysis could not type, and why. An instruction that
        /// underflows, or a join two paths reach with different depths, is recorded here rather
        /// than failing the whole body: CoreCLR's importer refuses it only if it imports it.
        /// What follows only from such an offset, joins it feeds included, is in `Reachable`
        /// but in neither `Entry` nor here.
        Invalid : Map<int, StackShapeError>
        /// Every offset control can reach in the body's graph, typed or not; see
        /// `StackShape.reachable`.
        Reachable : Set<int>
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

    /// What an instruction does to the stack: how many values it pops, then how many it pushes.
    type private Effect =
        {
            Pops : int
            Pushes : int
        }

    let private effect (pops : int) (pushes : int) : Effect =
        {
            Pops = pops
            Pushes = pushes
        }

    /// An instruction that only pops: a branch, a `leave`, or one that ends the method.
    let private pops (count : int) : Effect = effect count 0

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

    let private nullaryEffect
        (inputs : StackShapeInputs)
        (offset : int)
        (op : NullaryIlOp)
        : Result<Effect, StackShapeError>
        =
        let argument (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 0 1)

        let local (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 0 1)

        let storeLocal (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 1 0)

        match op with
        | NullaryIlOp.Nop
        | NullaryIlOp.Break
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly -> Ok (effect 0 0)
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
        | NullaryIlOp.Pop -> Ok (effect 1 0)
        | NullaryIlOp.Dup -> Ok (effect 1 2)
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
        | NullaryIlOp.Arglist -> Ok (effect 0 1)
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un
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
        | NullaryIlOp.Rem_un
        | NullaryIlOp.And
        | NullaryIlOp.Or
        | NullaryIlOp.Xor
        | NullaryIlOp.Shl
        | NullaryIlOp.Shr
        | NullaryIlOp.Shr_un -> Ok (effect 2 1)
        | NullaryIlOp.Neg
        | NullaryIlOp.Ckfinite
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
        | NullaryIlOp.Conv_R4
        | NullaryIlOp.Conv_R8
        | NullaryIlOp.Conv_r_un
        | NullaryIlOp.LdLen
        | NullaryIlOp.Localloc
        | NullaryIlOp.Refanytype -> Ok (effect 1 1)
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
        | NullaryIlOp.Ldind_u8
        | NullaryIlOp.Ldind_r4
        | NullaryIlOp.Ldind_r8 -> Ok (effect 1 1)
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8 -> Ok (effect 2 0)
        | NullaryIlOp.Ldelem_i
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_u4
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_u8
        | NullaryIlOp.Ldelem_ref
        | NullaryIlOp.Ldelem_r4
        | NullaryIlOp.Ldelem_r8 -> Ok (effect 2 1)
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
        | NullaryIlOp.Initblk -> Ok (effect 3 0)

    let private unaryConstEffect
        (inputs : StackShapeInputs)
        (offset : int)
        (op : UnaryConstIlOp)
        : Result<Effect, StackShapeError>
        =
        let argument (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 0 1)

        let storeArgument (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 1 0)

        let local (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 0 1)

        let storeLocal (index : int) : Result<Effect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 1 0)

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
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _ -> Ok (effect 0 1)
        | UnaryConstIlOp.Br _
        | UnaryConstIlOp.Br_s _ -> Ok (pops 0)
        | UnaryConstIlOp.Brfalse _
        | UnaryConstIlOp.Brtrue _
        | UnaryConstIlOp.Brfalse_s _
        | UnaryConstIlOp.Brtrue_s _ -> Ok (pops 1)
        | UnaryConstIlOp.Beq _
        | UnaryConstIlOp.Blt _
        | UnaryConstIlOp.Ble _
        | UnaryConstIlOp.Bgt _
        | UnaryConstIlOp.Bge _
        | UnaryConstIlOp.Bne_un _
        | UnaryConstIlOp.Bge_un _
        | UnaryConstIlOp.Bgt_un _
        | UnaryConstIlOp.Ble_un _
        | UnaryConstIlOp.Blt_un _
        | UnaryConstIlOp.Beq_s _
        | UnaryConstIlOp.Blt_s _
        | UnaryConstIlOp.Ble_s _
        | UnaryConstIlOp.Bgt_s _
        | UnaryConstIlOp.Bge_s _
        | UnaryConstIlOp.Bne_un_s _
        | UnaryConstIlOp.Bge_un_s _
        | UnaryConstIlOp.Bgt_un_s _
        | UnaryConstIlOp.Ble_un_s _
        | UnaryConstIlOp.Blt_un_s _ -> Ok (pops 2)
        | UnaryConstIlOp.Leave _
        | UnaryConstIlOp.Leave_s _ -> Ok (pops 0)
        | UnaryConstIlOp.Unaligned _ -> Ok (effect 0 0)

    let private tokenEffect
        (inputs : StackShapeInputs)
        (offset : int)
        (instruction : IlOp)
        (op : UnaryMetadataTokenIlOp)
        : Result<Effect, StackShapeError>
        =
        let callee (extraPops : int) : Result<Effect, StackShapeError> =
            match Map.tryFind offset inputs.Tokens with
            | Some (TokenShape.Callee (arguments, returnsValue)) ->
                Ok (effect (arguments + extraPops) (if returnsValue then 1 else 0))
            | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))

        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt -> callee 0
        | UnaryMetadataTokenIlOp.Calli -> callee 1
        | UnaryMetadataTokenIlOp.Newobj ->
            match Map.tryFind offset inputs.Tokens with
            | Some (TokenShape.Callee (arguments, _)) -> Ok (effect arguments 1)
            | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))
        | UnaryMetadataTokenIlOp.Jmp ->
            // `jmp` transfers to the target with the caller's own arguments; the stack must be
            // empty and nothing follows.
            Ok (pops 0)
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> Ok (effect 1 1)
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Ldelema -> Ok (effect 2 1)
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj -> Ok (effect 2 0)
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Initobj -> Ok (effect 1 0)
        | UnaryMetadataTokenIlOp.Stelem -> Ok (effect 3 0)
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Sizeof -> Ok (effect 0 1)
        | UnaryMetadataTokenIlOp.Constrained -> Ok (effect 0 0)

    let private effectOf
        (inputs : StackShapeInputs)
        (offset : int)
        (instruction : IlOp)
        : Result<Effect, StackShapeError>
        =
        match instruction with
        | IlOp.Nullary op -> nullaryEffect inputs offset op
        | IlOp.UnaryConst op -> unaryConstEffect inputs offset op
        | IlOp.UnaryMetadataToken (op, _) -> tokenEffect inputs offset instruction op
        | IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, _) -> Ok (effect 0 1)
        | IlOp.Switch _ -> Ok (pops 1)

    /// The entry depths CoreCLR gives handler code, which no instruction jumps to. A catch or
    /// filter handler starts with the exception on the stack; so does the filter's own code. A
    /// finally or fault handler starts empty.
    let private handlerEntries (regions : ImmutableArray<ExceptionRegion>) : (int * int) list =
        regions
        |> Seq.collect (fun region ->
            match region with
            | ExceptionRegion.Catch (_, offsets) -> [ offsets.HandlerOffset, 1 ]
            | ExceptionRegion.Filter (filterOffset, offsets) -> [ filterOffset, 1 ; offsets.HandlerOffset, 1 ]
            | ExceptionRegion.Finally offsets
            | ExceptionRegion.Fault offsets -> [ offsets.HandlerOffset, 0 ]
        )
        |> List.ofSeq

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
        /// Every path seen so far arrives at this depth.
        | Known of int
        /// Some path arrives at a depth the analysis cannot state: through a join two paths
        /// disagree at, or through an instruction it could not type. Nothing past here is typed.
        | Unknown

    /// Compute the depth of the evaluation stack at the entry of every instruction reachable
    /// from the method's entry or from a handler entry. An instruction that cannot be typed is
    /// recorded in `Invalid` and delivers nothing. A join that two paths reach with different
    /// depths is recorded as a conflict and delivers an unknown depth, which propagates: what
    /// follows only from the join is left untyped, and a join it feeds is untyped too rather
    /// than classified from its other arms.
    let analyse (inputs : StackShapeInputs) (body : MethodInstructions<'methodVars>) : StackShape =
        let locations = body.Locations
        let entry = System.Collections.Generic.Dictionary<int, Entry> ()
        let invalid = System.Collections.Generic.Dictionary<int, StackShapeError> ()
        let worklist = System.Collections.Generic.Queue<int> ()

        /// Where `offset` sends control, each arriving with `after` (a `leave` empties the stack).
        let successors (offset : int) (after : Entry) : (int * Entry) list =
            let instruction = locations.[offset]
            let fallThrough = offset + IlOp.NumberOfBytes instruction

            match successorsOf offset instruction with
            | Successors.None -> []
            | Successors.FallThrough -> [ fallThrough, after ]
            | Successors.Targets targets -> targets |> List.map (fun t -> t, after)
            | Successors.TargetsAndFallThrough targets ->
                (fallThrough, after) :: (targets |> List.map (fun t -> t, after))
            | Successors.Leave target -> [ target, Entry.Known 0 ]

        /// Where `offset` sends control, and what it knows about the stack on arrival there.
        let deliveries (offset : int) : Result<(int * Entry) list, StackShapeError> =
            let instruction = locations.[offset]

            match entry.[offset] with
            | Entry.Unknown -> Ok (successors offset Entry.Unknown)
            | Entry.Known depth ->

            match effectOf inputs offset instruction with
            | Error e -> Error e
            | Ok effect ->

            if depth < effect.Pops then
                Error (StackShapeError.StackUnderflow (offset, instruction, depth))
            else

            let targets = successors offset (Entry.Known (depth - effect.Pops + effect.Pushes))

            match targets |> List.tryFind (fun (target, _) -> not (locations.ContainsKey target)) with
            | Some (target, _) -> Error (StackShapeError.BranchOutsideBody (offset, target))
            | None -> Ok targets

        /// Record an arrival at `offset`, and queue it if what is known there changed. A depth
        /// that disagrees with the one already there makes the offset a conflict, which
        /// supersedes whatever the first arm found there on its own; an unknown arrival makes it
        /// unknown, likewise.
        let arrive (offset : int) (incoming : Entry) : unit =
            if locations.ContainsKey offset then
                match entry.TryGetValue offset, incoming with
                | (false, _), _ ->
                    entry.[offset] <- incoming
                    worklist.Enqueue offset
                | (true, Entry.Unknown), _ -> ()
                | (true, Entry.Known existing), Entry.Known depth when existing = depth -> ()
                | (true, Entry.Known existing), Entry.Known depth ->
                    entry.[offset] <- Entry.Unknown
                    invalid.[offset] <- StackShapeError.DepthMismatch (offset, existing, depth)
                    worklist.Enqueue offset
                | (true, Entry.Known _), Entry.Unknown ->
                    entry.[offset] <- Entry.Unknown
                    // Whatever the instruction did with the depth it had is no longer a claim
                    // about every path.
                    invalid.Remove offset |> ignore
                    worklist.Enqueue offset

        for offset, depth in (0, 0) :: handlerEntries body.ExceptionRegions do
            arrive offset (Entry.Known depth)

        // An offset is queued when first reached and again if it becomes unknown, so it is
        // processed at most twice. An instruction found invalid on its own delivers nothing; a
        // call whose token could not be read is no claim about the IL, so what follows it is
        // unknown rather than unreached.
        while worklist.Count > 0 do
            let offset = worklist.Dequeue ()

            match deliveries offset with
            | Error (StackShapeError.MissingTokenShape _ as e) ->
                invalid.[offset] <- e

                for target, arrival in successors offset Entry.Unknown do
                    arrive target arrival
            | Error e -> invalid.[offset] <- e
            | Ok targets ->
                for target, arrival in targets do
                    arrive target arrival

        {
            Entry =
                entry
                |> Seq.choose (fun kv ->
                    match kv.Value with
                    | Entry.Known depth when not (invalid.ContainsKey kv.Key) -> Some (kv.Key, depth)
                    | _ -> None
                )
                |> Map.ofSeq
            Invalid = invalid |> Seq.map (fun kv -> kv.Key, kv.Value) |> Map.ofSeq
            Reachable = reachable body
        }
