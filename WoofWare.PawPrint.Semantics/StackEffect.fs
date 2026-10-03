namespace WoofWare.PawPrint

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
    /// as constants: literals, static fields, arguments (which an inlinee may receive as
    /// constants), and calls on such values or on none. Not a claim
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

/// The type of a number on the evaluation stack, as ECMA-335 III.1.1 classifies the stack's
/// numeric types.
[<RequireQualifiedAccess>]
type StackNumber =
    | Int32
    | Int64
    | NativeInt
    | Float32
    | Float64

/// A value that the token of the instruction pushing it decides.
[<RequireQualifiedAccess>]
type TokenValue =
    /// What `call`, `callvirt` or `calli` returns.
    | CallResult
    /// The object or value `newobj` makes.
    | NewObject
    /// The field `ldfld` or `ldsfld` loads.
    | Field
    /// A value of the token's type: what `ldobj`, `unbox.any` or `ldelem` loads.
    | Loaded
    /// The operand of `castclass` or `isinst`, as the token's type, or null.
    | Cast
    /// The object `box` makes of a value of the token's type.
    | Boxed
    /// The vector `newarr` makes, of elements of the token's type.
    | NewArray
    /// The runtime handle `ldtoken` loads.
    | Handle
    /// The function pointer `ldftn` or `ldvirtftn` loads.
    | MethodPointer
    /// The `TypedReference` `mkrefany` makes.
    | TypedReference

/// What one value an instruction pushes is, as far as the instruction itself says. A slot lattice
/// says what that is in its own terms (`SlotLattice.Push`).
[<RequireQualifiedAccess>]
type Pushed =
    /// `ldarg`: the argument at this index, `this` at 0 for an instance method.
    | Argument of index : int
    /// `ldloc`.
    | Local of index : int
    /// One of the values the instruction popped, unchanged, counted from the top: what `dup`
    /// pushes twice, and what `neg` and `ckfinite` return.
    | Operand of fromTop : int
    /// What `add`, `sub`, `mul`, `div` or `rem`, in any of their forms, make of the two values
    /// popped.
    | Arithmetic
    /// What `and`, `or`, `xor`, `not`, `shl`, `shr` or `shr.un` make of the integers popped.
    | Bitwise
    /// A number whose type the instruction names: a literal, a conversion, a comparison, a
    /// length, a size, a primitive loaded through an address or from an array, or an unmanaged
    /// pointer.
    | Number of StackNumber
    /// `ldnull`.
    | Null
    /// `ldstr`.
    | String
    /// A managed pointer: `ldloca`, `ldarga`, `ldflda`, `ldsflda`, `ldelema`, `unbox`, `refanyval`.
    | Address
    /// The object reference `ldind.ref` loads through the address popped.
    | Indirect
    /// The element `ldelem.ref` loads from the array popped.
    | Element
    /// The `RuntimeArgumentHandle` `arglist` loads.
    | ArgumentHandle
    /// The `RuntimeTypeHandle` `refanytype` takes from the `TypedReference` popped.
    | TypedReferenceType
    /// A value the instruction's token decides.
    | FromToken of TokenValue

/// How many values a call takes from the stack, and whether it leaves one.
type CalleeArity =
    {
        /// Includes `this` for an instance method, and excludes the function pointer `calli`
        /// pops after them; for `newobj` it is the constructor's parameters alone, since the
        /// object is made rather than passed.
        Arguments : int
        Returns : bool
    }

/// What the stack effects of a body's instructions depend on beyond the instructions themselves.
type StackEffectInputs =
    {
        /// How many arguments `ldarg` and `starg` may name, `this` included.
        Arguments : int
        /// How many locals `ldloc` and `stloc` may name.
        Locals : int
        /// Whether `ret` pops a value.
        ReturnsValue : bool
        /// The arity of the callee of every `call`, `callvirt`, `calli` and `newobj`, keyed by the
        /// instruction's offset. One that has none is a `StackShapeError.MissingTokenShape`.
        Callees : Map<int, CalleeArity>
    }

/// What one instruction does to the evaluation stack: how many values it pops, and what it
/// pushes, top first.
type StackEffect =
    {
        Pops : int
        Pushes : Pushed list
    }

[<RequireQualifiedAccess>]
module StackEffect =

    let private effect (pops : int) (pushes : Pushed list) : StackEffect =
        {
            Pops = pops
            Pushes = pushes
        }

    /// An instruction that only pops: a branch, a `leave`, or one that ends the method.
    let private pops (count : int) : StackEffect = effect count []

    let private number (n : StackNumber) : Pushed list = [ Pushed.Number n ]

    let private nullaryEffect
        (inputs : StackEffectInputs)
        (offset : int)
        (next : IlOp option)
        (op : NullaryIlOp)
        : Result<StackEffect, StackShapeError>
        =
        let argument (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 0 [ Pushed.Argument index ])

        let local (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 0 [ Pushed.Local index ])

        let storeLocal (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
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
        | NullaryIlOp.Dup -> Ok (effect 1 [ Pushed.Operand 0 ; Pushed.Operand 0 ])
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
        | NullaryIlOp.LdcI4_m1 -> Ok (effect 0 (number StackNumber.Int32))
        | NullaryIlOp.LdNull -> Ok (effect 0 [ Pushed.Null ])
        | NullaryIlOp.Arglist -> Ok (effect 0 [ Pushed.ArgumentHandle ])
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un -> Ok (effect 2 (number StackNumber.Int32))
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
        | NullaryIlOp.Rem_un -> Ok (effect 2 [ Pushed.Arithmetic ])
        | NullaryIlOp.And
        | NullaryIlOp.Or
        | NullaryIlOp.Xor
        | NullaryIlOp.Shl
        | NullaryIlOp.Shr
        | NullaryIlOp.Shr_un -> Ok (effect 2 [ Pushed.Bitwise ])
        | NullaryIlOp.Neg
        | NullaryIlOp.Ckfinite -> Ok (effect 1 [ Pushed.Operand 0 ])
        | NullaryIlOp.Not -> Ok (effect 1 [ Pushed.Bitwise ])
        | NullaryIlOp.Conv_I
        | NullaryIlOp.Conv_U
        | NullaryIlOp.Conv_ovf_i
        | NullaryIlOp.Conv_ovf_u
        | NullaryIlOp.Conv_ovf_i_un
        | NullaryIlOp.Conv_ovf_u_un
        | NullaryIlOp.LdLen
        | NullaryIlOp.Localloc -> Ok (effect 1 (number StackNumber.NativeInt))
        | NullaryIlOp.Conv_I1
        | NullaryIlOp.Conv_I2
        | NullaryIlOp.Conv_I4
        | NullaryIlOp.Conv_U1
        | NullaryIlOp.Conv_U2
        | NullaryIlOp.Conv_U4
        | NullaryIlOp.Conv_ovf_i1
        | NullaryIlOp.Conv_ovf_i2
        | NullaryIlOp.Conv_ovf_i4
        | NullaryIlOp.Conv_ovf_u1
        | NullaryIlOp.Conv_ovf_u2
        | NullaryIlOp.Conv_ovf_u4
        | NullaryIlOp.Conv_ovf_i1_un
        | NullaryIlOp.Conv_ovf_u1_un
        | NullaryIlOp.Conv_ovf_i2_un
        | NullaryIlOp.Conv_ovf_u2_un
        | NullaryIlOp.Conv_ovf_i4_un
        | NullaryIlOp.Conv_ovf_u4_un -> Ok (effect 1 (number StackNumber.Int32))
        | NullaryIlOp.Conv_I8
        | NullaryIlOp.Conv_U8
        | NullaryIlOp.Conv_ovf_i8
        | NullaryIlOp.Conv_ovf_u8
        | NullaryIlOp.Conv_ovf_i8_un
        | NullaryIlOp.Conv_ovf_u8_un -> Ok (effect 1 (number StackNumber.Int64))
        | NullaryIlOp.Refanytype -> Ok (effect 1 [ Pushed.TypedReferenceType ])
        | NullaryIlOp.Conv_R4 -> Ok (effect 1 (number StackNumber.Float32))
        | NullaryIlOp.Conv_R8 -> Ok (effect 1 (number StackNumber.Float64))
        | NullaryIlOp.Conv_r_un ->
            // There is no `conv.r4.un`, so compilers emit `conv.r.un; conv.r4` for an unsigned
            // source cast to float32, and CoreCLR's importer types this result as float32 when
            // the next opcode is `conv.r4` (`CEE_CONV_R_UN` in importer.cpp). The `conv.r4` that
            // follows then finds a float32 and leaves it single.
            match next with
            | Some (IlOp.Nullary NullaryIlOp.Conv_R4) -> Ok (effect 1 (number StackNumber.Float32))
            | _ -> Ok (effect 1 (number StackNumber.Float64))
        | NullaryIlOp.Endfilter -> Ok (pops 1)
        | NullaryIlOp.Endfinally
        | NullaryIlOp.Rethrow -> Ok (pops 0)
        | NullaryIlOp.Throw -> Ok (pops 1)
        | NullaryIlOp.Ldind_ref -> Ok (effect 1 [ Pushed.Indirect ])
        | NullaryIlOp.Ldind_i -> Ok (effect 1 (number StackNumber.NativeInt))
        | NullaryIlOp.Ldind_i1
        | NullaryIlOp.Ldind_i2
        | NullaryIlOp.Ldind_i4
        | NullaryIlOp.Ldind_u1
        | NullaryIlOp.Ldind_u2
        | NullaryIlOp.Ldind_u4 -> Ok (effect 1 (number StackNumber.Int32))
        | NullaryIlOp.Ldind_i8
        | NullaryIlOp.Ldind_u8 -> Ok (effect 1 (number StackNumber.Int64))
        | NullaryIlOp.Ldind_r4 -> Ok (effect 1 (number StackNumber.Float32))
        | NullaryIlOp.Ldind_r8 -> Ok (effect 1 (number StackNumber.Float64))
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8 -> Ok (effect 2 [])
        | NullaryIlOp.Ldelem_i -> Ok (effect 2 (number StackNumber.NativeInt))
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_u4 -> Ok (effect 2 (number StackNumber.Int32))
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_u8 -> Ok (effect 2 (number StackNumber.Int64))
        | NullaryIlOp.Ldelem_ref -> Ok (effect 2 [ Pushed.Element ])
        | NullaryIlOp.Ldelem_r4 -> Ok (effect 2 (number StackNumber.Float32))
        | NullaryIlOp.Ldelem_r8 -> Ok (effect 2 (number StackNumber.Float64))
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
        (inputs : StackEffectInputs)
        (offset : int)
        (op : UnaryConstIlOp)
        : Result<StackEffect, StackShapeError>
        =
        let argument (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 0 [ Pushed.Argument index ])

        let storeArgument (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Arguments then
                Error (StackShapeError.ArgumentOutOfRange (offset, index))
            else
                Ok (effect 1 [])

        let local (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
                Error (StackShapeError.LocalOutOfRange (offset, index))
            else
                Ok (effect 0 [ Pushed.Local index ])

        let storeLocal (index : int) : Result<StackEffect, StackShapeError> =
            if index < 0 || index >= inputs.Locals then
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
        | UnaryConstIlOp.Ldarga_s _ -> Ok (effect 0 [ Pushed.Address ])
        | UnaryConstIlOp.Ldc_I8 _ -> Ok (effect 0 (number StackNumber.Int64))
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_I4_s _ -> Ok (effect 0 (number StackNumber.Int32))
        | UnaryConstIlOp.Ldc_R4 _ -> Ok (effect 0 (number StackNumber.Float32))
        | UnaryConstIlOp.Ldc_R8 _ -> Ok (effect 0 (number StackNumber.Float64))
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
        | UnaryConstIlOp.Unaligned _ -> Ok (effect 0 [])

    let private tokenEffect
        (inputs : StackEffectInputs)
        (offset : int)
        (instruction : IlOp)
        (op : UnaryMetadataTokenIlOp)
        : Result<StackEffect, StackShapeError>
        =
        let callee (extraPops : int) : Result<StackEffect, StackShapeError> =
            match Map.tryFind offset inputs.Callees with
            | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))
            | Some arity ->
                let pushes =
                    if arity.Returns then
                        [ Pushed.FromToken TokenValue.CallResult ]
                    else
                        []

                Ok (effect (arity.Arguments + extraPops) pushes)

        let fromToken (pops : int) (value : TokenValue) : Result<StackEffect, StackShapeError> =
            Ok (effect pops [ Pushed.FromToken value ])

        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt -> callee 0
        | UnaryMetadataTokenIlOp.Calli -> callee 1
        | UnaryMetadataTokenIlOp.Newobj ->
            match Map.tryFind offset inputs.Callees with
            | None -> Error (StackShapeError.MissingTokenShape (offset, instruction))
            | Some arity -> fromToken arity.Arguments TokenValue.NewObject
        | UnaryMetadataTokenIlOp.Jmp ->
            // `jmp` transfers to the target with the caller's own arguments; the stack must be
            // empty and nothing follows.
            Ok (pops 0)
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst -> fromToken 1 TokenValue.Cast
        | UnaryMetadataTokenIlOp.Newarr -> fromToken 1 TokenValue.NewArray
        | UnaryMetadataTokenIlOp.Box -> fromToken 1 TokenValue.Boxed
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Refanyval -> Ok (effect 1 [ Pushed.Address ])
        | UnaryMetadataTokenIlOp.Ldvirtftn -> fromToken 1 TokenValue.MethodPointer
        | UnaryMetadataTokenIlOp.Mkrefany -> fromToken 1 TokenValue.TypedReference
        | UnaryMetadataTokenIlOp.Ldelema -> Ok (effect 2 [ Pushed.Address ])
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj -> Ok (effect 2 [])
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Initobj -> Ok (effect 1 [])
        | UnaryMetadataTokenIlOp.Stelem -> Ok (effect 3 [])
        | UnaryMetadataTokenIlOp.Ldfld -> fromToken 1 TokenValue.Field
        | UnaryMetadataTokenIlOp.Ldsfld -> fromToken 0 TokenValue.Field
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Unbox_Any -> fromToken 1 TokenValue.Loaded
        | UnaryMetadataTokenIlOp.Ldelem -> fromToken 2 TokenValue.Loaded
        | UnaryMetadataTokenIlOp.Ldsflda -> Ok (effect 0 [ Pushed.Address ])
        | UnaryMetadataTokenIlOp.Ldftn -> fromToken 0 TokenValue.MethodPointer
        | UnaryMetadataTokenIlOp.Ldtoken -> fromToken 0 TokenValue.Handle
        | UnaryMetadataTokenIlOp.Sizeof -> Ok (effect 0 (number StackNumber.Int32))
        | UnaryMetadataTokenIlOp.Constrained -> Ok (effect 0 [])

    /// What the instruction at `offset` does to the stack, where `locations` maps every offset of
    /// the body to its instruction. Only `conv.r.un` looks at the instruction after it.
    let ofInstruction
        (inputs : StackEffectInputs)
        (locations : Map<int, IlOp>)
        (offset : int)
        (instruction : IlOp)
        : Result<StackEffect, StackShapeError>
        =
        match instruction with
        | IlOp.Nullary op ->
            nullaryEffect inputs offset (Map.tryFind (offset + IlOp.NumberOfBytes instruction) locations) op
        | IlOp.UnaryConst op -> unaryConstEffect inputs offset op
        | IlOp.UnaryMetadataToken (op, _) -> tokenEffect inputs offset instruction op
        | IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, _) -> Ok (effect 0 [ Pushed.String ])
        | IlOp.Switch _ -> Ok (pops 1)
