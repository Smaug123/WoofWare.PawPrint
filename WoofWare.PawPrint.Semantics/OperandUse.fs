namespace WoofWare.PawPrint

/// What an instruction does with one of the values it pops, as far as a value whose content is
/// undefined is concerned.
///
/// An interpreter that tracks undefined values (memory nothing wrote) lets them be moved and
/// refuses to let them decide anything. This is the table of which is which, kept in one place so
/// that the line can be moved: a later model that propagated undefinedness through arithmetic,
/// say, would turn arithmetic operands from `Observes` into `Moves` here and teach the arithmetic
/// to produce an undefined result.
[<RequireQualifiedAccess>]
type OperandUse =
    /// The value is carried somewhere without being looked at: into a local, an argument, a
    /// field, an array element or memory, into a callee's parameter, back to the caller, or
    /// nowhere (`pop`). An undefined value may be moved.
    | Moves
    /// What the instruction does — its result, where control goes, which memory it touches —
    /// depends on the value's content.
    | Observes

/// How one instruction uses the values it pops.
[<RequireQualifiedAccess>]
type OperandUses =
    /// The instruction pops these, top of the stack first. `ret` in a method that returns nothing
    /// pops nothing; its entry says what a `ret` that does pop one does with it.
    | Popped of OperandUse list
    /// A `call`, `callvirt`, `newobj` or `calli`: `above` values on top (the function pointer
    /// `calli` pops), each observed, and beneath them the callee's arguments, as many as its
    /// signature has. Every argument is moved into the callee, except that the receiver — the
    /// deepest argument, of a callee that has one — is `receiver`. Only the caller can say how
    /// many arguments there are, so only it can find the receiver.
    | Call of above : int * receiver : OperandUse

[<RequireQualifiedAccess>]
module OperandUse =

    let private moves : OperandUse = OperandUse.Moves
    let private observes : OperandUse = OperandUse.Observes

    let private nullary (op : NullaryIlOp) : OperandUses =
        let popped (uses : OperandUse list) = OperandUses.Popped uses

        match op with
        | NullaryIlOp.Nop
        | NullaryIlOp.Break
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly
        | NullaryIlOp.LdArg0
        | NullaryIlOp.LdArg1
        | NullaryIlOp.LdArg2
        | NullaryIlOp.LdArg3
        | NullaryIlOp.Ldloc_0
        | NullaryIlOp.Ldloc_1
        | NullaryIlOp.Ldloc_2
        | NullaryIlOp.Ldloc_3
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
        | NullaryIlOp.Arglist
        | NullaryIlOp.Endfinally
        | NullaryIlOp.Rethrow -> popped []
        | NullaryIlOp.Stloc_0
        | NullaryIlOp.Stloc_1
        | NullaryIlOp.Stloc_2
        | NullaryIlOp.Stloc_3
        | NullaryIlOp.Pop
        | NullaryIlOp.Dup
        | NullaryIlOp.Ret -> popped [ moves ]
        // Comparison, arithmetic and bitwise operations: the result is computed from both.
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
        | NullaryIlOp.Shr_un -> popped [ observes ; observes ]
        // Unary operations and conversions compute their result from the value.
        | NullaryIlOp.Neg
        | NullaryIlOp.Not
        | NullaryIlOp.Ckfinite
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
        | NullaryIlOp.Conv_R4
        | NullaryIlOp.Conv_R8
        | NullaryIlOp.Conv_r_un
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
        // The array whose length is read; the size to allocate; the typed reference whose type
        // is read; the verdict of a filter; the exception thrown.
        | NullaryIlOp.LdLen
        | NullaryIlOp.Localloc
        | NullaryIlOp.Refanytype
        | NullaryIlOp.Endfilter
        | NullaryIlOp.Throw
        // The address read from.
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
        | NullaryIlOp.Ldind_r8 -> popped [ observes ]
        // The value is stored to the address beneath it, which is observed.
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8 -> popped [ moves ; observes ]
        // The index, then the array.
        | NullaryIlOp.Ldelem_i
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_u4
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_u8
        | NullaryIlOp.Ldelem_r4
        | NullaryIlOp.Ldelem_r8
        | NullaryIlOp.Ldelem_ref -> popped [ observes ; observes ]
        // The value is stored into the array element the index and array beneath it name. A
        // reference stored by `stelem.ref` is also type-checked against the array, which is a use
        // this table cannot see; the instruction reports that one itself.
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
        | NullaryIlOp.Stelem_ref -> popped [ moves ; observes ; observes ]
        // The byte count, the source (or fill byte) and the destination. The memory `cpblk`
        // copies is moved, but that memory is not an operand.
        | NullaryIlOp.Cpblk
        | NullaryIlOp.Initblk -> popped [ observes ; observes ; observes ]

    let private unaryConst (op : UnaryConstIlOp) : OperandUses =
        match op with
        | UnaryConstIlOp.Stloc _
        | UnaryConstIlOp.Stloc_s _
        | UnaryConstIlOp.Starg _
        | UnaryConstIlOp.Starg_s _ -> OperandUses.Popped [ moves ]
        | UnaryConstIlOp.Ldloc _
        | UnaryConstIlOp.Ldloc_s _
        | UnaryConstIlOp.Ldarg _
        | UnaryConstIlOp.Ldarg_s _
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _
        | UnaryConstIlOp.Br _
        | UnaryConstIlOp.Br_s _
        | UnaryConstIlOp.Leave _
        | UnaryConstIlOp.Leave_s _
        | UnaryConstIlOp.Unaligned _ -> OperandUses.Popped []
        | UnaryConstIlOp.Brfalse _
        | UnaryConstIlOp.Brtrue _
        | UnaryConstIlOp.Brfalse_s _
        | UnaryConstIlOp.Brtrue_s _ -> OperandUses.Popped [ observes ]
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
        | UnaryConstIlOp.Blt_un_s _ -> OperandUses.Popped [ observes ; observes ]

    let private token (op : UnaryMetadataTokenIlOp) : OperandUses =
        match op with
        | UnaryMetadataTokenIlOp.Call
        // `newobj` has no receiver among its arguments: the object is made, not passed.
        | UnaryMetadataTokenIlOp.Newobj -> OperandUses.Call (0, moves)
        // Virtual dispatch reads the receiver's type, and even a non-virtual `callvirt` checks it
        // for null.
        | UnaryMetadataTokenIlOp.Callvirt -> OperandUses.Call (0, observes)
        | UnaryMetadataTokenIlOp.Calli -> OperandUses.Call (1, moves)
        | UnaryMetadataTokenIlOp.Jmp
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Constrained -> OperandUses.Popped []
        | UnaryMetadataTokenIlOp.Stsfld -> OperandUses.Popped [ moves ]
        // The object or address a field is read from or addressed in; the reference cast, tested,
        // unboxed or dispatched on; the length of the array made; the address read from, typed or
        // cleared.
        //
        // `box` observes too, though it copies its operand into a new object, because boxing a
        // `Nullable<T>` reads its `hasValue`: an undefined primitive is refused outright rather
        // than having that distinction drawn.
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval
        | UnaryMetadataTokenIlOp.Initobj -> OperandUses.Popped [ observes ]
        // The value is stored into the field of, or the memory at, what lies beneath it.
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stobj -> OperandUses.Popped [ moves ; observes ]
        // The source and destination addresses. The value `cpobj` copies is moved, but it is not
        // an operand.
        | UnaryMetadataTokenIlOp.Cpobj
        // The index, then the array.
        | UnaryMetadataTokenIlOp.Ldelema
        | UnaryMetadataTokenIlOp.Ldelem -> OperandUses.Popped [ observes ; observes ]
        | UnaryMetadataTokenIlOp.Stelem -> OperandUses.Popped [ moves ; observes ; observes ]

    /// How `instruction` uses the values it pops.
    let ofInstruction (instruction : IlOp) : OperandUses =
        match instruction with
        | IlOp.Nullary op -> nullary op
        | IlOp.UnaryConst op -> unaryConst op
        | IlOp.UnaryMetadataToken (op, _) -> token op
        | IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, _) -> OperandUses.Popped []
        // The value switched on decides where control goes.
        | IlOp.Switch _ -> OperandUses.Popped [ observes ]

    /// The positions, counting from the top of the stack, of the operands `instruction` observes
    /// that do not depend on a callee's signature. For a call, that is the function pointer
    /// `calli` pops, and nothing else: a `callvirt` receiver lies beneath arguments only the
    /// caller can count.
    let observedPositions (instruction : IlOp) : int list =
        match ofInstruction instruction with
        | OperandUses.Popped uses ->
            uses
            |> List.indexed
            |> List.choose (fun (position, used) ->
                match used with
                | OperandUse.Observes -> Some position
                | OperandUse.Moves -> None
            )
        | OperandUses.Call (above, _) -> List.init above id
