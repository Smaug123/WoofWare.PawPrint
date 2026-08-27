namespace WoofWare.PawPrint

/// A fault the execution engine raises from an instruction itself, as opposed to one that reaches
/// the instruction from a callee.
///
/// Named abstractly rather than as a resolved type, because this library has no assemblies loaded
/// and so has no type to name. A consumer that does — the interpreter, an analyser holding a
/// corelib — maps these onto its own representation.
[<RequireQualifiedAccess>]
type OpcodeFault =
    /// `System.NullReferenceException`.
    | NullReference
    /// `System.IndexOutOfRangeException`.
    | IndexOutOfRange
    /// `System.ArrayTypeMismatchException`.
    | ArrayTypeMismatch
    /// `System.InvalidCastException`.
    | InvalidCast
    /// `System.OverflowException`.
    | Overflow
    /// `System.DivideByZeroException`.
    | DivideByZero
    /// `System.ArithmeticException`, which is what `ckfinite` raises — the base of `Overflow` and
    /// `DivideByZero`, and raised as itself here rather than as either of them.
    | Arithmetic
    /// `System.OutOfMemoryException`.
    | OutOfMemory
    /// `System.StackOverflowException`.
    | StackOverflow
    /// `System.TypeInitializationException`, from a `.cctor` that a static-field access triggered
    /// and that threw.
    | TypeInitialization

/// What performing one instruction can raise by itself.
[<RequireQualifiedAccess>]
type OpcodeFaults =
    /// Performing the operation raises exactly these and nothing else. The empty list is therefore
    /// a positive claim that the instruction cannot fault, not an absence of information.
    ///
    /// **Two things this deliberately excludes**, because neither is a fact about the opcode:
    ///
    /// * What a callee raises. `call`, `callvirt`, `newobj`, `calli` and `jmp` transfer control,
    ///   and whatever the target does reaches the caller; that belongs to the call graph. The
    ///   entries here are only what the transfer itself can fault on — a null receiver, say.
    /// * Faults from *resolving* the instruction's metadata token: `TypeLoadException`,
    ///   `MissingFieldException`, `MissingMethodException`, `BadImageFormatException`,
    ///   `InvalidProgramException`. These attach uniformly to every token-bearing instruction
    ///   rather than distinguishing between them, so listing them per-opcode would add the same
    ///   five entries to most of the table while telling a reader nothing. `IlOp` already says
    ///   which instructions bear a token: exactly `UnaryMetadataToken` and `UnaryStringToken`. A
    ///   consumer analysing a well-formed, fully-resolvable closure may ignore them; one analysing
    ///   an arbitrary package may not.
    | Raises of OpcodeFault list
    /// Not classified, and not classifiable from the opcode alone. A consumer must treat this as
    /// "may raise anything" and must never read it as "cannot raise".
    | Unmodelled

/// Which faults each IL instruction can raise by itself.
///
/// This is a table rather than an interpreter, which is the point: the same fact serves the
/// concrete interpreter, which decides *whether* a fault happens on this execution, and an analyser
/// that never executes anything and needs only to know *which* are possible.
///
/// Every match below is exhaustive with no wildcard, so an opcode added to `IlOp` fails the build
/// here rather than silently acquiring `Raises []`. That distinction is the whole value of the
/// table: `Raises []` is a claim, and a wrong one is a false negative in an analysis that is
/// supposed to be sound.
[<RequireQualifiedAccess>]
module OpcodeFaults =

    let private none = OpcodeFaults.Raises []

    let private nullDeref = OpcodeFaults.Raises [ OpcodeFault.NullReference ]

    let private overflow = OpcodeFaults.Raises [ OpcodeFault.Overflow ]

    let private arrayLoad =
        OpcodeFaults.Raises [ OpcodeFault.NullReference ; OpcodeFault.IndexOutOfRange ]

    /// An array *store* can additionally fail the covariance check that a load cannot.
    let private arrayStore =
        OpcodeFaults.Raises
            [
                OpcodeFault.NullReference
                OpcodeFault.IndexOutOfRange
                OpcodeFault.ArrayTypeMismatch
            ]

    let ofNullary (op : NullaryIlOp) : OpcodeFaults =
        match op with
        // Signed division and remainder fault twice over: on a zero divisor, and on
        // MinValue / -1, whose quotient is not representable.
        | NullaryIlOp.Div
        | NullaryIlOp.Rem -> OpcodeFaults.Raises [ OpcodeFault.DivideByZero ; OpcodeFault.Overflow ]
        // The unsigned forms have no unrepresentable quotient, so a zero divisor is the whole of it.
        | NullaryIlOp.Div_un
        | NullaryIlOp.Rem_un -> OpcodeFaults.Raises [ OpcodeFault.DivideByZero ]
        | NullaryIlOp.Add_ovf
        | NullaryIlOp.Add_ovf_un
        | NullaryIlOp.Sub_ovf
        | NullaryIlOp.Sub_ovf_un
        | NullaryIlOp.Mul_ovf
        | NullaryIlOp.Mul_ovf_un
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
        | NullaryIlOp.Conv_ovf_u8_un -> overflow
        | NullaryIlOp.Ckfinite -> OpcodeFaults.Raises [ OpcodeFault.Arithmetic ]
        | NullaryIlOp.Localloc -> OpcodeFaults.Raises [ OpcodeFault.StackOverflow ]
        | NullaryIlOp.LdLen -> nullDeref
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
        | NullaryIlOp.Ldind_r8
        | NullaryIlOp.Stind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8
        | NullaryIlOp.Cpblk
        | NullaryIlOp.Initblk -> nullDeref
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
        | NullaryIlOp.Ldelem_ref -> arrayLoad
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
        | NullaryIlOp.Stelem_ref -> arrayStore
        // A null operand makes `throw` raise instead of throwing what it was handed. What it
        // throws when the operand is *not* null is a fact about the operand, which a consumer
        // reads off the evaluation stack; it is not a fact about the opcode.
        | NullaryIlOp.Throw -> nullDeref
        // `rethrow` re-raises whatever the enclosing handler caught. That is a fact about the
        // handler, not about the instruction, so there is nothing this table can say.
        | NullaryIlOp.Rethrow -> OpcodeFaults.Unmodelled
        | NullaryIlOp.Nop
        | NullaryIlOp.Break
        | NullaryIlOp.LdArg0
        | NullaryIlOp.LdArg1
        | NullaryIlOp.LdArg2
        | NullaryIlOp.LdArg3
        | NullaryIlOp.Ldloc_0
        | NullaryIlOp.Ldloc_1
        | NullaryIlOp.Ldloc_2
        | NullaryIlOp.Ldloc_3
        | NullaryIlOp.Stloc_0
        | NullaryIlOp.Stloc_1
        | NullaryIlOp.Stloc_2
        | NullaryIlOp.Stloc_3
        | NullaryIlOp.Pop
        | NullaryIlOp.Dup
        | NullaryIlOp.Ret
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
        | NullaryIlOp.Ceq
        | NullaryIlOp.Cgt
        | NullaryIlOp.Cgt_un
        | NullaryIlOp.Clt
        | NullaryIlOp.Clt_un
        | NullaryIlOp.Sub
        | NullaryIlOp.Add
        | NullaryIlOp.Mul
        | NullaryIlOp.Neg
        | NullaryIlOp.Not
        | NullaryIlOp.Shr
        | NullaryIlOp.Shr_un
        | NullaryIlOp.Shl
        | NullaryIlOp.And
        | NullaryIlOp.Or
        | NullaryIlOp.Xor
        | NullaryIlOp.Conv_I
        | NullaryIlOp.Conv_I1
        | NullaryIlOp.Conv_I2
        | NullaryIlOp.Conv_I4
        | NullaryIlOp.Conv_I8
        | NullaryIlOp.Conv_R4
        | NullaryIlOp.Conv_R8
        | NullaryIlOp.Conv_U
        | NullaryIlOp.Conv_U1
        | NullaryIlOp.Conv_U2
        | NullaryIlOp.Conv_U4
        | NullaryIlOp.Conv_U8
        | NullaryIlOp.Conv_r_un
        | NullaryIlOp.Endfilter
        | NullaryIlOp.Endfinally
        | NullaryIlOp.Volatile
        | NullaryIlOp.Tail
        | NullaryIlOp.Readonly
        | NullaryIlOp.Arglist
        | NullaryIlOp.Refanytype -> none

    let ofUnaryMetadata (op : UnaryMetadataTokenIlOp) : OpcodeFaults =
        match op with
        | UnaryMetadataTokenIlOp.Castclass -> OpcodeFaults.Raises [ OpcodeFault.InvalidCast ]
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any ->
            OpcodeFaults.Raises [ OpcodeFault.InvalidCast ; OpcodeFault.NullReference ]
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> OpcodeFaults.Raises [ OpcodeFault.InvalidCast ]
        // A negative length is the overflow; the allocation itself is the OOM.
        | UnaryMetadataTokenIlOp.Newarr -> OpcodeFaults.Raises [ OpcodeFault.Overflow ; OpcodeFault.OutOfMemory ]
        // What the constructor raises travels by the call edge, not by this entry.
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Box -> OpcodeFaults.Raises [ OpcodeFault.OutOfMemory ]
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Ldobj -> nullDeref
        | UnaryMetadataTokenIlOp.Ldelem -> arrayLoad
        // `ldelema` takes the covariance check too: handing out a writable address to an element
        // of an array whose element type is not the one named would defeat the check `stelem`
        // makes.
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelema -> arrayStore
        // A static-field access runs the declaring type's `.cctor`, and a `.cctor` that threw
        // surfaces here on this and every later access.
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stsfld -> OpcodeFaults.Raises [ OpcodeFault.TypeInitialization ]
        // These fault on nothing themselves. For the three that transfer control, what the target
        // raises reaches the caller by the call graph rather than from here.
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Jmp
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Sizeof -> none

    let ofUnaryConst (op : UnaryConstIlOp) : OpcodeFaults =
        match op with
        | UnaryConstIlOp.Stloc _
        | UnaryConstIlOp.Stloc_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Br _
        | UnaryConstIlOp.Br_s _
        | UnaryConstIlOp.Brfalse_s _
        | UnaryConstIlOp.Brtrue_s _
        | UnaryConstIlOp.Brfalse _
        | UnaryConstIlOp.Brtrue _
        | UnaryConstIlOp.Beq_s _
        | UnaryConstIlOp.Blt_s _
        | UnaryConstIlOp.Ble_s _
        | UnaryConstIlOp.Bgt_s _
        | UnaryConstIlOp.Bge_s _
        | UnaryConstIlOp.Beq _
        | UnaryConstIlOp.Blt _
        | UnaryConstIlOp.Ble _
        | UnaryConstIlOp.Bgt _
        | UnaryConstIlOp.Bge _
        | UnaryConstIlOp.Bne_un_s _
        | UnaryConstIlOp.Bge_un_s _
        | UnaryConstIlOp.Bgt_un_s _
        | UnaryConstIlOp.Ble_un_s _
        | UnaryConstIlOp.Blt_un_s _
        | UnaryConstIlOp.Bne_un _
        | UnaryConstIlOp.Bge_un _
        | UnaryConstIlOp.Bgt_un _
        | UnaryConstIlOp.Ble_un _
        | UnaryConstIlOp.Blt_un _
        | UnaryConstIlOp.Ldloc_s _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarg_s _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Leave _
        | UnaryConstIlOp.Leave_s _
        | UnaryConstIlOp.Starg_s _
        | UnaryConstIlOp.Starg _
        | UnaryConstIlOp.Unaligned _
        | UnaryConstIlOp.Ldloc _
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldarg _ -> none

    let ofUnaryStringToken (op : UnaryStringTokenIlOp) : OpcodeFaults =
        match op with
        // The literal is interned, so there is no allocation for this to fail on.
        | UnaryStringTokenIlOp.Ldstr -> none

    let ofIlOp (op : IlOp) : OpcodeFaults =
        match op with
        | IlOp.Nullary op -> ofNullary op
        | IlOp.UnaryConst op -> ofUnaryConst op
        | IlOp.UnaryMetadataToken (op, _) -> ofUnaryMetadata op
        | IlOp.UnaryStringToken (op, _) -> ofUnaryStringToken op
        // A multi-target branch on the integer at the top of the stack; the out-of-range case
        // falls through rather than faulting.
        | IlOp.Switch _ -> none

    /// Can an instruction with these faults raise `fault`? `Unmodelled` answers yes to everything,
    /// which is what makes it safe to meet: a consumer that asks this question can never read an
    /// unclassified instruction as harmless.
    let mayRaise (fault : OpcodeFault) (faults : OpcodeFaults) : bool =
        match faults with
        | OpcodeFaults.Unmodelled -> true
        | OpcodeFaults.Raises xs -> List.contains fault xs
