namespace ExnSpike

open WoofWare.PawPrint

/// What an opcode can raise by itself, with no callee involved.
[<RequireQualifiedAccess>]
type Faults =
    /// The opcode raises exactly these by itself, and nothing else. The empty list is a positive
    /// claim that it cannot raise at all, not an absence of information.
    | Raises of string list
    /// Deliberately not classified here. An analysis must widen to `Unknown` on meeting one; it
    /// must never read this as "cannot raise".
    | Unmodelled

/// Which exceptions each IL opcode can raise by itself. This is the whole of "IL semantics" that
/// an exception-escape analysis needs: a classification, not an interpreter.
///
/// Every match here is exhaustive, with `TreatWarningsAsErrors` on and FS0025 not suppressed, so
/// an opcode added to `IlOp` fails this build rather than silently acquiring `Raises []`. That
/// distinction is the whole point: `Raises []` is a claim, and a wrong one is a false negative in
/// an analysis that is supposed to be sound.
[<RequireQualifiedAccess>]
module Implicit =

    let private none = Faults.Raises []

    let private nullDeref = Faults.Raises [ "System.NullReferenceException" ]

    let private overflow = Faults.Raises [ "System.OverflowException" ]

    let private arrayLoad =
        Faults.Raises [ "System.NullReferenceException" ; "System.IndexOutOfRangeException" ]

    let private arrayStore =
        Faults.Raises
            [
                "System.NullReferenceException"
                "System.IndexOutOfRangeException"
                "System.ArrayTypeMismatchException"
            ]

    let ofNullary (op : NullaryIlOp) : Faults =
        match op with
        // Signed division and remainder fault twice over: a zero divisor, and MinValue / -1.
        | NullaryIlOp.Div
        | NullaryIlOp.Rem -> Faults.Raises [ "System.DivideByZeroException" ; "System.OverflowException" ]
        // The unsigned forms have no representable overflow, so a zero divisor is the whole of it.
        | NullaryIlOp.Div_un
        | NullaryIlOp.Rem_un -> Faults.Raises [ "System.DivideByZeroException" ]
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
        | NullaryIlOp.Ckfinite -> Faults.Raises [ "System.ArithmeticException" ]
        | NullaryIlOp.Localloc -> Faults.Raises [ "System.StackOverflowException" ]
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
        // `throw` on a null object reference raises instead of throwing what it was handed; the
        // typed half is the caller's business, not this table's.
        | NullaryIlOp.Throw -> nullDeref
        // `rethrow` re-raises whatever the enclosing handler caught. Naming that needs the
        // handler's clause type, which is not an opcode-local fact.
        | NullaryIlOp.Rethrow -> Faults.Unmodelled
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

    let ofUnaryMetadata (op : UnaryMetadataTokenIlOp) : Faults =
        match op with
        | UnaryMetadataTokenIlOp.Castclass -> Faults.Raises [ "System.InvalidCastException" ]
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any ->
            Faults.Raises [ "System.InvalidCastException" ; "System.NullReferenceException" ]
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> Faults.Raises [ "System.InvalidCastException" ]
        | UnaryMetadataTokenIlOp.Newarr -> Faults.Raises [ "System.OverflowException" ; "System.OutOfMemoryException" ]
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Box -> Faults.Raises [ "System.OutOfMemoryException" ]
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
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelema -> arrayStore
        // A static field access runs the declaring type's `.cctor`, whose failure surfaces as this.
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stsfld -> Faults.Raises [ "System.TypeInitializationException" ]
        // These raise nothing themselves; the call edge carries whatever the callee does.
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Jmp
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Sizeof -> none

    let ofUnaryConst (op : UnaryConstIlOp) : Faults =
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

    let ofUnaryStringToken (op : UnaryStringTokenIlOp) : Faults =
        match op with
        | UnaryStringTokenIlOp.Ldstr -> none

    let ofIlOp (op : IlOp) : Faults =
        match op with
        | IlOp.Nullary n -> ofNullary n
        | IlOp.UnaryConst c -> ofUnaryConst c
        | IlOp.UnaryMetadataToken (m, _) -> ofUnaryMetadata m
        | IlOp.UnaryStringToken (s, _) -> ofUnaryStringToken s
        | IlOp.Switch _ -> none
