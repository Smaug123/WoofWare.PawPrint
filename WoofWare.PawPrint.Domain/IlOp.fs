namespace WoofWare.PawPrint

open System.Collections.Immutable

type NullaryIlOp =
    | Nop
    /// Load the argument at index 0 onto the eval stack.
    | LdArg0
    /// Load the argument at index 1 onto the eval stack.
    | LdArg1
    /// Load the argument at index 2 onto the eval stack.
    | LdArg2
    /// Load the argument at index 3 onto the eval stack.
    | LdArg3
    /// Load the local variable at index 0 onto the eval stack.
    | Ldloc_0
    /// Load the local variable at index 1 onto the eval stack.
    | Ldloc_1
    /// Load the local variable at index 2 onto the eval stack.
    | Ldloc_2
    /// Load the local variable at index 3 onto the eval stack.
    | Ldloc_3
    /// Remove the value on top of the eval stack.
    | Pop
    /// Push another copy of the value on top of the eval stack onto the top of the eval stack.
    | Dup
    | Ret
    /// Push the int32 value 0 to the eval stack.
    | LdcI4_0
    /// Push the int32 value 1 to the eval stack.
    | LdcI4_1
    /// Push the int32 value 2 to the eval stack.
    | LdcI4_2
    /// Push the int32 value 3 to the eval stack.
    | LdcI4_3
    /// Push the int32 value 4 to the eval stack.
    | LdcI4_4
    /// Push the int32 value 5 to the eval stack.
    | LdcI4_5
    /// Push the int32 value 6 to the eval stack.
    | LdcI4_6
    /// Push the int32 value 7 to the eval stack.
    | LdcI4_7
    /// Push the int32 value 8 to the eval stack.
    | LdcI4_8
    /// Push the int32 value -1 to the eval stack.
    | LdcI4_m1
    /// Push a null object reference onto the stack.
    | LdNull
    /// Pop two values from the stack; push 1 if they're equal, 0 otherwise
    | Ceq
    | Cgt
    | Cgt_un
    | Clt
    | Clt_un
    /// Pop from the eval stack and store the result in local variable 0
    | Stloc_0
    /// Pop from the eval stack and store the result in local variable 1
    | Stloc_1
    /// Pop from the eval stack and store the result in local variable 2
    | Stloc_2
    /// Pop from the eval stack and store the result in local variable 3
    | Stloc_3
    | Sub
    | Sub_ovf
    | Sub_ovf_un
    | Add
    | Add_ovf
    | Add_ovf_un
    | Mul
    | Mul_ovf
    | Mul_ovf_un
    | Div
    | Div_un
    | Rem
    | Rem_un
    | Neg
    | Not
    | Shr
    | Shr_un
    | Shl
    | Conv_ovf_i
    | Conv_ovf_u
    | And
    | Or
    | Xor
    /// Converts the value on top of the eval stack to nativeint
    | Conv_I
    /// Converts the value on top of the eval stack to int8, then pads to int32
    | Conv_I1
    /// Converts the value on top of the eval stack to int16, then pads to int32
    | Conv_I2
    /// Converts the value on top of the eval stack to int32
    | Conv_I4
    /// Converts the value on top of the eval stack to int64
    | Conv_I8
    | Conv_R4
    | Conv_R8
    /// Converts the value on top of the eval stack to unsigned nativeint, then extends to nativeint
    | Conv_U
    /// Converts the value on top of the eval stack to uint8, then extends to int32
    | Conv_U1
    /// Converts the value on top of the eval stack to uint16, then extends to int 32
    | Conv_U2
    /// Converts the value on top of the eval stack to uint32, then extends to int32
    | Conv_U4
    /// Converts the value on top of the eval stack to uint64, then extends to int64
    | Conv_U8
    | Conv_ovf_u1
    | Conv_ovf_u2
    | Conv_ovf_u4
    | Conv_ovf_u8
    | Conv_ovf_i1
    | Conv_ovf_i2
    | Conv_ovf_i4
    | Conv_ovf_i8
    /// Pushes the number of elements of a zero-based, one-dimensional array onto the evaluation stack.
    | LdLen
    | Endfilter
    | Endfinally
    | Rethrow
    | Throw
    | Localloc
    | Ldind_ref
    | Stind_ref
    | Stind_I
    | Stind_I1
    | Stind_I2
    | Stind_I4
    | Stind_I8
    | Stind_R4
    | Stind_R8
    | Ldind_i
    | Ldind_i1
    | Ldind_i2
    /// Loads a value of type int32 as an int32 onto the evaluation stack indirectly from the specified address.
    | Ldind_i4
    | Ldind_i8
    | Ldind_u1
    | Ldind_u2
    | Ldind_u4
    | Ldind_u8
    | Ldind_r4
    | Ldind_r8
    | Volatile
    | Tail
    | Conv_ovf_i_un
    | Conv_ovf_u_un
    | Conv_ovf_i1_un
    | Conv_ovf_u1_un
    | Conv_ovf_i2_un
    | Conv_ovf_u2_un
    | Conv_ovf_i4_un
    | Conv_ovf_u4_un
    | Conv_ovf_i8_un
    | Conv_ovf_u8_un
    | Ldelem_i
    | Ldelem_i1
    | Ldelem_u1
    | Ldelem_i2
    | Ldelem_u2
    | Ldelem_i4
    | Ldelem_u4
    | Ldelem_i8
    | Ldelem_u8
    | Ldelem_r4
    | Ldelem_r8
    | Ldelem_ref
    | Stelem_i
    | Stelem_i1
    | Stelem_u1
    | Stelem_i2
    | Stelem_u2
    | Stelem_i4
    | Stelem_u4
    | Stelem_i8
    | Stelem_u8
    | Stelem_r4
    | Stelem_r8
    | Stelem_ref
    | Cpblk
    | Initblk
    | Break
    | Conv_r_un
    | Arglist
    | Ckfinite
    | Readonly
    | Refanytype

    override this.ToString () =
        match this with
        | NullaryIlOp.Nop -> "Nop"
        | NullaryIlOp.LdArg0 -> "LdArg0"
        | NullaryIlOp.LdArg1 -> "LdArg1"
        | NullaryIlOp.LdArg2 -> "LdArg2"
        | NullaryIlOp.LdArg3 -> "LdArg3"
        | NullaryIlOp.Ldloc_0 -> "Ldloc_0"
        | NullaryIlOp.Ldloc_1 -> "Ldloc_1"
        | NullaryIlOp.Ldloc_2 -> "Ldloc_2"
        | NullaryIlOp.Ldloc_3 -> "Ldloc_3"
        | NullaryIlOp.Pop -> "Pop"
        | NullaryIlOp.Dup -> "Dup"
        | NullaryIlOp.Ret -> "Ret"
        | NullaryIlOp.LdcI4_0 -> "LdcI4_0"
        | NullaryIlOp.LdcI4_1 -> "LdcI4_1"
        | NullaryIlOp.LdcI4_2 -> "LdcI4_2"
        | NullaryIlOp.LdcI4_3 -> "LdcI4_3"
        | NullaryIlOp.LdcI4_4 -> "LdcI4_4"
        | NullaryIlOp.LdcI4_5 -> "LdcI4_5"
        | NullaryIlOp.LdcI4_6 -> "LdcI4_6"
        | NullaryIlOp.LdcI4_7 -> "LdcI4_7"
        | NullaryIlOp.LdcI4_8 -> "LdcI4_8"
        | NullaryIlOp.LdcI4_m1 -> "LdcI4_m1"
        | NullaryIlOp.LdNull -> "LdNull"
        | NullaryIlOp.Ceq -> "Ceq"
        | NullaryIlOp.Cgt -> "Cgt"
        | NullaryIlOp.Cgt_un -> "Cgt_un"
        | NullaryIlOp.Clt -> "Clt"
        | NullaryIlOp.Clt_un -> "Clt_un"
        | NullaryIlOp.Stloc_0 -> "Stloc_0"
        | NullaryIlOp.Stloc_1 -> "Stloc_1"
        | NullaryIlOp.Stloc_2 -> "Stloc_2"
        | NullaryIlOp.Stloc_3 -> "Stloc_3"
        | NullaryIlOp.Sub -> "Sub"
        | NullaryIlOp.Sub_ovf -> "Sub_ovf"
        | NullaryIlOp.Sub_ovf_un -> "Sub_ovf_un"
        | NullaryIlOp.Add -> "Add"
        | NullaryIlOp.Add_ovf -> "Add_ovf"
        | NullaryIlOp.Add_ovf_un -> "Add_ovf_un"
        | NullaryIlOp.Mul -> "Mul"
        | NullaryIlOp.Mul_ovf -> "Mul_ovf"
        | NullaryIlOp.Mul_ovf_un -> "Mul_ovf_un"
        | NullaryIlOp.Div -> "Div"
        | NullaryIlOp.Div_un -> "Div_un"
        | NullaryIlOp.Rem -> "Rem"
        | NullaryIlOp.Rem_un -> "Rem_un"
        | NullaryIlOp.Neg -> "Neg"
        | NullaryIlOp.Not -> "Not"
        | NullaryIlOp.Shr -> "Shr"
        | NullaryIlOp.Shr_un -> "Shr_un"
        | NullaryIlOp.Shl -> "Shl"
        | NullaryIlOp.Conv_ovf_i -> "Conv_ovf_i"
        | NullaryIlOp.Conv_ovf_u -> "Conv_ovf_u"
        | NullaryIlOp.And -> "And"
        | NullaryIlOp.Or -> "Or"
        | NullaryIlOp.Xor -> "Xor"
        | NullaryIlOp.Conv_I -> "Conv_I"
        | NullaryIlOp.Conv_I1 -> "Conv_I1"
        | NullaryIlOp.Conv_I2 -> "Conv_I2"
        | NullaryIlOp.Conv_I4 -> "Conv_I4"
        | NullaryIlOp.Conv_I8 -> "Conv_I8"
        | NullaryIlOp.Conv_R4 -> "Conv_R4"
        | NullaryIlOp.Conv_R8 -> "Conv_R8"
        | NullaryIlOp.Conv_U -> "Conv_U"
        | NullaryIlOp.Conv_U1 -> "Conv_U1"
        | NullaryIlOp.Conv_U2 -> "Conv_U2"
        | NullaryIlOp.Conv_U4 -> "Conv_U4"
        | NullaryIlOp.Conv_U8 -> "Conv_U8"
        | NullaryIlOp.Conv_ovf_u1 -> "Conv_ovf_u1"
        | NullaryIlOp.Conv_ovf_u2 -> "Conv_ovf_u2"
        | NullaryIlOp.Conv_ovf_u4 -> "Conv_ovf_u4"
        | NullaryIlOp.Conv_ovf_u8 -> "Conv_ovf_u8"
        | NullaryIlOp.Conv_ovf_i1 -> "Conv_ovf_i1"
        | NullaryIlOp.Conv_ovf_i2 -> "Conv_ovf_i2"
        | NullaryIlOp.Conv_ovf_i4 -> "Conv_ovf_i4"
        | NullaryIlOp.Conv_ovf_i8 -> "Conv_ovf_i8"
        | NullaryIlOp.LdLen -> "LdLen"
        | NullaryIlOp.Endfilter -> "Endfilter"
        | NullaryIlOp.Endfinally -> "Endfinally"
        | NullaryIlOp.Rethrow -> "Rethrow"
        | NullaryIlOp.Throw -> "Throw"
        | NullaryIlOp.Localloc -> "Localloc"
        | NullaryIlOp.Ldind_ref -> "Ldind_ref"
        | NullaryIlOp.Stind_ref -> "Stind_ref"
        | NullaryIlOp.Stind_I -> "Stind_I"
        | NullaryIlOp.Stind_I1 -> "Stind_I1"
        | NullaryIlOp.Stind_I2 -> "Stind_I2"
        | NullaryIlOp.Stind_I4 -> "Stind_I4"
        | NullaryIlOp.Stind_I8 -> "Stind_I8"
        | NullaryIlOp.Stind_R4 -> "Stind_R4"
        | NullaryIlOp.Stind_R8 -> "Stind_R8"
        | NullaryIlOp.Ldind_i -> "Ldind_i"
        | NullaryIlOp.Ldind_i1 -> "Ldind_i1"
        | NullaryIlOp.Ldind_i2 -> "Ldind_i2"
        | NullaryIlOp.Ldind_i4 -> "Ldind_i4"
        | NullaryIlOp.Ldind_i8 -> "Ldind_i8"
        | NullaryIlOp.Ldind_u1 -> "Ldind_u1"
        | NullaryIlOp.Ldind_u2 -> "Ldind_u2"
        | NullaryIlOp.Ldind_u4 -> "Ldind_u4"
        | NullaryIlOp.Ldind_u8 -> "Ldind_u8"
        | NullaryIlOp.Ldind_r4 -> "Ldind_r4"
        | NullaryIlOp.Ldind_r8 -> "Ldind_r8"
        | NullaryIlOp.Volatile -> "Volatile"
        | NullaryIlOp.Tail -> "Tail"
        | NullaryIlOp.Conv_ovf_i_un -> "Conv_ovf_i_un"
        | NullaryIlOp.Conv_ovf_u_un -> "Conv_ovf_u_un"
        | NullaryIlOp.Conv_ovf_i1_un -> "Conv_ovf_i1_un"
        | NullaryIlOp.Conv_ovf_u1_un -> "Conv_ovf_u1_un"
        | NullaryIlOp.Conv_ovf_i2_un -> "Conv_ovf_i2_un"
        | NullaryIlOp.Conv_ovf_u2_un -> "Conv_ovf_u2_un"
        | NullaryIlOp.Conv_ovf_i4_un -> "Conv_ovf_i4_un"
        | NullaryIlOp.Conv_ovf_u4_un -> "Conv_ovf_u4_un"
        | NullaryIlOp.Conv_ovf_i8_un -> "Conv_ovf_i8_un"
        | NullaryIlOp.Conv_ovf_u8_un -> "Conv_ovf_u8_un"
        | NullaryIlOp.Ldelem_i -> "Ldelem_i"
        | NullaryIlOp.Ldelem_i1 -> "Ldelem_i1"
        | NullaryIlOp.Ldelem_u1 -> "Ldelem_u1"
        | NullaryIlOp.Ldelem_i2 -> "Ldelem_i2"
        | NullaryIlOp.Ldelem_u2 -> "Ldelem_u2"
        | NullaryIlOp.Ldelem_i4 -> "Ldelem_i4"
        | NullaryIlOp.Ldelem_u4 -> "Ldelem_u4"
        | NullaryIlOp.Ldelem_i8 -> "Ldelem_i8"
        | NullaryIlOp.Ldelem_u8 -> "Ldelem_u8"
        | NullaryIlOp.Ldelem_r4 -> "Ldelem_r4"
        | NullaryIlOp.Ldelem_r8 -> "Ldelem_r8"
        | NullaryIlOp.Ldelem_ref -> "Ldelem_ref"
        | NullaryIlOp.Stelem_i -> "Stelem_i"
        | NullaryIlOp.Stelem_i1 -> "Stelem_i1"
        | NullaryIlOp.Stelem_u1 -> "Stelem_u1"
        | NullaryIlOp.Stelem_i2 -> "Stelem_i2"
        | NullaryIlOp.Stelem_u2 -> "Stelem_u2"
        | NullaryIlOp.Stelem_i4 -> "Stelem_i4"
        | NullaryIlOp.Stelem_u4 -> "Stelem_u4"
        | NullaryIlOp.Stelem_i8 -> "Stelem_i8"
        | NullaryIlOp.Stelem_u8 -> "Stelem_u8"
        | NullaryIlOp.Stelem_r4 -> "Stelem_r4"
        | NullaryIlOp.Stelem_r8 -> "Stelem_r8"
        | NullaryIlOp.Stelem_ref -> "Stelem_ref"
        | NullaryIlOp.Cpblk -> "Cpblk"
        | NullaryIlOp.Initblk -> "Initblk"
        | NullaryIlOp.Break -> "Break"
        | NullaryIlOp.Conv_r_un -> "Conv_r_un"
        | NullaryIlOp.Arglist -> "Arglist"
        | NullaryIlOp.Ckfinite -> "Ckfinite"
        | NullaryIlOp.Readonly -> "Readonly"
        | NullaryIlOp.Refanytype -> "Refanytype"

    /// The number of bytes this instruction takes in memory.
    static member NumberOfBytes (op : NullaryIlOp) : int =
        match op with
        | Arglist
        | Ceq
        | Cgt
        | Cgt_un
        | Clt
        | Clt_un
        | Localloc
        | Endfilter
        | Volatile
        | Tail
        | Cpblk
        | Initblk
        | Rethrow
        | Refanytype
        | Readonly -> 2
        | _ -> 1

type UnaryConstIlOp =
    | Stloc of uint16
    | Stloc_s of int8
    | Ldc_I8 of int64
    | Ldc_I4 of int32
    | Ldc_R4 of single
    | Ldc_R8 of float
    | Ldc_I4_s of int8
    | Br of int32
    | Br_s of int8
    | Brfalse_s of int8
    | Brtrue_s of int8
    | Brfalse of int32
    | Brtrue of int32
    | Beq_s of int8
    | Blt_s of int8
    | Ble_s of int8
    | Bgt_s of int8
    | Bge_s of int8
    | Beq of int32
    | Blt of int32
    | Ble of int32
    | Bgt of int32
    | Bge of int32
    | Bne_un_s of int8
    | Bge_un_s of int8
    | Bgt_un_s of int8
    | Ble_un_s of int8
    | Blt_un_s of int8
    | Bne_un of int32
    | Bge_un of int32
    | Bgt_un of int32
    | Ble_un of int32
    | Blt_un of int32
    /// Loads the local variable at a specific index onto the evaluation stack.
    | Ldloc_s of uint8
    | Ldloca_s of uint8
    /// Load the address of an argument onto the stack.
    | Ldarga of uint16
    | Ldarg_s of uint8
    /// Load the address of an argument onto the stack.
    | Ldarga_s of uint8
    /// Unconditionally transfer control to this offset from the next instruction;
    /// like Br but can leave a try/filter/catch block too, and ensures surrounding `finally` blocks execute.
    /// Unconditionally empties the evaluation stack; so a Leave outside an exception-handling block is just a Br which
    /// also clears the eval stack.
    | Leave of int32
    /// Unconditionally transfer control to this offset from the next instruction;
    /// like Br but can leave a try/filter/catch block too, and ensures surrounding `finally` blocks execute.
    /// Unconditionally empties the evaluation stack; so a Leave outside an exception-handling block is just a Br which
    /// also clears the eval stack.
    | Leave_s of int8
    | Starg_s of uint8
    | Starg of uint16
    | Unaligned of uint8
    | Ldloc of uint16
    | Ldloca of uint16
    | Ldarg of uint16

    /// The number of bytes this instruction takes in memory, including its constant argument that is inline in the
    /// byte stream.
    static member NumberOfBytes (op : UnaryConstIlOp) : int =
        match op with
        | Ldarg _uint16
        | Ldarga _uint16
        | Starg _uint16
        | Ldloc _uint16
        | Ldloca _uint16
        | Stloc _uint16 -> 2 + 2 // Two-byte opcode + two-byte argument
        | Ldarg_s _
        | Ldarga_s _
        | Starg_s _
        | Ldloc_s _
        | Ldloca_s _
        | Stloc_s _
        | Ldc_I4_s _
        | Br_s _
        | Brfalse_s _
        | Brtrue_s _
        | Beq_s _
        | Blt_s _
        | Ble_s _
        | Bgt_s _
        | Bge_s _
        | Bne_un_s _
        | Bge_un_s _
        | Bgt_un_s _
        | Ble_un_s _
        | Blt_un_s _
        | Leave_s _
        | Unaligned _ -> 1 + 1 // One-byte opcode + one-byte argument
        | Ldc_I8 _ -> 1 + 8 // One-byte opcode + 8-byte argument
        | Ldc_I4 _
        | Br _
        | Brfalse _
        | Brtrue _
        | Beq _
        | Blt _
        | Ble _
        | Bgt _
        | Bge _
        | Bne_un _
        | Bge_un _
        | Bgt_un _
        | Ble_un _
        | Blt_un _
        | Leave _ -> 1 + 4 // One-byte opcode + 4-byte argument
        | Ldc_R4 _ -> 1 + 4 // One-byte opcode + 4-byte argument
        | Ldc_R8 _ -> 1 + 8 // One-byte opcode + 8-byte argument

    override this.ToString () =
        match this with
        | UnaryConstIlOp.Stloc i -> $"Stloc %i{i}"
        | UnaryConstIlOp.Stloc_s i -> $"Stloc_s %i{i}"
        | UnaryConstIlOp.Ldc_I8 i -> $"Ldc_I8 %i{i}"
        | UnaryConstIlOp.Ldc_I4 i -> $"Ldc_I4 %i{i}"
        | UnaryConstIlOp.Ldc_R4 f -> $"Ldc_R4 %f{f}"
        | UnaryConstIlOp.Ldc_R8 f -> $"Ldc_R8 %f{f}"
        | UnaryConstIlOp.Ldc_I4_s i -> $"Ldc_I4_s %i{i}"
        | UnaryConstIlOp.Br i -> $"Br %i{i}"
        | UnaryConstIlOp.Br_s i -> $"Br_s %i{i}"
        | UnaryConstIlOp.Brfalse_s i -> $"Brfalse_s %i{i}"
        | UnaryConstIlOp.Brtrue_s i -> $"Brtrue_s %i{i}"
        | UnaryConstIlOp.Brfalse i -> $"Brfalse %i{i}"
        | UnaryConstIlOp.Brtrue i -> $"Brtrue %i{i}"
        | UnaryConstIlOp.Beq_s i -> $"Beq_s %i{i}"
        | UnaryConstIlOp.Blt_s i -> $"Blt_s %i{i}"
        | UnaryConstIlOp.Ble_s i -> $"Ble_s %i{i}"
        | UnaryConstIlOp.Bgt_s i -> $"Bgt_s %i{i}"
        | UnaryConstIlOp.Bge_s i -> $"Bge_s %i{i}"
        | UnaryConstIlOp.Beq i -> $"Beq %i{i}"
        | UnaryConstIlOp.Blt i -> $"Blt %i{i}"
        | UnaryConstIlOp.Ble i -> $"Ble %i{i}"
        | UnaryConstIlOp.Bgt i -> $"Bgt %i{i}"
        | UnaryConstIlOp.Bge i -> $"Bge %i{i}"
        | UnaryConstIlOp.Bne_un_s i -> $"Bne_un_s %i{i}"
        | UnaryConstIlOp.Bge_un_s i -> $"Bge_un_s %i{i}"
        | UnaryConstIlOp.Bgt_un_s i -> $"Bgt_un_s %i{i}"
        | UnaryConstIlOp.Ble_un_s i -> $"Ble_un_s %i{i}"
        | UnaryConstIlOp.Blt_un_s i -> $"Blt_un_s %i{i}"
        | UnaryConstIlOp.Bne_un i -> $"Bne_un %i{i}"
        | UnaryConstIlOp.Bge_un i -> $"Bge_un %i{i}"
        | UnaryConstIlOp.Bgt_un i -> $"Bgt_un %i{i}"
        | UnaryConstIlOp.Ble_un i -> $"Ble_un %i{i}"
        | UnaryConstIlOp.Blt_un i -> $"Blt_un %i{i}"
        | UnaryConstIlOp.Ldloc_s i -> $"Ldloc_s %i{i}"
        | UnaryConstIlOp.Ldloca_s i -> $"Ldloca_s %i{i}"
        | UnaryConstIlOp.Ldarga i -> $"Ldarga %i{i}"
        | UnaryConstIlOp.Ldarg_s i -> $"Ldarg_s %i{i}"
        | UnaryConstIlOp.Ldarga_s i -> $"Ldarga_s %i{i}"
        | UnaryConstIlOp.Leave i -> $"Leave %i{i}"
        | UnaryConstIlOp.Leave_s i -> $"Leave_s %i{i}"
        | UnaryConstIlOp.Starg_s i -> $"Starg_s %i{i}"
        | UnaryConstIlOp.Starg i -> $"Starg %i{i}"
        | UnaryConstIlOp.Unaligned i -> $"Unaligned %i{i}"
        | UnaryConstIlOp.Ldloc i -> $"Ldloc %i{i}"
        | UnaryConstIlOp.Ldloca i -> $"Ldloca %i{i}"
        | UnaryConstIlOp.Ldarg i -> $"Ldarg %i{i}"

type UnaryMetadataTokenIlOp =
    | Call
    | Calli
    | Callvirt
    | Castclass
    | Newobj
    | Newarr
    | Box
    | Ldelema
    | Isinst
    /// Pop value from stack; pop object ref from stack; set specified field on that object to that value.
    | Stfld
    /// Pop value from eval stack; set specified static field to that value.
    | Stsfld
    /// Pop object ref from eval stack; look up specified field on that object; push field's value to eval stack.
    | Ldfld
    /// Pop object ref from eval stack; find address of specified field on that object; push address to eval stack.
    | Ldflda
    /// Push value of specified static field onto eval stack.
    | Ldsfld
    | Ldsflda
    | Unbox_Any
    /// Replaces the array element at a given index with the value on the eval stack
    | Stelem
    /// Pushes the element at a specified array index onto the eval stack
    | Ldelem
    | Initobj
    /// Pushes an unmanaged pointer (native int) to the stack, pointing to native code.
    | Ldftn
    | Stobj
    | Constrained
    | Ldtoken
    | Cpobj
    | Ldobj
    | Sizeof
    | Unbox
    | Ldvirtftn
    | Mkrefany
    | Refanyval
    | Jmp

    override this.ToString () =
        match this with
        | UnaryMetadataTokenIlOp.Call -> "Call"
        | UnaryMetadataTokenIlOp.Calli -> "Calli"
        | UnaryMetadataTokenIlOp.Callvirt -> "Callvirt"
        | UnaryMetadataTokenIlOp.Castclass -> "Castclass"
        | UnaryMetadataTokenIlOp.Newobj -> "Newobj"
        | UnaryMetadataTokenIlOp.Newarr -> "Newarr"
        | UnaryMetadataTokenIlOp.Box -> "Box"
        | UnaryMetadataTokenIlOp.Ldelema -> "Ldelema"
        | UnaryMetadataTokenIlOp.Isinst -> "Isinst"
        | UnaryMetadataTokenIlOp.Stfld -> "Stfld"
        | UnaryMetadataTokenIlOp.Stsfld -> "Stsfld"
        | UnaryMetadataTokenIlOp.Ldfld -> "Ldfld"
        | UnaryMetadataTokenIlOp.Ldflda -> "Ldflda"
        | UnaryMetadataTokenIlOp.Ldsfld -> "Ldsfld"
        | UnaryMetadataTokenIlOp.Ldsflda -> "Ldsflda"
        | UnaryMetadataTokenIlOp.Unbox_Any -> "Unbox_Any"
        | UnaryMetadataTokenIlOp.Stelem -> "Stelem"
        | UnaryMetadataTokenIlOp.Ldelem -> "Ldelem"
        | UnaryMetadataTokenIlOp.Initobj -> "Initobj"
        | UnaryMetadataTokenIlOp.Ldftn -> "Ldftn"
        | UnaryMetadataTokenIlOp.Stobj -> "Stobj"
        | UnaryMetadataTokenIlOp.Constrained -> "Constrained"
        | UnaryMetadataTokenIlOp.Ldtoken -> "Ldtoken"
        | UnaryMetadataTokenIlOp.Cpobj -> "Cpobj"
        | UnaryMetadataTokenIlOp.Ldobj -> "Ldobj"
        | UnaryMetadataTokenIlOp.Sizeof -> "Sizeof"
        | UnaryMetadataTokenIlOp.Unbox -> "Unbox"
        | UnaryMetadataTokenIlOp.Ldvirtftn -> "Ldvirtftn"
        | UnaryMetadataTokenIlOp.Mkrefany -> "Mkrefany"
        | UnaryMetadataTokenIlOp.Refanyval -> "Refanyval"
        | UnaryMetadataTokenIlOp.Jmp -> "Jmp"

    /// The number of bytes this instruction takes in memory, including its metadata token argument.
    static member NumberOfBytes (op : UnaryMetadataTokenIlOp) : int =
        match op with
        | Ldftn
        | Ldvirtftn
        | Initobj
        | Constrained
        | Sizeof -> 2 + 4 // Two-byte opcode + 4-byte token
        | Call
        | Calli
        | Callvirt
        | Castclass
        | Newobj
        | Newarr
        | Box
        | Ldelema
        | Isinst
        | Stfld
        | Stsfld
        | Ldfld
        | Ldflda
        | Ldsfld
        | Ldsflda
        | Unbox_Any
        | Stelem
        | Ldelem
        | Stobj
        | Ldtoken
        | Cpobj
        | Ldobj
        | Unbox
        | Mkrefany
        | Refanyval
        | Jmp -> 1 + 4 // One-byte opcode + 4-byte token

type UnaryStringTokenIlOp =
    | Ldstr

    static member NumberOfBytes (op : UnaryStringTokenIlOp) : int =
        match op with
        | Ldstr -> 1 + 4

type IlOp =
    | Nullary of NullaryIlOp
    | UnaryConst of UnaryConstIlOp
    | UnaryMetadataToken of UnaryMetadataTokenIlOp * MetadataToken
    | UnaryStringToken of UnaryStringTokenIlOp * StringToken
    | Switch of int32 ImmutableArray

    override this.ToString () =
        match this with
        | IlOp.Nullary op -> $"Nullary %O{op}"
        | IlOp.UnaryConst op -> $"UnaryConst.%O{op}"
        | IlOp.UnaryMetadataToken (op, _) -> $"UnaryMetadataToken.%O{op}"
        | IlOp.UnaryStringToken (op, _) -> $"UnaryStringToken.%O{op}"
        | IlOp.Switch arr -> $"Switch[%i{arr.Length}]"

    static member Format (opCode : IlOp) (offset : int) : string = $"    IL_%04X{offset}: %-20O{opCode}"

    static member NumberOfBytes (op : IlOp) =
        match op with
        | Nullary op -> NullaryIlOp.NumberOfBytes op
        | UnaryConst op -> UnaryConstIlOp.NumberOfBytes op
        | UnaryMetadataToken (op, _) -> UnaryMetadataTokenIlOp.NumberOfBytes op
        | UnaryStringToken (op, _) -> UnaryStringTokenIlOp.NumberOfBytes op
        | Switch arr -> 1 + 4 + arr.Length * 4
