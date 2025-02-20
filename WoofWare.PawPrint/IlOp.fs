namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

type NullaryIlOp =
    | Nop
    | LdArg0
    | LdArg1
    | LdArg2
    | LdArg3
    | Ldloc_0
    | Ldloc_1
    | Ldloc_2
    | Ldloc_3
    | Pop
    | Dup
    | Ret
    | LdcI4_0
    | LdcI4_1
    | LdcI4_2
    | LdcI4_3
    | LdcI4_4
    | LdcI4_5
    | LdcI4_6
    | LdcI4_7
    | LdcI4_8
    | LdcI4_m1
    | LdNull
    | Ceq
    | Cgt
    | Cgt_un
    | Clt
    | Clt_un
    | Stloc_0
    | Stloc_1
    | Stloc_2
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
    | Shr
    | Shr_un
    | Shl
    | And
    | Or
    | Xor
    | Conv_I
    | Conv_I1
    | Conv_I2
    | Conv_I4
    | Conv_I8
    | Conv_R4
    | Conv_R8
    | Conv_U
    | Conv_U1
    | Conv_U2
    | Conv_U4
    | Conv_U8
    | LdLen
    | Endfilter
    | Endfinally
    | Rethrow
    | Throw

type UnaryConstIlOp =
    | Stloc of uint16
    | Stloc_s of int8
    | Ldc_I8 of int64
    | Ldc_I4 of int32
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
    | Ldloc_s of uint8
    | Ldloca_s of uint8
    | Ldarga of uint16
    | Ldarg_s of uint8
    | Ldarga_s of uint8
    | Leave of int32
    | Leave_s of int8
    | Starg_s of uint8
    | Starg of uint16

type UnaryMetadataTokenIlOp =
    | Call
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
    | Unbox_Any
    | Stelem
    | Ldelem

type UnaryStringTokenIlOp = | Ldstr

/// A four-byte metadata token.
type MetadataToken = EntityHandle
type StringToken = StringHandle

type IlOp =
    | Nullary of NullaryIlOp
    | UnaryConst of UnaryConstIlOp
    | UnaryMetadataToken of UnaryMetadataTokenIlOp * MetadataToken
    | UnaryStringToken of UnaryStringTokenIlOp * StringToken
    | Switch of int32 ImmutableArray

    static member Format (opCode : IlOp) (offset : int) : string = $"    IL_%04X{offset}: %-20O{opCode}"
