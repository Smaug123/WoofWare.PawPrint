namespace WoofWare.PawPrint

#nowarn "9"

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.FSharp.Core

type Parameter =
    {
        Name : string
        DefaultValue : Constant
        SequenceNumber : int
    }

type GenericParameter =
    {
        Name : string
        SequenceNumber : int
    }

type TypeMethodSignature<'Types> =
    {
        Header : SignatureHeader
        ParameterTypes : ImmutableArray<'Types>
        GenericParameterCount : int
        RequiredParameterCount : int
        ReturnType : 'Types
    }

[<RequireQualifiedAccess>]
module TypeMethodSignature =
    let make<'T> (p : MethodSignature<'T>) : TypeMethodSignature<'T> =
        {
            Header = p.Header
            ReturnType = p.ReturnType
            ParameterTypes = p.ParameterTypes
            GenericParameterCount = p.GenericParameterCount
            RequiredParameterCount = p.RequiredParameterCount
        }

type PrimitiveType =
    | Void
    | Boolean
    | Char
    | SByte
    | Byte
    | Int16
    | UInt16
    | Int32
    | UInt32
    | Int64
    | UInt64
    | Single
    | Double
    | String
    | TypedReference
    | IntPtr
    | UIntPtr
    | Object

    static member OfEnum (ptc : PrimitiveTypeCode) : PrimitiveType =
        match ptc with
        | PrimitiveTypeCode.Void -> PrimitiveType.Void
        | PrimitiveTypeCode.Boolean -> PrimitiveType.Boolean
        | PrimitiveTypeCode.Char -> PrimitiveType.Char
        | PrimitiveTypeCode.SByte -> PrimitiveType.SByte
        | PrimitiveTypeCode.Byte -> PrimitiveType.Byte
        | PrimitiveTypeCode.Int16 -> PrimitiveType.Int16
        | PrimitiveTypeCode.UInt16 -> PrimitiveType.UInt16
        | PrimitiveTypeCode.Int32 -> PrimitiveType.Int32
        | PrimitiveTypeCode.UInt32 -> PrimitiveType.UInt32
        | PrimitiveTypeCode.Int64 -> PrimitiveType.Int64
        | PrimitiveTypeCode.UInt64 -> PrimitiveType.UInt64
        | PrimitiveTypeCode.Single -> PrimitiveType.Single
        | PrimitiveTypeCode.Double -> PrimitiveType.Double
        | PrimitiveTypeCode.String -> PrimitiveType.String
        | PrimitiveTypeCode.TypedReference -> PrimitiveType.TypedReference
        | PrimitiveTypeCode.IntPtr -> PrimitiveType.IntPtr
        | PrimitiveTypeCode.UIntPtr -> PrimitiveType.UIntPtr
        | PrimitiveTypeCode.Object -> PrimitiveType.Object
        | x -> failwithf $"Unrecognised primitive type code: %O{x}"

type TypeDefn =
    | PrimitiveType of PrimitiveType
    | Pinned of TypeDefn
    | Pointer of TypeDefn
    | Byref of TypeDefn
    | OneDimensionalArrayLowerBoundZero of elements : TypeDefn
    | Modified of original : TypeDefn * afterMod : TypeDefn * modificationRequired : bool
    | FromReference of SignatureTypeKind
    | FromDefinition of SignatureTypeKind
    | GenericInstantiation of generic : TypeDefn * args : ImmutableArray<TypeDefn>
    | FunctionPointer of TypeMethodSignature<TypeDefn>
    | GenericTypeParameter of index : int
    | GenericMethodParameter of index : int


type MethodInfo =
    {
        Handle : MethodDefinitionHandle
        Name : string
        /// also stores the offset of this instruction
        Instructions : (IlOp * int) list
        /// inverted Instructions: a mapping of program counter to op
        Locations : Map<int, IlOp>
        Parameters : Parameter ImmutableArray
        Generics : GenericParameter ImmutableArray
        Signature : TypeMethodSignature<TypeDefn>
    }

type TypeInfo =
    {
        Namespace : string
        Name : string
        Methods : MethodInfo list
    }

[<RequireQualifiedAccess>]
module TypeInfo =
    let private readOpCode (reader : byref<BlobReader>) : ILOpCode =
        let op = reader.ReadByte ()

        if op = 0xFEuy then
            let op2 = reader.ReadByte ()
            LanguagePrimitives.EnumOfValue (0xFE00us ||| (uint16 op2))
        else
            LanguagePrimitives.EnumOfValue (uint16 op)

    let private readMetadataToken (reader : byref<BlobReader>) : MetadataToken =
        [|
            reader.ReadByte ()
            reader.ReadByte ()
            reader.ReadByte ()
            reader.ReadByte ()
        |]

    let private readMethodBody (peReader : PEReader) (methodDef : MethodDefinition) : (IlOp * int) list =
        if methodDef.RelativeVirtualAddress = 0 then
            []
        else
            let methodBody = peReader.GetMethodBody (methodDef.RelativeVirtualAddress)
            let ilBytes = methodBody.GetILBytes ()
            use bytes = fixed ilBytes
            let mutable reader : BlobReader = BlobReader (bytes, ilBytes.Length)

            let rec readInstructions acc =
                if reader.Offset >= ilBytes.Length then
                    List.rev acc
                else
                    let offset = reader.Offset
                    let opCode = readOpCode (&reader)

                    let opCode =
                        match opCode with
                        | ILOpCode.Nop -> IlOp.Nullary NullaryIlOp.Nop
                        | ILOpCode.Break -> failwith "todo"
                        | ILOpCode.Ldarg_0 -> IlOp.Nullary NullaryIlOp.LdArg0
                        | ILOpCode.Ldarg_1 -> IlOp.Nullary NullaryIlOp.LdArg1
                        | ILOpCode.Ldarg_2 -> IlOp.Nullary NullaryIlOp.LdArg2
                        | ILOpCode.Ldarg_3 -> IlOp.Nullary NullaryIlOp.LdArg3
                        | ILOpCode.Ldloc_0 -> IlOp.Nullary NullaryIlOp.Ldloc_0
                        | ILOpCode.Ldloc_1 -> IlOp.Nullary NullaryIlOp.Ldloc_1
                        | ILOpCode.Ldloc_2 -> IlOp.Nullary NullaryIlOp.Ldloc_2
                        | ILOpCode.Ldloc_3 -> IlOp.Nullary NullaryIlOp.Ldloc_3
                        | ILOpCode.Stloc_0 -> IlOp.Nullary NullaryIlOp.Stloc_0
                        | ILOpCode.Stloc_1 -> IlOp.Nullary NullaryIlOp.Stloc_1
                        | ILOpCode.Stloc_2 -> IlOp.Nullary NullaryIlOp.Stloc_2
                        | ILOpCode.Stloc_3 -> IlOp.Nullary NullaryIlOp.Stloc_3
                        | ILOpCode.Ldarg_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldarg_s (reader.ReadByte ()))
                        | ILOpCode.Ldarga_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldarga_s (reader.ReadByte ()))
                        | ILOpCode.Starg_s -> IlOp.UnaryConst (UnaryConstIlOp.Starg_s (reader.ReadByte ()))
                        | ILOpCode.Ldloc_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldloc_s (reader.ReadByte ()))
                        | ILOpCode.Ldloca_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldloca_s (reader.ReadByte ()))
                        | ILOpCode.Stloc_s -> IlOp.UnaryConst (UnaryConstIlOp.Stloc_s (reader.ReadSByte ()))
                        | ILOpCode.Ldnull -> IlOp.Nullary NullaryIlOp.LdNull
                        | ILOpCode.Ldc_i4_m1 -> IlOp.Nullary NullaryIlOp.LdcI4_m1
                        | ILOpCode.Ldc_i4_0 -> IlOp.Nullary NullaryIlOp.LdcI4_0
                        | ILOpCode.Ldc_i4_1 -> IlOp.Nullary NullaryIlOp.LdcI4_1
                        | ILOpCode.Ldc_i4_2 -> IlOp.Nullary NullaryIlOp.LdcI4_2
                        | ILOpCode.Ldc_i4_3 -> IlOp.Nullary NullaryIlOp.LdcI4_3
                        | ILOpCode.Ldc_i4_4 -> IlOp.Nullary NullaryIlOp.LdcI4_4
                        | ILOpCode.Ldc_i4_5 -> IlOp.Nullary NullaryIlOp.LdcI4_5
                        | ILOpCode.Ldc_i4_6 -> IlOp.Nullary NullaryIlOp.LdcI4_6
                        | ILOpCode.Ldc_i4_7 -> IlOp.Nullary NullaryIlOp.LdcI4_7
                        | ILOpCode.Ldc_i4_8 -> IlOp.Nullary NullaryIlOp.LdcI4_8
                        | ILOpCode.Ldc_i4_s -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4_s (reader.ReadSByte ()))
                        | ILOpCode.Ldc_i4 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 (reader.ReadInt32 ()))
                        | ILOpCode.Ldc_i8 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_I8 (reader.ReadInt64 ()))
                        | ILOpCode.Ldc_r4 -> failwith "todo"
                        | ILOpCode.Ldc_r8 -> failwith "todo"
                        | ILOpCode.Dup -> IlOp.Nullary NullaryIlOp.Dup
                        | ILOpCode.Pop -> IlOp.Nullary NullaryIlOp.Pop
                        | ILOpCode.Jmp -> failwith "todo"
                        | ILOpCode.Call ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, readMetadataToken &reader)
                        | ILOpCode.Calli -> failwith "todo"
                        | ILOpCode.Ret -> IlOp.Nullary NullaryIlOp.Ret
                        | ILOpCode.Br_s -> IlOp.UnaryConst (UnaryConstIlOp.Br_s (reader.ReadSByte ()))
                        | ILOpCode.Brfalse_s -> IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s (reader.ReadSByte ()))
                        | ILOpCode.Brtrue_s -> IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s (reader.ReadSByte ()))
                        | ILOpCode.Beq_s -> IlOp.UnaryConst (UnaryConstIlOp.Beq_s (reader.ReadSByte ()))
                        | ILOpCode.Bge_s -> IlOp.UnaryConst (UnaryConstIlOp.Bge_s (reader.ReadSByte ()))
                        | ILOpCode.Bgt_s -> IlOp.UnaryConst (UnaryConstIlOp.Bgt_s (reader.ReadSByte ()))
                        | ILOpCode.Ble_s -> IlOp.UnaryConst (UnaryConstIlOp.Ble_s (reader.ReadSByte ()))
                        | ILOpCode.Blt_s -> IlOp.UnaryConst (UnaryConstIlOp.Blt_s (reader.ReadSByte ()))
                        | ILOpCode.Bne_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Bne_un_s (reader.ReadSByte ()))
                        | ILOpCode.Bge_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Bge_un_s (reader.ReadSByte ()))
                        | ILOpCode.Bgt_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Bgt_un_s (reader.ReadSByte ()))
                        | ILOpCode.Ble_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Ble_un_s (reader.ReadSByte ()))
                        | ILOpCode.Blt_un_s -> IlOp.UnaryConst (UnaryConstIlOp.Blt_un_s (reader.ReadSByte ()))
                        | ILOpCode.Br -> IlOp.UnaryConst (UnaryConstIlOp.Br (reader.ReadInt32 ()))
                        | ILOpCode.Brfalse -> IlOp.UnaryConst (UnaryConstIlOp.Brfalse (reader.ReadInt32 ()))
                        | ILOpCode.Brtrue -> IlOp.UnaryConst (UnaryConstIlOp.Brtrue (reader.ReadInt32 ()))
                        | ILOpCode.Beq -> failwith "todo"
                        | ILOpCode.Bge -> failwith "todo"
                        | ILOpCode.Bgt -> failwith "todo"
                        | ILOpCode.Ble -> failwith "todo"
                        | ILOpCode.Blt -> failwith "todo"
                        | ILOpCode.Bne_un -> IlOp.UnaryConst (UnaryConstIlOp.Bne_un (reader.ReadInt32 ()))
                        | ILOpCode.Bge_un -> IlOp.UnaryConst (UnaryConstIlOp.Bge_un (reader.ReadInt32 ()))
                        | ILOpCode.Bgt_un -> IlOp.UnaryConst (UnaryConstIlOp.Bgt_un (reader.ReadInt32 ()))
                        | ILOpCode.Ble_un -> IlOp.UnaryConst (UnaryConstIlOp.Ble_un (reader.ReadInt32 ()))
                        | ILOpCode.Blt_un -> IlOp.UnaryConst (UnaryConstIlOp.Blt_un (reader.ReadInt32 ()))
                        | ILOpCode.Switch ->
                            let count = reader.ReadUInt32 ()

                            if count > uint32 Int32.MaxValue then
                                failwith "Debugger error: can't create a jump table with more than int32.Max entries"

                            let count = int count
                            let result = ImmutableArray.CreateBuilder count

                            for i = 0 to count - 1 do
                                result.Add (reader.ReadInt32 ())

                            IlOp.Switch (result.ToImmutable ())
                        | ILOpCode.Ldind_i1 -> failwith "todo"
                        | ILOpCode.Ldind_u1 -> failwith "todo"
                        | ILOpCode.Ldind_i2 -> failwith "todo"
                        | ILOpCode.Ldind_u2 -> failwith "todo"
                        | ILOpCode.Ldind_i4 -> failwith "todo"
                        | ILOpCode.Ldind_u4 -> failwith "todo"
                        | ILOpCode.Ldind_i8 -> failwith "todo"
                        | ILOpCode.Ldind_i -> failwith "todo"
                        | ILOpCode.Ldind_r4 -> failwith "todo"
                        | ILOpCode.Ldind_r8 -> failwith "todo"
                        | ILOpCode.Ldind_ref -> failwith "todo"
                        | ILOpCode.Stind_ref -> failwith "todo"
                        | ILOpCode.Stind_i1 -> failwith "todo"
                        | ILOpCode.Stind_i2 -> failwith "todo"
                        | ILOpCode.Stind_i4 -> failwith "todo"
                        | ILOpCode.Stind_i8 -> failwith "todo"
                        | ILOpCode.Stind_r4 -> failwith "todo"
                        | ILOpCode.Stind_r8 -> failwith "todo"
                        | ILOpCode.Add -> IlOp.Nullary NullaryIlOp.Add
                        | ILOpCode.Sub -> IlOp.Nullary NullaryIlOp.Sub
                        | ILOpCode.Mul -> IlOp.Nullary NullaryIlOp.Mul
                        | ILOpCode.Div -> IlOp.Nullary NullaryIlOp.Div
                        | ILOpCode.Div_un -> IlOp.Nullary NullaryIlOp.Div_un
                        | ILOpCode.Rem -> failwith "todo"
                        | ILOpCode.Rem_un -> failwith "todo"
                        | ILOpCode.And -> IlOp.Nullary NullaryIlOp.And
                        | ILOpCode.Or -> IlOp.Nullary NullaryIlOp.Or
                        | ILOpCode.Xor -> IlOp.Nullary NullaryIlOp.Xor
                        | ILOpCode.Shl -> IlOp.Nullary NullaryIlOp.Shl
                        | ILOpCode.Shr -> IlOp.Nullary NullaryIlOp.Shr
                        | ILOpCode.Shr_un -> IlOp.Nullary NullaryIlOp.Shr_un
                        | ILOpCode.Neg -> failwith "todo"
                        | ILOpCode.Not -> failwith "todo"
                        | ILOpCode.Conv_i1 -> IlOp.Nullary NullaryIlOp.Conv_I1
                        | ILOpCode.Conv_i2 -> IlOp.Nullary NullaryIlOp.Conv_I2
                        | ILOpCode.Conv_i4 -> IlOp.Nullary NullaryIlOp.Conv_I4
                        | ILOpCode.Conv_i8 -> IlOp.Nullary NullaryIlOp.Conv_I8
                        | ILOpCode.Conv_r4 -> IlOp.Nullary NullaryIlOp.Conv_R4
                        | ILOpCode.Conv_r8 -> IlOp.Nullary NullaryIlOp.Conv_R8
                        | ILOpCode.Conv_u4 -> IlOp.Nullary NullaryIlOp.Conv_U4
                        | ILOpCode.Conv_u8 -> IlOp.Nullary NullaryIlOp.Conv_U8
                        | ILOpCode.Callvirt ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, readMetadataToken &reader)
                        | ILOpCode.Cpobj -> failwith "todo"
                        | ILOpCode.Ldobj -> failwith "todo"
                        | ILOpCode.Ldstr ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldstr, readMetadataToken &reader)
                        | ILOpCode.Newobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, readMetadataToken &reader)
                        | ILOpCode.Castclass ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Castclass, readMetadataToken &reader)
                        | ILOpCode.Isinst ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Isinst, readMetadataToken &reader)
                        | ILOpCode.Conv_r_un -> failwith "todo"
                        | ILOpCode.Unbox -> failwith "todo"
                        | ILOpCode.Throw -> IlOp.Nullary NullaryIlOp.Throw
                        | ILOpCode.Ldfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, readMetadataToken &reader)
                        | ILOpCode.Ldflda ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldflda, readMetadataToken &reader)
                        | ILOpCode.Stfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stfld, readMetadataToken &reader)
                        | ILOpCode.Ldsfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, readMetadataToken &reader)
                        | ILOpCode.Ldsflda -> failwith "todo"
                        | ILOpCode.Stsfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stsfld, readMetadataToken &reader)
                        | ILOpCode.Stobj -> failwith "todo"
                        | ILOpCode.Conv_ovf_i1_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_i2_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_i4_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_i8_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_u1_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_u2_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_u4_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_u8_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_i_un -> failwith "todo"
                        | ILOpCode.Conv_ovf_u_un -> failwith "todo"
                        | ILOpCode.Box ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, readMetadataToken &reader)
                        | ILOpCode.Newarr ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, readMetadataToken &reader)
                        | ILOpCode.Ldlen -> IlOp.Nullary NullaryIlOp.LdLen
                        | ILOpCode.Ldelema ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelema, readMetadataToken &reader)
                        | ILOpCode.Ldelem_i1 -> failwith "todo"
                        | ILOpCode.Ldelem_u1 -> failwith "todo"
                        | ILOpCode.Ldelem_i2 -> failwith "todo"
                        | ILOpCode.Ldelem_u2 -> failwith "todo"
                        | ILOpCode.Ldelem_i4 -> failwith "todo"
                        | ILOpCode.Ldelem_u4 -> failwith "todo"
                        | ILOpCode.Ldelem_i8 -> failwith "todo"
                        | ILOpCode.Ldelem_i -> failwith "todo"
                        | ILOpCode.Ldelem_r4 -> failwith "todo"
                        | ILOpCode.Ldelem_r8 -> failwith "todo"
                        | ILOpCode.Ldelem_ref -> failwith "todo"
                        | ILOpCode.Stelem_i -> failwith "todo"
                        | ILOpCode.Stelem_i1 -> failwith "todo"
                        | ILOpCode.Stelem_i2 -> failwith "todo"
                        | ILOpCode.Stelem_i4 -> failwith "todo"
                        | ILOpCode.Stelem_i8 -> failwith "todo"
                        | ILOpCode.Stelem_r4 -> failwith "todo"
                        | ILOpCode.Stelem_r8 -> failwith "todo"
                        | ILOpCode.Stelem_ref -> failwith "todo"
                        | ILOpCode.Ldelem ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelem, readMetadataToken &reader)
                        | ILOpCode.Stelem ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stelem, readMetadataToken &reader)
                        | ILOpCode.Unbox_any ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox_Any, readMetadataToken &reader)
                        | ILOpCode.Conv_ovf_i1 -> failwith "todo"
                        | ILOpCode.Conv_ovf_u1 -> failwith "todo"
                        | ILOpCode.Conv_ovf_i2 -> failwith "todo"
                        | ILOpCode.Conv_ovf_u2 -> failwith "todo"
                        | ILOpCode.Conv_ovf_i4 -> failwith "todo"
                        | ILOpCode.Conv_ovf_u4 -> failwith "todo"
                        | ILOpCode.Conv_ovf_i8 -> failwith "todo"
                        | ILOpCode.Conv_ovf_u8 -> failwith "todo"
                        | ILOpCode.Refanyval -> failwith "todo"
                        | ILOpCode.Ckfinite -> failwith "todo"
                        | ILOpCode.Mkrefany -> failwith "todo"
                        | ILOpCode.Ldtoken -> failwith "todo"
                        | ILOpCode.Conv_u2 -> IlOp.Nullary NullaryIlOp.Conv_U2
                        | ILOpCode.Conv_u1 -> IlOp.Nullary NullaryIlOp.Conv_U1
                        | ILOpCode.Conv_i -> IlOp.Nullary NullaryIlOp.Conv_I
                        | ILOpCode.Conv_ovf_i -> failwith "todo"
                        | ILOpCode.Conv_ovf_u -> failwith "todo"
                        | ILOpCode.Add_ovf -> failwith "todo"
                        | ILOpCode.Add_ovf_un -> failwith "todo"
                        | ILOpCode.Mul_ovf -> failwith "todo"
                        | ILOpCode.Mul_ovf_un -> failwith "todo"
                        | ILOpCode.Sub_ovf -> failwith "todo"
                        | ILOpCode.Sub_ovf_un -> failwith "todo"
                        | ILOpCode.Endfinally -> IlOp.Nullary NullaryIlOp.Endfinally
                        | ILOpCode.Leave -> IlOp.UnaryConst (UnaryConstIlOp.Leave (reader.ReadInt32 ()))
                        | ILOpCode.Leave_s -> IlOp.UnaryConst (UnaryConstIlOp.Leave_s (reader.ReadSByte ()))
                        | ILOpCode.Stind_i -> failwith "todo"
                        | ILOpCode.Conv_u -> failwith "todo"
                        | ILOpCode.Arglist -> failwith "todo"
                        | ILOpCode.Ceq -> IlOp.Nullary NullaryIlOp.Ceq
                        | ILOpCode.Cgt -> IlOp.Nullary NullaryIlOp.Cgt
                        | ILOpCode.Cgt_un -> IlOp.Nullary NullaryIlOp.Cgt_un
                        | ILOpCode.Clt -> IlOp.Nullary NullaryIlOp.Clt
                        | ILOpCode.Clt_un -> IlOp.Nullary NullaryIlOp.Clt_un
                        | ILOpCode.Ldftn -> failwith "todo"
                        | ILOpCode.Ldvirtftn -> failwith "todo"
                        | ILOpCode.Ldarg -> failwith "todo"
                        | ILOpCode.Ldarga -> failwith "todo"
                        | ILOpCode.Starg -> IlOp.UnaryConst (UnaryConstIlOp.Starg (reader.ReadUInt16 ()))
                        | ILOpCode.Ldloc -> failwith "todo"
                        | ILOpCode.Ldloca -> failwith "todo"
                        | ILOpCode.Stloc -> IlOp.UnaryConst (UnaryConstIlOp.Stloc (reader.ReadUInt16 ()))
                        | ILOpCode.Localloc -> failwith "todo"
                        | ILOpCode.Endfilter -> IlOp.Nullary NullaryIlOp.Endfilter
                        | ILOpCode.Unaligned -> failwith "todo"
                        | ILOpCode.Volatile -> failwith "todo"
                        | ILOpCode.Tail -> failwith "todo"
                        | ILOpCode.Initobj -> failwith "todo"
                        | ILOpCode.Constrained -> failwith "todo"
                        | ILOpCode.Cpblk -> failwith "todo"
                        | ILOpCode.Initblk -> failwith "todo"
                        | ILOpCode.Rethrow -> IlOp.Nullary NullaryIlOp.Rethrow
                        | ILOpCode.Sizeof -> failwith "todo"
                        | ILOpCode.Refanytype -> failwith "todo"
                        | ILOpCode.Readonly -> failwith "todo"
                        | i -> failwithf "Unknown opcode: %A" i

                    readInstructions ((opCode, offset) :: acc)

            readInstructions []

    let private readMethodParams
        (metadata : MetadataReader)
        (param : ParameterHandleCollection)
        : Parameter ImmutableArray
        =
        param
        |> Seq.map (fun param ->
            let param = metadata.GetParameter param

            {
                Name = metadata.GetString param.Name
                DefaultValue = metadata.GetConstant (param.GetDefaultValue ())
                SequenceNumber = param.SequenceNumber
            }
        )
        |> ImmutableArray.CreateRange

    let private readGenericMethodParam
        (metadata : MetadataReader)
        (param : GenericParameterHandleCollection)
        : GenericParameter ImmutableArray
        =
        param
        |> Seq.map (fun param ->
            let param = metadata.GetGenericParameter param

            {
                Name = metadata.GetString param.Name
                SequenceNumber = param.Index
            }
        )
        |> ImmutableArray.CreateRange

    let private typeProvider =
        { new ISignatureTypeProvider<TypeDefn, unit> with
            member this.GetArrayType (elementType : TypeDefn, shape : ArrayShape) : TypeDefn = failwith "TODO"
            member this.GetByReferenceType (elementType : TypeDefn) : TypeDefn = TypeDefn.Byref elementType

            member this.GetSZArrayType (elementType : TypeDefn) : TypeDefn =
                TypeDefn.OneDimensionalArrayLowerBoundZero elementType

            member this.GetPrimitiveType (elementType : PrimitiveTypeCode) : TypeDefn =
                PrimitiveType.OfEnum elementType |> TypeDefn.PrimitiveType

            member this.GetGenericInstantiation
                (generic : TypeDefn, typeArguments : ImmutableArray<TypeDefn>)
                : TypeDefn
                =
                TypeDefn.GenericInstantiation (generic, typeArguments)

            member this.GetTypeFromDefinition
                (reader : MetadataReader, handle : TypeDefinitionHandle, rawTypeKind : byte)
                : TypeDefn
                =
                let typeKind = reader.ResolveSignatureTypeKind (handle, rawTypeKind)
                TypeDefn.FromDefinition typeKind

            member this.GetTypeFromReference
                (reader : MetadataReader, foo : TypeReferenceHandle, rawTypeKind : byte)
                : TypeDefn
                =
                let typeKind = reader.ResolveSignatureTypeKind (foo, rawTypeKind)
                TypeDefn.FromReference typeKind

            member this.GetPointerType (typeCode : TypeDefn) : TypeDefn = TypeDefn.Pointer typeCode

            member this.GetFunctionPointerType (signature) =
                TypeDefn.FunctionPointer (TypeMethodSignature.make signature)

            member this.GetGenericMethodParameter (genericContext, index) = TypeDefn.GenericMethodParameter index
            member this.GetGenericTypeParameter (genericContext, index) = TypeDefn.GenericTypeParameter index

            member this.GetModifiedType (modifier, unmodifiedType, isRequired) =
                TypeDefn.Modified (unmodifiedType, modifier, isRequired)

            member this.GetPinnedType (elementType) = TypeDefn.Pinned elementType
            member this.GetTypeFromSpecification (reader, genericContext, handle, rawTypeKind) = failwith "todo"
        }

    let private readMethod
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (methodHandle : MethodDefinitionHandle)
        : MethodInfo
        =
        let methodDef = metadataReader.GetMethodDefinition methodHandle
        let methodName = metadataReader.GetString methodDef.Name
        let methodSig = methodDef.DecodeSignature (typeProvider, ())
        let methodBody = readMethodBody peReader methodDef
        let methodParams = readMethodParams metadataReader (methodDef.GetParameters ())

        let methodGenericParams =
            readGenericMethodParam metadataReader (methodDef.GetGenericParameters ())

        {
            Handle = methodHandle
            Name = methodName
            Instructions = methodBody
            Locations = methodBody |> List.map (fun (a, b) -> b, a) |> Map.ofList
            Parameters = methodParams
            Generics = methodGenericParams
            Signature = TypeMethodSignature.make methodSig
        }

    let internal read
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (typeHandle : TypeDefinitionHandle)
        : TypeInfo
        =
        let typeDef = metadataReader.GetTypeDefinition (typeHandle)
        let methods = typeDef.GetMethods ()

        {
            Namespace = metadataReader.GetString (typeDef.Namespace)
            Name = metadataReader.GetString (typeDef.Name)
            Methods = methods |> Seq.map (readMethod peReader metadataReader) |> Seq.toList
        }
