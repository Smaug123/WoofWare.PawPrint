namespace WoofWare.PawPrint

#nowarn "9"

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open Microsoft.FSharp.Core

type ExportedTypeData =
    | ForwardsTo of AssemblyReferenceHandle
    | NonForwarded of MetadataToken

type ExportedType =
    {
        Name : string
        Namespace : string
        TypeAttrs : TypeAttributes
        Data : ExportedTypeData
    }

[<RequireQualifiedAccess>]
module ExportedType =
    let make (getString : StringHandle -> string) (ty : System.Reflection.Metadata.ExportedType) : ExportedType =
        let name = getString ty.Name
        let ns = getString ty.Namespace
        let impl = MetadataToken.ofEntityHandle ty.Implementation

        let data =
            if ty.IsForwarder then
                match impl with
                | MetadataToken.AssemblyReference e -> ExportedTypeData.ForwardsTo e
                | _ -> failwith $"Expected forwarder type to have an assembly reference: {impl}"
            else
                ExportedTypeData.NonForwarded impl

        {
            Name = name
            Namespace = ns
            TypeAttrs = ty.Attributes
            Data = data
        }

type CustomAttribute =
    {
        Handle : CustomAttributeHandle
        Constructor : MetadataToken
    }

[<RequireQualifiedAccess>]
module CustomAttribute =
    let make
        (string : StringToken -> string)
        (handle : CustomAttributeHandle)
        (attr : System.Reflection.Metadata.CustomAttribute)
        : CustomAttribute
        =
        let ctor = attr.Constructor |> MetadataToken.ofEntityHandle

        {
            Handle = handle
            Constructor = ctor
        }

type FieldInfo =
    {
        Handle : FieldDefinitionHandle
        Name : string
        DeclaringType : TypeDefinitionHandle
        Signature : TypeDefn
        Attributes : System.Reflection.FieldAttributes
    }

[<RequireQualifiedAccess>]
module FieldInfo =
    let make (getString : StringToken -> string) (handle : FieldDefinitionHandle) (def : FieldDefinition) : FieldInfo =
        let name = StringToken.String def.Name |> getString
        let fieldSig = def.DecodeSignature (TypeDefn.typeProvider, ())
        let declaringType = def.GetDeclaringType ()

        {
            Name = name
            Signature = fieldSig
            DeclaringType = declaringType
            Handle = handle
            Attributes = def.Attributes
        }

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

type MethodSpec =
    {
        Method : MetadataToken
    }

[<RequireQualifiedAccess>]
module MethodSpec =
    let make (p : MethodSpecification) : MethodSpec =
        {
            // Horrible abuse to get this as an int
            Method = MetadataToken.ofInt (p.Method.GetHashCode ())
        }

type MethodInfo =
    {
        DeclaringType : TypeDefinitionHandle * AssemblyName
        Handle : MethodDefinitionHandle
        Name : string
        /// also stores the offset of this instruction
        Instructions : (IlOp * int) list
        /// inverted Instructions: a mapping of program counter to op
        Locations : Map<int, IlOp>
        Parameters : Parameter ImmutableArray
        Generics : GenericParameter ImmutableArray
        Signature : TypeMethodSignature<TypeDefn>
        IsPinvokeImpl : bool
        LocalsInit : bool
        IsStatic : bool
    }

type BaseTypeInfo =
    | TypeDef of TypeDefinitionHandle
    | TypeRef of TypeReferenceHandle
    | ForeignAssemblyType of assemblyName : AssemblyName * TypeDefinitionHandle

type TypeInfo =
    {
        Namespace : string
        Name : string
        Methods : MethodInfo list
        MethodImpls : ImmutableDictionary<MethodImplementationHandle, EntityHandle>
        Fields : FieldInfo list
        BaseType : BaseTypeInfo option
        TypeAttributes : TypeAttributes
        Attributes : CustomAttributeHandle list
    }

type TypeRef =
    {
        Name : string
        Namespace : string
        ResolutionScope : MetadataToken
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
        reader.ReadUInt32 () |> int |> MetadataToken.ofInt

    let private readStringToken (reader : byref<BlobReader>) : StringToken =
        let value = reader.ReadUInt32 () |> int
        StringToken.ofInt value

    type private MethodBody =
        {
            Instructions : (IlOp * int) list
            LocalInit : bool
            MaxStackSize : int
            ExceptionRegions : ImmutableArray<ExceptionRegion>
        }

    let private readMethodBody (peReader : PEReader) (methodDef : MethodDefinition) : MethodBody option =
        if methodDef.RelativeVirtualAddress = 0 then
            None
        else
            let methodBody = peReader.GetMethodBody methodDef.RelativeVirtualAddress
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
                        | ILOpCode.Beq -> IlOp.UnaryConst (UnaryConstIlOp.Beq (reader.ReadInt32 ()))
                        | ILOpCode.Bge -> IlOp.UnaryConst (UnaryConstIlOp.Bge (reader.ReadInt32 ()))
                        | ILOpCode.Bgt -> IlOp.UnaryConst (UnaryConstIlOp.Bgt (reader.ReadInt32 ()))
                        | ILOpCode.Ble -> IlOp.UnaryConst (UnaryConstIlOp.Ble (reader.ReadInt32 ()))
                        | ILOpCode.Blt -> IlOp.UnaryConst (UnaryConstIlOp.Blt (reader.ReadInt32 ()))
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
                        | ILOpCode.Ldind_i -> IlOp.Nullary NullaryIlOp.Ldind_i
                        | ILOpCode.Ldind_i1 -> IlOp.Nullary NullaryIlOp.Ldind_i1
                        | ILOpCode.Ldind_u1 -> IlOp.Nullary NullaryIlOp.Ldind_u1
                        | ILOpCode.Ldind_i2 -> IlOp.Nullary NullaryIlOp.Ldind_i2
                        | ILOpCode.Ldind_u2 -> IlOp.Nullary NullaryIlOp.Ldind_u2
                        | ILOpCode.Ldind_i4 -> IlOp.Nullary NullaryIlOp.Ldind_i4
                        | ILOpCode.Ldind_u4 -> IlOp.Nullary NullaryIlOp.Ldind_u4
                        | ILOpCode.Ldind_i8 -> IlOp.Nullary NullaryIlOp.Ldind_i8
                        | ILOpCode.Ldind_r4 -> IlOp.Nullary NullaryIlOp.Ldind_r4
                        | ILOpCode.Ldind_r8 -> IlOp.Nullary NullaryIlOp.Ldind_r8
                        | ILOpCode.Ldind_ref -> IlOp.Nullary NullaryIlOp.Ldind_ref
                        | ILOpCode.Stind_ref -> IlOp.Nullary NullaryIlOp.Stind_ref
                        | ILOpCode.Stind_i1 -> IlOp.Nullary NullaryIlOp.Stind_I1
                        | ILOpCode.Stind_i2 -> IlOp.Nullary NullaryIlOp.Stind_I2
                        | ILOpCode.Stind_i4 -> IlOp.Nullary NullaryIlOp.Stind_I4
                        | ILOpCode.Stind_i8 -> IlOp.Nullary NullaryIlOp.Stind_I8
                        | ILOpCode.Stind_r4 -> IlOp.Nullary NullaryIlOp.Stind_R4
                        | ILOpCode.Stind_r8 -> IlOp.Nullary NullaryIlOp.Stind_R8
                        | ILOpCode.Add -> IlOp.Nullary NullaryIlOp.Add
                        | ILOpCode.Sub -> IlOp.Nullary NullaryIlOp.Sub
                        | ILOpCode.Mul -> IlOp.Nullary NullaryIlOp.Mul
                        | ILOpCode.Div -> IlOp.Nullary NullaryIlOp.Div
                        | ILOpCode.Div_un -> IlOp.Nullary NullaryIlOp.Div_un
                        | ILOpCode.Rem -> IlOp.Nullary NullaryIlOp.Rem
                        | ILOpCode.Rem_un -> IlOp.Nullary NullaryIlOp.Rem_un
                        | ILOpCode.And -> IlOp.Nullary NullaryIlOp.And
                        | ILOpCode.Or -> IlOp.Nullary NullaryIlOp.Or
                        | ILOpCode.Xor -> IlOp.Nullary NullaryIlOp.Xor
                        | ILOpCode.Shl -> IlOp.Nullary NullaryIlOp.Shl
                        | ILOpCode.Shr -> IlOp.Nullary NullaryIlOp.Shr
                        | ILOpCode.Shr_un -> IlOp.Nullary NullaryIlOp.Shr_un
                        | ILOpCode.Neg -> IlOp.Nullary NullaryIlOp.Neg
                        | ILOpCode.Not -> IlOp.Nullary NullaryIlOp.Not
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
                        | ILOpCode.Cpobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Cpobj, readMetadataToken &reader)
                        | ILOpCode.Ldobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldobj, readMetadataToken &reader)
                        | ILOpCode.Ldstr -> IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, readStringToken &reader)
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
                        | ILOpCode.Ldsflda ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsflda, readMetadataToken &reader)
                        | ILOpCode.Stsfld ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stsfld, readMetadataToken &reader)
                        | ILOpCode.Stobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stobj, readMetadataToken &reader)
                        | ILOpCode.Conv_ovf_i_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i_un
                        | ILOpCode.Conv_ovf_i1_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i1_un
                        | ILOpCode.Conv_ovf_i2_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i2_un
                        | ILOpCode.Conv_ovf_i4_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i4_un
                        | ILOpCode.Conv_ovf_i8_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_i8_un
                        | ILOpCode.Conv_ovf_u_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u_un
                        | ILOpCode.Conv_ovf_u1_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u1_un
                        | ILOpCode.Conv_ovf_u2_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u2_un
                        | ILOpCode.Conv_ovf_u4_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u4_un
                        | ILOpCode.Conv_ovf_u8_un -> IlOp.Nullary NullaryIlOp.Conv_ovf_u8_un
                        | ILOpCode.Box ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, readMetadataToken &reader)
                        | ILOpCode.Newarr ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, readMetadataToken &reader)
                        | ILOpCode.Ldlen -> IlOp.Nullary NullaryIlOp.LdLen
                        | ILOpCode.Ldelema ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelema, readMetadataToken &reader)
                        | ILOpCode.Ldelem_i1 -> IlOp.Nullary NullaryIlOp.Ldelem_i1
                        | ILOpCode.Ldelem_u1 -> IlOp.Nullary NullaryIlOp.Ldelem_u1
                        | ILOpCode.Ldelem_i2 -> IlOp.Nullary NullaryIlOp.Ldelem_i2
                        | ILOpCode.Ldelem_u2 -> IlOp.Nullary NullaryIlOp.Ldelem_u2
                        | ILOpCode.Ldelem_i4 -> IlOp.Nullary NullaryIlOp.Ldelem_i4
                        | ILOpCode.Ldelem_u4 -> IlOp.Nullary NullaryIlOp.Ldelem_u4
                        | ILOpCode.Ldelem_i8 -> IlOp.Nullary NullaryIlOp.Ldelem_i8
                        | ILOpCode.Ldelem_i -> IlOp.Nullary NullaryIlOp.Ldelem_i
                        | ILOpCode.Ldelem_r4 -> IlOp.Nullary NullaryIlOp.Ldelem_r4
                        | ILOpCode.Ldelem_r8 -> IlOp.Nullary NullaryIlOp.Ldelem_r8
                        | ILOpCode.Ldelem_ref -> IlOp.Nullary NullaryIlOp.Ldelem_ref
                        | ILOpCode.Stelem_i -> IlOp.Nullary NullaryIlOp.Stelem_i
                        | ILOpCode.Stelem_i1 -> IlOp.Nullary NullaryIlOp.Stelem_i1
                        | ILOpCode.Stelem_i2 -> IlOp.Nullary NullaryIlOp.Stelem_i2
                        | ILOpCode.Stelem_i4 -> IlOp.Nullary NullaryIlOp.Stelem_i4
                        | ILOpCode.Stelem_i8 -> IlOp.Nullary NullaryIlOp.Stelem_i8
                        | ILOpCode.Stelem_r4 -> IlOp.Nullary NullaryIlOp.Stelem_r4
                        | ILOpCode.Stelem_r8 -> IlOp.Nullary NullaryIlOp.Stelem_r8
                        | ILOpCode.Stelem_ref -> IlOp.Nullary NullaryIlOp.Stelem_ref
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
                        | ILOpCode.Ldtoken ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, readMetadataToken &reader)
                        | ILOpCode.Conv_u2 -> IlOp.Nullary NullaryIlOp.Conv_U2
                        | ILOpCode.Conv_u1 -> IlOp.Nullary NullaryIlOp.Conv_U1
                        | ILOpCode.Conv_i -> IlOp.Nullary NullaryIlOp.Conv_I
                        | ILOpCode.Conv_ovf_i -> IlOp.Nullary NullaryIlOp.Conv_ovf_i
                        | ILOpCode.Conv_ovf_u -> IlOp.Nullary NullaryIlOp.Conv_ovf_u
                        | ILOpCode.Add_ovf -> IlOp.Nullary NullaryIlOp.Add_ovf
                        | ILOpCode.Add_ovf_un -> IlOp.Nullary NullaryIlOp.Add_ovf_un
                        | ILOpCode.Mul_ovf -> IlOp.Nullary NullaryIlOp.Mul_ovf
                        | ILOpCode.Mul_ovf_un -> IlOp.Nullary NullaryIlOp.Mul_ovf_un
                        | ILOpCode.Sub_ovf -> IlOp.Nullary NullaryIlOp.Sub_ovf
                        | ILOpCode.Sub_ovf_un -> IlOp.Nullary NullaryIlOp.Sub_ovf_un
                        | ILOpCode.Endfinally -> IlOp.Nullary NullaryIlOp.Endfinally
                        | ILOpCode.Leave -> IlOp.UnaryConst (UnaryConstIlOp.Leave (reader.ReadInt32 ()))
                        | ILOpCode.Leave_s -> IlOp.UnaryConst (UnaryConstIlOp.Leave_s (reader.ReadSByte ()))
                        | ILOpCode.Stind_i -> failwith "todo"
                        | ILOpCode.Conv_u -> IlOp.Nullary NullaryIlOp.Conv_U
                        | ILOpCode.Arglist -> failwith "todo"
                        | ILOpCode.Ceq -> IlOp.Nullary NullaryIlOp.Ceq
                        | ILOpCode.Cgt -> IlOp.Nullary NullaryIlOp.Cgt
                        | ILOpCode.Cgt_un -> IlOp.Nullary NullaryIlOp.Cgt_un
                        | ILOpCode.Clt -> IlOp.Nullary NullaryIlOp.Clt
                        | ILOpCode.Clt_un -> IlOp.Nullary NullaryIlOp.Clt_un
                        | ILOpCode.Ldftn ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldftn, readMetadataToken &reader)
                        | ILOpCode.Ldvirtftn -> failwith "todo"
                        | ILOpCode.Ldarg -> failwith "todo"
                        | ILOpCode.Ldarga -> failwith "todo"
                        | ILOpCode.Starg -> IlOp.UnaryConst (UnaryConstIlOp.Starg (reader.ReadUInt16 ()))
                        | ILOpCode.Ldloc -> failwith "todo"
                        | ILOpCode.Ldloca -> failwith "todo"
                        | ILOpCode.Stloc -> IlOp.UnaryConst (UnaryConstIlOp.Stloc (reader.ReadUInt16 ()))
                        | ILOpCode.Localloc -> IlOp.Nullary NullaryIlOp.Localloc
                        | ILOpCode.Endfilter -> IlOp.Nullary NullaryIlOp.Endfilter
                        | ILOpCode.Unaligned -> failwith "todo"
                        | ILOpCode.Volatile -> IlOp.Nullary NullaryIlOp.Volatile
                        | ILOpCode.Tail -> IlOp.Nullary NullaryIlOp.Tail
                        | ILOpCode.Initobj ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Initobj, readMetadataToken &reader)
                        | ILOpCode.Constrained ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Constrained, readMetadataToken &reader)
                        | ILOpCode.Cpblk -> failwith "todo"
                        | ILOpCode.Initblk -> failwith "todo"
                        | ILOpCode.Rethrow -> IlOp.Nullary NullaryIlOp.Rethrow
                        | ILOpCode.Sizeof -> failwith "todo"
                        | ILOpCode.Refanytype -> failwith "todo"
                        | ILOpCode.Readonly -> failwith "todo"
                        | i -> failwithf "Unknown opcode: %A" i

                    readInstructions ((opCode, offset) :: acc)

            let instructions = readInstructions []

            {
                Instructions = instructions
                LocalInit = methodBody.LocalVariablesInitialized
                MaxStackSize = methodBody.MaxStack
                ExceptionRegions = methodBody.ExceptionRegions
            }
            |> Some

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

    let private readMethod
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (methodHandle : MethodDefinitionHandle)
        : MethodInfo option
        =
        let assemblyName = metadataReader.GetAssemblyDefinition().GetAssemblyName ()
        let methodDef = metadataReader.GetMethodDefinition methodHandle
        let methodName = metadataReader.GetString methodDef.Name
        let methodSig = methodDef.DecodeSignature (TypeDefn.typeProvider, ())
        let methodBody = readMethodBody peReader methodDef
        let declaringType = methodDef.GetDeclaringType ()

        match methodBody with
        | None ->
            Console.Error.WriteLine $"[INF] No method body for {metadataReader.GetString methodDef.Name}"
            None
        | Some methodBody ->

        let methodParams = readMethodParams metadataReader (methodDef.GetParameters ())

        let methodGenericParams =
            readGenericMethodParam metadataReader (methodDef.GetGenericParameters ())

        {
            DeclaringType = (declaringType, assemblyName)
            Handle = methodHandle
            Name = methodName
            Instructions = methodBody.Instructions
            Locations = methodBody.Instructions |> List.map (fun (a, b) -> b, a) |> Map.ofList
            Parameters = methodParams
            Generics = methodGenericParams
            Signature = TypeMethodSignature.make methodSig
            IsPinvokeImpl = methodDef.Attributes.HasFlag MethodAttributes.PinvokeImpl
            LocalsInit = methodBody.LocalInit
            IsStatic = not methodSig.Header.IsInstance
        }
        |> Some

    let internal read
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (typeHandle : TypeDefinitionHandle)
        : TypeInfo
        =
        let typeDef = metadataReader.GetTypeDefinition typeHandle
        let methods = typeDef.GetMethods ()

        let methodImpls =
            typeDef.GetMethodImplementations ()
            |> Seq.map (fun handle ->
                let m = metadataReader.GetMethodImplementation handle

                if not (m.MethodBody.Kind.HasFlag HandleKind.MethodImplementation) then
                    failwith "unexpected kind"

                KeyValuePair (handle, m.MethodBody)
            )
            |> ImmutableDictionary.CreateRange

        // TODO: render this up front
        let strings x =
            match x with
            | StringToken.String s -> metadataReader.GetString s
            | StringToken.UserString s -> metadataReader.GetUserString s

        let fields =
            metadataReader.FieldDefinitions
            |> Seq.map (fun h -> FieldInfo.make strings h (metadataReader.GetFieldDefinition h))
            |> Seq.toList

        let baseType =
            match MetadataToken.ofEntityHandle typeDef.BaseType with
            | TypeReference typeReferenceHandle -> Some (BaseTypeInfo.TypeRef typeReferenceHandle)
            | TypeDefinition typeDefinitionHandle -> Some (BaseTypeInfo.TypeDef typeDefinitionHandle)
            | t -> failwith $"Unrecognised base-type entity identifier: %O{t}"

        let name = metadataReader.GetString typeDef.Name
        let ns = metadataReader.GetString typeDef.Namespace
        let typeAttrs = typeDef.Attributes
        let attrs = typeDef.GetCustomAttributes () |> Seq.toList

        {
            Namespace = ns
            Name = name
            Methods =
                methods
                |> Seq.choose (fun m ->
                    let result = readMethod peReader metadataReader m

                    match result with
                    | None -> None
                    | Some x -> Some x
                )
                |> Seq.toList
            MethodImpls = methodImpls
            Fields = fields
            BaseType = baseType
            TypeAttributes = typeAttrs
            Attributes = attrs
        }
