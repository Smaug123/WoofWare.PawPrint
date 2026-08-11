namespace WoofWare.PawPrint

// `fixed`, to pin the IL bytes for the BlobReader below.
#nowarn "9"

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

/// <summary>
/// Decoding of a method body's IL byte stream into <see cref="IlOp"/>s.
/// </summary>
/// <remarks>
/// This is deliberately independent of where the bytes came from. A body decoded from a PE
/// image supplies them from <c>MethodBodyBlock.GetILBytes</c>, but ECMA-335 IL is
/// self-contained apart from its tokens, and the only thing decoding needs to know about
/// the source is which assembly's token universe the operands are drawn from.
/// </remarks>
[<RequireQualifiedAccess>]
module IlDecoding =
    let private readMetadataToken (assembly : AssemblyName) (reader : byref<BlobReader>) : SourcedMetadataToken =
        reader.ReadUInt32 () |> int |> SourcedMetadataToken.ofInt assembly

    let private readStringToken (assembly : AssemblyName) (reader : byref<BlobReader>) : SourcedStringToken =
        let value = reader.ReadUInt32 () |> int
        SourcedStringToken.ofInt assembly value

    // TODO: each opcode probably ought to store how many bytes it takes, so we can advance the program counter?
    let private readOpCode (reader : byref<BlobReader>) : ILOpCode =
        let op = reader.ReadByte ()

        if op = 0xFEuy then
            let op2 = reader.ReadByte ()
            LanguagePrimitives.EnumOfValue (0xFE00us ||| (uint16 op2))
        else
            LanguagePrimitives.EnumOfValue (uint16 op)

    /// <summary>
    /// Decode a complete IL byte stream, returning each instruction paired with its offset
    /// from the start of the stream. Offsets are exactly the branch targets and exception
    /// region boundaries the same body's metadata uses, so they double as program counters.
    /// </summary>
    /// <param name="assembly">
    /// The assembly whose token universe the body's metadata and string operands index into.
    /// </param>
    /// <param name="ilBytes">The body's IL, with no header and no exception-handling data.</param>
    let decodeInstructions (assembly : AssemblyName) (ilBytes : byte[]) : (IlOp * int) list =
        // A zero-length body decodes to no instructions; `fixed` yields a null pointer for an
        // empty array, which `BlobReader` accepts precisely when the length is zero.
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
                    | ILOpCode.Break -> IlOp.Nullary NullaryIlOp.Break
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
                    | ILOpCode.Stloc_s -> IlOp.UnaryConst (UnaryConstIlOp.Stloc_s (reader.ReadByte ()))
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
                    | ILOpCode.Ldc_r4 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 (reader.ReadSingle ()))
                    | ILOpCode.Ldc_r8 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 (reader.ReadDouble ()))
                    | ILOpCode.Dup -> IlOp.Nullary NullaryIlOp.Dup
                    | ILOpCode.Pop -> IlOp.Nullary NullaryIlOp.Pop
                    | ILOpCode.Jmp ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Jmp, readMetadataToken assembly &reader)
                    | ILOpCode.Call ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, readMetadataToken assembly &reader)
                    | ILOpCode.Calli ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, readMetadataToken assembly &reader)
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

                        if count > uint32 System.Int32.MaxValue then
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
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, readMetadataToken assembly &reader)
                    | ILOpCode.Cpobj ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Cpobj, readMetadataToken assembly &reader)
                    | ILOpCode.Ldobj ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldobj, readMetadataToken assembly &reader)
                    | ILOpCode.Ldstr ->
                        IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, readStringToken assembly &reader)
                    | ILOpCode.Newobj ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, readMetadataToken assembly &reader)
                    | ILOpCode.Castclass ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Castclass, readMetadataToken assembly &reader)
                    | ILOpCode.Isinst ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Isinst, readMetadataToken assembly &reader)
                    | ILOpCode.Conv_r_un -> IlOp.Nullary NullaryIlOp.Conv_r_un
                    | ILOpCode.Unbox ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox, readMetadataToken assembly &reader)
                    | ILOpCode.Throw -> IlOp.Nullary NullaryIlOp.Throw
                    | ILOpCode.Ldfld ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, readMetadataToken assembly &reader)
                    | ILOpCode.Ldflda ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldflda, readMetadataToken assembly &reader)
                    | ILOpCode.Stfld ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stfld, readMetadataToken assembly &reader)
                    | ILOpCode.Ldsfld ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, readMetadataToken assembly &reader)
                    | ILOpCode.Ldsflda ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsflda, readMetadataToken assembly &reader)
                    | ILOpCode.Stsfld ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stsfld, readMetadataToken assembly &reader)
                    | ILOpCode.Stobj ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stobj, readMetadataToken assembly &reader)
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
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, readMetadataToken assembly &reader)
                    | ILOpCode.Newarr ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, readMetadataToken assembly &reader)
                    | ILOpCode.Ldlen -> IlOp.Nullary NullaryIlOp.LdLen
                    | ILOpCode.Ldelema ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelema, readMetadataToken assembly &reader)
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
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldelem, readMetadataToken assembly &reader)
                    | ILOpCode.Stelem ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Stelem, readMetadataToken assembly &reader)
                    | ILOpCode.Unbox_any ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox_Any, readMetadataToken assembly &reader)
                    | ILOpCode.Conv_ovf_i1 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i1
                    | ILOpCode.Conv_ovf_u1 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u1
                    | ILOpCode.Conv_ovf_i2 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i2
                    | ILOpCode.Conv_ovf_u2 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u2
                    | ILOpCode.Conv_ovf_i4 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i4
                    | ILOpCode.Conv_ovf_u4 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u4
                    | ILOpCode.Conv_ovf_i8 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i8
                    | ILOpCode.Conv_ovf_u8 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u8
                    | ILOpCode.Refanyval ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Refanyval, readMetadataToken assembly &reader)
                    | ILOpCode.Ckfinite -> IlOp.Nullary NullaryIlOp.Ckfinite
                    | ILOpCode.Mkrefany ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Mkrefany, readMetadataToken assembly &reader)
                    | ILOpCode.Ldtoken ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldtoken, readMetadataToken assembly &reader)
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
                    | ILOpCode.Stind_i -> IlOp.Nullary NullaryIlOp.Stind_I
                    | ILOpCode.Conv_u -> IlOp.Nullary NullaryIlOp.Conv_U
                    | ILOpCode.Arglist -> IlOp.Nullary NullaryIlOp.Arglist
                    | ILOpCode.Ceq -> IlOp.Nullary NullaryIlOp.Ceq
                    | ILOpCode.Cgt -> IlOp.Nullary NullaryIlOp.Cgt
                    | ILOpCode.Cgt_un -> IlOp.Nullary NullaryIlOp.Cgt_un
                    | ILOpCode.Clt -> IlOp.Nullary NullaryIlOp.Clt
                    | ILOpCode.Clt_un -> IlOp.Nullary NullaryIlOp.Clt_un
                    | ILOpCode.Ldftn ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldftn, readMetadataToken assembly &reader)
                    | ILOpCode.Ldvirtftn ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldvirtftn, readMetadataToken assembly &reader)
                    | ILOpCode.Ldarg -> IlOp.UnaryConst (UnaryConstIlOp.Ldarg (reader.ReadUInt16 ()))
                    | ILOpCode.Ldarga -> IlOp.UnaryConst (UnaryConstIlOp.Ldarga (reader.ReadUInt16 ()))
                    | ILOpCode.Starg -> IlOp.UnaryConst (UnaryConstIlOp.Starg (reader.ReadUInt16 ()))
                    | ILOpCode.Ldloc -> IlOp.UnaryConst (UnaryConstIlOp.Ldloc (reader.ReadUInt16 ()))
                    | ILOpCode.Ldloca -> IlOp.UnaryConst (UnaryConstIlOp.Ldloca (reader.ReadUInt16 ()))
                    | ILOpCode.Stloc -> IlOp.UnaryConst (UnaryConstIlOp.Stloc (reader.ReadUInt16 ()))
                    | ILOpCode.Localloc -> IlOp.Nullary NullaryIlOp.Localloc
                    | ILOpCode.Endfilter -> IlOp.Nullary NullaryIlOp.Endfilter
                    | ILOpCode.Unaligned -> IlOp.UnaryConst (UnaryConstIlOp.Unaligned (reader.ReadByte ()))
                    | ILOpCode.Volatile -> IlOp.Nullary NullaryIlOp.Volatile
                    | ILOpCode.Tail -> IlOp.Nullary NullaryIlOp.Tail
                    | ILOpCode.Initobj ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Initobj, readMetadataToken assembly &reader)
                    | ILOpCode.Constrained ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Constrained, readMetadataToken assembly &reader)
                    | ILOpCode.Cpblk -> IlOp.Nullary NullaryIlOp.Cpblk
                    | ILOpCode.Initblk -> IlOp.Nullary NullaryIlOp.Initblk
                    | ILOpCode.Rethrow -> IlOp.Nullary NullaryIlOp.Rethrow
                    | ILOpCode.Sizeof ->
                        IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Sizeof, readMetadataToken assembly &reader)
                    | ILOpCode.Refanytype -> IlOp.Nullary NullaryIlOp.Refanytype
                    | ILOpCode.Readonly -> IlOp.Nullary NullaryIlOp.Readonly
                    | i -> failwithf "Unknown opcode: %A" i

                readInstructions ((opCode, offset) :: acc)

        readInstructions []
