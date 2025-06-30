namespace WoofWare.PawPrint

#nowarn "9"

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open Microsoft.Extensions.Logging

/// <summary>
/// Represents information about a method parameter.
/// Corresponds to Parameter in System.Reflection.Metadata.
/// </summary>
type Parameter =
    {
        /// <summary>The name of the parameter.</summary>
        Name : string

        /// <summary>
        /// The default value of the parameter, if one is specified.
        /// This is used for optional parameters.
        /// </summary>
        DefaultValue : Constant

        /// <summary>
        /// The position of the parameter in the parameter list.
        /// For instance methods, index 0 is the 'this' parameter.
        /// </summary>
        SequenceNumber : int
    }

[<RequireQualifiedAccess>]
module Parameter =
    let readAll (metadata : MetadataReader) (param : ParameterHandleCollection) : Parameter ImmutableArray =
        let result = ImmutableArray.CreateBuilder ()

        for param in param do
            let param = metadata.GetParameter param

            // The spec doesn't seem to mention this behaviour, but a sequence number of 0 (and an unnamed parameter)
            // seems to correspond with a ref return.
            if param.SequenceNumber <> 0 then
                {
                    Name = metadata.GetString param.Name
                    DefaultValue = metadata.GetConstant (param.GetDefaultValue ())
                    SequenceNumber = param.SequenceNumber
                }
                |> result.Add

        result.ToImmutable ()

/// <summary>
/// Represents a generic type or method parameter definition.
/// Corresponds to GenericParameter in System.Reflection.Metadata.
/// </summary>
type GenericParameter =
    {
        /// <summary>The name of the generic parameter (e.g., 'T', 'TKey', etc.).</summary>
        Name : string

        /// <summary>
        /// The zero-based index of the generic parameter in the generic parameter list.
        /// For example, in Dictionary&lt;TKey, TValue&rt;, TKey has index 0 and TValue has index 1.
        /// </summary>
        SequenceNumber : int
    }

[<RequireQualifiedAccess>]
module GenericParameter =
    let readAll
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

type ExceptionOffset =
    {
        TryLength : int
        TryOffset : int
        HandlerLength : int
        HandlerOffset : int
    }

type ExceptionRegion =
    | Filter of filterOffset : int * ExceptionOffset
    /// Token is a TypeRef, TypeDef, or TypeSpec
    | Catch of MetadataToken * ExceptionOffset
    | Finally of ExceptionOffset
    | Fault of ExceptionOffset

    static member OfExceptionRegion (r : System.Reflection.Metadata.ExceptionRegion) : ExceptionRegion =
        let offset =
            {
                HandlerLength = r.HandlerLength
                HandlerOffset = r.HandlerOffset
                TryLength = r.TryLength
                TryOffset = r.TryOffset
            }

        match r.Kind with
        | ExceptionRegionKind.Catch -> ExceptionRegion.Catch (MetadataToken.ofEntityHandle r.CatchType, offset)
        | ExceptionRegionKind.Filter -> ExceptionRegion.Filter (r.FilterOffset, offset)
        | ExceptionRegionKind.Finally -> ExceptionRegion.Finally offset
        | ExceptionRegionKind.Fault -> ExceptionRegion.Fault offset
        | _ -> raise (ArgumentOutOfRangeException ())

type MethodInstructions<'methodVars> =
    {
        /// <summary>
        /// The IL instructions that compose the method body, along with their offset positions.
        /// Each tuple contains the instruction and its offset in the method body.
        /// </summary>
        Instructions : (IlOp * int) list

        /// <summary>
        /// A map from instruction offset (program counter) to the corresponding IL operation.
        /// This is the inverse of Instructions for efficient lookup.
        /// </summary>
        Locations : Map<int, IlOp>

        /// <summary>
        /// Whether local variables in this method should be initialized to their default values.
        /// This corresponds to the localsinit flag in the method header.
        /// </summary>
        LocalsInit : bool

        LocalVars : ImmutableArray<'methodVars> option

        ExceptionRegions : ImmutableArray<ExceptionRegion>
    }

[<RequireQualifiedAccess>]
module MethodInstructions =
    let onlyRet () : MethodInstructions<'methodVars> =
        let op = IlOp.Nullary NullaryIlOp.Ret

        {
            Instructions = [ op, 0 ]
            Locations = Map.empty |> Map.add 0 op
            LocalsInit = false
            LocalVars = None
            ExceptionRegions = ImmutableArray.Empty
        }

    let setLocalVars<'a, 'b> (v : ImmutableArray<'b> option) (s : MethodInstructions<'a>) : MethodInstructions<'b> =
        {
            Instructions = s.Instructions
            Locations = s.Locations
            LocalsInit = s.LocalsInit
            LocalVars = v
            ExceptionRegions = s.ExceptionRegions
        }

/// <summary>
/// Represents detailed information about a method in a .NET assembly.
/// This is a strongly-typed representation of MethodDefinition from System.Reflection.Metadata.
/// </summary>
type MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars
    when 'typeGenerics :> IComparable<'typeGenerics> and 'typeGenerics : comparison> =
    {
        /// <summary>
        /// The type that declares this method, along with its assembly information.
        /// </summary>
        DeclaringType : ConcreteType<'typeGenerics>

        /// <summary>
        /// The metadata token handle that uniquely identifies this method in the assembly.
        /// </summary>
        Handle : MethodDefinitionHandle

        /// <summary>The name of the method.</summary>
        Name : string

        /// <summary>
        /// The IL instructions that compose the method body, along with their offset positions.
        ///
        /// There may be no instructions for this method, e.g. if it's an `InternalCall`.
        /// </summary>
        Instructions : MethodInstructions<'methodVars> option

        /// <summary>
        /// The parameters of this method.
        /// </summary>
        Parameters : Parameter ImmutableArray

        /// <summary>
        /// The generic type parameters defined by this method, if any.
        /// </summary>
        Generics : 'methodGenerics ImmutableArray

        /// <summary>
        /// The signature of the method, including return type and parameter types.
        /// </summary>
        Signature : TypeMethodSignature<'methodVars>

        RawSignature : TypeMethodSignature<TypeDefn>

        /// <summary>
        /// Custom attributes defined on the method. I've never yet seen one of these in practice.
        /// </summary>
        CustomAttributes : WoofWare.PawPrint.CustomAttribute ImmutableArray

        MethodAttributes : MethodAttributes

        ImplAttributes : MethodImplAttributes

        /// <summary>
        /// Whether this method is static (true) or an instance method (false).
        /// </summary>
        IsStatic : bool
    }

    override this.ToString () =
        $"{this.DeclaringType.Assembly.Name}.{this.DeclaringType.Name}.{this.Name}"

    /// <summary>
    /// Whether this method's implementation is directly supplied by the CLI, rather than being loaded
    /// from an assembly as IL.
    /// </summary>
    member this.IsCliInternal : bool =
        this.ImplAttributes.HasFlag MethodImplAttributes.InternalCall

    /// <summary>
    /// Whether this method is implemented as a platform invoke (P/Invoke) to unmanaged code.
    /// </summary>
    member this.IsPinvokeImpl : bool =
        this.MethodAttributes.HasFlag MethodAttributes.PinvokeImpl

[<RequireQualifiedAccess>]
module MethodInfo =
    let isJITIntrinsic
        (getMemberRefParentType : MemberReferenceHandle -> TypeRef)
        (methodDefs : IReadOnlyDictionary<MethodDefinitionHandle, MethodInfo<'a, 'b, 'c>>)
        (this : MethodInfo<'d, 'e, 'f>)
        : bool
        =
        this.CustomAttributes
        |> Seq.exists (fun attr ->
            match attr.Constructor with
            | MetadataToken.MethodDef handle ->
                let constructor = methodDefs.[handle]

                constructor.DeclaringType.Name = "IntrinsicAttribute"
                && constructor.DeclaringType.Assembly.FullName.StartsWith (
                    "System.Private.CoreLib, ",
                    StringComparison.Ordinal
                )
            | MetadataToken.MemberReference handle ->
                let ty = getMemberRefParentType handle
                ty.Namespace = "System" && ty.Name = "IntrinsicAttribute"
            | con -> failwith $"TODO: {con}"
        )

    let mapTypeGenerics<'a, 'b, 'methodGen, 'vars
        when 'a :> IComparable<'a> and 'a : comparison and 'b : comparison and 'b :> IComparable<'b>>
        (f : int -> 'a -> 'b)
        (m : MethodInfo<'a, 'methodGen, 'vars>)
        : MethodInfo<'b, 'methodGen, 'vars>
        =
        {
            DeclaringType = m.DeclaringType |> ConcreteType.mapGeneric f
            Handle = m.Handle
            Name = m.Name
            Instructions = m.Instructions
            Parameters = m.Parameters
            Generics = m.Generics
            Signature = m.Signature
            RawSignature = m.RawSignature
            CustomAttributes = m.CustomAttributes
            MethodAttributes = m.MethodAttributes
            ImplAttributes = m.ImplAttributes
            IsStatic = m.IsStatic
        }

    let mapMethodGenerics<'a, 'b, 'vars, 'typeGen when 'typeGen :> IComparable<'typeGen> and 'typeGen : comparison>
        (f : int -> 'a -> 'b)
        (m : MethodInfo<'typeGen, 'a, 'vars>)
        : MethodInfo<'typeGen, 'b, 'vars>
        =
        {
            DeclaringType = m.DeclaringType
            Handle = m.Handle
            Name = m.Name
            Instructions = m.Instructions
            Parameters = m.Parameters
            Generics = m.Generics |> Seq.mapi f |> ImmutableArray.CreateRange
            Signature = m.Signature
            RawSignature = m.RawSignature
            CustomAttributes = m.CustomAttributes
            MethodAttributes = m.MethodAttributes
            ImplAttributes = m.ImplAttributes
            IsStatic = m.IsStatic
        }

    let setMethodVars
        (vars2 : MethodInstructions<'vars2> option)
        (signature : TypeMethodSignature<'vars2>)
        (m : MethodInfo<'typeGen, 'methodGen, 'vars1>)
        : MethodInfo<'typeGen, 'methodGen, 'vars2>
        =
        {
            DeclaringType = m.DeclaringType
            Handle = m.Handle
            Name = m.Name
            Instructions = vars2
            Parameters = m.Parameters
            Generics = m.Generics
            Signature = signature
            RawSignature = m.RawSignature
            CustomAttributes = m.CustomAttributes
            MethodAttributes = m.MethodAttributes
            ImplAttributes = m.ImplAttributes
            IsStatic = m.IsStatic
        }

    type private Dummy = class end

    type private MethodBody =
        {
            Instructions : (IlOp * int) list
            LocalInit : bool
            LocalSig : ImmutableArray<TypeDefn> option
            MaxStackSize : int
            ExceptionRegions : ImmutableArray<ExceptionRegion>
        }

    let private readMetadataToken (reader : byref<BlobReader>) : MetadataToken =
        reader.ReadUInt32 () |> int |> MetadataToken.ofInt

    let private readStringToken (reader : byref<BlobReader>) : StringToken =
        let value = reader.ReadUInt32 () |> int
        StringToken.ofInt value

    // TODO: each opcode probably ought to store how many bytes it takes, so we can advance the program counter?
    let private readOpCode (reader : byref<BlobReader>) : ILOpCode =
        let op = reader.ReadByte ()

        if op = 0xFEuy then
            let op2 = reader.ReadByte ()
            LanguagePrimitives.EnumOfValue (0xFE00us ||| (uint16 op2))
        else
            LanguagePrimitives.EnumOfValue (uint16 op)

    let private readMethodBody
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (assembly : AssemblyName)
        (methodDef : MethodDefinition)
        : MethodBody option
        =
        if methodDef.RelativeVirtualAddress = 0 then
            None
        else
            let methodBody = peReader.GetMethodBody methodDef.RelativeVirtualAddress

            let localSig =
                if methodBody.LocalSignature.IsNil then
                    None
                else

                let s = methodBody.LocalSignature |> metadataReader.GetStandaloneSignature
                s.DecodeLocalSignature (TypeDefn.typeProvider assembly, ()) |> Some

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
                        | ILOpCode.Ldc_r4 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 (reader.ReadSingle ()))
                        | ILOpCode.Ldc_r8 -> IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 (reader.ReadDouble ()))
                        | ILOpCode.Dup -> IlOp.Nullary NullaryIlOp.Dup
                        | ILOpCode.Pop -> IlOp.Nullary NullaryIlOp.Pop
                        | ILOpCode.Jmp ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Jmp, readMetadataToken &reader)
                        | ILOpCode.Call ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, readMetadataToken &reader)
                        | ILOpCode.Calli ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Calli, readMetadataToken &reader)
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
                        | ILOpCode.Conv_r_un -> IlOp.Nullary NullaryIlOp.Conv_r_un
                        | ILOpCode.Unbox ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox, readMetadataToken &reader)
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
                        | ILOpCode.Conv_ovf_i1 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i1
                        | ILOpCode.Conv_ovf_u1 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u1
                        | ILOpCode.Conv_ovf_i2 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i2
                        | ILOpCode.Conv_ovf_u2 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u2
                        | ILOpCode.Conv_ovf_i4 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i4
                        | ILOpCode.Conv_ovf_u4 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u4
                        | ILOpCode.Conv_ovf_i8 -> IlOp.Nullary NullaryIlOp.Conv_ovf_i8
                        | ILOpCode.Conv_ovf_u8 -> IlOp.Nullary NullaryIlOp.Conv_ovf_u8
                        | ILOpCode.Refanyval ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Refanyval, readMetadataToken &reader)
                        | ILOpCode.Ckfinite -> IlOp.Nullary NullaryIlOp.Ckfinite
                        | ILOpCode.Mkrefany ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Mkrefany, readMetadataToken &reader)
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
                        | ILOpCode.Stind_i -> IlOp.Nullary NullaryIlOp.Stind_I
                        | ILOpCode.Conv_u -> IlOp.Nullary NullaryIlOp.Conv_U
                        | ILOpCode.Arglist -> IlOp.Nullary NullaryIlOp.Arglist
                        | ILOpCode.Ceq -> IlOp.Nullary NullaryIlOp.Ceq
                        | ILOpCode.Cgt -> IlOp.Nullary NullaryIlOp.Cgt
                        | ILOpCode.Cgt_un -> IlOp.Nullary NullaryIlOp.Cgt_un
                        | ILOpCode.Clt -> IlOp.Nullary NullaryIlOp.Clt
                        | ILOpCode.Clt_un -> IlOp.Nullary NullaryIlOp.Clt_un
                        | ILOpCode.Ldftn ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldftn, readMetadataToken &reader)
                        | ILOpCode.Ldvirtftn ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldvirtftn, readMetadataToken &reader)
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
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Initobj, readMetadataToken &reader)
                        | ILOpCode.Constrained ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Constrained, readMetadataToken &reader)
                        | ILOpCode.Cpblk -> IlOp.Nullary NullaryIlOp.Cpblk
                        | ILOpCode.Initblk -> IlOp.Nullary NullaryIlOp.Initblk
                        | ILOpCode.Rethrow -> IlOp.Nullary NullaryIlOp.Rethrow
                        | ILOpCode.Sizeof ->
                            IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Sizeof, readMetadataToken &reader)
                        | ILOpCode.Refanytype -> IlOp.Nullary NullaryIlOp.Refanytype
                        | ILOpCode.Readonly -> IlOp.Nullary NullaryIlOp.Readonly
                        | i -> failwithf "Unknown opcode: %A" i

                    readInstructions ((opCode, offset) :: acc)

            let instructions = readInstructions []

            let er =
                methodBody.ExceptionRegions
                |> Seq.map ExceptionRegion.OfExceptionRegion
                |> ImmutableArray.CreateRange

            {
                Instructions = instructions
                LocalInit = methodBody.LocalVariablesInitialized
                LocalSig = localSig
                MaxStackSize = methodBody.MaxStack
                ExceptionRegions = er
            }
            |> Some

    let read
        (loggerFactory : ILoggerFactory)
        (peReader : PEReader)
        (metadataReader : MetadataReader)
        (methodHandle : MethodDefinitionHandle)
        : MethodInfo<FakeUnit, GenericParameter, TypeDefn> option
        =
        let logger = loggerFactory.CreateLogger "MethodInfo"
        let assemblyName = metadataReader.GetAssemblyDefinition().GetAssemblyName ()
        let methodDef = metadataReader.GetMethodDefinition methodHandle
        let methodName = metadataReader.GetString methodDef.Name
        let methodSig = methodDef.DecodeSignature (TypeDefn.typeProvider assemblyName, ())
        let implAttrs = methodDef.ImplAttributes

        let methodBody =
            if
                implAttrs.HasFlag MethodImplAttributes.InternalCall
                || implAttrs.HasFlag MethodImplAttributes.Runtime
            then
                None
            elif methodDef.Attributes.HasFlag MethodAttributes.PinvokeImpl then
                None
            else
                match readMethodBody peReader metadataReader assemblyName methodDef with
                | None ->
                    logger.LogTrace $"no method body in {assemblyName.Name} {methodName}"
                    None
                | Some body ->
                    {
                        MethodInstructions.Instructions = body.Instructions
                        Locations = body.Instructions |> List.map (fun (a, b) -> b, a) |> Map.ofList
                        LocalsInit = body.LocalInit
                        LocalVars = body.LocalSig
                        ExceptionRegions = body.ExceptionRegions
                    }
                    |> Some

        let declaringType = methodDef.GetDeclaringType ()

        let declaringDefn = metadataReader.GetTypeDefinition (declaringType)

        let declaringTypeNamespace = metadataReader.GetString declaringDefn.Namespace

        let declaringTypeName = metadataReader.GetString declaringDefn.Name

        let declaringTypeGenericParams =
            metadataReader.GetTypeDefinition(declaringType).GetGenericParameters().Count

        let attrs =
            let result = ImmutableArray.CreateBuilder ()
            let attrs = methodDef.GetCustomAttributes ()

            for attr in attrs do
                metadataReader.GetCustomAttribute attr
                |> CustomAttribute.make attr
                |> result.Add

            result.ToImmutable ()

        let typeSig = TypeMethodSignature.make methodSig

        let methodParams = Parameter.readAll metadataReader (methodDef.GetParameters ())

        let methodGenericParams =
            GenericParameter.readAll metadataReader (methodDef.GetGenericParameters ())

        let declaringType =
            ConcreteType.make'
                assemblyName
                declaringType
                declaringTypeNamespace
                declaringTypeName
                declaringTypeGenericParams

        {
            DeclaringType = declaringType
            Handle = methodHandle
            Name = methodName
            Instructions = methodBody
            Parameters = methodParams
            Generics = methodGenericParams
            Signature = typeSig
            RawSignature = typeSig
            MethodAttributes = methodDef.Attributes
            CustomAttributes = attrs
            IsStatic = not methodSig.Header.IsInstance
            ImplAttributes = implAttrs
        }
        |> Some

    let rec resolveBaseType
        (methodGenerics : TypeDefn ImmutableArray option)
        (executingMethod : MethodInfo<TypeDefn, 'methodGen, 'vars>)
        (td : TypeDefn)
        : ResolvedBaseType
        =
        match td with
        | TypeDefn.Void -> failwith "Void isn't a type that appears at runtime and has no base type"
        | TypeDefn.PrimitiveType ty ->
            match ty with
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.Char
            | PrimitiveType.Boolean -> ResolvedBaseType.ValueType
            | PrimitiveType.String -> ResolvedBaseType.Object
            | PrimitiveType.TypedReference -> failwith "todo"
            | PrimitiveType.IntPtr -> failwith "todo"
            | PrimitiveType.UIntPtr -> failwith "todo"
            | PrimitiveType.Object -> failwith "todo"
        | TypeDefn.Array (elt, shape) -> failwith "todo"
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer typeDefn -> failwith "todo"
        | TypeDefn.Byref typeDefn -> failwith "todo"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
        | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
        | TypeDefn.FromReference (typeRef, signatureTypeKind) -> failwith "todo"
        | TypeDefn.FromDefinition (comparableTypeDefinitionHandle, _, signatureTypeKind) -> failwith "todo"
        | TypeDefn.GenericInstantiation (generic, args) -> failwith "todo"
        | TypeDefn.FunctionPointer typeMethodSignature -> failwith "todo"
        | TypeDefn.GenericTypeParameter index ->
            resolveBaseType methodGenerics executingMethod executingMethod.DeclaringType.Generics.[index]
        | TypeDefn.GenericMethodParameter index ->
            match methodGenerics with
            | None -> failwith "unexpectedly asked for a generic method parameter when we had none"
            | Some generics -> resolveBaseType methodGenerics executingMethod generics.[index]
