namespace WoofWare.PawPrint

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// The IL CoreCLR's VM gives an `[Intrinsic]` method in place of the body CoreLib ships
/// (`getILIntrinsicImplementationFor*`, jitinterface.cpp), for the methods it names through
/// corelib.h's binder.
///
/// Only `System.Runtime.CompilerServices.Unsafe` is transcribed. Its stubs are the same IL for
/// every instantiation; the VM's other substitutions (`Interlocked.CompareExchange<T>`,
/// `RuntimeHelpers.IsBitwiseEquatable` and the rest) choose their IL from the type argument.
[<RequireQualifiedAccess>]
module VmSubstitution =

    /// A parameter type as corelib.h's signatures spell it (`GM_RefT_Int_RetRefT` and so on).
    [<RequireQualifiedAccess>]
    type private Shape =
        | RefT
        | T
        | Int32
        | NInt
        | NUInt
        | UInt32
        | Byte
        | RefByte
        | VoidPointer
        | Object

    let rec private shapeOf (ty : TypeDefn) : Shape option =
        match ty with
        | TypeDefn.Modified m -> shapeOf m.Unmodified
        | TypeDefn.GenericMethodParameter 0 -> Some Shape.T
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> Some Shape.Int32
        | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> Some Shape.NInt
        | TypeDefn.PrimitiveType PrimitiveType.UIntPtr -> Some Shape.NUInt
        | TypeDefn.PrimitiveType PrimitiveType.UInt32 -> Some Shape.UInt32
        | TypeDefn.PrimitiveType PrimitiveType.Byte -> Some Shape.Byte
        | TypeDefn.PrimitiveType PrimitiveType.Object -> Some Shape.Object
        | TypeDefn.Pointer TypeDefn.Void -> Some Shape.VoidPointer
        | TypeDefn.Byref inner ->
            match shapeOf inner with
            | Some Shape.T -> Some Shape.RefT
            | Some Shape.Byte -> Some Shape.RefByte
            | _ -> None
        | _ -> None

    /// The operand the VM's stubs use for their generic argument: the first TypeSpec in CoreLib
    /// whose signature is exactly `!!0` (`FindGenericMethodArgTypeSpec`, jitinterface.cpp).
    let private genericArgument (corelib : DumpedAssembly) : MetadataOperand =
        let candidates =
            corelib.TypeSpecs
            |> Seq.filter (fun (KeyValue (_, spec)) ->
                match spec.Signature with
                | TypeDefn.GenericMethodParameter 0 -> true
                | _ -> false
            )
            |> Seq.map (fun (KeyValue (handle, _)) -> handle)
            |> Seq.sortBy (fun (handle : TypeSpecificationHandle) ->
                MetadataTokens.GetRowNumber (TypeSpecificationHandle.op_Implicit handle : EntityHandle)
            )
            |> List.ofSeq

        match candidates with
        | first :: _ ->
            MetadataOperand.FromMetadata (
                SourcedMetadataToken.make corelib.Name (MetadataToken.TypeSpecification first)
            )
        | [] ->
            failwith
                $"VmSubstitution: %s{corelib.DefinitionFullName} has no TypeSpec for `!!0`, which CoreCLR's VM requires of CoreLib"

    let private nullary (op : NullaryIlOp) : IlOp = IlOp.Nullary op

    let private withToken (op : UnaryMetadataTokenIlOp) (operand : MetadataOperand) : IlOp =
        IlOp.UnaryMetadataToken (op, operand)

    let private unaligned : IlOp = IlOp.UnaryConst (UnaryConstIlOp.Unaligned 1uy)

    /// One of corelib.h's `DEFINE_METHOD(UNSAFE, ...)` rows: the method's name, its parameters
    /// (`None` for `NoSig`, which binds the method of that name), and the stub jitinterface.cpp
    /// gives it, from the `!!0` operand.
    type private Binding =
        {
            Name : string
            Parameters : Shape list option
            Stub : MetadataOperand -> IlOp list
        }

    let private binding (name : string) (parameters : Shape list option) (stub : MetadataOperand -> IlOp list) =
        {
            Name = name
            Parameters = parameters
            Stub = stub
        }

    let private ldarg0 = nullary NullaryIlOp.LdArg0
    let private ldarg1 = nullary NullaryIlOp.LdArg1
    let private ldarg2 = nullary NullaryIlOp.LdArg2
    let private ret = nullary NullaryIlOp.Ret

    let private returnFirstArgument (_ : MetadataOperand) = [ ldarg0 ; ret ]

    let private scaledBy (scale : IlOp list) (combine : NullaryIlOp) (t : MetadataOperand) =
        [ ldarg0 ; ldarg1 ; withToken UnaryMetadataTokenIlOp.Sizeof t ]
        @ scale
        @ [ nullary NullaryIlOp.Mul ; nullary combine ; ret ]

    let private byteOffset (combine : NullaryIlOp) (_ : MetadataOperand) =
        [ ldarg0 ; ldarg1 ; nullary combine ; ret ]

    let private compare (comparison : NullaryIlOp) (_ : MetadataOperand) =
        [ ldarg0 ; ldarg1 ; nullary comparison ; ret ]

    let private block (op : NullaryIlOp) (isUnaligned : bool) (_ : MetadataOperand) =
        [ ldarg0 ; ldarg1 ; ldarg2 ]
        @ (if isUnaligned then [ unaligned ] else [])
        @ [ nullary op ; ret ]

    let private copy (t : MetadataOperand) =
        [
            ldarg0
            ldarg1
            withToken UnaryMetadataTokenIlOp.Ldobj t
            withToken UnaryMetadataTokenIlOp.Stobj t
            ret
        ]

    let private readUnaligned (t : MetadataOperand) =
        [ ldarg0 ; unaligned ; withToken UnaryMetadataTokenIlOp.Ldobj t ; ret ]

    let private writeUnaligned (t : MetadataOperand) =
        [ ldarg0 ; ldarg1 ; unaligned ; withToken UnaryMetadataTokenIlOp.Stobj t ; ret ]

    /// corelib.h's rows in its own order, each with the stub its `MemberDef` selects in
    /// `getILIntrinsicImplementationForUnsafe`.
    ///
    /// corelib.h binds `BYREF_INIT_BLOCK` to `InitBlockUnaligned(ref byte, ...)` and
    /// `BYREF_INIT_BLOCK_UNALIGNED` to `InitBlock(ref byte, ...)`, so the byref `InitBlock` is the
    /// overload whose stub carries `unaligned.` and the byref `InitBlockUnaligned` is not. That is
    /// what CoreCLR runs, and it is transcribed as such.
    let private unsafeBindings : Binding list =
        let conv_i = [ nullary NullaryIlOp.Conv_I ]

        [
            binding "AsPointer" None (fun _ -> [ ldarg0 ; nullary NullaryIlOp.Conv_U ; ret ])
            binding
                "IsNullRef"
                None
                (fun _ ->
                    [
                        ldarg0
                        nullary NullaryIlOp.LdcI4_0
                        nullary NullaryIlOp.Conv_U
                        nullary NullaryIlOp.Ceq
                        ret
                    ]
                )
            binding "NullRef" None (fun _ -> [ nullary NullaryIlOp.LdcI4_0 ; nullary NullaryIlOp.Conv_U ; ret ])
            binding "AsRef" (Some [ Shape.RefT ]) returnFirstArgument
            binding "As" (Some [ Shape.RefT ]) returnFirstArgument
            binding "As" (Some [ Shape.Object ]) returnFirstArgument
            binding "Add" (Some [ Shape.RefT ; Shape.Int32 ]) (scaledBy conv_i NullaryIlOp.Add)
            binding "Add" (Some [ Shape.RefT ; Shape.NInt ]) (scaledBy [] NullaryIlOp.Add)
            binding "Add" (Some [ Shape.RefT ; Shape.NUInt ]) (scaledBy [] NullaryIlOp.Add)
            binding "Add" (Some [ Shape.VoidPointer ; Shape.Int32 ]) (scaledBy conv_i NullaryIlOp.Add)
            binding "ByteOffset" None (fun _ -> [ ldarg1 ; ldarg0 ; nullary NullaryIlOp.Sub ; ret ])
            binding "AddByteOffset" (Some [ Shape.RefT ; Shape.NInt ]) (byteOffset NullaryIlOp.Add)
            binding "AddByteOffset" (Some [ Shape.RefT ; Shape.NUInt ]) (byteOffset NullaryIlOp.Add)
            binding "AreSame" None (compare NullaryIlOp.Ceq)
            binding "Copy" (Some [ Shape.VoidPointer ; Shape.RefT ]) copy
            binding "Copy" (Some [ Shape.RefT ; Shape.VoidPointer ]) copy
            binding
                "CopyBlock"
                (Some [ Shape.VoidPointer ; Shape.VoidPointer ; Shape.UInt32 ])
                (block NullaryIlOp.Cpblk false)
            binding "CopyBlock" (Some [ Shape.RefByte ; Shape.RefByte ; Shape.UInt32 ]) (block NullaryIlOp.Cpblk false)
            binding
                "CopyBlockUnaligned"
                (Some [ Shape.VoidPointer ; Shape.VoidPointer ; Shape.UInt32 ])
                (block NullaryIlOp.Cpblk true)
            binding
                "CopyBlockUnaligned"
                (Some [ Shape.RefByte ; Shape.RefByte ; Shape.UInt32 ])
                (block NullaryIlOp.Cpblk true)
            binding "IsAddressGreaterThan" None (compare NullaryIlOp.Cgt_un)
            binding "IsAddressLessThan" None (compare NullaryIlOp.Clt_un)
            binding
                "InitBlockUnaligned"
                (Some [ Shape.RefByte ; Shape.Byte ; Shape.UInt32 ])
                (block NullaryIlOp.Initblk false)
            binding
                "InitBlock"
                (Some [ Shape.VoidPointer ; Shape.Byte ; Shape.UInt32 ])
                (block NullaryIlOp.Initblk false)
            binding "InitBlock" (Some [ Shape.RefByte ; Shape.Byte ; Shape.UInt32 ]) (block NullaryIlOp.Initblk true)
            binding
                "InitBlockUnaligned"
                (Some [ Shape.VoidPointer ; Shape.Byte ; Shape.UInt32 ])
                (block NullaryIlOp.Initblk true)
            binding "ReadUnaligned" (Some [ Shape.RefByte ]) readUnaligned
            binding "WriteUnaligned" (Some [ Shape.RefByte ; Shape.T ]) writeUnaligned
            binding "ReadUnaligned" (Some [ Shape.VoidPointer ]) readUnaligned
            binding "WriteUnaligned" (Some [ Shape.VoidPointer ; Shape.T ]) writeUnaligned
            binding "Read" None (fun t -> [ ldarg0 ; withToken UnaryMetadataTokenIlOp.Ldobj t ; ret ])
            binding "SkipInit" (Some [ Shape.RefT ]) (fun _ -> [ ret ])
            binding "Subtract" (Some [ Shape.RefT ; Shape.Int32 ]) (scaledBy conv_i NullaryIlOp.Sub)
            binding "Subtract" (Some [ Shape.RefT ; Shape.NInt ]) (scaledBy [] NullaryIlOp.Sub)
            binding "Subtract" (Some [ Shape.RefT ; Shape.NUInt ]) (scaledBy [] NullaryIlOp.Sub)
            binding "Subtract" (Some [ Shape.VoidPointer ; Shape.Int32 ]) (scaledBy conv_i NullaryIlOp.Sub)
            binding "SubtractByteOffset" (Some [ Shape.RefT ; Shape.NInt ]) (byteOffset NullaryIlOp.Sub)
            binding "SubtractByteOffset" (Some [ Shape.RefT ; Shape.NUInt ]) (byteOffset NullaryIlOp.Sub)
            binding "Write" None (fun t -> [ ldarg0 ; ldarg1 ; withToken UnaryMetadataTokenIlOp.Stobj t ; ret ])
            binding "Unbox" None (fun t -> [ ldarg0 ; withToken UnaryMetadataTokenIlOp.Unbox t ; ret ])
        ]

    let private bindsTo (definition : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>) =
        fun (b : Binding) ->
            b.Name = definition.Name
            && (
                match b.Parameters with
                | None -> true
                | Some shapes -> definition.Signature.ParameterTypes |> List.map shapeOf = List.map Some shapes
            )

    /// The IL CoreCLR's VM runs for `method` in place of its own, when `method` is a
    /// `System.Runtime.CompilerServices.Unsafe` method in `corelib` that corelib.h binds.
    ///
    /// CoreCLR substitutes whether or not the IL CoreLib ships is a placeholder: for a method whose
    /// shipped IL is a working implementation, the stub is what actually runs.
    let unsafeStub (corelib : DumpedAssembly) (method : MethodDefinitionHandle) : MethodInstructions<TypeDefn> option =
        let definition = corelib.Methods.[method]

        let declaringType =
            corelib.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        if
            corelib.ThisAssemblyDefinition.Name.Name <> "System.Private.CoreLib"
            || declaringType.Namespace <> "System.Runtime.CompilerServices"
            || declaringType.Name <> "Unsafe"
            || declaringType.IsNested
        then
            None
        else

        match unsafeBindings |> List.filter (bindsTo definition) with
        | [] -> None
        | [ b ] -> Some (IlStub.ofInstructions (b.Stub (genericArgument corelib)))
        | several ->
            failwith
                $"VmSubstitution: %d{several.Length} of corelib.h's Unsafe rows bind %s{definition.Name}, where the binder takes one"
