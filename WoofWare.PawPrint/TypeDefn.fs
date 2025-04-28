namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

/// <summary>
/// Represents a method signature with type parameters.
/// Corresponds to MethodSignature in System.Reflection.Metadata.
/// </summary>
type TypeMethodSignature<'Types> =
    {
        /// <summary>
        /// Contains calling convention and other method attributes encoded in the metadata.
        /// </summary>
        Header : SignatureHeader

        /// <summary>
        /// The types of all parameters of the method.
        /// </summary>
        ParameterTypes : 'Types list

        /// <summary>
        /// The number of generic type parameters defined by this method.
        /// </summary>
        GenericParameterCount : int

        /// <summary>
        /// The number of required parameters (non-optional parameters).
        /// </summary>
        RequiredParameterCount : int

        /// <summary>
        /// The return type of the method.
        /// </summary>
        ReturnType : 'Types
    }

[<RequireQualifiedAccess>]
module TypeMethodSignature =
    let make<'T> (p : MethodSignature<'T>) : TypeMethodSignature<'T> =
        {
            Header = p.Header
            ReturnType = p.ReturnType
            ParameterTypes = List.ofSeq p.ParameterTypes
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
    | Array of elt : TypeDefn * shape : ArrayShape
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

[<RequireQualifiedAccess>]
module TypeDefn =
    let isManaged (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | PrimitiveType primitiveType -> failwith "todo"
        | Array (elt, shape) -> failwith "todo"
        | Pinned typeDefn -> failwith "todo"
        | Pointer typeDefn -> failwith "todo"
        | Byref typeDefn -> failwith "todo"
        | OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
        | Modified (original, afterMod, modificationRequired) -> failwith "todo"
        | FromReference signatureTypeKind -> true
        | FromDefinition signatureTypeKind -> failwith "todo"
        | GenericInstantiation (generic, args) -> failwith "todo"
        | FunctionPointer typeMethodSignature -> failwith "todo"
        | GenericTypeParameter index -> failwith "todo"
        | GenericMethodParameter index -> failwith "todo"

    let fromTypeCode (s : SignatureTypeCode) : TypeDefn =
        match s with
        | SignatureTypeCode.Invalid -> failwith "todo"
        | SignatureTypeCode.Void -> TypeDefn.PrimitiveType PrimitiveType.Void
        | SignatureTypeCode.Boolean -> TypeDefn.PrimitiveType PrimitiveType.Boolean
        | SignatureTypeCode.Char -> TypeDefn.PrimitiveType PrimitiveType.Char
        | SignatureTypeCode.SByte -> TypeDefn.PrimitiveType PrimitiveType.SByte
        | SignatureTypeCode.Byte -> TypeDefn.PrimitiveType PrimitiveType.Byte
        | SignatureTypeCode.Int16 -> TypeDefn.PrimitiveType PrimitiveType.Int16
        | SignatureTypeCode.UInt16 -> TypeDefn.PrimitiveType PrimitiveType.UInt16
        | SignatureTypeCode.Int32 -> TypeDefn.PrimitiveType PrimitiveType.Int32
        | SignatureTypeCode.UInt32 -> TypeDefn.PrimitiveType PrimitiveType.UInt32
        | SignatureTypeCode.Int64 -> TypeDefn.PrimitiveType PrimitiveType.Int64
        | SignatureTypeCode.UInt64 -> TypeDefn.PrimitiveType PrimitiveType.UInt64
        | SignatureTypeCode.Single -> TypeDefn.PrimitiveType PrimitiveType.Single
        | SignatureTypeCode.Double -> TypeDefn.PrimitiveType PrimitiveType.Double
        | SignatureTypeCode.String -> TypeDefn.PrimitiveType PrimitiveType.String
        | SignatureTypeCode.Pointer -> failwith "todo"
        | SignatureTypeCode.ByReference -> failwith "TODO"
        | SignatureTypeCode.GenericTypeParameter -> failwith "todo"
        | SignatureTypeCode.Array -> failwith "todo"
        | SignatureTypeCode.GenericTypeInstance -> failwith "todo"
        | SignatureTypeCode.TypedReference -> TypeDefn.PrimitiveType PrimitiveType.TypedReference
        | SignatureTypeCode.IntPtr -> TypeDefn.PrimitiveType PrimitiveType.IntPtr
        | SignatureTypeCode.UIntPtr -> failwith "todo"
        | SignatureTypeCode.FunctionPointer -> failwith "todo"
        | SignatureTypeCode.Object -> failwith "todo"
        | SignatureTypeCode.SZArray -> failwith "todo"
        | SignatureTypeCode.GenericMethodParameter -> failwith "todo"
        | SignatureTypeCode.RequiredModifier -> failwith "todo"
        | SignatureTypeCode.OptionalModifier -> failwith "todo"
        | SignatureTypeCode.TypeHandle -> failwith "todo"
        | SignatureTypeCode.Sentinel -> failwith "todo"
        | SignatureTypeCode.Pinned -> failwith "todo"
        | x -> failwith $"Unrecognised type code: {x}"

    let typeProvider : ISignatureTypeProvider<TypeDefn, unit> =
        { new ISignatureTypeProvider<TypeDefn, unit> with
            member this.GetArrayType (elementType : TypeDefn, shape : ArrayShape) : TypeDefn =
                TypeDefn.Array (elementType, shape)

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
                let handle : EntityHandle = TypeDefinitionHandle.op_Implicit handle
                let typeKind = reader.ResolveSignatureTypeKind (handle, rawTypeKind)

                TypeDefn.FromDefinition typeKind

            member this.GetTypeFromReference
                (reader : MetadataReader, handle : TypeReferenceHandle, rawTypeKind : byte)
                : TypeDefn
                =
                let handle : EntityHandle = TypeReferenceHandle.op_Implicit handle
                let typeKind = reader.ResolveSignatureTypeKind (handle, rawTypeKind)
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
