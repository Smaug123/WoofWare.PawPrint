namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.FSharp.Core

/// <summary>
/// Represents a method signature with type parameters.
/// Corresponds to MethodSignature in System.Reflection.Metadata.
/// </summary>
type TypeMethodSignature<'Types> =
    {
        /// <summary>
        /// Contains calling convention and other method attributes encoded in the metadata.
        /// </summary>
        Header : ComparableSignatureHeader

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
            Header = ComparableSignatureHeader.Make p.Header
            ReturnType = p.ReturnType
            ParameterTypes = List.ofSeq p.ParameterTypes
            GenericParameterCount = p.GenericParameterCount
            RequiredParameterCount = p.RequiredParameterCount
        }

/// See I.8.2.2
type PrimitiveType =
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

    static member OfEnum (ptc : PrimitiveTypeCode) : PrimitiveType option =
        match ptc with
        | PrimitiveTypeCode.Void -> None
        | PrimitiveTypeCode.Boolean -> PrimitiveType.Boolean |> Some
        | PrimitiveTypeCode.Char -> PrimitiveType.Char |> Some
        | PrimitiveTypeCode.SByte -> PrimitiveType.SByte |> Some
        | PrimitiveTypeCode.Byte -> PrimitiveType.Byte |> Some
        | PrimitiveTypeCode.Int16 -> PrimitiveType.Int16 |> Some
        | PrimitiveTypeCode.UInt16 -> PrimitiveType.UInt16 |> Some
        | PrimitiveTypeCode.Int32 -> PrimitiveType.Int32 |> Some
        | PrimitiveTypeCode.UInt32 -> PrimitiveType.UInt32 |> Some
        | PrimitiveTypeCode.Int64 -> PrimitiveType.Int64 |> Some
        | PrimitiveTypeCode.UInt64 -> PrimitiveType.UInt64 |> Some
        | PrimitiveTypeCode.Single -> PrimitiveType.Single |> Some
        | PrimitiveTypeCode.Double -> PrimitiveType.Double |> Some
        | PrimitiveTypeCode.String -> PrimitiveType.String |> Some
        | PrimitiveTypeCode.TypedReference -> PrimitiveType.TypedReference |> Some
        | PrimitiveTypeCode.IntPtr -> PrimitiveType.IntPtr |> Some
        | PrimitiveTypeCode.UIntPtr -> PrimitiveType.UIntPtr |> Some
        | PrimitiveTypeCode.Object -> PrimitiveType.Object |> Some
        | x -> failwithf $"Unrecognised primitive type code: %O{x}"

type TypeDefn =
    | PrimitiveType of PrimitiveType
    | Array of elt : TypeDefn * shape : unit
    | Pinned of TypeDefn
    | Pointer of TypeDefn
    | Byref of TypeDefn
    | OneDimensionalArrayLowerBoundZero of elements : TypeDefn
    | Modified of original : TypeDefn * afterMod : TypeDefn * modificationRequired : bool
    | FromReference of TypeRef * SignatureTypeKind
    | FromDefinition of ComparableTypeDefinitionHandle * SignatureTypeKind
    | GenericInstantiation of generic : TypeDefn * args : ImmutableArray<TypeDefn>
    | FunctionPointer of TypeMethodSignature<TypeDefn>
    | GenericTypeParameter of index : int
    | GenericMethodParameter of index : int
    /// Not really a type: this indicates the *absence* of a return value.
    | Void

[<RequireQualifiedAccess>]
module TypeDefn =
    let isManaged (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | TypeDefn.PrimitiveType primitiveType -> failwith "todo"
        | TypeDefn.Array (elt, shape) -> failwith "todo"
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer typeDefn -> failwith "todo"
        | TypeDefn.Byref typeDefn -> failwith "todo"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
        | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
        | TypeDefn.FromReference _ -> true
        | TypeDefn.FromDefinition (_, signatureTypeKind) ->
            match signatureTypeKind with
            | SignatureTypeKind.Unknown -> failwith "todo"
            | SignatureTypeKind.ValueType -> false
            | SignatureTypeKind.Class -> true
            | s -> raise (System.ArgumentOutOfRangeException ())
        | TypeDefn.GenericInstantiation (generic, args) -> failwith "todo"
        | TypeDefn.FunctionPointer typeMethodSignature -> failwith "todo"
        | TypeDefn.GenericTypeParameter index -> failwith "todo"
        | TypeDefn.GenericMethodParameter index -> failwith "todo"
        | TypeDefn.Void -> false

    let fromTypeCode (s : SignatureTypeCode) : TypeDefn =
        match s with
        | SignatureTypeCode.Invalid -> failwith "todo"
        | SignatureTypeCode.Void -> TypeDefn.Void
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
                TypeDefn.Array (elementType, ())

            member this.GetByReferenceType (elementType : TypeDefn) : TypeDefn = TypeDefn.Byref elementType

            member this.GetSZArrayType (elementType : TypeDefn) : TypeDefn =
                TypeDefn.OneDimensionalArrayLowerBoundZero elementType

            member this.GetPrimitiveType (elementType : PrimitiveTypeCode) : TypeDefn =
                match PrimitiveType.OfEnum elementType with
                | None -> TypeDefn.Void
                | Some v -> TypeDefn.PrimitiveType v

            member this.GetGenericInstantiation
                (generic : TypeDefn, typeArguments : ImmutableArray<TypeDefn>)
                : TypeDefn
                =
                TypeDefn.GenericInstantiation (generic, typeArguments)

            member this.GetTypeFromDefinition
                (reader : MetadataReader, handle : TypeDefinitionHandle, rawTypeKind : byte)
                : TypeDefn
                =
                let handle' : EntityHandle = TypeDefinitionHandle.op_Implicit handle
                let typeKind = reader.ResolveSignatureTypeKind (handle', rawTypeKind)

                TypeDefn.FromDefinition (ComparableTypeDefinitionHandle.Make handle, typeKind)

            member this.GetTypeFromReference
                (reader : MetadataReader, handle : TypeReferenceHandle, rawTypeKind : byte)
                : TypeDefn
                =
                let handle' : EntityHandle = TypeReferenceHandle.op_Implicit handle
                let ref = handle |> TypeRef.make reader
                let typeKind = reader.ResolveSignatureTypeKind (handle', rawTypeKind)
                TypeDefn.FromReference (ref, typeKind)

            member this.GetPointerType (typeCode : TypeDefn) : TypeDefn = TypeDefn.Pointer typeCode

            member this.GetFunctionPointerType signature =
                TypeDefn.FunctionPointer (TypeMethodSignature.make signature)

            member this.GetGenericMethodParameter (genericContext, index) = TypeDefn.GenericMethodParameter index
            member this.GetGenericTypeParameter (genericContext, index) = TypeDefn.GenericTypeParameter index

            member this.GetModifiedType (modifier, unmodifiedType, isRequired) =
                TypeDefn.Modified (unmodifiedType, modifier, isRequired)

            member this.GetPinnedType elementType = TypeDefn.Pinned elementType
            member this.GetTypeFromSpecification (reader, genericContext, handle, rawTypeKind) = failwith "todo"
        }
