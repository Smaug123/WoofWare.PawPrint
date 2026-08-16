namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
[<NoComparison>]
type ResolvedBaseType =
    | Enum
    | ValueType
    | Object
    | Delegate

/// A method signature return shape. `void` is not a runtime type; it means the
/// callee returns no value to the caller. `System.Void` remains an ordinary
/// metadata type for reflection.
[<RequireQualifiedAccess>]
type MethodReturnType<'Types> =
    | Void
    | Returns of 'Types

    override this.ToString () =
        match this with
        | MethodReturnType.Void -> "void"
        | MethodReturnType.Returns ty -> string<'Types> ty

[<RequireQualifiedAccess>]
module MethodReturnType =
    let map<'a, 'b, 'state>
        (state : 'state)
        (f : 'state -> 'a -> 'state * 'b)
        (ret : MethodReturnType<'a>)
        : 'state * MethodReturnType<'b>
        =
        match ret with
        | MethodReturnType.Void -> state, MethodReturnType.Void
        | MethodReturnType.Returns ty ->
            let state, ty = f state ty
            state, MethodReturnType.Returns ty

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
        /// The return shape of the method.
        /// </summary>
        ReturnType : MethodReturnType<'Types>
    }

[<RequireQualifiedAccess>]
module TypeMethodSignature =
    let make<'T> (returnType : 'T -> MethodReturnType<'T>) (p : MethodSignature<'T>) : TypeMethodSignature<'T> =
        {
            Header = ComparableSignatureHeader.Make p.Header
            ReturnType = returnType p.ReturnType
            ParameterTypes = List.ofSeq p.ParameterTypes
            GenericParameterCount = p.GenericParameterCount
            RequiredParameterCount = p.RequiredParameterCount
        }

    let map<'a, 'b, 'state>
        (state : 'state)
        (f : 'state -> 'a -> 'state * 'b)
        (signature : TypeMethodSignature<'a>)
        : 'state * TypeMethodSignature<'b>
        =
        let state, ret = MethodReturnType.map state f signature.ReturnType

        let state, pars =
            ((state, []), signature.ParameterTypes)
            ||> List.fold (fun (state, acc) par ->
                let state, result = f state par
                state, result :: acc
            )

        let pars = List.rev pars

        let answer =
            {
                Header = signature.Header
                ReturnType = ret
                ParameterTypes = pars
                GenericParameterCount = signature.GenericParameterCount
                RequiredParameterCount = signature.RequiredParameterCount
            }

        state, answer

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
    /// I.8.2.1.1
    /// contains both a managed pointer to a location and a runtime representation of the type that can be stored at that location.
    ///
    /// In practice encountered only with varargs (which C# barely exposes) or `__makeref`-style low-level interop.
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

    override this.ToString () =
        match this with
        | PrimitiveType.Boolean -> "bool"
        | PrimitiveType.Char -> "char"
        | PrimitiveType.SByte -> "int8"
        | PrimitiveType.Byte -> "uint8"
        | PrimitiveType.Int16 -> "int16"
        | PrimitiveType.UInt16 -> "uint16"
        | PrimitiveType.Int32 -> "int32"
        | PrimitiveType.UInt32 -> "uint32"
        | PrimitiveType.Int64 -> "int64"
        | PrimitiveType.UInt64 -> "uint64"
        | PrimitiveType.Single -> "single"
        | PrimitiveType.Double -> "double"
        | PrimitiveType.String -> "string"
        | PrimitiveType.TypedReference -> "typedref"
        | PrimitiveType.IntPtr -> "intptr"
        | PrimitiveType.UIntPtr -> "uintptr"
        | PrimitiveType.Object -> "obj"

[<RequireQualifiedAccess>]
module PrimitiveType =
    let sizeOf (pt : PrimitiveType) : int =
        match pt with
        | PrimitiveType.Boolean -> 1
        | PrimitiveType.Char -> 2
        | PrimitiveType.SByte -> 1
        | PrimitiveType.Byte -> 1
        | PrimitiveType.Int16 -> 2
        | PrimitiveType.UInt16 -> 2
        | PrimitiveType.Int32 -> 4
        | PrimitiveType.UInt32 -> 4
        | PrimitiveType.Int64 -> 8
        | PrimitiveType.UInt64 -> 8
        | PrimitiveType.Single -> 4
        | PrimitiveType.Double -> 8
        | PrimitiveType.String -> 8
        | PrimitiveType.TypedReference -> failwith "todo"
        | PrimitiveType.IntPtr -> NATIVE_INT_SIZE
        | PrimitiveType.UIntPtr -> NATIVE_INT_SIZE
        | PrimitiveType.Object -> 8

type TypeDefn =
    | PrimitiveType of PrimitiveType
    /// A general (potentially multi-dimensional) array. Rank distinguishes e.g. int[,] from int[,,].
    /// Sizes and lower bounds from `ArrayShape` are not preserved: rather than silently conflating
    /// shapes that differ only in those, the signature decoder (`typeProvider`) accepts exactly one
    /// canonical encoding -- no sizes, one explicit zero lower bound per dimension -- and refuses
    /// every other. Every `Array` reaching the rest of the system is therefore known to be that
    /// shape, so consumers may treat rank as the whole of it. Whoever lifts that restriction must
    /// carry the bounds through `ConcreteTypeHandle.Array` too, which is what signature comparison
    /// actually comes down to.
    | Array of elt : TypeDefn * rank : int
    | Pinned of TypeDefn
    | Pointer of TypeDefn
    | Byref of TypeDefn
    | OneDimensionalArrayLowerBoundZero of elements : TypeDefn
    /// A type carrying a custom modifier (ECMA-335 `modreq`/`modopt`). The payload is a record
    /// rather than a tuple because the two `TypeDefn`s are trivially confusable and reading the
    /// wrong one is silent: see the field docs on <see cref="T:ModifiedTypeDefn"/>.
    | Modified of ModifiedTypeDefn
    | FromReference of TypeRef * SignatureTypeKind
    | FromDefinition of ResolvedTypeIdentity * SignatureTypeKind
    | GenericInstantiation of generic : TypeDefn * args : ImmutableArray<TypeDefn>
    | FunctionPointer of TypeMethodSignature<TypeDefn>
    /// <summary>
    /// A class/interface generic.
    /// </summary>
    /// <example>
    /// The type <c>List&lt;T&gt;</c> has a generic parameter; an instance method on that <c>List</c> would refer to
    /// <c>T</c> as <c>GenericTypeParameter 0</c>.
    /// </example>
    | GenericTypeParameter of index : int
    /// <summary>
    /// A method generic.
    /// </summary>
    /// <example>
    /// The method <c>List.map&lt;'a, 'b&gt;</c> takes two generic parameters; those are referred to as
    /// <c>GenericMethodParameter 0</c> and <c>GenericMethodParameter 1</c> respectively.
    /// </example>
    | GenericMethodParameter of index : int
    /// Not really a type: this indicates the *absence* of a return value.
    | Void

    override this.ToString () =
        match this with
        | TypeDefn.PrimitiveType primitiveType -> $"%O{primitiveType}"
        | TypeDefn.Array (elt, rank) -> $"arr[%O{elt} ; rank=%i{rank}]"
        | TypeDefn.Pinned typeDefn -> $"pinned[%s{string<TypeDefn> typeDefn}]"
        | TypeDefn.Pointer typeDefn -> $"ptr[%s{string<TypeDefn> typeDefn}]"
        | TypeDefn.Byref typeDefn -> $"byref[%s{string<TypeDefn> typeDefn}]"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> $"arr[%s{string<TypeDefn> elements}]"
        | TypeDefn.Modified m ->
            let req = if m.IsRequired then "modreq" else "modopt"

            $"modified[%s{string<TypeDefn> m.Unmodified} ; %s{req}=%s{string<TypeDefn> m.Modifier}]"
        | TypeDefn.FromReference (typeRef, _) -> $"ref[%s{typeRef.Namespace}.%s{typeRef.Name}]"
        | TypeDefn.FromDefinition (identity, _) ->
            let name = identity.AssemblyFullName.Split ',' |> Array.head

            $"<type defined in %s{name}>"
        | TypeDefn.GenericInstantiation (generic, args) ->
            let args = args |> Seq.map string<TypeDefn> |> String.concat ", "
            $"%s{string<TypeDefn> generic}[%s{args}]"
        | TypeDefn.FunctionPointer typeMethodSignature ->
            let args =
                typeMethodSignature.ParameterTypes
                |> List.map string<TypeDefn>
                |> String.concat " -> "

            $"*(%s{args} -> %s{string<MethodReturnType<TypeDefn>> typeMethodSignature.ReturnType})"
        | TypeDefn.GenericTypeParameter index -> $"<type param %i{index}>"
        | TypeDefn.GenericMethodParameter index -> $"<method param %i{index}>"
        | TypeDefn.Void -> "void"

/// The payload of <see cref="T:TypeDefn.Modified"/>: a type with an ECMA-335 custom modifier
/// (`modreq`/`modopt`) attached.
and ModifiedTypeDefn =
    {
        /// The type the modifier is attached to — i.e. what the signature would have said with the
        /// modifier deleted. Runtime type identity and storage shape follow this, so this is almost
        /// always the field you want when looking *through* a modifier.
        Unmodified : TypeDefn
        /// The modifier itself: `System.Runtime.InteropServices.InAttribute`,
        /// `System.Runtime.CompilerServices.IsVolatile`, `System.Runtime.CompilerServices.CallConvCdecl`,
        /// and friends. This is an annotation on the signature, not the type being described.
        Modifier : TypeDefn
        /// `true` for `modreq` (a consumer that doesn't understand the modifier must reject the
        /// signature); `false` for `modopt` (it may ignore it).
        IsRequired : bool
    }

[<RequireQualifiedAccess>]
module TypeDefn =
    /// The width a field of this type occupies, when that follows from the signature's head alone.
    ///
    /// This is CoreCLR's `FieldDesc::LoadSize` (field.cpp:655): it reads the field's normalised
    /// `CorElementType` and takes the width from `CorTypeInfo`'s table (cortypeinfo.h), loading a
    /// type only for `ELEMENT_TYPE_VALUETYPE` — the one row whose width the table cannot state.
    ///
    /// `None` is that row, and means the width genuinely needs the type loaded: for a signature
    /// mentioning a generic parameter, that in turn needs an instantiation. `Some` means no
    /// instantiation can change the answer, however the operand types are spelled — a `T*` is one
    /// pointer wide whatever `T` is, which is why an RVA field on an open generic type can still
    /// be sized.
    let rec tryFixedSize (ty : TypeDefn) : int option =
        match ty with
        | TypeDefn.PrimitiveType primitiveType ->
            match primitiveType with
            | PrimitiveType.TypedReference ->
                // CoreCLR's table gives this two pointers, but `PrimitiveType.sizeOf` has no
                // answer for it and no field can legally hold one — `TypedReference` is
                // byref-like. Defer rather than invent a width that nothing can check.
                None
            | primitiveType -> Some (PrimitiveType.sizeOf primitiveType)
        // ELEMENT_TYPE_PTR, BYREF, FNPTR, SZARRAY and ARRAY are each one pointer wide, whatever
        // they refer to; the referent never has to be loaded to say so.
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.FunctionPointer _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Array _ -> Some NATIVE_INT_SIZE
        // A custom modifier does not change the width of what it modifies, and `Pinned` is a
        // local-variable decoration that cannot appear in a field signature at all.
        | TypeDefn.Modified modified -> tryFixedSize modified.Unmodified
        | TypeDefn.Pinned inner -> tryFixedSize inner
        | TypeDefn.FromReference (_, signatureTypeKind)
        | TypeDefn.FromDefinition (_, signatureTypeKind) ->
            match signatureTypeKind with
            | SignatureTypeKind.Class -> Some NATIVE_INT_SIZE
            // `ELEMENT_TYPE_VALUETYPE`: the width is the type's instance-field bytes, so it needs
            // the type loaded.
            | SignatureTypeKind.ValueType -> None
            // An undecoded signature kind is not a licence to guess a width.
            | _ -> None
        | TypeDefn.GenericInstantiation (generic, _) ->
            // The instantiation's element type is the generic definition's — `List<int>` is a
            // CLASS, `Nullable<int>` a VALUETYPE — and the arguments cannot change that.
            tryFixedSize generic
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ ->
            // `ELEMENT_TYPE_VAR`/`MVAR`. A `FieldDesc` carries an element type fixed when its
            // instantiation was loaded, so there is no width to read off the open signature.
            None
        | TypeDefn.Void -> None

    let isManaged (typeDefn : TypeDefn) : bool =
        match typeDefn with
        | TypeDefn.PrimitiveType primitiveType -> failwith "todo"
        | TypeDefn.Array (elt, rank) -> failwith "todo"
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer typeDefn -> failwith "todo"
        | TypeDefn.Byref typeDefn -> failwith "todo"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
        | TypeDefn.Modified _ ->
            failwith "todo: TypeDefn.isManaged of a type carrying a custom modifier (modreq/modopt)"
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

    let typeProvider (a : AssemblyName) : ISignatureTypeProvider<TypeDefn, unit> =
        { new ISignatureTypeProvider<TypeDefn, unit> with
            member this.GetArrayType (elementType : TypeDefn, shape : ArrayShape) : TypeDefn =
                // `TypeDefn.Array` records only the rank, so a shape carrying explicit sizes or a
                // non-zero lower bound would decode to the same `TypeDefn` -- and thence to the same
                // `ConcreteTypeHandle` -- as the unadorned array of that rank. Handle equality
                // decides more than type identity: `NativeRuntimeTypeHelpers` matches vtable
                // slots by comparing concretised signatures, and CoreCLR's `CompareElementType` does
                // compare sizes and bounds, so silently conflating the two would bind an override to
                // the wrong slot. Refuse at the point of loss rather than answer wrongly downstream.
                //
                // The accepted shape is one *canonical encoding*, not one canonical meaning, because
                // ECMA-335 II.23.2.13 makes both counts optional and CoreCLR compares the counts
                // themselves: `MetaSig::CompareElementType` returns FALSE on
                // `dimension_lowerb1 != dimension_lowerb2` before it ever looks at the values
                // (siginfo.cpp:4317). So an omitted lower-bound vector and an explicitly encoded
                // all-zero one denote the same type but are *not* interchangeable for override
                // matching, and `ArrayShape` does preserve the distinction: a decoded blob with
                // `numLoBounds = 0` yields `LowerBounds = []`, not a synthesised vector of zeros.
                // Accepting both would therefore reintroduce exactly the conflation this guard
                // exists to prevent.
                //
                // Canonical here means: no sizes, and one explicit zero per dimension. That is what
                // real compilers emit -- measured over the linux-x64 runtime pack, FSharp.Core, the
                // Roslyn assemblies and this repo's own test binaries, all 339 multidimensional
                // array signatures use it and not one uses the omitted form. `Array.CreateInstance`
                // can make a non-zero-based array at runtime without ever writing one into a
                // signature.
                let lowerBoundsAreCanonical =
                    shape.LowerBounds.Length = shape.Rank
                    && shape.LowerBounds |> Seq.forall (fun bound -> bound = 0)

                if not shape.Sizes.IsEmpty || not lowerBoundsAreCanonical then
                    failwithf
                        "TODO: multidimensional array signature in %s with a non-canonical ArrayShape (rank %i, sizes %A, lower bounds %A); the canonical encoding has no sizes and one explicit zero lower bound per dimension. TypeDefn.Array records only the rank, so accepting this would make it compare equal to the canonically-encoded array of that rank, which CoreCLR treats as a different signature"
                        a.FullName
                        shape.Rank
                        (List.ofSeq shape.Sizes)
                        (List.ofSeq shape.LowerBounds)

                TypeDefn.Array (elementType, shape.Rank)

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

                TypeDefn.FromDefinition (ResolvedTypeIdentity.ofTypeDefinition a handle, typeKind)

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
                TypeDefn.FunctionPointer (
                    TypeMethodSignature.make
                        (function
                        | TypeDefn.Void -> MethodReturnType.Void
                        | retType -> MethodReturnType.Returns retType)
                        signature
                )

            member this.GetGenericMethodParameter (genericContext, index) = TypeDefn.GenericMethodParameter index
            member this.GetGenericTypeParameter (genericContext, index) = TypeDefn.GenericTypeParameter index

            // Note the BCL's parameter order here: `ISignatureTypeProvider.GetModifiedType` takes the
            // *modifier* first and the type it is attached to second.
            member this.GetModifiedType (modifier, unmodifiedType, isRequired) =
                TypeDefn.Modified
                    {
                        Unmodified = unmodifiedType
                        Modifier = modifier
                        IsRequired = isRequired
                    }

            member this.GetPinnedType elementType = TypeDefn.Pinned elementType
            member this.GetTypeFromSpecification (reader, genericContext, handle, rawTypeKind) = failwith "todo"
        }
