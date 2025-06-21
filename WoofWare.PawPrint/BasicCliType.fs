namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

/// Source:
/// Table I.6: Data Types Directly Supported by the CLI
type CliSupportedObject =
    /// Can be assigned the null value 0
    /// This is the 'O' type.
    | ObjectReference of ManagedHeapAddress option
    /// This is the '&' type. It can point to managed or unmanaged memory.
    /// TODO: the contents of this are therefore wrong
    | PointerType of ManagedHeapAddress option
    | Int8 of int8
    | UInt8 of uint8
    | Int16 of int16
    | UInt16 of uint16
    | Int32 of int32
    | UInt32 of uint32
    | Int64 of int64
    | UInt64 of uint64
    | Float32 of float32
    | Float64 of float
    | NativeInt of int64
    | NativeUint of uint64

/// Defined in III.1.1
type BasicCliType =
    | ObjectReference of ManagedHeapAddress option
    | PointerType of ManagedHeapAddress option
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | NativeFloat of float

/// Defined in III.1.1.1
type CliNumericType =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | NativeFloat of float
    | Int8 of int8
    | Int16 of int16
    | UInt8 of uint8
    | UInt16 of uint16
    | Float32 of float32
    | Float64 of float
    /// Not a real CLI numeric type! Represents an int64 obtained by taking a NativeInt from the eval stack.
    | ProvenanceTrackedNativeInt64 of MethodInfo<FakeUnit, WoofWare.PawPrint.GenericParameter>
    /// Not a real CLI numeric type! An opaque TypeHandle pointer.
    | TypeHandlePtr of int64<typeHandle>

type CliValueType =
    private
    | Bool of byte
    /// A UTF-16 code unit, i.e. two bytes. We store the most significant one first.
    | Char of byte * byte
    | UInt8 of uint8
    | UInt16 of uint16
    | Int8 of int8
    | Int16 of int16
    | Float32 of float32
    | Float64 of float

[<RequireQualifiedAccess>]
type CliRuntimePointerSource =
    | LocalVariable of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Argument of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Heap of ManagedHeapAddress
    | Null

type CliRuntimePointer =
    | Unmanaged of int64
    | Managed of CliRuntimePointerSource

/// This is the kind of type that can be stored in arguments, local variables, statics, array elements, fields.
type CliType =
    /// III.1.1.1
    | Numeric of CliNumericType
    /// III.1.1.2
    | Bool of byte
    /// III.1.1.3
    | Char of high : byte * low : byte
    /// III.1.1.4 - this is a completely opaque handle to a managed object; arithmetic is forbidden
    | ObjectRef of ManagedHeapAddress option
    /// III.1.1.5
    | RuntimePointer of CliRuntimePointer
    /// This is *not* a CLI type as such. I don't actually know its status. A value type is represented simply
    /// as a concatenated list of its fields.
    | ValueType of CliType list

    /// In fact any non-zero value will do for True, but we'll use 1
    static member OfBool (b : bool) = CliType.Bool (if b then 1uy else 0uy)

    static member OfChar (c : char) =
        CliType.Char (byte (int c / 256), byte (int c % 256))

    static member OfManagedObject (ptr : ManagedHeapAddress) = CliType.ObjectRef (Some ptr)

type CliTypeResolutionResult =
    | Resolved of CliType
    | FirstLoad of WoofWare.PawPrint.AssemblyReference

[<RequireQualifiedAccess>]
module CliType =

    let zeroOfPrimitive (primitiveType : PrimitiveType) : CliType =
        match primitiveType with
        | PrimitiveType.Boolean -> CliType.Bool 0uy
        | PrimitiveType.Char -> CliType.Char (0uy, 0uy)
        | PrimitiveType.SByte -> CliType.Numeric (CliNumericType.Int8 0y)
        | PrimitiveType.Byte -> CliType.Numeric (CliNumericType.UInt8 0uy)
        | PrimitiveType.Int16 -> CliType.Numeric (CliNumericType.Int16 0s)
        | PrimitiveType.UInt16 -> CliType.Numeric (CliNumericType.UInt16 0us)
        | PrimitiveType.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
        | PrimitiveType.UInt32 ->
            // uint32 doesn't exist; the spec has them stored on the stack as if signed, with two's complement wraparound
            CliType.Numeric (CliNumericType.Int32 0)
        | PrimitiveType.Int64 -> CliType.Numeric (CliNumericType.Int64 0L)
        | PrimitiveType.UInt64 ->
            // uint64 doesn't exist; the spec has them stored on the stack as if signed, with two's complement wraparound
            CliType.Numeric (CliNumericType.Int64 0L)
        | PrimitiveType.Single -> CliType.Numeric (CliNumericType.Float32 0.0f)
        | PrimitiveType.Double -> CliType.Numeric (CliNumericType.Float64 0.0)
        | PrimitiveType.String -> CliType.ObjectRef None
        | PrimitiveType.TypedReference -> failwith "todo"
        | PrimitiveType.IntPtr -> CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null)
        | PrimitiveType.UIntPtr -> CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null)
        | PrimitiveType.Object -> CliType.ObjectRef None

    let rec zeroOf
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (assy : DumpedAssembly)
        (typeGenerics : TypeDefn ImmutableArray option)
        (methodGenerics : TypeDefn ImmutableArray option)
        (ty : TypeDefn)
        : CliTypeResolutionResult
        =
        match ty with
        | TypeDefn.PrimitiveType primitiveType -> CliTypeResolutionResult.Resolved (zeroOfPrimitive primitiveType)
        | TypeDefn.Array _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer _ ->
            CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null)
            |> CliTypeResolutionResult.Resolved
        | TypeDefn.Byref _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
        | TypeDefn.FromReference (typeRef, signatureTypeKind) ->
            match signatureTypeKind with
            | SignatureTypeKind.Unknown -> failwith "todo"
            | SignatureTypeKind.ValueType ->
                match Assembly.resolveTypeRef assemblies assy typeRef typeGenerics with
                | TypeResolutionResult.Resolved (sourceAssy, ty) ->
                    let fields =
                        ty.Fields
                        |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                        |> List.map (fun fi ->
                            match zeroOf assemblies corelib sourceAssy typeGenerics methodGenerics fi.Signature with
                            | CliTypeResolutionResult.Resolved ty -> Ok ty
                            | CliTypeResolutionResult.FirstLoad a -> Error a
                        )
                        |> Result.allOkOrError

                    match fields with
                    | Error (_, []) -> failwith "logic error"
                    | Error (_, f :: _) -> CliTypeResolutionResult.FirstLoad f
                    | Ok fields -> CliType.ValueType fields |> CliTypeResolutionResult.Resolved
                | TypeResolutionResult.FirstLoadAssy assy -> CliTypeResolutionResult.FirstLoad assy
            | SignatureTypeKind.Class -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
            | _ -> raise (ArgumentOutOfRangeException ())
        | TypeDefn.FromDefinition (typeDefinitionHandle, _, signatureTypeKind) ->
            let typeDef = assy.TypeDefs.[typeDefinitionHandle.Get]

            if typeDef = corelib.Int32 then
                zeroOfPrimitive PrimitiveType.Int32 |> CliTypeResolutionResult.Resolved
            elif typeDef = corelib.Int64 then
                zeroOfPrimitive PrimitiveType.Int64 |> CliTypeResolutionResult.Resolved
            elif typeDef = corelib.UInt32 then
                zeroOfPrimitive PrimitiveType.UInt32 |> CliTypeResolutionResult.Resolved
            elif typeDef = corelib.UInt64 then
                zeroOfPrimitive PrimitiveType.UInt64 |> CliTypeResolutionResult.Resolved
            else
            // TODO: the rest
            match signatureTypeKind with
            | SignatureTypeKind.Unknown -> failwith "todo"
            | SignatureTypeKind.ValueType ->
                let fields =
                    typeDef.Fields
                    // oh lord, this is awfully ominous - I really don't want to store the statics here
                    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                    |> List.map (fun fi ->
                        match zeroOf assemblies corelib assy typeGenerics methodGenerics fi.Signature with
                        | CliTypeResolutionResult.Resolved ty -> Ok ty
                        | CliTypeResolutionResult.FirstLoad a -> Error a
                    )
                    |> Result.allOkOrError

                match fields with
                | Error (_, []) -> failwith "logic error"
                | Error (_, f :: _) -> CliTypeResolutionResult.FirstLoad f
                | Ok fields ->

                CliType.ValueType fields |> CliTypeResolutionResult.Resolved
            | SignatureTypeKind.Class -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
            | _ -> raise (ArgumentOutOfRangeException ())
        | TypeDefn.GenericInstantiation (generic, args) ->
            zeroOf assemblies corelib assy (Some args) methodGenerics generic
        | TypeDefn.FunctionPointer typeMethodSignature -> failwith "todo"
        | TypeDefn.GenericTypeParameter index ->
            // TODO: can generics depend on other generics? presumably, so we pass the array down again
            match typeGenerics with
            | None -> failwith "asked for a type parameter of generic type, but no generics in scope"
            | Some generics -> zeroOf assemblies corelib assy (Some generics) methodGenerics generics.[index]
        | TypeDefn.GenericMethodParameter index ->
            match methodGenerics with
            | None -> failwith "asked for a method parameter of generic type, but no generics in scope"
            | Some generics -> zeroOf assemblies corelib assy typeGenerics (Some generics) generics.[index]
        | TypeDefn.Void -> failwith "should never construct an element of type Void"
