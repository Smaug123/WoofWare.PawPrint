namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata

/// Currently this is just an opaque handle; it can't be treated as a pointer.
type ManagedHeapAddress = | ManagedHeapAddress of int

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
type CliRuntimePointerSource = | LocalVariable of sourceThread : ThreadId * methodFrame : int * whichVar : uint16

type CliRuntimePointer =
    | Unmanaged of unit
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
    let rec zeroOf
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (assy : DumpedAssembly)
        (typeGenerics : TypeDefn ImmutableArray option)
        (methodGenerics : TypeDefn ImmutableArray option)
        (ty : TypeDefn)
        : CliTypeResolutionResult
        =
        match ty with
        | TypeDefn.PrimitiveType primitiveType ->
            match primitiveType with
            | PrimitiveType.Boolean -> CliType.Bool 0uy
            | PrimitiveType.Char -> CliType.Char (0uy, 0uy)
            | PrimitiveType.SByte -> CliType.Numeric (CliNumericType.Int8 0y)
            | PrimitiveType.Byte -> CliType.Numeric (CliNumericType.UInt8 0uy)
            | PrimitiveType.Int16 -> CliType.Numeric (CliNumericType.Int16 0s)
            | PrimitiveType.UInt16 -> CliType.Numeric (CliNumericType.UInt16 0us)
            | PrimitiveType.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
            | PrimitiveType.UInt32 -> failwith "todo"
            | PrimitiveType.Int64 -> CliType.Numeric (CliNumericType.Int64 0L)
            | PrimitiveType.UInt64 -> failwith "todo"
            | PrimitiveType.Single -> CliType.Numeric (CliNumericType.Float32 0.0f)
            | PrimitiveType.Double -> CliType.Numeric (CliNumericType.Float64 0.0)
            | PrimitiveType.String -> CliType.ObjectRef None
            | PrimitiveType.TypedReference -> failwith "todo"
            | PrimitiveType.IntPtr -> CliType.Numeric (CliNumericType.Int64 0L)
            | PrimitiveType.UIntPtr -> CliType.Numeric (CliNumericType.Int64 0L)
            | PrimitiveType.Object -> CliType.ObjectRef None
            |> CliTypeResolutionResult.Resolved
        | TypeDefn.Array _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.Byref _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
        | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
        | TypeDefn.FromReference (typeRef, signatureTypeKind) ->
            match signatureTypeKind with
            | SignatureTypeKind.Unknown -> failwith "todo"
            | SignatureTypeKind.ValueType ->
                match Assembly.resolveTypeRef assemblies assy typeRef typeGenerics with
                | TypeResolutionResult.Resolved (_, ty) -> failwith $"TODO: {ty}"
                | TypeResolutionResult.FirstLoadAssy assy -> CliTypeResolutionResult.FirstLoad assy
            | SignatureTypeKind.Class -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
            | _ -> raise (ArgumentOutOfRangeException ())
        | TypeDefn.FromDefinition (typeDefinitionHandle, signatureTypeKind) ->
            match signatureTypeKind with
            | SignatureTypeKind.Unknown -> failwith "todo"
            | SignatureTypeKind.ValueType ->
                let typeDef = assy.TypeDefs.[typeDefinitionHandle.Get]

                let fields =
                    typeDef.Fields
                    |> List.map (fun fi -> zeroOf assemblies assy typeGenerics methodGenerics fi.Signature)

                CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
            | SignatureTypeKind.Class -> CliType.ObjectRef None |> CliTypeResolutionResult.Resolved
            | _ -> raise (ArgumentOutOfRangeException ())
        | TypeDefn.GenericInstantiation (generic, args) -> zeroOf assemblies assy (Some args) methodGenerics generic
        | TypeDefn.FunctionPointer typeMethodSignature -> failwith "todo"
        | TypeDefn.GenericTypeParameter index ->
            // TODO: can generics depend on other generics? presumably, so we pass the array down again
            match typeGenerics with
            | None -> failwith "asked for a type parameter of generic type, but no generics in scope"
            | Some generics -> zeroOf assemblies assy (Some generics) methodGenerics generics.[index]
        | TypeDefn.GenericMethodParameter index ->
            match methodGenerics with
            | None -> failwith "asked for a method parameter of generic type, but no generics in scope"
            | Some generics -> zeroOf assemblies assy typeGenerics (Some generics) generics.[index]
        | TypeDefn.Void -> failwith "should never construct an element of type Void"
