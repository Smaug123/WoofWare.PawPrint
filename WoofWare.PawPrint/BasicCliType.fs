namespace WoofWare.PawPrint

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

type CliRuntimePointer =
    | Unmanaged of unit
    | Managed of unit

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

[<RequireQualifiedAccess>]
module CliType =
    let zeroOf (ty : TypeDefn) : CliType =
        match ty with
        | TypeDefn.PrimitiveType primitiveType ->
            match primitiveType with
            | PrimitiveType.Void -> failwith "todo"
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
            | PrimitiveType.Single -> failwith "todo"
            | PrimitiveType.Double -> failwith "todo"
            | PrimitiveType.String -> CliType.ObjectRef None
            | PrimitiveType.TypedReference -> failwith "todo"
            | PrimitiveType.IntPtr -> failwith "todo"
            | PrimitiveType.UIntPtr -> failwith "todo"
            | PrimitiveType.Object -> failwith "todo"
        | TypeDefn.Array (elt, shape) -> CliType.ObjectRef None
        | TypeDefn.Pinned typeDefn -> failwith "todo"
        | TypeDefn.Pointer typeDefn -> failwith "todo"
        | TypeDefn.Byref typeDefn -> failwith "todo"
        | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
        | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
        | TypeDefn.FromReference (typeReferenceHandle, signatureTypeKind) -> failwith "todo"
        | TypeDefn.FromDefinition (typeDefinitionHandle, signatureTypeKind) -> failwith "todo"
        | TypeDefn.GenericInstantiation (generic, args) -> failwith "todo"
        | TypeDefn.FunctionPointer typeMethodSignature -> failwith "todo"
        | TypeDefn.GenericTypeParameter index -> failwith "todo"
        | TypeDefn.GenericMethodParameter index -> failwith "todo"
