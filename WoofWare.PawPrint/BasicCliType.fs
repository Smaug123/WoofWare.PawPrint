namespace WoofWare.PawPrint

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

[<NoComparison>]
type ManagedPointerSource =
    | LocalVariable of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Argument of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Heap of ManagedHeapAddress
    | ArrayIndex of arr : ManagedHeapAddress * index : int
    | Null

    override this.ToString () =
        match this with
        | ManagedPointerSource.Null -> "<null pointer>"
        | ManagedPointerSource.Heap addr -> $"%O{addr}"
        | ManagedPointerSource.LocalVariable (source, method, var) ->
            $"<variable %i{var} in method frame %i{method} of thread %O{source}>"
        | ManagedPointerSource.Argument (source, method, var) ->
            $"<argument %i{var} in method frame %i{method} of thread %O{source}>"
        | ManagedPointerSource.ArrayIndex (arr, index) -> $"<index %i{index} of array %O{arr}>"

[<RequireQualifiedAccess>]
type UnsignedNativeIntSource =
    | Verbatim of uint64
    | FromManagedPointer of ManagedPointerSource

[<RequireQualifiedAccess>]
type NativeIntSource =
    | Verbatim of int64
    | ManagedPointer of ManagedPointerSource
    | FunctionPointer of MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
    | TypeHandlePtr of ConcreteTypeHandle

    override this.ToString () : string =
        match this with
        | NativeIntSource.Verbatim int64 -> $"%i{int64}"
        | NativeIntSource.ManagedPointer ptr -> $"<managed pointer {ptr}>"
        | NativeIntSource.FunctionPointer methodDefinition ->
            $"<pointer to {methodDefinition.Name} in {methodDefinition.DeclaringType.Assembly.Name}>"
        | NativeIntSource.TypeHandlePtr ptr -> $"<type ID %O{ptr}>"

[<RequireQualifiedAccess>]
module NativeIntSource =
    let isZero (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i = 0L
        | NativeIntSource.TypeHandlePtr _ -> false
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.ManagedPointer src ->
            match src with
            | ManagedPointerSource.Null -> true
            | _ -> false

    let isNonnegative (n : NativeIntSource) : bool =
        match n with
        | NativeIntSource.Verbatim i -> i >= 0L
        | NativeIntSource.FunctionPointer _ -> failwith "TODO"
        | NativeIntSource.TypeHandlePtr _ -> true
        | NativeIntSource.ManagedPointer _ -> true

    /// True if a < b.
    let isLess (a : NativeIntSource) (b : NativeIntSource) : bool =
        match a, b with
        | NativeIntSource.Verbatim a, NativeIntSource.Verbatim b -> a < b
        | _, _ -> failwith "TODO"


/// Defined in III.1.1.1
type CliNumericType =
    | Int32 of int32
    | Int64 of int64
    /// The real CLR just represents these as native ints, but we track their provenance.
    | NativeInt of NativeIntSource
    | NativeFloat of float
    | Int8 of int8
    | Int16 of int16
    | UInt8 of uint8
    | UInt16 of uint16
    | Float32 of float32
    | Float64 of float

[<RequireQualifiedAccess>]
type CliRuntimePointerSource =
    | LocalVariable of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Argument of sourceThread : ThreadId * methodFrame : int * whichVar : uint16
    | Heap of ManagedHeapAddress
    | ArrayIndex of arr : ManagedHeapAddress * index : int
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
    | ValueType of CliValueType

and CliValueType =
    {
        Fields : (string * CliType) list
    }

type CliTypeResolutionResult =
    | Resolved of CliType
    | FirstLoad of WoofWare.PawPrint.AssemblyReference

[<RequireQualifiedAccess>]
module CliType =
    /// In fact any non-zero value will do for True, but we'll use 1
    let ofBool (b : bool) : CliType = CliType.Bool (if b then 1uy else 0uy)

    let ofChar (c : char) : CliType =
        CliType.Char (byte (int c / 256), byte (int c % 256))

    let ofManagedObject (ptr : ManagedHeapAddress) : CliType = CliType.ObjectRef (Some ptr)

    let rec sizeOf (ty : CliType) : int =
        match ty with
        | CliType.Numeric ty ->
            match ty with
            | CliNumericType.Int32 _ -> 4
            | CliNumericType.Int64 _ -> 8
            | CliNumericType.NativeInt _ -> 8
            | CliNumericType.NativeFloat _ -> 8
            | CliNumericType.Int8 _ -> 1
            | CliNumericType.Int16 _ -> 2
            | CliNumericType.UInt8 _ -> 1
            | CliNumericType.UInt16 _ -> 2
            | CliNumericType.Float32 _ -> 4
            | CliNumericType.Float64 _ -> 8
        | CliType.Bool _ -> 1
        | CliType.Char _ -> 2
        | CliType.ObjectRef _ -> 8
        | CliType.RuntimePointer _ -> 8
        | CliType.ValueType vt ->
            match vt.Fields with
            | [] -> failwith "is it even possible to instantiate a value type with no fields"
            | [ _, f ] -> sizeOf f
            | _ -> failwith $"TODO: %O{vt.Fields} (need to consider struct layout)"

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
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        : CliType * AllConcreteTypes
        =
        zeroOfWithVisited concreteTypes assemblies corelib handle Set.empty

    and zeroOfWithVisited
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (visited : Set<ConcreteTypeHandle>)
        : CliType * AllConcreteTypes
        =

        // Handle constructed types first
        match handle with
        | ConcreteTypeHandle.Byref _ ->
            // Byref types are managed references - the zero value is a null reference
            CliType.RuntimePointer (CliRuntimePointer.Managed CliRuntimePointerSource.Null), concreteTypes

        | ConcreteTypeHandle.Pointer _ ->
            // Pointer types are unmanaged pointers - the zero value is a null pointer
            CliType.RuntimePointer (CliRuntimePointer.Unmanaged 0L), concreteTypes

        | ConcreteTypeHandle.Concrete _ ->
            // This is a concrete type - look it up in the mapping
            let concreteType =
                match AllConcreteTypes.lookup handle concreteTypes with
                | Some ct -> ct
                | None -> failwithf "ConcreteTypeHandle %A not found in AllConcreteTypes" handle

            // Get the type definition from the assembly
            let assembly = assemblies.[concreteType.Assembly.FullName]
            let typeDef = assembly.TypeDefs.[concreteType.Definition.Get]

            // Check if it's a primitive type by comparing with corelib types FIRST
            if concreteType.Assembly = corelib.Corelib.Name && concreteType.Generics.IsEmpty then
                // Check against known primitive types
                if TypeInfo.NominallyEqual typeDef corelib.Boolean then
                    zeroOfPrimitive PrimitiveType.Boolean, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Char then
                    zeroOfPrimitive PrimitiveType.Char, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.SByte then
                    zeroOfPrimitive PrimitiveType.SByte, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Byte then
                    zeroOfPrimitive PrimitiveType.Byte, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Int16 then
                    zeroOfPrimitive PrimitiveType.Int16, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UInt16 then
                    zeroOfPrimitive PrimitiveType.UInt16, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Int32 then
                    zeroOfPrimitive PrimitiveType.Int32, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UInt32 then
                    zeroOfPrimitive PrimitiveType.UInt32, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Int64 then
                    zeroOfPrimitive PrimitiveType.Int64, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UInt64 then
                    zeroOfPrimitive PrimitiveType.UInt64, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Single then
                    zeroOfPrimitive PrimitiveType.Single, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Double then
                    zeroOfPrimitive PrimitiveType.Double, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.String then
                    zeroOfPrimitive PrimitiveType.String, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.Object then
                    zeroOfPrimitive PrimitiveType.Object, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.IntPtr then
                    zeroOfPrimitive PrimitiveType.IntPtr, concreteTypes
                elif TypeInfo.NominallyEqual typeDef corelib.UIntPtr then
                    zeroOfPrimitive PrimitiveType.UIntPtr, concreteTypes
                else if
                    // Check if it's an array type
                    typeDef = corelib.Array
                then
                    CliType.ObjectRef None, concreteTypes // Arrays are reference types
                else if
                    // Not a known primitive, now check for cycles
                    Set.contains handle visited
                then
                    // We're in a cycle - return a default zero value for the type
                    // For value types in cycles, we'll return a null reference as a safe fallback
                    // This should only happen with self-referential types
                    CliType.ObjectRef None, concreteTypes
                else
                    let visited = Set.add handle visited
                    // Not a known primitive, check if it's a value type or reference type
                    determineZeroForCustomType concreteTypes assemblies corelib handle concreteType typeDef visited
            else if
                // Not from corelib or has generics
                concreteType.Assembly = corelib.Corelib.Name
                && typeDef = corelib.Array
                && concreteType.Generics.Length = 1
            then
                // This is an array type
                CliType.ObjectRef None, concreteTypes
            else if
                // Custom type - now check for cycles
                Set.contains handle visited
            then
                // We're in a cycle - return a default zero value for the type
                // For value types in cycles, we'll return a null reference as a safe fallback
                // This should only happen with self-referential types
                CliType.ObjectRef None, concreteTypes
            else
                let visited = Set.add handle visited
                // Custom type - need to determine if it's a value type or reference type
                determineZeroForCustomType concreteTypes assemblies corelib handle concreteType typeDef visited

    and private determineZeroForCustomType
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (handle : ConcreteTypeHandle)
        (concreteType : ConcreteType<ConcreteTypeHandle>)
        (typeDef : WoofWare.PawPrint.TypeInfo<WoofWare.PawPrint.GenericParameter, TypeDefn>)
        (visited : Set<ConcreteTypeHandle>)
        : CliType * AllConcreteTypes
        =

        // Determine if this is a value type by checking inheritance
        let isValueType =
            match DumpedAssembly.resolveBaseType corelib assemblies typeDef.Assembly typeDef.BaseType with
            | ResolvedBaseType.ValueType
            | ResolvedBaseType.Enum -> true
            | ResolvedBaseType.Delegate -> false // Delegates are reference types
            | ResolvedBaseType.Object -> false

        if isValueType then
            // It's a value type - need to create zero values for all non-static fields
            let mutable currentConcreteTypes = concreteTypes

            let fieldZeros =
                typeDef.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))
                |> List.map (fun field ->
                    // Need to concretize the field type with the concrete type's generics
                    let fieldTypeDefn = field.Signature

                    let fieldHandle, updatedConcreteTypes =
                        concretizeFieldType currentConcreteTypes assemblies corelib concreteType fieldTypeDefn

                    currentConcreteTypes <- updatedConcreteTypes

                    let fieldZero, updatedConcreteTypes2 =
                        zeroOfWithVisited currentConcreteTypes assemblies corelib fieldHandle visited

                    currentConcreteTypes <- updatedConcreteTypes2
                    (field.Name, fieldZero)
                )

            let vt =
                {
                    Fields = fieldZeros
                }

            CliType.ValueType vt, currentConcreteTypes
        else
            // It's a reference type
            CliType.ObjectRef None, concreteTypes

    and private concretizeFieldType
        (concreteTypes : AllConcreteTypes)
        (assemblies : ImmutableDictionary<string, DumpedAssembly>)
        (corelib : BaseClassTypes<DumpedAssembly>)
        (declaringType : ConcreteType<ConcreteTypeHandle>)
        (fieldType : TypeDefn)
        : ConcreteTypeHandle * AllConcreteTypes
        =

        // Create a concretization context
        let ctx =
            {
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
                TypeConcretization.ConcretizationContext.ConcreteTypes = concreteTypes
                TypeConcretization.ConcretizationContext.LoadedAssemblies = assemblies
                TypeConcretization.ConcretizationContext.BaseTypes = corelib
            }

        // The field type might reference generic parameters of the declaring type
        let methodGenerics = ImmutableArray.Empty // Fields don't have method generics

        let loadAssembly
            (assyName : AssemblyName)
            (ref : AssemblyReferenceHandle)
            : (ImmutableDictionary<string, DumpedAssembly> * DumpedAssembly)
            =
            match assemblies.TryGetValue assyName.FullName with
            | true, currentAssy ->
                let targetAssyRef = currentAssy.AssemblyReferences.[ref]

                match assemblies.TryGetValue targetAssyRef.Name.FullName with
                | true, targetAssy -> assemblies, targetAssy
                | false, _ ->
                    failwithf "Assembly %s not loaded when trying to resolve reference" targetAssyRef.Name.FullName
            | false, _ -> failwithf "Current assembly %s not loaded when trying to resolve reference" assyName.FullName

        let handle, newCtx =
            TypeConcretization.concretizeType
                ctx
                loadAssembly
                declaringType.Assembly
                declaringType.Generics
                methodGenerics
                fieldType

        handle, newCtx.ConcreteTypes
