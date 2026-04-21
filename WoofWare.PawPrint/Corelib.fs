namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module Corelib =

    let private findCorelibType
        (corelib : DumpedAssembly)
        (``namespace`` : string)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        corelib.TypeDefs
        |> Seq.choose (fun (KeyValue (_, v)) ->
            if v.Namespace = ``namespace`` && v.Name = name then
                Some v
            else
                None
        )
        |> Seq.exactlyOne

    let private tryFindCorelibType
        (corelib : DumpedAssembly)
        (``namespace`` : string)
        (names : string list)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        corelib.TypeDefs
        |> Seq.tryPick (fun (KeyValue (_, v)) ->
            if v.Namespace = ``namespace`` && List.contains v.Name names then
                Some v
            else
                None
        )

    let getBaseTypes (corelib : DumpedAssembly) : BaseClassTypes<DumpedAssembly> =
        let stringType = findCorelibType corelib "System" "String"
        let arrayType = findCorelibType corelib "System" "Array"
        let enumType = findCorelibType corelib "System" "Enum"
        let objType = findCorelibType corelib "System" "Object"
        let valueType = findCorelibType corelib "System" "ValueType"
        let boolean = findCorelibType corelib "System" "Boolean"
        let char = findCorelibType corelib "System" "Char"
        let byte = findCorelibType corelib "System" "Byte"
        let sbyte = findCorelibType corelib "System" "SByte"
        let int16 = findCorelibType corelib "System" "Int16"
        let int32 = findCorelibType corelib "System" "Int32"
        let int64 = findCorelibType corelib "System" "Int64"
        let uint16 = findCorelibType corelib "System" "UInt16"
        let uint32 = findCorelibType corelib "System" "UInt32"
        let uint64 = findCorelibType corelib "System" "UInt64"
        let single = findCorelibType corelib "System" "Single"
        let double = findCorelibType corelib "System" "Double"
        let delegateType = findCorelibType corelib "System" "Delegate"
        let runtimeMethodHandleType = findCorelibType corelib "System" "RuntimeMethodHandle"
        let runtimeTypeHandleType = findCorelibType corelib "System" "RuntimeTypeHandle"
        let runtimeTypeType = findCorelibType corelib "System" "RuntimeType"
        let runtimeFieldHandleType = findCorelibType corelib "System" "RuntimeFieldHandle"
        let voidType = findCorelibType corelib "System" "Void"
        let typedReferenceType = findCorelibType corelib "System" "TypedReference"
        let intPtrType = findCorelibType corelib "System" "IntPtr"
        let uintPtrType = findCorelibType corelib "System" "UIntPtr"

        let byReferenceType =
            tryFindCorelibType corelib "System" [ "ByReference" ; "ByReference`1" ]

        let runtimeFieldInfoStubType =
            findCorelibType corelib "System" "RuntimeFieldInfoStub"

        let runtimeFieldHandleInternalType =
            findCorelibType corelib "System" "RuntimeFieldHandleInternal"

        let exceptionType = findCorelibType corelib "System" "Exception"
        let arithmeticException = findCorelibType corelib "System" "ArithmeticException"
        let divideByZeroException = findCorelibType corelib "System" "DivideByZeroException"
        let overflowException = findCorelibType corelib "System" "OverflowException"

        let stackOverflowException =
            findCorelibType corelib "System" "StackOverflowException"

        let typeLoadException = findCorelibType corelib "System" "TypeLoadException"

        let typeInitializationException =
            findCorelibType corelib "System" "TypeInitializationException"

        let indexOutOfRangeException =
            findCorelibType corelib "System" "IndexOutOfRangeException"

        let invalidCastException = findCorelibType corelib "System" "InvalidCastException"
        let missingFieldException = findCorelibType corelib "System" "MissingFieldException"

        let missingMethodException =
            findCorelibType corelib "System" "MissingMethodException"

        let nullReferenceException =
            findCorelibType corelib "System" "NullReferenceException"

        let outOfMemoryException = findCorelibType corelib "System" "OutOfMemoryException"
        let argumentException = findCorelibType corelib "System" "ArgumentException"
        let argumentNullException = findCorelibType corelib "System" "ArgumentNullException"

        {
            Corelib = corelib
            String = stringType
            Boolean = boolean
            Char = char
            SByte = sbyte
            Byte = byte
            Int16 = int16
            UInt16 = uint16
            Int32 = int32
            UInt32 = uint32
            Int64 = int64
            UInt64 = uint64
            Single = single
            Double = double
            Array = arrayType
            Enum = enumType
            ValueType = valueType
            DelegateType = delegateType
            Object = objType
            RuntimeTypeHandle = runtimeTypeHandleType
            RuntimeMethodHandle = runtimeMethodHandleType
            RuntimeFieldHandle = runtimeFieldHandleType
            RuntimeFieldInfoStub = runtimeFieldInfoStubType
            RuntimeFieldHandleInternal = runtimeFieldHandleInternalType
            RuntimeType = runtimeTypeType
            Void = voidType
            TypedReference = typedReferenceType
            IntPtr = intPtrType
            UIntPtr = uintPtrType
            ByReference = byReferenceType
            Exception = exceptionType
            ArithmeticException = arithmeticException
            DivideByZeroException = divideByZeroException
            OverflowException = overflowException
            StackOverflowException = stackOverflowException
            TypeLoadException = typeLoadException
            TypeInitializationException = typeInitializationException
            IndexOutOfRangeException = indexOutOfRangeException
            InvalidCastException = invalidCastException
            MissingFieldException = missingFieldException
            MissingMethodException = missingMethodException
            NullReferenceException = nullReferenceException
            OutOfMemoryException = outOfMemoryException
            ArgumentException = argumentException
            ArgumentNullException = argumentNullException
        }

    let concretizeAll
        (loaded : ImmutableDictionary<string, DumpedAssembly>)
        (bct : BaseClassTypes<DumpedAssembly>)
        (t : AllConcreteTypes)
        : AllConcreteTypes
        =
        let ctx =
            {
                TypeConcretization.ConcretizationContext.ConcreteTypes = t
                TypeConcretization.ConcretizationContext.LoadedAssemblies = loaded
                TypeConcretization.ConcretizationContext.BaseTypes = bct
            }

        let loader =
            { new IAssemblyLoad with
                member _.LoadAssembly _ _ _ =
                    failwith "should have already loaded this assembly"
            }

        let tys =
            [
                bct.String
                bct.Boolean
                bct.Char
                bct.SByte
                bct.Byte
                bct.Int16
                bct.UInt16
                bct.Int32
                bct.UInt32
                bct.Int64
                bct.UInt64
                bct.Single
                bct.Double
                bct.Array
                bct.Enum
                bct.ValueType
                bct.DelegateType
                bct.Object
                bct.RuntimeTypeHandle
                bct.RuntimeMethodHandle
                bct.RuntimeFieldHandle
                bct.RuntimeFieldInfoStub
                bct.RuntimeFieldHandleInternal
                bct.RuntimeType
                bct.Void
                bct.TypedReference
                bct.IntPtr
                bct.UIntPtr
            ]

        (ctx, tys)
        ||> List.fold (fun ctx ty ->
            let stk = DumpedAssembly.signatureTypeKind ctx.BaseTypes ctx.LoadedAssemblies ty

            let _handle, ctx =
                TypeConcretization.concretizeType
                    ctx
                    loader
                    ty.Assembly
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (ty.Identity, stk))

            ctx
        )
        |> _.ConcreteTypes

/// How a primitive-like BCL struct flattens onto the eval stack.
///
/// Several BCL types are nominally `struct { single_field }` at metadata level (so `ldfld`,
/// reflection, and heap layout see a one-field struct), but the real CLR's JIT treats them
/// as if they were just the underlying primitive/reference. At the interpreter's eval-stack
/// boundary we mirror that: storage keeps the wrapped struct form; the stack sees the
/// flattened primitive form via the kind below.
[<RequireQualifiedAccess>]
type PrimitiveLikeKind =
    /// `System.IntPtr`, `System.UIntPtr` — flattens to `EvalStackValue.NativeInt`.
    | FlattenToNativeInt
    /// `System.RuntimeTypeHandle` (field `m_type : RuntimeType`),
    /// `System.RuntimeMethodHandle` (field `m_value : IRuntimeMethodInfo`),
    /// `System.RuntimeFieldHandle` (field `m_ptr : IRuntimeFieldInfo`) —
    /// flattens to `EvalStackValue.ObjectRef`. On CoreCLR these handles are ref-backed:
    /// `ldtoken` imports a managed reference, not a raw pointer.
    | FlattenToObjectRef
    /// `System.RuntimeFieldHandleInternal` (field `m_handle : IntPtr`) —
    /// flattens to a runtime-pointer-valued `EvalStackValue.NativeInt`.
    | FlattenToRuntimePointer
    /// `System.ByReference`/`System.ByReference<T>` — flattens to `EvalStackValue.ManagedPointer`.
    | FlattenToManagedPointer

[<RequireQualifiedAccess>]
module PrimitiveLikeStruct =
    /// Returns `Some kind` if the concrete type is one of the BCL structs whose storage form is a
    /// single-field wrapper but whose eval-stack form should be the underlying primitive/reference.
    /// Returns `None` for everything else, including user-defined single-field structs.
    let kind (bct : BaseClassTypes<DumpedAssembly>) (ct : ConcreteType<'a>) : PrimitiveLikeKind option =
        if not ct.Generics.IsEmpty then
            // Only ByReference<T> is generic; match it structurally below if present.
            match bct.ByReference with
            | Some br when ct.Identity = br.Identity -> Some PrimitiveLikeKind.FlattenToManagedPointer
            | _ -> None
        else
            let identity = ct.Identity

            if identity = bct.IntPtr.Identity then
                Some PrimitiveLikeKind.FlattenToNativeInt
            elif identity = bct.UIntPtr.Identity then
                Some PrimitiveLikeKind.FlattenToNativeInt
            elif identity = bct.RuntimeTypeHandle.Identity then
                Some PrimitiveLikeKind.FlattenToObjectRef
            elif identity = bct.RuntimeMethodHandle.Identity then
                Some PrimitiveLikeKind.FlattenToObjectRef
            elif identity = bct.RuntimeFieldHandle.Identity then
                Some PrimitiveLikeKind.FlattenToObjectRef
            elif identity = bct.RuntimeFieldHandleInternal.Identity then
                Some PrimitiveLikeKind.FlattenToRuntimePointer
            else
                match bct.ByReference with
                | Some br when identity = br.Identity -> Some PrimitiveLikeKind.FlattenToManagedPointer
                | _ -> None

    let isPrimitiveLike (bct : BaseClassTypes<DumpedAssembly>) (ct : ConcreteType<'a>) : bool =
        kind bct ct |> Option.isSome
