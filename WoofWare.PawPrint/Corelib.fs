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

        let indexOutOfRangeException =
            findCorelibType corelib "System" "IndexOutOfRangeException"

        let invalidCastException = findCorelibType corelib "System" "InvalidCastException"
        let missingFieldException = findCorelibType corelib "System" "MissingFieldException"

        let missingMethodException =
            findCorelibType corelib "System" "MissingMethodException"

        let nullReferenceException =
            findCorelibType corelib "System" "NullReferenceException"

        let outOfMemoryException = findCorelibType corelib "System" "OutOfMemoryException"

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
            Exception = exceptionType
            ArithmeticException = arithmeticException
            DivideByZeroException = divideByZeroException
            OverflowException = overflowException
            StackOverflowException = stackOverflowException
            TypeLoadException = typeLoadException
            IndexOutOfRangeException = indexOutOfRangeException
            InvalidCastException = invalidCastException
            MissingFieldException = missingFieldException
            MissingMethodException = missingMethodException
            NullReferenceException = nullReferenceException
            OutOfMemoryException = outOfMemoryException
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
            let stk =
                match DumpedAssembly.resolveBaseType ctx.BaseTypes ctx.LoadedAssemblies ty.Assembly ty.BaseType with
                | ResolvedBaseType.Enum
                | ResolvedBaseType.ValueType -> SignatureTypeKind.ValueType
                | ResolvedBaseType.Object
                | ResolvedBaseType.Delegate -> SignatureTypeKind.Class

            let _handle, ctx =
                TypeConcretization.concretizeType
                    ctx
                    loader
                    ty.Assembly
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (
                        ComparableTypeDefinitionHandle.Make ty.TypeDefHandle,
                        ty.Assembly.FullName,
                        stk
                    ))

            ctx
        )
        |> _.ConcreteTypes
