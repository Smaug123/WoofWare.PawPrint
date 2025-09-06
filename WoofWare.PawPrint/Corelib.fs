namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module Corelib =

    let getBaseTypes (corelib : DumpedAssembly) : BaseClassTypes<DumpedAssembly> =
        let stringType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "String" then Some v else None)
            |> Seq.exactlyOne

        let arrayType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Array" then Some v else None)
            |> Seq.exactlyOne

        let enumType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Enum" then Some v else None)
            |> Seq.exactlyOne

        let objType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Object" then Some v else None)
            |> Seq.exactlyOne

        let valueType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "ValueType" then Some v else None)
            |> Seq.exactlyOne

        let boolean =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Boolean" then Some v else None)
            |> Seq.exactlyOne

        let char =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Char" then Some v else None)
            |> Seq.exactlyOne

        let byte =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Byte" then Some v else None)
            |> Seq.exactlyOne

        let sbyte =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "SByte" then Some v else None)
            |> Seq.exactlyOne

        let int16 =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Int16" then Some v else None)
            |> Seq.exactlyOne

        let int32 =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Int32" then Some v else None)
            |> Seq.exactlyOne

        let int64 =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Int64" then Some v else None)
            |> Seq.exactlyOne

        let uint16 =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "UInt16" then Some v else None)
            |> Seq.exactlyOne

        let uint32 =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "UInt32" then Some v else None)
            |> Seq.exactlyOne

        let uint64 =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "UInt64" then Some v else None)
            |> Seq.exactlyOne

        let single =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Single" then Some v else None)
            |> Seq.exactlyOne

        let double =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Double" then Some v else None)
            |> Seq.exactlyOne

        let delegateType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Delegate" then Some v else None)
            |> Seq.exactlyOne

        let runtimeMethodHandleType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "RuntimeMethodHandle" then Some v else None)
            |> Seq.exactlyOne

        let runtimeTypeHandleType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "RuntimeTypeHandle" then Some v else None)
            |> Seq.exactlyOne

        let runtimeTypeType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "RuntimeType" then Some v else None)
            |> Seq.exactlyOne

        let runtimeFieldHandleType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "RuntimeFieldHandle" then Some v else None)
            |> Seq.exactlyOne

        let voidType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Void" then Some v else None)
            |> Seq.exactlyOne

        let typedReferenceType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "TypedReference" then Some v else None)
            |> Seq.exactlyOne

        let intPtrType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "IntPtr" then Some v else None)
            |> Seq.exactlyOne

        let uintPtrType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "UIntPtr" then Some v else None)
            |> Seq.exactlyOne

        let runtimeFieldInfoStubType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "RuntimeFieldInfoStub" then Some v else None)
            |> Seq.exactlyOne

        let runtimeFieldHandleInternalType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) ->
                if v.Name = "RuntimeFieldHandleInternal" then
                    Some v
                else
                    None
            )
            |> Seq.exactlyOne

        let exceptionType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Exception" then Some v else None)
            |> Seq.exactlyOne

        let arithmeticException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "ArithmeticException" then Some v else None)
            |> Seq.exactlyOne

        let divideByZeroException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "DivideByZeroException" then Some v else None)
            |> Seq.exactlyOne

        let overflowException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "OverflowException" then Some v else None)
            |> Seq.exactlyOne

        let stackOverflowException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "StackOverflowException" then Some v else None)
            |> Seq.exactlyOne

        let typeLoadException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "TypeLoadException" then Some v else None)
            |> Seq.exactlyOne

        let indexOutOfRangeException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "IndexOutOfRangeException" then Some v else None)
            |> Seq.exactlyOne

        let invalidCastException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "InvalidCastException" then Some v else None)
            |> Seq.exactlyOne

        let missingFieldException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "MissingFieldException" then Some v else None)
            |> Seq.exactlyOne

        let missingMethodException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "MissingMethodException" then Some v else None)
            |> Seq.exactlyOne

        let nullReferenceException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "NullReferenceException" then Some v else None)
            |> Seq.exactlyOne

        let outOfMemoryException =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "OutOfMemoryException" then Some v else None)
            |> Seq.exactlyOne

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
                TypeConcretization.ConcretizationContext.InProgress = ImmutableDictionary.Empty
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
