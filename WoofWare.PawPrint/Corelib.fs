namespace WoofWare.PawPrint

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
            RuntimeType = runtimeTypeType
            Void = voidType
            TypedReference = typedReferenceType
            IntPtr = intPtrType
            UIntPtr = uintPtrType
        }
