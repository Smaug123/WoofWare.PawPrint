namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

[<RequireQualifiedAccess>]
module BaseClassTypes =
    /// The CoreLib type a signature's primitive element type denotes: <c>ELEMENT_TYPE_I4</c> is
    /// <c>System.Int32</c>, <c>ELEMENT_TYPE_OBJECT</c> is <c>System.Object</c>, and so on.
    let ofPrimitive
        (baseClassTypes : BaseClassTypes<'corelib>)
        (primitive : PrimitiveType)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match primitive with
        | PrimitiveType.Boolean -> baseClassTypes.Boolean
        | PrimitiveType.Char -> baseClassTypes.Char
        | PrimitiveType.SByte -> baseClassTypes.SByte
        | PrimitiveType.Byte -> baseClassTypes.Byte
        | PrimitiveType.Int16 -> baseClassTypes.Int16
        | PrimitiveType.UInt16 -> baseClassTypes.UInt16
        | PrimitiveType.Int32 -> baseClassTypes.Int32
        | PrimitiveType.UInt32 -> baseClassTypes.UInt32
        | PrimitiveType.Int64 -> baseClassTypes.Int64
        | PrimitiveType.UInt64 -> baseClassTypes.UInt64
        | PrimitiveType.Single -> baseClassTypes.Single
        | PrimitiveType.Double -> baseClassTypes.Double
        | PrimitiveType.String -> baseClassTypes.String
        | PrimitiveType.Object -> baseClassTypes.Object
        | PrimitiveType.TypedReference -> baseClassTypes.TypedReference
        | PrimitiveType.IntPtr -> baseClassTypes.IntPtr
        | PrimitiveType.UIntPtr -> baseClassTypes.UIntPtr

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

    /// The CoreLib types that PawPrint and its analyses name directly, read from CoreLib's own image.
    let ofCorelib (corelib : DumpedAssembly) : BaseClassTypes<DumpedAssembly> =
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

        let multicastDelegateType = findCorelibType corelib "System" "MulticastDelegate"
        let runtimeMethodHandleType = findCorelibType corelib "System" "RuntimeMethodHandle"

        let runtimeMethodInfoStubType =
            findCorelibType corelib "System" "RuntimeMethodInfoStub"

        let dynamicMethodType =
            findCorelibType corelib "System.Reflection.Emit" "DynamicMethod"

        let varArgMethodType =
            findCorelibType corelib "System.Reflection.Emit" "VarArgMethod"

        let genericFieldInfoType =
            findCorelibType corelib "System.Reflection.Emit" "GenericFieldInfo"

        let runtimeMethodHandleInternalType =
            findCorelibType corelib "System" "RuntimeMethodHandleInternal"

        let runtimeTypeHandleType = findCorelibType corelib "System" "RuntimeTypeHandle"
        let runtimeTypeType = findCorelibType corelib "System" "RuntimeType"
        let runtimeFieldHandleType = findCorelibType corelib "System" "RuntimeFieldHandle"
        let voidType = findCorelibType corelib "System" "Void"
        let typedReferenceType = findCorelibType corelib "System" "TypedReference"
        let intPtrType = findCorelibType corelib "System" "IntPtr"
        let uintPtrType = findCorelibType corelib "System" "UIntPtr"

        let byReferenceType =
            tryFindCorelibType corelib "System" [ "ByReference" ; "ByReference`1" ]

        let nullableType = findCorelibType corelib "System" "Nullable`1"

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

        let arrayTypeMismatchException =
            findCorelibType corelib "System" "ArrayTypeMismatchException"

        let invalidProgramException =
            findCorelibType corelib "System" "InvalidProgramException"

        let badImageFormatException =
            findCorelibType corelib "System" "BadImageFormatException"

        let fileNotFoundException =
            findCorelibType corelib "System.IO" "FileNotFoundException"

        let argumentOutOfRangeException =
            findCorelibType corelib "System" "ArgumentOutOfRangeException"

        let missingFieldException = findCorelibType corelib "System" "MissingFieldException"

        let fieldAccessException = findCorelibType corelib "System" "FieldAccessException"

        let memberAccessException = findCorelibType corelib "System" "MemberAccessException"

        let missingMethodException =
            findCorelibType corelib "System" "MissingMethodException"

        let notSupportedException = findCorelibType corelib "System" "NotSupportedException"

        let invalidOperationException =
            findCorelibType corelib "System" "InvalidOperationException"

        let duplicateWaitObjectException =
            findCorelibType corelib "System" "DuplicateWaitObjectException"

        let verificationException =
            findCorelibType corelib "System.Security" "VerificationException"

        let ambiguousMatchException =
            findCorelibType corelib "System.Reflection" "AmbiguousMatchException"

        let entryPointNotFoundException =
            findCorelibType corelib "System" "EntryPointNotFoundException"

        let nullReferenceException =
            findCorelibType corelib "System" "NullReferenceException"

        let outOfMemoryException = findCorelibType corelib "System" "OutOfMemoryException"
        let argumentException = findCorelibType corelib "System" "ArgumentException"
        let argumentNullException = findCorelibType corelib "System" "ArgumentNullException"
        let dateTime = findCorelibType corelib "System" "DateTime"
        let decimal = findCorelibType corelib "System" "Decimal"

        let targetInvocationException =
            findCorelibType corelib "System.Reflection" "TargetInvocationException"

        // The five interfaces in CoreCLR's `IsImplicitInterfaceOfSZArray` set.
        // Their open-generic TypeDef rows live in System.Collections.Generic in
        // the corelib; the metadata Name carries the backtick-arity suffix.
        let iListGeneric = findCorelibType corelib "System.Collections.Generic" "IList`1"

        let iEnumerableGeneric =
            findCorelibType corelib "System.Collections.Generic" "IEnumerable`1"

        let iCollectionGeneric =
            findCorelibType corelib "System.Collections.Generic" "ICollection`1"

        let iReadOnlyListGeneric =
            findCorelibType corelib "System.Collections.Generic" "IReadOnlyList`1"

        let iReadOnlyCollectionGeneric =
            findCorelibType corelib "System.Collections.Generic" "IReadOnlyCollection`1"

        // The shim class supplying the bodies for those five interfaces on an SZ array.
        // Internal to the corelib, so it is only reachable via the dispatch carve-out.
        let szArrayHelper = findCorelibType corelib "System" "SZArrayHelper"

        // Consulted by the object-cast slow path to refuse a cast CoreCLR would resolve by
        // calling back into managed code. See the field's doc comment on `BaseClassTypes`.
        let iDynamicInterfaceCastable =
            findCorelibType corelib "System.Runtime.InteropServices" "IDynamicInterfaceCastable"

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
            MulticastDelegateType = multicastDelegateType
            Object = objType
            RuntimeTypeHandle = runtimeTypeHandleType
            RuntimeMethodHandle = runtimeMethodHandleType
            RuntimeMethodInfoStub = runtimeMethodInfoStubType
            DynamicMethod = dynamicMethodType
            VarArgMethod = varArgMethodType
            GenericFieldInfo = genericFieldInfoType
            RuntimeMethodHandleInternal = runtimeMethodHandleInternalType
            RuntimeFieldHandle = runtimeFieldHandleType
            RuntimeFieldInfoStub = runtimeFieldInfoStubType
            RuntimeFieldHandleInternal = runtimeFieldHandleInternalType
            RuntimeType = runtimeTypeType
            Void = voidType
            TypedReference = typedReferenceType
            IntPtr = intPtrType
            UIntPtr = uintPtrType
            ByReference = byReferenceType
            Nullable = nullableType
            Exception = exceptionType
            ArithmeticException = arithmeticException
            DivideByZeroException = divideByZeroException
            OverflowException = overflowException
            StackOverflowException = stackOverflowException
            TypeLoadException = typeLoadException
            TypeInitializationException = typeInitializationException
            IndexOutOfRangeException = indexOutOfRangeException
            InvalidCastException = invalidCastException
            ArrayTypeMismatchException = arrayTypeMismatchException
            InvalidProgramException = invalidProgramException
            BadImageFormatException = badImageFormatException
            FileNotFoundException = fileNotFoundException
            ArgumentOutOfRangeException = argumentOutOfRangeException
            MissingFieldException = missingFieldException
            FieldAccessException = fieldAccessException
            MemberAccessException = memberAccessException
            MissingMethodException = missingMethodException
            NotSupportedException = notSupportedException
            InvalidOperationException = invalidOperationException
            DuplicateWaitObjectException = duplicateWaitObjectException
            VerificationException = verificationException
            AmbiguousMatchException = ambiguousMatchException
            EntryPointNotFoundException = entryPointNotFoundException
            NullReferenceException = nullReferenceException
            OutOfMemoryException = outOfMemoryException
            ArgumentException = argumentException
            ArgumentNullException = argumentNullException
            DateTime = dateTime
            Decimal = decimal
            TargetInvocationException = targetInvocationException
            IListGeneric = iListGeneric
            IEnumerableGeneric = iEnumerableGeneric
            ICollectionGeneric = iCollectionGeneric
            IReadOnlyListGeneric = iReadOnlyListGeneric
            IReadOnlyCollectionGeneric = iReadOnlyCollectionGeneric
            SZArrayHelper = szArrayHelper
            IDynamicInterfaceCastable = iDynamicInterfaceCastable
        }
