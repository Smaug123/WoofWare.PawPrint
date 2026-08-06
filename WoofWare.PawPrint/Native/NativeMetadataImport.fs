namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeMetadataImport =
    let private metadataTokenTypeCustomAttribute : int32 = 0x0c000000
    let private metadataTokenTypeFieldDef : int32 = 0x04000000
    let private metadataTokenTypeExportedType : int32 = 0x27000000
    let private metadataEnumSmallResultLimit : int = 16

    /// <c>mdTypeDefNil</c>: TypeDef table code (0x02) | row 0. Returned by
    /// <c>MetadataImport.GetParentToken</c> for top-level types (no NestedClass row).
    let private metadataTypeDefNil : int32 = 0x02000000

    let private metadataReaderOf (assembly : DumpedAssembly) : System.Reflection.Metadata.MetadataReader =
        System.Reflection.Metadata.PEReaderExtensions.GetMetadataReader assembly.PeReader

    let private metadataTokenOfFieldDefinitionHandle
        (fieldHandle : System.Reflection.Metadata.FieldDefinitionHandle)
        : int32
        =
        let fieldHandle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit fieldHandle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken fieldHandle

    let private metadataImportHandleOfArg (operation : string) (arg : CliType) : string =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MetadataImportHandle assemblyFullName)) ->
            assemblyFullName
        | other -> failwith $"%s{operation}: expected MetadataImportHandle argument, got %O{other}"

    let private metadataImportAssembly
        (operation : string)
        (state : IlMachineState)
        (assemblyFullName : string)
        : DumpedAssembly
        =
        state.LoadedAssembly' assemblyFullName
        |> Option.defaultWith (fun () ->
            failwith $"%s{operation}: metadata import assembly is not loaded: %s{assemblyFullName}"
        )

    let private writeInt32AtPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : int32)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase baseClassTypes state ptr (CliType.Numeric (CliNumericType.Int32 value))

    let private int32BufferElementPointer
        (operation : string)
        (buffer : ManagedPointerSource)
        (index : int)
        : ManagedPointerSource option
        =
        match buffer with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex), []) ->
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex + index), [])
            |> Some
        | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
            ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset + (index * 4)), [])
            |> Some
        | ManagedPointerSource.Null -> failwith $"%s{operation}: expected non-null Int32 result buffer"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot use fake non-null byref @ 0x%x{bits} as Int32 result buffer; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ -> None

    let private tryWriteSmallInt32Buffer
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (buffer : ManagedPointerSource)
        (values : int32 list)
        : IlMachineState option
        =
        if values.Length > metadataEnumSmallResultLimit then
            None
        else
            let mutable state = state
            let mutable index = 0
            let mutable canWrite = true

            while canWrite && index < values.Length do
                match int32BufferElementPointer operation buffer index with
                | Some ptr ->
                    state <- writeInt32AtPointer baseClassTypes state ptr values.[index]
                    index <- index + 1
                | None -> canWrite <- false

            if canWrite then Some state else None

    let private allocateInt32Array
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (values : int32 list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                values.Length
                state

        let state =
            ((state, 0), values)
            ||> List.fold (fun (state, index) value ->
                IlMachineState.setArrayValue arrayAddr (CliType.Numeric (CliNumericType.Int32 value)) index state,
                index + 1
            )
            |> fst

        arrayAddr, state

    let private moduleHandleOfRuntimeModuleRef
        (operation : string)
        (state : IlMachineState)
        (runtimeModuleRef : EvalStackValue)
        : string
        =
        let runtimeModuleAddr =
            match runtimeModuleRef with
            | EvalStackValue.ObjectRef addr -> addr
            | other -> failwith $"%s{operation}: expected ObjectRef for RuntimeModule argument, got %O{other}"

        let heapObj = ManagedHeap.get runtimeModuleAddr state.ManagedHeap

        let pDataField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_pData"

        match
            AllocatedNonArrayObject.DereferenceFieldById pDataField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)) -> assemblyFullName
        | other -> failwith $"%s{operation}: expected ModuleHandle in RuntimeModule.m_pData, got %O{other}"

    let private typeDefinitionNamespace
        (operation : string)
        (state : IlMachineState)
        (assemblyFullName : string)
        (mdToken : int32)
        : string
        =
        let assembly = metadataImportAssembly operation state assemblyFullName

        match MetadataToken.ofInt mdToken with
        | MetadataToken.TypeDefinition typeDefHandle ->
            let mutable typeInfo =
                Unchecked.defaultof<TypeInfo<GenericParamFromMetadata, TypeDefn>>

            if assembly.TypeDefs.TryGetValue (typeDefHandle, &typeInfo) then
                typeInfo.Namespace
            else
                failwith $"%s{operation}: TypeDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
        | token ->
            failwith
                $"%s{operation}: expected TypeDef token for MetadataImport.GetNamespace, got %O{token} from 0x%08x{mdToken}"

    let private fieldDefinitionsForTypeDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (parent : int32)
        : int32 list
        =
        match MetadataToken.ofInt parent with
        | MetadataToken.TypeDefinition typeDefHandle ->
            let mutable typeInfo =
                Unchecked.defaultof<TypeInfo<GenericParamFromMetadata, TypeDefn>>

            if assembly.TypeDefs.TryGetValue (typeDefHandle, &typeInfo) then
                typeInfo.Fields
                |> List.map (fun field -> metadataTokenOfFieldDefinitionHandle field.Handle)
            else
                failwith $"%s{operation}: TypeDef token 0x%08x{parent} was not present in %s{assembly.Name.FullName}"
        | token ->
            failwith
                $"%s{operation}: expected TypeDef parent token for FieldDef enumeration, got %O{token} from 0x%08x{parent}"

    let private fieldDefinition
        (operation : string)
        (assembly : DumpedAssembly)
        (mdToken : int32)
        : FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
        match MetadataToken.ofInt mdToken with
        | MetadataToken.FieldDefinition fieldDefHandle ->
            let mutable fieldInfo =
                Unchecked.defaultof<FieldInfo<GenericParamFromMetadata, TypeDefn>>

            if assembly.Fields.TryGetValue (fieldDefHandle, &fieldInfo) then
                fieldInfo
            else
                failwith $"%s{operation}: FieldDef token 0x%08x{mdToken} was not present in %s{assembly.Name.FullName}"
        | token -> failwith $"%s{operation}: expected FieldDef token, got %O{token} from 0x%08x{mdToken}"

    /// Walk the assembly's methods to find which one owns <paramref name="paramHandle"/>.
    /// CLI metadata exposes the param->method relation only via per-method ranges, so
    /// answering "who owns this Param row?" requires iterating method definitions.
    let private methodOwningParameter
        (assembly : DumpedAssembly)
        (paramHandle : System.Reflection.Metadata.ParameterHandle)
        : System.Reflection.Metadata.MethodDefinitionHandle option
        =
        let mr = metadataReaderOf assembly

        assembly.Methods.Keys
        |> Seq.tryFind (fun methodHandle ->
            let methodDef = mr.GetMethodDefinition methodHandle
            let parameters = methodDef.GetParameters ()
            let mutable enumerator = parameters.GetEnumerator ()
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if enumerator.Current = paramHandle then
                    found <- true

            found
        )

    /// Walk the assembly's TypeDefs to find which one owns <paramref name="eventHandle"/>.
    let private typeOwningEvent
        (assembly : DumpedAssembly)
        (eventHandle : System.Reflection.Metadata.EventDefinitionHandle)
        : System.Reflection.Metadata.TypeDefinitionHandle option
        =
        let mr = metadataReaderOf assembly

        assembly.TypeDefs.Keys
        |> Seq.tryFind (fun typeHandle ->
            let typeDef = mr.GetTypeDefinition typeHandle
            let events = typeDef.GetEvents ()
            let mutable enumerator = events.GetEnumerator ()
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if enumerator.Current = eventHandle then
                    found <- true

            found
        )

    /// Walk the assembly's TypeDefs to find which one owns <paramref name="propertyHandle"/>.
    let private typeOwningProperty
        (assembly : DumpedAssembly)
        (propertyHandle : System.Reflection.Metadata.PropertyDefinitionHandle)
        : System.Reflection.Metadata.TypeDefinitionHandle option
        =
        let mr = metadataReaderOf assembly

        assembly.TypeDefs.Keys
        |> Seq.tryFind (fun typeHandle ->
            let typeDef = mr.GetTypeDefinition typeHandle
            let properties = typeDef.GetProperties ()
            let mutable enumerator = properties.GetEnumerator ()
            let mutable found = false

            while not found && enumerator.MoveNext () do
                if enumerator.Current = propertyHandle then
                    found <- true

            found
        )

    /// Resolve the <c>System.Reflection.ConstArray</c> TypeInfo from the loaded corelib.
    /// <c>ConstArray</c> is the shape returned by several <c>MetadataImport</c> InternalCalls
    /// (<c>GetCustomAttributeProps</c>, <c>GetMemberRefProps</c>, <c>GetSigOfMethodDef</c>, …).
    let private constArrayTypeInfo
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        match baseClassTypes.Corelib.TryGetTopLevelTypeDef "System.Reflection" "ConstArray" with
        | Some ty -> ty
        | None ->
            failwith
                $"%s{operation}: System.Reflection.ConstArray was not found in corelib %s{baseClassTypes.Corelib.Name.FullName}"

    /// Concretize <c>System.Reflection.ConstArray</c> and return its handle (cached implicitly via
    /// <c>AllConcreteTypes</c>).
    let private concretizeConstArray
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let typeInfo = constArrayTypeInfo operation baseClassTypes

        let state, handle =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition baseClassTypes.Corelib.Name typeInfo.TypeDefHandle,
                System.Reflection.Metadata.SignatureTypeKind.ValueType
            )
            |> IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        state, handle, typeInfo

    /// Build a <c>System.Reflection.ConstArray</c> value with <c>m_length = blob.Length</c> and
    /// <c>m_constArray</c> pointing at the first byte of <paramref name="blob"/>. Allocates the
    /// backing managed <c>byte[]</c> on the heap.
    let private buildConstArray
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (blob : ImmutableArray<byte>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, constArrayHandle, typeInfo =
            concretizeConstArray loggerFactory baseClassTypes operation state

        let lengthFieldInfo = FieldIdentity.requiredOwnInstanceField typeInfo "m_length"

        let pointerFieldInfo =
            FieldIdentity.requiredOwnInstanceField typeInfo "m_constArray"

        match lengthFieldInfo.Signature with
        | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
        | s -> failwith $"%s{operation}: ConstArray.m_length had unexpected signature %O{s}"

        match pointerFieldInfo.Signature with
        | TypeDefn.PrimitiveType PrimitiveType.IntPtr -> ()
        | s -> failwith $"%s{operation}: ConstArray.m_constArray had unexpected signature %O{s}"

        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let intPtrHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.IntPtr

        let storage : byte array = Array.init blob.Length (fun i -> blob.[i])

        let pointerValue, state =
            if storage.Length = 0 then
                CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null), state
            else
                let bytePtr, state = NativeCall.allocateBlobByteArray baseClassTypes storage state
                CliType.RuntimePointer (CliRuntimePointer.Managed bytePtr), state

        let lengthField =
            FieldIdentity.cliField
                constArrayHandle
                lengthFieldInfo
                (CliType.Numeric (CliNumericType.Int32 storage.Length))
                int32Handle

        let pointerField =
            FieldIdentity.cliField constArrayHandle pointerFieldInfo pointerValue intPtrHandle

        let valueType =
            [ lengthField ; pointerField ]
            |> CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                constArrayHandle
                typeInfo.Layout
                (CharSetMetadata.ofTypeAttributes typeInfo.TypeAttributes)
            |> CliType.ValueType

        valueType, state

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        // The executing-method name is deliberately omitted from the match. CoreLib declares
        // `MetadataImport.Enum` with `[LibraryImport(RuntimeHelpers.QCall, EntryPoint =
        // "MetadataImport_Enum")]`, so Roslyn emits a marshalling stub whose synthesised
        // name (`<Enum>g____PInvoke|N_M`) carries source-generator counters that drift
        // whenever neighbouring members are reordered. The entry-point name plus the
        // parameter/return signature shape are stable and disambiguate the QCall on their
        // own. Same approach as `NativeCustomAttribute.tryExecuteQCall`.
        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "MetadataImport_Enum",
          "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when objectHandleGenerics.IsEmpty ->
            let operation = "MetadataImport.Enum"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]

            let assembly = metadataImportAssembly operation state assemblyFullName

            let tokenType =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 tokenType) -> tokenType
                | other -> failwith $"%s{operation}: expected Int32 token type argument, got %O{other}"

            let parent =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[2] with
                | CliType.Numeric (CliNumericType.Int32 parent) -> parent
                | other -> failwith $"%s{operation}: expected Int32 parent token argument, got %O{other}"

            let values =
                if tokenType = metadataTokenTypeExportedType && parent = 0 then
                    []
                elif tokenType = metadataTokenTypeCustomAttribute then
                    match assembly.CustomAttributesByParentToken.TryGetValue parent with
                    | true, tokens -> tokens |> Seq.toList
                    | false, _ -> []
                elif tokenType = metadataTokenTypeFieldDef then
                    fieldDefinitionsForTypeDefinition operation assembly parent
                else
                    failwith
                        $"TODO: %s{operation} does not yet support token type 0x%08x{tokenType} with parent 0x%08x{parent}"

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length" instruction.Arguments.[3]

            let state =
                if values.IsEmpty then
                    state
                else
                    let shortResult =
                        NativeCall.managedPointerOfPointerArgument operation "shortResult" instruction.Arguments.[4]

                    match tryWriteSmallInt32Buffer operation ctx.BaseClassTypes state shortResult values with
                    | Some state -> state
                    | None ->
                        // Some fixed inline-array byrefs are not yet addressable as Int32 spans in PawPrint.
                        // The CoreLib wrapper checks _largeResult before reading _smallResult, so using the
                        // large-result escape hatch preserves the managed contract for those shapes.
                        let longResult =
                            NativeCall.objectHandleOnStackTarget operation state "longResult" instruction.Arguments.[5]

                        let resultArrayAddr, state = allocateInt32Array ctx.BaseClassTypes values state

                        IlMachineState.writeManagedByrefWithBase
                            ctx.BaseClassTypes
                            state
                            longResult
                            (CliType.ObjectRef (Some resultArrayAddr))

            let state = writeInt32AtPointer ctx.BaseClassTypes state lengthOut values.Length

            NativeHandlerResult.completed state |> Some
        | _ -> None

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetMetadataImport",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Reflection",
                                              "RuntimeModule",
                                              runtimeModuleGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) when
            runtimeModuleGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetMetadataImport"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeModuleRef, state = IlMachineState.popEvalStack ctx.Thread state

            let assemblyFullName =
                moduleHandleOfRuntimeModuleRef operation state runtimeModuleRef

            // CoreCLR returns an IMDInternalImport pointer distinct from RuntimeModule.m_pData.
            // PawPrint preserves that handle-domain split while using the same module identity payload.
            let state =
                IlMachineState.pushToEvalStack'
                    (EvalStackValue.NativeInt (NativeIntSource.MetadataImportHandle assemblyFullName))
                    ctx.Thread
                    state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetNamespace",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetNamespace"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let namespaceOut =
                NativeCall.managedPointerOfPointerArgument operation "namespace out pointer" instruction.Arguments.[2]

            let namespaceName = typeDefinitionNamespace operation state assemblyFullName mdToken

            let namespacePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes namespaceName state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    namespaceOut
                    (CliType.RuntimePointer (CliRuntimePointer.Managed namespacePtr))

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetFieldDefProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetFieldDefProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "fieldAttributes out pointer"
                    instruction.Arguments.[2]

            let field = fieldDefinition operation assembly mdToken

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state attributesOut (int32 field.Attributes)

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetCustomAttributeProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32)
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetCustomAttributeProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 customAttributeToken argument, got %O{other}"

            let ctorTokenOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "constructorToken out pointer"
                    instruction.Arguments.[2]

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[3]

            let attrHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.CustomAttribute h -> h
                | token -> failwith $"%s{operation}: expected CustomAttribute token, got %O{token} from 0x%08x{mdToken}"

            let mutable attr = Unchecked.defaultof<WoofWare.PawPrint.CustomAttribute>

            let attr =
                if assembly.Attributes.TryGetValue (attrHandle, &attr) then
                    attr
                else
                    failwith
                        $"%s{operation}: CustomAttribute token 0x%08x{mdToken} was not present in %s{assemblyFullName}"

            let state =
                writeInt32AtPointer ctx.BaseClassTypes state ctorTokenOut (MetadataToken.toInt attr.Constructor)

            let constArrayValue, state =
                buildConstArray ctx.LoggerFactory ctx.BaseClassTypes operation attr.Value state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetSigOfMethodDef",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetSigOfMethodDef"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 methodToken argument, got %O{other}"

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[2]

            let methodDefHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.MethodDef h -> h
                | token -> failwith $"%s{operation}: expected MethodDef token, got %O{token} from 0x%08x{mdToken}"

            let mutable methodInfo =
                Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

            if not (assembly.Methods.TryGetValue (methodDefHandle, &methodInfo)) then
                failwith $"%s{operation}: MethodDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"

            // The MetadataImport.GetSigOfMethodDef contract is "raw signature blob bytes
            // for the supplied MethodDef token". PawPrint's MethodInfo decodes the signature
            // eagerly; the unparsed blob is recovered on demand from the metadata reader.
            let mr = metadataReaderOf assembly
            let methodDef = mr.GetMethodDefinition methodDefHandle
            let blob = ImmutableArray.Create<byte> (mr.GetBlobBytes methodDef.Signature)

            let constArrayValue, state =
                buildConstArray ctx.LoggerFactory ctx.BaseClassTypes operation blob state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetMemberRefProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System.Reflection",
                                                             "ConstArray",
                                                             constArrayGenerics)) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            constArrayGenerics.IsEmpty
            ->
            let operation = "MetadataImport.GetMemberRefProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 memberTokenRef argument, got %O{other}"

            let signatureOut =
                NativeCall.managedPointerOfPointerArgument operation "signature out pointer" instruction.Arguments.[2]

            let memberRefHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.MemberReference h -> h
                | token -> failwith $"%s{operation}: expected MemberRef token, got %O{token} from 0x%08x{mdToken}"

            if not (assembly.Members.ContainsKey memberRefHandle) then
                failwith $"%s{operation}: MemberRef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"

            // CoreCLR's FCall forwards to `IMDInternalImport::GetNameAndSigOfMemberRef` and discards
            // the name, so the contract is "raw signature blob bytes for the supplied MemberRef
            // token". PawPrint decodes MemberRef signatures eagerly into `MemberSignature`; the
            // unparsed blob is recovered on demand from the metadata reader, as for GetSigOfMethodDef.
            let mr = metadataReaderOf assembly
            let memberRef = mr.GetMemberReference memberRefHandle
            let blob = ImmutableArray.Create<byte> (mr.GetBlobBytes memberRef.Signature)

            let constArrayValue, state =
                buildConstArray ctx.LoggerFactory ctx.BaseClassTypes operation blob state

            let state =
                IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state signatureOut constArrayValue

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetParentToken",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetParentToken"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let parentOut =
                NativeCall.managedPointerOfPointerArgument
                    operation
                    "parent token out pointer"
                    instruction.Arguments.[2]

            let parentToken =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.TypeDefinition typeDefHandle ->
                    let mutable typeInfo =
                        Unchecked.defaultof<TypeInfo<GenericParamFromMetadata, TypeDefn>>

                    if assembly.TypeDefs.TryGetValue (typeDefHandle, &typeInfo) then
                        if typeInfo.DeclaringType.IsNil then
                            metadataTypeDefNil
                        else
                            let parentHandle : System.Reflection.Metadata.EntityHandle =
                                System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeInfo.DeclaringType

                            System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    else
                        failwith $"%s{operation}: TypeDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.MethodDef methodDefHandle ->
                    let mutable methodInfo =
                        Unchecked.defaultof<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>>

                    if assembly.Methods.TryGetValue (methodDefHandle, &methodInfo) then
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit
                                methodInfo.DeclaringType.Definition.Get

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    else
                        failwith
                            $"%s{operation}: MethodDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.FieldDefinition fieldDefHandle ->
                    let mutable fieldInfo =
                        Unchecked.defaultof<FieldInfo<GenericParamFromMetadata, TypeDefn>>

                    if assembly.Fields.TryGetValue (fieldDefHandle, &fieldInfo) then
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit
                                fieldInfo.DeclaringType.Definition.Get

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    else
                        failwith
                            $"%s{operation}: FieldDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.CustomAttribute attrHandle ->
                    let mutable attr = Unchecked.defaultof<WoofWare.PawPrint.CustomAttribute>

                    if assembly.Attributes.TryGetValue (attrHandle, &attr) then
                        MetadataToken.toInt attr.Parent
                    else
                        failwith
                            $"%s{operation}: CustomAttribute token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.MemberReference memberRefHandle ->
                    let mutable memberRef =
                        Unchecked.defaultof<WoofWare.PawPrint.MemberReference<MetadataToken>>

                    if assembly.Members.TryGetValue (memberRefHandle, &memberRef) then
                        MetadataToken.toInt memberRef.Parent
                    else
                        failwith
                            $"%s{operation}: MemberRef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.MethodSpecification methodSpecHandle ->
                    let mutable methodSpec = Unchecked.defaultof<WoofWare.PawPrint.MethodSpec>

                    if assembly.MethodSpecs.TryGetValue (methodSpecHandle, &methodSpec) then
                        MetadataToken.toInt methodSpec.Method
                    else
                        failwith
                            $"%s{operation}: MethodSpec token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | MetadataToken.GenericParameter genericParamHandle ->
                    let mr = metadataReaderOf assembly

                    let parent : System.Reflection.Metadata.EntityHandle =
                        (mr.GetGenericParameter genericParamHandle).Parent

                    if parent.IsNil then
                        failwith
                            $"%s{operation}: GenericParameter token 0x%08x{mdToken} has nil parent in %s{assemblyFullName}"
                    else
                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parent
                | MetadataToken.Parameter paramHandle ->
                    match methodOwningParameter assembly paramHandle with
                    | Some methodHandle ->
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.MethodDefinitionHandle.op_Implicit methodHandle

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    | None ->
                        failwith
                            $"%s{operation}: Parameter token 0x%08x{mdToken} had no owning method in %s{assemblyFullName}"
                | MetadataToken.EventDefinition eventHandle ->
                    match typeOwningEvent assembly eventHandle with
                    | Some typeHandle ->
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeHandle

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    | None ->
                        failwith
                            $"%s{operation}: Event token 0x%08x{mdToken} had no owning type in %s{assemblyFullName}"
                | MetadataToken.PropertyDefinition propertyHandle ->
                    match typeOwningProperty assembly propertyHandle with
                    | Some typeHandle ->
                        let parentHandle : System.Reflection.Metadata.EntityHandle =
                            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit typeHandle

                        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken parentHandle
                    | None ->
                        failwith
                            $"%s{operation}: Property token 0x%08x{mdToken} had no owning type in %s{assemblyFullName}"
                | token ->
                    failwith $"TODO: %s{operation} does not yet support token kind %O{token} for token 0x%08x{mdToken}"

            let state = writeInt32AtPointer ctx.BaseClassTypes state parentOut parentToken

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "GetGenericParamProps",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteByref (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "MetadataImport.GetGenericParamProps"
            let assemblyFullName = metadataImportHandleOfArg operation instruction.Arguments.[0]
            let assembly = metadataImportAssembly operation state assemblyFullName

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 genericParameter argument, got %O{other}"

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument operation "flags out pointer" instruction.Arguments.[2]

            let genericParamHandle =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.GenericParameter h -> h
                | token ->
                    failwith $"%s{operation}: expected GenericParameter token, got %O{token} from 0x%08x{mdToken}"

            let mr = metadataReaderOf assembly
            let genericParam = mr.GetGenericParameter genericParamHandle
            let flags = int genericParam.Attributes

            let state = writeInt32AtPointer ctx.BaseClassTypes state attributesOut flags

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None
