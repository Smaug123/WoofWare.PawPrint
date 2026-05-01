namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeMetadataImport =
    let private metadataTokenTypeCustomAttribute : int32 = 0x0c000000
    let private mdAssemblyToken : int32 = 0x20000001
    let private metadataTokenTypeFieldDef : int32 = 0x04000000
    let private metadataTokenTypeExportedType : int32 = 0x27000000

    let private metadataImportHandleOfArg (operation : string) (arg : CliType) : string =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MetadataImportHandle assemblyFullName)) ->
            assemblyFullName
        | other -> failwith $"%s{operation}: expected MetadataImportHandle argument, got %O{other}"

    let private fieldDefinitionToken (handle : System.Reflection.Metadata.FieldDefinitionHandle) : int32 =
        let handle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.FieldDefinitionHandle.op_Implicit handle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

    let private writeInt32AtPointer
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (value : int)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefBytes state ptr (CliType.Numeric (CliNumericType.Int32 value))

    let private allocateInt32Array
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (values : int list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let intHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero intHandle)
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
        let assembly =
            state.LoadedAssembly' assemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: metadata import assembly is not loaded: %s{assemblyFullName}"
            )

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

    let private allocateNullTerminatedUtf8
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (value : string)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        let bytes = System.Text.Encoding.UTF8.GetBytes value
        let storage = Array.zeroCreate<byte> (bytes.Length + 1)
        Array.blit bytes 0 storage 0 bytes.Length

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero byteHandle)
                (fun () -> CliType.Numeric (CliNumericType.UInt8 0uy))
                storage.Length
                state

        let state =
            ((state, 0), storage)
            ||> Array.fold (fun (state, index) b ->
                IlMachineState.setArrayValue arrayAddr (CliType.Numeric (CliNumericType.UInt8 b)) index state,
                index + 1
            )
            |> fst

        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    let tryExecute (ctx : NativeCallContext) : ExecutionResult option =
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

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "MetadataImport",
          "<Enum>g____PInvoke|8_0",
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

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: metadata import assembly is not loaded: %s{assemblyFullName}"
                )

            let tokenType =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 tokenType) -> tokenType
                | other -> failwith $"%s{operation}: expected Int32 token type argument, got %O{other}"

            let parent =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[2] with
                | CliType.Numeric (CliNumericType.Int32 parent) -> parent
                | other -> failwith $"%s{operation}: expected Int32 parent token argument, got %O{other}"

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length" instruction.Arguments.[3]

            let longResult =
                NativeCall.objectHandleOnStackTarget operation state "longResult" instruction.Arguments.[5]

            let values =
                if
                    (tokenType = metadataTokenTypeExportedType && parent = 0)
                    || (tokenType = metadataTokenTypeCustomAttribute && parent = mdAssemblyToken)
                then
                    []
                elif tokenType = metadataTokenTypeFieldDef then
                    match MetadataToken.ofInt parent with
                    | MetadataToken.TypeDefinition typeDefHandle ->
                        let typeInfo =
                            match assembly.TypeDefs.TryGetValue typeDefHandle with
                            | true, typeInfo -> typeInfo
                            | false, _ ->
                                failwith
                                    $"%s{operation}: parent TypeDef token 0x%08x{parent} was not present in %s{assemblyFullName}"

                        typeInfo.Fields |> List.map (fun field -> fieldDefinitionToken field.Handle)
                    | token ->
                        failwith
                            $"%s{operation}: FieldDef enumeration expected TypeDef parent, got %O{token} from 0x%08x{parent}"
                else
                    failwith
                        $"TODO: %s{operation} only supports empty ExportedType enumeration, empty assembly CustomAttribute enumeration, and FieldDef by TypeDef; got token type 0x%08x{tokenType}, parent 0x%08x{parent}"

            let state =
                if values.IsEmpty then
                    state
                else
                    // The CoreCLR native helper only allocates this array when the short
                    // inline buffer is too small. PawPrint returns non-empty results through
                    // the large-result path so managed reflection does not have to reinterpret
                    // InlineArray struct storage as raw Int32 bytes.
                    let arrayAddr, state = allocateInt32Array ctx.BaseClassTypes values state

                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        longResult
                        (CliType.ObjectRef (Some arrayAddr))

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    lengthOut
                    (CliType.Numeric (CliNumericType.Int32 values.Length))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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
                allocateNullTerminatedUtf8 ctx.BaseClassTypes namespaceName state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    namespaceOut
                    (CliType.RuntimePointer (CliRuntimePointer.Managed namespacePtr))

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
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

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: metadata import assembly is not loaded: %s{assemblyFullName}"
                )

            let mdToken =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 mdToken) -> mdToken
                | other -> failwith $"%s{operation}: expected Int32 mdToken argument, got %O{other}"

            let fieldInfo =
                match MetadataToken.ofInt mdToken with
                | MetadataToken.FieldDefinition fieldHandle ->
                    match assembly.Fields.TryGetValue fieldHandle with
                    | true, fieldInfo -> fieldInfo
                    | false, _ ->
                        failwith
                            $"%s{operation}: FieldDef token 0x%08x{mdToken} was not present in %s{assemblyFullName}"
                | token -> failwith $"%s{operation}: expected FieldDef token, got %O{token} from 0x%08x{mdToken}"

            let attributesOut =
                NativeCall.managedPointerOfPointerArgument operation "fieldAttributes" instruction.Arguments.[2]

            let state = writeInt32AtPointer state attributesOut (int32 fieldInfo.Attributes)

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.Int32 0) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
