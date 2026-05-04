namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module NativeRuntimeAssembly =
    open System.Collections.Immutable

    let private splitAtLastDot (name : string) : string * string =
        // CoreCLR's ns::FindSep walks back from the end and splits at the
        // final '.': everything before becomes the namespace, everything
        // after becomes the simple name. Names with no '.' are top-level
        // (empty namespace).
        let idx = name.LastIndexOf '.'

        if idx < 0 then
            "", name
        else
            name.Substring (0, idx), name.Substring (idx + 1)

    let private writeLength
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (lengthOut : ManagedPointerSource)
        (length : uint32)
        : IlMachineState
        =
        IlMachineState.writeManagedByrefWithBase ctx.BaseClassTypes state lengthOut (NativeCall.cliUInt32 length)

    let private assemblyHandleOfRuntimeAssemblyRef
        (operation : string)
        (state : IlMachineState)
        (runtimeAssemblyRef : EvalStackValue)
        : string
        =
        let runtimeAssemblyAddr =
            match runtimeAssemblyRef with
            | EvalStackValue.ObjectRef addr -> addr
            | EvalStackValue.NullObjectRef -> failwith $"TODO: %s{operation} on null RuntimeAssembly should throw NRE"
            | other -> failwith $"%s{operation}: expected ObjectRef for RuntimeAssembly argument, got %O{other}"

        let heapObj = ManagedHeap.get runtimeAssemblyAddr state.ManagedHeap

        let assemblyField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_assembly"

        match
            AllocatedNonArrayObject.DereferenceFieldById assemblyField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)) ->
            assemblyFullName
        | other -> failwith $"%s{operation}: expected AssemblyHandle in RuntimeAssembly.m_assembly, got %O{other}"

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
          "RuntimeAssembly",
          ("GetToken" | "GetTokenInternal"),
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Reflection",
                                              "RuntimeAssembly",
                                              runtimeAssemblyGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeAssemblyGenerics.IsEmpty
            ->
            let operation = "RuntimeAssembly.GetToken"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeAssemblyRef, state = IlMachineState.popEvalStack ctx.Thread state

            assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef |> ignore

            // Every assembly manifest has a single Assembly metadata row.
            let mdAssemblyToken = 0x20000001

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 mdAssemblyToken)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          "GetManifestModule",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Reflection",
                                              "RuntimeAssembly",
                                              runtimeAssemblyGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeModule",
                                                                      runtimeModuleGenerics)) when
            runtimeAssemblyGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            let operation = "RuntimeAssembly.GetManifestModule"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeAssemblyRef, state = IlMachineState.popEvalStack ctx.Thread state

            let assemblyFullName =
                assemblyHandleOfRuntimeAssemblyRef operation state runtimeAssemblyRef

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let runtimeModuleAddr, state =
                NativeRuntimeType.getOrAllocateRuntimeModule ctx.LoggerFactory ctx.BaseClassTypes assembly.Name state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some runtimeModuleAddr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "AssemblyNative_GetResource",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallAssembly",
                                              qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ],
          MethodReturnType.Returns (ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)) when
            qCallAssemblyGenerics.IsEmpty
            ->
            let operation = "AssemblyNative_GetResource"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let resourceNamePtr =
                NativeCall.managedPointerOfPointerArgument operation "resourceName" instruction.Arguments.[1]

            let lengthOut =
                NativeCall.managedPointerOfPointerArgument operation "length" instruction.Arguments.[2]

            let resourceName =
                NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state resourceNamePtr

            if resourceName.Length = 0 then
                failwith $"TODO: %s{operation} with empty resource name should throw ArgumentException"

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let state =
                match AssemblyApi.findManifestResource assembly resourceName with
                | ManifestResourceLookupResult.NotFound ->
                    let state = writeLength ctx state lengthOut 0u

                    IlMachineState.pushToEvalStack'
                        (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
                        ctx.Thread
                        state
                | ManifestResourceLookupResult.Embedded resource ->
                    let state = writeLength ctx state lengthOut (uint32 resource.PayloadLength)
                    let peByteRange = IlMachineState.peByteRangeForEmbeddedManifestResource resource

                    // Return a pointer even when PayloadLength is zero: null
                    // means "resource not found", while a zero-sized PE range
                    // means "resource exists and is empty".
                    let state, dataPtr =
                        IlMachineState.peByteRangePointer ctx.LoggerFactory ctx.BaseClassTypes peByteRange state

                    IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer dataPtr) ctx.Thread state
                | ManifestResourceLookupResult.ExternalFile resource ->
                    // Deliberately fail loudly until linked-file resources are
                    // implemented. CoreCLR returns null for manifest resources
                    // stored in separate files.
                    failwith
                        $"TODO: %s{operation} does not support external-file manifest resource %s{resource.Name} in %s{resource.AssemblyFullName} from %s{resource.FileName}"
                | ManifestResourceLookupResult.ReferencedAssembly (actualResourceName, assemblyReference) ->
                    // Deliberately fail loudly until forwarded resources are
                    // implemented. CoreCLR follows the AssemblyRef chain, as
                    // used by satellite/resource-carrier assemblies.
                    failwith
                        $"TODO: %s{operation} does not support assembly-forwarded manifest resource %s{actualResourceName} in %s{assemblyFullName} forwarded to %s{assemblyReference.Name.FullName}"

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "AssemblyNative_GetTypeCore",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeAssembly",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallAssembly",
                                              qCallAssemblyGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Byte)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Void when qCallAssemblyGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            let operation = "AssemblyNative_GetTypeCore"

            if instruction.Arguments.Length <> 5 then
                failwith $"%s{operation}: expected five native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                instruction.Arguments.[0]
                |> NativeCall.qCallAssemblyToAssemblyFullName operation state

            let typeNamePtr =
                NativeCall.managedPointerOfPointerArgument operation "typeName" instruction.Arguments.[1]

            let nestedNamesPtr =
                NativeCall.managedPointerOfPointerArgument operation "nestedTypeNames" instruction.Arguments.[2]

            let nestedCount = NativeCall.int32Argument operation instruction.Arguments.[3]

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "retType" instruction.Arguments.[4]

            if nestedCount < 0 then
                failwith $"%s{operation}: nested type count %d{nestedCount} is negative"

            match typeNamePtr with
            | ManagedPointerSource.Null ->
                failwith $"TODO: %s{operation} with null typeName should throw ArgumentNullException"
            | ManagedPointerSource.Byref _ -> ()

            if nestedCount > 0 then
                match nestedNamesPtr with
                | ManagedPointerSource.Null ->
                    failwith
                        $"%s{operation}: nestedTypeNames pointer was null but nestedCount=%d{nestedCount} (caller invariant violated)"
                | ManagedPointerSource.Byref _ -> ()

            let typeName =
                NativeCall.readNullTerminatedUtf8 operation ctx.BaseClassTypes state typeNamePtr

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            let nestedNames =
                if nestedCount = 0 then
                    []
                else
                    // sizeof<nativeint> matches CoreCLR's IntPtr ABI on the
                    // host. PawPrint's interpreter is a 64-bit-only host today.
                    let intPtrStride = sizeof<nativeint>

                    let byteConcreteType =
                        let h =
                            AllConcreteTypes.findExistingNonGenericConcreteType
                                state.ConcreteTypes
                                ctx.BaseClassTypes.Byte.Identity
                            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Byte is not concretized")

                        AllConcreteTypes.lookup h state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{operation}: concrete System.Byte handle %O{h} not found"
                        )

                    [
                        for i in 0 .. nestedCount - 1 do
                            let entryPtr =
                                ManagedPointerByteView.addByteOffset
                                    ctx.BaseClassTypes
                                    state
                                    byteConcreteType
                                    (i * intPtrStride)
                                    nestedNamesPtr

                            // Read an IntPtr-sized native int from the cell.
                            let entry =
                                IlMachineState.readManagedByrefBytesAs
                                    state
                                    entryPtr
                                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

                            let stringPtr =
                                NativeCall.managedPointerOfPointerArgument operation $"nestedTypeNames[{i}]" entry

                            yield NativeCall.readNullTerminatedUtf8 operation ctx.BaseClassTypes state stringPtr
                    ]

            let ns, simple = splitAtLastDot typeName

            let topLevel = assembly.TryGetTopLevelTypeDef ns simple

            let resolved =
                match topLevel with
                | None -> None
                | Some top ->
                    let rec walk
                        (parent : TypeInfo<GenericParamFromMetadata, TypeDefn>)
                        (rest : string list)
                        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
                        =
                        match rest with
                        | [] -> Some parent
                        | name :: rest ->
                            // Each nested entry is normally a simple name; keep
                            // the same split-at-last-'.' rule as CoreCLR uses
                            // when consumers smuggle a dotted name through.
                            let _, nestedSimple = splitAtLastDot name

                            match assembly.TryGetNestedTypeDef parent.TypeDefHandle nestedSimple with
                            | None -> None
                            | Some child -> walk child rest

                    walk top nestedNames

            match resolved with
            | None ->
                // CoreCLR also follows type forwarders (manifest exported types)
                // here when the TypeDef lookup misses. Be explicit about that
                // unimplemented path so tests that hit it fail loudly rather
                // than silently returning null.
                if assembly.TryGetTopLevelExportedType (Some ns) simple |> Option.isSome then
                    failwith $"TODO: %s{operation} type forwarding for %s{ns}.%s{simple} in %s{assemblyFullName}"

                // Caller's local was preinitialized to null (Type? type = null);
                // leaving retType untouched preserves that.
                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
            | Some typeInfo ->
                let runtimeTypeAddr, state =
                    if typeInfo.Generics.IsEmpty then
                        NativeRuntimeType.getOrAllocateNonGenericRuntimeType
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            state
                            typeInfo
                    else
                        // Generic type definition: matches typeof(List<>) — the
                        // RuntimeType represents the open generic, not a
                        // construction. Constructed generics arrive via
                        // Type.MakeGenericType, not here.
                        IlMachineState.getOrAllocateType
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            (RuntimeTypeHandleTarget.OpenGenericTypeDefinition typeInfo.Identity)
                            state

                let state =
                    IlMachineState.writeManagedByrefWithBase
                        ctx.BaseClassTypes
                        state
                        retType
                        (CliType.ObjectRef (Some runtimeTypeAddr))

                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
