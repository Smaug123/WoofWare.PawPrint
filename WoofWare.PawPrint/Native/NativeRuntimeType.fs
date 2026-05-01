namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module NativeRuntimeType =
    let private primitiveCorElementType (primitive : PrimitiveType) : int32 =
        match primitive with
        | PrimitiveType.Boolean -> 0x02
        | PrimitiveType.Char -> 0x03
        | PrimitiveType.SByte -> 0x04
        | PrimitiveType.Byte -> 0x05
        | PrimitiveType.Int16 -> 0x06
        | PrimitiveType.UInt16 -> 0x07
        | PrimitiveType.Int32 -> 0x08
        | PrimitiveType.UInt32 -> 0x09
        | PrimitiveType.Int64 -> 0x0A
        | PrimitiveType.UInt64 -> 0x0B
        | PrimitiveType.Single -> 0x0C
        | PrimitiveType.Double -> 0x0D
        | PrimitiveType.String -> 0x12
        | PrimitiveType.TypedReference -> 0x16
        | PrimitiveType.IntPtr -> 0x18
        | PrimitiveType.UIntPtr -> 0x19
        | PrimitiveType.Object -> 0x12

    let private nominalCorElementType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<_, _>)
        : int32
        =
        if DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo then
            0x11
        else
            0x12

    let private corElementType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : int32
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            nominalCorElementType baseClassTypes state typeInfo
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteVoid state.ConcreteTypes -> 0x01
            | ConcretePrimitive state.ConcreteTypes primitive -> primitiveCorElementType primitive
            | ConcreteTypeHandle.Byref _ -> 0x10
            | ConcreteTypeHandle.Pointer _ -> 0x0F
            | ConcreteTypeHandle.OneDimArrayZero _ -> 0x1D
            | ConcreteTypeHandle.Array _ -> 0x14
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                nominalCorElementType baseClassTypes state typeInfo

    let private enumUnderlyingPrimitive (operation : string) (typeInfo : TypeInfo<_, TypeDefn>) : PrimitiveType option =
        let instanceFields =
            typeInfo.Fields |> List.filter (fun field -> not field.IsStatic)

        match instanceFields with
        | [ field ] when field.Name = "value__" ->
            match field.Signature with
            | TypeDefn.PrimitiveType primitive -> Some primitive
            | other -> failwith $"%s{operation}: enum value__ field had non-primitive signature %O{other}"
        | _ -> None

    let private primitiveMethodTableCorElementType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (methodTableFor : ConcreteTypeHandle)
        : int32
        =
        match methodTableFor with
        | ConcretePrimitive state.ConcreteTypes primitive -> primitiveCorElementType primitive
        | ConcreteTypeHandle.Concrete _ ->
            let concreteType =
                AllConcreteTypes.lookup methodTableFor state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{methodTableFor}"
                )

            let assembly =
                state.LoadedAssembly concreteType.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                )

            let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]

            match enumUnderlyingPrimitive operation typeInfo with
            // CoreCLR debug-builds assert IsPrimitive for GetPrimitiveCorElementType, which excludes
            // enums. Release builds still fall through to the underlying primitive element type; match
            // that observable behaviour because managed enum code can reach this helper.
            | Some primitive -> primitiveCorElementType primitive
            | None ->
                failwith
                    $"%s{operation}: expected primitive or enum MethodTable, got %s{typeInfo.Namespace}.%s{typeInfo.Name}"
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith $"%s{operation}: expected primitive or enum MethodTable, got %O{methodTableFor}"

    let private requiredValueTypeMethod
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (name : string)
        (parameterCount : int)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        baseClassTypes.ValueType.Methods
        |> List.filter (fun methodInfo ->
            methodInfo.Name = name
            && methodInfo.Parameters.Length = parameterCount
            && not methodInfo.IsStatic
        )
        |> function
            | [ methodInfo ] -> methodInfo
            | [] -> failwith $"%s{operation}: could not find System.ValueType::%s{name}"
            | methods ->
                let signatures =
                    methods
                    |> List.map (fun methodInfo -> $"%s{methodInfo.Name}/%i{methodInfo.Parameters.Length}")
                    |> String.concat ", "

                failwith $"%s{operation}: ambiguous System.ValueType::%s{name} candidates: %s{signatures}"

    let private overridesValueTypeMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodTableFor : ConcreteTypeHandle)
        (valueTypeMethod : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let state, concretizedMethod, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                valueTypeMethod
                ImmutableArray.Empty
                state

        let state, directImplementation =
            IlMachineStateExecution.tryResolveVirtualImplementation
                loggerFactory
                baseClassTypes
                thread
                ImmutableArray.Empty
                concretizedMethod
                methodTableFor
                false
                state

        state, Option.isSome directImplementation

    let rec private fieldAllowsFastCompare
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (valueTypeEquals : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (valueTypeGetHashCode :
            WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (seen : Set<ConcreteTypeHandle>)
        (field : CliField)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let rec canCompareValueType
            (seen : Set<ConcreteTypeHandle>)
            (methodTableFor : ConcreteTypeHandle)
            (state : IlMachineState)
            : IlMachineState * bool
            =
            canCompareBitsOrUseFastGetHashCodeImpl
                loggerFactory
                baseClassTypes
                thread
                valueTypeEquals
                valueTypeGetHashCode
                seen
                methodTableFor
                state

        match CliType.unwrapPrimitiveLikeDeep field.Contents with
        | CliType.Numeric numeric ->
            match numeric with
            | CliNumericType.Float32 _
            | CliNumericType.Float64 _
            | CliNumericType.NativeFloat _
            | CliNumericType.NativeInt _ -> state, false
            | CliNumericType.Int32 _
            | CliNumericType.Int64 _
            | CliNumericType.Int8 _
            | CliNumericType.Int16 _
            | CliNumericType.UInt8 _
            | CliNumericType.UInt16 _ -> state, true
        | CliType.Bool _
        | CliType.Char _ -> state, true
        | CliType.ObjectRef _
        | CliType.RuntimePointer _ -> state, false
        | CliType.ValueType _ -> canCompareValueType seen field.Type state

    and private canCompareBitsOrUseFastGetHashCodeImpl
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (valueTypeEquals : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (valueTypeGetHashCode :
            WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (seen : Set<ConcreteTypeHandle>)
        (methodTableFor : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        if Set.contains methodTableFor seen then
            failwith
                $"MethodTable_CanCompareBitsOrUseFastGetHashCode: recursive value-type layout for %O{methodTableFor}"

        match methodTableFor with
        | ConcreteTypeHandle.Concrete _ ->
            let _, typeInfo =
                match IlMachineState.tryGetConcreteTypeInfo state methodTableFor with
                | Some result -> result
                | None ->
                    failwith
                        $"MethodTable_CanCompareBitsOrUseFastGetHashCode: concrete type handle was not registered: %O{methodTableFor}"

            if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo) then
                failwith
                    $"MethodTable_CanCompareBitsOrUseFastGetHashCode: expected value-type MethodTable, got %s{typeInfo.Namespace}.%s{typeInfo.Name}"

            let zero, state =
                IlMachineState.cliTypeZeroOfHandle state baseClassTypes methodTableFor

            let fieldLayoutIsTightlyPacked =
                match zero with
                | CliType.Numeric (CliNumericType.Float32 _)
                | CliType.Numeric (CliNumericType.Float64 _)
                | CliType.Numeric (CliNumericType.NativeFloat _)
                | CliType.Numeric (CliNumericType.NativeInt _)
                | CliType.ObjectRef _
                | CliType.RuntimePointer _ -> false
                | CliType.Numeric _
                | CliType.Bool _
                | CliType.Char _ -> true
                | CliType.ValueType vt -> CliValueType.IsTightlyPacked vt

            if not fieldLayoutIsTightlyPacked || CliType.containsObjectReferences zero then
                state, false
            else

            let state, overridesEquals =
                overridesValueTypeMethod loggerFactory baseClassTypes thread methodTableFor valueTypeEquals state

            let state, overridesGetHashCode =
                overridesValueTypeMethod loggerFactory baseClassTypes thread methodTableFor valueTypeGetHashCode state

            if overridesEquals || overridesGetHashCode then
                state, false
            else

            let state, fields =
                IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state methodTableFor

            let seen = Set.add methodTableFor seen

            ((state, true), fields)
            ||> List.fold (fun (state, canCompare) field ->
                if not canCompare then
                    state, false
                else
                    fieldAllowsFastCompare
                        loggerFactory
                        baseClassTypes
                        thread
                        valueTypeEquals
                        valueTypeGetHashCode
                        seen
                        field
                        state
            )
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            failwith
                $"MethodTable_CanCompareBitsOrUseFastGetHashCode: expected value-type MethodTable, got %O{methodTableFor}"

    let private canCompareBitsOrUseFastGetHashCode
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (methodTableFor : ConcreteTypeHandle)
        (state : IlMachineState)
        : IlMachineState * bool
        =
        let operation = "MethodTable_CanCompareBitsOrUseFastGetHashCode"

        let valueTypeEquals = requiredValueTypeMethod operation baseClassTypes "Equals" 1

        let valueTypeGetHashCode =
            requiredValueTypeMethod operation baseClassTypes "GetHashCode" 0

        canCompareBitsOrUseFastGetHashCodeImpl
            loggerFactory
            baseClassTypes
            thread
            valueTypeEquals
            valueTypeGetHashCode
            Set.empty
            methodTableFor
            state

    let private mdTypeDefNil : int32 = 0x02000000

    let private typeDefinitionToken (handle : System.Reflection.Metadata.TypeDefinitionHandle) : int32 =
        let handle : System.Reflection.Metadata.EntityHandle =
            System.Reflection.Metadata.TypeDefinitionHandle.op_Implicit handle

        System.Reflection.Metadata.Ecma335.MetadataTokens.GetToken handle

    let private typeDefinitionTokenOfRuntimeTypeHandleTarget
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : int32
        =
        // Generic parameter definitions have their own 0x2A metadata-token table.
        // RuntimeTypeHandleTarget cannot represent those today; Ldtoken rejects unbound
        // GenericTypeParameter/GenericMethodParameter tokens before allocating a RuntimeType.
        // If we add a generic-parameter RuntimeTypeHandleTarget later, handle it here rather
        // than projecting it through a TypeDef token.
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> typeDefinitionToken identity.TypeDefinition.Get
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                    )

                typeDefinitionToken concreteType.Definition.Get
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> mdTypeDefNil

    let private containsGenericVariables
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : bool
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            not typeInfo.Generics.IsEmpty
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            let rec closedTypeHandleContainsGenericVariables (typeHandle : ConcreteTypeHandle) : bool =
                match typeHandle with
                | ConcreteTypeHandle.Byref inner
                | ConcreteTypeHandle.Pointer inner
                | ConcreteTypeHandle.OneDimArrayZero inner
                | ConcreteTypeHandle.Array (inner, _) -> closedTypeHandleContainsGenericVariables inner
                | ConcreteTypeHandle.Concrete _ ->
                    let concreteType =
                        AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                        )

                    concreteType.Generics |> Seq.exists closedTypeHandleContainsGenericVariables

            closedTypeHandleContainsGenericVariables typeHandle

    let private getOrAllocateNonGenericRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ManagedHeapAddress * IlMachineState
        =
        if not typeInfo.Generics.IsEmpty then
            failwith
                $"RuntimeTypeHandle.GetDeclaringType: expected non-generic runtime type for %s{typeInfo.Name}, but metadata has %i{typeInfo.Generics.Length} generic parameters"

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeInfo

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                typeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, stk))

        IlMachineState.getOrAllocateType loggerFactory baseClassTypes (RuntimeTypeHandleTarget.Closed typeHandle) state

    let private declaringTypeInfo
        (operation : string)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> option
        =
        if not typeInfo.IsNested then
            None
        else
            let assembly =
                state.LoadedAssembly typeInfo.Assembly
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: declaring assembly is not loaded: %s{typeInfo.Assembly.FullName}"
                )

            Some assembly.TypeDefs.[typeInfo.DeclaringType]

    let private getOrAllocateDeclaringRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ManagedHeapAddress option * IlMachineState
        =
        match declaringTypeInfo "RuntimeTypeHandle.GetDeclaringType" state typeInfo with
        | None -> None, state
        | Some declaringTypeInfo when declaringTypeInfo.Generics.IsEmpty ->
            let addr, state =
                getOrAllocateNonGenericRuntimeType loggerFactory baseClassTypes state declaringTypeInfo

            Some addr, state
        | Some declaringTypeInfo ->
            let addr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.OpenGenericTypeDefinition declaringTypeInfo.Identity)
                    state

            Some addr, state

    let private declaringRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : ManagedHeapAddress option * IlMachineState
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"RuntimeTypeHandle.GetDeclaringType: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            getOrAllocateDeclaringRuntimeType loggerFactory baseClassTypes state typeInfo
        | RuntimeTypeHandleTarget.Closed typeHandle ->
            match typeHandle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> None, state
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"RuntimeTypeHandle.GetDeclaringType: concrete type handle was not registered: %O{typeHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"RuntimeTypeHandle.GetDeclaringType: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                getOrAllocateDeclaringRuntimeType loggerFactory baseClassTypes state typeInfo

    let private baseRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : ManagedHeapAddress option * IlMachineState
        =
        let baseHandle, state =
            match typeHandleTarget with
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                let assembly =
                    state.LoadedAssembly identity.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"RuntimeTypeHandle.GetBaseType: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                    )

                let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]

                match typeInfo.BaseType with
                | None -> None, state
                | Some baseTypeInfo ->
                    let state, baseAssembly, baseTypeDefn =
                        IlMachineState.resolveBaseTypeInfo loggerFactory baseClassTypes state assembly baseTypeInfo

                    let state, baseHandle =
                        IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssembly.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            baseTypeDefn

                    Some baseHandle, state
            | RuntimeTypeHandleTarget.Closed typeHandle ->
                match typeHandle with
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _ -> None, state
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    let state, baseHandle =
                        IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state typeHandle

                    baseHandle, state

        match baseHandle with
        | None -> None, state
        | Some baseHandle ->
            let addr, state =
                IlMachineState.getOrAllocateType
                    loggerFactory
                    baseClassTypes
                    (RuntimeTypeHandleTarget.Closed baseHandle)
                    state

            Some addr, state

    let private findCorelibType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (``namespace`` : string)
        (name : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        baseClassTypes.Corelib.TypeDefs
        |> Seq.choose (fun (KeyValue (_, typeInfo)) ->
            if typeInfo.Namespace = ``namespace`` && typeInfo.Name = name then
                Some typeInfo
            else
                None
        )
        |> Seq.exactlyOne

    let private concretizeNonGenericCorelibType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (``namespace`` : string)
        (name : string)
        : IlMachineState * TypeInfo<GenericParamFromMetadata, TypeDefn> * ConcreteTypeHandle
        =
        let typeInfo = findCorelibType baseClassTypes ``namespace`` name

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies typeInfo

        let state, typeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, stk))

        state, typeInfo, typeHandle

    let private allocateManagedObjectOfConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (typeHandle : ConcreteTypeHandle)
        : ManagedHeapAddress * IlMachineState
        =
        let state, allFields =
            IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state typeHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes typeHandle typeInfo.Layout allFields

        IlMachineState.allocateManagedObject typeHandle fields state

    let private getOrAllocateRuntimeAssembly
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assemblyFullName = assemblyName.FullName

        match state.RuntimeAssemblyObjects.TryGetValue assemblyFullName with
        | true, cachedAddr -> cachedAddr, state
        | false, _ ->
            let state, runtimeAssemblyTypeInfo, runtimeAssemblyTypeHandle =
                concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System.Reflection" "RuntimeAssembly"

            let addr, state =
                allocateManagedObjectOfConcreteType
                    loggerFactory
                    baseClassTypes
                    state
                    runtimeAssemblyTypeInfo
                    runtimeAssemblyTypeHandle

            // Set the m_assembly field to a tagged native pointer so downstream native
            // calls can map back to the PawPrint DumpedAssembly.
            let assemblyField =
                FieldIdentity.requiredOwnInstanceField runtimeAssemblyTypeInfo "m_assembly"
                |> FieldIdentity.fieldId runtimeAssemblyTypeHandle

            let updatedObj =
                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetFieldById
                    assemblyField
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)))

            let state =
                { state with
                    ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                    RuntimeAssemblyObjects = state.RuntimeAssemblyObjects.Add (assemblyFullName, addr)
                }

            addr, state

    let private getOrAllocateModuleRuntimeType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assembly =
            state.LoadedAssembly assemblyName
            |> Option.defaultWith (fun () ->
                failwith
                    $"RuntimeTypeHandle.GetModule: assembly %s{assemblyName.FullName} for module type is not loaded"
            )

        let moduleTypeInfo =
            assembly.TypeDefs.Values
            |> Seq.tryFind (fun typeInfo -> typeInfo.Namespace = "" && typeInfo.Name = "<Module>")
            |> Option.defaultWith (fun () ->
                failwith $"RuntimeTypeHandle.GetModule: assembly %s{assemblyName.FullName} has no <Module> type"
            )

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies moduleTypeInfo

        let state, moduleTypeHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                moduleTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (moduleTypeInfo.Identity, stk))

        IlMachineState.getOrAllocateType
            loggerFactory
            baseClassTypes
            (RuntimeTypeHandleTarget.Closed moduleTypeHandle)
            state

    let getOrAllocateRuntimeModule
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyName : System.Reflection.AssemblyName)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let assemblyFullName = assemblyName.FullName

        match state.RuntimeModuleObjects.TryGetValue assemblyFullName with
        | true, cachedAddr -> cachedAddr, state
        | false, _ ->
            let runtimeAssemblyAddr, state =
                getOrAllocateRuntimeAssembly loggerFactory baseClassTypes assemblyName state

            let moduleRuntimeTypeAddr, state =
                getOrAllocateModuleRuntimeType loggerFactory baseClassTypes assemblyName state

            let state, runtimeModuleTypeInfo, runtimeModuleTypeHandle =
                concretizeNonGenericCorelibType loggerFactory baseClassTypes state "System.Reflection" "RuntimeModule"

            let addr, state =
                allocateManagedObjectOfConcreteType
                    loggerFactory
                    baseClassTypes
                    state
                    runtimeModuleTypeInfo
                    runtimeModuleTypeHandle

            let updatedObj =
                let runtimeAssemblyField =
                    FieldIdentity.requiredOwnInstanceField runtimeModuleTypeInfo "m_runtimeAssembly"
                    |> FieldIdentity.fieldId runtimeModuleTypeHandle

                let runtimeTypeField =
                    FieldIdentity.requiredOwnInstanceField runtimeModuleTypeInfo "m_runtimeType"
                    |> FieldIdentity.fieldId runtimeModuleTypeHandle

                let pDataField =
                    FieldIdentity.requiredOwnInstanceField runtimeModuleTypeInfo "m_pData"
                    |> FieldIdentity.fieldId runtimeModuleTypeHandle

                ManagedHeap.get addr state.ManagedHeap
                |> AllocatedNonArrayObject.SetFieldById
                    runtimeAssemblyField
                    (CliType.ObjectRef (Some runtimeAssemblyAddr))
                |> AllocatedNonArrayObject.SetFieldById
                    runtimeTypeField
                    (CliType.ObjectRef (Some moduleRuntimeTypeAddr))
                |> AllocatedNonArrayObject.SetFieldById
                    pDataField
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)))

            let state =
                { state with
                    ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                    RuntimeModuleObjects = state.RuntimeModuleObjects.Add (assemblyFullName, addr)
                }

            addr, state

    let private formatNamespaceFlag : int32 = 0x00000001
    let private formatFullInstFlag : int32 = 0x00000002
    let private formatAssemblyFlag : int32 = 0x00000004
    let private formatNoVersionFlag : int32 = 0x00000010

    let private hasFormatFlag (flag : int32) (flags : int32) : bool = flags &&& flag <> 0

    let private typeInfoDisplayName
        (includeNamespace : bool)
        (assembly : DumpedAssembly)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : string
        =
        if includeNamespace then
            TypeInfo.fullName (fun h -> assembly.TypeDefs.[h]) typeInfo
        else
            typeInfo.Name

    let private assemblyDisplayName (noVersion : bool) (assemblyName : System.Reflection.AssemblyName) : string =
        if noVersion then
            assemblyName.Name
        else
            assemblyName.FullName

    let private runtimeTypeHandleName
        (operation : string)
        (state : IlMachineState)
        (flags : int32)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : string
        =
        let includeNamespace = hasFormatFlag formatNamespaceFlag flags
        let includeGenericInstantiation = hasFormatFlag formatFullInstFlag flags
        let includeAssembly = hasFormatFlag formatAssemblyFlag flags
        let noVersion = hasFormatFlag formatNoVersionFlag flags

        let rec concreteTypeHandleName (typeHandle : ConcreteTypeHandle) : string =
            match typeHandle with
            | ConcreteTypeHandle.Byref inner -> $"%s{concreteTypeHandleName inner}&"
            | ConcreteTypeHandle.Pointer inner -> $"%s{concreteTypeHandleName inner}*"
            | ConcreteTypeHandle.OneDimArrayZero inner -> $"%s{concreteTypeHandleName inner}[]"
            | ConcreteTypeHandle.Array (inner, rank) ->
                let dims = if rank <= 1 then "*" else System.String (',', rank - 1)
                $"%s{concreteTypeHandleName inner}[%s{dims}]"
            | ConcreteTypeHandle.Concrete _ ->
                let concreteType =
                    AllConcreteTypes.lookup typeHandle state.ConcreteTypes
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                    )

                let assembly =
                    state.LoadedAssembly concreteType.Assembly
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                    )

                let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                let name = typeInfoDisplayName includeNamespace assembly typeInfo

                let name =
                    if includeGenericInstantiation && not concreteType.Generics.IsEmpty then
                        let args =
                            concreteType.Generics |> Seq.map concreteTypeHandleName |> String.concat ","

                        $"%s{name}[%s{args}]"
                    else
                        name

                if includeAssembly then
                    $"%s{name}, %s{assemblyDisplayName noVersion concreteType.Assembly}"
                else
                    name

        match typeHandleTarget with
        | RuntimeTypeHandleTarget.Closed typeHandle -> concreteTypeHandleName typeHandle
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assembly =
                state.LoadedAssembly identity.Assembly
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                )

            let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
            let name = typeInfoDisplayName includeNamespace assembly typeInfo

            if includeAssembly then
                $"%s{name}, %s{assemblyDisplayName noVersion identity.Assembly}"
            else
                name

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : ExecutionResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "RuntimeTypeHandle_ConstructName",
          "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "ConstructName",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallTypeHandle",
                                              qCallGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "TypeNameFormatFlags", flagsGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "StringHandleOnStack",
                                              stringHandleGenerics) ],
          MethodReturnType.Void when qCallGenerics.IsEmpty && flagsGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            let operation = "RuntimeTypeHandle.ConstructName"
            let qCallHandle = instruction.Arguments.[0] |> EvalStackValue.ofCliType

            let typeHandleTarget =
                NativeCall.qCallTypeHandleToRuntimeTypeHandleTarget operation state qCallHandle

            let flags =
                match CliType.unwrapPrimitiveLikeDeep instruction.Arguments.[1] with
                | CliType.Numeric (CliNumericType.Int32 flags) -> flags
                | other -> failwith $"%s{operation}: expected TypeNameFormatFlags as Int32, got %O{other}"

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[2]

            let name = runtimeTypeHandleName operation state flags typeHandleTarget

            let nameAddr, state =
                IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes name state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some nameAddr))

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "MethodTable_CanCompareBitsOrUseFastGetHashCode",
          "System.Private.CoreLib",
          "System",
          "ValueType",
          _,
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
                                                               "MethodTable",
                                                               methodTableGenerics)) ],
          returnType when methodTableGenerics.IsEmpty ->
            let operation = "MethodTable_CanCompareBitsOrUseFastGetHashCode"

            match returnType with
            | MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean)
            | MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) -> ()
            | other -> failwith $"%s{operation}: unexpected QCall stub return type %O{other}"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let methodTableArg = instruction.Arguments.[0] |> EvalStackValue.ofCliType
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let state, canCompare =
                canCompareBitsOrUseFastGetHashCode ctx.LoggerFactory ctx.BaseClassTypes ctx.Thread methodTableFor state

            let state =
                let ret = if canCompare then 1 else 0

                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 ret)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None

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
          "System.Runtime.CompilerServices",
          "MethodTable",
          "GetNumInstanceFieldBytes",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            let operation = "MethodTable.GetNumInstanceFieldBytes"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let methodTableArg, state = IlMachineState.popEvalStack ctx.Thread state
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let bytes, state =
                MethodTableProjection.numInstanceFieldBytes ctx.BaseClassTypes state methodTableFor

            let state =
                IlMachineState.pushToEvalStack (NativeCall.cliUInt32 bytes) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "MethodTable",
          "GetPrimitiveCorElementType",
          [],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "CorElementType",
                                                                      corElementTypeGenerics)) when
            corElementTypeGenerics.IsEmpty
            ->
            let operation = "MethodTable.GetPrimitiveCorElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let methodTableArg, state = IlMachineState.popEvalStack ctx.Thread state
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let elementType =
                primitiveMethodTableCorElementType operation ctx.BaseClassTypes state methodTableFor

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetCorElementType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "CorElementType",
                                                                      corElementTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && corElementTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetCorElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let elementType = corElementType operation ctx.BaseClassTypes state typeHandleTarget

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetToken",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetToken"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let token =
                typeDefinitionTokenOfRuntimeTypeHandleTarget operation state typeHandleTarget

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 token)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "IsGenericVariable",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.IsGenericVariable"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef
            |> ignore

            // RuntimeTypeHandleTarget cannot currently represent generic parameter
            // handles. Ldtoken rejects unbound generic parameters before allocating a
            // RuntimeType, and open generic type definitions are not generic variables.
            let state = IlMachineState.pushToEvalStack (CliType.ofBool false) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetDeclaringType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let declaringTypeAddr, state =
                declaringRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget declaringTypeAddr ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "ContainsGenericVariables",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.ContainsGenericVariables"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let result = containsGenericVariables operation state typeHandleTarget

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetBaseType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetBaseType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let baseTypeAddr, state =
                baseRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget baseTypeAddr ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetAssembly",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeAssembly",
                                                                      runtimeAssemblyGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeAssemblyGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetAssembly"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName = NativeCall.typeAssemblyName operation state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeAssembly ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetModule",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeModule",
                                                                      runtimeModuleGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetModule"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName = NativeCall.typeAssemblyName operation state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeModule ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            (state, WhatWeDid.Executed) |> ExecutionResult.Stepped |> Some
        | _ -> None
