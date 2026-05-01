namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IlMachineState =
    let loadAssembly = IlMachineTypeResolution.loadAssembly

    let internal loader = IlMachineTypeResolution.loader

    let concretizeType = IlMachineTypeResolution.concretizeType

    let internal resolveTopLevelTypeFromName =
        IlMachineTypeResolution.resolveTopLevelTypeFromName

    let resolveTypeFromExport = IlMachineTypeResolution.resolveTypeFromExport

    let resolveTypeFromRef = IlMachineTypeResolution.resolveTypeFromRef

    let resolveType = IlMachineTypeResolution.resolveType

    let resolveTypeFromDefn = IlMachineTypeResolution.resolveTypeFromDefn

    let resolveTypeFromSpec = IlMachineTypeResolution.resolveTypeFromSpec

    let resolveTypeFromSpecConcrete =
        IlMachineTypeResolution.resolveTypeFromSpecConcrete

    let resolveTypeFromDefnConcrete =
        IlMachineTypeResolution.resolveTypeFromDefnConcrete

    let runtimeTypeHandleTargetForTypeToken =
        IlMachineTypeResolution.runtimeTypeHandleTargetForTypeToken

    let cliTypeZeroOfHandle = IlMachineTypeResolution.cliTypeZeroOfHandle

    let concretizeFieldDeclaringType =
        IlMachineTypeResolution.concretizeFieldDeclaringType

    let cliTypeZeroOf = IlMachineTypeResolution.cliTypeZeroOf

    let ensureByteConcreteType = IlMachineTypeResolution.ensureByteConcreteType

    let peByteRangeForFieldRva = IlMachineTypeResolution.peByteRangeForFieldRva

    let peByteRangePointer = IlMachineTypeResolution.peByteRangePointer

    let getFrame = IlMachineThreadState.getFrame

    let setFrame = IlMachineThreadState.setFrame

    let mapFrame = IlMachineThreadState.mapFrame

    let pushToEvalStack' = IlMachineThreadState.pushToEvalStack'

    let pushToEvalStack = IlMachineThreadState.pushToEvalStack

    let peekEvalStack = IlMachineThreadState.peekEvalStack

    let popEvalStack = IlMachineThreadState.popEvalStack

    let advanceProgramCounter = IlMachineThreadState.advanceProgramCounter

    let setArrayValue = IlMachineThreadState.setArrayValue

    let getArrayValue = IlMachineThreadState.getArrayValue

    let returnFromSyntheticStackFrame =
        IlMachineThreadState.returnFromSyntheticStackFrame

    let returnStackFrame = IlMachineThreadState.returnStackFrame

    let initial = IlMachineThreadState.initial

    let addThread = IlMachineThreadState.addThread

    let allocateArray = IlMachineThreadState.allocateArray

    let allocateStringData = IlMachineThreadState.allocateStringData

    let setStringData = IlMachineThreadState.setStringData

    let allocateManagedObject = IlMachineThreadState.allocateManagedObject

    let popFromStackToLocalVariable = IlMachineThreadState.popFromStackToLocalVariable

    let popFromStackToArgument = IlMachineThreadState.popFromStackToArgument

    let jumpProgramCounter = IlMachineThreadState.jumpProgramCounter

    let loadArgument = IlMachineThreadState.loadArgument


    let resolveMemberWithGenerics = IlMachineMemberResolution.resolveMemberWithGenerics

    let resolveMember = IlMachineMemberResolution.resolveMember

    let getLocalVariable = IlMachineThreadState.getLocalVariable

    let setLocalVariable = IlMachineThreadState.setLocalVariable

    let setArgument = IlMachineThreadState.setArgument

    let allocateLocalMemory = IlMachineThreadState.allocateLocalMemory

    let readLocalMemoryBytes = IlMachineThreadState.readLocalMemoryBytes

    let writeLocalMemoryBytes = IlMachineThreadState.writeLocalMemoryBytes

    let setSyncBlock = IlMachineThreadState.setSyncBlock

    let getSyncBlock = IlMachineThreadState.getSyncBlock


    let setStatic = IlMachineManagedByref.setStatic

    let getStatic = IlMachineManagedByref.getStatic

    let readManagedByrefBytesAs = IlMachineManagedByref.readManagedByrefBytesAs

    let readManagedByref = IlMachineManagedByref.readManagedByref

    let readManagedByrefField = IlMachineManagedByref.readManagedByrefField

    let writeManagedByrefBytes = IlMachineManagedByref.writeManagedByrefBytes

    let writeManagedByref = IlMachineManagedByref.writeManagedByref

    let writeManagedByrefWithBase = IlMachineManagedByref.writeManagedByrefWithBase

    let executeDelegateConstructor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (instruction : MethodState)
        (state : IlMachineState)
        : IlMachineState
        =
        // We've been called with arguments already popped from the stack into local arguments.
        let constructing = instruction.Arguments.[0]
        let targetObj = instruction.Arguments.[1]
        let methodPtr = instruction.Arguments.[2]

        let targetObj =
            match targetObj with
            | CliType.ObjectRef (Some target) -> Some target
            | CliType.ObjectRef None -> None
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
            | _ -> failwith $"Unexpected target type for delegate: {targetObj}"

        let constructing =
            match constructing with
            | CliType.ObjectRef None -> failwith "unexpectedly constructing the null delegate"
            | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) ->
                failwith "unexpectedly constructing the null delegate"
            | CliType.ObjectRef (Some target) -> target
            | _ -> failwith $"Unexpectedly not constructing a managed object: {constructing}"

        let heapObj =
            match state.ManagedHeap.NonArrayObjects.TryGetValue constructing with
            | true, obj -> obj
            | false, _ -> failwith $"Delegate object {constructing} not found on heap"

        let delegateTypeHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.DelegateType

        let targetField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.DelegateType "_target"
            |> FieldIdentity.fieldId delegateTypeHandle

        let methodPtrField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.DelegateType "_methodPtr"
            |> FieldIdentity.fieldId delegateTypeHandle

        let updatedObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById targetField (CliType.ObjectRef targetObj)
            |> AllocatedNonArrayObject.SetFieldById methodPtrField methodPtr

        let updatedHeap =
            { state.ManagedHeap with
                NonArrayObjects = state.ManagedHeap.NonArrayObjects |> Map.add constructing updatedObj
            }

        { state with
            ManagedHeap = updatedHeap
        }

    /// Returns the type handle and an allocated System.RuntimeType.
    let getOrAllocateType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (defn : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, runtimeType =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeType.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            TypeHandleRegistry.getOrAllocate
                state.ConcreteTypes
                baseClassTypes
                state
                (fun fields state -> allocateManagedObject runtimeType fields state)
                defn
                state.TypeHandles

        let state =
            { state with
                TypeHandles = reg
            }

        result, state

    /// Returns a System.RuntimeFieldHandle.
    let getOrAllocateField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssy : AssemblyName)
        (fieldHandle : FieldDefinitionHandle)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let field = state.LoadedAssembly(declaringAssy).Value.Fields.[fieldHandle]

        // For LdToken, we need to convert GenericParamFromMetadata to TypeDefn
        // When we don't have generic context, we use the generic type parameters directly
        let declaringTypeWithGenerics =
            field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringType, state =
            concretizeFieldDeclaringType loggerFactory baseClassTypes declaringTypeWithGenerics state

        let state, runtimeFieldInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeFieldInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            FieldHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> allocateManagedObject runtimeFieldInfoStub fields state)
                declaringAssy
                declaringType
                fieldHandle
                state.FieldHandles

        let state =
            { state with
                FieldHandles = reg
            }

        result, state

    /// Returns a System.RuntimeMethodHandle.
    let getOrAllocateMethod
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let state, runtimeMethodInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeMethodInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let result, reg, state =
            MethodHandleRegistry.getOrAllocate
                baseClassTypes
                state.ConcreteTypes
                state
                (fun fields state -> allocateManagedObject runtimeMethodInfoStub fields state)
                method
                state.MethodHandles

        let state =
            { state with
                MethodHandles = reg
            }

        result, state

    let evalStackValueToObjectRef (state : IlMachineState) (value : EvalStackValue) : ManagedHeapAddress option =
        match value with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | EvalStackValue.ManagedPointer src ->
            match readManagedByref state src with
            | CliType.ObjectRef addr -> addr
            | other -> failwith $"expected object reference, got {other}"
        | other -> failwith $"expected object reference, got {other}"

    let lookupTypeDefn
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeDef : TypeDefinitionHandle)
        : IlMachineState * TypeDefn
        =
        let defn = activeAssy.TypeDefs.[typeDef]
        state, DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies defn

    let lookupTypeRef
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        typeGenerics
        (ref : TypeReferenceHandle)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        let ref = activeAssy.TypeRefs.[ref]

        // Convert ConcreteTypeHandles back to TypeDefn for metadata operations
        let typeGenerics =
            typeGenerics
            |> Seq.map (fun handle ->
                Concretization.concreteHandleToTypeDefn
                    baseClassTypes
                    handle
                    state.ConcreteTypes
                    state._LoadedAssemblies
            )
            |> ImmutableArray.CreateRange

        let state, assy, resolved =
            resolveTypeFromRef loggerFactory activeAssy ref typeGenerics state

        state, DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved, assy

    let private ensureAssemblyLoadedByName
        (loggerFactory : ILoggerFactory)
        (state : IlMachineState)
        (referencedInAssembly : DumpedAssembly)
        (assemblyName : AssemblyName)
        : IlMachineState * DumpedAssembly
        =
        match state.LoadedAssembly assemblyName with
        | Some loadedAssembly -> state, loadedAssembly
        | None ->
            let handle =
                referencedInAssembly.AssemblyReferences
                |> Seq.tryPick (fun (KeyValue (assemblyRefHandle, assemblyRef)) ->
                    if assemblyRef.Name.FullName = assemblyName.FullName then
                        Some assemblyRefHandle
                    else
                        None
                )
                |> Option.defaultWith (fun () ->
                    failwithf
                        "Assembly %s needs base assembly %s, but no AssemblyReferenceHandle was found"
                        referencedInAssembly.Name.FullName
                        assemblyName.FullName
                )

            let state, loadedAssembly, _ =
                loadAssembly loggerFactory referencedInAssembly handle state

            state, loadedAssembly

    /// Resolve a BaseTypeInfo to the assembly and TypeDefn of the base type.
    let resolveBaseTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (currentAssembly : DumpedAssembly)
        (baseTypeInfo : BaseTypeInfo)
        : IlMachineState * DumpedAssembly * TypeDefn
        =
        match baseTypeInfo with
        | BaseTypeInfo.TypeDef handle ->
            let typeInfo = currentAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, currentAssembly, typeDefn
        | BaseTypeInfo.TypeRef handle ->
            let state, assy, resolved =
                resolveTypeFromRef
                    loggerFactory
                    currentAssembly
                    (currentAssembly.TypeRefs.[handle])
                    ImmutableArray.Empty
                    state

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn baseClassTypes state._LoadedAssemblies resolved

            state, assy, typeDefn
        | BaseTypeInfo.ForeignAssemblyType (assemblyName, handle) ->
            let state, foreignAssembly =
                ensureAssemblyLoadedByName loggerFactory state currentAssembly assemblyName

            let typeInfo = foreignAssembly.TypeDefs.[handle]

            let typeDefn =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

            state, foreignAssembly, typeDefn
        | BaseTypeInfo.TypeSpec handle ->
            let signature = currentAssembly.TypeSpecs.[handle].Signature
            state, currentAssembly, signature

    /// Given a ConcreteTypeHandle, resolve and return its base type as a ConcreteTypeHandle.
    /// Returns None for types without a base type (System.Object).
    let resolveBaseConcreteType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * ConcreteTypeHandle option
        =
        match concreteType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Structural array handles keep their own runtime identity; their base type is System.Array.
            let state, arrayHandle =
                DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Array
                |> concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            state, Some arrayHandle
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ ->

            match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | Some ct ->
                let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
                let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

                match typeInfo.BaseType with
                | None -> state, None
                | Some baseTypeInfo ->
                    let state, baseAssy, baseTypeDefn =
                        resolveBaseTypeInfo loggerFactory baseClassTypes state assy baseTypeInfo

                    let state, baseHandle =
                        concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssy.Name
                            ct.Generics
                            ImmutableArray.Empty
                            baseTypeDefn

                    state, Some baseHandle

    /// Collect ALL instance fields from the entire type hierarchy for a given ConcreteTypeHandle,
    /// walking from base to derived (base class fields appear first in the returned list).
    let rec collectAllInstanceFields
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * CliField list
        =
        let ct =
            AllConcreteTypes.lookup concreteType state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"collectAllInstanceFields: ConcreteTypeHandle %O{concreteType} not found in AllConcreteTypes"
            )

        let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]
        let typeInfo = assy.TypeDefs.[ct.Identity.TypeDefinition.Get]

        // Get this type's own instance fields
        let state, ownFields =
            let instanceFields =
                typeInfo.Fields
                |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

            ((state, []), instanceFields)
            ||> List.fold (fun (state, fields) field ->
                let state, zero, fieldTypeHandle =
                    cliTypeZeroOf
                        loggerFactory
                        baseClassTypes
                        assy
                        field.Signature
                        ct.Generics
                        ImmutableArray.Empty
                        state

                let cliField : CliField =
                    {
                        Id = FieldId.metadata concreteType field.Handle field.Name
                        Name = field.Name
                        Contents = zero
                        Offset = field.Offset
                        Type = fieldTypeHandle
                    }

                state, cliField :: fields
            )

        let ownFields = List.rev ownFields

        // Recurse into base type
        let state, baseHandle =
            resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

        match baseHandle with
        | None -> state, ownFields
        | Some parentHandle ->
            let state, baseFields =
                collectAllInstanceFields loggerFactory baseClassTypes state parentHandle

            state, baseFields @ ownFields

    /// Allocate a new System.String managed object on the heap with the given contents.
    /// Does NOT intern the string: every call returns a fresh heap object.  The Ldstr opcode
    /// wraps this with its own interning cache (see UnaryStringTokenIlOp); runtime-generated
    /// strings (stack traces, type names, etc.) call this directly.
    let allocateManagedString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : string)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        // String type is:
        // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/libraries/System.Private.CoreLib/src/System/String.cs#L26
        let stringInstanceFields =
            baseClassTypes.String.Fields
            |> List.choose (fun field ->
                if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                    Some (field.Name, field.Signature)
                else
                    None
            )
            |> List.sortBy fst

        if
            stringInstanceFields
            <> [
                ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
            ]
        then
            failwith $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

        let dataAddr, state = allocateStringData contents.Length state
        let state = setStringData dataAddr contents state

        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.String
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let fields =
            let firstCharField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.String "_firstChar"

            let stringLengthField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.String "_stringLength"

            [
                FieldIdentity.cliField
                    stringType
                    firstCharField
                    (CliType.ofChar state.ManagedHeap.StringArrayData.[dataAddr])
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Char)
                FieldIdentity.cliField
                    stringType
                    stringLengthField
                    (CliType.Numeric (CliNumericType.Int32 contents.Length))
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32)
            ]
            |> CliValueType.OfFields baseClassTypes state.ConcreteTypes stringType Layout.Default

        let addr, state = allocateManagedObject stringType fields state

        let state =
            { state with
                ManagedHeap =
                    state.ManagedHeap
                    |> ManagedHeap.recordStringContents addr contents
                    |> ManagedHeap.recordStringDataOffset addr dataAddr
            }

        addr, state

    let private concreteTypeFullName (state : IlMachineState) (ty : ConcreteType<ConcreteTypeHandle>) : string =
        match state.LoadedAssembly ty.Assembly with
        | Some assy -> Assembly.fullName assy ty.Identity
        | None when String.IsNullOrEmpty ty.Namespace -> ty.Name
        | None -> $"{ty.Namespace}.{ty.Name}"

    let private renderExceptionStackFrame
        (state : IlMachineState)
        (frame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : string
        =
        let typeName = concreteTypeFullName state frame.Method.DeclaringType
        $"   at %s{typeName}.%s{frame.Method.Name}()"

    let private renderExceptionStackTrace
        (state : IlMachineState)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        : string
        =
        stackTrace
        |> List.map (renderExceptionStackFrame state)
        |> String.concat Environment.NewLine

    /// Project PawPrint's structured exception trace into the managed `System.Exception`
    /// object so guest code observing `Exception.StackTrace` sees a non-null trace string.
    let setExceptionStackTraceString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (stackTrace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list)
        (state : IlMachineState)
        : IlMachineState
        =
        match stackTrace with
        | [] -> state
        | _ :: _ ->
            // Low-level dispatch tests sometimes use synthetic exception addresses in skeletal states.
            // Full guest execution has both pieces, so only then can we project into the managed object.
            match
                state.ManagedHeap.NonArrayObjects |> Map.tryFind exceptionAddr,
                AllConcreteTypes.findExistingNonGenericConcreteType
                    state.ConcreteTypes
                    baseClassTypes.Exception.Identity
            with
            | Some heapObj, Some exceptionHandle ->
                let trace = renderExceptionStackTrace state stackTrace

                let traceAddr, state =
                    allocateManagedString loggerFactory baseClassTypes trace state

                let stackTraceStringField =
                    FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_stackTraceString"
                    |> FieldIdentity.fieldId exceptionHandle

                let heapObj =
                    heapObj
                    |> AllocatedNonArrayObject.SetFieldById stackTraceStringField (CliType.ObjectRef (Some traceAddr))

                { state with
                    ManagedHeap = ManagedHeap.set exceptionAddr heapObj state.ManagedHeap
                }
            | None, _
            | _, None -> state

    /// Return the managed `System.Threading.Thread` heap object corresponding to the given guest
    /// thread, allocating it on first request and caching the address thereafter so that repeated
    /// calls yield reference-identical objects. Populates only the fields whose zero-initialised
    /// defaults would observably diverge from the CLR: `_managedThreadId` (ThreadId 0 is
    /// hardcoded to managed ID 1; others consume `NextManagedThreadId`), `_priority` (CLR
    /// exposes `ThreadPriority.Normal = 2`, not zero-valued `Lowest`), and
    /// `_DONT_USE_InternalThread` (non-zero sentinel so `GetNativeHandle()` doesn't throw).
    /// The Thread constructor is NOT run; other fields remain zero-initialised.
    let getOrAllocateManagedThreadObject
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (threadId : ThreadId)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr -> addr, state
        | None ->

        let threadTypeInfo =
            baseClassTypes.Corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) ->
                if v.Namespace = "System.Threading" && v.Name = "Thread" then
                    Some v
                else
                    None
            )
            |> Seq.exactlyOne

        let state, threadTypeHandle =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies threadTypeInfo
            |> concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state threadTypeHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes threadTypeHandle threadTypeInfo.Layout allFields

        let addr, state = allocateManagedObject threadTypeHandle fields state

        // The main thread (ThreadId 0) always gets managed ID 1 — the CLR assigns it at
        // startup, before user code runs.  Other scheduler-created threads consume the shared
        // counter so IDs remain globally unique.
        let managedThreadId, state =
            let (ThreadId idx) = threadId

            if idx = 0 then
                1, state
            else
                let id = state.NextManagedThreadId

                id,
                { state with
                    NextManagedThreadId = id + 1
                }

        let threadPriorityNormal = 2
        let (ManagedHeapAddress addrInt) = addr

        let managedThreadIdField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_managedThreadId"
            |> FieldIdentity.fieldId threadTypeHandle

        let priorityField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_priority"
            |> FieldIdentity.fieldId threadTypeHandle

        let internalThreadField =
            FieldIdentity.requiredOwnInstanceField threadTypeInfo "_DONT_USE_InternalThread"
            |> FieldIdentity.fieldId threadTypeHandle

        let updatedObj =
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.SetFieldById
                managedThreadIdField
                (CliType.Numeric (CliNumericType.Int32 managedThreadId))
            |> AllocatedNonArrayObject.SetFieldById
                priorityField
                (CliType.Numeric (CliNumericType.Int32 threadPriorityNormal))
            |> AllocatedNonArrayObject.SetFieldById
                internalThreadField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim (int64 addrInt))))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr updatedObj state.ManagedHeap
                ManagedThreadObjects = state.ManagedThreadObjects |> Map.add threadId addr
            }

        addr, state

    /// Return the CLR-visible managed thread ID for the current guest thread.
    /// This is distinct from PawPrint's scheduler ThreadId.
    let getCurrentManagedThreadId (threadId : ThreadId) (state : IlMachineState) : int =
        match state.ManagedThreadObjects.TryFind threadId with
        | Some addr ->
            let threadObj = ManagedHeap.get addr state.ManagedHeap

            let threadConcreteType =
                AllConcreteTypes.lookup threadObj.ConcreteType state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"Environment.CurrentManagedThreadId: Thread object has unknown concrete type %O{threadObj.ConcreteType}"
                )

            let threadAssembly =
                state._LoadedAssemblies.[threadConcreteType.Identity.AssemblyFullName]

            let threadTypeInfo =
                threadAssembly.TypeDefs.[threadConcreteType.Identity.TypeDefinition.Get]

            let managedThreadIdField =
                FieldIdentity.requiredOwnInstanceField threadTypeInfo "_managedThreadId"
                |> FieldIdentity.fieldId threadObj.ConcreteType

            match AllocatedNonArrayObject.DereferenceFieldById managedThreadIdField threadObj with
            | CliType.Numeric (CliNumericType.Int32 id) -> id
            | other ->
                failwith
                    $"Environment.CurrentManagedThreadId: Thread object for ThreadId %O{threadId} has non-int32 _managedThreadId field %O{other}"
        | None ->
            match threadId with
            | ThreadId.ThreadId 0 -> 1
            | ThreadId.ThreadId _ ->
                failwith
                    $"Environment.CurrentManagedThreadId: non-main ThreadId %O{threadId} has no managed Thread object"

    /// Synthesize a TypeInitializationException wrapping the given inner exception object.
    /// Allocates the exception on the heap with zero-initialized fields (constructor is NOT run).
    /// Sets the _innerException, _typeName, and _HResult fields on the TIE to match what the
    /// TypeInitializationException(string, Exception) ctor would have done.
    /// Returns the heap address, the ConcreteTypeHandle, and the updated state.
    let synthesizeTypeInitializationException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeFullName : string)
        (innerExceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let tieTypeInfo = baseClassTypes.TypeInitializationException

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies tieTypeInfo

        let state, tieHandle =
            concretizeType
                loggerFactory
                baseClassTypes
                state
                tieTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (tieTypeInfo.Identity, stk))

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state tieHandle

        let fields =
            CliValueType.OfFields baseClassTypes state.ConcreteTypes tieHandle tieTypeInfo.Layout allFields

        let addr, state = allocateManagedObject tieHandle fields state

        let typeNameAddr, state =
            allocateManagedString loggerFactory baseClassTypes typeFullName state

        // Set _innerException, _typeName and _HResult on the allocated TIE, matching what the
        // TypeInitializationException(string, Exception) ctor would have done.
        // See CLR's EEException::CreateThrowable:
        // https://github.com/dotnet/dotnet/blob/10060d128e3f470e77265f8490f5e4f72dae738e/src/runtime/src/coreclr/vm/clrex.cpp#L972-L1019
        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let exceptionHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Exception

        let innerExceptionField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_innerException"
            |> FieldIdentity.fieldId exceptionHandle

        let typeNameField =
            FieldIdentity.requiredOwnInstanceField tieTypeInfo "_typeName"
            |> FieldIdentity.fieldId tieHandle

        let hresultField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_HResult"
            |> FieldIdentity.fieldId exceptionHandle

        let heapObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById innerExceptionField (CliType.ObjectRef (Some innerExceptionAddr))
            |> AllocatedNonArrayObject.SetFieldById typeNameField (CliType.ObjectRef (Some typeNameAddr))
            |> AllocatedNonArrayObject.SetFieldById
                hresultField
                (CliType.Numeric (CliNumericType.Int32 (ExceptionHResults.lookup "System.TypeInitializationException")))

        let state =
            { state with
                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
            }

        addr, tieHandle, state

    /// Resolve a MetadataToken (TypeDefinition, TypeReference, or TypeSpecification) to a TypeDefn,
    /// together with the assembly the type was resolved in.
    let resolveTypeMetadataToken
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (activeAssy : DumpedAssembly)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (token : MetadataToken)
        : IlMachineState * TypeDefn * DumpedAssembly
        =
        match token with
        | MetadataToken.TypeDefinition h ->
            let state, ty = lookupTypeDefn baseClassTypes state activeAssy h
            state, ty, activeAssy
        | MetadataToken.TypeReference ref ->
            lookupTypeRef loggerFactory baseClassTypes state activeAssy typeGenerics ref
        | MetadataToken.TypeSpecification spec -> state, activeAssy.TypeSpecs.[spec].Signature, activeAssy
        | m -> failwith $"unexpected type metadata token {m}"

    /// Get the metadata row directly represented by this concrete handle.
    /// Structural arrays, byrefs, and pointers have no direct TypeDef row; callers that are walking
    /// inheritance should ask for their base type explicitly.
    let tryGetConcreteTypeInfo
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : (ConcreteType<ConcreteTypeHandle> * TypeInfo<GenericParamFromMetadata, TypeDefn>) option
        =
        match concreteType with
        | ConcreteTypeHandle.Concrete _ ->
            match AllConcreteTypes.lookup concreteType state.ConcreteTypes with
            | None -> failwith $"ConcreteTypeHandle {concreteType} not found in AllConcreteTypes"
            | Some concreteType ->
                let assembly = state._LoadedAssemblies.[concreteType.Identity.AssemblyFullName]

                Some (concreteType, assembly.TypeDefs.[concreteType.Identity.TypeDefinition.Get])
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> None

    let requiredOwnInstanceFieldId
        (state : IlMachineState)
        (declaringType : ConcreteTypeHandle)
        (fieldName : string)
        : FieldId
        =
        match tryGetConcreteTypeInfo state declaringType with
        | Some (_, typeInfo) ->
            FieldIdentity.requiredOwnInstanceField typeInfo fieldName
            |> FieldIdentity.fieldId declaringType
        | None ->
            failwith
                $"requiredOwnInstanceFieldId: %O{declaringType} has no TypeDef row; cannot resolve field '%s{fieldName}'"

    /// Check whether the concrete type `objType` is assignable to `targetType`.
    /// Walks the base type chain and checks implemented interfaces at each level.
    /// Returns true if objType = targetType, or targetType is a base class of objType,
    /// or targetType is an interface implemented by objType or any of its base classes.
    let rec isConcreteTypeAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (objType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if objType = targetType then
            state, true
        else

        let isReferenceTypeHandle (state : IlMachineState) (handle : ConcreteTypeHandle) : bool =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> true
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _ -> false
            | ConcreteTypeHandle.Concrete _ ->
                match tryGetConcreteTypeInfo state handle with
                | Some (_, typeInfo) -> DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
                | None -> failwith $"isReferenceTypeHandle: concrete type handle %O{handle} has no TypeDef row"

        let arrayShape (handle : ConcreteTypeHandle) : (ConcreteTypeHandle * int option) option =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero element -> Some (element, None)
            | ConcreteTypeHandle.Array (element, rank) -> Some (element, Some rank)
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _ -> None

        let rec checkInterfaces (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match tryGetConcreteTypeInfo state current with
            | None ->
                // This node has no metadata-declared interfaces. The caller decides whether to walk its base.
                state, false
            | Some (ct, typeInfo) ->
                let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]

                ((state, false), typeInfo.ImplementedInterfaces)
                ||> Seq.fold (fun (state, found) impl ->
                    if found then
                        state, true
                    else
                        let implAssy =
                            match state.LoadedAssembly impl.RelativeToAssembly with
                            | Some a -> a
                            | None ->
                                // Assembly not yet loaded; use the assembly we already have since
                                // RelativeToAssembly is set to the assembly containing the type definition.
                                assy

                        let state, implTypeDefn, implResolvedAssy =
                            resolveTypeMetadataToken
                                loggerFactory
                                baseClassTypes
                                state
                                implAssy
                                ct.Generics
                                impl.InterfaceHandle

                        let state, implHandle =
                            concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                implResolvedAssy.Name
                                ct.Generics
                                ImmutableArray.Empty
                                implTypeDefn

                        // Check exact match, then recurse into the interface's own parent interfaces.
                        walk state implHandle
                )

        and walkBase (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            match current with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _ -> state, false
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ ->
                let state, baseType =
                    resolveBaseConcreteType loggerFactory baseClassTypes state current

                match baseType with
                | None ->
                    // Every reference type (including interfaces) is assignable to System.Object.
                    match targetType with
                    | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, true
                    | _ -> state, false
                | Some parent -> walk state parent

        and walk (state : IlMachineState) (current : ConcreteTypeHandle) : IlMachineState * bool =
            if current = targetType then
                state, true
            else

            match tryGetConcreteTypeInfo state current with
            | None -> walkBase state current
            | Some (currentCt, _) ->
                // If two types share the same definition but differ in generics, check whether
                // variance could apply. Classes are invariant so the answer is definitively false.
                // Interfaces and delegates can have variance, so we must crash rather than guess.
                let sameDefnDifferentGenerics =
                    match AllConcreteTypes.lookup targetType state.ConcreteTypes with
                    | Some targetCt when
                        currentCt.Identity = targetCt.Identity
                        && currentCt.Generics <> targetCt.Generics
                        ->
                        Some targetCt
                    | _ -> None

                match sameDefnDifferentGenerics with
                | Some targetCt ->
                    let targetAssy = state._LoadedAssemblies.[targetCt.Identity.AssemblyFullName]
                    let targetTypeInfo = targetAssy.TypeDefs.[targetCt.Identity.TypeDefinition.Get]

                    let hasVariantGenericParams =
                        targetTypeInfo.Generics
                        |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

                    if hasVariantGenericParams then
                        failwith $"TODO: generic variance check needed: is %O{currentCt} assignable to %O{targetCt}?"
                    else
                        // All generic parameters are invariant; same definition + different generics = not assignable.
                        state, false
                | None ->
                    let state, interfaceMatch = checkInterfaces state current

                    if interfaceMatch then
                        state, true
                    else
                        walkBase state current

        let checkArraySpecificRules
            (state : IlMachineState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : IlMachineState * bool option
            =
            match arrayShape objType, arrayShape targetType with
            | Some (objElement, objShape), Some (targetElement, targetShape) ->
                if objShape <> targetShape then
                    state, Some false
                elif objElement = targetElement then
                    state, Some true
                elif
                    isReferenceTypeHandle state objElement
                    && isReferenceTypeHandle state targetElement
                then
                    let state, elementAssignable =
                        isConcreteTypeAssignableTo loggerFactory baseClassTypes state objElement targetElement

                    state, Some elementAssignable
                else
                    // TODO: ECMA-335 permits some value-type array assignments when
                    // the element types have equivalent underlying primitive types
                    // (for example int[] <-> uint[]). Model that rule explicitly
                    // before broadening this branch.
                    state, Some false
            | Some _, None -> state, None
            | None, _ -> failwith $"checkArraySpecificRules called with non-array source %O{objType}"

        match objType with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            let state, assignable = walk state objType

            if assignable then
                state, assignable
            else
                match checkArraySpecificRules state objType targetType with
                | state, Some assignable -> state, assignable
                | state, None ->
                    let targetTypeInfo = tryGetConcreteTypeInfo state targetType

                    let targetNeedsArraySpecificRules =
                        match targetType with
                        | ConcreteTypeHandle.OneDimArrayZero _
                        | ConcreteTypeHandle.Array _ -> true
                        | ConcreteTypeHandle.Concrete _
                        | ConcreteTypeHandle.Byref _
                        | ConcreteTypeHandle.Pointer _ ->
                            match targetTypeInfo with
                            | Some (targetCt, targetTypeInfo) ->
                                targetTypeInfo.IsInterface && not targetCt.Generics.IsEmpty
                            | None -> false

                    if targetNeedsArraySpecificRules then
                        failwith $"TODO: array assignability check from %O{objType} to %O{targetType}"
                    else
                        state, false
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _ -> walk state objType
