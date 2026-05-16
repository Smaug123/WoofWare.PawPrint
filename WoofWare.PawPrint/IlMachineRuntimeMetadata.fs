namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module IlMachineRuntimeMetadata =
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
            |> IlMachineTypeResolution.concretizeType
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
                (fun fields state -> IlMachineThreadState.allocateManagedObject runtimeType fields state)
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
            IlMachineTypeResolution.concretizeFieldDeclaringType
                loggerFactory
                baseClassTypes
                declaringTypeWithGenerics
                state

        let state, runtimeFieldInfoStub =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    baseClassTypes.Corelib.Name
                    baseClassTypes.RuntimeFieldInfoStub.TypeDefHandle,
                SignatureTypeKind.Class
            )
            |> IlMachineTypeResolution.concretizeType
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
                (fun fields state -> IlMachineThreadState.allocateManagedObject runtimeFieldInfoStub fields state)
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
            |> IlMachineTypeResolution.concretizeType
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
                (fun fields state -> IlMachineThreadState.allocateManagedObject runtimeMethodInfoStub fields state)
                method
                state.MethodHandles

        let state =
            { state with
                MethodHandles = reg
            }

        result, state

    let evalStackValueToObjectRef
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (value : EvalStackValue)
        : ManagedHeapAddress option
        =
        match value with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | EvalStackValue.ManagedPointer src ->
            match IlMachineManagedByref.readManagedByref baseClassTypes state src with
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
            IlMachineTypeResolution.resolveTypeFromRef loggerFactory activeAssy ref typeGenerics state

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
                IlMachineTypeResolution.loadAssembly loggerFactory referencedInAssembly handle state

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
                IlMachineTypeResolution.resolveTypeFromRef
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
                |> IlMachineTypeResolution.concretizeType
                    loggerFactory
                    baseClassTypes
                    state
                    baseClassTypes.Corelib.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty

            state, Some arrayHandle
        | ConcreteTypeHandle.FunctionPointer _ ->
            failwith
                $"TODO: resolveBaseConcreteType: function pointer types (%O{concreteType}) not yet supported; the runtime base type is System.ValueType but the lookup path needs adjusting"
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
                        IlMachineTypeResolution.concretizeType
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
                    IlMachineTypeResolution.cliTypeZeroOf
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
                        MarshallingDescriptor = field.MarshallingDescriptor
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

        let dataAddr, state = IlMachineThreadState.allocateStringData contents.Length state
        let state = IlMachineThreadState.setStringData dataAddr contents state

        let state, stringType =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.String
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let fields =
            // `_firstChar` is intentionally omitted: its canonical storage is
            // `StringArrayData[dataOffset]`, and `RuntimeFieldProjection` synthesises
            // ldfld/ldflda/stfld access against that side-table. Materialising a
            // separate field cell would create a second source of truth for the
            // same char and historically led to drift after `stfld _firstChar`
            // (e.g. CoreLib's `String.CreateFromChar`'s `result._firstChar = c`)
            // bypassed `setStringChar` and left the byte view at NUL.
            let stringLengthField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.String "_stringLength"

            [
                FieldIdentity.cliField
                    stringType
                    stringLengthField
                    (CliType.Numeric (CliNumericType.Int32 contents.Length))
                    (AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32)
            ]
            |> CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                stringType
                Layout.Default
                (CharSetMetadata.ofTypeAttributes baseClassTypes.String.TypeAttributes)

        let addr, state = IlMachineThreadState.allocateManagedObject stringType fields state

        let state =
            { state with
                ManagedHeap =
                    state.ManagedHeap
                    |> ManagedHeap.recordStringContents addr contents
                    |> ManagedHeap.recordStringDataOffset addr dataAddr
            }

        addr, state

    /// Return the address of the canonical empty managed string, allocating it lazily
    /// on first request. This is the single shared instance that backs both `ldstr ""`
    /// and `ldsfld System.String::Empty`, satisfying the CLR's invariant that
    /// `ReferenceEquals(string.Empty, "")` holds.
    let internCanonicalEmptyString
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match state.InternedStrings.TryGetValue "" with
        | true, addr -> addr, state
        | false, _ ->
            let addr, state = allocateManagedString loggerFactory baseClassTypes "" state

            addr,
            { state with
                InternedStrings = state.InternedStrings.Add ("", addr)
            }

    /// Allocate a sentinel 2-entry CastCache backing array shaped to match what the native
    /// EE's `CastCache::Initialize` writes into `CastHelpers::s_table` at startup. The
    /// array stays a forever-empty sentinel: managed `CastCache.TryGet` reads `version == 0`
    /// on every probe and returns `CastResult.MaybeCast`, so callers fall through to the
    /// slow path that PawPrint's type system already handles. `CastCache.TrySet` would
    /// take the `TableMask == 1` early-return on the sentinel and never mutate it, but
    /// in any case PawPrint never invokes the instance `TrySet` (CoreCLR only writes to
    /// the cache from native code).
    ///
    /// Layout under managed `CastCache.CreateCastCache(2)` on a 64-bit guest:
    /// * `int32[]` length = `(size + 1) * sizeof(CastCacheEntry) / 4` = `3 * 24 / 4` = 18.
    /// * `TableData(table)` is `ref array[0]`: it loads `RawData::Data` (which on an array
    ///   points at the length field) and then skips `sizeof(nint)` bytes, landing at the
    ///   first element. So the `HashShift`/`TableMask`/`VictimCounter` accessors in
    ///   `CastCache.cs:113-130` index from `array[0]`, putting the auxiliary header at
    ///   element indices 0, 1, 2. The remaining ints are zero-initialised — in particular
    ///   indices 3..5 are the unused tail of the aux-slot `CastCacheEntry`, and indices
    ///   6..17 are entries 0 and 1 (zero `_version` triggers the immediate `break` in
    ///   `TryGet`).
    /// * `hashShift = BitOperations.LeadingZeroCount((nuint)1)` = 63 on 64-bit; PawPrint
    ///   targets 64-bit guests exclusively, so we hard-code 63. This bounds the initial
    ///   `KeyToBucket` index to {0, 1}, keeping `Element(tableData, k)` inside the
    ///   `int[18]` table.
    /// * `tableMask = size - 1 = 1`.
    let internCastCacheSentinelTable
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, int32Handle =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.Int32
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let arrayTypeHandle = ConcreteTypeHandle.OneDimArrayZero int32Handle

        let zeroInt () : CliType =
            CliType.Numeric (CliNumericType.Int32 0)

        let addr, state =
            IlMachineThreadState.allocateArray arrayTypeHandle zeroInt 18 state

        // Auxiliary header: hashShift = 63 (LeadingZeroCount((nuint)1) on 64-bit),
        // tableMask = size - 1 = 1, victimCounter = 0 (already zero, written for clarity).
        // These live at element indices 0, 1, 2 because `CastCache.TableData` resolves
        // `GetRawData(table) + sizeof(nint)` to the first int element — `GetRawData` on
        // arrays returns a pointer at `RawArrayData.Length`, so the 8-byte skip walks past
        // `Length` + 64-bit padding and lands at element 0.
        let state =
            state
            |> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 63)) 0
            |> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 1)) 1
            |> IlMachineThreadState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 0)) 2

        addr, state

    let private concreteTypeFullName (state : IlMachineState) (ty : ConcreteType<ConcreteTypeHandle>) : string =
        match state.LoadedAssembly ty.Assembly with
        | Some assy -> Assembly.fullName assy ty.Identity
        | None when String.IsNullOrEmpty ty.Namespace -> ty.Name
        | None -> $"{ty.Namespace}.{ty.Name}"

    /// `Type.Name` for the BCL primitive types — CoreCLR's stack-trace rendering emits these
    /// rather than the IL keyword forms (e.g. `"Int32"`, not `"int32"`).
    let private primitiveBclName (pt : PrimitiveType) : string =
        match pt with
        | PrimitiveType.Boolean -> "Boolean"
        | PrimitiveType.Char -> "Char"
        | PrimitiveType.SByte -> "SByte"
        | PrimitiveType.Byte -> "Byte"
        | PrimitiveType.Int16 -> "Int16"
        | PrimitiveType.UInt16 -> "UInt16"
        | PrimitiveType.Int32 -> "Int32"
        | PrimitiveType.UInt32 -> "UInt32"
        | PrimitiveType.Int64 -> "Int64"
        | PrimitiveType.UInt64 -> "UInt64"
        | PrimitiveType.Single -> "Single"
        | PrimitiveType.Double -> "Double"
        | PrimitiveType.String -> "String"
        | PrimitiveType.TypedReference -> "TypedReference"
        | PrimitiveType.IntPtr -> "IntPtr"
        | PrimitiveType.UIntPtr -> "UIntPtr"
        | PrimitiveType.Object -> "Object"

    /// Render a parameter's type using the CLR's stack-trace convention: just `Type.Name`.
    /// `Type.Name` for a constructed generic such as `List<int>` is `"List`1"` — the
    /// instantiation is NOT appended (verified against `typeof(List<int>).Name` in CoreCLR),
    /// so this differs from full reflection name rendering (cf. `NativeRuntimeType.fs`
    /// `concreteTypeHandleName`, which appends `[args]` under FormatNamespace/Assembly).
    /// Array, pointer, and byref wrappers do show up in `Type.Name`, so we render those.
    ///
    /// Generic-method and generic-type parameter references resolve to the parameter's
    /// declared name (e.g. `TC`, `TM`) via the supplied name arrays. This mirrors
    /// CoreCLR — stack frames captured for shared-generic JITted code keep the formal
    /// parameter names rather than the call-site substitution, so a call to
    /// `Container<int>.Throw<string>(int, string)` renders as `Throw[TM](TC c, TM m)`.
    let rec private renderTypeDefnForStackFrame
        (state : IlMachineState)
        (typeGenericNames : string array)
        (methodGenericNames : string array)
        (ty : TypeDefn)
        : string
        =
        let recurse = renderTypeDefnForStackFrame state typeGenericNames methodGenericNames

        match ty with
        | TypeDefn.PrimitiveType pt -> primitiveBclName pt
        | TypeDefn.Void -> "Void"
        | TypeDefn.Byref inner -> recurse inner + "&"
        | TypeDefn.Pointer inner -> recurse inner + "*"
        | TypeDefn.OneDimensionalArrayLowerBoundZero inner -> recurse inner + "[]"
        | TypeDefn.Array (inner, rank) ->
            let dims = if rank <= 1 then "*" else System.String (',', rank - 1)
            recurse inner + "[" + dims + "]"
        | TypeDefn.Pinned inner -> recurse inner
        // Modified types: render the underlying (post-modifier) type so optional/required
        // custom modifiers (e.g. `modreq IsExternalInit`) don't leak into the printed name.
        | TypeDefn.Modified (_, afterMod, _) -> recurse afterMod
        // CLR `Type.Name` on `List<int>` is `"List`1"`; the instantiation is dropped.
        | TypeDefn.GenericInstantiation (generic, _args) -> recurse generic
        | TypeDefn.GenericTypeParameter index ->
            if index >= 0 && index < typeGenericNames.Length then
                typeGenericNames.[index]
            else
                // The signature referenced a type-generic position the declaring type
                // doesn't declare — bad metadata. Render a debuggable placeholder rather
                // than crash the stack-trace path.
                $"!{index}"
        | TypeDefn.GenericMethodParameter index ->
            if index >= 0 && index < methodGenericNames.Length then
                methodGenericNames.[index]
            else
                $"!!{index}"
        | TypeDefn.FromReference (typeRef, _) -> typeRef.Name
        | TypeDefn.FromDefinition (identity, _) ->
            match state.LoadedAssembly identity.Assembly with
            | None -> "<unresolved>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue identity.TypeDefinition.Get with
                | true, ti -> ti.Name
                | false, _ -> "<unresolved>"
        // CoreCLR's TypeString::AppendType emits the empty string for FnPtr when FormatNamespace
        // is unset; stack-trace parameter rendering uses the no-namespace form, so match that.
        | TypeDefn.FunctionPointer _ -> ""

    let private renderExceptionStackFrame
        (state : IlMachineState)
        (frame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : string
        =
        let typeName = concreteTypeFullName state frame.Method.DeclaringType

        // The method's defining assembly is the assembly that contains its declaring type;
        // both the type-level and the method-level generic-parameter names live in there.
        let declaringAssembly = state.LoadedAssembly frame.Method.DeclaringType.Assembly

        let typeGenericNames : string array =
            match declaringAssembly with
            | None -> Array.empty
            | Some assy ->
                match assy.TypeDefs.TryGetValue frame.Method.DeclaringType.Definition.Get with
                | true, ti -> ti.Generics |> Seq.map (fun (gp, _) -> gp.Name) |> Seq.toArray
                | false, _ -> Array.empty

        let methodGenericNames : string array =
            match declaringAssembly with
            | None -> Array.empty
            | Some assy ->
                match assy.Methods.TryGetValue frame.Method.Handle with
                | true, m -> m.Generics |> Seq.map (fun (gp, _) -> gp.Name) |> Seq.toArray
                | false, _ -> Array.empty

        // CoreCLR renders the method's generic argument list as `[T1,T2]` (comma-separated,
        // no space) between the method name and the parameter list. Non-generic methods get
        // no `[...]` suffix at all.
        let methodGenericsText =
            if methodGenericNames.Length = 0 then
                ""
            else
                "[" + (methodGenericNames |> String.concat ",") + "]"

        // Metadata Parameters skip SequenceNumber=0 (`this` / ref return), so signature index `i`
        // pairs with the parameter whose SequenceNumber is `i + 1` regardless of static-ness.
        let parameterByPosition =
            frame.Method.Parameters
            |> Seq.map (fun p -> p.SequenceNumber, p.Name)
            |> Map.ofSeq

        // Walk the raw (TypeDefn) signature rather than the concretized one so
        // `GenericTypeParameter`/`GenericMethodParameter` references survive to render
        // as their formal names (`TC`, `TM`, etc.).
        let paramText =
            frame.Method.RawSignature.ParameterTypes
            |> List.mapi (fun i ty ->
                let typeStr =
                    renderTypeDefnForStackFrame state typeGenericNames methodGenericNames ty

                match Map.tryFind (i + 1) parameterByPosition with
                | Some name when not (String.IsNullOrEmpty name) -> $"%s{typeStr} %s{name}"
                | _ -> typeStr
            )
            |> String.concat ", "

        $"   at %s{typeName}.%s{frame.Method.Name}%s{methodGenericsText}(%s{paramText})"

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
            |> IlMachineTypeResolution.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty

        let state, allFields =
            collectAllInstanceFields loggerFactory baseClassTypes state threadTypeHandle

        let fields =
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                threadTypeHandle
                threadTypeInfo.Layout
                (CharSetMetadata.ofTypeAttributes threadTypeInfo.TypeAttributes)
                allFields

        let addr, state =
            IlMachineThreadState.allocateManagedObject threadTypeHandle fields state

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
            IlMachineTypeResolution.concretizeType
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
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                tieHandle
                tieTypeInfo.Layout
                (CharSetMetadata.ofTypeAttributes tieTypeInfo.TypeAttributes)
                allFields

        let addr, state = IlMachineThreadState.allocateManagedObject tieHandle fields state

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

    /// Synthesize a TargetInvocationException wrapping the given inner exception object.
    /// Allocates the exception on the heap with zero-initialized fields (constructor is NOT run).
    /// Sets the _innerException, _message and _HResult fields on the base Exception to match what
    /// `new TargetInvocationException(inner)` would have done in CoreCLR (whose base ctor sets
    /// `_message` to the SR.Arg_TargetInvocationException string). Returns the heap address, the
    /// ConcreteTypeHandle, and the updated state.
    /// See https://github.com/dotnet/runtime/blob/HEAD/src/libraries/System.Private.CoreLib/src/System/Reflection/TargetInvocationException.cs
    let synthesizeTargetInvocationException
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (innerExceptionAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedHeapAddress * ConcreteTypeHandle * IlMachineState
        =
        let tieTypeInfo = baseClassTypes.TargetInvocationException

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies tieTypeInfo

        let state, tieHandle =
            IlMachineTypeResolution.concretizeType
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
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                tieHandle
                tieTypeInfo.Layout
                (CharSetMetadata.ofTypeAttributes tieTypeInfo.TypeAttributes)
                allFields

        let addr, state = IlMachineThreadState.allocateManagedObject tieHandle fields state

        // CoreCLR's TargetInvocationException(Exception) ctor calls
        //     base(SR.Arg_TargetInvocationException, inner)
        // which sets `_message` to the canonical string below. Bypassing the ctor would leave
        // `_message` null and divert `Message` / `ToString()` to the
        // "Exception of type '...' was thrown." fallback in Exception.Message, so allocate and
        // store the message explicitly.
        let messageAddr, state =
            allocateManagedString
                loggerFactory
                baseClassTypes
                "Exception has been thrown by the target of an invocation."
                state

        let heapObj = ManagedHeap.get addr state.ManagedHeap

        let exceptionHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Exception

        let innerExceptionField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_innerException"
            |> FieldIdentity.fieldId exceptionHandle

        let messageField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_message"
            |> FieldIdentity.fieldId exceptionHandle

        let hresultField =
            FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_HResult"
            |> FieldIdentity.fieldId exceptionHandle

        let heapObj =
            heapObj
            |> AllocatedNonArrayObject.SetFieldById innerExceptionField (CliType.ObjectRef (Some innerExceptionAddr))
            |> AllocatedNonArrayObject.SetFieldById messageField (CliType.ObjectRef (Some messageAddr))
            |> AllocatedNonArrayObject.SetFieldById
                hresultField
                (CliType.Numeric (
                    CliNumericType.Int32 (ExceptionHResults.lookup "System.Reflection.TargetInvocationException")
                ))

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
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None

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
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> false
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
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> None

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
                            IlMachineTypeResolution.concretizeType
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
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, false
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

        // Returns true if `handle` is a CLR enum value type — a nominal type whose
        // immediate runtime base is `System.Enum`. Used by the array-element
        // assignability rule below to detect cases where ECMA-335 / CoreCLR
        // enum-underlying-type equivalence (e.g. `MyEnum : int` ↔ `int`, or two
        // enums sharing an underlying integer) might permit a store.
        let isEnumValueType (state : IlMachineState) (handle : ConcreteTypeHandle) : IlMachineState * bool =
            match handle with
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> state, false
            | ConcreteTypeHandle.Concrete _ ->
                let state, baseHandle =
                    resolveBaseConcreteType loggerFactory baseClassTypes state handle

                match baseHandle with
                | None -> state, false
                | Some bh ->
                    match AllConcreteTypes.lookup bh state.ConcreteTypes with
                    | Some baseTy -> state, baseTy.Identity = baseClassTypes.Enum.Identity
                    | None -> state, false

        // For an enum `ConcreteTypeHandle`, return the `ConcreteTypeHandle` of its
        // underlying integer type by concretising the signature of its sole instance
        // field (`value__`, the CLR-reserved name for the integer slot of an enum;
        // ECMA-335 §II.14.3). Returns `None` if `handle` is not an enum, has no TypeDef
        // row, or — defensively — has a malformed Fields list. The caller is expected
        // to have first verified enum-ness via `isEnumValueType`; this helper does the
        // metadata read.
        let enumUnderlyingHandle
            (state : IlMachineState)
            (handle : ConcreteTypeHandle)
            : (IlMachineState * ConcreteTypeHandle) option
            =
            match tryGetConcreteTypeInfo state handle with
            | None -> None
            | Some (ct, typeInfo) ->
                let instanceFields =
                    typeInfo.Fields
                    |> List.filter (fun f -> not (f.Attributes.HasFlag FieldAttributes.Static))

                match instanceFields with
                | [ valueField ] when valueField.Name = "value__" ->
                    let assy = state._LoadedAssemblies.[ct.Identity.AssemblyFullName]

                    let state, underlying =
                        IlMachineTypeResolution.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            assy.Name
                            ct.Generics
                            ImmutableArray.Empty
                            valueField.Signature

                    Some (state, underlying)
                | _ -> None

        // ECMA-335 III.8.7 / CoreCLR `GetNormalizedIntegralArrayElementType`:
        // signed and unsigned primitive integers of equal width are interchangeable
        // as array element types (`int[]` ↔ `uint[]`, `short[]` ↔ `ushort[]`, etc.).
        // Returns `Some normalizedIdentity` when `handle` is one of those primitive
        // integers; otherwise `None`. Floating-point, Boolean, and Char have no
        // normalization partners.
        let normalizedPrimitiveIntegerIdentity (handle : ConcreteTypeHandle) : ResolvedTypeIdentity option =
            match tryGetConcreteTypeInfo state handle with
            | Some (ct, _) when ct.Generics.IsEmpty ->
                let id = ct.Identity

                if id = baseClassTypes.SByte.Identity || id = baseClassTypes.Byte.Identity then
                    Some baseClassTypes.SByte.Identity
                elif id = baseClassTypes.Int16.Identity || id = baseClassTypes.UInt16.Identity then
                    Some baseClassTypes.Int16.Identity
                elif id = baseClassTypes.Int32.Identity || id = baseClassTypes.UInt32.Identity then
                    Some baseClassTypes.Int32.Identity
                elif id = baseClassTypes.Int64.Identity || id = baseClassTypes.UInt64.Identity then
                    Some baseClassTypes.Int64.Identity
                elif id = baseClassTypes.IntPtr.Identity || id = baseClassTypes.UIntPtr.Identity then
                    Some baseClassTypes.IntPtr.Identity
                else
                    None
            | _ -> None

        // ECMA-335 III.4.3 / CoreCLR `CanCastParam`: for value-typed array elements the
        // assignment-compatibility relation reduces to "the normalised integer identity
        // of each element matches". The normalised identity of a primitive integer is
        // the signed canonical (see `normalizedPrimitiveIntegerIdentity`); the normalised
        // identity of an enum is the normalised identity of its underlying integer.
        // Anything else (`float`, `double`, `bool`, `char`, non-integer struct) has no
        // normalised identity. Returns `None` when the input has no equivalence partner;
        // returns `Some id` otherwise.
        let valueElementNormalisedIdentity
            (state : IlMachineState)
            (handle : ConcreteTypeHandle)
            : IlMachineState * ResolvedTypeIdentity option
            =
            let state, isEnum = isEnumValueType state handle

            if isEnum then
                match enumUnderlyingHandle state handle with
                | None -> state, None
                | Some (state, underlying) -> state, normalizedPrimitiveIntegerIdentity underlying
            else
                state, normalizedPrimitiveIntegerIdentity handle

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
                else
                    let objIsRef = isReferenceTypeHandle state objElement
                    let targetIsRef = isReferenceTypeHandle state targetElement

                    if objIsRef && targetIsRef then
                        let state, elementAssignable =
                            isConcreteTypeAssignableTo loggerFactory baseClassTypes state objElement targetElement

                        state, Some elementAssignable
                    elif objIsRef <> targetIsRef then
                        // One element is reference-typed and the other is value-typed.
                        // ECMA-335 covariance applies only across reference types, and
                        // primitive/enum equivalence applies only between value types,
                        // so the answer is definitively non-assignable here.
                        state, Some false
                    else
                        // Both element types are value-typed. ECMA-335 III.4.3 /
                        // CoreCLR `CanCastParam` reduces the relation to
                        // "normalised integer identities match", combining two rules:
                        //   (a) signed/unsigned primitive integers of equal width
                        //       (`int[]` ↔ `uint[]`, etc.) — `GetNormalizedIntegralArrayElementType`.
                        //   (b) enum equivalence with the underlying integer or with
                        //       another enum that shares the same underlying integer
                        //       (e.g. `MyEnum : int` ↔ `int[]` ↔ `uint[]`, or `MyEnum:int` ↔ `OtherEnum:int`).
                        // `valueElementNormalisedIdentity` looks the underlying integer
                        // up for enums and applies `normalizedPrimitiveIntegerIdentity`
                        // to the result, giving a definitive yes/no answer.
                        let state, objNormalised = valueElementNormalisedIdentity state objElement
                        let state, targetNormalised = valueElementNormalisedIdentity state targetElement

                        match objNormalised, targetNormalised with
                        | Some a, Some b when a = b -> state, Some true
                        | _, _ -> state, Some false
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
                        | ConcreteTypeHandle.Pointer _
                        | ConcreteTypeHandle.FunctionPointer _ ->
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
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> walk state objType
