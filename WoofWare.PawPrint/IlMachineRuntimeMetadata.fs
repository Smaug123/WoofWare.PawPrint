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

    /// Returns a System.RuntimeFieldHandle for the given field, observed on
    /// `declaringType`. The caller is responsible for supplying the correct
    /// instantiation context: in CoreCLR, `typeof(G<int>).GetField(...).FieldHandle`
    /// and `typeof(G<>).GetField(...).FieldHandle` are observably different — each
    /// carries its own declaring `RuntimeTypeHandle` — so this helper preserves the
    /// distinction by keying on the full target. Type-parameter targets are rejected
    /// by the registry because they cannot own a field.
    let getOrAllocateField
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringAssy : AssemblyName)
        (declaringType : RuntimeTypeHandleTarget)
        (fieldHandle : FieldDefinitionHandle)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
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
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName
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

    /// True iff `ty` references any `GenericTypeParameter` / `GenericMethodParameter`. The
    /// open-generic source cast walk uses this to decide whether a base/interface edge can be
    /// materialised to a closed `ConcreteTypeHandle` (when false) or only stripped to its
    /// definition identity for continued identity-walking (when true). A method generic
    /// parameter has no legitimate appearance in a type definition's base or interfaces,
    /// but treating it as unbound is the safe default.
    let rec containsAnyGenericParameter (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.Array (element, _)
        | TypeDefn.Pinned element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.OneDimensionalArrayLowerBoundZero element -> containsAnyGenericParameter element
        | TypeDefn.Modified m ->
            containsAnyGenericParameter m.Unmodified
            || containsAnyGenericParameter m.Modifier
        | TypeDefn.GenericInstantiation (generic, args) ->
            containsAnyGenericParameter generic
            || (args |> Seq.exists containsAnyGenericParameter)
        | TypeDefn.FunctionPointer signature ->
            let returnContains =
                match signature.ReturnType with
                | MethodReturnType.Void -> false
                | MethodReturnType.Returns ret -> containsAnyGenericParameter ret

            returnContains
            || (signature.ParameterTypes |> List.exists containsAnyGenericParameter)
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void -> false

    /// Given a `RuntimeTypeHandleTarget`, resolve and return its parent's
    /// `RuntimeTypeHandleTarget`. Returns `None` only at `System.Object`.
    ///
    /// `Closed` defers to `resolveBaseConcreteType` and rewraps as `Closed`.
    ///
    /// `OpenGenericTypeDefinition` represents CoreCLR's canonical MethodTable
    /// for a generic typedef (i.e. `G<__Canon>`). Its parent MT is the parent
    /// type after substituting `__Canon` for each of `G`'s parameters. The
    /// case where the base type doesn't mention `G`'s parameters at all (e.g.
    /// `class G<T> : object`, `class G<T> : Base<int>`) is fully closed and
    /// concretizes with no generics. The shared-base case (e.g.
    /// `class G<T> : Base<T>` → parent is `Base<__Canon>` →
    /// `OpenGenericTypeDefinition Base`) is not yet implemented and surfaces
    /// loudly; the immediate callers (`MethodTable::ParentMethodTable` lookups
    /// driven by reflection on open typedefs) only exercise the closed-parent
    /// case today.
    ///
    /// `GenericParameter` and `MethodGenericParameter` are TypeDescs in
    /// CoreCLR and carry no MethodTable; asking for their parent is a bug.
    let resolveBaseRuntimeTypeHandleTarget
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * RuntimeTypeHandleTarget option
        =
        match target with
        | RuntimeTypeHandleTarget.Closed handle ->
            let state, parent =
                resolveBaseConcreteType loggerFactory baseClassTypes state handle

            state, parent |> Option.map RuntimeTypeHandleTarget.Closed
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            let assy =
                match state.LoadedAssembly identity.Assembly with
                | Some assembly -> assembly
                | None ->
                    failwith
                        $"resolveBaseRuntimeTypeHandleTarget: assembly %s{identity.AssemblyFullName} not loaded for open generic typedef %O{identity.TypeDefinition.Get}"

            let typeInfo = assy.TypeDefs.[identity.TypeDefinition.Get]

            match typeInfo.BaseType with
            | None -> state, None
            | Some baseTypeInfo ->
                let state, baseAssy, baseTypeDefn =
                    resolveBaseTypeInfo loggerFactory baseClassTypes state assy baseTypeInfo

                if containsAnyGenericParameter baseTypeDefn then
                    failwith
                        $"TODO: resolveBaseRuntimeTypeHandleTarget for open generic typedef %O{identity.TypeDefinition.Get} in %s{identity.AssemblyFullName}: base type %O{baseTypeDefn} references generic parameters (shared/canonical parent); only closed parents are supported today"
                else
                    let state, baseHandle =
                        IlMachineTypeResolution.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseAssy.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            baseTypeDefn

                    state, Some (RuntimeTypeHandleTarget.Closed baseHandle)
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwith
                $"resolveBaseRuntimeTypeHandleTarget: refused for generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}: TypeDescs have no MethodTable in CoreCLR"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"resolveBaseRuntimeTypeHandleTarget: refused for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}: TypeDescs have no MethodTable in CoreCLR"

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

        let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName
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

        // An `[InlineArray(N)]` type's storage is N repeats of its one declared field; see
        // `InlineArrayStorage.expand`. This site is reached for real by `newobj` on a struct with a
        // constructor, which is exactly how CoreLib builds `TwoObjects` for `SR.Format`.
        //
        // Unlike the other expansion sites, this one also walks *reference* types — for which the
        // attribute is inert; see `InlineArrayStorage.effectiveLength`.
        let ownFields =
            List.rev ownFields
            |> InlineArrayStorage.expand
                (fun () -> $"%s{typeInfo.Namespace}.%s{typeInfo.Name}")
                typeInfo.Layout
                (InlineArrayStorage.effectiveLength
                    (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies typeInfo)
                    typeInfo.InlineArrayLength)

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
        // Modified types: render the type the modifier is attached to, so optional/required
        // custom modifiers (e.g. the `modreq InAttribute` C# emits on an `in` parameter of a
        // virtual method) don't leak into the printed name.
        | TypeDefn.Modified m -> recurse m.Unmodified
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

    /// Write `_message` on an already-allocated exception object.
    ///
    /// `ExceptionDispatching.allocateRuntimeException` only zero-initialises the object and does
    /// not run any constructor, so runtime-synthesised exceptions otherwise carry a null `_message`
    /// and `Exception.Message` falls back to the generic "Exception of type X was thrown" string.
    /// Where the CLR would have passed a specific resource string to the constructor, call this so
    /// a guest that catches the exception and reads `.Message` sees what it would really see.
    let setExceptionMessage
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (exceptionAddr : ManagedHeapAddress)
        (message : string)
        (state : IlMachineState)
        : IlMachineState
        =
        match
            state.ManagedHeap.NonArrayObjects |> Map.tryFind exceptionAddr,
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Exception.Identity
        with
        | Some _, Some exceptionHandle ->
            let messageAddr, state =
                allocateManagedString loggerFactory baseClassTypes message state

            let messageField =
                FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_message"
                |> FieldIdentity.fieldId exceptionHandle

            IlMachineThreadState.setInstanceFieldById
                exceptionAddr
                messageField
                (CliType.ObjectRef (Some messageAddr))
                state
        // Mirrors `setExceptionStackTraceString`: skeletal states in low-level dispatch tests may
        // lack either piece, and there is nothing to project into in that case.
        | None, _
        | _, None -> state

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
            | Some _, Some exceptionHandle ->
                let trace = renderExceptionStackTrace state stackTrace

                let traceAddr, state =
                    allocateManagedString loggerFactory baseClassTypes trace state

                let stackTraceStringField =
                    FieldIdentity.requiredOwnInstanceField baseClassTypes.Exception "_stackTraceString"
                    |> FieldIdentity.fieldId exceptionHandle

                IlMachineThreadState.setInstanceFieldById
                    exceptionAddr
                    stackTraceStringField
                    (CliType.ObjectRef (Some traceAddr))
                    state
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
                state._LoadedAssemblies.ByDefinitionName threadConcreteType.Identity.AssemblyFullName

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
    /// See https://github.com/dotnet/runtime/blob/7706f546bac1a99b3d891afe3591dc88c67f0cc4/src/libraries/System.Private.CoreLib/src/System/Reflection/TargetInvocationException.cs#L13-L17
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
                let assembly =
                    state._LoadedAssemblies.ByDefinitionName concreteType.Identity.AssemblyFullName

                Some (concreteType, assembly.TypeDefs.[concreteType.Identity.TypeDefinition.Get])
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> None

    /// Returns true if `handle` is a CLR enum value type — a nominal type whose immediate runtime
    /// base is `System.Enum`.
    let isEnumValueType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * bool
        =
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

    /// For an enum `ConcreteTypeHandle`, return the `ConcreteTypeHandle` of its underlying integer
    /// type by concretising the signature of its sole instance field (`value__`, the CLR-reserved
    /// name for the integer slot of an enum; ECMA-335 §II.14.3). Returns `None` if `handle` is not
    /// an enum, has no TypeDef row, or — defensively — has a malformed Fields list. The caller is
    /// expected to have first verified enum-ness via `isEnumValueType`; this helper does the
    /// metadata read.
    let enumUnderlyingHandle
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
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
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName

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

    /// CoreCLR `MethodTable::GetPrimitiveCorElementType`, restricted to the question `unbox` asks
    /// of it: which primitive `CorElementType` does this handle report, if it is in the
    /// primitive-value-type category at all?
    ///
    /// The category is CoreCLR's `enum_flag_Category_PrimitiveValueType`, which *includes enums*
    /// (see the `// Enum is included` remarks in RuntimeHelpers.CoreCLR.cs); an enum reports the
    /// element type of its underlying integer. Everything else — user structs, `Nullable\`1`,
    /// `System.Decimal`, reference types, the structural handles — answers `None`.
    ///
    /// The identity is returned *exactly*: `Int32` and `UInt32` are different answers, as are
    /// `Char`/`UInt16`, `Boolean`/`Byte` and `IntPtr`/`Int64`. This is deliberately narrower than
    /// both ECMA-335's verification types and the array-element rule below, each of which collapses
    /// signedness — see `unboxPermitted` for why we make that distinction.
    let primitiveElementIdentity
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * ResolvedTypeIdentity option
        =
        // The built-in primitives, each its own distinct answer. Not a normalisation table: the
        // pairs that other CLR rules collapse (Int32/UInt32, Char/UInt16, Boolean/Byte,
        // IntPtr/UIntPtr) are listed separately precisely so they stay distinct here.
        // Takes `state` explicitly rather than capturing it: `enumUnderlyingHandle` concretises the
        // underlying type, so the enum branch below must consult the state it returns, not the one
        // this function was entered with.
        let builtInPrimitiveIdentity
            (state : IlMachineState)
            (handle : ConcreteTypeHandle)
            : ResolvedTypeIdentity option
            =
            match tryGetConcreteTypeInfo state handle with
            | None -> None
            | Some (ct, _) when not ct.Generics.IsEmpty -> None
            | Some (ct, _) ->
                let id = ct.Identity

                let isPrimitive =
                    [
                        baseClassTypes.Boolean
                        baseClassTypes.Char
                        baseClassTypes.SByte
                        baseClassTypes.Byte
                        baseClassTypes.Int16
                        baseClassTypes.UInt16
                        baseClassTypes.Int32
                        baseClassTypes.UInt32
                        baseClassTypes.Int64
                        baseClassTypes.UInt64
                        baseClassTypes.IntPtr
                        baseClassTypes.UIntPtr
                        baseClassTypes.Single
                        baseClassTypes.Double
                    ]
                    |> List.exists (fun ty -> ty.Identity = id)

                if isPrimitive then
                    Some id
                else if
                    // CoreCLR puts three CoreLib handle structs in the primitive category too,
                    // by name, reporting ELEMENT_TYPE_I — the same element type as `IntPtr`
                    // (MethodTableBuilder, the `g_RuntimeMethodHandleInternalName` /
                    // `g_RuntimeFieldHandleInternalName` / `g_RuntimeArgumentHandleName` arms of
                    // `SetInternalCorElementType`). So `unbox.any IntPtr` on one of them is legal.
                    // PawPrint already flattens the two `*HandleInternal` structs to a
                    // runtime-pointer NativeInt, so they can be honoured exactly.
                    //
                    // `RuntimeArgumentHandle` is deliberately absent: PawPrint has no
                    // `PrimitiveLikeKind` for it, so it is not stored flattened and answering
                    // `Some` here would license an unbox this interpreter cannot materialise.
                    // It stays unclassified, which costs an InvalidCastException in a case only
                    // `__arglist` IL can reach.
                    id = baseClassTypes.RuntimeMethodHandleInternal.Identity
                    || id = baseClassTypes.RuntimeFieldHandleInternal.Identity
                then
                    Some baseClassTypes.IntPtr.Identity
                else
                    None

        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> state, None
        | ConcreteTypeHandle.Concrete _ ->
            match builtInPrimitiveIdentity state handle with
            | Some id -> state, Some id
            | None ->
                // An enum is in the primitive category too, reporting the element type of its
                // underlying integer. That underlying must itself be a built-in primitive — the
                // CLR type loader will not admit an enum whose `value__` is anything else — so
                // this resolves in exactly one step rather than recursing.
                let state, isEnum = isEnumValueType loggerFactory baseClassTypes state handle

                if not isEnum then
                    state, None
                else
                    match enumUnderlyingHandle loggerFactory baseClassTypes state handle with
                    | None -> state, None
                    | Some (state, underlying) -> state, builtInPrimitiveIdentity state underlying

    /// Does PawPrint store values of `handle` in the flattened form that the eval stack expects for
    /// a bare primitive?
    ///
    /// True for the built-in primitives themselves, and for enums over the fixed-width integers.
    /// False for enums over `bool`, `char` or a native int: ECMA-335 II.14.3 permits those and the
    /// CLR does load them (C# cannot declare one, but Reflection.Emit can), yet
    /// `CliValueType.IsEnumStructural` deliberately answers false for them, so their storage stays
    /// a wrapped `CliValueType`.
    let private unboxMaterialisesFlattened
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        let state, isEnum = isEnumValueType loggerFactory baseClassTypes state handle

        if not isEnum then
            // The caller has already established a primitive element identity for `handle`, so it
            // is either a built-in primitive (flattened by definition) or one of the two
            // `*HandleInternal` structs, which `PrimitiveLikeKind.FlattenToRuntimePointer` flattens.
            state, true
        else
            match enumUnderlyingHandle loggerFactory baseClassTypes state handle with
            | None -> state, false
            | Some (state, underlying) ->
                let state, underlyingId =
                    primitiveElementIdentity loggerFactory baseClassTypes state underlying

                match underlyingId with
                | None -> state, false
                | Some underlyingId ->
                    [
                        baseClassTypes.SByte
                        baseClassTypes.Byte
                        baseClassTypes.Int16
                        baseClassTypes.UInt16
                        baseClassTypes.Int32
                        baseClassTypes.UInt32
                        baseClassTypes.Int64
                        baseClassTypes.UInt64
                    ]
                    |> List.exists (fun ty -> ty.Identity = underlyingId)
                    |> fun flattenable -> state, flattenable

    /// CoreCLR `CastHelpers.Unbox_Helper`: `unbox` and the value-typed form of `unbox.any` accept a
    /// boxed operand when the two handles are identical, or when both types are in the primitive
    /// category and report the *same* primitive element type. That second clause is what lets a
    /// boxed enum unbox to its underlying integer and back.
    ///
    /// It is narrower than it first looks, and deliberately so:
    ///   - ECMA-335's verification types collapse signedness (`int32` and `uint32` share one), but
    ///     this does not: `(uint)(object)1` raises InvalidCastException on a real runtime;
    ///   - the array-element rule (`CanCastParam`, via `valueElementNormalisedIdentity` below)
    ///     *does* collapse signedness, which is why `(uint[])(object)new int[1]` succeeds while the
    ///     scalar cast fails. Do not reach for that helper here — the two rules genuinely differ.
    ///
    /// `Nullable\`1` never reaches this predicate: it matches its argument by exact equivalence
    /// (`Nullable::IsNullableForTypeHelper`), so a boxed enum is not a `T?` of its underlying type.
    let unboxPermitted
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (boxedType : ConcreteTypeHandle)
        (targetType : ConcreteTypeHandle)
        : IlMachineState * bool
        =
        if boxedType = targetType then
            state, true
        else

        let state, boxedPrimitive =
            primitiveElementIdentity loggerFactory baseClassTypes state boxedType

        match boxedPrimitive with
        | None -> state, false
        | Some boxedPrimitive ->
            let state, targetPrimitive =
                primitiveElementIdentity loggerFactory baseClassTypes state targetType

            match targetPrimitive with
            | None -> state, false
            | Some targetPrimitive ->
                if boxedPrimitive <> targetPrimitive then
                    state, false
                else
                    // About to license the relaxation, which pairs two *different* handles. Both
                    // sides must be ones PawPrint stores in flattened form, and for different
                    // reasons:
                    //   - the boxed side drives materialisation, so an unflattened one would push a
                    //     wrapped `UserDefinedValueType` where the next instruction expects a bare
                    //     stack primitive;
                    //   - the target side is the slot the value lands in, and `toCliTypeCoerced`
                    //     rejects a bare primitive into a value-type slot unless that slot is
                    //     primitive-like (see the `failwith` in its `CliType.ValueType` arm), so an
                    //     unflattened target would abort on the following `stloc`/`stfld` instead.
                    // The identity case never reaches here, so this only ever rejects genuinely
                    // mixed pairs. Fail loudly rather than answering `false`, which would raise
                    // InvalidCastException where a real runtime would succeed.
                    let state, boxedFlattened =
                        unboxMaterialisesFlattened loggerFactory baseClassTypes state boxedType

                    let state, targetFlattened =
                        unboxMaterialisesFlattened loggerFactory baseClassTypes state targetType

                    if boxedFlattened && targetFlattened then
                        state, true
                    else
                        let offender = if boxedFlattened then targetType else boxedType

                        failwith
                            $"unbox of %O{boxedType} to %O{targetType}: CoreCLR permits this (both report the same primitive element type), but PawPrint does not store %O{offender} in flattened form — see CliValueType.IsEnumStructural, which covers only enums over the fixed-width integers, not over bool/char/native int"

    /// Does this handle denote a reference type (as opposed to a value type)?
    ///
    /// The structural handles answer without any metadata: arrays of every rank are reference
    /// types, while byrefs, pointers and function pointers are not (they are neither, strictly,
    /// but every caller asks this question to decide whether reference-type rules — covariance,
    /// array-store checks, atomic reference exchange — apply, and for those the answer is "no").
    /// Nominal handles defer to the TypeDef row.
    ///
    /// `context` names the caller in the diagnostic raised when a nominal handle has no TypeDef
    /// row, which would be a bug in whatever produced the handle.
    let isReferenceTypeHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (context : string)
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> false
        | ConcreteTypeHandle.Concrete _ ->
            match tryGetConcreteTypeInfo state handle with
            | Some (_, typeInfo) -> DumpedAssembly.isReferenceType baseClassTypes state._LoadedAssemblies typeInfo
            | None -> failwith $"%s{context}: concrete type handle %O{handle} has no TypeDef row"

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

        let isReferenceTypeHandle =
            isReferenceTypeHandle baseClassTypes "isConcreteTypeAssignableTo"

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
                let assy = state._LoadedAssemblies.ByDefinitionName ct.Identity.AssemblyFullName

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
                // Same TypeDef but different instantiations is the variance hook
                // (ECMA-335 §I.8.7.2 / CoreCLR
                // `CanCastByVarianceToInterfaceOrDelegate`). Classes are invariant
                // by spec, so when none of the parameters declare variance the
                // answer is definitively false. Interfaces and delegates can
                // declare `+`/`-` on each parameter; per-parameter assignability
                // resolves the cast.
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
                    let targetAssy =
                        state._LoadedAssemblies.ByDefinitionName targetCt.Identity.AssemblyFullName

                    let targetTypeInfo = targetAssy.TypeDefs.[targetCt.Identity.TypeDefinition.Get]

                    let hasVariantGenericParams =
                        targetTypeInfo.Generics
                        |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

                    if hasVariantGenericParams then
                        checkVariantGenericArgs state currentCt targetCt targetTypeInfo
                    else
                        // All generic parameters are invariant; same definition + different generics = not assignable.
                        state, false
                | None ->
                    let state, interfaceMatch = checkInterfaces state current

                    if interfaceMatch then
                        state, true
                    else
                        walkBase state current

        // ECMA-335 §I.8.7 / CoreCLR `MethodTable::CanCastByVarianceToInterfaceOrDelegate`:
        // when two generic instantiations share the same TypeDef and the
        // definition declares variance on at least one parameter, the cast
        // reduces to a per-parameter check.
        //   - Identical arguments are always accepted.
        //   - Covariant (`out`) parameter: `fromArg` must be a reference type
        //     and reference-assignable to `toArg`. (CoreCLR's `IsBoxedAndCanCastTo`
        //     rejects value-typed `fromArg` regardless of the declared variance —
        //     boxing changes identity, and the variance walk assumes the
        //     argument is in its boxed form.)
        //   - Contravariant (`in`) parameter: `toArg` must be a reference type
        //     and reference-assignable to `fromArg`.
        //   - Invariant parameter: arguments must be identical, so a difference
        //     here short-circuits to `false`.
        // Recursion into `isConcreteTypeAssignableTo` for the per-argument check
        // is necessary because variance composes (e.g. `Func<Func<Derived>>` ⊑
        // `Func<Func<Base>>` for the nested covariant `out` parameter).
        and checkVariantGenericArgs
            (state : IlMachineState)
            (currentCt : ConcreteType<ConcreteTypeHandle>)
            (targetCt : ConcreteType<ConcreteTypeHandle>)
            (targetTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
            : IlMachineState * bool
            =
            let rec loop (state : IlMachineState) (i : int) : IlMachineState * bool =
                if i >= currentCt.Generics.Length then
                    state, true
                else
                    let fromArg = currentCt.Generics.[i]
                    let toArg = targetCt.Generics.[i]

                    if fromArg = toArg then
                        loop state (i + 1)
                    else
                        let _, paramMetadata = targetTypeInfo.Generics.[i]

                        let state, argOk =
                            match paramMetadata.Variance with
                            | None ->
                                // Invariant parameter with non-identical arguments.
                                state, false
                            | Some GenericVariance.Covariant ->
                                if not (isReferenceTypeHandle state fromArg) then
                                    state, false
                                else
                                    isConcreteTypeAssignableTo loggerFactory baseClassTypes state fromArg toArg
                            | Some GenericVariance.Contravariant ->
                                if not (isReferenceTypeHandle state toArg) then
                                    state, false
                                else
                                    isConcreteTypeAssignableTo loggerFactory baseClassTypes state toArg fromArg

                        if argOk then loop state (i + 1) else state, false

            loop state 0

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
            let state, isEnum = isEnumValueType loggerFactory baseClassTypes state handle

            if isEnum then
                match enumUnderlyingHandle loggerFactory baseClassTypes state handle with
                | None -> state, None
                | Some (state, underlying) -> state, normalizedPrimitiveIntegerIdentity underlying
            else
                state, normalizedPrimitiveIntegerIdentity handle

        // ECMA-335 III.4.3 / CoreCLR `TypeDesc::CanCastParam`: element-compatibility
        // for parameterised array slots (whether array-to-array or SZ-array-to-
        // implicit-generic-interface) reduces to one of three cases.
        //   1. Identical elements — always compatible.
        //   2. Both reference-typed — recursive assignability (covariance).
        //   3. Both value-typed — same normalised integer identity, applying both
        //      ECMA-335 III.8.7 primitive-width equivalence and enum-underlying-
        //      type equivalence (see `valueElementNormalisedIdentity`).
        // Anything else (ref/value mismatch, non-integer value types, generic
        // type variables) answers definitively false.
        let elementCovariantlyCompatible
            (state : IlMachineState)
            (objElement : ConcreteTypeHandle)
            (targetElement : ConcreteTypeHandle)
            : IlMachineState * bool
            =
            if objElement = targetElement then
                state, true
            else
                let objIsRef = isReferenceTypeHandle state objElement
                let targetIsRef = isReferenceTypeHandle state targetElement

                if objIsRef && targetIsRef then
                    isConcreteTypeAssignableTo loggerFactory baseClassTypes state objElement targetElement
                elif objIsRef <> targetIsRef then
                    state, false
                else
                    let state, objNormalised = valueElementNormalisedIdentity state objElement
                    let state, targetNormalised = valueElementNormalisedIdentity state targetElement

                    match objNormalised, targetNormalised with
                    | Some a, Some b when a = b -> state, true
                    | _, _ -> state, false

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
                else
                    let state, compatible = elementCovariantlyCompatible state objElement targetElement
                    state, Some compatible
            | Some _, None -> state, None
            | None, _ -> failwith $"checkArraySpecificRules called with non-array source %O{objType}"

        // CoreCLR `MethodTable::ArraySupportsBizarreInterface` /
        // `IsImplicitInterfaceOfSZArray` (`src/coreclr/vm/array.cpp`): an
        // SZ-array `T[]` implicitly implements the five generic interfaces
        // `IList<U>`, `ICollection<U>`, `IEnumerable<U>`, `IReadOnlyList<U>`,
        // and `IReadOnlyCollection<U>` whenever `T` is element-compatible
        // with `U` under the CoreCLR `CanCastParam` rule (recursive
        // reference covariance for ref elements; normalised-integer
        // equivalence for value elements). The carve-out applies even for
        // the invariant interfaces (`IList<U>`, `ICollection<U>`).
        //
        // Multi-dim arrays do NOT participate in this carve-out, and other
        // generic interfaces (anything that isn't one of the five) are
        // never implicitly implemented by arrays. Returns `None` when the
        // pair does not fit the carve-out, leaving the caller to default
        // to `false`.
        let tryCheckSzArrayImplicitInterface
            (state : IlMachineState)
            (objType : ConcreteTypeHandle)
            (targetType : ConcreteTypeHandle)
            : (IlMachineState * bool) option
            =
            match objType with
            | ConcreteTypeHandle.OneDimArrayZero objElement ->
                match tryGetConcreteTypeInfo state targetType with
                | Some (targetCt, _) when targetCt.Generics.Length = 1 ->
                    if baseClassTypes.IsImplicitInterfaceOfSzArray targetCt.Identity then
                        let targetElement = targetCt.Generics.[0]
                        let state, compatible = elementCovariantlyCompatible state objElement targetElement
                        Some (state, compatible)
                    else
                        None
                | _ -> None
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> None

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
                    match tryCheckSzArrayImplicitInterface state objType targetType with
                    | Some result -> result
                    | None ->
                        // The remaining structural shapes — multi-dim arrays
                        // against any generic interface, or SZ-arrays against
                        // a generic interface that isn't one of the five
                        // implicit ones — are definitively not assignable.
                        // CoreCLR's `ArraySupportsBizarreInterface` agrees.
                        state, false
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> walk state objType

    /// Apply a `TypeDefn` substitution to every `GenericTypeParameter` reference inside `ty`.
    /// This is purely syntactic; assembly identifiers and primitives pass through unchanged.
    /// `GenericMethodParameter` references are left in place because they have no legitimate
    /// appearance in a type definition's base or interface signatures (method generics belong
    /// to method signatures, not type metadata edges).
    let rec private substituteTypeDefn (subs : ImmutableArray<TypeDefn>) (ty : TypeDefn) : TypeDefn =
        match ty with
        | TypeDefn.GenericTypeParameter idx ->
            if idx < subs.Length then
                subs.[idx]
            else
                failwithf
                    "substituteTypeDefn: GenericTypeParameter %d out of bounds (substitution arity %d)"
                    idx
                    subs.Length
        | TypeDefn.GenericMethodParameter _ -> ty
        | TypeDefn.GenericInstantiation (generic, args) ->
            let builder = ImmutableArray.CreateBuilder args.Length

            for arg in args do
                builder.Add (substituteTypeDefn subs arg)

            TypeDefn.GenericInstantiation (substituteTypeDefn subs generic, builder.ToImmutable ())
        | TypeDefn.Array (element, rank) -> TypeDefn.Array (substituteTypeDefn subs element, rank)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            TypeDefn.OneDimensionalArrayLowerBoundZero (substituteTypeDefn subs element)
        | TypeDefn.Pointer element -> TypeDefn.Pointer (substituteTypeDefn subs element)
        | TypeDefn.Byref element -> TypeDefn.Byref (substituteTypeDefn subs element)
        | TypeDefn.Pinned element -> TypeDefn.Pinned (substituteTypeDefn subs element)
        | TypeDefn.Modified m ->
            TypeDefn.Modified
                {
                    Unmodified = substituteTypeDefn subs m.Unmodified
                    Modifier = substituteTypeDefn subs m.Modifier
                    IsRequired = m.IsRequired
                }
        | TypeDefn.FunctionPointer _
        | TypeDefn.PrimitiveType _
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Void -> ty

    /// Strip outer `GenericInstantiation` layers and resolve the underlying definition to
    /// `(assembly, TypeInfo, genericArgs)`. Returns `None` when the stripped TypeDefn is not a
    /// nominal type definition (e.g. an array, pointer, byref, primitive, or generic
    /// parameter) — none of which is a legitimate `BaseType` or interface implementation, so
    /// the caller treats them as dead-end edges for the identity walk. The returned
    /// `genericArgs` are the args from the outermost `GenericInstantiation` (already
    /// substituted by the caller), or `ImmutableArray.Empty` when the type was a plain
    /// definition without an explicit generic instantiation around it.
    let rec private stripToTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (declaringAssembly : DumpedAssembly)
        (ty : TypeDefn)
        : IlMachineState *
          (DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn> * ImmutableArray<TypeDefn>) option
        =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            let state, inner =
                stripToTypeInfo loggerFactory baseClassTypes state declaringAssembly generic

            match inner with
            | Some (assy, typeInfo, _) -> state, Some (assy, typeInfo, args)
            | None -> state, None
        | TypeDefn.FromDefinition (identity, _) ->
            match state.LoadedAssembly identity.Assembly with
            | Some assy -> state, Some (assy, assy.TypeDefs.[identity.TypeDefinition.Get], ImmutableArray.Empty)
            | None ->
                failwithf "stripToTypeInfo: assembly for type definition %s was not loaded" identity.AssemblyFullName
        | TypeDefn.FromReference _ ->
            // resolveTypeFromDefn handles FromReference and returns a TypeInfo<TypeDefn,TypeDefn>;
            // the un-substituted raw TypeInfo<GenericParamFromMetadata,TypeDefn> is what the walk
            // wants, so re-fetch from the resolved identity's assembly TypeDefs table.
            let state, _, resolved =
                IlMachineTypeResolution.resolveTypeFromDefn
                    loggerFactory
                    baseClassTypes
                    ty
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    declaringAssembly
                    state

            let identity = resolved.Identity

            match state.LoadedAssembly identity.Assembly with
            | Some assy -> state, Some (assy, assy.TypeDefs.[identity.TypeDefinition.Get], ImmutableArray.Empty)
            | None ->
                failwithf
                    "stripToTypeInfo: assembly for resolved type reference %s was not loaded"
                    identity.AssemblyFullName
        | TypeDefn.PrimitiveType _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.Pinned _
        | TypeDefn.Modified _
        | TypeDefn.FunctionPointer _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> state, None

    /// Walk a `TypeDefn` and rewrite every `FromReference` (assembly-relative) into the
    /// equivalent `FromDefinition` (carrying a fully-resolved identity), preserving structure
    /// elsewhere. The caller supplies the assembly whose TypeRef tables interpret the input;
    /// after this transform the result is assembly-independent and can be substituted into a
    /// TypeDefn that lives in a different assembly without resolution errors.
    ///
    /// Used by the open-generic cast walk at the strip boundary: when a recursion crosses
    /// from `currentAssy` to a different `strippedAssy`, the args carried into the deeper
    /// walk may contain TypeRefs declared in the outer assembly (e.g. `Derived<T> :
    /// Base<Arg, T>` in assembly E, where `Arg` lives in A and `Base` in B). Without
    /// canonicalisation, deeper materialisations would try to resolve those TypeRefs against
    /// the wrong assembly's reference tables and fail. `GenericTypeParameter` positions are
    /// positional and assembly-independent, so they pass through unchanged.
    let rec private canonicalizeTypeDefn
        (loggerFactory : ILoggerFactory)
        (state : IlMachineState)
        (sourceAssy : DumpedAssembly)
        (ty : TypeDefn)
        : IlMachineState * TypeDefn
        =
        match ty with
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.FromDefinition _ -> state, ty
        | TypeDefn.FromReference (typeRef, sigKind) ->
            let state, _, resolved =
                IlMachineTypeResolution.resolveTypeFromRef loggerFactory sourceAssy typeRef ImmutableArray.Empty state

            state, TypeDefn.FromDefinition (resolved.Identity, sigKind)
        | TypeDefn.GenericInstantiation (generic, args) ->
            let state, generic' = canonicalizeTypeDefn loggerFactory state sourceAssy generic

            let state, argsList =
                ((state, []), args)
                ||> Seq.fold (fun (state, acc) arg ->
                    let state, arg' = canonicalizeTypeDefn loggerFactory state sourceAssy arg
                    state, arg' :: acc
                )

            let argsArr = argsList |> List.rev |> ImmutableArray.CreateRange
            state, TypeDefn.GenericInstantiation (generic', argsArr)
        | TypeDefn.Array (element, rank) ->
            let state, element' = canonicalizeTypeDefn loggerFactory state sourceAssy element
            state, TypeDefn.Array (element', rank)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element ->
            let state, element' = canonicalizeTypeDefn loggerFactory state sourceAssy element
            state, TypeDefn.OneDimensionalArrayLowerBoundZero element'
        | TypeDefn.Pointer element ->
            let state, element' = canonicalizeTypeDefn loggerFactory state sourceAssy element
            state, TypeDefn.Pointer element'
        | TypeDefn.Byref element ->
            let state, element' = canonicalizeTypeDefn loggerFactory state sourceAssy element
            state, TypeDefn.Byref element'
        | TypeDefn.Pinned element ->
            let state, element' = canonicalizeTypeDefn loggerFactory state sourceAssy element
            state, TypeDefn.Pinned element'
        | TypeDefn.Modified m ->
            let state, unmodified' =
                canonicalizeTypeDefn loggerFactory state sourceAssy m.Unmodified

            let state, modifier' =
                canonicalizeTypeDefn loggerFactory state sourceAssy m.Modifier

            state,
            TypeDefn.Modified
                {
                    Unmodified = unmodified'
                    Modifier = modifier'
                    IsRequired = m.IsRequired
                }
        | TypeDefn.FunctionPointer _ ->
            // FunctionPointer carries a TypeMethodSignature with parameter and return TypeDefns;
            // canonicalising those requires walking the signature shape. None of the current
            // open-generic walk targets exercises this case (you can't author
            // `delegate*<X, void>` as a generic argument in C#), so defer until a real test
            // case forces it.
            failwithf
                "TODO: canonicalizeTypeDefn: FunctionPointer not yet supported in cross-assembly substitutions (%O)"
                ty

    /// Cast oracle entry point over the full `RuntimeTypeHandleTarget` DU. The Closed/Closed case
    /// delegates to `isConcreteTypeAssignableTo`; the open-generic and generic-parameter variants
    /// are handled at this layer so that callers (e.g. the `TypeHandle_CanCastTo_NoCacheLookup`
    /// QCall) need not coerce open handles back to closed ones.
    ///
    /// The rule table:
    /// - Closed / Closed                 → existing oracle
    /// - Closed / OpenGenericTypeDefinition → false (open defs are not instantiable; nothing
    ///   closed can be assigned to a type token that names "the open def" itself)
    /// - OpenGenericTypeDefinition s / OpenGenericTypeDefinition t → s = t (identity only;
    ///   stripping during a parent walk is for traversal, not for matching an open target)
    /// - OpenGenericTypeDefinition / Closed → walk the source's base chain and implemented
    ///   interfaces, threading a substitution context so that partially-closed inheritance
    ///   like `class C<T> : B<int,T>` propagates the `int` binding into B's own walk. At
    ///   each edge: substitute with the current context, then if no `GenericTypeParameter`
    ///   reference remains, materialise to a `ConcreteTypeHandle` and delegate to
    ///   Closed/Closed; otherwise strip to the definition's `TypeInfo` and recurse with the
    ///   (substituted) generic args as the new context. The fallback when both the base and
    ///   the interface chain are exhausted is to accept iff the target is System.Object,
    ///   matching the closed oracle's `walkBase` `None` branch for interfaces.
    /// - GenericParameter / MethodGenericParameter (either side) → TODO until constraints
    ///   are modelled in the cast oracle
    let isRuntimeTypeHandleTargetAssignableTo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (source : RuntimeTypeHandleTarget)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * bool
        =
        match source, target with
        | RuntimeTypeHandleTarget.Closed s, RuntimeTypeHandleTarget.Closed t ->
            isConcreteTypeAssignableTo loggerFactory baseClassTypes state s t
        | RuntimeTypeHandleTarget.Closed _, RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
            // An OpenGenericTypeDefinition handle represents "the open generic definition itself"
            // (e.g. typeof(Box<>)), which is not an instantiable type. No closed type is assignable
            // to it. CoreCLR's managed wrapper short-circuits the "ref-type → TypeDesc" case before
            // ever invoking the QCall; this branch is the analogue at the cast-oracle level.
            state, false
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition s, RuntimeTypeHandleTarget.OpenGenericTypeDefinition t ->
            state, s = t
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition s, RuntimeTypeHandleTarget.Closed t ->
            let sAssy =
                match state.LoadedAssembly s.Assembly with
                | Some assy -> assy
                | None ->
                    failwithf "isRuntimeTypeHandleTargetAssignableTo: source assembly %s not loaded" s.AssemblyFullName

            let sTypeInfo = sAssy.TypeDefs.[s.TypeDefinition.Get]

            // Capture target's identity and whether any of its generic parameters carry
            // variance. CoreCLR can assign an open generic interface to a closed
            // instantiation of itself when the parameter is variant and its constraints
            // are satisfied (e.g. `IOut<out T> where T : class` makes
            // `typeof(IOut<object>).IsAssignableFrom(typeof(IOut<>))` true). We don't yet
            // model constraint-aware variance during an open walk, so when the walk
            // encounters a node whose identity matches a variant target we crash loudly
            // rather than silently returning false. The mirror at the closed/closed oracle
            // (lines 1215-1220) uses the same shape.
            let targetIdentityWithVariance =
                match AllConcreteTypes.lookup t state.ConcreteTypes with
                | Some targetCt ->
                    let targetAssy =
                        state._LoadedAssemblies.ByDefinitionName targetCt.Identity.AssemblyFullName

                    let targetTypeInfo = targetAssy.TypeDefs.[targetCt.Identity.TypeDefinition.Get]

                    let hasVariantGenericParams =
                        targetTypeInfo.Generics
                        |> Seq.exists (fun (_, metadata) -> metadata.Variance.IsSome)

                    Some (targetCt.Identity, hasVariantGenericParams)
                | None -> None

            // The walk begins by treating every one of the source's generic parameters as
            // unbound: each `GenericTypeParameter i` substitutes to itself, so the source's
            // own metadata edges keep their original parameter references when first
            // inspected, and only fully-bound positions get materialised as we descend.
            let initialSubstitutions =
                let builder = ImmutableArray.CreateBuilder sTypeInfo.Generics.Length

                for i in 0 .. sTypeInfo.Generics.Length - 1 do
                    builder.Add (TypeDefn.GenericTypeParameter i)

                builder.ToImmutable ()

            let rec walkOpen
                (state : IlMachineState)
                (visited : Set<ResolvedTypeIdentity>)
                (substitutions : ImmutableArray<TypeDefn>)
                (currentAssy : DumpedAssembly)
                (currentTypeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
                : IlMachineState * bool
                =
                let currentIdentity = currentTypeInfo.Identity

                if Set.contains currentIdentity visited then
                    state, false
                else

                match targetIdentityWithVariance with
                | Some (targetIdentity, true) when currentIdentity = targetIdentity ->
                    failwithf
                        "TODO: isRuntimeTypeHandleTargetAssignableTo: open source %O reaches target identity %O which has variant generic parameters; need constraint-aware variance check"
                        s.TypeDefinition.Get
                        targetIdentity.TypeDefinition.Get
                | _ ->

                let visited = Set.add currentIdentity visited

                let tryEdge
                    (state : IlMachineState)
                    (edgeAssy : DumpedAssembly)
                    (edgeTypeDefn : TypeDefn)
                    : IlMachineState * bool
                    =
                    let substituted = substituteTypeDefn substitutions edgeTypeDefn

                    if not (containsAnyGenericParameter substituted) then
                        // Edge is fully closed under the current substitution. Materialise and
                        // delegate to the closed oracle, which handles all variance, array, and
                        // base-chain rules.
                        let state, edgeHandle =
                            IlMachineTypeResolution.concretizeType
                                loggerFactory
                                baseClassTypes
                                state
                                edgeAssy.Name
                                ImmutableArray.Empty
                                ImmutableArray.Empty
                                substituted

                        isConcreteTypeAssignableTo loggerFactory baseClassTypes state edgeHandle t
                    else
                        // Edge still mentions an unbound parameter from the original open
                        // source. It can never be identical to a closed target, but its own
                        // base/interface chain might be. Strip to the edge's `TypeInfo` and
                        // recurse, threading the (already-substituted) generic args as the
                        // edge's substitution context.
                        let state, stripped =
                            stripToTypeInfo loggerFactory baseClassTypes state edgeAssy substituted

                        match stripped with
                        | None -> state, false
                        | Some (strippedAssy, strippedTypeInfo, strippedArgs) ->
                            // Canonicalise each arg against the edge's authoring assembly
                            // before recursing. `strippedArgs` are the GenericInstantiation
                            // args from `substituted`, which inherits TypeRefs from
                            // `edgeAssy` (where the edge was authored) plus whatever the
                            // outer substitutions had filled in (already canonical by this
                            // function's invariant at this point of the recursion). The
                            // deeper walk's currentAssy becomes `strippedAssy`, which is a
                            // different assembly's reference tables, so unresolved TypeRefs
                            // must be turned into `FromDefinition` here to remain
                            // interpretable downstream.
                            let state, canonicalisedArgs =
                                ((state, []), strippedArgs)
                                ||> Seq.fold (fun (state, acc) arg ->
                                    let state, arg' = canonicalizeTypeDefn loggerFactory state edgeAssy arg

                                    state, arg' :: acc
                                )

                            let canonicalisedArgs = canonicalisedArgs |> List.rev |> ImmutableArray.CreateRange

                            walkOpen state visited canonicalisedArgs strippedAssy strippedTypeInfo

                let state, interfaceMatch =
                    ((state, false), currentTypeInfo.ImplementedInterfaces)
                    ||> Seq.fold (fun (state, found) impl ->
                        if found then
                            state, true
                        else
                            let implAssy =
                                match state.LoadedAssembly impl.RelativeToAssembly with
                                | Some a -> a
                                | None -> currentAssy

                            let state, implTypeDefn, implResolvedAssy =
                                resolveTypeMetadataToken
                                    loggerFactory
                                    baseClassTypes
                                    state
                                    implAssy
                                    ImmutableArray.Empty
                                    impl.InterfaceHandle

                            tryEdge state implResolvedAssy implTypeDefn
                    )

                if interfaceMatch then
                    state, true
                else

                match currentTypeInfo.BaseType with
                | None ->
                    // Interfaces (and System.Object itself) carry no `extends` clause, so
                    // `BaseType` is `None` in metadata. Every reference type is assignable
                    // to System.Object, so mirror the closed oracle's `walkBase` fallback
                    // (lines 1183-1187): when the chain runs out, accept iff the target is
                    // System.Object. Open generic *classes* and *structs* never hit this
                    // branch — their metadata BaseType is always System.Object or
                    // System.ValueType — so in practice this fires for open interfaces with
                    // no further parent interfaces.
                    match t with
                    | ConcreteActivePatterns.ConcreteObj state.ConcreteTypes -> state, true
                    | _ -> state, false
                | Some baseTypeInfo ->
                    let state, baseAssy, baseTypeDefn =
                        resolveBaseTypeInfo loggerFactory baseClassTypes state currentAssy baseTypeInfo

                    tryEdge state baseAssy baseTypeDefn

            walkOpen state Set.empty initialSubstitutions sAssy sTypeInfo
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position), _ ->
            failwithf
                "TODO: isRuntimeTypeHandleTargetAssignableTo: generic parameter source #%d of %O; need to model constraint-based assignability"
                position
                declaringType.TypeDefinition.Get
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position), _ ->
            failwithf
                "TODO: isRuntimeTypeHandleTargetAssignableTo: method generic parameter source #%d of method %O on %O; need to model constraint-based assignability"
                position
                declaringMethod.Get
                declaringType.TypeDefinition.Get
        | _, RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            failwithf
                "TODO: isRuntimeTypeHandleTargetAssignableTo: generic parameter target #%d of %O; need to model constraint-based assignability"
                position
                declaringType.TypeDefinition.Get
        | _, RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwithf
                "TODO: isRuntimeTypeHandleTargetAssignableTo: method generic parameter target #%d of method %O on %O; need to model constraint-based assignability"
                position
                declaringMethod.Get
                declaringType.TypeDefinition.Get
