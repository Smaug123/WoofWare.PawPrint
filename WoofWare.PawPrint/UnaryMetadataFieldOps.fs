namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataFieldOps =
    /// `System.String::Empty` is `[Intrinsic]` in the BCL: the source declares it as
    /// `static readonly string Empty;` with no initialiser, and the CLR's execution engine
    /// populates it during startup rather than via a class constructor. Because PawPrint has
    /// no equivalent EE startup hook, the field reads as uninitialised when guest code touches
    /// it, and `cliTypeZeroOf` of a string yields a null object reference. Returning null here
    /// triggers downstream NREs deep in the BCL (e.g. `MemberInfoCache.Populate` calling
    /// `GetListByName` on a null cache key), which then trip SR's resource-lookup recursion
    /// guard and `FailFast` the whole process. Detect the field at `ldsfld`/`ldsflda` time
    /// and lazily intern the canonical empty managed string so that
    /// `ReferenceEquals(string.Empty, "")` holds, matching CLR semantics.
    let private isSystemStringEmptyField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        field.Name = "Empty"
        && field.DeclaringType.Generics.IsEmpty
        && field.DeclaringType.Identity = baseClassTypes.String.Identity

    /// `System.Runtime.CompilerServices.CastHelpers::s_table` is the BCL's managed
    /// cast-cache backing array. In CoreCLR it is populated at native-EE startup by
    /// `CastCache::Initialize` (`coreclr/vm/castcache.cpp`), invoked from
    /// `SystemDomain::LoadBaseSystemClasses`, with a 2-entry sentinel cache. PawPrint
    /// has no equivalent startup hook, so the field reads as null and the BCL's
    /// `ldflda RawData::Data` against null inside `CastCache.TableData` throws a
    /// spurious NRE the first time anything goes through the cache (notably during
    /// resource-string lookup for the *first* genuine NRE, which then trips SR's
    /// recursion guard and `FailFast`s). Detect the field at `ldsfld`/`ldsflda` time
    /// and lazily install the sentinel cache, matching what CoreCLR's EE would have done.
    /// See `docs/runtime-initialised-statics.md` for the full Category-B catalogue.
    let private isCastHelpersTableField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : bool
        =
        field.Name = "s_table"
        && field.DeclaringType.Generics.IsEmpty
        && field.DeclaringType.Namespace = "System.Runtime.CompilerServices"
        && field.DeclaringType.Name = "CastHelpers"
        && field.DeclaringType.AssemblyFullName = baseClassTypes.Corelib.DefinitionFullName

    /// Resolve a field-bearing operand — a metadata token, or a `DynamicScope` entry already
    /// resolved to a `FieldHandle` — to the field it names, together with the assembly whose
    /// metadata scopes that field.
    ///
    /// A `FieldInfo` is decoded from its declaring
    /// assembly's tables, so that assembly — not the executing one — is what interprets
    /// `field.Signature` (whose `FromReference` case carries a `TypeRef` whose `ResolutionScope`
    /// indexes the declaring assembly's `AssemblyRef` table), `field.Handle`, and
    /// `field.RelativeVirtualAddress`. For a `FieldDefinition` token the two assemblies coincide;
    /// for a `MemberReference` they need not.
    ///
    /// `opName` and `verb` appear in diagnostics only, the latter in the phrase "Unexpectedly asked
    /// to <verb> a non-field".
    let private resolveFieldToken
        (opName : string)
        (verb : string)
        (ctx : UnaryMetadataIlOpContext)
        (state : IlMachineState)
        : IlMachineState * FieldInfo<TypeDefn, TypeDefn> * DumpedAssembly
        =
        match ctx.FieldOperand with
        | ResolvedFieldOperand.FromScope fieldHandle ->
            // A `DynamicScope` operand arrives already resolved to the field's *identity*: the
            // assembly, the declaring `RuntimeTypeHandleTarget`, and the `FieldDefinitionHandle`
            // that the field-handle registry recorded when the guest asked for the handle. All that
            // remains is the projection to a `FieldInfo`, which is the same table read the
            // `FieldDefinition` arm below performs.
            let declaringAssy, field = FieldRvaData.fieldForHandle opName fieldHandle state

            // The declaring type's generic arguments, substituted in from the instantiation the
            // registry recorded rather than left as metadata parameters. The field's *signature* is
            // deliberately not touched: it may itself mention those parameters (`Box<T>.Item : T`),
            // and the ops resolve it against `field.DeclaringType.Generics` through
            // `concretizeFieldForExecution` — which is exactly what the metadata `MemberReference`
            // arm relies on too.
            let typeGenerics =
                match fieldHandle.GetDeclaringTypeHandle () with
                | RuntimeTypeHandleTarget.Closed declaringTypeHandle ->
                    match AllConcreteTypes.lookup declaringTypeHandle state.ConcreteTypes with
                    | Some declaringType ->
                        declaringType.Generics
                        |> Seq.map (fun handle ->
                            Concretization.concreteHandleToTypeDefn
                                ctx.BaseClassTypes
                                handle
                                state.ConcreteTypes
                                state._LoadedAssemblies
                        )
                        |> ImmutableArray.CreateRange
                    | None ->
                        failwith
                            $"BUG: %s{opName}: the declaring type %O{declaringTypeHandle} of a DynamicScope field operand is not concretized, but the field-handle registry only ever records concretized targets"
                | notClosed ->
                    // `DynamicScopeOperand.field` refuses these as an invalid program, so execution
                    // never reaches an op with one.
                    failwith
                        $"BUG: %s{opName}: a DynamicScope field operand reached the ops with the non-closed declaring type %O{notClosed}, which DynamicScopeOperand.field is supposed to have refused"

            let field =
                field
                |> FieldInfo.mapTypeGenerics (fun index _ ->
                    if index < 0 || index >= typeGenerics.Length then
                        failwith
                            $"%s{opName}: field %s{field.Name} names type generic parameter %d{index}, but its declaring type has %d{typeGenerics.Length}"
                    else
                        typeGenerics.[index]
                )

            state, field, declaringAssy
        | ResolvedFieldOperand.FromMetadata (activeAssy, metadataToken) ->

        let state, field =
            match metadataToken with
            | MetadataToken.FieldDefinition fieldHandle ->
                match activeAssy.Fields.TryGetValue fieldHandle with
                | false, _ ->
                    failwith
                        $"TODO: %s{opName} - throw MissingFieldException. Field definition handle %O{fieldHandle} is absent from %s{activeAssy.DefinitionFullName}."
                | true, field ->
                    let field =
                        field
                        |> FieldInfo.mapTypeGenerics (fun _ ->
                            failwith $"%s{opName}: generics are not allowed on a FieldDefinition token"
                        )

                    state, field
            | MetadataToken.MemberReference mr ->
                let state, _, resolved, _ =
                    IlMachineState.resolveMember ctx.LoggerFactory ctx.BaseClassTypes ctx.Thread activeAssy mr state

                match resolved with
                | Choice1Of2 method ->
                    failwith
                        $"%s{opName}: member reference resolved to a method (%s{method.Name}), not a field. This indicates invalid IL or a misresolved token."
                | Choice2Of2 field -> state, field
            | t -> failwith $"Unexpectedly asked to %s{verb} a non-field: {t}"

        // Resolving the token is what loads the declaring assembly, so it is expected to be present.
        let declaringAssy =
            state.LoadedAssembly field.DeclaringType.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"%s{opName}: declaring assembly %s{field.DeclaringType.AssemblyFullName} of field %s{field.DeclaringType.Namespace}.%s{field.DeclaringType.Name}::%s{field.Name} was not loaded. Resolving the field token is expected to have loaded it."
            )

        state, field, declaringAssy

    /// Assert that a field reached through a *static* field op really is static, and vice versa.
    /// The static ops key their storage off `(declaringTypeHandle, fieldHandle)` with no instance,
    /// and the instance ops project through an object reference; feeding either the other's kind of
    /// field silently misfiles the value rather than failing, so check rather than assume.
    ///
    /// The two directions are not symmetric, and the diagnostic says so:
    ///
    /// * a *static* op on an *instance* field is invalid IL — CoreCLR's importer raises
    ///   `BADCODE("static access on an instance field")` (`jit/importer.cpp`, at the `isLoadStatic`
    ///   and `isStoreStatic` checks);
    /// * an *instance* op on a *static* field is legal, and CoreCLR accepts it a few lines later
    ///   ("We are using ldfld/a on a static field. We allow it, but need to get side-effect from
    ///   obj."), evaluating the receiver for its side effects and discarding it. PawPrint has not
    ///   implemented that form: none of these ops has a path to static storage. So we reject it
    ///   as unimplemented.
    ///
    /// No compiler emits either mismatch, but a *scope* operand reaches both in three lines of guest
    /// code, since `ILGenerator.Emit` takes whatever `FieldInfo` it is handed and does not check the
    /// opcode against it. Measured on real .NET: a static op on an instance field is a catchable
    /// `InvalidProgramException`, and an instance op on a static field runs. Neither is implemented
    /// here, in either token universe; see `docs/divergences.md`.
    let private checkFieldStaticness
        (opName : string)
        (verb : string)
        (expectedStatic : bool)
        (alternativeOp : string)
        (field : FieldInfo<'typeGeneric, 'fieldGeneric>)
        : unit
        =
        if field.Attributes.HasFlag FieldAttributes.Static <> expectedStatic then
            // The adjective describes the kind the field turned out to be, i.e. the wrong one.
            let adjective = if expectedStatic then "instance" else "static"

            let reason =
                if expectedStatic then
                    "This indicates invalid IL or a misresolved field token."
                else
                    "ECMA-335 permits this form — the receiver is evaluated for its side effects and discarded — but PawPrint does not implement it; it has no path from an instance field op to static storage."

            failwith
                $"%s{opName} cannot %s{verb} %s{adjective} field %O{AssemblyDefinitionName.simpleName field.DeclaringType.AssemblyFullName}.%s{field.DeclaringType.Namespace}.%s{field.DeclaringType.Name}::%s{field.Name}; use %s{alternativeOp}. %s{reason}"

    let executeStfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread
        let logger = ctx.Logger

        let state, field, declaringAssy = resolveFieldToken "stfld" "store to" ctx state

        do
            logger.LogTrace (
                "Storing in object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                field.DeclaringType.AssemblyFullName,
                field.DeclaringType.Name,
                field.Name,
                field.Signature
            )

        checkFieldStaticness "stfld" "store" false "stsfld" field

        let valueToStore, state = IlMachineState.popEvalStack thread state
        let currentObj, state = IlMachineState.popEvalStack thread state

        let state, declaringTypeHandle, typeGenerics =
            ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

        let fieldId = FieldId.metadata declaringTypeHandle field.Handle field.Name

        let state, zero, concreteTypeHandle =
            IlMachineState.cliTypeZeroOf
                loggerFactory
                baseClassTypes
                declaringAssy
                field.Signature
                typeGenerics
                ImmutableArray.Empty // field can't have its own generics
                state

        let valueToStore = EvalStackValue.toCliTypeCoerced zero valueToStore

        match currentObj with
        | EvalStackValue.NullObjectRef ->
            IlMachineStateExecution.raiseOpcodeFault loggerFactory baseClassTypes OpcodeFault.NullReference thread state
        | _ ->

        let state =
            match currentObj with
            | EvalStackValue.Int32 _ -> failwith "unexpectedly setting field on an int"
            | EvalStackValue.Int64 _ -> failwith "unexpectedly setting field on an int64"
            | EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                failwith
                    $"TODO: stfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTableAuxiliaryDataPtr %O{methodTableFor}; synthetic MethodTableAuxiliaryData cache writes are not modelled"
            | EvalStackValue.NativeInt _ -> failwith "unexpectedly setting field on a nativeint"
            | EvalStackValue.Float _ -> failwith "unexpectedly setting field on a float"
            | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
            | EvalStackValue.ObjectRef addr ->
                match
                    RuntimeFieldProjection.tryProjectFieldStore baseClassTypes field addr valueToStore state.ManagedHeap
                with
                | Some heap ->
                    { state with
                        ManagedHeap = heap
                    }
                | None ->
                    { state with
                        ManagedHeap = ManagedHeap.setFieldById addr fieldId valueToStore state.ManagedHeap
                    }
            | EvalStackValue.ManagedPointer src ->
                IlMachineState.writeManagedByrefWithBase
                    baseClassTypes
                    state
                    (ManagedPointerSource.appendProjection (ByrefProjection.Field fieldId) src)
                    valueToStore
            | EvalStackValue.UserDefinedValueType _ -> failwith "todo"

        state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeStsfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread
        let logger = ctx.Logger

        let state, field, declaringAssy = resolveFieldToken "stsfld" "store to" ctx state

        checkFieldStaticness "stsfld" "store" true "stfld" field

        // A `[ThreadStatic]` field is written into the storing thread's own slot. This holds
        // uniformly, with no `.cctor` special-casing: `[ThreadStatic] static int x = 5;` is legal
        // C#, and only the thread that runs the initialiser ends up seeing 5 — which is exactly
        // the real .NET behaviour.
        let owner = StaticOwner.forField thread field

        // See `executeLdfld` for the rationale: avoid `activeAssy.TypeDefs.[…]` because a
        // cross-assembly MemberReference yields a TypeDef handle that is only valid in the
        // declaring assembly's metadata.
        logger.LogTrace (
            "Storing in static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
            field.DeclaringType.AssemblyFullName,
            field.DeclaringType.Name,
            field.Name,
            field.Signature
        )

        let state, declaringTypeHandle, typeGenerics =
            ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Blocked (state, blockedBy) -> state, WhatWeDid.BlockedOnClassInit blockedBy
        | NothingToDo state ->

        let popped, state = IlMachineState.popEvalStack thread state

        let state, zero, concreteTypeHandle =
            IlMachineState.cliTypeZeroOf
                loggerFactory
                baseClassTypes
                declaringAssy
                field.Signature
                typeGenerics
                ImmutableArray.Empty // field can't have its own generics
                state

        let toStore = EvalStackValue.toCliTypeCoerced zero popped

        let state =
            IlMachineState.setStatic
                owner
                declaringTypeHandle
                (ComparableFieldDefinitionHandle.Make field.Handle)
                toStore
                state
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    let executeLdfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread
        let logger = ctx.Logger

        let state, field, _declaringAssy = resolveFieldToken "ldfld" "load from" ctx state

        // The declaring type's name is carried on `field.DeclaringType` directly; we
        // do not dereference `Definition.Get` against `activeAssy.TypeDefs`
        // because a cross-assembly MemberReference (e.g. ValueTuple<,>.Item1 referenced
        // from a guest assembly) yields a TypeDef handle valid only in the declaring
        // assembly's metadata, not the active assembly's.
        logger.LogTrace (
            "Loading object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
            field.DeclaringType.AssemblyFullName,
            field.DeclaringType.Name,
            field.Name,
            field.Signature
        )

        checkFieldStaticness "ldfld" "load" false "ldsfld" field

        let currentObj, state = IlMachineState.popEvalStack thread state

        let state, declaringTypeHandle, typeGenerics =
            ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

        let fieldId = FieldId.metadata declaringTypeHandle field.Handle field.Name

        match currentObj with
        | EvalStackValue.NullObjectRef ->
            IlMachineStateExecution.raiseOpcodeFault loggerFactory baseClassTypes OpcodeFault.NullReference thread state
        | _ ->

        let state =
            match currentObj with
            | EvalStackValue.Int32 i -> failwith $"todo: Ldfld on an int32 object reference (%O{i})"
            | EvalStackValue.Int64 int64 -> failwith "todo: int64"
            | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr methodTableFor) ->
                match
                    MethodTableProjection.tryProjectFieldForRuntimeTypeHandleTarget
                        loggerFactory
                        baseClassTypes
                        field
                        methodTableFor
                        state
                with
                | Some (value, state) -> IlMachineState.pushToEvalStack value thread state
                | None ->
                    failwith
                        $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through RuntimeTypeHandleTarget %O{methodTableFor}"
            | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr methodTableFor) ->
                match
                    MethodTableProjection.tryProjectFieldForRuntimeTypeHandleTarget
                        loggerFactory
                        baseClassTypes
                        field
                        methodTableFor
                        state
                with
                | Some (value, state) -> IlMachineState.pushToEvalStack value thread state
                | None ->
                    failwith
                        $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTablePtr %O{methodTableFor}"
            | EvalStackValue.NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                match MethodTableProjection.tryProjectAuxiliaryDataField baseClassTypes field methodTableFor state with
                | Some (value, state) -> IlMachineState.pushToEvalStack value thread state
                | None ->
                    failwith
                        $"TODO: ldfld {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTableAuxiliaryDataPtr %O{methodTableFor}"
            | EvalStackValue.NativeInt nativeIntSource -> failwith $"todo: nativeint {nativeIntSource}"
            | EvalStackValue.Float f -> failwith "todo: float"
            | EvalStackValue.NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
            | EvalStackValue.ObjectRef managedHeapAddress ->
                match RuntimeFieldProjection.tryProjectFieldLoad baseClassTypes field managedHeapAddress state with
                | Some value -> IlMachineState.pushToEvalStack value thread state
                | None ->
                    // `get` discriminates "this is an array" from "this address is not
                    // allocated at all".
                    IlMachineState.pushToEvalStack
                        (AllocatedNonArrayObject.DereferenceFieldById
                            fieldId
                            (ManagedHeap.get managedHeapAddress state.ManagedHeap))
                        thread
                        state
            | EvalStackValue.ManagedPointer src ->
                let currentValue =
                    IlMachineState.readManagedByrefField baseClassTypes state src fieldId

                IlMachineState.pushToEvalStack currentValue thread state
            | EvalStackValue.UserDefinedValueType vt ->
                let result = vt |> CliValueType.DereferenceFieldById fieldId

                IlMachineState.pushToEvalStack result thread state

        state
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeLdflda (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread

        let ptr, state = IlMachineState.popEvalStack thread state

        // TODO: generics
        let state, field, _declaringAssy = resolveFieldToken "ldflda" "load from" ctx state

        checkFieldStaticness "ldflda" "take the address of" false "ldsflda" field

        let state, declaringTypeHandle, _typeGenerics =
            ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

        let fieldId = FieldId.metadata declaringTypeHandle field.Handle field.Name

        match ptr with
        | NullObjectRef ->
            IlMachineStateExecution.raiseOpcodeFault loggerFactory baseClassTypes OpcodeFault.NullReference thread state
        | _ ->

        let state, projection =
            match ptr with
            | Int32 _
            | Int64 _
            | Float _ -> failwith "expected pointer type"
            | NativeInt (NativeIntSource.MethodTableAuxiliaryDataPtr methodTableFor) ->
                match
                    MethodTableProjection.tryProjectAuxiliaryDataFieldAddress
                        loggerFactory
                        baseClassTypes
                        field
                        methodTableFor
                        state
                with
                | Some (ptr, state) -> state, ptr
                | None ->
                    failwith
                        $"TODO: ldflda {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through MethodTableAuxiliaryDataPtr %O{methodTableFor}; this auxiliary-data field has no synthetic address modelled"
            | NativeInt (NativeIntSource.TypeDescPtr typeDescFor) ->
                match
                    MethodTableProjection.tryProjectTypeDescFieldAddress
                        loggerFactory
                        baseClassTypes
                        field
                        typeDescFor
                        state
                with
                | Some (ptr, state) -> state, ptr
                | None ->
                    failwith
                        $"TODO: ldflda {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through TypeDescPtr %O{typeDescFor}; this TypeDesc field has no synthetic address modelled"
            | NativeInt nativeIntSource ->
                failwith
                    $"TODO: ldflda {field.DeclaringType.Namespace}.{field.DeclaringType.Name}::{field.Name} through native pointer %O{nativeIntSource}"
            | ManagedPointer src -> state, ManagedPointerSource.appendProjection (ByrefProjection.Field fieldId) src
            | NullObjectRef -> failwith "unreachable: NullObjectRef handled above"
            | ObjectRef addr ->
                match RuntimeFieldProjection.tryProjectFieldAddress baseClassTypes field addr state with
                | Some ptr -> state, ptr
                | None -> state, ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, fieldId), [])
            | UserDefinedValueType evalStackValueUserType -> failwith "todo"

        let result = EvalStackValue.ManagedPointer projection

        state
        |> IlMachineState.pushToEvalStack' result thread
        |> IlMachineState.advanceProgramCounter thread
        |> Tuple.withRight WhatWeDid.Executed

    let executeLdsfld (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread
        let logger = ctx.Logger

        let state, field, declaringAssy = resolveFieldToken "ldsfld" "load from" ctx state

        checkFieldStaticness "ldsfld" "load" true "ldfld" field

        // A `[ThreadStatic]` field is read from the loading thread's own slot; a thread that has
        // never written it simply misses and gets `cliTypeZeroOf` below, which is exactly .NET's
        // zero-initialisation guarantee.
        let owner = StaticOwner.forField thread field

        do
            let declaring =
                state.LoadedAssembly field.DeclaringType.AssemblyFullName
                |> Option.get
                |> fun a -> a.TypeDefs.[field.DeclaringType.Definition.Get]

            logger.LogTrace (
                "Loading from static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                field.DeclaringType.AssemblyFullName,
                declaring.Name,
                field.Name,
                field.Signature
            )

        let state, declaringTypeHandle, typeGenerics =
            ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Blocked (state, blockedBy) -> state, WhatWeDid.BlockedOnClassInit blockedBy
        | NothingToDo state ->

        let fieldValue, state =
            match
                IlMachineState.getStatic
                    owner
                    declaringTypeHandle
                    (ComparableFieldDefinitionHandle.Make field.Handle)
                    state
            with
            | None when isSystemStringEmptyField baseClassTypes field ->
                let addr, state =
                    IlMachineState.internCanonicalEmptyString loggerFactory baseClassTypes state

                let newVal = CliType.ObjectRef (Some addr)

                newVal,
                IlMachineState.setStatic
                    owner
                    declaringTypeHandle
                    (ComparableFieldDefinitionHandle.Make field.Handle)
                    newVal
                    state
            | None when isCastHelpersTableField baseClassTypes field ->
                let addr, state =
                    IlMachineState.internCastCacheSentinelTable loggerFactory baseClassTypes state

                let newVal = CliType.ObjectRef (Some addr)

                newVal,
                IlMachineState.setStatic
                    owner
                    declaringTypeHandle
                    (ComparableFieldDefinitionHandle.Make field.Handle)
                    newVal
                    state
            | None ->
                let state, newVal, concreteTypeHandle =
                    IlMachineState.cliTypeZeroOf
                        loggerFactory
                        baseClassTypes
                        declaringAssy
                        field.Signature
                        typeGenerics
                        ImmutableArray.Empty // field can't have its own generics
                        state

                newVal,
                IlMachineState.setStatic
                    owner
                    declaringTypeHandle
                    (ComparableFieldDefinitionHandle.Make field.Handle)
                    newVal
                    state
            | Some v -> v, state

        do
            let declaring =
                state
                    .LoadedAssembly(field.DeclaringType.AssemblyFullName)
                    .Value.TypeDefs.[field.DeclaringType.Definition.Get]

            logger.LogTrace (
                "Loaded from static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType}), value {LoadedValue}",
                field.DeclaringType.AssemblyFullName,
                declaring.Name,
                field.Name,
                field.Signature,
                fieldValue
            )

        let state =
            IlMachineState.pushToEvalStack fieldValue thread state
            |> IlMachineState.advanceProgramCounter thread

        state, WhatWeDid.Executed

    let executeLdsflda (ctx : UnaryMetadataIlOpContext) (state : IlMachineState) : IlMachineState * WhatWeDid =
        let loggerFactory = ctx.LoggerFactory
        let baseClassTypes = ctx.BaseClassTypes
        let thread = ctx.Thread


        // TODO: check whether we should throw FieldAccessException

        let state, field, declaringAssy = resolveFieldToken "ldsflda" "load" ctx state

        checkFieldStaticness "ldsflda" "take the address of" true "ldflda" field

        // Resolved before the field-RVA branch below, both so the `[ThreadStatic]`-implies-not-RVA
        // assert inside `forField` fires on every path, and because the owner is baked into the
        // byref we hand out: the pointer addresses *this* thread's slot forever after, even if
        // some other thread dereferences it.
        let owner = StaticOwner.forField thread field

        let state, declaringTypeHandle, typeGenerics =
            ExecutionConcretization.concretizeFieldForExecution loggerFactory baseClassTypes thread field state

        match IlMachineStateExecution.loadClass loggerFactory baseClassTypes declaringTypeHandle thread state with
        | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | ThrowingTypeInitializationException state -> state, WhatWeDid.ThrowingTypeInitializationException
        | Blocked (state, blockedBy) -> state, WhatWeDid.BlockedOnClassInit blockedBy
        | NothingToDo state ->

        match
            IlMachineState.peByteRangeForFieldRva loggerFactory baseClassTypes declaringAssy field typeGenerics state
        with
        | state, Some peByteRange ->
            let state, ptr =
                IlMachineState.peByteRangePointer loggerFactory baseClassTypes peByteRange state

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
        | state, None ->
            // TODO: if field type is unmanaged, push an unmanaged pointer
            let fieldHandle = ComparableFieldDefinitionHandle.Make field.Handle

            let state =
                match IlMachineState.getStatic owner declaringTypeHandle fieldHandle state with
                | Some _ -> state
                | None when isSystemStringEmptyField baseClassTypes field ->
                    // See `isSystemStringEmptyField` for why this is special-cased.
                    let addr, state =
                        IlMachineState.internCanonicalEmptyString loggerFactory baseClassTypes state

                    IlMachineState.setStatic owner declaringTypeHandle fieldHandle (CliType.ObjectRef (Some addr)) state
                | None when isCastHelpersTableField baseClassTypes field ->
                    // See `isCastHelpersTableField` for why this is special-cased. The BCL
                    // does not actually take the address of `s_table`, but installing the
                    // sentinel symmetrically with `ldsfld` keeps the two arms consistent
                    // and defends against future BCL changes.
                    let addr, state =
                        IlMachineState.internCastCacheSentinelTable loggerFactory baseClassTypes state

                    IlMachineState.setStatic owner declaringTypeHandle fieldHandle (CliType.ObjectRef (Some addr)) state
                | None ->
                    // Field is not yet initialised
                    let state, zero, _concreteTypeHandle =
                        IlMachineState.cliTypeZeroOf
                            loggerFactory
                            baseClassTypes
                            declaringAssy
                            field.Signature
                            typeGenerics
                            ImmutableArray.Empty // field can't have its own generics
                            state

                    IlMachineState.setStatic owner declaringTypeHandle fieldHandle zero state

            let ptr =
                ManagedPointerSource.Byref (ByrefRoot.StaticField (declaringTypeHandle, fieldHandle, owner), [])

            state
            |> IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer ptr) thread
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
