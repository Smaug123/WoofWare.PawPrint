namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

open NativeRuntimeTypeHelpers

/// QCalls declared on `System.Runtime.CompilerServices.CastHelpers`, the managed home of the
/// cast helpers the JIT would otherwise emit calls to.
[<RequireQualifiedAccess>]
module NativeCastHelpers =
    /// CoreCLR's `TypeHandle::IsTypeDesc` (`typehandle.h`): a handle is a `TypeDesc` rather
    /// than a `MethodTable` exactly when it names a byref, pointer, function pointer or
    /// generic variable. Arrays of every rank *do* have MethodTables and are not TypeDescs;
    /// nor is an open generic type definition, which has a canonical MethodTable of its own,
    /// nor an open constructed type, which CoreCLR's class loader gives a MethodTable of its
    /// own too (`TypeVarTypeDesc::LoadConstraints`, `vm/typedesc.cpp:826`).
    ///
    /// `ObjIsInstanceOfCore` answers a flat `false` for a TypeDesc target without consulting
    /// the structural walk at all.
    let private isTypeDescTarget (target : RuntimeTypeHandleTarget) : bool =
        match target with
        // `CreateMinimalMethodTable` produces a MethodTable, not a TypeDesc (methodtable.cpp:663);
        // this is the same answer `TypeHandleTag.forTarget` gives, and must stay consistent with it.
        | RuntimeTypeHandleTarget.DynamicMethodsClass _ -> false
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ -> true
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
        | RuntimeTypeHandleTarget.OpenConstructed _ -> false
        | RuntimeTypeHandleTarget.Closed handle ->
            match handle with
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> true
            | ConcreteTypeHandle.Concrete _
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _ -> false

    /// Is `target` an interface type? Only a nominal, MethodTable-backed handle can be one:
    /// CoreCLR asks this of `toTypeHnd.AsMethodTable()`, which is only reached once the
    /// TypeDesc case has already been excluded.
    let private isInterfaceTarget (state : IlMachineState) (target : RuntimeTypeHandleTarget) : bool =
        let identity =
            match target with
            // `CreateMinimalMethodTable` sets no category flag, so the class is exactly that: a
            // class, never an interface.
            | RuntimeTypeHandleTarget.DynamicMethodsClass _ -> None
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.map (fun ct -> ct.Identity)
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity
            // An open constructed type carries the interface flag of its definition; this is
            // the same reading `RuntimeType.IsActualInterface` performs for one.
            | RuntimeTypeHandleTarget.OpenConstructed (identity, _) -> Some identity
            | RuntimeTypeHandleTarget.Closed _
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> None

        match identity with
        | None -> false
        | Some identity ->
            match state.LoadedAssembly identity.Assembly with
            | None -> false
            | Some assy ->
                let typeInfo = assy.TypeDefs.[identity.TypeDefinition.Get]
                typeInfo.TypeAttributes.HasFlag TypeAttributes.Interface

    /// Obtain (interning if necessary) the `ConcreteTypeHandle` for the non-generic corelib
    /// interface `System.Runtime.InteropServices.IDynamicInterfaceCastable`.
    let private iDynamicInterfaceCastableHandle
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeInfo = baseClassTypes.IDynamicInterfaceCastable

        match AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes typeInfo.Identity with
        | Some handle -> state, handle
        | None ->
            let ct =
                ConcreteType.makeFromIdentity typeInfo.Identity typeInfo.Namespace typeInfo.Name ImmutableArray.Empty

            let handle, concreteTypes = AllConcreteTypes.add ct state.ConcreteTypes

            { state with
                ConcreteTypes = concreteTypes
            },
            handle

    /// CoreCLR's `ObjIsInstanceOfCore` (`src/coreclr/vm/jithelpers.cpp:385`), minus the cast
    /// cache — the caller has already missed in it, which is what "NoCacheLookup" names.
    ///
    /// The branch order is CoreCLR's and changes the answer, not merely the cost:
    ///
    /// 1. `Nullable::IsNullableForType` first, and deliberately never cached, because object
    ///    castability and type castability disagree on `T -> Nullable<T>`: the two share a
    ///    boxed representation, so a boxed `T` *is* a `Nullable<T>` for this question.
    /// 2. A `TypeDesc` target is a flat `false`. Taking this before the structural walk is
    ///    what keeps this function total: PawPrint's cast oracle refuses generic-parameter
    ///    targets outright (`IlMachineRuntimeMetadata.isRuntimeTypeHandleTargetAssignableTo`),
    ///    but CoreCLR never asks it about one.
    /// 3. Otherwise the ordinary structural walk (`MethodTable::CanCastTo`). An *open
    ///    constructed* target reaches this walk (it is not a TypeDesc) and the cast oracle
    ///    refuses it loudly; such a target only arises from a reflected generic-parameter
    ///    constraint.
    /// 4. If that failed and the target is an interface, CoreCLR consults the COM and
    ///    `IDynamicInterfaceCastable` fallbacks. COM is unreachable here — `FEATURE_COMINTEROP`
    ///    is Windows-only and PawPrint has no RCWs — but `IDynamicInterfaceCastable` is real
    ///    managed BCL surface, and answering it requires calling back into the guest's
    ///    `IsInterfaceImplemented`. PawPrint does not model that, so refuse loudly rather than
    ///    return a `false` that may be wrong.
    ///
    ///    That fallback is gated on `pMT->IsIDynamicInterfaceCastable()`, a MethodTable flag —
    ///    *not* on an assignability question. `MethodTableBuilder` sets it only inside a
    ///    `!IsValueClass()` guard (`vm/methodtablebuilder.cpp:1991`), so a struct implementing
    ///    the interface never takes the callback however it is tested. A boxed struct shares
    ///    its unboxed MethodTable, so the flag is absent there too and the cast simply answers
    ///    `false`. Reading the flag as "does this type implement the interface" would refuse a
    ///    case CoreCLR answers without complaint.
    let private objIsInstanceOfCore
        (operation : string)
        (ctx : NativeCallContext)
        (state : IlMachineState)
        (objType : ConcreteTypeHandle)
        (target : RuntimeTypeHandleTarget)
        : IlMachineState * bool
        =
        let nullableMatches =
            match target with
            | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
                RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
            | RuntimeTypeHandleTarget.Closed targetHandle ->
                IlMachineState.isNullableForType ctx.BaseClassTypes state targetHandle objType
            // `Nullable::IsNullableForType` compares the target against a *closed* `Nullable<U>`;
            // none of these can be one. (The object's own type is always closed, so an open
            // target could not match even if the shape were right.)
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.OpenConstructed _
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.MethodGenericParameter _ -> false

        if nullableMatches then
            state, true
        elif isTypeDescTarget target then
            state, false
        else

        let state, canCast =
            IlMachineState.isRuntimeTypeHandleTargetAssignableTo
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                (RuntimeTypeHandleTarget.Closed objType)
                target

        if canCast then
            state, true
        elif not (isInterfaceTarget state target) then
            state, false
        // `MethodTableBuilder` never sets the flag on a value class, so a boxed struct that
        // implements the interface still takes the ordinary `false`.
        elif argumentIsValueType ctx.BaseClassTypes state objType then
            state, false
        else

        let state, dynamicCastableHandle =
            iDynamicInterfaceCastableHandle ctx.BaseClassTypes state

        let state, objIsDynamicCastable =
            IlMachineState.isConcreteTypeAssignableTo
                ctx.LoggerFactory
                ctx.BaseClassTypes
                state
                objType
                dynamicCastableHandle

        if objIsDynamicCastable then
            // PawPrint's own `isinst` / `castclass` opcodes
            // (`UnaryMetadataObjectOps.castToReferenceType`) do not model this feature either;
            // they would answer `false` here without complaint. Closing that hole means
            // driving a managed callback out of the cast path generally, not just from this
            // QCall.
            failwith
                $"TODO: %s{operation}: object of type %O{objType} failed the structural cast to interface %O{target}, but its type implements System.Runtime.InteropServices.IDynamicInterfaceCastable; CoreCLR would call back into the guest's IsInterfaceImplemented (DynamicInterfaceCastable::IsInstanceOf, src/coreclr/vm/dynamicinterfacecastable.cpp) to decide, which PawPrint does not model"
        else
            state, false

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
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
        // The method name is deliberately not matched: the `[LibraryImport]` stub Roslyn
        // generates for this QCall carries a mangled local-function name
        // (`<IsInstanceOf_NoCacheLookup>g____PInvoke|4_0`). Entry point + declaring type +
        // signature already disambiguate it.
        //
        // Both `BOOL`s marshal to a plain `int32` here rather than `Interop.BOOL`, per the
        // stub as it appears in the CoreLib we execute.
        | "IsInstanceOf_NoCacheLookup",
          "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "CastHelpers",
          [ ConcretePointer (ConcreteVoid state.ConcreteTypes)
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "ObjectHandleOnStack",
                                              objectHandleGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            objectHandleGenerics.IsEmpty
            ->
            let operation = "CastHelpers.IsInstanceOf_NoCacheLookup"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let target =
                NativeCall.runtimeTypeHandleTargetOfEvalStackValue
                    operation
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let throwCastException =
                NativeCall.int32Argument operation instruction.Arguments.[1] <> 0

            // `ObjectHandleOnStack` is a byref to the caller's slot. CoreCLR reads it and never
            // writes it back on this path: the `&obj` it passes to `DynamicInterfaceCastable`
            // is its own GC-protected local, not the caller's slot.
            let objAddr =
                let ptr =
                    NativeCall.objectHandleOnStackTarget operation state "obj" instruction.Arguments.[2]

                match IlMachineState.readManagedByref ctx.BaseClassTypes state ptr with
                | CliType.ObjectRef (Some addr) -> addr
                | CliType.ObjectRef None ->
                    // `ObjIsInstanceOfCore` has `PRECONDITION(CheckPointer(pObject))` and
                    // dereferences immediately; every managed caller in `CastHelpers` checks
                    // for null before the slow path. A null here is a broken BCL contract, not
                    // a cast that should answer `false`.
                    failwith
                        $"%s{operation}: ObjectHandleOnStack held a null reference, but CoreCLR requires a non-null object here"
                | other -> failwith $"%s{operation}: expected ObjectRef behind ObjectHandleOnStack, got %O{other}"

            let objType = ManagedHeap.getObjectConcreteType objAddr state.ManagedHeap

            let state, canCast = objIsInstanceOfCore operation ctx state objType target

            if canCast || not throwCastException then
                let state =
                    IlMachineState.pushToEvalStack
                        (CliType.Numeric (CliNumericType.Int32 (if canCast then 1 else 0)))
                        ctx.Thread
                        state

                NativeHandlerResult.completed state |> Some
            else

            // `COMPlusThrowInvalidCastException(&obj, toTypeHnd)` (`src/coreclr/vm/excep.cpp`),
            // which formats IDS_EE_CANNOTCAST with both names from `TypeHandle::GetName`.
            let fromName =
                typeHandleGetName operation state (RuntimeTypeHandleTarget.Closed objType)

            let toName = typeHandleGetName operation state target

            if fromName = toName then
                // CoreCLR diverts equal names to `CheckAndThrowSameTypeAndAssemblyInvalidCastException`,
                // which throws IDS_EE_CANNOTCASTSAME ("[A]%1 cannot be cast to [B]%2. %3. %4.")
                // naming each type's assembly and load context. That message needs assembly
                // display details PawPrint does not assemble today, and emitting the ordinary
                // message instead would be silently wrong, so refuse.
                failwith
                    $"TODO: %s{operation}: cast from %s{fromName} to %s{toName} failed with identical formatted type names, so CoreCLR would throw the IDS_EE_CANNOTCASTSAME form naming both assemblies and load contexts; PawPrint does not build that message yet"

            let state =
                NativeHandlerResult.raiseExceptionWithMessage
                    ctx.BaseClassTypes.InvalidCastException
                    (Some $"Unable to cast object of type '%s{fromName}' to type '%s{toName}'.")
                    state

            Some state
        | _ -> None
