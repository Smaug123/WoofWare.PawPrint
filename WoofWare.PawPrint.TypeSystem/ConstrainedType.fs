namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// What CoreCLR dispatches on an enum's underlying primitive, where it retargets a `constrained.`
/// call on the enum to the primitive (`ConstrainedType.EnumUnderlying`).
[<RequireQualifiedAccess>]
type RetargetedCall =
    /// The method the call names, an interface's.
    | Named
    /// `Object.GetHashCode` (`ConstrainedType.objectGetHashCode`). The call names a class's method, which CoreCLR resolves on
    /// a value type by the slot it holds (`GetMethodDescForSlot`, in
    /// `MethodTable::TryResolveConstraintMethodApprox`), whichever class declares it and whether or
    /// not the primitive derives from that class; and every class holds `Object.GetHashCode` in that
    /// slot.
    | ObjectGetHashCode

/// The type CoreCLR resolves a `constrained.` call against, which is not always the type the prefix
/// names (`CEEInfo::getCallInfo`, jitinterface.cpp).
[<RequireQualifiedAccess>]
type ConstrainedType =
    /// The type the prefix names.
    | AsNamed of ConcreteTypeHandle
    /// The prefix names the enum `enum`, and the call names a method holding the slot number that
    /// `Object.GetHashCode` holds in `Object`'s method table. CoreCLR resolves the call as though
    /// the prefix named `underlying`, the enum's underlying primitive, and passes the enum's address
    /// as the primitive's, unboxed.
    ///
    /// The comparison is of slot numbers alone, each in the table of the method's own declaring
    /// type, so a method of an unrelated interface in that slot is retargeted too.
    | EnumUnderlying of enum : ConcreteTypeHandle * underlying : ConcreteTypeHandle * dispatches : RetargetedCall
    /// The prefix names an enum, and whether CoreCLR retargets the call is not known: the method
    /// holds no slot PawPrint places (a method the runtime synthesises), or the enum has no single
    /// instance field `value__` to read its underlying type from.
    | Undecided of enum : ConcreteTypeHandle

/// What CoreCLR's `MethodTable::TryResolveConstraintMethodApprox` (methodtable.cpp) resolves a
/// `constrained.` call of a class's instance method to, on a value type
/// (`ConstrainedType.resolveClassMethodOnValueType`).
[<RequireQualifiedAccess>]
type ClassMethodOnValueType =
    /// This method, which the call runs on the receiver's address, unboxed: the method holding, in
    /// the value type's vtable, the slot the method called holds in its own declaring type's
    /// (`GetMethodDescForSlot`), where the value type declares it; or the method called itself where
    /// a value type declares that, which need not be the value type the prefix names.
    | Resolves of MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
    /// No method: the method called is not virtual, or the value type inherits the method in that
    /// slot from a class. `getCallInfo` (jitinterface.cpp) then boxes the receiver
    /// (`CORINFO_BOX_THIS`) and calls on the box the method the call names, which runs whatever
    /// the box's vtable holds in that slot if the method can be overridden, and the method itself
    /// if it cannot.
    | Boxes
    /// The method called holds a slot beyond the vtable of every class that both the value type and
    /// the method's own class derive from, which therefore means something else in the value type's
    /// table, if anything: `GetMethodDescForSlot` reads past the value type's vtable, or finds a
    /// method of the value type's own, which CoreCLR then runs whatever its signature. Or it holds no
    /// slot PawPrint places (a method the runtime synthesises).
    | Undecided

[<RequireQualifiedAccess>]
module ConstrainedType =

    let private operation = "resolving a constrained. call on an enum"

    /// `Object.GetHashCode`, as CoreLib's binder names it, by its signature (`IM_RetInt`).
    let objectGetHashCode
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        baseClassTypes.Object.Methods
        |> List.filter (fun m ->
            m.Name = "GetHashCode"
            && not m.IsStatic
            && m.Signature.ParameterTypes.IsEmpty
            && m.Signature.ReturnType = MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32)
        )
        |> List.exactlyOne

    /// `MethodDesc::GetSlot`: the slot a method holds in the method table of its own declaring type,
    /// whichever type that is. `None` for a method PawPrint does not place.
    let private slotOf
        (operation : string)
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (method : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : TypeSystemState * int option
        =
        match method.TryDeclaringType with
        | None -> state, None
        | Some declaringType ->
            let state, table =
                ConcreteMethodTable.slotTableOfDefinition
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    state
                    declaringType.Identity

            state, MethodTableLayout.slotIndexInTable (method.DeclaringAssemblyFullName, method.IdentityKey) table

    /// The type CoreCLR resolves a `constrained.` call of `called` against, where the prefix names
    /// the closed type `named`.
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (named : ConcreteTypeHandle)
        (called : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : TypeSystemState * ConstrainedType
        =
        match TypeSystemState.isEnumValueType loggerFactory dotnetRuntimeDirs baseClassTypes state named with
        | state, false -> state, ConstrainedType.AsNamed named
        | state, true ->

        let state, hashCodeSlot =
            match
                slotOf operation loggerFactory dotnetRuntimeDirs baseClassTypes state (objectGetHashCode baseClassTypes)
            with
            | state, Some slot -> state, slot
            | _, None -> failwith $"BUG: %s{operation}: Object.GetHashCode holds no slot in Object's method table"

        match slotOf operation loggerFactory dotnetRuntimeDirs baseClassTypes state called with
        | state, None -> state, ConstrainedType.Undecided named
        | state, Some slot when slot <> hashCodeSlot -> state, ConstrainedType.AsNamed named
        | state, Some _ ->
            match TypeSystemState.enumUnderlyingHandle loggerFactory dotnetRuntimeDirs baseClassTypes state named with
            | None -> state, ConstrainedType.Undecided named
            | Some (state, underlying) ->
                // `slotOf` found a slot, so the method has a declaring type.
                let _, declaring =
                    MethodTableLayout.definitionMetadata
                        operation
                        state._LoadedAssemblies
                        called.RequiredDeclaringType.Identity

                let dispatches =
                    if declaring.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Interface then
                        RetargetedCall.Named
                    else
                        RetargetedCall.ObjectGetHashCode

                state, ConstrainedType.EnumUnderlying (named, underlying, dispatches)

    /// What CoreCLR's `MethodTable::TryResolveConstraintMethodApprox` (methodtable.cpp) resolves a
    /// `constrained.` call of `called`, an instance method of a class, to, where the prefix names
    /// the closed value type `valueType` (or `ConstrainedType.resolve` resolves it against that).
    let resolveClassMethodOnValueType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (valueType : ConcreteTypeHandle)
        (called : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * ClassMethodOnValueType
        =
        let operation = "resolving a constrained. call of a class's method on a value type"

        let valueTypeIdentity =
            match TypeSystemState.tryGetConcreteTypeInfo state valueType with
            | Some (concrete, ty) when LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies ty ->
                concrete.Identity
            | _ -> failwith $"BUG: %s{operation}: %O{valueType} is not a value type"

        if called.IsStatic then
            failwith $"BUG: %s{operation}: %s{called.Name} is static"

        match called.TryDeclaringType with
        | None -> state, ClassMethodOnValueType.Undecided
        | Some declaringType ->

        let _, declaring =
            MethodTableLayout.definitionMetadata operation state._LoadedAssemblies declaringType.Identity

        if declaring.TypeAttributes.HasFlag System.Reflection.TypeAttributes.Interface then
            failwith $"BUG: %s{operation}: %s{called.Name} is an interface's"

        if not called.IsVirtual then
            // "The pMD will be NULL if calling a non-virtual instance methods on System.Object".
            state, ClassMethodOnValueType.Boxes
        elif LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies declaring then
            // A value type's virtual has no vtable slot of its own (its unboxing stub holds that), so
            // `TryResolveConstraintMethodApprox` takes the method itself ("GetMethodDescForSlot would AV
            // for this slot").
            state, ClassMethodOnValueType.Resolves called
        else

        match slotOf operation loggerFactory dotnetRuntimeDirs baseClassTypes state called with
        | state, None -> state, ClassMethodOnValueType.Undecided
        | state, Some slot ->

        // A definition and the classes it derives from, itself first.
        let rec chainOf (state : TypeSystemState) (identity : ResolvedTypeIdentity) =
            let owner = ConcreteMethodTable.ownerOfDefinition operation state identity

            let _, typeInfo =
                MethodTableLayout.definitionMetadata operation state._LoadedAssemblies identity

            match
                ConcreteMethodTable.baseOfDefinition
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    state
                    owner
                    typeInfo
            with
            | state, None -> state, [ identity ]
            | state, Some (parent, _) ->
                let state, rest = chainOf state parent
                state, identity :: rest

        let state, valueTypeChain = chainOf state valueTypeIdentity
        let state, calledChain = chainOf state declaringType.Identity

        // `GetMethodDescForSlot` reads the slot by its number alone, which means the same in both
        // tables only below the vtable of a class both derive from: each type's table begins with
        // its parent's (`CopyParentVtable`). Beyond that it reads past the value type's vtable, or
        // finds a method of another signature.
        let state, sharedSlots =
            match
                calledChain
                |> List.tryFind (fun identity -> List.contains identity valueTypeChain)
            with
            | None -> state, 0
            | Some shared ->
                ConcreteMethodTable.numVirtualsOfDefinition
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    state
                    shared

        if slot >= sharedSlots then
            state, ClassMethodOnValueType.Undecided
        else

        match
            ConcreteMethodTable.dispatchTableOfClosed
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                operation
                state
                valueType
        with
        | _, None -> failwith $"BUG: %s{operation}: value type %O{valueType} has no method table"
        | state, Some table ->
            let occupant = table.Occupants.[slot]

            // "Only return a method if the value type itself declares the method".
            if occupant.DeclaredBy.Identity = valueTypeIdentity then
                state, ClassMethodOnValueType.Resolves occupant.Method
            else
                state, ClassMethodOnValueType.Boxes
