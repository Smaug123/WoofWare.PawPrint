namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

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
    | EnumUnderlying of enum : ConcreteTypeHandle * underlying : ConcreteTypeHandle
    /// The prefix names an enum, and whether CoreCLR retargets the call is not known: the method
    /// holds no slot PawPrint places (a method the runtime synthesises), or the enum has no single
    /// instance field `value__` to read its underlying type from.
    | Undecided of enum : ConcreteTypeHandle

[<RequireQualifiedAccess>]
module ConstrainedType =

    let private operation = "resolving a constrained. call on an enum"

    /// The type CoreCLR resolves a `constrained.` call of `called` against, where the prefix names
    /// the closed type `named`.
    let resolve
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (named : ConcreteTypeHandle)
        (called : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : TypeSystemState * ConstrainedType
        =
        match TypeSystemState.isEnumValueType loggerFactory dotnetRuntimeDirs baseClassTypes state named with
        | state, false -> state, ConstrainedType.AsNamed named
        | state, true ->

        // `MethodDesc::GetSlot`: the slot a method holds in the method table of its own declaring
        // type, whichever type that is.
        let slotOf
            (state : TypeSystemState)
            (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
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

        // CoreLib's binder names `Object.GetHashCode` by its signature, `IM_RetInt`.
        let getHashCode =
            baseClassTypes.Object.Methods
            |> List.filter (fun m ->
                m.Name = "GetHashCode"
                && not m.IsStatic
                && m.Signature.ParameterTypes.IsEmpty
                && m.Signature.ReturnType = MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32)
            )
            |> List.exactlyOne

        let state, hashCodeSlot =
            match slotOf state getHashCode with
            | state, Some slot -> state, slot
            | _, None -> failwith $"BUG: %s{operation}: Object.GetHashCode holds no slot in Object's method table"

        match slotOf state called with
        | state, None -> state, ConstrainedType.Undecided named
        | state, Some slot when slot <> hashCodeSlot -> state, ConstrainedType.AsNamed named
        | state, Some _ ->
            match TypeSystemState.enumUnderlyingHandle loggerFactory dotnetRuntimeDirs baseClassTypes state named with
            | None -> state, ConstrainedType.Undecided named
            | Some (state, underlying) -> state, ConstrainedType.EnumUnderlying (named, underlying)
