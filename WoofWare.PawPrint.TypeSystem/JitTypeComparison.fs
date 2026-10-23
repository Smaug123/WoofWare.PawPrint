namespace WoofWare.PawPrint

/// A type as the JIT sees it where a method it compiles names it.
type JitTypeView =
    {
        /// The type the instance names.
        Type : ConcreteTypeHandle
        /// Whether the JIT sees `Type` only in its canonical form, as shared code does where the
        /// spelling names a type argument that canonicalises to `__Canon`
        /// (`CodeSharing.namesSharedArgument`).
        Canonical : bool
    }

/// What the JIT learns as it compiles a method by asking the runtime to compare two types it
/// names: CoreCLR's `CEEInfo::compareTypesForEquality` and `CEEInfo::compareTypesForCast`
/// (jitinterface.cpp), each of which answers `Must`, `MustNot` or `May`.
[<RequireQualifiedAccess>]
module JitTypeComparison =

    /// Whether `compareTypesForEquality` answers `Must`: where neither type is canonical, whether
    /// they are the same type. Where either is, it answers `MustNot` at best.
    let provesEqual (a : JitTypeView) (b : JitTypeView) : bool =
        not a.Canonical && not b.Canonical && a.Type = b.Type

    /// Whether `compareTypesForCast` decides a cast from `from`, a value type, to `target` either
    /// way (`Must` or `MustNot`) rather than answering `May`. `None` where its answer turns on
    /// `CanCastTo` over a canonical form, which PawPrint does not build: a cast from a canonical
    /// form to a generic interface.
    let decidesCastFromValueType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (from : JitTypeView)
        (target : JitTypeView)
        : bool option
        =
        // It never decides a cast from a COM object or a type marked `IDynamicInterfaceCastable`,
        // but those are classes (`MethodTableBuilder` marks no value type).
        if TypeSystemState.isReferenceTypeHandle baseClassTypes "JitTypeComparison" state from.Type then
            failwith $"BUG: JitTypeComparison.decidesCastFromValueType: %O{from.Type} is a reference type"

        let nominal =
            match target.Type with
            | ConcreteTypeHandle.Concrete _ -> TypeSystemState.tryGetConcreteTypeInfo state target.Type
            | ConcreteTypeHandle.OneDimArrayZero _
            | ConcreteTypeHandle.Array _
            | ConcreteTypeHandle.Byref _
            | ConcreteTypeHandle.Pointer _
            | ConcreteTypeHandle.FunctionPointer _ -> None

        let toNullable =
            nominal
            |> Option.exists (fun (concrete, _) -> concrete.Identity = baseClassTypes.Nullable.Identity)

        if toNullable then
            Some false
        elif not from.Canonical && not target.Canonical then
            Some true
        elif from.Canonical && not target.Canonical then
            match nominal with
            | Some (concrete, info) when info.IsInterface ->
                // `Must` where the canonical form casts, and otherwise `MustNot` for an interface
                // with no type arguments; for one with them, `May`, since a `__Canon` in the form
                // may hide a match.
                if concrete.Generics.IsEmpty then Some true else None
            | _ -> Some false
        else
            Some false
