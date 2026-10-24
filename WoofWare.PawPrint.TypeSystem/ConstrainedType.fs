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
        state, ConstrainedType.AsNamed named
