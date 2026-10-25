namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// The rule of a type parameter's constraints that an argument fails, as CoreCLR's
/// `TypeVarTypeDesc::SatisfiesConstraints` (typedesc.cpp) checks them.
[<RequireQualifiedAccess>]
type ConstraintRule =
    /// `struct`: the argument is not a value type, or is an instantiation of `System.Nullable`.
    | NonNullableValueType
    /// `class`: the argument is a value type.
    | ReferenceType
    /// `new()`: the argument is abstract, or has no public instance constructor taking nothing
    /// (a value type always has one).
    | ParameterlessConstructor
    /// The argument is byref-like, and the parameter does not carry `allows ref struct`.
    | ByRefLike
    /// The argument cannot be cast to this general constraint, instantiated as the declarer is.
    | Assignable of required : ConcreteTypeHandle

/// An argument of an instantiation that its type parameter's constraints reject.
type ConstraintViolation =
    {
        /// The index of the type parameter among its owner's.
        Parameter : int
        Argument : ConcreteTypeHandle
        /// The first rule it fails, in the order CoreCLR checks them.
        Rule : ConstraintRule
    }

/// Whether an instantiation's arguments satisfy its type parameters' constraints.
[<RequireQualifiedAccess>]
type ConstraintVerdict =
    | Satisfied
    /// The first argument, in order, that its parameter's constraints reject.
    | Violated of ConstraintViolation
    /// Up to this parameter every argument satisfies its constraints, and this one's argument, an
    /// abstract class or an interface, can be cast to a constraint that is an interface with static
    /// virtual methods. CoreCLR then also requires each of those methods to resolve on the argument
    /// (typedesc.cpp:1686), which this does not decide; should that pass, later arguments are checked
    /// in turn.
    | Undecided of parameter : int * required : ConcreteTypeHandle

/// An argument CoreCLR does not admit in any instantiation (`Generics::CheckInstantiation`,
/// generics.cpp).
[<RequireQualifiedAccess>]
type MalformedArgument =
    | Byref
    | TypedReference
    | Void
    | Pointer
    | FunctionPointer

/// Whether a closed instantiation of a generic type or method is one CoreCLR admits: well formed
/// (`malformedArgument`), and satisfying its type parameters' constraints (`satisfiesClosed`).
///
/// CoreCLR checks a type's constraints as it completes the instantiation's load to `CLASS_LOADED`
/// (`SatisfiesClassConstraints`, from `MethodTable::DoFullyLoad`), raising `TypeLoadException`; and a
/// method's as it first creates the instantiated method (`InstantiatedMethodDesc::NewInstantiatedMethodDesc`),
/// raising `VerificationException`. It exempts an instantiation that code shared between
/// instantiations sees, one with an argument in its canonical form; whether an instantiation is such
/// is the caller's to decide.
///
/// Whatever loads an assembly or registers a concrete type on the way returns the state it leaves
/// behind; `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module ConstraintSatisfaction =

    /// Not yet implemented: admits every argument.
    let malformedArgument
        (_ : BaseClassTypes<DumpedAssembly>)
        (_ : TypeSystemState)
        (_ : ConcreteTypeHandle list)
        : (int * MalformedArgument) option
        =
        None

    /// Not yet implemented: admits every instantiation.
    let satisfiesClosed
        (_ : ILoggerFactory)
        (_ : string seq)
        (_ : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (_ : string)
        (_ : ImmutableArray<ConcreteTypeHandle>)
        (_ : ImmutableArray<ConcreteTypeHandle>)
        (_ : GenericParamFromMetadata ImmutableArray)
        (_ : ConcreteTypeHandle list)
        : TypeSystemState * Result<ConstraintVerdict, TypeBindingFailure>
        =
        state, Ok ConstraintVerdict.Satisfied
