namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// An argument CoreCLR does not admit in any instantiation (`Generics::CheckInstantiation`,
/// generics.cpp).
[<RequireQualifiedAccess>]
type MalformedArgument =
    | Byref
    | TypedReference
    | Void
    | Pointer
    | FunctionPointer

/// <summary>
/// A type that CoreCLR's class loader refuses to load, whatever kind of type it is, and why.
/// </summary>
/// <remarks>
/// This is a fact about the type, not about where it is named: CoreCLR raises it wherever it loads
/// the type, and does not raise it where it never loads the type (a <c>CLASS</c> local the JIT need
/// not resolve, say). A consumer that turns a refusal into a guest exception must therefore know that
/// CoreCLR loads the type at that point. Each case says which exception CoreCLR raises; the type
/// names are those its <c>TypeLoadException.TypeName</c> reports.
/// </remarks>
[<RequireQualifiedAccess>]
type TypeLoadRefusal =
    /// An instantiation of `definition` whose argument at index `argument` is one no instantiation may
    /// have. `TypeLoadException` (`IDS_CLASSLOAD_INVALIDINSTANTIATION`) naming the definition.
    | InvalidInstantiation of definition : ResolvedTypeIdentity * argument : int * MalformedArgument
    /// A signature spelled `named` as `spelled`, and the definition is the other kind.
    /// `TypeLoadException` (`BFA_CLASSLOAD_VALUETYPEMISMATCH`) naming `named`.
    | KindMismatch of named : ResolvedTypeIdentity * spelled : SignatureTypeKind
    /// An instantiation of `definition`, which declares `declared` generic parameters, with
    /// `supplied` arguments. `TypeLoadException` (`IDS_CLASSLOAD_TYPEWRONGNUMGENERICARGS`) naming the
    /// definition.
    | WrongArity of definition : ResolvedTypeIdentity * declared : int * supplied : int
    /// The generic `definition` named without an instantiation where a type is required.
    /// `TypeLoadException` (`IDS_CLASSLOAD_GENERAL`) naming the definition.
    | UninstantiatedDefinition of definition : ResolvedTypeIdentity
    /// `!index`, where the context instantiates `inScope` type parameters.
    /// `BadImageFormatException`.
    | TypeParameterOutOfScope of index : int * inScope : int
    /// `!!index`, where the context instantiates `inScope` method parameters.
    /// `BadImageFormatException`.
    | MethodParameterOutOfScope of index : int * inScope : int

/// Functions on <see cref="TypeLoadRefusal" />.
[<RequireQualifiedAccess>]
module TypeLoadRefusal =

    /// The refusal, described for a run that stops on it.
    let describe (refusal : TypeLoadRefusal) : string =
        match refusal with
        | TypeLoadRefusal.InvalidInstantiation (definition, argument, malformed) ->
            $"CoreCLR refuses to load an instantiation of %O{definition} with TypeLoadException: its argument %d{argument} is a %O{malformed}, which no instantiation may have"
        | TypeLoadRefusal.KindMismatch (named, spelled) ->
            $"CoreCLR refuses to load %O{named} with TypeLoadException: a signature spells it %O{spelled}, which it is not"
        | TypeLoadRefusal.WrongArity (definition, declared, supplied) ->
            $"CoreCLR refuses to load an instantiation of %O{definition} with TypeLoadException: it declares %d{declared} generic parameter(s), and the instantiation names %d{supplied} argument(s)"
        | TypeLoadRefusal.UninstantiatedDefinition definition ->
            $"CoreCLR refuses to load %O{definition} with TypeLoadException: it is a generic definition, named without an instantiation where a type is required"
        | TypeLoadRefusal.TypeParameterOutOfScope (index, inScope) ->
            $"CoreCLR refuses a signature with BadImageFormatException: it names the type parameter !%d{index}, and its context instantiates %d{inScope}"
        | TypeLoadRefusal.MethodParameterOutOfScope (index, inScope) ->
            $"CoreCLR refuses a signature with BadImageFormatException: it names the method parameter !!%d{index}, and its context instantiates %d{inScope}"
