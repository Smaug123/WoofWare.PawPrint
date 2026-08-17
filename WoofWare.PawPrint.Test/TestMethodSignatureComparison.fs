namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `TypeConcretization.signaturesEquivalent` mirrors `MetaSig::CompareMethodSigs`, and most of its
/// rules cannot be reached by a guest: a C# program cannot declare two methods whose signatures
/// differ only in a modifier's `modreq`/`modopt` flag, or in the order of two modifiers, or in
/// whether a type is spelled `ELEMENT_TYPE_OBJECT` or `class System.Object`. The rules that decide
/// generic methods are unreachable for a different reason — the reflection surface that would ask
/// about them is parked on other primitives.
///
/// So those arms are pinned here, against signatures either decoded from a compiled assembly or
/// built directly.
[<TestFixture>]
module TestMethodSignatureComparison =

    let private loadAssemblyFromSource
        (source : string)
        : Microsoft.Extensions.Logging.ILoggerFactory * BaseClassTypes<DumpedAssembly> * DumpedAssembly * IlMachineState
        =
        let image =
            Roslyn.compileAssembly
                "SignatureComparisonTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory typeof<obj>.Assembly.Location

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            let state = initialState.WithLoadedAssembly corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        loggerFactory, baseClassTypes, assembly, state

    let private source =
        """
public static unsafe class Shapes
{
    public static void TakesInt (int x) { }

    public static void TakesObject (object x) { }

    public static void TakesRefInt (ref int x) { }

    public static void TakesCdecl (delegate* unmanaged[Cdecl, SuppressGCTransition]<void> f) { }

    public static void TakesStdcall (delegate* unmanaged[Stdcall, SuppressGCTransition]<void> f) { }
}

public class OpenGeneric<T>
{
    // Signature `void (!0)`: what it means depends on the instantiation supplied for the
    // declaring type.
    public virtual void TakesTypeParameter (T x) { }

    // Signature `void (!!0)`: compared positionally, never substituted.
    public virtual void TakesMethodParameter<U> (U x) { }

    public virtual void TakesTwoMethodParameters<U, V> (U u, V v) { }

    public virtual void TakesTwoMethodParametersSwapped<U, V> (V v, U u) { }

    public virtual void TakesIntNotTypeParameter (int x) { }

    // The two shapes that meet when one side's parameter has been substituted away and the other's
    // is still open: `void (!0)` against `void (!!0)`, at equal method-generic arity so that
    // nothing rejects the pair before the parameters are compared.
    public virtual void GenericMethodTakingTypeParameter<U> (T x) { }

    public virtual void GenericMethodTakingMethodParameter<U> (U x) { }
}
"""

    let private findMethod
        (declaringTypeName : string)
        (methodName : string)
        (assembly : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        assembly.Methods.Values
        |> Seq.find (fun method ->
            method.RequiredDeclaringType.Name = declaringTypeName
            && method.Name = methodName
        )

    /// A `MethodDefSig` for a static method, which is the shape most of these tests want: only the
    /// element types under comparison should differ between two comparands.
    let private staticSignature (parameters : TypeDefn list) : TypeMethodSignature<TypeDefn> =
        {
            Header =
                ComparableSignatureHeader.Make (
                    SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
                )
            ParameterTypes = parameters
            GenericParameterCount = 0
            RequiredParameterCount = parameters.Length
            ReturnType = MethodReturnType.Void
        }

    type private Fixture =
        {
            LoggerFactory : Microsoft.Extensions.Logging.ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assembly : DumpedAssembly
            State : IlMachineState
        }

    let private fixture () : Fixture =
        let loggerFactory, baseClassTypes, assembly, state = loadAssemblyFromSource source

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Assembly = assembly
            State = state
        }

    /// Both sides read in the test assembly's own token space, with `generics` standing for the
    /// declaring type's instantiation on both.
    let private equivalentUnder
        (fixture : Fixture)
        (generics : ImmutableArray<ConcreteTypeHandle>)
        (left : TypeMethodSignature<TypeDefn>)
        (right : TypeMethodSignature<TypeDefn>)
        : bool
        =
        let comparand (signature : TypeMethodSignature<TypeDefn>) : TypeConcretization.SignatureComparand =
            {
                Signature = signature
                Assembly = fixture.Assembly.Name
                DeclaringTypeGenerics = generics
            }

        let _, equivalent =
            IlMachineState.signaturesEquivalent
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.State
                false
                (comparand left)
                (comparand right)

        equivalent

    let private equivalent
        (fixture : Fixture)
        (left : TypeMethodSignature<TypeDefn>)
        (right : TypeMethodSignature<TypeDefn>)
        : bool
        =
        equivalentUnder fixture ImmutableArray.Empty left right

    let private int32Handle (fixture : Fixture) : ConcreteTypeHandle =
        let state, handle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.State
                fixture.Assembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.PrimitiveType PrimitiveType.Int32)

        ignore<IlMachineState> state
        handle

    let private stringHandle (fixture : Fixture) : ConcreteTypeHandle =
        let state, handle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.State
                fixture.Assembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.PrimitiveType PrimitiveType.String)

        ignore<IlMachineState> state
        handle

    [<Test>]
    let ``a signature is equivalent to itself`` () =
        let fixture = fixture ()
        let takesInt = (findMethod "Shapes" "TakesInt" fixture.Assembly).Signature

        equivalent fixture takesInt takesInt |> shouldEqual true

    [<Test>]
    let ``differing parameter types are not equivalent`` () =
        let fixture = fixture ()
        let takesInt = (findMethod "Shapes" "TakesInt" fixture.Assembly).Signature
        let takesRefInt = (findMethod "Shapes" "TakesRefInt" fixture.Assembly).Signature

        equivalent fixture takesInt takesRefInt |> shouldEqual false

    /// The guest-reachable case, pinned here at the level the comparison works at: both parameters
    /// are function pointers with the same `unmanaged` CallKind byte and the same (empty) argument
    /// list, differing only in the `modopt` that names the convention.
    [<Test>]
    let ``function pointers differing only in a calling-convention modifier are not equivalent`` () =
        let fixture = fixture ()
        let cdecl = (findMethod "Shapes" "TakesCdecl" fixture.Assembly).Signature
        let stdcall = (findMethod "Shapes" "TakesStdcall" fixture.Assembly).Signature

        equivalent fixture cdecl stdcall |> shouldEqual false

    /// The control for the test above: those two signatures agree everywhere except the modifiers,
    /// so the comparison must not be rejecting them for some other reason.
    [<Test>]
    let ``the calling-convention overloads agree once their modifiers are stripped`` () =
        let fixture = fixture ()
        let cdecl = (findMethod "Shapes" "TakesCdecl" fixture.Assembly).Signature
        let stdcall = (findMethod "Shapes" "TakesStdcall" fixture.Assembly).Signature

        let strip (signature : TypeMethodSignature<TypeDefn>) : TypeMethodSignature<TypeDefn> =
            let rec stripAll (ty : TypeDefn) : TypeDefn =
                match ty with
                | TypeDefn.Modified m -> stripAll m.Unmodified
                | TypeDefn.Pointer inner -> TypeDefn.Pointer (stripAll inner)
                | TypeDefn.FunctionPointer inner ->
                    TypeDefn.FunctionPointer
                        { inner with
                            ReturnType =
                                match inner.ReturnType with
                                | MethodReturnType.Void -> MethodReturnType.Void
                                | MethodReturnType.Returns ty -> MethodReturnType.Returns (stripAll ty)
                            ParameterTypes = inner.ParameterTypes |> List.map stripAll
                        }
                | ty -> ty

            { signature with
                ParameterTypes = signature.ParameterTypes |> List.map stripAll
            }

        equivalent fixture (strip cdecl) (strip stdcall) |> shouldEqual true

    /// C# cannot produce a `modopt` and a `modreq` naming the same type in the same position, so the
    /// pair is derived by flipping the flag on a decoded one.
    [<Test>]
    let ``a modreq is not equivalent to a modopt naming the same type`` () =
        let fixture = fixture ()
        let takesRefInt = (findMethod "Shapes" "TakesRefInt" fixture.Assembly).Signature

        let modifier =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    fixture.BaseClassTypes.Object.Assembly
                    fixture.BaseClassTypes.Object.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let modified (isRequired : bool) =
            staticSignature
                [
                    TypeDefn.Modified
                        {
                            Unmodified = takesRefInt.ParameterTypes |> List.exactlyOne
                            Modifier = modifier
                            IsRequired = isRequired
                        }
                ]

        equivalent fixture (modified true) (modified true) |> shouldEqual true
        equivalent fixture (modified true) (modified false) |> shouldEqual false

    [<Test>]
    let ``a modified parameter is not equivalent to the unmodified one`` () =
        let fixture = fixture ()

        let modifier =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    fixture.BaseClassTypes.Object.Assembly
                    fixture.BaseClassTypes.Object.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let plain = staticSignature [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        let modified =
            staticSignature
                [
                    TypeDefn.Modified
                        {
                            Unmodified = TypeDefn.PrimitiveType PrimitiveType.Int32
                            Modifier = modifier
                            IsRequired = true
                        }
                ]

        equivalent fixture plain modified |> shouldEqual false

    /// Modifiers are compared in blob order, so two of them applied in opposite orders are two
    /// different signatures.
    [<Test>]
    let ``modifiers applied in opposite orders are not equivalent`` () =
        let fixture = fixture ()

        let nominal (assembly : System.Reflection.AssemblyName) (handle : TypeDefinitionHandle) =
            TypeDefn.FromDefinition (ResolvedTypeIdentity.ofTypeDefinition assembly handle, SignatureTypeKind.Class)

        let outerFirst =
            nominal fixture.BaseClassTypes.Object.Assembly fixture.BaseClassTypes.Object.TypeDefHandle

        let innerFirst =
            nominal fixture.BaseClassTypes.String.Assembly fixture.BaseClassTypes.String.TypeDefHandle

        let doublyModified (outer : TypeDefn) (inner : TypeDefn) =
            staticSignature
                [
                    TypeDefn.Modified
                        {
                            Unmodified =
                                TypeDefn.Modified
                                    {
                                        Unmodified = TypeDefn.PrimitiveType PrimitiveType.Int32
                                        Modifier = inner
                                        IsRequired = false
                                    }
                            Modifier = outer
                            IsRequired = false
                        }
                ]

        equivalent fixture (doublyModified outerFirst innerFirst) (doublyModified outerFirst innerFirst)
        |> shouldEqual true

        equivalent fixture (doublyModified outerFirst innerFirst) (doublyModified innerFirst outerFirst)
        |> shouldEqual false

    /// `M(object)` and `M(class System.Object)` are different signatures: `CompareElementType` fails
    /// on the element-type bytes before it ever resolves a token. Roslyn only emits the first, so the
    /// second is built here.
    [<Test>]
    let ``an object parameter is not equivalent to one spelled as a class reference`` () =
        let fixture = fixture ()
        let takesObject = (findMethod "Shapes" "TakesObject" fixture.Assembly).Signature

        takesObject.ParameterTypes
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Object ]

        let spelledAsClass =
            staticSignature
                [
                    TypeDefn.FromDefinition (
                        ResolvedTypeIdentity.ofTypeDefinition
                            fixture.BaseClassTypes.Object.Assembly
                            fixture.BaseClassTypes.Object.TypeDefHandle,
                        SignatureTypeKind.Class
                    )
                ]

        equivalent fixture takesObject spelledAsClass |> shouldEqual false

    [<Test>]
    let ``an instance signature is not equivalent to a static one with the same parameters`` () =
        let fixture = fixture ()
        let asStatic = staticSignature [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        let asInstance =
            { asStatic with
                Header =
                    ComparableSignatureHeader.Make (
                        SignatureHeader (
                            SignatureKind.Method,
                            SignatureCallingConvention.Default,
                            SignatureAttributes.Instance
                        )
                    )
            }

        equivalent fixture asStatic asInstance |> shouldEqual false

    /// A generic *method* parameter is compared positionally and never substituted, which is what
    /// lets two generic methods be compared without an instantiation for either.
    [<Test>]
    let ``method generic parameters are compared positionally`` () =
        let fixture = fixture ()

        let oneParameter =
            (findMethod "OpenGeneric`1" "TakesMethodParameter" fixture.Assembly).Signature

        let twoParameters =
            (findMethod "OpenGeneric`1" "TakesTwoMethodParameters" fixture.Assembly).Signature

        let swapped =
            (findMethod "OpenGeneric`1" "TakesTwoMethodParametersSwapped" fixture.Assembly).Signature

        oneParameter.ParameterTypes |> shouldEqual [ TypeDefn.GenericMethodParameter 0 ]

        equivalent fixture oneParameter oneParameter |> shouldEqual true
        equivalent fixture twoParameters twoParameters |> shouldEqual true
        // `void M<U, V>(U, V)` and `void M<U, V>(V, U)` have the same arity and the same set of
        // parameter types, and differ only in which index appears where.
        equivalent fixture twoParameters swapped |> shouldEqual false

    [<Test>]
    let ``a method generic parameter is not equivalent to a concrete type`` () =
        let fixture = fixture ()

        let takesMethodParameter =
            (findMethod "OpenGeneric`1" "TakesMethodParameter" fixture.Assembly).Signature

        let takesInt =
            { staticSignature [ TypeDefn.PrimitiveType PrimitiveType.Int32 ] with
                Header = takesMethodParameter.Header
                GenericParameterCount = takesMethodParameter.GenericParameterCount
            }

        equivalent fixture takesMethodParameter takesInt |> shouldEqual false

    /// A generic *type* parameter is resolved through the declaring type's instantiation before
    /// anything else, so whether `void (!0)` matches `void (int32)` is a question about the
    /// instantiation rather than about the two blobs.
    [<Test>]
    let ``a type generic parameter is compared through the declaring type's instantiation`` () =
        let fixture = fixture ()

        let takesTypeParameter =
            (findMethod "OpenGeneric`1" "TakesTypeParameter" fixture.Assembly).Signature

        let takesInt =
            (findMethod "OpenGeneric`1" "TakesIntNotTypeParameter" fixture.Assembly).Signature

        takesTypeParameter.ParameterTypes
        |> shouldEqual [ TypeDefn.GenericTypeParameter 0 ]

        let atInt = ImmutableArray.Create (int32Handle fixture)
        let atString = ImmutableArray.Create (stringHandle fixture)

        equivalentUnder fixture atInt takesTypeParameter takesInt |> shouldEqual true

        equivalentUnder fixture atString takesTypeParameter takesInt
        |> shouldEqual false
        // And the substitution is not what makes them match: at no instantiation is `!0` equal to a
        // parameter the comparison has already rejected.
        equivalentUnder fixture atInt takesTypeParameter takesTypeParameter
        |> shouldEqual true

    /// A substituted parameter is a closed runtime type, and a method generic parameter is not one, so
    /// the two can never be equal. Answering that has to come before resolving the open side, which
    /// has no instantiation to be resolved against — `void C&lt;int&gt;.M&lt;U&gt;(int)` and
    /// `void C&lt;int&gt;.M&lt;U&gt;(U)` are both legal C# and reach this pairing during dispatch.
    [<Test>]
    let ``a substituted type parameter is not equivalent to an open method parameter`` () =
        let fixture = fixture ()

        let takesTypeParameter =
            (findMethod "OpenGeneric`1" "GenericMethodTakingTypeParameter" fixture.Assembly).Signature

        let takesMethodParameter =
            (findMethod "OpenGeneric`1" "GenericMethodTakingMethodParameter" fixture.Assembly).Signature

        takesTypeParameter.ParameterTypes
        |> shouldEqual [ TypeDefn.GenericTypeParameter 0 ]

        takesMethodParameter.ParameterTypes
        |> shouldEqual [ TypeDefn.GenericMethodParameter 0 ]

        let atInt = ImmutableArray.Create (int32Handle fixture)

        equivalentUnder fixture atInt takesTypeParameter takesMethodParameter
        |> shouldEqual false

        // Symmetrically, so that the answer does not depend on which side was substituted first.
        equivalentUnder fixture atInt takesMethodParameter takesTypeParameter
        |> shouldEqual false

    /// Where the two differ in parameter count, only a vararg caller can still match: the parameters
    /// past its sentinel are the `...` part, and the callee has to end exactly where the sentinel is.
    [<Test>]
    let ``a vararg caller matches a callee that ends at its sentinel`` () =
        let fixture = fixture ()

        let varargHeader =
            ComparableSignatureHeader.Make (
                SignatureHeader (SignatureKind.Method, SignatureCallingConvention.VarArgs, SignatureAttributes.None)
            )

        let caller (fixedParams : TypeDefn list) (variadic : TypeDefn list) =
            { staticSignature (fixedParams @ variadic) with
                Header = varargHeader
                RequiredParameterCount = fixedParams.Length
            }

        let callee (parameters : TypeDefn list) =
            { staticSignature parameters with
                Header = varargHeader
            }

        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32
        let string = TypeDefn.PrimitiveType PrimitiveType.String

        equivalent fixture (caller [ int32 ] [ string ]) (callee [ int32 ])
        |> shouldEqual true
        // The callee must end at the sentinel, not merely start the same way.
        equivalent fixture (caller [ int32 ] [ string ]) (callee [ int32 ; string ])
        |> shouldEqual false
        // The fixed parameters still have to match.
        equivalent fixture (caller [ string ] [ string ]) (callee [ int32 ])
        |> shouldEqual false

    /// Substituting a generic type parameter must not lose the *other* side's modifiers. The
    /// substituted side is a closed handle, and concretising the spelled side to compare handles
    /// would strip its modifier — making a derived `M(int32 modreq(X))` look like an override of
    /// `Base&lt;int32&gt;.M(!0)`, which CoreCLR gives a fresh vtable slot.
    [<Test>]
    let ``a substituted type parameter is not equivalent to a modified spelling of the same type`` () =
        let fixture = fixture ()

        let takesTypeParameter =
            (findMethod "OpenGeneric`1" "TakesTypeParameter" fixture.Assembly).Signature

        let takesInt =
            (findMethod "OpenGeneric`1" "TakesIntNotTypeParameter" fixture.Assembly).Signature

        let modifier =
            TypeDefn.FromDefinition (
                ResolvedTypeIdentity.ofTypeDefinition
                    fixture.BaseClassTypes.Object.Assembly
                    fixture.BaseClassTypes.Object.TypeDefHandle,
                SignatureTypeKind.Class
            )

        let takesModifiedInt =
            { takesInt with
                ParameterTypes =
                    [
                        TypeDefn.Modified
                            {
                                Unmodified = TypeDefn.PrimitiveType PrimitiveType.Int32
                                Modifier = modifier
                                IsRequired = true
                            }
                    ]
            }

        let atInt = ImmutableArray.Create (int32Handle fixture)

        // The control: with the modifier gone, the substitution does make the two agree, so the
        // rejection below is about the modifier and not about the substitution failing.
        equivalentUnder fixture atInt takesTypeParameter takesInt |> shouldEqual true

        equivalentUnder fixture atInt takesTypeParameter takesModifiedInt
        |> shouldEqual false

        equivalentUnder fixture atInt takesModifiedInt takesTypeParameter
        |> shouldEqual false

    /// Generic arity is not recoverable from anything else the comparison reads: `void M&lt;T&gt;()` and
    /// `void M&lt;U, V&gt;()` have the same header byte (both carry the GENERIC bit) and the same, empty,
    /// parameter list. Both are legal in one C# type, so a MemberRef naming one of them would
    /// otherwise match both — and nothing screens arity ahead of this comparison any more.
    [<Test>]
    let ``signatures differing only in generic arity are not equivalent`` () =
        let fixture = fixture ()

        let ofArity (arity : int) : TypeMethodSignature<TypeDefn> =
            { staticSignature [] with
                Header =
                    ComparableSignatureHeader.Make (
                        SignatureHeader (
                            SignatureKind.Method,
                            SignatureCallingConvention.Default,
                            SignatureAttributes.Generic
                        )
                    )
                GenericParameterCount = arity
            }

        (ofArity 1).Header |> shouldEqual (ofArity 2).Header

        equivalent fixture (ofArity 1) (ofArity 1) |> shouldEqual true
        equivalent fixture (ofArity 1) (ofArity 2) |> shouldEqual false

    /// A sentinel is illegal in the *callee's* signature. CoreCLR asserts rather than checks, and
    /// says its comparison "would simply fail" anyway, because it would meet the sentinel where a
    /// real element was expected. The decoded form has no element to meet, so failing has to be
    /// explicit — otherwise the callee's post-sentinel parameters would go uncompared.
    [<Test>]
    let ``a callee carrying a sentinel matches nothing`` () =
        let fixture = fixture ()
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32
        let string = TypeDefn.PrimitiveType PrimitiveType.String

        let varargHeader =
            ComparableSignatureHeader.Make (
                SignatureHeader (SignatureKind.Method, SignatureCallingConvention.VarArgs, SignatureAttributes.None)
            )

        let withSentinel (fixedParams : TypeDefn list) (variadic : TypeDefn list) =
            { staticSignature (fixedParams @ variadic) with
                Header = varargHeader
                RequiredParameterCount = fixedParams.Length
            }

        let double = TypeDefn.PrimitiveType PrimitiveType.Double

        // The pairing that turns on the check rather than on the counts: the caller's sentinel sits
        // at 2, the callee has 2 parameters, and its own sentinel sits at 1. Reading only the
        // sentinel position would compare the callee's whole parameter list against the caller's
        // first two and answer "equivalent", silently treating the callee's variadic parameter as a
        // fixed one.
        let callee = withSentinel [ int32 ] [ string ]
        let caller = withSentinel [ int32 ; string ] [ double ]

        caller.RequiredParameterCount |> shouldEqual callee.ParameterTypes.Length

        caller.ParameterTypes |> List.truncate 2 |> shouldEqual callee.ParameterTypes

        equivalent fixture caller callee |> shouldEqual false

        // And the same signature is a perfectly good *caller*, so the rejection is about which side
        // carries the sentinel rather than about the signature itself.
        equivalent fixture callee (withSentinel [ int32 ] []) |> shouldEqual true

    /// A function pointer is a *type*, so its signature is compared at exact arity: the vararg
    /// sentinel rule matches a call site against a callee, which is not a question one can ask of a
    /// type. Applying it inside a function pointer would make `void(int32, ..., string)` and
    /// `void(int32)` name the same type.
    [<Test>]
    let ``function pointer signatures are compared at exact arity`` () =
        let fixture = fixture ()
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32
        let string = TypeDefn.PrimitiveType PrimitiveType.String

        let functionPointer (fixedParams : TypeDefn list) (variadic : TypeDefn list) =
            TypeDefn.FunctionPointer
                { staticSignature (fixedParams @ variadic) with
                    Header =
                        ComparableSignatureHeader.Make (
                            SignatureHeader (
                                SignatureKind.Method,
                                SignatureCallingConvention.VarArgs,
                                SignatureAttributes.None
                            )
                        )
                    RequiredParameterCount = fixedParams.Length
                }

        let takes (ty : TypeDefn) = staticSignature [ ty ]

        // Same fixed prefix, different variadic tails: as call signatures these would match, but as
        // types they are distinct.
        equivalent fixture (takes (functionPointer [ int32 ] [ string ])) (takes (functionPointer [ int32 ] []))
        |> shouldEqual false

        equivalent fixture (takes (functionPointer [ int32 ] [ string ])) (takes (functionPointer [ int32 ] [ string ]))
        |> shouldEqual true

        // And the sentinel's position is part of the type: the same parameter list with the
        // sentinel elsewhere is a different function pointer.
        equivalent
            fixture
            (takes (functionPointer [ int32 ] [ string ]))
            (takes (functionPointer [ int32 ; string ] []))
        |> shouldEqual false

    [<Test>]
    let ``differing parameter counts are not equivalent without a vararg sentinel`` () =
        let fixture = fixture ()
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32

        equivalent fixture (staticSignature [ int32 ]) (staticSignature [ int32 ; int32 ])
        |> shouldEqual false

    // ----- generic method constraints -------------------------------------------------------

    let private nominalOf (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : TypeDefn =
        TypeDefn.FromDefinition (
            ResolvedTypeIdentity.ofTypeDefinition ty.Assembly ty.TypeDefHandle,
            SignatureTypeKind.Class
        )

    let private parameter
        (kind : GenericConstraint option)
        (requiresParameterlessConstructor : bool)
        (allowsByRefLike : bool)
        (constraints : TypeDefn list)
        : GenericParamMetadata
        =
        {
            Variance = None
            Constraint = kind
            RequiresParameterlessConstructor = requiresParameterlessConstructor
            AllowsByRefLike = allowsByRefLike
            Constraints = ImmutableArray.CreateRange constraints
        }

    let private unconstrained : GenericParamMetadata = parameter None false false []

    /// `impl` is the overriding side.
    let private constraintsMatch
        (fixture : Fixture)
        (impl : GenericParamMetadata list)
        (decl : GenericParamMetadata list)
        : bool
        =
        let comparand (parameters : GenericParamMetadata list) : TypeConcretization.ConstraintComparand =
            {
                Parameters = parameters
                Assembly = fixture.Assembly.Name
                DeclaringTypeGenerics = ImmutableArray.Empty
            }

        let _, matches =
            IlMachineState.methodConstraintsMatch
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.State
                (comparand impl)
                (comparand decl)

        matches

    /// The case ordinary C# always produces, since Roslyn copies a base method's constraints onto the
    /// override verbatim. If this went wrong every constrained generic override would take a slot of
    /// its own.
    [<Test>]
    let ``identical constraints match`` () =
        let fixture = fixture ()
        let comparable = nominalOf fixture.BaseClassTypes.String

        let cases =
            [
                unconstrained
                parameter (Some GenericConstraint.Reference) false false []
                parameter (Some GenericConstraint.NonNullableValue) false false []
                parameter None true false []
                parameter None false true []
                parameter (Some GenericConstraint.Reference) false false [ comparable ]
            ]

        for case in cases do
            constraintsMatch fixture [ case ] [ case ] |> shouldEqual true

    /// An override may drop a requirement its base had, but not add one: a type argument that
    /// satisfied the base must still satisfy the override.
    [<Test>]
    let ``an override may not add a special constraint`` () =
        let fixture = fixture ()

        let valueType = parameter (Some GenericConstraint.NonNullableValue) false false []
        let referenceType = parameter (Some GenericConstraint.Reference) false false []

        constraintsMatch fixture [ valueType ] [ unconstrained ] |> shouldEqual false
        constraintsMatch fixture [ unconstrained ] [ valueType ] |> shouldEqual true

        constraintsMatch fixture [ referenceType ] [ unconstrained ]
        |> shouldEqual false

        constraintsMatch fixture [ unconstrained ] [ referenceType ] |> shouldEqual true

        // The two are compared for equality rather than merely for presence.
        constraintsMatch fixture [ valueType ] [ referenceType ] |> shouldEqual false

    /// `new()` is the one special constraint with a second way to be satisfied: a non-nullable value
    /// type always has a parameterless constructor.
    [<Test>]
    let ``a value-type constraint satisfies a parameterless-constructor constraint`` () =
        let fixture = fixture ()
        let requiresCtor = parameter None true false []
        let valueType = parameter (Some GenericConstraint.NonNullableValue) false false []

        constraintsMatch fixture [ requiresCtor ] [ unconstrained ] |> shouldEqual false
        constraintsMatch fixture [ requiresCtor ] [ valueType ] |> shouldEqual true
        constraintsMatch fixture [ requiresCtor ] [ requiresCtor ] |> shouldEqual true

    /// `allows ref struct` is compared in the opposite direction from the rest, because it *widens*
    /// what the parameter accepts rather than narrowing it.
    [<Test>]
    let ``an override may not withdraw allows-ref-struct`` () =
        let fixture = fixture ()
        let allows = parameter None false true []

        constraintsMatch fixture [ unconstrained ] [ allows ] |> shouldEqual false
        constraintsMatch fixture [ allows ] [ unconstrained ] |> shouldEqual true

    [<Test>]
    let ``an override may not add a type constraint`` () =
        let fixture = fixture ()

        let constrained =
            parameter None false false [ nominalOf fixture.BaseClassTypes.String ]

        constraintsMatch fixture [ constrained ] [ unconstrained ] |> shouldEqual false
        constraintsMatch fixture [ unconstrained ] [ constrained ] |> shouldEqual true

        // A different type does not satisfy it either; only a matching one does.
        let other = parameter None false false [ nominalOf fixture.BaseClassTypes.Array ]

        constraintsMatch fixture [ constrained ] [ other ] |> shouldEqual false
        constraintsMatch fixture [ constrained ] [ constrained ] |> shouldEqual true

    /// A constraint naming `System.Object` says nothing, and neither does `System.ValueType` on a
    /// parameter already constrained to a value type. CoreCLR skips both rather than looking for a
    /// match, because the overridden parameter is entitled to leave them implicit.
    [<Test>]
    let ``vacuous type constraints are not required to match`` () =
        let fixture = fixture ()

        let objectConstrained =
            parameter None false false [ nominalOf fixture.BaseClassTypes.Object ]

        constraintsMatch fixture [ objectConstrained ] [ unconstrained ]
        |> shouldEqual true

        let valueTypeConstrained =
            parameter
                (Some GenericConstraint.NonNullableValue)
                false
                false
                [ nominalOf fixture.BaseClassTypes.ValueType ]

        constraintsMatch
            fixture
            [ valueTypeConstrained ]
            [ parameter (Some GenericConstraint.NonNullableValue) false false [] ]
        |> shouldEqual true

        // Only vacuous *because* the parameter is value-type constrained: without that, a
        // `System.ValueType` constraint is an ordinary one and has to be matched.
        let plainValueTypeConstrained =
            parameter None false false [ nominalOf fixture.BaseClassTypes.ValueType ]

        constraintsMatch fixture [ plainValueTypeConstrained ] [ unconstrained ]
        |> shouldEqual false

    /// The slot matcher only asks this once the signatures already agree, which settles the arity —
    /// but this is a comparison in its own right, and a caller that has not established that must get
    /// an answer rather than an exception from zipping two different-length lists.
    [<Test>]
    let ``differing numbers of type parameters do not match`` () =
        let fixture = fixture ()

        constraintsMatch fixture [ unconstrained ] [ unconstrained ; unconstrained ]
        |> shouldEqual false

        constraintsMatch fixture [ unconstrained ; unconstrained ] [ unconstrained ]
        |> shouldEqual false

    /// `System.Object` has a primitive spelling as well as a nominal one, and a constraint may use
    /// either — the GenericParamConstraint column is a TypeDefOrRefOrSpec, and a TypeSpec may hold a
    /// bare `ELEMENT_TYPE_OBJECT`. Both are equally vacuous.
    [<Test>]
    let ``an object constraint is vacuous in either spelling`` () =
        let fixture = fixture ()

        let spelledPrimitively =
            parameter None false false [ TypeDefn.PrimitiveType PrimitiveType.Object ]

        let spelledNominally =
            parameter None false false [ nominalOf fixture.BaseClassTypes.Object ]

        constraintsMatch fixture [ spelledPrimitively ] [ unconstrained ]
        |> shouldEqual true

        constraintsMatch fixture [ spelledNominally ] [ unconstrained ]
        |> shouldEqual true

        // The control: a primitive spelling of some *other* type is an ordinary constraint, so the
        // arm above is about `object` rather than about primitives.
        let spelledOtherPrimitive =
            parameter None false false [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        constraintsMatch fixture [ spelledOtherPrimitive ] [ unconstrained ]
        |> shouldEqual false

    [<Test>]
    let ``every parameter is compared, not just the first`` () =
        let fixture = fixture ()
        let valueType = parameter (Some GenericConstraint.NonNullableValue) false false []

        constraintsMatch fixture [ unconstrained ; valueType ] [ unconstrained ; unconstrained ]
        |> shouldEqual false

        constraintsMatch fixture [ unconstrained ; unconstrained ] [ unconstrained ; valueType ]
        |> shouldEqual true

    /// `skipReturnType` is how CoreCLR expresses "a covariant return is acceptable", so it must omit
    /// the return column and nothing else.
    [<Test>]
    let ``skipping the return type ignores differing returns but not differing parameters`` () =
        let fixture = fixture ()
        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32
        let string = TypeDefn.PrimitiveType PrimitiveType.String

        let withReturn (ret : TypeDefn) (parameter : TypeDefn) =
            { staticSignature [ parameter ] with
                ReturnType = MethodReturnType.Returns ret
            }

        let compare (skipReturnType : bool) (left, right) =
            let comparand (signature : TypeMethodSignature<TypeDefn>) : TypeConcretization.SignatureComparand =
                {
                    Signature = signature
                    Assembly = fixture.Assembly.Name
                    DeclaringTypeGenerics = ImmutableArray.Empty
                }

            let _, equivalent =
                IlMachineState.signaturesEquivalent
                    fixture.LoggerFactory
                    fixture.BaseClassTypes
                    fixture.State
                    skipReturnType
                    (comparand left)
                    (comparand right)

            equivalent

        let differingReturns = withReturn int32 int32, withReturn string int32
        let differingParameters = withReturn int32 int32, withReturn int32 string

        compare false differingReturns |> shouldEqual false
        compare true differingReturns |> shouldEqual true
        compare true differingParameters |> shouldEqual false
