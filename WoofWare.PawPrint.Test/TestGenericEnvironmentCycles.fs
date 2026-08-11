namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Regression coverage for issue #903: unbounded recursion in <c>TypeResolution</c> when the
/// generic environment it is handed is cyclic.
/// </summary>
/// <remarks>
/// <para>
/// Substitution in <c>TypeResolution</c> re-enters its own output: expanding
/// <c>GenericTypeParameter i</c> recurses on <c>typeGenericArgs.[i]</c> under the *same*
/// environment, in <c>substituteGenericsInTypeDefn</c> and again in
/// <c>resolveTypeFromDefnUnprimed</c>. Substitution is therefore only well-founded when the
/// "parameter i's argument mentions parameter j" relation is acyclic. It is acyclic for every
/// live interpreter path, because those environments come from <c>concreteHandleToTypeDefn</c>,
/// which emits closed types only — but the resolvers are public and nothing enforced it, so a
/// cyclic environment used to recurse until the stack overflowed, taking the process with it.
/// </para>
/// <para>
/// A stack overflow is uncatchable in .NET, so it can never be diagnosed, attributed, or
/// contained; these tests pin the failure to a loud, specific error instead. The positive cases
/// are as load-bearing as the negative ones: the guard must reject exactly the cyclic
/// environments and nothing else, in particular not an argument that legitimately names a
/// parameter of the *other* environment.
/// </para>
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGenericEnvironmentCycles =

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    /// The text the resolver uses to report a cyclic environment. Every failure these tests
    /// tolerate has to be this one: any other exception means the resolver fell over for a
    /// reason the test did not intend to exercise.
    let private cyclicMarker = "generic environment is cyclic"

    type private Fixture =
        {
            LoggerFactory : Microsoft.Extensions.Logging.ILoggerFactory
            Corelib : DumpedAssembly
            BaseTypes : BaseClassTypes<DumpedAssembly>
            Loaded : LoadedAssemblies
        }

    let private setUp () : Fixture =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = Assembly.readFile loggerFactory corelibPath

        {
            LoggerFactory = loggerFactory
            Corelib = corelib
            BaseTypes = Corelib.getBaseTypes corelib
            Loaded = LoadedAssemblies.ofAssemblies [ corelib ]
        }

    let private topLevel (fixture : Fixture) (ns : string) (name : string) : TypeDefn =
        fixture.Corelib.TryGetTopLevelTypeDef ns name
        |> Option.defaultWith (fun () -> failwith $"Expected %s{ns}.%s{name} in corelib")
        |> fun ty -> TypeDefn.FromDefinition (ty.Identity, SignatureTypeKind.Class)

    let private resolve
        (fixture : Fixture)
        (ty : TypeDefn)
        (typeGenericArgs : TypeDefn list)
        (methodGenericArgs : TypeDefn list)
        : TypeInfo<TypeDefn, TypeDefn>
        =
        let _, _, resolved =
            TypeResolution.resolveTypeFromDefn
                fixture.LoggerFactory
                [ runtimeDir ]
                fixture.BaseTypes
                ty
                (ImmutableArray.CreateRange typeGenericArgs)
                (ImmutableArray.CreateRange methodGenericArgs)
                fixture.Corelib
                fixture.Loaded

        resolved

    let private genericInstantiation (head : TypeDefn) (args : TypeDefn list) : TypeDefn =
        TypeDefn.GenericInstantiation (head, ImmutableArray.CreateRange args)

    // ------------------------------------------------------------------
    // Cyclic environments: rejected, loudly.
    // ------------------------------------------------------------------

    /// The shape from issue #903: `T := List<T>`, reached through the GenericInstantiation
    /// argument fold in substituteGenericsInTypeDefn. Before the fix this overflowed the stack
    /// after ~1084 rounds of the fold.
    [<Test>]
    let ``type parameter whose argument mentions itself is rejected`` () : unit =
        let fixture = setUp ()

        let listOfParam =
            genericInstantiation
                (topLevel fixture "System.Collections.Generic" "List`1")
                [ TypeDefn.GenericTypeParameter 0 ]

        let exn =
            Assert.Throws (fun () ->
                resolve fixture listOfParam [ listOfParam ] []
                |> ignore<TypeInfo<TypeDefn, TypeDefn>>
            )

        exn.Message |> shouldContainText cyclicMarker

    /// The same shape in the method environment: `!!0 := List<!!0>`. This reaches the
    /// GenericMethodParameter arm of substituteGenericsInTypeDefn, which the type-parameter cases
    /// leave untouched.
    [<Test>]
    let ``method parameter whose argument mentions itself is rejected`` () : unit =
        let fixture = setUp ()

        let listOfParam =
            genericInstantiation
                (topLevel fixture "System.Collections.Generic" "List`1")
                [ TypeDefn.GenericMethodParameter 0 ]

        let exn =
            Assert.Throws (fun () ->
                resolve fixture listOfParam [] [ listOfParam ]
                |> ignore<TypeInfo<TypeDefn, TypeDefn>>
            )

        exn.Message |> shouldContainText cyclicMarker

    /// The identity environment, which reaches the parameter case of resolveTypeFromDefnUnprimed
    /// rather than the one in substituteGenericsInTypeDefn: `!0 := !0` re-resolves itself
    /// directly, with no instantiation anywhere.
    [<Test>]
    let ``identity type environment is rejected`` () : unit =
        let fixture = setUp ()

        let exn =
            Assert.Throws (fun () ->
                resolve fixture (TypeDefn.GenericTypeParameter 0) [ TypeDefn.GenericTypeParameter 0 ] []
                |> ignore<TypeInfo<TypeDefn, TypeDefn>>
            )

        exn.Message |> shouldContainText cyclicMarker

    /// The same for the method environment.
    [<Test>]
    let ``identity method environment is rejected`` () : unit =
        let fixture = setUp ()

        let exn =
            Assert.Throws (fun () ->
                resolve fixture (TypeDefn.GenericMethodParameter 0) [] [ TypeDefn.GenericMethodParameter 0 ]
                |> ignore<TypeInfo<TypeDefn, TypeDefn>>
            )

        exn.Message |> shouldContainText cyclicMarker

    /// A cycle that closes across the two environments rather than within one: `!0 := !!0` and
    /// `!!0 := !0`. Neither environment is cyclic on its own, so a guard that tracked only one
    /// kind of parameter would still diverge here.
    [<Test>]
    let ``cycle spanning the type and method environments is rejected`` () : unit =
        let fixture = setUp ()

        let exn =
            Assert.Throws (fun () ->
                resolve
                    fixture
                    (TypeDefn.GenericTypeParameter 0)
                    [ TypeDefn.GenericMethodParameter 0 ]
                    [ TypeDefn.GenericTypeParameter 0 ]
                |> ignore<TypeInfo<TypeDefn, TypeDefn>>
            )

        exn.Message |> shouldContainText cyclicMarker

    // ------------------------------------------------------------------
    // Acyclic environments: unaffected. These pin down what the guard must NOT reject.
    // ------------------------------------------------------------------

    /// A type argument that names a *method* parameter is a legitimate one-step indirection: the
    /// chain `!0 -> !!0 -> int` is acyclic and terminates. A guard that refused to re-substitute
    /// at all — or that keyed only on "we already expanded some parameter" — would break this.
    [<Test>]
    let ``type argument naming a method parameter still resolves`` () : unit =
        let fixture = setUp ()
        let list = topLevel fixture "System.Collections.Generic" "List`1"

        let indirect =
            resolve
                fixture
                (genericInstantiation list [ TypeDefn.GenericTypeParameter 0 ])
                [ TypeDefn.GenericMethodParameter 0 ]
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        let direct =
            resolve fixture (genericInstantiation list [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]) [] []

        indirect.Name |> shouldEqual "List`1"
        indirect.Generics |> shouldEqual direct.Generics

    /// The same parameter used twice in sibling positions is not a cycle: the trail must be
    /// per-path, so that finishing one expansion does not poison the next.
    [<Test>]
    let ``a parameter repeated in sibling positions still resolves`` () : unit =
        let fixture = setUp ()
        let dictionary = topLevel fixture "System.Collections.Generic" "Dictionary`2"
        let list = topLevel fixture "System.Collections.Generic" "List`1"

        let resolved =
            resolve
                fixture
                (genericInstantiation
                    dictionary
                    [
                        TypeDefn.GenericTypeParameter 0
                        genericInstantiation list [ TypeDefn.GenericTypeParameter 0 ]
                    ])
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                []

        resolved.Name |> shouldEqual "Dictionary`2"
        resolved.Generics.Length |> shouldEqual 2

    /// Nesting the *same parameter index* under a fresh environment is not a cycle either: the
    /// inner `List<!0>` is resolved under the environment `[!0 := int]` introduced by the outer
    /// instantiation, which has nothing to do with the outer `!0`.
    [<Test>]
    let ``a parameter index reused under a fresh environment still resolves`` () : unit =
        let fixture = setUp ()
        let list = topLevel fixture "System.Collections.Generic" "List`1"

        let resolved =
            resolve
                fixture
                (genericInstantiation list [ genericInstantiation list [ TypeDefn.GenericTypeParameter 0 ] ])
                [ TypeDefn.PrimitiveType PrimitiveType.String ]
                []

        resolved.Name |> shouldEqual "List`1"
        resolved.Generics.Length |> shouldEqual 1

    // ------------------------------------------------------------------
    // The property: over arbitrary environments, cyclic or not, resolution terminates.
    // ------------------------------------------------------------------

    /// Heads to build instantiations from, paired with their arity.
    let private heads (fixture : Fixture) : (TypeDefn * int) list =
        [
            topLevel fixture "System.Collections.Generic" "List`1", 1
            topLevel fixture "System.Collections.Generic" "Dictionary`2", 2
        ]

    let private leaves (fixture : Fixture) : TypeDefn list =
        [
            TypeDefn.PrimitiveType PrimitiveType.Int32
            TypeDefn.PrimitiveType PrimitiveType.String
            topLevel fixture "System" "Object"
        ]

    /// Terms over `paramCount` type parameters and `methodParamCount` method parameters, of
    /// bounded depth. Parameters are drawn in range by construction, so an index-out-of-range is
    /// a genuine failure rather than a badly-shaped input.
    let private termGen (fixture : Fixture) (paramCount : int) (methodParamCount : int) (depth : int) : Gen<TypeDefn> =
        let rec go (depth : int) : Gen<TypeDefn> =
            let atoms =
                [
                    yield! leaves fixture |> List.map Gen.constant
                    for i in 0 .. paramCount - 1 do
                        yield Gen.constant (TypeDefn.GenericTypeParameter i)
                    for i in 0 .. methodParamCount - 1 do
                        yield Gen.constant (TypeDefn.GenericMethodParameter i)
                ]

            if depth <= 0 then
                Gen.oneof atoms
            else
                let constructed =
                    [
                        for head, arity in heads fixture do
                            yield
                                gen {
                                    let! args = Gen.listOfLength arity (go (depth - 1))
                                    return genericInstantiation head args
                                }

                        yield
                            gen {
                                let! element = go (depth - 1)
                                return TypeDefn.OneDimensionalArrayLowerBoundZero element
                            }
                    ]

                Gen.oneof (atoms @ constructed)

        go depth

    /// <summary>
    /// Resolution terminates on every environment, well-founded or not.
    /// </summary>
    /// <remarks>
    /// The generator deliberately makes no attempt to avoid cycles: environment entries are drawn
    /// from the same term generator as the subject, over the same parameters, so
    /// <c>!0 := List&lt;!0&gt;</c> and mutual chains through the method environment are ordinary
    /// draws. That is the point — the property is that the resolver either answers or refuses,
    /// and never runs away. Termination is what the test run itself demonstrates: before the fix
    /// this crashed the test host outright rather than failing.
    /// </remarks>
    [<Test>]
    let ``resolution terminates on arbitrary generic environments`` () : unit =
        let fixture = setUp ()

        let inputGen =
            gen {
                let! paramCount = Gen.choose (0, 2)
                let! methodParamCount = Gen.choose (0, 2)
                let term = termGen fixture paramCount methodParamCount 2
                let! subject = term
                let! typeArgs = Gen.listOfLength paramCount term
                let! methodArgs = Gen.listOfLength methodParamCount term
                return subject, typeArgs, methodArgs
            }

        // The property is only worth anything if the generator actually reaches the guard, so
        // count the two outcomes and insist on having seen both. Without this a narrowed
        // generator — or a guard that stopped firing — would leave the test passing vacuously.
        let mutable resolved = 0
        let mutable rejected = 0

        let property (subject : TypeDefn, typeArgs : TypeDefn list, methodArgs : TypeDefn list) : bool =
            try
                resolve fixture subject typeArgs methodArgs
                |> ignore<TypeInfo<TypeDefn, TypeDefn>>

                resolved <- resolved + 1
                true
            with e ->
                if e.Message.Contains cyclicMarker then
                    rejected <- rejected + 1
                    true
                else
                    raise (exn ($"Unexpected failure for %O{subject}, type args %A{typeArgs}", e))

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen inputGen) property)

        TestContext.Out.WriteLine $"resolved %d{resolved}, rejected as cyclic %d{rejected}"

        if resolved = 0 then
            Assert.Fail "Generator produced no resolvable environment; the property is vacuous."

        if rejected = 0 then
            Assert.Fail "Generator produced no cyclic environment; the property never reached the guard."
