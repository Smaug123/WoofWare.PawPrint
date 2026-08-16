namespace WoofWare.PawPrint.Test

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Diagnostics
open System.IO
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// A <c>TypeDefn</c> is a DAG, not a tree: nothing stops the same subterm appearing in several
/// argument positions, and the interpreter routinely builds types that way. Resolving one must
/// cost its size <i>as a DAG</i>, not as a tree, which is exponentially larger.
/// </summary>
/// <remarks>
/// <para>
/// The witness is the closed, perfectly-shared nest
/// <c>nest(d) := Dictionary&lt;nest(d-1), nest(d-1)&gt;</c>: <c>d + 1</c> nodes as a DAG,
/// 2^<c>d</c> as a tree. Two independent defects each make resolution exponential:
/// <c>substituteGenericsInTypeDefn</c> walking the DAG as a tree, and a redundant second
/// substitution pass over the instantiation it has just rebuilt — freshly allocated nodes, which
/// a reference-keyed memo can never hit. Memoising the walk guards only the first; the regression
/// test below is sensitive to both, and restoring either defect alone fails it.
/// </para>
/// <para>
/// The properties pin the facts that make this sound. One: the memo — keyed on reference
/// identity, so it fires exactly when the input is shared — cannot change the answer. The other:
/// substitution is idempotent, which is what licenses not substituting a second time.
/// </para>
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSharedTypeGraphResolution =

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

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

    let private genericInstantiation (head : TypeDefn) (args : TypeDefn list) : TypeDefn =
        TypeDefn.GenericInstantiation (head, ImmutableArray.CreateRange args)

    /// Resolve, and report the answer in the same language as the input, so that inputs and
    /// outputs can be compared directly.
    let private resolveToTypeDefn
        (fixture : Fixture)
        (ty : TypeDefn)
        (typeGenericArgs : TypeDefn list)
        (methodGenericArgs : TypeDefn list)
        : TypeDefn
        =
        let assemblies, _, resolved =
            TypeResolution.resolveTypeFromDefn
                fixture.LoggerFactory
                [ runtimeDir ]
                fixture.BaseTypes
                ty
                (ImmutableArray.CreateRange typeGenericArgs)
                (ImmutableArray.CreateRange methodGenericArgs)
                fixture.Corelib
                fixture.Loaded

        DumpedAssembly.typeInfoToTypeDefn fixture.BaseTypes assemblies resolved

    /// The number of distinct nodes in <paramref name="ty"/> counted <i>by reference</i>, i.e. its
    /// size as a DAG. A traversal that counted structurally would itself be the exponential this
    /// file exists to rule out.
    let private dagSize (ty : TypeDefn) : int =
        let seen = HashSet<TypeDefn> (HashIdentity.Reference)

        let rec go (t : TypeDefn) : unit =
            if seen.Add t then
                match t with
                | TypeDefn.GenericInstantiation (generic, args) ->
                    go generic

                    for arg in args do
                        go arg
                | TypeDefn.OneDimensionalArrayLowerBoundZero elt
                | TypeDefn.Pointer elt
                | TypeDefn.Byref elt
                | TypeDefn.Pinned elt -> go elt
                | TypeDefn.Array (elt, _) -> go elt
                | TypeDefn.Modified m ->
                    go m.Unmodified
                    go m.Modifier
                | TypeDefn.PrimitiveType _
                | TypeDefn.FromReference _
                | TypeDefn.FromDefinition _
                | TypeDefn.FunctionPointer _
                | TypeDefn.GenericTypeParameter _
                | TypeDefn.GenericMethodParameter _
                | TypeDefn.Void -> ()

        go ty
        seen.Count

    /// Rebuild <paramref name="ty"/> so that no node is shared with any other: the same type, as a
    /// tree. Resolving this cannot benefit from reference-keyed memoisation of the input, so it is
    /// the control against which the shared original is compared.
    let rec private unshare (ty : TypeDefn) : TypeDefn =
        match ty with
        | TypeDefn.GenericInstantiation (generic, args) ->
            TypeDefn.GenericInstantiation (unshare generic, args |> Seq.map unshare |> ImmutableArray.CreateRange)
        | TypeDefn.OneDimensionalArrayLowerBoundZero elt -> TypeDefn.OneDimensionalArrayLowerBoundZero (unshare elt)
        | TypeDefn.Array (elt, rank) -> TypeDefn.Array (unshare elt, rank)
        | TypeDefn.Pointer elt -> TypeDefn.Pointer (unshare elt)
        | TypeDefn.Byref elt -> TypeDefn.Byref (unshare elt)
        | TypeDefn.PrimitiveType prim -> TypeDefn.PrimitiveType prim
        | TypeDefn.FromDefinition (identity, kind) -> TypeDefn.FromDefinition (identity, kind)
        | TypeDefn.FromReference (ref, kind) -> TypeDefn.FromReference (ref, kind)
        | TypeDefn.GenericTypeParameter i -> TypeDefn.GenericTypeParameter i
        | TypeDefn.GenericMethodParameter i -> TypeDefn.GenericMethodParameter i
        // The generators below never reach these, and a Void or a nullary case has nothing to
        // unshare in any event.
        | TypeDefn.Pinned _
        | TypeDefn.Modified _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> ty

    // ------------------------------------------------------------------
    // The regression: resolving a perfectly-shared nest costs its size as a DAG, not as a tree.
    // ------------------------------------------------------------------

    /// <summary>
    /// How deep a nest to resolve.
    /// </summary>
    /// <remarks>
    /// Deep enough that any of the exponential behaviours is unmissable, and shallow enough that
    /// this test <i>fails</i> on a regression rather than hanging. Each defect on its own costs
    /// 2^<c>d</c>, and the two together 4^<c>d</c>; restoring either one and measuring gives, at
    /// this depth, half a minute and 13 GB of allocation, or 51 seconds and 21 GB, against 0 ms
    /// and 67 KB with both fixed. Nearly all of that is churn rather than live data, so a
    /// regression fails the assertion below rather than exhausting the test host.
    /// </remarks>
    let private nestDepth = 22

    /// <summary>
    /// A ceiling on what resolving the nest may allocate.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Allocation, not elapsed time. It is what the defects actually produce — a redundant
    /// traversal of a <c>TypeDefn</c> <i>rebuilds</i> the subtree it walks — and unlike a clock it
    /// is deterministic, so the bound can be tight without a busy or slow machine making it flaky.
    /// </para>
    /// <para>
    /// A correct resolution at this depth allocates about 67 KB. This ceiling sits some sixty
    /// times above that and three thousand times below the smaller of the two regressions, so it
    /// is nowhere near either edge.
    /// </para>
    /// </remarks>
    let private nestAllocationBudget = 4L * 1024L * 1024L

    let private nest (fixture : Fixture) (depth : int) : TypeDefn =
        let dictionary = topLevel fixture "System.Collections.Generic" "Dictionary`2"

        let rec go (d : int) : TypeDefn =
            if d = 0 then
                TypeDefn.PrimitiveType PrimitiveType.Int32
            else
                // One `prev`, used in both argument positions: this is the sharing under test.
                let prev = go (d - 1)
                genericInstantiation dictionary [ prev ; prev ]

        go depth

    [<Test>]
    let ``a deeply shared nest resolves in work linear in its depth`` () : unit =
        let fixture = setUp ()
        let subject = nest fixture nestDepth

        // The input really is shared: a head, an instantiation per level, and the leaf.
        dagSize subject |> shouldBeSmallerThan (4 * (nestDepth + 1))

        // Warm the resolver first, so that one-off work — reading corelib's tables, priming
        // Dictionary`2's base chain — is not charged to the measurement below.
        resolveToTypeDefn fixture (nest fixture 1) [] [] |> ignore<TypeDefn>

        let before = GC.GetAllocatedBytesForCurrentThread ()
        let stopwatch = Stopwatch.StartNew ()
        let resolved = resolveToTypeDefn fixture subject [] []
        stopwatch.Stop ()
        let allocated = GC.GetAllocatedBytesForCurrentThread () - before

        TestContext.Out.WriteLine
            $"resolved nest(%d{nestDepth}) in %d{stopwatch.ElapsedMilliseconds} ms, allocating %d{allocated} bytes"

        allocated |> shouldBeSmallerThan nestAllocationBudget

        // The answer must be a DAG too: an implementation that walked the input as a tree would
        // have to *build* one, so it would report 2^d nodes here rather than a couple per level.
        // This is also what keeps the traversals below affordable.
        dagSize resolved |> shouldBeSmallerThan (4 * (nestDepth + 1))

        // And it must be the right answer: Dictionary`2 all the way down, with Int32 at the
        // bottom. Walking the spine is O(d) precisely because both arguments agree.
        let expectedLeaf =
            resolveToTypeDefn fixture (TypeDefn.PrimitiveType PrimitiveType.Int32) [] []

        let expectedHead =
            resolveToTypeDefn
                fixture
                (genericInstantiation
                    (topLevel fixture "System.Collections.Generic" "Dictionary`2")
                    [
                        TypeDefn.PrimitiveType PrimitiveType.Int32
                        TypeDefn.PrimitiveType PrimitiveType.Int32
                    ])
                []
                []
            |> function
                | TypeDefn.GenericInstantiation (head, _) -> head
                | ty -> failwith $"expected Dictionary`2 to resolve to an instantiation, got %O{ty}"

        let rec check (depth : int) (ty : TypeDefn) : unit =
            if depth = 0 then
                ty |> shouldEqual expectedLeaf
            else
                match ty with
                | TypeDefn.GenericInstantiation (head, args) ->
                    head |> shouldEqual expectedHead
                    args.Length |> shouldEqual 2
                    // Not merely equal: the *same node*. The resolved nest is a DAG, which is
                    // what makes this walk, and the resolution that produced it, linear.
                    System.Object.ReferenceEquals (args.[0], args.[1]) |> shouldEqual true
                    check (depth - 1) args.[0]
                | ty -> failwith $"expected an instantiation %d{depth} levels into the resolved nest, got %O{ty}"

        check nestDepth resolved

    // ------------------------------------------------------------------
    // What substitution owes its callers: a closed answer.
    // ------------------------------------------------------------------

    /// <summary>
    /// A bare generic definition reached under an environment that is legal but not closed still
    /// resolves to a closed type.
    /// </summary>
    /// <remarks>
    /// <para>
    /// A bare <c>FromDefinition</c> of a generic type is not a type and no signature can spell it;
    /// the resolver's convention is that it names that definition instantiated at the ambient
    /// environment. Resolution reads those arguments straight out of the environment, so unless
    /// substitution is applied to them afterwards they arrive in the answer exactly as the caller
    /// wrote them — <i>including</i> any generic parameters they mention.
    /// </para>
    /// <para>
    /// The environment here is <c>!0 := !1</c>, <c>!1 := int</c>: well-founded, which is all
    /// <c>resolveTypeFromDefn</c> requires, but not closed. The one-step indirection is the whole
    /// point — a resolver that leaks the raw entry answers <c>List&lt;List&lt;List&lt;!1&gt;&gt;&gt;</c>,
    /// an open type which then cannot be resolved again on its own.
    /// </para>
    /// </remarks>
    [<Test>]
    let ``a bare generic definition is instantiated at the substituted environment`` () : unit =
        let fixture = setUp ()
        let list = topLevel fixture "System.Collections.Generic" "List`1"

        // !0 := !1, !1 := int. Nothing here is cyclic; !0 just takes one hop to reach int.
        let environment =
            [ TypeDefn.GenericTypeParameter 1 ; TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        // List<List<List`1>>, where the innermost List`1 is the bare definition.
        let subject = genericInstantiation list [ genericInstantiation list [ list ] ]

        let resolved = resolveToTypeDefn fixture subject environment []

        let expected =
            resolveToTypeDefn
                fixture
                (genericInstantiation
                    list
                    [
                        genericInstantiation
                            list
                            [ genericInstantiation list [ TypeDefn.PrimitiveType PrimitiveType.Int32 ] ]
                    ])
                environment
                []

        resolved |> shouldEqual expected

        // Being closed is the substantive claim, and it is what the equality above rests on: a
        // closed answer is one that needs no environment to resolve a second time.
        resolveToTypeDefn fixture resolved [] [] |> shouldEqual resolved

    // ------------------------------------------------------------------
    // The properties.
    // ------------------------------------------------------------------

    let private leaves (fixture : Fixture) : TypeDefn list =
        [
            TypeDefn.PrimitiveType PrimitiveType.Int32
            TypeDefn.PrimitiveType PrimitiveType.String
            topLevel fixture "System" "Object"
        ]

    /// Terms which deliberately share subterms, so that resolving one exercises the memo.
    let private sharedTermGen
        (fixture : Fixture)
        (paramCount : int)
        (methodParamCount : int)
        (depth : int)
        : Gen<TypeDefn>
        =
        let dictionary = topLevel fixture "System.Collections.Generic" "Dictionary`2"
        let list = topLevel fixture "System.Collections.Generic" "List`1"

        let rec go (depth : int) : Gen<TypeDefn> =
            let atoms =
                [
                    yield! leaves fixture |> List.map Gen.constant
                    for i in 0 .. paramCount - 1 do
                        yield Gen.constant (TypeDefn.GenericTypeParameter i)
                    for i in 0 .. methodParamCount - 1 do
                        yield Gen.constant (TypeDefn.GenericMethodParameter i)

                    // A *bare* generic definition, as opposed to an instantiation of one. This is
                    // not a type, and metadata cannot spell it in a signature; the resolver's
                    // convention is that it means "this definition, instantiated at the ambient
                    // environment". It has to be drawn, because it is the only shape that puts an
                    // environment entry into the answer without substitution having touched it,
                    // and a generator whose alphabet omits it cannot tell a resolver that
                    // substitutes those entries from one that does not. It is only in range when
                    // the environment has a slot for every parameter of the definition — with
                    // fewer, resolution indexes past the end of the environment, which is a
                    // malformed call rather than the question under test.
                    if paramCount >= 1 then
                        yield Gen.constant list

                    if paramCount >= 2 then
                        yield Gen.constant dictionary
                ]

            if depth <= 0 then
                Gen.oneof atoms
            else
                Gen.oneof
                    [
                        yield! atoms

                        yield
                            gen {
                                let! left = go (depth - 1)
                                let! right = go (depth - 1)
                                // Half the time, hand the *same node* to both parameters. This is
                                // what makes the input a DAG rather than a tree.
                                let! share = Gen.elements [ true ; false ]
                                return genericInstantiation dictionary [ left ; (if share then left else right) ]
                            }

                        yield
                            gen {
                                let! element = go (depth - 1)
                                return genericInstantiation list [ element ]
                            }

                        yield
                            gen {
                                let! element = go (depth - 1)
                                return TypeDefn.OneDimensionalArrayLowerBoundZero element
                            }
                    ]

        go depth

    let private environmentGen (fixture : Fixture) : Gen<TypeDefn * TypeDefn list * TypeDefn list> =
        gen {
            let! paramCount = Gen.choose (0, 2)
            let! methodParamCount = Gen.choose (0, 2)
            let term = sharedTermGen fixture paramCount methodParamCount 3
            let! subject = term
            // Environments are drawn from the same generator, so they may well be cyclic; the
            // properties below tolerate a cyclic-environment refusal, but nothing else.
            let! typeArgs = Gen.listOfLength paramCount term
            let! methodArgs = Gen.listOfLength methodParamCount term
            return subject, typeArgs, methodArgs
        }

    /// <summary>
    /// Whether resolving <paramref name="ty"/> runs substitution over its generic arguments at
    /// all — which, at the outermost node, means being an instantiation.
    /// </summary>
    /// <remarks>
    /// <para>
    /// Only <c>GenericInstantiation</c> substitutes: its arguments are put through
    /// <c>substituteGenericsInTypeDefn</c> before the head is resolved under them. Every other
    /// outermost shape reaches the resolver's <c>FromDefinition</c> case directly — either because
    /// it <i>is</i> a bare definition, or because chasing a parameter arrived at one — and that
    /// case instantiates the definition by copying environment entries across verbatim. An
    /// argument therefore comes back exactly as the caller spelled it, <c>PrimitiveType Int32</c>
    /// rather than the <c>System.Int32</c> definition it would canonicalise to, and still
    /// mentioning any parameters it mentioned going in.
    /// </para>
    /// <para>
    /// Every live caller passes canonical closed arguments, so this is harmless — but it does
    /// mean such an answer is not a fixed point, so the idempotence property below asks only
    /// about subjects that actually exercise substitution. A bare definition <i>nested</i> inside
    /// an instantiation does go through it, and that is the case the property must cover.
    /// </para>
    /// </remarks>
    let private substitutes (ty : TypeDefn) : bool =
        match ty with
        | TypeDefn.GenericInstantiation _ -> true
        | _ -> false

    /// Either the resolved type, or the resolver's refusal to resolve a cyclic environment.
    type private Outcome =
        | Resolved of TypeDefn
        | Cyclic

    let private outcome
        (fixture : Fixture)
        (ty : TypeDefn)
        (typeArgs : TypeDefn list)
        (methodArgs : TypeDefn list)
        : Outcome
        =
        try
            Outcome.Resolved (resolveToTypeDefn fixture ty typeArgs methodArgs)
        with e when e.Message.Contains cyclicMarker ->
            Outcome.Cyclic

    /// <summary>
    /// Sharing in the input changes what resolution costs, and nothing else.
    /// </summary>
    /// <remarks>
    /// This is the property the memo has to satisfy: it is keyed on reference identity, so it
    /// fires exactly when a subterm is physically shared, and it would be a silent
    /// wrong-answer machine if that ever changed the answer. The control is the same type rebuilt
    /// as a tree, which the memo cannot help with.
    /// </remarks>
    [<Test>]
    let ``sharing the input does not change the answer`` () : unit =
        let fixture = setUp ()

        let mutable actuallyShared = 0
        let mutable resolved = 0
        let mutable cyclic = 0

        let property (subject : TypeDefn, typeArgs : TypeDefn list, methodArgs : TypeDefn list) : bool =
            let asTree = unshare subject
            let treeArgs = typeArgs |> List.map unshare
            let treeMethodArgs = methodArgs |> List.map unshare

            if dagSize subject < dagSize asTree then
                actuallyShared <- actuallyShared + 1

            let shared = outcome fixture subject typeArgs methodArgs
            let unshared = outcome fixture asTree treeArgs treeMethodArgs

            match shared with
            | Outcome.Resolved _ -> resolved <- resolved + 1
            | Outcome.Cyclic -> cyclic <- cyclic + 1

            shared = unshared

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (environmentGen fixture)) property
        )

        TestContext.Out.WriteLine $"shared inputs %d{actuallyShared}, resolved %d{resolved}, cyclic %d{cyclic}"

        // Without genuinely-shared inputs the memo never fires and the property says nothing.
        if actuallyShared = 0 then
            Assert.Fail "Generator produced no shared input; the property never exercised the memo."

        if resolved = 0 then
            Assert.Fail "Generator produced no resolvable environment; the property is vacuous."

    /// <summary>
    /// Resolution is idempotent: feeding an answer back in, under the empty environment, returns
    /// it unchanged.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is the fact that licenses the shape of <c>substituteGenericsInTypeDefn</c>'s
    /// instantiation case: it substitutes an argument list once and does not re-enter
    /// <c>resolveTypeFromDefn</c> on the instantiation it has just rebuilt. Omitting that second
    /// pass is only sound because it cannot change anything, and this is that claim, tested.
    /// </para>
    /// <para>
    /// The subject is an instantiation, because that is the shape which substitutes at all; see
    /// <c>substitutes</c> for why the others are not fixed points.
    /// </para>
    /// </remarks>
    [<Test>]
    let ``resolution is idempotent`` () : unit =
        let fixture = setUp ()

        let mutable checked' = 0

        let property (subject : TypeDefn, typeArgs : TypeDefn list, methodArgs : TypeDefn list) : bool =
            match outcome fixture subject typeArgs methodArgs with
            | Outcome.Cyclic -> true
            | Outcome.Resolved once ->
                checked' <- checked' + 1

                // The answer is closed, so it needs no environment to resolve a second time; if it
                // still mentioned a parameter, this would throw an index-out-of-range rather than
                // quietly disagree.
                match outcome fixture once [] [] with
                | Outcome.Cyclic -> false
                | Outcome.Resolved twice -> once = twice

        let subjects =
            environmentGen fixture |> Gen.where (fun (subject, _, _) -> substitutes subject)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen subjects) property)

        TestContext.Out.WriteLine $"checked idempotence on %d{checked'} resolved types"

        if checked' = 0 then
            Assert.Fail "Generator produced no resolvable environment; the property is vacuous."
