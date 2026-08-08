namespace WoofWare.PawPrint.Test

open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the pure cores of the `RuntimeMethodHandle` natives (NativeRuntimeMethodHandle.fs):
/// the predicates and projections those FCalls and QCalls return, each named after and documented
/// against the CoreCLR function it ports. Most have only partial end-to-end coverage in
/// `sourcesPure/` -- several are unreachable in isolation, being consecutive statements of one BCL
/// method -- so these tests pin the arms an end-to-end case cannot reach, plus the ordering and
/// fail-loud behaviour a passing one would not distinguish. Each section below cites its CoreCLR
/// source and says what it is for; there is deliberately no index here to fall out of date.
///
/// The end-to-end coverage lives in `sourcesPure/MethodIsGenericMethodDefinition.cs`, which pins
/// two of the predicate's three arms: a generic method definition, and a plain non-generic
/// method. Its third arm -- a non-generic method declared on a *generic* type, which must not be
/// reported as a generic method definition even though its declaring type is generic -- can't yet
/// be reached end-to-end: reflecting any method off a generic type (open or closed) currently
/// hits unrelated pre-existing gaps (`RuntimeMethodHandle_GetStubIfNeededSlow` for closed
/// instantiations, `RuntimeTypeHandle.GetNumVirtuals` for open generic type definitions -- see the
/// comment in that C# source file). These tests exercise the predicate directly so all three arms
/// are pinned regardless.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeRuntimeMethodHandle =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    [<Test>]
    let ``generic method definition: declares generic parameters, handle unbound`` () : unit =
        NativeRuntimeMethodHandle.isGenericMethodDefinition 1 0 |> shouldEqual true
        NativeRuntimeMethodHandle.isGenericMethodDefinition 3 0 |> shouldEqual true

    [<Test>]
    let ``constructed generic method: declares generic parameters, handle bound`` () : unit =
        NativeRuntimeMethodHandle.isGenericMethodDefinition 1 1 |> shouldEqual false
        NativeRuntimeMethodHandle.isGenericMethodDefinition 3 3 |> shouldEqual false

    [<Test>]
    let ``non-generic method (whether or not its declaring type is generic): never a generic method definition``
        ()
        : unit
        =
        // `handleInstantiationCount` here stands for the *method's* bound generic arguments; a
        // non-generic method carries none regardless of how many generic parameters its
        // declaring type has, but the predicate must be false for a non-generic method
        // regardless of what's passed for the second count, since class-level generics never
        // make CoreCLR's `MethodDesc::IsGenericMethodDefinition` observe `mcInstantiated`.
        NativeRuntimeMethodHandle.isGenericMethodDefinition 0 0 |> shouldEqual false
        NativeRuntimeMethodHandle.isGenericMethodDefinition 0 1 |> shouldEqual false

    [<Test>]
    let ``property: any bound handle is never a generic method definition`` () : unit =
        let property (methodGenericParamCount : int) (handleInstantiationCount : int) : bool =
            let methodGenericParamCount = abs methodGenericParamCount
            let handleInstantiationCount = 1 + abs handleInstantiationCount

            not (NativeRuntimeMethodHandle.isGenericMethodDefinition methodGenericParamCount handleInstantiationCount)

        Check.One (propertyConfig, property)

    [<Test>]
    let ``property: a method with no generic parameters of its own is never a generic method definition`` () : unit =
        let property (handleInstantiationCount : int) : bool =
            let handleInstantiationCount = abs handleInstantiationCount
            not (NativeRuntimeMethodHandle.isGenericMethodDefinition 0 handleInstantiationCount)

        Check.One (propertyConfig, property)

    [<Test>]
    let ``property: a method with its own generic parameters, referenced through an unbound handle, always is a generic method definition``
        ()
        : unit
        =
        let property (methodGenericParamCount : int) : bool =
            let methodGenericParamCount = 1 + abs methodGenericParamCount
            NativeRuntimeMethodHandle.isGenericMethodDefinition methodGenericParamCount 0

        Check.One (propertyConfig, property)

    // ---------------------------------------------------------------------------------------
    // `methodInstantiationTargets`: the instantiation reported by CoreCLR's
    // `MethodDesc::LoadMethodInstantiation`, which backs the
    // `RuntimeMethodHandle_GetMethodInstantiation` QCall.
    //
    // End-to-end coverage lives in `sourcesPure/MethodGetGenericArguments.cs`, but that can only
    // reach the unbound arms: PawPrint has no reflection path yet that hands the BCL a *bound*
    // method handle (see that file's comment). These tests pin all three arms directly.
    // ---------------------------------------------------------------------------------------

    let private operation : string = "test"

    /// Synthetic declaring type. `methodInstantiationTargets` is pure: it never dereferences the
    /// identity or the MethodDef token, it only copies them into each result element, so entirely
    /// fabricated metadata handles are sufficient (and keep the test independent of any assembly).
    let private declaringType : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofTypeDefinition
            (AssemblyName "Synthetic.Assembly")
            (MetadataTokens.TypeDefinitionHandle 1)

    let private methodDefinition : ComparableMethodDefinitionHandle =
        ComparableMethodDefinitionHandle.Make (MetadataTokens.MethodDefinitionHandle 7)

    let private syntheticArgument (index : int) : ConcreteTypeHandle =
        ConcreteTypeHandle.Concrete (100 + index)

    /// A *consistent* (declared-count, handle-instantiation) pair: either the handle is unbound
    /// (the method definition / non-generic case) or it binds exactly as many arguments as the
    /// method declares. Generating the consistency by construction matters: an unconstrained pair
    /// would almost always be inconsistent, and every such case takes the fail-loud path instead
    /// of exercising the arms these properties are about. The mismatch path is pinned separately
    /// below.
    let private consistentInstantiation : Gen<int * ConcreteTypeHandle list> =
        gen {
            let! declaredCount = Gen.choose (0, 8)
            let! bound = Gen.elements [ true ; false ]

            let handleInstantiation =
                if bound then
                    List.init declaredCount syntheticArgument
                else
                    []

            return declaredCount, handleInstantiation
        }

    let private targetsFor
        (declaredCount : int)
        (handleInstantiation : ConcreteTypeHandle list)
        : RuntimeTypeHandleTarget list
        =
        NativeRuntimeMethodHandle.methodInstantiationTargets
            operation
            declaringType
            methodDefinition
            declaredCount
            handleInstantiation

    [<Test>]
    let ``property: the instantiation always has one element per declared generic parameter`` () : unit =
        let property (declaredCount : int, handleInstantiation : ConcreteTypeHandle list) : bool =
            List.length (targetsFor declaredCount handleInstantiation) = declaredCount

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen consistentInstantiation) property)

    [<Test>]
    let ``property: a generic method definition reports its own type variables, in declaration order`` () : unit =
        let property (declaredCount : int) : bool =
            let declaredCount = 1 + abs (declaredCount % 8)

            let expected : RuntimeTypeHandleTarget list =
                List.init
                    declaredCount
                    (fun position ->
                        RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, methodDefinition, position)
                    )

            // Precondition of this arm, stated in terms of the classifier the implementation uses.
            NativeRuntimeMethodHandle.isGenericMethodDefinition declaredCount 0
            && targetsFor declaredCount [] = expected

        Check.One (propertyConfig, property)

    [<Test>]
    let ``property: a bound handle reports exactly its type arguments, order-preserving`` () : unit =
        let property (declaredCount : int) : bool =
            let declaredCount = 1 + abs (declaredCount % 8)
            let handleInstantiation = List.init declaredCount syntheticArgument

            let expected : RuntimeTypeHandleTarget list =
                handleInstantiation |> List.map RuntimeTypeHandleTarget.Closed

            targetsFor declaredCount handleInstantiation = expected

        Check.One (propertyConfig, property)

    [<Test>]
    let ``a non-generic method has an empty instantiation`` () : unit = targetsFor 0 [] |> shouldEqual []

    [<Test>]
    let ``property: a handle whose instantiation disagrees with the metadata is rejected`` () : unit =
        let property (declaredCount : int, boundCount : int) : bool =
            let declaredCount = abs (declaredCount % 8)
            let boundCount = 1 + abs (boundCount % 8)

            if boundCount = declaredCount then
                // Not a mismatch; nothing to assert.
                true
            else
                let handleInstantiation = List.init boundCount syntheticArgument

                try
                    targetsFor declaredCount handleInstantiation
                    |> ignore<RuntimeTypeHandleTarget list>

                    false
                with _ ->
                    true

        Check.One (propertyConfig, property)

    [<Test>]
    let ``a negative declared generic-parameter count is rejected`` () : unit =
        (fun () -> targetsFor -1 [] |> ignore<RuntimeTypeHandleTarget list>)
        |> shouldFail<exn>

    // ---------------------------------------------------------------------------------------
    // `fastPathReturnsOriginal` / `stubOutcome`: the decision behind
    // `RuntimeMethodHandle_GetStubIfNeededSlow`, i.e. CoreCLR's
    // `MethodDesc::FindOrCreateAssociatedMethodDescForReflection`.
    //
    // `sourcesPure/MethodOnClosedGenericType.cs` covers the reachable end-to-end shapes; these
    // tests pin the outcome table itself, including the cases the BCL never routes here.
    // ---------------------------------------------------------------------------------------

    let private methodTable
        (isValueType : bool)
        (hasInstantiation : bool)
        (isGenericTypeDefinition : bool)
        (isInterface : bool)
        : StubDeclaringType
        =
        StubDeclaringType.MethodTable
            {
                IsValueType = isValueType
                HasInstantiation = hasInstantiation
                IsGenericTypeDefinition = isGenericTypeDefinition
                IsInterface = isInterface
            }

    /// Every MethodTable fact combination, plus the TypeDesc case.
    let private allDeclaringTypes : StubDeclaringType list =
        StubDeclaringType.TypeDesc
        :: [
            for isValueType in [ true ; false ] do
                for hasInstantiation in [ true ; false ] do
                    for isGenericTypeDefinition in [ true ; false ] do
                        for isInterface in [ true ; false ] ->
                            methodTable isValueType hasInstantiation isGenericTypeDefinition isInterface
        ]

    /// The load-bearing property. CoreCLR documents the fast-path FCall predicate as "duplicated
    /// from FindOrCreateAssociatedMethodDescForReflection" (runtimehandles.cpp:1899-1900). The two
    /// are written independently here, so a typo in either breaks this: whenever the fast path
    /// short-circuits to the original MethodDesc, the slow path -- which the BCL would then never
    /// call -- must agree that no rebinding is needed. Exhaustive over the fact space.
    [<Test>]
    let ``the slow path agrees with the fast path wherever the fast path short-circuits`` () : unit =
        for declaringType in allDeclaringTypes do
            for methodGenericParamCount in 0..3 do
                for methodIsStatic in [ true ; false ] do
                    // The fast path is only consulted for a null instantiation
                    // (RuntimeHandles.cs:1258).
                    let methodHasInstantiation = methodGenericParamCount > 0

                    if NativeRuntimeMethodHandle.fastPathReturnsOriginal methodHasInstantiation declaringType then
                        NativeRuntimeMethodHandle.stubOutcome declaringType methodIsStatic methodGenericParamCount 0
                        |> shouldEqual StubOutcome.Original

    [<Test>]
    let ``a TypeDesc declaring type never gets a stub, whatever the instantiation`` () : unit =
        // genmeth.cpp:1247-1249 returns before even looking at the instantiation.
        NativeRuntimeMethodHandle.stubOutcome StubDeclaringType.TypeDesc false 0 0
        |> shouldEqual StubOutcome.Original

        NativeRuntimeMethodHandle.stubOutcome StubDeclaringType.TypeDesc true 1 1
        |> shouldEqual StubOutcome.Original

        // Even an arity that would otherwise be rejected. This is load-bearing for the QCall arm's
        // ordering: because the answer here is `Original`, the arm must not have already tried to
        // narrow the instantiation's elements to closed types, since CoreCLR never inspects them
        // on this path.
        NativeRuntimeMethodHandle.stubOutcome StubDeclaringType.TypeDesc false 2 1
        |> shouldEqual StubOutcome.Original

    [<Test>]
    let ``the empty-instantiation outcomes match CoreCLR's predicate`` () : unit =
        // genmeth.cpp:1272-1277: a stub is needed iff the method is non-generic AND the declaring
        // type is a value type, or is a bound generic on which the method is static or which is an
        // interface.
        let check (declaringType : StubDeclaringType) (isStatic : bool) (genericCount : int) (expected : StubOutcome) =
            NativeRuntimeMethodHandle.stubOutcome declaringType isStatic genericCount 0
            |> shouldEqual expected

        // Instance method on a closed generic class: no stub (this is the measured
        // `Container<int>.Instance` case).
        check (methodTable false true false false) false 0 StubOutcome.Original
        // Static method on a closed generic class: stub.
        check (methodTable false true false false) true 0 StubOutcome.Rebind
        // Instance method on a generic struct: stub (value types always need one).
        check (methodTable true true false false) false 0 StubOutcome.Rebind
        // Instance method on a *non*-generic struct: stub.
        check (methodTable true false false false) false 0 StubOutcome.Rebind
        // Method on a closed generic interface: stub.
        check (methodTable false true false true) false 0 StubOutcome.Rebind
        // Open generic type definition: no stub, however static/interface-y.
        check (methodTable false true true true) true 0 StubOutcome.Original
        // Plain non-generic reference type: no stub.
        check (methodTable false false false false) false 0 StubOutcome.Original
        // A *generic* method with an unbound handle never gets one, because CoreCLR's second
        // branch requires !pMethod->HasMethodInstantiation().
        check (methodTable true true false true) true 1 StubOutcome.Original

    [<Test>]
    let ``property: a well-formed instantiation always rebinds, on any MethodTable`` () : unit =
        let property (declaredCount : int) : bool =
            let declaredCount = 1 + abs (declaredCount % 8)

            allDeclaringTypes
            |> List.filter (fun d -> d <> StubDeclaringType.TypeDesc)
            |> List.forall (fun declaringType ->
                [ true ; false ]
                |> List.forall (fun isStatic ->
                    NativeRuntimeMethodHandle.stubOutcome declaringType isStatic declaredCount declaredCount = StubOutcome.Rebind
                )
            )

        Check.One (propertyConfig, property)

    [<Test>]
    let ``property: an instantiation whose arity disagrees with the method is rejected`` () : unit =
        let property (declaredCount : int, boundCount : int) : bool =
            let declaredCount = abs (declaredCount % 8)
            let boundCount = 1 + abs (boundCount % 8)

            if boundCount = declaredCount then
                true
            else
                // Includes the non-generic-method case (declaredCount = 0), where CoreCLR's
                // `_ASSERTE(pMethod->HasMethodInstantiation())` fires in debug and the arity check
                // catches it in release.
                allDeclaringTypes
                |> List.filter (fun d -> d <> StubDeclaringType.TypeDesc)
                |> List.forall (fun declaringType ->
                    NativeRuntimeMethodHandle.stubOutcome declaringType false declaredCount boundCount = StubOutcome.ArityMismatch
                )

        Check.One (propertyConfig, property)

    /// A `ConcreteTypeHandle` of each shape, nested to the given depth. `methodTableOfDeclaringType`
    /// classifies on the outermost constructor only, so depth exists purely to stop the nominal case
    /// from being the only inhabitant reached.
    let rec private concreteTypeHandleOfDepth (depth : int) : Gen<ConcreteTypeHandle> =
        let nominal = Gen.choose (0, 20) |> Gen.map ConcreteTypeHandle.Concrete

        if depth <= 0 then
            nominal
        else

        let inner = concreteTypeHandleOfDepth (depth - 1)

        let functionPointer =
            inner
            |> Gen.map (fun ret ->
                ConcreteTypeHandle.FunctionPointer
                    {
                        Header = ComparableSignatureHeader.Make (System.Reflection.Metadata.SignatureHeader 0uy)
                        ParameterTypes = []
                        GenericParameterCount = 0
                        RequiredParameterCount = 0
                        ReturnType = MethodReturnType.Returns ret
                    }
            )

        let array =
            gen {
                let! element = inner
                let! rank = Gen.choose (1, 3)
                return ConcreteTypeHandle.Array (element, rank)
            }

        Gen.oneof
            [
                nominal
                inner |> Gen.map ConcreteTypeHandle.Byref
                inner |> Gen.map ConcreteTypeHandle.Pointer
                inner |> Gen.map ConcreteTypeHandle.OneDimArrayZero
                array
                functionPointer
            ]

    let private isNominal (handle : ConcreteTypeHandle) : bool =
        match handle with
        | ConcreteTypeHandle.Concrete _ -> true
        | _ -> false

    [<Test>]
    let ``property: GetMethodTable accepts exactly the nominal declaring types, unchanged`` () : unit =
        let property (handle : ConcreteTypeHandle) : bool =
            match NativeRuntimeMethodHandle.methodTableOfDeclaringType handle with
            | Ok target ->
                // Accepted handles are returned verbatim -- no canonicalisation, no projection --
                // and must be MethodTable-backed by the single home of CoreCLR's tagged-pointer
                // rule, since `NativeIntSource.MethodTablePtr` is what the caller wraps them in.
                isNominal handle
                && target = RuntimeTypeHandleTarget.Closed handle
                && TypeHandleTag.forTarget target = 0L
            | Error _ -> not (isNominal handle)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (concreteTypeHandleOfDepth 3)) property)

    [<Test>]
    let ``property: the shapes refused despite owning a MethodTable are exactly the arrays`` () : unit =
        // Arrays are the one refusal that is not justified by CoreCLR: they are not TypeDescs, they
        // do own MethodTables, and their Get/Set/Address MethodDescs live on them. They are refused
        // only because PawPrint's registry cannot mint such a handle. Keeping that distinction
        // visible in a test means the day it becomes wrong, it fails here rather than silently
        // rejecting a legitimate handle.
        let property (handle : ConcreteTypeHandle) : bool =
            let refused =
                match NativeRuntimeMethodHandle.methodTableOfDeclaringType handle with
                | Ok _ -> false
                | Error _ -> true

            let methodTableBacked =
                TypeHandleTag.forTarget (RuntimeTypeHandleTarget.Closed handle) = 0L

            let isArray =
                match handle with
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> true
                | _ -> false

            (refused && methodTableBacked) = isArray

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (concreteTypeHandleOfDepth 3)) property)

    [<Test>]
    let ``GetMethodTable refusals say which shape they refused and why`` () : unit =
        let reasonFor (handle : ConcreteTypeHandle) : string =
            match NativeRuntimeMethodHandle.methodTableOfDeclaringType handle with
            | Ok target -> failwith $"expected %O{handle} to be refused, but it produced %O{target}"
            | Error reason -> reason

        reasonFor (ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 1))
        |> shouldContainText "TypeDesc"

        reasonFor (ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Concrete 1))
        |> shouldContainText "array"

    // ---------------------------------------------------------------------------------------
    // `isConstructorOrClassConstructor`: CoreCLR's `MethodDesc::IsClassConstructorOrCtor`
    // (method.hpp:491), which the `RuntimeMethodHandle.IsConstructor` FCall
    // (runtimehandles.cpp:2135) returns verbatim.
    //
    // `RuntimeType.GetMethodBase` (RuntimeType.CoreCLR.cs:1932) branches on it to decide whether
    // to build a `RuntimeConstructorInfo` or a `RuntimeMethodInfo`, so the answer selects which
    // reflection object a guest gets back, not merely a flag it can read.
    // ---------------------------------------------------------------------------------------

    let private withRtSpecialName (attrs : MethodAttributes) : MethodAttributes =
        enum<MethodAttributes> (int attrs ||| int MethodAttributes.RTSpecialName)

    let private withoutRtSpecialName (attrs : MethodAttributes) : MethodAttributes =
        enum<MethodAttributes> (int attrs &&& ~~~(int MethodAttributes.RTSpecialName))

    /// Arbitrary `MethodAttributes` bit patterns. The predicate consults exactly one bit, so the
    /// point of generating the rest is to pin that: no other flag may change the answer.
    let private methodAttributes : Gen<MethodAttributes> =
        Gen.choose (0, 65535) |> Gen.map enum<MethodAttributes>

    /// Names spanning the two that count and several near misses. `.CCTOR` is here because CoreCLR
    /// compares with `strcmp` (corhdr.h:433-435), so the match is case-sensitive.
    let private candidateNames : Gen<string> =
        Gen.elements [ ".ctor" ; ".cctor" ; ".CCTOR" ; ".Ctor" ; "ctor" ; ".ctorx" ; "Foo" ; "" ]

    [<Test>]
    let ``IsConstructor: the truth table CoreCLR's macros spell out`` () : unit =
        // `IsMdInstanceInitializer` / `IsMdClassConstructor` (corhdr.h:433,435) each re-test
        // mdRTSpecialName, so the whole predicate is: the bit is set, and the name is one of the
        // two initializer names. Note neither macro consults mdStatic: a `.cctor` is reported by
        // name and flag alone.
        let rtSpecial = MethodAttributes.RTSpecialName

        NativeRuntimeMethodHandle.isConstructorOrClassConstructor rtSpecial ".ctor"
        |> shouldEqual true

        NativeRuntimeMethodHandle.isConstructorOrClassConstructor rtSpecial ".cctor"
        |> shouldEqual true

        // The name alone is not enough: an ordinary method that happens to be called `.ctor`
        // without the flag is not a constructor to CoreCLR.
        NativeRuntimeMethodHandle.isConstructorOrClassConstructor MethodAttributes.Public ".ctor"
        |> shouldEqual false

        NativeRuntimeMethodHandle.isConstructorOrClassConstructor MethodAttributes.Public ".cctor"
        |> shouldEqual false

        // The flag alone is not enough either: RTSpecialName is also set on other runtime-special
        // members, and it is the name that distinguishes an initializer.
        NativeRuntimeMethodHandle.isConstructorOrClassConstructor rtSpecial "Foo"
        |> shouldEqual false

        // Case-sensitive, and no prefix matching.
        NativeRuntimeMethodHandle.isConstructorOrClassConstructor rtSpecial ".CCTOR"
        |> shouldEqual false

        NativeRuntimeMethodHandle.isConstructorOrClassConstructor rtSpecial ".ctorx"
        |> shouldEqual false

    [<Test>]
    let ``property: without RTSpecialName nothing is a constructor, whatever it is called`` () : unit =
        let property (attrs : MethodAttributes, name : string) : bool =
            not (NativeRuntimeMethodHandle.isConstructorOrClassConstructor (withoutRtSpecialName attrs) name)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip methodAttributes candidateNames)) property)

    [<Test>]
    let ``property: only the two initializer names can be constructors, whatever the flags`` () : unit =
        let property (attrs : MethodAttributes, name : string) : bool =
            if name = ".ctor" || name = ".cctor" then
                // With the flag set these must be true; the other flags must not interfere.
                NativeRuntimeMethodHandle.isConstructorOrClassConstructor (withRtSpecialName attrs) name
            else
                not (NativeRuntimeMethodHandle.isConstructorOrClassConstructor (withRtSpecialName attrs) name)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.zip methodAttributes candidateNames)) property)

    // ---------------------------------------------------------------------------------------
    // `hasMethodInstantiation`: CoreCLR's `MethodDesc::HasMethodInstantiation` (method.hpp:3812),
    // which the `RuntimeMethodHandle.HasMethodInstantiation` FCall (runtimehandles.cpp:1722)
    // returns verbatim.
    //
    // The trap this set of properties exists to catch: the name suggests "this handle has type
    // arguments bound to it", but CoreCLR's is `mcInstantiated == GetClassification() &&
    // IMD_HasMethodInstantiation()`, and `IMD_HasMethodInstantiation` (method.hpp:3520) returns
    // TRUE for a generic method *definition*. `RuntimeMethodInfo.IsGenericMethod` is this predicate
    // verbatim (RuntimeMethodInfo.CoreCLR.cs:471), and that is true of an open definition. So the
    // question is "does this method declare type parameters", not "are they bound".
    // ---------------------------------------------------------------------------------------

    [<Test>]
    let ``HasMethodInstantiation: declared arity decides it, bound or not`` () : unit =
        NativeRuntimeMethodHandle.hasMethodInstantiation 0 |> shouldEqual false
        NativeRuntimeMethodHandle.hasMethodInstantiation 1 |> shouldEqual true
        NativeRuntimeMethodHandle.hasMethodInstantiation 3 |> shouldEqual true

    [<Test>]
    let ``property: a generic method definition always has a method instantiation`` () : unit =
        // `IMD_HasMethodInstantiation` returns TRUE outright for the definition (method.hpp:3524),
        // so the two predicates cannot disagree in this direction. Reading the FCall as "arguments
        // are bound to this handle" breaks exactly here, which is what this catches.
        let property (declaredCount : int, handleInstantiation : ConcreteTypeHandle list) : bool =
            if NativeRuntimeMethodHandle.isGenericMethodDefinition declaredCount handleInstantiation.Length then
                NativeRuntimeMethodHandle.hasMethodInstantiation declaredCount
            else
                true

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen consistentInstantiation) property)

    [<Test>]
    let ``property: no method instantiation implies no generic method definition`` () : unit =
        let property (declaredCount : int, handleInstantiation : ConcreteTypeHandle list) : bool =
            if NativeRuntimeMethodHandle.hasMethodInstantiation declaredCount then
                true
            else
                not (NativeRuntimeMethodHandle.isGenericMethodDefinition declaredCount handleInstantiation.Length)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen consistentInstantiation) property)

    [<Test>]
    let ``property: has a method instantiation exactly when the reported instantiation is non-empty`` () : unit =
        // `MethodInfo.GetGenericArguments()` is served by `methodInstantiationTargets` and
        // `IsGenericMethod` by this predicate, so a guest that sees `IsGenericMethod = true` must
        // get a non-empty argument array, and vice versa. Pinning the two against each other is
        // what keeps that guest-visible pair coherent.
        let property (declaredCount : int, handleInstantiation : ConcreteTypeHandle list) : bool =
            let reported = targetsFor declaredCount handleInstantiation

            let hasInstantiation =
                NativeRuntimeMethodHandle.hasMethodInstantiation declaredCount

            hasInstantiation = not reported.IsEmpty

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen consistentInstantiation) property)

    [<Test>]
    let ``property: the generic-method-info branch is taken exactly for bound generic methods`` () : unit =
        // `RuntimeType.GetMethodBase` (RuntimeType.CoreCLR.cs:1940) routes to
        // `Cache.GetGenericMethodInfo` when `HasMethodInstantiation && !IsGenericMethodDefinition`,
        // and to `Cache.GetMethod` otherwise. That conjunction should hold exactly for a handle of
        // a generic method with its arguments bound.
        let property (declaredCount : int, handleInstantiation : ConcreteTypeHandle list) : bool =
            let genericMethodInfoBranch =
                NativeRuntimeMethodHandle.hasMethodInstantiation declaredCount
                && not (NativeRuntimeMethodHandle.isGenericMethodDefinition declaredCount handleInstantiation.Length)

            genericMethodInfoBranch = (declaredCount > 0 && not handleInstantiation.IsEmpty)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen consistentInstantiation) property)
