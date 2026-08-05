namespace WoofWare.PawPrint.Test

open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the pure cores of the `RuntimeMethodHandle` natives (NativeRuntimeMethodHandle.fs):
/// `isGenericMethodDefinition` (behind the `IsGenericMethodDefinition` InternalCall) and
/// `methodInstantiationTargets` (behind the `RuntimeMethodHandle_GetMethodInstantiation` QCall).
/// Each has partial end-to-end coverage in `sourcesPure/`; these tests pin the arms that are not
/// yet reachable end-to-end, plus the ordering and fail-loud behaviour that a passing end-to-end
/// case would not distinguish.
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
