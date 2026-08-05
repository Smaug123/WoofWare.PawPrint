namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the predicate behind the `RuntimeMethodHandle.IsGenericMethodDefinition`
/// InternalCall (NativeRuntimeMethodHandle.fs).
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
