namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Pins the identity of a method's two entry points. CoreCLR gives a value type's instance method
/// an unboxed entry point (`this` is a byref) and a boxed one (an unboxing stub, whose `this` is
/// the box), at different addresses: measured, `ActivatorCache`'s `_pfnRefCtor` and
/// `_pfnValueCtor` differ for the same constructor. `ceq` over two function pointers answers
/// through `FunctionPointerTarget.Equals`, so that is where the two must stay apart.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFunctionPointerTargetIdentity =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        BaseClassTypes.ofCorelib corelib

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let loadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty
        }

    /// A uniquely-named instance method of the value type `System.Int32`, concretised afresh, so
    /// that two calls give records which agree only nominally.
    let private int32Method
        (state : IlMachineState)
        (methodName : string)
        : IlMachineState * MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let rawMethod =
            baseClassTypes.Int32.Methods
            |> List.filter (fun m -> m.Name = methodName)
            |> function
                | [ method ] -> method
                | methods -> failwith $"expected one System.Int32.%s{methodName}, found %d{methods.Length}"

        let state, method, _declaringType =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        state, method

    [<Test>]
    let ``a method's boxed and unboxed entry points are equal exactly when both flavour and method agree`` () : unit =
        // Every method twice over, from independent concretisations, so equality has to be
        // nominal rather than referential to pass.
        let methods =
            let state = baseState ()
            let state, hashA = int32Method state "GetHashCode"
            let state, typeCodeA = int32Method state "GetTypeCode"
            let state, hashB = int32Method state "GetHashCode"
            let _, typeCodeB = int32Method state "GetTypeCode"

            [
                "GetHashCode", hashA
                "GetTypeCode", typeCodeA
                "GetHashCode", hashB
                "GetTypeCode", typeCodeB
            ]

        let targets =
            [
                for name, method in methods do
                    yield ("Managed", name), FunctionPointerTarget.Managed method
                    yield ("UnboxingStub", name), FunctionPointerTarget.UnboxingStub method
            ]

        for leftKey, left in targets do
            for rightKey, right in targets do
                let expected = leftKey = rightKey

                (left = right, (leftKey, rightKey))
                |> shouldEqual (expected, (leftKey, rightKey))

                let viaCeq =
                    NativeIntSourceComparison.equalsForCli
                        PointerHashState.empty
                        (NativeIntSource.FunctionPointer left)
                        (NativeIntSource.FunctionPointer right)

                (viaCeq, (leftKey, rightKey)) |> shouldEqual (expected, (leftKey, rightKey))

                if expected then
                    hash left |> shouldEqual (hash right)
