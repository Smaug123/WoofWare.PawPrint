namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Direct coverage of the `RuntimeFieldHandle.GetStaticFieldForGenericType` InternalCall.
///
/// Its only managed caller, `RuntimeType.PopulateRtFields`, always hands it a handle that
/// `RuntimeTypeHandle.GetFields` minted against the very type it passes as the MethodTable, so
/// `sourcesPure/ReflectionFieldGetValueGenericStatic*.cs` exercise only the arm that answers. The
/// refusals are here because each is a shape that caller cannot produce, and each stands for an
/// assumption the answering arm depends on.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeRuntimeFieldHandleGetStaticFieldForGenericType =
    open NativeRuntimeFieldHandleFixture

    let private makeFixture () : Fixture =
        make (NativeEntry.InternalCall "GetStaticFieldForGenericType")

    let private genericHolder (fixture : Fixture) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        requiredTopLevelType fixture.GuestAssembly "" "GenericHolder`1"

    /// `GenericHolder<arg>`, concretized.
    let private closedGenericHolder
        (fixture : Fixture)
        (arg : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle
        =
        let argDefn =
            DumpedAssembly.typeInfoToTypeDefn' fixture.BaseClassTypes state._LoadedAssemblies arg

        let openDefn =
            TypeDefn.FromDefinition (
                (genericHolder fixture).Identity,
                System.Reflection.Metadata.SignatureTypeKind.Class
            )

        IlMachineState.concretizeType
            fixture.LoggerFactory
            fixture.BaseClassTypes
            state
            fixture.GuestAssembly.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            (TypeDefn.GenericInstantiation (openDefn, ImmutableArray.Create argDefn))

    let private genericHolderField (fixture : Fixture) (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        (genericHolder fixture).Fields |> List.find (fun f -> f.Name = name)

    let private methodTableArgument (target : ConcreteTypeHandle) : CliType =
        CliType.RuntimePointer (CliRuntimePointer.MethodTablePtr (RuntimeTypeHandleTarget.Closed target))

    let private invokeExpectingRefusal (fixture : Fixture) (args : CliType list) (state : IlMachineState) : string =
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message

    [<Test>]
    let ``answers the handle it was given when that handle is declared on the MethodTable`` () =
        let fixture = makeFixture ()

        let state, holderOfInt =
            closedGenericHolder fixture fixture.BaseClassTypes.Int32 fixture.State

        let handle, state =
            runtimeFieldHandleInternalFor
                fixture
                (RuntimeTypeHandleTarget.Closed holderOfInt)
                (genericHolderField fixture "Count")
                state

        let thread, result =
            invoke fixture [ handle ; methodTableArgument holderOfInt ] state

        let state =
            match result with
            | NativeHandlerResult.Completed (state, _) -> state
            | other -> failwithf "expected Completed, got %A" other

        let returned, _ = IlMachineState.popEvalStack thread state
        returned |> shouldEqual (EvalStackValue.ofCliType handle)

    [<Test>]
    let ``refuses a handle declared on a different instantiation from the MethodTable`` () =
        let fixture = makeFixture ()

        let state, holderOfInt =
            closedGenericHolder fixture fixture.BaseClassTypes.Int32 fixture.State

        let state, holderOfString =
            closedGenericHolder fixture fixture.BaseClassTypes.String state

        // PawPrint's handles are exact, so a handle for `GenericHolder<int>.Count` asked about
        // `GenericHolder<string>` names the other instantiation's storage; answering it would read
        // the wrong instantiation's statics.
        let handle, state =
            runtimeFieldHandleInternalFor
                fixture
                (RuntimeTypeHandleTarget.Closed holderOfInt)
                (genericHolderField fixture "Count")
                state

        invokeExpectingRefusal fixture [ handle ; methodTableArgument holderOfString ] state
        |> shouldContainText "expected to agree"

    [<Test>]
    let ``refuses a handle declared on the open generic definition`` () =
        let fixture = makeFixture ()

        let state, holderOfInt =
            closedGenericHolder fixture fixture.BaseClassTypes.Int32 fixture.State

        // The open definition is the closest PawPrint gets to CoreCLR's canonical MethodTable, which
        // is exactly the handle CoreCLR's version of this call exists to re-key.
        let handle, state =
            runtimeFieldHandleInternalFor
                fixture
                (RuntimeTypeHandleTarget.OpenGenericTypeDefinition (genericHolder fixture).Identity)
                (genericHolderField fixture "Count")
                state

        invokeExpectingRefusal fixture [ handle ; methodTableArgument holderOfInt ] state
        |> shouldContainText "expected to agree"

    [<Test>]
    let ``refuses an instance field`` () =
        let fixture = makeFixture ()

        let state, holderOfInt =
            closedGenericHolder fixture fixture.BaseClassTypes.Int32 fixture.State

        let handle, state =
            runtimeFieldHandleInternalFor
                fixture
                (RuntimeTypeHandleTarget.Closed holderOfInt)
                (genericHolderField fixture "Value")
                state

        invokeExpectingRefusal fixture [ handle ; methodTableArgument holderOfInt ] state
        |> shouldContainText "is not static"

    [<Test>]
    let ``refuses the null handle`` () =
        let fixture = makeFixture ()

        let state, holderOfInt =
            closedGenericHolder fixture fixture.BaseClassTypes.Int32 fixture.State

        let nullHandle = CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)

        invokeExpectingRefusal fixture [ nullHandle ; methodTableArgument holderOfInt ] state
        |> shouldContainText "null field handle"
