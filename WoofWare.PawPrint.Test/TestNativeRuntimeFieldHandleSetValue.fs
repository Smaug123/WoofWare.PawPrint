namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Direct coverage of the `RuntimeFieldHandle_SetValue` QCall.
///
/// The `pIsClassInitialized` out-parameter is the reason this fixture exists. Its only managed
/// consumer is `if (isClassInitialized) Initialize();` in `FieldAccessor`, and under PawPrint
/// `Initialize` immediately answers `IsFastPathSupported = false` and parks the accessor on the
/// slow path — so an implementation that never wrote the cell, or wrote the wrong value, would
/// pass every end-to-end guest. Asserting the cell here is the only way to kill that mutation;
/// `sourcesPure/ReflectionFieldSetValue.cs` and `ReflectionFieldSetValueInitOnly.cs` cover
/// everything the guest *can* see.
[<TestFixture>]
module TestNativeRuntimeFieldHandleSetValue =
    open NativeRuntimeFieldHandleFixture

    let private makeFixture () : Fixture = make "RuntimeFieldHandle_SetValue"

    /// Arguments for setting `Holder.Number` on a freshly allocated instance, with the
    /// out-cell pre-poisoned so that "the handler never wrote it" is distinguishable from
    /// "the handler wrote 0".
    let private instanceSetArgs (fixture : Fixture) (incomingIsClassInitialized : int) (state : IlMachineState) =
        let instanceAddr, state = allocateHolder fixture state

        let fieldDesc, state = fieldDescArgument fixture "Number" state

        let _, instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef (Some instanceAddr)) state

        let boxed, state = boxedInt32 fixture 42 state
        let _, valueHandle, state = objectHandleOnStack fixture boxed state

        let fieldType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fixture.Int32Handle) state

        let declaringType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fixture.HolderTypeHandle) state

        let outArr, outPtr, state = int32OutCell fixture incomingIsClassInitialized state

        instanceAddr,
        outArr,
        [
            fieldDesc
            instanceHandle
            valueHandle
            fieldType
            declaringType
            outPtr
        ],
        state

    [<Test>]
    let ``reports the declaring class as initialised once its initialiser has run`` () =
        let fixture = makeFixture ()

        // `Holder` has no initialiser to run, so `ensureTypeInitialised` completes it in place and
        // the handler must answer "initialised" — which is what makes managed `FieldAccessor` stop
        // asking on every subsequent set.
        let instanceAddr, outArr, args, state = instanceSetArgs fixture 0 fixture.State

        let _, result = invoke fixture args state

        let state =
            match result with
            | NativeHandlerResult.Completed (state, _) -> state
            | other -> failwithf "expected Completed, got %A" other

        readInt32Cell outArr state |> shouldEqual 1

        // ... and the write itself landed, so the assertion above is not passing on a handler that
        // did nothing else.
        let fieldId =
            FieldId.metadata fixture.HolderTypeHandle (fieldNamed fixture "Number").Handle "Number"

        match
            ManagedHeap.get instanceAddr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById fieldId
        with
        | CliType.Numeric (CliNumericType.Int32 i) -> i |> shouldEqual 42
        | other -> failwithf "expected Holder.Number to hold an Int32, got %A" other

    [<Test>]
    let ``writes the out-cell even when the caller already reported the class as initialised`` () =
        let fixture = makeFixture ()

        // `FieldAccessor`'s permanent slow-path arm passes `true`, meaning "do not bother running
        // the initialiser". CoreCLR leaves the cell alone in that case — its whole write sits
        // inside the `if (*pIsClassInitialized == FALSE)` block — and so must we, rather than
        // recomputing an answer from a `TypeInitTable` that has no entry for a type we were told
        // not to initialise.
        let _, outArr, args, state = instanceSetArgs fixture 1 fixture.State

        let _, result = invoke fixture args state

        let state =
            match result with
            | NativeHandlerResult.Completed (state, _) -> state
            | other -> failwithf "expected Completed, got %A" other

        readInt32Cell outArr state |> shouldEqual 1

    [<Test>]
    let ``suspends for the declaring class initialiser rather than storing first`` () =
        let fixture = makeFixture ()

        // `LazyHolder` has a `.cctor`, so `ensureTypeInitialised` pushes it as a frame. The
        // handler must hand that suspension straight back — it will be re-entered once the
        // initialiser returns — rather than storing the value now, which the initialiser would
        // then overwrite.
        let state = fixture.State
        let lazyHolderType = requiredTopLevelType fixture.GuestAssembly "" "LazyHolder"

        let state, lazyHolderHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state lazyHolderType

        let field = lazyHolderType.Fields |> List.find (fun f -> f.Name = "Total")

        let fieldDesc, state = fieldDescArgumentFor fixture lazyHolderHandle field state

        let _, instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        let boxed, state = boxedInt32 fixture 42 state
        let _, valueHandle, state = objectHandleOnStack fixture boxed state

        let fieldType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fixture.Int32Handle) state

        let declaringType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed lazyHolderHandle) state

        let outArr, outPtr, state = int32OutCell fixture 0 state

        let _, result =
            invoke
                fixture
                [
                    fieldDesc
                    instanceHandle
                    valueHandle
                    fieldType
                    declaringType
                    outPtr
                ]
                state

        let state =
            match result with
            | NativeHandlerResult.SuspendedForClassInit (state, _) -> state
            | other -> failwithf "expected SuspendedForClassInit, got %A" other

        // Nothing may have been written yet: neither the field nor the out-cell, since the
        // handler is going to run again from the top once the initialiser returns.
        readInt32Cell outArr state |> shouldEqual 0

        IlMachineState.getStatic
            (StaticOwner.forField (ThreadId 0) field)
            lazyHolderHandle
            (ComparableFieldDefinitionHandle.Make field.Handle)
            state
        |> shouldEqual None

    [<Test>]
    let ``refuses an RVA-backed static field`` () =
        let fixture = makeFixture ()
        let state, rvaTypeHandle, rvaField = rvaField fixture fixture.State

        let fieldDesc, state = fieldDescArgumentFor fixture rvaTypeHandle rvaField state

        let _, instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        let boxed, state = boxedInt32 fixture 1 state
        let _, valueHandle, state = objectHandleOnStack fixture boxed state

        let fieldType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fixture.Int32Handle) state

        let declaringType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed rvaTypeHandle) state

        let _, outPtr, state = int32OutCell fixture 1 state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke
                    fixture
                    [
                        fieldDesc
                        instanceHandle
                        valueHandle
                        fieldType
                        declaringType
                        outPtr
                    ]
                    state
                |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "RVA-backed static field"

    [<Test>]
    let ``refuses a boxed value whose type the field cannot accept`` () =
        let fixture = makeFixture ()

        // A `System.Object` box for an `int`-typed field: not the field's type, and not related to
        // it by the enum/underlying relaxation either. Managed `CheckValue` would have rejected
        // this before the QCall, so a refusal is right; what matters is that it is loud and names
        // both types rather than storing something wrong.
        let instanceAddr, _, args, state = instanceSetArgs fixture 0 fixture.State
        ignore<ManagedHeapAddress> instanceAddr

        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let state, wrongContents =
            IlMachineState.buildInstanceStorage fixture.LoggerFactory fixture.BaseClassTypes state objectHandle

        let wrongAddr, state =
            IlMachineState.allocateManagedObject objectHandle wrongContents state

        let _, wrongHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef (Some wrongAddr)) state

        let args = args |> List.mapi (fun i a -> if i = 2 then wrongHandle else a)

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "cannot store a value boxed as"

    [<Test>]
    let ``refuses a null value for a value-typed field`` () =
        let fixture = makeFixture ()

        // Unreachable from managed `FieldAccessor`, which boxes a default first; the refusal
        // exists so the arm cannot silently become a wrong store if that ever changes.
        let _, _, args, state = instanceSetArgs fixture 0 fixture.State

        let _, nullHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        let args = args |> List.mapi (fun i a -> if i = 2 then nullHandle else a)

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "null value for the value-typed field type"
