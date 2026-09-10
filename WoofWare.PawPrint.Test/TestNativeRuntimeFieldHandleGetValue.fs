namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Direct coverage of the `RuntimeFieldHandle_GetValue` QCall.
///
/// As for the SetValue fixture, the `pIsClassInitialized` out-parameter is the reason this exists:
/// its only managed consumer is `if (isClassInitialized) Initialize();` in `FieldAccessor.GetValue`,
/// and under PawPrint `Initialize` immediately parks the accessor on the slow path whatever the
/// cell says, so no guest can tell whether it was written. `sourcesPure/ReflectionFieldGetValue.cs`
/// covers everything the guest *can* see; the refusal arms are here because each is a shape the
/// managed caller cannot produce.
[<TestFixture>]
module TestNativeRuntimeFieldHandleGetValue =
    open NativeRuntimeFieldHandleFixture

    let private makeFixture () : Fixture = make "RuntimeFieldHandle_GetValue"

    /// The six QCall arguments for reading `field` of `declaringTypeHandle`, typed
    /// `fieldTypeHandle`, from `instance` (`None` for a static), with the out-cell holding
    /// `incomingIsClassInitialized` and the result slot holding null. Returns the out-cell's
    /// array and the result slot's array so both can be read back.
    let private getArgs
        (fixture : Fixture)
        (declaringTypeHandle : ConcreteTypeHandle)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldTypeHandle : ConcreteTypeHandle)
        (instance : ManagedHeapAddress option)
        (incomingIsClassInitialized : int)
        (state : IlMachineState)
        =
        let fieldDesc, state = fieldDescArgumentFor fixture declaringTypeHandle field state

        let _, instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef instance) state

        let fieldType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fieldTypeHandle) state

        let declaringType, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed declaringTypeHandle) state

        let outArr, outPtr, state = int32OutCell fixture incomingIsClassInitialized state

        let resultArr, resultHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        outArr,
        resultArr,
        [
            fieldDesc
            instanceHandle
            fieldType
            declaringType
            outPtr
            resultHandle
        ],
        state

    /// Arguments for reading `Holder.Number` from a freshly allocated instance whose `Number`
    /// holds 42, with the out-cell pre-set to `incomingIsClassInitialized`.
    let private instanceGetArgs (fixture : Fixture) (incomingIsClassInitialized : int) (state : IlMachineState) =
        let instanceAddr, state = allocateHolder fixture state
        let field = fieldNamed fixture "Number"
        let fieldId = FieldId.metadata fixture.HolderTypeHandle field.Handle "Number"

        let state =
            { state with
                ManagedHeap =
                    ManagedHeap.setFieldById
                        instanceAddr
                        fieldId
                        (CliType.Numeric (CliNumericType.Int32 42))
                        state.ManagedHeap
            }

        getArgs
            fixture
            fixture.HolderTypeHandle
            field
            fixture.Int32Handle
            (Some instanceAddr)
            incomingIsClassInitialized
            state

    let private completed (result : NativeHandlerResult) : IlMachineState =
        match result with
        | NativeHandlerResult.Completed (state, _) -> state
        | other -> failwithf "expected Completed, got %A" other

    [<Test>]
    let ``answers a fresh box of the cell and reports the declaring class as initialised`` () =
        let fixture = makeFixture ()

        // `Holder` has no initialiser to run, so `ensureTypeInitialised` completes it in place and
        // the handler must answer "initialised" — which is what makes managed `FieldAccessor` stop
        // asking on every subsequent read.
        let outArr, resultArr, args, state = instanceGetArgs fixture 0 fixture.State

        let _, result = invoke fixture args state
        let state = completed result

        readInt32Cell outArr state |> shouldEqual 1

        // ... and the read itself landed: the result slot holds a box of `System.Int32`, holding
        // the cell's 42.
        let boxAddr =
            readObjectCell resultArr state
            |> Option.defaultWith (fun () -> failwith "the result slot was left null")

        unboxInt32 fixture boxAddr state |> shouldEqual 42

    [<Test>]
    let ``leaves the out-cell alone when the caller already reported the class as initialised`` () =
        let fixture = makeFixture ()

        // `FieldAccessor`'s permanent slow-path arm passes `true`, meaning "do not bother running
        // the initialiser". CoreCLR leaves the cell alone in that case — its whole write sits
        // inside the `if (*pIsClassInitialized == FALSE)` block — and so must we, rather than
        // recomputing an answer from a `TypeInitTable` that has no entry for a type we were told
        // not to initialise.
        let outArr, resultArr, args, state = instanceGetArgs fixture 1 fixture.State

        let _, result = invoke fixture args state
        let state = completed result

        readInt32Cell outArr state |> shouldEqual 1

        let boxAddr =
            readObjectCell resultArr state
            |> Option.defaultWith (fun () -> failwith "the result slot was left null")

        unboxInt32 fixture boxAddr state |> shouldEqual 42

    [<Test>]
    let ``suspends for the declaring class initialiser rather than reading first`` () =
        let fixture = makeFixture ()

        // `LazyHolder` has a `.cctor`, so `ensureTypeInitialised` pushes it as a frame. The
        // handler must hand that suspension straight back — it will be re-entered once the
        // initialiser returns — rather than reading the slot now, which would answer the value
        // from before the initialiser ran.
        let state = fixture.State
        let lazyHolderType = requiredTopLevelType fixture.GuestAssembly "" "LazyHolder"

        let state, lazyHolderHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state lazyHolderType

        let field = lazyHolderType.Fields |> List.find (fun f -> f.Name = "Total")

        let outArr, resultArr, args, state =
            getArgs fixture lazyHolderHandle field fixture.Int32Handle None 0 state

        let _, result = invoke fixture args state

        let state =
            match result with
            | NativeHandlerResult.SuspendedForClassInit (state, _) -> state
            | other -> failwithf "expected SuspendedForClassInit, got %A" other

        // Nothing may have been written yet: neither the result slot nor the out-cell, since the
        // handler is going to run again from the top once the initialiser returns.
        readInt32Cell outArr state |> shouldEqual 0
        readObjectCell resultArr state |> shouldEqual None

    [<Test>]
    let ``answers the field type's zero for static storage nothing has written`` () =
        let fixture = makeFixture ()

        // `Holder.Total` has no initialiser and no `stsfld` has run, so `getStatic` has no entry
        // for it. That is the state `ldsfld` answers with the field type's zero, and so does this.
        let field = fieldNamed fixture "Total"

        let outArr, resultArr, args, state =
            getArgs fixture fixture.HolderTypeHandle field fixture.Int32Handle None 0 fixture.State

        let _, result = invoke fixture args state
        let state = completed result

        readInt32Cell outArr state |> shouldEqual 1

        let boxAddr =
            readObjectCell resultArr state
            |> Option.defaultWith (fun () -> failwith "the result slot was left null")

        unboxInt32 fixture boxAddr state |> shouldEqual 0

    [<Test>]
    let ``refuses an RVA-backed static field`` () =
        let fixture = makeFixture ()
        let state, rvaTypeHandle, rvaField = rvaField fixture fixture.State

        let _, _, args, state =
            getArgs fixture rvaTypeHandle rvaField fixture.Int32Handle None 1 state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "RVA-backed static field"

    [<Test>]
    let ``refuses a pointer-typed field`` () =
        let fixture = makeFixture ()
        let state = fixture.State

        let pointerHolderType =
            requiredTopLevelType fixture.GuestAssembly "" "PointerHolder"

        let state, pointerHolderHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state pointerHolderType

        let field = pointerHolderType.Fields |> List.find (fun f -> f.Name = "Ptr")

        let state, contents =
            IlMachineState.buildInstanceStorage fixture.LoggerFactory fixture.BaseClassTypes state pointerHolderHandle

        let instanceAddr, state =
            IlMachineState.allocateManagedObject pointerHolderHandle contents state

        // CoreCLR answers a `System.Reflection.Pointer` here; the refusal must name that rather
        // than hand back some box of the provenance-tracked cell.
        let _, _, args, state =
            getArgs
                fixture
                pointerHolderHandle
                field
                (ConcreteTypeHandle.Pointer fixture.Int32Handle)
                (Some instanceAddr)
                1
                state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "System.Reflection.Pointer"

    [<Test>]
    let ``refuses a null instance for an instance field`` () =
        let fixture = makeFixture ()

        // Unreachable from managed `FieldAccessor`, whose `VerifyTarget` throws `TargetException`
        // first; the refusal exists so the arm cannot silently become a read of nothing.
        let field = fieldNamed fixture "Number"

        let _, _, args, state =
            getArgs fixture fixture.HolderTypeHandle field fixture.Int32Handle None 1 fixture.State

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "null instance for the instance field"
