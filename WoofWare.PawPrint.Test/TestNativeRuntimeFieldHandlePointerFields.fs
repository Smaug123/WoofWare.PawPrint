namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `RuntimeFieldHandle_GetValue` and `RuntimeFieldHandle_SetValue` on fields of unmanaged pointer
/// and function-pointer type, driven directly.
///
/// `sourcesPure/ReflectionFieldPointer.cs` covers what a guest can see. This pins the cells
/// themselves: a guest observes a pointer's provenance only by dereferencing it, so a handler that
/// turned a managed pointer into some other pointer to the same storage would pass there. It also
/// covers the argument shapes that the managed caller never produces.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeRuntimeFieldHandlePointerFields =
    open NativeRuntimeFieldHandleFixture

    type private Fixtures =
        {
            Get : Fixture
            Set : Fixture
            PointerHolderHandle : ConcreteTypeHandle
            /// `PointerHolder.Ptr`, of type `int*`.
            PtrField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            PtrFieldType : ConcreteTypeHandle
            /// `PointerHolder.Fn`, of type `delegate*<int, int>`.
            FnField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            FnFieldType : ConcreteTypeHandle
            ReflectionPointerHandle : ConcreteTypeHandle
            /// An `int[]` whose elements the generated managed pointers address.
            Cells : ManagedHeapAddress
            /// Every handler call starts from this state, in which both native methods and every
            /// type above are concretised.
            State : IlMachineState
        }

    let private cellCount = 4

    let private makeFixtures () : Fixtures =
        let set = make (NativeEntry.QCall "RuntimeFieldHandle_SetValue")
        let get = retarget (NativeEntry.QCall "RuntimeFieldHandle_GetValue") set
        let state = get.State

        let pointerHolderType = requiredTopLevelType get.GuestAssembly "" "PointerHolder"

        let state, pointerHolderHandle =
            concretizeTypeInfo get.LoggerFactory get.BaseClassTypes state pointerHolderType

        let fieldAndType (name : string) (state : IlMachineState) =
            let field = pointerHolderType.Fields |> List.find (fun f -> f.Name = name)

            let state, handle =
                IlMachineState.concretizeType
                    get.LoggerFactory
                    get.BaseClassTypes
                    state
                    pointerHolderType.AssemblyFullName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    field.Signature

            field, handle, state

        let ptrField, ptrFieldType, state = fieldAndType "Ptr" state
        let fnField, fnFieldType, state = fieldAndType "Fn" state

        match ptrFieldType, fnFieldType with
        | ConcreteTypeHandle.Pointer _, ConcreteTypeHandle.FunctionPointer _ -> ()
        | other -> failwith $"expected PointerHolder's fields to be int* and a function pointer, got %O{other}"

        let pointerClassType =
            requiredTopLevelType get.Corelib "System.Reflection" "Pointer"

        let state, reflectionPointerHandle =
            concretizeTypeInfo get.LoggerFactory get.BaseClassTypes state pointerClassType

        let cells, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero get.Int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                cellCount
                state

        {
            Get = get
            Set = set
            PointerHolderHandle = pointerHolderHandle
            PtrField = ptrField
            PtrFieldType = ptrFieldType
            FnField = fnField
            FnFieldType = fnFieldType
            ReflectionPointerHandle = reflectionPointerHandle
            Cells = cells
            State = state
        }

    let private allocatePointerHolder (fixtures : Fixtures) (state : IlMachineState) =
        let state, contents =
            IlMachineState.buildInstanceStorage
                fixtures.Get.LoggerFactory
                fixtures.Get.BaseClassTypes
                state
                fixtures.PointerHolderHandle

        IlMachineState.allocateManagedObject fixtures.PointerHolderHandle contents state

    let private fieldId (fixtures : Fixtures) (field : FieldInfo<GenericParamFromMetadata, TypeDefn>) : FieldId =
        FieldId.metadata fixtures.PointerHolderHandle field.Handle field.Name

    let private readField
        (fixtures : Fixtures)
        (instance : ManagedHeapAddress)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : CliType
        =
        ManagedHeap.get instance state.ManagedHeap
        |> AllocatedNonArrayObject.DereferenceFieldById (fieldId fixtures field)

    let private writeField
        (fixtures : Fixtures)
        (instance : ManagedHeapAddress)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        { state with
            ManagedHeap = ManagedHeap.setFieldById instance (fieldId fixtures field) value state.ManagedHeap
        }

    let private completed (result : NativeHandlerResult) : IlMachineState =
        match result with
        | NativeHandlerResult.Completed (state, _) -> state
        | other -> failwithf "expected Completed, got %A" other

    /// Run `RuntimeFieldHandle_GetValue` on `field` of `instance`, typed `fieldType`, and return
    /// the object it answered.
    let private getValue
        (fixtures : Fixtures)
        (instance : ManagedHeapAddress)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldType : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress option * IlMachineState
        =
        let fixture = fixtures.Get

        let fieldDesc, state =
            fieldDescArgumentFor fixture fixtures.PointerHolderHandle field state

        let _, instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef (Some instance)) state

        let fieldTypeArg, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fieldType) state

        let declaringTypeArg, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fixtures.PointerHolderHandle) state

        let _, outPtr, state = int32OutCell fixture 1 state

        let resultArr, resultHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        let _, result =
            invoke
                fixture
                [
                    fieldDesc
                    instanceHandle
                    fieldTypeArg
                    declaringTypeArg
                    outPtr
                    resultHandle
                ]
                state

        let state = completed result
        readObjectCell resultArr state, state

    /// Run `RuntimeFieldHandle_SetValue` on `field` of `instance`, typed `fieldType`, passing
    /// `value` as the `object?` to store.
    let private setValue
        (fixtures : Fixtures)
        (instance : ManagedHeapAddress)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (fieldType : ConcreteTypeHandle)
        (value : ManagedHeapAddress option)
        (state : IlMachineState)
        : ThreadId * NativeHandlerResult
        =
        let fixture = fixtures.Set

        let fieldDesc, state =
            fieldDescArgumentFor fixture fixtures.PointerHolderHandle field state

        let _, instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef (Some instance)) state

        let _, valueHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef value) state

        let fieldTypeArg, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fieldType) state

        let declaringTypeArg, state =
            qCallTypeHandleValue fixture (RuntimeTypeHandleTarget.Closed fixtures.PointerHolderHandle) state

        let _, outPtr, state = int32OutCell fixture 1 state

        invoke
            fixture
            [
                fieldDesc
                instanceHandle
                valueHandle
                fieldTypeArg
                declaringTypeArg
                outPtr
            ]
            state

    /// A box of the `System.IntPtr` or `System.UIntPtr` named by `primitive`, holding `value`.
    let private boxNativeInt
        (fixtures : Fixtures)
        (primitive : BaseClassTypes<DumpedAssembly> -> TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (value : EvalStackValue)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes (primitive fixtures.Get.BaseClassTypes)

        Boxing.boxValueType fixtures.Get.LoggerFactory fixtures.Get.BaseClassTypes handle value state

    /// A `System.Reflection.Pointer` whose `_ptr` holds `ptr`, built field by field rather than by
    /// the code under test. `_ptrType` is left null: the SetValue QCall does not read it.
    let private reflectionPointer
        (fixtures : Fixtures)
        (ptr : CliRuntimePointer)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, contents =
            IlMachineState.buildInstanceStorage
                fixtures.Get.LoggerFactory
                fixtures.Get.BaseClassTypes
                state
                fixtures.ReflectionPointerHandle

        let addr, state =
            IlMachineState.allocateManagedObject fixtures.ReflectionPointerHandle contents state

        addr, IlMachineState.setOwnInstanceField addr "_ptr" (CliType.RuntimePointer ptr) state

    /// The `_ptr` and `_ptrType` of the `System.Reflection.Pointer` at `addr`, failing unless
    /// that is what `addr` holds.
    let private pointerFields
        (fixtures : Fixtures)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : CliType * CliType
        =
        let obj = ManagedHeap.get addr state.ManagedHeap
        obj.ConcreteType |> shouldEqual fixtures.ReflectionPointerHandle

        let field (name : string) : CliType =
            obj
            |> AllocatedNonArrayObject.DereferenceFieldById (
                IlMachineState.requiredOwnInstanceFieldId state obj.ConcreteType name
            )

        field "_ptr", field "_ptrType"

    /// The payload of the `System.IntPtr` box at `addr` as the evaluation stack would see it after
    /// an `unbox`, failing unless `addr` holds such a box.
    let private unboxIntPtr
        (fixtures : Fixtures)
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : EvalStackValue
        =
        let boxed = ManagedHeap.get addr state.ManagedHeap

        boxed.ConcreteType
        |> shouldEqual (
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixtures.Get.BaseClassTypes.IntPtr
        )

        BoxedValue.contents fixtures.Get.BaseClassTypes boxed.ConcreteType boxed.Contents state
        |> fst
        |> EvalStackValue.ofCliType

    /// The `RuntimeType` object for `ty`, which `getOrAllocateType` memoises.
    let private runtimeTypeOf
        (fixtures : Fixtures)
        (ty : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        IlMachineState.getOrAllocateType
            fixtures.Get.LoggerFactory
            fixtures.Get.BaseClassTypes
            (RuntimeTypeHandleTarget.Closed ty)
            state

    /// A pointer an `int*` field can hold: either bare bits, or a managed pointer to one of the
    /// fixture's cells, which is the provenance a guest's `&x` carries.
    [<RequireQualifiedAccess>]
    type private PointerValue =
        | Bits of int64
        | Cell of index : int

    let private toRuntimePointer (fixtures : Fixtures) (value : PointerValue) : CliRuntimePointer =
        match value with
        | PointerValue.Bits bits -> CliRuntimePointer.Verbatim bits
        | PointerValue.Cell index ->
            CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (fixtures.Cells, index), []))

    let private pointerValueGen : Gen<PointerValue> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map PointerValue.Bits
                Gen.choose (0, cellCount - 1) |> Gen.map PointerValue.Cell
            ]

    /// Each spelling of a pointer that the SetValue QCall accepts for a pointer field.
    [<RequireQualifiedAccess>]
    type private Wrapper =
        | IntPtr
        | UIntPtr
        | ReflectionPointer

    let private wrapperGen : Gen<Wrapper> =
        Gen.elements [ Wrapper.IntPtr ; Wrapper.UIntPtr ; Wrapper.ReflectionPointer ]

    let private wrap
        (fixtures : Fixtures)
        (wrapper : Wrapper)
        (ptr : CliRuntimePointer)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let asStackValue () =
            EvalStackValue.ofCliType (CliType.RuntimePointer ptr)

        match wrapper with
        | Wrapper.IntPtr -> boxNativeInt fixtures (fun b -> b.IntPtr) (asStackValue ()) state
        | Wrapper.UIntPtr -> boxNativeInt fixtures (fun b -> b.UIntPtr) (asStackValue ()) state
        | Wrapper.ReflectionPointer -> reflectionPointer fixtures ptr state

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 100

    [<Test>]
    let ``GetValue on a pointer field answers a Pointer recording the field's pointer type around the cell`` () =
        let fixtures = makeFixtures ()

        let property (value : PointerValue) : unit =
            let ptr = toRuntimePointer fixtures value
            let instance, state = allocatePointerHolder fixtures fixtures.State

            let state =
                writeField fixtures instance fixtures.PtrField (CliType.RuntimePointer ptr) state

            let result, state =
                getValue fixtures instance fixtures.PtrField fixtures.PtrFieldType state

            let resultAddr =
                result |> Option.defaultWith (fun () -> failwith "GetValue answered null")

            let ptrCell, ptrType = pointerFields fixtures resultAddr state
            // Exactly the cell, provenance and all.
            ptrCell |> shouldEqual (CliType.RuntimePointer ptr)

            // `int*` itself, not its pointee `int`: `RuntimeType.CheckValue` compares this with a
            // destination's pointer type when the `Pointer` is passed back in.
            let expectedType, _ = runtimeTypeOf fixtures fixtures.PtrFieldType state
            ptrType |> shouldEqual (CliType.ObjectRef (Some expectedType))

            // A read does not disturb the field.
            readField fixtures instance fixtures.PtrField state
            |> shouldEqual (CliType.RuntimePointer ptr)

        Check.One (config, Prop.forAll (Arb.fromGen pointerValueGen) property)

    [<Test>]
    let ``GetValue on a null pointer field answers a Pointer around null rather than null`` () =
        let fixtures = makeFixtures ()
        let instance, state = allocatePointerHolder fixtures fixtures.State
        let unset = readField fixtures instance fixtures.PtrField state

        let result, state =
            getValue fixtures instance fixtures.PtrField fixtures.PtrFieldType state

        let resultAddr =
            result |> Option.defaultWith (fun () -> failwith "GetValue answered null")

        let ptrCell, _ = pointerFields fixtures resultAddr state
        ptrCell |> shouldEqual unset

    [<Test>]
    let ``GetValue on a function-pointer field answers a boxed IntPtr around the cell`` () =
        let fixtures = makeFixtures ()

        let property (bits : int64) : unit =
            let cell =
                CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim bits))

            let instance, state = allocatePointerHolder fixtures fixtures.State
            let state = writeField fixtures instance fixtures.FnField cell state

            let result, state =
                getValue fixtures instance fixtures.FnField fixtures.FnFieldType state

            let resultAddr =
                result |> Option.defaultWith (fun () -> failwith "GetValue answered null")

            unboxIntPtr fixtures resultAddr state
            |> shouldEqual (EvalStackValue.ofCliType cell)

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``a pointer stored through SetValue in any accepted spelling reads back unchanged through GetValue`` () =
        let fixtures = makeFixtures ()

        let property (value : PointerValue, wrapper : Wrapper) : unit =
            let ptr = toRuntimePointer fixtures value
            let instance, state = allocatePointerHolder fixtures fixtures.State
            let wrapped, state = wrap fixtures wrapper ptr state

            let _, result =
                setValue fixtures instance fixtures.PtrField fixtures.PtrFieldType (Some wrapped) state

            let state = completed result

            readField fixtures instance fixtures.PtrField state
            |> shouldEqual (CliType.RuntimePointer ptr)

            let read, state =
                getValue fixtures instance fixtures.PtrField fixtures.PtrFieldType state

            let readAddr =
                read |> Option.defaultWith (fun () -> failwith "GetValue answered null")

            fst (pointerFields fixtures readAddr state)
            |> shouldEqual (CliType.RuntimePointer ptr)

        Check.One (config, Prop.forAll (Arb.fromGen (Gen.zip pointerValueGen wrapperGen)) property)

    [<Test>]
    let ``a function pointer stored through SetValue as an IntPtr reads back unchanged through GetValue`` () =
        let fixtures = makeFixtures ()

        let property (bits : int64) : unit =
            let cell =
                CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim bits))

            let instance, state = allocatePointerHolder fixtures fixtures.State

            let wrapped, state =
                boxNativeInt fixtures (fun b -> b.IntPtr) (EvalStackValue.ofCliType cell) state

            let _, result =
                setValue fixtures instance fixtures.FnField fixtures.FnFieldType (Some wrapped) state

            let state = completed result
            readField fixtures instance fixtures.FnField state |> shouldEqual cell

            let read, state =
                getValue fixtures instance fixtures.FnField fixtures.FnFieldType state

            let readAddr =
                read |> Option.defaultWith (fun () -> failwith "GetValue answered null")

            unboxIntPtr fixtures readAddr state
            |> shouldEqual (EvalStackValue.ofCliType cell)

        Check.One (config, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int64>) property)

    [<Test>]
    let ``SetValue with a null value stores the field type's null`` () =
        let fixtures = makeFixtures ()

        for field, fieldType in
            [
                fixtures.PtrField, fixtures.PtrFieldType
                fixtures.FnField, fixtures.FnFieldType
            ] do
            let instance, state = allocatePointerHolder fixtures fixtures.State
            let unset = readField fixtures instance field state

            // Something non-null first, so that a handler which left the field alone would fail.
            let wrapped, state =
                boxNativeInt
                    fixtures
                    (fun b -> b.IntPtr)
                    (EvalStackValue.NativeInt (NativeIntSource.Verbatim 1234L))
                    state

            let state =
                setValue fixtures instance field fieldType (Some wrapped) state
                |> snd
                |> completed

            readField fixtures instance field state |> shouldNotEqual unset

            let state =
                setValue fixtures instance field fieldType None state |> snd |> completed

            readField fixtures instance field state |> shouldEqual unset

    [<Test>]
    let ``SetValue refuses a Pointer for a function-pointer field`` () =
        let fixtures = makeFixtures ()
        let instance, state = allocatePointerHolder fixtures fixtures.State

        // Managed `CheckValue` refuses this before the QCall; CoreCLR's native arm would read the
        // `Pointer`'s own bytes as the function pointer.
        let wrapped, state =
            reflectionPointer fixtures (CliRuntimePointer.Verbatim 1234L) state

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                setValue fixtures instance fixtures.FnField fixtures.FnFieldType (Some wrapped) state
                |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message
        |> shouldContainText "System.Reflection.Pointer for the function-pointer field"

    [<Test>]
    let ``SetValue refuses a box of a type that is not pointer-width for a pointer field`` () =
        let fixtures = makeFixtures ()
        let instance, state = allocatePointerHolder fixtures fixtures.State
        let boxed, state = boxedInt32 fixtures.Get 7 state

        let wrapped =
            match boxed with
            | CliType.ObjectRef (Some addr) -> addr
            | other -> failwithf "expected a box, got %A" other

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                setValue fixtures instance fixtures.PtrField fixtures.PtrFieldType (Some wrapped) state
                |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "into the pointer field type"
