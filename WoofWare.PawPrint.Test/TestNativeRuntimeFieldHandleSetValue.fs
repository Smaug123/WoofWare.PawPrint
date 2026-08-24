namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
open Microsoft.Extensions.Logging
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

    /// `Holder` deliberately has no static initialisers, so Roslyn emits no `.cctor` for it and
    /// `ensureTypeInitialised` completes in place. `LazyHolder` has one, so the same call suspends
    /// — which is the other half of the class-init contract, and is what the suspension test below
    /// pins.
    let private guestSource =
        """
public sealed class Holder
{
    public int Number;
    public static int Total;
}

public sealed class LazyHolder
{
    public static int Total = Compute();
    private static int Compute() { return 3; }
}

public sealed class RvaHolder
{
    // Roslyn lowers this initialiser into a `HasFieldRVA` blob field on
    // `<PrivateImplementationDetails>`, which is the only way to get one out of C#.
    public static readonly byte[] Data = { 1, 2, 3, 4, 5, 6, 7, 8 };
}
"""

    type private Fixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Corelib : DumpedAssembly
            GuestAssembly : DumpedAssembly
            RuntimeFieldHandleType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            QCallMethod : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            HolderType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            HolderTypeHandle : ConcreteTypeHandle
            Int32Handle : ConcreteTypeHandle
            State : IlMachineState
        }

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () ->
            failwith $"type %s{namespaceName}.%s{typeName} not found in %s{assembly.Name.Name}"
        )

    let private concretizeTypeInfo
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * ConcreteTypeHandle
        =
        let typeDefn =
            DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies typeInfo

        IlMachineState.concretizeType
            loggerFactory
            baseClassTypes
            state
            typeInfo.AssemblyFullName
            ImmutableArray.Empty
            ImmutableArray.Empty
            typeDefn

    /// Roslyn mangles the QCall stub's own name, so the entry point is the only stable handle
    /// on it.
    let private findQCallStub
        (entryPoint : string)
        (declaringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        declaringType.Methods
        |> List.filter (fun method ->
            match method.TryNativeImport with
            | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
            | None -> false
        )
        |> function
            | [ method ] -> method
            | [] -> failwith $"QCall entry point %s{entryPoint} not found on %s{declaringType.Name}"
            | methods -> failwith $"QCall entry point %s{entryPoint} was ambiguous: %d{methods.Length} matches"

    let private makeFixture () : Fixture =
        let image =
            Roslyn.compileAssembly
                "RuntimeFieldHandleSetValueTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ guestSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let guestAssembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty guestAssembly

            let state = initialState.WithLoadedAssembly corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        let runtimeFieldHandleType =
            requiredTopLevelType corelib "System" "RuntimeFieldHandle"

        // Concretizing the stub also concretizes its parameter types, which is what lets the
        // handler's active-pattern match on the signature succeed at dispatch time.
        let state, qCallMethod, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                (findQCallStub "RuntimeFieldHandle_SetValue" runtimeFieldHandleType)
                None
                corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        let holderType = requiredTopLevelType guestAssembly "" "Holder"

        let state, holderTypeHandle =
            concretizeTypeInfo loggerFactory baseClassTypes state holderType

        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Corelib = corelib
            GuestAssembly = guestAssembly
            RuntimeFieldHandleType = runtimeFieldHandleType
            QCallMethod = qCallMethod
            HolderType = holderType
            HolderTypeHandle = holderTypeHandle
            Int32Handle = int32Handle
            State = state
        }

    let private fieldNamed (fixture : Fixture) (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        fixture.HolderType.Fields |> List.find (fun f -> f.Name = name)

    /// The `IntPtr` a `FieldDesc*` argument is spelled as: the registry id of a freshly
    /// allocated handle for `fieldName` on `Holder`.
    let private fieldDescArgument
        (fixture : Fixture)
        (fieldName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let field = fieldNamed fixture fieldName

        let runtimeFieldHandle, state =
            IlMachineState.getOrAllocateField
                fixture.LoggerFactory
                fixture.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed fixture.HolderTypeHandle)
                field.Handle
                state

        let stubAddress =
            match runtimeFieldHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_ptr" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
            | other -> failwith $"expected RuntimeFieldHandle value type, got %O{other}"

        let id =
            FieldHandleRegistry.resolveFieldIdFromAddress stubAddress state.FieldHandles
            |> Option.defaultWith (fun () -> failwith "freshly allocated field handle was not in the registry")

        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr id)), state

    let private qCallTypeHandleValue
        (fixture : Fixture)
        (target : RuntimeTypeHandleTarget)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let qCallTypeHandleType =
            requiredTopLevelType fixture.Corelib "System.Runtime.CompilerServices" "QCallTypeHandle"

        let state, handle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state qCallTypeHandleType

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes handle

        match zero with
        | CliType.ValueType vt ->
            let handleField = IlMachineState.requiredOwnInstanceFieldId state handle "_handle"

            CliValueType.WithFieldSetById
                handleField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"QCallTypeHandle zero value was not a value type: %O{other}"

    /// A one-cell `object[]` holding `value`, plus an `ObjectHandleOnStack` whose `_ptr`
    /// addresses cell 0 — the shape `ObjectHandleOnStack.Create(ref x)` produces.
    let private objectHandleOnStack
        (fixture : Fixture)
        (value : CliType)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero objectHandle) (fun () -> value) 1 state

        let target = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let objectHandleOnStackType =
            requiredTopLevelType fixture.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let state, handle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state objectHandleOnStackType

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state fixture.BaseClassTypes handle

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
            |> CliType.ValueType,
            state
        | other -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

    /// A boxed `System.Int32` holding `value`, which is what the managed caller hands the QCall
    /// for an `int`-typed field. Built by the same `box` path the guest would have taken, so the
    /// contents' shape is the one `BoxedValue.contents` is the inverse of.
    let private boxedInt32 (fixture : Fixture) (value : int) (state : IlMachineState) : CliType * IlMachineState =
        let addr, state =
            UnaryMetadataObjectOps.boxValueType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.Int32Handle
                (EvalStackValue.Int32 (Int32Source.Verbatim value))
                state

        CliType.ObjectRef (Some addr), state

    let private int32OutCell (fixture : Fixture) (initial : int) (state : IlMachineState) =
        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero fixture.Int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 initial))
                1
                state

        let ptr =
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []))
            )

        arrayAddr, ptr, state

    /// Install the six-argument QCall frame on a fresh thread and run the handler.
    let private invoke
        (fixture : Fixture)
        (args : CliType list)
        (state : IlMachineState)
        : ThreadId * NativeHandlerResult
        =
        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.Corelib
                    fixture.QCallMethod
                    ImmutableArray.Empty
                    (ImmutableArray.CreateRange args)
                    None
            with
            | Ok methodState -> methodState
            | Error missing -> failwith $"Unexpected missing assembly references creating QCall frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState = Map.empty |> Map.add thread (ThreadState.New methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.Corelib
                TargetType = fixture.RuntimeFieldHandleType
            }

        match NativeRuntimeFieldHandle.tryExecuteQCall "RuntimeFieldHandle_SetValue" ctx with
        | Some result -> thread, result
        | None -> failwith "NativeRuntimeFieldHandle handler did not match RuntimeFieldHandle_SetValue"

    let private readInt32Cell (arr : ManagedHeapAddress) (state : IlMachineState) : int =
        match IlMachineState.getArrayValue arr 0 state with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwithf "expected an Int32 cell, got %A" other

    /// Arguments for setting `Holder.Number` on a freshly allocated instance, with the
    /// out-cell pre-poisoned so that "the handler never wrote it" is distinguishable from
    /// "the handler wrote 0".
    let private instanceSetArgs (fixture : Fixture) (incomingIsClassInitialized : int) (state : IlMachineState) =
        let state, contents =
            IlMachineState.buildInstanceStorage
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.HolderTypeHandle

        let instanceAddr, state =
            IlMachineState.allocateManagedObject fixture.HolderTypeHandle contents state

        let fieldDesc, state = fieldDescArgument fixture "Number" state

        let instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef (Some instanceAddr)) state

        let boxed, state = boxedInt32 fixture 42 state
        let valueHandle, state = objectHandleOnStack fixture boxed state

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

        let runtimeFieldHandle, state =
            IlMachineState.getOrAllocateField
                fixture.LoggerFactory
                fixture.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed lazyHolderHandle)
                field.Handle
                state

        let stubAddress =
            match runtimeFieldHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_ptr" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
            | other -> failwith $"expected RuntimeFieldHandle value type, got %O{other}"

        let fieldDescId =
            FieldHandleRegistry.resolveFieldIdFromAddress stubAddress state.FieldHandles
            |> Option.defaultWith (fun () -> failwith "freshly allocated field handle was not in the registry")

        let fieldDesc =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr fieldDescId))

        let instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        let boxed, state = boxedInt32 fixture 42 state
        let valueHandle, state = objectHandleOnStack fixture boxed state

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
        let state = fixture.State

        // `<PrivateImplementationDetails>` is unspeakable in C# but perfectly reachable through
        // reflection, so this shape is not hypothetical; find its blob field by the attribute
        // rather than by a name Roslyn is free to change.
        let rvaDeclaringType, rvaField =
            fixture.GuestAssembly.TypeDefs
            |> Seq.collect (fun kvp -> kvp.Value.Fields |> Seq.map (fun f -> kvp.Value, f))
            |> Seq.filter (fun (_, f) -> f.HasFieldRVA)
            |> Seq.tryHead
            |> Option.defaultWith (fun () ->
                failwith "the fixture's guest assembly was expected to contain an RVA-backed field"
            )

        let state, rvaTypeHandle =
            concretizeTypeInfo fixture.LoggerFactory fixture.BaseClassTypes state rvaDeclaringType

        let runtimeFieldHandle, state =
            IlMachineState.getOrAllocateField
                fixture.LoggerFactory
                fixture.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed rvaTypeHandle)
                rvaField.Handle
                state

        let stubAddress =
            match runtimeFieldHandle with
            | CliType.ValueType vt ->
                match CliValueType.DereferenceField "m_ptr" vt with
                | CliType.ObjectRef (Some addr) -> addr
                | other -> failwith $"expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
            | other -> failwith $"expected RuntimeFieldHandle value type, got %O{other}"

        let fieldDescId =
            FieldHandleRegistry.resolveFieldIdFromAddress stubAddress state.FieldHandles
            |> Option.defaultWith (fun () -> failwith "freshly allocated field handle was not in the registry")

        let fieldDesc =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr fieldDescId))

        let instanceHandle, state =
            objectHandleOnStack fixture (CliType.ObjectRef None) state

        let boxed, state = boxedInt32 fixture 1 state
        let valueHandle, state = objectHandleOnStack fixture boxed state

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

        let wrongHandle, state =
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

        let nullHandle, state = objectHandleOnStack fixture (CliType.ObjectRef None) state

        let args = args |> List.mapi (fun i a -> if i = 2 then nullHandle else a)

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                invoke fixture args state |> ignore<ThreadId * NativeHandlerResult>
            )

        exn.Message |> shouldContainText "null value for the value-typed field type"
