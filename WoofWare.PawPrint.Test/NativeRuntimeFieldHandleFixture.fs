namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open WoofWare.PawPrint

/// The machinery for driving the `RuntimeFieldHandle_GetValue` and `RuntimeFieldHandle_SetValue`
/// QCalls directly: a guest assembly with the field shapes the two fixtures need, and builders
/// for each argument spelling the QCalls take — a `FieldDesc*` as a registry id, a
/// `QCallTypeHandle`, an `ObjectHandleOnStack` over a one-cell `object[]`, and the four-byte cell
/// a `[MarshalAs(UnmanagedType.Bool)] ref bool` addresses.
module NativeRuntimeFieldHandleFixture =

    /// `Holder` deliberately has no static initialisers, so Roslyn emits no `.cctor` for it and
    /// `ensureTypeInitialised` completes in place. `LazyHolder` has one, so the same call suspends
    /// — which is the other half of the class-init contract, and is what the suspension tests
    /// pin.
    let guestSource =
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

public sealed unsafe class PointerHolder
{
    public int* Ptr;
}
"""

    type Fixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Corelib : DumpedAssembly
            GuestAssembly : DumpedAssembly
            RuntimeFieldHandleType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            /// The QCall entry point this fixture drives.
            EntryPoint : string
            QCallMethod : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
            HolderType : TypeInfo<GenericParamFromMetadata, TypeDefn>
            HolderTypeHandle : ConcreteTypeHandle
            Int32Handle : ConcreteTypeHandle
            State : IlMachineState
        }

    let requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () ->
            failwith $"type %s{namespaceName}.%s{typeName} not found in %s{assembly.Name.Name}"
        )

    let concretizeTypeInfo
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

    let make (entryPoint : string) : Fixture =
        let image =
            Roslyn.compileAssembly
                "RuntimeFieldHandleQCallTestAssembly"
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
                (findQCallStub entryPoint runtimeFieldHandleType)
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
            EntryPoint = entryPoint
            QCallMethod = qCallMethod
            HolderType = holderType
            HolderTypeHandle = holderTypeHandle
            Int32Handle = int32Handle
            State = state
        }

    let fieldNamed (fixture : Fixture) (name : string) : FieldInfo<GenericParamFromMetadata, TypeDefn> =
        fixture.HolderType.Fields |> List.find (fun f -> f.Name = name)

    /// The `IntPtr` a `FieldDesc*` argument is spelled as: the registry id of a freshly
    /// allocated handle for `field`, declared on the closed type `declaringTypeHandle`.
    let fieldDescArgumentFor
        (fixture : Fixture)
        (declaringTypeHandle : ConcreteTypeHandle)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let runtimeFieldHandle, state =
            IlMachineState.getOrAllocateField
                fixture.LoggerFactory
                fixture.BaseClassTypes
                (RuntimeTypeHandleTarget.Closed declaringTypeHandle)
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

    /// `fieldDescArgumentFor` on `Holder`'s field named `fieldName`.
    let fieldDescArgument (fixture : Fixture) (fieldName : string) (state : IlMachineState) : CliType * IlMachineState =
        fieldDescArgumentFor fixture fixture.HolderTypeHandle (fieldNamed fixture fieldName) state

    let qCallTypeHandleValue
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
    /// addresses cell 0 — the shape `ObjectHandleOnStack.Create(ref x)` produces. The array's
    /// address is returned so a test can read the cell back after the QCall has written it.
    let objectHandleOnStack
        (fixture : Fixture)
        (value : CliType)
        (state : IlMachineState)
        : ManagedHeapAddress * CliType * IlMachineState
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

            arrayAddr,
            CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
            |> CliType.ValueType,
            state
        | other -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

    /// The object reference cell 0 of the `object[]` behind an `ObjectHandleOnStack` holds.
    let readObjectCell (arr : ManagedHeapAddress) (state : IlMachineState) : ManagedHeapAddress option =
        match IlMachineState.getArrayValue arr 0 state with
        | CliType.ObjectRef addr -> addr
        | other -> failwithf "expected an object reference cell, got %A" other

    /// A boxed `System.Int32` holding `value`, which is what the managed caller hands the SetValue
    /// QCall for an `int`-typed field and what the GetValue QCall hands back for one. Built by the
    /// same `box` path the guest would have taken, so the contents' shape is the one
    /// `BoxedValue.contents` is the inverse of.
    let boxedInt32 (fixture : Fixture) (value : int) (state : IlMachineState) : CliType * IlMachineState =
        let addr, state =
            Boxing.boxValueType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.Int32Handle
                (EvalStackValue.Int32 (Int32Source.Verbatim value))
                state

        CliType.ObjectRef (Some addr), state

    /// What a box at `addr` holds, if it is a box of `System.Int32`.
    let unboxInt32 (fixture : Fixture) (addr : ManagedHeapAddress) (state : IlMachineState) : int =
        let boxed = ManagedHeap.get addr state.ManagedHeap

        if boxed.ConcreteType <> fixture.Int32Handle then
            failwithf "expected a box of System.Int32, got a box of %A" boxed.ConcreteType

        let contents, _ =
            BoxedValue.contents fixture.BaseClassTypes boxed.ConcreteType boxed.Contents state

        match contents with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwithf "expected the box to hold an Int32, got %A" other

    /// The four-byte cell a `[MarshalAs(UnmanagedType.Bool)] ref bool` argument addresses,
    /// holding `initial`, plus the pointer argument spelling it.
    let int32OutCell (fixture : Fixture) (initial : int) (state : IlMachineState) =
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

    let readInt32Cell (arr : ManagedHeapAddress) (state : IlMachineState) : int =
        match IlMachineState.getArrayValue arr 0 state with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwithf "expected an Int32 cell, got %A" other

    /// A freshly allocated `Holder` instance.
    let allocateHolder (fixture : Fixture) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let state, contents =
            IlMachineState.buildInstanceStorage
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.HolderTypeHandle

        IlMachineState.allocateManagedObject fixture.HolderTypeHandle contents state

    /// Install the QCall frame for `fixture.EntryPoint` on a fresh thread and run the handler.
    let invoke (fixture : Fixture) (args : CliType list) (state : IlMachineState) : ThreadId * NativeHandlerResult =
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

        match NativeRuntimeFieldHandle.tryExecuteQCall fixture.EntryPoint ctx with
        | Some result -> thread, result
        | None -> failwith $"NativeRuntimeFieldHandle handler did not match %s{fixture.EntryPoint}"

    /// The RVA-backed blob field Roslyn generates for `RvaHolder.Data`, with its declaring type
    /// concretized. `<PrivateImplementationDetails>` is unspeakable in C# but perfectly reachable
    /// through reflection, so the shape is not hypothetical; the field is found by its attribute
    /// rather than by a name Roslyn is free to change.
    let rvaField
        (fixture : Fixture)
        (state : IlMachineState)
        : IlMachineState * ConcreteTypeHandle * FieldInfo<GenericParamFromMetadata, TypeDefn>
        =
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

        state, rvaTypeHandle, rvaField
