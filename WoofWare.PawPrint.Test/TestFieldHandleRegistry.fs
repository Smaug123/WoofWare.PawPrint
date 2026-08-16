namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Runtime.InteropServices
open FsUnitTyped
open Microsoft.Extensions.Logging
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestFieldHandleRegistry =

    let private fieldHandleSource =
        """
public static class HasField
{
    public static int Data = 1;
    private static int Other = 2;
    public const int Constant = 3;
}

public class MixedFieldOrder
{
    public static int StaticFirst = 5;
    public int InstanceSecond = 6;
    private static int StaticThird = 7;
    private int InstanceFourth = 8;
}

public class BaseWithField
{
    public int BaseData = 3;
}

public class DerivedWithField : BaseWithField
{
    public int DerivedData = 4;
}

public class GenericHolder<T>
{
    public T Value;
    public static int StaticCount;
}
"""

    type private FieldHandleFixture =
        {
            LoggerFactory : ILoggerFactory
            BaseClassTypes : BaseClassTypes<DumpedAssembly>
            Assembly : DumpedAssembly
            Field : FieldInfo<GenericParamFromMetadata, TypeDefn>
            OtherField : FieldInfo<GenericParamFromMetadata, TypeDefn>
            State : IlMachineState
        }

    let private makeFieldHandleFixture () : FieldHandleFixture =
        let image =
            Roslyn.compileAssembly
                "FieldHandleTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ fieldHandleSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let field =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "HasField" && field.Name = "Data")

        let otherField =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "HasField" && field.Name = "Other")

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib

        let state : IlMachineState =
            (state,
             [
                 baseClassTypes.Object
                 baseClassTypes.Byte
                 baseClassTypes.Int32
                 baseClassTypes.IntPtr
                 baseClassTypes.RuntimeType
                 baseClassTypes.RuntimeFieldHandle
                 baseClassTypes.RuntimeFieldHandleInternal
                 baseClassTypes.RuntimeFieldInfoStub
             ])
            ||> List.fold (fun state ty ->
                let typeDefn =
                    DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies ty

                let state, _ =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        baseClassTypes.Corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        typeDefn

                state
            )

        {
            LoggerFactory = loggerFactory
            BaseClassTypes = baseClassTypes
            Assembly = assembly
            Field = field
            OtherField = otherField
            State = state
        }

    let private getOrAllocateField
        (fixture : FieldHandleFixture)
        (field : FieldInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        // Derive the declaring target the same way callers in the runtime do: closed
        // instantiation for a non-generic declaring type, open generic definition when the
        // declaring type still has unbound parameters. Tests that need a specific target —
        // e.g. asserting OpenGenericTypeDefinition on a closed `G<int>` instantiation — should
        // call `IlMachineState.getOrAllocateField` directly.
        let declaringTarget, state =
            if field.DeclaringType.Generics.IsEmpty then
                let ctx : TypeConcretization.ConcretizationContext<_> =
                    {
                        ConcreteTypes = state.ConcreteTypes
                        LoadedAssemblies = state._LoadedAssemblies
                        BaseTypes = fixture.BaseClassTypes
                    }

                let handle, ctx =
                    TypeConcretization.concretizeTypeDefinition ctx field.DeclaringType.Identity

                let state =
                    { state with
                        ConcreteTypes = ctx.ConcreteTypes
                        _LoadedAssemblies = ctx.LoadedAssemblies
                    }

                RuntimeTypeHandleTarget.Closed handle, state
            else
                RuntimeTypeHandleTarget.OpenGenericTypeDefinition field.DeclaringType.Identity, state

        IlMachineState.getOrAllocateField
            fixture.LoggerFactory
            fixture.BaseClassTypes
            fixture.Assembly.Name
            declaringTarget
            field.Handle
            state

    let private runtimeFieldInfoStubAddress (fieldHandle : CliType) : ManagedHeapAddress =
        match fieldHandle with
        | CliType.ValueType vt ->
            match CliValueType.DereferenceField "m_ptr" vt with
            | CliType.ObjectRef (Some addr) -> addr
            | other -> failwith $"Expected RuntimeFieldHandle.m_ptr to be an object ref, got %O{other}"
        | other -> failwith $"Expected RuntimeFieldHandle value type, got %O{other}"

    let private runtimeFieldHandleInternalInRuntimeFieldInfoStub (allocated : AllocatedNonArrayObject) : CliType =
        match CliValueType.DereferenceField "m_fieldHandle" allocated.Contents with
        | CliType.ValueType _ as runtimeFieldHandleInternal -> runtimeFieldHandleInternal
        | other ->
            failwith $"Expected RuntimeFieldInfoStub.m_fieldHandle to be a RuntimeFieldHandleInternal, got %O{other}"

    let private fieldHandleIdInRuntimeFieldInfoStub (allocated : AllocatedNonArrayObject) : int64 =
        let runtimeFieldHandleInternal =
            runtimeFieldHandleInternalInRuntimeFieldInfoStub allocated

        NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal
            "fieldHandleIdInRuntimeFieldInfoStub"
            runtimeFieldHandleInternal
        |> Option.defaultWith (fun () -> failwith "Expected RuntimeFieldInfoStub.m_fieldHandle to be non-null")

    let private fieldHandleIdAtAddress (address : ManagedHeapAddress) (state : IlMachineState) : int64 =
        ManagedHeap.get address state.ManagedHeap |> fieldHandleIdInRuntimeFieldInfoStub

    let private allocatePlainObject
        (fixture : FieldHandleFixture)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let objectType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Object

        let contents =
            ([] : CliField list)
            |> SynthesisedLayoutKind.ofFields
                fixture.BaseClassTypes
                state.ConcreteTypes
                objectType
                Layout.Default
                CharSet.Ansi

        IlMachineState.allocateManagedObject objectType contents state

    [<Test>]
    let ``Field handle allocation stores RuntimeFieldInfoStub object`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let allocated = ManagedHeap.get runtimeFieldInfoStubAddr state.ManagedHeap

        let runtimeFieldInfoStubType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.RuntimeFieldInfoStub

        allocated.ConcreteType |> shouldEqual runtimeFieldInfoStubType

        let fieldHandleId = fieldHandleIdInRuntimeFieldInfoStub allocated

        let resolved =
            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{fieldHandleId}")

        resolved.GetAssemblyFullName () |> shouldEqual fixture.Assembly.Name.FullName
        resolved.GetFieldDefinitionHandle().Get |> shouldEqual fixture.Field.Handle

    [<Test>]
    let ``RuntimeFieldInfoStub address resolves to field handle id`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let fieldHandleId = fieldHandleIdAtAddress runtimeFieldInfoStubAddr state

        let resolvedId =
            FieldHandleRegistry.resolveFieldIdFromAddress runtimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddr}"
            )

        resolvedId |> shouldEqual fieldHandleId

        let resolvedFromAddress =
            FieldHandleRegistry.resolveFieldFromAddress runtimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddr}"
            )

        let resolvedFromId =
            FieldHandleRegistry.resolveFieldFromId resolvedId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{resolvedId}")

        resolvedFromId.GetAssemblyFullName ()
        |> shouldEqual (resolvedFromAddress.GetAssemblyFullName ())

        resolvedFromId.GetFieldDefinitionHandle().Get
        |> shouldEqual (resolvedFromAddress.GetFieldDefinitionHandle().Get)

    [<Test>]
    let ``Unknown or non-field-stub addresses do not resolve to field handle ids`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let _, state = getOrAllocateField fixture fixture.Field fixture.State

        FieldHandleRegistry.resolveFieldIdFromAddress
            (ManagedHeapAddress (HeapObserver.nextAddress state.ManagedHeap))
            state.FieldHandles
        |> shouldEqual None

        let objectAddress, state = allocatePlainObject fixture state

        FieldHandleRegistry.resolveFieldIdFromAddress objectAddress state.FieldHandles
        |> shouldEqual None

        FieldHandleRegistry.resolveFieldIdFromAddress
            (ManagedHeapAddress (HeapObserver.nextAddress state.ManagedHeap))
            state.FieldHandles
        |> shouldEqual None

    [<Test>]
    let ``Reallocating a field preserves its field-stub address and id`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let fieldHandleId = fieldHandleIdAtAddress runtimeFieldInfoStubAddr state

        let fieldHandleAgain, state = getOrAllocateField fixture fixture.Field state

        let runtimeFieldInfoStubAddrAgain = runtimeFieldInfoStubAddress fieldHandleAgain

        runtimeFieldInfoStubAddrAgain |> shouldEqual runtimeFieldInfoStubAddr

        let resolvedIdAgain =
            FieldHandleRegistry.resolveFieldIdFromAddress runtimeFieldInfoStubAddrAgain state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{runtimeFieldInfoStubAddrAgain}"
            )

        resolvedIdAgain |> shouldEqual fieldHandleId

    [<Test>]
    let ``Different fields resolve to different field-stub addresses and ids`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State

        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let fieldHandleId = fieldHandleIdAtAddress runtimeFieldInfoStubAddr state

        let otherFieldHandle, state = getOrAllocateField fixture fixture.OtherField state

        let otherRuntimeFieldInfoStubAddr = runtimeFieldInfoStubAddress otherFieldHandle

        otherRuntimeFieldInfoStubAddr |> shouldNotEqual runtimeFieldInfoStubAddr

        let otherFieldHandleId =
            FieldHandleRegistry.resolveFieldIdFromAddress otherRuntimeFieldInfoStubAddr state.FieldHandles
            |> Option.defaultWith (fun () ->
                failwith $"Could not resolve field handle address %O{otherRuntimeFieldInfoStubAddr}"
            )

        otherFieldHandleId |> shouldNotEqual fieldHandleId

        let otherResolved =
            FieldHandleRegistry.resolveFieldFromId otherFieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{otherFieldHandleId}")

        otherResolved.GetFieldDefinitionHandle().Get
        |> shouldEqual fixture.OtherField.Handle

        let originalResolved =
            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{fieldHandleId}")

        originalResolved.GetFieldDefinitionHandle().Get
        |> shouldEqual fixture.Field.Handle

    [<Test>]
    let ``Field handle on open generic declaring type records OpenGenericTypeDefinition`` () : unit =
        // The test helper supplies OpenGenericTypeDefinition for a still-open declaring type,
        // mirroring `typeof(Foo<>).GetField(...).FieldHandle` in CoreCLR. Mapping the typedef's
        // generic parameters onto TypeDefn.GenericTypeParameter placeholders and concretising
        // with empty typeGenerics raises IndexOutOfRangeException.
        let fixture = makeFieldHandleFixture ()

        let genericField =
            fixture.Assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "GenericHolder`1" && field.Name = "Value")

        let _, state = getOrAllocateField fixture genericField fixture.State

        let registeredHandle =
            state.FieldHandles
            |> FieldHandleRegistry.resolveFieldFromId 1L
            |> Option.defaultWith (fun () -> failwith "Expected a freshly allocated FieldHandle at id 1")

        registeredHandle.GetFieldDefinitionHandle().Get
        |> shouldEqual genericField.Handle

        match registeredHandle.GetDeclaringTypeHandle () with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            identity |> shouldEqual genericField.DeclaringType.Identity
        | other ->
            failwithf
                "Expected OpenGenericTypeDefinition for the open generic declaring type, got %A. The helper passes OpenGenericTypeDefinition for callers that observe the field via `typeof(Foo<>).GetField(...)`."
                other

    [<Test>]
    let ``Closed and open declaring targets for the same field allocate distinct handles`` () : unit =
        // CoreCLR distinguishes `typeof(Foo<int>).GetField(...).FieldHandle` from
        // `typeof(Foo<>).GetField(...).FieldHandle`: `FieldInfo.GetFieldFromHandle` rejects a
        // mismatched declaring `RuntimeTypeHandle` between the two. Mirror that here: feeding
        // the same FieldDefinitionHandle with two distinct `RuntimeTypeHandleTarget` values must
        // allocate two distinct `FieldHandle` ids.
        let fixture = makeFieldHandleFixture ()

        let genericField =
            fixture.Assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "GenericHolder`1" && field.Name = "Value")

        let openTarget =
            RuntimeTypeHandleTarget.OpenGenericTypeDefinition genericField.DeclaringType.Identity

        let openFieldHandle, state =
            IlMachineState.getOrAllocateField
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.Assembly.Name
                openTarget
                genericField.Handle
                fixture.State

        // Build `GenericHolder<int>` as a closed instantiation, mirroring what
        // `typeof(GenericHolder<int>)` would yield in the guest.
        let int32Defn =
            DumpedAssembly.typeInfoToTypeDefn'
                fixture.BaseClassTypes
                state._LoadedAssemblies
                fixture.BaseClassTypes.Int32

        let openHolderDefn =
            TypeDefn.FromDefinition (
                genericField.DeclaringType.Identity,
                System.Reflection.Metadata.SignatureTypeKind.Class
            )

        let closedHolderDefn =
            TypeDefn.GenericInstantiation (openHolderDefn, ImmutableArray.Create int32Defn)

        let state, closedHolderHandle =
            IlMachineState.concretizeType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                state
                fixture.Assembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                closedHolderDefn

        let closedTarget = RuntimeTypeHandleTarget.Closed closedHolderHandle

        let closedFieldHandle, state =
            IlMachineState.getOrAllocateField
                fixture.LoggerFactory
                fixture.BaseClassTypes
                fixture.Assembly.Name
                closedTarget
                genericField.Handle
                state

        let openAddr = runtimeFieldInfoStubAddress openFieldHandle
        let closedAddr = runtimeFieldInfoStubAddress closedFieldHandle

        closedAddr |> shouldNotEqual openAddr

        let openId = fieldHandleIdAtAddress openAddr state
        let closedId = fieldHandleIdAtAddress closedAddr state
        closedId |> shouldNotEqual openId

        let resolveOpen =
            FieldHandleRegistry.resolveFieldFromId openId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve open field handle id %d{openId}")

        let resolveClosed =
            FieldHandleRegistry.resolveFieldFromId closedId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve closed field handle id %d{closedId}")

        match resolveOpen.GetDeclaringTypeHandle () with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            identity |> shouldEqual genericField.DeclaringType.Identity
        | other -> failwithf "Expected OpenGenericTypeDefinition, got %A" other

        match resolveClosed.GetDeclaringTypeHandle () with
        | RuntimeTypeHandleTarget.Closed handle -> handle |> shouldEqual closedHolderHandle
        | other -> failwithf "Expected Closed concrete handle, got %A" other

    [<Test>]
    let ``Field handle on open generic declaring type is stable across allocations`` () : unit =
        // Calling getOrAllocateField twice with the same declaring target must return the same
        // FieldHandle id (and the same RuntimeFieldInfoStub address). This guarantees that
        // repeated `typeof(Foo<>).GetField(...).FieldHandle` lookups in the guest are equal, and
        // that separate calls into the same QCall path share allocations.
        let fixture = makeFieldHandleFixture ()

        let genericField =
            fixture.Assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "GenericHolder`1" && field.Name = "StaticCount")

        let firstHandle, state = getOrAllocateField fixture genericField fixture.State
        let secondHandle, state = getOrAllocateField fixture genericField state

        let firstAddr = runtimeFieldInfoStubAddress firstHandle
        let secondAddr = runtimeFieldInfoStubAddress secondHandle

        secondAddr |> shouldEqual firstAddr

        fieldHandleIdAtAddress secondAddr state
        |> shouldEqual (fieldHandleIdAtAddress firstAddr state)

    let private requiredTopLevelType
        (assembly : DumpedAssembly)
        (namespaceName : string)
        (typeName : string)
        : TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        assembly.TryGetTopLevelTypeDef namespaceName typeName
        |> Option.defaultWith (fun () -> failwith $"type %s{namespaceName}.%s{typeName} not found")

    let private runtimeTypeHandleGetFieldsMethod
        (fixture : FieldHandleFixture)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let runtimeTypeHandleType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System" "RuntimeTypeHandle"

        // .NET 10 routes this through the RuntimeTypeHandle_GetFields QCall: the public
        // wrapper has IL that pins its Span<IntPtr> argument and Conv_U's the resulting
        // byref into a ptr[intptr] before invoking the QCall stub (which takes three raw
        // pointers: MethodTable*, ptr[intptr], ptr[int32]). Find the stub by its NativeImport
        // entry point so the matcher in NativeRuntimeType.tryExecuteQCall picks it up.
        let rawMethod =
            runtimeTypeHandleType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import ->
                    import.ModuleName = "QCall"
                    && import.EntryPointName = "RuntimeTypeHandle_GetFields"
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith "QCall entry point RuntimeTypeHandle_GetFields not found on System.RuntimeTypeHandle"
                | methods ->
                    failwith
                        $"QCall entry point RuntimeTypeHandle_GetFields was ambiguous on System.RuntimeTypeHandle: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                fixture.LoggerFactory
                fixture.BaseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                fixture.BaseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, runtimeTypeHandleType, method

    let private runtimeFieldHandleGetAttributesMethod
        (fixture : FieldHandleFixture)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let runtimeFieldHandleType =
            requiredTopLevelType fixture.BaseClassTypes.Corelib "System" "RuntimeFieldHandle"

        let rawMethod =
            runtimeFieldHandleType.Methods
            |> List.filter (fun method -> method.Name = "GetAttributes" && (MethodInfo.arity method) = 1)
            |> function
                | [ method ] -> method
                | [] -> failwith "RuntimeFieldHandle.GetAttributes native method not found"
                | methods ->
                    failwith $"RuntimeFieldHandle.GetAttributes native method was ambiguous: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                fixture.LoggerFactory
                fixture.BaseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                fixture.BaseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, runtimeFieldHandleType, method

    let private readInt32Pointer
        (bct : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : int
        =
        match IlMachineState.readManagedByrefBytesAs bct state ptr (CliType.Numeric (CliNumericType.Int32 0)) with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwith $"Expected Int32 pointer read, got %O{other}"

    let private fieldHandleIdOfCliType (value : CliType) : int64 =
        NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal "fieldHandleIdOfCliType" value
        |> Option.defaultWith (fun () -> failwith $"Expected non-null RuntimeFieldHandleInternal value, got %O{value}")

    let private resolveFieldHandleName
        (state : IlMachineState)
        (fixture : FieldHandleFixture)
        (value : CliType)
        : string
        =
        let fieldHandleId = fieldHandleIdOfCliType value

        let fieldHandle =
            FieldHandleRegistry.resolveFieldFromId fieldHandleId state.FieldHandles
            |> Option.defaultWith (fun () -> failwith $"Could not resolve field handle id %d{fieldHandleId}")

        fixture.Assembly.Fields.[fieldHandle.GetFieldDefinitionHandle().Get].Name

    let private runtimeFieldHandleInternalValue
        (fixture : FieldHandleFixture)
        (state : IlMachineState)
        (handleValue : CliType)
        : CliType
        =
        let runtimeFieldHandleInternalType =
            AllConcreteTypes.getRequiredNonGenericHandle
                state.ConcreteTypes
                fixture.BaseClassTypes.RuntimeFieldHandleInternal

        let intPtrType =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.IntPtr

        let field =
            fixture.BaseClassTypes.RuntimeFieldHandleInternal.Fields |> List.exactlyOne

        if field.Name <> "m_handle" then
            failwith $"unexpected field name %s{field.Name} for BCL type RuntimeFieldHandleInternal"

        FieldIdentity.cliField runtimeFieldHandleInternalType field handleValue intPtrType
        |> List.singleton
        |> SynthesisedLayoutKind.ofFields
            fixture.BaseClassTypes
            state.ConcreteTypes
            runtimeFieldHandleInternalType
            Layout.Default
            CharSet.Ansi
        |> CliType.ValueType

    let private readNativeIntValueAtPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (index : int)
        (ptr : ManagedPointerSource)
        : CliType
        =
        let nativeIntSize =
            CliType.sizeOf (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))

        let ptr =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex), []) ->
                ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, baseIndex + index), [])
            | ManagedPointerSource.Byref (ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset), []) ->
                ManagedPointerSource.Byref (
                    ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset + (index * nativeIntSize)),
                    []
                )
            | _ when index = 0 -> ptr
            | _ -> failwith $"Expected native int buffer pointer, got %O{ptr}"

        IlMachineState.readManagedByref baseClassTypes state ptr

    let private invokeRuntimeTypeHandleGetFields
        (fixture : FieldHandleFixture)
        (declaringTypeHandle : ConcreteTypeHandle)
        (capacity : int)
        (state : IlMachineState)
        : bool * int * CliType list * IlMachineState
        =
        let thread = ThreadId 0

        let state, runtimeTypeHandleType, getFieldsMethod =
            runtimeTypeHandleGetFieldsMethod fixture state

        let intPtrHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.IntPtr

        let resultBufferAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero intPtrHandle)
                (fun () -> CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
                (max 1 capacity)
                state

        let resultBuffer =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (resultBufferAddr, 0), [])

        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes fixture.BaseClassTypes.Int32

        let countAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                1
                state

        let countPtr =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (countAddr, 0), [])

        let state =
            IlMachineState.setArrayValue countAddr (CliType.Numeric (CliNumericType.Int32 capacity)) 0 state

        // The QCall stub takes three raw pointers: `(ptr[MethodTable], ptr[intptr], ptr[int32])`.
        // The wrapper's IL pins its Span<IntPtr> via GetPinnableReference and Conv_U's the
        // resulting byref into a managed pointer; we mirror that by passing the buffer and
        // count addresses directly as managed pointers. Arg 0 is the MethodTable* — modelled
        // as a NativeInt MethodTablePtr because the .NET 10 wrapper resolves the RuntimeType
        // to a MethodTable before invoking the QCall.
        let methodTableArg =
            CliType.Numeric (
                CliNumericType.NativeInt (
                    NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed declaringTypeHandle)
                )
            )

        let methodArgs =
            ImmutableArray.CreateRange
                [
                    methodTableArg
                    CliType.RuntimePointer (CliRuntimePointer.Managed resultBuffer)
                    CliType.RuntimePointer (CliRuntimePointer.Managed countPtr)
                ]

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    getFieldsMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith
                    $"Unexpected missing assembly references creating RuntimeTypeHandle.GetFields frame: %O{missing}"

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.BaseClassTypes.Corelib
                TargetType = runtimeTypeHandleType
            }

        let state =
            match NativeRuntimeType.tryExecuteQCall "RuntimeTypeHandle_GetFields" ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected RuntimeTypeHandle_GetFields execution result: %O{result}"
            | None -> failwith "RuntimeTypeHandle_GetFields did not match"

        let returnValue, state = IlMachineState.popEvalStack thread state

        // Interop.BOOL.TRUE / FALSE — represented as Int32 1 / 0.
        let success =
            match returnValue with
            | EvalStackValue.Int32 (Int32Source.Verbatim 0) -> false
            | EvalStackValue.Int32 (Int32Source.Verbatim 1) -> true
            | other -> failwith $"Expected RuntimeTypeHandle_GetFields Interop.BOOL result, got %O{other}"

        let count = readInt32Pointer fixture.BaseClassTypes state countPtr

        let valuesToRead = if success then count else max 1 capacity

        let fieldHandleValues =
            [ 0 .. valuesToRead - 1 ]
            |> List.map (fun index -> readNativeIntValueAtPointer fixture.BaseClassTypes state index resultBuffer)

        success, count, fieldHandleValues, state

    let private invokeRuntimeFieldHandleGetAttributes
        (fixture : FieldHandleFixture)
        (fieldHandleInternal : CliType)
        (state : IlMachineState)
        : EvalStackValue * IlMachineState
        =
        let state, runtimeFieldHandleType, getAttributesMethod =
            runtimeFieldHandleGetAttributesMethod fixture state

        let methodArgs = ImmutableArray.Create fieldHandleInternal

        let methodState =
            match
                MethodState.Empty
                    state.ConcreteTypes
                    fixture.BaseClassTypes
                    state._LoadedAssemblies
                    fixture.BaseClassTypes.Corelib
                    getAttributesMethod
                    ImmutableArray.Empty
                    methodArgs
                    None
            with
            | Ok methodState -> methodState
            | Error missing ->
                failwith
                    $"Unexpected missing assembly references creating RuntimeFieldHandle.GetAttributes frame: %O{missing}"

        let thread = ThreadId 0

        let state =
            { state with
                ThreadState =
                    Map.empty
                    |> Map.add thread (ThreadState.New (CpuId 0) (OsThreadId 1u) methodState)
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = fixture.LoggerFactory
                BaseClassTypes = fixture.BaseClassTypes
                Thread = thread
                State = state
                Instruction = state.ThreadState.[thread].MethodState
                TargetAssembly = fixture.BaseClassTypes.Corelib
                TargetType = runtimeFieldHandleType
            }

        let state =
            match NativeRuntimeFieldHandle.tryExecute ctx with
            | Some (NativeHandlerResult.Completed (state, _)) -> state
            | Some result -> failwith $"unexpected RuntimeFieldHandle.GetAttributes execution result: %O{result}"
            | None -> failwith "RuntimeFieldHandle.GetAttributes did not match"

        IlMachineState.popEvalStack thread state

    [<Test>]
    let ``RuntimeFieldHandle GetAttributes returns metadata attributes`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let fieldHandle, state = getOrAllocateField fixture fixture.Field fixture.State
        let runtimeFieldInfoStubAddr = runtimeFieldInfoStubAddress fieldHandle
        let allocated = ManagedHeap.get runtimeFieldInfoStubAddr state.ManagedHeap
        let fieldHandleInternal = runtimeFieldHandleInternalInRuntimeFieldInfoStub allocated

        let returnValue, _ =
            invokeRuntimeFieldHandleGetAttributes fixture fieldHandleInternal state

        returnValue
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim (int fixture.Field.Attributes)))

    [<Test>]
    let ``RuntimeTypeHandle GetFields writes field handle ids into caller buffer`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let declaringType =
            fixture.Field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringTypeHandle, state =
            IlMachineState.concretizeFieldDeclaringType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                declaringType
                fixture.State

        let success, count, fieldHandleValues, state =
            invokeRuntimeTypeHandleGetFields fixture declaringTypeHandle 2 state

        success |> shouldEqual true
        count |> shouldEqual 2

        let resolvedHandles =
            fieldHandleValues
            |> List.map (resolveFieldHandleName state fixture)
            |> Set.ofList

        resolvedHandles |> shouldEqual (Set.ofList [ "Data" ; "Other" ])

    [<Test>]
    let ``RuntimeTypeHandle GetFields leaves buffer untouched when capacity is too small`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let declaringType =
            fixture.Field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringTypeHandle, state =
            IlMachineState.concretizeFieldDeclaringType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                declaringType
                fixture.State

        let success, count, fieldHandleValues, _ =
            invokeRuntimeTypeHandleGetFields fixture declaringTypeHandle 1 state

        success |> shouldEqual false
        count |> shouldEqual 2

        fieldHandleValues
        |> shouldEqual [ CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) ]

    [<Test>]
    let ``RuntimeTypeHandle GetFields returns instance fields before static fields`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let mixedField =
            fixture.Assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "MixedFieldOrder" && field.Name = "StaticFirst")

        let declaringType =
            mixedField.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringTypeHandle, state =
            IlMachineState.concretizeFieldDeclaringType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                declaringType
                fixture.State

        let success, count, fieldHandleValues, state =
            invokeRuntimeTypeHandleGetFields fixture declaringTypeHandle 4 state

        success |> shouldEqual true
        count |> shouldEqual 4

        fieldHandleValues
        |> List.map (resolveFieldHandleName state fixture)
        |> shouldEqual [ "InstanceSecond" ; "InstanceFourth" ; "StaticFirst" ; "StaticThird" ]

    [<Test>]
    let ``RuntimeTypeHandle GetFields buffer values feed RuntimeFieldHandle GetAttributes`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let declaringType =
            fixture.Field.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringTypeHandle, state =
            IlMachineState.concretizeFieldDeclaringType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                declaringType
                fixture.State

        let success, count, fieldHandleValues, state =
            invokeRuntimeTypeHandleGetFields fixture declaringTypeHandle 2 state

        success |> shouldEqual true
        count |> shouldEqual 2
        fixture.OtherField.Attributes |> shouldNotEqual fixture.Field.Attributes

        let byName =
            fieldHandleValues
            |> List.map (fun value -> resolveFieldHandleName state fixture value, value)
            |> Map.ofList

        let otherFieldHandleInternal =
            runtimeFieldHandleInternalValue fixture state byName.["Other"]

        let returnValue, _ =
            invokeRuntimeFieldHandleGetAttributes fixture otherFieldHandleInternal state

        returnValue
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim (int fixture.OtherField.Attributes)))

    [<Test>]
    let ``RuntimeTypeHandle GetFields returns fields declared on requested type`` () : unit =
        let fixture = makeFieldHandleFixture ()

        let derivedField =
            fixture.Assembly.Fields.Values
            |> Seq.find (fun field -> field.DeclaringType.Name = "DerivedWithField" && field.Name = "DerivedData")

        let declaringType =
            derivedField.DeclaringType
            |> ConcreteType.mapGeneric (fun _index (param, _metadata) ->
                TypeDefn.GenericTypeParameter param.SequenceNumber
            )

        let declaringTypeHandle, state =
            IlMachineState.concretizeFieldDeclaringType
                fixture.LoggerFactory
                fixture.BaseClassTypes
                declaringType
                fixture.State

        let success, count, fieldHandleValues, state =
            invokeRuntimeTypeHandleGetFields fixture declaringTypeHandle 2 state

        success |> shouldEqual true
        count |> shouldEqual 1

        let resolvedHandles =
            fieldHandleValues
            |> List.map (resolveFieldHandleName state fixture)
            |> Set.ofList

        resolvedHandles |> shouldEqual (Set.ofList [ "DerivedData" ])

    [<Test>]
    let ``RVA field data can be read through managed byte pointer`` () : unit =
        let source =
            """
using System;

public static class HasRvaData
{
    public static int Length()
    {
        ReadOnlySpan<byte> bytes = new byte[] { 0x11, 0x22, 0x33, 0x44, 0x55 };
        return bytes.Length;
    }
}
"""

        let image =
            Roslyn.compileAssembly
                "RvaFieldTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let rvaField =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.RelativeVirtualAddress.IsSome)

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib

        let state, peByteRange =
            IlMachineState.peByteRangeForFieldRva
                loggerFactory
                baseClassTypes
                assembly
                rvaField
                ImmutableArray.Empty
                state

        let peByteRange =
            peByteRange
            |> Option.defaultWith (fun () ->
                failwith "Expected compiler-generated field to have a field-RVA PE byte range"
            )

        peByteRange.Size |> shouldEqual 5

        let state, ptr =
            IlMachineState.peByteRangePointer loggerFactory baseClassTypes peByteRange state

        let byteTemplate = CliType.Numeric (CliNumericType.UInt8 0uy)

        ManagedPointerSource.tryStableAddressBits ptr
        |> shouldEqual (Some (int64 peByteRange.RelativeVirtualAddress))

        IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr byteTemplate
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0x11uy))

        let offsetPtr =
            ptr |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset 4)

        ManagedPointerSource.tryStableAddressBits offsetPtr
        |> shouldEqual (Some (int64 peByteRange.RelativeVirtualAddress + 4L))

        offsetPtr
        |> fun ptr -> IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr byteTemplate
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 0x55uy))

        let outOfBoundsPtr =
            ptr
            |> ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset peByteRange.Size)

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IlMachineState.readManagedByrefBytesAs baseClassTypes state outOfBoundsPtr byteTemplate
                |> ignore
            )

        ex.Message.Contains "outside byte range size" |> shouldEqual true

    /// A hand-built assembly declaring `G<T>` with a static `T*` field carrying a field RVA.
    ///
    /// Roslyn cannot be asked for this shape: it only ever emits field RVAs into the non-generic
    /// `<PrivateImplementationDetails>`, as `__StaticArrayInitTypeSize=N` value types, so the
    /// signature never mentions a type parameter. The shape is legal all the same — CoreCLR's type
    /// loader rejects only RVA-plus-thread-static and RVA-plus-GC-references
    /// (methodtablebuilder.cpp:4129 and :4500) — and it is exactly the case that separates sizing
    /// the field by `FieldDesc::LoadSize`'s element-type dispatch from sizing it by concretising
    /// the signature: a `T*` is one pointer wide whatever `T` is, so CoreCLR never needs the
    /// instantiation to answer.
    let private genericPointerRvaImage () : byte[] =
        let metadata = MetadataBuilder ()

        metadata.AddModule (
            0,
            metadata.GetOrAddString "GenericPointerRva.dll",
            metadata.GetOrAddGuid (System.Guid "3f2a1b0c-9d8e-4c7b-6a5f-4e3d2c1b0a9f"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "GenericPointerRva",
            System.Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString "System.Private.CoreLib",
                System.Version (10, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        let fieldSignature = BlobBuilder ()
        BlobEncoder(fieldSignature).FieldSignature().Pointer().GenericTypeParameter 0

        let fieldHandle =
            metadata.AddFieldDefinition (
                FieldAttributes.Public
                ||| FieldAttributes.Static
                ||| FieldAttributes.HasFieldRVA,
                metadata.GetOrAddString "Data",
                metadata.GetOrAddBlob fieldSignature
            )

        // The offset is within the mapped field data below; the PE builder turns it into the
        // real RVA.
        metadata.AddFieldRelativeVirtualAddress (fieldHandle, 0)

        // The real runtime declines an image with no `<Module>` row, and it owns no fields, so its
        // field list points at the first field `G` owns.
        metadata.AddTypeDefinition (
            Unchecked.defaultof<TypeAttributes>,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            MetadataTokens.MethodDefinitionHandle 1
        )
        |> ignore<TypeDefinitionHandle>

        let genericType =
            metadata.AddTypeDefinition (
                TypeAttributes.Public
                ||| TypeAttributes.Class
                ||| TypeAttributes.BeforeFieldInit,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddString "G`1",
                (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
                MetadataTokens.FieldDefinitionHandle 1,
                MetadataTokens.MethodDefinitionHandle 1
            )

        metadata.AddGenericParameter (
            (TypeDefinitionHandle.op_Implicit genericType : EntityHandle),
            GenericParameterAttributes.None,
            metadata.GetOrAddString "T",
            0
        )
        |> ignore<GenericParameterHandle>

        // One pointer's worth of data for the field to name.
        let mappedFieldData = BlobBuilder ()
        mappedFieldData.WriteBytes (0x11uy, NATIVE_INT_SIZE)

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = (Characteristics.ExecutableImage ||| Characteristics.Dll)),
                MetadataRootBuilder metadata,
                BlobBuilder (),
                mappedFieldData,
                null,
                null,
                null,
                0,
                Unchecked.defaultof<MethodDefinitionHandle>,
                CorFlags.ILOnly
            )

        let peImage = BlobBuilder ()
        peBuilder.Serialize peImage |> ignore<BlobContentId>
        peImage.ToArray ()

    /// `FieldDesc::LoadSize` (field.cpp:655) switches on the field's normalised `CorElementType`
    /// and only loads a type for `ELEMENT_TYPE_VALUETYPE`; `ELEMENT_TYPE_PTR` is a table lookup.
    /// So an RVA field of type `T*` on an open generic type is one pointer wide, and CoreCLR
    /// answers that without any instantiation. Sizing it by concretising the signature cannot:
    /// it demands an argument for `T` that a caller holding only the open definition has not got.
    [<Test>]
    let ``RVA field whose signature mentions a type parameter is sized without an instantiation`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (genericPointerRvaImage ())

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let rvaField =
            assembly.Fields.Values
            |> Seq.find (fun field -> field.RelativeVirtualAddress.IsSome)

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            initialState.WithLoadedAssembly corelib

        // No generic arguments: the caller has the open definition, exactly as a `FieldHandle`
        // whose declaring type is an `OpenGenericTypeDefinition` does.
        let _, peByteRange =
            IlMachineState.peByteRangeForFieldRva
                loggerFactory
                baseClassTypes
                assembly
                rvaField
                ImmutableArray.Empty
                state

        peByteRange
        |> Option.map (fun range -> range.Size)
        |> shouldEqual (Some NATIVE_INT_SIZE)
