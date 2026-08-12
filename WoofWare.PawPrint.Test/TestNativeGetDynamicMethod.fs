namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// `ModuleHandle_GetDynamicMethod` (coreclr/vm/runtimehandles.cpp:2388), the QCall behind
/// `DynamicMethod.GetMethodDescriptor()`. It mints CoreCLR's no-metadata `DynamicMethodDesc` --
/// the thing `MethodDesc::IsNoMetadata()`, and so `RuntimeMethodHandle.IsDynamicMethod`, exists to
/// distinguish -- attaches the managed `DynamicResolver`, and writes back a
/// `RuntimeMethodInfoStub` naming it.
///
/// The outside oracle for this QCall is the guest-level case
/// (`sourcesImpure/DynamicMethodStubFromModule.cs`), which is differential against real .NET; but
/// all it can see is *that* a non-null stub came back. Nothing yet reads the name, the signature
/// or the resolver back out, so no guest can observe them, and this file pins them instead.
///
/// Read honestly, that makes these consistency checks rather than appeals to CoreCLR: they assert
/// that the QCall handler and the rest of PawPrint agree, not that either matches the real
/// runtime. One consequence is worth stating because no test here can catch it: these tests build
/// the six native arguments at the same indices the handler reads them from, so a handler and a
/// test that agreed on the *wrong* index for `name` versus `sig` would both pass, and the guest
/// case would too, since nothing downstream observes either value. The order was checked by hand
/// against the pinned managed signature (RuntimeHandles.cs:1773-1780). Whoever implements the
/// first consumer of the recorded name or signature should bring a differential guest assertion
/// with it.
[<TestFixture>]
module TestNativeGetDynamicMethod =

    /// A trivial guest; nothing here reads it, but `Program.prepare` needs an entry assembly and
    /// the QCall needs a loaded assembly to scope the minted method to.
    let private guestSource =
        """
public static class Entry
{
    public static int Main(string[] args)
    {
        return 0;
    }
}
"""

    let private prepareGuest
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (image : byte[])
        : Program.PreparedProgram
        =
        let dotnetRuntimes =
            DotnetRuntime.SelectForDll (typeof<RunResult>.Assembly.Location)
            |> ImmutableArray.CreateRange

        use peImage = new MemoryStream (image)

        match
            Program.prepare loggerFactory (Some "DynamicMethodTestGuest.cs") peImage (HostConfig.Default dotnetRuntimes)
        with
        | Program.ProgramStartResult.Ready prepared -> prepared
        | Program.ProgramStartResult.CompletedBeforeMain outcome ->
            failwith $"expected guest to be ready before Main, but got %O{outcome}"

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

    let private moduleHandle = ("System", "ModuleHandle")

    let private runtimeMethodHandle = ("System", "RuntimeMethodHandle")

    /// Locates the QCall entry point on its declaring type and concretizes it, so the handler sees
    /// the same `ExecutingMethod` signature the interpreter would have handed it.
    let private qCallMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringNamespace : string, declaringTypeName : string)
        (entryPoint : string)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            requiredTopLevelType baseClassTypes.Corelib declaringNamespace declaringTypeName

        let rawMethod =
            declaringType.Methods
            |> List.filter (fun method ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
                | None -> false
            )
            |> function
                | [ method ] -> method
                | [] -> failwith $"QCall entry point %s{entryPoint} not found on %s{declaringTypeName}"
                | methods ->
                    failwith
                        $"QCall entry point %s{entryPoint} was ambiguous on %s{declaringTypeName}: %d{methods.Length} matches"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, declaringType, method

    /// Locates a non-QCall (FCall / `InternalCall`) method by name on its declaring type. Used for
    /// `RuntimeMethodHandle.IsDynamicMethod`.
    let private fcallMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (declaringNamespace : string, declaringTypeName : string)
        (methodName : string)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            requiredTopLevelType baseClassTypes.Corelib declaringNamespace declaringTypeName

        let rawMethod =
            declaringType.Methods
            |> List.filter (fun method -> method.Name = methodName)
            |> function
                | [ method ] -> method
                | [] -> failwith $"method %s{methodName} not found on %s{declaringTypeName}"
                | methods -> failwith $"method %s{methodName} was ambiguous on %s{declaringTypeName}"

        let state, method, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                rawMethod
                None
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                state

        state, declaringType, method

    let private concreteValueTypeZero
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : ConcreteTypeHandle * CliType * IlMachineState
        =
        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle
        handle, zero, state

    /// `struct QCallModule { void* _ptr; IntPtr _module; }`. PawPrint models one module per
    /// assembly, so `_module` carries the assembly's full name.
    let private qCallModuleValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (assemblyFullName : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let qCallModuleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "QCallModule"

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state qCallModuleType

        match zero with
        | CliType.ValueType vt ->
            let moduleField = IlMachineState.requiredOwnInstanceFieldId state handle "_module"

            CliValueType.WithFieldSetById
                moduleField
                (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)))
                vt
            |> CliType.ValueType,
            state
        | other -> failwith $"QCallModule zero value was not a value type: %O{other}"

    /// `new ObjectHandleOnStack(ref local)`: a lone `void* _ptr` wrapping a byref to the caller's
    /// slot. The `object[1]` cell stands in for that slot, and starts null exactly as the C#
    /// wrapper's `IRuntimeMethodInfo? methodInfo = null` does.
    let private objectHandleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (contents : ManagedHeapAddress option)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * IlMachineState
        =
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef contents)
                1
                state

        let target = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let handleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let handle, zero, state =
            concreteValueTypeZero loggerFactory baseClassTypes state handleType

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            let value =
                CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
                |> CliType.ValueType

            value, target, state
        | other -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

    /// A `byte*` into a freshly allocated `byte[]`, standing in for what the `LibraryImport` stub
    /// hands the QCall: `Utf8StringMarshaller`'s buffer for `name`, and the pinned `byte[]` for
    /// `sig`.
    let private bytePointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (bytes : byte array)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let ptr, state = NativeCall.allocateBlobByteArray baseClassTypes bytes state
        CliType.RuntimePointer (CliRuntimePointer.Managed ptr), state

    /// A NUL-terminated `char*`, as `StringMarshalling.Utf8` produces for the `name` parameter.
    let private utf8StringPointer
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (value : string)
        (state : IlMachineState)
        : CliType * IlMachineState
        =
        let bytes = Array.append (System.Text.Encoding.UTF8.GetBytes value) [| 0uy |]
        bytePointer baseClassTypes bytes state

    /// Runs `entryPoint` against the entry thread. Deliberately through `NativeQCall.tryExecute`
    /// rather than straight at `NativeModuleHandle.tryExecuteQCall`: that is the path the
    /// interpreter takes, and it derives the entry point from the method's own import metadata, so
    /// this also fails if the handler exists but was never registered in the dispatch table --
    /// which is otherwise an entirely silent mistake.
    let private invokeQCall
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (declaringType : string * string)
        (entryPoint : string)
        (arguments : CliType list)
        (state : IlMachineState)
        : IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let state, declaringTypeInfo, method =
            qCallMethod loggerFactory baseClassTypes declaringType entryPoint state

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = method
                Arguments = ImmutableArray.CreateRange arguments
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = declaringTypeInfo
            }

        match NativeQCall.tryExecute ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> state
        | Some result -> failwith $"unexpected %s{entryPoint} execution result: %O{result}"
        | None -> failwith $"%s{entryPoint} QCall did not match, or is not registered in NativeQCall"

    /// Runs the `RuntimeMethodHandle.IsDynamicMethod` FCall over a `RuntimeMethodHandleInternal`.
    ///
    /// This is PawPrint's model of `MethodDesc::IsNoMetadata()` — the bit
    /// `RuntimeType.GetMethodBase` branches on first, precisely because such a method has no token
    /// to look up — so asserting through it pins that the QCall handler and that FCall handler
    /// agree. It is a cross-component consistency check, not an outside oracle; what makes it
    /// non-vacuous is that the polarity is pinned in both directions, by the pre-existing
    /// `IsDynamicMethod is false for a registry-minted handle` in `TestMethodHandleRegistry.fs`.
    let private invokeIsDynamicMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (internalHandle : CliType)
        (state : IlMachineState)
        : EvalStackValue
        =
        let baseClassTypes = prepared.BaseClassTypes

        let state, declaringTypeInfo, method =
            fcallMethod loggerFactory baseClassTypes runtimeMethodHandle "IsDynamicMethod" state

        let instruction =
            { state.ThreadState.[prepared.EntryThread].MethodState with
                ExecutingMethod = method
                Arguments = ImmutableArray.CreateRange [ internalHandle ]
            }

        let ctx : NativeCallContext =
            {
                LoggerFactory = loggerFactory
                BaseClassTypes = baseClassTypes
                Thread = prepared.EntryThread
                State = state
                Instruction = instruction
                TargetAssembly = baseClassTypes.Corelib
                TargetType = declaringTypeInfo
            }

        match NativeDispatch.tryExecute ctx with
        | Some (NativeHandlerResult.Completed (state, _)) ->
            IlMachineState.popEvalStack prepared.EntryThread state |> fst
        | Some result -> failwith $"unexpected IsDynamicMethod execution result: %O{result}"
        | None -> failwith "IsDynamicMethod did not match any native handler"

    /// A signature blob deliberately containing interior zero bytes. A real method signature does:
    /// `void` is `ELEMENT_TYPE_VOID` = 0x01, but `ELEMENT_TYPE_END` is 0x00 and padded blobs and
    /// nested type tokens routinely carry zeroes, so a handler that read `sig` with a
    /// null-terminated scan rather than the supplied length would truncate here.
    let private signatureWithInteriorNuls =
        [| 0x00uy ; 0x01uy ; 0x00uy ; 0x01uy ; 0x00uy |]

    /// The `RuntimeMethodHandleInternal` in the `m_value` field of the `RuntimeMethodInfoStub` the
    /// QCall wrote back — i.e. what the managed caller would hand on as an `IRuntimeMethodInfo`.
    let private internalHandleOfStub (state : IlMachineState) (stubAddress : ManagedHeapAddress) : CliType =
        let stub : AllocatedNonArrayObject = ManagedHeap.get stubAddress state.ManagedHeap

        let mValueField =
            IlMachineState.requiredOwnInstanceFieldId state stub.ConcreteType "m_value"

        CliValueType.DereferenceFieldById mValueField stub.Contents

    /// Reads back what the QCall recorded about the single dynamic method behind `stubAddress`.
    let private definitionBehindStub
        (state : IlMachineState)
        (stubAddress : ManagedHeapAddress)
        : DynamicMethodHandle * DynamicMethodDefinition
        =
        let registryId =
            internalHandleOfStub state stubAddress
            |> NativeCall.methodHandleIdOfRuntimeMethodHandleInternal "test"
            |> Option.defaultWith (fun () -> failwith "stub carried a null RuntimeMethodHandleInternal")

        match MethodHandleRegistry.resolveMethodFromId registryId state.MethodHandles with
        | Some (MethodHandle.FromDynamic handle) ->
            let definition =
                MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
                |> Option.defaultWith (fun () -> failwith $"%O{handle} had no recorded definition")

            handle, definition
        | other -> failwith $"registry id %d{registryId} did not resolve to a dynamic method: %O{other}"

    /// Mints one dynamic method through the QCall and returns the stub the handler wrote, along
    /// with the resolver object it was given.
    let private mintOne
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (name : string)
        (signature : byte array)
        (state : IlMachineState)
        : ManagedHeapAddress * ManagedHeapAddress * IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallModule, state =
            qCallModuleValue loggerFactory baseClassTypes state.EntryAssembly.FullName state

        let namePtr, state = utf8StringPointer baseClassTypes name state
        let sigPtr, state = bytePointer baseClassTypes signature state

        // Stands in for the `DynamicResolver`. The QCall records whatever object it is handed --
        // CoreCLR's `resolver.Get()` is an untyped OBJECTREF too -- so any heap object will do.
        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        let resolverObj, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                1
                state

        let resolverHandle, _, state =
            objectHandleOnStackValue loggerFactory baseClassTypes (Some resolverObj) state

        let resultHandle, resultSlot, state =
            objectHandleOnStackValue loggerFactory baseClassTypes None state

        let state =
            invokeQCall
                loggerFactory
                prepared
                moduleHandle
                "ModuleHandle_GetDynamicMethod"
                [
                    qCallModule
                    namePtr
                    sigPtr
                    CliType.Numeric (CliNumericType.Int32 signature.Length)
                    resolverHandle
                    resultHandle
                ]
                state

        let stubAddress =
            match
                IlMachineState.readManagedByref prepared.BaseClassTypes state resultSlot
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.ObjectRef (Some address) -> address
            | CliType.ObjectRef None -> failwith "ModuleHandle_GetDynamicMethod left the result handle null"
            | other -> failwith $"expected an object reference in the result handle, got %O{other}"

        stubAddress, resolverObj, state

    let private loadFixture () =
        let image = Roslyn.compile [ guestSource ]

        let _, loggerFactory = LoggerFactory.makeTestWithProperties []

        let prepared = prepareGuest loggerFactory image
        loggerFactory, prepared, prepared.State

    [<Test>]
    let ``the minted handle is a dynamic method`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" signatureWithInteriorNuls state

        // The whole point. Before this QCall existed, every handle the registry could mint
        // answered `false` here, because `MethodHandle` had no case that could denote a
        // no-metadata method.
        invokeIsDynamicMethod loggerFactory prepared (internalHandleOfStub state stubAddress) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

    [<Test>]
    let ``the name and signature round-trip`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" signatureWithInteriorNuls state

        let _, definition = definitionBehindStub state stubAddress

        definition.GetName () |> shouldEqual "Probe"

        // Counted, not terminated: reading to the first NUL would yield an empty array here.
        definition.GetSignature ()
        |> Seq.toArray
        |> shouldEqual signatureWithInteriorNuls

    [<Test>]
    let ``the scope is the module's assembly`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] state

        let _, definition = definitionBehindStub state stubAddress

        definition.GetScopeAssemblyFullName ()
        |> shouldEqual state.EntryAssembly.FullName

    [<Test>]
    let ``the resolver is recorded`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, resolverObj, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] state

        let _, definition = definitionBehindStub state stubAddress

        definition.GetResolver () |> shouldEqual (Some resolverObj)

    /// The property Option B of the design exists to get right. CoreCLR mints a fresh
    /// `DynamicMethodDesc` per call, so two `DynamicMethod`s agreeing on name, signature and
    /// module are still different methods; a registry that deduped them structurally would fuse
    /// two guest objects into one and make `dm1.CreateDelegate` and `dm2.CreateDelegate` run the
    /// same IL.
    [<Test>]
    let ``two mints with identical inputs are distinct methods`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let firstStub, _, state =
            mintOne loggerFactory prepared "Same" signatureWithInteriorNuls state

        let secondStub, _, state =
            mintOne loggerFactory prepared "Same" signatureWithInteriorNuls state

        firstStub |> shouldNotEqual secondStub

        let firstHandle, _ = definitionBehindStub state firstStub
        let secondHandle, _ = definitionBehindStub state secondStub

        firstHandle |> shouldNotEqual secondHandle

    /// A no-metadata method has no MethodDef token, so every native that reads one must refuse
    /// rather than fabricate. `resolveMetadataIdentityFromArg` is the single funnel they share.
    [<Test>]
    let ``a metadata query on a dynamic method fails loudly`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" signatureWithInteriorNuls state

        let internalHandle = internalHandleOfStub state stubAddress

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                NativeRuntimeMethodHandle.resolveMetadataIdentityFromArg "test" state internalHandle
                |> ignore<MetadataMethodIdentity>
            )

        ex.Message |> shouldContainText "no MethodDef token to read"
        ex.Message |> shouldContainText "Probe"
