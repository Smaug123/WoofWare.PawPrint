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
/// all it can see is *that* a non-null stub came back. Nothing yet reads the name or the signature
/// back out, so no guest can observe those, and this file pins them instead.
///
/// Read honestly, that makes most of these consistency checks rather than appeals to CoreCLR: they
/// assert that the QCall handler and the rest of PawPrint agree, not that either matches the real
/// runtime. One consequence is worth stating because no test here can catch it: these tests build
/// the six native arguments at the same indices the handler reads them from, so a handler and a
/// test that agreed on the *wrong* index for `name` versus `sig` would both pass, and the guest
/// case would too, since nothing downstream observes either value. The order was checked by hand
/// against the pinned managed signature (RuntimeHandles.cs:1773-1780). Whoever implements the
/// first consumer of the recorded name or signature should bring a differential guest assertion
/// with it.
///
/// The body-reading tests are the exception, and are better supported than that. The handler now
/// reads the `DynamicResolver`'s fields by name, and the guest case above drives a *real*
/// `DynamicILGenerator` through this same handler — so a field name that had drifted from CoreLib
/// would make that guest fail, differentially, rather than merely making these tests disagree with
/// the handler. What the guest cannot see is the decoded *result*, which is what this file pins.
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

    /// Drives the `RuntimeMethodHandle.GetMethodTable` FCall and hands back what it pushed.
    let private invokeGetMethodTable
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (internalHandle : CliType)
        (state : IlMachineState)
        : EvalStackValue
        =
        let baseClassTypes = prepared.BaseClassTypes

        let state, declaringTypeInfo, method =
            fcallMethod loggerFactory baseClassTypes runtimeMethodHandle "GetMethodTable" state

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
        | Some result -> failwith $"unexpected GetMethodTable execution result: %O{result}"
        | None -> failwith "GetMethodTable did not match any native handler"

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

    /// What a `DynamicResolver`'s fields will be made to say. Everything here is a field the
    /// resolver's constructor assigns and `DynamicMethodBody` reads back.
    type private ResolverBody =
        {
            /// `m_code`: the baked IL.
            Code : byte[]
            /// `m_localSignature`: a LocalVarSig blob (0x07, then a count, then that many types).
            LocalSignature : byte[]
            /// `DynamicMethod._initLocals`, which `GetCodeInfo` reports as `initLocals`.
            InitLocals : bool
            /// `m_exceptions`, the `__ExceptionInfo[]` the `ILGenerator` path fills in. `None`
            /// leaves the field null, which is what an `ILGenerator` that saw no `try` produces.
            ExceptionCount : int option
            /// `m_exceptionHeader`, non-null only on the `DynamicILInfo` path.
            ExceptionHeader : byte[] option
        }

    /// `ldarg.0; ldarg.0; add; ret` — the smallest body that computes something, and deliberately
    /// operand-free, since a body carrying a token cannot yet be stored.
    let private doublingBody =
        {
            Code = [| 0x02uy ; 0x02uy ; 0x58uy ; 0x2Auy |]
            // LocalVarSig with a count of zero: no locals.
            LocalSignature = [| 0x07uy ; 0x00uy |]
            InitLocals = true
            ExceptionCount = None
            ExceptionHeader = None
        }

    /// Allocate an instance of `typeInfo` with every instance field zeroed, as `newobj` would
    /// before running a constructor.
    let private allocateZeroed
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.Class))

        let state, allFields =
            IlMachineState.collectAllInstanceFields loggerFactory baseClassTypes state handle

        let fields =
            CliValueType.OfFields
                baseClassTypes
                state.ConcreteTypes
                handle
                (DeclaredTypeFacts.ofTypeInfo baseClassTypes state._LoadedAssemblies typeInfo)
                allFields

        IlMachineState.allocateManagedObject handle fields state

    /// A `System.Reflection.Emit.DynamicResolver` whose fields say what `body` says.
    ///
    /// Built field by field rather than by running `DynamicILGenerator`, because nothing yet binds
    /// or invokes a dynamic method: a guest that emitted a body would have no way to show what had
    /// been decoded from it. Constructing the resolver here is what makes the decoded result
    /// assertable, and it is also the only way to reach the refusals below, several of which no
    /// `ILGenerator` will produce.
    ///
    /// The field *names* are the part of this with an outside referent, and they are not pinned
    /// here: `sourcesImpure/DynamicMethodStubFromModule.cs` drives a real `DynamicILGenerator`
    /// through the same handler, so a name that had drifted from CoreLib
    /// (`DynamicILGenerator.cs`, `DynamicMethod.cs`) fails there, differentially against real .NET.
    let private allocateResolver
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (body : ResolverBody)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let emitType (name : string) =
            requiredTopLevelType baseClassTypes.Corelib "System.Reflection.Emit" name

        let methodAddr, state =
            allocateZeroed loggerFactory baseClassTypes (emitType "DynamicMethod") state

        let state =
            IlMachineState.setOwnInstanceField methodAddr "_initLocals" (CliType.ofBool body.InitLocals) state

        let codeAddr, state =
            NativeCall.allocateManagedByteArray baseClassTypes body.Code state

        let localSigAddr, state =
            NativeCall.allocateManagedByteArray baseClassTypes body.LocalSignature state

        let resolverAddr, state =
            allocateZeroed loggerFactory baseClassTypes (emitType "DynamicResolver") state

        let state =
            state
            |> IlMachineState.setOwnInstanceField resolverAddr "m_code" (CliType.ObjectRef (Some codeAddr))
            |> IlMachineState.setOwnInstanceField
                resolverAddr
                "m_localSignature"
                (CliType.ObjectRef (Some localSigAddr))
            |> IlMachineState.setOwnInstanceField resolverAddr "m_method" (CliType.ObjectRef (Some methodAddr))

        let state =
            match body.ExceptionHeader with
            | None -> state
            | Some header ->
                let headerAddr, state =
                    NativeCall.allocateManagedByteArray baseClassTypes header state

                IlMachineState.setOwnInstanceField
                    resolverAddr
                    "m_exceptionHeader"
                    (CliType.ObjectRef (Some headerAddr))
                    state

        let state =
            match body.ExceptionCount with
            | None -> state
            | Some count ->
                let exceptionInfo =
                    requiredTopLevelType baseClassTypes.Corelib "System.Reflection.Emit" "__ExceptionInfo"

                let state, elementHandle =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        baseClassTypes.Corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        (TypeDefn.FromDefinition (exceptionInfo.Identity, SignatureTypeKind.Class))

                let arrayAddr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero elementHandle)
                        (fun () -> CliType.ObjectRef None)
                        count
                        state

                IlMachineState.setOwnInstanceField
                    resolverAddr
                    "m_exceptions"
                    (CliType.ObjectRef (Some arrayAddr))
                    state

        resolverAddr, state

    /// Mints one dynamic method through the QCall and returns the stub the handler wrote, along
    /// with the resolver object it was given.
    let private mintOne
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (name : string)
        (signature : byte array)
        (body : ResolverBody)
        (state : IlMachineState)
        : ManagedHeapAddress * ManagedHeapAddress * IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let qCallModule, state =
            qCallModuleValue loggerFactory baseClassTypes state.EntryAssembly.FullName state

        let namePtr, state = utf8StringPointer baseClassTypes name state
        let sigPtr, state = bytePointer baseClassTypes signature state

        let resolverObj, state = allocateResolver loggerFactory baseClassTypes body state

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
            mintOne loggerFactory prepared "Probe" signatureWithInteriorNuls doublingBody state

        // The whole point. Before this QCall existed, every handle the registry could mint
        // answered `false` here, because `MethodHandle` had no case that could denote a
        // no-metadata method.
        invokeIsDynamicMethod loggerFactory prepared (internalHandleOfStub state stubAddress) state
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim 1))

    [<Test>]
    let ``the name and signature round-trip`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" signatureWithInteriorNuls doublingBody state

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
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] doublingBody state

        let _, definition = definitionBehindStub state stubAddress

        definition.GetScopeAssemblyFullName ()
        |> shouldEqual state.EntryAssembly.FullName

    [<Test>]
    let ``the resolver is recorded`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, resolverObj, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] doublingBody state

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
            mintOne loggerFactory prepared "Same" signatureWithInteriorNuls doublingBody state

        let secondStub, _, state =
            mintOne loggerFactory prepared "Same" signatureWithInteriorNuls doublingBody state

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
            mintOne loggerFactory prepared "Probe" signatureWithInteriorNuls doublingBody state

        let internalHandle = internalHandleOfStub state stubAddress

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                NativeRuntimeMethodHandle.resolveMetadataIdentityFromArg "test" state internalHandle
                |> ignore<MetadataMethodIdentity>
            )

        ex.Message |> shouldContainText "no MethodDef token to read"
        ex.Message |> shouldContainText "Probe"

    /// Mints with the given body and returns whatever the handler threw, so that each refusal can
    /// be checked to fire for its own reason rather than for whichever one happens to come first.
    let private mintExpectingFailure (body : ResolverBody) : string =
        let loggerFactory, prepared, state = loadFixture ()

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state
                |> ignore<ManagedHeapAddress * ManagedHeapAddress * IlMachineState>
            )

        ex.Message

    [<Test>]
    let ``the body is read back from the resolver`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] doublingBody state

        let _, definition = definitionBehindStub state stubAddress
        let instructions = definition.GetBody ()

        // The offsets matter as much as the opcodes: they are what branch targets and exception
        // region bounds are expressed in, so a decoder that produced the right instructions at the
        // wrong offsets would still misplace every jump.
        // Unwrapped to the nullary payload because `IlOp` has no equality: it carries a
        // `SourcedMetadataToken`, whose `AssemblyName` has none. The match is not a workaround —
        // a body that decoded to anything token-bearing here would fail this test rather than
        // silently compare unequal.
        instructions.Instructions
        |> List.map (fun (op, offset) ->
            match op with
            | IlOp.Nullary op -> op, offset
            | other -> failwith $"expected only nullary instructions, got %O{other} at IL_%04x{offset}"
        )
        |> shouldEqual
            [
                NullaryIlOp.LdArg0, 0
                NullaryIlOp.LdArg0, 1
                NullaryIlOp.Add, 2
                NullaryIlOp.Ret, 3
            ]

        instructions.Locations
        |> Map.toList
        |> List.map fst
        |> shouldEqual [ 0 ; 1 ; 2 ; 3 ]

        // A LocalVarSig with a count of zero decodes to no locals, which is a different fact from
        // "there was no signature to read": the latter is refused.
        instructions.LocalVars |> Option.map Seq.toList |> shouldEqual (Some [])

        instructions.ExceptionRegions |> Seq.toList |> shouldEqual []

    /// `initLocals` is not cosmetic: `MethodInstructions.LocalsInit` is what `localloc` reads to
    /// choose zero-initialised over uninitialised stack memory. But minting must not decide it, in
    /// either direction: `DynamicMethod.InitLocals` has a setter that never latches, and CoreCLR
    /// does not read it until the method's first JIT (`DynamicILGenerator.cs:729`, reached from
    /// `LCGMethodResolver::GetCodeInfo`). A mint that recorded the current value would capture one
    /// the guest is still entitled to change.
    ///
    /// Both `TestCase`s assert the *same* thing — that nothing was decided — precisely because the
    /// field's value at mint must make no difference at all.
    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``initLocals is not read when the method is minted`` (initLocals : bool) : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            { doublingBody with
                InitLocals = initLocals
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state

        let _, definition = definitionBehindStub state stubAddress
        definition.GetLatchedLocalsInit () |> shouldEqual None

    /// A well-formed `MethodDefSig` for `(int32) -> int32`, which is what `doublingBody` computes:
    /// default calling convention, one parameter, `ELEMENT_TYPE_I4` for the return and again for
    /// the parameter. The other tests here can hand the QCall arbitrary bytes because minting
    /// stores the blob without looking at it; anything that *executes* the method decodes it.
    let private doublingSignature = [| 0x00uy ; 0x01uy ; 0x08uy ; 0x08uy |]

    /// The `DynamicMethod` a resolver was built for: the object whose `_initLocals` a guest goes on
    /// mutating after the method has been minted, and which `readInitLocals` reaches through.
    let private methodBehindResolver (state : IlMachineState) (resolver : ManagedHeapAddress) : ManagedHeapAddress =
        match
            ManagedHeap.get resolver state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceField "m_method"
            |> CliType.unwrapPrimitiveLikeDeep
        with
        | CliType.ObjectRef (Some addr) -> addr
        | other -> failwith $"expected the resolver's m_method to be an object reference, got %O{other}"

    /// Assign `DynamicMethod.InitLocals`, as the guest's property setter does.
    let private setInitLocals (resolver : ManagedHeapAddress) (value : bool) (state : IlMachineState) : IlMachineState =
        IlMachineState.setOwnInstanceField
            (methodBehindResolver state resolver)
            "_initLocals"
            (CliType.ofBool value)
            state

    /// The `initLocals` a concretised dynamic method will run under.
    let private localsInitOf (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) : bool =
        MethodInfo.tryIlBody method
        |> Option.defaultWith (fun () -> failwith $"%s{method.Name} was concretised without an IL body")
        |> fun instructions -> instructions.LocalsInit

    /// Minted while the guest said `true`, executed after it changed its mind. CoreCLR reads
    /// `InitLocals` when it first compiles the method, not when the method is created, so the value
    /// in force is the later one.
    [<Test>]
    let ``initLocals is read at first execution, not at mint`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            { doublingBody with
                InitLocals = true
            }

        let stubAddress, resolver, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let handle, _ = definitionBehindStub state stubAddress

        let state = setInitLocals resolver false state

        let _, method =
            DynamicMethodExecution.concretize loggerFactory prepared.BaseClassTypes "test" handle state

        localsInitOf method |> shouldEqual false

    /// ...and never read again. `LCGMethodResolver::GetCodeInfo` computes `m_Options` only under
    /// `if (!m_Code)`, so the first compilation fixes the flag for the method's whole life; a guest
    /// that assigns `InitLocals` afterwards is not refused, it is simply ignored.
    [<Test>]
    let ``initLocals is latched by the first execution`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            { doublingBody with
                InitLocals = true
            }

        let stubAddress, resolver, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let handle, _ = definitionBehindStub state stubAddress

        let state, first =
            DynamicMethodExecution.concretize loggerFactory prepared.BaseClassTypes "test" handle state

        localsInitOf first |> shouldEqual true

        let state = setInitLocals resolver false state

        let _, second =
            DynamicMethodExecution.concretize loggerFactory prepared.BaseClassTypes "test" handle state

        localsInitOf second |> shouldEqual true

    /// The latch has to survive in the *state*, not merely in the method that was handed back. A
    /// build that computed the right flag and dropped the updated registry would satisfy both tests
    /// above on its first call and quietly re-read the guest's field on every later one.
    [<Test>]
    let ``the latch is written back into the returned state`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            { doublingBody with
                InitLocals = false
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let handle, _ = definitionBehindStub state stubAddress

        let state, _ =
            DynamicMethodExecution.concretize loggerFactory prepared.BaseClassTypes "test" handle state

        let _, definition = definitionBehindStub state stubAddress
        definition.GetLatchedLocalsInit () |> shouldEqual (Some false)

    /// The refusal the whole design turns on. A `DynamicScope` operand is a well-formed
    /// `MethodDef`/`TypeDef`/`String` token that names an unrelated *real* row, so a body carrying
    /// one must not be stored: decoded as-is it would execute against whatever happened to sit at
    /// that index in the scope assembly.
    ///
    /// `ldstr` and a metadata token are checked separately because they are separate `IlOp` cases
    /// reached through separate decoder paths, and a `carriesToken` that had lost either arm would
    /// still pass the other's test.
    [<Test>]
    let ``a body carrying a metadata token is refused`` () : unit =
        // ldnull; call 0x06000001; ret
        let body =
            { doublingBody with
                Code = [| 0x14uy ; 0x28uy ; 0x01uy ; 0x00uy ; 0x00uy ; 0x06uy ; 0x2Auy |]
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "token operand"
        message |> shouldContainText "DynamicScope"
        // Names the offending instruction, so a failing run says which one to look at.
        message |> shouldContainText "IL_0001"

    [<Test>]
    let ``a body carrying a string token is refused`` () : unit =
        // ldstr 0x70000001; pop; ret
        let body =
            { doublingBody with
                Code = [| 0x72uy ; 0x01uy ; 0x00uy ; 0x00uy ; 0x70uy ; 0x26uy ; 0x2Auy |]
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "token operand"
        message |> shouldContainText "IL_0000"

    /// A `catch` clause's type arrives as a `DynamicScope` index in `ClassTokenOrFilterOffset`,
    /// which `ExceptionRegion.Catch` has nowhere to put; so clauses are refused as a body, rather
    /// than being dropped, which would silently turn a guarded method into an unguarded one.
    [<Test>]
    let ``an exception region is refused`` () : unit =
        let body =
            { doublingBody with
                ExceptionCount = Some 1
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "exception region"
        message |> shouldContainText "DynamicScope"

    /// An empty `m_exceptions` array is not a clause and must not be refused: `ILGenerator`
    /// produces one for a method that opened no `try`, and refusing it would reject most bodies.
    [<Test>]
    let ``an empty exception array is not an exception region`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            { doublingBody with
                ExceptionCount = Some 0
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state

        let _, definition = definitionBehindStub state stubAddress
        (definition.GetBody ()).ExceptionRegions |> Seq.toList |> shouldEqual []

    /// The two EH sources are genuinely different: `DynamicILInfo` supplies a fat/thin blob in
    /// `m_exceptionHeader` and leaves `m_exceptions` null, so an implementation that looked only at
    /// `m_exceptions` would see nothing and silently lose every clause. Refuse by name instead.
    [<Test>]
    let ``a DynamicILInfo resolver is refused`` () : unit =
        let body =
            { doublingBody with
                ExceptionHeader = Some [| 0x01uy ; 0x0Cuy ; 0x00uy ; 0x00uy |]
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "DynamicILInfo"
        message |> shouldContainText "m_exceptionHeader"

    /// The QCall's `resolver` parameter is a bare `ObjectHandleOnStack`, so nothing upstream has
    /// established what is in it. Reading `m_code` off some other type would fail obscurely, or --
    /// for a type that happened to have a field of that name -- succeed wrongly.
    [<Test>]
    let ``a resolver of the wrong type is refused`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()
        let baseClassTypes = prepared.BaseClassTypes

        let notAResolver, state =
            allocateZeroed
                loggerFactory
                baseClassTypes
                (requiredTopLevelType baseClassTypes.Corelib "System" "Version")
                state

        let qCallModule, state =
            qCallModuleValue loggerFactory baseClassTypes state.EntryAssembly.FullName state

        let namePtr, state = utf8StringPointer baseClassTypes "Probe" state
        let sigPtr, state = bytePointer baseClassTypes [| 0x01uy |] state

        let resolverHandle, _, state =
            objectHandleOnStackValue loggerFactory baseClassTypes (Some notAResolver) state

        let resultHandle, _, state =
            objectHandleOnStackValue loggerFactory baseClassTypes None state

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeQCall
                    loggerFactory
                    prepared
                    moduleHandle
                    "ModuleHandle_GetDynamicMethod"
                    [
                        qCallModule
                        namePtr
                        sigPtr
                        CliType.Numeric (CliNumericType.Int32 1)
                        resolverHandle
                        resultHandle
                    ]
                    state
                |> ignore<IlMachineState>
            )

        ex.Message |> shouldContainText "DynamicResolver"
        ex.Message |> shouldContainText "System.Version"

    /// Locals reach a dynamic method as a raw blob with no `StandaloneSignature` row to look up,
    /// which is why `LocalSignatureDecoding` exists. Two locals rather than one, so that a decoder
    /// that read the count but returned only the first would fail here.
    [<Test>]
    let ``locals are decoded from the raw signature blob`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        // LOCAL_SIG (0x07), two locals, ELEMENT_TYPE_I4 (0x08) then ELEMENT_TYPE_STRING (0x0E).
        let body =
            { doublingBody with
                LocalSignature = [| 0x07uy ; 0x02uy ; 0x08uy ; 0x0Euy |]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state

        let _, definition = definitionBehindStub state stubAddress

        (definition.GetBody ()).LocalVars
        |> Option.map Seq.toList
        |> shouldEqual (
            Some
                [
                    TypeDefn.PrimitiveType PrimitiveType.Int32
                    TypeDefn.PrimitiveType PrimitiveType.String
                ]
        )

    /// `SignatureHelper` always emits the calling-convention byte, so an empty blob is not a
    /// signature that says "no locals" — it is a signature that was never written. Decoding it as
    /// the former would silently drop every local of a method whose signature failed to arrive.
    [<Test>]
    let ``an empty local signature is refused`` () : unit =
        let body =
            { doublingBody with
                LocalSignature = [||]
            }

        mintExpectingFailure body |> shouldContainText "calling-convention byte"

    /// The count this decoder reads is only a local count if the blob really is a LocalVarSig; read
    /// off a FIELD or METHOD signature it would be some unrelated byte, and the decode that
    /// followed would be nonsense rather than an error.
    [<Test>]
    let ``a signature that is not a LocalVarSig is refused`` () : unit =
        // FIELD (0x06), then ELEMENT_TYPE_I4.
        let body =
            { doublingBody with
                LocalSignature = [| 0x06uy ; 0x08uy |]
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "LOCAL_SIG"
        message |> shouldContainText "Field"

    /// A body containing `localloc` used to be refused here, because `localloc` is the one
    /// instruction whose behaviour depends on `initLocals` and the flag was being snapshotted at
    /// mint. Now that the flag is read late and latched at first execution, there is nothing to
    /// refuse: such a body stores like any other, and the flag it will run under is not decided
    /// yet.
    [<Test>]
    let ``a body containing localloc is stored`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        // ldc.i4.1; localloc; pop; ret
        let body =
            { doublingBody with
                Code = [| 0x17uy ; 0xFEuy ; 0x0Fuy ; 0x26uy ; 0x2Auy |]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state

        let _, definition = definitionBehindStub state stubAddress

        // Unwrapped to the nullary payload for the same reason as `the body is read back from the
        // resolver`: `IlOp` carries a `SourcedMetadataToken` and so has no equality.
        (definition.GetBody ()).Instructions
        |> List.map (fun (op, offset) ->
            match op with
            | IlOp.Nullary op -> op, offset
            | other -> failwith $"expected only nullary instructions, got %O{other} at IL_%04x{offset}"
        )
        |> shouldEqual
            [
                NullaryIlOp.LdcI4_1, 0
                // Two bytes wide (0xFE 0x0F), which is why `pop` lands at 3 and not at 2.
                NullaryIlOp.Localloc, 1
                NullaryIlOp.Pop, 3
                NullaryIlOp.Ret, 4
            ]

        definition.GetLatchedLocalsInit () |> shouldEqual None

    /// The `<Module>` type of the entry assembly, concretised the way the handler concretises it.
    let private moduleTypeHandle
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        : ConcreteTypeHandle * IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let assembly =
            state.LoadedAssembly state.EntryAssembly
            |> Option.defaultWith (fun () -> failwith "entry assembly is not loaded")

        let moduleTypeInfo =
            assembly.TypeDefs.Values
            |> Seq.tryFind (fun (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>) ->
                typeInfo.Namespace = "" && typeInfo.Name = "<Module>"
            )
            |> Option.defaultWith (fun () -> failwith "entry assembly has no <Module> type")

        let stk =
            DumpedAssembly.signatureTypeKind baseClassTypes state._LoadedAssemblies moduleTypeInfo

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                moduleTypeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (moduleTypeInfo.Identity, stk))

        handle, state

    /// `RuntimeMethodHandle.GetMethodTable` is legal on a dynamic method — CoreCLR answers with the
    /// `DynamicMethodTable`'s synthetic MethodTable — and is what `Delegate.CreateDelegate` reaches
    /// through `GetDeclaringType` (Delegate.CoreCLR.cs:381-391) before binding.
    [<Test>]
    let ``GetMethodTable answers with the scope assembly's dynamic-methods class`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] doublingBody state

        let expected =
            RuntimeTypeHandleTarget.DynamicMethodsClass state.EntryAssembly.FullName

        invokeGetMethodTable loggerFactory prepared (internalHandleOfStub state stubAddress) state
        |> shouldEqual (EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr expected))

    /// Why the synthetic case has to exist at all, rather than the scope module's `<Module>` type
    /// standing in for it: `TypeHandleRegistry` keys guest `Type` object identity on
    /// `RuntimeTypeHandleTarget`, so under that design a global (`<Module>`-declared) method and a
    /// dynamic method in one assembly would come back as the *same* `Type`, where CoreCLR keeps them
    /// distinct.
    ///
    /// Read precisely: the test above is what rejects that design — mutating the handler to answer
    /// with `Closed <Module>` fails it, and was measured doing so. This one guards the *consequence*
    /// that made the design wrong, and so is the test that would fail if someone later reintroduced
    /// the collapse further down — by resolving the synthetic target to `<Module>` inside
    /// `getOrAllocateType`, say, which is the shape the mistake would most naturally take once the
    /// producer is correct.
    [<Test>]
    let ``the dynamic-methods class is distinct from the module type`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let moduleType, state = moduleTypeHandle loggerFactory prepared state

        let dynamicClass =
            RuntimeTypeHandleTarget.DynamicMethodsClass state.EntryAssembly.FullName

        let moduleTarget = RuntimeTypeHandleTarget.Closed moduleType

        // Distinct as targets...
        dynamicClass |> shouldNotEqual moduleTarget

        // ...and therefore distinct as guest `Type` objects, which is the consequence that matters:
        // this registry is what `RuntimeTypeHandle.GetRuntimeType` hands the guest.
        let dynamicType, state =
            IlMachineState.getOrAllocateType loggerFactory prepared.BaseClassTypes dynamicClass state

        let moduleTypeObj, _state =
            IlMachineState.getOrAllocateType loggerFactory prepared.BaseClassTypes moduleTarget state

        dynamicType |> shouldNotEqual moduleTypeObj

    /// CoreCLR's answer is a property of the scope *module* and of nothing else, so two dynamic
    /// methods minted against the same module share one declaring type however they differ
    /// otherwise.
    ///
    /// Read precisely, this pins invariance across the two things a mint can vary here — the name
    /// and the signature blob — and nothing stronger. The owner cannot be varied and so cannot be
    /// tested: `ModuleHandle_GetDynamicMethod` receives only a `QCall::ModuleHandle`, and
    /// `DynamicMethod._typeOwner` never crosses that boundary, so PawPrint's registry never learns
    /// it (`DynamicMethodDefinition` carries only the scope assembly). An owner-keyed answer is
    /// unrepresentable rather than merely untested, which is the reason it is safe to leave
    /// unasserted.
    [<Test>]
    let ``two dynamic methods in one module share a declaring type`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let firstStub, _, state =
            mintOne loggerFactory prepared "First" [| 0x01uy |] doublingBody state

        let secondStub, _, state =
            mintOne loggerFactory prepared "Second" signatureWithInteriorNuls doublingBody state

        // Distinct methods, as `two mints with identical inputs are distinct methods` pins...
        firstStub |> shouldNotEqual secondStub

        // ...but one declaring type.
        let first =
            invokeGetMethodTable loggerFactory prepared (internalHandleOfStub state firstStub) state

        let second =
            invokeGetMethodTable loggerFactory prepared (internalHandleOfStub state secondStub) state

        first |> shouldEqual second
