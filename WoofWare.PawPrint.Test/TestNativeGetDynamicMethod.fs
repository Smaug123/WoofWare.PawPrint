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

    /// One entry of a synthetic `DynamicScope`. The scope is `List&lt;object?&gt; m_tokens`, so an
    /// entry is any object at all; these are the shapes the tests need to put in one.
    type private ScopeEntry =
        /// A null slot. Index 0 of every real scope is one, because `DynamicScope` initialises
        /// `m_tokens` as `new List&lt;object?&gt; { null }`.
        | Null
        /// A string, as `GetTokenFor(string literal)` adds for `Emit(OpCodes.Ldstr, string)`.
        | Str of string
        /// A `byte[]`, as `GetTokenFor(byte[] signature)` adds. Index 1 of every real scope is one:
        /// `DynamicILGenerator`'s constructor puts the method's own signature there before any user
        /// code runs, and no instruction ever names it.
        | Blob of byte[]
        /// A boxed `System.RuntimeTypeHandle` naming <paramref name="target"/>, as
        /// `GetTokenFor(RuntimeTypeHandle)` adds for `Emit(OpCode, Type)`.
        | TypeHandle of target : RuntimeTypeHandleTarget
        /// A `System.Reflection.Emit.VarArgMethod`, as `GetMemberRefToken` adds for *every*
        /// `EmitCall`, vararg call site or not. Every field zeroed, `m_dynamicMethod` included,
        /// which is all decoding needs: decoding classifies by type and unwrapping happens when the
        /// instruction runs.
        | VarArgMethodObject
        /// A `System.Reflection.Emit.DynamicMethod`, as `GetTokenFor(DynamicMethod)` adds for
        /// `Emit(OpCode, MethodInfo)` when the operand is itself a dynamic method.
        ///
        /// Every field zeroed, `_methodHandle` included, which is what a target that has not been
        /// minted looks like — and is all decoding needs, since decoding classifies the entry by its
        /// *type* and reads nothing out of it. Which method it names is read when the instruction
        /// runs, by which time `GetMethodDescriptor` has assigned that field.
        | DynamicMethodObject

    /// One clause of an `__ExceptionInfo`, in the shape the parallel arrays hold it.
    type private ClauseSpec =
        {
            /// `m_type[c]`: 0 catch, 1 filter, 2 finally, 4 fault.
            Flags : int
            /// `m_filterAddr[c]`: a `DynamicScope` token for a catch, an IL offset for a filter,
            /// and 0 for the cleanup kinds.
            FilterAddrOrToken : int
            /// `m_catchAddr[c]`.
            HandlerStart : int
            /// `m_catchEndAddr[c]`.
            HandlerEnd : int
        }

    /// What a `__ExceptionInfo` will be made to say. Deliberately expressed as the fields rather
    /// than as clauses-plus-offsets, because the projection under test is exactly the arithmetic
    /// that turns these into clauses, and a fixture that did any of it would be testing itself.
    type private ExceptionInfoSpec =
        {
            /// `m_startAddr`.
            StartAddr : int
            /// `m_endAddr`, which every clause but `finally` takes its try length from.
            EndAddr : int
            /// `m_endFinally`, which a `finally` clause takes its try length from instead. `-1`
            /// when the region has none, which is what the constructor sets.
            EndFinally : int
            /// The clauses, in order. `m_currentCatch` is set to this many.
            Clauses : ClauseSpec list
            /// How many slots each parallel array has beyond `Clauses`. `__ExceptionInfo`
            /// allocates them four at a time and doubles, so a reader walking their length rather
            /// than `m_currentCatch` would decode the zeroed tail as extra catch clauses.
            SpareCapacity : int
        }

    /// What a `DynamicResolver`'s fields will be made to say. Everything here is a field the
    /// resolver's constructor assigns and `DynamicMethodBody` reads back.
    type private ResolverBody =
        {
            /// `m_scope.m_tokens`, in index order starting at 0.
            Scope : ScopeEntry list
            /// How many slots the backing `object[]` has beyond `Scope`'s length. `List&lt;T&gt;`
            /// over-allocates, so a reader that walked `_items.Length` rather than `_size` would
            /// see stale slots; a non-zero value here is what catches that.
            ScopeSpareCapacity : int
            /// `m_code`: the baked IL.
            Code : byte[]
            /// `m_localSignature`: a LocalVarSig blob (0x07, then a count, then that many types).
            LocalSignature : byte[]
            /// `DynamicMethod._initLocals`, which `GetCodeInfo` reports as `initLocals`.
            InitLocals : bool
            /// `m_exceptions`, the `__ExceptionInfo[]` the `ILGenerator` path fills in. `None`
            /// leaves the field null, which is what an `ILGenerator` that saw no `try` produces.
            Exceptions : ExceptionInfoSpec list option
            /// `m_exceptionHeader`, non-null only on the `DynamicILInfo` path.
            ExceptionHeader : byte[] option
        }

    /// The scope every `DynamicILGenerator` starts with and no body can avoid: the seeded null at
    /// index 0, and at index 1 the method's own signature blob, which `GetCallableMethod` reads out
    /// by field and no instruction ever names.
    let private baselineScope =
        [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy ; 0x00uy |] ]

    /// `ldarg.0; ldarg.0; add; ret` — the smallest body that computes something, and deliberately
    /// operand-free.
    let private doublingBody =
        {
            Code = [| 0x02uy ; 0x02uy ; 0x58uy ; 0x2Auy |]
            // LocalVarSig with a count of zero: no locals.
            LocalSignature = [| 0x07uy ; 0x00uy |]
            InitLocals = true
            Exceptions = None
            ExceptionHeader = None
            Scope = baselineScope
            ScopeSpareCapacity = 0
        }

    /// A `ldstr` naming scope entry <paramref name="index"/>, then `ret`. The tag is the one
    /// `GetTokenFor(string)` applies; `scopeToken` builds others.
    let private ldstrBody (index : int) (scope : ScopeEntry list) =
        { doublingBody with
            Code =
                Array.append
                    (Array.append [| 0x72uy |] (System.BitConverter.GetBytes (index ||| 0x70000000)))
                    [| 0x2Auy |]
            Scope = scope
        }

    /// A `newarr` naming scope entry <paramref name="index"/>, then `ret`. The tag is the one
    /// `GetTokenFor(RuntimeTypeHandle)` applies, which `DynamicScope`'s indexer masks off and
    /// ignores; the entry is what decides whether this resolves.
    let private newarrBody (index : int) (scope : ScopeEntry list) =
        { doublingBody with
            Code =
                Array.concat
                    [
                        [| 0x17uy |]
                        [| 0x8Duy |]
                        System.BitConverter.GetBytes (index ||| 0x02000000)
                        [| 0x2Auy |]
                    ]
            Scope = scope
        }

    /// Allocate an instance of `typeInfo` with every instance field zeroed, as `newobj` would
    /// before running a constructor. <paramref name="typeDefn"/> is how the type is spelled for
    /// concretization, which differs from `typeInfo.Identity` alone for a generic instantiation.
    let private allocateZeroedAs
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeDefn : TypeDefn)
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
                typeDefn

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

    let private allocateZeroed
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        allocateZeroedAs
            loggerFactory
            baseClassTypes
            (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.Class))
            typeInfo
            state

    /// A `System.Reflection.Emit.DynamicScope` whose `m_tokens` holds <paramref name="entries"/>.
    ///
    /// Built as a real `List&lt;object&gt;` — a genuine generic instantiation with a real `_items`
    /// and `_size` — rather than as any object that happens to have fields of those names, because
    /// the over-allocation `_size` exists to describe is exactly what one of the tests below is for.
    let private allocateScope
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (entries : ScopeEntry list)
        (spareCapacity : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let objectHandle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.PrimitiveType PrimitiveType.Object)

        let state, objectHandle = objectHandle

        let itemsAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef None)
                (List.length entries + spareCapacity)
                state

        let state =
            (state, List.indexed entries)
            ||> List.fold (fun state (i, entry) ->
                let value, state =
                    match entry with
                    | ScopeEntry.Null -> CliType.ObjectRef None, state
                    | ScopeEntry.Str s ->
                        let addr, state =
                            IlMachineState.allocateManagedString loggerFactory baseClassTypes s state

                        CliType.ObjectRef (Some addr), state
                    | ScopeEntry.Blob bytes ->
                        let addr, state = NativeCall.allocateManagedByteArray baseClassTypes bytes state

                        CliType.ObjectRef (Some addr), state
                    | ScopeEntry.DynamicMethodObject ->
                        let addr, state =
                            allocateZeroed loggerFactory baseClassTypes baseClassTypes.DynamicMethod state

                        CliType.ObjectRef (Some addr), state
                    | ScopeEntry.VarArgMethodObject ->
                        let addr, state =
                            allocateZeroed loggerFactory baseClassTypes baseClassTypes.VarArgMethod state

                        CliType.ObjectRef (Some addr), state
                    | ScopeEntry.TypeHandle target ->
                        // A real boxed `RuntimeTypeHandle` pointing at the registry's canonical
                        // `RuntimeType` for the target, rather than any object with an `m_type`
                        // field: the walk under test is entry -> m_type -> RuntimeType -> m_handle,
                        // and `getOrAllocateType` is the same choke point through which a guest's
                        // own `typeof(T).TypeHandle` would have reached the scope.
                        let runtimeType, state =
                            IlMachineState.getOrAllocateType loggerFactory baseClassTypes target state

                        let addr, state =
                            allocateZeroedAs
                                loggerFactory
                                baseClassTypes
                                (TypeDefn.FromDefinition (
                                    baseClassTypes.RuntimeTypeHandle.Identity,
                                    SignatureTypeKind.ValueType
                                ))
                                baseClassTypes.RuntimeTypeHandle
                                state

                        let heapObj = ManagedHeap.get addr state.ManagedHeap

                        let fieldId =
                            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_type"

                        let heapObj =
                            AllocatedNonArrayObject.SetFieldById fieldId (CliType.ObjectRef (Some runtimeType)) heapObj

                        let state =
                            { state with
                                ManagedHeap = ManagedHeap.set addr heapObj state.ManagedHeap
                            }

                        CliType.ObjectRef (Some addr), state

                IlMachineState.setArrayValue itemsAddr value i state
            )

        // Fill the spare capacity with something a reader that ignored `_size` would trip over: a
        // string, so it would be classified as a resolvable entry rather than merely refused.
        let state =
            (state, [ List.length entries .. List.length entries + spareCapacity - 1 ])
            ||> List.fold (fun state i ->
                let addr, state =
                    IlMachineState.allocateManagedString loggerFactory baseClassTypes $"stale-%d{i}" state

                IlMachineState.setArrayValue itemsAddr (CliType.ObjectRef (Some addr)) i state
            )

        let listType =
            requiredTopLevelType baseClassTypes.Corelib "System.Collections.Generic" "List`1"

        let listDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (listType.Identity, SignatureTypeKind.Class),
                ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Object)
            )

        let listAddr, state =
            allocateZeroedAs loggerFactory baseClassTypes listDefn listType state

        let state =
            state
            |> IlMachineState.setOwnInstanceField listAddr "_items" (CliType.ObjectRef (Some itemsAddr))
            |> IlMachineState.setOwnInstanceField
                listAddr
                "_size"
                (CliType.Numeric (CliNumericType.Int32 (List.length entries)))

        let scopeAddr, state =
            allocateZeroed
                loggerFactory
                baseClassTypes
                (requiredTopLevelType baseClassTypes.Corelib "System.Reflection.Emit" "DynamicScope")
                state

        let state =
            IlMachineState.setOwnInstanceField scopeAddr "m_tokens" (CliType.ObjectRef (Some listAddr)) state

        scopeAddr, state

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

        let scopeAddr, state =
            allocateScope loggerFactory baseClassTypes body.Scope body.ScopeSpareCapacity state

        let state =
            IlMachineState.setOwnInstanceField resolverAddr "m_scope" (CliType.ObjectRef (Some scopeAddr)) state

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
            match body.Exceptions with
            | None -> state
            | Some infos ->
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

                let state, int32Handle =
                    IlMachineState.concretizeType
                        loggerFactory
                        baseClassTypes
                        state
                        baseClassTypes.Corelib.Name
                        ImmutableArray.Empty
                        ImmutableArray.Empty
                        (TypeDefn.PrimitiveType PrimitiveType.Int32)

                let allocateIntArray (values : int list) (spare : int) (state : IlMachineState) =
                    let padded = values @ List.replicate spare 0

                    let addr, state =
                        IlMachineState.allocateArray
                            (ConcreteTypeHandle.OneDimArrayZero int32Handle)
                            (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                            padded.Length
                            state

                    let state =
                        padded
                        |> List.indexed
                        |> List.fold
                            (fun state (i, v) ->
                                IlMachineState.setArrayValue addr (CliType.Numeric (CliNumericType.Int32 v)) i state
                            )
                            state

                    addr, state

                let arrayAddr, state =
                    IlMachineState.allocateArray
                        (ConcreteTypeHandle.OneDimArrayZero elementHandle)
                        (fun () -> CliType.ObjectRef None)
                        infos.Length
                        state

                let state =
                    infos
                    |> List.indexed
                    |> List.fold
                        (fun state (i, info) ->
                            let infoAddr, state =
                                allocateZeroed loggerFactory baseClassTypes exceptionInfo state

                            let state =
                                [
                                    "m_startAddr", info.StartAddr
                                    "m_endAddr", info.EndAddr
                                    "m_endFinally", info.EndFinally
                                    "m_currentCatch", info.Clauses.Length
                                ]
                                |> List.fold
                                    (fun state (name, value) ->
                                        IlMachineState.setOwnInstanceField
                                            infoAddr
                                            name
                                            (CliType.Numeric (CliNumericType.Int32 value))
                                            state
                                    )
                                    state

                            let state =
                                [
                                    "m_type", info.Clauses |> List.map _.Flags
                                    "m_filterAddr", info.Clauses |> List.map _.FilterAddrOrToken
                                    "m_catchAddr", info.Clauses |> List.map _.HandlerStart
                                    "m_catchEndAddr", info.Clauses |> List.map _.HandlerEnd
                                ]
                                |> List.fold
                                    (fun state (name, values) ->
                                        let addr, state = allocateIntArray values info.SpareCapacity state

                                        IlMachineState.setOwnInstanceField
                                            infoAddr
                                            name
                                            (CliType.ObjectRef (Some addr))
                                            state
                                    )
                                    state

                            IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some infoAddr)) i state
                        )
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

    /// Mints with the given body in an existing state, and returns whatever the handler threw, so
    /// that each refusal can be checked to fire for its own reason rather than for whichever one
    /// happens to come first. Takes the state because a test whose scope holds a `TypeHandle` entry
    /// has to concretize that entry's target somewhere before it can build the body.
    let private mintExpectingFailureIn
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (body : ResolverBody)
        (state : IlMachineState)
        : string
        =
        let ex =
            Assert.Throws<System.Exception> (fun () ->
                mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state
                |> ignore<ManagedHeapAddress * ManagedHeapAddress * IlMachineState>
            )

        ex.Message

    /// `mintExpectingFailureIn` against a fresh fixture, for the tests that need no prior state.
    let private mintExpectingFailure (body : ResolverBody) : string =
        let loggerFactory, prepared, state = loadFixture ()
        mintExpectingFailureIn loggerFactory prepared body state

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
        definition.GetPreparation () |> shouldEqual None

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

    /// The method `concretize` produced, insisting it produced one. Its `Error` is a `catch` clause
    /// whose scope entry does not name a type, which none of these fixtures has.
    let private requireConcretized
        (result :
            Result<
                MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>,
                TypeInfo<GenericParamFromMetadata, TypeDefn> * string
             >)
        : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        match result with
        | Ok method -> method
        | Error (exceptionType, why) ->
            failwith $"expected the method to concretize, but it was refused with %s{exceptionType.Name}: %s{why}"

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

        localsInitOf (requireConcretized method) |> shouldEqual false

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

        localsInitOf (requireConcretized first) |> shouldEqual true

        let state = setInitLocals resolver false state

        let _, second =
            DynamicMethodExecution.concretize loggerFactory prepared.BaseClassTypes "test" handle state

        localsInitOf (requireConcretized second) |> shouldEqual true

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

        definition.GetPreparation ()
        |> Option.map _.LocalsInit
        |> shouldEqual (Some false)

    /// A preparation that fails leaves the method exactly as unprepared as it found it — including
    /// its `initLocals`, which is read *after* the clause types precisely so that this holds.
    ///
    /// Measured on real .NET: a first invocation that fails to compile latches nothing, and a
    /// second invocation after the guest repairs the scope compiles and runs. No guest can reach
    /// that today (repairing `m_tokens` needs reflection PawPrint does not implement), so this is
    /// the only thing standing between the rule and a build that latches `initLocals` on the way
    /// past and then refuses.
    ///
    /// The clause names entry 2, which holds an *open generic definition* rather than a closed
    /// type: `BeginCatchBlock` accepts one, because it is a perfectly good `RuntimeType`, and real
    /// .NET raises `InvalidProgramException` for it when it compiles the method. Entry 2 is a
    /// genuine `RuntimeTypeHandle`, so the mint-time check passes and the refusal lands here.
    [<Test>]
    let ``a failed preparation latches nothing`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let openGeneric =
            RuntimeTypeHandleTarget.OpenGenericTypeDefinition
                (requiredTopLevelType prepared.BaseClassTypes.Corelib "System.Collections.Generic" "List`1").Identity

        let body =
            { doublingBody with
                InitLocals = false
                Scope = baselineScope @ [ ScopeEntry.TypeHandle openGeneric ]
                Exceptions =
                    Some
                        [
                            {
                                StartAddr = 0
                                EndAddr = 11
                                EndFinally = -1
                                SpareCapacity = 3
                                Clauses =
                                    [
                                        {
                                            Flags = 0
                                            FilterAddrOrToken = 0x02000002
                                            HandlerStart = 11
                                            HandlerEnd = 20
                                        }
                                    ]
                            }
                        ]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let handle, _ = definitionBehindStub state stubAddress

        let state, result =
            DynamicMethodExecution.concretize loggerFactory prepared.BaseClassTypes "test" handle state

        match result with
        | Ok method -> failwith $"expected preparation to be refused, but it produced %s{method.Name}"
        | Error (exceptionType, _) ->
            exceptionType.Name
            |> shouldEqual prepared.BaseClassTypes.InvalidProgramException.Name

        let _, definition = definitionBehindStub state stubAddress
        definition.GetPreparation () |> shouldEqual None

    /// The `DynamicScope` index every `ldstr` in a body names.
    ///
    /// Projected rather than compared as a `StringOperand`, which has no equality: its other case
    /// carries a `SourcedStringToken`, hence an `AssemblyName`, which has none. The projection is
    /// not a workaround — an `ldstr` that had decoded to the metadata case here would fail this
    /// rather than silently compare unequal.
    let private scopeStringOperands (body : MintedDynamicMethodBody) : int list =
        body.Instructions
        |> List.map fst
        |> List.choose (fun op ->
            match op with
            | IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, operand) ->
                match operand with
                | StringOperand.FromDynamicScope index -> Some index
                | StringOperand.FromMetadata token ->
                    failwith $"expected a dynamic-scope operand, got the metadata token %O{token.Token}"
            | _ -> None
        )

    /// The refusal the design still turns on for every operand kind but `ldstr`. A `DynamicScope`
    /// operand is a well-formed `MethodDef`/`TypeDef` token that names an unrelated *real* row, so
    /// a body carrying one must not be stored: decoded as-is it would execute against whatever
    /// happened to sit at that index in the scope assembly.
    [<Test>]
    let ``a body carrying a metadata token is refused`` () : unit =
        // ldnull; call 0x06000001; ret
        let body =
            { doublingBody with
                Code = [| 0x14uy ; 0x28uy ; 0x01uy ; 0x00uy ; 0x00uy ; 0x06uy ; 0x2Auy |]
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "DynamicScope"
        message |> shouldContainText "0x06000001"

    /// The scope entry an `ldstr` names becomes the operand's value.
    [<Test>]
    let ``an ldstr resolves against the DynamicScope`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            ldstrBody 2 [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy |] ; ScopeEntry.Str "hello" ]

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let _, definition = definitionBehindStub state stubAddress

        scopeStringOperands (definition.GetBody ()) |> shouldEqual [ 2 ]

    /// The finding that forces demand-driven resolution: `DynamicILGenerator`'s constructor puts the
    /// method's own signature blob in the scope before any user code runs, and nothing ever names
    /// it. A reader that required every entry to be resolvable would refuse *every* dynamic method,
    /// including this one, whose body is nothing but `ldstr; ret`.
    [<Test>]
    let ``an unreferenced entry of an unsupported kind does not prevent minting`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            ldstrBody
                2
                [
                    ScopeEntry.Null
                    // Exactly the shape `GetTokenFor(byte[] signature)` leaves at index 1.
                    ScopeEntry.Blob [| 0x00uy ; 0x01uy ; 0x08uy ; 0x08uy |]
                    ScopeEntry.Str "fine"
                ]

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let _, definition = definitionBehindStub state stubAddress

        scopeStringOperands (definition.GetBody ()) |> shouldEqual [ 2 ]

    /// `DynamicScope`'s indexer masks the tag off and never looks at it again
    /// (`DynamicILGenerator.cs:976-987`), so a token tagged `MethodDef` whose low bits name a string
    /// entry resolves happily on real .NET. Refusing it on the strength of its tag would reject a
    /// program the real runtime runs — the *entry* is authoritative, not the tag.
    [<Test>]
    let ``the tag bits of a scope token are not consulted`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        // ldstr 0x06000002; ret -- a MethodDef tag over an index that holds a string.
        let body =
            { doublingBody with
                Code = [| 0x72uy ; 0x02uy ; 0x00uy ; 0x00uy ; 0x06uy ; 0x2Auy |]
                Scope = [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy |] ; ScopeEntry.Str "tagged" ]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let _, definition = definitionBehindStub state stubAddress

        scopeStringOperands (definition.GetBody ()) |> shouldEqual [ 2 ]

    /// `List<T>` over-allocates, so `_items` is longer than the list. A reader that walked the
    /// backing array rather than `_size` would see slots holding whatever was last there — here,
    /// strings, so they would be classified as perfectly good entries rather than merely refused.
    [<Test>]
    let ``scope entries past _size are not read`` () : unit =
        let body =
            { ldstrBody 3 [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy |] ; ScopeEntry.Str "real" ] with
                ScopeSpareCapacity = 4
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "does not exist"
        message |> shouldContainText "entry 3"

    [<Test>]
    let ``an ldstr naming an entry of an unsupported kind is refused`` () : unit =
        let body =
            ldstrBody 1 [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy |] ; ScopeEntry.Str "unused" ]

        let message = mintExpectingFailure body

        message |> shouldContainText "entry 1"
        message |> shouldContainText "System.Byte"
        message |> shouldContainText "rather than a string"

    /// Index 0 is the `null` `DynamicScope` seeds `m_tokens` with. Distinguishable in the message
    /// from "no such entry", because the two mean different things about the emitted IL.
    [<Test>]
    let ``an ldstr naming the scope's seeded null is refused`` () : unit =
        let body =
            ldstrBody 0 [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy |] ; ScopeEntry.Str "unused" ]

        let message = mintExpectingFailure body

        message |> shouldContainText "entry 0"
        message |> shouldContainText "null"

    [<Test>]
    let ``an ldstr naming a nonexistent entry is refused`` () : unit =
        let body = ldstrBody 7 baselineScope

        let message = mintExpectingFailure body

        message |> shouldContainText "entry 7"
        message |> shouldContainText "does not exist"

    /// The `DynamicScope` index every `newarr` in a body names. Projected rather than compared as a
    /// `MetadataOperand`, for the same reason `scopeStringOperands` is: the other case carries an
    /// `AssemblyName`, which has no equality. A `newarr` that had decoded to the metadata case fails
    /// here rather than silently comparing unequal.
    let private scopeTypeOperands (body : MintedDynamicMethodBody) : int list =
        body.Instructions
        |> List.map fst
        |> List.choose (fun op ->
            match op with
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newarr, operand) ->
                match operand with
                | MetadataOperand.FromDynamicScope index -> Some index
                | MetadataOperand.FromMetadata token ->
                    failwith $"expected a dynamic-scope operand, got the metadata token %O{token.Token}"
            | _ -> None
        )

    /// The scope indices this body's `call`s name, in order. The sibling of `scopeTypeOperands`, and
    /// separate from it so that a `call` decoded as a metadata token fails here loudly rather than
    /// being silently dropped.
    let private scopeMethodOperands (body : MintedDynamicMethodBody) : int list =
        body.Instructions
        |> List.map fst
        |> List.choose (fun op ->
            match op with
            | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, operand) ->
                match operand with
                | MetadataOperand.FromDynamicScope index -> Some index
                | MetadataOperand.FromMetadata token ->
                    failwith $"expected a dynamic-scope operand, got the metadata token %O{token.Token}"
            | _ -> None
        )

    /// A closed `RuntimeTypeHandleTarget` to hang a scope entry on. `System.Int32` rather than
    /// anything more exotic because what is under test is the walk to the target, not the target.
    let private closedInt32
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        : RuntimeTypeHandleTarget * IlMachineState
        =
        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                prepared.BaseClassTypes
                state
                prepared.BaseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.PrimitiveType PrimitiveType.Int32)

        RuntimeTypeHandleTarget.Closed handle, state

    /// The scope entry a `newarr` names becomes the operand, exactly as an `ldstr`'s does — the
    /// point being that the *tag* in the token (0x02, TypeDef) is not what decided it. `DynamicScope`
    /// masks the tag off and ignores it, so the entry is the only authority on what an index holds.
    [<Test>]
    let ``a newarr resolves against the DynamicScope`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()
        let target, state = closedInt32 loggerFactory prepared state

        let body =
            newarrBody
                2
                [
                    ScopeEntry.Null
                    ScopeEntry.Blob [| 0x00uy |]
                    ScopeEntry.TypeHandle target
                ]

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let _, definition = definitionBehindStub state stubAddress
        let minted = definition.GetBody ()

        scopeTypeOperands minted |> shouldEqual [ 2 ]

        // Nothing about *where* the entry lives is recorded, deliberately: the object is read out of
        // the live `m_scope.m_tokens` when the instruction runs, because a guest can replace a slot
        // between minting and first invocation and real .NET compiles against the replacement. There
        // is no captured address here to go stale, which is a stronger guarantee than a test for one.
        minted.LocalVars |> Option.map Seq.toList |> shouldEqual (Some [])

    /// The kind check is real, and it is the entry that supplies it. A `newarr` whose index holds a
    /// string is a program CoreCLR would reject at JIT; PawPrint rejects it when the method is
    /// minted, as it already does for the mirror-image `ldstr`.
    [<Test>]
    let ``a newarr naming a string entry is refused`` () : unit =
        let body =
            newarrBody 2 [ ScopeEntry.Null ; ScopeEntry.Blob [| 0x00uy |] ; ScopeEntry.Str "not a type" ]

        let message = mintExpectingFailure body

        message |> shouldContainText "Newarr"
        message |> shouldContainText "entry 2"
        message |> shouldContainText "rather than a type handle"

    [<Test>]
    let ``a newarr naming the method's own signature blob is refused`` () : unit =
        let body = newarrBody 1 baselineScope

        let message = mintExpectingFailure body

        message |> shouldContainText "entry 1"
        message |> shouldContainText "System.Byte[]"
        message |> shouldContainText "rather than a type handle"

    [<Test>]
    let ``a newarr naming a nonexistent entry is refused`` () : unit =
        let body = newarrBody 7 baselineScope

        let message = mintExpectingFailure body

        message |> shouldContainText "entry 7"
        message |> shouldContainText "does not exist"

    /// The mirror image of the check above: `ldstr` must not accept a type entry either.
    [<Test>]
    let ``an ldstr naming a type entry is refused`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()
        let target, state = closedInt32 loggerFactory prepared state

        let body =
            ldstrBody
                2
                [
                    ScopeEntry.Null
                    ScopeEntry.Blob [| 0x00uy |]
                    ScopeEntry.TypeHandle target
                ]

        let message = mintExpectingFailureIn loggerFactory prepared body state

        message |> shouldContainText "entry 2"
        message |> shouldContainText "type handle rather than a string"

    /// "This opcode is not wired for scope operands yet" and "this entry is the wrong kind" are
    /// different facts and must read differently: a guest that trips either just gets parked, so the
    /// message is the only diagnostic anyone gets. A method-shaped opcode naming a perfectly good
    /// *method* entry is the case that separates them, and `callvirt` is the one to use now that
    /// `call` is wired: a `DynamicMethod` is always static, so real .NET answers a `callvirt` naming
    /// one with MissingMethodException (measured) rather than by resolving it.
    [<Test>]
    let ``a callvirt naming a dynamic-method entry is refused as unsupported rather than as wrong-kind`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        // ldnull; callvirt <scope 2>; ret
        let body =
            { doublingBody with
                Code =
                    Array.concat
                        [
                            [| 0x14uy ; 0x6Fuy |]
                            System.BitConverter.GetBytes (2 ||| 0x0A000000)
                            [| 0x2Auy |]
                        ]
                Scope =
                    [
                        ScopeEntry.Null
                        ScopeEntry.Blob [| 0x00uy |]
                        ScopeEntry.DynamicMethodObject
                    ]
            }

        let message = mintExpectingFailureIn loggerFactory prepared body state

        message |> shouldContainText "Callvirt"
        message |> shouldContainText "MissingMethodException"
        // Not the wrong-kind wording ("... which holds X rather than Y"): the entry is fine, the
        // opcode is what is missing.
        message |> shouldNotContainText "which holds"

    /// The other half of that distinction, and the case the previous slice pinned the opposite way:
    /// now that `call` *is* wired, a `call` naming a type entry is a wrong-kind refusal rather than
    /// an unsupported-opcode one.
    [<Test>]
    let ``a call naming a type entry is refused as wrong-kind`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()
        let target, state = closedInt32 loggerFactory prepared state

        // ldnull; call <scope 2>; ret
        let body =
            { doublingBody with
                Code =
                    Array.concat
                        [
                            [| 0x14uy ; 0x28uy |]
                            System.BitConverter.GetBytes (2 ||| 0x0A000000)
                            [| 0x2Auy |]
                        ]
                Scope =
                    [
                        ScopeEntry.Null
                        ScopeEntry.Blob [| 0x00uy |]
                        ScopeEntry.TypeHandle target
                    ]
            }

        let message = mintExpectingFailureIn loggerFactory prepared body state

        message |> shouldContainText "Call"
        message |> shouldContainText "entry 2"
        message |> shouldContainText "a type handle rather than a method"

    /// The accepting direction, which is what this slice adds: a `call` naming a `DynamicMethod`
    /// entry is a body PawPrint will mint. Nothing is read out of the entry here — the method it
    /// names lives in its `_methodHandle`, which is still null at this point precisely because a
    /// dynamic method may name *itself* and so cannot be minted before its own body is decoded.
    [<Test>]
    let ``a call naming a dynamic-method entry is minted`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        // ldnull; call <scope 2>; ret
        let body =
            { doublingBody with
                Code =
                    Array.concat
                        [
                            [| 0x14uy ; 0x28uy |]
                            System.BitConverter.GetBytes (2 ||| 0x06000000)
                            [| 0x2Auy |]
                        ]
                Scope =
                    [
                        ScopeEntry.Null
                        ScopeEntry.Blob [| 0x00uy |]
                        ScopeEntry.DynamicMethodObject
                    ]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let _, definition = definitionBehindStub state stubAddress

        scopeMethodOperands (definition.GetBody ()) |> shouldEqual [ 2 ]

    /// `EmitCall` spells the same call as `Emit(OpCode, MethodInfo)` but stores a `VarArgMethod`
    /// wrapper rather than the bare `DynamicMethod` — unconditionally, so this is what an ordinary
    /// `EmitCall(OpCodes.Call, dm, null)` produces rather than a vararg-only curiosity. Both must be
    /// minted, or a guest that used the other overload would be refused a program real .NET runs.
    [<Test>]
    let ``a call naming a vararg-wrapped dynamic method is minted`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        // ldnull; call <scope 2>; ret
        let body =
            { doublingBody with
                Code =
                    Array.concat
                        [
                            [| 0x14uy ; 0x28uy |]
                            System.BitConverter.GetBytes (2 ||| 0x0A000000)
                            [| 0x2Auy |]
                        ]
                Scope =
                    [
                        ScopeEntry.Null
                        ScopeEntry.Blob [| 0x00uy |]
                        ScopeEntry.VarArgMethodObject
                    ]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" doublingSignature body state

        let _, definition = definitionBehindStub state stubAddress

        scopeMethodOperands (definition.GetBody ()) |> shouldEqual [ 2 ]

    /// The mirror image, as for type entries: `ldstr` must not accept a method entry either.
    [<Test>]
    let ``an ldstr naming a dynamic-method entry is refused`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            ldstrBody
                2
                [
                    ScopeEntry.Null
                    ScopeEntry.Blob [| 0x00uy |]
                    ScopeEntry.DynamicMethodObject
                ]

        let message = mintExpectingFailureIn loggerFactory prepared body state

        message |> shouldContainText "entry 2"
        message |> shouldContainText "a dynamic method rather than a string"

    /// The regions a body was minted with.
    let private regionsOf
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (state : IlMachineState)
        (infos : ExceptionInfoSpec list)
        : ExceptionRegion list
        =
        // Entry 2 is the catch clauses' type in every fixture below. Only its *kind* is read at
        // mint, so any closed type serves.
        let target, state = closedInt32 loggerFactory prepared state

        let body =
            { doublingBody with
                Exceptions = Some infos
                Scope = baselineScope @ [ ScopeEntry.TypeHandle target ]
            }

        let stubAddress, _, state =
            mintOne loggerFactory prepared "Probe" [| 0x01uy |] body state

        let _, definition = definitionBehindStub state stubAddress
        (definition.GetBody ()).ExceptionRegions |> Seq.toList

    /// The catch-clause token every fixture below uses: `mdtTypeDef | 2`, the shape
    /// `DynamicScope.GetTokenFor` hands back for the third entry. The tag is deliberately a real
    /// metadata tag, because that is what makes the value indistinguishable from a token naming a
    /// `TypeDef` row and is the whole reason `ExceptionCatchType` exists.
    let private catchToken = 0x02000002

    /// The measured shape of `try { … } catch (T) { … }`: `GetEHInfo` reads the type out of
    /// `m_filterAddr`, where `BeginCatchBlock` put it, and the try range out of
    /// `m_startAddr`/`m_endAddr`. Offsets are those of a real emitted body (`ldarg.0; throw` in a
    /// try, storing 42 in the catch).
    [<Test>]
    let ``a catch clause is decoded from m_filterAddr`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        regionsOf
            loggerFactory
            prepared
            state
            [
                {
                    StartAddr = 0
                    EndAddr = 11
                    EndFinally = -1
                    SpareCapacity = 3
                    Clauses =
                        [
                            {
                                Flags = 0
                                FilterAddrOrToken = catchToken
                                HandlerStart = 11
                                HandlerEnd = 20
                            }
                        ]
                }
            ]
        |> shouldEqual
            [
                ExceptionRegion.Catch (
                    ExceptionCatchType.FromDynamicScope 2,
                    {
                        TryOffset = 0
                        TryLength = 11
                        HandlerOffset = 11
                        HandlerLength = 9
                    }
                )
            ]

    /// Two clauses on one region, which is what a second `BeginCatchBlock` produces. The
    /// `SpareCapacity` is what a real `__ExceptionInfo` looks like — its arrays are allocated four
    /// at a time — so a reader walking their length rather than `m_currentCatch` decodes two extra
    /// catch clauses covering `[0, 0)` and naming scope entry 0.
    [<Test>]
    let ``clauses past m_currentCatch are not clauses`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        regionsOf
            loggerFactory
            prepared
            state
            [
                {
                    StartAddr = 0
                    EndAddr = 11
                    EndFinally = -1
                    SpareCapacity = 2
                    Clauses =
                        [
                            {
                                Flags = 0
                                FilterAddrOrToken = catchToken
                                HandlerStart = 11
                                HandlerEnd = 19
                            }
                            {
                                Flags = 0
                                FilterAddrOrToken = catchToken
                                HandlerStart = 19
                                HandlerEnd = 27
                            }
                        ]
                }
            ]
        |> List.length
        |> shouldEqual 2

    /// The one arithmetic special case: a `finally` clause's try length comes from `m_endFinally`
    /// and every other kind's from `m_endAddr`. Measured on a real `try/catch/finally`, where a
    /// single `__ExceptionInfo` yields a catch covering `[0,+11)` and a finally covering `[0,+25)`
    /// — so the two clauses of one region genuinely have different try ranges, and a projection
    /// that hoisted the length out of the clause loop gets one of them wrong.
    [<Test>]
    let ``a finally clause takes its try length from m_endFinally`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        regionsOf
            loggerFactory
            prepared
            state
            [
                {
                    StartAddr = 0
                    EndAddr = 11
                    EndFinally = 25
                    SpareCapacity = 2
                    Clauses =
                        [
                            {
                                Flags = 0
                                FilterAddrOrToken = catchToken
                                HandlerStart = 11
                                HandlerEnd = 20
                            }
                            {
                                Flags = 2
                                FilterAddrOrToken = 0
                                HandlerStart = 25
                                HandlerEnd = 30
                            }
                        ]
                }
            ]
        |> shouldEqual
            [
                ExceptionRegion.Catch (
                    ExceptionCatchType.FromDynamicScope 2,
                    {
                        TryOffset = 0
                        TryLength = 11
                        HandlerOffset = 11
                        HandlerLength = 9
                    }
                )
                ExceptionRegion.Finally
                    {
                        TryOffset = 0
                        TryLength = 25
                        HandlerOffset = 25
                        HandlerLength = 5
                    }
            ]

    /// A filter's `m_filterAddr` slot is an IL offset, not a token — the same field, read
    /// differently according to `m_type`. A reader that took every clause's slot as a token would
    /// resolve this one against whichever scope entry offset 11 happened to name.
    [<Test>]
    let ``a filter clause keeps its offset rather than a token`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        regionsOf
            loggerFactory
            prepared
            state
            [
                {
                    StartAddr = 0
                    EndAddr = 11
                    EndFinally = -1
                    SpareCapacity = 3
                    Clauses =
                        [
                            {
                                Flags = 1
                                FilterAddrOrToken = 11
                                HandlerStart = 15
                                HandlerEnd = 24
                            }
                        ]
                }
            ]
        |> shouldEqual
            [
                ExceptionRegion.Filter (
                    11,
                    {
                        TryOffset = 0
                        TryLength = 11
                        HandlerOffset = 15
                        HandlerLength = 9
                    }
                )
            ]

    /// `fault` is `0x0004`, which `__ExceptionInfo` also spells `PreserveStack`. Reading it as
    /// fault is safe only because `MarkHelper` writes nothing else into `m_type`; a
    /// `PreserveStack` flag could arrive only through the `DynamicILInfo` blob, which is refused.
    [<Test>]
    let ``a fault clause is decoded`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        regionsOf
            loggerFactory
            prepared
            state
            [
                {
                    StartAddr = 0
                    EndAddr = 7
                    EndFinally = -1
                    SpareCapacity = 3
                    Clauses =
                        [
                            {
                                Flags = 4
                                FilterAddrOrToken = 0
                                HandlerStart = 7
                                HandlerEnd = 11
                            }
                        ]
                }
            ]
        |> shouldEqual
            [
                ExceptionRegion.Fault
                    {
                        TryOffset = 0
                        TryLength = 7
                        HandlerOffset = 7
                        HandlerLength = 4
                    }
            ]

    /// Region order is preserved exactly as `m_exceptions` holds it, because `GetExceptions` has
    /// already sorted innermost-first and `findAcceptingClause`'s tie-break relies on that being
    /// the order it sees. Measured on a real nested `try`: the inner region (`[0,+11)`) comes back
    /// first and the outer (`[0,+24)`) second, which is *not* emit order.
    [<Test>]
    let ``regions keep the order m_exceptions holds them in`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        regionsOf
            loggerFactory
            prepared
            state
            [
                {
                    StartAddr = 0
                    EndAddr = 11
                    EndFinally = -1
                    SpareCapacity = 3
                    Clauses =
                        [
                            {
                                Flags = 0
                                FilterAddrOrToken = catchToken
                                HandlerStart = 11
                                HandlerEnd = 19
                            }
                        ]
                }
                {
                    StartAddr = 0
                    EndAddr = 24
                    EndFinally = -1
                    SpareCapacity = 3
                    Clauses =
                        [
                            {
                                Flags = 0
                                FilterAddrOrToken = catchToken
                                HandlerStart = 24
                                HandlerEnd = 32
                            }
                        ]
                }
            ]
        |> List.map (fun region ->
            match region with
            | ExceptionRegion.Catch (_, offset) -> offset.TryLength
            | other -> failwith $"expected a catch clause, got %O{other}"
        )
        |> shouldEqual [ 11 ; 24 ]

    /// A clause whose scope entry is not a type handle is refused when the method is minted, for
    /// the same reason and to the same standard as an instruction operand naming the wrong kind:
    /// the body could never execute, and finding that out during exception dispatch is far too
    /// late. Entry 1 is the signature blob every scope carries.
    [<Test>]
    let ``a catch clause naming a non-type entry is refused`` () : unit =
        let body =
            { doublingBody with
                Exceptions =
                    Some
                        [
                            {
                                StartAddr = 0
                                EndAddr = 11
                                EndFinally = -1
                                SpareCapacity = 3
                                Clauses =
                                    [
                                        {
                                            Flags = 0
                                            FilterAddrOrToken = 0x02000001
                                            HandlerStart = 11
                                            HandlerEnd = 20
                                        }
                                    ]
                            }
                        ]
            }

        let message = mintExpectingFailure body

        message |> shouldContainText "catch clause"
        message |> shouldContainText "entry 1"

    /// An empty `m_exceptions` array is not a clause and must not be refused: `ILGenerator`
    /// produces one for a method that opened no `try`, and refusing it would reject most bodies.
    [<Test>]
    let ``an empty exception array is not an exception region`` () : unit =
        let loggerFactory, prepared, state = loadFixture ()

        let body =
            { doublingBody with
                Exceptions = Some []
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

        definition.GetPreparation () |> shouldEqual None

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
