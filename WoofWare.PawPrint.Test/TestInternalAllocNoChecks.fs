namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Tests for `RuntimeTypeHandle.InternalAllocNoChecks`, the allocation primitive underneath
/// `MulticastDelegate.NewMulticastDelegate` (issue #959). CoreLib spells it
/// `InternalAllocNoChecks_FastPath(pMT) ?? Worker(pMT)` (RuntimeHandles.cs:304), so it is two
/// natives: an InternalCall that bump-allocates out of the thread's allocation context or
/// answers null, and the `RuntimeTypeHandle_InternalAllocNoChecks` QCall that allocates
/// properly. PawPrint has no allocation contexts, so the fast path always declines and the
/// QCall is where every such allocation lands.
///
/// There is no end-to-end guest coverage, and cannot be until multicast delegates work: the
/// only three managed callers in CoreLib are `MulticastDelegate.NewMulticastDelegate`, which
/// hits the next unimplemented native (`Delegate::GetMulticastInvoke`) one statement later;
/// `CastHelpers.Box`, reached only from the *internal* `RuntimeHelpers.Box(MethodTable*, ref
/// byte)` overload (the public `Box(ref byte, RuntimeTypeHandle)` that `RuntimeHelpersBox.cs`
/// covers goes through `RuntimeType.BoxCache` and `ReflectionInvocation_GetBoxInfo` to a
/// `calli` on the *other* allocator instead); and `AsyncHelpers.AllocContinuation`, which is
/// runtime-async. So this drives the two handlers directly, in the shape
/// `TestAssemblyNativeQCalls` established for the same reason. `sourcesPure/DelegateCombine.cs`
/// stays parked meanwhile, and its `unimplemented` comment records what remains.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInternalAllocNoChecks =

    /// The corpus is chosen so that each claim below has something to fail on:
    ///
    /// * `Base` has fields of two different shapes, one of them a reference, so "the object came
    ///   back zeroed" is not satisfiable by a single default.
    /// * `Derived` inherits those fields and adds one, so a handler that collected only the
    ///   type's *own* fields cannot serve it.
    /// * `WithCctor` declares an explicit static constructor — which is what strips
    ///   `beforefieldinit` — so "allocating did not initialise the type" has a witness. That is
    ///   the property `AllocateNoChecks` exists for (methodtable.h:2701), and it is the one a
    ///   well-meaning "make this consistent with the sibling handlers" change would break.
    let private guestSource =
        """
public class Base
{
    public int BaseInt;
    public string BaseRef;
}

public class Derived : Base
{
    public long DerivedLong;
}

public class WithCctor
{
    public static int Sentinel;
    public int Field;

    static WithCctor()
    {
        Sentinel = 7;
    }
}

public static class Program
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
            Program.prepare
                loggerFactory
                (Some "InternalAllocNoChecksTestGuest.cs")
                peImage
                (HostConfig.Default dotnetRuntimes)
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

    /// The guest's own `DumpedAssembly`, read from the same image `prepareGuest` loaded, so
    /// that the type identities below name types the machine already knows about.
    let private readGuestAssembly
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (image : byte[])
        : DumpedAssembly
        =
        use peImage = new MemoryStream (image)
        Assembly.read loggerFactory None peImage

    let private concretiseGuestClass
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (guestAssembly : DumpedAssembly)
        (typeName : string)
        (state : IlMachineState)
        : ConcreteTypeHandle * IlMachineState
        =
        let typeInfo = requiredTopLevelType guestAssembly "" typeName

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                guestAssembly.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.Class))

        handle, state

    /// A `MethodTable*` argument, in the shape `RuntimeHelpers.GetMethodTable` produces.
    let private methodTablePointer (handle : ConcreteTypeHandle) : CliType =
        RuntimeTypeHandleTarget.Closed handle
        |> NativeIntSource.MethodTablePtr
        |> CliNumericType.NativeInt
        |> CliType.Numeric

    /// How to pick the `System.RuntimeTypeHandle` member to drive. Both natives here are
    /// spelled `InternalAllocNoChecks`-something, and the QCall shares its *name* with the
    /// managed `??` wrapper that calls it, so name alone cannot address it. Selecting the QCall
    /// by its import metadata is also what the interpreter itself keys on.
    type private MemberSelector =
        | ByName of name : string
        | ByQCallEntryPoint of entryPoint : string

        override this.ToString () : string =
            match this with
            | MemberSelector.ByName name -> name
            | MemberSelector.ByQCallEntryPoint entryPoint -> entryPoint

    /// Locates a member of corelib's `System.RuntimeTypeHandle` and concretizes it, so the
    /// handler sees the same `ExecutingMethod` signature the interpreter would hand it.
    let private runtimeTypeHandleMethod
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (selector : MemberSelector)
        (state : IlMachineState)
        : IlMachineState *
          TypeInfo<GenericParamFromMetadata, TypeDefn> *
          MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>
        =
        let declaringType =
            requiredTopLevelType baseClassTypes.Corelib "System" "RuntimeTypeHandle"

        let matches
            (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
            : bool
            =
            match selector with
            | MemberSelector.ByName name -> method.Name = name
            | MemberSelector.ByQCallEntryPoint entryPoint ->
                match method.TryNativeImport with
                | Some import -> import.ModuleName = "QCall" && import.EntryPointName = entryPoint
                | None -> false

        let rawMethod =
            declaringType.Methods
            |> List.filter matches
            |> function
                | [ method ] -> method
                | [] -> failwith $"member %O{selector} not found on System.RuntimeTypeHandle"
                | methods ->
                    failwith
                        $"member %O{selector} was ambiguous on System.RuntimeTypeHandle: %d{methods.Length} matches"

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

    /// Runs the selected `System.RuntimeTypeHandle` native with the given arguments.
    ///
    /// Deliberately through `NativeDispatch.tryExecute` — the interpreter's own entry point —
    /// rather than at the owning module. That is what makes "the handler exists but was never
    /// registered" a failure here; registration is otherwise an entirely silent mistake, and
    /// for the QCall it is `NativeDispatch` that derives the entry point from the method's
    /// import metadata rather than from anything this test says.
    let private invokeNative
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (selector : MemberSelector)
        (arguments : CliType list)
        (state : IlMachineState)
        : IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let state, declaringTypeInfo, method =
            runtimeTypeHandleMethod loggerFactory baseClassTypes selector state

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

        match NativeDispatch.tryExecute ctx with
        | Some (NativeHandlerResult.Completed (state, _)) -> state
        | Some result -> failwith $"unexpected %O{selector} execution result: %O{result}"
        | None -> failwith $"%O{selector} did not match any handler, or is not registered in NativeDispatch"

    /// An `object[1]` cell standing in for the caller's local, wrapped in an
    /// `ObjectHandleOnStack`. Seeded with a *non-null* object, so a handler that never wrote
    /// through the handle is distinguishable from one that wrote null — with the C# wrapper's
    /// own `object? result = null` a "never wrote" bug would read back exactly as the caller
    /// initialised it and look like a legitimate answer.
    let private objectHandleOnStackValue
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : CliType * ManagedPointerSource * ManagedHeapAddress * IlMachineState
        =
        let sentinelAddr, state =
            IlMachineState.allocateManagedString loggerFactory baseClassTypes "unwritten" state

        let objectHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Object

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero objectHandle)
                (fun () -> CliType.ObjectRef (Some sentinelAddr))
                1
                state

        let target = ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), [])

        let handleType =
            requiredTopLevelType baseClassTypes.Corelib "System.Runtime.CompilerServices" "ObjectHandleOnStack"

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                state
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (handleType.Identity, SignatureTypeKind.ValueType))

        let zero, state = IlMachineState.cliTypeZeroOfHandle state baseClassTypes handle

        match zero with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state handle "_ptr"

            let value =
                CliValueType.WithFieldSetById ptrField (CliType.RuntimePointer (CliRuntimePointer.Managed target)) vt
                |> CliType.ValueType

            value, target, sentinelAddr, state
        | other -> failwith $"ObjectHandleOnStack zero value was not a value type: %O{other}"

    /// Reads back what the QCall wrote through its `ObjectHandleOnStack`.
    let private readAllocated
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (target : ManagedPointerSource)
        : ManagedHeapAddress
        =
        match IlMachineState.readManagedByref baseClassTypes state target with
        | CliType.ObjectRef (Some addr) -> addr
        | CliType.ObjectRef None -> failwith "handler wrote a null reference through the ObjectHandleOnStack"
        | other -> failwith $"expected an ObjectRef behind the ObjectHandleOnStack, got %O{other}"

    /// Allocates one instance of `typeName` through the QCall and hands back its address.
    let private allocate
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (handle : ConcreteTypeHandle)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let baseClassTypes = prepared.BaseClassTypes

        let handleValue, target, sentinelAddr, state =
            objectHandleOnStackValue loggerFactory baseClassTypes state

        let state =
            invokeNative
                loggerFactory
                prepared
                (MemberSelector.ByQCallEntryPoint "RuntimeTypeHandle_InternalAllocNoChecks")
                [ methodTablePointer handle ; handleValue ]
                state

        let addr = readAllocated baseClassTypes state target

        // The slot really was written, rather than left holding what the caller put there.
        addr |> shouldNotEqual sentinelAddr

        addr, state

    [<Test>]
    let ``the fast path declines, so the slow path is always taken`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestClass loggerFactory prepared.BaseClassTypes guestAssembly "Base" prepared.State

        let state =
            invokeNative
                loggerFactory
                prepared
                (MemberSelector.ByName "InternalAllocNoChecks_FastPath")
                [ methodTablePointer handle ]
                state

        // CoreCLR returns NULL here whenever it cannot bump-allocate out of the thread's
        // allocation context, and its managed caller falls through to the QCall. PawPrint has
        // no allocation contexts, so that is unconditional. Anything else pushed here — most
        // obviously an allocated object — would make the slow path unreachable and the
        // `HasFinalizer` refusal it exists to serve silently dead.
        let returned, _state = IlMachineState.popEvalStack prepared.EntryThread state
        returned |> shouldEqual EvalStackValue.NullObjectRef

    [<Test>]
    let ``allocates an object of the MethodTable's type, zeroed`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestClass loggerFactory prepared.BaseClassTypes guestAssembly "Base" prepared.State

        let addr, state = allocate loggerFactory prepared handle state

        let heapObj = ManagedHeap.get addr state.ManagedHeap
        heapObj.ConcreteType |> shouldEqual handle

        AllocatedNonArrayObject.DereferenceField "BaseInt" heapObj
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

        AllocatedNonArrayObject.DereferenceField "BaseRef" heapObj
        |> shouldEqual (CliType.ObjectRef None)

    [<Test>]
    let ``each call allocates a fresh object`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestClass loggerFactory prepared.BaseClassTypes guestAssembly "Base" prepared.State

        let first, state = allocate loggerFactory prepared handle state
        let second, state = allocate loggerFactory prepared handle state

        // `NewMulticastDelegate` calls this once per combine and then writes the result's
        // fields, so a handler that cached and reused one instance per type would make every
        // multicast delegate of a given type alias every other.
        first |> shouldNotEqual second

        // ... and both are live objects of the right type, so the distinctness above is not
        // satisfied by one of them being a dangling address.
        (ManagedHeap.get first state.ManagedHeap).ConcreteType |> shouldEqual handle
        (ManagedHeap.get second state.ManagedHeap).ConcreteType |> shouldEqual handle

    [<Test>]
    let ``inherited fields are allocated too`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestClass loggerFactory prepared.BaseClassTypes guestAssembly "Derived" prepared.State

        let addr, state = allocate loggerFactory prepared handle state
        let heapObj = ManagedHeap.get addr state.ManagedHeap

        AllocatedNonArrayObject.DereferenceField "DerivedLong" heapObj
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))

        // The base's fields, which a handler that walked only the type's own fields would omit.
        // `NewMulticastDelegate` depends on exactly this: every field it writes (`_target`,
        // `_methodPtr`, `_invocationList`, ...) is declared on `Delegate` or
        // `MulticastDelegate`, never on the delegate type whose MethodTable it passes.
        AllocatedNonArrayObject.DereferenceField "BaseInt" heapObj
        |> CliType.unwrapPrimitiveLikeDeep
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0))

        AllocatedNonArrayObject.DereferenceField "BaseRef" heapObj
        |> shouldEqual (CliType.ObjectRef None)

    [<Test>]
    let ``allocating does not run the type initialiser`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestClass loggerFactory prepared.BaseClassTypes guestAssembly "WithCctor" prepared.State

        // Precondition: nothing has initialised it yet, so the assertion below is about this
        // call rather than about the order the fixture happens to run in.
        TypeInitTable.tryGet handle state.TypeInitTable |> shouldEqual None

        let _addr, state = allocate loggerFactory prepared handle state

        // `MethodTable::AllocateNoChecks` (methodtable.h:2701) may only be used when the
        // caller already knows the type is initialised, and correspondingly performs no
        // initialisation itself — which is the entire reason the "NoChecks" entry point exists
        // alongside `RuntimeTypeHandle_InternalAlloc`. Its sibling handlers in
        // `NativeRuntimeTypeQCall` *do* initialise, so making this one "consistent" with them
        // is a plausible change; it would be wrong, and this is what catches it.
        TypeInitTable.tryGet handle state.TypeInitTable |> shouldEqual None

    [<Test>]
    let ``refuses to put a Nullable on the heap`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let baseClassTypes = prepared.BaseClassTypes

        let nullableType = requiredTopLevelType baseClassTypes.Corelib "System" "Nullable`1"

        let nullableDefn =
            TypeDefn.GenericInstantiation (
                TypeDefn.FromDefinition (nullableType.Identity, SignatureTypeKind.ValueType),
                ImmutableArray.Create (TypeDefn.PrimitiveType PrimitiveType.Int32)
            )

        let state, handle =
            IlMachineState.concretizeType
                loggerFactory
                baseClassTypes
                prepared.State
                baseClassTypes.Corelib.Name
                ImmutableArray.Empty
                ImmutableArray.Empty
                nullableDefn

        // PawPrint boxes a `Nullable<T>` as its underlying value or as null; a heap object
        // carrying a Nullable MethodTable is a shape no reader here can interpret, so the QCall
        // refuses rather than creating one. This is a guard, not an unreachable arm: it is
        // provokeable, and this provokes it.
        let exn =
            Assert.Throws (fun () ->
                allocate loggerFactory prepared handle state
                |> ignore<ManagedHeapAddress * IlMachineState>
            )

        exn.Message |> shouldContainText "refusing to allocate a Nullable<T>"
