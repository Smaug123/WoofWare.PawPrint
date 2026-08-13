namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.DotnetRuntimeLocator
open WoofWare.PawPrint

/// Tests for `RuntimeTypeHandle_InternalAlloc` (reflectioninvocation.cpp:119), which is
/// `pMT->Allocate()`: the checked counterpart of the `InternalAllocNoChecks` QCall that
/// `TestInternalAllocNoChecks.fs` covers. Same allocation; `MethodTable::Allocate`
/// (methodtable.cpp:4056) additionally runs `EnsureInstanceActive` and a class initialiser.
///
/// The two files are deliberately near-identical, because the *difference* between them is the
/// point. `AllocateNoChecks` exists precisely so that a caller who already knows the type is
/// initialised can skip that work, so "does allocating run the `.cctor`?" must have opposite
/// answers here and there, and each file pins its own. A change that made the two handlers agree
/// would be caught by whichever file it broke.
///
/// There is no end-to-end guest coverage yet, and cannot be until a delegate can be bound: the
/// sole managed caller is `Delegate.InternalAlloc` (Delegate.CoreCLR.cs:435), reached from all
/// four `Delegate.CreateDelegate` overloads and from `CreateDelegateInternal`, and every one of
/// them goes on to `Delegate_BindToMethodName`/`Delegate_BindToMethodInfo` — neither of which is
/// implemented — one statement later. So this drives the handler directly, in the shape
/// `TestAssemblyNativeQCalls` established for the same reason.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInternalAlloc =

    /// The corpus is chosen so that each claim below has something to fail on:
    ///
    /// * `Base` has fields of two different shapes, one of them a reference, so "the object came
    ///   back zeroed" is not satisfiable by a single default.
    /// * `Derived` inherits those fields and adds one, so a handler that collected only the
    ///   type's *own* fields cannot serve it.
    /// * `WithCctor` declares an explicit static constructor — which is what strips
    ///   `beforefieldinit`, making it a precise-init type, the kind `MethodTable::Allocate` really
    ///   does initialise — so "allocating ran the type initialiser" has a witness. This is the one
    ///   claim that must come out the opposite way from `TestInternalAllocNoChecks`.
    /// * `SomeStruct` is a value type, for the refusal.
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

public struct SomeStruct
{
    public int Value;
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
            Program.prepare loggerFactory (Some "InternalAllocTestGuest.cs") peImage (HostConfig.Default dotnetRuntimes)
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

    /// How to pick the `System.RuntimeTypeHandle` member to drive. The QCall shares its *name*
    /// with the managed wrapper that calls it, so name alone cannot address it. Selecting the
    /// QCall by its import metadata is also what the interpreter itself keys on.
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
                (MemberSelector.ByQCallEntryPoint "RuntimeTypeHandle_InternalAlloc")
                [ methodTablePointer handle ; handleValue ]
                state

        let addr = readAllocated baseClassTypes state target

        // The slot really was written, rather than left holding what the caller put there.
        addr |> shouldNotEqual sentinelAddr

        addr, state


    /// As `concretiseGuestClass`, but for a value type. `SignatureTypeKind` is not cosmetic here:
    /// it is what `DumpedAssembly.isValueType` — and so the handler's refusal — keys on.
    let private concretiseGuestStruct
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
                (TypeDefn.FromDefinition (typeInfo.Identity, SignatureTypeKind.ValueType))

        handle, state

    /// `invokeNative`, but handing back the handler's result rather than insisting it completed.
    /// Allocating a type that needs initialising suspends instead of completing, and that
    /// suspension is a fact worth asserting rather than an inconvenience to route around.
    let private invokeNativeRaw
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (prepared : Program.PreparedProgram)
        (arguments : CliType list)
        (state : IlMachineState)
        : NativeHandlerResult
        =
        let baseClassTypes = prepared.BaseClassTypes

        let state, declaringTypeInfo, method =
            runtimeTypeHandleMethod
                loggerFactory
                baseClassTypes
                (MemberSelector.ByQCallEntryPoint "RuntimeTypeHandle_InternalAlloc")
                state

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
        | Some result -> result
        | None -> failwith "RuntimeTypeHandle_InternalAlloc did not match any handler"

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

    /// A handler that collected only the type's *own* fields would serve `Base` and fail here.
    [<Test>]
    let ``an inherited field is present and zeroed`` () : unit =
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
        let second, _state = allocate loggerFactory prepared handle state

        first |> shouldNotEqual second

    /// The claim that separates this QCall from `InternalAllocNoChecks`, and the direct opposite
    /// of `TestInternalAllocNoChecks`'s `allocating does not run the type initialiser`.
    ///
    /// `MethodTable::Allocate` (methodtable.cpp:4070) runs `CheckRunClassInitAsIfConstructingThrowing`
    /// for a type with precise-init cctors, which `WithCctor` is. PawPrint initialises
    /// unconditionally — see the handler's comment — so this witnesses the initialisation, not the
    /// precise-init predicate.
    [<Test>]
    let ``allocating runs the type initialiser`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestClass loggerFactory prepared.BaseClassTypes guestAssembly "WithCctor" prepared.State

        // Precondition: the assertion below is about this call, not about the order the fixture
        // happened to run in.
        TypeInitTable.tryGet handle state.TypeInitTable |> shouldEqual None

        let handleValue, target, _sentinel, state =
            objectHandleOnStackValue loggerFactory prepared.BaseClassTypes state

        // The initialiser is guest code, so the handler cannot run it inline: it suspends, the
        // dispatch loop runs the `.cctor`, and the QCall is re-entered. Asserting the suspension
        // rather than routing around it is what distinguishes "ran the initialiser" from "ignored
        // the question and allocated anyway".
        let state =
            match invokeNativeRaw loggerFactory prepared [ methodTablePointer handle ; handleValue ] state with
            | NativeHandlerResult.SuspendedForClassInit (state, _) -> state
            | other -> failwith $"expected the handler to suspend for class init, got %O{other}"

        TypeInitTable.tryGet handle state.TypeInitTable |> shouldNotEqual None

        // Nothing was written through the result handle on the suspending pass: the caller's
        // local must not be left holding a half-built answer while the `.cctor` runs.
        readAllocated prepared.BaseClassTypes state target |> ignore<ManagedHeapAddress>

    /// `Delegate.InternalAlloc` asserts its argument derives from `MulticastDelegate`, so a value
    /// type cannot arrive through the only caller. Allocating one anyway would put an object on
    /// the heap that no reader could interpret as a box, so refuse by name.
    [<Test>]
    let ``a value type is refused`` () : unit =
        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory

        let image = Roslyn.compile [ guestSource ]
        let prepared = prepareGuest loggerFactory image
        let guestAssembly = readGuestAssembly loggerFactory image

        let handle, state =
            concretiseGuestStruct loggerFactory prepared.BaseClassTypes guestAssembly "SomeStruct" prepared.State

        let handleValue, _target, _sentinel, state =
            objectHandleOnStackValue loggerFactory prepared.BaseClassTypes state

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                invokeNativeRaw loggerFactory prepared [ methodTablePointer handle ; handleValue ] state
                |> ignore<NativeHandlerResult>
            )

        ex.Message |> shouldContainText "SomeStruct"
        ex.Message |> shouldContainText "value type"
