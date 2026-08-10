namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module IlMachineState =
    let loadAssembly = IlMachineTypeResolution.loadAssembly

    let internal loader = IlMachineTypeResolution.loader

    let concretizeType = IlMachineTypeResolution.concretizeType

    let internal resolveTopLevelTypeFromName =
        IlMachineTypeResolution.resolveTopLevelTypeFromName

    let resolveTypeFromExport = IlMachineTypeResolution.resolveTypeFromExport

    let resolveTypeFromRef = IlMachineTypeResolution.resolveTypeFromRef

    let resolveType = IlMachineTypeResolution.resolveType

    let resolveTypeFromDefn = IlMachineTypeResolution.resolveTypeFromDefn

    let resolveTypeFromSpec = IlMachineTypeResolution.resolveTypeFromSpec

    let resolveTypeFromSpecConcrete =
        IlMachineTypeResolution.resolveTypeFromSpecConcrete

    let resolveTypeFromDefnConcrete =
        IlMachineTypeResolution.resolveTypeFromDefnConcrete

    let runtimeTypeHandleTargetForTypeToken =
        IlMachineTypeResolution.runtimeTypeHandleTargetForTypeToken

    let cliTypeZeroOfHandle = IlMachineTypeResolution.cliTypeZeroOfHandle

    let concretizeFieldDeclaringType =
        IlMachineTypeResolution.concretizeFieldDeclaringType

    let cliTypeZeroOf = IlMachineTypeResolution.cliTypeZeroOf

    let ensureByteConcreteType = IlMachineTypeResolution.ensureByteConcreteType

    let peByteRangeForFieldRva = IlMachineTypeResolution.peByteRangeForFieldRva

    let peByteRangeForEmbeddedManifestResource =
        IlMachineTypeResolution.peByteRangeForEmbeddedManifestResource

    let peByteRangeForFieldSignatureBlob =
        IlMachineTypeResolution.peByteRangeForFieldSignatureBlob

    let peByteRangeForMethodSignatureBlob =
        IlMachineTypeResolution.peByteRangeForMethodSignatureBlob

    let peByteRangeForConstantBlob = IlMachineTypeResolution.peByteRangeForConstantBlob

    let peByteRangePointer = IlMachineTypeResolution.peByteRangePointer

    let peByteRangeCharPointer = IlMachineTypeResolution.peByteRangeCharPointer

    let getFrame = IlMachineThreadState.getFrame

    let setFrame = IlMachineThreadState.setFrame

    let mapFrame = IlMachineThreadState.mapFrame

    let markActiveFrameWrapInTargetInvocation =
        IlMachineThreadState.markActiveFrameWrapInTargetInvocation

    let pushToEvalStack' = IlMachineThreadState.pushToEvalStack'

    let pushToEvalStack = IlMachineThreadState.pushToEvalStack

    let peekEvalStack = IlMachineThreadState.peekEvalStack

    let popEvalStack = IlMachineThreadState.popEvalStack

    let advanceProgramCounter = IlMachineThreadState.advanceProgramCounter

    let setArrayValue = IlMachineThreadState.setArrayValue

    let getArrayValue = IlMachineThreadState.getArrayValue

    let returnFromSyntheticStackFrame =
        IlMachineThreadState.returnFromSyntheticStackFrame

    let returnStackFrame = IlMachineThreadState.returnStackFrame

    let initial = IlMachineThreadState.initial

    let addThread = IlMachineThreadState.addThread

    let allocateUnstartedThread = IlMachineThreadState.allocateUnstartedThread

    let allocateParkedThread = IlMachineThreadState.allocateParkedThread

    let startUnstartedThread = IlMachineThreadState.startUnstartedThread

    let startParkedDispatcher = IlMachineThreadState.startParkedDispatcher

    let reParkDispatcher = IlMachineThreadState.reParkDispatcher

    let allocateArray = IlMachineThreadState.allocateArray

    let allocateMultiDimArray = IlMachineThreadState.allocateMultiDimArray

    let cloneArray = IlMachineThreadState.cloneArray

    let cloneObject = IlMachineThreadState.cloneObject

    let allocateStringData = IlMachineThreadState.allocateStringData

    let setStringData = IlMachineThreadState.setStringData

    let allocateManagedObject = IlMachineThreadState.allocateManagedObject

    let popFromStackToLocalVariable = IlMachineThreadState.popFromStackToLocalVariable

    let popFromStackToArgument = IlMachineThreadState.popFromStackToArgument

    let jumpProgramCounter = IlMachineThreadState.jumpProgramCounter

    let loadArgument = IlMachineThreadState.loadArgument


    let resolveMemberWithGenerics = IlMachineMemberResolution.resolveMemberWithGenerics

    let resolveMember = IlMachineMemberResolution.resolveMember

    let getLocalVariable = IlMachineThreadState.getLocalVariable

    let setLocalVariable = IlMachineThreadState.setLocalVariable

    let setArgument = IlMachineThreadState.setArgument

    let allocateStackMemory = IlMachineThreadState.allocateStackMemory

    let getStackMemoryPool = IlMachineThreadState.getStackMemoryPool

    let setStackMemoryPool = IlMachineThreadState.setStackMemoryPool

    let allocateNativeMemory = IlMachineThreadState.allocateNativeMemory

    let freeNativeMemory = IlMachineThreadState.freeNativeMemory

    let getNativeMemoryPool = IlMachineThreadState.getNativeMemoryPool

    let setNativeMemoryPool = IlMachineThreadState.setNativeMemoryPool

    let setSyncBlock = IlMachineThreadState.setSyncBlock

    let getSyncBlock = IlMachineThreadState.getSyncBlock


    let setStatic = IlMachineManagedByref.setStatic

    let getStatic = IlMachineManagedByref.getStatic

    let readManagedByrefBytesAs = IlMachineManagedByref.readManagedByrefBytesAs

    let readManagedByref = IlMachineManagedByref.readManagedByref

    let readManagedByrefField = IlMachineManagedByref.readManagedByrefField

    let readPeByteRangeBytesAs = IlMachineManagedByref.readPeByteRangeBytesAs

    let writeManagedByrefBytesOrTypedCell =
        IlMachineManagedByref.writeManagedByrefBytesOrTypedCell

    let writeManagedByref = IlMachineManagedByref.writeManagedByref

    let writeManagedByrefWithBase = IlMachineManagedByref.writeManagedByrefWithBase

    let writeIndirectPrimitiveStore = IlMachineManagedByref.writeIndirectPrimitiveStore

    let executeDelegateConstructor = IlMachineRuntimeMetadata.executeDelegateConstructor

    let getOrAllocateType = IlMachineRuntimeMetadata.getOrAllocateType

    let getOrAllocateField = IlMachineRuntimeMetadata.getOrAllocateField

    let getOrAllocateMethod = IlMachineRuntimeMetadata.getOrAllocateMethod

    let evalStackValueToObjectRef = IlMachineRuntimeMetadata.evalStackValueToObjectRef

    let lookupTypeDefn = IlMachineRuntimeMetadata.lookupTypeDefn

    let lookupTypeRef = IlMachineRuntimeMetadata.lookupTypeRef

    let resolveBaseTypeInfo = IlMachineRuntimeMetadata.resolveBaseTypeInfo

    let resolveBaseConcreteType = IlMachineRuntimeMetadata.resolveBaseConcreteType

    let resolveBaseRuntimeTypeHandleTarget =
        IlMachineRuntimeMetadata.resolveBaseRuntimeTypeHandleTarget

    let collectAllInstanceFields = IlMachineRuntimeMetadata.collectAllInstanceFields

    let allocateUninitialisedInstance =
        IlMachineRuntimeMetadata.allocateUninitialisedInstance

    let allocateManagedString = IlMachineRuntimeMetadata.allocateManagedString

    let internCanonicalEmptyString = IlMachineRuntimeMetadata.internCanonicalEmptyString

    let internCastCacheSentinelTable =
        IlMachineRuntimeMetadata.internCastCacheSentinelTable

    let setExceptionMessage = IlMachineRuntimeMetadata.setExceptionMessage

    let setExceptionStackTraceString =
        IlMachineRuntimeMetadata.setExceptionStackTraceString

    let recordThrownStackTrace = IlMachineRuntimeMetadata.recordThrownStackTrace

    let frozenStackTraceToken = IlMachineRuntimeMetadata.frozenStackTraceToken

    let frozenStackTraceFrames = IlMachineRuntimeMetadata.frozenStackTraceFrames

    let getOrAllocateManagedThreadObject =
        IlMachineRuntimeMetadata.getOrAllocateManagedThreadObject

    let getCurrentManagedThreadId = IlMachineRuntimeMetadata.getCurrentManagedThreadId

    let synthesizeTypeInitializationException =
        IlMachineRuntimeMetadata.synthesizeTypeInitializationException

    let synthesizeTargetInvocationException =
        IlMachineRuntimeMetadata.synthesizeTargetInvocationException

    let resolveTypeMetadataToken = IlMachineRuntimeMetadata.resolveTypeMetadataToken

    let tryGetConcreteTypeInfo = IlMachineRuntimeMetadata.tryGetConcreteTypeInfo

    let isReferenceTypeHandle = IlMachineRuntimeMetadata.isReferenceTypeHandle

    let isEnumValueType = IlMachineRuntimeMetadata.isEnumValueType

    let enumUnderlyingHandle = IlMachineRuntimeMetadata.enumUnderlyingHandle

    let primitiveElementIdentity = IlMachineRuntimeMetadata.primitiveElementIdentity

    let unboxPermitted = IlMachineRuntimeMetadata.unboxPermitted

    let requiredOwnInstanceFieldId = IlMachineRuntimeMetadata.requiredOwnInstanceFieldId

    let setInstanceFieldById = IlMachineThreadState.setInstanceFieldById

    /// Overwrite the field named `fieldName` on the non-array heap object at `addr`, resolving
    /// the field against the object's *own* concrete type. Fails loudly if that type does not
    /// itself declare `fieldName`; an inherited field must be resolved against its declaring
    /// type and written with `setInstanceFieldById`.
    let setOwnInstanceField
        (addr : ManagedHeapAddress)
        (fieldName : string)
        (value : CliType)
        (state : IlMachineState)
        : IlMachineState
        =
        let obj = ManagedHeap.get addr state.ManagedHeap
        let field = requiredOwnInstanceFieldId state obj.ConcreteType fieldName
        setInstanceFieldById addr field value state

    let isConcreteTypeAssignableTo = IlMachineRuntimeMetadata.isConcreteTypeAssignableTo

    let isRuntimeTypeHandleTargetAssignableTo =
        IlMachineRuntimeMetadata.isRuntimeTypeHandleTargetAssignableTo

    let containsAnyGenericParameter =
        IlMachineRuntimeMetadata.containsAnyGenericParameter

    /// Rebind the state's logging sink to `lf`. `Logger`/`LoggerFactory` are documented as a
    /// sink that nothing about a run's behaviour may depend on, which is exactly what makes this
    /// legitimate: it changes where the run's diagnostics go and nothing else.
    ///
    /// Exists for `Program.resumeFork`. A machine state computed once and resumed under many
    /// scheduler seeds would otherwise log every one of those runs through the factory the
    /// *prefix* was built with, so each seed's per-run log properties would be missing or wrong.
    let withLoggerFactory (lf : ILoggerFactory) (state : IlMachineState) : IlMachineState =
        { state with
            // Same category `IlMachineState.initial` uses, so a rebound state is indistinguishable
            // from a freshly built one in the log.
            Logger = lf.CreateLogger "IlMachineState"
            LoggerFactory = lf
        }

    /// Replace the scheduling policy on `state` with a fresh PCT policy
    /// seeded from `seed`. Idempotent in `state` apart from `Scheduling`:
    /// any previous `Pct` priorities/Rng are discarded. Round-robin runs
    /// don't call this and stay on the `SchedulerState.RoundRobin` default
    /// set by `IlMachineState.initial`.
    ///
    /// Two callers, and the difference between them is worth understanding. `Program.beginStartup`
    /// calls it before any `chooseNext` has run, so the seed is installed on a virgin policy.
    /// `Program.resumeFork` calls it on a state that has already executed thousands of steps —
    /// which is sound because the scheduler only mutates its policy state at contended decisions
    /// (see `Scheduler`), and a fork snapshot is by construction a state reached without one. So
    /// in both cases the policy the seed lands on is the one `PctState.ofSeed` would have built.
    ///
    /// Resuming a *mid-run* snapshot, where contended decisions have already happened, is a
    /// different thing: the discarded priorities are real, and installing a fresh seed means "re-
    /// randomise the future from here" rather than "replay seed `s` from the start". That is the
    /// intended semantics for a schedule-space tree search, but it is not schedule replay.
    ///
    /// Lives here rather than on `SchedulerState` so callers don't need to
    /// open the policy module just to plug a seed in — the seam is "the
    /// machine has a scheduling policy", and that lives on `IlMachineState`.
    let withPctSeed (seed : uint64) (state : IlMachineState) : IlMachineState =
        { state with
            Scheduling = SchedulerState.Pct (PctState.ofSeed seed)
        }
