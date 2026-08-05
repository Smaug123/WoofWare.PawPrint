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

    let peByteRangePointer = IlMachineTypeResolution.peByteRangePointer

    let getFrame = IlMachineThreadState.getFrame
    let tryGetFrame = IlMachineThreadState.tryGetFrame

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

    let allocateManagedString = IlMachineRuntimeMetadata.allocateManagedString

    let internCanonicalEmptyString = IlMachineRuntimeMetadata.internCanonicalEmptyString

    let internCastCacheSentinelTable =
        IlMachineRuntimeMetadata.internCastCacheSentinelTable

    let setExceptionMessage = IlMachineRuntimeMetadata.setExceptionMessage

    let setExceptionStackTraceString =
        IlMachineRuntimeMetadata.setExceptionStackTraceString

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

    /// Replace the scheduling policy on `state` with a fresh PCT policy
    /// seeded from `seed`. Idempotent in `state` apart from `Scheduling`:
    /// any previous `Pct` priorities/Rng are discarded (this is meant to be
    /// called exactly once, at program prepare time, before any
    /// `Scheduler.chooseNext` call has observed threads). Round-robin runs
    /// don't call this and stay on the `SchedulerState.RoundRobin` default
    /// set by `IlMachineState.initial`.
    ///
    /// Lives here rather than on `SchedulerState` so callers don't need to
    /// open the policy module just to plug a seed in — the seam is "the
    /// machine has a scheduling policy", and that lives on `IlMachineState`.
    let withPctSeed (seed : uint64) (state : IlMachineState) : IlMachineState =
        { state with
            Scheduling = SchedulerState.Pct (PctState.ofSeed seed)
        }
