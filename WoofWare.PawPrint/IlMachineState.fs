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

    let peByteRangePointer = IlMachineTypeResolution.peByteRangePointer

    let getFrame = IlMachineThreadState.getFrame

    let setFrame = IlMachineThreadState.setFrame

    let mapFrame = IlMachineThreadState.mapFrame

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

    let allocateArray = IlMachineThreadState.allocateArray

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

    let allocateLocalMemory = IlMachineThreadState.allocateLocalMemory

    let readLocalMemoryBytes = IlMachineThreadState.readLocalMemoryBytes

    let writeLocalMemoryBytes = IlMachineThreadState.writeLocalMemoryBytes

    let setSyncBlock = IlMachineThreadState.setSyncBlock

    let getSyncBlock = IlMachineThreadState.getSyncBlock


    let setStatic = IlMachineManagedByref.setStatic

    let getStatic = IlMachineManagedByref.getStatic

    let readManagedByrefBytesAs = IlMachineManagedByref.readManagedByrefBytesAs

    let readManagedByref = IlMachineManagedByref.readManagedByref

    let readManagedByrefField = IlMachineManagedByref.readManagedByrefField

    let writeManagedByrefBytes = IlMachineManagedByref.writeManagedByrefBytes

    let writeManagedByref = IlMachineManagedByref.writeManagedByref

    let writeManagedByrefWithBase = IlMachineManagedByref.writeManagedByrefWithBase

    let executeDelegateConstructor = IlMachineRuntimeMetadata.executeDelegateConstructor

    let getOrAllocateType = IlMachineRuntimeMetadata.getOrAllocateType

    let getOrAllocateField = IlMachineRuntimeMetadata.getOrAllocateField

    let getOrAllocateMethod = IlMachineRuntimeMetadata.getOrAllocateMethod

    let evalStackValueToObjectRef = IlMachineRuntimeMetadata.evalStackValueToObjectRef

    let lookupTypeDefn = IlMachineRuntimeMetadata.lookupTypeDefn

    let lookupTypeRef = IlMachineRuntimeMetadata.lookupTypeRef

    let resolveBaseTypeInfo = IlMachineRuntimeMetadata.resolveBaseTypeInfo

    let resolveBaseConcreteType = IlMachineRuntimeMetadata.resolveBaseConcreteType

    let collectAllInstanceFields = IlMachineRuntimeMetadata.collectAllInstanceFields

    let allocateManagedString = IlMachineRuntimeMetadata.allocateManagedString

    let setExceptionStackTraceString =
        IlMachineRuntimeMetadata.setExceptionStackTraceString

    let getOrAllocateManagedThreadObject =
        IlMachineRuntimeMetadata.getOrAllocateManagedThreadObject

    let getCurrentManagedThreadId = IlMachineRuntimeMetadata.getCurrentManagedThreadId

    let synthesizeTypeInitializationException =
        IlMachineRuntimeMetadata.synthesizeTypeInitializationException

    let resolveTypeMetadataToken = IlMachineRuntimeMetadata.resolveTypeMetadataToken

    let tryGetConcreteTypeInfo = IlMachineRuntimeMetadata.tryGetConcreteTypeInfo

    let requiredOwnInstanceFieldId = IlMachineRuntimeMetadata.requiredOwnInstanceFieldId

    let isConcreteTypeAssignableTo = IlMachineRuntimeMetadata.isConcreteTypeAssignableTo
