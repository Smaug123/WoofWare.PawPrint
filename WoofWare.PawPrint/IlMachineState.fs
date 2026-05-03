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

    let private tryScaledOffset (count : int) (scale : int) : int64 option =
        if scale < 0 then
            None
        else
            let count = int64<int> count
            let scale = int64<int> scale

            // Defensive if a future caller widens either input to int64; with
            // today's int inputs these overflow branches are unreachable.
            if scale = 0L then Some 0L
            elif count > 0L && count > Int64.MaxValue / scale then None
            elif count < 0L && count < Int64.MinValue / scale then None
            else Some (count * scale)

    let private tryProjectionByteOffset (projs : ByrefProjection list) : int64 option =
        ((Some 0L), projs)
        ||> List.fold (fun (offset : int64 option) (projection : ByrefProjection) ->
            match offset with
            | None -> None
            | Some offset ->
                match projection with
                | ByrefProjection.ReinterpretAs _ -> Some offset
                | ByrefProjection.ByteOffset n -> CheckedInt64.tryAdd offset (int64<int> n)
                // Field projections need layout-aware byte offsets. Preserve that
                // provenance instead of guessing a flattened address.
                | ByrefProjection.Field _ -> None
        )

    let internal tryManagedPointerAddress
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedAddress option
        =
        match ptr with
        | ManagedPointerSource.Null ->
            Some
                {
                    Storage = None
                    Offset = 0L
                }
        | ManagedPointerSource.Byref (root, projs) ->
            match tryProjectionByteOffset projs with
            | None -> None
            | Some projectionOffset ->
                let make (storage : ByteStorageIdentity) (offset : int64) : ManagedAddress =
                    {
                        Storage = Some storage
                        Offset = offset
                    }

                match root with
                | ByrefRoot.LocalVariable (thread, frame, local) ->
                    make (ByteStorageIdentity.StackLocal (thread, frame, local)) projectionOffset
                    |> Some
                | ByrefRoot.Argument (thread, frame, arg) ->
                    make (ByteStorageIdentity.StackArgument (thread, frame, arg)) projectionOffset
                    |> Some
                | ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset) ->
                    CheckedInt64.tryAdd (int64<int> rootByteOffset) projectionOffset
                    |> Option.map (make (ByteStorageIdentity.LocalMemory (thread, frame, block)))
                | ByrefRoot.ArrayElement (arr, index) ->
                    ManagedPointerByteView.arrayElementSize baseClassTypes state arr
                    |> tryScaledOffset index
                    |> Option.bind (fun elementOffset -> CheckedInt64.tryAdd elementOffset projectionOffset)
                    |> Option.map (make (ByteStorageIdentity.Array arr))
                | ByrefRoot.PeByteRange peByteRange ->
                    make (ByteStorageIdentity.PeByteRange peByteRange) projectionOffset |> Some
                | ByrefRoot.StaticField (declaringType, field) ->
                    make (ByteStorageIdentity.StaticField (declaringType, field)) projectionOffset
                    |> Some
                | ByrefRoot.StringCharAt (str, charIndex) ->
                    tryScaledOffset charIndex 2
                    |> Option.bind (fun charOffset -> CheckedInt64.tryAdd charOffset projectionOffset)
                    |> Option.map (make (ByteStorageIdentity.String str))
                // These roots need object-layout-aware storage identities and
                // byte offsets before `conv.u8` can expose them as tagged
                // addresses. Refuse for now instead of inventing raw bits.
                | ByrefRoot.HeapValue _
                | ByrefRoot.HeapObjectField _ -> None


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
