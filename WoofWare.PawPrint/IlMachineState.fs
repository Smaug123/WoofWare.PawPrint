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

    type internal ManagedPointerIntegerAddress =
        {
            Storage : ByteStorageIdentity option
            // Retained alongside Bits so debuggers and future diagnostics can
            // show the offset within the storage identity without unpacking
            // the synthetic integer address layout.
            Offset : int64
            Bits : int64
        }

    let internal tryManagedPointerLowAddressBits (state : IlMachineState) (ptr : ManagedPointerSource) : int64 option =
        match ManagedPointerSource.tryStableAddressBits ptr with
        | Some bits -> Some bits
        | None ->
            // This path is only for low-bit masks such as alignment checks.
            // Stack slots and static fields do have synthetic integer
            // addresses below, but we do not claim any low-bit alignment for
            // those storage roots.
            match ptr with
            | ManagedPointerSource.Null -> failwith "unreachable: tryStableAddressBits handles null"
            | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), projs) ->
                let arrObj = state.ManagedHeap.Arrays.[arr]

                let elementSize =
                    if arrObj.Length = 0 then
                        // Array.Empty<T>() has no representative element from
                        // which to read the byte stride. The only stable address
                        // we can derive without the element type is index zero.
                        if index = 0 then Some 0 else None
                    else
                        CliType.sizeOf arrObj.Elements.[0] |> Some

                match elementSize, ManagedPointerSource.tryProjectionByteOffset 0L projs with
                | Some elementSize, Some byteOffset -> Some (int64<int> index * int64<int> elementSize + byteOffset)
                | _ -> None
            | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (_, charIndex), projs) ->
                ManagedPointerSource.tryProjectionByteOffset 0L projs
                |> Option.map (fun byteOffset -> int64<int> charIndex * 2L + byteOffset)
            | ManagedPointerSource.Byref _ -> None

    let private fnv1a32 (text : string) : uint32 =
        let step (hash : uint32) (byte : uint32) : uint32 =
            ((uint64 (hash ^^^ byte) * 16_777_619UL) &&& 0xFFFF_FFFFUL) |> uint32

        let mutable hash = 2_166_136_261u

        for ch in text do
            let code = uint32<int> (int ch)
            hash <- step hash (code &&& 0xFFu)
            hash <- step hash ((code >>> 8) &&& 0xFFu)

        hash

    let private syntheticAddressKind (storage : ByteStorageIdentity) : int64 =
        match storage with
        | ByteStorageIdentity.Array _ -> 1L
        | ByteStorageIdentity.String _ -> 2L
        | ByteStorageIdentity.PeByteRange _ -> 3L
        | ByteStorageIdentity.StaticField _ -> 4L
        | ByteStorageIdentity.LocalMemory _ -> 5L
        | ByteStorageIdentity.StackLocal _ -> 6L
        | ByteStorageIdentity.StackArgument _ -> 7L

    let private syntheticAddressDescriptor (storage : ByteStorageIdentity) : string =
        match storage with
        | ByteStorageIdentity.Array addr -> $"array:%O{addr}"
        | ByteStorageIdentity.String addr -> $"string:%O{addr}"
        // PE pointers use a stable synthetic image base plus RVA. The range
        // identity remains in Storage so direct managed-pointer comparisons
        // still refuse different manifest resources or field-RVA blobs.
        | ByteStorageIdentity.PeByteRange pe -> $"pe-image:%s{pe.AssemblyFullName}"
        | ByteStorageIdentity.StaticField (declaringType, field) -> $"static:%O{declaringType}:%d{field.GetHashCode ()}"
        | ByteStorageIdentity.LocalMemory (thread, frame, block) -> $"local-memory:%O{thread}:%O{frame}:%O{block}"
        | ByteStorageIdentity.StackLocal (thread, frame, local) -> $"stack-local:%O{thread}:%O{frame}:%d{local}"
        | ByteStorageIdentity.StackArgument (thread, frame, arg) -> $"stack-arg:%O{thread}:%O{frame}:%d{arg}"

    let private syntheticAddressBase (storage : ByteStorageIdentity) : int64 =
        let kind = syntheticAddressKind storage
        let ordinal = fnv1a32 (syntheticAddressDescriptor storage) &&& 0x0FFF_FFFFu

        // Kinds occupy the high nibble, but stay below 8 so the sign bit is
        // clear. The 28-bit stable hash and 32-bit offset then form one
        // positive synthetic address space per storage kind.
        Diagnostics.Debug.Assert (kind >= 0L && kind < 8L)
        (kind <<< 60) ||| (int64<uint32> ordinal <<< 32)

    let private maxSyntheticAddressOffset : int64 = 4_294_967_295L

    let private makeManagedPointerIntegerAddress
        (storage : ByteStorageIdentity)
        (offset : int64)
        : ManagedPointerIntegerAddress option
        =
        if offset < 0L || offset > maxSyntheticAddressOffset then
            None
        else
            Some
                {
                    Storage = Some storage
                    Offset = offset
                    Bits = syntheticAddressBase storage + offset
                }

    let internal tryManagedPointerIntegerAddress
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : ManagedPointerIntegerAddress option
        =
        match ptr with
        | ManagedPointerSource.Null ->
            Some
                {
                    Storage = None
                    Offset = 0L
                    Bits = 0L
                }
        | ManagedPointerSource.Byref (root, projs) ->
            match ManagedPointerSource.tryProjectionByteOffset 0L projs with
            | None -> None
            | Some projectionOffset ->
                match root with
                | ByrefRoot.LocalVariable (thread, frame, local) ->
                    makeManagedPointerIntegerAddress
                        (ByteStorageIdentity.StackLocal (thread, frame, local))
                        projectionOffset
                | ByrefRoot.Argument (thread, frame, arg) ->
                    makeManagedPointerIntegerAddress
                        (ByteStorageIdentity.StackArgument (thread, frame, arg))
                        projectionOffset
                | ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset) ->
                    makeManagedPointerIntegerAddress
                        (ByteStorageIdentity.LocalMemory (thread, frame, block))
                        (int64<int> rootByteOffset + projectionOffset)
                | ByrefRoot.ArrayElement (arr, index) ->
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    let elementOffset =
                        if arrObj.Length = 0 then
                            if index = 0 then Some 0L else None
                        else
                            let elementSize = CliType.sizeOf arrObj.Elements.[0]
                            Some (int64<int> index * int64<int> elementSize)

                    elementOffset
                    |> Option.bind (fun elementOffset ->
                        makeManagedPointerIntegerAddress
                            (ByteStorageIdentity.Array arr)
                            (elementOffset + projectionOffset)
                    )
                | ByrefRoot.PeByteRange peByteRange ->
                    makeManagedPointerIntegerAddress
                        (ByteStorageIdentity.PeByteRange peByteRange)
                        (int64<int> peByteRange.RelativeVirtualAddress + projectionOffset)
                | ByrefRoot.StaticField (declaringType, field) ->
                    makeManagedPointerIntegerAddress
                        (ByteStorageIdentity.StaticField (declaringType, field))
                        projectionOffset
                | ByrefRoot.StringCharAt (str, charIndex) ->
                    makeManagedPointerIntegerAddress
                        (ByteStorageIdentity.String str)
                        (int64<int> charIndex * 2L + projectionOffset)
                | ByrefRoot.HeapValue _
                | ByrefRoot.HeapObjectField _ -> None

    type internal StableNativeAddressForUnsignedComparison =
        | Verbatim of bits : int64
        | ManagedPointer of address : ManagedPointerIntegerAddress

    let internal stableNativeAddressForUnsignedComparison
        (state : IlMachineState)
        (value : EvalStackValue)
        : StableNativeAddressForUnsignedComparison option
        =
        match value with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim bits)
        | EvalStackValue.NativeInt (NativeIntSource.SyntheticCrossArrayOffset bits) ->
            StableNativeAddressForUnsignedComparison.Verbatim bits |> Some
        | EvalStackValue.ManagedPointer ptr ->
            tryManagedPointerIntegerAddress state ptr
            |> Option.map StableNativeAddressForUnsignedComparison.ManagedPointer
        | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ptr) ->
            tryManagedPointerIntegerAddress state ptr
            |> Option.map StableNativeAddressForUnsignedComparison.ManagedPointer
        | _ -> None

    let internal compareStableNativeAddressBitsForUnsignedComparison
        (state : IlMachineState)
        (left : EvalStackValue)
        (right : EvalStackValue)
        : (uint64 * uint64) option
        =
        match
            stableNativeAddressForUnsignedComparison state left, stableNativeAddressForUnsignedComparison state right
        with
        | Some (StableNativeAddressForUnsignedComparison.ManagedPointer leftAddress),
          Some (StableNativeAddressForUnsignedComparison.ManagedPointer rightAddress) ->
            // Direct managed-pointer comparisons stay inside one exact storage
            // identity. Same-assembly PE ranges have comparable synthetic bits,
            // but refusing distinct ranges is the conservative model until a
            // concrete runtime path needs cross-range pointer ordering.
            if
                leftAddress.Storage = rightAddress.Storage
                || leftAddress.Storage.IsNone
                || rightAddress.Storage.IsNone
            then
                Some (uint64<int64> leftAddress.Bits, uint64<int64> rightAddress.Bits)
            else
                failwith $"Unsigned managed pointer comparison between different address roots: %O{left} vs %O{right}"
        // This accepts integer bits that have lost managed-pointer provenance,
        // such as CoreLib resource-stream code that computes an end pointer
        // after Conv_U8 and native integer arithmetic. Literal native ints may
        // compare against the same synthetic namespace, but there is no stack
        // provenance left by this point to distinguish them.
        | Some (StableNativeAddressForUnsignedComparison.ManagedPointer leftAddress),
          Some (StableNativeAddressForUnsignedComparison.Verbatim rightBits) ->
            Some (uint64<int64> leftAddress.Bits, uint64<int64> rightBits)
        | Some (StableNativeAddressForUnsignedComparison.Verbatim leftBits),
          Some (StableNativeAddressForUnsignedComparison.ManagedPointer rightAddress) ->
            Some (uint64<int64> leftBits, uint64<int64> rightAddress.Bits)
        | _ -> None


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
