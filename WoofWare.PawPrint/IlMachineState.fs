namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
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

    let private tryAddInt64 (left : int64) (right : int64) : int64 option =
        let result = left + right

        if ((left ^^^ result) &&& (right ^^^ result)) < 0L then
            None
        else
            Some result

    let private tryNonNegativeScaledOffset (count : int) (scale : int) : int64 option =
        if count < 0 || scale < 0 then
            None
        else
            let count = int64<int> count
            let scale = int64<int> scale

            if scale <> 0L && count > Int64.MaxValue / scale then
                None
            else
                Some (count * scale)

    type internal ManagedPointerIntegerAddress =
        {
            Storage : ByteStorageIdentity option
            // Execution currently consumes Bits and Storage; Offset is retained
            // alongside them so debuggers and failure diagnostics can show the
            // offset within the storage identity without unpacking the
            // synthetic integer address layout.
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
                | Some elementSize, Some byteOffset ->
                    tryNonNegativeScaledOffset index elementSize
                    |> Option.bind (fun elementOffset -> tryAddInt64 elementOffset byteOffset)
                | _ -> None
            | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (_, charIndex), projs) ->
                ManagedPointerSource.tryProjectionByteOffset 0L projs
                |> Option.bind (fun byteOffset ->
                    tryNonNegativeScaledOffset charIndex 2
                    |> Option.bind (fun charOffset -> tryAddInt64 charOffset byteOffset)
                )
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

    [<RequireQualifiedAccess>]
    type private SyntheticAddressOrdinal =
        | Direct of ordinal : uint32
        | Hashed of ordinal : uint32

    let private syntheticAddressOrdinalValue (ordinal : SyntheticAddressOrdinal) : uint32 =
        match ordinal with
        | SyntheticAddressOrdinal.Direct ordinal
        | SyntheticAddressOrdinal.Hashed ordinal -> ordinal

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
        | ByteStorageIdentity.StaticField (declaringType, field) ->
            let fieldHandle : EntityHandle = FieldDefinitionHandle.op_Implicit field.Get
            let fieldRow = MetadataTokens.GetRowNumber fieldHandle
            $"static:%O{declaringType}:%d{fieldRow}"
        | ByteStorageIdentity.LocalMemory (thread, frame, block) -> $"local-memory:%O{thread}:%O{frame}:%O{block}"
        | ByteStorageIdentity.StackLocal (thread, frame, local) -> $"stack-local:%O{thread}:%O{frame}:%d{local}"
        | ByteStorageIdentity.StackArgument (thread, frame, arg) -> $"stack-arg:%O{thread}:%O{frame}:%d{arg}"

    let private maxSyntheticAddressOrdinal : int64 = 268_435_455L

    let private tryDirectSyntheticAddressOrdinal (value : int) : SyntheticAddressOrdinal option =
        let value = int64<int> value

        if value < 0L || value > maxSyntheticAddressOrdinal then
            None
        else
            uint32<int64> value |> SyntheticAddressOrdinal.Direct |> Some

    let private syntheticAddressOrdinalUnchecked (storage : ByteStorageIdentity) : SyntheticAddressOrdinal option =
        match storage with
        | ByteStorageIdentity.Array (ManagedHeapAddress addr)
        | ByteStorageIdentity.String (ManagedHeapAddress addr) -> tryDirectSyntheticAddressOrdinal addr
        | ByteStorageIdentity.PeByteRange _
        | ByteStorageIdentity.StaticField _
        | ByteStorageIdentity.LocalMemory _
        | ByteStorageIdentity.StackLocal _
        | ByteStorageIdentity.StackArgument _ ->
            fnv1a32 (syntheticAddressDescriptor storage) &&& 0x0FFF_FFFFu
            |> SyntheticAddressOrdinal.Hashed
            |> Some

    let private slotIndexes (length : int) : uint16 seq =
        seq {
            for slot in 0 .. min (length - 1) (int UInt16.MaxValue) do
                yield uint16<int> slot
        }

    let private peImageStorageIdentity (assemblyFullName : string) : ByteStorageIdentity =
        // The PE-image collision descriptor reads only AssemblyFullName; the
        // range fields are placeholders for constructing that image identity.
        {
            AssemblyFullName = assemblyFullName
            Source = PeByteRangePointerSource.ManagedResource ""
            RelativeVirtualAddress = 0
            Size = 0
        }
        |> ByteStorageIdentity.PeByteRange

    let private liveStorageIdentities (state : IlMachineState) : ByteStorageIdentity seq =
        seq {
            for KeyValue (array, _) in state.ManagedHeap.Arrays do
                yield ByteStorageIdentity.Array array

            for KeyValue (stringObject, _) in state.ManagedHeap.StringDataOffsets do
                yield ByteStorageIdentity.String stringObject

            for KeyValue (assemblyFullName, _) in state._LoadedAssemblies do
                yield peImageStorageIdentity assemblyFullName

            for KeyValue (declaringType, fields) in state._Statics do
                for KeyValue (field, _) in fields do
                    yield ByteStorageIdentity.StaticField (declaringType, field)

            for KeyValue (thread, threadState) in state.ThreadState do
                for KeyValue (frame, methodState) in threadState.MethodStates do
                    for KeyValue (block, _) in methodState.LocalMemoryPool.Blocks do
                        yield ByteStorageIdentity.LocalMemory (thread, frame, block)

                    for local in slotIndexes methodState.LocalVariables.Length do
                        yield ByteStorageIdentity.StackLocal (thread, frame, local)

                    for arg in slotIndexes methodState.Arguments.Length do
                        yield ByteStorageIdentity.StackArgument (thread, frame, arg)
        }

    let private hasLiveSyntheticAddressCollision
        (state : IlMachineState)
        (storage : ByteStorageIdentity)
        (ordinal : uint32)
        : bool
        =
        let kind = syntheticAddressKind storage
        let descriptor = syntheticAddressDescriptor storage

        liveStorageIdentities state
        |> Seq.exists (fun liveStorage ->
            syntheticAddressKind liveStorage = kind
            && syntheticAddressDescriptor liveStorage <> descriptor
            && match syntheticAddressOrdinalUnchecked liveStorage with
               | Some liveOrdinal -> syntheticAddressOrdinalValue liveOrdinal = ordinal
               | None -> false
        )

    let private syntheticAddressBase (state : IlMachineState) (storage : ByteStorageIdentity) : int64 option =
        let kind = syntheticAddressKind storage

        // Kinds occupy the high nibble, but stay below 8 so the sign bit is
        // clear. The 28-bit root ordinal and 32-bit offset then form one
        // positive synthetic address space per storage kind. Roots that cannot
        // be packed directly use a hash only after checking currently-live
        // roots for collisions; exposing equal bits for distinct live roots
        // would be silent pointer corruption after Conv_U8 drops provenance.
        Diagnostics.Debug.Assert (kind >= 0L && kind < 8L)

        match syntheticAddressOrdinalUnchecked storage with
        | None -> None
        | Some (SyntheticAddressOrdinal.Direct ordinal) -> Some ((kind <<< 60) ||| (int64<uint32> ordinal <<< 32))
        | Some (SyntheticAddressOrdinal.Hashed ordinal) ->
            if hasLiveSyntheticAddressCollision state storage ordinal then
                None
            else
                Some ((kind <<< 60) ||| (int64<uint32> ordinal <<< 32))

    let private maxSyntheticAddressOffset : int64 = 4_294_967_295L

    let private makeManagedPointerIntegerAddress
        (state : IlMachineState)
        (storage : ByteStorageIdentity)
        (offset : int64)
        : ManagedPointerIntegerAddress option
        =
        if offset < 0L || offset > maxSyntheticAddressOffset then
            None
        else
            syntheticAddressBase state storage
            |> Option.map (fun addressBase ->
                {
                    Storage = Some storage
                    Offset = offset
                    Bits = addressBase + offset
                }
            )

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
                        state
                        (ByteStorageIdentity.StackLocal (thread, frame, local))
                        projectionOffset
                | ByrefRoot.Argument (thread, frame, arg) ->
                    makeManagedPointerIntegerAddress
                        state
                        (ByteStorageIdentity.StackArgument (thread, frame, arg))
                        projectionOffset
                | ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset) ->
                    tryAddInt64 (int64<int> rootByteOffset) projectionOffset
                    |> Option.bind (
                        makeManagedPointerIntegerAddress state (ByteStorageIdentity.LocalMemory (thread, frame, block))
                    )
                | ByrefRoot.ArrayElement (arr, index) ->
                    let arrObj = state.ManagedHeap.Arrays.[arr]

                    let elementOffset =
                        if arrObj.Length = 0 then
                            if index = 0 then Some 0L else None
                        else
                            let elementSize = CliType.sizeOf arrObj.Elements.[0]
                            tryNonNegativeScaledOffset index elementSize

                    elementOffset
                    |> Option.bind (fun elementOffset -> tryAddInt64 elementOffset projectionOffset)
                    |> Option.bind (makeManagedPointerIntegerAddress state (ByteStorageIdentity.Array arr))
                | ByrefRoot.PeByteRange peByteRange ->
                    tryAddInt64 (int64<int> peByteRange.RelativeVirtualAddress) projectionOffset
                    |> Option.bind (
                        makeManagedPointerIntegerAddress state (ByteStorageIdentity.PeByteRange peByteRange)
                    )
                | ByrefRoot.StaticField (declaringType, field) ->
                    makeManagedPointerIntegerAddress
                        state
                        (ByteStorageIdentity.StaticField (declaringType, field))
                        projectionOffset
                | ByrefRoot.StringCharAt (str, charIndex) ->
                    tryNonNegativeScaledOffset charIndex 2
                    |> Option.bind (fun charOffset -> tryAddInt64 charOffset projectionOffset)
                    |> Option.bind (makeManagedPointerIntegerAddress state (ByteStorageIdentity.String str))
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
