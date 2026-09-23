namespace WoofWare.PawPrint

open System
open System.Globalization
open System.Text.Json

/// What the structured value encoding reads besides the value itself: enough to name the type of
/// the heap object a reference points at, and the field a static byref addresses.
type internal DebuggerValueContext =
    {
        Assemblies : LoadedAssemblies
        ConcreteTypes : AllConcreteTypes
        Heap : ManagedHeap
    }

/// The debugger server's structured JSON encoding of a guest value, written as a `structured`
/// property beside the `%O` rendering in `value`.
///
/// Every node is an object whose `kind` names its shape, and the set of kinds is closed, so a
/// client can switch on it exhaustively:
///
/// * `int` — `bits` (8, 16, 32 or 64), `signedness` (`signed`, `unsigned`, or `unspecified` for
///   the int32 and int64 cells, whose signedness the CLI leaves to the instruction that reads
///   them), and `value`: a JSON number up to 32 bits, a decimal string at 64 bits, since a JSON
///   number is not exact beyond 2^53. A 32- or 64-bit value is its two's-complement reading as
///   signed.
/// * `float` — `bits` (32 or 64), `native` (held in a native-float `F` storage cell rather than a
///   float32 or float64 one; always false on the evaluation stack, which records width instead),
///   `value` (a JSON number, or one of the strings `NaN`, `Infinity`, `-Infinity`), and
///   `rawBits`, the IEEE bit pattern in hex, exact where `value` is not.
/// * `bool` — `value`, and `raw`, the stored byte (which need not be 0 or 1).
/// * `char` — `codeUnit`, one UTF-16 code unit (possibly half a surrogate pair).
/// * `null` — a null object reference.
/// * `objectRef` — `address` of a heap object, and `type`, its type description, or null if the
///   address is not live.
/// * `managedPointer` — `pointer`, a byref (see below).
/// * `nativeInt` and `runtimePointer` — `source`: what the pointer-sized value is (see below).
///   PawPrint models most native ints as an identity rather than an address, so only a `number`
///   source has a numeric value.
/// * `valueType` — `type`, and `fields`: each with `name`, `offset` and `size` in bytes, the
///   field's declared `type`, and its `value`. `rawBytesBase64` is the value's byte image when it
///   has no fields, and otherwise null.
/// * `truncatedPointer` — an int32 holding a byref that `conv.i4` truncated: `pointer`.
/// * `nativeIntByte` — one byte (`index`, little-endian) of an identity-modelled native int,
///   `source`, held in a cell of `bits` 8 or 32.
/// * `widenedNativeInt` — an int64 holding a native int `source` widened by `conv.i8`
///   (`signedConversion` true) or `conv.u8`.
/// * `opaqueHashBits` — synthesised pointer-hash bits, `value` in hex; never a real address.
/// * `crossStorageOffset` — the distance between two locations in different storage, which has
///   no numeric value: `target` and `source`, each a `storage` description and a decimal
///   `offset` within it.
/// * `opaque` — `text`, for a value this encoding could not decode.
///
/// A `pointer` has `kind` `null`, `placeholder` (a non-null byref that is only a bit pattern,
/// `bits`), or `byref`, with a `root` and a list of `projections` applied to it in order. Root
/// kinds: `local` and `argument` (`thread`, `frame`, `index`), `stackMemory` (`thread`, `frame`,
/// `block`, `byteOffset`), `nativeMemory` (`block`, `byteOffset`), `heapValue` (`address`,
/// `type`), `heapObjectField` (`address`, `type`, `field`), `arrayElement` (`address`, `type`,
/// `index`), `peByteRange` (`assembly`, `source`, `rva`, `size`), `staticField`
/// (`declaringType`, `field`, `fieldToken`, `owner`), `stringChar` (`address`, `index`),
/// `exposedClassObject` (`target`). Projection kinds: `field` (`field`), `reinterpretAs`
/// (`type`, `text`), `byteOffset` (`bytes`).
///
/// A native `source` has `kind` `number` (`value`, decimal string), `managedPointer`
/// (`pointer`), `functionPointer` (`target`, and `method` when the target is a managed method),
/// `typeHandle` / `typeDesc` / `methodTable` / `methodTableAuxiliaryData` (`target`),
/// `perInstInfo` / `perInstDict` (`type`), `methodHandle` / `fieldHandle` / `eventPipeProvider`
/// / `eventPipeEvent` / `evpMdCtx` (`id`, decimal string), `assembly` / `module` /
/// `metadataImport` (`name`), `gcHandle` (`handle`, `tag`), `lowLevelMonitor` / `waitHandle`
/// (`id`), `evpMd` (`algorithm`), `crossStorageOffset` (as above), or `opaqueHashBits`
/// (`value`). A type `target` has `type`, the description of a closed type or null for an open
/// one, and `text`.
[<RequireQualifiedAccess>]
module internal DebuggerValueJson =
    let ofState (state : IlMachineState) : DebuggerValueContext =
        {
            Assemblies = state._LoadedAssemblies
            ConcreteTypes = state.ConcreteTypes
            Heap = state.ManagedHeap
        }

    [<RequireQualifiedAccess>]
    type private IntWidth =
        | Bits8
        | Bits16
        | Bits32
        | Bits64

    [<RequireQualifiedAccess>]
    type private Signedness =
        | Signed
        | Unsigned
        /// The int32 and int64 cells: the CLI records no signedness for them.
        | Unspecified

    let private int64Text (value : int64) : string =
        value.ToString CultureInfo.InvariantCulture

    let private hex64 (value : int64) : string = $"0x%016X{value}"

    let private typeDescription (context : DebuggerValueContext) (handle : ConcreteTypeHandle) : string =
        AllConcreteTypes.describe context.Assemblies context.ConcreteTypes handle

    let private writeType
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (name : string)
        (handle : ConcreteTypeHandle option)
        : unit
        =
        match handle with
        | Some handle -> writer.WriteString (name, typeDescription context handle)
        | None -> writer.WriteNull name

    /// The type of the live heap object at `address`, or None if nothing is live there.
    let heapObjectType (context : DebuggerValueContext) (address : ManagedHeapAddress) : ConcreteTypeHandle option =
        match HeapObserver.tryGetNonArrayObject address context.Heap with
        | Some object -> Some object.ConcreteType
        | None ->
            HeapObserver.tryGetArray address context.Heap
            |> Option.map (fun array -> array.Shape.ConcreteType)

    let private heapAddressValue (address : ManagedHeapAddress) : int =
        match address with
        | ManagedHeapAddress.ManagedHeapAddress i -> i

    let private writeHeapAddress
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (address : ManagedHeapAddress)
        : unit
        =
        writer.WriteNumber ("address", heapAddressValue address)
        writeType writer context "type" (heapObjectType context address)

    let private writeInt (writer : Utf8JsonWriter) (width : IntWidth) (signedness : Signedness) (value : int64) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("kind", "int")

        let bits =
            match width with
            | IntWidth.Bits8 -> 8
            | IntWidth.Bits16 -> 16
            | IntWidth.Bits32 -> 32
            | IntWidth.Bits64 -> 64

        writer.WriteNumber ("bits", bits)

        writer.WriteString (
            "signedness",
            match signedness with
            | Signedness.Signed -> "signed"
            | Signedness.Unsigned -> "unsigned"
            | Signedness.Unspecified -> "unspecified"
        )

        match width with
        | IntWidth.Bits8
        | IntWidth.Bits16
        | IntWidth.Bits32 -> writer.WriteNumber ("value", value)
        | IntWidth.Bits64 -> writer.WriteString ("value", int64Text value)

        writer.WriteEndObject ()

    /// `value` as a JSON number, or the name of a non-finite value, which JSON has no number for.
    let private writeFloatValue (writer : Utf8JsonWriter) (value : float) (writeFinite : unit -> unit) : unit =
        if Double.IsNaN value then
            writer.WriteString ("value", "NaN")
        elif Double.IsPositiveInfinity value then
            writer.WriteString ("value", "Infinity")
        elif Double.IsNegativeInfinity value then
            writer.WriteString ("value", "-Infinity")
        else
            writeFinite ()

    let private writeSingle (writer : Utf8JsonWriter) (value : float32) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("kind", "float")
        writer.WriteNumber ("bits", 32)
        writer.WriteBoolean ("native", false)
        writeFloatValue writer (float value) (fun () -> writer.WriteNumber ("value", value))
        writer.WriteString ("rawBits", $"0x%08X{BitConverter.SingleToInt32Bits value}")
        writer.WriteEndObject ()

    let private writeDouble (writer : Utf8JsonWriter) (native : bool) (value : float) : unit =
        writer.WriteStartObject ()
        writer.WriteString ("kind", "float")
        writer.WriteNumber ("bits", 64)
        writer.WriteBoolean ("native", native)
        writeFloatValue writer value (fun () -> writer.WriteNumber ("value", value))
        writer.WriteString ("rawBits", hex64 (BitConverter.DoubleToInt64Bits value))
        writer.WriteEndObject ()

    let private writeTypeTarget
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (name : string)
        (target : RuntimeTypeHandleTarget)
        : unit
        =
        writer.WriteStartObject name

        match target with
        | RuntimeTypeHandleTarget.Closed handle -> writer.WriteString ("type", typeDescription context handle)
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _
        | RuntimeTypeHandleTarget.OpenConstructed _
        | RuntimeTypeHandleTarget.DynamicMethodsClass _
        | RuntimeTypeHandleTarget.Composite _
        | RuntimeTypeHandleTarget.FunctionPointer _ -> writer.WriteNull "type"

        writer.WriteString ("text", string target)
        writer.WriteEndObject ()

    let private writeStaticOwner (writer : Utf8JsonWriter) (name : string) (owner : StaticOwner) : unit =
        writer.WriteStartObject name

        match owner with
        | StaticOwner.Shared -> writer.WriteString ("kind", "shared")
        | StaticOwner.OwnedBy (ThreadId.ThreadId thread) ->
            writer.WriteString ("kind", "thread")
            writer.WriteNumber ("thread", thread)

        writer.WriteEndObject ()

    let private writeFieldId
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (name : string)
        (field : FieldId)
        : unit
        =
        writer.WriteStartObject name
        writer.WriteString ("name", field.Name)

        match field with
        | FieldId.Metadata (declaringType = declaringType)
        | FieldId.InlineArrayElement (declaringType = declaringType) ->
            writer.WriteString ("declaringType", typeDescription context declaringType)
        | FieldId.Named _ -> writer.WriteNull "declaringType"

        writer.WriteEndObject ()

    /// The name of static field `field` of `declaringType`, from the metadata that declares it.
    let staticFieldName
        (context : DebuggerValueContext)
        (declaringType : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        : string option
        =
        AllConcreteTypes.tryTypeInfo context.Assemblies context.ConcreteTypes declaringType
        |> Option.bind (fun (_, typeInfo) -> typeInfo.Fields |> List.tryFind (fun f -> f.Handle = field.Get))
        |> Option.map (fun f -> f.Name)

    let private threadIdValue (thread : ThreadId) : int =
        match thread with
        | ThreadId.ThreadId i -> i

    let private frameIdValue (frame : FrameId) : int =
        match frame with
        | FrameId.FrameId i -> i

    let private writeStackSlot
        (writer : Utf8JsonWriter)
        (kind : string)
        (thread : ThreadId)
        (frame : FrameId)
        (index : uint16)
        : unit
        =
        writer.WriteString ("kind", kind)
        writer.WriteNumber ("thread", threadIdValue thread)
        writer.WriteNumber ("frame", frameIdValue frame)
        writer.WriteNumber ("index", int index)

    let private writeByrefRoot (writer : Utf8JsonWriter) (context : DebuggerValueContext) (root : ByrefRoot) : unit =
        writer.WriteStartObject "root"

        match root with
        | ByrefRoot.LocalVariable (thread, frame, index) -> writeStackSlot writer "local" thread frame index
        | ByrefRoot.Argument (thread, frame, index) -> writeStackSlot writer "argument" thread frame index
        | ByrefRoot.StackMemoryByte (thread, frame, StackMemoryBlockId.StackMemoryBlockId block, byteOffset) ->
            writer.WriteString ("kind", "stackMemory")
            writer.WriteNumber ("thread", threadIdValue thread)
            writer.WriteNumber ("frame", frameIdValue frame)
            writer.WriteNumber ("block", block)
            writer.WriteNumber ("byteOffset", byteOffset)
        | ByrefRoot.NativeMemoryByte (NativeMemoryBlockId.NativeMemoryBlockId block, byteOffset) ->
            writer.WriteString ("kind", "nativeMemory")
            writer.WriteNumber ("block", block)
            writer.WriteNumber ("byteOffset", byteOffset)
        | ByrefRoot.HeapValue address ->
            writer.WriteString ("kind", "heapValue")
            writeHeapAddress writer context address
        | ByrefRoot.HeapObjectField (address, field) ->
            writer.WriteString ("kind", "heapObjectField")
            writeHeapAddress writer context address
            writeFieldId writer context "field" field
        | ByrefRoot.ArrayElement (address, index) ->
            writer.WriteString ("kind", "arrayElement")
            writeHeapAddress writer context address
            writer.WriteNumber ("index", index)
        | ByrefRoot.PeByteRange range ->
            writer.WriteString ("kind", "peByteRange")
            writer.WriteString ("assembly", range.AssemblyFullName)
            writer.WriteString ("source", string range)
            writer.WriteNumber ("rva", range.RelativeVirtualAddress)
            writer.WriteNumber ("size", range.Size)
        | ByrefRoot.StaticField (declaringType, field, owner) ->
            writer.WriteString ("kind", "staticField")
            writer.WriteString ("declaringType", typeDescription context declaringType)

            match staticFieldName context declaringType field with
            | Some name -> writer.WriteString ("field", name)
            | None -> writer.WriteNull "field"

            writer.WriteString ("fieldToken", string field)
            writeStaticOwner writer "owner" owner
        | ByrefRoot.StringCharAt (address, index) ->
            writer.WriteString ("kind", "stringChar")
            writer.WriteNumber ("address", heapAddressValue address)
            writer.WriteNumber ("index", index)
        | ByrefRoot.ExposedClassObject target ->
            writer.WriteString ("kind", "exposedClassObject")
            writeTypeTarget writer context "target" target

        writer.WriteEndObject ()

    let private writeProjection
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (projection : ByrefProjection)
        : unit
        =
        writer.WriteStartObject ()

        match projection with
        | ByrefProjection.Field field ->
            writer.WriteString ("kind", "field")
            writeFieldId writer context "field" field
        | ByrefProjection.ReinterpretAs ty ->
            writer.WriteString ("kind", "reinterpretAs")

            writeType
                writer
                context
                "type"
                (AllConcreteTypes.findExistingConcreteType context.ConcreteTypes ty.Identity ty.Generics)

            writer.WriteString ("text", string ty)
        | ByrefProjection.ByteOffset bytes ->
            writer.WriteString ("kind", "byteOffset")
            writer.WriteNumber ("bytes", bytes)

        writer.WriteEndObject ()

    /// A managed pointer, as the `pointer` object documented on this module.
    let writePointer
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (pointer : ManagedPointerSource)
        : unit
        =
        writer.WriteStartObject ()

        match pointer with
        | ManagedPointerSource.Null -> writer.WriteString ("kind", "null")
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            writer.WriteString ("kind", "placeholder")
            writer.WriteString ("bits", hex64 bits)
        | ManagedPointerSource.Byref addressed ->
            writer.WriteString ("kind", "byref")
            writeByrefRoot writer context addressed.Root
            writer.WriteStartArray "projections"

            for projection in addressed.Projections do
                writeProjection writer context projection

            writer.WriteEndArray ()

        writer.WriteEndObject ()

    let private writeCrossStorageOffsetProperties
        (writer : Utf8JsonWriter)
        (offset : SyntheticCrossArrayOffset)
        : unit
        =
        let writeEnd (name : string) (storage : ByteStorageIdentity) (byteOffset : int64) : unit =
            writer.WriteStartObject name
            writer.WriteString ("storage", string storage)
            writer.WriteString ("offset", int64Text byteOffset)
            writer.WriteEndObject ()

        writeEnd "target" (SyntheticCrossArrayOffset.targetRoot offset) (SyntheticCrossArrayOffset.targetOffset offset)

        writeEnd "source" (SyntheticCrossArrayOffset.sourceRoot offset) (SyntheticCrossArrayOffset.sourceOffset offset)

    let private writeIdSource (writer : Utf8JsonWriter) (kind : string) (id : int64) : unit =
        writer.WriteString ("kind", kind)
        writer.WriteString ("id", int64Text id)

    let private writeNameSource (writer : Utf8JsonWriter) (kind : string) (name : string) : unit =
        writer.WriteString ("kind", kind)
        writer.WriteString ("name", name)

    let private writeGcHandleSource
        (writer : Utf8JsonWriter)
        (GcHandleAddress.GcHandleAddress handle)
        (tag : int64)
        : unit
        =
        writer.WriteString ("kind", "gcHandle")
        writer.WriteNumber ("handle", handle)
        writer.WriteString ("tag", int64Text tag)

    let private writeTargetSource
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (kind : string)
        (target : RuntimeTypeHandleTarget)
        : unit
        =
        writer.WriteString ("kind", kind)
        writeTypeTarget writer context "target" target

    let private writeTypeSource
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (kind : string)
        (handle : ConcreteTypeHandle)
        : unit
        =
        writer.WriteString ("kind", kind)
        writer.WriteString ("type", typeDescription context handle)

    let private writeNumberSource (writer : Utf8JsonWriter) (value : int64) : unit =
        writer.WriteString ("kind", "number")
        writer.WriteString ("value", int64Text value)

    let private writeManagedPointerSource
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (pointer : ManagedPointerSource)
        : unit
        =
        writer.WriteString ("kind", "managedPointer")
        writer.WritePropertyName "pointer"
        writePointer writer context pointer

    /// What a pointer-sized value is, as the native `source` object documented on this module.
    let writeNativeIntSource
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (source : NativeIntSource)
        : unit
        =
        writer.WriteStartObject ()

        match source with
        | NativeIntSource.Verbatim value -> writeNumberSource writer value
        | NativeIntSource.ManagedPointer pointer -> writeManagedPointerSource writer context pointer
        | NativeIntSource.FunctionPointer target ->
            writer.WriteString ("kind", "functionPointer")
            writer.WriteString ("target", string target)

            match target with
            | FunctionPointerTarget.Managed method -> writer.WriteString ("method", string method)
            | FunctionPointerTarget.RuntimeAllocator
            | FunctionPointerTarget.Dynamic _
            | FunctionPointerTarget.OpenDelegateShuffleThunk
            | FunctionPointerTarget.VirtualCallStub _
            | FunctionPointerTarget.UnboxingStub _ -> writer.WriteNull "method"
        | NativeIntSource.TypeHandlePtr target -> writeTargetSource writer context "typeHandle" target
        | NativeIntSource.TypeDescPtr target -> writeTargetSource writer context "typeDesc" target
        | NativeIntSource.MethodTablePtr target -> writeTargetSource writer context "methodTable" target
        | NativeIntSource.MethodTableAuxiliaryDataPtr target ->
            writeTargetSource writer context "methodTableAuxiliaryData" target
        | NativeIntSource.PerInstInfoPtr handle -> writeTypeSource writer context "perInstInfo" handle
        | NativeIntSource.PerInstDictPtr handle -> writeTypeSource writer context "perInstDict" handle
        | NativeIntSource.MethodHandlePtr id -> writeIdSource writer "methodHandle" id
        | NativeIntSource.FieldHandlePtr id -> writeIdSource writer "fieldHandle" id
        | NativeIntSource.AssemblyHandle name -> writeNameSource writer "assembly" name
        | NativeIntSource.ModuleHandle name -> writeNameSource writer "module" name
        | NativeIntSource.MetadataImportHandle name -> writeNameSource writer "metadataImport" name
        | NativeIntSource.GcHandlePtr (handle, tag) -> writeGcHandleSource writer handle tag
        | NativeIntSource.EventPipeProviderPtr id -> writeIdSource writer "eventPipeProvider" id
        | NativeIntSource.EventPipeEventPtr id -> writeIdSource writer "eventPipeEvent" id
        | NativeIntSource.LowLevelMonitorPtr (LowLevelMonitorId.LowLevelMonitorId id) ->
            writer.WriteString ("kind", "lowLevelMonitor")
            writer.WriteNumber ("id", id)
        | NativeIntSource.WaitHandlePtr (WaitHandleId.WaitHandleId id) ->
            writer.WriteString ("kind", "waitHandle")
            writer.WriteNumber ("id", id)
        | NativeIntSource.EvpMdPtr algorithm ->
            writer.WriteString ("kind", "evpMd")
            writer.WriteString ("algorithm", string algorithm)
        | NativeIntSource.EvpMdCtxPtr (EvpMdCtxHandle.EvpMdCtxHandle id) -> writeIdSource writer "evpMdCtx" id
        | NativeIntSource.SyntheticCrossArrayOffset offset ->
            writer.WriteString ("kind", "crossStorageOffset")
            writeCrossStorageOffsetProperties writer offset
        | NativeIntSource.OpaqueHashBits bits ->
            writer.WriteString ("kind", "opaqueHashBits")
            writer.WriteString ("value", hex64 bits)

        writer.WriteEndObject ()

    let private writeRuntimePointerSource
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (pointer : CliRuntimePointer)
        : unit
        =
        writer.WriteStartObject ()

        match pointer with
        | CliRuntimePointer.Verbatim value -> writeNumberSource writer value
        | CliRuntimePointer.TypeHandlePtr target -> writeTargetSource writer context "typeHandle" target
        | CliRuntimePointer.TypeDescPtr target -> writeTargetSource writer context "typeDesc" target
        | CliRuntimePointer.FieldRegistryHandle id -> writeIdSource writer "fieldHandle" id
        | CliRuntimePointer.MethodRegistryHandle id -> writeIdSource writer "methodHandle" id
        | CliRuntimePointer.MethodTablePtr target -> writeTargetSource writer context "methodTable" target
        | CliRuntimePointer.MethodTableAuxiliaryDataPtr target ->
            writeTargetSource writer context "methodTableAuxiliaryData" target
        | CliRuntimePointer.PerInstInfoPtr handle -> writeTypeSource writer context "perInstInfo" handle
        | CliRuntimePointer.PerInstDictPtr handle -> writeTypeSource writer context "perInstDict" handle
        | CliRuntimePointer.Managed pointer -> writeManagedPointerSource writer context pointer
        | CliRuntimePointer.GcHandlePtr (handle, tag) -> writeGcHandleSource writer handle tag

        writer.WriteEndObject ()

    let private writeNativeInt
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (source : NativeIntSource)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteString ("kind", "nativeInt")
        writer.WritePropertyName "source"
        writeNativeIntSource writer context source
        writer.WriteEndObject ()

    let private writeNativeIntByte
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (cellBits : int)
        (source : NativeIntSource)
        (index : int)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteString ("kind", "nativeIntByte")
        writer.WriteNumber ("bits", cellBits)
        writer.WriteNumber ("index", index)
        writer.WritePropertyName "source"
        writeNativeIntSource writer context source
        writer.WriteEndObject ()

    let private writeObjectRef
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (address : ManagedHeapAddress option)
        : unit
        =
        writer.WriteStartObject ()

        match address with
        | None -> writer.WriteString ("kind", "null")
        | Some address ->
            writer.WriteString ("kind", "objectRef")
            writeHeapAddress writer context address

        writer.WriteEndObject ()

    let private writeInt64Source
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (source : Int64Source)
        : unit
        =
        match source with
        | Int64Source.Verbatim value -> writeInt writer IntWidth.Bits64 Signedness.Unspecified value
        | Int64Source.SyntheticCrossArrayOffset offset ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "crossStorageOffset")
            writeCrossStorageOffsetProperties writer offset
            writer.WriteEndObject ()
        | Int64Source.WidenedNativeInt (source, signed) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "widenedNativeInt")
            writer.WriteBoolean ("signedConversion", signed)
            writer.WritePropertyName "source"
            writeNativeIntSource writer context source
            writer.WriteEndObject ()
        | Int64Source.OpaqueHashBits bits ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "opaqueHashBits")
            writer.WriteString ("value", hex64 bits)
            writer.WriteEndObject ()

    let private writeNumeric
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (value : CliNumericType)
        : unit
        =
        match value with
        | CliNumericType.Int8 i -> writeInt writer IntWidth.Bits8 Signedness.Signed (int64 i)
        | CliNumericType.UInt8 (UInt8Source.Verbatim b) -> writeInt writer IntWidth.Bits8 Signedness.Unsigned (int64 b)
        | CliNumericType.UInt8 (UInt8Source.NativeIntByte (source, index)) ->
            writeNativeIntByte writer context 8 source index
        | CliNumericType.Int16 i -> writeInt writer IntWidth.Bits16 Signedness.Signed (int64 i)
        | CliNumericType.UInt16 i -> writeInt writer IntWidth.Bits16 Signedness.Unsigned (int64 i)
        | CliNumericType.Int32 i -> writeInt writer IntWidth.Bits32 Signedness.Unspecified (int64 i)
        | CliNumericType.Int64 source -> writeInt64Source writer context source
        | CliNumericType.NativeInt source -> writeNativeInt writer context source
        | CliNumericType.Float32 f -> writeSingle writer f
        | CliNumericType.Float64 f -> writeDouble writer false f
        | CliNumericType.NativeFloat f -> writeDouble writer true f

    /// A field's current value. With explicit layout, a field that shares bytes with another may
    /// have been overwritten through its sibling, leaving its own cell stale, so for those the
    /// value is read back through the bytes the fields share.
    let rec private writeFieldValue
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (valueType : CliValueType)
        (overlapsAnother : bool)
        (field : CliField)
        : unit
        =
        if not overlapsAnother then
            writeCliType writer context field.Contents
        else
            // `DereferenceFieldById` throws when the shared bytes cannot be decoded as the field's
            // shape (for instance, when a sibling holds a pointer); the debugger reports that
            // rather than failing the whole response.
            let current =
                try
                    Ok (CliValueType.DereferenceFieldById field.Id valueType)
                with e ->
                    Error e.Message

            match current with
            | Ok current -> writeCliType writer context current
            | Error message ->
                writer.WriteStartObject ()
                writer.WriteString ("kind", "opaque")
                writer.WriteString ("text", $"overlapping field %s{field.Name} could not be decoded: %s{message}")
                writer.WriteEndObject ()

    /// The `fields` array (and `rawBytesBase64`) of a value type or of a heap object's contents.
    and writeValueTypeFields
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (valueType : CliValueType)
        : unit
        =
        let fields =
            CliValueType.TryAllFields valueType
            |> List.map (fun field ->
                let field = CliConcreteField.ToCliField field
                let offset, size = CliValueType.GetFieldLayoutById field.Id valueType
                field, offset, size
            )

        writer.WriteStartArray "fields"

        for field, offset, size in fields do
            let overlapsAnother =
                fields
                |> List.exists (fun (other, otherOffset, otherSize) ->
                    not (FieldId.exactlyEqual other.Id field.Id)
                    && otherOffset < offset + size
                    && offset < otherOffset + otherSize
                )

            writer.WriteStartObject ()
            writer.WriteString ("name", field.Name)
            writer.WriteNumber ("offset", offset)
            writer.WriteNumber ("size", size)
            writer.WriteString ("type", typeDescription context field.Type)
            writer.WritePropertyName "value"
            writeFieldValue writer context valueType overlapsAnother field
            writer.WriteEndObject ()

        writer.WriteEndArray ()

        match fields with
        | [] -> writer.WriteBase64String ("rawBytesBase64", ReadOnlySpan (CliValueType.ToBytes valueType))
        | _ :: _ -> writer.WriteNull "rawBytesBase64"

    and private writeValueType
        (writer : Utf8JsonWriter)
        (context : DebuggerValueContext)
        (valueType : CliValueType)
        : unit
        =
        writer.WriteStartObject ()
        writer.WriteString ("kind", "valueType")
        writer.WriteString ("type", typeDescription context valueType.Declared)
        writeValueTypeFields writer context valueType
        writer.WriteEndObject ()

    /// A value stored in an argument, local, static, array element or field.
    and writeCliType (writer : Utf8JsonWriter) (context : DebuggerValueContext) (value : CliType) : unit =
        match value with
        | CliType.Numeric numeric -> writeNumeric writer context numeric
        | CliType.Bool b ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "bool")
            writer.WriteBoolean ("value", b <> 0uy)
            writer.WriteNumber ("raw", int b)
            writer.WriteEndObject ()
        | CliType.Char (high, low) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "char")
            writer.WriteNumber ("codeUnit", (int high <<< 8) ||| int low)
            writer.WriteEndObject ()
        | CliType.ObjectRef address -> writeObjectRef writer context address
        | CliType.RuntimePointer pointer ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "runtimePointer")
            writer.WritePropertyName "source"
            writeRuntimePointerSource writer context pointer
            writer.WriteEndObject ()
        | CliType.ValueType valueType -> writeValueType writer context valueType

    /// A value on the evaluation stack.
    let writeEvalStackValue (writer : Utf8JsonWriter) (context : DebuggerValueContext) (value : EvalStackValue) : unit =
        match value with
        | EvalStackValue.Int32 (Int32Source.Verbatim i) ->
            writeInt writer IntWidth.Bits32 Signedness.Unspecified (int64 i)
        | EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer pointer) ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "truncatedPointer")
            writer.WritePropertyName "pointer"
            writePointer writer context pointer
            writer.WriteEndObject ()
        | EvalStackValue.Int32 (Int32Source.NativeIntByte (source, index)) ->
            writeNativeIntByte writer context 32 source index
        | EvalStackValue.Int64 source -> writeInt64Source writer context source
        | EvalStackValue.NativeInt source -> writeNativeInt writer context source
        | EvalStackValue.Float (EvalStackFloat.Single f) -> writeSingle writer f
        | EvalStackValue.Float (EvalStackFloat.Double f) -> writeDouble writer false f
        | EvalStackValue.ManagedPointer pointer ->
            writer.WriteStartObject ()
            writer.WriteString ("kind", "managedPointer")
            writer.WritePropertyName "pointer"
            writePointer writer context pointer
            writer.WriteEndObject ()
        | EvalStackValue.NullObjectRef -> writeObjectRef writer context None
        | EvalStackValue.ObjectRef address -> writeObjectRef writer context (Some address)
        | EvalStackValue.UserDefinedValueType valueType -> writeValueType writer context valueType
