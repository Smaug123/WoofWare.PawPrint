namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module IlMachineManagedByref =
    /// `true` when a `ReinterpretAs ty` projection against a value of the given
    /// shape can be treated as a no-op. Matches same-width primitive reinterprets
    /// within the integer family (including signed<->unsigned and char<->ushort
    /// pairs, which share bit patterns and round-trip through the Int32 stack
    /// slot with modular narrowing) and within the float family (same width
    /// only). Rejects float<->int bit reinterprets, overlay structs, enum
    /// underlying coercions, and any size change; those still need a proper
    /// bytewise implementation.
    let private classifyValueForReinterpret (value : CliType) : (string * int) voption =
        match value with
        | CliType.Bool _ -> ValueSome ("int", 1)
        | CliType.Char _ -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.UInt8 _) -> ValueSome ("int", 1)
        | CliType.Numeric (CliNumericType.Int16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.UInt16 _) -> ValueSome ("int", 2)
        | CliType.Numeric (CliNumericType.Int32 _) -> ValueSome ("int", 4)
        | CliType.Numeric (CliNumericType.Int64 _) -> ValueSome ("int", 8)
        | CliType.Numeric (CliNumericType.Float32 _) -> ValueSome ("float", 4)
        | CliType.Numeric (CliNumericType.Float64 _) -> ValueSome ("float", 8)
        | _ -> ValueNone

    let private classifyTypeForReinterpret (ty : ConcreteType<ConcreteTypeHandle>) : (string * int) voption =
        if ty.Namespace <> "System" then
            ValueNone
        else
            match ty.Name with
            | "Boolean"
            | "SByte"
            | "Byte" -> ValueSome ("int", 1)
            | "Int16"
            | "UInt16"
            | "Char" -> ValueSome ("int", 2)
            | "Int32"
            | "UInt32" -> ValueSome ("int", 4)
            | "Int64"
            | "UInt64" -> ValueSome ("int", 8)
            | "Single" -> ValueSome ("float", 4)
            | "Double" -> ValueSome ("float", 8)
            | _ -> ValueNone

    let private isSafeReinterpretPassthrough (value : CliType) (ty : ConcreteType<ConcreteTypeHandle>) : bool =
        match classifyValueForReinterpret value, classifyTypeForReinterpret ty with
        | ValueSome v, ValueSome t -> v = t
        | _ -> false

    let private bytesEqual (left : byte[]) (right : byte[]) : bool =
        if left.Length <> right.Length then
            false
        else
            let mutable equal = true
            let mutable i = 0

            while equal && i < left.Length do
                equal <- left.[i] = right.[i]
                i <- i + 1

            equal

    let private zeroForPrimitiveReinterpret (ty : ConcreteType<ConcreteTypeHandle>) : CliType voption =
        if ty.Namespace <> "System" || not ty.Generics.IsEmpty then
            ValueNone
        else
            match ty.Name with
            | "Boolean" -> ValueSome (CliType.Bool 0uy)
            | "SByte" -> ValueSome (CliType.Numeric (CliNumericType.Int8 0y))
            | "Byte" -> ValueSome (CliType.Numeric (CliNumericType.UInt8 0uy))
            | "Int16" -> ValueSome (CliType.Numeric (CliNumericType.Int16 0s))
            | "UInt16" -> ValueSome (CliType.Numeric (CliNumericType.UInt16 0us))
            | "Char" -> ValueSome (CliType.Char (0uy, 0uy))
            | "Int32"
            | "UInt32" ->
                // ECMA III.1.1.1 has no separate unsigned 32-bit stack type;
                // PawPrint stores UInt32-shaped values in the same CliType as Int32.
                ValueSome (CliType.Numeric (CliNumericType.Int32 0))
            | "Int64"
            | "UInt64" -> ValueSome (CliType.Numeric (CliNumericType.Int64 0L))
            | "IntPtr"
            | "UIntPtr" -> ValueSome (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
            | "Single" -> ValueSome (CliType.Numeric (CliNumericType.Float32 0.0f))
            | "Double" -> ValueSome (CliType.Numeric (CliNumericType.Float64 0.0))
            | _ -> ValueNone

    let setStatic
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (value : CliType)
        (this : IlMachineState)
        : IlMachineState
        =
        let statics =
            match this._Statics.TryGetValue ty with
            | false, _ -> this._Statics.Add (ty, Map.ofList [ field, value ])
            | true, v -> this._Statics.SetItem (ty, Map.add field value v)

        { this with
            _Statics = statics
        }

    let getStatic
        (ty : ConcreteTypeHandle)
        (field : ComparableFieldDefinitionHandle)
        (this : IlMachineState)
        : CliType option
        =
        match this._Statics.TryGetValue ty with
        | false, _ -> None
        | true, v -> Map.tryFind field v

    let private tryReadInitializedLocalMemoryBytes
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : LocallocBlockId)
        (byteOffset : int)
        (byteCount : int)
        : byte[] voption
        =
        let frameState = IlMachineThreadState.getFrame thread frame state
        let blockData = LocalMemoryPool.getBlock block frameState.LocalMemoryPool
        let rangeEnd = int64 byteOffset + int64 byteCount

        if byteOffset < 0 || byteCount < 0 || rangeEnd > int64 blockData.Bytes.Length then
            ValueNone
        else
            let result = Array.zeroCreate byteCount
            let mutable initialized = true
            let mutable i = 0

            while initialized && i < byteCount do
                match blockData.Bytes.[byteOffset + i] with
                | LocalMemoryByte.Initialized b -> result.[i] <- b
                | LocalMemoryByte.Uninitialized -> initialized <- false

                i <- i + 1

            if initialized then ValueSome result else ValueNone

    let private readRootValue (state : IlMachineState) (root : ByrefRoot) : CliType =
        match root with
        | ByrefRoot.LocalVariable (t, f, v) -> (IlMachineThreadState.getFrame t f state).LocalVariables.[int<uint16> v]
        | ByrefRoot.Argument (t, f, v) -> (IlMachineThreadState.getFrame t f state).Arguments.[int<uint16> v]
        | ByrefRoot.LocalMemoryByte (t, f, block, byteOffset) ->
            IlMachineThreadState.readLocalMemoryBytes t f block byteOffset 1 state
            |> Array.exactlyOne
            |> CliNumericType.UInt8
            |> CliType.Numeric
        | ByrefRoot.HeapValue addr -> CliType.ValueType (ManagedHeap.get addr state.ManagedHeap).Contents
        | ByrefRoot.HeapObjectField (addr, field) ->
            ManagedHeap.get addr state.ManagedHeap
            |> AllocatedNonArrayObject.DereferenceFieldById field
        | ByrefRoot.ArrayElement (arr, index) -> IlMachineThreadState.getArrayValue arr index state
        | ByrefRoot.PeByteRange peByteRange ->
            failwith
                $"TODO: reading PE byte-range root %O{peByteRange} requires a primitive byte-view projection; plain typed PE byte-range root reads are not modelled"
        | ByrefRoot.StaticField (ty, field) ->
            match getStatic ty field state with
            | Some value -> value
            | None ->
                failwith
                    $"Static field byref %O{field.Get} on concrete type %O{ty} was read before the static slot was initialised"
        | ByrefRoot.StringCharAt (str, charIndex) ->
            ManagedHeap.getStringChar str charIndex state.ManagedHeap |> CliType.ofChar

    let private writeRootValue (state : IlMachineState) (root : ByrefRoot) (updated : CliType) : IlMachineState =
        // The ReferenceEquals checks in this function are allocation shortcuts for direct root
        // writes where the caller is storing the exact object already present. Semantic no-op
        // detection for byte/projection writes is represented explicitly by `option` results
        // before this function is called.
        match root with
        | ByrefRoot.LocalVariable (t, f, v) ->
            let existing = IlMachineThreadState.getLocalVariable t f v state

            if System.Object.ReferenceEquals (existing, updated) then
                state
            else
                state |> IlMachineThreadState.setLocalVariable t f v updated
        | ByrefRoot.Argument (t, f, v) ->
            let existing = (IlMachineThreadState.getFrame t f state).Arguments.[int<uint16> v]

            if System.Object.ReferenceEquals (existing, updated) then
                state
            else
                state |> IlMachineThreadState.setArgument t f v updated
        | ByrefRoot.LocalMemoryByte (t, f, block, byteOffset) ->
            // A bare LocalMemoryByte root is a single-byte cell. Wider local-memory
            // writes go through splitTrailingByteView/writeManagedByrefBytes instead.
            let byteValue =
                match updated with
                | CliType.Numeric (CliNumericType.UInt8 b) -> b
                | other -> failwith $"cannot write non-byte value %O{other} through local-memory byte root %O{block}"

            match tryReadInitializedLocalMemoryBytes state t f block byteOffset 1 with
            | ValueSome existing when byteValue = Array.exactlyOne existing -> state
            | _ -> IlMachineThreadState.writeLocalMemoryBytes t f block byteOffset [| byteValue |] state
        | ByrefRoot.HeapValue addr ->
            let contents =
                match updated with
                | CliType.ValueType contents -> contents
                | other -> failwith $"cannot write non-value-type {other} through heap value byref"

            let existing = ManagedHeap.get addr state.ManagedHeap

            if System.Object.ReferenceEquals (contents, existing.Contents) then
                state
            else
                { state with
                    ManagedHeap =
                        ManagedHeap.set
                            addr
                            { existing with
                                Contents = contents
                            }
                            state.ManagedHeap
                }
        | ByrefRoot.HeapObjectField (addr, field) ->
            let existing = ManagedHeap.get addr state.ManagedHeap
            let existingField = AllocatedNonArrayObject.DereferenceFieldById field existing

            if System.Object.ReferenceEquals (existingField, updated) then
                state
            else
                let withUpdatedField =
                    existing |> AllocatedNonArrayObject.SetFieldById field updated

                { state with
                    ManagedHeap = ManagedHeap.set addr withUpdatedField state.ManagedHeap
                }
        | ByrefRoot.ArrayElement (arr, index) ->
            let existing = IlMachineThreadState.getArrayValue arr index state

            if System.Object.ReferenceEquals (existing, updated) then
                state
            else
                state |> IlMachineThreadState.setArrayValue arr updated index
        | ByrefRoot.PeByteRange peByteRange ->
            failwith $"PE byte range is read-only; refusing to write %O{updated} through %O{peByteRange}"
        | ByrefRoot.StaticField (ty, field) ->
            match getStatic ty field state with
            | Some existing when System.Object.ReferenceEquals (existing, updated) -> state
            | _ -> state |> setStatic ty field updated
        | ByrefRoot.StringCharAt (str, charIndex) ->
            let updated =
                match updated with
                | CliType.Char (high, low) -> char (int high * 256 + int low)
                | other ->
                    // Direct same-width primitive writes, for example Stind.I2
                    // storing a UInt16 through a ref char byref, preserve the
                    // raw UTF-16 bits while normalising the stored cell to char.
                    let charTemplate = CliType.ofChar (char 0)
                    let updatedBytes = CliType.ToBytes other

                    if updatedBytes.Length <> CliType.sizeOf charTemplate then
                        failwith
                            $"string character write expected a 2-byte char-compatible value, got %d{updatedBytes.Length} bytes from %O{other}"

                    match CliType.ofBytesLike charTemplate updatedBytes with
                    | CliType.Char (high, low) -> char (int high * 256 + int low)
                    | reconstructed -> failwith $"string character write reconstructed non-char value %O{reconstructed}"

            if ManagedHeap.getStringChar str charIndex state.ManagedHeap = updated then
                state
            else
                { state with
                    ManagedHeap = ManagedHeap.setStringChar str charIndex updated state.ManagedHeap
                }

    let private readProjectedValue (rootValue : CliType) (projs : ByrefProjection list) : CliType =
        projs
        |> List.fold
            (fun value proj ->
                match proj with
                | ByrefProjection.Field field ->
                    match value with
                    | CliType.ValueType vt -> CliValueType.DereferenceFieldById field vt
                    | v -> failwith $"could not find field {field.Name} on non-ValueType {v}"
                | ByrefProjection.ReinterpretAs ty ->
                    if isSafeReinterpretPassthrough value ty then
                        value
                    else
                        failwith
                            $"TODO: read through `ReinterpretAs` from value %O{value} as type %s{ty.Namespace}.%s{ty.Name}; needs a bytewise implementation"
                | ByrefProjection.ByteOffset n ->
                    failwith
                        $"TODO: readManagedByref via ByteOffset %d{n} requires a trailing byte-view byref shape; generic Ldind at a non-normalised byte offset is not modelled (value: %O{value})"
            )
            rootValue

    let private validateByteAddressableCell (context : string) (value : CliType) : unit =
        // Keep this caller-side check even though CliType byte helpers validate too: this layer
        // can report which byref shape requested the byte view, while CliType protects direct
        // callers of the byte helpers.
        match CliType.ByteAddressability value with
        | CliByteAddressability.ByteAddressable -> ()
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"refusing byte view over %s{rejection.Description} in %s{context}. Value layout:\n%s{CliType.DescribeByteLayout None value}"

    let private byteAddressableCellSize (context : string) (value : CliType) : int =
        validateByteAddressableCell context value
        CliType.sizeOf value

    let private byteAddressableCellBytesAt (context : string) (offset : int) (count : int) (value : CliType) : byte[] =
        validateByteAddressableCell context value
        CliType.BytesAt offset count value

    let private withByteAddressableCellBytesAtIfChanged
        (context : string)
        (offset : int)
        (bytes : byte[])
        (value : CliType)
        : CliType option
        =
        validateByteAddressableCell context value
        CliType.WithBytesAtIfChanged offset bytes value

    let private splitTrailingByteView (src : ManagedPointerSource) : (ByrefRoot * ByrefProjection list * int) voption =
        match src with
        | ManagedPointerSource.Null -> ValueNone
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset n :: ByrefProjection.ReinterpretAs _ :: revPrefix ->
                ValueSome (root, List.rev revPrefix, n)
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | ByrefProjection.ReinterpretAs _ :: revPrefix -> ValueSome (root, List.rev revPrefix, 0)
            | _ -> ValueNone

    let private floorDivRem (value : int) (divisor : int) : int * int =
        if divisor <= 0 then
            failwith $"floorDivRem requires a positive divisor, got %d{divisor}"

        let q = value / divisor
        let r = value - q * divisor

        if r < 0 then q - 1, r + divisor else q, r

    let private readArrayBytesAs
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let arrObj = state.ManagedHeap.Arrays.[arr]

        if arrObj.Length = 0 then
            failwith $"TODO: byte-view read from empty array %O{arr} at index %d{index} offset %d{byteOffset}"

        let firstCellSize =
            byteAddressableCellSize $"array %O{arr} element 0" arrObj.Elements.[0]

        let cellAdvance, inCellStart = floorDivRem byteOffset firstCellSize
        let buf = Array.zeroCreate<byte> targetSize
        let mutable filled = 0
        let mutable cell = index + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < targetSize do
            if cell < 0 || cell >= arrObj.Length then
                failwith
                    $"TODO: byte-view read past array bounds at cell %d{cell} of length %d{arrObj.Length} while gathering %d{targetSize} bytes"

            let cellSize =
                byteAddressableCellSize $"array %O{arr} element %d{cell}" arrObj.Elements.[cell]

            let canTake = cellSize - inCellOffset
            let take = min canTake (targetSize - filled)

            let bytes =
                byteAddressableCellBytesAt $"array %O{arr} element %d{cell}" inCellOffset take arrObj.Elements.[cell]

            Array.blit bytes 0 buf filled take
            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        CliType.ofBytesLike targetTemplate buf

    let private readPeByteRangeBytesAs
        (state : IlMachineState)
        (peByteRange : PeByteRangePointer)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 || targetSize > peByteRange.Size - byteOffset then
            failwith
                $"PE byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside byte range size %d{peByteRange.Size}: %O{peByteRange}"

        let assembly =
            state.LoadedAssembly' peByteRange.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"PE byte-view read needs loaded assembly %s{peByteRange.AssemblyFullName}"
            )

        let sectionData =
            assembly.PeReader.GetSectionData peByteRange.RelativeVirtualAddress

        let mutable reader = sectionData.GetReader ()
        reader.Offset <- byteOffset
        let bytes = reader.ReadBytes targetSize

        CliType.ofBytesLike targetTemplate bytes

    let private readStringBytesAs
        (state : IlMachineState)
        (str : ManagedHeapAddress)
        (charIndex : int)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate
        let cellAdvance, inCellStart = floorDivRem byteOffset 2
        let buf = Array.zeroCreate<byte> targetSize
        let mutable filled = 0
        let mutable cell = charIndex + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < targetSize do
            let charBytes =
                ManagedHeap.getStringChar str cell state.ManagedHeap
                |> CliType.ofChar
                |> CliType.ToBytes

            let canTake = charBytes.Length - inCellOffset
            let take = min canTake (targetSize - filled)
            Array.blit charBytes inCellOffset buf filled take
            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        CliType.ofBytesLike targetTemplate buf

    let private heapValueForByteView
        (operation : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : AllocatedNonArrayObject
        =
        let obj = ManagedHeap.get addr state.ManagedHeap

        match CliValueType.ByteAddressability obj.Contents with
        | CliByteAddressability.ByteAddressable -> obj
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"%s{operation}: refusing byte view over boxed %s{rejection.Description} at %O{addr}. Boxed value layout:\n%s{CliValueType.DescribeByteLayout (Some state.ConcreteTypes) obj.Contents}"

    let private heapValueByteSize
        (operation : string)
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        : AllocatedNonArrayObject * int
        =
        let obj = heapValueForByteView operation state addr
        obj, CliValueType.SizeOf(obj.Contents).Size

    let private readHeapValueBytesAs
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let existing, payloadSize =
            heapValueByteSize "boxed value byte-view read" state addr

        let targetSize = CliType.sizeOf targetTemplate

        if byteOffset < 0 || targetSize > payloadSize - byteOffset then
            failwith
                $"boxed value byte-view read at offset %d{byteOffset} for %d{targetSize} bytes is outside %d{payloadSize}-byte boxed payload at %O{addr}"

        CliValueType.BytesAt byteOffset targetSize existing.Contents
        |> CliType.ofBytesLike targetTemplate

    let private readLocalMemoryBytesAs
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : LocallocBlockId)
        (byteOffset : int)
        (targetTemplate : CliType)
        : CliType
        =
        let targetSize = CliType.sizeOf targetTemplate

        let bytes =
            IlMachineThreadState.readLocalMemoryBytes thread frame block byteOffset targetSize state

        CliType.ofBytesLike targetTemplate bytes

    let readManagedByrefBytesAs
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (targetTemplate : CliType)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) -> readHeapValueBytesAs state addr 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset), []) ->
            readLocalMemoryBytesAs state thread frame block byteOffset targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            readArrayBytesAs state arr index 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, []) ->
            readPeByteRangeBytesAs state peByteRange 0 targetTemplate
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            readStringBytesAs state str charIndex 0 targetTemplate
        | ManagedPointerSource.Byref (outerRoot, outerProjs) ->
            match splitTrailingByteView src with
            | ValueSome (ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset), [], byteOffset) ->
                readLocalMemoryBytesAs state thread frame block (rootByteOffset + byteOffset) targetTemplate
            | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                readArrayBytesAs state arr index byteOffset targetTemplate
            | ValueSome (ByrefRoot.PeByteRange peByteRange, [], byteOffset) ->
                readPeByteRangeBytesAs state peByteRange byteOffset targetTemplate
            | ValueSome (ByrefRoot.PeByteRange peByteRange, prefixProjs, _) ->
                failwith $"TODO: PE byte-view read with non-empty prefix projections %O{prefixProjs}: %O{peByteRange}"
            | ValueSome (ByrefRoot.StringCharAt (str, charIndex), [], byteOffset) ->
                readStringBytesAs state str charIndex byteOffset targetTemplate
            | ValueSome (ByrefRoot.HeapValue addr, [], byteOffset) ->
                readHeapValueBytesAs state addr byteOffset targetTemplate
            | ValueSome (byteViewRoot, prefixProjs, byteOffset) ->
                let rootValue = readRootValue state byteViewRoot
                let cell = readProjectedValue rootValue prefixProjs
                let cellSize = byteAddressableCellSize $"single-cell byref %O{src}" cell
                let targetSize = CliType.sizeOf targetTemplate

                if byteOffset < 0 || targetSize > cellSize - byteOffset then
                    failwith
                        $"TODO: byte-view read at offset %d{byteOffset} for %d{targetSize} bytes does not fit in single primitive cell of size %d{cellSize}: %O{src}"

                let bytes =
                    byteAddressableCellBytesAt $"single-cell byref %O{src}" byteOffset targetSize cell

                CliType.ofBytesLike targetTemplate bytes
            | ValueNone ->
                let raw = readProjectedValue (readRootValue state outerRoot) outerProjs
                let rawSize = byteAddressableCellSize $"plain byref %O{src}" raw
                let targetSize = CliType.sizeOf targetTemplate

                if targetSize > rawSize then
                    failwith
                        $"TODO: byte-view read of %d{targetSize} bytes does not fit in plain primitive cell of size %d{rawSize}: %O{src}"

                byteAddressableCellBytesAt $"plain byref %O{src}" 0 targetSize raw
                |> CliType.ofBytesLike targetTemplate

    let readManagedByref (state : IlMachineState) (src : ManagedPointerSource) : CliType =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs ty :: _
            | ByrefProjection.ReinterpretAs ty :: _ ->
                match zeroForPrimitiveReinterpret ty with
                | ValueSome targetTemplate -> readManagedByrefBytesAs state src targetTemplate
                | ValueNone ->
                    failwith
                        $"TODO: read through `ReinterpretAs` as non-primitive type %s{ty.Namespace}.%s{ty.Name}; struct/object byte views are not modelled in PR B"
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ -> readProjectedValue (readRootValue state root) projs

    let private zeroForConcreteType
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ty : ConcreteType<ConcreteTypeHandle>)
        : CliType
        =
        let handle =
            AllConcreteTypes.findExistingConcreteType state.ConcreteTypes ty.Identity ty.Generics
            |> Option.defaultWith (fun () ->
                failwith $"ReinterpretAs target %O{ty} is not present in the concrete-type registry"
            )

        CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes handle
        |> fst

    let private readReinterpretedByrefField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (field : FieldId)
        : CliType
        =
        let targetTemplate = zeroForConcreteType baseClassTypes state reinterpretTy
        let fieldTemplate = CliType.getFieldById field targetTemplate
        let fieldOffset, fieldSize = CliType.getFieldLayoutById field targetTemplate

        match fieldTemplate with
        | CliType.ObjectRef _ ->
            match splitTrailingByteView src with
            | ValueSome (root, prefixProjs, byteOffset) ->
                let totalByteOffset = byteOffset + fieldOffset

                if totalByteOffset <> 0 then
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} starts at byte offset %d{totalByteOffset}; object-reference interior byte views are not modelled"

                if fieldSize <> CliType.sizeOf fieldTemplate then
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} has storage size %d{fieldSize}, expected %d{CliType.sizeOf fieldTemplate}"

                match readProjectedValue (readRootValue state root) prefixProjs with
                | CliType.ObjectRef _ as value -> value
                | other ->
                    failwith
                        $"TODO: object-reference field %O{field} through %O{reinterpretTy} over non-object storage %O{other}"
            | ValueNone ->
                failwith
                    $"TODO: object-reference field %O{field} through %O{reinterpretTy} without a trailing ReinterpretAs byte-view shape: %O{src}"
        | CliType.RuntimePointer _ ->
            failwith
                $"TODO: runtime-pointer field %O{field} through %O{reinterpretTy}; pointer byte views are not modelled"
        | CliType.Numeric _
        | CliType.Bool _
        | CliType.Char _
        | CliType.ValueType _ ->
            let fieldPtr =
                if fieldOffset = 0 then
                    src
                else
                    ManagedPointerSource.appendProjection (ByrefProjection.ByteOffset fieldOffset) src

            readManagedByrefBytesAs state fieldPtr fieldTemplate

    let readManagedByrefField
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (field : FieldId)
        : CliType
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (root, projs) ->
            match List.rev projs with
            | ByrefProjection.ByteOffset _ :: ByrefProjection.ReinterpretAs ty :: _
            | ByrefProjection.ReinterpretAs ty :: _ -> readReinterpretedByrefField baseClassTypes state src ty field
            | ByrefProjection.ByteOffset n :: _ ->
                failwith
                    $"ByteOffset %d{n} without a preceding ReinterpretAs in projection chain: %O{src} (this is an interpreter bug)"
            | _ ->
                readProjectedValue (readRootValue state root) projs
                |> CliType.getFieldById field

    let private applyProjectionsForWriteIfChanged
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType option
        =
        let rec go (rootValue : CliType) (projs : ByrefProjection list) (newValue : CliType) : CliType option =
            match projs with
            | [] -> Some newValue
            | [ ByrefProjection.Field field ] -> Some (CliType.withFieldSetById field newValue rootValue)
            | ByrefProjection.Field field :: rest ->
                let fieldValue = CliType.getFieldById field rootValue

                match go fieldValue rest newValue with
                | None -> None
                | Some updatedField -> Some (CliType.withFieldSetById field updatedField rootValue)
            | ByrefProjection.ReinterpretAs ty :: _ ->
                failwith
                    $"TODO: write through `ReinterpretAs` as %s{ty.Namespace}.%s{ty.Name} followed by further projections; needs a bytewise implementation"
            | ByrefProjection.ByteOffset n :: _ ->
                // Symmetric to the readManagedByref ByteOffset case: byte-offset
                // writes go through Unsafe.WriteUnaligned (which scatters bytes
                // into the cell stream directly), not through the generic write
                // fold. Reaching here means a generic Stind at a non-zero byte
                // offset, which we don't yet model.
                failwith
                    $"TODO: writeManagedByref via ByteOffset %d{n} requires the bytewise scatter implemented by Unsafe.WriteUnaligned; generic Stind at a non-zero byte offset is out of scope for this PR"

        go rootValue projs newValue

    let private writeArrayBytes
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (index : int)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let arrObj = state.ManagedHeap.Arrays.[arr]

        if arrObj.Length = 0 then
            failwith $"TODO: byte-view write to empty array %O{arr} at index %d{index} offset %d{byteOffset}"

        let firstCellSize =
            byteAddressableCellSize $"array %O{arr} element 0" arrObj.Elements.[0]

        let cellAdvance, inCellStart = floorDivRem byteOffset firstCellSize
        let mutable state = state
        let mutable filled = 0
        let mutable cell = index + cellAdvance
        let mutable inCellOffset = inCellStart

        while filled < bytes.Length do
            if cell < 0 || cell >= arrObj.Length then
                failwith $"TODO: byte-view write past array bounds at cell %d{cell} of length %d{arrObj.Length}"

            let existing = state.ManagedHeap.Arrays.[arr].Elements.[cell]

            let existingSize =
                byteAddressableCellSize $"array %O{arr} element %d{cell}" existing

            let canTake = existingSize - inCellOffset
            let take = min canTake (bytes.Length - filled)
            let cellBytes = bytes.[filled .. filled + take - 1]

            match
                withByteAddressableCellBytesAtIfChanged
                    $"array %O{arr} element %d{cell}"
                    inCellOffset
                    cellBytes
                    existing
            with
            | None -> ()
            | Some newCell -> state <- IlMachineThreadState.setArrayValue arr newCell cell state

            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        state

    let private writeLocalMemoryBytesAt
        (state : IlMachineState)
        (thread : ThreadId)
        (frame : FrameId)
        (block : LocallocBlockId)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        match tryReadInitializedLocalMemoryBytes state thread frame block byteOffset bytes.Length with
        | ValueSome existing when bytesEqual existing bytes -> state
        | _ -> IlMachineThreadState.writeLocalMemoryBytes thread frame block byteOffset bytes state

    let private writeStringBytes
        (state : IlMachineState)
        (str : ManagedHeapAddress)
        (charIndex : int)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let cellAdvance, inCellStart = floorDivRem byteOffset 2
        let mutable state = state
        let mutable filled = 0
        let mutable cell = charIndex + cellAdvance
        let mutable inCellOffset = inCellStart
        let charTemplate = CliType.ofChar (char 0)
        let cellSize = CliType.sizeOf charTemplate

        while filled < bytes.Length do
            let existingChar = ManagedHeap.getStringChar str cell state.ManagedHeap
            let canTake = cellSize - inCellOffset
            let take = min canTake (bytes.Length - filled)

            let newCellBytes =
                if inCellOffset = 0 && take = cellSize then
                    bytes.[filled .. filled + cellSize - 1]
                else
                    let existingBytes = existingChar |> CliType.ofChar |> CliType.ToBytes

                    let newCellBytes = Array.copy existingBytes
                    Array.blit bytes filled newCellBytes inCellOffset take
                    newCellBytes

            let newChar =
                match CliType.ofBytesLike charTemplate newCellBytes with
                | CliType.Char (high, low) -> char (int high * 256 + int low)
                | other -> failwith $"string byte-view write reconstructed non-char value %O{other}"

            if newChar <> existingChar then
                state <-
                    { state with
                        ManagedHeap = ManagedHeap.setStringChar str cell newChar state.ManagedHeap
                    }

            filled <- filled + take
            cell <- cell + 1
            inCellOffset <- 0

        state

    let private writeHeapValueBytes
        (state : IlMachineState)
        (addr : ManagedHeapAddress)
        (byteOffset : int)
        (bytes : byte[])
        : IlMachineState
        =
        let existing, payloadSize =
            heapValueByteSize "boxed value byte-view write" state addr

        if byteOffset < 0 || bytes.Length > payloadSize - byteOffset then
            failwith
                $"boxed value byte-view write at offset %d{byteOffset} for %d{bytes.Length} bytes is outside %d{payloadSize}-byte boxed payload at %O{addr}"

        match CliValueType.WithBytesAtIfChanged byteOffset bytes existing.Contents with
        | None -> state
        | Some updatedContents ->
            let updated =
                { existing with
                    Contents = updatedContents
                }

            { state with
                ManagedHeap = ManagedHeap.set addr updated state.ManagedHeap
            }

    let writeManagedByrefBytes
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        let bytes = CliType.ToBytes newValue

        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, []) -> writeHeapValueBytes state addr 0 bytes
        | ManagedPointerSource.Byref (ByrefRoot.LocalMemoryByte (thread, frame, block, byteOffset), []) ->
            writeLocalMemoryBytesAt state thread frame block byteOffset bytes
        | ManagedPointerSource.Byref (ByrefRoot.PeByteRange peByteRange, _) ->
            failwith
                $"PE byte range is read-only; refusing byte-view write of %d{bytes.Length} bytes through %O{peByteRange}"
        | ManagedPointerSource.Byref (ByrefRoot.StringCharAt (str, charIndex), []) ->
            writeStringBytes state str charIndex 0 bytes
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []) ->
            writeArrayBytes state arr index 0 bytes
        | ManagedPointerSource.Byref (outerRoot, outerProjs) ->
            match splitTrailingByteView src with
            | ValueSome (ByrefRoot.LocalMemoryByte (thread, frame, block, rootByteOffset), [], byteOffset) ->
                writeLocalMemoryBytesAt state thread frame block (rootByteOffset + byteOffset) bytes
            | ValueSome (ByrefRoot.ArrayElement (arr, index), [], byteOffset) ->
                writeArrayBytes state arr index byteOffset bytes
            | ValueSome (ByrefRoot.StringCharAt (str, charIndex), [], byteOffset) ->
                writeStringBytes state str charIndex byteOffset bytes
            | ValueSome (ByrefRoot.HeapValue addr, [], byteOffset) -> writeHeapValueBytes state addr byteOffset bytes
            | ValueSome (byteViewRoot, prefixProjs, byteOffset) ->
                let rootValue = readRootValue state byteViewRoot
                let cell = readProjectedValue rootValue prefixProjs
                let cellSize = byteAddressableCellSize $"single-cell byref %O{src}" cell

                if byteOffset < 0 || bytes.Length > cellSize - byteOffset then
                    failwith
                        $"TODO: byte-view write at offset %d{byteOffset} for %d{bytes.Length} bytes does not fit in single primitive cell of size %d{cellSize}: %O{src}"

                match withByteAddressableCellBytesAtIfChanged $"single-cell byref %O{src}" byteOffset bytes cell with
                | None -> state
                | Some updatedCell ->
                    match applyProjectionsForWriteIfChanged rootValue prefixProjs updatedCell with
                    | None -> state
                    | Some updatedRoot -> writeRootValue state byteViewRoot updatedRoot
            | ValueNone ->
                let rootValue = readRootValue state outerRoot
                let cell = readProjectedValue rootValue outerProjs
                let cellSize = byteAddressableCellSize $"plain byref %O{src}" cell

                if bytes.Length > cellSize then
                    failwith
                        $"TODO: byte-view write of %d{bytes.Length} bytes does not fit in plain primitive cell of size %d{cellSize}: %O{src}"

                match withByteAddressableCellBytesAtIfChanged $"plain byref %O{src}" 0 bytes cell with
                | None -> state
                | Some updatedCell ->
                    match applyProjectionsForWriteIfChanged rootValue outerProjs updatedCell with
                    | None -> state
                    | Some updatedRoot -> writeRootValue state outerRoot updatedRoot

    let private splitFirstReinterpret
        (projs : ByrefProjection list)
        : (ByrefProjection list * ConcreteType<ConcreteTypeHandle> * ByrefProjection list) option
        =
        let rec loop (revPrefix : ByrefProjection list) (remaining : ByrefProjection list) =
            match remaining with
            | [] -> None
            | ByrefProjection.ReinterpretAs ty :: rest -> Some (List.rev revPrefix, ty, rest)
            | proj :: rest -> loop (proj :: revPrefix) rest

        loop [] projs

    let private describeCliStorage (state : IlMachineState) (value : CliType) : string =
        CliType.DescribeByteLayout (Some state.ConcreteTypes) value

    let private reinterpretStorageBytes
        (state : IlMachineState)
        (operation : string)
        (storageValue : CliType)
        : byte[]
        =
        match CliType.ByteAddressability storageValue with
        | CliByteAddressability.ByteAddressable -> CliType.ToBytes storageValue
        | CliByteAddressability.Rejected rejection ->
            failwith
                $"TODO: %s{operation}: write through `ReinterpretAs` over byte-unaddressable storage (%s{rejection.Description}) is not modelled; storage layout:\n%s{describeCliStorage state storageValue}"

    let private ofBytesLikeForReinterpret
        (state : IlMachineState)
        (operation : string)
        (storageValue : CliType)
        (bytes : byte[])
        : CliType
        =
        try
            CliType.ofBytesLike storageValue bytes
        with ex ->
            failwith
                $"%s{operation}: failed to reconstruct storage from reinterpreted bytes. Reinterpret writes into unrepresented padding are not modelled. Storage layout:\n%s{describeCliStorage state storageValue}\nInner error: %s{ex.Message}"

    let private splitTrailingPrefixByteOffset (projs : ByrefProjection list) : ByrefProjection list * int =
        match List.rev projs with
        | ByrefProjection.ByteOffset n :: revPrefix -> List.rev revPrefix, n
        | _ -> projs, 0

    let rec private writeProjectedValueIfChanged
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (rootValue : CliType)
        (projs : ByrefProjection list)
        (newValue : CliType)
        : CliType option
        =
        match baseClassTypes, splitFirstReinterpret projs with
        | Some baseClassTypes, Some (prefixProjs, reinterpretTy, reinterpretProjs) ->
            let storageProjs, byteOffset = splitTrailingPrefixByteOffset prefixProjs
            let storageValue = readProjectedValue rootValue storageProjs

            match
                writeReinterpretedStorageIfChanged
                    baseClassTypes
                    state
                    storageValue
                    byteOffset
                    reinterpretTy
                    reinterpretProjs
                    newValue
            with
            | None -> None
            | Some updatedStorage -> applyProjectionsForWriteIfChanged rootValue storageProjs updatedStorage
        | _ -> applyProjectionsForWriteIfChanged rootValue projs newValue

    and private writeReinterpretedStorageIfChanged
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (storageValue : CliType)
        (byteOffset : int)
        (reinterpretTy : ConcreteType<ConcreteTypeHandle>)
        (reinterpretProjs : ByrefProjection list)
        (newValue : CliType)
        : CliType option
        =
        // Reinterpret writes are byte updates to the original storage shape. This covers patterns
        // such as `Unsafe.As<bool, VolatileBoolean>(ref location).Value = value`, and recurses for
        // nested `Unsafe.As` chains before rebuilding the original cell.
        let operation =
            $"write through `ReinterpretAs` as %s{reinterpretTy.Namespace}.%s{reinterpretTy.Name}"

        let storageBytes = reinterpretStorageBytes state operation storageValue
        let reinterpretZero = zeroForConcreteType baseClassTypes state reinterpretTy
        let reinterpretSize = CliType.sizeOf reinterpretZero

        if byteOffset < 0 || reinterpretSize > storageBytes.Length - byteOffset then
            failwith
                $"TODO: %s{operation} requires %d{reinterpretSize} bytes at offset %d{byteOffset}, but storage has %d{storageBytes.Length} bytes. Storage layout:\n%s{describeCliStorage state storageValue}"

        let reinterpretBytes = storageBytes.[byteOffset .. byteOffset + reinterpretSize - 1]

        let reinterpretTemplate =
            ofBytesLikeForReinterpret state operation reinterpretZero reinterpretBytes

        match
            writeProjectedValueIfChanged (Some baseClassTypes) state reinterpretTemplate reinterpretProjs newValue
        with
        | None -> None
        | Some updatedReinterpret ->
            let updatedBytes = CliType.ToBytes updatedReinterpret

            if updatedBytes.Length <> reinterpretSize then
                failwith
                    $"TODO: %s{operation} produced %d{updatedBytes.Length} bytes for reinterpret type %O{reinterpretTy}, expected %d{reinterpretSize}. Storage layout:\n%s{describeCliStorage state storageValue}"

            if bytesEqual updatedBytes reinterpretBytes then
                None
            else
                let updatedStorageBytes = Array.copy storageBytes
                Array.blit updatedBytes 0 updatedStorageBytes byteOffset updatedBytes.Length
                Some (ofBytesLikeForReinterpret state operation storageValue updatedStorageBytes)

    let private writeManagedByrefCore
        (baseClassTypes : BaseClassTypes<DumpedAssembly> option)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        match src with
        | ManagedPointerSource.Null -> failwith "TODO: throw NullReferenceException"
        | ManagedPointerSource.Byref (root, []) -> writeRootValue state root newValue
        | ManagedPointerSource.Byref (root, projs) ->
            match splitTrailingByteView src with
            | ValueSome _ -> writeManagedByrefBytes state src newValue
            | ValueNone ->
                let rootValue = readRootValue state root

                match writeProjectedValueIfChanged baseClassTypes state rootValue projs newValue with
                | None -> state
                | Some updatedRoot -> writeRootValue state root updatedRoot

    let writeManagedByref (state : IlMachineState) (src : ManagedPointerSource) (newValue : CliType) : IlMachineState =
        // Call sites that can supply BaseClassTypes should use writeManagedByrefWithBase so
        // non-trailing ReinterpretAs projections can be applied bytewise. This legacy entry point
        // remains for primitive/external boundaries that do not currently carry type metadata.
        writeManagedByrefCore None state src newValue

    let writeManagedByrefWithBase
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (src : ManagedPointerSource)
        (newValue : CliType)
        : IlMachineState
        =
        writeManagedByrefCore (Some baseClassTypes) state src newValue
