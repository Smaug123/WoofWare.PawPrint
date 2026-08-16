namespace WoofWare.PawPrint

open System.Collections.Immutable


type ManagedHeap =
    {
        NonArrayObjects : Map<ManagedHeapAddress, AllocatedNonArrayObject>
        Arrays : Map<ManagedHeapAddress, AllocatedArray>
        FirstAvailableAddress : int
        /// Strings are special-cased in the runtime anyway and have a whole lot of unsafe code in them,
        /// so we'll have a special pool for their bytes.
        StringArrayData : ImmutableArray<char>
        /// Side-table mapping a String object's address to its full character content.
        /// The managed representation of a String only carries `_stringLength` as a
        /// regular field; the chars (including the metadata-level `_firstChar`) live
        /// in `StringArrayData` and are projected via `RuntimeFieldProjection`. We
        /// record the canonical text here at allocation time so operations like
        /// `String.Equals` can compare full contents without re-reading the byte view.
        StringContents : ImmutableDictionary<ManagedHeapAddress, string>
        /// Side-table mapping a String object's address to the first character's index in
        /// `StringArrayData`. `_firstChar` and byref/trailing-data reads both walk
        /// from this offset.
        StringDataOffsets : ImmutableDictionary<ManagedHeapAddress, int>
        /// Object headers, keyed by address. In CoreCLR every heap object carries an
        /// `ObjHeader` immediately preceding its payload — arrays and strings included
        /// (`src/coreclr/vm/object.h`); `Object::EnterObjMonitor` and friends are defined
        /// once on the base `Object` and there is no array-shaped carve-out. The sync
        /// block therefore belongs to the *address*, not to which of `NonArrayObjects` /
        /// `Arrays` holds the payload, so it lives here rather than as a field of either
        /// payload record. Keeping it out of the payload records also means a fresh
        /// allocation can never inherit a stale header: `Array.Clone` reuses the source's
        /// `AllocatedArray` record verbatim (see `IlMachineThreadState.cloneArray`), which
        /// would otherwise copy the source's monitor ownership onto the clone.
        ///
        /// Invariant: the key set is exactly the union of the `NonArrayObjects` and
        /// `Arrays` key sets. `allocateNonArray` / `allocateArray` are the only ways to
        /// mint an address and both register an `Empty` entry here.
        SyncBlocks : Map<ManagedHeapAddress, SyncBlock>
    }

[<RequireQualifiedAccess>]
module ManagedHeap =
    let empty : ManagedHeap =
        {
            NonArrayObjects = Map.empty
            FirstAvailableAddress = 1
            Arrays = Map.empty
            StringArrayData = ImmutableArray.Empty
            StringContents = ImmutableDictionary.Empty
            StringDataOffsets = ImmutableDictionary.Empty
            SyncBlocks = Map.empty
        }

    /// The object header of the object at `addr`. Every live heap object has one,
    /// whatever its payload kind; an address with no header is not a live allocation.
    let getSyncBlock (addr : ManagedHeapAddress) (heap : ManagedHeap) : SyncBlock =
        match heap.SyncBlocks.TryGetValue addr with
        | false, _ -> failwith $"getSyncBlock: %O{addr} is not a live managed heap allocation, so has no object header"
        | true, v -> v

    /// Overwrite the object header of the object at `addr`. Rejects addresses that were
    /// never allocated rather than conjuring a header for them: a caller reaching here
    /// with a dangling address has a bug we would otherwise hide.
    let setSyncBlock (addr : ManagedHeapAddress) (syncValue : SyncBlock) (heap : ManagedHeap) : ManagedHeap =
        if not (heap.SyncBlocks.ContainsKey addr) then
            failwith $"setSyncBlock: %O{addr} is not a live managed heap allocation, so has no object header"

        { heap with
            SyncBlocks = heap.SyncBlocks |> Map.add addr syncValue
        }

    /// Every object whose monitor is currently `Held` by `thread`, with the ownership
    /// state, in ascending address order.
    ///
    /// Ownership is a property of the object header rather than of any object's payload,
    /// so this is monitor bookkeeping and not a read of guest memory.
    let syncBlocksHeldBy (thread : ThreadId) (heap : ManagedHeap) : (ManagedHeapAddress * LockedSyncBlock) list =
        // Folded rather than `Map.toList |> List.choose`, which would allocate a tuple and a
        // cons cell for every object on the heap before discarding all but the matches — and
        // a terminating thread almost never holds any. `Map.foldBack` visits keys in
        // descending order, so consing during the fold produces the ascending list directly
        // and allocates only for matches. Same reasoning as `Scheduler.runnableThreads`.
        (heap.SyncBlocks, [])
        ||> Map.foldBack (fun addr syncBlock acc ->
            match syncBlock.Lock with
            | SyncBlockLock.Held locked when locked.LockingThread = thread -> (addr, locked) :: acc
            | SyncBlockLock.Held _
            | SyncBlockLock.Free -> acc
        )

    /// Every (object, thread) pair where `thread` is parked in the object's `WaitQueue`
    /// from a `Monitor.Wait`, in ascending address order and then in wait-queue (FIFO)
    /// order within each object.
    ///
    /// The ordering lives here rather than at the call sites because it matters:
    /// `SyncBlockMonitor.applySpuriousWakeups` folds a wake over this list, and a
    /// different enumeration order would give a different interleaving for the same seed.
    /// Objects with an empty wait queue contribute nothing.
    let syncBlockWaiters (heap : ManagedHeap) : (ManagedHeapAddress * ThreadId) list =
        // Folded rather than `Map.toList |> List.collect`, for the same reason as
        // `syncBlocksHeldBy`: most objects have no waiters, so the intermediate would be one
        // entry per object on the heap to produce a handful. The inner `List.foldBack`
        // preserves each queue's FIFO order while prepending it.
        (heap.SyncBlocks, [])
        ||> Map.foldBack (fun addr syncBlock acc ->
            (syncBlock.WaitQueue, acc)
            ||> List.foldBack (fun (tid, _) acc -> (addr, tid) :: acc)
        )

    /// Allocate `ty` at a fresh address.
    ///
    /// The element-type facts on `ty.Shape` are checked here rather than trusted. They are
    /// the parts of `ArrayShape` that duplicate information also derivable from the cells,
    /// and this is the single chokepoint through which every array reaches the heap —
    /// including `cloneArray`, which reuses a source record verbatim. Callers read them
    /// instead of measuring a cell precisely so that they never touch guest memory to learn
    /// them, which would be worthless if the recorded values could be wrong.
    ///
    /// Cell 0 is checked by *size* only. A cell's CLI shape legitimately drifts from
    /// `ElementZero`'s — an `IntPtr[]` slot holding a `TypeHandlePtr` — whereas its width
    /// never may, since the array's layout depends on it.
    let allocateArray (ty : AllocatedArray) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        // Strictly, the element-zero check below subsumes this one: it forces the stride to
        // equal a `CliType.sizeOf`, which is never less than 1. This stays as a direct
        // assertion of the property consumers actually depend on — `floorDivRem` divides by
        // the stride — rather than leaving that to a two-step argument across two files. It
        // also names the specific failure, which the size-mismatch message would not.
        if ty.Shape.ElementStride <= 0 then
            failwith
                $"allocateArray: array of %O{ty.Shape.ConcreteType} declares a non-positive element stride %d{ty.Shape.ElementStride}; every CLI type occupies at least one byte"

        let zeroSize = CliType.sizeOf ty.Shape.ElementZero

        if zeroSize <> ty.Shape.ElementStride then
            failwith
                $"allocateArray: array of %O{ty.Shape.ConcreteType} declares element stride %d{ty.Shape.ElementStride} but its element zero %O{ty.Shape.ElementZero} measures %d{zeroSize}; the stride is by definition the size of the element zero"

        if not ty.Elements.IsEmpty then
            let cellSize = CliType.sizeOf ty.Elements.[0]

            if cellSize <> ty.Shape.ElementStride then
                failwith
                    $"allocateArray: array of %O{ty.Shape.ConcreteType} declares element stride %d{ty.Shape.ElementStride} but its first cell measures %d{cellSize}; the stride must be the element type's size, so one of the two is wrong"

        // `getArrayValue` bounds-checks the index against the *shape* and then indexes the
        // *cells*, which is only sound while the two agree. Splitting the shape out of the
        // payload record is what made it possible for them not to.
        if ty.Elements.Length <> ty.Shape.Length then
            failwith
                $"allocateArray: array of %O{ty.Shape.ConcreteType} declares length %d{ty.Shape.Length} but carries %d{ty.Elements.Length} cell(s)"

        // `Length` is documented as the product of `Lengths`, and the multi-dimensional
        // accessors rely on it being so: `UnaryMetadataCallOps`' array `Get`/`Set`/`Address`
        // bounds-check each index against `Lengths`, flatten to a row-major offset, and hand
        // that to `getArrayValue`, which bounds-checks against `Length`. If the two disagree,
        // an index every per-dimension check accepts can still fall outside the cells.
        //
        // Accumulated in int64 so that a caller's overflowing dimensions are reported here
        // rather than wrapping into an apparently-agreeing product. Callers that build a
        // length from guest input reject the overflow themselves, with the guest exception
        // CoreCLR raises; see `allocateMultiDimArray`.
        let product = ty.Shape.Lengths |> Seq.fold (fun acc d -> acc * int64<int> d) 1L

        if product <> int64<int> ty.Shape.Length then
            failwith
                $"allocateArray: array of %O{ty.Shape.ConcreteType} declares length %d{ty.Shape.Length} but its per-dimension lengths multiply to %d{product}"

        let addr = heap.FirstAvailableAddress

        let heap =
            { heap with
                FirstAvailableAddress = heap.FirstAvailableAddress + 1
                Arrays = heap.Arrays |> Map.add (ManagedHeapAddress addr) ty
                SyncBlocks = heap.SyncBlocks |> Map.add (ManagedHeapAddress addr) SyncBlock.Empty
            }

        ManagedHeapAddress addr, heap

    let tryGetObjectConcreteType (alloc : ManagedHeapAddress) (heap : ManagedHeap) : ConcreteTypeHandle option =
        match heap.NonArrayObjects.TryGetValue alloc with
        | true, obj -> Some obj.ConcreteType
        | false, _ ->
            match heap.Arrays.TryGetValue alloc with
            | true, arr -> Some arr.Shape.ConcreteType
            | false, _ -> None

    let getObjectConcreteType (alloc : ManagedHeapAddress) (heap : ManagedHeap) : ConcreteTypeHandle =
        match tryGetObjectConcreteType alloc heap with
        | Some concreteType -> concreteType
        | None -> failwith $"Could not find managed heap object at address %O{alloc}"

    let allocateString (len : int) (heap : ManagedHeap) : int * ManagedHeap =
        let addr = heap.StringArrayData.Length

        let heap =
            { heap with
                // strings are also null-terminated
                // https://github.com/dotnet/runtime/blob/ab105b51f8b50ec5567d7cfe9001ca54dd6f64c3/src/libraries/System.Private.CoreLib/src/System/String.cs#L56
                StringArrayData = heap.StringArrayData.AddRange (Seq.replicate (len + 1) (char 0))
            }

        addr, heap

    let setStringData (addr : int) (contents : string) (heap : ManagedHeap) : ManagedHeap =
        let newArr = heap.StringArrayData.ToBuilder ()

        for i = 0 to contents.Length - 1 do
            newArr.[addr + i] <- contents.[i]

        let heap =
            { heap with
                StringArrayData = newArr.ToImmutable ()
            }

        heap

    let allocateNonArray (ty : AllocatedNonArrayObject) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        let addr = heap.FirstAvailableAddress

        let heap =
            { heap with
                FirstAvailableAddress = addr + 1
                NonArrayObjects = heap.NonArrayObjects |> Map.add (ManagedHeapAddress addr) ty
                SyncBlocks = heap.SyncBlocks |> Map.add (ManagedHeapAddress addr) SyncBlock.Empty
            }

        ManagedHeapAddress addr, heap

    /// Record the full character content of a string object located at `addr`, so that
    /// string-level operations (equality, hashing, etc.) can read it back.
    let recordStringContents (addr : ManagedHeapAddress) (contents : string) (heap : ManagedHeap) : ManagedHeap =
        { heap with
            StringContents = heap.StringContents.SetItem (addr, contents)
        }

    /// Record where a string object's trailing UTF-16 data starts in `StringArrayData`.
    let recordStringDataOffset (addr : ManagedHeapAddress) (dataOffset : int) (heap : ManagedHeap) : ManagedHeap =
        { heap with
            StringDataOffsets = heap.StringDataOffsets.SetItem (addr, dataOffset)
        }

    /// Retrieve the character content of a string object previously registered via
    /// `recordStringContents`.  Returns None if no content was recorded (which indicates
    /// a string that was allocated without using the standard allocation path, or a
    /// non-string address).
    let getStringContents (addr : ManagedHeapAddress) (heap : ManagedHeap) : string option =
        match heap.StringContents.TryGetValue addr with
        | true, s -> Some s
        | false, _ -> None

    /// The index in `StringArrayData` of the first character of the string object at
    /// `addr`, or None if none was recorded — a string allocated off the standard path,
    /// or an address that is not a string at all.
    let tryGetStringDataOffset (addr : ManagedHeapAddress) (heap : ManagedHeap) : int option =
        match heap.StringDataOffsets.TryGetValue addr with
        | true, offset -> Some offset
        | false, _ -> None

    let getStringDataOffset (addr : ManagedHeapAddress) (heap : ManagedHeap) : int =
        match tryGetStringDataOffset addr heap with
        | Some offset -> offset
        | None -> failwith $"string data offset for %O{addr} was not recorded"

    let private requireStringContents (operation : string) (addr : ManagedHeapAddress) (heap : ManagedHeap) : string =
        match getStringContents addr heap with
        | Some contents -> contents
        | None -> failwith $"%s{operation}: string contents for %O{addr} were not recorded"

    /// Update a character in the runtime string data side-table. `charIndex` equal
    /// to the string length addresses the null terminator; that updates
    /// `StringArrayData` but not the logical `StringContents` value.
    let setStringChar (addr : ManagedHeapAddress) (charIndex : int) (value : char) (heap : ManagedHeap) : ManagedHeap =
        if charIndex < 0 then
            failwith $"string character index must be non-negative, got %d{charIndex} for %O{addr}"

        let contents = requireStringContents "setStringChar" addr heap

        if charIndex > contents.Length then
            failwith
                $"string character index %d{charIndex} is beyond the null terminator of string %O{addr} with length %d{contents.Length}"

        let dataOffset = getStringDataOffset addr heap
        let newArr = heap.StringArrayData.ToBuilder ()
        // The metadata-level `_firstChar` field is a synthetic projection over
        // `StringArrayData[dataOffset]` (see `RuntimeFieldProjection`) and therefore
        // requires no separate mirror.
        newArr.[dataOffset + charIndex] <- value

        let heap =
            { heap with
                StringArrayData = newArr.ToImmutable ()
            }

        if charIndex < contents.Length then
            let chars = contents.ToCharArray ()
            chars.[charIndex] <- value

            { heap with
                StringContents = heap.StringContents.SetItem (addr, System.String chars)
            }
        else
            heap

    /// Read a character from the runtime string data side-table. `charIndex` equal
    /// to the string length addresses the null terminator, matching CoreCLR's
    /// string layout; larger offsets would walk into unrelated string storage and
    /// are rejected.
    let getStringChar (addr : ManagedHeapAddress) (charIndex : int) (heap : ManagedHeap) : char =
        if charIndex < 0 then
            failwith $"string character index must be non-negative, got %d{charIndex} for %O{addr}"

        let contents = requireStringContents "getStringChar" addr heap

        if charIndex > contents.Length then
            failwith
                $"string character index %d{charIndex} is beyond the null terminator of string %O{addr} with length %d{contents.Length}"

        let dataOffset = getStringDataOffset addr heap
        heap.StringArrayData.[dataOffset + charIndex]

    /// Value-level equality between two managed string objects addressed by `a1` and `a2`.
    /// Mirrors the semantics of System.String.Equals(string, string): null-aware, reference
    /// equal implies equal, otherwise compares full character contents.
    /// Fails if either address is not a known string and the two addresses are distinct
    /// (i.e., the character content is needed to answer).
    let stringsEqual (a1 : ManagedHeapAddress) (a2 : ManagedHeapAddress) (heap : ManagedHeap) : bool =
        if a1 = a2 then
            true
        else
            match getStringContents a1 heap, getStringContents a2 heap with
            | Some s1, Some s2 -> s1 = s2
            | None, _
            | _, None ->
                failwith
                    $"stringsEqual: one or both addresses %O{a1}, %O{a2} are not registered strings; cannot compare contents"

    /// Whether `addr` is a live array. False for a live non-array object and for an
    /// address that was never allocated; use `tryGetObjectConcreteType` to tell those apart.
    let isArray (addr : ManagedHeapAddress) (heap : ManagedHeap) : bool = heap.Arrays.ContainsKey addr

    /// Whether `addr` is a live heap allocation of either kind.
    ///
    /// Asked by callers that only need to know a reference points at *something* — the
    /// object's kind and payload are then somebody else's question.
    let isLive (addr : ManagedHeapAddress) (heap : ManagedHeap) : bool =
        // Deliberately derived from the two payload maps rather than from `SyncBlocks`, whose
        // key set is documented to be their union: that invariant is asserted elsewhere, and
        // an accessor that assumed it would turn a broken invariant into a wrong answer
        // instead of a caught one.
        heap.NonArrayObjects.ContainsKey addr || heap.Arrays.ContainsKey addr

    /// The dimensions and element type of the array at `addr`, or `None` if `addr` is not
    /// a live array. Carries no cells: see `ArrayShape`.
    let tryGetArrayShape (addr : ManagedHeapAddress) (heap : ManagedHeap) : ArrayShape option =
        match heap.Arrays.TryGetValue addr with
        | true, arr -> Some arr.Shape
        | false, _ -> None

    /// The dimensions and element type of the array at `addr`. Carries no cells: see
    /// `ArrayShape`.
    ///
    /// The two rejection cases are reported differently because they are different bugs:
    /// a non-array address means the caller misjudged the type of the reference it was
    /// handed, whereas an unallocated address means the reference itself is bogus.
    let getArrayShape (addr : ManagedHeapAddress) (heap : ManagedHeap) : ArrayShape =
        match tryGetArrayShape addr heap with
        | Some shape -> shape
        | None ->
            if heap.NonArrayObjects.ContainsKey addr then
                failwith $"getArrayShape: %O{addr} is not an array, so has no array shape"
            else
                failwith $"getArrayShape: %O{addr} is not a live managed heap allocation, so has no array shape"

    /// The byte distance between consecutive cells of the array at `addr`. Well defined for
    /// an empty array, and never a read of a cell: see `ArrayShape.ElementStride`.
    let getArrayElementStride (addr : ManagedHeapAddress) (heap : ManagedHeap) : int =
        (getArrayShape addr heap).ElementStride

    /// The zero value of the element type of the array at `addr`: the witness for questions
    /// about what shape a cell of this array has. Well defined for an empty array, and never
    /// a read of a cell — which is the point; see `ArrayShape.ElementZero`. Use
    /// `getArrayValue` when you want to know what a cell actually holds.
    let getArrayElementZero (addr : ManagedHeapAddress) (heap : ManagedHeap) : CliType =
        (getArrayShape addr heap).ElementZero

    let getArrayValue (alloc : ManagedHeapAddress) (offset : int) (heap : ManagedHeap) : CliType =
        match heap.Arrays.TryGetValue alloc with
        | false, _ -> failwith $"TODO: array not on heap (no array registered at %O{alloc})"
        | true, arr ->

        if offset < 0 then
            failwith
                $"TODO: raise IndexOutOfRangeException: negative array index %d{offset} on array at %O{alloc} (length %d{arr.Shape.Length}). A negative index here typically means a byref obtained via `RawData::Data` on an array was read without first applying the canonical `+sizeof(nint)` skip past the length-header region; if you intended to read the length, use `RawArrayData::Length` instead."
        elif offset >= arr.Shape.Length then
            failwith
                $"TODO: raise IndexOutOfRangeException: array index %d{offset} >= length %d{arr.Shape.Length} on array at %O{alloc}"

        arr.Elements.[offset]

    /// The non-array object at `addr`, or `None` if there is no live non-array object there.
    /// `None` for a live *array* too — an array has no `AllocatedNonArrayObject` payload;
    /// `isArray` and `tryGetArrayShape` answer for those.
    let tryGet (alloc : ManagedHeapAddress) (heap : ManagedHeap) : AllocatedNonArrayObject option =
        match heap.NonArrayObjects.TryGetValue alloc with
        | true, v -> Some v
        | false, _ -> None

    /// The non-array object at `addr`.
    ///
    /// The two rejection cases are reported differently for the same reason `getArrayShape`
    /// separates them: an array address means the caller misjudged the kind of the reference
    /// it was handed, whereas an unallocated address means the reference itself is bogus.
    let get (alloc : ManagedHeapAddress) (heap : ManagedHeap) : AllocatedNonArrayObject =
        match tryGet alloc heap with
        | Some v -> v
        | None ->
            if heap.Arrays.ContainsKey alloc then
                failwith $"get: %O{alloc} is an array, so has no non-array object payload"
            else
                failwith $"get: %O{alloc} is not a live managed heap allocation"

    /// Allocate a fresh array object that is a shallow copy of the array at `source`: same
    /// element type, same rank, same per-dimension lengths, and the same element values.
    /// `CliType` cells are immutable, so sharing the backing `ImmutableArray` gives exactly
    /// the shallow-copy semantics `System.Array.Clone` promises: a later write through
    /// either array replaces only that array's cell, while reference-typed elements continue
    /// to name the same heap objects from both arrays.
    ///
    /// The clone gets a fresh address, and with it a fresh `SyncBlock`: the source's
    /// monitor state belongs to the source's identity, not to its contents.
    ///
    /// The two rejection cases are reported differently because they are different bugs: a
    /// non-array address means the caller misjudged the kind of the reference it was handed,
    /// whereas an unallocated address means the reference itself is bogus.
    let cloneArray (source : ManagedHeapAddress) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        match heap.Arrays.TryGetValue source with
        // Reuses the source `AllocatedArray` wholesale, so the clone is *identical* rather
        // than merely equivalent — there is no second construction of the shape to get wrong.
        | true, arr -> allocateArray arr heap
        | false, _ ->
            if heap.NonArrayObjects.ContainsKey source then
                failwith $"cloneArray: %O{source} is a non-array object, so has no array to clone"
            else
                failwith $"cloneArray: %O{source} is not a live managed heap allocation, so has no array to clone"

    /// Replace the entire payload of the non-array object at `alloc`. Rejects an address
    /// that is not already a live non-array object rather than conjuring one there:
    /// `allocateNonArray` is the only way to mint an address, and an object minted here
    /// instead would have no entry in `SyncBlocks`, breaking the invariant that
    /// `getSyncBlock` is total over live addresses.
    let set (alloc : ManagedHeapAddress) (v : AllocatedNonArrayObject) (heap : ManagedHeap) : ManagedHeap =
        // TODO: arrays too
        if not (heap.NonArrayObjects.ContainsKey alloc) then
            failwith $"set: %O{alloc} is not a live managed heap allocation of non-array kind"

        { heap with
            NonArrayObjects = heap.NonArrayObjects |> Map.add alloc v
        }

    /// Store `value` into the field `field` of the non-array object at `alloc`, leaving
    /// that object's other fields, and every other object, untouched.
    ///
    /// This is the single read-modify-write primitive for a field store, so that a field
    /// store is one identifiable operation on the heap rather than a pattern that call
    /// sites open-code. `stfld` and delegate construction both route through it.
    ///
    /// The two rejection cases are reported differently because they are different bugs:
    /// an array address means the caller misjudged the type of the reference it was
    /// handed, whereas an unallocated address means the reference itself is bogus.
    let setFieldById
        (alloc : ManagedHeapAddress)
        (field : FieldId)
        (value : CliType)
        (heap : ManagedHeap)
        : ManagedHeap
        =
        match heap.NonArrayObjects.TryGetValue alloc with
        | true, obj -> set alloc (AllocatedNonArrayObject.SetFieldById field value obj) heap
        | false, _ ->
            if heap.Arrays.ContainsKey alloc then
                failwith
                    $"setFieldById: %O{alloc} is an array, so has no field %O{field} to store to; a field store reached an array reference"
            else
                failwith
                    $"setFieldById: %O{alloc} is not a live managed heap allocation, so has no field %O{field} to store to"

    let setArrayValue (alloc : ManagedHeapAddress) (offset : int) (v : CliType) (heap : ManagedHeap) : ManagedHeap =
        let newArrs =
            heap.Arrays
            |> Map.change
                alloc
                (fun arr ->
                    match arr with
                    | None -> failwith $"tried to change element of nonexistent array at %O{alloc}"
                    | Some arr ->
                        if offset < 0 then
                            failwith
                                $"TODO: raise IndexOutOfRangeException: negative array index %d{offset} on array at %O{alloc} (length %d{arr.Shape.Length}). A negative index here typically means a byref obtained via `RawData::Data` on an array was written without first applying the canonical `+sizeof(nint)` skip past the length-header region."
                        elif offset >= arr.Shape.Length then
                            failwith
                                $"TODO: raise IndexOutOfRangeException: array index %d{offset} >= length %d{arr.Shape.Length} on array at %O{alloc}"

                        { arr with
                            Elements = arr.Elements.SetItem (offset, v)
                        }
                        |> Some
                )

        { heap with
            Arrays = newArrs
        }

/// Read-only introspection of the heap by code that is *not the running guest*: the
/// debugger server, crash reporting, and tests.
///
/// The split from `ManagedHeap` is by *caller identity*, not by what is read. A debugger
/// dumping an array reads exactly the cells `ManagedHeap.getArrayValue` reads; the
/// difference is that the guest did not ask for them, and so a race detector must not
/// treat them as an access by any thread. Keeping the two sets of functions in separate
/// modules means that boundary is visible at every call site and mechanical to act on:
/// when heap accesses come to emit events, `ManagedHeap`'s functions are the ones that
/// emit and `HeapObserver`'s are the ones that deliberately do not.
///
/// Consequently these functions must stay pure reads. Anything the interpreter proper
/// needs belongs in `ManagedHeap`, even if a test is its only current caller.
///
/// Several of these mirror a `ManagedHeap` function of the same name and answer exactly
/// the same question. That is not accidental duplication: the two must stay distinct
/// *functions* precisely so that one can come to emit an access event and the other cannot.
/// For the same reason they read the representation directly rather than delegating to
/// their `ManagedHeap` counterpart, which would route an observer's read straight back
/// through the emitting path. `HeapObserver mirrors agree with their ManagedHeap
/// counterparts` pins them together.
[<RequireQualifiedAccess>]
module HeapObserver =
    /// The number of live non-array objects, arrays excluded.
    let nonArrayObjectCount (heap : ManagedHeap) : int = heap.NonArrayObjects.Count

    /// The number of live arrays.
    let arrayCount (heap : ManagedHeap) : int = heap.Arrays.Count

    /// The number of objects with recorded string content. Not the number of live
    /// `System.String` instances as such: it counts exactly those registered via
    /// `ManagedHeap.recordStringContents`.
    let stringContentCount (heap : ManagedHeap) : int = heap.StringContents.Count

    /// The address the next allocation will be given. Exposed for tests that need to name
    /// an address which is guaranteed *not* to be live.
    let nextAddress (heap : ManagedHeap) : int = heap.FirstAvailableAddress

    /// The whole array at `addr`, cells included, or None if `addr` is not a live array.
    ///
    /// The only accessor that hands out every cell at once, because dumping an array is
    /// the one thing an observer legitimately wants and the guest never does. Interpreter
    /// code reads one cell at a time through `ManagedHeap.getArrayValue`.
    let tryGetArray (addr : ManagedHeapAddress) (heap : ManagedHeap) : AllocatedArray option =
        match heap.Arrays.TryGetValue addr with
        | true, arr -> Some arr
        | false, _ -> None

    /// Every live non-array object with its payload, in ascending address order.
    let nonArrayObjects (heap : ManagedHeap) : (ManagedHeapAddress * AllocatedNonArrayObject) list =
        heap.NonArrayObjects |> Map.toList

    /// The non-array object at `addr`, or None if there is no live non-array object there
    /// — including when `addr` is a live *array*.
    let tryGetNonArrayObject (addr : ManagedHeapAddress) (heap : ManagedHeap) : AllocatedNonArrayObject option =
        match heap.NonArrayObjects.TryGetValue addr with
        | true, v -> Some v
        | false, _ -> None

    /// The character content of the string object at `addr`, or None if none was recorded.
    let getStringContents (addr : ManagedHeapAddress) (heap : ManagedHeap) : string option =
        match heap.StringContents.TryGetValue addr with
        | true, s -> Some s
        | false, _ -> None

    /// The object header of the object at `addr`. Fails for an address that is not a live
    /// allocation.
    let getSyncBlock (addr : ManagedHeapAddress) (heap : ManagedHeap) : SyncBlock =
        match heap.SyncBlocks.TryGetValue addr with
        | true, v -> v
        | false, _ ->
            failwith
                $"HeapObserver.getSyncBlock: %O{addr} is not a live managed heap allocation, so has no object header"

    /// Whether `addr` is a live heap allocation of either kind.
    let isLive (addr : ManagedHeapAddress) (heap : ManagedHeap) : bool =
        heap.NonArrayObjects.ContainsKey addr || heap.Arrays.ContainsKey addr

    /// The addresses of all live allocations, arrays and non-arrays alike.
    ///
    /// Derived from the two payload maps, deliberately *not* from `SyncBlocks`, so that it
    /// remains an independent answer to the same question `syncBlockAddresses` answers.
    /// Tests compare the two to check the header-table invariant; implementing either in
    /// terms of the other would turn that check into a tautology.
    let liveAddresses (heap : ManagedHeap) : Set<ManagedHeapAddress> =
        Set.union (heap.NonArrayObjects |> Map.keys |> Set.ofSeq) (heap.Arrays |> Map.keys |> Set.ofSeq)

    /// The addresses that have an object header. See `liveAddresses` for why this is
    /// computed from `SyncBlocks` alone.
    let syncBlockAddresses (heap : ManagedHeap) : Set<ManagedHeapAddress> =
        heap.SyncBlocks |> Map.keys |> Set.ofSeq

    /// Whether `addr` has an object header. Computed from `SyncBlocks` alone; see
    /// `liveAddresses`.
    let hasSyncBlock (addr : ManagedHeapAddress) (heap : ManagedHeap) : bool = heap.SyncBlocks.ContainsKey addr

    /// Every object header, keyed by address. Exposed as a whole so that a test can
    /// compare monitor state across two machine states in one equality.
    let syncBlocks (heap : ManagedHeap) : Map<ManagedHeapAddress, SyncBlock> = heap.SyncBlocks
