namespace WoofWare.PawPrint

/// Where a byref root's storage sits inside the byte-addressable container that holds it.
///
/// Some roots *are* a whole container — a local slot, an argument slot, a static slot, a
/// whole boxed value — and start at offset 0 of it. The rest are **views**: an array
/// element is a window into its array, a class field is a window into its object, a string
/// character is a window into the string's character data. For those, the container is the
/// enclosing allocation and the offset says where in it the root begins.
///
/// This is one question with one answer, and it is asked from two places that cannot call
/// each other. `StorageLocation` (fsproj slot 90) asks it to turn a byref into a flat
/// coordinate it can compare by arithmetic; `IlMachineManagedByref` (slot 79) asks it to
/// find the storage a byte access should actually be served from once the access walks out
/// of the cell the root names. Slot 90 is built *on* slot 79, so neither can delegate to
/// the other; the shared thing moves below both instead. Hand-written copies of this step
/// are what produced #987 (a class field treated as its own container, so two overlapping
/// fields were reported disjoint), #1009 (`Unsafe.ByteOffset` carrying a cut-down copy of
/// the walk) and #729 (a byte access refused because it left the element it started in).
[<RequireQualifiedAccess>]
module ByrefContainer =
    /// The byte-addressable container holding this root's storage, and the byte offset at
    /// which the root's own storage begins within it. Roots that are a whole container of
    /// their own return that container and offset 0.
    ///
    /// `None` for `ExposedClassObject`: the cache of a type's canonical `RuntimeType` is a
    /// single object reference held outside any byte-addressable allocation, so there is no
    /// container to offset into. Every other root has one.
    ///
    /// The offset is `int64` for the same reason `walkProjectionByteOffset`'s accumulator
    /// is: it is a coordinate, not an access offset. Callers that need to index inside the
    /// container narrow it themselves, at a boundary that refuses rather than truncates.
    ///
    /// Raises when the heap lookup a view root needs cannot be served — an array or object
    /// address that is not allocated, or a `FieldId` the object does not carry. Those are
    /// interpreter bugs rather than guest-reachable states; a caller that would rather
    /// degrade than fail (`StorageLocation`, which falls back to a coarse storage key)
    /// catches around this.
    let tryOfRoot (heap : ManagedHeap) (root : ByrefRoot) : (ByteStorageIdentity * int64) option =
        match root with
        | ByrefRoot.LocalVariable (thread, frame, local) ->
            Some (ByteStorageIdentity.StackLocal (thread, frame, local), 0L)
        | ByrefRoot.Argument (thread, frame, arg) -> Some (ByteStorageIdentity.StackArgument (thread, frame, arg), 0L)
        | ByrefRoot.StaticField (declaringType, field, owner) ->
            Some (ByteStorageIdentity.StaticField (declaringType, field, owner), 0L)
        | ByrefRoot.PeByteRange peByteRange -> Some (ByteStorageIdentity.PeByteRange peByteRange, 0L)
        | ByrefRoot.HeapValue addr -> Some (ByteStorageIdentity.HeapObject addr, 0L)
        // The remaining roots are views into a container that is larger than they are.
        | ByrefRoot.StackMemoryByte (thread, frame, block, byteOffset) ->
            Some (ByteStorageIdentity.StackMemory (thread, frame, block), int64<int> byteOffset)
        | ByrefRoot.NativeMemoryByte (block, byteOffset) ->
            Some (ByteStorageIdentity.NativeMemory block, int64<int> byteOffset)
        | ByrefRoot.StringCharAt (str, charIndex) ->
            // A string's character data is UTF-16, so one character index is two bytes.
            Some (ByteStorageIdentity.String str, int64<int> charIndex * 2L)
        | ByrefRoot.ArrayElement (arr, index) ->
            // The stride is the array's recorded element stride rather than the measured
            // size of cell 0: a cell may carry provenance that has no byte width, and an
            // empty array has no cell to measure at all. Multiplied in `int64` because
            // `index * stride` is address arithmetic, and wrapping it would place the
            // element inside the array rather than outside it.
            let stride = ManagedHeap.getArrayElementStride arr heap
            Some (ByteStorageIdentity.Array arr, int64<int> index * int64<int> stride)
        | ByrefRoot.HeapObjectField (addr, field) ->
            // One heap allocation is one container, and a field is a view into it at the
            // field's layout offset. Giving each field its own container would assert that
            // two fields cannot overlap, which `[StructLayout(LayoutKind.Explicit)]` on a
            // class makes false — measured as a wrong-direction `Memmove` in
            // `SpanMemmoveOverlappingExplicitLayoutClassFields.cs` (#987).
            let contents = CliType.ValueType (ManagedHeap.get addr heap).Contents
            let fieldOffset, _ = CliType.getFieldLayoutById field contents
            Some (ByteStorageIdentity.HeapObject addr, int64<int> fieldOffset)
        | ByrefRoot.ExposedClassObject _ -> None
