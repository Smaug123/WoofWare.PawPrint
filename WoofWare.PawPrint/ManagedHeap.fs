namespace WoofWare.PawPrint

open System.Collections.Immutable

type SyncBlock =
    | Free
    | Locked of lockingThread : ThreadId * reentrancyCount : int

type AllocatedNonArrayObject =
    {
        // TODO: this is a slightly odd domain; the same type for value types as class types!
        Contents : CliValueType
        ConcreteType : ConcreteTypeHandle
        SyncBlock : SyncBlock
    }

    static member DereferenceField (name : string) (f : AllocatedNonArrayObject) : CliType =
        CliValueType.DereferenceField name f.Contents

    static member DereferenceFieldById (field : FieldId) (f : AllocatedNonArrayObject) : CliType =
        CliValueType.DereferenceFieldById field f.Contents

    static member SetField (name : string) (v : CliType) (f : AllocatedNonArrayObject) : AllocatedNonArrayObject =
        { f with
            Contents = CliValueType.WithFieldSet name v f.Contents
        }

    static member SetFieldById (field : FieldId) (v : CliType) (f : AllocatedNonArrayObject) : AllocatedNonArrayObject =
        { f with
            Contents = CliValueType.WithFieldSetById field v f.Contents
        }

type AllocatedArray =
    {
        ConcreteType : ConcreteTypeHandle
        /// Total element count, equal to the product of `Lengths`. For szarrays this is just
        /// the array length; for multi-dim arrays it is the size of the flat backing store.
        Length : int
        /// Per-dimension lengths in row-major order. Length is 1 for szarrays, equal to the
        /// rank for multi-dim arrays. Multiplying these produces `Length`.
        Lengths : ImmutableArray<int>
        /// Backing store in row-major order. For multi-dim arrays the element at
        /// `(i_0, ..., i_{n-1})` lives at flat offset
        /// `((((i_0)*d_1)+i_1)*d_2 + i_2)*...*d_{n-1} + i_{n-1}`, where `d_k = Lengths.[k]`.
        Elements : ImmutableArray<CliType>
    }

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
        }

    let getSyncBlock (addr : ManagedHeapAddress) (heap : ManagedHeap) : SyncBlock =
        match heap.NonArrayObjects.TryGetValue addr with
        | false, _ -> failwith "TODO: getting sync block of array"
        | true, v -> v.SyncBlock

    let setSyncBlock (addr : ManagedHeapAddress) (syncValue : SyncBlock) (heap : ManagedHeap) : ManagedHeap =
        match heap.NonArrayObjects.TryGetValue addr with
        | false, _ -> failwith "TODO: locked on an array object"
        | true, v ->
            let newV =
                { v with
                    SyncBlock = syncValue
                }

            { heap with
                NonArrayObjects = heap.NonArrayObjects |> Map.add addr newV
            }

    let allocateArray (ty : AllocatedArray) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        let addr = heap.FirstAvailableAddress

        let heap =
            { heap with
                FirstAvailableAddress = heap.FirstAvailableAddress + 1
                Arrays = heap.Arrays |> Map.add (ManagedHeapAddress addr) ty
            }

        ManagedHeapAddress addr, heap

    let tryGetObjectConcreteType (alloc : ManagedHeapAddress) (heap : ManagedHeap) : ConcreteTypeHandle option =
        match heap.NonArrayObjects.TryGetValue alloc with
        | true, obj -> Some obj.ConcreteType
        | false, _ ->
            match heap.Arrays.TryGetValue alloc with
            | true, arr -> Some arr.ConcreteType
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

    let getStringDataOffset (addr : ManagedHeapAddress) (heap : ManagedHeap) : int =
        match heap.StringDataOffsets.TryGetValue addr with
        | true, offset -> offset
        | false, _ -> failwith $"string data offset for %O{addr} was not recorded"

    let private requireStringContents (operation : string) (addr : ManagedHeapAddress) (heap : ManagedHeap) : string =
        match getStringContents addr heap with
        | Some contents -> contents
        | None -> failwith $"%s{operation}: string contents for %O{addr} were not recorded"

    /// Update a character in the runtime string data side-table. `charIndex` equal
    /// to the string length addresses the null terminator; that updates
    /// `StringArrayData` but not the logical `StringContents` value. The metadata-
    /// level `_firstChar` field is a synthetic projection over
    /// `StringArrayData[dataOffset]` (see `RuntimeFieldProjection`) and therefore
    /// requires no separate mirror.
    let setStringChar (addr : ManagedHeapAddress) (charIndex : int) (value : char) (heap : ManagedHeap) : ManagedHeap =
        if charIndex < 0 then
            failwith $"string character index must be non-negative, got %d{charIndex} for %O{addr}"

        let contents = requireStringContents "setStringChar" addr heap

        if charIndex > contents.Length then
            failwith
                $"string character index %d{charIndex} is beyond the null terminator of string %O{addr} with length %d{contents.Length}"

        let dataOffset = getStringDataOffset addr heap
        let newArr = heap.StringArrayData.ToBuilder ()
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
    /// (i.e., we genuinely need the character content to answer).
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

    let getArrayValue (alloc : ManagedHeapAddress) (offset : int) (heap : ManagedHeap) : CliType =
        match heap.Arrays.TryGetValue alloc with
        | false, _ -> failwith $"TODO: array not on heap (no array registered at %O{alloc})"
        | true, arr ->

        if offset < 0 then
            failwith
                $"TODO: raise IndexOutOfRangeException: negative array index %d{offset} on array at %O{alloc} (length %d{arr.Length}). A negative index here typically means a byref obtained via `RawData::Data` on an array was read without first applying the canonical `+sizeof(nint)` skip past the length-header region; if you intended to read the length, use `RawArrayData::Length` instead."
        elif offset >= arr.Length then
            failwith
                $"TODO: raise IndexOutOfRangeException: array index %d{offset} >= length %d{arr.Length} on array at %O{alloc}"

        arr.Elements.[offset]

    let get (alloc : ManagedHeapAddress) (heap : ManagedHeap) : AllocatedNonArrayObject =
        // TODO: arrays too
        heap.NonArrayObjects.[alloc]

    let set (alloc : ManagedHeapAddress) (v : AllocatedNonArrayObject) (heap : ManagedHeap) : ManagedHeap =
        // TODO: arrays too
        { heap with
            NonArrayObjects = heap.NonArrayObjects |> Map.add alloc v
        }

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
                                $"TODO: raise IndexOutOfRangeException: negative array index %d{offset} on array at %O{alloc} (length %d{arr.Elements.Length}). A negative index here typically means a byref obtained via `RawData::Data` on an array was written without first applying the canonical `+sizeof(nint)` skip past the length-header region."
                        elif offset >= arr.Elements.Length then
                            failwith
                                $"TODO: raise IndexOutOfRangeException: array index %d{offset} >= length %d{arr.Elements.Length} on array at %O{alloc}"

                        { arr with
                            Elements = arr.Elements.SetItem (offset, v)
                        }
                        |> Some
                )

        { heap with
            Arrays = newArrs
        }
