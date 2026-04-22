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

    static member SetField (name : string) (v : CliType) (f : AllocatedNonArrayObject) : AllocatedNonArrayObject =
        { f with
            Contents = CliValueType.WithFieldSet name v f.Contents
        }

type AllocatedArray =
    {
        Length : int
        Elements : ImmutableArray<CliType>
    }

type ManagedHeap =
    {
        NonArrayObjects : Map<ManagedHeapAddress, AllocatedNonArrayObject>
        Arrays : Map<ManagedHeapAddress, AllocatedArray>
        FirstAvailableAddress : int
        /// Side-table mapping a String heap address to the heap address of its sibling
        /// `char[]`. The sibling is an ordinary entry in `Arrays`; this map records the
        /// linkage so `stringsEqual` and the `Ldfld`/`Ldflda` interception for
        /// `System.String._firstChar` can reach the char storage from the String address.
        /// Populated at allocation time by `allocateManagedString`.
        StringCharArrays : ImmutableDictionary<ManagedHeapAddress, ManagedHeapAddress>
    }

[<RequireQualifiedAccess>]
module ManagedHeap =
    let empty : ManagedHeap =
        {
            NonArrayObjects = Map.empty
            FirstAvailableAddress = 1
            Arrays = Map.empty
            StringCharArrays = ImmutableDictionary.Empty
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

    let allocateNonArray (ty : AllocatedNonArrayObject) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        let addr = heap.FirstAvailableAddress

        let heap =
            { heap with
                FirstAvailableAddress = addr + 1
                NonArrayObjects = heap.NonArrayObjects |> Map.add (ManagedHeapAddress addr) ty
            }

        ManagedHeapAddress addr, heap

    /// Record the linkage from a String heap address to the heap address of its sibling
    /// `char[]`. Called by `allocateManagedString` immediately after allocating the String
    /// object and the char array; consumers read the linkage back via `resolveStringChars`.
    let recordStringCharArray
        (strAddr : ManagedHeapAddress)
        (charArrAddr : ManagedHeapAddress)
        (heap : ManagedHeap)
        : ManagedHeap
        =
        { heap with
            StringCharArrays = heap.StringCharArrays.SetItem (strAddr, charArrAddr)
        }

    /// Resolve a String heap address to its sibling `char[]` allocation. The sibling
    /// stores the null-terminated char data; its length is the string's text length + 1.
    /// Fails if the address was not registered via `recordStringCharArray` (i.e. the
    /// address is not a String, or was allocated via a path that didn't go through
    /// `allocateManagedString`).
    let resolveStringChars (strAddr : ManagedHeapAddress) (heap : ManagedHeap) : AllocatedArray =
        match heap.StringCharArrays.TryGetValue strAddr with
        | false, _ ->
            failwith
                $"resolveStringChars: address %O{strAddr} has no registered sibling char[]; not a String allocated via allocateManagedString"
        | true, arrAddr ->

        match heap.Arrays.TryGetValue arrAddr with
        | false, _ ->
            failwith
                $"resolveStringChars: String %O{strAddr} is linked to sibling %O{arrAddr} but that array is missing from the heap"
        | true, arr -> arr

    /// Same as `resolveStringChars`, but returns the sibling's heap address alongside the
    /// array payload. Used by the `Ldflda` interception to construct an `ArrayElement`
    /// byref rooted at the sibling.
    let resolveStringCharArrayAddr (strAddr : ManagedHeapAddress) (heap : ManagedHeap) : ManagedHeapAddress =
        match heap.StringCharArrays.TryGetValue strAddr with
        | false, _ ->
            failwith
                $"resolveStringCharArrayAddr: address %O{strAddr} has no registered sibling char[]; not a String allocated via allocateManagedString"
        | true, arrAddr -> arrAddr

    /// Value-level equality between two managed string objects addressed by `a1` and `a2`.
    /// Mirrors the semantics of System.String.Equals(string, string): null-aware, reference
    /// equal implies equal, otherwise compares full character contents by walking the
    /// sibling `char[]` of each string.
    /// Fails if either address is not a registered string and the two addresses are distinct
    /// (i.e., we genuinely need the character content to answer).
    let stringsEqual (a1 : ManagedHeapAddress) (a2 : ManagedHeapAddress) (heap : ManagedHeap) : bool =
        if a1 = a2 then
            true
        else
            let chars1 = resolveStringChars a1 heap
            let chars2 = resolveStringChars a2 heap
            // Sibling arrays are allocated with length = text length + 1 (null terminator).
            // The terminator is not part of the comparison, but we compare the whole stored
            // range anyway: two strings with the same non-terminator contents will also have
            // matching null terminators at the same index. If the lengths differ, the strings
            // differ.
            if chars1.Length <> chars2.Length then
                false
            else
                let mutable i = 0
                let mutable equal = true

                while equal && i < chars1.Length do
                    if chars1.Elements.[i] <> chars2.Elements.[i] then
                        equal <- false

                    i <- i + 1

                equal

    let getArrayValue (alloc : ManagedHeapAddress) (offset : int) (heap : ManagedHeap) : CliType =
        match heap.Arrays.TryGetValue alloc with
        | false, _ -> failwith "TODO: array not on heap"
        | true, arr ->

        if offset < 0 || offset >= arr.Length then
            failwith "TODO: raise IndexOutOfBoundsException"

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
                    | None -> failwith "tried to change element of nonexistent array"
                    | Some arr ->
                        if offset < 0 || offset >= arr.Elements.Length then
                            failwith "TODO: throw somehow"

                        { arr with
                            Elements = arr.Elements.SetItem (offset, v)
                        }
                        |> Some
                )

        { heap with
            Arrays = newArrs
        }
