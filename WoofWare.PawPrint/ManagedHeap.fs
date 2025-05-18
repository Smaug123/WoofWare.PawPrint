namespace WoofWare.PawPrint

open System.Collections.Immutable

type AllocatedNonArrayObject =
    {
        Fields : Map<string, CliType>
        Type : WoofWare.PawPrint.TypeInfo
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
        /// Strings are special-cased in the runtime anyway and have a whole lot of unsafe code in them,
        /// so we'll have a special pool for their bytes.
        StringArrayData : ImmutableArray<char>
    }

    static member Empty : ManagedHeap =
        {
            NonArrayObjects = Map.empty
            FirstAvailableAddress = 1
            Arrays = Map.empty
            StringArrayData = ImmutableArray.Empty
        }

    static member AllocateArray (ty : AllocatedArray) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        let addr = heap.FirstAvailableAddress

        let heap =
            {
                FirstAvailableAddress = heap.FirstAvailableAddress + 1
                NonArrayObjects = heap.NonArrayObjects
                Arrays = heap.Arrays |> Map.add (ManagedHeapAddress addr) ty
                StringArrayData = heap.StringArrayData
            }

        ManagedHeapAddress addr, heap

    static member AllocateString (len : int) (heap : ManagedHeap) : int * ManagedHeap =
        let addr = heap.StringArrayData.Length

        let heap =
            { heap with
                // strings are also null-terminated
                // https://github.com/dotnet/runtime/blob/ab105b51f8b50ec5567d7cfe9001ca54dd6f64c3/src/libraries/System.Private.CoreLib/src/System/String.cs#L56
                StringArrayData = heap.StringArrayData.AddRange (Seq.replicate (len + 1) (char 0))
            }

        addr, heap

    static member SetStringData (addr : int) (contents : string) (heap : ManagedHeap) : ManagedHeap =
        let newArr =
            (heap.StringArrayData, seq { 0 .. contents.Length - 1 })
            ||> Seq.fold (fun data count -> data.SetItem (addr + count, contents.[count]))

        let heap =
            { heap with
                StringArrayData = newArr
            }

        heap

    static member AllocateNonArray
        (ty : AllocatedNonArrayObject)
        (heap : ManagedHeap)
        : ManagedHeapAddress * ManagedHeap
        =
        let addr = heap.FirstAvailableAddress

        let heap =
            {
                FirstAvailableAddress = addr + 1
                NonArrayObjects = heap.NonArrayObjects |> Map.add (ManagedHeapAddress addr) ty
                Arrays = heap.Arrays
                StringArrayData = heap.StringArrayData
            }

        ManagedHeapAddress addr, heap

    static member SetArrayValue
        (alloc : ManagedHeapAddress)
        (offset : int)
        (v : CliType)
        (heap : ManagedHeap)
        : ManagedHeap
        =
        let newArrs =
            heap.Arrays
            |> Map.change
                alloc
                (fun arr ->
                    match arr with
                    | None -> failwith "tried to change element of nonexistent array"
                    | Some arr ->
                        { arr with
                            Elements = arr.Elements.SetItem (offset, v)
                        }
                        |> Some
                )

        { heap with
            Arrays = newArrs
        }
