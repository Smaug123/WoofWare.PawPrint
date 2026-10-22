namespace WoofWare.PawPrint

type NativeBlockAlignment =
    private
    | NativeBlockAlignment of int

    member this.Bytes : int =
        match this with
        | NativeBlockAlignment.NativeBlockAlignment bytes -> bytes

[<RequireQualifiedAccess>]
module NativeBlockAlignment =
    let ofBytes (bytes : int) : NativeBlockAlignment =
        if bytes <= 0 || not (System.Numerics.BitOperations.IsPow2 bytes) then
            failwith $"NativeBlockAlignment.ofBytes: %d{bytes} is not a positive power of two"

        NativeBlockAlignment.NativeBlockAlignment bytes

    let malloc : NativeBlockAlignment = ofBytes 16

/// How the next base is chosen, with the state that choice needs. Only `place` reads it.
[<RequireQualifiedAccess>]
type private Placement =
    /// Each block at the lowest suitably aligned address at or above `next`, which is the end
    /// of the last block placed. Released ranges are never reused.
    | Ascending of next : uint64

/// Where one live block lies.
type private PlacedBlock =
    {
        Base : uint64
        /// The bytes the block occupies: its size, or 1 for a block of no bytes.
        Extent : uint64
        Alignment : NativeBlockAlignment
    }

type NativeAddressSpace =
    private
        {
            Placement : Placement
            Blocks : Map<NativeMemoryBlockId, PlacedBlock>
        }

[<RequireQualifiedAccess>]
module NativeAddressSpace =
    let lowest : uint64 = 1UL <<< 32

    let limit : uint64 = 1UL <<< 46

    let empty : NativeAddressSpace =
        {
            Placement = Placement.Ascending lowest
            Blocks = Map.empty
        }

    let private alignUp (alignment : NativeBlockAlignment) (address : uint64) : uint64 =
        let mask = uint64 alignment.Bytes - 1UL
        (address + mask) &&& ~~~mask

    let private isAligned (block : PlacedBlock) : bool =
        block.Base % uint64 block.Alignment.Bytes = 0UL

    let private isWithinBounds (block : PlacedBlock) : bool =
        lowest <= block.Base && block.Base + block.Extent <= limit

    let place
        (blockId : NativeMemoryBlockId)
        (byteCount : int)
        (alignment : NativeBlockAlignment)
        (space : NativeAddressSpace)
        : uint64 * NativeAddressSpace
        =
        if byteCount < 0 then
            failwith $"NativeAddressSpace.place: %O{blockId} has negative byte count %d{byteCount}"

        if Map.containsKey blockId space.Blocks then
            failwith
                $"NativeAddressSpace.place: %O{blockId} already has a base (this is an interpreter bug: block ids are never reused)"

        let extent = uint64 (max byteCount 1)

        let candidate, placement =
            match space.Placement with
            | Placement.Ascending next ->
                let start = alignUp alignment next
                start, Placement.Ascending (start + extent)

        let block =
            {
                Base = candidate
                Extent = extent
                Alignment = alignment
            }

        // A strategy is trusted to avoid live blocks, which `checkInvariants` holds it to;
        // what it is not trusted with, and what is cheap to check, is checked here.
        if not (isAligned block) then
            failwith
                $"NativeAddressSpace.place: chose base 0x%x{candidate} for %O{blockId}, which is not %d{alignment.Bytes}-byte aligned (this is an interpreter bug)"

        if not (isWithinBounds block) then
            failwith
                $"NativeAddressSpace.place: no room for %O{blockId}, %d{byteCount} bytes at %d{alignment.Bytes}-byte alignment, below 0x%x{limit}: the native heap's address range is exhausted"

        candidate,
        {
            Placement = placement
            Blocks = Map.add blockId block space.Blocks
        }

    let release (blockId : NativeMemoryBlockId) (space : NativeAddressSpace) : NativeAddressSpace =
        if not (Map.containsKey blockId space.Blocks) then
            failwith
                $"NativeAddressSpace.release: %O{blockId} has no base (this is an interpreter bug: the pool frees each block once)"

        { space with
            Blocks = Map.remove blockId space.Blocks
        }

    let tryBase (blockId : NativeMemoryBlockId) (space : NativeAddressSpace) : uint64 option =
        Map.tryFind blockId space.Blocks |> Option.map _.Base

    let internal checkInvariants (space : NativeAddressSpace) : unit =
        for KeyValue (blockId, block) in space.Blocks do
            if not (isAligned block) then
                failwith $"%O{blockId} at 0x%x{block.Base} is not %d{block.Alignment.Bytes}-byte aligned"

            if not (isWithinBounds block) then
                failwith
                    $"%O{blockId} occupies [0x%x{block.Base}, 0x%x{block.Base + block.Extent}), outside [0x%x{lowest}, 0x%x{limit})"

        space.Blocks
        |> Map.toList
        |> List.sortBy (fun (_, block) -> block.Base)
        |> List.pairwise
        |> List.iter (fun ((lowerId, lower), (upperId, upper)) ->
            if lower.Base + lower.Extent > upper.Base then
                failwith
                    $"%O{lowerId} at [0x%x{lower.Base}, 0x%x{lower.Base + lower.Extent}) overlaps %O{upperId} at 0x%x{upper.Base}"
        )
