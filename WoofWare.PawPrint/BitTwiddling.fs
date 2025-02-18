namespace WoofWare.PawPrint

open System

[<AutoOpen>]
module internal BitTwiddling =

    let inline toUint32 (bytes : ReadOnlySpan<byte>) : uint32 =
        uint32 bytes.[0]
        + uint32 bytes.[1] * 256u
        + uint32 bytes.[2] * 256u * 256u
        + uint32 bytes.[3] * 256u * 256u * 256u

    let inline toUint16 (bytes : ReadOnlySpan<byte>) : uint16 =
        uint16 bytes.[0] + uint16 bytes.[1] * 256us

    let inline toUint64 (bytes : ReadOnlySpan<byte>) : uint64 =
        uint64 (toUint32 (bytes.Slice (0, 4)))
        + 0x10000UL * uint64 (toUint32 (bytes.Slice (4, 4)))
