namespace WoofWare.PawPrint

open System
open System.Buffers.Binary
open System.Collections.Immutable

/// The running state of a SHA-256 computation (FIPS 180-4 section 6.2), between two
/// `Sha256.update` calls.
///
/// An immutable value: copying it is free, `Sha256.finish` does not consume it, and feeding
/// the same bytes in different chunkings reaches the same state.
type Sha256State =
    private
        {
            /// The eight working hash words H0..H7 after every complete 64-byte block fed so far.
            Hash : ImmutableArray<uint32>
            /// Bytes fed since the last complete block: always fewer than 64.
            Pending : ImmutableArray<byte>
            /// Bytes fed in total, which the padding encodes as a bit count.
            Length : uint64
        }

[<RequireQualifiedAccess>]
module Sha256 =
    /// The block size in bytes: the message is padded to a multiple of this.
    [<Literal>]
    let BlockSize = 64

    /// The digest size in bytes.
    [<Literal>]
    let DigestSize = 32

    /// FIPS 180-4 section 4.2.2: the first 32 bits of the fractional parts of the cube roots of
    /// the first 64 primes.
    let private roundConstants : uint32[] =
        [|
            0x428a2f98u
            0x71374491u
            0xb5c0fbcfu
            0xe9b5dba5u
            0x3956c25bu
            0x59f111f1u
            0x923f82a4u
            0xab1c5ed5u
            0xd807aa98u
            0x12835b01u
            0x243185beu
            0x550c7dc3u
            0x72be5d74u
            0x80deb1feu
            0x9bdc06a7u
            0xc19bf174u
            0xe49b69c1u
            0xefbe4786u
            0x0fc19dc6u
            0x240ca1ccu
            0x2de92c6fu
            0x4a7484aau
            0x5cb0a9dcu
            0x76f988dau
            0x983e5152u
            0xa831c66du
            0xb00327c8u
            0xbf597fc7u
            0xc6e00bf3u
            0xd5a79147u
            0x06ca6351u
            0x14292967u
            0x27b70a85u
            0x2e1b2138u
            0x4d2c6dfcu
            0x53380d13u
            0x650a7354u
            0x766a0abbu
            0x81c2c92eu
            0x92722c85u
            0xa2bfe8a1u
            0xa81a664bu
            0xc24b8b70u
            0xc76c51a3u
            0xd192e819u
            0xd6990624u
            0xf40e3585u
            0x106aa070u
            0x19a4c116u
            0x1e376c08u
            0x2748774cu
            0x34b0bcb5u
            0x391c0cb3u
            0x4ed8aa4au
            0x5b9cca4fu
            0x682e6ff3u
            0x748f82eeu
            0x78a5636fu
            0x84c87814u
            0x8cc70208u
            0x90befffau
            0xa4506cebu
            0xbef9a3f7u
            0xc67178f2u
        |]

    /// FIPS 180-4 section 5.3.3: the first 32 bits of the fractional parts of the square roots of
    /// the first 8 primes.
    let private initialHash : ImmutableArray<uint32> =
        ImmutableArray.Create<uint32> (
            0x6a09e667u,
            0xbb67ae85u,
            0x3c6ef372u,
            0xa54ff53au,
            0x510e527fu,
            0x9b05688cu,
            0x1f83d9abu,
            0x5be0cd19u
        )

    /// The state before any byte has been fed.
    let empty : Sha256State =
        {
            Hash = initialHash
            Pending = ImmutableArray<byte>.Empty
            Length = 0UL
        }

    /// How many bytes have been fed so far.
    let length (state : Sha256State) : uint64 = state.Length

    let inline private rotr (x : uint32) (n : int) : uint32 = (x >>> n) ||| (x <<< (32 - n))

    /// FIPS 180-4 section 6.2.2: fold one 64-byte block, starting at `offset` of `block`, into
    /// the hash words.
    let private compress (hash : ImmutableArray<uint32>) (block : byte[]) (offset : int) : ImmutableArray<uint32> =
        let w = Array.zeroCreate<uint32> 64

        for t = 0 to 15 do
            w.[t] <- BinaryPrimitives.ReadUInt32BigEndian (ReadOnlySpan<byte> (block, offset + 4 * t, 4))

        for t = 16 to 63 do
            let s0 = rotr w.[t - 15] 7 ^^^ rotr w.[t - 15] 18 ^^^ (w.[t - 15] >>> 3)
            let s1 = rotr w.[t - 2] 17 ^^^ rotr w.[t - 2] 19 ^^^ (w.[t - 2] >>> 10)
            w.[t] <- w.[t - 16] + s0 + w.[t - 7] + s1

        let mutable a = hash.[0]
        let mutable b = hash.[1]
        let mutable c = hash.[2]
        let mutable d = hash.[3]
        let mutable e = hash.[4]
        let mutable f = hash.[5]
        let mutable g = hash.[6]
        let mutable h = hash.[7]

        for t = 0 to 63 do
            let bigS1 = rotr e 6 ^^^ rotr e 11 ^^^ rotr e 25
            let ch = (e &&& f) ^^^ (~~~e &&& g)
            let t1 = h + bigS1 + ch + roundConstants.[t] + w.[t]
            let bigS0 = rotr a 2 ^^^ rotr a 13 ^^^ rotr a 22
            let maj = (a &&& b) ^^^ (a &&& c) ^^^ (b &&& c)
            let t2 = bigS0 + maj
            h <- g
            g <- f
            f <- e
            e <- d + t1
            d <- c
            c <- b
            b <- a
            a <- t1 + t2

        ImmutableArray.Create<uint32> (
            hash.[0] + a,
            hash.[1] + b,
            hash.[2] + c,
            hash.[3] + d,
            hash.[4] + e,
            hash.[5] + f,
            hash.[6] + g,
            hash.[7] + h
        )

    /// Feed `bytes`, in any chunking: a chunk may end in the middle of a block, and the partial
    /// block is carried to the next call.
    let update (bytes : ImmutableArray<byte>) (state : Sha256State) : Sha256State =
        if bytes.IsEmpty then
            state
        else

        let buffered = Array.zeroCreate<byte> (state.Pending.Length + bytes.Length)
        state.Pending.CopyTo buffered
        bytes.CopyTo (buffered, state.Pending.Length)

        let completeBlocks = buffered.Length / BlockSize
        let mutable hash = state.Hash

        for block = 0 to completeBlocks - 1 do
            hash <- compress hash buffered (block * BlockSize)

        let remainder = completeBlocks * BlockSize

        {
            Hash = hash
            Pending = ImmutableArray.Create<byte> (buffered, remainder, buffered.Length - remainder)
            Length = state.Length + uint64 bytes.Length
        }

    /// The digest of everything fed so far. Pure: the state is untouched, so feeding more bytes
    /// afterwards continues the same computation.
    let finish (state : Sha256State) : ImmutableArray<byte> =
        // FIPS 180-4 section 5.1.1: a 0x80 byte, zeros up to 8 bytes short of a block boundary,
        // then the bit length as a big-endian 64-bit integer.
        let pendingLength = state.Pending.Length

        let paddedLength =
            if pendingLength + 1 + 8 <= BlockSize then
                BlockSize
            else
                2 * BlockSize

        let padded = Array.zeroCreate<byte> paddedLength
        state.Pending.CopyTo padded
        padded.[pendingLength] <- 0x80uy

        BinaryPrimitives.WriteUInt64BigEndian (Span<byte> (padded, paddedLength - 8, 8), state.Length * 8UL)

        let mutable hash = state.Hash

        for block = 0 to paddedLength / BlockSize - 1 do
            hash <- compress hash padded (block * BlockSize)

        let digest = Array.zeroCreate<byte> DigestSize

        for i = 0 to 7 do
            BinaryPrimitives.WriteUInt32BigEndian (Span<byte> (digest, 4 * i, 4), hash.[i])

        ImmutableArray.Create<byte> digest

    /// The digest of `bytes` in one step.
    let hash (bytes : ImmutableArray<byte>) : ImmutableArray<byte> = empty |> update bytes |> finish
