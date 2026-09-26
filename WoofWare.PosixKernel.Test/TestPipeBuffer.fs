namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One call on a pipe's buffer: a non-blocking write of this many bytes, or a
/// read of up to this many.
[<RequireQualifiedAccess>]
type PipeBufferOp =
    | Write of count : int
    | Read of count : int

/// Naive statements of each flavour's measured buffer rule, written for
/// obviousness rather than speed and in a different representation from the
/// library's, to hold `PipeBuffer` to. The rules are the ones
/// `docs/plans/2026-08-23-posix-kernel-extraction/pipe-buffer-model.py` checked
/// against every recorded row.
[<RequireQualifiedAccess>]
module PipeBufferReference =

    /// Linux: sixteen page-sized slots, each recorded as (offset of its first
    /// unread byte within the page, number of unread bytes), oldest first, and
    /// the held bytes as one flat list.
    type Linux =
        {
            Page : int
            Slots : (int * int) list
            Bytes : byte list
        }

    let linuxEmpty (page : int) : Linux =
        {
            Page = page
            Slots = []
            Bytes = []
        }

    let linuxWrite (payload : byte list) (s : Linux) : int * Linux =
        let n = List.length payload

        if n = 0 then
            0, s
        else
            let chars = n % s.Page

            let merged, slots =
                match List.tryLast s.Slots with
                | Some (offset, length) when chars > 0 && offset + length + chars <= s.Page ->
                    chars, List.take (List.length s.Slots - 1) s.Slots @ [ offset, length + chars ]
                | _ -> 0, s.Slots

            let mutable accepted = merged
            let mutable slots = slots

            while accepted < n && List.length slots < 16 do
                let c = min s.Page (n - accepted)
                slots <- slots @ [ 0, c ]
                accepted <- accepted + c

            accepted,
            { s with
                Slots = slots
                Bytes = s.Bytes @ List.take accepted payload
            }

    let linuxRead (count : int) (s : Linux) : byte list * Linux =
        let taken = min count (List.length s.Bytes)
        let mutable remaining = taken
        let mutable slots = s.Slots

        while remaining > 0 do
            match slots with
            | (offset, length) :: rest ->
                let c = min remaining length
                remaining <- remaining - c

                slots <-
                    if c = length then
                        rest
                    else
                        (offset + c, length - c) :: rest
            | [] -> failwith "reference: bytes held with no slot to hold them"

        List.take taken s.Bytes,
        { s with
            Slots = slots
            Bytes = List.skip taken s.Bytes
        }

    let linuxWritable (s : Linux) : bool = List.length s.Slots < 16

    /// Darwin: the held bytes, and the size the buffer has grown to.
    type Darwin =
        {
            Size : int
            Bytes : byte list
        }

    let darwinEmpty : Darwin =
        {
            Size = 512
            Bytes = []
        }

    /// The smallest block strictly larger than both, or the largest block.
    let private choose (current : int) (expected : int) : int =
        let blocks = [| 512 ; 1024 ; 2048 ; 4096 ; 8192 ; 16384 ; 65536 |]
        let target = max current expected
        let mutable i = blocks.Length - 1

        while i > 0 && blocks.[i - 1] > target do
            i <- i - 1

        blocks.[i]

    let darwinWrite (payload : byte list) (s : Darwin) : int * Darwin =
        let n = List.length payload
        let held = List.length s.Bytes

        if n = 0 then
            0, s
        else
            let size =
                if n > s.Size - held then
                    // Saturating rather than wrapping: only whether the target
                    // reaches each block size matters.
                    choose s.Size (int (min (int64 held + int64 n) (int64 Int32.MaxValue)))
                else
                    s.Size

            let free = size - held

            let accepted =
                if n <= 512 then
                    (if free >= n then n else 0)
                else
                    min n free

            accepted,
            {
                Size = size
                Bytes = s.Bytes @ List.take accepted payload
            }

    let darwinRead (count : int) (s : Darwin) : byte list * Darwin =
        let taken = min count (List.length s.Bytes)

        List.take taken s.Bytes,
        { s with
            Bytes = List.skip taken s.Bytes
        }

    let darwinWritable (s : Darwin) : bool =
        max 16384 s.Size - List.length s.Bytes >= 512

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipeBuffer =

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    /// The bytes of a write whose first byte is the `start`th byte the pipe has
    /// been offered, numbered modulo 256, so that FIFO order is checkable.
    let payload (start : int) (count : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (Array.init count (fun i -> byte ((start + i) % 256)))

    let private sizeGen : Gen<int> =
        Gen.frequency
            [
                2, Gen.choose (1, 8)
                2, Gen.choose (1, 600)
                3,
                Gen.elements
                    [
                        0
                        511
                        512
                        513
                        4095
                        4096
                        4097
                        8191
                        8192
                        8193
                        15872
                        15873
                        16383
                        16384
                        16385
                        65535
                        65536
                        65537
                    ]
                2, Gen.choose (1, 5000)
                2, Gen.choose (1, 20000)
                1, Gen.choose (1, 70000)
            ]

    let opsGen : Gen<PipeBufferOp list> =
        Gen.frequency [ 3, Gen.map PipeBufferOp.Write sizeGen ; 1, Gen.map PipeBufferOp.Read sizeGen ]
        |> Gen.listOf
        |> Gen.map (List.truncate 60)

    let private platformGen : Gen<SimulatedUnixPlatform> = Gen.elements platforms

    /// A reference model behind the same four questions as `PipeBuffer`.
    type private Reference =
        | Linux of PipeBufferReference.Linux
        | Darwin of PipeBufferReference.Darwin

    let private referenceFor (platform : SimulatedUnixPlatform) : Reference =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            Reference.Linux (
                PipeBufferReference.linuxEmpty (SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform))
            )
        | SimulatedUnixFlavour.Darwin -> Reference.Darwin PipeBufferReference.darwinEmpty

    let private referenceWrite (bytes : ImmutableArray<byte>) (r : Reference) : int * Reference =
        match r with
        | Reference.Linux s ->
            let a, s = PipeBufferReference.linuxWrite (List.ofSeq bytes) s
            a, Reference.Linux s
        | Reference.Darwin s ->
            let a, s = PipeBufferReference.darwinWrite (List.ofSeq bytes) s
            a, Reference.Darwin s

    let private referenceRead (count : int) (r : Reference) : byte list * Reference =
        match r with
        | Reference.Linux s ->
            let b, s = PipeBufferReference.linuxRead count s
            b, Reference.Linux s
        | Reference.Darwin s ->
            let b, s = PipeBufferReference.darwinRead count s
            b, Reference.Darwin s

    let private referenceHeld (r : Reference) : int =
        match r with
        | Reference.Linux s -> List.length s.Bytes
        | Reference.Darwin s -> List.length s.Bytes

    let private referenceWritable (r : Reference) : bool =
        match r with
        | Reference.Linux s -> PipeBufferReference.linuxWritable s
        | Reference.Darwin s -> PipeBufferReference.darwinWritable s

    [<Test>]
    let ``the buffer answers every call exactly as the reference rule does`` () : unit =
        let property (platform : SimulatedUnixPlatform, ops : PipeBufferOp list) : unit =
            let mutable buffer = PipeBuffer.empty platform
            let mutable reference = referenceFor platform
            let mutable offered = 0

            for op in ops do
                match op with
                | PipeBufferOp.Write count ->
                    let bytes = payload offered count
                    let accepted, b = PipeBuffer.write bytes buffer
                    let expected, r = referenceWrite bytes reference

                    if accepted <> expected then
                        failwith $"%O{platform}: write of %d{count} accepted %d{accepted}, reference %d{expected}"

                    buffer <- b
                    reference <- r
                    offered <- offered + accepted
                | PipeBufferOp.Read count ->
                    let got, b = PipeBuffer.read count buffer
                    let expected, r = referenceRead count reference
                    List.ofSeq got |> shouldEqual expected
                    buffer <- b
                    reference <- r

                PipeBuffer.held buffer |> shouldEqual (referenceHeld reference)
                PipeBuffer.readable buffer |> shouldEqual (referenceHeld reference > 0)

                if PipeBuffer.writable buffer <> referenceWritable reference then
                    failwith
                        $"%O{platform}: after %O{op}, writable is %b{PipeBuffer.writable buffer} with %d{PipeBuffer.held buffer} held; reference says %b{referenceWritable reference}"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip platformGen opsGen)) property
        )

    [<Test>]
    let ``bytes come out in the order they went in, and none is lost or invented`` () : unit =
        let property (platform : SimulatedUnixPlatform, ops : PipeBufferOp list) : unit =
            let mutable buffer = PipeBuffer.empty platform
            let accepted = ResizeArray<byte> ()
            let delivered = ResizeArray<byte> ()

            for op in ops do
                match op with
                | PipeBufferOp.Write count ->
                    let bytes = payload accepted.Count count
                    let n, b = PipeBuffer.write bytes buffer
                    accepted.AddRange (Seq.take n bytes)
                    buffer <- b
                | PipeBufferOp.Read count ->
                    let before = PipeBuffer.held buffer
                    let got, b = PipeBuffer.read count buffer
                    got.Length |> shouldEqual (min count before)
                    delivered.AddRange got
                    buffer <- b

            let rest, _ = PipeBuffer.read Int32.MaxValue buffer
            delivered.AddRange rest
            List.ofSeq delivered |> shouldEqual (List.ofSeq accepted)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip platformGen opsGen)) property
        )

    [<Test>]
    let ``a write of at most PIPE_BUF bytes is taken whole or not at all, and nothing overfills`` () : unit =
        let property (platform : SimulatedUnixPlatform, ops : PipeBufferOp list) : unit =
            let capacity =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> 16 * SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform)
                | SimulatedUnixFlavour.Darwin -> 65536

            let mutable buffer = PipeBuffer.empty platform
            let limit = PipeBuffer.atomicWriteLimit buffer

            for op in ops do
                match op with
                | PipeBufferOp.Write count ->
                    let n, b = PipeBuffer.write (payload 0 count) buffer

                    if count <= limit && n <> 0 && n <> count then
                        failwith $"%O{platform}: a %d{count}-byte write, within PIPE_BUF %d{limit}, took %d{n}"

                    if n < 0 || n > count then
                        failwith $"%O{platform}: a %d{count}-byte write took %d{n}"

                    buffer <- b
                | PipeBufferOp.Read count -> buffer <- snd (PipeBuffer.read count buffer)

                if PipeBuffer.held buffer > capacity then
                    failwith $"%O{platform}: holds %d{PipeBuffer.held buffer}, beyond %d{capacity}"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip platformGen opsGen)) property
        )

    [<Test>]
    let ``a zero-length write or read changes nothing a later call can see`` () : unit =
        let observe (buffer : PipeBuffer) (ops : PipeBufferOp list) : (int * byte list * int * bool) list =
            let mutable buffer = buffer
            let mutable offered = 0

            [
                for op in ops do
                    match op with
                    | PipeBufferOp.Write count ->
                        let n, b = PipeBuffer.write (payload offered count) buffer
                        buffer <- b
                        offered <- offered + n
                        yield n, [], PipeBuffer.held buffer, PipeBuffer.writable buffer
                    | PipeBufferOp.Read count ->
                        let got, b = PipeBuffer.read count buffer
                        buffer <- b
                        yield 0, List.ofSeq got, PipeBuffer.held buffer, PipeBuffer.writable buffer
            ]

        let property (platform : SimulatedUnixPlatform, prefix : PipeBufferOp list, suffix : PipeBufferOp list) : unit =
            let mutable buffer = PipeBuffer.empty platform

            for op in prefix do
                match op with
                | PipeBufferOp.Write count -> buffer <- snd (PipeBuffer.write (payload 0 count) buffer)
                | PipeBufferOp.Read count -> buffer <- snd (PipeBuffer.read count buffer)

            let wrote, afterWrite = PipeBuffer.write ImmutableArray.Empty buffer
            wrote |> shouldEqual 0
            let got, afterRead = PipeBuffer.read 0 afterWrite
            got.Length |> shouldEqual 0
            observe afterRead suffix |> shouldEqual (observe buffer suffix)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (Arb.fromGen (Gen.zip3 platformGen opsGen opsGen)) property
        )

    /// Fill a fresh buffer with non-blocking writes of `size` until one takes
    /// nothing: the bytes held, the writes that took anything, and how many of
    /// those were short. What `pipe-capacity.c` does against a real pipe.
    let private fill (platform : SimulatedUnixPlatform) (size : int) : int * int * int =
        let mutable buffer = PipeBuffer.empty platform
        let mutable writes = 0
        let mutable shorts = 0
        let mutable stop = false

        while not stop do
            let n, b = PipeBuffer.write (payload 0 size) buffer
            buffer <- b

            if n = 0 then
                stop <- true
            else
                writes <- writes + 1

                if n < size then
                    shorts <- shorts + 1

        PipeBuffer.held buffer, writes, shorts

    // (write size, bytes held when a write first takes nothing, writes that took
    // anything, of which short), from `pipe-capacity.c`'s output.
    let private linuxFills : (int * int * int * int) list =
        [
            1, 65536, 65536, 0
            2, 65536, 32768, 0
            3, 65520, 21840, 0
            7, 65520, 9360, 0
            100, 64000, 640, 0
            511, 65408, 128, 0
            512, 65536, 128, 0
            513, 57456, 112, 0
            1000, 64000, 64, 0
            1024, 65536, 64, 0
            4095, 65520, 16, 0
            4096, 65536, 16, 0
            4097, 45066, 11, 1
            8191, 65528, 8, 0
            8192, 65536, 8, 0
            8193, 53254, 7, 1
            16383, 65532, 4, 0
            16384, 65536, 4, 0
            16385, 57348, 4, 1
            32768, 65536, 2, 0
            65535, 65535, 1, 0
            65536, 65536, 1, 0
            65537, 65536, 1, 1
            131072, 65536, 1, 1
            1048576, 65536, 1, 1
        ]

    let private darwinFills : (int * int * int * int) list =
        [
            1, 65536, 65536, 0
            2, 65536, 32768, 0
            3, 65535, 21845, 0
            7, 65534, 9362, 0
            100, 65500, 655, 0
            511, 65408, 128, 0
            512, 65536, 128, 0
            513, 65536, 128, 1
            1000, 65536, 66, 1
            1024, 65536, 64, 0
            4095, 65536, 17, 1
            4096, 65536, 16, 0
            4097, 65536, 16, 1
            8191, 65536, 9, 1
            8192, 65536, 8, 0
            8193, 65536, 8, 1
            16383, 65536, 5, 1
            16384, 65536, 4, 0
            16385, 65536, 4, 1
            32768, 65536, 2, 0
            65535, 65536, 2, 1
            65536, 65536, 1, 0
            65537, 65536, 1, 1
            131072, 65536, 1, 1
            1048576, 65536, 1, 1
        ]

    [<Test>]
    let ``filling a pipe with writes of each size stops where each kernel stopped`` () : unit =
        for platform, rows in
            [
                SimulatedUnixPlatform.linuxX64, linuxFills
                SimulatedUnixPlatform.linuxArm64, linuxFills
                SimulatedUnixPlatform.macOsArm64, darwinFills
            ] do
            for size, held, writes, shorts in rows do
                (size, fill platform size) |> shouldEqual (size, (held, writes, shorts))

    [<Test>]
    let ``PIPE_BUF is what each kernel's fpathconf reported`` () : unit =
        PipeBuffer.atomicWriteLimit (PipeBuffer.empty SimulatedUnixPlatform.linuxX64)
        |> shouldEqual 4096

        PipeBuffer.atomicWriteLimit (PipeBuffer.empty SimulatedUnixPlatform.linuxArm64)
        |> shouldEqual 4096

        PipeBuffer.atomicWriteLimit (PipeBuffer.empty SimulatedUnixPlatform.macOsArm64)
        |> shouldEqual 512

    /// Apply each call in turn and report whether the write end is ready after
    /// every one.
    let private writableAfter (platform : SimulatedUnixPlatform) (ops : PipeBufferOp list) : bool list =
        let mutable buffer = PipeBuffer.empty platform

        [
            for op in ops do
                match op with
                | PipeBufferOp.Write count -> buffer <- snd (PipeBuffer.write (payload 0 count) buffer)
                | PipeBufferOp.Read count -> buffer <- snd (PipeBuffer.read count buffer)

                yield PipeBuffer.writable buffer
        ]

    [<Test>]
    let ``Darwin's write end becomes ready by growing, and stays grown`` () : unit =
        // `states.c` in the options document's raw output: 16000 bytes at once
        // leave the 16 KiB buffer without 512 free; 1000 more grow it to 64 KiB,
        // which is then ready; draining it and writing 16000 again finds it
        // still grown.
        let calls =
            [
                PipeBufferOp.Write 16000
                PipeBufferOp.Write 1000
                PipeBufferOp.Read 17000
                PipeBufferOp.Write 16000
            ]

        writableAfter SimulatedUnixPlatform.macOsArm64 calls
        |> shouldEqual [ false ; true ; true ; true ]

        // The same calls on Linux: 16000 bytes take four of sixteen slots.
        writableAfter SimulatedUnixPlatform.linuxX64 calls
        |> shouldEqual [ true ; true ; true ; true ]

    [<Test>]
    let ``Darwin's buffer starts at 512 bytes`` () : unit =
        // `pipe-buffer-doubling.c` lands the held count on successive powers of
        // two up to 16384. Each block size either absorbs the next landing or
        // grows past it, so where the buffer starts decides whether it is at
        // 8192 when the last write arrives (and grows to 64 KiB, ready) or at
        // 16384 already (and absorbs it, not ready). Measured ready in both
        // cases below: case 1 rules out a 1024-byte start, case 7 a 0-byte one.
        let lastWritable (ops : PipeBufferOp list) : bool =
            writableAfter SimulatedUnixPlatform.macOsArm64 ops |> List.last

        [ 1 ; 511 ; 512 ; 1024 ; 2048 ; 4096 ; 8192 ]
        |> List.map PipeBufferOp.Write
        |> lastWritable
        |> shouldEqual true

        [ 512 ; 512 ; 1024 ; 2048 ; 4096 ; 8192 ]
        |> List.map PipeBufferOp.Write
        |> lastWritable
        |> shouldEqual true

    [<Test>]
    let ``Linux takes a write's remainder into the newest slot while every slot is taken`` () : unit =
        // Sixteen 4095-byte writes take every slot and leave one byte free in
        // each. The write end is not ready, but a one-byte write still merges
        // into the newest slot, and a two-byte write does not.
        let mutable buffer = PipeBuffer.empty SimulatedUnixPlatform.linuxX64

        for _ in 1..16 do
            buffer <- snd (PipeBuffer.write (payload 0 4095) buffer)

        PipeBuffer.writable buffer |> shouldEqual false
        fst (PipeBuffer.write (payload 0 2) buffer) |> shouldEqual 0
        let n, buffer = PipeBuffer.write (payload 0 1) buffer
        n |> shouldEqual 1
        PipeBuffer.held buffer |> shouldEqual 65521

    [<Test>]
    let ``Linux frees a slot only when a read empties it`` () : unit =
        let mutable buffer = PipeBuffer.empty SimulatedUnixPlatform.linuxX64

        for _ in 1..16 do
            buffer <- snd (PipeBuffer.write (payload 0 4096) buffer)

        // `states.c`: a full pipe less one byte, or less 4095, is still full.
        buffer <- snd (PipeBuffer.read 4095 buffer)
        PipeBuffer.writable buffer |> shouldEqual false
        fst (PipeBuffer.write (payload 0 1) buffer) |> shouldEqual 0
        buffer <- snd (PipeBuffer.read 1 buffer)
        PipeBuffer.writable buffer |> shouldEqual true

    [<Test>]
    let ``a negative read count is the caller's bug`` () : unit =
        for platform in platforms do
            Assert.Throws<Exception> (fun () -> PipeBuffer.read -1 (PipeBuffer.empty platform) |> ignore)
            |> ignore

    [<Test>]
    let ``draining a full buffer a byte at a time copies only the bytes read`` () : unit =
        // A read that leaves part of a chunk behind must not copy what it leaves:
        // 65536 one-byte reads of one 64 KiB chunk would otherwise copy about
        // 2 GiB. The bound is generous, and far below that.
        for platform in platforms do
            let _, full = PipeBuffer.write (payload 0 65536) (PipeBuffer.empty platform)
            let mutable buffer = full
            let before = GC.GetAllocatedBytesForCurrentThread ()

            for _ in 1..65536 do
                buffer <- snd (PipeBuffer.read 1 buffer)

            let allocated = GC.GetAllocatedBytesForCurrentThread () - before

            if allocated > 64L * 1024L * 1024L then
                failwith $"%O{platform}: draining 64 KiB a byte at a time allocated %d{allocated} bytes"

            PipeBuffer.held buffer |> shouldEqual 0

    [<Test>]
    let ``a write too large to add to what the buffer holds takes what fits`` () : unit =
        // `pipe-buffer-huge-write.c`: with 512 bytes held, a count whose sum
        // with them exceeds Int32.MaxValue takes exactly what a 65536- or
        // 70000-byte count takes.
        let huge =
            ImmutableCollectionsMarshal.AsImmutableArray (Array.zeroCreate<byte> 2147483500)

        for platform, expected in
            [
                SimulatedUnixPlatform.macOsArm64, 65024
                SimulatedUnixPlatform.linuxX64, 61440
                SimulatedUnixPlatform.linuxArm64, 61440
            ] do
            let _, buffer = PipeBuffer.write (payload 0 512) (PipeBuffer.empty platform)

            (platform, fst (PipeBuffer.write huge buffer))
            |> shouldEqual (platform, expected)

            (platform, fst (PipeBuffer.write (payload 0 70000) buffer))
            |> shouldEqual (platform, (if expected = 65024 then 65024 else 61808))
