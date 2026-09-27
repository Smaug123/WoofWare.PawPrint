namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// Bytes in first-in, first-out order, kept as the chunks they arrived in, so
/// that neither appending nor taking copies what stays behind.
[<NoEquality ; NoComparison>]
type internal ByteQueue =
    private
        {
            /// The oldest chunk; empty exactly when the queue is.
            Head : ImmutableArray<byte>
            /// How many of `Head`'s bytes have been taken already. Always less
            /// than `Head`'s length unless the queue is empty.
            HeadTaken : int
            /// The later chunks, oldest first, none of them empty.
            Rest : ImmutableQueue<ImmutableArray<byte>>
            Length : int
        }

[<RequireQualifiedAccess>]
module internal ByteQueue =
    let empty : ByteQueue =
        {
            Head = ImmutableArray.Empty
            HeadTaken = 0
            Rest = ImmutableQueue.Empty
            Length = 0
        }

    let length (queue : ByteQueue) : int = queue.Length

    let append (chunk : ImmutableArray<byte>) (queue : ByteQueue) : ByteQueue =
        if chunk.IsEmpty then
            queue
        elif queue.Length = 0 then
            {
                Head = chunk
                HeadTaken = 0
                Rest = ImmutableQueue.Empty
                Length = chunk.Length
            }
        else
            { queue with
                Rest = queue.Rest.Enqueue chunk
                Length = queue.Length + chunk.Length
            }

    /// The oldest `count` bytes, which must not be more than the queue holds,
    /// and the queue without them. Copies the bytes returned and nothing else.
    let take (count : int) (queue : ByteQueue) : ImmutableArray<byte> * ByteQueue =
        if count < 0 || count > queue.Length then
            failwith
                $"ByteQueue.take: %d{count} bytes asked of a queue holding %d{queue.Length} (this is a bug in this library)."

        let taken = ImmutableArray.CreateBuilder<byte> count
        let mutable head = queue.Head
        let mutable headTaken = queue.HeadTaken
        let mutable rest = queue.Rest
        let mutable remaining = count

        while remaining > 0 do
            let unread = head.Length - headTaken

            if unread <= remaining then
                taken.AddRange (head.AsSpan (headTaken, unread))
                remaining <- remaining - unread
                headTaken <- 0

                if rest.IsEmpty then
                    head <- ImmutableArray.Empty
                else
                    let mutable next = ImmutableArray.Empty
                    rest <- rest.Dequeue &next
                    head <- next
            else
                taken.AddRange (head.AsSpan (headTaken, remaining))
                headTaken <- headTaken + remaining
                remaining <- 0

        taken.MoveToImmutable (),
        {
            Head = head
            HeadTaken = headTaken
            Rest = rest
            Length = queue.Length - count
        }

/// One of a Linux pipe's page-sized slots.
[<NoEquality ; NoComparison>]
type internal LinuxPipeSlot =
    {
        /// Where in the page the slot's first unread byte sits. Reading advances
        /// it, and the space before it is never written again.
        Offset : int
        /// The unread bytes, which run from `Offset` to at most the page's end.
        /// Never empty: a read that empties a slot frees it.
        Bytes : ByteQueue
    }

/// A Linux pipe's buffer: a ring of sixteen page-sized slots, the oldest first.
[<NoEquality ; NoComparison>]
type internal LinuxPipeBuffer =
    {
        PageSize : int
        /// The most bytes one write moves (`MAX_RW_COUNT`), from
        /// `SimulatedUnixPlatform.transferCountLimit`.
        MaxTransfer : int
        Slots : LinuxPipeSlot list
    }

/// The sizes a Darwin pipe's buffer takes, in order. It starts at the smallest
/// and grows to a larger one when a write does not fit; it never shrinks.
[<RequireQualifiedAccess>]
type internal DarwinPipeBufferSize =
    | B512
    | B1024
    | B2048
    | B4096
    | B8192
    | B16384
    | B65536

[<RequireQualifiedAccess>]
module internal DarwinPipeBufferSize =
    let private ascending : DarwinPipeBufferSize list =
        [
            DarwinPipeBufferSize.B512
            DarwinPipeBufferSize.B1024
            DarwinPipeBufferSize.B2048
            DarwinPipeBufferSize.B4096
            DarwinPipeBufferSize.B8192
            DarwinPipeBufferSize.B16384
            DarwinPipeBufferSize.B65536
        ]

    let bytes (size : DarwinPipeBufferSize) : int =
        match size with
        | DarwinPipeBufferSize.B512 -> 512
        | DarwinPipeBufferSize.B1024 -> 1024
        | DarwinPipeBufferSize.B2048 -> 2048
        | DarwinPipeBufferSize.B4096 -> 4096
        | DarwinPipeBufferSize.B8192 -> 8192
        | DarwinPipeBufferSize.B16384 -> 16384
        | DarwinPipeBufferSize.B65536 -> 65536

    /// What a buffer of `current` grows to when a write would leave it holding
    /// `wanted` bytes: the smallest size strictly larger than both, or the
    /// largest size if none is. "Strictly" is measured: a buffer that must hold
    /// exactly 16384 grows past 16384 rather than to it.
    let grownFor (current : DarwinPipeBufferSize) (wanted : int64) : DarwinPipeBufferSize =
        let target = max (int64 (bytes current)) wanted

        ascending
        |> List.tryFind (fun size -> int64 (bytes size) > target)
        |> Option.defaultValue DarwinPipeBufferSize.B65536

/// A Darwin pipe's buffer: the bytes it holds, and the size it has grown to.
[<NoEquality ; NoComparison>]
type internal DarwinPipeBuffer =
    {
        Size : DarwinPipeBufferSize
        Bytes : ByteQueue
    }

/// The bytes a pipe holds between its write end and its read end, stored as the
/// simulated flavour's kernel stores them. The two flavours store them
/// differently, and a process can tell the difference: in how much a pipe takes
/// before a write would block, in which writes come back short, and in when the
/// write end polls ready.
///
/// This is only the buffer. Whether the other end is still open, and so whether
/// an empty read is end-of-file or would block, and whether a write is EPIPE,
/// is a question about the pipe's ends, not about its buffer.
[<NoEquality ; NoComparison>]
type PipeBuffer =
    private
    | Linux of LinuxPipeBuffer
    | Darwin of DarwinPipeBuffer

[<RequireQualifiedAccess>]
module PipeBuffer =

    // Linux: `fs/pipe.c`'s default ring of sixteen buffers, each a page. The
    // rule below was checked against 120,000 recorded calls on Linux 6.18.5
    // aarch64 with 4 KiB pages, and agrees with every one; a buffer that counts
    // bytes against a 64 KiB capacity agreed with none of the 200 seeds. See
    // docs/plans/2026-08-23-posix-kernel-extraction/pipe-buffer-trace.c.
    [<Literal>]
    let private LinuxSlots = 16

    // Darwin: the size a buffer may have before the write end stops polling
    // ready, and the most a write may take all-or-nothing. Checked against
    // 120,000 recorded calls on Darwin 27.0.0 arm64, plus pipe-buffer-growth.c
    // and pipe-buffer-doubling.c, which pin the starting size to 512 (0, 1024
    // and 16384 each disagree with some recorded row). A buffer that is always
    // 64 KiB disagreed with 7 and 11 of 200 seeds, all on when the write end
    // polls ready.
    [<Literal>]
    let private DarwinReadyFloor = 16384

    [<Literal>]
    let private DarwinPipeBuf = 512

    /// An empty buffer, as `pipe(2)` creates one on `platform`'s kernel.
    let empty (platform : SimulatedUnixPlatform) : PipeBuffer =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            PipeBuffer.Linux
                {
                    PageSize = SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform)
                    MaxTransfer = TransferCountLimit.maxTransfer (SimulatedUnixPlatform.transferCountLimit platform)
                    Slots = []
                }
        | SimulatedUnixFlavour.Darwin ->
            PipeBuffer.Darwin
                {
                    Size = DarwinPipeBufferSize.B512
                    Bytes = ByteQueue.empty
                }

    /// How many bytes the buffer holds: what `FIONREAD` on the read end reports.
    let held (buffer : PipeBuffer) : int =
        match buffer with
        | PipeBuffer.Linux linux -> linux.Slots |> List.sumBy (fun slot -> ByteQueue.length slot.Bytes)
        | PipeBuffer.Darwin darwin -> ByteQueue.length darwin.Bytes

    /// `PIPE_BUF`: a write of at most this many bytes is taken whole or not at
    /// all, never in part. 4096 on Linux, where it is the page size, and 512 on
    /// Darwin.
    let atomicWriteLimit (buffer : PipeBuffer) : int =
        match buffer with
        | PipeBuffer.Linux linux -> linux.PageSize
        | PipeBuffer.Darwin _ -> DarwinPipeBuf

    /// Whether the read end polls ready to read: the buffer holds something.
    let readable (buffer : PipeBuffer) : bool = held buffer > 0

    /// Whether the write end polls ready to write.
    ///
    /// Not the same as "a write would take something". On Linux the write end
    /// is ready when one of the sixteen slots is free, however many bytes the
    /// taken slots have room for; on Darwin it is ready when 512 bytes are free
    /// below 16 KiB or the size the buffer has grown to, whichever is larger,
    /// though a write may grow the buffer and be taken regardless.
    let writable (buffer : PipeBuffer) : bool =
        match buffer with
        | PipeBuffer.Linux linux -> List.length linux.Slots < LinuxSlots
        | PipeBuffer.Darwin darwin ->
            max DarwinReadyFloor (DarwinPipeBufferSize.bytes darwin.Size)
            - ByteQueue.length darwin.Bytes
            >= DarwinPipeBuf

    /// A non-blocking write of `bytes`: how many of them, from the start, the
    /// buffer takes now, and the buffer after taking them.
    ///
    /// Zero means nothing fits, which a caller answers with `EAGAIN` or by
    /// waiting; a count below `bytes.Length` is a short write. A write of at most
    /// `atomicWriteLimit` bytes is never short. A write of no bytes takes none
    /// and changes nothing.
    ///
    /// `bytes` is what reaches the pipe, after the kernel's limit on one call's
    /// transfer. On Linux that limit is `INT_MAX` rounded down to a page, and a
    /// longer count is the caller's to shorten first: the pipe takes a
    /// different amount from a clamped count than from the unclamped one.
    let write (bytes : ImmutableArray<byte>) (buffer : PipeBuffer) : int * PipeBuffer =
        if bytes.IsDefault then
            failwith
                "PipeBuffer.write: bytes is the default ImmutableArray, whose underlying array is null. That is not an empty write; pass ImmutableArray<byte>.Empty."

        let n = bytes.Length

        if n = 0 then
            0, buffer
        else

        match buffer with
        | PipeBuffer.Linux linux ->
            let page = linux.PageSize

            // MAX_RW_COUNT, which the platform states once for every transfer.
            // Measured for pipes with pipe-buffer-huge-write.c.
            let maxTransfer = linux.MaxTransfer

            if n > maxTransfer then
                failwith
                    $"PipeBuffer.write: a count of %d{n} exceeds Linux's per-call limit of %d{maxTransfer}, which the kernel applies before the pipe sees the write; clamp the count first (this is a bug in the caller of PipeBuffer.write)."


            let slice (start : int) (length : int) =
                ImmutableArray.Create (bytes, start, length)

            // The write's remainder below a whole page goes first, into the
            // newest slot, if it fits after that slot's last byte. A write of a
            // whole number of pages merges nothing.
            let remainder = n % page

            let merged, slots =
                match List.tryLast linux.Slots with
                | Some newest when
                    remainder > 0
                    && newest.Offset + ByteQueue.length newest.Bytes + remainder <= page
                    ->
                    let grown =
                        { newest with
                            Bytes = ByteQueue.append (slice 0 remainder) newest.Bytes
                        }

                    remainder, List.take (List.length linux.Slots - 1) linux.Slots @ [ grown ]
                | Some _
                | None -> 0, linux.Slots

            // The rest takes whole slots, a page at a time, while any is free.
            let rec fill (taken : int) (slots : LinuxPipeSlot list) (count : int) =
                if taken = n || count = LinuxSlots then
                    taken, slots
                else
                    let length = min page (n - taken)

                    let slot =
                        {
                            Offset = 0
                            Bytes = ByteQueue.append (slice taken length) ByteQueue.empty
                        }

                    fill (taken + length) (slots @ [ slot ]) (count + 1)

            let taken, slots = fill merged slots (List.length slots)

            taken,
            PipeBuffer.Linux
                { linux with
                    Slots = slots
                }
        | PipeBuffer.Darwin darwin ->
            let holding = ByteQueue.length darwin.Bytes

            let size =
                if n > DarwinPipeBufferSize.bytes darwin.Size - holding then
                    DarwinPipeBufferSize.grownFor darwin.Size (int64 holding + int64 n)
                else
                    darwin.Size

            let free = DarwinPipeBufferSize.bytes size - holding

            let taken =
                if n <= DarwinPipeBuf then
                    (if free >= n then n else 0)
                else
                    min n free

            taken,
            PipeBuffer.Darwin
                {
                    Size = size
                    Bytes = ByteQueue.append (ImmutableArray.Create (bytes, 0, taken)) darwin.Bytes
                }

    /// A read of up to `count` bytes: the oldest `min count (held buffer)` bytes,
    /// and the buffer without them. Returns no bytes when the buffer is empty,
    /// which a caller answers as end-of-file, `EAGAIN` or by waiting, according
    /// to the pipe's write end and the read's blocking mode. `count` must not be
    /// negative.
    let read (count : int) (buffer : PipeBuffer) : ImmutableArray<byte> * PipeBuffer =
        if count < 0 then
            failwith
                $"PipeBuffer.read: a count of %d{count} is not a request a kernel ever sees; the caller must answer a negative count before asking."

        let taking = min count (held buffer)

        match buffer with
        | PipeBuffer.Darwin darwin ->
            let taken, rest = ByteQueue.take taking darwin.Bytes

            taken,
            PipeBuffer.Darwin
                { darwin with
                    Bytes = rest
                }
        | PipeBuffer.Linux linux ->
            // Oldest slot first; a slot the read empties is freed, and one it
            // leaves bytes in keeps its place, its unread bytes now further in.
            let rec drain (remaining : int) (slots : LinuxPipeSlot list) (chunks : ImmutableArray<byte> list) =
                match slots with
                | slot :: rest when remaining > 0 ->
                    let length = ByteQueue.length slot.Bytes

                    if length <= remaining then
                        let chunk, _ = ByteQueue.take length slot.Bytes
                        drain (remaining - length) rest (chunk :: chunks)
                    else
                        let chunk, left = ByteQueue.take remaining slot.Bytes

                        let slot =
                            {
                                Offset = slot.Offset + remaining
                                Bytes = left
                            }

                        slot :: rest, chunk :: chunks
                | _ -> slots, chunks

            let slots, chunks = drain taking linux.Slots []
            let taken = ImmutableArray.CreateBuilder<byte> taking

            for chunk in List.rev chunks do
                taken.AddRange chunk

            taken.MoveToImmutable (),
            PipeBuffer.Linux
                { linux with
                    Slots = slots
                }
