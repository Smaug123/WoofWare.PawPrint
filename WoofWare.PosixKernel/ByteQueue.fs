namespace WoofWare.PosixKernel

open System.Collections.Immutable

/// Bytes in first-in, first-out order, kept as the chunks they arrived in, so
/// that neither appending nor taking copies what stays behind.
///
/// Two queues are equal when they hold the same bytes in the same order,
/// however those bytes are chunked.
[<CustomEquality ; NoComparison>]
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

    /// Every byte held, oldest first.
    member private this.Bytes : byte seq =
        seq {
            if this.Length > 0 then
                for i in this.HeadTaken .. this.Head.Length - 1 do
                    yield this.Head.[i]

                for chunk in this.Rest do
                    yield! chunk
        }

    override this.Equals (other : obj) : bool =
        match other with
        | :? ByteQueue as other -> this.Length = other.Length && Seq.forall2 (=) this.Bytes other.Bytes
        | _ -> false

    override this.GetHashCode () : int = this.Length

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

    /// The oldest `count` bytes, which must not be more than the queue holds,
    /// leaving the queue as it was. Copies the bytes returned and nothing else.
    let peek (count : int) (queue : ByteQueue) : ImmutableArray<byte> =
        if count < 0 || count > queue.Length then
            failwith
                $"ByteQueue.peek: %d{count} bytes asked of a queue holding %d{queue.Length} (this is a bug in this library)."

        fst (take count queue)
