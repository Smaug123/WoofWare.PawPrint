namespace WoofWare.PosixKernel

open System
open System.Buffers.Binary
open System.Collections.Immutable

/// A watch descriptor: the number `inotify_add_watch(2)` answers for a watch,
/// which `inotify_rm_watch(2)` and every record about the watch name it by.
///
/// Positive. An instance hands out 1, 2, 3, ... and never reuses one, even
/// after its watch is removed.
type WatchDescriptor =
    | WatchDescriptor of value : int

    override this.ToString () : string =
        match this with
        | WatchDescriptor value -> string<int> value

[<RequireQualifiedAccess>]
module WatchDescriptor =
    /// The number a caller is given for the watch.
    let toInt (watch : WatchDescriptor) : int =
        match watch with
        | WatchDescriptor value -> value

/// One of the events an inotify watch can ask for: a bit of `IN_ALL_EVENTS`,
/// or `IN_UNMOUNT`. The numbering is Linux's `<sys/inotify.h>`, which every
/// architecture shares (`InotifyEvent.bit`).
[<RequireQualifiedAccess>]
type InotifyEvent =
    /// `IN_ACCESS`: the file was read.
    | Access
    /// `IN_MODIFY`: the file's contents changed.
    | Modify
    /// `IN_ATTRIB`: the inode's metadata changed.
    | Attrib
    /// `IN_CLOSE_WRITE`: a description opened for writing was released.
    | CloseWrite
    /// `IN_CLOSE_NOWRITE`: a description not opened for writing was released.
    | CloseNoWrite
    /// `IN_OPEN`: the file was opened.
    | Open
    /// `IN_MOVED_FROM`: a name left the watched directory.
    | MovedFrom
    /// `IN_MOVED_TO`: a name arrived in the watched directory.
    | MovedTo
    /// `IN_CREATE`: a name was created in the watched directory.
    | Create
    /// `IN_DELETE`: a name was removed from the watched directory.
    | Delete
    /// `IN_DELETE_SELF`: the watched inode itself was deleted.
    | DeleteSelf
    /// `IN_MOVE_SELF`: the watched inode itself was renamed.
    | MoveSelf
    /// `IN_UNMOUNT`: the filesystem holding the watched inode was unmounted.
    | Unmount

[<RequireQualifiedAccess>]
module InotifyEvent =
    /// Every event, in the order of their bits.
    let all : InotifyEvent list =
        [
            InotifyEvent.Access
            InotifyEvent.Modify
            InotifyEvent.Attrib
            InotifyEvent.CloseWrite
            InotifyEvent.CloseNoWrite
            InotifyEvent.Open
            InotifyEvent.MovedFrom
            InotifyEvent.MovedTo
            InotifyEvent.Create
            InotifyEvent.Delete
            InotifyEvent.DeleteSelf
            InotifyEvent.MoveSelf
            InotifyEvent.Unmount
        ]

    /// The event's bit in a mask, in Linux's `<sys/inotify.h>` numbering.
    let bit (event : InotifyEvent) : uint32 =
        match event with
        | InotifyEvent.Access -> 0x1u
        | InotifyEvent.Modify -> 0x2u
        | InotifyEvent.Attrib -> 0x4u
        | InotifyEvent.CloseWrite -> 0x8u
        | InotifyEvent.CloseNoWrite -> 0x10u
        | InotifyEvent.Open -> 0x20u
        | InotifyEvent.MovedFrom -> 0x40u
        | InotifyEvent.MovedTo -> 0x80u
        | InotifyEvent.Create -> 0x100u
        | InotifyEvent.Delete -> 0x200u
        | InotifyEvent.DeleteSelf -> 0x400u
        | InotifyEvent.MoveSelf -> 0x800u
        | InotifyEvent.Unmount -> 0x2000u

    /// Whether this kernel models the event, so that a watch may ask for it.
    ///
    /// `IN_ACCESS`, `IN_OPEN` and the two `IN_CLOSE_*` events are raised by
    /// opening, reading and releasing a file, which this kernel does not
    /// report to watches; nothing here unmounts a filesystem, so `IN_UNMOUNT`
    /// is never raised. `UnixInotify.inotifyAddWatch` refuses a watch asking
    /// for any of them rather than storing an event it would never raise.
    let modelled (event : InotifyEvent) : bool =
        match event with
        | InotifyEvent.Modify
        | InotifyEvent.Attrib
        | InotifyEvent.MovedFrom
        | InotifyEvent.MovedTo
        | InotifyEvent.Create
        | InotifyEvent.Delete
        | InotifyEvent.DeleteSelf
        | InotifyEvent.MoveSelf -> true
        | InotifyEvent.Access
        | InotifyEvent.CloseWrite
        | InotifyEvent.CloseNoWrite
        | InotifyEvent.Open
        | InotifyEvent.Unmount -> false

/// What a watch asks for: the events it reports, and whether it ignores events
/// raised through a name that has since been unlinked.
type InotifyMask =
    {
        /// The events the watch reports. Every one is `InotifyEvent.modelled`.
        /// May be empty: a watch whose mask held only flags exists, and reports
        /// nothing but its own removal.
        Events : Set<InotifyEvent>
        /// `IN_EXCL_UNLINK`: do not report an event raised through a name that
        /// has been unlinked from the watched directory.
        ExcludeUnlinked : bool
    }

/// One watch an inotify instance holds: the inode it watches, and what it
/// asks for.
type InotifyWatch =
    {
        /// The watched inode.
        Inode : InodeNumber
        /// What the watch asks for.
        Mask : InotifyMask
    }

/// One record an inotify instance queues for `read(2)` to return.
[<RequireQualifiedAccess>]
type InotifyRecord =
    /// Something happened to the inode `watch` watches, or to an entry of it.
    ///
    /// `events` is never empty. `subjectIsDirectory` is `IN_ISDIR`: the inode
    /// the event is about (the entry, or the watched inode itself) is a
    /// directory. `cookie` pairs an `IN_MOVED_FROM` with its `IN_MOVED_TO`, and
    /// is 0 otherwise. `name` is the entry's name, for an event about an entry
    /// of a watched directory, and `None` for an event about the watched inode
    /// itself.
    | Event of
        watch : WatchDescriptor *
        events : Set<InotifyEvent> *
        subjectIsDirectory : bool *
        cookie : uint32 *
        name : DirectoryEntryName option
    /// `IN_IGNORED`: the watch `watch` is gone, and no record will name it
    /// again.
    | Ignored of watch : WatchDescriptor
    /// `IN_Q_OVERFLOW`: records were dropped because the queue was full.
    | QueueOverflow

[<RequireQualifiedAccess>]
module InotifyRecord =
    /// `sizeof(struct inotify_event)`: the bytes before the name.
    [<Literal>]
    let HeaderSize : int = 16

    /// `IN_IGNORED`.
    let private ignoredBit : uint32 = 0x8000u

    /// `IN_Q_OVERFLOW`.
    let private overflowBit : uint32 = 0x4000u

    /// `IN_ISDIR`.
    let private isDirectoryBit : uint32 = 0x40000000u

    /// The `len` field: the name's bytes and its NUL, rounded up to a multiple
    /// of the header's size, or 0 for a record with no name.
    let private nameLength (record : InotifyRecord) : int =
        match record with
        | InotifyRecord.Event (_, _, _, _, Some name) ->
            let bytes = (DirectoryEntryName.toByteString name |> UnixByteString.toBytes).Length
            // Measured (`inotify-probe.c`, section L): 1 to 15 bytes give 16,
            // 16 to 31 give 32, 32 gives 48 and 255 gives 256.
            (bytes + 1 + HeaderSize - 1) / HeaderSize * HeaderSize
        | InotifyRecord.Event (_, _, _, _, None)
        | InotifyRecord.Ignored _
        | InotifyRecord.QueueOverflow -> 0

    /// How many bytes `read(2)` returns for the record.
    let size (record : InotifyRecord) : int = HeaderSize + nameLength record

    /// The record as `read(2)` returns it: `struct inotify_event`, which is
    /// `wd` (an `int32`), `mask`, `cookie` and `len` (each a `uint32`), all
    /// little-endian, then `len` bytes holding the name and zero padding.
    /// `IN_Q_OVERFLOW`'s watch descriptor is -1.
    let toBytes (record : InotifyRecord) : ImmutableArray<byte> =
        let length = nameLength record
        let bytes = Array.zeroCreate<byte> (HeaderSize + length)

        let watch, mask, cookie =
            match record with
            | InotifyRecord.Event (watch, events, subjectIsDirectory, cookie, _) ->
                let mask =
                    (0u, events) ||> Set.fold (fun mask event -> mask ||| InotifyEvent.bit event)

                let mask = if subjectIsDirectory then mask ||| isDirectoryBit else mask
                WatchDescriptor.toInt watch, mask, cookie
            | InotifyRecord.Ignored watch -> WatchDescriptor.toInt watch, ignoredBit, 0u
            | InotifyRecord.QueueOverflow -> -1, overflowBit, 0u

        BinaryPrimitives.WriteInt32LittleEndian (Span<byte> (bytes, 0, 4), watch)
        BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> (bytes, 4, 4), mask)
        BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> (bytes, 8, 4), cookie)
        BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> (bytes, 12, 4), uint32 length)

        match record with
        | InotifyRecord.Event (_, _, _, _, Some name) ->
            let nameBytes = DirectoryEntryName.toByteString name |> UnixByteString.toBytes
            nameBytes.CopyTo (bytes, HeaderSize)
        | InotifyRecord.Event (_, _, _, _, None)
        | InotifyRecord.Ignored _
        | InotifyRecord.QueueOverflow -> ()

        ImmutableArray.Create<byte> bytes

    /// Whether `next`, arriving behind `last`, is dropped as a repeat of it.
    ///
    /// Linux compares the two records' watch, mask and name, and not their
    /// cookie: two `IN_MOVED_FROM` records for one name, differing only in
    /// cookie, coalesce (measured, `inotify-instance-probe.c` row Q
    /// moved-from-twice). Nothing coalesces into an `IN_IGNORED`.
    let internal repeats (last : InotifyRecord) (next : InotifyRecord) : bool =
        match last, next with
        | InotifyRecord.Event (lastWatch, lastEvents, lastIsDirectory, _, lastName),
          InotifyRecord.Event (watch, events, isDirectory, _, name) ->
            lastWatch = watch
            && lastEvents = events
            && lastIsDirectory = isDirectory
            && lastName = name
        | InotifyRecord.Ignored _, _
        | InotifyRecord.QueueOverflow, _
        | InotifyRecord.Event _, InotifyRecord.Ignored _
        | InotifyRecord.Event _, InotifyRecord.QueueOverflow -> false

/// The records an inotify instance holds for `read(2)`, oldest first.
///
/// Its representation is private; `InotifyQueue.toList` reads it. Two queues
/// are equal when they hold the same records in the same order.
[<CustomEquality ; CustomComparison>]
type InotifyQueue =
    private
        {
            Records : ImmutableQueue<InotifyRecord>
            Count : int
            Bytes : int64
            Last : InotifyRecord option
            Overflowed : bool
        }

    member private this.AsList : InotifyRecord list = this.Records |> List.ofSeq

    override this.Equals (other : obj) : bool =
        match other with
        | :? InotifyQueue as other -> this.Count = other.Count && Seq.forall2 (=) this.Records other.Records
        | _ -> false

    override this.GetHashCode () : int = this.Count

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? InotifyQueue as other -> compare this.AsList other.AsList
            | _ -> invalidArg "other" "an InotifyQueue is compared only with another"

    override this.ToString () : string = $"%A{this.AsList}"

/// What the head of an inotify queue says about a read of a given count.
[<RequireQualifiedAccess>]
type internal InotifyTake =
    /// The queue holds nothing.
    | Empty
    /// The oldest record is larger than the count.
    | FirstTooLarge
    /// The oldest records that fit in the count, at least one, and the queue
    /// without them.
    | Taken of records : InotifyRecord list * rest : InotifyQueue

[<RequireQualifiedAccess>]
module InotifyQueue =
    /// A queue holding nothing.
    let empty : InotifyQueue =
        {
            Records = ImmutableQueue.Empty
            Count = 0
            Bytes = 0L
            Last = None
            Overflowed = false
        }

    /// The records held, oldest first.
    let toList (queue : InotifyQueue) : InotifyRecord list = queue.Records |> List.ofSeq

    /// How many records are held, the `IN_Q_OVERFLOW` record included.
    let length (queue : InotifyQueue) : int = queue.Count

    /// How many bytes a read of every held record would return: what
    /// `ioctl(FIONREAD)` reports.
    let byteCount (queue : InotifyQueue) : int64 = queue.Bytes

    /// Whether the queue holds nothing.
    let isEmpty (queue : InotifyQueue) : bool = queue.Count = 0

    /// The queue with `record` added by Linux's `fsnotify_insert_event`, for an
    /// instance whose limit is `maxQueuedEvents` records, and whether it was
    /// added (rather than dropped).
    ///
    /// The rules, in Linux's order: at a queue already holding
    /// `maxQueuedEvents` records (counting an `IN_Q_OVERFLOW` record among
    /// them), `record` is dropped, and an `IN_Q_OVERFLOW` record is added
    /// instead unless one is already held; otherwise `record` is dropped if it
    /// repeats the newest record (`InotifyRecord.repeats`), and added if not.
    /// The limit is tested before the repeat (measured, row Q
    /// identical-at-full), and a record can follow an `IN_Q_OVERFLOW` record
    /// once reads have made room (row Q after-partial-read).
    ///
    /// `record` must not itself be an `IN_Q_OVERFLOW` record: only the queue
    /// makes those.
    let internal enqueue (maxQueuedEvents : int) (record : InotifyRecord) (queue : InotifyQueue) : InotifyQueue * bool =
        if maxQueuedEvents < 1 then
            failwith
                $"InotifyQueue.enqueue: a limit of %d{maxQueuedEvents} records, which UnixBootImage.withInotifyLimits refuses (this is a bug in this library, or in a caller that assembled the state by hand)."

        let append (record : InotifyRecord) : InotifyQueue =
            {
                Records = queue.Records.Enqueue record
                Count = queue.Count + 1
                Bytes = queue.Bytes + int64 (InotifyRecord.size record)
                Last = Some record
                Overflowed =
                    queue.Overflowed
                    || (
                        match record with
                        | InotifyRecord.QueueOverflow -> true
                        | InotifyRecord.Event _
                        | InotifyRecord.Ignored _ -> false
                    )
            }

        match record with
        | InotifyRecord.QueueOverflow ->
            failwith
                "InotifyQueue.enqueue: asked to add an IN_Q_OVERFLOW record, which only the queue itself adds (this is a bug in this library)."
        | InotifyRecord.Event _
        | InotifyRecord.Ignored _ ->

        if queue.Count >= maxQueuedEvents then
            if queue.Overflowed then
                queue, false
            else
                append InotifyRecord.QueueOverflow, true
        else
            match queue.Last with
            | Some last when InotifyRecord.repeats last record -> queue, false
            | Some _
            | None -> append record, true

    /// The oldest records a read of `count` bytes takes: as many whole records
    /// as fit, oldest first, and at least one.
    let internal take (count : int) (queue : InotifyQueue) : InotifyTake =
        let rec go (taken : InotifyRecord list) (room : int) (records : ImmutableQueue<InotifyRecord>) =
            if records.IsEmpty then
                List.rev taken, records
            else
                let next = records.Peek ()
                let size = InotifyRecord.size next

                if size > room then
                    List.rev taken, records
                else
                    go (next :: taken) (room - size) (records.Dequeue ())

        if queue.Count = 0 then
            InotifyTake.Empty
        elif InotifyRecord.size (queue.Records.Peek ()) > count then
            InotifyTake.FirstTooLarge
        else
            let taken, rest = go [] count queue.Records

            let takenBytes =
                taken |> List.sumBy (fun record -> int64 (InotifyRecord.size record))

            let overflowTaken =
                taken
                |> List.exists (fun record ->
                    match record with
                    | InotifyRecord.QueueOverflow -> true
                    | InotifyRecord.Event _
                    | InotifyRecord.Ignored _ -> false
                )

            let rest =
                {
                    Records = rest
                    Count = queue.Count - List.length taken
                    Bytes = queue.Bytes - takenBytes
                    Last = if rest.IsEmpty then None else queue.Last
                    Overflowed = queue.Overflowed && not overflowTaken
                }

            InotifyTake.Taken (taken, rest)

    /// The queue without its oldest record; the queue itself if it holds none.
    let internal dropOldest (queue : InotifyQueue) : InotifyQueue =
        if queue.Count = 0 then
            queue
        else
            let oldest = queue.Records.Peek ()
            let rest = queue.Records.Dequeue ()

            {
                Records = rest
                Count = queue.Count - 1
                Bytes = queue.Bytes - int64 (InotifyRecord.size oldest)
                Last = if rest.IsEmpty then None else queue.Last
                Overflowed =
                    queue.Overflowed
                    && (
                        match oldest with
                        | InotifyRecord.QueueOverflow -> false
                        | InotifyRecord.Event _
                        | InotifyRecord.Ignored _ -> true
                    )
            }

    /// A queue holding exactly `records`, oldest first, as `enqueue` could
    /// not necessarily have made it; for a caller assembling a state by hand.
    let internal ofList (records : InotifyRecord list) : InotifyQueue =
        let queue =
            (ImmutableQueue<InotifyRecord>.Empty, records)
            ||> List.fold (fun queue record -> queue.Enqueue record)

        {
            Records = queue
            Count = List.length records
            Bytes = records |> List.sumBy (fun record -> int64 (InotifyRecord.size record))
            Last = List.tryLast records
            Overflowed =
                records
                |> List.exists (fun record ->
                    match record with
                    | InotifyRecord.QueueOverflow -> true
                    | InotifyRecord.Event _
                    | InotifyRecord.Ignored _ -> false
                )
        }

/// Everything one inotify instance holds: its watches and the records it has
/// queued for `read(2)`.
type InotifyState =
    {
        /// The effective user ID of the process that created the instance,
        /// against whose per-user limits (`InotifyLimits`) the instance and its
        /// watches count.
        Owner : UserId
        /// The watches, by the descriptor each was given.
        Watches : Map<WatchDescriptor, InotifyWatch>
        /// The number the next watch is to be given. Starts at 1, and is never
        /// lowered: every number below it has been handed out, or burned by an
        /// add that failed for want of room under the per-user limit.
        NextWatchDescriptor : int
        /// The records queued for `read(2)`, oldest first.
        Queue : InotifyQueue
    }

/// The machine's inotify limits: Linux's `fs.inotify` sysctls, which an
/// administrator sets.
type InotifyLimits =
    {
        /// `fs.inotify.max_user_instances`: how many instances one user may
        /// hold at once, across every process on the machine. One more is
        /// `EMFILE`.
        MaxUserInstances : int
        /// `fs.inotify.max_user_watches`: how many watches one user's
        /// instances may hold at once. One more is `ENOSPC`.
        MaxUserWatches : int
        /// `fs.inotify.max_queued_events`: how many records one instance
        /// queues before it drops them and queues an `IN_Q_OVERFLOW` record.
        MaxQueuedEvents : int
    }

[<RequireQualifiedAccess>]
module InotifyLimits =
    /// The largest `MaxQueuedEvents` this kernel admits: a full queue of
    /// records of the longest name, and its `IN_Q_OVERFLOW` record, still
    /// count to a number of bytes an `int` holds, which is how
    /// `ioctl(FIONREAD)` reports them.
    let largestQueuedEvents : int =
        (System.Int32.MaxValue - InotifyRecord.HeaderSize)
        / (InotifyRecord.HeaderSize + 256)

    /// Whether this kernel admits `limits`: each positive, and
    /// `MaxQueuedEvents` at most `largestQueuedEvents`.
    let internal admissible (limits : InotifyLimits) : bool =
        limits.MaxUserInstances >= 1
        && limits.MaxUserWatches >= 1
        && limits.MaxQueuedEvents >= 1
        && limits.MaxQueuedEvents <= largestQueuedEvents

/// What `inotify_add_watch(2)`'s mask word says, once it is known to name only
/// bits `<sys/inotify.h>` defines.
type internal InotifyMaskWord =
    {
        /// Every event bit the word sets.
        Events : Set<InotifyEvent>
        /// `IN_ONLYDIR`.
        OnlyDirectory : bool
        /// `IN_DONT_FOLLOW`.
        DontFollow : bool
        /// `IN_EXCL_UNLINK`.
        ExcludeUnlinked : bool
        /// `IN_MASK_CREATE`.
        MaskCreate : bool
        /// `IN_MASK_ADD`.
        MaskAdd : bool
        /// `IN_ONESHOT`.
        OneShot : bool
    }

[<RequireQualifiedAccess>]
module internal InotifyMaskWord =
    let private onlyDirectory : uint32 = 0x01000000u
    let private dontFollow : uint32 = 0x02000000u
    let private excludeUnlinked : uint32 = 0x04000000u
    let private maskCreate : uint32 = 0x10000000u
    let private maskAdd : uint32 = 0x20000000u
    let private oneShot : uint32 = 0x80000000u

    /// Linux's `ALL_INOTIFY_BITS`: every event, the bits the kernel adds to a
    /// record (`IN_Q_OVERFLOW`, `IN_IGNORED`, `IN_ISDIR`), and every flag.
    let allBits : uint32 =
        (0u, InotifyEvent.all)
        ||> List.fold (fun bits event -> bits ||| InotifyEvent.bit event)
        ||| 0x4000u
        ||| 0x8000u
        ||| 0x40000000u
        ||| onlyDirectory
        ||| dontFollow
        ||| excludeUnlinked
        ||| maskCreate
        ||| maskAdd
        ||| oneShot

    /// The word, if it names only bits Linux defines and at least one of
    /// them; `None` is `EINVAL`. The bits the kernel adds to a record are
    /// accepted and mean nothing here (measured, `inotify-instance-probe.c`
    /// section M).
    let parse (word : uint32) : InotifyMaskWord option =
        if word &&& ~~~allBits <> 0u || word &&& allBits = 0u then
            None
        else
            Some
                {
                    Events =
                        InotifyEvent.all
                        |> List.filter (fun event -> word &&& InotifyEvent.bit event <> 0u)
                        |> Set.ofList
                    OnlyDirectory = word &&& onlyDirectory <> 0u
                    DontFollow = word &&& dontFollow <> 0u
                    ExcludeUnlinked = word &&& excludeUnlinked <> 0u
                    MaskCreate = word &&& maskCreate <> 0u
                    MaskAdd = word &&& maskAdd <> 0u
                    OneShot = word &&& oneShot <> 0u
                }
