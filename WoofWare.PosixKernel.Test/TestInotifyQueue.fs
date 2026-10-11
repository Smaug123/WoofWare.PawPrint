namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// An inotify instance's queue (`InotifyQueue`), its records' layout
/// (`InotifyRecord.toBytes`) and the mask word's parse, held to references
/// written from the measurements: `inotify-probe.c` sections L, C and O, and
/// `inotify-instance-probe.c` sections M and Q (docs/plans/2026-10-11-inotify).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyQueue =

    /// Fails unless `value` is at most `limit`.
    let private atMost<'a when 'a : comparison> (limit : 'a) (value : 'a) : unit =
        if value > limit then
            failwith $"%A{value} is above %A{limit}"

    let private name (text : string) : DirectoryEntryName =
        match DirectoryEntryName.parse text with
        | Ok name -> name
        | Error error -> failwith $"%s{text}: %A{error}"

    let private isOverflow (record : InotifyRecord) : bool =
        match record with
        | InotifyRecord.QueueOverflow -> true
        | InotifyRecord.Event _
        | InotifyRecord.Ignored _ -> false

    /// Linux's `event_compare`, stated again: the watch, the mask and the name,
    /// and never into an `IN_IGNORED`.
    let private repeatsLast (last : InotifyRecord) (next : InotifyRecord) : bool =
        match last, next with
        | InotifyRecord.Event (w, e, d, _, n), InotifyRecord.Event (w', e', d', _, n') ->
            w = w' && e = e' && d = d' && n = n'
        | _ -> false

    /// The records a queue holding at most `max` records ends with, given
    /// `records` and no read between them, stated over the whole sequence at
    /// once: drop each record that repeats the last one kept; if fewer than
    /// `max` are kept, that is the queue; otherwise the queue is the first
    /// `max` kept, and an `IN_Q_OVERFLOW` record if any record at all came
    /// after the one that filled it.
    let private settled (max : int) (records : InotifyRecord list) : InotifyRecord list =
        let kept =
            records
            |> List.indexed
            |> List.fold
                (fun (kept : (int * InotifyRecord) list) (index, record) ->
                    match kept with
                    | (_, last) :: _ when repeatsLast last record -> kept
                    | _ -> (index, record) :: kept
                )
                []
            |> List.rev

        if List.length kept < max then
            List.map snd kept
        else
            let fillingIndex, _ = kept.[max - 1]
            let first = kept |> List.take max |> List.map snd

            if List.length records - 1 > fillingIndex then
                first @ [ InotifyRecord.QueueOverflow ]
            else
                first

    let private recordGen : Gen<InotifyRecord> =
        gen {
            let! watch = Gen.elements [ 1 ; 2 ] |> Gen.map WatchDescriptor
            let! ignored = Gen.frequency [ 1, Gen.constant true ; 6, Gen.constant false ]

            if ignored then
                return InotifyRecord.Ignored watch
            else
                let! events =
                    Gen.elements
                        [
                            [ InotifyEvent.Modify ]
                            [ InotifyEvent.Attrib ]
                            [ InotifyEvent.Modify ; InotifyEvent.Attrib ]
                        ]

                let! isDirectory = Gen.elements [ false ; false ; true ]
                let! cookie = Gen.elements [ 0u ; 0u ; 7u ; 8u ]
                let! recordName = Gen.elements [ None ; Some (name "a") ; Some (name "b") ]
                return InotifyRecord.Event (watch, Set.ofList events, isDirectory, cookie, recordName)
        }

    [<RequireQualifiedAccess>]
    type private QueueLabel =
        | Coalesced
        | CoalescedAcrossCookies
        | Overflowed
        | RepeatAtFullOverflowed
        | NeverFilled

    [<Test>]
    let ``enqueueing a sequence ends with the records the whole-sequence reference states`` () : unit =
        let gen =
            gen {
                let! max = Gen.choose (1, 6)
                let! records = Gen.listOf recordGen |> Gen.resize 20
                return max, records
            }

        let property (cover : QueueLabel -> unit) (max : int, records : InotifyRecord list) : unit =
            let queue =
                (InotifyQueue.empty, records)
                ||> List.fold (fun queue record -> InotifyQueue.enqueue max record queue |> fst)

            let expected = settled max records
            InotifyQueue.toList queue |> shouldEqual expected
            InotifyQueue.length queue |> shouldEqual expected.Length

            InotifyQueue.byteCount queue
            |> shouldEqual (expected |> List.sumBy (InotifyRecord.size >> int64))

            if List.exists isOverflow expected then
                cover QueueLabel.Overflowed
            else
                cover QueueLabel.NeverFilled

            for previous, next in List.pairwise records do
                if repeatsLast previous next then
                    cover QueueLabel.Coalesced

                    match previous, next with
                    | InotifyRecord.Event (_, _, _, a, _), InotifyRecord.Event (_, _, _, b, _) when a <> b ->
                        cover QueueLabel.CoalescedAcrossCookies
                    | _ -> ()

            // A repeat of the newest record arriving at a full queue makes the
            // overflow record (measured, row Q identical-at-full).
            let full =
                (InotifyQueue.empty, records)
                ||> List.fold (fun queue record ->
                    if
                        InotifyQueue.length queue = max
                        && not (List.exists isOverflow (InotifyQueue.toList queue))
                        && repeatsLast (List.last (InotifyQueue.toList queue)) record
                    then
                        cover QueueLabel.RepeatAtFullOverflowed

                    InotifyQueue.enqueue max record queue |> fst
                )

            ignore<InotifyQueue> full

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 2000) (Arb.fromGen gen) property

        for label in
            [
                QueueLabel.Coalesced
                QueueLabel.CoalescedAcrossCookies
                QueueLabel.Overflowed
                QueueLabel.RepeatAtFullOverflowed
                QueueLabel.NeverFilled
            ] do
            if coverage.Count label < 20 then
                failwith $"the fixed sample reached %A{label} only %d{coverage.Count label} times"

    /// What one step does to the queue.
    [<RequireQualifiedAccess>]
    type private Step =
        | Enqueue of InotifyRecord
        | Take of count : int
        | DropOldest

    /// Linux's `fsnotify_insert_event` and `inotify_read`, over a list.
    let private referenceStep (max : int) (step : Step) (queue : InotifyRecord list) : InotifyRecord list =
        match step with
        | Step.Enqueue record ->
            if List.length queue >= max then
                if List.exists isOverflow queue then
                    queue
                else
                    queue @ [ InotifyRecord.QueueOverflow ]
            else
                match List.tryLast queue with
                | Some last when repeatsLast last record -> queue
                | _ -> queue @ [ record ]
        | Step.Take count ->
            let rec go (room : int) (rest : InotifyRecord list) =
                match rest with
                | record :: tail when InotifyRecord.size record <= room -> go (room - InotifyRecord.size record) tail
                | _ -> rest

            match queue with
            | first :: _ when InotifyRecord.size first <= count -> go count queue
            | _ -> queue
        | Step.DropOldest ->
            match queue with
            | [] -> []
            | _ :: rest -> rest

    [<RequireQualifiedAccess>]
    type private StepLabel =
        | RecordBehindOverflow
        | FirstTooLarge
        | TookSeveral
        | OverflowTaken

    [<Test>]
    let ``reads between records leave the queue as Linux's list would, and its invariants hold`` () : unit =
        let stepGen : Gen<Step> =
            Gen.frequency
                [
                    6, recordGen |> Gen.map Step.Enqueue
                    2, Gen.elements [ 0 ; 15 ; 16 ; 31 ; 32 ; 48 ; 64 ; 100 ] |> Gen.map Step.Take
                    1, Gen.constant Step.DropOldest
                ]

        let gen =
            gen {
                let! max = Gen.choose (1, 5)
                let! steps = Gen.listOf stepGen |> Gen.resize 30
                return max, steps
            }

        let property (cover : StepLabel -> unit) (max : int, steps : Step list) : unit =
            let mutable queue = InotifyQueue.empty
            let mutable reference : InotifyRecord list = []

            for step in steps do
                let before = queue

                queue <-
                    match step with
                    | Step.Enqueue record -> InotifyQueue.enqueue max record queue |> fst
                    | Step.Take count ->
                        match InotifyQueue.take count queue with
                        | InotifyTake.Empty ->
                            InotifyQueue.isEmpty queue |> shouldEqual true
                            queue
                        | InotifyTake.FirstTooLarge ->
                            cover StepLabel.FirstTooLarge

                            InotifyRecord.size (List.head (InotifyQueue.toList queue))
                            |> shouldBeGreaterThan count

                            queue
                        | InotifyTake.Taken (records, rest) ->
                            if List.length records > 1 then
                                cover StepLabel.TookSeveral

                            if List.exists isOverflow records then
                                cover StepLabel.OverflowTaken

                            records @ InotifyQueue.toList rest |> shouldEqual (InotifyQueue.toList before)
                            records |> List.sumBy InotifyRecord.size |> atMost count
                            rest
                    | Step.DropOldest -> InotifyQueue.dropOldest queue

                reference <- referenceStep max step reference

                let records = InotifyQueue.toList queue
                records |> shouldEqual reference
                InotifyQueue.length queue |> shouldEqual records.Length

                InotifyQueue.byteCount queue
                |> shouldEqual (records |> List.sumBy (InotifyRecord.size >> int64))

                InotifyQueue.isEmpty queue |> shouldEqual records.IsEmpty
                records.Length |> atMost (max + 1)
                records |> List.filter isOverflow |> List.length |> atMost 1

                // The representation does not leak into equality.
                queue |> shouldEqual (InotifyQueue.ofList records)

                (queue :> obj).GetHashCode ()
                |> shouldEqual ((InotifyQueue.ofList records :> obj).GetHashCode ())

                match List.tryFindIndex isOverflow records with
                | Some index when index < records.Length - 1 -> cover StepLabel.RecordBehindOverflow
                | _ -> ()

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1000) (Arb.fromGen gen) property

        for label in
            [
                StepLabel.RecordBehindOverflow
                StepLabel.FirstTooLarge
                StepLabel.TookSeveral
                StepLabel.OverflowTaken
            ] do
            if coverage.Count label < 20 then
                failwith $"the fixed sample reached %A{label} only %d{coverage.Count label} times"

    [<Test>]
    let ``a full queue's overflow record is the 16385th of 16384, and nothing follows it until a read makes room``
        ()
        : unit
        =
        // `inotify-probe.c` section O and `inotify-instance-probe.c` section Q,
        // with alternating records so that none coalesces.
        let attrib (n : string) =
            InotifyRecord.Event (WatchDescriptor 1, Set.singleton InotifyEvent.Attrib, false, 0u, Some (name n))

        let fill (count : int) (queue : InotifyQueue) : InotifyQueue =
            (queue, [ 0 .. count - 1 ])
            ||> List.fold (fun queue i ->
                InotifyQueue.enqueue 16384 (attrib (if i % 2 = 0 then "f" else "g")) queue
                |> fst
            )

        let full = fill 16384 InotifyQueue.empty
        InotifyQueue.byteCount full |> shouldEqual 524288L

        // Row Q identical-at-full: a repeat of the newest, at a full queue.
        let overflowed = InotifyQueue.enqueue 16384 (attrib "g") full |> fst
        InotifyQueue.byteCount overflowed |> shouldEqual 524304L
        InotifyQueue.length overflowed |> shouldEqual 16385

        List.last (InotifyQueue.toList overflowed)
        |> shouldEqual InotifyRecord.QueueOverflow

        // Rows Q partial-read and after-partial-read: two records read, then
        // two more arrive; the first follows the overflow record, and the
        // second is dropped.
        let afterRead =
            match InotifyQueue.take 64 (fill 10 overflowed) with
            | InotifyTake.Taken (taken, rest) ->
                List.length taken |> shouldEqual 2
                rest
            | other -> failwith $"%A{other}"

        let afterRecords =
            afterRead
            |> InotifyQueue.enqueue 16384 (attrib "h")
            |> fst
            |> InotifyQueue.enqueue 16384 (attrib "f")
            |> fst

        let records = InotifyQueue.toList afterRecords
        records.Length |> shouldEqual 16384
        List.findIndex isOverflow records |> shouldEqual 16382

        // Row Q after-read-one: with one read, nothing more fits.
        let afterOne =
            match InotifyQueue.take 32 overflowed with
            | InotifyTake.Taken (_, rest) -> rest |> InotifyQueue.enqueue 16384 (attrib "h") |> fst
            | other -> failwith $"%A{other}"

        let records = InotifyQueue.toList afterOne
        records.Length |> shouldEqual 16384
        List.findIndex isOverflow records |> shouldEqual 16383

    /// `struct inotify_event`, read back from bytes.
    let private header (bytes : byte[]) : int * uint32 * uint32 * uint32 =
        BinaryPrimitives.ReadInt32LittleEndian (ReadOnlySpan<byte> (bytes, 0, 4)),
        BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, 4, 4)),
        BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, 8, 4)),
        BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan<byte> (bytes, 12, 4))

    [<Test>]
    let ``a record's len is the name and its NUL rounded up to 16, zero-padded, as measured`` () : unit =
        // `inotify-probe.c` section L: name length, then the record's size and len.
        let measured =
            [
                1, 32, 16
                2, 32, 16
                11, 32, 16
                12, 32, 16
                13, 32, 16
                15, 32, 16
                16, 48, 32
                17, 48, 32
                31, 48, 32
                32, 64, 48
                255, 272, 256
            ]

        InotifyRecord.HeaderSize |> shouldEqual 16

        for length, size, len in measured do
            let text = String ('x', length)

            let record =
                InotifyRecord.Event (WatchDescriptor 3, Set.singleton InotifyEvent.Create, false, 0u, Some (name text))

            InotifyRecord.size record |> shouldEqual size
            let bytes = (InotifyRecord.toBytes record).AsSpan().ToArray ()
            bytes.Length |> shouldEqual size
            header bytes |> shouldEqual (3, 0x100u, 0u, uint32 len)
            bytes.[16 .. 16 + length - 1] |> shouldEqual (Text.Encoding.ASCII.GetBytes text)
            bytes.[16 + length ..] |> Array.forall ((=) 0uy) |> shouldEqual true

    [<Test>]
    let ``the kernel's own records are laid out as measured`` () : unit =
        // `inotify-probe.c` rows W rm-events and O drained.
        let ignored =
            (InotifyRecord.toBytes (InotifyRecord.Ignored (WatchDescriptor 1))).AsSpan().ToArray ()

        header ignored |> shouldEqual (1, 0x8000u, 0u, 0u)
        ignored.Length |> shouldEqual 16

        let overflow =
            (InotifyRecord.toBytes InotifyRecord.QueueOverflow).AsSpan().ToArray ()

        header overflow |> shouldEqual (-1, 0x4000u, 0u, 0u)
        overflow.Length |> shouldEqual 16

        // `inotify-probe.c` row E chmod-watched-dir: ATTRIB+ISDIR, len 0.
        let self =
            (InotifyRecord.toBytes (
                InotifyRecord.Event (WatchDescriptor 1, Set.singleton InotifyEvent.Attrib, true, 0u, None)
            ))
                .AsSpan()
                .ToArray ()

        header self |> shouldEqual (1, 0x40000004u, 0u, 0u)

        // `inotify-probe.c` row K adjacent-renames: a cookie is carried as is.
        let moved =
            (InotifyRecord.toBytes (
                InotifyRecord.Event (
                    WatchDescriptor 2,
                    Set.singleton InotifyEvent.MovedFrom,
                    false,
                    7544u,
                    Some (name "a")
                )
            ))
                .AsSpan()
                .ToArray ()

        header moved |> shouldEqual (2, 0x40u, 7544u, 16u)

    /// `ALL_INOTIFY_BITS`, from `<sys/inotify.h>`: the twelve events,
    /// `IN_UNMOUNT`, `IN_Q_OVERFLOW`, `IN_IGNORED`, and the flags `IN_ONLYDIR`,
    /// `IN_DONT_FOLLOW`, `IN_EXCL_UNLINK`, `IN_MASK_CREATE`, `IN_MASK_ADD`,
    /// `IN_ISDIR`, `IN_ONESHOT`.
    let private allInotifyBits : uint32 =
        0x00000FFFu
        ||| 0x2000u
        ||| 0x4000u
        ||| 0x8000u
        ||| 0x01000000u
        ||| 0x02000000u
        ||| 0x04000000u
        ||| 0x10000000u
        ||| 0x20000000u
        ||| 0x40000000u
        ||| 0x80000000u

    [<Test>]
    let ``the mask word is EINVAL exactly when it names an undefined bit or none at all`` () : unit =
        // `inotify-instance-probe.c` section M, and `inotify-probe.c` row W mask-0.
        let measured =
            [
                0x1000u, false
                0x08000000u, false
                0x8000u, true
                0x40000000u, true
                0x4000u, true
                0x2000u, true
                0x80000000u, true
                0x04000000u, true
                0x02000000u, true
                0x20000000u, true
                0x10000000u, true
                0x100u ||| 0x1000u, false
                0x100u ||| 0x08000000u, false
                0u, false
                0x01000000u, true
            ]

        for word, accepted in measured do
            (InotifyMaskWord.parse word).IsSome |> shouldEqual accepted

        let property (word : uint32) : unit =
            match InotifyMaskWord.parse word with
            | None -> (word &&& ~~~allInotifyBits <> 0u || word = 0u) |> shouldEqual true
            | Some parsed ->
                (word &&& ~~~allInotifyBits) |> shouldEqual 0u
                word |> shouldNotEqual 0u

                parsed.Events
                |> Set.toList
                |> List.map InotifyEvent.bit
                |> List.fold (|||) 0u
                |> shouldEqual (word &&& (0x00000FFFu ||| 0x2000u))

                parsed.OnlyDirectory |> shouldEqual (word &&& 0x01000000u <> 0u)
                parsed.DontFollow |> shouldEqual (word &&& 0x02000000u <> 0u)
                parsed.ExcludeUnlinked |> shouldEqual (word &&& 0x04000000u <> 0u)
                parsed.MaskCreate |> shouldEqual (word &&& 0x10000000u <> 0u)
                parsed.MaskAdd |> shouldEqual (word &&& 0x20000000u <> 0u)
                parsed.OneShot |> shouldEqual (word &&& 0x80000000u <> 0u)

        let wordGen =
            Gen.oneof
                [
                    ArbMap.defaults |> ArbMap.generate<uint32>
                    Gen.subListOf [ for bit in 0..31 -> 1u <<< bit ] |> Gen.map (List.fold (|||) 0u)
                    Gen.subListOf
                        [
                            for bit in 0..31 do
                                if (1u <<< bit) &&& allInotifyBits <> 0u then
                                    yield 1u <<< bit
                        ]
                    |> Gen.map (List.fold (|||) 0u)
                ]

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen wordGen) property)
