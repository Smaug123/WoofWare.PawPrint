namespace WoofWare.PosixKernel.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test.InotifyReplay

/// Which records a filesystem change queues on the inotify instances watching
/// it, replayed against what Linux 6.18.5 aarch64 answered in
/// docs/plans/2026-10-11-inotify: `inotify-probe.c` sections E, N, L, C, K
/// and O, and `inotify-delivery-probe.c` sections V, Z, Y, X, R, P, T and C.
/// Each test makes the probe's calls on the model, renders each row as the
/// probe printed it, and compares the rows with the recorded ones.
///
/// Rows are skipped, each named, where the model cannot make the call or does
/// not yet report what it records:
/// - a change made through a descriptor (`write`, `pwrite`, `ftruncate`), which
///   the model does not report to a watch;
/// - `truncate(2)`, which the model does not have;
/// - the records of the watched inode itself going (`IN_DELETE_SELF`,
///   `IN_MOVE_SELF`, and the `IN_IGNORED` after them), which the model does not
///   queue;
/// - a change through the current directory after `rmdir` removed it, which
///   Linux reports to the old parent under the old name, a name the model does
///   not keep.
///
/// A watch the probe gave `IN_ALL_EVENTS` asks here for the events the model
/// raises, and the rows lose the records of the events it does not
/// (`withoutUnmodelled`).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyDeliveryMeasured =

    let private probe = lazy recorded "WoofWare.PosixKernel.Test.inotify.linux.txt"

    let private deliveryProbe =
        lazy recorded "WoofWare.PosixKernel.Test.inotifyDelivery.linux.txt"

    /// The rows `section` of `file` recorded, but `skipped`, with the records
    /// of unmodelled events dropped and cookies numbered by first appearance.
    let private expected
        (file : Lazy<(string * string * string) list>)
        (section : string)
        (skipped : string list)
        : (string * string) list
        =
        rows file section skipped
        |> List.map (fun (case, text) -> case, withoutUnmodelled text)
        |> normaliseCookies

    /// The rows the replay produced, but `skipped`, with cookies numbered by
    /// first appearance.
    let private actual (skipped : string list) (produced : (string * string) seq) : (string * string) list =
        produced
        |> List.ofSeq
        |> List.filter (fun (case, _) -> not (List.contains case skipped))
        |> normaliseCookies

    /// Rows of changes made through a descriptor.
    let private throughDescriptor : string list = [ "write" ; "pwrite" ; "ftruncate" ]

    /// `inotify-probe.c`'s `operations`: every operation of section E or N on
    /// the directory `dir`, watched with `mask`, each row the records it
    /// queued.
    let private operations (dir : string) (mask : uint32) : (string * string) list =
        let mutable system =
            boot
                root
                [
                    dir,
                    directory
                        0o755
                        [
                            "existing", file "seed\n"
                            "victim", file "seed\n"
                            "sub-victim", directory 0o755 []
                        ]
                    "u", directory 0o755 [ "incoming", file "seed\n" ]
                ]

        let p (entry : string) = $"%s{dir}/%s{entry}"
        let fd, s = init1 nonblock system
        let wd, s = watch fd dir mask s
        system <- s
        let produced = ResizeArray<string * string> ()

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        let f, s =
            openFd (openFlags FileAccessMode.WriteOnly true true false) (p "f") 0o644 system

        system <- s
        row "open-creat"
        system <- write f "hello" system
        row "write"
        system <- write f "" system
        row "write-0"
        system <- pwrite f "p" 0L system
        row "pwrite"
        system <- ftruncate f 2L system
        row "ftruncate"
        system <- close f system
        row "close-write"

        let f, s =
            openFd (openFlags FileAccessMode.ReadOnly false false false) (p "f") 0 system

        system <- s
        row "open-rdonly"
        system <- readFile f 8 system
        row "read"
        system <- readFile f 8 system
        row "read-at-eof"
        system <- close f system
        row "close-nowrite"

        let f, s =
            openFd (openFlags FileAccessMode.WriteOnly false false true) (p "existing") 0 system

        system <- s
        row "open-trunc-existing"
        system <- close f system
        row "close-after-trunc"
        system <- chmod (p "f") 0o600 system
        row "chmod"
        system <- chown (p "f") (Some 1000u) (Some 1000u) system
        row "chown"
        system <- utimensat (p "f") (TimesArgument.Fields (timespec 1000L 0L, timespec 2000L 0L)) system
        row "utimensat"
        // `truncate(2)`, which the model does not have.
        row "truncate"
        system <- chmod dir 0o755 system
        row "chmod-watched-dir"
        system <- mkdir (p "sub") 0o755 system
        row "mkdir"
        system <- writeFile (p "sub/inner") "x" system
        row "write-in-unwatched-subdir"
        system <- unlink (p "sub/inner") system
        system <- rmdir (p "sub") system
        row "rmdir"
        system <- symlinkAt "f" (p "lnk") system
        row "symlink"
        system <- link (p "f") (p "hard") system
        row "link"
        system <- unlink (p "hard") system
        row "unlink-one-of-two-names"
        system <- unlink (p "lnk") system
        row "unlink-symlink"
        system <- rename (p "f") (p "g") system
        row "rename-within"
        system <- rename (p "g") (p "existing") system
        row "rename-over-existing"
        system <- rename "u/incoming" (p "arrived") system
        row "rename-in"
        system <- rename (p "arrived") "u/departed" system
        row "rename-out"
        system <- rename (p "sub-victim") (p "sub-renamed") system
        row "rename-dir-within"
        system <- rename (p "existing") (p "existing") system
        row "rename-noop"
        system <- unlink (p "existing") system
        row "unlink"
        system <- unlink (p "victim") system
        system <- rmdir (p "sub-renamed") system
        row "rmdir-renamed-dir"
        let again, s = watch fd dir mask system
        system <- s
        produced.Add ("wd-after-all", $"add again=%d{again} (was %d{wd})")
        List.ofSeq produced

    [<Test>]
    let ``section E: what each operation queues on a watched directory`` () : unit =
        let skipped = "truncate" :: throughDescriptor

        actual skipped (operations "e" modelledEvents)
        |> shouldEqual (expected probe "E" skipped)

    [<Test>]
    let ``section N: what each operation queues under FileSystemWatcher's mask`` () : unit =
        let skipped = "truncate" :: throughDescriptor

        actual skipped (operations "n" dotnetMask)
        |> shouldEqual (expected probe "N" skipped)

    [<Test>]
    let ``section L: a record's length for names of several lengths`` () : unit =
        let mutable system = boot root [ "l", directory 0o755 [] ]
        let fd, s = init1 nonblock system
        let _, s = watch fd "l" create s
        system <- s
        let produced = ResizeArray<string * string> ()

        for i, length in List.indexed [ 1 ; 2 ; 11 ; 12 ; 13 ; 15 ; 16 ; 17 ; 31 ; 32 ; 255 ] do
            let entry = String (char (int 'a' + i % 26), length)
            system <- writeFile $"l/%s{entry}" "x" system

            match UnixReadWrite.tryRead 0 fd UserBuffer.Mapped 1024UL system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), s) ->
                system <- s
                let first = parse bytes |> List.head

                produced.Add (
                    "name-length",
                    $"%d{length} chars: record=%d{bytes.Length} bytes len=%d{first.Len} zero-padded=%d{(if first.ZeroPadded then 1 else 0)}"
                )
            | other -> failwith $"read: %A{other}"

            system <- unlink $"l/%s{entry}" system

        produced.Add ("sizeof", $"sizeof(struct inotify_event)=%d{InotifyRecord.HeaderSize}")
        actual [] produced |> shouldEqual (expected probe "L" [])

    [<Test>]
    let ``section C: a record repeating the newest is dropped`` () : unit =
        let mutable system =
            boot root [ "c", directory 0o755 [ "f", file "seed" ; "g", file "seed" ] ]

        let fd, s = init1 nonblock system
        let _, s = watch fd "c" modelledEvents s

        let f, s = openFd (openFlags FileAccessMode.WriteOnly false false false) "c/f" 0 s

        let _, s = drain fd s
        system <- s
        let produced = ResizeArray<string * string> ()

        let counted (case : string) =
            let records, s = drainRecords fd system
            system <- s
            let text = records |> List.map render |> String.concat " | "
            produced.Add (case, $"%d{records.Length} events: %s{text}")

        system <- system |> write f "1" |> write f "2" |> write f "3"
        counted "three-writes"
        system <- system |> write f "4" |> chmod "c/f" 0o600 |> write f "5"
        counted "write-chmod-write"
        system <- system |> chmod "c/f" 0o644 |> chmod "c/g" 0o644 |> chmod "c/f" 0o600
        counted "chmod-f-g-f"
        system <- system |> chmod "c/f" 0o644 |> chmod "c/f" 0o600
        counted "chmod-f-f"
        system <- close f system
        let _, s = drain fd system
        system <- writeFile "c/h" "x" s

        let g, s =
            openFd (openFlags FileAccessMode.WriteOnly false false false) "c/g" 0 system

        system <- s |> write g "a" |> write g "b" |> writeFile "c/i" "x" |> close g
        counted "create-write-write-create"
        system <- chmod "c/f" 0o644 system

        let first =
            match UnixReadWrite.tryRead 0 fd UserBuffer.Mapped 1024UL system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), s) ->
                system <- s
                bytes.Length
            | other -> failwith $"read: %A{other}"

        system <- chmod "c/f" 0o600 system

        let second =
            match UnixReadWrite.tryRead 0 fd UserBuffer.Mapped 1024UL system with
            | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), s) ->
                system <- s
                bytes.Length
            | other -> failwith $"read: %A{other}"

        produced.Add ("separated-by-read", $"first read=%d{first} second read=%d{second}")
        let skipped = [ "three-writes" ; "write-chmod-write" ; "create-write-write-create" ]
        actual skipped produced |> shouldEqual (expected probe "C" skipped)

    [<Test>]
    let ``section K: the two records of a rename share a cookie, and the next rename's is one more`` () : unit =
        let mutable system =
            boot root [ "k", directory 0o755 [ "a", file "x" ; "b", file "x" ] ]

        let fd, s = init1 nonblock system
        let fd2, s = init1 nonblock s
        let _, s = watch fd "k" (movedFrom ||| movedTo) s
        let _, s = watch fd2 "k" (movedFrom ||| movedTo) s
        system <- s |> rename "k/a" "k/a2" |> rename "k/b" "k/b2"
        let produced = ResizeArray<string * string> ()
        let first, s = readRecords fd 1024 system
        system <- s

        let e1, e2, e3 =
            match first with
            | e1 :: e2 :: e3 :: _ -> e1, e2, e3
            | other -> failwith $"%A{other}"

        produced.Add (
            "adjacent-renames",
            $"events=%d{first.Length} cookie1=cookie2:%d{(if e1.Cookie = e2.Cookie then 1 else 0)} cookie3-cookie1=%d{int (e3.Cookie - e1.Cookie)}"
        )

        let second, _ = readRecords fd2 1024 system

        produced.Add (
            "second-instance",
            $"events=%d{second.Length} same cookie as first instance=%d{(if (List.head second).Cookie = e1.Cookie then 1 else 0)}"
        )

        actual [] produced |> shouldEqual (expected probe "K" [ "first-cookie" ])

    [<Test>]
    let ``section O: changes past the queue's limit, and after it drains`` () : unit =
        let limit = 16384
        let produced = ResizeArray<string * string> ()

        let mutable system =
            boot root [ "o", directory 0o755 [ "f", file "x" ; "g", file "x" ] ]

        let fd, s = init1 nonblock system
        let _, s = watch fd "o" (attrib ||| create) s
        system <- s
        let changes = limit + 5000

        let fill () =
            for i in 0 .. changes - 1 do
                system <- chmod (if i % 2 = 1 then "o/f" else "o/g") (0o600 + i % 2) system

        fill ()

        let available =
            match UnixDescriptor.tryBytesAvailable fd UserBuffer.Mapped system with
            | Ok (BytesAvailableAnswer.Reported count) -> count
            | other -> failwith $"FIONREAD: %A{other}"

        produced.Add ("fionread-full", $"%d{available} bytes after %d{changes} alternating chmods")
        let records, s = drainRecords fd system
        system <- s

        let overflowAt =
            records |> List.findIndex (fun record -> record.Mask &&& 0x4000u <> 0u)

        let overflow = records.[overflowAt]

        produced.Add (
            "drained",
            $"records=%d{records.Length} overflow-record-index=%d{overflowAt} (0-based) records-after-overflow=%d{records.Length - overflowAt - 1} overflow=wd=%d{overflow.Wd} mask=%s{maskNames overflow.Mask} cookie=%d{overflow.Cookie} len=%d{overflow.Len}"
        )

        system <- chmod "o/f" 0o644 system
        let drained, s = drain fd system
        system <- s
        produced.Add ("after-overflow-drained", drained)
        fill ()
        let records, s = drainRecords fd system
        system <- s

        let overflows =
            records
            |> List.filter (fun record -> record.Mask &&& 0x4000u <> 0u)
            |> List.length

        produced.Add ("second-fill", $"records=%d{records.Length} overflow-records=%d{overflows}")
        fill ()
        system <- mkdir "o/late" 0o755 system
        let late, s = watch fd "o/late" create system
        system <- writeFile "o/late/x" "x" s
        let records, s = drainRecords fd system
        system <- s
        let forLate = records |> List.filter (fun record -> record.Wd = late) |> List.length

        produced.Add (
            "event-for-new-watch-while-full",
            $"late wd=%d{late} records=%d{records.Length} records-for-late=%d{forLate}"
        )

        system <- writeFile "o/late/y" "x" system
        let drained, _ = drain fd system
        produced.Add ("late-after-drain", drained)
        actual [] produced |> shouldEqual (expected probe "O" [ "sysctl" ])

    // ------------------------------------------------- inotify-delivery-probe.c

    /// The probe's `timesUnder`: `utimensat` setting both times, one, or
    /// neither, on a file in the directory `dir`, watched with `mask`, and on
    /// the directory itself.
    let private timesUnder (maskName : string) (mask : uint32) : (string * string) list =
        let dir = $"v-%s{maskName}"
        let mutable system = boot root [ dir, directory 0o755 [ "f", file "x" ] ]
        let fd, s = init1 nonblock system
        let _, s = watch fd dir mask s
        system <- s
        let produced = ResizeArray<string * string> ()

        let cases =
            [
                "both", TimesArgument.Fields (timespec 1000L 0L, timespec 2000L 0L)
                "null", TimesArgument.Null
                "atime-only", TimesArgument.Fields (timespec 3000L 0L, timespec 0L utimeOmit)
                "mtime-only", TimesArgument.Fields (timespec 0L utimeOmit, timespec 4000L 0L)
                "atime-now-only", TimesArgument.Fields (timespec 0L utimeNow, timespec 0L utimeOmit)
                "mtime-now-only", TimesArgument.Fields (timespec 0L utimeOmit, timespec 0L utimeNow)
                "neither", TimesArgument.Fields (timespec 0L utimeOmit, timespec 0L utimeOmit)
            ]

        for case, times in cases do
            system <- utimensat $"%s{dir}/f" times system
            let drained, s = drain fd system
            system <- s
            produced.Add ($"%s{maskName}-%s{case}", drained)

        system <- utimensat dir TimesArgument.Null system
        let drained, _ = drain fd system
        produced.Add ($"%s{maskName}-watched-dir", drained)
        List.ofSeq produced

    [<Test>]
    let ``delivery section V: utimensat of both times, one, or neither`` () : unit =
        actual [] (timesUnder "all" modelledEvents @ timesUnder "dotnet" dotnetMask)
        |> shouldEqual (expected deliveryProbe "V" [])

    /// The probe's `fillToOneShort`: chmods alternating between `a` and `b`
    /// until the instance `fd` holds one record fewer than `limit`.
    let private fillToOneShort
        (fd : int)
        (a : string)
        (b : string)
        (limit : int)
        (system : UnixSystem<int, string>)
        : UnixSystem<int, string>
        =
        let system =
            (system, [ 0 .. limit - 2 ])
            ||> List.fold (fun system i -> chmod (if i % 2 = 1 then a else b) (0o600 + i % 2) system)

        match UnixDescriptor.tryBytesAvailable fd UserBuffer.Mapped system with
        | Ok (BytesAvailableAnswer.Reported count) when count = (limit - 1) * 32 -> system
        | other -> failwith $"filled to %A{other}, not %d{(limit - 1) * 32} bytes"

    [<Test>]
    let ``delivery section Z: a truncation or chown that strips set-ID bits is one record`` () : unit =
        let limit = 16384
        let produced = ResizeArray<string * string> ()

        // `open(O_TRUNC)` by uid 1000, of its own file.
        let mutable system = boot unprivileged [ "z", directoryOwnedBy 1000u 0o777 [] ]

        for full in [ false ; true ] do
            let d = if full then "z/full" else "z/d"
            system <- system |> mkdir d 0o777 |> chmod d 0o777

            system <-
                system
                |> writeFile $"%s{d}/f" "hello"
                |> writeFile $"%s{d}/a" "x"
                |> writeFile $"%s{d}/b" "x"

            system <- chmod $"%s{d}/f" 0o6755 system
            let before = modeOf $"%s{d}/f" system
            let fd, s = init1 nonblock system
            let _, s = watch fd d modelledEvents s

            system <-
                if full then
                    fillToOneShort fd $"%s{d}/a" $"%s{d}/b" limit s
                else
                    s

            let t, s =
                openFd (openFlags FileAccessMode.WriteOnly false false true) $"%s{d}/f" 0 system

            system <- close t s
            let after = modeOf $"%s{d}/f" system

            if full then
                let records, s = drainRecords fd system
                system <- s

                let overflows =
                    records
                    |> List.filter (fun record -> record.Mask &&& 0x4000u <> 0u)
                    |> List.length

                produced.Add (
                    "open-trunc-at-one-short",
                    $"records=%d{records.Length} overflows=%d{overflows} (filled %d{limit - 1})"
                )
            else
                let drained, s = drain fd system
                system <- s
                produced.Add ("open-trunc", $"mode %o{before} -> %o{after}: %s{drained}")

            system <- close fd system

        // `chown` by root.
        let mutable system = boot root [ "z", directory 0o777 [] ]

        for full in [ false ; true ] do
            let d = if full then "z/full" else "z/r"
            system <- mkdir d 0o777 system

            system <-
                system
                |> writeFile $"%s{d}/f" "hello"
                |> writeFile $"%s{d}/a" "x"
                |> writeFile $"%s{d}/b" "x"

            system <- chmod $"%s{d}/f" 0o6755 system
            let fd, s = init1 nonblock system
            let _, s = watch fd d modelledEvents s

            system <-
                if full then
                    fillToOneShort fd $"%s{d}/a" $"%s{d}/b" limit s
                else
                    s

            system <- chown $"%s{d}/f" (Some 1000u) (Some 1000u) system
            let after = modeOf $"%s{d}/f" system

            if full then
                let records, s = drainRecords fd system
                system <- s

                let overflows =
                    records
                    |> List.filter (fun record -> record.Mask &&& 0x4000u <> 0u)
                    |> List.length

                produced.Add (
                    "chown-at-one-short",
                    $"records=%d{records.Length} overflows=%d{overflows} (filled %d{limit - 1})"
                )
            else
                let drained, s = drain fd system
                system <- s
                produced.Add ("chown", $"mode 6755 -> %o{after}: %s{drained}")

            system <- close fd system

        // `truncate(2)`, which the model does not have, and writes, which are
        // made through a descriptor.
        let skipped =
            [
                "truncate"
                "truncate-at-one-short"
                "truncate-dotnet"
                "write"
                "write-at-one-short"
            ]

        actual skipped produced |> shouldEqual (expected deliveryProbe "Z" skipped)

    [<Test>]
    let ``delivery section Y: an inode watched beside its parent hears of its own change after the parent`` () : unit =
        let mutable system =
            boot root [ "y", directory 0o755 [ "d", directory 0o755 [] ; "f", file "x" ] ]

        let fd, s = init1 nonblock system
        let wp, s = watch fd "y" modelledEvents s
        let wf, s = watch fd "y/f" modelledEvents s
        let wd, s = watch fd "y/d" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"parent=%d{wp} f=%d{wf} d=%d{wd}")

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        system <- chmod "y/f" 0o600 system
        row "chmod-file"
        system <- chmod "y/d" 0o700 system
        row "chmod-dir"

        let t, s =
            openFd (openFlags FileAccessMode.WriteOnly false false true) "y/f" 0 system

        system <- close t s
        row "open-trunc-file"
        system <- utimensat "y/f" TimesArgument.Null system
        row "utimensat-file"
        let fd2, s = init1 nonblock system
        let _, s = watch fd2 "y" modelledEvents s
        system <- chmod "y/f" 0o644 s
        row "two-instances-first"
        let drained, _ = drain fd2 system
        produced.Add ("two-instances-second", drained)
        let skipped = [ "truncate-file" ]
        actual skipped produced |> shouldEqual (expected deliveryProbe "Y" skipped)

    [<Test>]
    let ``delivery section X: a link count goes to the inode's own watch, before the parent's name`` () : unit =
        let mutable system = boot root [ "x", directory 0o755 [ "f", file "x" ] ]
        let fd, s = init1 nonblock system
        let wp, s = watch fd "x" modelledEvents s
        let wf, s = watch fd "x/f" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"parent=%d{wp} f=%d{wf}")

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        system <- link "x/f" "x/g" system
        row "link"
        system <- unlink "x/g" system
        row "unlink-other-name"
        system <- mkdir "x/d" 0o755 system
        let wd, s = watch fd "x/d" modelledEvents system
        let _, s = drain fd s
        system <- mkdir "x/d/sub" 0o755 s
        let drained, s = drain fd system
        system <- s
        produced.Add ("mkdir-in-watched-dir", $"wd=%d{wd}: %s{drained}")
        system <- rmdir "x/d/sub" system
        row "rmdir-in-watched-dir"
        actual [] produced |> shouldEqual (expected deliveryProbe "X" [])

    [<Test>]
    let ``delivery section R: the inode a rename displaces hears of its lost link`` () : unit =
        let mutable system =
            boot
                root
                [
                    "r",
                    directory
                        0o755
                        [
                            "a", file "a"
                            "b", file "b"
                            "sd", directory 0o755 []
                            "td", directory 0o755 []
                        ]
                ]

        let fd, s = init1 nonblock system
        let wp, s = watch fd "r" modelledEvents s
        let wb, s = watch fd "r/b" modelledEvents s
        let wt, s = watch fd "r/td" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"parent=%d{wp} b=%d{wb} td=%d{wt}")

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        system <- rename "r/a" "r/b" system
        row "over-watched-file"
        system <- rename "r/sd" "r/td" system
        row "over-watched-empty-dir"
        system <- system |> writeFile "r/c" "c" |> writeFile "r/e" "e"
        let we, s = watch fd "r/e" modelledEvents system

        let opened, s =
            openFd (openFlags FileAccessMode.ReadOnly false false false) "r/e" 0 s

        let _, s = drain fd s
        system <- rename "r/c" "r/e" s
        let drained, s = drain fd system
        system <- s
        produced.Add ("over-watched-open-file", $"wd=%d{we}: %s{drained}")
        system <- close opened system
        row "close-displaced"
        // The displaced inode's own going: `IN_DELETE_SELF` and its `IN_IGNORED`.
        let skipped = [ "over-watched-file" ; "over-watched-empty-dir" ; "close-displaced" ]
        actual skipped produced |> shouldEqual (expected deliveryProbe "R" skipped)

    [<Test>]
    let ``delivery section P: the name a path's change is reported under`` () : unit =
        let mutable system =
            boot
                root
                [
                    "p",
                    directory
                        0o755
                        [
                            "d", directory 0o755 [ "e", directory 0o755 [] ; "f", file "x" ]
                            "lf", symlink "d/f"
                        ]
                ]

        let fd, s = init1 nonblock system
        let wp, s = watch fd "p" modelledEvents s
        let wd, s = watch fd "p/d" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"p=%d{wp} d=%d{wd}")

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        system <- chmod "p/lf" 0o600 system
        row "chmod-through-symlink"
        system <- chmod "p/d/." 0o700 system
        row "chmod-dot"
        system <- chmod "p/d/e/.." 0o755 system
        row "chmod-dotdot"
        system <- chmod "p/d/e/" 0o755 system
        row "chmod-trailing-slash"
        system <- close fd system

        let rootFd, s = init1 nonblock system
        let wr, s = watch rootFd "/" modelledEvents s
        let wdev, s = watch rootFd "/dev" modelledEvents s
        system <- chmod "/dev" (modeOf "/dev" s) s
        let drained, s = drain rootFd system
        system <- close rootFd s
        produced.Add ("chmod-mount-point", $"root=%d{wr} dev=%d{wdev}: %s{drained}")

        system <- mkdir "p/gone" 0o755 system
        let gone, s = init1 nonblock system
        let wpg, s = watch gone "p" modelledEvents s
        let wg, s = watch gone "p/gone" modelledEvents s
        system <- s |> chdir "p/gone" |> rmdir "../gone"
        let drained, s = drain gone system
        system <- s
        produced.Add ("rmdir-of-current", $"p=%d{wpg} gone=%d{wg}: %s{drained}")
        system <- chmod "." 0o700 system
        let drained, s = drain gone system
        system <- s
        produced.Add ("chmod-removed-current", $"chmod=0: %s{drained}")
        system <- system |> chdir "/" |> close gone
        // Linux reports the change to the removed directory's old parent, under
        // its old name, which the model does not keep.
        let skipped = [ "chmod-removed-current" ]
        actual skipped produced |> shouldEqual (expected deliveryProbe "P" skipped)

    [<Test>]
    let ``delivery section T: open's O_CREAT and O_TRUNC`` () : unit =
        let mutable system =
            boot root [ "t", directory 0o755 [ "existing", file "seed" ; "empty", file "" ] ]

        let fd, s = init1 nonblock system
        let _, s = watch fd "t" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()

        let opened (case : string) (flags : OpenFlags) (path : string) =
            let f, s = openFd flags path 0o644 system
            system <- close f s
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        opened "creat-trunc-new" (openFlags FileAccessMode.WriteOnly true false true) "t/new"
        opened "creat-existing" (openFlags FileAccessMode.WriteOnly true false false) "t/existing"
        opened "creat-trunc-existing" (openFlags FileAccessMode.WriteOnly true false true) "t/existing"
        opened "trunc-empty" (openFlags FileAccessMode.WriteOnly false false true) "t/empty"

        let f, s =
            openFd (openFlags FileAccessMode.ReadOnly false false true) "t/existing" 0 system

        system <- close f s
        let drained, _ = drain fd system
        produced.Add ("rdonly-trunc", $"open=1: %s{drained}")
        actual [] produced |> shouldEqual (expected deliveryProbe "T" [])

    [<Test>]
    let ``delivery section C: chown and chmod that change nothing`` () : unit =
        let mutable system =
            boot root [ "c", directory 0o755 [ "f", fileOwnedBy 1000u "x" ] ]

        let fd, s = init1 nonblock system
        let _, s = watch fd "c" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        system <- chown "c/f" None None system
        row "chown-neither"
        system <- chown "c/f" (Some 1000u) (Some 1000u) system
        row "chown-same"
        system <- chown "c/f" (Some 1000u) None system
        row "chown-same-user-only"
        system <- chmod "c/f" 0o644 system
        row "chmod-same"
        actual [] produced |> shouldEqual (expected deliveryProbe "C" [])
