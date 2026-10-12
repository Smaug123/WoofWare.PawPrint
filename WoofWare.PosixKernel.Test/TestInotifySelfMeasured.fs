namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test.InotifyReplay

/// When a watched inode's own going and moving are reported, replayed
/// against what Linux 6.18.5 aarch64 answered in docs/plans/2026-10-11-inotify:
/// `inotify-probe.c` section F, and `inotify-self-probe.c` sections U, N, R,
/// D, W and Q. Each test makes the probe's calls on the model, renders each
/// row as the probe printed it, and compares the rows with the recorded ones.
///
/// Linux queues `IN_DELETE_SELF` and `IN_IGNORED`, and drops the watch, when
/// a name of the inode dies while the inode has no link left: at an unlink of
/// a name nothing holds, or at the release of the last descriptor opened
/// through a name since unlinked. A watch the probe gave `IN_ALL_EVENTS` asks
/// here for the events the model raises, and the rows lose the records of the
/// events it does not (`withoutUnmodelled`).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifySelfMeasured =

    let private probe = lazy recorded "WoofWare.PosixKernel.Test.inotify.linux.txt"

    let private selfProbe =
        lazy recorded "WoofWare.PosixKernel.Test.inotifySelf.linux.txt"

    /// The rows `section` of `file` recorded, with the records of unmodelled
    /// events dropped and cookies numbered by first appearance.
    let private expected (file : Lazy<(string * string * string) list>) (section : string) : (string * string) list =
        rows file section []
        |> List.map (fun (case, text) -> case, withoutUnmodelled text)
        |> normaliseCookies

    let private actual (produced : (string * string) seq) : (string * string) list =
        produced |> List.ofSeq |> normaliseCookies

    let private writeOnly : OpenFlags =
        openFlags FileAccessMode.WriteOnly false false false

    let private readOnly : OpenFlags =
        openFlags FileAccessMode.ReadOnly false false false

    /// `IN_DELETE_SELF` and `IN_MOVE_SELF`.
    let private selfEvents : uint32 = 0xC00u

    let private deleteSelf : uint32 = 0x400u
    let private moveSelf : uint32 = 0x800u

    /// The inotify-self probe's `rc`: a count, or `-1 errnoN` with Linux's
    /// number.
    let private rcErrno (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed value -> $"%d{value}"
        | SyscallAnswer.Failed error ->
            let numbering =
                SimulatedUnixPlatform.rawErrnoNumbering SimulatedUnixPlatform.linuxArm64

            $"-1 errno%d{UnixError.toRawErrnoUnder numbering error}"

    /// Records `case` with what `fd` holds, as the probe's `drain`.
    let private recorder (produced : ResizeArray<string * string>) (fd : int) =
        fun (case : string) (system : UnixSystem<int, string>) ->
            let drained, system = drain fd system
            produced.Add (case, drained)
            system

    // ------------------------------------------------------- inotify-probe.c

    [<Test>]
    let ``section F: a watch on a file hears it move, lose a name, and go at its last close`` () : unit =
        let system = boot root [ "fw", directory 0o755 [] ] |> writeFile "fw/f" "seed"
        let system = link "fw/f" "fw/f2" system
        let fd, system = init1 nonblock system
        let wd, system = watch fd "fw/f" modelledEvents system
        let wd2, system = watch fd "fw/f2" modelledEvents system
        let produced = ResizeArray<string * string> ()
        let row = recorder produced fd
        produced.Add ("two-names-one-inode", $"wd via f=%d{wd} via f2=%d{wd2}")
        let f, system = openFd writeOnly "fw/f2" 0 system
        let system = system |> write f "x" |> close f |> row "open-write-close-via-f2"
        let system = system |> chmod "fw/f" 0o600 |> row "chmod"
        let system = system |> rename "fw/f" "fw/g" |> row "rename-of-watched-file"
        let system = system |> unlink "fw/f2" |> row "unlink-other-name"
        let f, system = openFd readOnly "fw/g" 0 system
        let system = system |> unlink "fw/g" |> row "unlink-last-name-while-open"
        let system = system |> close f |> row "close-last-reference"
        let answer, _ = rmWatch fd wd system
        produced.Add ("rm-after-delete-self", $"rm wd=%s{rc answer}")
        actual produced |> shouldEqual (expected probe "F")

    // -------------------------------------------------- inotify-self-probe.c

    [<Test>]
    let ``self section U: an unlink of a watched file's last name reports its going before the directory's DELETE``
        ()
        : unit
        =
        let system = boot root [ "u", directory 0o755 [ "f", file "x" ; "g", file "x" ] ]

        let fd, system = init1 nonblock system
        let wu, system = watch fd "u" modelledEvents system
        let wf, system = watch fd "u/f" modelledEvents system
        let wg, system = watch fd "u/g" modify system
        let produced = ResizeArray<string * string> ()
        let row = recorder produced fd
        produced.Add ("wds", $"u=%d{wu} f=%d{wf} g=%d{wg}")
        let system = system |> unlink "u/f" |> row "unlink-last-name"
        let answer, system = rmWatch fd wf system
        produced.Add ("rm-after", rcErrno answer)
        let system = row "rm-after-events" system
        let system = system |> unlink "u/g" |> row "unlink-last-name-no-self-event"
        let system = system |> writeFile "u/f" "y" |> row "recreate"
        let again, system = watch fd "u/f" modelledEvents system
        let drained, _ = drain fd system
        produced.Add ("rewatch", $"wd=%d{again}: %s{drained}")
        actual produced |> shouldEqual (expected selfProbe "U")

    /// The probe's `namesSetup`: `n/a`, also named `n/b`, and an instance
    /// watching `n` and then the file.
    let private namesSetup () : int * int * UnixSystem<int, string> =
        let system =
            boot root [ "n", directory 0o755 [ "a", file "x" ] ] |> link "n/a" "n/b"

        let instance, system = init1 nonblock system
        let _, system = watch instance "n" modelledEvents system
        let wf, system = watch instance "n/a" modelledEvents system
        instance, wf, system

    [<Test>]
    let ``self section N: a file goes for inotify when a name of it dies with no link left`` () : unit =
        let produced = ResizeArray<string * string> ()

        // Opened through a, a unlinked, then b.
        let instance, wf, system = namesSetup ()
        let row = recorder produced instance
        let fa, system = openFd writeOnly "n/a" 0 system
        produced.Add ("one-wds", $"dir=1 file=%d{wf}")
        let system = system |> unlink "n/a" |> row "one-unlink-held-name"
        let system = system |> unlink "n/b" |> row "one-unlink-last-name"
        let system = system |> write fa "1" |> row "one-write"
        let system = system |> close fa |> row "one-close"
        let answer, _ = rmWatch instance wf system
        produced.Add ("one-rm-after", rcErrno answer)

        // Opened through a and through b, both unlinked, then closed in turn.
        let instance, _, system = namesSetup ()
        let row = recorder produced instance
        let fa, system = openFd writeOnly "n/a" 0 system
        let fb, system = openFd writeOnly "n/b" 0 system
        let system = system |> unlink "n/a" |> row "two-unlink-a"
        let system = system |> unlink "n/b" |> row "two-unlink-b"
        let system = system |> close fa |> row "two-close-a"
        system |> close fb |> row "two-close-b" |> ignore<UnixSystem<int, string>>

        // Opened through a twice: one name, two descriptions.
        let instance, _, system = namesSetup ()
        let row = recorder produced instance
        let fa, system = openFd writeOnly "n/a" 0 system
        let fa2, system = openFd writeOnly "n/a" 0 system
        let system = system |> unlink "n/b" |> row "same-unlink-b"
        let system = system |> unlink "n/a" |> row "same-unlink-a"
        let system = system |> close fa |> row "same-close-first"

        system
        |> close fa2
        |> row "same-close-second"
        |> ignore<UnixSystem<int, string>>

        // Opened through b, b unlinked and linked again, and opened again.
        let instance, _, system = namesSetup ()
        let row = recorder produced instance
        let old, system = openFd writeOnly "n/b" 0 system
        let system = system |> unlink "n/b" |> link "n/a" "n/b"
        let renewed, system = openFd writeOnly "n/b" 0 system
        let _, system = system |> unlink "n/a" |> drain instance
        let system = system |> unlink "n/b" |> row "relinked-unlink-last-name"
        let system = system |> close old |> row "relinked-close-old"

        system
        |> close renewed
        |> row "relinked-close-renewed"
        |> ignore<UnixSystem<int, string>>

        // Opened through a, a unlinked, and the last name b renamed over.
        let instance, _, system = namesSetup ()
        let row = recorder produced instance
        let system = writeFile "n/x" "x" system
        let fa, system = openFd writeOnly "n/a" 0 system
        let _, system = system |> unlink "n/a" |> drain instance
        let system = system |> rename "n/x" "n/b" |> row "rename-over-last-name"

        system |> close fa |> row "rename-over-close" |> ignore<UnixSystem<int, string>>

        actual produced |> shouldEqual (expected selfProbe "N")

    [<Test>]
    let ``self section R: a rename reports the moved inode's IN_MOVE_SELF, then the displaced inode's going``
        ()
        : unit
        =
        let produced = ResizeArray<string * string> ()

        // One machine for the three, as the probe's, so that the renames'
        // cookies differ.
        let system =
            boot
                root
                [
                    "r", directory 0o755 [ "s", file "s" ; "t", file "t" ]
                    "rd", directory 0o755 [ "sd", directory 0o755 [] ; "td", directory 0o755 [] ]
                    "rs", directory 0o755 [ "s", file "s" ]
                ]

        let fd, system = init1 nonblock system
        let row = recorder produced fd
        let wr, system = watch fd "r" modelledEvents system
        let ws, system = watch fd "r/s" modelledEvents system
        let wt, system = watch fd "r/t" modelledEvents system
        produced.Add ("wds", $"r=%d{wr} s=%d{ws} t=%d{wt}")
        let system = system |> rename "r/s" "r/t" |> row "watched-over-watched"
        let _, system = system |> link "r/t" "r/u" |> drain fd
        let system = system |> rename "r/u" "r/v" |> row "other-name-of-watched"
        let system = system |> rename "r/v" "r/t" |> row "name-onto-same-file"
        let system = system |> rename "r/t" "r/t" |> row "name-onto-itself"
        let _, system = system |> unlink "r/v" |> drain fd
        let system = close fd system

        let fd, system = init1 nonblock system
        let row = recorder produced fd
        let wr, system = watch fd "rd" modelledEvents system
        let wsd, system = watch fd "rd/sd" modelledEvents system
        let wtd, system = watch fd "rd/td" modelledEvents system
        produced.Add ("dir-wds", $"r=%d{wr} sd=%d{wsd} td=%d{wtd}")
        let system = system |> rename "rd/sd" "rd/td" |> row "watched-dir-over-watched-dir"
        let system = close fd system

        let fd, system = init1 nonblock system
        let ws, system = watch fd "rs/s" (modelledEvents &&& ~~~moveSelf) system
        let drained, _ = system |> rename "rs/s" "rs/s2" |> drain fd
        produced.Add ("no-move-self-asked", $"wd=%d{ws}: %s{drained}")

        actual produced |> shouldEqual (expected selfProbe "R")

    [<Test>]
    let ``self section D: a watched directory goes when the last thing holding it lets go`` () : unit =
        let produced = ResizeArray<string * string> ()

        let fresh () =
            boot root [ "d", directory 0o755 [ "sub", directory 0o755 [] ] ]

        let fd, system = init1 nonblock (fresh ())
        let row = recorder produced fd
        let wd, system = watch fd "d" dotnetMask system
        let ws, system = watch fd "d/sub" dotnetMask system
        produced.Add ("dotnet-wds", $"d=%d{wd} sub=%d{ws}")
        system |> rmdir "d/sub" |> row "dotnet-rmdir" |> ignore<UnixSystem<int, string>>

        let dfd, system =
            openFd
                { readOnly with
                    Directory = true
                }
                "d/sub"
                0
                (fresh ())

        let fd, system = init1 nonblock system
        let row = recorder produced fd
        let wd, system = watch fd "d" modelledEvents system
        let ws, system = watch fd "d/sub" modelledEvents system
        produced.Add ("held-wds", $"d=%d{wd} sub=%d{ws}")
        let system = system |> rmdir "d/sub" |> row "held-rmdir"
        system |> close dfd |> row "held-close" |> ignore<UnixSystem<int, string>>

        let fd, system = init1 nonblock (fresh ())
        let row = recorder produced fd
        let wd, system = watch fd "d" modelledEvents system
        let ws, system = watch fd "d/sub" modelledEvents system
        produced.Add ("cwd-wds", $"d=%d{wd} sub=%d{ws}")
        let system = system |> chdir "d/sub" |> rmdir "/d/sub" |> row "cwd-rmdir"
        system |> chdir "/" |> row "cwd-leave" |> ignore<UnixSystem<int, string>>

        let system =
            boot root [ "d", directory 0o755 [ "a", directory 0o755 [ "b", directory 0o755 [] ] ] ]

        let fd, system = init1 nonblock system
        let row = recorder produced fd
        let wd, system = watch fd "d" modelledEvents system
        let wa, system = watch fd "d/a" modelledEvents system
        let wb, system = watch fd "d/a/b" modelledEvents system
        produced.Add ("chain-wds", $"d=%d{wd} a=%d{wa} b=%d{wb}")
        let system = system |> chdir "d/a/b" |> rmdir "/d/a/b" |> row "chain-rmdir-inner"
        let system = system |> rmdir "/d/a" |> row "chain-rmdir-outer"
        system |> chdir "/" |> row "chain-leave" |> ignore<UnixSystem<int, string>>

        actual produced |> shouldEqual (expected selfProbe "D")

    [<Test>]
    let ``self section W: two instances each hear of a file's going`` () : unit =
        let system = boot root [ "w", directory 0o755 [ "f", file "x" ] ]
        let one, system = init1 nonblock system
        let _, system = watch one "w/f" selfEvents system
        let two, system = init1 nonblock system
        let _, system = watch two "w" modelledEvents system
        let _, system = watch two "w/f" modelledEvents system
        let system = unlink "w/f" system
        let first, system = drain one system
        let second, _ = drain two system

        actual [ "first", first ; "second", second ]
        |> shouldEqual (expected selfProbe "W")

    [<Test>]
    let ``self section Q: a going at a queue one short of full overflows, and the watch goes all the same`` () : unit =
        let produced = ResizeArray<string * string> ()

        for shortBy in 1..2 do
            let system =
                boot root [ "q", directory 0o755 [ "x", file "x" ; "y", file "y" ; "f", file "f" ] ]

            let fd, system = init1 nonblock system
            let _, system = watch fd "q/x" attrib system
            let _, system = watch fd "q/y" attrib system
            let wf, system = watch fd "q/f" deleteSelf system

            let system =
                (system, [ 0 .. 16384 - shortBy - 1 ])
                ||> List.fold (fun system i -> chmod (if i % 2 = 1 then "q/y" else "q/x") 0o644 system)

            let records, system = system |> unlink "q/f" |> drainRecords fd

            let tail =
                records
                |> List.skip (max 0 (records.Length - 3))
                |> List.map (fun record -> " | " + render record)
                |> String.concat ""

            produced.Add ($"short-by-%d{shortBy}", $"records=%d{records.Length}%s{tail}")
            let answer, _ = rmWatch fd wf system
            produced.Add ($"short-by-%d{shortBy}-rm-after", rcErrno answer)

        actual produced |> shouldEqual (expected selfProbe "Q")

    [<Test>]
    let ``self section X: files another process holds go at its exit in the order its descriptors let go`` () : unit =
        let produced = ResizeArray<string * string> ()

        for name, order in [ "open-a-b-a", "aba" ; "open-a-b", "ab" ; "open-b-a", "ba" ] do
            let pids, machine = Machines.withTasks SimulatedUnixPlatform.linuxArm64 2 1
            let child, parent = pids.[0], pids.[1]

            let machine =
                machine
                |> Machines.doIn
                    child
                    (fun view -> view |> mkdir "/x" 0o755 |> writeFile "/x/a" "a" |> writeFile "/x/b" "b")

            let (instance, wa, wb), machine =
                Machines.inProcess
                    parent
                    (fun view ->
                        let instance, view = init1 nonblock view
                        let wa, view = watch instance "/x/a" selfEvents view
                        let wb, view = watch instance "/x/b" selfEvents view
                        (instance, wa, wb), view
                    )
                    machine

            let machine =
                Machines.doIn
                    child
                    (fun view ->
                        let view =
                            (view, order)
                            ||> Seq.fold (fun view letter -> openFd readOnly $"/x/%c{letter}" 0 view |> snd)

                        view |> unlink "/x/a" |> unlink "/x/b"
                    )
                    machine

            let ended = UnixTaskLifecycle.exitGroup 0 0 (Machines.viewOf child machine)

            let machine =
                match SimulatedMachine.tryEndProcess ended machine with
                | Ok (ProcessTermination.Exited _, machine) -> machine
                | other -> failwith $"the child's exit: %A{other}"

            Machines.assertClean machine
            let drained, _ = Machines.inProcess parent (drain instance) machine
            produced.Add (name, $"a=%d{wa} b=%d{wb} status=0: %s{drained}")

        actual produced |> shouldEqual (expected selfProbe "X")
