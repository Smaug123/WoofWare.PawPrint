namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open System.Text
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test.InotifyReplay

/// Which watch hears of a change made through a descriptor, and under which
/// name, replayed against what Linux 6.18.5 aarch64 answered in
/// docs/plans/2026-10-11-inotify: `inotify-probe.c` sections H and D, and
/// `inotify-dentry-probe.c` sections E, L, O, Q, U, V and K. Each test makes
/// the probe's calls on the model, renders each row as the probe printed it,
/// and compares the rows with the recorded ones.
///
/// Linux reports such a change at the watch on the directory of the name the
/// descriptor was opened through, under that name as `rename` has since moved
/// it, and, once that name is unlinked, under the name it had, except to a
/// watch with `IN_EXCL_UNLINK`, which hears nothing of a write's `IN_MODIFY`
/// through it.
///
/// A watch the probe gave `IN_ALL_EVENTS` asks here for the events the model
/// raises, and the rows lose the records of the events it does not
/// (`withoutUnmodelled`).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyDentryMeasured =

    let private probe = lazy recorded "WoofWare.PosixKernel.Test.inotify.linux.txt"

    let private dentryProbe =
        lazy recorded "WoofWare.PosixKernel.Test.inotifyDentry.linux.txt"

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

    let private exclUnlink : uint32 = 0x04000000u

    let private readWrite : OpenFlags =
        openFlags FileAccessMode.ReadWrite false false false

    let private writeOnly : OpenFlags =
        openFlags FileAccessMode.WriteOnly false false false

    let private readOnly : OpenFlags =
        openFlags FileAccessMode.ReadOnly false false false

    /// `write`, answering the count it wrote, as the probe prints it.
    let private writeCounted
        (fd : int)
        (text : string)
        (system : UnixSystem<int, string>)
        : string * UnixSystem<int, string>
        =
        let bytes = ImmutableArray.CreateRange (Encoding.ASCII.GetBytes text)

        match
            WriteOutcomes.admitThenWrite 0 fd UserBuffer.Mapped bytes system
            |> WriteOutcomes.returned
        with
        | Ok (WriteAnswer.Completed count, system) -> $"%d{count}", system
        | other -> failwith $"write(%d{fd}): %A{other}"

    /// `pwrite`, answering the count it wrote.
    let private pwriteCounted
        (fd : int)
        (text : string)
        (offset : int64)
        (system : UnixSystem<int, string>)
        : string * UnixSystem<int, string>
        =
        let bytes = ImmutableArray.CreateRange (Encoding.ASCII.GetBytes text)

        match UnixReadWrite.tryPwrite 0 fd bytes offset system with
        | Ok (WriteAnswer.Completed count, system) -> $"%d{count}", system
        | other -> failwith $"pwrite(%d{fd}): %A{other}"

    /// The dentry probe's `rc` of a call that failed: `-1 errnoN` with
    /// Linux's number.
    let private rcErrno (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed value -> $"%d{value}"
        | SyscallAnswer.Failed error ->
            let numbering =
                SimulatedUnixPlatform.rawErrnoNumbering SimulatedUnixPlatform.linuxArm64

            $"-1 errno%d{UnixError.toRawErrnoUnder numbering error}"

    /// The answer of a call that must not be refused, as the probe's `rc`.
    let private answered
        (call : string)
        (result : Result<SyscallAnswer * UnixSystem<int, string>, 'r>)
        : string * UnixSystem<int, string>
        =
        match result with
        | Ok (answer, system) -> rc answer, system
        | Error refusal -> failwith $"%s{call} refused: %A{refusal}"

    // ------------------------------------------------------- inotify-probe.c

    [<Test>]
    let ``section H: a change through a descriptor is heard under the name it was opened through`` () : unit =
        let mutable system =
            boot root [ "h", directory 0o755 [ "other", directory 0o755 [] ] ]
            |> writeFile "h/a" "seed"
            |> link "h/a" "h/b"
            |> link "h/a" "h/other/c"

        let fd, s = init1 nonblock system
        let wh, s = watch fd "h" modelledEvents s
        let wo, s = watch fd "h/other" modelledEvents s
        let _, s = drain fd s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"h=%d{wh} other=%d{wo}")

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        let fa, s = openFd writeOnly "h/a" 0 system
        let fb, s = openFd writeOnly "h/b" 0 s
        let fc, s = openFd writeOnly "h/other/c" 0 s
        system <- s
        row "open-three-names"
        system <- write fa "1" system
        row "write-via-a"
        system <- write fb "2" system
        row "write-via-b"
        system <- write fc "3" system
        row "write-via-other/c"
        system <- fchmod fa 0o600 system
        row "fchmod-via-a"
        let _, s = system |> rename "h/a" "h/renamed" |> drain fd
        system <- write fa "4" s
        row "write-via-a-after-rename-to-renamed"
        let _, s = system |> rename "h/renamed" "h/other/moved" |> drain fd
        system <- write fa "5" s
        row "write-via-a-after-move-to-other/moved"
        let _, s = system |> unlink "h/b" |> drain fd
        system <- write fb "6" s
        row "write-via-b-after-unlink-of-b"
        system <- system |> close fa |> close fb |> close fc
        row "closes"
        actual [] produced |> shouldEqual (expected probe "H" [])

    [<Test>]
    let ``section D: a write through a name since unlinked, with and without IN_EXCL_UNLINK`` () : unit =
        let mutable system =
            boot
                root
                [
                    "dd", directory 0o755 [ "sub", directory 0o755 [ "inner", file "x" ] ]
                    "u", directory 0o755 []
                ]

        let fd, s = init1 nonblock system
        let p, s = watch fd "dd" modelledEvents s
        let sub, s = watch fd "dd/sub" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"parent=%d{p} sub=%d{sub}")

        let row (case : string) =
            let drained, s = drain fd system
            system <- s
            produced.Add (case, drained)

        system <- rename "dd/sub" "dd/moved" system
        row "rename-watched-dir"
        system <- writeFile "dd/moved/after" "x" system
        row "create-in-renamed-watched-dir"
        system <- system |> unlink "dd/moved/inner" |> unlink "dd/moved/after"
        row "unlink-inside"
        system <- rmdir "dd/moved" system
        row "rmdir-watched-dir"
        let answer, s = rmWatch fd sub system
        system <- s
        produced.Add ("rm-after-ignored", $"rm sub's wd=%s{rc answer}")
        system <- mkdir "dd/sub2" 0o755 system
        let s2, s = watch fd "dd/sub2" modelledEvents system
        system <- s
        produced.Add ("new-watch-wd", $"sub2=%d{s2}")
        row "mkdir-sub2-events"
        system <- rename "dd/sub2" "u/sub2-gone" system
        row "watched-dir-moved-out"
        system <- writeFile "u/sub2-gone/x" "x" system
        row "create-in-moved-out-dir"
        let _, s = system |> writeFile "dd/open" "x" |> drain fd
        let f, s = openFd writeOnly "dd/open" 0 s
        system <- unlink "dd/open" s
        row "unlink-open-file"
        system <- write f "y" system
        row "write-unlinked-no-excl"
        let p2, s = watch fd "dd" (modelledEvents ||| exclUnlink) system
        system <- write f "z" s
        let drained, s = drain fd system
        system <- s
        produced.Add ("write-unlinked-excl-unlink", $"wd=%d{p2} (parent was %d{p}): %s{drained}")
        system <- close f system
        row "close-unlinked-excl-unlink"
        actual [] produced |> shouldEqual (expected probe "D" [])

    // ------------------------------------------------- inotify-dentry-probe.c

    /// The probe's `throughUnlinked`: each change a descriptor can make,
    /// through a name since unlinked, under a watch on the old directory
    /// (`parent`), on the file (`self`), each with `IN_EXCL_UNLINK` when
    /// `excl`.
    let private throughUnlinked (config : string) (parent : bool) (self : bool) (excl : bool) : (string * string) list =
        let mutable system =
            boot root [ "e", directory 0o755 [ "f", file "hello" ; "src", file "abc" ] ]

        let f, s = openFd readWrite "e/f" 0 system
        let src, s = openFd readOnly "e/src" 0 s
        let instance, s = init1 nonblock s
        let mask = modelledEvents ||| (if excl then exclUnlink else 0u)
        let s = if parent then snd (watch instance "e" mask s) else s
        let s = if self then snd (watch instance "e/f" mask s) else s
        system <- unlink "e/f" s
        let produced = ResizeArray<string * string> ()

        let row (case : string) =
            let drained, s = drain instance system
            system <- s
            produced.Add ($"%s{config}-%s{case}", drained)

        let op (case : string) (call : UnixSystem<int, string> -> string * UnixSystem<int, string>) =
            let result, s = call system
            let drained, s = drain instance s
            system <- s
            produced.Add ($"%s{config}-%s{case}", $"%s{result}: %s{drained}")

        row "unlink"
        op "write" (writeCounted f "y")
        op "pwrite" (pwriteCounted f "z" 3L)

        op
            "copy_file_range"
            (fun system -> UnixReadWrite.tryCopyFileRange src f 3UL 0 system |> answered "copy_file_range")

        op "ftruncate" (fun system -> UnixDescriptor.tryFtruncate f 2L system |> answered "ftruncate")
        op "fchmod" (fun system -> UnixPathResolution.tryFchmod f 0o600 system |> answered "fchmod")

        op
            "fchown"
            (fun system ->
                UnixPathResolution.tryFchown f (Some UserId.root) (Some (group 0u)) system
                |> answered "fchown"
            )

        op
            "futimens"
            (fun system ->
                UnixPathResolution.tryUtimensat
                    f
                    NullablePathArgument.Null
                    (TimesArgument.Fields (timespec 1000L 0L, timespec 2000L 0L))
                    0
                    system
                |> answered "futimens"
            )

        op
            "utimensat-empty-path"
            (fun system ->
                UnixPathResolution.tryUtimensat
                    f
                    (NullablePathArgument.NotNull (PathArg.ofText ""))
                    TimesArgument.Null
                    atEmptyPath
                    system
                |> answered "utimensat"
            )

        op
            "fchmodat-empty-path"
            (fun system ->
                UnixPathResolution.tryFchmodat f (PathArg.ofText "") 0o644 atEmptyPath system
                |> answered "fchmodat"
            )

        op
            "fchownat-empty-path"
            (fun system ->
                UnixPathResolution.tryFchownat
                    f
                    (PathArg.ofText "")
                    (Some UserId.root)
                    (Some (group 0u))
                    atEmptyPath
                    system
                |> answered "fchownat"
            )

        system <- system |> close f |> close src
        row "close"
        List.ofSeq produced

    [<Test>]
    let ``dentry section E: IN_EXCL_UNLINK keeps out a write through an unlinked name, and no setattr`` () : unit =
        let produced =
            [
                yield! throughUnlinked "parent" true false false
                yield! throughUnlinked "parent-excl" true false true
                yield! throughUnlinked "self" false true false
                yield! throughUnlinked "self-excl" false true true
                yield! throughUnlinked "both-excl" true true true
            ]

        actual [] produced |> shouldEqual (expected dentryProbe "E" [])

    [<Test>]
    let ``dentry section L: a name unlinked and linked again is a new name, and the old stays unlinked`` () : unit =
        let produced = ResizeArray<string * string> ()

        for excl in [ false ; true ] do
            let config = if excl then "excl" else "no-excl"

            let system =
                boot root [ "l", directory 0o755 [ "a", file "x" ] ] |> link "l/a" "l/b"

            let fb, system = openFd writeOnly "l/b" 0 system
            let instance, system = init1 nonblock system

            let _, system =
                watch instance "l" (modelledEvents ||| (if excl then exclUnlink else 0u)) system

            let _, system = system |> unlink "l/b" |> link "l/a" "l/b" |> drain instance
            let drained, system = system |> write fb "1" |> drain instance
            produced.Add ($"%s{config}-write-through-relinked-name", drained)
            let again, system = openFd writeOnly "l/b" 0 system
            let _, system = drain instance system
            let drained, system = system |> write again "2" |> drain instance
            produced.Add ($"%s{config}-write-through-fresh-open", drained)

            system
            |> close again
            |> close fb
            |> close instance
            |> ignore<UnixSystem<int, string>>

        actual [] produced |> shouldEqual (expected dentryProbe "L" [])

    [<Test>]
    let ``dentry section O: a name a rename displaced the file from is unlinked`` () : unit =
        let produced = ResizeArray<string * string> ()

        // One machine for both, as the probe's, so that the two renames'
        // cookies differ.
        let mutable system =
            boot
                root
                [
                    for config in [ "no-excl" ; "excl" ] do
                        $"o-%s{config}", directory 0o755 [ "s", file "s" ; "t", file "t" ]
                ]

        for excl in [ false ; true ] do
            let config = if excl then "excl" else "no-excl"
            let d = $"o-%s{config}"
            let ft, s = openFd writeOnly $"%s{d}/t" 0 system
            let fs, s = openFd writeOnly $"%s{d}/s" 0 s
            let instance, s = init1 nonblock s

            let _, s = watch instance d (modelledEvents ||| (if excl then exclUnlink else 0u)) s

            let drained, s = s |> rename $"%s{d}/s" $"%s{d}/t" |> drain instance
            produced.Add ($"%s{config}-rename-over", drained)
            let drained, s = s |> write ft "1" |> drain instance
            produced.Add ($"%s{config}-write-through-displaced", drained)
            let drained, s = s |> fchmod ft 0o600 |> drain instance
            produced.Add ($"%s{config}-fchmod-through-displaced", drained)
            let drained, s = s |> write fs "2" |> drain instance
            produced.Add ($"%s{config}-write-through-moved", drained)
            system <- s |> close ft |> close fs |> close instance

        actual [] produced |> shouldEqual (expected dentryProbe "O" [])

    [<Test>]
    let ``dentry section Q: a directory's descriptor is heard under its name as renamed, moved and removed`` () : unit =
        let produced = ResizeArray<string * string> ()

        for excl in [ false ; true ] do
            let config = if excl then "excl" else "no-excl"

            let mutable system =
                boot
                    root
                    [
                        "q", directory 0o755 [ "d", directory 0o755 [] ; "other", directory 0o755 [] ]
                    ]

            let dfd, s = openFd readOnly "q/d" 0 system
            let instance, s = init1 nonblock s
            let mask = modelledEvents ||| (if excl then exclUnlink else 0u)
            let wq, s = watch instance "q" mask s
            let wo, s = watch instance "q/other" mask s
            system <- s
            produced.Add ($"%s{config}-wds", $"q=%d{wq} other=%d{wo}")

            let row (case : string) =
                let drained, s = drain instance system
                system <- s
                produced.Add ($"%s{config}-%s{case}", drained)

            system <- fchmod dfd 0o700 system
            row "fchmod"
            let _, s = system |> rename "q/d" "q/d2" |> drain instance
            system <- fchmod dfd 0o755 s
            row "fchmod-after-rename"
            let _, s = system |> rename "q/d2" "q/other/d3" |> drain instance
            system <- fchmod dfd 0o700 s
            row "fchmod-after-move"
            system <- rmdir "q/other/d3" system
            row "rmdir"
            let result, s = UnixPathResolution.tryFchmod dfd 0o755 system |> answered "fchmod"
            let drained, s = drain instance s
            system <- s
            produced.Add ($"%s{config}-fchmod-after-rmdir", $"%s{result}: %s{drained}")

            let result, s =
                UnixPathResolution.tryUtimensat
                    dfd
                    NullablePathArgument.Null
                    (TimesArgument.Fields (timespec 1000L 0L, timespec 2000L 0L))
                    0
                    system
                |> answered "futimens"

            let drained, s = drain instance s
            system <- s
            produced.Add ($"%s{config}-futimens-after-rmdir", $"%s{result}: %s{drained}")
            system <- close dfd system
            row "close"

        actual [] produced |> shouldEqual (expected dentryProbe "Q" [])

    [<Test>]
    let ``dentry section U: a file unlinked while open holds the directory it was named in`` () : unit =
        let mutable system =
            boot root [ "u", directory 0o755 [ "sub", directory 0o755 [ "f", file "x" ] ] ]

        let f, s = openFd writeOnly "u/sub/f" 0 system
        let instance, s = init1 nonblock s
        let wu, s = watch instance "u" modelledEvents s
        let ws, s = watch instance "u/sub" modelledEvents s
        system <- s
        let produced = ResizeArray<string * string> ()
        produced.Add ("wds", $"u=%d{wu} sub=%d{ws}")

        let row (case : string) =
            let drained, s = drain instance system
            system <- s
            produced.Add (case, drained)

        system <- unlink "u/sub/f" system
        row "unlink"
        system <- rmdir "u/sub" system
        row "rmdir-parent"
        system <- write f "y" system
        row "write-in-removed-parent"
        system <- close f system
        row "close"
        let answer, s = rmWatch instance ws system
        system <- s
        produced.Add ("rm-sub-watch-after", rcErrno answer)
        row "rm-sub-watch-events"
        actual [] produced |> shouldEqual (expected dentryProbe "U" [])

    [<Test>]
    let ``a file unlinked while open keeps the directory it was named in until it is closed`` () : unit =
        let system =
            boot root [ "u", directory 0o755 [ "sub", directory 0o755 [ "f", file "x" ] ] ]

        let sub = numberOf "u/sub" system
        let f, system = openFd writeOnly "u/sub/f" 0 system
        let system = system |> unlink "u/sub/f" |> rmdir "u/sub"

        VirtualFileSystem.tryGet sub (UnixSystem.fileSystem system)
        |> Option.isSome
        |> shouldEqual true

        UnixSystem.checkInvariants system |> shouldEqual []
        let system = close f system

        VirtualFileSystem.tryGet sub (UnixSystem.fileSystem system)
        |> Option.isSome
        |> shouldEqual false

        UnixSystem.checkInvariants system |> shouldEqual []

    [<Test>]
    let ``dentry section V: a write to /dev/null is heard at no watch on /dev`` () : unit =
        let system = boot root []
        let instance, system = init1 nonblock system
        let wd, system = watch instance "/dev" modelledEvents system
        let null', system = openFd writeOnly "/dev/null" 0 system
        let _, system = drain instance system
        let count, system = writeCounted null' "x" system
        let drained, _ = drain instance system

        actual [] [ "write-dev-null", $"wd=%d{wd} write=%s{count}: %s{drained}" ]
        |> shouldEqual (expected dentryProbe "V" [])

    [<Test>]
    let ``dentry section K: the Kestrel handoff is heard under FileSystemWatcher's mask`` () : unit =
        let system = boot root [ "k", directory 0o755 [] ]
        let instance, system = init1 nonblock system
        let wd, system = watch instance "k" dotnetMask system

        // `File.WriteAllText`: `FileMode.Create`.
        let creating =
            { openFlags FileAccessMode.WriteOnly true false true with
                CloseOnExec = true
            }

        let f, system = openFd creating "k/server-address.tmp" 0o666 system

        let written, system =
            system |> write f "http://127.0.0.1:5000" |> close f |> drain instance

        // `File.Move`, the destination absent: `rename(2)`.
        let moved, _ =
            system |> rename "k/server-address.tmp" "k/server-address" |> drain instance

        actual [] [ "write-all-text", $"wd=%d{wd}: %s{written}" ; "move", moved ]
        |> shouldEqual (expected dentryProbe "K" [])
