namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test.InotifyReplay

/// The changes this kernel refuses rather than report wrongly to an inotify
/// watch, and the same changes going through where no watch would hear of
/// them.
///
/// A change made through a descriptor is reported by Linux at the watch on the
/// directory of the name the descriptor was opened through, which this kernel
/// does not record, and which may since have been unlinked; so it is refused
/// when a watch asking for an event it raises is on the inode itself, or on
/// any directory but one with `IN_EXCL_UNLINK`, not naming the inode, for a
/// write (for a directory's descriptor, its parent alone). A change through a directory
/// `rmdir` removed, reached by ".", is reported by Linux at its old parent
/// under its old name, which the graph does not keep; so it is refused when
/// that parent's watch asks for an event it raises.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyRefusals =

    let private seed : (string * SeedEntry) list =
        [
            "a", directory 0o755 [ "x", file "x" ]
            "b", directory 0o755 []
            "u", directory 0o755 []
        ]

    let private writable : OpenFlags =
        openFlags FileAccessMode.ReadWrite false false false

    let private contentsOf (path : string) (system : UnixSystem<int, string>) : string =
        match (inodeOf path system).Content with
        | InodeContent.RegularFile (contents, _) -> System.Text.Encoding.ASCII.GetString (contents.AsSpan().ToArray ())
        | other -> failwith $"%s{path} is %A{other}"

    /// A machine of `seed`, an instance watching `path` for `mask`, and `a/x`
    /// open for reading and writing.
    let private watchingThenOpen (path : string) (mask : uint32) : int * int * UnixSystem<int, string> =
        let system = boot root seed
        let instance, system = init1 nonblock system
        let _, system = watch instance path mask system
        let fd, system = openFd writable "a/x" 0 system
        instance, fd, system

    [<Test>]
    let ``a write through a descriptor is refused under a watch on its directory asking for IN_MODIFY`` () : unit =
        let _, fd, system = watchingThenOpen "a" modify
        refused throughDescriptor (fun () -> write fd "y" system)

    [<Test>]
    let ``each change through a descriptor is refused under a watch that would hear of it`` () : unit =
        let _, fd, system = watchingThenOpen "a" (modify ||| attrib)
        refused throughDescriptor (fun () -> pwrite fd "y" 0L system)
        refused throughDescriptor (fun () -> ftruncate fd 0L system)

        refused
            throughDescriptor
            (fun () ->
                UnixPathResolution.tryFchmod fd 0o600 system
                |> Result.map (fun (answer, _) -> answer)
            )

        refused
            throughDescriptor
            (fun () -> UnixPathResolution.tryFchown fd (Some UserId.root) None system |> Result.map fst)

        refused
            throughDescriptor
            (fun () ->
                UnixPathResolution.tryUtimensat fd NullablePathArgument.Null TimesArgument.Null 0 system
                |> Result.map fst
            )

        // `fchmodat`'s empty path under Linux's `AT_EMPTY_PATH` is the descriptor.
        refused
            throughDescriptor
            (fun () ->
                UnixPathResolution.tryFchmodat fd (PathArg.ofText "") 0o600 0x1000 system
                |> Result.map fst
            )

    [<Test>]
    let ``copy_file_range into a file under a watch asking for IN_MODIFY is refused`` () : unit =
        let system = boot root (seed @ [ "source", file "abc" ])
        let instance, system = init1 nonblock system
        let _, system = watch instance "a" modify system

        let source, system =
            openFd (openFlags FileAccessMode.ReadOnly false false false) "source" 0 system

        let destination, system = openFd writable "a/x" 0 system

        refused
            throughDescriptor
            (fun () -> UnixReadWrite.tryCopyFileRange source destination 3UL 0 system |> Result.map fst)

    [<Test>]
    let ``a change through a descriptor goes through, queuing nothing, where no watch asks for its events`` () : unit =
        let instance, fd, system = watchingThenOpen "a" (create ||| delete)
        let system = write fd "y" system |> ftruncate fd 1L
        contentsOf "a/x" system |> shouldEqual "y"
        drain instance system |> fst |> shouldEqual "(none)"

    [<Test>]
    let ``a change through a descriptor goes through where no instance exists`` () : unit =
        let system = boot root seed
        let fd, system = openFd writable "a/x" 0 system
        let system = write fd "y" system
        contentsOf "a/x" system |> shouldEqual "y"

    [<Test>]
    let ``a write is refused under a watch on the file itself`` () : unit =
        let _, fd, system = watchingThenOpen "a/x" modify
        refused throughDescriptor (fun () -> write fd "y" system)

    [<Test>]
    let ``a write is refused under a watch on any directory holding a name of the file`` () : unit =
        // `a/x` was opened, and `b` holds the file under another name: which
        // name the descriptor came through is not recorded.
        let instance, fd, system = watchingThenOpen "b" modify
        let system = link "a/x" "b/other" system
        let _, system = drain instance system
        refused throughDescriptor (fun () -> write fd "y" system)

    [<Test>]
    let ``a write to a file with no name left is refused under any watch asking for IN_MODIFY`` () : unit =
        let _, fd, system = watchingThenOpen "u" modify
        let system = unlink "a/x" system
        refused throughDescriptor (fun () -> write fd "y" system)

    [<Test>]
    let ``a write through a name since unlinked is refused under a watch on that name's directory, though another name survives``
        ()
        : unit
        =
        // Found by Codex: Linux reports the write at `a` under the old name
        // (`inotify-probe.c` row H write-via-b-after-unlink-of-b), and `a`
        // no longer names the file.
        let instance, fd, system = watchingThenOpen "a" modify
        let system = system |> link "a/x" "b/y" |> unlink "a/x"
        let _, system = drain instance system
        refused throughDescriptor (fun () -> write fd "y" system)

    [<Test>]
    let ``a write is refused under a watch on any directory asking for IN_MODIFY, which could hold the name it was opened through``
        ()
        : unit
        =
        let _, fd, system = watchingThenOpen "u" modify
        refused throughDescriptor (fun () -> write fd "y" system)

    [<Test>]
    let ``a write goes through under a watch with IN_EXCL_UNLINK on a directory not naming the file`` () : unit =
        // Measured (`inotify-probe.c` row D write-unlinked-excl-unlink): such
        // a watch hears nothing of a write through an unlinked name.
        let instance, fd, system = watchingThenOpen "u" (modify ||| 0x04000000u)
        let system = write fd "y" system
        drain instance system |> fst |> shouldEqual "(none)"

    [<Test>]
    let ``fchmod is refused under a watch with IN_EXCL_UNLINK on a directory not naming the file`` () : unit =
        let _, fd, system = watchingThenOpen "u" (attrib ||| 0x04000000u)

        refused throughDescriptor (fun () -> UnixPathResolution.tryFchmod fd 0o600 system |> Result.map fst)

    [<Test>]
    let ``fchmod of a directory's descriptor goes through under a watch on a directory that is not its parent``
        ()
        : unit
        =
        let system = boot root seed
        let instance, system = init1 nonblock system
        let _, system = watch instance "u" attrib system

        let fd, system =
            openFd (openFlags FileAccessMode.ReadOnly false false false) "a" 0 system

        match UnixPathResolution.tryFchmod fd 0o700 system with
        | Ok (SyscallAnswer.Completed 0L, system) -> drain instance system |> fst |> shouldEqual "(none)"
        | other -> failwith $"%A{other}"

    [<Test>]
    let ``a write goes through under a watch on another file asking for IN_MODIFY, which holds no name`` () : unit =
        let system = boot root (seed @ [ "other", file "o" ])
        let instance, system = init1 nonblock system
        let _, system = watch instance "other" modify system
        let fd, system = openFd writable "a/x" 0 system
        let system = write fd "y" system
        drain instance system |> fst |> shouldEqual "(none)"

    [<Test>]
    let ``a write to a file with no name left goes through where no watch asks for IN_MODIFY`` () : unit =
        let instance, fd, system = watchingThenOpen "u" create
        let system = unlink "a/x" system |> write fd "y"
        drain instance system |> fst |> shouldEqual "(none)"

    [<Test>]
    let ``a write that clears set-ID bits is refused under a watch asking only for IN_ATTRIB, and one that clears none goes through``
        ()
        : unit
        =
        let system =
            boot
                unprivileged
                [
                    "z", directoryOwnedBy 1000u 0o777 [ "f", fileOwnedBy 1000u "x" ; "g", fileOwnedBy 1000u "x" ]
                ]

        let system = chmod "z/f" 0o6755 system
        let instance, system = init1 nonblock system
        let _, system = watch instance "z" attrib system
        let f, system = openFd writable "z/f" 0 system
        let g, system = openFd writable "z/g" 0 system
        refused throughDescriptor (fun () -> write f "y" system)
        let system = write g "y" system
        drain instance system |> fst |> shouldEqual "(none)"

    [<Test>]
    let ``fchmod of a directory's descriptor is refused under a watch on its parent`` () : unit =
        let system = boot root seed
        let instance, system = init1 nonblock system
        let _, system = watch instance "." attrib system

        let fd, system =
            openFd (openFlags FileAccessMode.ReadOnly false false false) "a" 0 system

        refused throughDescriptor (fun () -> UnixPathResolution.tryFchmod fd 0o700 system |> Result.map fst)

    /// A machine whose process stands in `gone`, which `rmdir` has removed,
    /// with an instance watching `.` (the old parent) for `parentMask`, and
    /// `gone` itself for every event.
    let private standingInRemoved (parentMask : uint32) : int * UnixSystem<int, string> =
        let system = boot root seed |> mkdir "gone" 0o755
        let instance, system = init1 nonblock system
        let _, system = watch instance "." parentMask system
        let _, system = watch instance "gone" modelledEvents system
        let system = system |> chdir "gone" |> rmdir "../gone"
        let _, system = drain instance system
        instance, system

    [<Test>]
    let ``a change through a removed current directory is refused under a watch on its old parent that would hear of it``
        ()
        : unit
        =
        let _, system = standingInRemoved attrib
        refused throughRemovedDirectory (fun () -> chmod "." 0o700 system)

        refused
            throughRemovedDirectory
            (fun () ->
                UnixPathResolution.tryUtimensat
                    -100
                    (NullablePathArgument.NotNull (PathArg.ofText "."))
                    TimesArgument.Null
                    0
                    system
                |> Result.map fst
            )

        // `fchmodat`'s empty path from `AT_FDCWD` under `AT_EMPTY_PATH` is ".".
        refused
            throughRemovedDirectory
            (fun () ->
                UnixPathResolution.tryFchmodat -100 (PathArg.ofText "") 0o700 0x1000 system
                |> Result.map fst
            )

    [<Test>]
    let ``a change through a removed current directory goes through, heard at its own watch, where its old parent's watch asks for nothing it raises``
        ()
        : unit
        =
        let instance, system = standingInRemoved create
        let system = chmod "." 0o700 system

        drain instance system
        |> fst
        |> shouldEqual "wd=2 mask=ATTRIB+ISDIR cookie=0 len=0 name=\"\""

    [<Test>]
    let ``a change through a removed current directory goes through where no instance exists`` () : unit =
        let system = boot root seed |> mkdir "gone" 0o755 |> chdir "gone" |> rmdir "../gone"
        let system = chmod "." 0o700 system
        modeOf "." system |> shouldEqual 0o700
