namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test.InotifyReplay

/// The changes this kernel refuses rather than report wrongly to an inotify
/// watch, and the same changes going through where no watch would hear of
/// them.
///
/// A change through a directory `rmdir` removed, reached by "." or through a
/// descriptor opened by ".", is reported by Linux at its old parent under its
/// old name, which the graph does not keep; so it is refused when that
/// parent's watch asks for an event it raises. A descriptor opened through
/// the directory's name before it was removed keeps that name, and its
/// changes are heard (`TestInotifyDentryMeasured`, section Q).
///
/// A refusal is `UnplacedChangeRefusal`, inside each call's own refusal, from
/// its internal `tryX`; the public entry point raises it as
/// `UnmodelledException`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInotifyRefusals =

    let private seed : (string * SeedEntry) list =
        [
            "a", directory 0o755 [ "x", file "x" ]
            "b", directory 0o755 []
            "u", directory 0o755 []
        ]

    let private contentsOf (path : string) (system : UnixSystem<int, string>) : string =
        match (inodeOf path system).Content with
        | InodeContent.RegularFile (contents, _) -> System.Text.Encoding.ASCII.GetString (contents.AsSpan().ToArray ())
        | other -> failwith $"%s{path} is %A{other}"

    [<Test>]
    let ``a change through a descriptor goes through where no instance exists`` () : unit =
        let system = boot root seed

        let fd, system =
            openFd (openFlags FileAccessMode.ReadWrite false false false) "a/x" 0 system

        let system = write fd "y" system
        contentsOf "a/x" system |> shouldEqual "y"

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
        chmodRefusal "." 0o700 system |> throughRemovedDirectory

        UnixPathResolution.tryUtimensat
            -100
            (NullablePathArgument.NotNull (PathArg.ofText "."))
            TimesArgument.Null
            0
            system
        |> unplaced
            "utimensat(.)"
            (fun refusal ->
                match refusal with
                | UTimensAtRefusal.UnplacedChange refusal -> Some refusal
                | _ -> None
            )
        |> throughRemovedDirectory

        // `fchmodat`'s empty path from `AT_FDCWD` under `AT_EMPTY_PATH` is ".".
        UnixPathResolution.tryFchmodat -100 (PathArg.ofText "") 0o700 0x1000 system
        |> unplaced
            "fchmodat(AT_FDCWD, \"\", AT_EMPTY_PATH)"
            (fun refusal ->
                match refusal with
                | FChModAtRefusal.ChMod (ChModRefusal.UnplacedChange refusal) -> Some refusal
                | _ -> None
            )
        |> throughRemovedDirectory

    [<Test>]
    let ``the public entry point raises the refusal of a change through a removed directory as UnmodelledException``
        ()
        : unit
        =
        let _, system = standingInRemoved attrib

        let error =
            Assert.Throws<UnmodelledException> (fun () ->
                UnixPathResolution.chmod (PathArg.ofText ".") 0o700 system
                |> ignore<SyscallAnswer * UnixSystem<int, string>>
            )

        error.Message |> shouldContainText "under the name it had"

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

    /// `standingInRemoved`, and a descriptor opened through "." of the
    /// removed directory.
    let private openedInRemoved (parentMask : uint32) : int * int * UnixSystem<int, string> =
        let instance, system = standingInRemoved parentMask

        let fd, system =
            openFd (openFlags FileAccessMode.ReadOnly false false false) "." 0 system

        instance, fd, system

    [<Test>]
    let ``a change through a descriptor opened by "." of a removed directory is refused under a watch on its old parent that would hear of it``
        ()
        : unit
        =
        let _, fd, system = openedInRemoved attrib

        UnixPathResolution.tryFchmod fd 0o700 system
        |> unplaced
            "fchmod"
            (fun refusal ->
                match refusal with
                | FChModRefusal.UnplacedChange refusal -> Some refusal
                | _ -> None
            )
        |> throughRemovedDirectory

        UnixPathResolution.tryFchown fd (Some UserId.root) None system
        |> unplaced
            "fchown"
            (fun refusal ->
                match refusal with
                | FChOwnRefusal.UnplacedChange refusal -> Some refusal
                | _ -> None
            )
        |> throughRemovedDirectory

        UnixPathResolution.tryUtimensat fd NullablePathArgument.Null TimesArgument.Null 0 system
        |> unplaced
            "futimens"
            (fun refusal ->
                match refusal with
                | UTimensAtRefusal.UnplacedChange refusal -> Some refusal
                | _ -> None
            )
        |> throughRemovedDirectory

        // `fchmodat`'s empty path under Linux's `AT_EMPTY_PATH` is the descriptor.
        UnixPathResolution.tryFchmodat fd (PathArg.ofText "") 0o700 atEmptyPath system
        |> unplaced
            "fchmodat(fd, \"\", AT_EMPTY_PATH)"
            (fun refusal ->
                match refusal with
                | FChModAtRefusal.Descriptor (FChModRefusal.UnplacedChange refusal) -> Some refusal
                | _ -> None
            )
        |> throughRemovedDirectory

    [<Test>]
    let ``the public entry point raises the refusal of a change through a descriptor of a removed directory as UnmodelledException``
        ()
        : unit
        =
        let _, fd, system = openedInRemoved attrib

        let error =
            Assert.Throws<UnmodelledException> (fun () ->
                UnixPathResolution.fchmod fd 0o700 system
                |> ignore<SyscallAnswer * UnixSystem<int, string>>
            )

        error.Message |> shouldContainText "under the name it had"

    [<Test>]
    let ``a change through a descriptor opened by "." of a removed directory goes through, heard at its own watch, where its old parent's watch asks for nothing it raises``
        ()
        : unit
        =
        let instance, fd, system = openedInRemoved create
        let system = fchmod fd 0o700 system

        drain instance system
        |> fst
        |> shouldEqual "wd=2 mask=ATTRIB+ISDIR cookie=0 len=0 name=\"\""
