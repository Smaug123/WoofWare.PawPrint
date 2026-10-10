namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// A symbolic link's own permission bits: what each flavour creates a link
/// with (`SimulatedUnixPlatform.symlinkCreationPermissions`, against
/// `link-symlink.c`'s SYMMODE rows), what a seeded link gets, that every read
/// of a link's mode reads the bits its inode stores, and that
/// `UnixSystem.checkInvariants` refuses bits the flavour never creates.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSymlinkMode =

    let private context : string = "TestSymlinkMode"
    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L
    let private bits (raw : int) : PermissionBits = PermissionBits.parseOrFail context raw

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    // ------------------------------------------------------------ the probe's rows

    /// The probe's SYMMODE rows: each umask, and the mode `lstat` then reported
    /// for a link made under it.
    let private symmodeRows (resource : string) : (int * int) list =
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream resource

        if isNull stream then
            failwith $"%s{context}: no embedded resource %s{resource}"

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Seq.map (fun line -> line.Split '\t')
        |> Seq.filter (fun fields -> fields.[0] = "SYMMODE")
        |> Seq.map (fun fields ->
            let umask = Convert.ToInt32 (fields.[1].Substring "umask=".Length, 8)
            let mode = fields.[2].Substring (fields.[2].IndexOf "mode=" + "mode=".Length)
            umask, Convert.ToInt32 (mode, 8)
        )
        |> List.ofSeq

    let private envelopes : (string * SimulatedUnixPlatform * string) list =
        [
            "Linux root", linux, "WoofWare.PosixKernel.Test.linkSymlink.linuxRoot.txt"
            "Linux uid 1000", linux, "WoofWare.PosixKernel.Test.linkSymlink.linuxUser.txt"
            "Darwin uid 501", darwin, "WoofWare.PosixKernel.Test.linkSymlink.darwin.txt"
        ]

    [<Test>]
    let ``a new link's bits are what the probe measured under every umask it tried`` () : unit =
        for label, platform, resource in envelopes do
            let rows = symmodeRows resource
            rows.Length |> shouldEqual 5

            rows
            |> List.map (fun (umask, _) ->
                umask,
                SimulatedUnixPlatform.symlinkCreationPermissions platform (bits umask)
                |> PermissionBits.toInt
            )
            |> fun actual -> (label, actual) |> shouldEqual (label, rows)

    [<Test>]
    let ``Linux ignores the umask and Darwin applies it as it does to a file made with 0777`` () : unit =
        // The probe's five umasks generalised over all 512: Darwin's rule is a
        // creating open's with mode 0777 and no bits beyond the low nine.
        for umask in 0..0o777 do
            SimulatedUnixPlatform.symlinkCreationPermissions linux (bits umask)
            |> shouldEqual (bits 0o777)

            SimulatedUnixPlatform.symlinkCreationPermissions darwin (bits umask)
            |> shouldEqual (PermissionBits.fromCreationMode (bits 0o777) (bits umask) 0o777)

    // ------------------------------------------------------------ what reads the bits

    let private other : InodeOwner =
        {
            User = UserId.parseOrFail context 2000u
            Group = GroupId.parseOrFail context 2000u
        }

    let private caller : Credentials =
        Credentials.ofIds (UserId.parseOrFail context 501u) (GroupId.parseOrFail context 20u) []

    /// A process on `platform` as `caller`, in `/`, holding `/l -> f`, which
    /// `other` owns with `linkBits`, beside a file `f` everyone may read.
    let private systemWithLink (platform : SimulatedUnixPlatform) (linkBits : int) : UnixSystem<int, string> =
        let system : UnixSystem<int, string> =
            UnixSystem.initial platform
            |> Launched.bootWith (Launched.credentials caller) UnixSystem.pipedStandardStreams 0 (CpuId 0)

        let vfs = VirtualFileSystem.empty epoch (InodeOwner.ofProcess caller)
        let root = VirtualFileSystem.root vfs

        let vfs =
            match
                VirtualFileSystem.createFile root (name "f") (bits 0o644) other epoch ImmutableArray<byte>.Empty vfs
            with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"%s{context}: could not create f: %O{error}"

        let vfs =
            match
                VirtualFileSystem.createSymlink
                    root
                    (name "l")
                    (bits linkBits)
                    other
                    epoch
                    (SymlinkTarget.parseOrFail context "f")
                    vfs
            with
            | Ok (_, vfs) -> vfs
            | Error error -> failwith $"%s{context}: could not create l: %O{error}"

        { system with
            Machine =
                { system.Machine with
                    FileSystem = vfs
                }
            Process =
                { system.Process with
                    CurrentDirectoryInode = root
                }
        }
        |> Launched.restand

    let private lstatMode (system : UnixSystem<int, string>) : int =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofText "/l") system with
        | Ok (FileStatusAnswer.Reported status) -> status.Mode
        | other -> failwith $"%s{context}: lstat(/l) did not report: %A{other}"

    [<Test>]
    let ``lstat reports the bits a Darwin link stores`` () : unit =
        for linkBits in [ 0o777 ; 0o755 ; 0o700 ; 0o077 ; 0 ] do
            lstatMode (systemWithLink darwin linkBits)
            |> shouldEqual (0o120000 ||| linkBits)

    [<Test>]
    let ``Darwin's faccessat without following asks about the link's stored bits`` () : unit =
        // AT_SYMLINK_NOFOLLOW is 0x20 on Darwin, AT_FDCWD -2; R_OK is 4. The
        // caller is neither the link's owner nor in its group, so "other"
        // decides.
        let ask (linkBits : int) : SyscallAnswer =
            match UnixPathResolution.faccessat -2 (PathArg.ofText "/l") 4 0x20 (systemWithLink darwin linkBits) with
            | Ok answer -> answer
            | Error refusal -> failwith $"%s{context}: faccessat was refused: %s{AccessRefusal.describe refusal}"

        ask 0o777 |> shouldEqual (SyscallAnswer.Completed 0L)
        ask 0o770 |> shouldEqual (SyscallAnswer.Failed UnixError.EACCES)

    // ------------------------------------------------------------ seeds

    let private seeded (platform : SimulatedUnixPlatform) (umask : int) : UnixSystem<int, string> =
        let seed =
            Map.ofList
                [
                    name "f", SeedEntry.file ImmutableArray<byte>.Empty
                    name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail context "f", None)
                ]

        let image : UnixBootImage<int, string> = UnixSystem.initial platform

        match
            UnixBootImage.withFileSystem
                epoch
                (InodeOwner.ofProcess (UnixSystem.defaultCredentials (SimulatedUnixPlatform.flavour platform)))
                seed
                image
        with
        | Ok image ->
            (Launched.bootWith
                (Launched.umask (bits umask)
                 >> ProcessLaunch.withCurrentDirectory (AbsoluteUnixPath.parseOrFail context "/"))
                UnixSystem.pipedStandardStreams
                0
                (CpuId 0))
                image
        | Error fault -> failwith $"%s{context}: could not seed: %A{fault}"

    [<Test>]
    let ``a seeded link has what a link made under umask 022 has, whatever the process's umask`` () : unit =
        // A seed is a tree some other process built, so the process's own umask
        // never applied to it.
        for umask in [ 0o022 ; 0o077 ; 0 ] do
            lstatMode (seeded linux umask) |> shouldEqual 0o120777
            lstatMode (seeded darwin umask) |> shouldEqual 0o120755

    // ------------------------------------------------------------ the invariant

    let private symlinkDefects (system : UnixSystem<int, string>) : UnixSystemDefect<int> list =
        UnixSystem.checkInvariants system
        |> List.filter (fun defect ->
            match defect with
            | UnixSystemDefect.SymlinkPermissionsNotOfFlavour _ -> true
            | _ -> false
        )

    [<Test>]
    let ``checkInvariants refuses a link's bits exactly where its flavour can never give it them`` () : unit =
        for platform in [ linux ; darwin ] do
            let flavour = SimulatedUnixPlatform.flavour platform

            for linkBits in [ 0..0o777 ] @ [ 0o1777 ; 0o2777 ; 0o4755 ] do
                let system = systemWithLink platform linkBits

                let creatable =
                    [ 0..0o777 ]
                    |> List.exists (fun umask ->
                        SimulatedUnixPlatform.symlinkCreationPermissions platform (bits umask) = bits linkBits
                    )

                // A flavour whose fchmodat(AT_SYMLINK_NOFOLLOW) changes a link
                // can give it any bits its owner asks for.
                let changeable =
                    match SimulatedUnixPlatform.symlinkModeChange platform with
                    | SymlinkModeChange.ChangesLink -> true
                    | SymlinkModeChange.NotSupported -> false

                let expected =
                    if creatable || changeable then
                        []
                    else
                        let inode =
                            match
                                PathWalk.resolveExisting
                                    (SimulatedUnixPlatform.pathLimits platform)
                                    Owners.root
                                    SymlinkProtection.Off
                                    system.Process.CurrentDirectoryInode
                                    SymlinkPolicy.NoFollowFinal
                                    (UnixPath.parseOrFail context "/l")
                                    system.Machine.FileSystem
                            with
                            | Ok inode -> inode
                            | Error failure -> failwith $"%s{context}: /l does not resolve: %A{failure}"

                        [
                            UnixSystemDefect.SymlinkPermissionsNotOfFlavour (inode, bits linkBits, flavour)
                        ]

                (flavour, linkBits, symlinkDefects system)
                |> shouldEqual (flavour, linkBits, expected)

    [<Test>]
    let ``only 0777 is a Linux link's, and any bits a Darwin link's`` () : unit =
        // The previous test's reference, stated as the flavours' own facts.
        symlinkDefects (systemWithLink linux 0o777) |> shouldEqual []
        symlinkDefects (systemWithLink linux 0o755) |> List.length |> shouldEqual 1
        symlinkDefects (systemWithLink darwin 0) |> shouldEqual []
        symlinkDefects (systemWithLink darwin 0o700) |> shouldEqual []
        symlinkDefects (systemWithLink darwin 0o4777) |> shouldEqual []
        symlinkDefects (systemWithLink darwin 0o7777) |> shouldEqual []
