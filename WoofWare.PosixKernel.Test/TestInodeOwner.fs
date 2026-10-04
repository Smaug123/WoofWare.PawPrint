namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Who owns an inode: what a new one is given, what a seed gives one, what
/// `stat` reports, and that nothing else moves an owner.
///
/// The creation rows come from
/// `docs/plans/2026-08-23-posix-kernel-extraction/new-inode-owner.c`, run on
/// Linux 6.18.5 (aarch64) as root and on Darwin 27.0 at uid 501.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInodeOwner =

    let private context : string = "TestInodeOwner"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private epoch : UnixTimestamp = UnixTimestamp.ofMillisecondsSinceEpoch 0L

    let private owner (user : uint32) (group : uint32) : InodeOwner =
        {
            User = UserId.parseOrFail context user
            Group = GroupId.parseOrFail context group
        }

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    let private mode (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits

    let private name (text : string) : DirectoryEntryName =
        DirectoryEntryName.parseOrFail context text

    let private ok (result : Result<'a, 'e>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected Ok, got %O{error}"

    let private ownerGen : Gen<InodeOwner> =
        Gen.map2
            (fun user group ->
                {
                    User = user
                    Group = group
                }
            )
            CredentialsGen.userId
            CredentialsGen.groupId

    // ------------------------------------------------------------- the creation rule

    [<Test>]
    let ``each flavour takes a new inode's group from where it was measured to`` () : unit =
        SimulatedUnixPlatform.newInodeGroupRule SimulatedUnixPlatform.linuxX64
        |> shouldEqual NewInodeGroupRule.CreatorsUnlessParentSetGroupId

        SimulatedUnixPlatform.newInodeGroupRule SimulatedUnixPlatform.macOsArm64
        |> shouldEqual NewInodeGroupRule.Parents

    [<Test>]
    let ``a new inode belongs to the creator's effective user, and to the group its rule names`` () : unit =
        // Every parent mode, so that only the set-group-ID bit can matter.
        let property
            (rule : NewInodeGroupRule)
            (credentials : Credentials)
            (parent : InodeOwner)
            (parentMode : int)
            : unit
            =
            let created = InodeOwner.ofNewInode rule credentials parent (mode parentMode)

            created.User |> shouldEqual credentials.EffectiveUser

            let expectedGroup =
                match rule with
                | NewInodeGroupRule.Parents -> parent.Group
                | NewInodeGroupRule.CreatorsUnlessParentSetGroupId ->
                    if parentMode &&& 0o2000 <> 0 then
                        parent.Group
                    else
                        credentials.EffectiveGroup

            created.Group |> shouldEqual expectedGroup

        let gen =
            gen {
                let! rule =
                    Gen.elements [ NewInodeGroupRule.CreatorsUnlessParentSetGroupId ; NewInodeGroupRule.Parents ]

                let! credentials = CredentialsGen.credentials
                let! parent = ownerGen
                let! parentMode = Gen.choose (0, 0o7777)
                return rule, credentials, parent, parentMode
            }

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen gen)
                (fun (rule, credentials, parent, parentMode) -> property rule credentials parent parentMode)
        )

    [<Test>]
    let ``the measured Linux rows`` () : unit =
        // The creator: real 1000, effective 1000 or 1001, effective group 1000,
        // supplementary groups {1000, 2000}. Group 3000 is one it is not in.
        let creator (effective : uint32) : Credentials =
            { Credentials.ofIds (uid effective) (gid 1000u) [ gid 1000u ; gid 2000u ] with
                RealUser = uid 1000u
            }

        let rule = NewInodeGroupRule.CreatorsUnlessParentSetGroupId

        let rows =
            [
                // effective uid, parent group, parent mode, measured owner
                1000u, 2000u, 0o777, owner 1000u 1000u
                1001u, 2000u, 0o777, owner 1001u 1000u
                1000u, 2000u, 0o2777, owner 1000u 2000u
                1001u, 2000u, 0o2777, owner 1001u 2000u
                1000u, 3000u, 0o2777, owner 1000u 3000u
                1001u, 3000u, 0o2777, owner 1001u 3000u
                1000u, 3000u, 0o777, owner 1000u 1000u
                1001u, 3000u, 0o777, owner 1001u 1000u
            ]

        for effective, parentGroup, parentMode, expected in rows do
            InodeOwner.ofNewInode rule (creator effective) (owner 0u parentGroup) (mode parentMode)
            |> shouldEqual expected

    [<Test>]
    let ``the measured Darwin rows`` () : unit =
        // uid 501, effective group 20, supplementary groups 20, 12, 61, 100, 701.
        let creator =
            Credentials.ofIds (uid 501u) (gid 20u) [ gid 20u ; gid 12u ; gid 61u ; gid 100u ; gid 701u ]

        let rule = NewInodeGroupRule.Parents

        let rows =
            [
                // parent group, parent mode, measured owner
                20u, 0o777, owner 501u 20u
                12u, 0o777, owner 501u 12u
                12u, 0o2777, owner 501u 12u
                // wheel, which the creator is not in
                0u, 0o777, owner 501u 0u
            ]

        for parentGroup, parentMode, expected in rows do
            InodeOwner.ofNewInode rule creator (owner 501u parentGroup) (mode parentMode)
            |> shouldEqual expected

    /// A system on `platform` running as `credentials`, holding a directory
    /// `/p` owned by `parentOwner` with mode `parentMode`, and nothing else.
    let private systemWithParent
        (platform : SimulatedUnixPlatform)
        (credentials : Credentials)
        (parentOwner : InodeOwner)
        (parentMode : int)
        : UnixSystem<int, string>
        =
        let seed =
            Map.ofList [ name "p", SeedEntry.Directory (Map.empty, mode parentMode, Some parentOwner) ]

        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.withFileSystemAndCurrentDirectory epoch (owner 0u 0u) seed AbsoluteUnixPath.root
        |> ok
        |> UnixBootImage.withCredentials context credentials
        |> UnixBootImage.boot

    let private ownerAt (path : string) (system : UnixSystem<int, string>) : InodeOwner =
        match
            UnixPathResolution.stat
                SymlinkPolicy.NoFollowFinal
                (PathArg.ofPath (UnixPath.parseOrFail context path))
                system
        with
        | Ok (FileStatusAnswer.Reported status) ->
            {
                User = status.UserId
                Group = status.GroupId
            }
        | other -> failwith $"expected a status for %s{path}, got %A{other}"

    let private creating : OpenFlags =
        {
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = true
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    /// Create `/p/file` with `open(O_CREAT)` and `/p/dir` with `mkdir`, and
    /// report who owns each.
    let private createBoth (system : UnixSystem<int, string>) : InodeOwner * InodeOwner =
        let system =
            match Answered.openPath creating (UnixPath.parseOrFail context "/p/file") 0o644 system with
            | SyscallAnswer.Completed _, system -> system
            | other -> failwith $"expected the file to be created, got %O{other}"

        let system =
            match Answered.mkdir (PathArg.ofPath (UnixPath.parseOrFail context "/p/dir")) 0o755 system with
            | SyscallAnswer.Completed 0L, system -> system
            | other -> failwith $"expected the directory to be created, got %O{other}"

        ownerAt "/p/file" system, ownerAt "/p/dir" system

    [<Test>]
    let ``open and mkdir give a new inode the owner the Linux rule names`` () : unit =
        // Six distinct IDs, so that an owner read from the wrong one names which.
        let credentials =
            {
                RealUser = uid 51u
                EffectiveUser = uid 52u
                SavedUser = uid 53u
                RealGroup = gid 61u
                EffectiveGroup = gid 62u
                SavedGroup = gid 63u
                SupplementaryGroups = [ gid 64u ]
            }

        let plain =
            systemWithParent SimulatedUnixPlatform.linuxX64 credentials (owner 70u 71u) 0o777

        createBoth plain |> shouldEqual (owner 52u 62u, owner 52u 62u)

        let setGroupId =
            systemWithParent SimulatedUnixPlatform.linuxX64 credentials (owner 70u 71u) 0o2777

        createBoth setGroupId |> shouldEqual (owner 52u 71u, owner 52u 71u)

    [<Test>]
    let ``open and mkdir give a new inode the owner the Darwin rule names`` () : unit =
        let credentials = Credentials.ofIds (uid 501u) (gid 20u) []

        for parentMode in [ 0o777 ; 0o2777 ] do
            systemWithParent SimulatedUnixPlatform.macOsArm64 credentials (owner 70u 71u) parentMode
            |> createBoth
            |> shouldEqual (owner 501u 71u, owner 501u 71u)

    // ------------------------------------------------------------- stat

    [<Test>]
    let ``stat reports the inode's owner, not the caller's`` () : unit =
        let credentials = Credentials.ofIds (uid 41u) (gid 43u) []

        let system =
            systemWithParent SimulatedUnixPlatform.linuxX64 credentials (owner 70u 71u) 0o755

        ownerAt "/p" system |> shouldEqual (owner 70u 71u)
        // The root is the seed's default owner's.
        ownerAt "/" system |> shouldEqual (owner 0u 0u)

    // ------------------------------------------------------------- seeds

    [<Test>]
    let ``a seed entry is owned by its own owner, or by the default, and never by its parent's`` () : unit =
        let defaultOwner = owner 1u 2u
        let outer = owner 3u 4u
        let inner = owner 5u 6u
        let link = owner 7u 8u

        let target = SymlinkTarget.parseOrFail context "nowhere"

        let seed =
            Map.ofList
                [
                    name "plain", SeedEntry.file ImmutableArray.Empty
                    name "owned",
                    SeedEntry.Directory (
                        Map.ofList
                            [
                                // No owner of its own, inside a directory that has one.
                                name "inherits-nothing", SeedEntry.file ImmutableArray.Empty
                                name "own", SeedEntry.File (ImmutableArray.Empty, mode 0o600, Some inner)
                                name "defaultLink", SeedEntry.Symlink (target, None)
                                name "ownLink", SeedEntry.Symlink (target, Some link)
                                name "plainDir", SeedEntry.directory Map.empty
                            ],
                        mode 0o755,
                        Some outer
                    )
                ]

        let vfs =
            VirtualFileSystem.ofFileSystemSeed epoch defaultOwner SymlinkModes.linux seed

        let system =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)
            |> UnixBootImage.withFileSystemAndCurrentDirectory epoch defaultOwner seed AbsoluteUnixPath.root
            |> ok
            |> UnixBootImage.boot

        let expected =
            [
                "/", defaultOwner
                "/plain", defaultOwner
                "/owned", outer
                "/owned/inherits-nothing", defaultOwner
                "/owned/own", inner
                "/owned/defaultLink", defaultOwner
                "/owned/ownLink", link
                "/owned/plainDir", defaultOwner
            ]

        for path, expectedOwner in expected do
            ownerAt path system |> shouldEqual expectedOwner

        // The system realised the seed exactly as the filesystem function does,
        // and then mounted its device filesystem over `dev`, as it does at boot.
        let withDevices =
            VirtualFileSystem.mountAtRoot
                MountedFileSystem.Devtmpfs
                (name "dev")
                (mode 0o755)
                (owner 0u 0u)
                (CharacterDevice.all
                 |> List.map (fun device -> CharacterDevice.name device, device, CharacterDevice.permissions device))
                epoch
                vfs
            |> ok

        system.Machine.FileSystem |> shouldEqual withDevices

    [<Test>]
    let ``every inode of a seed without owners belongs to the default owner`` () : unit =
        let property (defaultOwner : InodeOwner) : unit =
            let seed =
                Map.ofList
                    [
                        name "d",
                        SeedEntry.directory (
                            Map.ofList
                                [
                                    name "f", SeedEntry.file ImmutableArray.Empty
                                    name "l", SeedEntry.Symlink (SymlinkTarget.parseOrFail context "f", None)
                                ]
                        )
                    ]

            VirtualFileSystem.ofFileSystemSeed epoch defaultOwner SymlinkModes.linux seed
            |> VirtualFileSystem.inodes
            |> Map.iter (fun _ inode -> inode.Owner |> shouldEqual defaultOwner)

        Check.One (config, Prop.forAll (Arb.fromGen ownerGen) property)

    // ------------------------------------------------------------- nothing else moves an owner

    [<Test>]
    let ``no operation on the graph changes an existing inode's owner`` () : unit =
        // Every inode has an owner of its own, so an operation that rebuilt one
        // inode from another's record, or from a constant, would show up here.
        let a = owner 10u 11u
        let b = owner 20u 21u
        let c = owner 30u 31u
        let d = owner 40u 41u
        let e = owner 50u 51u
        let f = owner 60u 61u
        let g = owner 70u 71u
        let fresh = owner 80u 81u

        let vfs = VirtualFileSystem.empty epoch a
        let root = VirtualFileSystem.root vfs

        let p, vfs =
            VirtualFileSystem.createDirectory root (name "p") (mode 0o755) b epoch vfs |> ok

        let q, vfs =
            VirtualFileSystem.createDirectory root (name "q") (mode 0o755) c epoch vfs |> ok

        let file, vfs =
            VirtualFileSystem.createFile p (name "f") (mode 0o644) d epoch ImmutableArray.Empty vfs
            |> ok

        let sub, vfs =
            VirtualFileSystem.createDirectory p (name "sub") (mode 0o755) e epoch vfs |> ok

        let other, vfs =
            VirtualFileSystem.createFile q (name "g") (mode 0o644) f epoch ImmutableArray.Empty vfs
            |> ok

        let link, vfs =
            VirtualFileSystem.createSymlink
                q
                (name "l")
                SymlinkModes.linux
                g
                epoch
                (SymlinkTarget.parseOrFail context "g")
                vfs
            |> ok

        let before = VirtualFileSystem.inodes vfs |> Map.map (fun _ inode -> inode.Owner)

        let later = UnixTimestamp.ofMillisecondsSinceEpoch 1000L

        let added, vfs =
            VirtualFileSystem.createFile p (name "new") (mode 0o644) fresh later ImmutableArray.Empty vfs
            |> ok

        let vfs = VirtualFileSystem.hardLink q (name "f2") file later vfs |> ok

        let _, vfs =
            VirtualFileSystem.unbind UnbindTargetEffect.LostALink q (name "g") later vfs
            |> ok

        // Within one directory, which rebuilds that directory once...
        let _, vfs = VirtualFileSystem.rename q (name "f2") q (name "f4") later vfs |> ok

        // ...and across directories, a file and then a directory, whose `..` moves.
        let _, vfs = VirtualFileSystem.rename p (name "f") q (name "f3") later vfs |> ok

        let _, vfs = VirtualFileSystem.rename p (name "sub") q (name "sub2") later vfs |> ok

        let vfs =
            VirtualFileSystem.writeFile
                file
                0L
                (ImmutableArray.Create 1uy)
                SetGroupIdOnWrite.StripAlways
                Owners.linuxDefaultCaller
                later
                vfs
            |> ok

        let vfs =
            VirtualFileSystem.truncateFile file 0L SetIdBitsOnTruncation.Strip Owners.linuxDefaultCaller later vfs
            |> ok

        let after = VirtualFileSystem.inodes vfs |> Map.map (fun _ inode -> inode.Owner)

        for KeyValue (inode, owner) in before do
            Map.tryFind inode after |> shouldEqual (Some owner)

        Map.find added after |> shouldEqual fresh

        // Every inode the operations touched survived them, so none of the
        // comparisons above was skipped for want of an inode to compare.
        for inode in [ root ; p ; q ; file ; sub ; other ; link ] do
            Map.containsKey inode after |> shouldEqual true
