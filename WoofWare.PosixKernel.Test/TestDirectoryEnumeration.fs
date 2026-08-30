namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `VirtualFileSystem.nextDirectoryEntry`: the walk an open directory stream
/// makes, and what a mutation part-way through does to it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryEnumeration =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private buildTime : UnixTimestamp =
        UnixTimestamp.createOrFail "test" 1_700_000_000L 123_456_789

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    let private ok (result : Result<'a, 'e>) : 'a =
        match result with
        | Ok value -> value
        | Error error -> failwith $"expected success, got %O{error}"

    let private mkfile (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createFile parent (name n) PermissionBits.defaultForRegularFile buildTime noBytes vfs
        |> ok
        |> snd

    let private mkdir (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.createDirectory parent (name n) PermissionBits.defaultForDirectory buildTime vfs
        |> ok
        |> snd

    let private unbind (parent : InodeNumber) (n : string) (vfs : VirtualFileSystem) : VirtualFileSystem =
        VirtualFileSystem.unbind UnbindTargetEffect.LostALink parent (name n) buildTime vfs
        |> ok
        |> snd

    /// Drive a stream to exhaustion from `cursor`, collecting what it hands
    /// back. The filesystem is fixed, so this is the no-mutation case.
    let private drain
        (directory : InodeNumber)
        (cursor : DirectoryCursor)
        (vfs : VirtualFileSystem)
        : DirectoryStreamName list
        =
        // Capped, and the cap is not paranoia: a cursor that failed to advance
        // strictly — `>=` where the rule is `>` — would hand back one name for
        // ever, and an uncapped drain would hang the suite rather than fail it.
        let rec go (cursor : DirectoryCursor) (fuel : int) (acc : DirectoryStreamName list) =
            if fuel <= 0 then
                failwith $"the stream over %O{directory} did not terminate: %A{List.rev acc |> List.truncate 8} ..."

            match VirtualFileSystem.nextDirectoryEntry directory cursor vfs with
            | None -> List.rev acc
            | Some (entry, _, next) -> go next (fuel - 1) (entry :: acc)

        go cursor 1000 []

    let private strings (entries : DirectoryStreamName list) : string list =
        entries |> List.map (fun e -> e.ToString ())

    /// The root, and a directory `d` beneath it holding the named entries as
    /// regular files.
    let private withEntries (names : string list) : VirtualFileSystem * InodeNumber =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        let directory, vfs =
            VirtualFileSystem.createDirectory root (name "d") PermissionBits.defaultForDirectory buildTime vfs
            |> ok

        let vfs = (vfs, names) ||> List.fold (fun vfs n -> mkfile directory n vfs)
        vfs, directory

    // --------------------------------------------------------- the plain walk

    [<Test>]
    let ``a stream yields the names in the map's order, then dot-dot and dot`` () : unit =
        // The dots are *last*, and that is the whole point of this order: a
        // guest that consumes two entries to skip them, or that expects the
        // first entry to be one, is broken on ext4 and must be broken here.
        let vfs, directory = withEntries [ "m" ; "a" ; "z" ]

        drain directory DirectoryCursor.Start vfs
        |> strings
        |> shouldEqual [ "a" ; "m" ; "z" ; ".." ; "." ]

    [<Test>]
    let ``dot is the directory itself and dot-dot is its physical parent`` () : unit =
        // Reached from `Start` over an empty directory, which is the only cursor
        // position a caller can construct without first walking the names.
        let vfs, directory = withEntries []
        let root = VirtualFileSystem.root vfs

        match VirtualFileSystem.nextDirectoryEntry directory DirectoryCursor.Start vfs with
        | Some (DirectoryStreamName.DotDot, parent, next) ->
            parent |> shouldEqual root

            match VirtualFileSystem.nextDirectoryEntry directory next vfs with
            | Some (DirectoryStreamName.Dot, inode, _) -> inode |> shouldEqual directory
            | other -> failwith $"expected ., got %O{other}"
        | other -> failwith $"expected .., got %O{other}"

    [<Test>]
    let ``the root's dot-dot is the root`` () : unit =
        let vfs = VirtualFileSystem.empty buildTime
        let root = VirtualFileSystem.root vfs

        match VirtualFileSystem.nextDirectoryEntry root DirectoryCursor.Start vfs with
        | Some (DirectoryStreamName.DotDot, parent, _) -> parent |> shouldEqual root
        | other -> failwith $"expected .., got %O{other}"

    [<Test>]
    let ``an empty directory yields exactly dot-dot and dot`` () : unit =
        let vfs, directory = withEntries []

        drain directory DirectoryCursor.Start vfs
        |> strings
        |> shouldEqual [ ".." ; "." ]

    [<Test>]
    let ``a directory entry reports the inode that name binds`` () : unit =
        // So that the handler can derive `d_type` without a second lookup, and
        // so a wrong-inode bug cannot hide behind a right name.
        let vfs, directory = withEntries [ "f" ]

        let expected =
            match VirtualFileSystem.tryGetContent directory vfs with
            | Some (InodeContent.Directory content) -> content.Entries.[name "f"]
            | other -> failwith $"test setup: %O{other}"

        match VirtualFileSystem.nextDirectoryEntry directory DirectoryCursor.Start vfs with
        | Some (DirectoryStreamName.Entry n, inode, _) ->
            n |> shouldEqual (name "f")
            inode |> shouldEqual expected
        | other -> failwith $"expected an entry, got %O{other}"

    // ------------------------------------------------- mutation mid-enumeration

    [<Test>]
    let ``deleting the entry just returned does not skip the next one`` () : unit =
        // The measured behaviour this model exists to reproduce: at 5000
        // entries on both kernels, deleting each name as it is returned skips
        // nothing. A cursor that indexed a position would skip every other name
        // here.
        let vfs, directory = withEntries [ "a" ; "b" ; "c" ]

        let rec go (cursor : DirectoryCursor) (vfs : VirtualFileSystem) (acc : string list) =
            match VirtualFileSystem.nextDirectoryEntry directory cursor vfs with
            | None -> List.rev acc, vfs
            | Some (DirectoryStreamName.Entry n, _, next) ->
                go next (unbind directory (DirectoryEntryName.toString n) vfs) (DirectoryEntryName.toString n :: acc)
            | Some (_, _, next) -> go next vfs acc

        let seen, after = go DirectoryCursor.Start vfs []

        seen |> shouldEqual [ "a" ; "b" ; "c" ]

        // And the directory really is empty, which is what makes CoreLib's
        // `RemoveDirectoryRecursive` succeed rather than answer ENOTEMPTY.
        match VirtualFileSystem.tryGetContent directory after with
        | Some (InodeContent.Directory content) -> content.Entries |> Map.isEmpty |> shouldEqual true
        | other -> failwith $"expected a directory, got %O{other}"

    [<Test>]
    let ``deleting a name the stream has not reached removes it from the listing`` () : unit =
        let vfs, directory = withEntries [ "a" ; "b" ; "c" ]
        let after = unbind directory "c" vfs

        drain directory (DirectoryCursor.After (name "a")) after
        |> strings
        |> shouldEqual [ "b" ; ".." ; "." ]

    [<Test>]
    let ``a name added ahead of the cursor appears and one added behind it does not`` () : unit =
        let vfs, directory = withEntries [ "b" ]
        let after = mkfile directory "a" vfs |> mkfile directory "c"

        drain directory (DirectoryCursor.After (name "b")) after
        |> strings
        |> shouldEqual [ "c" ; ".." ; "." ]

    // ------------------------------------------------------------- the orphan

    [<Test>]
    let ``a stream over a directory rmdir has removed is at end-of-stream at once`` () : unit =
        // Including the dots: probed on both kernels, `opendir` then `rmdir`
        // then `readdir` answers NULL without yielding either. Every cursor
        // position answers the same way, so a stream part-way through stops too.
        let vfs, directory = withEntries []
        let root = VirtualFileSystem.root vfs
        let orphaned = unbind root "d" vfs

        VirtualFileSystem.isOrphanedDirectory directory orphaned |> shouldEqual true

        for cursor in
            [
                DirectoryCursor.Start
                DirectoryCursor.After (name "gone")
                DirectoryCursor.ReturnedDotDot
                DirectoryCursor.ReturnedDot
            ] do
            VirtualFileSystem.nextDirectoryEntry directory cursor orphaned
            |> shouldEqual None

    [<Test>]
    let ``a directory that still has a name yields its dots`` () : unit =
        // The control for the test above: without it, an implementation that
        // returned `None` for every directory would pass.
        let vfs, directory = withEntries []

        VirtualFileSystem.nextDirectoryEntry directory DirectoryCursor.Start vfs
        |> Option.isSome
        |> shouldEqual true

    // ---------------------------------------------------------- the properties

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 300

    /// Names drawn from a small alphabet, so that a generated directory really
    /// does exercise ordering and collisions rather than 300 distinct singletons.
    let private nameGen : Gen<string> =
        Gen.elements [ "a" ; "b" ; "c" ; "d" ; "A" ; "Z" ; "aa" ; "ab" ; "é" ; "中" ]

    [<Test>]
    let ``a full enumeration returns exactly the names the directory binds`` () : unit =
        let property (names : string list) : bool =
            let names = List.distinct names
            let vfs, directory = withEntries names

            let seen =
                drain directory DirectoryCursor.Start vfs
                |> List.choose (fun e ->
                    match e with
                    | DirectoryStreamName.Entry n -> Some (DirectoryEntryName.toString n)
                    | DirectoryStreamName.Dot
                    | DirectoryStreamName.DotDot -> None
                )
                |> List.sort

            seen = List.sort names

        Check.One (config, Prop.forAll (Gen.listOf nameGen |> Arb.fromGen) property)

    [<Test>]
    let ``the names a stream returns strictly increase`` () : unit =
        // What makes the cursor a cursor: were it not monotone, an enumeration
        // could repeat a name for ever and `Directory.GetFiles` would not
        // terminate.
        let property (names : string list) : bool =
            let vfs, directory = withEntries (List.distinct names)

            let seen =
                drain directory DirectoryCursor.Start vfs
                |> List.choose (fun e ->
                    match e with
                    | DirectoryStreamName.Entry n -> Some (DirectoryEntryName.toString n)
                    | DirectoryStreamName.Dot
                    | DirectoryStreamName.DotDot -> None
                )

            seen = List.sort seen && List.length (List.distinct seen) = List.length seen

        Check.One (config, Prop.forAll (Gen.listOf nameGen |> Arb.fromGen) property)

    [<Test>]
    let ``deleting each name as it is returned always empties the directory`` () : unit =
        // The property behind CoreLib's `RemoveDirectoryRecursive`, generalised
        // past the three-name example above.
        let property (names : string list) : bool =
            let vfs, directory = withEntries (List.distinct names)

            let rec go (cursor : DirectoryCursor) (vfs : VirtualFileSystem) =
                match VirtualFileSystem.nextDirectoryEntry directory cursor vfs with
                | None -> vfs
                | Some (DirectoryStreamName.Entry n, _, next) ->
                    go next (unbind directory (DirectoryEntryName.toString n) vfs)
                | Some (_, _, next) -> go next vfs

            match VirtualFileSystem.tryGetContent directory (go DirectoryCursor.Start vfs) with
            | Some (InodeContent.Directory content) -> Map.isEmpty content.Entries
            | _ -> false

        Check.One (config, Prop.forAll (Gen.listOf nameGen |> Arb.fromGen) property)
