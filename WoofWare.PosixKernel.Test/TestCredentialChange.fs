namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `setresuid(2)`, `setresgid(2)`, `setgroups(2)`, `getresuid(2)` and
/// `getresgid(2)`: how a process changes who it is, and reads it back.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/setresid.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, forking a child per row)
/// and on Darwin 27.0 at uid 501. Its output is embedded from beside it, and
/// every row is replayed here.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCredentialChange =

    let private context : string = "TestCredentialChange"

    let private linux : SimulatedUnixPlatform = SimulatedUnixPlatform.linuxX64
    let private darwin : SimulatedUnixPlatform = SimulatedUnixPlatform.macOsArm64

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    let private rows (flavour : string) : string[] list =
        let assembly = Assembly.GetExecutingAssembly ()
        let resourceName = $"WoofWare.PosixKernel.Test.setresid.%s{flavour}.txt"

        use stream =
            match assembly.GetManifestResourceStream resourceName with
            | null -> failwith $"embedded resource %s{resourceName} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.map (fun line -> line.Split '\t')

    let private linuxRows : string[] list = rows "linux"
    let private darwinRows : string[] list = rows "darwin"

    let private section (name : string) (all : string[] list) : string[] list =
        all |> List.filter (fun row -> row.[0] = name)

    /// `"a,b,c"`, each an ID or -1.
    let private triple (text : string) : int64 * int64 * int64 =
        match text.Split ',' |> Array.map int64 with
        | [| a ; b ; c |] -> a, b, c
        | other -> failwith $"not a triple: %s{text} (%A{other})"

    let private requestedUser (raw : int64) : UserId option =
        if raw = -1L then None else Some (uid (uint32 raw))

    let private requestedGroup (raw : int64) : GroupId option =
        if raw = -1L then None else Some (gid (uint32 raw))

    let private imageWith
        (platform : SimulatedUnixPlatform)
        (coreDumps : CoreDumps)
        (credentials : Credentials)
        : UnixBootImage<int, string>
        =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.withCredentials context credentials
        |> UnixBootImage.withCoreDumps coreDumps

    let private systemWith
        (platform : SimulatedUnixPlatform)
        (coreDumps : CoreDumps)
        (credentials : Credentials)
        : UnixSystem<int, string>
        =
        imageWith platform coreDumps credentials |> UnixBootImage.boot

    let private answerText (answer : SyscallAnswer) : string =
        match answer with
        | SyscallAnswer.Completed 0L -> "OK"
        | SyscallAnswer.Completed other -> failwith $"a credential call returned %d{other}, not 0"
        | SyscallAnswer.Failed error -> $"%A{error}"

    let private userTriple (credentials : Credentials) : int64 * int64 * int64 =
        int64 (UserId.toUInt32 credentials.RealUser),
        int64 (UserId.toUInt32 credentials.EffectiveUser),
        int64 (UserId.toUInt32 credentials.SavedUser)

    let private groupTriple (credentials : Credentials) : int64 * int64 * int64 =
        int64 (GroupId.toUInt32 credentials.RealGroup),
        int64 (GroupId.toUInt32 credentials.EffectiveGroup),
        int64 (GroupId.toUInt32 credentials.SavedGroup)

    let private ok<'a, 'e> (result : Result<'a, 'e>) : 'a =
        match result with
        | Ok value -> value
        | Error refusal -> failwith $"refused: %A{refusal}"

    // ------------------------------------------------------------- the run's own facts

    [<Test>]
    let ``the probe ran as root on Linux and as uid 501 on Darwin, with every row present`` () : unit =
        // A truncated or regenerated file would otherwise pass by replaying fewer rows.
        (section "RUN" linuxRows |> List.exactlyOne).[1]
        |> shouldEqual "uid=0 euid=0 gid=0 egid=0 NGROUPS_MAX=65536"

        (section "RUN" darwinRows |> List.exactlyOne).[1]
        |> shouldEqual "uid=501 euid=501 gid=20 egid=20 NGROUPS_MAX=16"

        // The sysctl `SetIdsRefusal.DumpabilityUnmodelled` is about, at the
        // value the dumpable column below was measured under.
        (section "SUID_DUMPABLE" linuxRows |> List.exactlyOne).[1] |> shouldEqual "0"

        section "RESUID" linuxRows |> List.length |> shouldEqual (27 * 125)
        section "RESGID" linuxRows |> List.length |> shouldEqual (3 * 27 * 125)
        section "SETGROUPS" linuxRows |> List.length |> shouldEqual (4 * 19)
        section "SETGROUPS" darwinRows |> List.length |> shouldEqual 19
        section "GETRES" linuxRows |> List.length |> shouldEqual 8
        section "FSOWNER" linuxRows |> List.length |> shouldEqual 1

        for all in [ linuxRows ; darwinRows ] do
            (List.last all).[0] |> shouldEqual "DONE"
            section "SETUP-FAILED" all |> shouldBeEmpty
            section "CHILD-FAILED" all |> shouldBeEmpty

    // ------------------------------------------------------------- setresuid

    /// One `RESUID` row: the starting user triple, the request, the errno, the
    /// triple afterwards, the filesystem uid, the dumpable flag, and whether the
    /// process could then open a root-owned 0600 file.
    let private resuidRows : (string[] * (int64 * int64 * int64) * (int64 * int64 * int64)) list =
        section "RESUID" linuxRows
        |> List.map (fun row -> row, triple row.[1], triple row.[2])

    let private startingUsers ((r, e, s) : int64 * int64 * int64) : Credentials =
        { Credentials.ofIds (uid (uint32 e)) (gid 1000u) [] with
            RealUser = uid (uint32 r)
            SavedUser = uid (uint32 s)
        }

    [<Test>]
    let ``setresuid answers every measured row, and leaves the process as the probe found it`` () : unit =
        for row, start, (tr, te, ts) in resuidRows do
            let before = systemWith linux CoreDumps.Suppressed (startingUsers start)

            let answer, after =
                UnixCredentials.setresuid (requestedUser tr) (requestedUser te) (requestedUser ts) before
                |> ok

            let label = String.Join ("\t", row)
            let expectedAfter = triple row.[4]

            if answerText answer <> row.[3] then
                failwith $"%s{label}: answered %s{answerText answer}"

            match
                UnixCredentials.getresuid UserBuffer.Mapped UserBuffer.Mapped UserBuffer.Mapped after
                |> ok
            with
            | GetIdsAnswer.Copied (r, e, s) ->
                if
                    (int64 (UserId.toUInt32 r), int64 (UserId.toUInt32 e), int64 (UserId.toUInt32 s))
                    <> expectedAfter
                then
                    failwith $"%s{label}: getresuid reported %O{r},%O{e},%O{s}"
            | other -> failwith $"%s{label}: getresuid answered %A{other}"

            // The kernel's filesystem uid followed the effective uid in every row,
            // which is why this model keeps no filesystem uid of its own.
            let _, measuredEffective, _ = expectedAfter
            row.[5] |> shouldEqual $"fsuid=%d{measuredEffective}"

            let privileged =
                match UnixProcessState.callerPrivilege after.Process with
                | CallerPrivilege.Privileged -> "yes"
                | CallerPrivilege.Unprivileged -> "no"

            if row.[7] <> $"privileged=%s{privileged}" then
                failwith $"%s{label}: the model's privilege is %s{privileged}"

            // Nothing but the user IDs moves.
            groupTriple after.Process.Credentials
            |> shouldEqual (groupTriple before.Process.Credentials)

            { after with
                Process =
                    { after.Process with
                        Credentials = before.Process.Credentials
                    }
            }
            |> shouldEqual before

    [<Test>]
    let ``setresuid refuses exactly the rows that cleared the dumpable flag, for a process that writes core dumps`` () =
        for row, start, (tr, te, ts) in resuidRows do
            let label = String.Join ("\t", row)

            let call (coreDumps : CoreDumps) =
                systemWith linux coreDumps (startingUsers start)
                |> UnixCredentials.setresuid (requestedUser tr) (requestedUser te) (requestedUser ts)

            match row.[6], call CoreDumps.Written with
            | "dumpable=1->0", Error SetIdsRefusal.DumpabilityUnmodelled -> ()
            | "dumpable=1->1", Ok (answer, after) ->
                let suppressedAnswer, _ = call CoreDumps.Suppressed |> ok
                answer |> shouldEqual suppressedAnswer
                after.Process.CoreDumps |> shouldEqual CoreDumps.Written
            | measured, outcome -> failwith $"%s{label}: measured %s{measured}, the model gave %A{outcome}"

            // A process that writes no core dump goes on writing none, whatever
            // `fs.suid_dumpable` says: a cleared flag only ever removes a dump.
            match call CoreDumps.Suppressed with
            | Ok (_, after) -> after.Process.CoreDumps |> shouldEqual CoreDumps.Suppressed
            | Error refusal -> failwith $"%s{label}: refused a process that writes no core dumps: %A{refusal}"

    // ------------------------------------------------------------- setresgid

    let private resgidRows
        : (string[] * (int64 * int64 * int64) * (int64 * int64 * int64) * (int64 * int64 * int64)) list =
        section "RESGID" linuxRows
        |> List.map (fun row -> row, triple row.[1], triple row.[2], triple row.[3])

    let private startingIds ((ur, ue, us) : int64 * int64 * int64) ((r, e, s) : int64 * int64 * int64) : Credentials =
        {
            RealUser = uid (uint32 ur)
            EffectiveUser = uid (uint32 ue)
            SavedUser = uid (uint32 us)
            RealGroup = gid (uint32 r)
            EffectiveGroup = gid (uint32 e)
            SavedGroup = gid (uint32 s)
            SupplementaryGroups = [ gid 1002u ]
        }

    [<Test>]
    let ``setresgid answers every measured row, and leaves the process as the probe found it`` () : unit =
        for row, users, start, (tr, te, ts) in resgidRows do
            let before = systemWith linux CoreDumps.Suppressed (startingIds users start)

            let answer, after =
                UnixCredentials.setresgid (requestedGroup tr) (requestedGroup te) (requestedGroup ts) before
                |> ok

            let label = String.Join ("\t", row)
            let expectedAfter = triple row.[5]

            if answerText answer <> row.[4] then
                failwith $"%s{label}: answered %s{answerText answer}"

            match
                UnixCredentials.getresgid UserBuffer.Mapped UserBuffer.Mapped UserBuffer.Mapped after
                |> ok
            with
            | GetIdsAnswer.Copied (r, e, s) ->
                if
                    (int64 (GroupId.toUInt32 r), int64 (GroupId.toUInt32 e), int64 (GroupId.toUInt32 s))
                    <> expectedAfter
                then
                    failwith $"%s{label}: getresgid reported %O{r},%O{e},%O{s}"
            | other -> failwith $"%s{label}: getresgid answered %A{other}"

            let _, measuredEffective, _ = expectedAfter
            row.[6] |> shouldEqual $"fsgid=%d{measuredEffective}"

            { after with
                Process =
                    { after.Process with
                        Credentials =
                            { after.Process.Credentials with
                                RealGroup = before.Process.Credentials.RealGroup
                                EffectiveGroup = before.Process.Credentials.EffectiveGroup
                                SavedGroup = before.Process.Credentials.SavedGroup
                            }
                    }
            }
            |> shouldEqual before

    [<Test>]
    let ``setresgid refuses exactly the rows that cleared the dumpable flag, for a process that writes core dumps`` () =
        for row, users, start, (tr, te, ts) in resgidRows do
            let label = String.Join ("\t", row)

            let call (coreDumps : CoreDumps) =
                systemWith linux coreDumps (startingIds users start)
                |> UnixCredentials.setresgid (requestedGroup tr) (requestedGroup te) (requestedGroup ts)

            match row.[7], call CoreDumps.Written with
            | "dumpable=1->0", Error SetIdsRefusal.DumpabilityUnmodelled -> ()
            | "dumpable=1->1", Ok (answer, after) ->
                let suppressedAnswer, _ = call CoreDumps.Suppressed |> ok
                answer |> shouldEqual suppressedAnswer
                after.Process.CoreDumps |> shouldEqual CoreDumps.Written
            | measured, outcome -> failwith $"%s{label}: measured %s{measured}, the model gave %A{outcome}"

            match call CoreDumps.Suppressed with
            | Ok (_, after) -> after.Process.CoreDumps |> shouldEqual CoreDumps.Suppressed
            | Error refusal -> failwith $"%s{label}: refused a process that writes no core dumps: %A{refusal}"

    // ------------------------------------------------------------- setgroups

    /// The size and words a probe row's label names, given the platform's
    /// `NGROUPS_MAX`.
    let private groupsArgument (limit : int) (label : string) : int * GroupListWords =
        let distinct (count : int) =
            List.init count (fun i -> 3000u + uint32 i)

        match label with
        | "size=0 buffer=NULL" -> 0, GroupListWords.FaultsAfter []
        | "size=0 buffer=[]" -> 0, GroupListWords.Readable []
        | "size=1 buffer=[1000]" -> 1, GroupListWords.Readable [ 1000u ]
        | "size=3 buffer=[30,10,20]" -> 3, GroupListWords.Readable [ 30u ; 10u ; 20u ]
        | "size=4 buffer=[7,7,3,7]" -> 4, GroupListWords.Readable [ 7u ; 7u ; 3u ; 7u ]
        | "size=2 buffer=[1000,-1]" -> 2, GroupListWords.Readable [ 1000u ; UInt32.MaxValue ]
        | "size=2 buffer=[-1,1000]" -> 2, GroupListWords.Readable [ UInt32.MaxValue ; 1000u ]
        | "size=3 buffer=[1000,1001|fault]" -> 3, GroupListWords.FaultsAfter [ 1000u ; 1001u ]
        | "size=3 buffer=[1000,-1|fault]" -> 3, GroupListWords.FaultsAfter [ 1000u ; UInt32.MaxValue ]
        | "size=1 buffer=NULL" -> 1, GroupListWords.FaultsAfter []
        | "size=2 buffer=NULL" -> 2, GroupListWords.FaultsAfter []
        // A buffer of negative or oversized length is never copied, so the
        // words it held do not matter; see `GroupListWords.FaultsAfter`.
        | "size=-1 buffer=NULL"
        | "size=-1 buffer=[1000]" -> -1, GroupListWords.FaultsAfter []
        | "size=INT_MIN buffer=NULL" -> Int32.MinValue, GroupListWords.FaultsAfter []
        | "size=INT_MAX buffer=NULL" -> Int32.MaxValue, GroupListWords.FaultsAfter []
        | "size=limit buffer=distinct" -> limit, GroupListWords.Readable (distinct limit)
        | "size=limit buffer=NULL" -> limit, GroupListWords.FaultsAfter []
        | "size=limit+1 buffer=distinct" -> limit + 1, GroupListWords.Readable (distinct (limit + 1))
        | "size=limit+1 buffer=NULL" -> limit + 1, GroupListWords.FaultsAfter []
        | other -> failwith $"unrecognised setgroups row: %s{other}"

    /// The list as the probe printed it: every group for a short list, and
    /// the count with the first and last for a long one.
    let private groupsText (groups : GroupId list) : string =
        let raw = groups |> List.map GroupId.toUInt32

        if List.length raw > 8 then
            $"[%d{List.length raw} groups: %d{List.head raw}..%d{List.last raw}]"
        else
            raw |> List.map string<uint32> |> String.concat "," |> sprintf "[%s]"

    [<Test>]
    let ``Linux setgroups answers every measured row`` () : unit =
        let limit = SimulatedUnixPlatform.supplementaryGroupLimit linux

        for row in section "SETGROUPS" linuxRows do
            let label = String.Join ("\t", row)
            let ur, ue, us = triple row.[1]

            let before =
                { Credentials.ofIds (uid (uint32 ue)) (gid 1000u) [ gid 2000u ] with
                    RealUser = uid (uint32 ur)
                    SavedUser = uid (uint32 us)
                }
                |> systemWith linux CoreDumps.Written

            let size, words = groupsArgument limit row.[2]
            let answer, after = UnixCredentials.setgroups size words before |> ok

            if answerText answer <> row.[3] then
                failwith $"%s{label}: answered %s{answerText answer}"

            let reported =
                Credentials.reportedGroups (SimulatedUnixPlatform.groupListReport linux) after.Process.Credentials
                |> Option.get

            if $"after=%s{groupsText reported}" <> row.[5] then
                failwith $"%s{label}: the model's list is %s{groupsText reported}"

            // setgroups clears no dumpable flag, so a process that writes core
            // dumps goes on writing them, and is not refused.
            row.[7] |> shouldEqual "dumpable=1->1"

            { after with
                Process =
                    { after.Process with
                        Credentials =
                            { after.Process.Credentials with
                                SupplementaryGroups = before.Process.Credentials.SupplementaryGroups
                            }
                    }
            }
            |> shouldEqual before

    [<Test>]
    let ``Darwin setgroups answers every measured row for an unprivileged process`` () : unit =
        let limit = SimulatedUnixPlatform.supplementaryGroupLimit darwin

        for row in section "SETGROUPS" darwinRows do
            let label = String.Join ("\t", row)

            let before =
                systemWith darwin CoreDumps.Suppressed (Credentials.ofIds (uid 501u) (gid 20u) [ gid 12u ])

            let size, words = groupsArgument limit row.[2]
            let answer, after = UnixCredentials.setgroups size words before |> ok

            if answerText answer <> row.[3] then
                failwith $"%s{label}: answered %s{answerText answer}"

            // Every row failed, so nothing moves.
            row.[4] |> shouldEqual (row.[5].Replace ("after=", "before="))
            after |> shouldEqual before

    [<Test>]
    let ``Darwin setgroups is refused for a privileged process, whose answer needs root to measure`` () : unit =
        let root =
            systemWith darwin CoreDumps.Suppressed (Credentials.ofIds UserId.root (gid 0u) [])

        let property (size : int, words : uint32 list) : unit =
            UnixCredentials.setgroups size (GroupListWords.Readable words) root
            |> Result.map fst
            |> shouldEqual (Error (SetGroupsRefusal.UnmeasuredPrivileged SimulatedUnixFlavour.Darwin))

        // Any size at all, since the refusal comes before the size is read; the
        // words agree with the size whenever a kernel would copy them.
        let gen =
            gen {
                let! words = Gen.listOf CredentialsGen.rawId
                let! size = Gen.oneof [ Gen.constant (List.length words) ; ArbMap.defaults |> ArbMap.generate<int> ]

                return
                    if size >= 0 && size <= 16 then
                        List.length words, words
                    else
                        size, words
            }

        Check.One (Config.QuickThrowOnFailure, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a list whose words disagree with its size is the caller's mistake, on both flavours`` () : unit =
        let root = Credentials.ofIds UserId.root (gid 0u) []

        for platform, credentials in [ linux, root ; darwin, Credentials.ofIds (uid 501u) (gid 20u) [] ] do
            let system = systemWith platform CoreDumps.Suppressed credentials

            for words in
                [
                    GroupListWords.Readable [ 1u ]
                    GroupListWords.Readable [ 1u ; 2u ; 3u ]
                    GroupListWords.FaultsAfter [ 1u ; 2u ]
                ] do
                Assert.Throws<Exception> (fun () ->
                    UnixCredentials.setgroups 2 words system
                    |> ignore<Result<SyscallAnswer * UnixSystem<int, string>, SetGroupsRefusal>>
                )
                |> ignore<Exception>

    // ------------------------------------------------------------- getresuid, getresgid

    [<Test>]
    let ``getresuid and getresgid write each ID in turn, and stop at the first unwritable buffer`` () : unit =
        let credentials =
            {
                RealUser = uid 20u
                EffectiveUser = uid 21u
                SavedUser = uid 22u
                RealGroup = gid 10u
                EffectiveGroup = gid 11u
                SavedGroup = gid 12u
                SupplementaryGroups = []
            }

        let system = systemWith linux CoreDumps.Suppressed credentials

        let buffers (nullAt : string) : UserBuffer * UserBuffer * UserBuffer =
            let at (name : string) =
                if name = nullAt then
                    UserBuffer.Unmapped 0UL
                else
                    UserBuffer.Mapped

            at "real", at "effective", at "saved"

        // What each of the three buffers holds afterwards, as the probe printed it.
        let written (nullAt : string) (answer : GetIdsAnswer<uint32>) : string =
            let values =
                match answer with
                | GetIdsAnswer.Copied (r, e, s) -> [ r ; e ; s ]
                | GetIdsAnswer.Faulted written -> written

            [ "real" ; "effective" ; "saved" ]
            |> List.mapi (fun i name ->
                if name = nullAt then "null"
                elif i < List.length values then string<uint32> values.[i]
                else "unwritten"
            )
            |> String.concat ","

        let errno (answer : GetIdsAnswer<uint32>) : string =
            match answer with
            | GetIdsAnswer.Copied _ -> "OK"
            | GetIdsAnswer.Faulted _ -> "EFAULT"

        let rowsSeen =
            section "GETRES" linuxRows
            |> List.map (fun row ->
                let nullAt = row.[2].Substring "null=".Length
                let real, effective, saved = buffers nullAt

                let answer =
                    match row.[1] with
                    | "uid" ->
                        match UnixCredentials.getresuid real effective saved system |> ok with
                        | GetIdsAnswer.Copied (r, e, s) ->
                            GetIdsAnswer.Copied (UserId.toUInt32 r, UserId.toUInt32 e, UserId.toUInt32 s)
                        | GetIdsAnswer.Faulted written -> GetIdsAnswer.Faulted (written |> List.map UserId.toUInt32)
                    | "gid" ->
                        match UnixCredentials.getresgid real effective saved system |> ok with
                        | GetIdsAnswer.Copied (r, e, s) ->
                            GetIdsAnswer.Copied (GroupId.toUInt32 r, GroupId.toUInt32 e, GroupId.toUInt32 s)
                        | GetIdsAnswer.Faulted written -> GetIdsAnswer.Faulted (written |> List.map GroupId.toUInt32)
                    | other -> failwith $"unrecognised GETRES row: %s{other}"

                (errno answer, written nullAt answer) |> shouldEqual (row.[3], row.[4])
                row.[1]
            )

        rowsSeen
        |> shouldEqual [ "uid" ; "uid" ; "uid" ; "uid" ; "gid" ; "gid" ; "gid" ; "gid" ]

    [<Test>]
    let ``a buffer whose bytes have no answer is refused at the write that reaches it, and not before`` () : unit =
        let system =
            systemWith linux CoreDumps.Suppressed (Credentials.ofIds (uid 7u) (gid 8u) [])

        UnixCredentials.getresuid UserBuffer.Mapped UserBuffer.Opaque (UserBuffer.Unmapped 0UL) system
        |> shouldEqual (Error (GetIdsRefusal.Buffer BufferRefusal.OpaqueAtTransfer))

        UnixCredentials.getresgid UserBuffer.Mapped UserBuffer.Addressless UserBuffer.Mapped system
        |> shouldEqual (Error (GetIdsRefusal.Buffer BufferRefusal.AddresslessAtTransfer))

        // An earlier fault ends the call before it reaches the buffer it cannot answer for.
        UnixCredentials.getresuid (UserBuffer.Unmapped 0UL) UserBuffer.Opaque UserBuffer.Addressless system
        |> shouldEqual (Ok (GetIdsAnswer.Faulted []))

    [<Test>]
    let ``Darwin has none of setresuid, setresgid, getresuid and getresgid`` () : unit =
        let property (credentials : Credentials, requested : uint32 option list) : unit =
            // Darwin admits only credentials whose three IDs agree.
            let credentials =
                Credentials.ofIds credentials.EffectiveUser credentials.EffectiveGroup []

            let system = systemWith darwin CoreDumps.Suppressed credentials
            let users = requested |> List.map (Option.map uid)
            let groups = requested |> List.map (Option.map gid)
            let noSuchCall = SimulatedUnixFlavour.Darwin

            UnixCredentials.setresuid users.[0] users.[1] users.[2] system
            |> Result.map fst
            |> shouldEqual (Error (SetIdsRefusal.NoSuchCall noSuchCall))

            UnixCredentials.setresgid groups.[0] groups.[1] groups.[2] system
            |> Result.map fst
            |> shouldEqual (Error (SetIdsRefusal.NoSuchCall noSuchCall))

            UnixCredentials.getresuid UserBuffer.Mapped UserBuffer.Mapped UserBuffer.Mapped system
            |> shouldEqual (Error (GetIdsRefusal.NoSuchCall noSuchCall))

            UnixCredentials.getresgid UserBuffer.Mapped UserBuffer.Mapped UserBuffer.Mapped system
            |> shouldEqual (Error (GetIdsRefusal.NoSuchCall noSuchCall))

        let gen =
            Gen.zip CredentialsGen.credentials (Gen.listOfLength 3 (Gen.optionOf CredentialsGen.rawId))

        Check.One (Config.QuickThrowOnFailure, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------- the filesystem's view

    [<Test>]
    let ``a file a process creates after changing its IDs is owned by its new effective user and group`` () : unit =
        let row = section "FSOWNER" linuxRows |> List.exactlyOne
        row.[1] |> shouldEqual "credentials uid=1003,1004,0 gid=1000,1001,1002"

        let seed =
            Map.ofList
                [
                    DirectoryEntryName.parseOrFail context "tmp",
                    SeedEntry.Directory (Map.empty, PermissionBits.parseOrFail context 0o1777, None)
                ]

        let system =
            match
                imageWith linux CoreDumps.Suppressed (Credentials.ofIds UserId.root (gid 0u) [])
                |> UnixBootImage.withFileSystemAndCurrentDirectory
                    UnixTimestamp.epoch
                    (InodeOwner.ofProcess (Credentials.ofIds UserId.root (gid 0u) []))
                    seed
                    AbsoluteUnixPath.root
            with
            | Ok image -> UnixBootImage.boot image
            | Error fault -> failwith $"%A{fault}"

        let _, system =
            UnixCredentials.setresgid (Some (gid 1000u)) (Some (gid 1001u)) (Some (gid 1002u)) system
            |> ok

        let _, system =
            UnixCredentials.setresuid (Some (uid 1003u)) (Some (uid 1004u)) (Some UserId.root) system
            |> ok

        let creating : OpenFlags =
            {
                Access = FileAccessMode.WriteOnly
                Create = true
                Exclusive = true
                Truncate = false
                NoFollow = false
                CloseOnExec = false
                Synchronous = false
                Directory = false
            }

        let system =
            match OpenFlagWords.openPath creating (PathArg.ofText "/tmp/owned") 0o600 system with
            | Ok (SyscallAnswer.Completed _, system) -> system
            | other -> failwith $"open: %A{other}"

        match UnixPathResolution.stat SymlinkPolicy.Follow (PathArg.ofText "/tmp/owned") system with
        | Ok (FileStatusAnswer.Reported status) -> $"owner=%O{status.UserId}:%O{status.GroupId}" |> shouldEqual row.[2]
        | other -> failwith $"stat: %A{other}"

    // ------------------------------------------------------------- properties

    let private linuxCredentialsGen : Gen<Credentials> =
        // Small IDs, so that requests often name an ID the process holds.
        let small = Gen.elements [ 0u ; 1u ; 2u ; 1000u ]

        gen {
            let! ids = Gen.listOfLength 6 small
            let! groups = Gen.listOf small

            return
                {
                    RealUser = uid ids.[0]
                    EffectiveUser = uid ids.[1]
                    SavedUser = uid ids.[2]
                    RealGroup = gid ids.[3]
                    EffectiveGroup = gid ids.[4]
                    SavedGroup = gid ids.[5]
                    SupplementaryGroups = groups |> List.map gid
                }
        }

    let private requestGen : Gen<uint32 option> =
        Gen.optionOf (Gen.oneof [ Gen.elements [ 0u ; 1u ; 2u ; 1000u ] ; CredentialsGen.rawId ])

    [<Test>]
    let ``an unprivileged process can only rearrange the IDs it already holds`` () : unit =
        let property (credentials : Credentials, requests : uint32 option list) : unit =
            let system = systemWith linux CoreDumps.Suppressed credentials

            match UnixProcessState.callerPrivilege system.Process with
            | CallerPrivilege.Privileged -> ()
            | CallerPrivilege.Unprivileged ->

            let heldUsers =
                Set.ofList [ credentials.RealUser ; credentials.EffectiveUser ; credentials.SavedUser ]

            let heldGroups =
                Set.ofList [ credentials.RealGroup ; credentials.EffectiveGroup ; credentials.SavedGroup ]

            let _, afterUsers =
                UnixCredentials.setresuid
                    (Option.map uid requests.[0])
                    (Option.map uid requests.[1])
                    (Option.map uid requests.[2])
                    system
                |> ok

            let after = afterUsers.Process.Credentials

            Set.isSubset (Set.ofList [ after.RealUser ; after.EffectiveUser ; after.SavedUser ]) heldUsers
            |> shouldEqual true

            let _, afterGroups =
                UnixCredentials.setresgid
                    (Option.map gid requests.[0])
                    (Option.map gid requests.[1])
                    (Option.map gid requests.[2])
                    system
                |> ok

            let after = afterGroups.Process.Credentials

            Set.isSubset (Set.ofList [ after.RealGroup ; after.EffectiveGroup ; after.SavedGroup ]) heldGroups
            |> shouldEqual true

            // And it can never install a supplementary list.
            UnixCredentials.setgroups 0 (GroupListWords.Readable []) system
            |> ok
            |> fst
            |> shouldEqual (SyscallAnswer.Failed UnixError.EPERM)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 2000,
            Prop.forAll (Arb.fromGen (Gen.zip linuxCredentialsGen (Gen.listOfLength 3 requestGen))) property
        )

    [<Test>]
    let ``a sequence of credential calls keeps the system sound, through step as through the primitives`` () : unit =
        let callGen : Gen<Syscall> =
            Gen.oneof
                [
                    Gen.listOfLength 3 requestGen
                    |> Gen.map (fun r ->
                        Syscall.SetResUid (Option.map uid r.[0], Option.map uid r.[1], Option.map uid r.[2])
                    )
                    Gen.listOfLength 3 requestGen
                    |> Gen.map (fun r ->
                        Syscall.SetResGid (Option.map gid r.[0], Option.map gid r.[1], Option.map gid r.[2])
                    )
                    gen {
                        let! words =
                            Gen.listOf (Gen.oneof [ Gen.elements [ 0u ; 1u ; UInt32.MaxValue ] ; CredentialsGen.rawId ])

                        let! faults = Gen.elements [ true ; false ]

                        return
                            if faults then
                                Syscall.SetGroups (List.length words + 1, GroupListWords.FaultsAfter words)
                            else
                                Syscall.SetGroups (List.length words, GroupListWords.Readable words)
                    }
                ]

        let primitive (call : Syscall) (system : UnixSystem<int, string>) =
            match call with
            | Syscall.SetResUid (r, e, s) ->
                UnixCredentials.setresuid r e s system |> Result.mapError SyscallRefusal.SetIds
            | Syscall.SetResGid (r, e, s) ->
                UnixCredentials.setresgid r e s system |> Result.mapError SyscallRefusal.SetIds
            | Syscall.SetGroups (size, words) ->
                UnixCredentials.setgroups size words system
                |> Result.mapError SyscallRefusal.SetGroups
            | other -> failwith $"not a credential call: %A{other}"

        let property (credentials : Credentials, calls : Syscall list) : unit =
            let mutable system = systemWith linux CoreDumps.Suppressed credentials

            for call in calls do
                let viaStep = UnixSystem.step 0 call system
                let direct = primitive call system

                match viaStep, direct with
                | Ok (SyscallOutcome.Answered stepAnswer, stepped), Ok (answer, after) ->
                    (stepAnswer, stepped) |> shouldEqual (answer, after)
                    UnixSystem.checkInvariants after |> shouldEqual []
                    system <- after
                | Error stepRefusal, Error refusal -> stepRefusal |> shouldEqual refusal
                | other -> failwith $"%A{call}: step and the primitive disagree: %A{other}"

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll (Arb.fromGen (Gen.zip linuxCredentialsGen (Gen.listOf callGen))) property
        )
