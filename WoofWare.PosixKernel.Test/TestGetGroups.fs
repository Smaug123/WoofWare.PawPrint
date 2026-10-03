namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `getegid(2)` and `getgroups(2)`: which groups a process reports, and how
/// `getgroups` screens its size and its buffer.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/getgroups.c`,
/// run on Linux 6.18.5 (aarch64, root in the container, forking a child per
/// list) and on Darwin 27.0 at uid 501; its output is beside it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGetGroups =

    let private context : string = "TestGetGroups"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw
    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    let private systemWith (platform : SimulatedUnixPlatform) (credentials : Credentials) : UnixSystem<int, string> =
        UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
        |> UnixBootImage.withCredentials context credentials
        |> UnixBootImage.boot

    let private linuxWithGroups (groups : uint32 list) : UnixSystem<int, string> =
        Credentials.ofIds (uid 1000u) (gid 1000u) (groups |> List.map gid)
        |> systemWith SimulatedUnixPlatform.linuxX64

    let private copied (groups : uint32 list) : Result<GetGroupsAnswer, GetGroupsRefusal> =
        Ok (GetGroupsAnswer.Copied (groups |> List.map gid))

    let private failed (error : UnixError) : Result<GetGroupsAnswer, GetGroupsRefusal> =
        Ok (GetGroupsAnswer.Failed error)

    // ------------------------------------------------------------------ getegid

    [<Test>]
    let ``getegid reports the effective group, through step as through the primitive`` () : unit =
        // Real, effective and saved all differ, so a primitive reading the wrong
        // one fails; Linux admits such credentials.
        let credentials =
            { Credentials.ofIds (uid 1000u) (gid 7u) [ gid 9u ] with
                EffectiveGroup = gid 8u
                SavedGroup = gid 10u
            }

        let linux = systemWith SimulatedUnixPlatform.linuxX64 credentials
        // Darwin refuses credentials whose IDs differ, so its row is away from
        // the default group instead.
        let darwin =
            systemWith SimulatedUnixPlatform.macOsArm64 (Credentials.ofIds (uid 501u) (gid 12u) [])

        for system, expected in [ linux, 8u ; darwin, 12u ] do
            UnixDescriptor.effectiveGroupId system |> shouldEqual (gid expected)

            match UnixSystem.step 0 Syscall.GetEffectiveGroupId system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed answer), after) ->
                answer |> shouldEqual (int64 expected)
                after |> shouldEqual system
            | other -> failwith $"unexpected: %A{other}"

    // ------------------------------------------------------------------ Linux

    /// Every destination a caller can hand the kernel, as the kernel sees it.
    let private destinationGen : Gen<UserBuffer> =
        Gen.oneof
            [
                Gen.constant UserBuffer.Mapped
                Gen.constant UserBuffer.Opaque
                Gen.constant UserBuffer.Addressless
                Gen.elements [ 0UL ; 8UL ; UInt64.MaxValue ] |> Gen.map UserBuffer.Unmapped
            ]

    /// A size around `count`, where every screen changes its answer, and
    /// otherwise anywhere at all.
    let private sizeAround (count : int) : Gen<int> =
        Gen.oneof
            [
                Gen.elements
                    [
                        Int32.MinValue
                        -2
                        -1
                        0
                        count - 1
                        count
                        count + 1
                        65536
                        Int32.MaxValue
                    ]
                ArbMap.defaults |> ArbMap.generate<int>
            ]

    /// Supplementary groups with duplicates and IDs past 2^31 weighted in.
    let private groupsGen : Gen<GroupId list> =
        gen {
            let! count = Gen.choose (0, 40)
            let! pool = Gen.listOfLength 4 CredentialsGen.groupId
            let fromPool = Gen.elements pool
            return! Gen.listOfLength count (Gen.oneof [ fromPool ; CredentialsGen.groupId ])
        }

    [<Test>]
    let ``Linux getgroups answers as its measured screens say, over every size and destination`` () : unit =
        let property (credentials : Credentials, size : int, destination : UserBuffer) : unit =
            let credentials =
                // Linux credentials may hold any IDs; only the list is under test.
                credentials

            let system = systemWith SimulatedUnixPlatform.linuxX64 credentials
            let count = List.length credentials.SupplementaryGroups
            let actual = UnixDescriptor.getgroups destination size system

            // The screens, in the order the probe found them: the sign of the
            // size, then a size of 0 asking only for the count, then a size too
            // small for the list, and only then the copy, which an empty list
            // does not make.
            if size < 0 then
                actual |> shouldEqual (failed UnixError.EINVAL)
            elif size = 0 then
                actual |> shouldEqual (Ok (GetGroupsAnswer.Counted count))
            elif size < count then
                actual |> shouldEqual (failed UnixError.EINVAL)
            elif count = 0 then
                actual |> shouldEqual (Ok (GetGroupsAnswer.Copied []))
            else
                match destination, actual with
                | UserBuffer.Mapped, Ok (GetGroupsAnswer.Copied reported) ->
                    // A permutation of the supplementary groups...
                    List.sortBy GroupId.toUInt32 reported
                    |> shouldEqual (List.sortBy GroupId.toUInt32 credentials.SupplementaryGroups)

                    // ...in ascending unsigned order, which with the line above
                    // pins the list exactly.
                    reported
                    |> List.pairwise
                    |> List.iter (fun (a, b) -> GroupId.toUInt32 a <= GroupId.toUInt32 b |> shouldEqual true)
                | UserBuffer.Unmapped _, _ -> actual |> shouldEqual (failed UnixError.EFAULT)
                | UserBuffer.Opaque, _ ->
                    actual
                    |> shouldEqual (Error (GetGroupsRefusal.Buffer BufferRefusal.OpaqueAtTransfer))
                | UserBuffer.Addressless, _ ->
                    actual
                    |> shouldEqual (Error (GetGroupsRefusal.Buffer BufferRefusal.AddresslessAtTransfer))
                | UserBuffer.Mapped, other -> failwith $"a mapped buffer of room %d{size} got %A{other}"

        let gen =
            gen {
                let! credentials = CredentialsGen.credentialsWithAtMost 0
                let! groups = groupsGen

                let credentials =
                    { credentials with
                        SupplementaryGroups = groups
                    }

                let! size = sizeAround (List.length groups)
                let! destination = destinationGen
                return credentials, size, destination
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``Linux getgroups reports each measured list as the probe saw it`` () : unit =
        let rows : (uint32 list * uint32 list) list =
            [
                [], []
                [ 5u ], [ 5u ]
                [ 30u ; 10u ; 20u ], [ 10u ; 20u ; 30u ]
                [ 7u ; 7u ; 3u ; 7u ], [ 3u ; 7u ; 7u ; 7u ]
                // The effective group is 1000 here, and is reported once per
                // time the list holds it and never added.
                [ 1000u ; 5u ; 1000u ], [ 5u ; 1000u ; 1000u ]
                List.rev [ 1u .. 20u ], [ 1u .. 20u ]
                [ 4294967294u ; 0u ; 2147483648u ; 65536u ], [ 0u ; 65536u ; 2147483648u ; 4294967294u ]
            ]

        for given, reported in rows do
            UnixDescriptor.getgroups UserBuffer.Mapped 256 (linuxWithGroups given)
            |> shouldEqual (copied reported)

        // An effective group that is not in the list is not reported.
        Credentials.ofIds (uid 1000u) (gid 4242u) ([ 30u ; 10u ; 20u ] |> List.map gid)
        |> systemWith SimulatedUnixPlatform.linuxX64
        |> UnixDescriptor.getgroups UserBuffer.Mapped 3
        |> shouldEqual (copied [ 10u ; 20u ; 30u ])

    [<Test>]
    let ``Linux getgroups screens as the probe measured`` () : unit =
        let unsorted = linuxWithGroups [ 30u ; 10u ; 20u ]
        let nothing = UserBuffer.Unmapped 0UL

        let rows : (int * UserBuffer * Result<GetGroupsAnswer, GetGroupsRefusal>) list =
            [
                0, UserBuffer.Mapped, Ok (GetGroupsAnswer.Counted 3)
                0, nothing, Ok (GetGroupsAnswer.Counted 3)
                -1, UserBuffer.Mapped, failed UnixError.EINVAL
                Int32.MinValue, UserBuffer.Mapped, failed UnixError.EINVAL
                -1, nothing, failed UnixError.EINVAL
                2, UserBuffer.Mapped, failed UnixError.EINVAL
                2, nothing, failed UnixError.EINVAL
                3, UserBuffer.Mapped, copied [ 10u ; 20u ; 30u ]
                4, UserBuffer.Mapped, copied [ 10u ; 20u ; 30u ]
                256, UserBuffer.Mapped, copied [ 10u ; 20u ; 30u ]
                3, nothing, failed UnixError.EFAULT
                4, nothing, failed UnixError.EFAULT
            ]

        for size, destination, expected in rows do
            (size, destination, UnixDescriptor.getgroups destination size unsorted)
            |> shouldEqual (size, destination, expected)

        // With no groups at all there is nothing to copy, so no buffer faults.
        let empty = linuxWithGroups []

        for size, expected in
            [
                0, Ok (GetGroupsAnswer.Counted 0)
                1, copied []
                -1, failed UnixError.EINVAL
            ] do
            (size, UnixDescriptor.getgroups nothing size empty)
            |> shouldEqual (size, expected)

    // ------------------------------------------------------------------ Darwin

    [<Test>]
    let ``Darwin getgroups answers a negative size and refuses every other call`` () : unit =
        let property (groups : GroupId list, size : int, destination : UserBuffer) : unit =
            let system =
                Credentials.ofIds (uid 501u) (gid 20u) groups
                |> systemWith SimulatedUnixPlatform.macOsArm64

            let actual = UnixDescriptor.getgroups destination size system

            if size < 0 then
                // Measured at -1, -2, -16, -65536, INT_MIN + 1 and INT_MIN,
                // with the process's own list; no list can make a negative
                // size acceptable.
                actual |> shouldEqual (failed UnixError.EINVAL)
            else
                actual
                |> shouldEqual (Error (GetGroupsRefusal.UnmeasuredGroupList SimulatedUnixFlavour.Darwin))

        let gen =
            gen {
                let! count = Gen.choose (0, 16)
                let! groups = Gen.listOfLength count CredentialsGen.groupId
                let! size = sizeAround count
                let! destination = destinationGen
                return groups, size, destination
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``each flavour's report is the one its platform names`` () : unit =
        SimulatedUnixPlatform.groupListReport SimulatedUnixPlatform.linuxX64
        |> shouldEqual GroupListReport.SortedSupplementaryGroups

        SimulatedUnixPlatform.groupListReport SimulatedUnixPlatform.linuxArm64
        |> shouldEqual GroupListReport.SortedSupplementaryGroups

        SimulatedUnixPlatform.groupListReport SimulatedUnixPlatform.macOsArm64
        |> shouldEqual GroupListReport.Unmeasured

/// `getgroups(2)` on the machine running the test, asked what the model is
/// asked with this process's own credentials. Nothing here can change the
/// credentials, so each host checks the screens against its own list: on Linux
/// in CI (x86-64) that falsifies the sort and the screens on an architecture
/// the probe did not run on.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestGetGroupsAgainstHost =

    [<DllImport("libc", EntryPoint = "getgroups", SetLastError = true)>]
    extern int private getgroups(int size, nativeint list)

    [<DllImport("libc", EntryPoint = "getegid")>]
    extern uint32 private getegid()

    [<DllImport("libc", EntryPoint = "geteuid")>]
    extern uint32 private geteuid()

    let private context : string = "TestGetGroupsAgainstHost"

    /// What the host answers, in the model's vocabulary. `buffer` must have
    /// room for `capacity` groups, and `size` must not exceed it, or the host
    /// writes past the allocation.
    let private host
        (platform : SimulatedUnixPlatform)
        (size : int)
        (buffer : nativeint)
        (capacity : int)
        : GetGroupsAnswer
        =
        if size > capacity then
            failwith $"getgroups(%d{size}) into room for %d{capacity} could write past the buffer"

        let returned = getgroups (size, buffer)

        if returned < 0 then
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno with
            | Some error -> GetGroupsAnswer.Failed error
            | None -> failwith $"getgroups(%d{size}) set errno %d{errno}, which the model does not name"
        elif size = 0 then
            GetGroupsAnswer.Counted returned
        else
            [ 0 .. returned - 1 ]
            |> List.map (fun i -> Marshal.ReadInt32 (buffer, 4 * i) |> uint32 |> GroupId.parseOrFail context)
            |> GetGroupsAnswer.Copied

    [<Test>]
    let ``the model's getgroups is this host's, for this process's own groups`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let count = getgroups (0, 0n)
            count >= 0 |> shouldEqual true
            // Room for every size asked below, sized from the count so that no
            // host list outgrows it.
            let capacity = count + 2
            let storage = Marshal.AllocHGlobal (4 * capacity)

            try
                // Asked with a positive size, since a size of 0 only counts.
                let listed =
                    match host platform capacity storage capacity with
                    | GetGroupsAnswer.Copied listed -> listed
                    | other -> failwith $"getgroups(%d{capacity}) answered %A{other}"

                // The process's supplementary groups are whatever the kernel
                // installed; on Linux they are reported as installed (sorted),
                // so handing the model the reported list is handing it the
                // installed one.
                let system : UnixSystem<int, string> =
                    UnixSystem.initial platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                    |> UnixBootImage.withCredentials
                        context
                        (Credentials.ofIds
                            (UserId.parseOrFail context (geteuid ()))
                            (GroupId.parseOrFail context (getegid ()))
                            listed)
                    |> UnixBootImage.boot

                let sizes =
                    [ Int32.MinValue ; -1 ; 0 ; count - 1 ; count ; count + 1 ; capacity ]
                    |> List.filter (fun size -> size <= capacity)
                    |> List.distinct

                for size in sizes do
                    for where, address, classified in
                        [ "storage", storage, UserBuffer.Mapped ; "NULL", 0n, UserBuffer.Unmapped 0UL ] do
                        let expected = host platform size address capacity

                        match UnixDescriptor.getgroups classified size system with
                        | Ok answer -> (size, where, answer) |> shouldEqual (size, where, expected)
                        | Error (GetGroupsRefusal.UnmeasuredGroupList refused) ->
                            // The model refuses Darwin's list outright, and only
                            // there; a negative size is answered on both.
                            (size, where, refused) |> shouldEqual (size, where, SimulatedUnixFlavour.Darwin)
                            flavour |> shouldEqual SimulatedUnixFlavour.Darwin
                            size >= 0 |> shouldEqual true
                        | Error refusal -> failwith $"getgroups(%d{size}, %s{where}) was refused: %A{refusal}"
            finally
                Marshal.FreeHGlobal storage
        )
