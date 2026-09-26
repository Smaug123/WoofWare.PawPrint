namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Who a process is, and the one place that decides whether that makes it
/// privileged.
///
/// The measured rows come from
/// `docs/plans/2026-08-23-posix-kernel-extraction/credentials.c`, run on Linux
/// 6.18.5 (aarch64) as root and on Darwin 27.0 at uid 501.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestCredentials =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private context : string = "TestCredentials"

    let private uid (raw : uint32) : UserId = UserId.parseOrFail context raw

    let private gid (raw : uint32) : GroupId = GroupId.parseOrFail context raw

    /// Every `uint32`, with `(uid_t)-1` and its neighbours weighted in.
    let private candidateGen : Gen<uint32> =
        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<uint32>
                Gen.elements [ 0u ; 1u ; UInt32.MaxValue - 1u ; UInt32.MaxValue ]
            ]

    [<Test>]
    let ``a user ID is every uint32 but (uid_t)-1, and gives itself back unchanged`` () : unit =
        // Measured on Linux: `setuid((uid_t)-1)` is EINVAL and
        // `setuid(0xFFFFFFFE)` succeeds.
        let property (candidate : uint32) : unit =
            match UserId.parse candidate with
            | Some parsed ->
                candidate |> shouldNotEqual UInt32.MaxValue
                UserId.toUInt32 parsed |> shouldEqual candidate
            | None -> candidate |> shouldEqual UInt32.MaxValue

        Check.One (config, Prop.forAll (Arb.fromGen candidateGen) property)

    [<Test>]
    let ``a group ID is every uint32 but (gid_t)-1, and gives itself back unchanged`` () : unit =
        // Measured on Linux: `setgid((gid_t)-1)` is EINVAL, and so is a
        // `setgroups` naming it.
        let property (candidate : uint32) : unit =
            match GroupId.parse candidate with
            | Some parsed ->
                candidate |> shouldNotEqual UInt32.MaxValue
                GroupId.toUInt32 parsed |> shouldEqual candidate
            | None -> candidate |> shouldEqual UInt32.MaxValue

        Check.One (config, Prop.forAll (Arb.fromGen candidateGen) property)

    [<Test>]
    let ``parseOrFail names its context when it refuses`` () : unit =
        let userRefusal =
            Assert.Throws<Exception> (fun () -> UserId.parseOrFail "ctx" UInt32.MaxValue |> ignore<UserId>)

        userRefusal.Message.StartsWith ("ctx: ", StringComparison.Ordinal)
        |> shouldEqual true

        let groupRefusal =
            Assert.Throws<Exception> (fun () -> GroupId.parseOrFail "ctx" UInt32.MaxValue |> ignore<GroupId>)

        groupRefusal.Message.StartsWith ("ctx: ", StringComparison.Ordinal)
        |> shouldEqual true

    [<Test>]
    let ``privilege is decided by the effective user ID alone`` () : unit =
        let property (credentials : Credentials) : unit =
            let expected =
                if UserId.toUInt32 credentials.EffectiveUser = 0u then
                    CallerPrivilege.Privileged
                else
                    CallerPrivilege.Unprivileged

            Credentials.privilege credentials |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen CredentialsGen.credentials) property)

    [<Test>]
    let ``privilege follows the measured Linux rows`` () : unit =
        let withIds (real : uint32) (effective : uint32) (saved : uint32) : Credentials =
            { Credentials.ofIds (uid effective) (gid 1000u) [] with
                RealUser = uid real
                SavedUser = uid saved
            }

        // bind(127.0.0.1:80), and opening a root-owned 0600 file, as each.
        withIds 0u 1000u 0u
        |> Credentials.privilege
        |> shouldEqual CallerPrivilege.Unprivileged

        withIds 1000u 0u 1000u
        |> Credentials.privilege
        |> shouldEqual CallerPrivilege.Privileged

        withIds 1000u 1000u 0u
        |> Credentials.privilege
        |> shouldEqual CallerPrivilege.Unprivileged

        // Group 0 is not root, whichever of the group IDs holds it.
        { Credentials.ofIds (uid 1000u) (gid 0u) [ gid 0u ] with
            RealGroup = gid 0u
            SavedGroup = gid 0u
        }
        |> Credentials.privilege
        |> shouldEqual CallerPrivilege.Unprivileged

    [<Test>]
    let ``ofIds gives all three user IDs and all three group IDs the same value`` () : unit =
        let property (user : UserId) (group : GroupId) : unit =
            let credentials = Credentials.ofIds user group []
            credentials.RealUser |> shouldEqual user
            credentials.EffectiveUser |> shouldEqual user
            credentials.SavedUser |> shouldEqual user
            credentials.RealGroup |> shouldEqual group
            credentials.EffectiveGroup |> shouldEqual group
            credentials.SavedGroup |> shouldEqual group
            credentials.SupplementaryGroups |> shouldEqual []

        Check.One (
            config,
            Prop.forAll
                (Arb.fromGen CredentialsGen.userId)
                (fun user -> Prop.forAll (Arb.fromGen CredentialsGen.groupId) (property user))
        )

    [<Test>]
    let ``the default process has one user ID, one group ID and no supplementary groups`` () : unit =
        for flavour in [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ] do
            UnixSystem.defaultCredentials flavour
            |> shouldEqual (Credentials.ofIds (UnixSystem.defaultUserId flavour) (UnixSystem.defaultGroupId flavour) [])

            UnixSystem.defaultCredentials flavour
            |> Credentials.privilege
            |> shouldEqual CallerPrivilege.Unprivileged

    let private agreeing (credentials : Credentials) : bool =
        credentials.RealUser = credentials.EffectiveUser
        && credentials.SavedUser = credentials.EffectiveUser
        && credentials.RealGroup = credentials.EffectiveGroup
        && credentials.SavedGroup = credentials.EffectiveGroup

    /// Credentials of both kinds: independently drawn, whose IDs almost always
    /// disagree, and built by `ofIds`, whose IDs always agree.
    let private eitherKind : Gen<Credentials> =
        Gen.oneof
            [
                CredentialsGen.credentials
                gen {
                    let! credentials = CredentialsGen.credentials
                    let! user = CredentialsGen.userId
                    let! group = CredentialsGen.groupId
                    return Credentials.ofIds user group credentials.SupplementaryGroups
                }
            ]

    [<Test>]
    let ``Linux takes any credentials it can hold, and stores exactly them`` () : unit =
        let initial : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64

        let property (credentials : Credentials) : unit =
            let after = initial |> UnixSystem.withCredentials context credentials
            after.Process.Credentials |> shouldEqual credentials

            // Nothing else about the system moves.
            { after with
                Process =
                    { after.Process with
                        Credentials = initial.Process.Credentials
                    }
            }
            |> shouldEqual initial

        Check.One (config, Prop.forAll (Arb.fromGen eitherKind) property)

    [<Test>]
    let ``Darwin refuses credentials whose real, effective and saved IDs differ`` () : unit =
        // Which of them a Darwin kernel consults is unmeasured, because changing
        // a user ID there needs root.
        let initial : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64

        let property (credentials : Credentials) : unit =
            if agreeing credentials then
                (initial |> UnixSystem.withCredentials context credentials).Process.Credentials
                |> shouldEqual credentials
            else
                let refusal =
                    Assert.Throws<Exception> (fun () ->
                        initial
                        |> UnixSystem.withCredentials "ctx" credentials
                        |> ignore<UnixSystem<int, string>>
                    )

                refusal.Message.StartsWith ("ctx: ", StringComparison.Ordinal)
                |> shouldEqual true

        Check.One (config, Prop.forAll (Arb.fromGen eitherKind) property)

    [<Test>]
    let ``Darwin refuses a disagreement in any single ID`` () : unit =
        // One ID at a time, so a check that compared only some of the six fails
        // here even if a random draw never happened to isolate it.
        let initial : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64

        let base' = Credentials.ofIds (uid 501u) (gid 20u) []

        let variants =
            [
                { base' with
                    RealUser = uid 502u
                }
                { base' with
                    SavedUser = uid 502u
                }
                { base' with
                    EffectiveUser = uid 502u
                }
                { base' with
                    RealGroup = gid 21u
                }
                { base' with
                    SavedGroup = gid 21u
                }
                { base' with
                    EffectiveGroup = gid 21u
                }
            ]

        for variant in variants do
            Assert.Throws<Exception> (fun () ->
                initial
                |> UnixSystem.withCredentials context variant
                |> ignore<UnixSystem<int, string>>
            )
            |> ignore<Exception>

    [<TestCase("linux", 65536)>]
    [<TestCase("darwin", 16)>]
    let ``each platform holds exactly NGROUPS_MAX supplementary groups`` (flavour : string, limit : int) : unit =
        // Measured as setgroups(2)'s own boundary on both: one more than this is
        // EINVAL.
        let platform =
            match flavour with
            | "linux" -> SimulatedUnixPlatform.linuxX64
            | "darwin" -> SimulatedUnixPlatform.macOsArm64
            | other -> failwith $"unknown flavour %s{other}"

        SimulatedUnixPlatform.supplementaryGroupLimit platform |> shouldEqual limit

        let initial : UnixSystem<int, string> = UnixSystem.initial platform

        let groups (count : int) : GroupId list =
            List.init count (fun i -> gid (uint32 (2000 + i)))

        let atLimit = Credentials.ofIds (uid 1000u) (gid 1000u) (groups limit)

        let held = initial |> UnixSystem.withCredentials context atLimit
        held.Process.Credentials |> shouldEqual atLimit
        UnixSystem.checkInvariants held |> shouldEqual []

        let aboveLimit = Credentials.ofIds (uid 1000u) (gid 1000u) (groups (limit + 1))

        Assert.Throws<Exception> (fun () ->
            initial
            |> UnixSystem.withCredentials context aboveLimit
            |> ignore<UnixSystem<int, string>>
        )
        |> ignore<Exception>

        // A state assembled without the setter is caught by the invariants.
        { initial with
            Process =
                { initial.Process with
                    Credentials = aboveLimit
                }
        }
        |> UnixSystem.checkInvariants
        |> shouldEqual [ UnixSystemDefect.TooManySupplementaryGroups (limit + 1, limit) ]
