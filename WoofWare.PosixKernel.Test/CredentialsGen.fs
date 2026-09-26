namespace WoofWare.PosixKernel.Test

open System
open FsCheck
open FsCheck.FSharp
open WoofWare.PosixKernel

/// Generators for `Credentials`, shared by every fixture that varies who a
/// process is.
[<RequireQualifiedAccess>]
module CredentialsGen =

    /// Every `uint32` a process can hold as an ID, with 0 (where the privilege
    /// rule lives), the defaults, and the largest holdable ID weighted in: a
    /// uniform draw almost never lands on 0.
    let rawId : Gen<uint32> =
        Gen.oneof
            [
                Gen.elements [ 0u ; 1u ; 20u ; 501u ; 1000u ; UInt32.MaxValue - 1u ]
                ArbMap.defaults
                |> ArbMap.generate<uint32>
                |> Gen.filter (fun candidate -> candidate <> UInt32.MaxValue)
            ]

    let userId : Gen<UserId> = rawId |> Gen.map (UserId.parseOrFail "CredentialsGen")

    let groupId : Gen<GroupId> = rawId |> Gen.map (GroupId.parseOrFail "CredentialsGen")

    /// Credentials whose six IDs are drawn independently, so real, effective
    /// and saved routinely disagree, and at most `maxGroups` supplementary
    /// groups.
    let credentialsWithAtMost (maxGroups : int) : Gen<Credentials> =
        gen {
            let! realUser = userId
            let! effectiveUser = userId
            let! savedUser = userId
            let! realGroup = groupId
            let! effectiveGroup = groupId
            let! savedGroup = groupId
            let! count = Gen.choose (0, maxGroups)
            let! groups = Gen.listOfLength count groupId

            return
                {
                    RealUser = realUser
                    EffectiveUser = effectiveUser
                    SavedUser = savedUser
                    RealGroup = realGroup
                    EffectiveGroup = effectiveGroup
                    SavedGroup = savedGroup
                    SupplementaryGroups = groups
                }
        }

    /// Credentials with few enough groups for any platform to hold.
    let credentials : Gen<Credentials> = credentialsWithAtMost 16
