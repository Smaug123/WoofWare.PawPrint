namespace WoofWare.PawPrint.Test

open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAccessCheck =

    let private level (vis : TypeAttributes) (name : string) : AccessLevelInfo =
        {
            Visibility = vis
            Name = name
        }

    let private party
        (chain : AccessLevelInfo list)
        (assemblyName : string)
        (ivt : string list)
        (ignoresAccessChecksTo : string list)
        : AccessParty
        =
        let toFriends (xs : string list) : FriendAssemblyName array =
            xs
            |> List.map (fun s ->
                match FriendAssemblyName.parse s with
                | Ok fan -> fan
                | Error e -> failwithf "test setup: parse %s failed: %s" s e
            )
            |> List.toArray

        {
            TypeChain = chain
            Assembly = AssemblyName assemblyName
            Friends =
                Ok
                    {
                        InternalsVisibleTo = toFriends ivt
                        IgnoresAccessChecksTo = toFriends ignoresAccessChecksTo
                    }
        }

    /// A party whose assembly-level friend declarations failed to parse, as
    /// `DumpedAssembly.Friends` records for an assembly carrying e.g. an
    /// `InternalsVisibleTo` name with a `PublicKeyToken=` segment.
    let private partyWithInvalidFriends (chain : AccessLevelInfo list) (assemblyName : string) : AccessParty =
        {
            TypeChain = chain
            Assembly = AssemblyName assemblyName
            Friends = Error (sprintf "test setup: %s declares an invalid friend" assemblyName)
        }

    let private expectInvalidFriendsOn (assemblyName : string) (result : Result<bool, string>) : unit =
        match result with
        | Ok visible -> failwithf "expected the invalid friend list to be reported, got Ok %b" visible
        | Error e ->
            e |> shouldContainText "invalid"
            e |> shouldContainText assemblyName

    // ----- canAccessClass: top-level visibility -----

    [<Test>]
    let ``canAccessClass: Public top-level type is visible cross-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible same-assembly`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "Asm" [] []

        AccessCheck.canAccessClass true accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is not visible cross-assembly without friend`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok false)

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible via target's IVT`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible via accessor's IgnoresAccessChecksTo`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessClass: NotPublic is not visible when IVT names a different assembly`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "WrongAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok false)

    // ----- canAccessClass: nested chains -----

    [<Test>]
    let ``canAccessClass: NestedPublic in Public outer is visible cross-assembly`` () : unit =
        let target =
            party
                [
                    level TypeAttributes.NestedPublic "Inner"
                    level TypeAttributes.Public "Outer"
                ]
                "TargetAsm"
                []
                []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessClass: NestedAssembly in Public outer is not visible cross-assembly`` () : unit =
        let target =
            party
                [
                    level TypeAttributes.NestedAssembly "Inner"
                    level TypeAttributes.Public "Outer"
                ]
                "TargetAsm"
                []
                []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok false)

    [<Test>]
    let ``canAccessClass: NestedAssembly in Public outer is visible cross-assembly with IVT`` () : unit =
        let target =
            party
                [
                    level TypeAttributes.NestedAssembly "Inner"
                    level TypeAttributes.Public "Outer"
                ]
                "TargetAsm"
                [ "AccessorAsm" ]
                []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessClass: NestedPublic in NotPublic outer is not visible cross-assembly`` () : unit =
        // The outer wraps the chain: an inner NestedPublic does not rescue
        // a non-friend cross-assembly access if the enclosing top-level type
        // is internal.
        let target =
            party
                [
                    level TypeAttributes.NestedPublic "Inner"
                    level TypeAttributes.NotPublic "Outer"
                ]
                "TargetAsm"
                []
                []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok false)

    [<Test>]
    let ``canAccessClass: NestedPublic in NotPublic outer is visible cross-assembly via IVT`` () : unit =
        let target =
            party
                [
                    level TypeAttributes.NestedPublic "Inner"
                    level TypeAttributes.NotPublic "Outer"
                ]
                "TargetAsm"
                [ "AccessorAsm" ]
                []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    // ----- canAccessClass: loud failures -----

    [<Test>]
    let ``canAccessClass: NestedPrivate raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedPrivate "Secret" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass false accessor target |> ignore)

        ex.Message |> shouldContainText "NestedPrivate"
        ex.Message |> shouldContainText "Secret"

    [<Test>]
    let ``canAccessClass: NestedFamily raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedFamily "FamType" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass false accessor target |> ignore)

        ex.Message |> shouldContainText "family"
        ex.Message |> shouldContainText "FamType"

    [<Test>]
    let ``canAccessClass: NestedFamORAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedFamORAssem "T" ] "TargetAsm" [] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass false accessor target |> ignore)

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessClass: NestedFamANDAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedFamANDAssem "T" ] "TargetAsm" [] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass false accessor target |> ignore)

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessClass: family flag deep in chain still raises`` () : unit =
        // The walk must reach every level: a family-flagged level even
        // beneath a Public outer must still raise rather than be silently
        // accepted.
        let target =
            party
                [
                    level TypeAttributes.NestedFamily "Inner"
                    level TypeAttributes.Public "Outer"
                ]
                "TargetAsm"
                []
                []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        Assert.Throws (fun () -> AccessCheck.canAccessClass false accessor target |> ignore)
        |> ignore

    // ----- canAccessMethod: visibility flags -----

    [<Test>]
    let ``canAccessMethod: Public method on Public class is visible cross-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Public
        |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible same-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "Asm" [] []

        AccessCheck.canAccessMethod true accessor target MethodAttributes.Assembly
        |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is not visible cross-assembly without friend`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> shouldEqual (Ok false)

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible cross-assembly via IVT`` () : unit =
        let target =
            party [ level TypeAttributes.Public "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible via IgnoresAccessChecksTo`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> shouldEqual (Ok true)

    [<Test>]
    let ``canAccessMethod: Public method on NotPublic class without friend yields false`` () : unit =
        // Class inaccessibility short-circuits regardless of the method's
        // own flag.
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Public
        |> shouldEqual (Ok false)

    [<Test>]
    let ``canAccessMethod: Public method on NotPublic class with IVT yields true`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Public
        |> shouldEqual (Ok true)

    // ----- canAccessMethod: loud failures -----

    [<Test>]
    let ``canAccessMethod: Private raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod false accessor target MethodAttributes.Private
                |> ignore
            )

        ex.Message |> shouldContainText "Private"

    [<Test>]
    let ``canAccessMethod: Family raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod false accessor target MethodAttributes.Family
                |> ignore
            )

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessMethod: FamORAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod false accessor target MethodAttributes.FamORAssem
                |> ignore
            )

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessMethod: FamANDAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod false accessor target MethodAttributes.FamANDAssem
                |> ignore
            )

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessMethod: PrivateScope raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod false accessor target MethodAttributes.PrivateScope
                |> ignore
            )

        ex.Message |> shouldContainText "PrivateScope"

    [<Test>]
    let ``canAccessMethod: inaccessible class short-circuits before raising on private member`` () : unit =
        // If the target class is itself inaccessible cross-assembly, the
        // class check fails first and we never reach the unimplemented
        // member-visibility branch. This means callers paying a loud-fail
        // tax only see it when access would otherwise have been granted by
        // the class check.
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Private
        |> shouldEqual (Ok false)

    // ----- invalid friend declarations surface only when consulted -----

    [<Test>]
    let ``invalid friends: Public target never consults either party`` () : unit =
        let target = partyWithInvalidFriends [ level TypeAttributes.Public "T" ] "TargetAsm"

        let accessor =
            partyWithInvalidFriends [ level TypeAttributes.Public "A" ] "AccessorAsm"

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Public
        |> shouldEqual (Ok true)

    [<Test>]
    let ``invalid friends: same assembly never consults either party`` () : unit =
        let target = partyWithInvalidFriends [ level TypeAttributes.NotPublic "T" ] "Asm"
        let accessor = partyWithInvalidFriends [ level TypeAttributes.Public "A" ] "Asm"

        AccessCheck.canAccessClass true accessor target |> shouldEqual (Ok true)

        AccessCheck.canAccessMethod true accessor target MethodAttributes.Assembly
        |> shouldEqual (Ok true)

    [<Test>]
    let ``invalid friends: NotPublic target cross-assembly reports the accessor first`` () : unit =
        // CoreCLR consults the accessor's IgnoresAccessChecksTo before the
        // target's InternalsVisibleTo, so with both invalid it is the
        // accessor's that throws.
        let target =
            partyWithInvalidFriends [ level TypeAttributes.NotPublic "T" ] "TargetAsm"

        let accessor =
            partyWithInvalidFriends [ level TypeAttributes.Public "A" ] "AccessorAsm"

        AccessCheck.canAccessClass false accessor target
        |> expectInvalidFriendsOn "AccessorAsm"

    [<Test>]
    let ``invalid friends: valid accessor that grants nothing reaches the invalid target`` () : unit =
        let target =
            partyWithInvalidFriends [ level TypeAttributes.NotPublic "T" ] "TargetAsm"

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target
        |> expectInvalidFriendsOn "TargetAsm"

    [<Test>]
    let ``invalid friends: accessor's IgnoresAccessChecksTo grant stops before the invalid target`` () : unit =
        let target =
            partyWithInvalidFriends [ level TypeAttributes.NotPublic "T" ] "TargetAsm"

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessClass false accessor target |> shouldEqual (Ok true)

    [<Test>]
    let ``invalid friends: Assembly method on Public class consults the target`` () : unit =
        let target = partyWithInvalidFriends [ level TypeAttributes.Public "T" ] "TargetAsm"
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> expectInvalidFriendsOn "TargetAsm"

    [<Test>]
    let ``invalid friends: inaccessible outer level is decided before an invalid inner one is consulted`` () : unit =
        // The walk runs innermost-first, so the NestedPublic inner level
        // passes without consulting anyone, and the NotPublic outer level
        // is what reaches the invalid declarations.
        let target =
            partyWithInvalidFriends
                [
                    level TypeAttributes.NestedPublic "Inner"
                    level TypeAttributes.NotPublic "Outer"
                ]
                "TargetAsm"

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target
        |> expectInvalidFriendsOn "TargetAsm"

    [<Test>]
    let ``invalid friends: consulted iff some level needs friend access`` () : unit =
        // Reference rule, for every chain drawn from the four visibilities
        // this slice implements: with both parties' declarations invalid and
        // nothing else to grant access, the check reports the invalid
        // declarations exactly when it is not same-assembly and some level
        // of the target's chain is assembly-scoped. Otherwise the answer is
        // `Ok true`, because every level is public.
        let visibilities =
            [
                TypeAttributes.Public
                TypeAttributes.NestedPublic
                TypeAttributes.NotPublic
                TypeAttributes.NestedAssembly
            ]

        let chainGen : Gen<TypeAttributes list> =
            Gen.listOf (Gen.elements visibilities) |> Gen.filter (fun c -> not c.IsEmpty)

        let arb = Gen.zip chainGen (Gen.elements [ true ; false ]) |> Arb.fromGen

        let property (chain : TypeAttributes list, sameAssembly : bool) : bool =
            let levels = chain |> List.mapi (fun i vis -> level vis (sprintf "L%d" i))
            let target = partyWithInvalidFriends levels "TargetAsm"

            let accessor =
                partyWithInvalidFriends [ level TypeAttributes.Public "A" ] "AccessorAsm"

            let needsFriendAccess =
                not sameAssembly
                && chain
                   |> List.exists (fun vis -> vis = TypeAttributes.NotPublic || vis = TypeAttributes.NestedAssembly)

            match AccessCheck.canAccessClass sameAssembly accessor target with
            | Ok true -> not needsFriendAccess
            | Ok false -> false
            | Error _ -> needsFriendAccess

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll arb property)
