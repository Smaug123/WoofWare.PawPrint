namespace WoofWare.PawPrint.Test

open System.Reflection
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
                {
                    InternalsVisibleTo = toFriends ivt
                    IgnoresAccessChecksTo = toFriends ignoresAccessChecksTo
                }
        }

    // ----- canAccessClass: top-level visibility -----

    [<Test>]
    let ``canAccessClass: Public top-level type is visible cross-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible same-assembly`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "Asm" [] []

        AccessCheck.canAccessClass true accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is not visible cross-assembly without friend`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual false

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible via target's IVT`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible via accessor's IgnoresAccessChecksTo`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessClass false accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic is not visible when IVT names a different assembly`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "WrongAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass false accessor target |> shouldEqual false

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

        AccessCheck.canAccessClass false accessor target |> shouldEqual true

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

        AccessCheck.canAccessClass false accessor target |> shouldEqual false

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

        AccessCheck.canAccessClass false accessor target |> shouldEqual true

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

        AccessCheck.canAccessClass false accessor target |> shouldEqual false

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

        AccessCheck.canAccessClass false accessor target |> shouldEqual true

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
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible same-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "Asm" [] []

        AccessCheck.canAccessMethod true accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is not visible cross-assembly without friend`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> shouldEqual false

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible cross-assembly via IVT`` () : unit =
        let target =
            party [ level TypeAttributes.Public "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible via IgnoresAccessChecksTo`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Public method on NotPublic class without friend yields false`` () : unit =
        // Class inaccessibility short-circuits regardless of the method's
        // own flag.
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Public
        |> shouldEqual false

    [<Test>]
    let ``canAccessMethod: Public method on NotPublic class with IVT yields true`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod false accessor target MethodAttributes.Public
        |> shouldEqual true

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
        |> shouldEqual false
