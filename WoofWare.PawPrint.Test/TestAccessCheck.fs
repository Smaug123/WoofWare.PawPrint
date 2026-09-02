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

        AccessCheck.canAccessClass accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible same-assembly`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "Asm" [] []

        AccessCheck.canAccessClass accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is not visible cross-assembly without friend`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass accessor target |> shouldEqual false

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible via target's IVT`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic top-level type is visible via accessor's IgnoresAccessChecksTo`` () : unit =
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessClass accessor target |> shouldEqual true

    [<Test>]
    let ``canAccessClass: NotPublic is not visible when IVT names a different assembly`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "WrongAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessClass accessor target |> shouldEqual false

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

        AccessCheck.canAccessClass accessor target |> shouldEqual true

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

        AccessCheck.canAccessClass accessor target |> shouldEqual false

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

        AccessCheck.canAccessClass accessor target |> shouldEqual true

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

        AccessCheck.canAccessClass accessor target |> shouldEqual false

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

        AccessCheck.canAccessClass accessor target |> shouldEqual true

    // ----- canAccessClass: loud failures -----

    [<Test>]
    let ``canAccessClass: NestedPrivate raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedPrivate "Secret" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass accessor target |> ignore)

        ex.Message |> shouldContainText "NestedPrivate"
        ex.Message |> shouldContainText "Secret"

    [<Test>]
    let ``canAccessClass: NestedFamily raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedFamily "FamType" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass accessor target |> ignore)

        ex.Message |> shouldContainText "family"
        ex.Message |> shouldContainText "FamType"

    [<Test>]
    let ``canAccessClass: NestedFamORAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedFamORAssem "T" ] "TargetAsm" [] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass accessor target |> ignore)

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessClass: NestedFamANDAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.NestedFamANDAssem "T" ] "TargetAsm" [] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessClass accessor target |> ignore)

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

        Assert.Throws (fun () -> AccessCheck.canAccessClass accessor target |> ignore)
        |> ignore

    // ----- canAccessMethod: visibility flags -----

    [<Test>]
    let ``canAccessMethod: Public method on Public class is visible cross-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod accessor target MethodAttributes.Public
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible same-assembly`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "Asm" [] []

        AccessCheck.canAccessMethod accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is not visible cross-assembly without friend`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod accessor target MethodAttributes.Assembly
        |> shouldEqual false

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible cross-assembly via IVT`` () : unit =
        let target =
            party [ level TypeAttributes.Public "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Assembly method on Public class is visible via IgnoresAccessChecksTo`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []

        let accessor =
            party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] [ "TargetAsm" ]

        AccessCheck.canAccessMethod accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessMethod: Public method on NotPublic class without friend yields false`` () : unit =
        // Class inaccessibility short-circuits regardless of the method's
        // own flag.
        let target = party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod accessor target MethodAttributes.Public
        |> shouldEqual false

    [<Test>]
    let ``canAccessMethod: Public method on NotPublic class with IVT yields true`` () : unit =
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "TargetAsm" [ "AccessorAsm" ] []

        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        AccessCheck.canAccessMethod accessor target MethodAttributes.Public
        |> shouldEqual true

    // ----- canAccessMethod: loud failures -----

    [<Test>]
    let ``canAccessMethod: Private raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessMethod accessor target MethodAttributes.Private |> ignore)

        ex.Message |> shouldContainText "Private"

    [<Test>]
    let ``canAccessMethod: Family raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () -> AccessCheck.canAccessMethod accessor target MethodAttributes.Family |> ignore)

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessMethod: FamORAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod accessor target MethodAttributes.FamORAssem
                |> ignore
            )

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessMethod: FamANDAssem raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod accessor target MethodAttributes.FamANDAssem
                |> ignore
            )

        ex.Message |> shouldContainText "family"

    [<Test>]
    let ``canAccessMethod: PrivateScope raises with diagnostic`` () : unit =
        let target = party [ level TypeAttributes.Public "T" ] "TargetAsm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "AccessorAsm" [] []

        let ex =
            Assert.Throws (fun () ->
                AccessCheck.canAccessMethod accessor target MethodAttributes.PrivateScope
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

        AccessCheck.canAccessMethod accessor target MethodAttributes.Private
        |> shouldEqual false

    // ----- same-assembly is decided from the two parties -----

    [<Test>]
    let ``canAccessClass: same assembly ignores friend declarations that name other assemblies`` () : unit =
        // Neither party names the other as a friend; the grant comes from the
        // two parties being the same assembly, and nothing else.
        let target =
            party [ level TypeAttributes.NotPublic "T" ] "Asm" [ "SomeoneElse" ] [ "SomeoneElse" ]

        let accessor =
            party [ level TypeAttributes.Public "A" ] "Asm" [ "SomeoneElse" ] [ "SomeoneElse" ]

        AccessCheck.canAccessClass accessor target |> shouldEqual true

        AccessCheck.canAccessMethod accessor target MethodAttributes.Assembly
        |> shouldEqual true

    [<Test>]
    let ``canAccessClass: assembly names differing only in case are different assemblies`` () : unit =
        // An assembly's identity is its display string, compared ordinally;
        // the case-insensitive comparison belongs to friend-name matching only.
        let target = party [ level TypeAttributes.NotPublic "T" ] "Asm" [] []
        let accessor = party [ level TypeAttributes.Public "A" ] "ASM" [] []

        AccessCheck.canAccessClass accessor target |> shouldEqual false

    /// The visibilities this slice decides without raising.
    let private supportedVisibilities : TypeAttributes list =
        [
            TypeAttributes.Public
            TypeAttributes.NestedPublic
            TypeAttributes.NotPublic
            TypeAttributes.NestedAssembly
        ]

    let private isPublic (vis : TypeAttributes) : bool =
        vis = TypeAttributes.Public || vis = TypeAttributes.NestedPublic

    /// A small alphabet, so that the two parties are often the same assembly
    /// and friend declarations often name the other party.
    let private assemblyNames : string list = [ "AsmA" ; "AsmB" ; "AsmC" ]

    /// One party: a non-empty chain of supported visibilities, an assembly
    /// name from the alphabet, and sparse friend lists drawn from the alphabet
    /// (each name is declared with probability 1/4, so that a cross-assembly
    /// check is denied more often than not).
    let private partyGen : Gen<string * TypeAttributes list * string list * string list> =
        let sparseSubset : Gen<string list> =
            assemblyNames
            |> List.map (fun name -> Gen.frequency [ 3, Gen.constant None ; 1, Gen.constant (Some name) ])
            |> Gen.sequenceToList
            |> Gen.map (List.choose id)

        gen {
            let! name = Gen.elements assemblyNames
            let! head = Gen.elements supportedVisibilities
            let! tail = Gen.listOf (Gen.elements supportedVisibilities)
            let! ivt = sparseSubset
            let! ignores = sparseSubset
            return name, head :: tail, ivt, ignores
        }

    let private toParty
        (name : string, chain : TypeAttributes list, ivt : string list, ignores : string list)
        : AccessParty
        =
        party (chain |> List.mapi (fun i vis -> level vis (sprintf "L%d" i))) name ivt ignores

    /// Reference decision for the supported visibilities, phrased on the
    /// inputs the parties were built from: an assembly-scoped level is
    /// visible iff the parties are one assembly or one of them declares the
    /// other a friend.
    let private referenceClassVisible
        (accessorName : string, _ : TypeAttributes list, _ : string list, accessorIgnores : string list)
        (targetName : string, targetChain : TypeAttributes list, targetIvt : string list, _ : string list)
        : bool
        =
        let assemblyScopedAllowed =
            accessorName = targetName
            || List.contains targetName accessorIgnores
            || List.contains accessorName targetIvt

        targetChain |> List.forall (fun vis -> isPublic vis || assemblyScopedAllowed)

    [<Test>]
    let ``canAccessClass agrees with the reference decision`` () : unit =
        let sameAssemblyDenied = ref 0
        let sameAssemblyGranted = ref 0
        let crossAssemblyDenied = ref 0
        let crossAssemblyGranted = ref 0

        let property (accessorSpec, targetSpec) : unit =
            let expected = referenceClassVisible accessorSpec targetSpec
            let accessorName, _, _, _ = accessorSpec
            let targetName, _, _, _ = targetSpec

            let counter =
                match accessorName = targetName, expected with
                | true, true -> sameAssemblyGranted
                | true, false -> sameAssemblyDenied
                | false, true -> crossAssemblyGranted
                | false, false -> crossAssemblyDenied

            counter.Value <- counter.Value + 1

            AccessCheck.canAccessClass (toParty accessorSpec) (toParty targetSpec)
            |> shouldEqual expected

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGen (Gen.zip partyGen partyGen)) property
        )

        // Same assembly is never denied, and each of the other three
        // outcomes is well represented: around a third of the pairs are
        // same-assembly, and a cross-assembly pair with a non-public level
        // and no friend grant is the commonest shape.
        sameAssemblyDenied.Value |> shouldEqual 0
        sameAssemblyGranted.Value |> shouldBeGreaterThan 150
        crossAssemblyGranted.Value |> shouldBeGreaterThan 100
        crossAssemblyDenied.Value |> shouldBeGreaterThan 100

    [<Test>]
    let ``canAccessMethod agrees with the reference decision`` () : unit =
        // Only the two member visibilities this slice decides without raising.
        let attrsGen : Gen<MethodAttributes> =
            Gen.elements [ MethodAttributes.Public ; MethodAttributes.Assembly ]

        let decidedByAssemblyIdentity = ref 0

        let property (accessorSpec, targetSpec, attrs : MethodAttributes) : unit =
            let accessorName, _, _, accessorIgnores = accessorSpec
            let targetName, _, targetIvt, _ = targetSpec

            let friendGranted =
                List.contains targetName accessorIgnores || List.contains accessorName targetIvt

            let expected =
                referenceClassVisible accessorSpec targetSpec
                && (attrs = MethodAttributes.Public || accessorName = targetName || friendGranted)

            // The shape that a caller-supplied same-assembly flag could get
            // wrong: an assembly-scoped member on a same-assembly target that
            // no friend declaration would rescue.
            if
                attrs = MethodAttributes.Assembly
                && accessorName = targetName
                && not friendGranted
            then
                decidedByAssemblyIdentity.Value <- decidedByAssemblyIdentity.Value + 1

            AccessCheck.canAccessMethod (toParty accessorSpec) (toParty targetSpec) attrs
            |> shouldEqual expected

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 1000,
            Prop.forAll (Arb.fromGen (Gen.zip3 partyGen partyGen attrsGen)) property
        )

        // 1/2 * 1/3 * (3/4)^2 of the runs, so about 90 of 1000.
        decidedByAssemblyIdentity.Value |> shouldBeGreaterThan 30
