namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `AssemblyDefinitionName.isNamed` answers "does this definition identity name that assembly?"
/// without parsing the identity, by relying on the display-name grammar putting the simple name
/// first. That makes it worth pinning against the thing it is a shortcut for: parsing the identity
/// with `AssemblyName` and comparing its `Name`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssemblyDefinitionName =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// A simple name that a display name carries verbatim: non-empty, no character that
    /// `AssemblyNameFormatter` would quote or escape, and no leading or trailing whitespace.
    /// Deliberately drawn from FsCheck's own `char` rather than an alphabet of letters, so that
    /// dots, digits and non-ASCII all appear — a dot in particular is what makes one simple name a
    /// prefix of another.
    let private simpleNameGen : Gen<string> =
        gen {
            let! chars =
                ArbMap.defaults
                |> ArbMap.generate<char>
                |> Gen.filter (fun c ->
                    not (Char.IsSurrogate c)
                    && not (Char.IsWhiteSpace c)
                    && not (Char.IsControl c)
                    && c <> ','
                    && c <> '='
                    && c <> '"'
                    && c <> '\''
                    && c <> '\\'
                    && c <> '/'
                    && c <> '\000'
                )
                |> Gen.nonEmptyListOf

            return System.String (Array.ofList chars)
        }

    /// The identity `AssemblyName` would serialise for an assembly with this simple name. This is
    /// the shape every definition identity in the interpreter has, because every one of them came
    /// from `AssemblyName.FullName`.
    let private identityOf (simpleName : string) : string =
        let name = AssemblyName ()
        name.Name <- simpleName
        name.Version <- Version (1, 2, 3, 4)
        name.FullName

    [<Test>]
    let ``isNamed accepts the identity of the name it is given`` () =
        let property (simpleName : string) : bool =
            AssemblyDefinitionName.isNamed simpleName (identityOf simpleName)

        Check.One (config, Prop.forAll (Arb.fromGen simpleNameGen) property)

    /// An identity is not obliged to carry a version at all: `AssemblyName "Foo"` serialises to
    /// bare `Foo`, with no comma to look for.
    [<Test>]
    let ``isNamed accepts a bare simple name as its own identity`` () =
        let property (simpleName : string) : bool =
            AssemblyDefinitionName.isNamed simpleName simpleName

        Check.One (config, Prop.forAll (Arb.fromGen simpleNameGen) property)

    /// The point of the comma rule. `System` is a prefix of `System.Runtime`, so a bare
    /// `StartsWith` would report that `System.Runtime, Version=...` is named `System`.
    [<Test>]
    let ``isNamed rejects a simple name that is only a prefix`` () =
        let property (simpleName : string) (suffix : string) : bool =
            let longer = simpleName + suffix

            not (AssemblyDefinitionName.isNamed simpleName (identityOf longer))
            && not (AssemblyDefinitionName.isNamed simpleName longer)

        let gen = Gen.zip simpleNameGen simpleNameGen |> Gen.map (fun (a, b) -> a, b)

        Check.One (config, Prop.forAll (Arb.fromGen gen) (fun (a, b) -> property a b))

    /// The general disagreement case: `isNamed` must agree with parsing the identity and comparing
    /// simple names ordinally, which is what the code it replaces did.
    [<Test>]
    let ``isNamed agrees with parsing the identity`` () =
        let property (candidate : string) (actual : string) : bool =
            let identity = identityOf actual
            AssemblyDefinitionName.isNamed candidate identity = (candidate = actual)

        let gen = Gen.zip simpleNameGen simpleNameGen

        Check.One (config, Prop.forAll (Arb.fromGen gen) (fun (a, b) -> property a b))

    [<Test>]
    let ``simpleName recovers the simple name from an identity`` () =
        let property (simpleName : string) : bool =
            AssemblyDefinitionName.simpleName (identityOf simpleName) = simpleName

        Check.One (config, Prop.forAll (Arb.fromGen simpleNameGen) property)

    /// A definition identity is a fixed point of `AssemblyName`'s round trip, so keying a lookup on
    /// the identity string answers the same question as parsing an `AssemblyName` out of it and
    /// asking for `FullName` back. Every lookup keyed on a definition identity rests on that.
    ///
    /// `FullName` does not fill in an absent culture or public key token, so a name carrying only a
    /// version is a fixed point too.
    [<Test>]
    let ``a definition identity is a fixed point of AssemblyName's round trip`` () =
        let property (simpleName : string) : bool =
            let identity = identityOf simpleName
            AssemblyName(identity).FullName = identity

        Check.One (config, Prop.forAll (Arb.fromGen simpleNameGen) property)

        // A real, public-key-bearing identity: the shape `identityOf` never generates, and the one
        // whose `FullName` re-derives its token by SHA-1 on every call.
        let corelib = typeof<obj>.Assembly.GetName().FullName

        AssemblyName(corelib).FullName |> shouldEqual corelib

    /// The active pattern the 66-arm intrinsic match is keyed on, pinned against the predicate it
    /// delegates to. It returns `bool` rather than an option so that applying it once per arm
    /// allocates nothing; a bool-returning partial active pattern still matches exactly when the
    /// predicate holds, and this says so over the prefix cases the comma rule exists for.
    [<Test>]
    let ``CorelibAssembly matches exactly the corelib identities`` () =
        let candidateGen : Gen<string> =
            Gen.oneof
                [
                    Gen.constant "System.Private.CoreLib"
                    Gen.constant "System.Private.CoreLibExtra"
                    Gen.constant "System.Private"
                    Gen.constant "System.Private.CoreLi"
                    simpleNameGen
                ]

        let property (simpleName : string) : bool =
            let identity = identityOf simpleName

            let matched =
                match identity with
                | CorelibAssembly -> true
                | _ -> false

            matched = (simpleName = "System.Private.CoreLib")
            && matched = AssemblyDefinitionName.isNamed "System.Private.CoreLib" identity

        Check.One (config, Prop.forAll (Arb.fromGen candidateGen) property)

    /// The bare simple name, with no version or culture after it, is also an identity the pattern
    /// must accept: `isNamed` allows the identity to end at the simple name.
    [<Test>]
    let ``CorelibAssembly matches the bare corelib simple name`` () =
        match "System.Private.CoreLib" with
        | CorelibAssembly -> ()
        | _ -> Assert.Fail "the bare simple name should name corelib"

        match "System.Private.CoreLibExtra" with
        | CorelibAssembly -> Assert.Fail "a longer simple name should not name corelib"
        | _ -> ()

    /// The corelib identity every classifier in the interpreter keys on, taken from the running
    /// runtime rather than written out, so that a version bump cannot silently make the assertion
    /// vacuous.
    [<Test>]
    let ``isNamed identifies the real corelib`` () =
        let corelib = typeof<obj>.Assembly.GetName ()

        AssemblyDefinitionName.isNamed "System.Private.CoreLib" corelib.FullName
        |> shouldEqual true

        AssemblyDefinitionName.isNamed "System.Private" corelib.FullName
        |> shouldEqual false

        AssemblyDefinitionName.isNamed "System.Private.CoreLibrary" corelib.FullName
        |> shouldEqual false

        AssemblyDefinitionName.simpleName corelib.FullName
        |> shouldEqual "System.Private.CoreLib"
