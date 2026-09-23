namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The name-to-value view CoreCLR's Unix PAL presents over the kernel's `envp`,
/// which is what `GetEnvironmentVariableW` and PawPrint's own knob lookups read.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEnvironmentPal =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    /// Unpaired surrogates, built from their code units: an F# literal such as
    /// `"\uD800"` compiles to U+FFFD instead, so could not provoke the case.
    let private unpairedHigh : string = string (char 0xD800)

    let private unpairedLow : string = string (char 0xDFFF)

    let private bytesOf (s : string) : UnixByteString =
        match UnixByteString.ofString s with
        | Ok bytes -> bytes
        | Error defect -> failwith $"test string %A{s}: %s{UnixPathText.describe defect}"

    /// `name` with every unpaired surrogate replaced by U+FFFD, which is the
    /// string whose UTF-8 encoding the PAL looks up. Spelled out code unit by code
    /// unit rather than taken from an `Encoding`, so that it cannot agree with the
    /// implementation merely by sharing a replacement fallback.
    let private withReplacements (name : string) : string =
        let builder = System.Text.StringBuilder ()
        let mutable i = 0

        while i < name.Length do
            let c = name.[i]

            if
                System.Char.IsHighSurrogate c
                && i + 1 < name.Length
                && System.Char.IsLowSurrogate name.[i + 1]
            then
                builder.Append(c).Append (name.[i + 1]) |> ignore<System.Text.StringBuilder>
                i <- i + 2
            elif System.Char.IsSurrogate c then
                builder.Append '\uFFFD' |> ignore<System.Text.StringBuilder>
                i <- i + 1
            else
                builder.Append c |> ignore<System.Text.StringBuilder>
                i <- i + 1

        builder.ToString ()

    /// What the PAL's lookup answers, stated over strings: nothing for an empty
    /// name or one containing `=`, and otherwise the value of the first entry
    /// that is exactly the name (an empty value) or begins with the name and a
    /// `=`. Comparing strings stands in for comparing their UTF-8 bytes because
    /// UTF-8 encodes each character separately, so one well-formed string begins
    /// with another exactly when its bytes do.
    let private expectedLookup (name : string) (entries : string list) : string option =
        let name = withReplacements name

        if name = "" || name.Contains '=' then
            None
        else
            entries
            |> List.tryPick (fun entry ->
                if entry = name then
                    Some ""
                elif entry.StartsWith (name + "=", System.StringComparison.Ordinal) then
                    Some (entry.Substring (name.Length + 1))
                else
                    None
            )

    /// Pieces a name or entry is built from. Few enough that generated names
    /// often match generated entries; `=` so that names and values contain it;
    /// U+FFFD so that an entry can be what an unpaired surrogate is looked up as.
    let private pieces : string list =
        [ "A" ; "a" ; "=" ; "é" ; "\uFFFD" ; "\U0001F436" ]

    let private genEntry : Gen<string> =
        gen {
            let! length = Gen.choose (0, 4)
            let! chosen = Gen.listOfLength length (Gen.elements pieces)
            return System.String.Concat chosen
        }

    /// Names may also hold unpaired surrogates, which a guest's string can.
    let private genName : Gen<string> =
        gen {
            let! length = Gen.choose (0, 3)

            let! chosen = Gen.listOfLength length (Gen.elements (unpairedHigh :: unpairedLow :: pieces))

            return System.String.Concat chosen
        }

    [<Test>]
    let ``a lookup finds what the PAL finds`` () : unit =
        let mutable hits = 0
        let mutable hitsThroughReplacement = 0
        let mutable hitsOnBareEntry = 0
        let mutable hitsShadowingLater = 0
        let mutable misses = 0

        let property (name : string, entries : string list) : unit =
            let expected = expectedLookup name entries

            EnvironmentPal.tryFindValue name (List.map bytesOf entries)
            |> Option.map UnixByteString.tryToString
            |> shouldEqual (Option.map Some expected)

            match expected with
            | None -> misses <- misses + 1
            | Some _ ->
                hits <- hits + 1
                let replaced = withReplacements name

                if replaced <> name then
                    hitsThroughReplacement <- hitsThroughReplacement + 1

                if List.contains replaced entries then
                    hitsOnBareEntry <- hitsOnBareEntry + 1

                let named =
                    entries
                    |> List.filter (fun entry -> entry = replaced || entry.StartsWith (replaced + "="))

                if List.length named > 1 then
                    hitsShadowingLater <- hitsShadowingLater + 1

        // Mostly free, but sometimes a name holding an unpaired surrogate beside
        // an entry named by its replacement, which free generation seldom pairs.
        let genReplacementCase : Gen<string * string list> =
            gen {
                let! name = Gen.elements [ unpairedHigh ; unpairedLow ; "A" + unpairedHigh ; unpairedLow + "a" ]
                let! target = Gen.elements [ withReplacements name + "=x" ; withReplacements name ]
                let! before = Gen.listOf genEntry
                let! after = Gen.listOf genEntry
                return name, before @ [ target ] @ after
            }

        let gen =
            Gen.frequency [ 3, Gen.zip genName (Gen.listOf genEntry) ; 1, genReplacementCase ]

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

        hits > 50 |> shouldEqual true
        hitsThroughReplacement > 20 |> shouldEqual true
        hitsOnBareEntry > 20 |> shouldEqual true
        hitsShadowingLater > 20 |> shouldEqual true
        misses > 50 |> shouldEqual true

    [<Test>]
    let ``an unpaired surrogate in a name is looked up as U+FFFD`` () : unit =
        let environment = [ bytesOf "\uFFFD=found" ]

        for name in [ unpairedHigh ; unpairedLow ; "\uFFFD" ] do
            EnvironmentPal.tryGetValue "test" name environment |> shouldEqual (Some "found")

        // One replacement per unpaired code unit, and none for a well-formed pair.
        EnvironmentPal.tryGetValue "test" (unpairedLow + unpairedHigh) environment
        |> shouldEqual None

        EnvironmentPal.tryGetValue "test" "\uD800\uDC00" environment |> shouldEqual None

    [<Test>]
    let ``a lookup is case-sensitive and takes the first entry of a name`` () : unit =
        let environment =
            List.map bytesOf [ "Key=first" ; "KEY=other" ; "Key=second" ; "Bare" ]

        EnvironmentPal.tryGetValue "test" "Key" environment
        |> shouldEqual (Some "first")

        EnvironmentPal.tryGetValue "test" "KEY" environment
        |> shouldEqual (Some "other")

        EnvironmentPal.tryGetValue "test" "key" environment |> shouldEqual None
        // An entry with no `=` is a variable with an empty value.
        EnvironmentPal.tryGetValue "test" "Bare" environment |> shouldEqual (Some "")
        // A name the PAL refuses outright, although an entry begins with it.
        EnvironmentPal.tryGetValue "test" "Key=first" environment |> shouldEqual None
        EnvironmentPal.tryGetValue "test" "" environment |> shouldEqual None

    [<Test>]
    let ``a value that is not UTF-8 is refused, naming its bytes`` () : unit =
        let invalid =
            match UnixByteString.ofBytes (ImmutableArray.Create<byte> (0x4Buy, 0x3Duy, 0xC3uy)) with
            | Ok s -> s
            | Error defect -> failwith (UnixByteString.describe defect)

        let exn =
            Assert.Throws<System.Exception> (fun () ->
                EnvironmentPal.tryGetValue "the caller" "K" [ invalid ] |> ignore<string option>
            )

        exn.Message |> shouldContainText "the caller"
        exn.Message |> shouldContainText "\\xC3"

    [<Test>]
    let ``a name-value entry reads back as its name and value`` () : unit =
        let property (name : string, value : string) : unit =
            let entry = EnvironmentPal.nameValueEntry name value

            EnvironmentPal.tryGetValue "test" name [ bytesOf entry ]
            |> shouldEqual (Some value)

        let genPair =
            Gen.zip (genEntry |> Gen.filter (fun name -> name <> "" && not (name.Contains '='))) genEntry

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPair) property)

    [<Test>]
    let ``an entry's name is the part before its first equals sign`` () : unit =
        for entry, name in [ "A=1", "A" ; "A==", "A" ; "NOEQ", "NOEQ" ; "=x", "" ; "", "" ] do
            EnvironmentPal.entryName (bytesOf entry) |> shouldEqual (bytesOf name)
