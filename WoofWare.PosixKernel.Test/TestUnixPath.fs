namespace WoofWare.PosixKernel.Test

open System
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixPath =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// An ordinary entry name: non-empty, separator-free, NUL-free, no
    /// unpaired surrogate, and not "." or "..". Draws from FsCheck's own `char`
    /// generator rather than an ASCII alphabet so multi-byte UTF-8 and
    /// well-formed surrogate pairs are exercised.
    let private nameGen : Gen<string> =
        gen {
            let! chars =
                ArbMap.defaults
                |> ArbMap.generate<char>
                |> Gen.filter (fun c -> c <> '/' && c <> '\000' && not (Char.IsSurrogate c))
                |> Gen.nonEmptyListOf

            // Astral characters are the interesting UTF-8 case (4 bytes, and a
            // surrogate *pair* in UTF-16), and the filter above excludes them,
            // so splice one in explicitly rather than hoping for it.
            let! includeAstral = Gen.frequency [ 1, Gen.constant true ; 4, Gen.constant false ]

            let chars =
                if includeAstral then
                    chars @ [ '\uD83D' ; '\uDC36' ]
                else
                    chars

            let candidate = System.String (List.toArray chars)

            return
                if candidate = "." || candidate = ".." then
                    candidate + "x"
                else
                    candidate
        }

    /// A path segment as a guest might write one, weighted so that "." and ".."
    /// appear far more often than chance would give them: they are the whole
    /// reason this type keeps components rather than a normalised string.
    let private segmentGen : Gen<string> =
        Gen.frequency [ 3, nameGen ; 1, Gen.constant "." ; 1, Gen.constant ".." ]

    /// A run of separators, so that the collapsing rule is exercised rather
    /// than assumed.
    let private separatorRunGen : Gen<string> =
        Gen.frequency [ 6, Gen.constant "/" ; 2, Gen.constant "//" ; 1, Gen.constant "///" ]

    /// A whole guest-supplied path: optionally rooted, optionally
    /// trailing-separated, with arbitrary separator runs between segments.
    let private pathStringGen : Gen<string> =
        gen {
            let! rooted = Gen.elements [ true ; false ]
            let! segments = Gen.listOf segmentGen
            let! trailing = Gen.elements [ true ; false ]

            let! joiners = Gen.listOfLength (max 0 (List.length segments - 1)) separatorRunGen

            let body =
                segments
                |> List.mapi (fun i segment -> if i = 0 then segment else joiners.[i - 1] + segment)
                |> String.concat ""

            let! prefix = if rooted then separatorRunGen else Gen.constant ""

            let! suffix =
                if trailing && not (List.isEmpty segments) then
                    separatorRunGen
                else
                    Gen.constant ""

            return prefix + body + suffix
        }

    /// A string in exactly the shape `AbsoluteUnixPath` accepts: rooted, single
    /// separators, no "." or "..", no trailing separator. `pathStringGen` hits
    /// this shape only by luck, so the cross-check against that type needs its
    /// own generator or it degenerates into testing the rejection path.
    let private absolutePathStringGen : Gen<string> =
        Gen.listOf nameGen
        |> Gen.map (fun segments -> "/" + System.String.Join ("/", segments))

    let private parseOk (candidate : string) : UnixPath =
        match UnixPath.parse candidate with
        | Ok path -> path
        | Error error -> failwith $"expected %s{candidate} to parse, but: %s{UnixPath.describe error}"

    let private parseError (candidate : string) : UnixPathError =
        match UnixPath.parse candidate with
        | Ok path -> failwith $"expected %s{candidate} to be rejected, but it parsed as %s{UnixPath.toString path}"
        | Error error -> error

    let private componentStrings (path : UnixPath) : string list =
        UnixPath.components path
        |> List.map (fun component_ ->
            match component_ with
            | PathComponent.Current -> "."
            | PathComponent.Parent -> ".."
            | PathComponent.Name name -> DirectoryEntryName.toString name
        )

    // ---------------------------------------------------------------- FileName

    [<Test>]
    let ``FileName rejects the names no directory entry can have`` () : unit =
        DirectoryEntryName.parse null |> shouldEqual (Error FileNameError.Empty)
        DirectoryEntryName.parse "" |> shouldEqual (Error FileNameError.Empty)
        DirectoryEntryName.parse "." |> shouldEqual (Error (FileNameError.Reserved "."))

        DirectoryEntryName.parse ".."
        |> shouldEqual (Error (FileNameError.Reserved ".."))

        DirectoryEntryName.parse "/"
        |> shouldEqual (Error (FileNameError.ContainsSeparator 0))

        DirectoryEntryName.parse "a/b"
        |> shouldEqual (Error (FileNameError.ContainsSeparator 1))

        DirectoryEntryName.parse "ab/"
        |> shouldEqual (Error (FileNameError.ContainsSeparator 2))

        DirectoryEntryName.parse "a\000b"
        |> shouldEqual (Error (FileNameError.Text (UnixPathTextDefect.ContainsNul 1)))

        // Built from `char` values rather than a literal: the F# lexer replaces
        // a lone surrogate in a string literal with U+FFFD, so a literal here
        // would silently test a replacement character instead.
        DirectoryEntryName.parse (System.String [| 'a' ; char 0xD83D ; 'b' |])
        |> shouldEqual (Error (FileNameError.Text (UnixPathTextDefect.UnpairedSurrogate 1)))

    [<Test>]
    let ``FileName accepts names that merely start with dots`` () : unit =
        for candidate in [ ".a" ; "..a" ; "a." ; "a.." ; "..." ; ".hidden" ] do
            match DirectoryEntryName.parse candidate with
            | Ok name -> DirectoryEntryName.toString name |> shouldEqual candidate
            | Error error -> failwith $"expected %s{candidate} to parse: %s{DirectoryEntryName.describe error}"

    [<Test>]
    let ``FileName round-trips through parse and toString`` () : unit =
        let property (candidate : string) : unit =
            match DirectoryEntryName.parse candidate with
            | Error error -> DirectoryEntryName.describe error |> ignore<string>
            | Ok name ->
                DirectoryEntryName.toString name |> shouldEqual candidate

                DirectoryEntryName.toString name
                |> DirectoryEntryName.parse
                |> shouldEqual (Ok name)

        Check.One (config, Prop.forAll (Arb.fromGen nameGen) property)

    [<Test>]
    let ``FileName encodes to NUL-free UTF-8 that decodes back`` () : unit =
        let property (candidate : string) : unit =
            match DirectoryEntryName.parse candidate with
            | Error error -> failwith $"expected %s{candidate} to parse: %s{DirectoryEntryName.describe error}"
            | Ok name ->
                let bytes = DirectoryEntryName.toUtf8 name |> Seq.toArray
                bytes |> Array.contains 0uy |> shouldEqual false
                // A strict decoder, so a malformed encoding fails rather than
                // silently producing U+FFFD and comparing unequal for the
                // wrong reason.
                UTF8Encoding(false, true).GetString bytes |> shouldEqual candidate

        Check.One (config, Prop.forAll (Arb.fromGen nameGen) property)

    [<Test>]
    let ``FileName parse never throws, whatever the input`` () : unit =
        let property (candidate : string) : unit =
            match DirectoryEntryName.parse candidate with
            | Ok _ -> ()
            | Error error -> DirectoryEntryName.describe error |> ignore<string>

        Check.One (config, property)

    [<Test>]
    let ``FileName parseOrFail names the offending boundary`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () ->
                DirectoryEntryName.parseOrFail "seed manifest entry" ".."
                |> ignore<DirectoryEntryName>
            )

        exn.Message |> shouldContainText "seed manifest entry"
        exn.Message |> shouldContainText ".."

    // ---------------------------------------------------------------- UnixPath

    [<Test>]
    let ``Null is rejected, but the empty path is not`` () : unit =
        parseError null |> shouldEqual UnixPathError.Null

        let empty = parseOk ""
        empty |> shouldEqual UnixPath.empty
        UnixPath.isEmpty empty |> shouldEqual true
        UnixPath.isRooted empty |> shouldEqual false
        UnixPath.components empty |> shouldEqual []
        UnixPath.hasTrailingSeparator empty |> shouldEqual false

    [<Test>]
    let ``The root parses as rooted with no components`` () : unit =
        for candidate in [ "/" ; "//" ; "///" ] do
            let path = parseOk candidate
            UnixPath.isRooted path |> shouldEqual true
            UnixPath.components path |> shouldEqual []
            // The separator that roots "/" is not a *trailing* one; treating it
            // as such would make the root the only path that demands its own
            // final component be a directory.
            UnixPath.hasTrailingSeparator path |> shouldEqual false
            UnixPath.isEmpty path |> shouldEqual false
            // Verbatim: these three resolve identically but are *not* the same
            // value, because a kernel can tell them apart. It counts the bytes
            // of the buffer it was handed, so "///" is two bytes more path than
            // "/" for the purpose of PATH_MAX.
            UnixPath.toString path |> shouldEqual candidate

        UnixPath.root |> shouldEqual (parseOk "/")
        parseOk "//" |> shouldNotEqual UnixPath.root

    [<Test>]
    let ``Dot and dot-dot survive parsing as their own components`` () : unit =
        componentStrings (parseOk "/a/./b/../c")
        |> shouldEqual [ "a" ; "." ; "b" ; ".." ; "c" ]

        UnixPath.components (parseOk "a/../..")
        |> shouldEqual
            [
                PathComponent.Name (DirectoryEntryName.parseOrFail "test" "a")
                PathComponent.Parent
                PathComponent.Parent
            ]

    [<Test>]
    let ``Repeated separators collapse in the components, but not in the text`` () : unit =
        componentStrings (parseOk "//a///b//") |> shouldEqual [ "a" ; "b" ]
        UnixPath.isRooted (parseOk "//a///b//") |> shouldEqual true
        UnixPath.hasTrailingSeparator (parseOk "//a///b//") |> shouldEqual true

        // The text is kept exactly as it arrived. A resolution walk sees two
        // components either way, but the *lengths* differ, and Darwin compares
        // lengths when it expands a symbolic link: measured on Darwin 25.6.0, a
        // remainder of "/a//b" costs one byte more than "/a/b", so a target one
        // byte shorter is the difference between resolving and ENAMETOOLONG.
        // Collapsing here would make two distinguishable paths equal and throw
        // away the count.
        UnixPath.toString (parseOk "//a///b//") |> shouldEqual "//a///b//"

    [<Test>]
    let ``A trailing separator is recorded, and only when something precedes it`` () : unit =
        let property (candidate : string) : unit =
            let path = parseOk candidate

            let expected =
                not (List.isEmpty (UnixPath.components path))
                && candidate.EndsWith ("/", StringComparison.Ordinal)

            UnixPath.hasTrailingSeparator path |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Appending a separator changes only the trailing flag`` () : unit =
        let property (candidate : string) : unit =
            let path = parseOk candidate

            // A path with no components has no final component to constrain, so
            // appending a separator is absorbed rather than recorded.
            if not (List.isEmpty (UnixPath.components path)) then
                let appended = parseOk (candidate + "/")
                UnixPath.components appended |> shouldEqual (UnixPath.components path)
                UnixPath.isRooted appended |> shouldEqual (UnixPath.isRooted path)
                UnixPath.hasTrailingSeparator appended |> shouldEqual true

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Rootedness is exactly a leading separator`` () : unit =
        let property (candidate : string) : unit =
            UnixPath.isRooted (parseOk candidate)
            |> shouldEqual (candidate.StartsWith ("/", StringComparison.Ordinal))

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Components are the non-empty segments, in order`` () : unit =
        let property (candidate : string) : unit =
            let expected =
                candidate.Split '/' |> Array.filter (fun s -> s <> "") |> Array.toList

            componentStrings (parseOk candidate) |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Every parsed path round-trips through toString and parse`` () : unit =
        let property (candidate : string) : unit =
            let once = parseOk candidate
            UnixPath.parse (UnixPath.toString once) |> shouldEqual (Ok once)

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Rendering is normalised: it re-renders to itself`` () : unit =
        let property (candidate : string) : unit =
            let once = UnixPath.toString (parseOk candidate)
            let twice = UnixPath.toString (parseOk once)
            twice |> shouldEqual once

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``The empty path is the only one that names nothing`` () : unit =
        let property (candidate : string) : unit =
            UnixPath.isEmpty (parseOk candidate) |> shouldEqual (candidate = "")

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Parse never throws, whatever the input`` () : unit =
        let property (candidate : string) : unit =
            match UnixPath.parse candidate with
            | Ok _ -> ()
            | Error error -> UnixPath.describe error |> ignore<string>

        Check.One (config, property)

    [<Test>]
    let ``A NUL anywhere is rejected, at the index of its first occurrence`` () : unit =
        let withNulGen : Gen<string * int> =
            gen {
                let! candidate = pathStringGen
                let! index = Gen.choose (0, candidate.Length)

                // Inserting between the halves of a surrogate pair would break
                // the pair, and the resulting unpaired *high* surrogate sits at
                // an earlier index than the NUL — so the scan would truthfully
                // report UnpairedSurrogate first. Step past such a position
                // rather than assert the wrong error.
                let index =
                    if index < candidate.Length && Char.IsLowSurrogate candidate.[index] then
                        index + 1
                    else
                        index

                return candidate.Insert (index, "\000"), index
            }

        let property (withNul : string, index : int) : unit =
            parseError withNul
            |> shouldEqual (UnixPathError.Text (UnixPathTextDefect.ContainsNul index))

        Check.One (config, Prop.forAll (Arb.fromGen withNulGen) property)

    [<Test>]
    let ``An unpaired surrogate is rejected`` () : unit =
        let highOnly = System.String [| '/' ; 'a' ; char 0xD83D ; 'b' |]
        let lowOnly = System.String [| '/' ; 'a' ; char 0xDC36 ; 'b' |]
        // A high surrogate at the very end has no successor at all, which is a
        // different branch from "successor is not a low surrogate".
        let highAtEnd = System.String [| '/' ; 'a' ; char 0xD83D |]

        for candidate in [ highOnly ; lowOnly ; highAtEnd ] do
            parseError candidate
            |> shouldEqual (UnixPathError.Text (UnixPathTextDefect.UnpairedSurrogate 2))

        // A well-formed pair in the same position must *not* be rejected, and
        // in particular the low half must not be reported as unpaired.
        UnixPath.toString (parseOk "/a🐶b") |> shouldEqual "/a🐶b"

    [<Test>]
    let ``No parsed component is ever a reserved name in disguise`` () : unit =
        let property (candidate : string) : unit =
            for component_ in UnixPath.components (parseOk candidate) do
                match component_ with
                | PathComponent.Current
                | PathComponent.Parent -> ()
                | PathComponent.Name name ->
                    let text = DirectoryEntryName.toString name
                    text |> shouldNotEqual "."
                    text |> shouldNotEqual ".."
                    // The name must survive its own parser, which is what makes
                    // it usable as a directory-entry key.
                    DirectoryEntryName.parse text |> shouldEqual (Ok name)

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``parseOrFail names the offending boundary`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> UnixPath.parseOrFail "seed manifest path" "a\000b" |> ignore<UnixPath>)

        exn.Message |> shouldContainText "seed manifest path"

    // ------------------------------------------- agreement with AbsoluteUnixPath

    [<Test>]
    let ``Every AbsoluteUnixPath is a UnixPath with the same rendering`` () : unit =
        // AbsoluteUnixPath has an independently-written parser with a stricter
        // grammar, so agreeing with it is a real cross-check rather than a
        // restatement of this parser's own rules.
        let property (candidate : string) : unit =
            match AbsoluteUnixPath.parse candidate with
            | Error error ->
                failwith
                    $"absolutePathStringGen produced %s{candidate}, which is not absolute: %s{AbsoluteUnixPath.describe error}"
            | Ok absolute ->
                let widened = UnixPath.ofAbsolute absolute
                UnixPath.toString widened |> shouldEqual candidate
                UnixPath.isRooted widened |> shouldEqual true
                UnixPath.hasTrailingSeparator widened |> shouldEqual false

                // A fully-resolved path has no navigation components left.
                for component_ in UnixPath.components widened do
                    match component_ with
                    | PathComponent.Name _ -> ()
                    | other -> failwith $"%A{other} survived in a fully-resolved path"

        Check.One (config, Prop.forAll (Arb.fromGen absolutePathStringGen) property)

    [<Test>]
    let ``A UnixPath is absolute exactly when it is rooted, resolved and unterminated`` () : unit =
        let property (candidate : string) : unit =
            let path = parseOk candidate

            let looksAbsolute =
                UnixPath.isRooted path
                && not (UnixPath.hasTrailingSeparator path)
                && UnixPath.components path
                   |> List.forall (fun component_ ->
                       match component_ with
                       | PathComponent.Name _ -> true
                       | PathComponent.Current
                       | PathComponent.Parent -> false
                   )
                // A repeated separator is a *spelling*, not a component: a UnixPath
                // keeps it verbatim in the rendered text, and `AbsoluteUnixPath`
                // rejects it, being the canonical shape `getcwd` can return.
                && not ((UnixPath.toString path).Contains "//")

            let rendered = UnixPath.toString path

            match AbsoluteUnixPath.parse rendered with
            | Ok absolute ->
                looksAbsolute |> shouldEqual true
                UnixPath.ofAbsolute absolute |> shouldEqual path
            | Error _ -> looksAbsolute |> shouldEqual false

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)
