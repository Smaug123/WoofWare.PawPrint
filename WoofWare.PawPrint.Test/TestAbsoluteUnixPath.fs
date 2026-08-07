namespace WoofWare.PawPrint.Test

open System
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAbsoluteUnixPath =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// A single path segment that `getcwd` could legitimately produce:
    /// non-empty, free of separators and NULs, not "." or "..", and containing
    /// no unpaired surrogate. Deliberately draws from FsCheck's own `char`
    /// generator (rather than an ASCII alphabet) so that multi-byte UTF-8 and
    /// well-formed surrogate pairs are exercised, since the byte length of the
    /// encoding is what the ERANGE rule turns on.
    let private segmentGen : Gen<string> =
        gen {
            let! chars =
                ArbMap.defaults
                |> ArbMap.generate<char>
                |> Gen.filter (fun c -> c <> AbsoluteUnixPath.separator && c <> '\000' && not (Char.IsSurrogate c))
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

    let private pathStringGen : Gen<string> =
        Gen.listOf segmentGen
        |> Gen.map (fun segments -> "/" + System.String.Join (string AbsoluteUnixPath.separator, segments))
        // The empty segment list yields "" + "/" collapsed to "/", the root.
        |> Gen.map (fun s -> if s = "/" then "/" else s)

    let private parseOk (candidate : string) : AbsoluteUnixPath =
        match AbsoluteUnixPath.parse candidate with
        | Ok path -> path
        | Error error -> failwith $"expected %s{candidate} to parse, but: %s{AbsoluteUnixPath.describe error}"

    let private parseError (candidate : string) : AbsoluteUnixPathError =
        match AbsoluteUnixPath.parse candidate with
        | Ok path -> failwith $"expected %s{candidate} to be rejected, but it parsed as %O{path}"
        | Error error -> error

    [<Test>]
    let ``The root parses and renders as "/"`` () : unit =
        AbsoluteUnixPath.toString AbsoluteUnixPath.root |> shouldEqual "/"
        parseOk "/" |> shouldEqual AbsoluteUnixPath.root

        AbsoluteUnixPath.toUtf8 AbsoluteUnixPath.root
        |> Seq.toList
        |> shouldEqual [ byte '/' ]

    [<Test>]
    let ``Every generated well-formed path round-trips through parse and toString`` () : unit =
        let property (candidate : string) : unit =
            AbsoluteUnixPath.toString (parseOk candidate) |> shouldEqual candidate

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Parsing is idempotent: reparsing a rendered path gives an equal value`` () : unit =
        let property (candidate : string) : unit =
            let once = parseOk candidate
            let twice = parseOk (AbsoluteUnixPath.toString once)
            twice |> shouldEqual once

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``The UTF-8 encoding decodes back to the path and contains no NUL`` () : unit =
        let property (candidate : string) : unit =
            let bytes = AbsoluteUnixPath.toUtf8 (parseOk candidate) |> Seq.toArray
            bytes |> Array.contains 0uy |> shouldEqual false
            // A strict decoder, so a malformed encoding fails rather than
            // silently producing U+FFFD and comparing unequal for the wrong reason.
            UTF8Encoding(false, true).GetString bytes |> shouldEqual candidate

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Parse never throws, whatever the input`` () : unit =
        let property (candidate : string) : unit =
            // The assertion is simply that this returns; `describe` is called
            // on the error path so that it, too, is total.
            match AbsoluteUnixPath.parse candidate with
            | Ok _ -> ()
            | Error error -> AbsoluteUnixPath.describe error |> ignore<string>

        Check.One (config, property)

    [<Test>]
    let ``Null and empty are rejected as Empty`` () : unit =
        parseError null |> shouldEqual AbsoluteUnixPathError.Empty
        parseError "" |> shouldEqual AbsoluteUnixPathError.Empty

    [<Test>]
    let ``A path not starting with the separator is rejected as NotRooted`` () : unit =
        let property (candidate : string) : unit =
            // Strip the leading separator to make an otherwise well-formed
            // path relative. "/" becomes "", which is Empty rather than
            // NotRooted, so skip it.
            let relative = candidate.Substring 1

            if not (String.IsNullOrEmpty relative) then
                parseError relative |> shouldEqual AbsoluteUnixPathError.NotRooted

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``A NUL anywhere is rejected, at the index of its first occurrence`` () : unit =
        // Insert after the leading separator, so the path stays rooted and the
        // NUL rule rather than the rooting rule is what fires.
        let withNulGen : Gen<string * int> =
            gen {
                let! candidate = pathStringGen
                let! offset = Gen.choose (0, candidate.Length - 1)
                let index = 1 + offset

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
            parseError withNul |> shouldEqual (AbsoluteUnixPathError.ContainsNul index)

        Check.One (config, Prop.forAll (Arb.fromGen withNulGen) property)

    [<Test>]
    let ``An unpaired surrogate is rejected`` () : unit =
        // Built from `char` values rather than written as `"\uD83D"`: the F#
        // lexer replaces a lone surrogate in a string literal with U+FFFD, so
        // a literal here would silently test a replacement character instead.
        let highOnly = System.String [| '/' ; 'a' ; char 0xD83D ; 'b' |]
        let lowOnly = System.String [| '/' ; 'a' ; char 0xDC36 ; 'b' |]
        // A high surrogate at the very end has no successor at all, which is a
        // different branch from "successor is not a low surrogate".
        let highAtEnd = System.String [| '/' ; 'a' ; char 0xD83D |]

        parseError highOnly |> shouldEqual (AbsoluteUnixPathError.UnpairedSurrogate 2)
        parseError lowOnly |> shouldEqual (AbsoluteUnixPathError.UnpairedSurrogate 2)
        parseError highAtEnd |> shouldEqual (AbsoluteUnixPathError.UnpairedSurrogate 2)

        // A well-formed pair in the same position must *not* be rejected, and
        // in particular the low half must not be reported as unpaired.
        parseOk "/a🐶b" |> AbsoluteUnixPath.toString |> shouldEqual "/a🐶b"

    [<Test>]
    let ``A repeated separator is rejected as an empty segment`` () : unit =
        parseError "/a//b" |> shouldEqual (AbsoluteUnixPathError.EmptySegment 3)
        parseError "//a" |> shouldEqual (AbsoluteUnixPathError.EmptySegment 1)
        // "//" trips both rules; the trailing-separator check runs first, so
        // that is what it reports. Either would be a truthful diagnosis.
        parseError "//" |> shouldEqual AbsoluteUnixPathError.TrailingSeparator

    [<Test>]
    let ``A trailing separator is rejected on every path but the root`` () : unit =
        let property (candidate : string) : unit =
            if candidate <> "/" then
                parseError (candidate + "/")
                |> shouldEqual AbsoluteUnixPathError.TrailingSeparator

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``Dot and dot-dot segments are rejected, at their own index`` () : unit =
        parseError "/."
        |> shouldEqual (AbsoluteUnixPathError.UnresolvedSegment (".", 1))

        parseError "/.."
        |> shouldEqual (AbsoluteUnixPathError.UnresolvedSegment ("..", 1))

        parseError "/a/./b"
        |> shouldEqual (AbsoluteUnixPathError.UnresolvedSegment (".", 3))

        parseError "/a/../b"
        |> shouldEqual (AbsoluteUnixPathError.UnresolvedSegment ("..", 3))
        // Segments that merely *start* with a dot are ordinary hidden files.
        parseOk "/a/.b/..c" |> AbsoluteUnixPath.toString |> shouldEqual "/a/.b/..c"

    [<Test>]
    let ``assertValid accepts every parsed path`` () : unit =
        let property (candidate : string) : unit =
            let path = parseOk candidate
            AbsoluteUnixPath.assertValid "test" path |> shouldEqual path

        Check.One (config, Prop.forAll (Arb.fromGen pathStringGen) property)

    [<Test>]
    let ``assertValid rejects the forged default value`` () : unit =
        // `private` on the union case stops construction, but not
        // `Unchecked.defaultof` — this is the one value that can carry a null
        // payload into an AbsoluteUnixPath, and the reason `assertValid` exists.
        let forged = Unchecked.defaultof<AbsoluteUnixPath>

        let exn =
            Assert.Throws<Exception> (fun () ->
                AbsoluteUnixPath.assertValid "KernelConfig.CurrentDirectory" forged
                |> ignore<AbsoluteUnixPath>
            )

        exn.Message |> shouldContainText "KernelConfig.CurrentDirectory"
        exn.Message |> shouldContainText "Unchecked.defaultof"

    [<Test>]
    let ``The kernel rejects a forged current directory at configuration time`` () : unit =
        // The boundary that matters: without this, a defaulted value would sail
        // into kernel state and fail as a null reference inside the first
        // SystemNative_GetCwd instead of naming the knob.
        let exn =
            Assert.Throws<Exception> (fun () ->
                EmulatedKernel.initial
                |> EmulatedKernel.withCurrentDirectory Unchecked.defaultof<AbsoluteUnixPath>
                |> ignore<EmulatedKernel>
            )

        exn.Message |> shouldContainText "EmulatedKernel.CurrentDirectory"

    [<Test>]
    let ``parseOrFail names the offending knob`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () ->
                AbsoluteUnixPath.parseOrFail "KernelConfig.CurrentDirectory" "relative"
                |> ignore<AbsoluteUnixPath>
            )

        exn.Message |> shouldContainText "KernelConfig.CurrentDirectory"
        exn.Message |> shouldContainText "relative"
