namespace WoofWare.PawPrint.Test

open System
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the comparison behind the `MdUtf8String_EqualsCaseInsensitive` QCall.
///
/// The end-to-end coverage lives in `sourcesPure/MdUtf8StringEqualsCaseInsensitive.cs` (ASCII,
/// passing) and `sourcesPure/MdUtf8StringEqualsCaseInsensitiveUnicode.cs` (non-ASCII, currently
/// in `TestPureCases.unimplemented` because `Encoding.UTF8.GetByteCount` on a non-ASCII name
/// hits an unrelated `clt.un`-on-`StringCharAt`-byrefs gap). These tests exercise the
/// comparison directly so the Unicode semantics are pinned regardless.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeMdUtf8String =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    let private operation = "test"

    let private utf8 (s : string) : byte array = Encoding.UTF8.GetBytes s

    let private eq (a : string) (b : string) : bool =
        NativeMdUtf8String.equalsCaseInsensitive operation (utf8 a) (utf8 b)

    [<Test>]
    let ``ASCII case folding covers exactly a-z`` () : unit =
        eq "FieldA" "fielda" |> shouldEqual true
        eq "FieldA" "FIELDA" |> shouldEqual true
        eq "fielda" "FieldA" |> shouldEqual true
        eq "FieldA" "FieldB" |> shouldEqual false

        // Digits, underscores and the rest of ASCII pass through unchanged.
        eq "_x9" "_X9" |> shouldEqual true
        eq "_x9" "_x8" |> shouldEqual false

        // A naive `byte ||| 0x20` fold would equate these pairs, which differ by 0x20 but are
        // not letters. '<'/'\\' and '>'/'^' are the pair Roslyn's `<Prop>k__BackingField`
        // naming scheme makes reachable from real metadata.
        eq "<Prop>" "\\prop^" |> shouldEqual false
        eq "@x" "`x" |> shouldEqual false
        eq "[x" "{x" |> shouldEqual false

    [<Test>]
    let ``length mismatches never compare equal`` () : unit =
        eq "" "" |> shouldEqual true
        eq "ab" "abc" |> shouldEqual false
        eq "abc" "ab" |> shouldEqual false
        // Equal UTF-8 byte length, different UTF-16 length: three ASCII bytes versus one
        // ASCII byte plus a two-byte U+00E9. CoreCLR's `count != source.GetRawCount()` check
        // is on the transcoded character count, so this must be unequal.
        (utf8 "abc").Length |> shouldEqual (utf8 "aé").Length
        eq "abc" "aé" |> shouldEqual false

    [<Test>]
    let ``non-ASCII folding uses the invariant simple uppercase mapping`` () : unit =
        // Latin-1 supplement.
        eq "CafÉ" "café" |> shouldEqual true
        eq "CafÉ" "cafè" |> shouldEqual false

        // U+03C2 (final sigma) and U+03C3 (sigma) share the uppercase mapping U+03A3.
        eq "Sigmaς" "sigmaσ" |> shouldEqual true
        eq "Sigmaς" "sigmaΣ" |> shouldEqual true
        eq "Sigmaσ" "sigmaΣ" |> shouldEqual true
        // ... but sigma is still distinct from other Greek letters.
        eq "Sigmaς" "sigmaρ" |> shouldEqual false

        // Cyrillic, to check the fold is not confined to Latin/Greek.
        eq "Дом" "дом" |> shouldEqual true
        eq "Дом" "дон" |> shouldEqual false

    [<Test>]
    let ``the two mappings that distinguish CoreCLR's casing table are honoured`` () : unit =
        // CoreCLR's table (`minipal_toupper_invariant`) carries U+0131 -> 'I' and
        // U+017F -> 'S'. The host's `Char.ToUpperInvariant` declines the first on every
        // configuration and the second under globalization-invariant mode, and
        // `StringComparison.OrdinalIgnoreCase` declines the second always -- so these two
        // assertions are what stop us from silently inheriting any of those behaviours.
        //
        // Both are confirmed against the real runtime: `GetField("Iı", IgnoreCase)` resolves
        // to a field named "ıi", and `GetField("Sſ", IgnoreCase)` to one named "ſs" -- see
        // `sourcesPure/MdUtf8StringEqualsCaseInsensitiveUnicode.cs`.
        eq "ıi" "iı" |> shouldEqual true
        eq "ſs" "sſ" |> shouldEqual true

        // Sanity: the host really does disagree, so these assertions are load-bearing.
        Char.ToUpperInvariant 'ı' |> shouldEqual 'ı'

        String.Equals ("ſ", "s", StringComparison.OrdinalIgnoreCase)
        |> shouldEqual false

    /// Every UTF-16 code unit, i.e. the whole domain of `simpleUpperInvariant`.
    let private allCodeUnits : char list = [ 0..0xFFFF ] |> List.map char

    [<Test>]
    let ``the embedded casing table matches the shape of CoreCLR's`` () : unit =
        let mapped =
            allCodeUnits
            |> List.filter (fun c -> NativeMdUtf8String.simpleUpperInvariant c <> c)

        // `unicodedata.c` at the pinned revision has 1195 `LOWER_CASE` rows, 26 of which are
        // `a`-`z` (handled by the ASCII branch). Every row maps a code point to a *different*
        // one, so the count of code units this function moves must come out at exactly 1195.
        // A duplicated or dropped row in the transcription changes this number.
        mapped |> List.length |> shouldEqual 1195

        // 26 ASCII rows, 1169 table rows, and nothing in between.
        mapped |> List.filter (fun c -> c <= '\u007F') |> shouldEqual [ 'a' .. 'z' ]

        // CoreCLR looks a code unit up directly, so surrogate halves miss the table and a
        // surrogate pair is never folded as a unit. Nothing may map a surrogate either way.
        mapped |> List.filter Char.IsSurrogate |> shouldEqual []

        mapped
        |> List.filter (fun c -> NativeMdUtf8String.simpleUpperInvariant c |> Char.IsSurrogate)
        |> shouldEqual []

        // The mapping is idempotent: uppercasing an already-uppercase code unit is a no-op,
        // which is what makes the comparison an equivalence relation.
        for c in allCodeUnits do
            let upper = NativeMdUtf8String.simpleUpperInvariant c

            if NativeMdUtf8String.simpleUpperInvariant upper <> upper then
                failwith
                    $"simpleUpperInvariant is not idempotent at U+%04X{int c} -> U+%04X{int upper} -> U+%04X{int (NativeMdUtf8String.simpleUpperInvariant upper)}"

    [<Test>]
    let ``the embedded casing table carries the expected value in each block it covers`` () : unit =
        // Spot values transcribed from `unicodedata.c`, one per region of the table, so a
        // wholesale mis-transcription (an off-by-one shift, a truncated block) is caught.
        let expected =
            [
                0x00B5, 0x039C // MICRO SIGN -> GREEK CAPITAL LETTER MU
                0x00E9, 0x00C9 // e-acute -> E-acute
                0x00FF, 0x0178 // y-diaeresis -> Y-diaeresis (not a -0x20 shift)
                0x0131, 0x0049 // DOTLESS I -> I
                0x017F, 0x0053 // LONG S -> S
                0x01C6, 0x01C4 // dz caron -> DZ caron (a titlecase triple)
                0x03C2, 0x03A3 // FINAL SIGMA -> SIGMA
                0x03C3, 0x03A3 // SIGMA -> SIGMA
                0x0450, 0x0400 // Cyrillic ie-grave
                0x0561, 0x0531 // Armenian ayb
                0x1E01, 0x1E00 // Latin Extended Additional (odd -> even pairs)
                0x1F00, 0x1F08 // Greek Extended (a +8 shift, not -0x20)
                0x2170, 0x2160 // SMALL ROMAN NUMERAL ONE
                0x24D0, 0x24B6 // CIRCLED LATIN SMALL LETTER A
                0x2D00, 0x10A0 // Georgian Supplement -> Georgian
                0xFF41, 0xFF21 // FULLWIDTH LATIN SMALL LETTER A
            ]

        for codeUnit, upper in expected do
            NativeMdUtf8String.simpleUpperInvariant (char codeUnit)
            |> shouldEqual (char upper)

        // Code units that no row covers must be returned unchanged, including ones adjacent to
        // covered ranges and ones inside them.
        for unmapped in [ 0x0080 ; 0x00B6 ; 0x00D7 ; 0x0100 ; 0x0130 ; 0x03A3 ; 0x2000 ; 0xFF5B ] do
            NativeMdUtf8String.simpleUpperInvariant (char unmapped)
            |> shouldEqual (char unmapped)

    [<Test>]
    let ``surrogate pairs are compared per code unit, not as scalars`` () : unit =
        // U+10428 DESERET SMALL LETTER LONG I uppercases to U+10400, but CoreCLR folds UTF-16
        // code units one at a time and its table has no surrogate entries, so an astral pair
        // never folds. Both encode to four UTF-8 bytes and two UTF-16 units, so nothing
        // short-circuits before the fold.
        eq "\U00010428" "\U00010400" |> shouldEqual false
        eq "\U00010428" "\U00010428" |> shouldEqual true

    [<Test>]
    let ``malformed UTF-8 is rejected loudly`` () : unit =
        // A bare continuation byte is not valid UTF-8. CoreCLR's callers can only hand us
        // metadata `#Strings` bytes or `Encoding.UTF8.GetBytes` output, both well-formed, so
        // this must fail rather than silently decode to U+FFFD (which would make distinct
        // malformed inputs compare equal).
        let bad = [| 0x80uy |]

        let exn =
            Assert.Throws<Exception> (fun () ->
                NativeMdUtf8String.equalsCaseInsensitive operation bad [| 0x41uy |]
                |> ignore<bool>
            )

        exn.Message |> shouldContainText "is not valid UTF-8"

    /// Alphabet for the property. Every BMP code unit except the surrogate range and the two
    /// code points where the oracle below is known to disagree (see the dedicated test above),
    /// weighted so that ASCII, cased Latin/Greek/Cyrillic and uncased characters all appear.
    let private genCodeUnit : Gen<char> =
        let ranges =
            [
                4, (0x0020, 0x007E) // printable ASCII
                2, (0x00C0, 0x024F) // Latin-1 supplement + Latin Extended-A/B
                2, (0x0370, 0x03FF) // Greek
                2, (0x0400, 0x04FF) // Cyrillic
                1, (0x0000, 0xFFFF) // anything else in the BMP
            ]

        ranges
        |> List.map (fun (weight, (lo, hi)) -> weight, Gen.choose (lo, hi))
        |> Gen.frequency
        |> Gen.map char
        |> Gen.filter (fun c -> not (Char.IsSurrogate c) && c <> 'ı' && c <> 'ſ')

    /// A pair of strings that agree on length and mostly agree on content, so that "equal",
    /// "equal only after folding" and "differs in exactly one position" all occur often. Also
    /// emits independent pairs so length mismatches are covered.
    let private genPair : Gen<string * string> =
        let perturb (c : char) : Gen<char> =
            Gen.frequency
                [
                    5, Gen.constant c
                    4,
                    Gen.constant (
                        if Char.IsUpper c then
                            Char.ToLowerInvariant c
                        else
                            Char.ToUpperInvariant c
                    )
                    1, genCodeUnit
                ]

        let related =
            gen {
                let! length = Gen.choose (0, 12)
                let! left = List.replicate length genCodeUnit |> Gen.sequenceToArray
                let! right = left |> Array.map perturb |> Gen.sequenceToArray
                return System.String left, System.String right
            }

        let independent =
            gen {
                let! left = Gen.arrayOf genCodeUnit
                let! right = Gen.arrayOf genCodeUnit
                return System.String left, System.String right
            }

        Gen.frequency [ 4, related ; 1, independent ]

    [<Test>]
    let ``agrees with OrdinalIgnoreCase away from its two known divergences`` () : unit =
        // `String.Equals(_, _, OrdinalIgnoreCase)` is an independent BCL implementation of
        // "compare UTF-16 code units under invariant simple uppercase". Sweeping all 65536 x
        // 65536 single-code-unit pairs shows it agrees with CoreCLR's casing table everywhere
        // except U+017F versus 'S'/'s' (and, separately from the oracle, `Char.ToUpperInvariant`
        // differs only at U+0131) -- both excluded from `genCodeUnit` and asserted directly in
        // the dedicated test above. So over this alphabet the oracle is exact.
        let mutable equalCases = 0
        let mutable foldedEqualCases = 0
        let mutable sameLengthUnequalCases = 0

        let property (left : string, right : string) : bool =
            let expected = String.Equals (left, right, StringComparison.OrdinalIgnoreCase)

            if expected then
                equalCases <- equalCases + 1

                if left <> right then
                    foldedEqualCases <- foldedEqualCases + 1
            elif left.Length = right.Length then
                sameLengthUnequalCases <- sameLengthUnequalCases + 1

            NativeMdUtf8String.equalsCaseInsensitive operation (utf8 left) (utf8 right) = expected

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPair) property)

        // Distribution checks: without them the property could pass vacuously on inputs that
        // are all trivially unequal.
        equalCases |> shouldBeGreaterThan 100
        foldedEqualCases |> shouldBeGreaterThan 50
        sameLengthUnequalCases |> shouldBeGreaterThan 100

    [<Test>]
    let ``is an equivalence relation on well-formed UTF-8`` () : unit =
        // Reflexivity and symmetry are cheap to state and would catch an accidental
        // asymmetry between the two operands (CoreCLR passes the metadata name as `szLhs`
        // and the requested name as `szRhs`, so an asymmetric comparison would be a real bug).
        let property (left : string, right : string) : bool =
            let l = utf8 left
            let r = utf8 right

            NativeMdUtf8String.equalsCaseInsensitive operation l l
            && NativeMdUtf8String.equalsCaseInsensitive operation r r
            && NativeMdUtf8String.equalsCaseInsensitive operation l r = NativeMdUtf8String.equalsCaseInsensitive
                operation
                r
                l

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPair) property)
