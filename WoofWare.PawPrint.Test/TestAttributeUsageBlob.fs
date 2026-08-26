// Native pointers are how the host runtime's own parser has to be called: its three out-params
// are `int*`.
#nowarn "9"
#nowarn "51"

namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Text
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.NativeInterop
open NUnit.Framework
open WoofWare.PawPrint

/// A blob rendered for a failure message.
module private BlobDescription =
    let describe (blob : byte array) : string =
        blob |> Array.map (sprintf "%02X") |> String.concat " "

/// The state of one of the parser's three out-param slots when it returned.
[<RequireQualifiedAccess>]
type OutParam =
    /// The parser returned without writing this slot.
    | Untouched
    | Written of int

/// Everything one call to the parser is observable by: its BOOL result, and each of its three
/// out-params.
type ParseOutcome =
    {
        Succeeded : bool
        ValidOn : OutParam
        AllowMultiple : OutParam
        Inherited : OutParam
    }

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAttributeUsageBlob =

    type private ParseDelegate = delegate of nativeint * int * nativeptr<int> * nativeptr<int> * nativeptr<int> -> int

    /// The host runtime's *own* `CustomAttribute_ParseAttributeUsageAttribute`, which is the
    /// primitive PawPrint is reimplementing.
    ///
    /// This is the oracle every case below is checked against, so no expectation here encodes
    /// anyone's reading of CoreCLR's parser: the parser itself answers. It is reached by private
    /// reflection because CoreLib exposes it nowhere else — if a future runtime renames or reshapes
    /// it, this fails loudly, which is the intended outcome rather than a silent skip.
    let private hostParse : Lazy<ParseDelegate> =
        lazy
            let declaring =
                typeof<obj>.Assembly.GetType "System.Reflection.CustomAttribute"
                |> Option.ofObj
                |> Option.defaultWith (fun () ->
                    failwith "System.Reflection.CustomAttribute is not present in the host's corelib"
                )

            // There are two overloads of this name; the QCall is the five-argument one. The other
            // is its managed `ConstArray` wrapper.
            declaring.GetMethods (BindingFlags.NonPublic ||| BindingFlags.Static)
            |> Array.filter (fun m -> m.Name = "ParseAttributeUsageAttribute" && m.GetParameters().Length = 5)
            |> Array.tryExactlyOne
            |> Option.defaultWith (fun () ->
                failwith
                    "the host's corelib has no five-argument System.Reflection.CustomAttribute.ParseAttributeUsageAttribute; the oracle needs updating for this runtime"
            )
            |> fun m -> m.CreateDelegate<ParseDelegate> ()

    /// One call, with the three out-params seeded to the given values.
    let private hostCall (blob : byte array) (seeds : int * int * int) : int * (int * int * int) =
        let parse = hostParse.Force ()
        // `fixed` on an empty array yields a null pointer, which is exactly what CoreCLR's callers
        // pass for an empty blob, so the empty case needs no special handling.
        use pinned = fixed blob
        let seedValidOn, seedAllowMultiple, seedInherited = seeds
        let mutable validOn = seedValidOn
        let mutable allowMultiple = seedAllowMultiple
        let mutable inherited = seedInherited

        let result =
            parse.Invoke (NativePtr.toNativeInt pinned, blob.Length, &&validOn, &&allowMultiple, &&inherited)

        result, (validOn, allowMultiple, inherited)

    /// Whether the parser wrote a slot cannot be read off a single call: any sentinel it is seeded
    /// with is a value the parser might itself have written. Calling twice with different seeds
    /// settles it exactly — an untouched slot returns each seed in turn, and a written one returns
    /// the same value both times.
    let private hostOutcome (blob : byte array) : ParseOutcome =
        let firstResult, (a1, b1, c1) = hostCall blob (0x5A5A5A5A, 0x11111111, 0x22222222)
        let secondResult, (a2, b2, c2) = hostCall blob (0x0F0F0F0F, 0x33333333, 0x44444444)

        if firstResult <> secondResult then
            failwithf "the host's parser is not deterministic on [%s]" (BlobDescription.describe blob)

        let slot (name : string) (seed1 : int) (seed2 : int) (got1 : int) (got2 : int) : OutParam =
            if got1 = seed1 && got2 = seed2 then
                OutParam.Untouched
            elif got1 = got2 then
                OutParam.Written got1
            else
                failwithf "the host's parser wrote %s inconsistently (%d then %d)" name got1 got2

        {
            Succeeded = firstResult <> 0
            ValidOn = slot "pTargets" 0x5A5A5A5A 0x0F0F0F0F a1 a2
            AllowMultiple = slot "pAllowMultiple" 0x11111111 0x33333333 b1 b2
            Inherited = slot "pInherited" 0x22222222 0x44444444 c1 c2
        }

    /// The same, derived from PawPrint's parse: exactly the mapping the native handler performs
    /// when it writes the guest's slots.
    let private ourOutcome (blob : byte array) : ParseOutcome =
        match CustomAttribute.parseAttributeUsage (ImmutableArray.CreateRange blob) with
        | AttributeUsageParse.Malformed _ ->
            {
                Succeeded = false
                ValidOn = OutParam.Untouched
                AllowMultiple = OutParam.Untouched
                Inherited = OutParam.Untouched
            }
        | AttributeUsageParse.ValidOnOnly (validOn, _) ->
            {
                Succeeded = false
                ValidOn = OutParam.Written validOn
                AllowMultiple = OutParam.Untouched
                Inherited = OutParam.Untouched
            }
        | AttributeUsageParse.Parsed usage ->
            {
                Succeeded = true
                ValidOn = OutParam.Written usage.ValidOn
                AllowMultiple = OutParam.Written (if usage.AllowMultiple then 1 else 0)
                Inherited = OutParam.Written (if usage.Inherited then 1 else 0)
            }

    /// The assertion every case makes: on this blob we do what the host's parser does, down to
    /// which of its three out-params it left alone.
    let private agreesWithHost (blob : byte array) : unit =
        let ours = ourOutcome blob
        let theirs = hostOutcome blob

        if ours <> theirs then
            failwithf
                "blob [%s]\n  PawPrint: %A\n  host:     %A\n  (PawPrint's parse: %A)"
                (BlobDescription.describe blob)
                ours
                theirs
                (CustomAttribute.parseAttributeUsage (ImmutableArray.CreateRange blob))

    // ---- blob construction -------------------------------------------------

    let private prolog = [| 0x01uy ; 0x00uy |]

    /// `AttributeTargets` bits chosen to have a byte set in more than one position, so a
    /// wrong-endianness or truncated read shows up as a wrong number rather than a coincidence.
    let private someTargets = [| 0x1Cuy ; 0x14uy ; 0x00uy ; 0x00uy |]

    let private FIELD = 0x53uy
    let private PROPERTY = 0x54uy
    let private BOOLEAN = 0x02uy
    let private I4 = 0x08uy
    let private SZARRAY = 0x1Duy
    let private ENUM = 0x55uy

    let private int16Bytes (v : int16) : byte array =
        [| byte (v &&& 0xFFs) ; byte ((v >>> 8) &&& 0xFFs) |]

    /// A `SerString`: a canonical `PackedLen` followed by the UTF-8 bytes.
    let private serString (s : string) : byte array =
        let utf8 = Encoding.UTF8.GetBytes s

        if utf8.Length >= 0x80 then
            failwith "test helper only encodes short names"

        Array.append [| byte utf8.Length |] utf8

    /// The same string with a deliberately non-canonical two-byte `PackedLen`. ECMA-335 gives one
    /// encoding per length, but CoreCLR's decoder accepts the longer forms, so this pins that we
    /// accept what it accepts rather than what the grammar prescribes.
    let private wideSerString (s : string) : byte array =
        let utf8 = Encoding.UTF8.GetBytes s

        if utf8.Length >= 0x80 then
            failwith "test helper only encodes short names"

        Array.concat [ [| 0x80uy ; byte utf8.Length |] ; utf8 ]

    let private namedArg (kind : byte) (elemType : byte array) (name : byte array) (value : byte array) : byte array =
        Array.concat [ [| kind |] ; elemType ; name ; value ]

    let private boolArg (kind : byte) (name : string) (value : byte) : byte array =
        namedArg kind [| BOOLEAN |] (serString name) [| value |]

    let private withNamedArgs (args : byte array list) : byte array =
        Array.concat [ prolog ; someTargets ; int16Bytes (int16 args.Length) ; Array.concat args ]

    let private noNamedArgs : byte array = Array.append prolog someTargets

    // ---- the corpus --------------------------------------------------------

    /// Every shape whose handling is a deliberate decision in `parseAttributeUsage`, including the
    /// several where CoreCLR's parser does something the ECMA-335 grammar does not describe.
    ///
    /// The expected answers are not written down: each row is checked against the host runtime's
    /// own parser, so a row cannot bake in a misreading of the C++.
    let corpus : (string * byte array) list =
        [
            "no named-arg count at all", noNamedArgs

            // `GetI2` fails whenever fewer than two bytes remain, and a failed count read is not an
            // error but a zero: both of these parse successfully, with the defaults.
            "one stray byte where the count would start", Array.append noNamedArgs [| 0xAAuy |]
            "count truncated to one byte", Array.append noNamedArgs [| 0x01uy |]

            "explicit zero count", withNamedArgs []
            "zero count with trailing bytes", Array.append (withNamedArgs []) [| 0xAAuy |]

            "AllowMultiple set true", withNamedArgs [ boolArg PROPERTY "AllowMultiple" 1uy ]
            "AllowMultiple set false", withNamedArgs [ boolArg PROPERTY "AllowMultiple" 0uy ]
            "Inherited set false", withNamedArgs [ boolArg PROPERTY "Inherited" 0uy ]
            "Inherited set true", withNamedArgs [ boolArg PROPERTY "Inherited" 1uy ]

            "both, AllowMultiple first",
            withNamedArgs [ boolArg PROPERTY "AllowMultiple" 1uy ; boolArg PROPERTY "Inherited" 0uy ]

            "both, Inherited first",
            withNamedArgs [ boolArg PROPERTY "Inherited" 0uy ; boolArg PROPERTY "AllowMultiple" 1uy ]

            // Matching compares the serialization type and the name, and never whether the blob
            // said FIELD or PROPERTY, so this sets the property.
            "AllowMultiple written as a FIELD", withNamedArgs [ boolArg FIELD "AllowMultiple" 1uy ]

            // The value is read as a raw byte and tested against zero.
            "bool value byte of 2", withNamedArgs [ boolArg PROPERTY "AllowMultiple" 2uy ]
            "bool value byte of 0xFF", withNamedArgs [ boolArg PROPERTY "Inherited" 0xFFuy ]

            "non-canonical PackedLen on a matching name",
            withNamedArgs [ namedArg PROPERTY [| BOOLEAN |] (wideSerString "AllowMultiple") [| 1uy |] ]

            "trailing bytes after the last named arg",
            Array.append (withNamedArgs [ boolArg PROPERTY "Inherited" 0uy ]) [| 0xAAuy ; 0xBBuy |]

            // The count is a *signed* int16 compared against a widened loop counter, so a count
            // with its high bit set runs no iterations and the parse succeeds.
            "count with the high bit set", Array.concat [ prolog ; someTargets ; int16Bytes 0x8001s ]

            "count with the high bit set, followed by a well-formed arg",
            Array.concat
                [
                    prolog
                    someTargets
                    int16Bytes 0x8001s
                    boolArg PROPERTY "AllowMultiple" 1uy
                ]

            // Rejections.
            "empty blob", [||]
            "one-byte blob", [| 0x01uy |]
            "wrong prolog", Array.append [| 0x02uy ; 0x00uy |] someTargets
            "fixed arg truncated", Array.concat [ prolog ; [| 0x1Cuy ; 0x14uy |] ]
            "unknown named-arg name", withNamedArgs [ boolArg PROPERTY "Nope" 1uy ]

            "matching name but I4-typed",
            withNamedArgs
                [
                    namedArg PROPERTY [| I4 |] (serString "Inherited") [| 0uy ; 0uy ; 0uy ; 0uy |]
                ]

            "matching name but SZARRAY-of-bool-typed",
            withNamedArgs
                [
                    namedArg PROPERTY [| SZARRAY ; BOOLEAN |] (serString "Inherited") [| 0uy ; 0uy ; 0uy ; 0uy |]
                ]

            // CoreCLR's `ParseEncodedType` validates no type tag, so an unknown one reaches its
            // matching loop and is rejected there; PawPrint rejects it a step earlier. Both refuse.
            "unknown type tag", withNamedArgs [ namedArg PROPERTY [| 0x2Auy |] (serString "Inherited") [| 0uy |] ]

            // An ENUM-tagged arg names its enum with a SerString. CoreCLR rejects the null form
            // while decoding the type; PawPrint decodes it and rejects it while matching.
            "ENUM-typed named arg",
            withNamedArgs
                [
                    namedArg PROPERTY (Array.append [| ENUM |] (serString "SomeEnum")) (serString "Inherited") [| 0uy |]
                ]

            "ENUM-typed named arg with a null type name",
            withNamedArgs [ namedArg PROPERTY [| ENUM ; 0xFFuy |] (serString "Inherited") [| 0uy |] ]

            "field/property tag that is neither 0x53 nor 0x54", withNamedArgs [ boolArg 0x52uy "AllowMultiple" 1uy ]

            "repeated named arg", withNamedArgs [ boolArg PROPERTY "Inherited" 0uy ; boolArg PROPERTY "Inherited" 1uy ]

            // `GetNonEmptyString` rejects both the null sentinel and the empty string.
            "empty named-arg name", withNamedArgs [ namedArg PROPERTY [| BOOLEAN |] [| 0x00uy |] [| 1uy |] ]
            "null named-arg name", withNamedArgs [ namedArg PROPERTY [| BOOLEAN |] [| 0xFFuy |] [| 1uy |] ]

            // Truncation at each point inside the named-arg section.
            "count promises an arg that is not there", Array.concat [ prolog ; someTargets ; int16Bytes 1s ]

            "count promises two args, one present",
            Array.concat [ prolog ; someTargets ; int16Bytes 2s ; boolArg PROPERTY "Inherited" 0uy ]

            "named arg truncated after its kind tag",
            Array.concat [ prolog ; someTargets ; int16Bytes 1s ; [| PROPERTY |] ]

            "named arg truncated after its type",
            Array.concat [ prolog ; someTargets ; int16Bytes 1s ; [| PROPERTY ; BOOLEAN |] ]

            "named arg name truncated mid-string",
            Array.concat
                [
                    prolog
                    someTargets
                    int16Bytes 1s
                    [| PROPERTY ; BOOLEAN ; 0x0Duy ; 0x41uy ; 0x6Cuy |]
                ]

            "matched named arg missing its value byte",
            Array.concat
                [
                    prolog
                    someTargets
                    int16Bytes 1s
                    [| PROPERTY ; BOOLEAN |]
                    serString "Inherited"
                ]
        ]

    /// `corpus` as NUnit cases, each named by its label so a failure says which shape broke.
    let corpusCases : TestCaseData list =
        corpus
        |> List.map (fun (label, blob) -> (TestCaseData [| box blob |]).SetName $"blob: %s{label}")

    [<TestCaseSource(nameof corpusCases)>]
    let ``agrees with the host runtime's own parser`` (blob : byte array) : unit = agreesWithHost blob

    /// The corpus is only worth its length if it actually splits. Without this, a
    /// `parseAttributeUsage` that rejected everything — or a corpus that had drifted into all-valid
    /// rows — would still show every row "agreeing".
    [<Test>]
    let ``the corpus contains both accepted and rejected blobs`` () : unit =
        let outcomes = corpus |> List.map (snd >> hostOutcome)

        outcomes
        |> List.filter (fun o -> not o.Succeeded)
        |> List.length
        |> shouldBeGreaterThan 10

        outcomes
        |> List.filter (fun o -> o.Succeeded)
        |> List.length
        |> shouldBeGreaterThan 10

        // And that the corpus reaches the *middle* outcome, where the fixed argument parsed and a
        // named argument did not — the case that has a written targets slot beside two untouched
        // flag slots, and the one this fixture previously could not see at all.
        outcomes
        |> List.filter (fun o -> not o.Succeeded && o.ValidOn <> OutParam.Untouched)
        |> List.length
        |> shouldBeGreaterThan 5

    // ---- the rules, stated ------------------------------------------------

    // The corpus above proves agreement without saying what is being agreed to. These few say it,
    // so the surprising rules are legible rather than merely enforced.

    let private parsed (blob : byte array) : AttributeUsageBlob =
        match CustomAttribute.parseAttributeUsage (ImmutableArray.CreateRange blob) with
        | AttributeUsageParse.Parsed usage -> usage
        | AttributeUsageParse.Malformed reason
        | AttributeUsageParse.ValidOnOnly (_, reason) ->
            failwithf "expected [%s] to parse, but: %s" (BlobDescription.describe blob) reason

    [<Test>]
    let ``an unnamed argument takes its default`` () : unit =
        parsed noNamedArgs
        |> shouldEqual
            {
                ValidOn = 0x141C
                AllowMultiple = false
                Inherited = true
            }

    [<Test>]
    let ``a field named AllowMultiple sets the property`` () : unit =
        (parsed (withNamedArgs [ boolArg FIELD "AllowMultiple" 1uy ])).AllowMultiple
        |> shouldEqual true

    [<Test>]
    let ``a named-arg count with the high bit set is read as no arguments`` () : unit =
        parsed (Array.concat [ prolog ; someTargets ; int16Bytes 0x8001s ])
        |> shouldEqual
            {
                ValidOn = 0x141C
                AllowMultiple = false
                Inherited = true
            }

    [<Test>]
    let ``a truncated named-arg count is read as no arguments`` () : unit =
        (parsed (Array.append noNamedArgs [| 0xAAuy |])).Inherited |> shouldEqual true

    [<Test>]
    let ``any non-zero value byte is true`` () : unit =
        (parsed (withNamedArgs [ boolArg PROPERTY "AllowMultiple" 2uy ])).AllowMultiple
        |> shouldEqual true

    /// Assert *why* a blob was rejected, not merely that it was.
    ///
    /// Several distinct rules all end in the same `FALSE` at the boundary, so the host-differential
    /// cases above cannot tell them apart — an empty name and a null name are both rejected by
    /// CoreCLR before matching begins, and would also be rejected (for the wrong reason) by falling
    /// through to "matches nothing". These pin the arms that exist to give the right reason.
    let private rejectedBecause (blob : byte array) (fragment : string) : unit =
        let reason =
            match CustomAttribute.parseAttributeUsage (ImmutableArray.CreateRange blob) with
            | AttributeUsageParse.Parsed usage ->
                failwithf "expected [%s] to be rejected, but it parsed to %A" (BlobDescription.describe blob) usage
            | AttributeUsageParse.Malformed reason
            | AttributeUsageParse.ValidOnOnly (_, reason) -> reason

        if not (reason.Contains fragment) then
            failwithf
                "expected the rejection of [%s] to mention '%s', but it said: %s"
                (BlobDescription.describe blob)
                fragment
                reason

    [<Test>]
    let ``an empty named-arg name is rejected as such`` () : unit =
        rejectedBecause (withNamedArgs [ namedArg PROPERTY [| BOOLEAN |] [| 0x00uy |] [| 1uy |] ]) "empty name"

    [<Test>]
    let ``a null named-arg name is rejected as such`` () : unit =
        rejectedBecause (withNamedArgs [ namedArg PROPERTY [| BOOLEAN |] [| 0xFFuy |] [| 1uy |] ]) "null name sentinel"

    [<Test>]
    let ``an unrecognised named arg is rejected as unmatched`` () : unit =
        rejectedBecause (withNamedArgs [ boolArg PROPERTY "Nope" 1uy ]) "matches no argument"

    [<Test>]
    let ``a repeated named arg is rejected as repeated`` () : unit =
        rejectedBecause
            (withNamedArgs [ boolArg PROPERTY "Inherited" 0uy ; boolArg PROPERTY "Inherited" 1uy ])
            "appears more than once"

    [<Test>]
    let ``an extra named argument rejects the whole blob`` () : unit =
        (ourOutcome (withNamedArgs [ boolArg PROPERTY "Inherited" 0uy ; boolArg PROPERTY "Nope" 1uy ])).Succeeded
        |> shouldEqual false

    // ---- differential property --------------------------------------------

    /// Well-formed blobs, plus blobs perturbed just enough to land on the error paths.
    ///
    /// The named-arg names are drawn from a set that includes both real names, a near-miss, and a
    /// name differing only in case, so the generator can produce matches, non-matches and repeats
    /// without relying on a mutation to stumble onto one.
    let private blobGen : Gen<byte array> =
        let nameGen =
            Gen.elements
                [
                    "AllowMultiple"
                    "Inherited"
                    "allowmultiple"
                    "AllowMultipleX"
                    ""
                    "Nope"
                ]

        let elemTypeGen =
            Gen.frequency
                [
                    6, Gen.constant [| BOOLEAN |]
                    1, Gen.constant [| I4 |]
                    1, Gen.constant [| SZARRAY ; BOOLEAN |]
                    1, Gen.constant (Array.append [| ENUM |] (serString "E"))
                ]

        let argGen =
            gen {
                let! kind = Gen.elements [ FIELD ; PROPERTY ; 0x52uy ]
                let! elemType = elemTypeGen
                let! name = nameGen
                let! value = Gen.elements [ 0uy ; 1uy ; 2uy ; 0xFFuy ]
                return namedArg kind elemType (serString name) [| value |]
            }

        let wellFormed =
            gen {
                // FsCheck's default int generator is size-bounded to roughly [-100, 100], which
                // would never exercise the high bytes of the targets word.
                let! targets = Gen.choose (Int32.MinValue, Int32.MaxValue)
                let! args = Gen.listOf argGen

                let! declaredCount =
                    Gen.frequency [ 8, Gen.constant args.Length ; 1, Gen.constant 0 ; 1, Gen.choose (0, 4) ]

                return
                    Array.concat
                        [
                            prolog
                            [|
                                byte targets
                                byte (targets >>> 8)
                                byte (targets >>> 16)
                                byte (targets >>> 24)
                            |]
                            int16Bytes (int16 declaredCount)
                            Array.concat args
                        ]
            }

        gen {
            let! blob = wellFormed
            // Truncate or corrupt some of the time, to reach the parser's failure arms.
            let! action =
                Gen.frequency
                    [
                        5, Gen.constant "keep"
                        2, Gen.constant "truncate"
                        2, Gen.constant "corrupt"
                    ]

            match action with
            | "truncate" ->
                let! keep = Gen.choose (0, blob.Length)
                return Array.sub blob 0 keep
            | "corrupt" when blob.Length > 0 ->
                let! index = Gen.choose (0, blob.Length - 1)
                let! replacement = Gen.choose (0, 255)
                let copy = Array.copy blob
                copy.[index] <- byte replacement
                return copy
            | _ -> return blob
        }

    /// The strongest statement available: on *any* byte string, PawPrint's parser and the host
    /// runtime's own parser reach the same verdict, and where they accept, the same three values.
    [<Test>]
    let ``agrees with the host runtime on generated blobs`` () : unit =
        let property (blob : byte array) : bool = ourOutcome blob = hostOutcome blob

        property |> Prop.forAll (Arb.fromGen blobGen) |> Check.QuickThrowOnFailure
