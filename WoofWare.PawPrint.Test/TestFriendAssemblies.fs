namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFriendAssemblies =

    let private loadAssembly (assemblyName : string) (source : string) : DumpedAssembly =
        let image =
            Roslyn.compileAssembly assemblyName OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image)
        global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

    /// A real, structurally-valid public-key blob borrowed from
    /// System.Private.CoreLib's AssemblyName. We use this for tests that
    /// need the BCL's AssemblyName.SetPublicKey / GetPublicKeyToken to
    /// accept the bytes; SetPublicKey validates the blob format.
    let private fakePublicKey : byte[] = typeof<obj>.Assembly.GetName().GetPublicKey ()

    let private fakePublicKeyToken : byte[] =
        let an = AssemblyName ()
        an.SetPublicKey fakePublicKey
        an.GetPublicKeyToken ()

    let private toHex (bytes : byte[]) : string =
        bytes |> Array.map (fun b -> sprintf "%02x" b) |> String.concat ""

    [<Test>]
    let ``parse: simple name only`` () : unit =
        match FriendAssemblyName.parse "FriendAsm" with
        | Ok fan ->
            fan.Name |> shouldEqual "FriendAsm"
            fan.PublicKey |> shouldEqual FriendPublicKey.NotStrongNamed
            fan.Version |> shouldEqual None
            fan.Culture |> shouldEqual None
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: name with full PublicKey`` () : unit =
        let key = fakePublicKey
        let input = sprintf "FriendAsm, PublicKey=%s" (toHex key)

        match FriendAssemblyName.parse input with
        | Ok fan ->
            fan.Name |> shouldEqual "FriendAsm"

            match fan.PublicKey with
            | FriendPublicKey.FullPublicKey bytes -> bytes |> shouldEqual key
            | other -> Assert.Fail (sprintf "expected FullPublicKey, got %A" other)
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: name with PublicKeyToken`` () : unit =
        let token = fakePublicKeyToken
        let input = sprintf "FriendAsm, PublicKeyToken=%s" (toHex token)

        match FriendAssemblyName.parse input with
        | Ok fan ->
            fan.Name |> shouldEqual "FriendAsm"

            match fan.PublicKey with
            | FriendPublicKey.PublicKeyToken bytes -> bytes |> shouldEqual token
            | other -> Assert.Fail (sprintf "expected PublicKeyToken, got %A" other)
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: PublicKey=null`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, PublicKey=null" with
        | Ok fan -> fan.PublicKey |> shouldEqual FriendPublicKey.NotStrongNamed
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: captures Version`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, Version=1.2.3.4" with
        | Ok fan -> fan.Version |> shouldEqual (Some (Version (1, 2, 3, 4)))
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: captures Culture=neutral as Some empty string`` () : unit =
        // BCL normalizes "neutral" to "" (the wire form used by CoreCLR
        // internally). The distinction we care about is presence-vs-absence
        // of the Culture segment: None (no segment) vs Some "" (Culture=neutral)
        // vs Some "en-US" (a specific culture).
        match FriendAssemblyName.parse "FriendAsm, Culture=neutral" with
        | Ok fan -> fan.Culture |> shouldEqual (Some "")
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: captures Culture=en-US`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, Culture=en-US" with
        | Ok fan -> fan.Culture |> shouldEqual (Some "en-US")
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: name with no Culture segment gives Culture=None`` () : unit =
        match FriendAssemblyName.parse "FriendAsm" with
        | Ok fan -> fan.Culture |> shouldEqual None
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: tolerates whitespace around segments and equals signs`` () : unit =
        let input = sprintf "FriendAsm , PublicKey = %s" (toHex fakePublicKey)

        match FriendAssemblyName.parse input with
        | Ok fan ->
            fan.Name |> shouldEqual "FriendAsm"

            match fan.PublicKey with
            | FriendPublicKey.FullPublicKey bytes -> bytes |> shouldEqual fakePublicKey
            | other -> Assert.Fail (sprintf "expected FullPublicKey, got %A" other)
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: empty input is an error`` () : unit =
        match FriendAssemblyName.parse "" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: null input is an error`` () : unit =
        match FriendAssemblyName.parse null with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: segment without = is an error`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, NoEqualsHere" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: bad PublicKey hex is an error`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, PublicKey=GGGG" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: PublicKeyToken with wrong length is an error`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, PublicKeyToken=0011" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: bad Version is an error`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, Version=not.a.version" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: duplicate PublicKey is an error`` () : unit =
        let hex = toHex fakePublicKey
        let input = sprintf "FriendAsm, PublicKey=%s, PublicKey=%s" hex hex

        match FriendAssemblyName.parse input with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``checkFriendRestrictions: bare name is ok`` () : unit =
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        FriendAssemblyName.checkFriendRestrictions fan |> shouldEqual (Ok ())

    [<Test>]
    let ``checkFriendRestrictions: full PublicKey is ok`` () : unit =
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        FriendAssemblyName.checkFriendRestrictions fan |> shouldEqual (Ok ())

    [<Test>]
    let ``checkFriendRestrictions: Version is rejected`` () : unit =
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = Some (Version (1, 0, 0, 0))
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        match FriendAssemblyName.checkFriendRestrictions fan with
        | Error _ -> ()
        | Ok _ -> Assert.Fail "expected Error for Version"

    [<Test>]
    let ``checkFriendRestrictions: Culture is rejected`` () : unit =
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = Some "neutral"
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        match FriendAssemblyName.checkFriendRestrictions fan with
        | Error _ -> ()
        | Ok _ -> Assert.Fail "expected Error for Culture"

    [<Test>]
    let ``checkFriendRestrictions: PublicKeyToken is rejected (must be full key)`` () : unit =
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.PublicKeyToken fakePublicKeyToken
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        match FriendAssemblyName.checkFriendRestrictions fan with
        | Error _ -> ()
        | Ok _ -> Assert.Fail "expected Error for PublicKeyToken"

    [<Test>]
    let ``checkFriendRestrictions: ProcessorArchitecture is rejected`` () : unit =
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = true
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        match FriendAssemblyName.checkFriendRestrictions fan with
        | Error _ -> ()
        | Ok _ -> Assert.Fail "expected Error for ProcessorArchitecture"

    [<Test>]
    let ``parse: backslash-escaped comma stays in the name`` () : unit =
        match FriendAssemblyName.parse "Friend\\,Asm" with
        | Ok fan ->
            fan.Name |> shouldEqual "Friend,Asm"
            fan.PublicKey |> shouldEqual FriendPublicKey.NotStrongNamed
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: double-quoted name with embedded comma`` () : unit =
        match FriendAssemblyName.parse "\"Friend,Asm\"" with
        | Ok fan -> fan.Name |> shouldEqual "Friend,Asm"
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: backslash-escaped equals in the name`` () : unit =
        match FriendAssemblyName.parse "Friend\\=Asm" with
        | Ok fan -> fan.Name |> shouldEqual "Friend=Asm"
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: captures ProcessorArchitecture presence`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, ProcessorArchitecture=MSIL" with
        | Ok fan ->
            fan.Name |> shouldEqual "FriendAsm"
            fan.HasProcessorArchitecture |> shouldEqual true
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: no ProcessorArchitecture segment leaves the flag false`` () : unit =
        match FriendAssemblyName.parse "FriendAsm" with
        | Ok fan -> fan.HasProcessorArchitecture |> shouldEqual false
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: accepts structurally malformed PublicKey blob`` () : unit =
        // CoreCLR's AssemblySpec::InitNoThrow and CheckFriendAssemblyName both
        // accept arbitrary bytes for PublicKey=<hex>; the blob is not validated
        // at parse time. An invalid blob simply won't match any real assembly's
        // bytes at CompareRefToDef time.
        match FriendAssemblyName.parse "FriendAsm, PublicKey=0011" with
        | Ok fan ->
            match fan.PublicKey with
            | FriendPublicKey.FullPublicKey bytes -> bytes |> shouldEqual [| 0x00uy ; 0x11uy |]
            | other -> Assert.Fail (sprintf "expected FullPublicKey, got %A" other)
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: rejects invalid backslash escape`` () : unit =
        // BCL's AssemblyName parser only permits backslash-escaping of
        // \, =, ", ', and \\; '\x' is malformed.
        match FriendAssemblyName.parse "Friend\\xAsm" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: rejects trailing backslash`` () : unit =
        match FriendAssemblyName.parse "FriendAsm\\" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: rejects unclosed double quote`` () : unit =
        match FriendAssemblyName.parse "\"FriendAsm" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: rejects empty key`` () : unit =
        // `=value` with no preceding key is malformed (CoreCLR rejects).
        match FriendAssemblyName.parse "FriendAsm, =x" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: rejects empty value`` () : unit =
        // `Key=` with no value is malformed (CoreCLR rejects).
        match FriendAssemblyName.parse "FriendAsm, Bar=" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: preserves whitespace inside double quotes`` () : unit =
        // The display-name grammar treats quoted regions as opaque text;
        // surrounding whitespace must not be trimmed.
        match FriendAssemblyName.parse "\" Friend \"" with
        | Ok fan -> fan.Name |> shouldEqual " Friend "
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: accepts single-quoted name`` () : unit =
        // BCL's parser treats `'` as an alternative quote delimiter.
        match FriendAssemblyName.parse "'FriendAsm'" with
        | Ok fan -> fan.Name |> shouldEqual "FriendAsm"
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: rejects mid-token quote`` () : unit =
        // Quote delimiters are only valid at the start of a token; an
        // embedded quote like `Friend"Asm"` is malformed.
        match FriendAssemblyName.parse "Friend\"Asm\"" with
        | Error _ -> ()
        | Ok r -> Assert.Fail (sprintf "expected Error, got Ok %A" r)

    [<Test>]
    let ``parse: captures Retargetable=Yes`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, Retargetable=Yes" with
        | Ok fan ->
            fan.HasRetargetable |> shouldEqual true
            fan.ContentType |> shouldEqual AssemblyContentType.Default
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``parse: captures ContentType=WindowsRuntime`` () : unit =
        match FriendAssemblyName.parse "FriendAsm, ContentType=WindowsRuntime" with
        | Ok fan ->
            fan.ContentType |> shouldEqual AssemblyContentType.WindowsRuntime
            fan.HasRetargetable |> shouldEqual false
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``checkFriendRestrictions: Retargetable is tolerated`` () : unit =
        // CoreCLR's CheckFriendAssemblyName does not reject Retargetable; the
        // flag flows through to CompareRefToDef at match time.
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = true
                ContentType = AssemblyContentType.Default
            }

        FriendAssemblyName.checkFriendRestrictions fan |> shouldEqual (Ok ())

    [<Test>]
    let ``checkFriendRestrictions: ContentType is tolerated`` () : unit =
        // CoreCLR's CheckFriendAssemblyName does not reject ContentType; the
        // value flows through to CompareRefToDef at match time.
        let fan =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.WindowsRuntime
            }

        FriendAssemblyName.checkFriendRestrictions fan |> shouldEqual (Ok ())

    let private makeDef
        (name : string)
        (publicKey : byte[] option)
        (version : Version option)
        (culture : string option)
        : AssemblyName
        =
        let an = AssemblyName ()
        an.Name <- name

        match publicKey with
        | Some k -> an.SetPublicKey k
        | None -> ()

        match version with
        | Some v -> an.Version <- v
        | None -> ()

        match culture with
        | Some c -> an.CultureName <- c
        | None -> ()

        an

    let private withRetargetable (an : AssemblyName) : AssemblyName =
        an.Flags <- an.Flags ||| AssemblyNameFlags.Retargetable
        an

    let private withContentType (ct : AssemblyContentType) (an : AssemblyName) : AssemblyName =
        an.ContentType <- ct
        an

    let private baseRef =
        {
            Name = "FriendAsm"
            PublicKey = FriendPublicKey.NotStrongNamed
            Version = None
            Culture = None
            HasProcessorArchitecture = false
            HasRetargetable = false
            ContentType = AssemblyContentType.Default
        }

    [<Test>]
    let ``matchesDef: non-strong-named ref matches by name only (case-insensitive)`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "friendasm" None None None

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: non-strong-named ref with wrong name fails`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "Other" None None None

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: non-strong-named ref matches even strong-named def`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "FriendAsm" (Some fakePublicKey) None None

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: strong-named ref against non-strong-named def fails`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "FriendAsm" None None None

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: strong-named ref with matching full key matches def`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "FriendAsm" (Some fakePublicKey) None None

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: strong-named ref with full key does not match token-only def`` () : unit =
        // CoreCLR's RefMatchesDef branches on the ref. When the ref carries a
        // full key it calls CompareRefToDef directly, which compares
        // m_pbPublicKeyOrToken bytes by length+memcmp; a token-only def has
        // shorter bytes than a full key, so the comparison fails. We must
        // NOT shortcut by deriving the ref's token: that would falsely grant
        // friend access to an identity that never supplied the full key.
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = AssemblyName ()
        def.Name <- "FriendAsm"
        def.SetPublicKeyToken fakePublicKeyToken

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: strong-named ref with wrong key fails`` () : unit =
        let otherKey =
            let k = Array.copy fakePublicKey
            k.[0] <- k.[0] ^^^ 0xFFuy
            k

        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "FriendAsm" (Some otherKey) None None

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: full-key ref does not match def with cleared PublicKey flag`` () : unit =
        // CoreCLR's CompareRefToDef includes afPublicKey in its masked-flags
        // strict-equality check, so a def whose bytes equal the ref's full
        // key but whose PublicKey flag is clear must NOT match. AssemblyName
        // exposes a mutable Flags property, so this inconsistent state is
        // constructible by a caller.
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = AssemblyName ()
        def.Name <- "FriendAsm"
        def.SetPublicKey fakePublicKey
        def.Flags <- def.Flags &&& (~~~AssemblyNameFlags.PublicKey)

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: ref version unspecified accepts any def version`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.NotStrongNamed
                Version = None
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def = makeDef "FriendAsm" None (Some (Version (9, 9, 9, 9))) None

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: ref version exact match`` () : unit =
        // Need strong-named for version comparison to be visible
        // (with non-strong-named ref, RefMatchesDef short-circuits on name only)
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = Some (Version (1, 2, 3, 4))
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def =
            makeDef "FriendAsm" (Some fakePublicKey) (Some (Version (1, 2, 3, 4))) None

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: ref version mismatch fails`` () : unit =
        let ref =
            {
                Name = "FriendAsm"
                PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
                Version = Some (Version (1, 2, 3, 4))
                Culture = None
                HasProcessorArchitecture = false
                HasRetargetable = false
                ContentType = AssemblyContentType.Default
            }

        let def =
            makeDef "FriendAsm" (Some fakePublicKey) (Some (Version (1, 2, 3, 5))) None

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    // Retargetable and ContentType only participate in matching when the ref is
    // strong-named: CoreCLR's RefMatchesDef short-circuits non-strong-named refs
    // on name alone.

    let private strongRef =
        { baseRef with
            PublicKey = FriendPublicKey.FullPublicKey fakePublicKey
        }

    [<Test>]
    let ``matchesDef: retargetable ref matches retargetable def`` () : unit =
        let ref =
            { strongRef with
                HasRetargetable = true
            }

        let def = makeDef "FriendAsm" (Some fakePublicKey) None None |> withRetargetable

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: retargetable ref does not match non-retargetable def`` () : unit =
        let ref =
            { strongRef with
                HasRetargetable = true
            }

        let def = makeDef "FriendAsm" (Some fakePublicKey) None None
        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: non-retargetable ref does not match retargetable def`` () : unit =
        // CoreCLR's masked-flags strict equality: Retargetable must agree.
        let ref = strongRef

        let def = makeDef "FriendAsm" (Some fakePublicKey) None None |> withRetargetable

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``matchesDef: ContentType=Default ref matches def with any ContentType`` () : unit =
        // Optional in ref: Default means "do not constrain def's ContentType".
        let ref = strongRef

        let def =
            makeDef "FriendAsm" (Some fakePublicKey) None None
            |> withContentType AssemblyContentType.WindowsRuntime

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: ContentType=WindowsRuntime ref requires matching def`` () : unit =
        let ref =
            { strongRef with
                ContentType = AssemblyContentType.WindowsRuntime
            }

        let defMatch =
            makeDef "FriendAsm" (Some fakePublicKey) None None
            |> withContentType AssemblyContentType.WindowsRuntime

        let defDefault = makeDef "FriendAsm" (Some fakePublicKey) None None
        FriendAssemblyName.matchesDef ref defMatch |> shouldEqual true
        FriendAssemblyName.matchesDef ref defDefault |> shouldEqual false

    [<Test>]
    let ``matchesDef: full-key ref with malformed bytes matches def with same malformed bytes`` () : unit =
        // CoreCLR's CompareRefToDef does a length+memcmp on the raw key
        // blob; the bytes are not validated. The host BCL's
        // GetPublicKeyToken() does try to derive a token from a full key
        // and throws SecurityException on a malformed blob, but matchesDef
        // must not call it on the full-key path.
        let malformed = [| 0x00uy ; 0x11uy |]

        let ref =
            { baseRef with
                PublicKey = FriendPublicKey.FullPublicKey malformed
            }

        let def = AssemblyName ()
        def.Name <- "FriendAsm"
        def.SetPublicKey malformed

        FriendAssemblyName.matchesDef ref def |> shouldEqual true

    [<Test>]
    let ``matchesDef: full-key ref with malformed bytes does not match def with different bytes`` () : unit =
        let malformed = [| 0x00uy ; 0x11uy |]
        let other = [| 0x22uy ; 0x33uy |]

        let ref =
            { baseRef with
                PublicKey = FriendPublicKey.FullPublicKey malformed
            }

        let def = AssemblyName ()
        def.Name <- "FriendAsm"
        def.SetPublicKey other

        FriendAssemblyName.matchesDef ref def |> shouldEqual false

    [<Test>]
    let ``scan: assembly with no IVT returns empty`` () : unit =
        let source =
            """
public class C { public static int F() => 1; }
"""

        let assembly = loadAssembly "ScanNoIvtTestAssembly" source

        match FriendAssemblies.scan assembly with
        | Ok friends ->
            friends.InternalsVisibleTo.Length |> shouldEqual 0
            friends.IgnoresAccessChecksTo.Length |> shouldEqual 0
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``scan: assembly with single IVT captures it`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
[assembly: InternalsVisibleTo("FriendAsm")]
public class C { public static int F() => 1; }
"""

        let assembly = loadAssembly "ScanSingleIvtTestAssembly" source

        match FriendAssemblies.scan assembly with
        | Ok friends ->
            friends.InternalsVisibleTo.Length |> shouldEqual 1
            friends.InternalsVisibleTo.[0].Name |> shouldEqual "FriendAsm"
            friends.IgnoresAccessChecksTo.Length |> shouldEqual 0
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``scan: assembly with multiple IVTs captures all`` () : unit =
        let source =
            """
using System.Runtime.CompilerServices;
[assembly: InternalsVisibleTo("FriendA")]
[assembly: InternalsVisibleTo("FriendB")]
[assembly: InternalsVisibleTo("FriendC")]
public class C { public static int F() => 1; }
"""

        let assembly = loadAssembly "ScanMultipleIvtTestAssembly" source

        match FriendAssemblies.scan assembly with
        | Ok friends ->
            let names = friends.InternalsVisibleTo |> Array.map (fun f -> f.Name) |> Array.sort
            names |> shouldEqual [| "FriendA" ; "FriendB" ; "FriendC" |]
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)

    [<Test>]
    let ``scan: assembly with IgnoresAccessChecksTo captures it in subjects`` () : unit =
        // IgnoresAccessChecksToAttribute is not in the BCL on net8; declare it
        // alongside the assembly attribute. Assembly-level attributes must
        // appear before any other top-level declarations in C# source.
        let source =
            """
[assembly: System.Runtime.CompilerServices.IgnoresAccessChecksTo("Target")]

namespace System.Runtime.CompilerServices
{
    [System.AttributeUsage(System.AttributeTargets.Assembly, AllowMultiple = true)]
    public class IgnoresAccessChecksToAttribute : System.Attribute
    {
        public IgnoresAccessChecksToAttribute(string assemblyName) { AssemblyName = assemblyName; }
        public string AssemblyName { get; }
    }
}

public class C { public static int F() => 1; }
"""

        let assembly = loadAssembly "ScanIgnoresAccessTestAssembly" source

        match FriendAssemblies.scan assembly with
        | Ok friends ->
            friends.InternalsVisibleTo.Length |> shouldEqual 0
            friends.IgnoresAccessChecksTo.Length |> shouldEqual 1
            friends.IgnoresAccessChecksTo.[0].Name |> shouldEqual "Target"
        | Error e -> Assert.Fail (sprintf "expected Ok, got Error %s" e)
