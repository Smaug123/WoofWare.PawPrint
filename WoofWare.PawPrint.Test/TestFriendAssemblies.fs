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
