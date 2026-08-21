namespace WoofWare.PawPrint.Test

open System
open System.IO
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// `NativeMetadataImport.isValidToken` against the host CLR's own answer for the same image.
/// </summary>
/// <remarks>
/// The oracle is indirect but exact. `RuntimeModule.ResolveField` asks
/// `MetadataImport.IsValidToken` about the caller's raw token *before* screening its kind
/// (RuntimeModule.cs:164), and throws `ArgumentOutOfRangeException` when the answer is false. Every
/// token below is one `ResolveField` cannot actually resolve, so the host either throws
/// `ArgumentOutOfRangeException` — meaning "invalid" — or some other exception, meaning the
/// validity check passed and a later screen refused it. That splits the host's behaviour exactly
/// along the predicate under test.
///
/// Worth an outside oracle rather than a table of expectations because the predicate is a
/// transcription of a 22-arm C++ switch (`MDInternalRO::IsValidToken`,
/// md/runtime/mdinternalro.cpp:3078) that is easy to misread: its near-identical sibling
/// `CMiniMdRO::_IsValidTokenBase` admits GenericParam and GenericParamConstraint, which it does
/// not, and omits the user-string case, which it has.
/// </remarks>
[<TestFixture>]
module TestMetadataTokenValidity =

    let private source =
        """
public class Outer<T>
{
    public class Inner { }
    public int Field;
    public void Method(int p) { }
    public int Property { get; set; }
    public event System.Action Event;
}

public interface IFace { }

public class Entry
{
    public static int Main(string[] args) => 0;
}
"""

    /// Table codes from `inc/corhdr.h`. `rid` is deliberately drawn from both inside and outside
    /// each table's row count, and includes 0 — the nil token of every table, which the predicate
    /// must reject before it even looks at the table.
    ///
    /// The user-string table (0x70) is excluded: `isValidToken` deliberately refuses it, because
    /// CoreCLR answers it from the `#US` heap through a validity check that is not in the pinned
    /// runtime's sparse checkout. `TestRefusesUserStringToken` pins that refusal instead.
    let private tokenCases : int list =
        [
            for table in
                [
                    0x00 // Module
                    0x01 // TypeRef
                    0x02 // TypeDef
                    0x04 // Field
                    0x06 // MethodDef
                    0x08 // Param
                    0x09 // InterfaceImpl
                    0x0A // MemberRef
                    0x0B // Constant -- a real table the switch does not admit
                    0x0C // CustomAttribute
                    0x0E // DeclSecurity
                    0x11 // StandAloneSig
                    0x14 // Event
                    0x17 // Property
                    0x19 // MethodImpl -- a real table the switch does not admit
                    0x1A // ModuleRef
                    0x1B // TypeSpec
                    0x20 // Assembly
                    0x23 // AssemblyRef
                    0x26 // File
                    0x27 // ExportedType
                    0x28 // ManifestResource
                    0x29 // NestedClass -- a real table the switch does not admit
                    0x2A // GenericParam -- admitted by _IsValidTokenBase but NOT by IsValidToken
                    0x2B // MethodSpec
                    0x2C // GenericParamConstraint -- likewise
                    0x2D // no such table
                    0x71 // mdtName -- in corhdr.h, but no table and not admitted
                    0xFF // no such table
                ] do
                for rid in [ 0 ; 1 ; 2 ; 3 ; 0x999 ] do
                    yield (table <<< 24) ||| rid
        ]

    /// The host CLR's verdict on one token: `true` when its validity check passed.
    let private hostSaysValid (hostModule : Reflection.Module) (token : int) : bool =
        try
            hostModule.ResolveField token |> ignore
            // Resolved, so the token was certainly valid.
            true
        with
        | :? ArgumentOutOfRangeException -> false
        // Any other exception means the validity check passed and a later screen refused the
        // token: wrong kind, no such member, a signature that is not a field's, and so on.
        | _ -> true

    [<Test>]
    let ``agrees with the host CLR over the whole token space`` () =
        let image =
            Roslyn.compileAssembly "TokenValidityGuest" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage

        let hostAssembly = Reflection.Assembly.Load image
        let hostModule = hostAssembly.ManifestModule

        let disagreements =
            tokenCases
            |> List.choose (fun token ->
                let ours = NativeMetadataImport.isValidToken "test" assembly token
                let theirs = hostSaysValid hostModule token

                if ours = theirs then
                    None
                else
                    Some (sprintf "0x%08x: PawPrint says %b, host says %b" token ours theirs)
            )

        disagreements |> shouldEqual []

        // Agreement is worthless if one side never says anything. Both verdicts must occur.
        let ourVerdicts =
            tokenCases
            |> List.map (fun token -> NativeMetadataImport.isValidToken "test" assembly token)

        ourVerdicts |> List.filter id |> List.isEmpty |> shouldEqual false
        ourVerdicts |> List.filter not |> List.isEmpty |> shouldEqual false

    [<Test>]
    let ``a real table the switch does not admit is invalid even when the row exists`` () =
        let image =
            Roslyn.compileAssembly "TokenValidityGuest" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage

        // `Outer<T>` has a generic parameter, so GenericParam row 1 certainly exists -- and the
        // NestedClass table has a row for `Inner`. Both tables are absent from
        // `MDInternalRO::IsValidToken`, so both tokens are invalid regardless.
        NativeMetadataImport.isValidToken "test" assembly 0x2A000001
        |> shouldEqual false

        NativeMetadataImport.isValidToken "test" assembly 0x29000001
        |> shouldEqual false

        // The control: TypeDef row 1 is the `<Module>` type, which always exists, and TypeDef *is*
        // admitted. Without this, the two assertions above would pass for an implementation that
        // called everything invalid.
        NativeMetadataImport.isValidToken "test" assembly 0x02000001 |> shouldEqual true

    [<Test>]
    let ``refuses a user-string token rather than guessing`` () =
        let image =
            Roslyn.compileAssembly "TokenValidityGuest" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _messages, loggerFactory = LoggerFactory.makeTest ()
        use peImage = new MemoryStream (image)
        let assembly = Assembly.read loggerFactory None peImage

        let exn =
            Assert.Throws<Exception> (fun () -> NativeMetadataImport.isValidToken "test" assembly 0x70000001 |> ignore)

        exn.Message |> shouldContainText "user-string token 0x70000001"
