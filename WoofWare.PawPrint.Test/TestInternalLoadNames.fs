namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The names `AssemblyNative_InternalLoad` reports and the keys it admits: the display name a
/// failure is reported under, a full public key's token, and `StrongNameIsValidPublicKey`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestInternalLoadNames =

    let private unspecified = RequestedAssemblyVersion.Unspecified

    let private requested (major : uint16) (minor : uint16) (build : uint16) (revision : uint16) =
        {
            Major = major
            Minor = minor
            Build = build
            Revision = revision
        }

    let private token =
        [| 0xb0uy ; 0x3fuy ; 0x5fuy ; 0x7fuy ; 0x11uy ; 0xd5uy ; 0x0auy ; 0x3auy |]

    /// Every row is the `FileName` of a `FileNotFoundException` measured on .NET 10 for the
    /// request it spells.
    [<Test>]
    let ``display name of a request`` () =
        let display
            (name : string)
            (v : RequestedAssemblyVersion)
            (culture : string)
            (t : byte[] option)
            (flags : int)
            =
            NativeRuntimeAssembly.displayNameWithToken name (RequestedAssemblyVersion.toVersion v) culture t flags

        display "No.Such.Assembly" (requested unspecified unspecified unspecified unspecified) "" None 0
        |> shouldEqual "No.Such.Assembly, Culture=neutral, PublicKeyToken=null"

        display "No.Such" (requested 1us 2us unspecified unspecified) "" None 0
        |> shouldEqual "No.Such, Version=1.2.65535.65535, Culture=neutral, PublicKeyToken=null"

        display "No.Such.Assembly" (requested 1us 2us 3us 4us) "" (Some token) 0
        |> shouldEqual "No.Such.Assembly, Version=1.2.3.4, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a"

        // afRetargetable
        display "No.Such" (requested 1us 0us 0us 0us) "" (Some token) 0x100
        |> shouldEqual "No.Such, Version=1.0.0.0, Culture=neutral, PublicKeyToken=b03f5f7f11d50a3a, Retargetable=Yes"

        // afPA_x86
        display "No.Such" (requested unspecified unspecified unspecified unspecified) "" None 0x20
        |> shouldEqual "No.Such, Culture=neutral, PublicKeyToken=null, processorArchitecture=x86"

        display "System.Security.Claims" (requested 1us 0us 0us 0us) "fr" None 0
        |> shouldEqual "System.Security.Claims, Version=1.0.0.0, Culture=fr, PublicKeyToken=null"

        display "No Such" (requested unspecified unspecified unspecified unspecified) "" None 0
        |> shouldEqual "No Such, Culture=neutral, PublicKeyToken=null"

        // afContentType_WindowsRuntime
        display "No.Such" (requested unspecified unspecified unspecified unspecified) "" None 0x200
        |> shouldEqual "No.Such, Culture=neutral, PublicKeyToken=null, ContentType=WindowsRuntime"

    let private microsoftKey =
        Convert.FromHexString
            "002400000480000094000000060200000024000052534131000400000100010007d1fa57c4aed9f0a32e84aa0faefd0de9e8fd6aec8f87fb03766c834c99921eb23be79ad9d5dcc1dd9ad236132102900b723cf980957fc4e177108fc607774f29e8320e92ea05ece4e821c0a5efe8f1645c4c0c93c1ab99285d622caa652c1dfad63d745d6f2de5f17e5eaf0fc4963d261c8a12436518206dc093344d5ad293"

    let private ecmaKey = Convert.FromHexString "00000000000000000400000000000000"

    /// Both tokens are what .NET 10 reports a missing assembly under when asked for by the
    /// corresponding full key.
    [<Test>]
    let ``token of a full public key`` () =
        NativeRuntimeAssembly.publicKeyToken microsoftKey
        |> Convert.ToHexString
        |> fun s -> s.ToLowerInvariant ()
        |> shouldEqual "b03f5f7f11d50a3a"

        NativeRuntimeAssembly.publicKeyToken ecmaKey
        |> Convert.ToHexString
        |> fun s -> s.ToLowerInvariant ()
        |> shouldEqual "b77a5c561934e089"

    [<Test>]
    let ``StrongNameIsValidPublicKey`` () =
        NativeRuntimeAssembly.isValidPublicKey microsoftKey |> shouldEqual true
        NativeRuntimeAssembly.isValidPublicKey ecmaKey |> shouldEqual true
        // Measured: `PublicKey=0011` is "Invalid assembly public key."
        NativeRuntimeAssembly.isValidPublicKey [| 0x00uy ; 0x11uy |]
        |> shouldEqual false
        // A header whose key length disagrees with the blob.
        NativeRuntimeAssembly.isValidPublicKey (Array.append microsoftKey [| 0uy |])
        |> shouldEqual false
        // A key that does not announce itself as a PUBLICKEYBLOB.
        let notAKeyBlob = Array.copy microsoftKey
        notAKeyBlob.[12] <- 7uy
        NativeRuntimeAssembly.isValidPublicKey notAKeyBlob |> shouldEqual false
        // A hash algorithm of the wrong class.
        let wrongHashClass = Array.copy microsoftKey
        wrongHashClass.[5] <- 0x20uy
        NativeRuntimeAssembly.isValidPublicKey wrongHashClass |> shouldEqual false
        // An unspecified hash algorithm is allowed.
        let noHash = Array.copy microsoftKey
        noHash.[4] <- 0uy
        noHash.[5] <- 0uy
        NativeRuntimeAssembly.isValidPublicKey noHash |> shouldEqual true
