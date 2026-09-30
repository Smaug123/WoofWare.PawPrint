namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.Extensions.Logging.Abstractions
open NUnit.Framework
open WoofWare.PawPrint

/// `DynamicAssemblyImage.build`, read back through the loader the interpreter uses for every other
/// assembly.
[<TestFixture>]
module TestDynamicAssemblyImage =

    // Every character a simple name can carry through `NativeAssemblyNameParts` except NUL, which
    // ends it, weighted towards the ones a display name has to escape.
    let private nameChars : char list =
        [
            'a'
            'Z'
            '0'
            '9'
            '.'
            '_'
            '-'
            ' '
            ','
            '='
            '"'
            '\''
            '\\'
            'é'
            '日'
        ]

    // `AssemblyName.CultureName` refuses a culture `CultureInfo` does not know before the name ever
    // reaches the QCall, so only known ones are generated.
    let private cultures : string list = [ "" ; "fr-FR" ; "en" ; "de-DE" ; "ja-JP" ]

    let private flagChoices : AssemblyFlags list =
        [
            enum<AssemblyFlags> 0
            AssemblyFlags.PublicKey
            AssemblyFlags.Retargetable
            AssemblyFlags.EnableJitCompileTracking
            AssemblyFlags.DisableJitCompileOptimizer
            AssemblyFlags.PublicKey ||| AssemblyFlags.Retargetable
        ]

    let private hashChoices : Configuration.Assemblies.AssemblyHashAlgorithm list =
        [
            Configuration.Assemblies.AssemblyHashAlgorithm.SHA1
            Configuration.Assemblies.AssemblyHashAlgorithm.SHA256
            Configuration.Assemblies.AssemblyHashAlgorithm.MD5
        ]

    // Only keys `StrongNameIsValidPublicKey` accepts: CoreCLR refuses any other with
    // `SecurityException` before the assembly exists.
    let private publicKeys : ImmutableArray<byte> list =
        [
            ImmutableArray<byte>.Empty
            // The ECMA standard key (ECMA-335 II.6.2.1.3).
            ImmutableArray.Create<byte>
                [|
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                    4uy
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                    0uy
                |]
            ImmutableArray.CreateRange (typeof<obj>.Assembly.GetName().GetPublicKey ())
        ]

    let private genName : Gen<DynamicAssemblyName> =
        gen {
            let! simpleName =
                Gen.elements nameChars
                |> Gen.nonEmptyListOf
                |> Gen.map (fun chars -> System.String (Array.ofList chars))

            let! components = Gen.arrayOfLength 4 (Gen.choose (0, 65535))
            let! culture = Gen.elements cultures

            let! publicKey = Gen.elements publicKeys

            let! flags = Gen.elements flagChoices
            let! hash = Gen.elements hashChoices

            return
                {
                    SimpleName = simpleName
                    Version = Version (components.[0], components.[1], components.[2], components.[3])
                    Culture = culture
                    PublicKey = publicKey
                    Flags = flags
                    HashAlgorithm = enum<AssemblyHashAlgorithm> (int hash)
                }
        }

    let private genGuid : Gen<Guid> =
        Gen.arrayOfLength 16 (Gen.choose (0, 255) |> Gen.map byte) |> Gen.map Guid

    let private read (image : byte[]) : DumpedAssembly =
        use stream = new MemoryStream (image)
        Assembly.read NullLoggerFactory.Instance None stream

    /// What `System.Reflection.AssemblyName` makes of the same parts: an oracle for the display name
    /// that does not go through the image.
    let private expectedName (name : DynamicAssemblyName) : AssemblyName =
        let expected = AssemblyName ()
        expected.Name <- name.SimpleName
        expected.Version <- name.Version
        expected.CultureName <- name.Culture

        // An empty key, not none, so the display name spells `PublicKeyToken=null` as CoreCLR's does.
        expected.SetPublicKey (Seq.toArray name.PublicKey)

        expected.Flags <- enum<AssemblyNameFlags> (int name.Flags)
        expected

    [<Test>]
    let ``the image carries the requested identity and nothing else`` () : unit =
        let property (name : DynamicAssemblyName) (mvid : Guid) : unit =
            let assy = DynamicAssemblyImage.build name mvid |> read

            assy.Name.Name |> shouldEqual name.SimpleName
            assy.Name.Version |> shouldEqual name.Version
            assy.Name.CultureName |> shouldEqual name.Culture
            assy.PublicKey |> Seq.toList |> shouldEqual (Seq.toList name.PublicKey)
            assy.Flags |> shouldEqual name.Flags
            assy.HashAlgorithm |> shouldEqual name.HashAlgorithm
            assy.DefinitionFullName |> shouldEqual (expectedName name).FullName

            assy.ScopeName |> shouldEqual DynamicAssemblyImage.manifestModuleName
            assy.ModuleVersionId |> shouldEqual mvid

            // Only `<Module>`, which `Assembly.GetTypes` and `Assembly.GetType` never report.
            assy.TypeDefs.Count |> shouldEqual 1
            assy.AssemblyReferences.Count |> shouldEqual 0

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.zip genName genGuid)) (fun (name, mvid) -> property name mvid)
        )

    [<Test>]
    let ``the image is a function of its arguments`` () : unit =
        let property (name : DynamicAssemblyName) (mvid : Guid) : unit =
            DynamicAssemblyImage.build name mvid
            |> shouldEqual (DynamicAssemblyImage.build name mvid)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 50,
            Prop.forAll (Arb.fromGen (Gen.zip genName genGuid)) (fun (name, mvid) -> property name mvid)
        )
