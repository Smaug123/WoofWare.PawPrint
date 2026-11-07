namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.PortableExecutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The references CoreCLR's emitter adds to a dynamic module for `ModuleBuilder_GetTypeRef`: each
/// `AssemblyRef` and `TypeRef` row is found among the rows already there before one is added, and an
/// `AssemblyRef` names the assembly whose own row it was made from.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmitScopeReferences =

    /// Every managed image in the test host's framework directory.
    let private frameworkImages : string list Lazy =
        lazy
            [
                for path in
                    Directory.GetFiles (Path.GetDirectoryName HostImage.coreLibPath, "*.dll")
                    |> Array.sort do
                    use pe = new PEReader (File.OpenRead path)

                    if pe.HasMetadata && pe.GetMetadataReader().IsAssembly then
                        yield path
            ]

    let private withReader (bytes : byte[]) (f : MetadataReader -> 'a) : 'a =
        use pe = new PEReader (ImmutableArray.Create<byte> bytes)
        f (pe.GetMetadataReader ())

    /// The display name of the assembly a reference made from `request` names.
    let private displayName (request : AssemblyRefRequest) : string =
        let name = AssemblyName request.Name

        name.Version <-
            Version (
                int request.Version.Major,
                int request.Version.Minor,
                int request.Version.Build,
                int request.Version.Revision
            )

        name.CultureName <- request.Culture
        name.SetPublicKeyToken (Seq.toArray request.PublicKeyToken)
        name.FullName

    [<Test>]
    let ``a reference made from an assembly's own row names that assembly`` () : unit =
        let unsigned =
            Roslyn.compileOptimizedAssembly
                "Unsigned"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [
                    "[assembly: System.Reflection.AssemblyVersion(\"1.2.3.4\")] public class C { }"
                ]

        let images =
            [
                for path in frameworkImages.Value -> path, File.ReadAllBytes path
                yield "Unsigned", unsigned
            ]

        // CoreLib and at least one other assembly, each with a key, and one without.
        List.length images |> shouldBeGreaterThan 3

        for path, bytes in images do
            withReader
                bytes
                (fun reader ->
                    let request = AssemblyRefRequest.ofDefinition reader
                    let definition = reader.GetAssemblyDefinition ()

                    displayName request |> shouldEqual (definition.GetAssemblyName().FullName)

                    // The key is reduced to its token, so the reference never says it holds a key.
                    request.Flags.HasFlag AssemblyFlags.PublicKey |> shouldEqual false

                    let key = reader.GetBlobContent definition.PublicKey

                    if key.IsEmpty then
                        request.PublicKeyToken.IsEmpty |> shouldEqual true
                    else
                        Seq.toArray request.PublicKeyToken
                        |> shouldEqual (AssemblyName.GetAssemblyName(path).GetPublicKeyToken ())
                )

    [<Test>]
    let ``a public key's token is the reversed tail of its SHA-1`` () : unit =
        // CoreLib's key, whose token every .NET display name of CoreLib carries.
        let key =
            withReader
                (File.ReadAllBytes HostImage.coreLibPath)
                (fun reader -> reader.GetBlobContent (reader.GetAssemblyDefinition ()).PublicKey)

        Seq.toArray (AssemblyRefRequest.publicKeyToken key)
        |> shouldEqual [| 0x7cuy ; 0xecuy ; 0x85uy ; 0xd7uy ; 0xbeuy ; 0xa7uy ; 0x79uy ; 0x8euy |]

    /// The characters a type name reserves, each of which a backslash escapes.
    let private reserved : char list = [ ',' ; '[' ; ']' ; '&' ; '*' ; '+' ; '\\' ]

    let private escape (name : string) : string =
        name
        |> Seq.map (fun c -> if List.contains c reserved then $"\\%c{c}" else string c)
        |> String.concat ""

    [<Test>]
    let ``unescaping a type name undoes escaping it`` () : unit =
        let genName =
            Gen.elements ([ 'a' ; 'B' ; '.' ; '`' ; '1' ; ' ' ] @ reserved)
            |> Gen.listOf
            |> Gen.map (fun cs -> System.String (Array.ofSeq cs))

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll
                (Arb.fromGen genName)
                (fun name -> EmitScopeReferences.unescapeTypeName (escape name) |> shouldEqual name)
        )

    [<TestCase("a\\b", "a\\b")>]
    [<TestCase("a\\", "a\\")>]
    [<TestCase("\\\\\\+", "\\+")>]
    [<TestCase("List`1", "List`1")>]
    [<TestCase("a+b", "a+b")>]
    let ``a backslash before a character no type name reserves is kept`` (escaped : string, expected : string) : unit =
        EmitScopeReferences.unescapeTypeName escaped |> shouldEqual expected

    [<TestCase("System.Object", "System", "Object")>]
    [<TestCase("A.B.C", "A.B", "C")>]
    [<TestCase("Name", "", "Name")>]
    [<TestCase("", "", "")>]
    [<TestCase(".ctor", "", ".ctor")>]
    [<TestCase("A..ctor", "A", ".ctor")>]
    [<TestCase("A.B..cctor", "A.B", ".cctor")>]
    [<TestCase("A.", "A", "")>]
    [<TestCase("..", "", ".")>]
    [<TestCase("A...B", "A.", ".B")>]
    let ``a full type name splits at its last separator`` (fullName : string, ns : string, name : string) : unit =
        EmitScopeReferences.splitTypeName fullName |> shouldEqual (ns, name)

    [<Test>]
    let ``a name with no doubled separator splits at its last dot, and joins back`` () : unit =
        let genName =
            Gen.elements [ 'a' ; 'B' ; '.' ; '`' ; '1' ]
            |> Gen.listOf
            |> Gen.map (fun cs -> System.String (Array.ofSeq cs))
            |> Gen.filter (fun s -> not (s.Contains ".."))

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 500,
            Prop.forAll
                (Arb.fromGen genName)
                (fun fullName ->
                    let ns, name = EmitScopeReferences.splitTypeName fullName
                    let last = fullName.LastIndexOf '.'

                    if last <= 0 then
                        (ns, name) |> shouldEqual ("", fullName)
                    else
                        $"%s{ns}.%s{name}" |> shouldEqual fullName
                        name.Contains '.' |> shouldEqual false
                )
        )

    let private emptyScope () : EmitScope = EmitScope.create "Dyn" Guid.Empty

    let private typeNames : string list =
        [ "A" ; "N.A" ; "N.B" ; "M.N.A" ; "N..ctor" ; ".ctor" ; "" ; "N." ; "n.a" ]

    /// A scope with two `AssemblyRef` rows, for type references to name.
    let private withTwoAssemblyRefs () : EmitScope =
        let request (name : string) : AssemblyRefRequest =
            {
                Name = name
                Version =
                    {
                        Major = 1us
                        Minor = 0us
                        Build = 0us
                        Revision = 0us
                    }
                Culture = ""
                PublicKeyToken = ImmutableArray.Empty
                Flags = enum<AssemblyFlags> 0
            }

        let scope = emptyScope ()
        let _, scope = EmitScopeReferences.defineAssemblyRef (request "One") scope
        let _, scope = EmitScopeReferences.defineAssemblyRef (request "Two") scope
        scope

    [<Test>]
    let ``a type reference is defined once for each scope, namespace and name, in first-asked order`` () : unit =
        let genOps : Gen<(int * int * int) list> =
            Gen.listOf (Gen.zip3 (Gen.choose (0, 2)) (Gen.choose (1, 1000)) (Gen.choose (0, typeNames.Length - 1)))
            |> Gen.map List.ofSeq

        let property (ops : (int * int * int) list) : unit =
            let start = withTwoAssemblyRefs ()

            // The rows defined so far, in RID order, as (scope, namespace, name).
            let mutable model : (ResolutionScope * string * string) list = []
            let mutable scope = start

            for kind, pick, nameIndex in ops do
                let resolution =
                    match kind with
                    | 0 -> ResolutionScope.Module
                    | 1 -> ResolutionScope.AssemblyRef (AssemblyRefRid (pick % 2 + 1))
                    | _ when model.IsEmpty -> ResolutionScope.Module
                    | _ -> ResolutionScope.TypeRef (TypeRefRid (pick % model.Length + 1))

                let fullName = typeNames.[nameIndex]
                let ns, name = EmitScopeReferences.splitTypeName fullName
                let key = resolution, ns, name

                let rid, next = EmitScopeReferences.defineTypeRefByName resolution fullName scope

                match List.tryFindIndex ((=) key) model with
                | Some index ->
                    rid |> shouldEqual (TypeRefRid (index + 1))
                    // Found, so nothing is added: no row, and no string.
                    (EmitScope.typeRefs next).Count |> shouldEqual (EmitScope.typeRefs scope).Count
                    (EmitScope.strings next).Count |> shouldEqual (EmitScope.strings scope).Count
                | None ->
                    model <- model @ [ key ]
                    rid |> shouldEqual (TypeRefRid model.Length)

                EmitScopeReferences.tryFindTypeRef resolution ns name next
                |> shouldEqual (Some rid)

                scope <- next

            let final = scope

            let rows =
                EmitScope.typeRefs final
                |> Seq.map (fun row ->
                    row.ResolutionScope, EmitScope.stringAt row.Namespace final, EmitScope.stringAt row.Name final
                )
                |> List.ofSeq

            rows |> shouldEqual model

            // Nothing but the TypeRef table and the string heap changes.
            EmitScope.assemblyRefs final |> shouldEqual (EmitScope.assemblyRefs start)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 300, Prop.forAll (Arb.fromGen genOps) property)

    [<Test>]
    let ``a type reference interns its namespace before its name`` () : unit =
        let _, scope =
            EmitScopeReferences.defineTypeRefByName ResolutionScope.Module "Space.Name" (emptyScope ())

        let strings = EmitScope.strings scope |> List.ofSeq
        strings.[strings.Length - 2 ..] |> shouldEqual [ "Space" ; "Name" ]

    let private token (bytes : byte list) : ImmutableArray<byte> = ImmutableArray.CreateRange bytes

    /// Requests that agree in some components and not others, so that a sequence of them both finds and
    /// adds rows.
    let private genRequest : Gen<AssemblyRefRequest> =
        gen {
            let! name = Gen.elements [ "Lib" ; "lib" ; "mscorlib" ; "MSCORLIB" ; "Microsoft.VisualC" ]
            let! culture = Gen.elements [ "" ; "fr" ]
            let versionPart = Gen.elements [ 0us ; 1us ; 65535us ]
            let! major = versionPart
            let! minor = versionPart
            let! build = versionPart
            let! revision = versionPart

            let! publicKeyToken =
                Gen.elements
                    [
                        ImmutableArray.Empty
                        token [ 1uy ; 2uy ; 3uy ; 4uy ; 5uy ; 6uy ; 7uy ; 8uy ]
                        token [ 8uy ; 7uy ; 6uy ; 5uy ; 4uy ; 3uy ; 2uy ; 1uy ]
                    ]

            // `EmitToken` passes `PublicKey` only for an assembly whose row has the flag but no key.
            let! flags =
                Gen.elements (
                    [ 0x000 ; 0x100 ; 0x010 ; 0x090 ; 0x0F0 ; 0x110 ]
                    @ (if publicKeyToken.IsEmpty then [ 0x001 ; 0x101 ] else [])
                )

            return
                {
                    Name = name
                    Version =
                        {
                            Major = major
                            Minor = minor
                            Build = build
                            Revision = revision
                        }
                    Culture = culture
                    PublicKeyToken = publicKeyToken
                    Flags = enum<AssemblyFlags> flags
                }
        }

    /// What a new row stores for a version versionPart: the emitter reads 65535 as "not given".
    let private stored (c : uint16) : uint16 = if c = UInt16.MaxValue then 0us else c

    /// Whether `ImportHelper::FindAssemblyRef` takes `row` for `request`.
    let private matches (request : AssemblyRefRequest) (row : AssemblyRefRequest) : bool =
        let unified =
            String.Equals (row.Name, "mscorlib", StringComparison.OrdinalIgnoreCase)
            || String.Equals (row.Name, "microsoft.visualc", StringComparison.OrdinalIgnoreCase)

        row.Name = request.Name
        && row.Culture = request.Culture
        && row.Version.Major = request.Version.Major
        && row.Version.Minor = request.Version.Minor
        && (unified
            || row.Version.Build = request.Version.Build
               && row.Version.Revision = request.Version.Revision)
        && row.PublicKeyToken.AsSpan().SequenceEqual (request.PublicKeyToken.AsSpan ())

    /// What a new row stores for the request's flags (`PrepareForSaving`).
    let private saved (flags : AssemblyFlags) : AssemblyFlags =
        let flags = int flags

        enum<AssemblyFlags> (
            if flags &&& 0x80 <> 0 then
                flags &&& ~~~0x80
            else
                flags &&& ~~~0xF0
        )

    [<Test>]
    let ``an assembly reference is found by name, culture, version and token, or added`` () : unit =
        let property (requests : AssemblyRefRequest list) : unit =
            // What each row holds, in RID order.
            let mutable model : AssemblyRefRequest list = []
            let mutable scope = emptyScope ()

            for request in requests do
                let rid, next = EmitScopeReferences.defineAssemblyRef request scope

                match List.tryFindIndex (matches request) model with
                | Some index ->
                    rid |> shouldEqual (AssemblyRefRid (index + 1))

                    (EmitScope.assemblyRefs next).Count
                    |> shouldEqual (EmitScope.assemblyRefs scope).Count

                    (EmitScope.strings next).Count |> shouldEqual (EmitScope.strings scope).Count
                    (EmitScope.blobs next).Count |> shouldEqual (EmitScope.blobs scope).Count
                | None ->
                    model <-
                        model
                        @ [
                            { request with
                                Version =
                                    {
                                        Major = stored request.Version.Major
                                        Minor = stored request.Version.Minor
                                        Build = stored request.Version.Build
                                        Revision = stored request.Version.Revision
                                    }
                                Flags = saved request.Flags
                            }
                        ]

                    rid |> shouldEqual (AssemblyRefRid model.Length)

                EmitScopeReferences.tryFindAssemblyRef request next
                |> shouldEqual (
                    List.tryFindIndex (matches request) model
                    |> Option.map (fun i -> AssemblyRefRid (i + 1))
                )

                // A row is for the request when it is found for it, or holds what defining it adds.
                let added =
                    { request with
                        Version =
                            {
                                Major = stored request.Version.Major
                                Minor = stored request.Version.Minor
                                Build = stored request.Version.Build
                                Revision = stored request.Version.Revision
                            }
                    }

                for index, row in List.indexed model do
                    let expected =
                        List.tryFindIndex (matches request) model = Some index
                        || row.Name = added.Name
                           && row.Culture = added.Culture
                           && row.Version = added.Version
                           && row.PublicKeyToken.AsSpan().SequenceEqual (added.PublicKeyToken.AsSpan ())
                           && row.Flags = saved request.Flags

                    EmitScopeReferences.isAssemblyRefFor request (AssemblyRefRid (index + 1)) next
                    |> shouldEqual expected

                scope <- next

            let final = scope

            let rows =
                EmitScope.assemblyRefs final
                |> Seq.map (fun row ->
                    (EmitScope.blobAt row.HashValue final).IsEmpty |> shouldEqual true

                    {
                        Name = EmitScope.stringAt row.Name final
                        Version = row.Version
                        Culture = EmitScope.stringAt row.Culture final
                        PublicKeyToken = EmitScope.blobAt row.PublicKeyOrToken final
                        Flags = row.Flags
                    }
                )
                |> List.ofSeq

            rows.Length |> shouldEqual model.Length

            for row, expected in List.zip rows model do
                row.Name |> shouldEqual expected.Name
                row.Version |> shouldEqual expected.Version
                row.Culture |> shouldEqual expected.Culture

                Seq.toArray row.PublicKeyToken
                |> shouldEqual (Seq.toArray expected.PublicKeyToken)

                row.Flags |> shouldEqual expected.Flags

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 300,
            Prop.forAll (Arb.fromGen (Gen.listOf genRequest |> Gen.map List.ofSeq)) property
        )

    [<Test>]
    let ``a version component of 65535 is never found again`` () : unit =
        let request : AssemblyRefRequest =
            {
                Name = "Lib"
                Version =
                    {
                        Major = 1us
                        Minor = 0us
                        Build = 65535us
                        Revision = 0us
                    }
                Culture = ""
                PublicKeyToken = ImmutableArray.Empty
                Flags = enum<AssemblyFlags> 0
            }

        let first, scope = EmitScopeReferences.defineAssemblyRef request (emptyScope ())
        let second, scope = EmitScopeReferences.defineAssemblyRef request scope
        (first, second) |> shouldEqual (AssemblyRefRid 1, AssemblyRefRid 2)
        (EmitScope.assemblyRefs scope).[0].Version.Build |> shouldEqual 0us
        // Each is a row defining the request adds, so each may be recorded as naming its assembly.
        EmitScopeReferences.isAssemblyRefFor request first scope |> shouldEqual true
        EmitScopeReferences.isAssemblyRefFor request second scope |> shouldEqual true

    /// `RegMeta::GetTokenFromSig`, which `TypeBuilder_SetMethodIL` calls for a body's local signature:
    /// in a dynamic module, whose emitter checks for duplicate signatures, the first `StandAloneSig`
    /// row with the same bytes (`ImportHelper::FindStandAloneSig`), or a new one. Real .NET's
    /// `ModuleBuilder.GetSignatureMetadataToken` reaches the same function, so it is the oracle.
    [<Test>]
    let ``a local signature is found among the StandAloneSig rows before one is added, as real .NET finds it``
        ()
        : unit
        =
        let localTypes : Type list list =
            [
                []
                [ typeof<int> ]
                [ typeof<int> ; typeof<string> ]
                [ typeof<string> ]
                [ typeof<int> ; typeof<int> ]
            ]

        let found = ref 0
        let added = ref 0

        let property (choices : int list) : unit =
            let assembly =
                System.Reflection.Emit.AssemblyBuilder.DefineDynamicAssembly (
                    AssemblyName "StandAloneSigOracle",
                    System.Reflection.Emit.AssemblyBuilderAccess.Run
                )

            let module' = assembly.DefineDynamicModule "StandAloneSigOracle"

            let signatureOf (types : Type list) =
                let helper = System.Reflection.Emit.SignatureHelper.GetLocalVarSigHelper module'

                for t in types do
                    helper.AddArgument t

                helper

            let mutable scope = EmitScope.create "StandAloneSigOracle" Guid.Empty

            for choice in choices do
                let helper = signatureOf localTypes.[choice % localTypes.Length]
                let bytes = ImmutableArray.Create<byte> (helper.GetSignature ())
                let expected = module'.GetSignatureMetadataToken helper
                let before = (EmitScope.standAloneSigs scope).Count
                let rid, next = EmitScopeReferences.defineStandAloneSig bytes scope

                0x11000000 ||| rid.Rid |> shouldEqual expected

                if (EmitScope.standAloneSigs next).Count = before then
                    found.Value <- found.Value + 1
                    Object.ReferenceEquals (next, scope) |> shouldEqual true
                else
                    added.Value <- added.Value + 1
                    (EmitScope.standAloneSigs next).Count |> shouldEqual (before + 1)

                    EmitScope.blobAt (EmitScope.standAloneSigs next).[rid.Rid - 1].Signature next
                    |> List.ofSeq
                    |> shouldEqual (List.ofSeq bytes)

                scope <- next

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 100,
            Prop.forAll (Arb.fromGen (Gen.listOf (Gen.choose (0, 100)) |> Gen.map List.ofSeq)) property
        )

        found.Value |> shouldBeGreaterThan 100
        added.Value |> shouldBeGreaterThan 100

    /// `RegMeta::GetTokenFromTypeSpec`, which `ModuleBuilder_GetTokenFromTypeSpec` calls: in a dynamic
    /// module, whose emitter checks for duplicate TypeSpecs (`MDDupTypeSpec`), the first TypeSpec row
    /// with the same signature, or a new one. Real .NET's `ModuleBuilder.GetTypeMetadataToken` reaches it
    /// for a generic instantiation or an array, so it is the oracle, and gives the signature too.
    [<Test>]
    let ``a TypeSpec is found among the rows before one is added, as real .NET finds it`` () : unit =
        let types : Type list =
            [
                typeof<Collections.Generic.List<int>>
                typeof<Collections.Generic.List<string>>
                typeof<Collections.Generic.Dictionary<string, int>>
                typeof<int[]>
                typeof<string[,]>
            ]

        let found = ref 0
        let added = ref 0

        let property (choices : int list) : unit =
            let assembly =
                System.Reflection.Emit.AssemblyBuilder.DefineDynamicAssembly (
                    AssemblyName "TypeSpecOracle",
                    System.Reflection.Emit.AssemblyBuilderAccess.Run
                )

            let module' = assembly.DefineDynamicModule "TypeSpecOracle"
            let mutable scope = EmitScope.create "TypeSpecOracle" Guid.Empty

            for choice in choices do
                let t = types.[choice % types.Length]
                let expected = module'.GetTypeMetadataToken t
                let bytes = ImmutableArray.Create<byte> (module'.ResolveSignature expected)
                let before = (EmitScope.typeSpecs scope).Count
                let rid, next = EmitScopeReferences.defineTypeSpec bytes scope

                0x1B000000 ||| rid.Rid |> shouldEqual expected

                if (EmitScope.typeSpecs next).Count = before then
                    found.Value <- found.Value + 1
                    Object.ReferenceEquals (next, scope) |> shouldEqual true
                else
                    added.Value <- added.Value + 1

                    EmitScope.blobAt (EmitScope.typeSpecs next).[rid.Rid - 1].Signature next
                    |> List.ofSeq
                    |> shouldEqual (List.ofSeq bytes)

                scope <- next

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 100,
            Prop.forAll (Arb.fromGen (Gen.listOf (Gen.choose (0, 100)) |> Gen.map List.ofSeq)) property
        )

        found.Value |> shouldBeGreaterThan 100
        added.Value |> shouldBeGreaterThan 100
