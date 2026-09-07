namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for the signature comparison behind the <c>Signature_AreEqual</c> QCall, which CoreCLR
/// implements as <c>MetaSig::CompareMethodSigs</c> with both <c>Substitution</c>s null.
/// </summary>
/// <remarks>
/// The end-to-end guests (<c>ReflectionPropertyHiding.cs</c> and its cross-module sibling) pin what
/// a guest can observe. These tests reach what no C# guest can: a signature mentioning a generic
/// parameter (properties on generic types are blocked well before this comparison, on
/// <c>ModuleHandle.ResolveMethod</c>), a modopt (which C# cannot emit), and two assemblies whose
/// property blobs collide byte for byte.
///
/// The corpus is a Roslyn-compiled assembly rather than corelib, because corelib contains no
/// TypeRef rows at all — every one of its property blobs spells its types as TypeDefs, so a
/// corelib-only corpus cannot exercise the reference-resolution arm that distinguishes this
/// comparison from a byte comparison.
/// </remarks>
[<TestFixture>]
module TestSignatureComparison =

    let private corpusSource =
        """
using System;
using System.Collections.Generic;

public class Corpus
{
    // Two properties whose types are spelled as TypeRefs into corelib.
    public DateTime WhenA { get; set; }
    public DateTime WhenB { get; set; }
    public TimeSpan HowLong { get; set; }

    // A generic instantiation, whose arguments are compared pairwise.
    public List<int> Ints { get; set; }
    public List<string> Strings { get; set; }

    // Primitives, which compare by element type alone.
    public int Number { get; set; }
    public long Bigger { get; set; }

    // `string` is ELEMENT_TYPE_STRING, not a token naming System.String.
    public string Text { get; set; }

    // Indexers, so parameter lists differ rather than property types.
    public int this[int i] { get { return i; } }
    public int this[long j] { get { return (int)j; } }
    public int this[int i, int k] { get { return i + k; } }

    // A static property, which carries no HASTHIS in its calling convention.
    public static int Stat { get; set; }

    // Custom modifiers: `ref readonly` puts a modreq(InAttribute) on the property's type.
    private static int _cell;
    public ref readonly int RefReadonly => ref _cell;
    public ref int RefMutable => ref _cell;
}

public class Generic<T>
{
    // `VAR 0` — a class type parameter, which the comparison must treat symbolically.
    public T Value { get; set; }
    public T[] Values { get; set; }
    public List<T> ListOfT { get; set; }
}

public class Generic2<T, U>
{
    // `VAR 0` and `VAR 1`, so index comparison is observable.
    public T First { get; set; }
    public U Second { get; set; }
}
"""

    type private Fixture =
        {
            LoggerFactory : Microsoft.Extensions.Logging.ILoggerFactory
            Assembly : DumpedAssembly
            Corelib : DumpedAssembly
            Assemblies : LoadedAssemblies
        }

    let private makeFixture () : Fixture =
        let image =
            Roslyn.compileAssembly
                "SignatureComparisonTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ corpusSource ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory typeof<obj>.Assembly.Location

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        {
            LoggerFactory = loggerFactory
            Assembly = assembly
            Corelib = corelib
            Assemblies = LoadedAssemblies.empty.WithLoadedAssembly(assembly).WithLoadedAssembly corelib
        }

    let private fixture : Fixture = makeFixture ()

    /// The decoded PropertySig of a named property on a named type of the corpus assembly.
    let private signature (typeName : string) (propertyName : string) : MethodSignature<TypeDefn> =
        let metadataReader = fixture.Assembly.PeReader.GetMetadataReader ()

        let typeDef =
            fixture.Assembly.TypeDefs.Values
            |> Seq.filter (fun td -> td.Name = typeName)
            |> Seq.toList
            |> function
                | [ one ] -> one
                | [] -> failwith $"no type named %s{typeName} in the corpus assembly"
                | many -> failwith $"%d{List.length many} types named %s{typeName} in the corpus assembly"

        let candidates =
            (metadataReader.GetTypeDefinition typeDef.TypeDefHandle).GetProperties ()
            |> Seq.filter (fun handle ->
                metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = propertyName
            )
            |> Seq.toList

        match candidates with
        | [ one ] ->
            PropertySignatureDecoding.decode
                fixture.Assembly.Name
                metadataReader
                (metadataReader.GetPropertyDefinition one).Signature
        | [] -> failwith $"no property named %s{propertyName} on %s{typeName}"
        | many -> failwith $"%d{List.length many} properties named %s{propertyName} on %s{typeName}"

    /// Every same-named property of a corpus type, in metadata order — for indexers, which share
    /// the name `Item`.
    let private overloads (typeName : string) (propertyName : string) : MethodSignature<TypeDefn> list =
        let metadataReader = fixture.Assembly.PeReader.GetMetadataReader ()

        let typeDef =
            fixture.Assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = typeName)

        (metadataReader.GetTypeDefinition typeDef.TypeDefHandle).GetProperties ()
        |> Seq.filter (fun handle ->
            metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = propertyName
        )
        |> Seq.map (fun handle ->
            PropertySignatureDecoding.decode
                fixture.Assembly.Name
                metadataReader
                (metadataReader.GetPropertyDefinition handle).Signature
        )
        |> Seq.toList

    /// No runtime directories: an assembly a test wants resolvable is loaded up front, and any
    /// attempt to load one that is not fails outright.
    let private noRuntimeDirs : string seq = Seq.empty

    let private compare (left : MethodSignature<TypeDefn>) (right : MethodSignature<TypeDefn>) : bool =
        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            fixture.Assemblies
            fixture.Assembly
            left
            fixture.Assembly
            right
        |> snd

    [<Test>]
    let ``a signature equals itself`` () : unit =
        compare (signature "Corpus" "Number") (signature "Corpus" "Number")
        |> shouldEqual true

    [<Test>]
    let ``two properties of the same primitive type are equal`` () : unit =
        // Distinct Property rows, so this is not an identity check.
        compare (signature "Corpus" "WhenA") (signature "Corpus" "WhenB")
        |> shouldEqual true

    [<Test>]
    let ``different primitive types are unequal`` () : unit =
        compare (signature "Corpus" "Number") (signature "Corpus" "Bigger")
        |> shouldEqual false

    [<Test>]
    let ``different referenced types are unequal`` () : unit =
        // Both are TypeRefs into corelib, so this needs each reference resolved to what it names
        // rather than compared as a token.
        compare (signature "Corpus" "WhenA") (signature "Corpus" "HowLong")
        |> shouldEqual false

    [<Test>]
    let ``a referenced type does not equal a primitive`` () : unit =
        compare (signature "Corpus" "WhenA") (signature "Corpus" "Number")
        |> shouldEqual false

    [<Test>]
    let ``ELEMENT_TYPE_STRING does not equal a referenced value type`` () : unit =
        compare (signature "Corpus" "Text") (signature "Corpus" "WhenA")
        |> shouldEqual false

    [<Test>]
    let ``generic instantiations differing in an argument are unequal`` () : unit =
        compare (signature "Corpus" "Ints") (signature "Corpus" "Strings")
        |> shouldEqual false

    [<Test>]
    let ``a generic instantiation equals itself`` () : unit =
        compare (signature "Corpus" "Ints") (signature "Corpus" "Ints")
        |> shouldEqual true

    [<Test>]
    let ``custom modifiers are compared`` () : unit =
        // `ref readonly int` and `ref int` differ only by a modreq(InAttribute) on the property's
        // type. `CompareState.IgnoreCustomModifiers` defaults to false on this path, so CoreCLR
        // calls them different; a comparison that stripped modifiers would see two `int&`s.
        compare (signature "Corpus" "RefReadonly") (signature "Corpus" "RefMutable")
        |> shouldEqual false

    [<Test>]
    let ``a modified type equals itself`` () : unit =
        compare (signature "Corpus" "RefReadonly") (signature "Corpus" "RefReadonly")
        |> shouldEqual true

    [<Test>]
    let ``a static property does not equal an instance one of the same type`` () : unit =
        // The calling-convention byte differs by HASTHIS alone; both are `int` properties with no
        // index parameters, so nothing else distinguishes them.
        let stat = signature "Corpus" "Stat"
        let instance = signature "Corpus" "Number"

        stat.Header.IsInstance |> shouldEqual false
        instance.Header.IsInstance |> shouldEqual true

        compare stat instance |> shouldEqual false

    [<Test>]
    let ``indexers differing in a parameter type are unequal`` () : unit =
        let byInt, byLong =
            match overloads "Corpus" "Item" with
            | [ a ; b ; _ ] -> a, b
            | other -> failwith $"expected three Item overloads, got %d{List.length other}"

        byInt.ParameterTypes
        |> List.ofSeq
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]

        byLong.ParameterTypes
        |> List.ofSeq
        |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.Int64 ]

        compare byInt byLong |> shouldEqual false

    [<Test>]
    let ``indexers differing in parameter count are unequal`` () : unit =
        let byInt, byTwo =
            match overloads "Corpus" "Item" with
            | [ a ; _ ; c ] -> a, c
            | other -> failwith $"expected three Item overloads, got %d{List.length other}"

        compare byInt byTwo |> shouldEqual false

    // The generic arms below are the ones no guest can reach: a property declared on a generic type
    // stops in `ModuleHandle.ResolveMethod` long before any signature is compared. They are what
    // pins the comparison as *symbolic* — CoreCLR passes null Substitutions, so a type parameter is
    // compared by index and never resolved to what an instantiation would supply.

    [<Test>]
    let ``a generic parameter equals the same-indexed generic parameter`` () : unit =
        compare (signature "Generic`1" "Value") (signature "Generic`1" "Value")
        |> shouldEqual true

    [<Test>]
    let ``generic parameters at different indices are unequal`` () : unit =
        let first = signature "Generic2`2" "First"
        let second = signature "Generic2`2" "Second"

        first.ReturnType |> shouldEqual (TypeDefn.GenericTypeParameter 0)
        second.ReturnType |> shouldEqual (TypeDefn.GenericTypeParameter 1)

        compare first second |> shouldEqual false

    [<Test>]
    let ``a generic parameter does not equal a concrete type`` () : unit =
        // This is the divergence that rules out comparing *concretized* signatures: under the
        // instantiation `Generic<int>`, concretizing both sides would make these equal, and the
        // base property would be silently dropped from a reflection query. CoreCLR compares
        // `VAR 0` against `ELEMENT_TYPE_I4` and says different.
        compare (signature "Generic`1" "Value") (signature "Corpus" "Number")
        |> shouldEqual false

    [<Test>]
    let ``a generic parameter under a structural type is compared symbolically`` () : unit =
        // `T[]` versus `List<T>`: same parameter, different surrounding structure.
        compare (signature "Generic`1" "Values") (signature "Generic`1" "ListOfT")
        |> shouldEqual false

    [<Test>]
    let ``a generic parameter nested in an instantiation equals itself`` () : unit =
        compare (signature "Generic`1" "ListOfT") (signature "Generic`1" "ListOfT")
        |> shouldEqual true

    [<Test>]
    let ``List of T does not equal List of int`` () : unit =
        compare (signature "Generic`1" "ListOfT") (signature "Corpus" "Ints")
        |> shouldEqual false

    // Two assemblies of identical shape, each declaring its own `Payload` type and a property of
    // it. Because the two images have the same structure their TypeDef tokens coincide, so the two
    // PropertySig blobs are byte-identical while naming genuinely different types. This is what the
    // byte-equality fast path must not be allowed to answer.

    let private collisionSource =
        """
public class Payload { }

public class Holder
{
    public Payload P { get; set; }
}
"""

    /// The assembly, the raw PropertySig bytes, and the decoded signature of `Holder.P` in a
    /// freshly compiled assembly of the given name.
    let private collisionOperand (assemblyName : string) : DumpedAssembly * byte[] * MethodSignature<TypeDefn> =
        let image =
            Roslyn.compileAssembly
                assemblyName
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ collisionSource ]

        use stream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None stream

        let metadataReader = assembly.PeReader.GetMetadataReader ()

        let holder = assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

        let propertyHandle =
            (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
            |> Seq.find (fun handle ->
                metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = "P"
            )

        let blob = (metadataReader.GetPropertyDefinition propertyHandle).Signature

        assembly, metadataReader.GetBlobBytes blob, PropertySignatureDecoding.decode assembly.Name metadataReader blob

    [<Test>]
    let ``byte-identical blobs in different assemblies name different types`` () : unit =
        let leftAssembly, leftBytes, left = collisionOperand "SignatureCollisionLeft"
        let rightAssembly, rightBytes, right = collisionOperand "SignatureCollisionRight"

        // The premise of the test. If Roslyn ever laid these images out differently the blobs would
        // stop colliding and the assertion below would pass without exercising the fast path's
        // gate at all, so check the premise rather than assume it.
        leftBytes |> shouldEqual rightBytes
        leftAssembly.Name.FullName |> shouldNotEqual rightAssembly.Name.FullName

        let assemblies =
            fixture.Assemblies.WithLoadedAssembly(leftAssembly).WithLoadedAssembly rightAssembly

        // Each `Payload` is a TypeDef in its own assembly, so these are different types despite the
        // identical bytes. A fast path that compared bytes without checking the assembly would say
        // they are equal.
        SignatureComparison.signaturesAreEqual
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            leftAssembly
            leftBytes
            left
            rightAssembly
            rightBytes
            right
        |> snd
        |> shouldEqual false

    [<Test>]
    let ``byte-identical blobs in the same assembly are equal`` () : unit =
        // The other side of the gate: within one assembly the tokens do mean the same thing, so the
        // fast path's answer is the one the structural comparison would reach anyway.
        let assembly, bytes, decoded = collisionOperand "SignatureCollisionSame"

        let assemblies = fixture.Assemblies.WithLoadedAssembly assembly

        SignatureComparison.signaturesAreEqual
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            assembly
            bytes
            decoded
            assembly
            bytes
            decoded
        |> snd
        |> shouldEqual true

    // A modopt cannot be produced from C#, so the required-versus-optional distinction is reached
    // by building the two signatures directly. `CompareElementType` fails a CMOD_REQD against a
    // CMOD_OPT on the element type alone, before it ever compares the modifier's token.

    /// `Corpus.RefReadonly`'s signature, with the required flag on its custom modifier set as
    /// asked. Derived from a real decoded signature rather than hand-built, so the modifier is a
    /// reference the resolver can actually follow — which is what the equal cases below need.
    let private refWithModifier (isRequired : bool) : MethodSignature<TypeDefn> =
        let original = signature "Corpus" "RefReadonly"

        // The modifier sits *outside* the byref: `ELEMENT_TYPE_CMOD_REQD` precedes the type it
        // modifies in the blob, so `ref readonly int` decodes as modreq(In) applied to `int&`.
        let modified =
            match original.ReturnType with
            | TypeDefn.Modified modifier ->
                TypeDefn.Modified
                    { modifier with
                        IsRequired = isRequired
                    }
            | other -> failwith $"expected `ref readonly int` to decode as a modified type, got %O{other}"

        MethodSignature<TypeDefn> (
            original.Header,
            modified,
            original.RequiredParameterCount,
            original.GenericParameterCount,
            original.ParameterTypes
        )

    [<Test>]
    let ``a modreq does not equal a modopt`` () : unit =
        // Same modifier type, same underlying type; only the required flag differs.
        compare (refWithModifier true) (refWithModifier false) |> shouldEqual false

    [<Test>]
    let ``a modreq equals the same modreq`` () : unit =
        compare (refWithModifier true) (refWithModifier true) |> shouldEqual true

    [<Test>]
    let ``a modopt equals the same modopt`` () : unit =
        compare (refWithModifier false) (refWithModifier false) |> shouldEqual true

    [<Test>]
    let ``comparing a reference does not load its target's base chain`` () : unit =
        // Deciding *which* type a reference names is metadata only, so it must not depend on
        // whether some assembly reachable from that type's base chain can be loaded. CoreCLR draws
        // the same line: its token comparison uses `ClassLoader::ResolveTokenToTypeDefThrowing`,
        // which loads no types.
        //
        // Three assemblies: `Holder.P` is typed `Mid`, and `Mid`'s base `TheBase` lives in a third
        // assembly which this state deliberately never loads and cannot find — the runtime-dir list
        // is empty, so any attempt to load it fails outright. Resolving the property's type to an
        // identity must therefore succeed while resolving it to a *usable* type could not.
        let baseImage =
            Roslyn.compileAssembly
                "SignatureBaseChainBase"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class TheBase { }" ]

        let midImage =
            Roslyn.compileAssembly
                "SignatureBaseChainMid"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage baseImage ]
                [ "public class Mid : TheBase { }" ]

        let userImage =
            Roslyn.compileAssembly
                "SignatureBaseChainUser"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage midImage ]
                [
                    "public class Holder { public Mid P { get; set; } public Mid Q { get; set; } }"
                ]

        let read (image : byte[]) : DumpedAssembly =
            use stream = new MemoryStream (image)
            global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None stream

        let mid = read midImage
        let user = read userImage
        let metadataReader = user.PeReader.GetMetadataReader ()

        let holder = user.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

        let signatureNamed (name : string) : MethodSignature<TypeDefn> =
            let handle =
                (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
                |> Seq.find (fun handle ->
                    metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = name
                )

            PropertySignatureDecoding.decode
                user.Name
                metadataReader
                (metadataReader.GetPropertyDefinition handle).Signature

        // `Mid` is spelled as a TypeRef here, so answering this needs the reference resolved.
        match (signatureNamed "P").ReturnType with
        | TypeDefn.FromReference _ -> ()
        | other -> failwith $"expected `Mid` to be spelled as a TypeRef in the user assembly, got %O{other}"

        // No runtime dirs, and `SignatureBaseChainBase` never loaded: `TheBase` is unreachable.
        let assemblies =
            LoadedAssemblies.empty.WithLoadedAssembly(user).WithLoadedAssembly mid

        // Distinct Property rows of the same type, so this is a real comparison rather than an
        // identity check, and both sides resolve the same reference.
        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            user
            (signatureNamed "P")
            user
            (signatureNamed "Q")
        |> snd
        |> shouldEqual true

    [<Test>]
    let ``the same reference spelled in two modules names different types`` () : unit =
        // The other side of the same-token shortcut's gate. Two assemblies each reference a
        // `Shared.Thing`, but from *different* defining assemblies, so the parsed references can be
        // identical — same name, same namespace, same AssemblyRef row index — while naming
        // genuinely different types. A shortcut that skipped the assembly check would say equal.
        let definition = "namespace Shared { public class Thing { } }"

        let user = "public class Holder { public Shared.Thing P { get; set; } }"

        let read (image : byte[]) : DumpedAssembly =
            use stream = new MemoryStream (image)
            global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None stream

        // The user assembly, and the assembly its `Shared.Thing` actually comes from.
        let compileUser (name : string) (definingAssemblyName : string) : DumpedAssembly * DumpedAssembly =
            let definingImage =
                Roslyn.compileAssembly
                    definingAssemblyName
                    Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                    []
                    [ definition ]

            let image =
                Roslyn.compileAssembly
                    name
                    Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                    [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage definingImage ]
                    [ user ]

            read image, read definingImage

        let left, leftDefining = compileUser "SigRefCollisionLeft" "SigRefCollisionDefA"
        let right, rightDefining = compileUser "SigRefCollisionRight" "SigRefCollisionDefB"

        let propertyOf (assembly : DumpedAssembly) : MethodSignature<TypeDefn> =
            let metadataReader = assembly.PeReader.GetMetadataReader ()
            let holder = assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

            let handle =
                (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
                |> Seq.exactlyOne

            PropertySignatureDecoding.decode
                assembly.Name
                metadataReader
                (metadataReader.GetPropertyDefinition handle).Signature

        let leftSig = propertyOf left
        let rightSig = propertyOf right

        // The premise: the two parsed references must actually be equal, or the assertion below
        // would pass without the gate being what decided it.
        match leftSig.ReturnType, rightSig.ReturnType with
        | TypeDefn.FromReference (lRef, _), TypeDefn.FromReference (rRef, _) -> lRef |> shouldEqual rRef
        | l, r -> failwith $"expected both to be TypeRefs, got %O{l} and %O{r}"

        // Both defining assemblies loaded, so each reference resolves to its own `Shared.Thing` and
        // the two identities are what separate them.
        let assemblies =
            LoadedAssemblies.empty
                .WithLoadedAssembly(left)
                .WithLoadedAssembly(right)
                .WithLoadedAssembly(leftDefining)
                .WithLoadedAssembly
                rightDefining

        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            left
            leftSig
            right
            rightSig
        |> snd
        |> shouldEqual false

    [<Test>]
    let ``the same reference in one module compares equal without resolving it`` () : unit =
        // `CompareTypeTokens`'s first step is same-module-and-same-token, answered before any
        // resolution. Two property blobs in one assembly that name a type through the *same*
        // TypeRef row therefore agree even when nothing can resolve that row.
        //
        // Reachable: `Signature_Init` strips custom modifiers without loading their types, so two
        // signatures can carry the same modifier from an assembly that was never loaded.
        let midImage =
            Roslyn.compileAssembly
                "SignatureSameTokenMid"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class Mid { }" ]

        let userImage =
            Roslyn.compileAssembly
                "SignatureSameTokenUser"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage midImage ]
                [
                    "public class Holder { public Mid P { get; set; } public Mid Q { get; set; } }"
                ]

        use userStream = new MemoryStream (userImage)

        let user =
            global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None userStream

        let metadataReader = user.PeReader.GetMetadataReader ()
        let holder = user.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

        let signatureNamed (name : string) : MethodSignature<TypeDefn> =
            let handle =
                (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
                |> Seq.find (fun handle ->
                    metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = name
                )

            PropertySignatureDecoding.decode
                user.Name
                metadataReader
                (metadataReader.GetPropertyDefinition handle).Signature

        // `SignatureSameTokenMid` is neither loaded nor on the (empty) runtime-dir list, so any
        // attempt to resolve this reference fails outright.
        let assemblies = LoadedAssemblies.empty.WithLoadedAssembly user

        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            user
            (signatureNamed "P")
            user
            (signatureNamed "Q")
        |> snd
        |> shouldEqual true

    [<Test>]
    let ``two rows describing one type are not the same reference`` () : unit =
        // The shortcut above is keyed on the *row*, not on what the row says. Two TypeRef rows of
        // one module may describe the same type — `CompareTypeTokens` misses `tk1 == tk2` for them
        // and falls through to resolution, so this must too.
        //
        // Roslyn emits one row per referenced type, so the duplicate is built here: the same
        // reference under a row number the corpus does not use. Its target cannot be resolved,
        // which is what tells the two behaviours apart — a shortcut keyed on the description would
        // answer "equal" without resolving, where falling through has to try.
        let midImage =
            Roslyn.compileAssembly
                "SignatureTwoRowsMid"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class Mid { }" ]

        let userImage =
            Roslyn.compileAssembly
                "SignatureTwoRowsUser"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage midImage ]
                [ "public class Holder { public Mid P { get; set; } }" ]

        use userStream = new MemoryStream (userImage)

        let user =
            global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None userStream

        let metadataReader = user.PeReader.GetMetadataReader ()
        let holder = user.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

        let original =
            let handle =
                (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
                |> Seq.exactlyOne

            PropertySignatureDecoding.decode
                user.Name
                metadataReader
                (metadataReader.GetPropertyDefinition handle).Signature

        let typeRef, kind =
            match original.ReturnType with
            | TypeDefn.FromReference (typeRef, kind) -> typeRef, kind
            | other -> failwith $"expected `Mid` to be spelled as a TypeRef, got %O{other}"

        // Same description, different row.
        let duplicateRow =
            let moved =
                { typeRef with
                    Handle = ComparableTypeReferenceHandle.Make (MetadataTokens.TypeReferenceHandle 0x00FFFFFF)
                }

            MethodSignature<TypeDefn> (
                original.Header,
                TypeDefn.FromReference (moved, kind),
                original.RequiredParameterCount,
                original.GenericParameterCount,
                original.ParameterTypes
            )

        // The premise: identical but for the row.
        match duplicateRow.ReturnType with
        | TypeDefn.FromReference (moved, _) ->
            moved.Name |> shouldEqual typeRef.Name
            moved.Namespace |> shouldEqual typeRef.Namespace
            moved.ResolutionScope |> shouldEqual typeRef.ResolutionScope
            moved.Handle |> shouldNotEqual typeRef.Handle
        | other -> failwith $"expected a TypeRef, got %O{other}"

        let assemblies = LoadedAssemblies.empty.WithLoadedAssembly user

        // Falling through to resolution is the point, and resolution cannot succeed here:
        // `SignatureTwoRowsMid` is neither loaded nor findable. Taking the shortcut would instead
        // answer "equal" without ever looking.
        let exn =
            Assert.Throws<exn> (fun () ->
                SignatureComparison.compareDecodedSignatures
                    fixture.LoggerFactory
                    noRuntimeDirs
                    "test"
                    assemblies
                    user
                    duplicateRow
                    user
                    original
                |> ignore
            )

        exn.Message |> shouldContainText "SignatureTwoRowsMid"

    [<Test>]
    let ``nested types are separated by their enclosing names, without resolving`` () : unit =
        // A nested type's row carries an empty namespace and only its own leaf name, its enclosing
        // type living in the resolution scope. So `OuterA+Shared` and `OuterB+Shared` look
        // identical to anything comparing the leaf alone, and would be left to resolution —
        // whereupon an assembly nobody can load decides whether the comparison succeeds at all.
        // `CompareTypeTokens` recurses on the scope and separates them by name.
        let midImage =
            Roslyn.compileAssembly
                "SignatureNestedMid"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [
                    "public class OuterA { public class Shared { } }\npublic class OuterB { public class Shared { } }"
                ]

        let userImage =
            Roslyn.compileAssembly
                "SignatureNestedUser"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage midImage ]
                [
                    "public class Holder { public OuterA.Shared P { get; set; } public OuterB.Shared Q { get; set; } }"
                ]

        use userStream = new MemoryStream (userImage)

        let user =
            global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None userStream

        let metadataReader = user.PeReader.GetMetadataReader ()
        let holder = user.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

        let signatureNamed (name : string) : MethodSignature<TypeDefn> =
            let handle =
                (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
                |> Seq.find (fun handle ->
                    metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = name
                )

            PropertySignatureDecoding.decode
                user.Name
                metadataReader
                (metadataReader.GetPropertyDefinition handle).Signature

        // The premise: both rows really do describe a type called `Shared` with no namespace, so
        // only the enclosing name tells them apart.
        for name in [ "P" ; "Q" ] do
            match (signatureNamed name).ReturnType with
            | TypeDefn.FromReference (typeRef, _) ->
                typeRef.Name |> shouldEqual "Shared"
                typeRef.Namespace |> shouldEqual ""
            | other -> failwith $"expected a nested TypeRef for %s{name}, got %O{other}"

        // `SignatureNestedMid` is neither loaded nor findable, so anything that reached resolution
        // would abort rather than answer.
        let assemblies = LoadedAssemblies.empty.WithLoadedAssembly user

        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            user
            (signatureNamed "P")
            user
            (signatureNamed "Q")
        |> snd
        |> shouldEqual false

    /// The decoded PropertySig of the single property named on `Holder` in the given assembly.
    let private holderProperty (assembly : DumpedAssembly) (name : string) : MethodSignature<TypeDefn> =
        let metadataReader = assembly.PeReader.GetMetadataReader ()
        let holder = assembly.TypeDefs.Values |> Seq.find (fun td -> td.Name = "Holder")

        let handle =
            (metadataReader.GetTypeDefinition holder.TypeDefHandle).GetProperties ()
            |> Seq.find (fun handle ->
                metadataReader.GetString (metadataReader.GetPropertyDefinition handle).Name = name
            )

        PropertySignatureDecoding.decode
            assembly.Name
            metadataReader
            (metadataReader.GetPropertyDefinition handle).Signature

    let private read (image : byte[]) : DumpedAssembly =
        use stream = new MemoryStream (image)
        global.WoofWare.PawPrint.AssemblyApi.read fixture.LoggerFactory None stream

    [<Test>]
    let ``a reference whose bound assembly does not declare the type compares unequal`` () : unit =
        // `CompareTypeTokens` resolves each side with `ClassLoader::ResolveTokenToTypeDefThrowing`,
        // which returns FALSE when the assembly it binds does not declare the name, and the
        // comparison then answers FALSE. That is a different fact from an assembly that cannot be
        // bound at all, which throws there and fails loudly here.
        //
        // Two user assemblies compiled against a `SignatureMissingTypeDef` that declares `Mid`,
        // then compared with a build of that same assembly identity which does not. Cross-module,
        // so the same-token shortcut cannot answer and resolution is reached.
        let realDefinition =
            Roslyn.compileAssembly
                "SignatureMissingTypeDef"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class Mid { }" ]

        let standIn =
            Roslyn.compileAssembly
                "SignatureMissingTypeDef"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class Other { }" ]

        let compileUser (name : string) : DumpedAssembly =
            Roslyn.compileAssembly
                name
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage realDefinition ]
                [ "public class Holder { public Mid P { get; set; } }" ]
            |> read

        let left = compileUser "SignatureMissingTypeLeft"
        let right = compileUser "SignatureMissingTypeRight"
        let leftSig = holderProperty left "P"
        let rightSig = holderProperty right "P"

        let compareUnder (definition : DumpedAssembly) : bool =
            let assemblies =
                LoadedAssemblies.empty.WithLoadedAssembly(left).WithLoadedAssembly(right).WithLoadedAssembly definition

            SignatureComparison.compareDecodedSignatures
                fixture.LoggerFactory
                noRuntimeDirs
                "test"
                assemblies
                left
                leftSig
                right
                rightSig
            |> snd

        // The control: with the assembly that does declare `Mid`, the two references resolve to
        // one definition. So what the stand-in changes below is the resolution's answer alone.
        compareUnder (read realDefinition) |> shouldEqual true

        // The stand-in carries the same definition identity, so the reference binds to it, and
        // then finds no `Mid` there.
        compareUnder (read standIn) |> shouldEqual false

    [<Test>]
    let ``one type spelled with different kinds is unequal`` () : unit =
        // `CompareElementType` compares the element-type byte before it reads a token, so a
        // CLASS-encoded and a VALUETYPE-encoded spelling of one type are unequal however the
        // tokens compare. Well-formed metadata spells a type with one kind everywhere, so the
        // other spelling is built here, and checked on both routes to an answer: the same-token
        // shortcut, and the resolution that a TypeRef against a TypeDef needs.
        let definingImage =
            Roslyn.compileAssembly
                "SignatureKindDef"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [
                    "public struct Thing { } public class Holder { public Thing R { get; set; } }"
                ]

        let user =
            Roslyn.compileAssembly
                "SignatureKindUser"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage definingImage ]
                [ "public class Holder { public Thing P { get; set; } }" ]
            |> read

        let defining = read definingImage

        let byReference = holderProperty user "P"
        let byDefinition = holderProperty defining "R"

        let respell (original : MethodSignature<TypeDefn>) : MethodSignature<TypeDefn> =
            let flipped =
                match original.ReturnType with
                | TypeDefn.FromReference (typeRef, SignatureTypeKind.ValueType) ->
                    TypeDefn.FromReference (typeRef, SignatureTypeKind.Class)
                | TypeDefn.FromDefinition (identity, SignatureTypeKind.ValueType) ->
                    TypeDefn.FromDefinition (identity, SignatureTypeKind.Class)
                | other -> failwith $"expected a VALUETYPE spelling of `Thing`, got %O{other}"

            MethodSignature<TypeDefn> (
                original.Header,
                flipped,
                original.RequiredParameterCount,
                original.GenericParameterCount,
                original.ParameterTypes
            )

        let assemblies =
            LoadedAssemblies.empty.WithLoadedAssembly(user).WithLoadedAssembly defining

        let compareAcross (left : MethodSignature<TypeDefn>) (right : MethodSignature<TypeDefn>) : bool =
            SignatureComparison.compareDecodedSignatures
                fixture.LoggerFactory
                noRuntimeDirs
                "test"
                assemblies
                user
                left
                defining
                right
            |> snd

        // The premise: a TypeRef in one module and a TypeDef in another, naming one type with one
        // kind, are equal. Nothing but the kind separates the cases below from this one.
        compareAcross byReference byDefinition |> shouldEqual true

        // Resolution route: the TypeRef against a respelled TypeDef.
        compareAcross byReference (respell byDefinition) |> shouldEqual false

        // Shortcut route: the same TypeRef row in one module, respelled on one side.
        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            assemblies
            user
            byReference
            user
            (respell byReference)
        |> snd
        |> shouldEqual false

    [<Test>]
    let ``an array's element is compared before its rank`` () : unit =
        // `CompareElementType` reads an ARRAY's element before its rank, so the element is
        // resolved even when the ranks then differ — and an element from an assembly that cannot
        // be bound throws there rather than answering FALSE. Comparing the rank first would answer
        // FALSE without the resolution, loading fewer assemblies than CoreCLR does.
        //
        // Cross-module, so the same-token shortcut cannot answer the element. Both arrays are
        // multidimensional: `Mid[]` is SZARRAY, a different element type from ARRAY, which CoreCLR
        // separates on the byte alone.
        let definingImage =
            Roslyn.compileAssembly
                "SignatureArrayOrderDef"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class Mid { }" ]

        let compileUser (name : string) (arrayType : string) : DumpedAssembly =
            Roslyn.compileAssembly
                name
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage definingImage ]
                [ $"public class Holder {{ public %s{arrayType} P {{ get; set; }} }}" ]
            |> read

        let left = compileUser "SignatureArrayOrderLeft" "Mid[,]"
        let right = compileUser "SignatureArrayOrderRight" "Mid[,,]"
        let leftSig = holderProperty left "P"
        let rightSig = holderProperty right "P"

        // The premise: two ARRAYs of differing rank over one nominal element.
        match leftSig.ReturnType, rightSig.ReturnType with
        | TypeDefn.Array (TypeDefn.FromReference _, 2), TypeDefn.Array (TypeDefn.FromReference _, 3) -> ()
        | l, r -> failwith $"expected rank-2 and rank-3 arrays of a TypeRef, got %O{l} and %O{r}"

        let compareUnder (assemblies : LoadedAssemblies) : bool =
            SignatureComparison.compareDecodedSignatures
                fixture.LoggerFactory
                noRuntimeDirs
                "test"
                assemblies
                left
                leftSig
                right
                rightSig
            |> snd

        // The control: with the element's assembly loaded, the elements agree and the ranks
        // decide.
        let loadable =
            LoadedAssemblies.empty
                .WithLoadedAssembly(left)
                .WithLoadedAssembly(right)
                .WithLoadedAssembly (read definingImage)

        compareUnder loadable |> shouldEqual false

        // Without it, the element's resolution fails before the ranks are ever read.
        let unloadable =
            LoadedAssemblies.empty.WithLoadedAssembly(left).WithLoadedAssembly right

        let exn = Assert.Throws<exn> (fun () -> compareUnder unloadable |> ignore)
        exn.Message |> shouldContainText "SignatureArrayOrderDef"

    [<Test>]
    let ``a generic instantiation's definition is compared before its argument count`` () : unit =
        // `CompareElementType` reads a GENERICINST's definition before its argument count. A
        // count mismatch over one definition needs malformed metadata, since the arity is part of
        // a generic type's name, so the second operand is built by hand; the order still decides
        // whether an unbindable definition throws, as in CoreCLR, or is never looked at.
        let definingImage =
            Roslyn.compileAssembly
                "SignatureGenericOrderDef"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ "public class Gen<T> { }" ]

        let compileUser (name : string) : DumpedAssembly =
            Roslyn.compileAssembly
                name
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage definingImage ]
                [ "public class Holder { public Gen<int> P { get; set; } }" ]
            |> read

        let left = compileUser "SignatureGenericOrderLeft"
        let right = compileUser "SignatureGenericOrderRight"
        let leftSig = holderProperty left "P"
        let original = holderProperty right "P"

        // `Gen<int, int>` spelled with `Gen`1`'s own row.
        let rightSig =
            let doubled =
                match original.ReturnType with
                | TypeDefn.GenericInstantiation (generic, args) ->
                    TypeDefn.GenericInstantiation (generic, args.AddRange args)
                | other -> failwith $"expected a generic instantiation, got %O{other}"

            MethodSignature<TypeDefn> (
                original.Header,
                doubled,
                original.RequiredParameterCount,
                original.GenericParameterCount,
                original.ParameterTypes
            )

        let compareUnder (assemblies : LoadedAssemblies) : bool =
            SignatureComparison.compareDecodedSignatures
                fixture.LoggerFactory
                noRuntimeDirs
                "test"
                assemblies
                left
                leftSig
                right
                rightSig
            |> snd

        // The control: with the definition's assembly loaded, the definitions agree and the counts
        // decide.
        let loadable =
            LoadedAssemblies.empty
                .WithLoadedAssembly(left)
                .WithLoadedAssembly(right)
                .WithLoadedAssembly (read definingImage)

        compareUnder loadable |> shouldEqual false

        // Without it, the definition's resolution fails before the counts are ever compared.
        let unloadable =
            LoadedAssemblies.empty.WithLoadedAssembly(left).WithLoadedAssembly right

        let exn = Assert.Throws<exn> (fun () -> compareUnder unloadable |> ignore)
        exn.Message |> shouldContainText "SignatureGenericOrderDef"

    [<Test>]
    let ``enclosing types are resolved on both sides before either nested leaf`` () : unit =
        // `CompareTypeTokens` recurses on the enclosing tokens, and that recursion resolves the
        // enclosing type on *both* sides, before either leaf is resolved. So when two `Outer+Inner`
        // references bind `Outer` to different assemblies, the right-hand assembly is bound — or
        // fails to bind, which throws — even if the left-hand one turns out not to declare `Inner`
        // at all. Resolving the whole left leaf first would answer FALSE on that miss without ever
        // touching the right-hand assembly.
        let nested = "public class Outer { public class Inner { } }"

        let compileDefinition (name : string) (source : string) : byte[] =
            Roslyn.compileAssembly name Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let x = compileDefinition "SignatureNestedOrderX" nested
        let y = compileDefinition "SignatureNestedOrderY" nested
        // The same identity as `x`, but `Outer` has no `Inner`.
        let xStandIn = compileDefinition "SignatureNestedOrderX" "public class Outer { }"

        let compileUser (name : string) (definition : byte[]) : DumpedAssembly =
            Roslyn.compileAssembly
                name
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                [ Microsoft.CodeAnalysis.MetadataReference.CreateFromImage definition ]
                [ "public class Holder { public Outer.Inner P { get; set; } }" ]
            |> read

        let left = compileUser "SignatureNestedOrderLeft" x
        let right = compileUser "SignatureNestedOrderRight" y
        let leftSig = holderProperty left "P"
        let rightSig = holderProperty right "P"

        let compareUnder (assemblies : LoadedAssemblies) : bool =
            SignatureComparison.compareDecodedSignatures
                fixture.LoggerFactory
                noRuntimeDirs
                "test"
                assemblies
                left
                leftSig
                right
                rightSig
            |> snd

        // The control: both bound, the two `Outer`s are different definitions, and that decides.
        let both =
            LoadedAssemblies.empty
                .WithLoadedAssembly(left)
                .WithLoadedAssembly(right)
                .WithLoadedAssembly(read x)
                .WithLoadedAssembly (read y)

        compareUnder both |> shouldEqual false

        // And the same reference resolved from two users of one `Outer+Inner` is equal, so the
        // recursion over the enclosing chain does reach the leaf when the enclosers agree.
        let leftOfX = compileUser "SignatureNestedOrderLeftOfX" x

        let sameDefinition =
            LoadedAssemblies.empty.WithLoadedAssembly(leftOfX).WithLoadedAssembly(right).WithLoadedAssembly (read x)

        SignatureComparison.compareDecodedSignatures
            fixture.LoggerFactory
            noRuntimeDirs
            "test"
            sameDefinition
            leftOfX
            (holderProperty leftOfX "P")
            left
            leftSig
        |> snd
        |> shouldEqual true

        // The order: the left `Outer` binds to the stand-in, which does declare `Outer`, so the
        // enclosing comparison goes on to bind the right `Outer` — and `SignatureNestedOrderY` is
        // neither loaded nor findable. That failure must come before the left `Inner` is looked
        // for and found missing.
        let standInOnly =
            LoadedAssemblies.empty.WithLoadedAssembly(left).WithLoadedAssembly(right).WithLoadedAssembly (read xStandIn)

        let exn = Assert.Throws<exn> (fun () -> compareUnder standInOnly |> ignore)
        exn.Message |> shouldContainText "SignatureNestedOrderY"

    // ----- function pointers ---------------------------------------------------------------------

    /// A property whose type is the given function pointer. Built from a real decoded signature so
    /// that the header and parameter list are a property's, with only the type swapped: C# cannot
    /// declare a property of generic function-pointer type.
    let private propertyOfFunctionPointerType (functionPointer : TypeDefn) : MethodSignature<TypeDefn> =
        let original = signature "Corpus" "Number"

        MethodSignature<TypeDefn> (
            original.Header,
            functionPointer,
            original.RequiredParameterCount,
            original.GenericParameterCount,
            original.ParameterTypes
        )

    /// A function pointer taking one `int32` and returning void, whose own signature carries the
    /// given calling-convention attributes and generic-parameter count.
    let private functionPointerType (attributes : SignatureAttributes) (genericParameterCount : int) : TypeDefn =
        TypeDefn.FunctionPointer
            {
                Header =
                    ComparableSignatureHeader.Make (
                        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, attributes)
                    )
                ParameterTypes = [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                GenericParameterCount = genericParameterCount
                RequiredParameterCount = 1
                ReturnType = MethodReturnType.Void
            }

    /// Two function pointers spelling the same GENERIC calling convention *and the same
    /// generic-parameter count* are a shape CoreCLR cannot compare. Its FNPTR arm (siginfo.cpp:4135)
    /// reads one compressed integer after the calling-convention bytes and compares it as `argCnt`; for
    /// a GENERIC signature that integer is the generic-parameter count, so having consumed it CoreCLR
    /// reads the real parameter count as an element type. Refuse rather than guess.
    ///
    /// Differing counts are rejected by that same `argCnt` comparison, before any element is parsed, so
    /// they have a defined answer and must not be refused — this arm previously had no count comparison
    /// at all and would have compared the parameter lists and called them equal.
    [<Test>]
    let ``two function pointers with the same generic calling convention and count are refused`` () : unit =
        let compareAt (leftCount : int) (rightCount : int) : unit -> bool =
            fun () ->
                compare
                    (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.Generic leftCount))
                    (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.Generic rightCount))

        let thrown = Assert.Throws<exn> (fun () -> compareAt 1 1 () |> ignore<bool>)
        thrown.Message |> shouldContainText "GENERIC calling convention"

        // The parameter lists are identical here, so without the count comparison these would have
        // answered "equal" rather than "unequal".
        compareAt 1 2 () |> shouldEqual false
        compareAt 2 1 () |> shouldEqual false

        // Differing *parameter* counts at equal generic arity are refused too, not answered: that
        // comparison happens inside `CompareElementType` with the count byte reinterpreted as an element
        // type, so the answer is a fact about byte values rather than about the decoded signature.
        let generic (parameters : TypeDefn list) : TypeDefn =
            TypeDefn.FunctionPointer
                {
                    Header =
                        ComparableSignatureHeader.Make (
                            SignatureHeader (
                                SignatureKind.Method,
                                SignatureCallingConvention.Default,
                                SignatureAttributes.Generic
                            )
                        )
                    ParameterTypes = parameters
                    GenericParameterCount = 1
                    RequiredParameterCount = List.length parameters
                    ReturnType = MethodReturnType.Void
                }

        let int32 = TypeDefn.PrimitiveType PrimitiveType.Int32

        let differingParameterCounts =
            Assert.Throws<exn> (fun () ->
                compare
                    (propertyOfFunctionPointerType (generic [ int32 ]))
                    (propertyOfFunctionPointerType (generic [ int32 ; int32 ]))
                |> ignore<bool>
            )

        differingParameterCounts.Message
        |> shouldContainText "GENERIC calling convention"

    /// The refusal sits *after* the calling-convention comparison, because every pairing whose bytes
    /// differ has an answer CoreCLR defines: it compares those bytes before it reads any count.
    [<Test>]
    let ``a generic function pointer is unequal, not refused, when the convention bytes differ`` () : unit =
        // The GENERIC bit alone makes the bytes differ.
        compare
            (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.Generic 1))
            (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.None 0))
        |> shouldEqual false

        // Both generic, differing in HASTHIS: still a byte mismatch, so still a defined answer.
        compare
            (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.Generic 1))
            (propertyOfFunctionPointerType (
                functionPointerType (SignatureAttributes.Generic ||| SignatureAttributes.Instance) 1
            ))
        |> shouldEqual false

    /// The control: an ordinary non-generic function pointer carries no GENERIC bit, so the refusal
    /// must not fire for it and the arm must still compare it.
    [<Test>]
    let ``a non-generic function pointer is still compared`` () : unit =
        compare
            (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.None 0))
            (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.None 0))
        |> shouldEqual true

        // A differing parameter list is what the arm is for, and it must still separate these.
        compare
            (propertyOfFunctionPointerType (functionPointerType SignatureAttributes.None 0))
            (propertyOfFunctionPointerType (
                TypeDefn.FunctionPointer
                    {
                        Header =
                            ComparableSignatureHeader.Make (
                                SignatureHeader (
                                    SignatureKind.Method,
                                    SignatureCallingConvention.Default,
                                    SignatureAttributes.None
                                )
                            )
                        ParameterTypes = [ TypeDefn.PrimitiveType PrimitiveType.String ]
                        GenericParameterCount = 0
                        RequiredParameterCount = 1
                        ReturnType = MethodReturnType.Void
                    }
            ))
        |> shouldEqual false
