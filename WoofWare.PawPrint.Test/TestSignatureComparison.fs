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
            State : IlMachineState
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

        let state =
            (IlMachineState.initial loggerFactory ImmutableArray.Empty assembly).WithLoadedAssembly corelib

        {
            LoggerFactory = loggerFactory
            Assembly = assembly
            Corelib = corelib
            State = state
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

    let private compare (left : MethodSignature<TypeDefn>) (right : MethodSignature<TypeDefn>) : bool =
        NativeSignature.compareDecodedSignatures
            fixture.LoggerFactory
            "test"
            fixture.State
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

        let state =
            fixture.State.WithLoadedAssembly(leftAssembly).WithLoadedAssembly rightAssembly

        // Each `Payload` is a TypeDef in its own assembly, so these are different types despite the
        // identical bytes. A fast path that compared bytes without checking the assembly would say
        // they are equal.
        NativeSignature.signaturesAreEqual
            fixture.LoggerFactory
            "test"
            state
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

        let state = fixture.State.WithLoadedAssembly assembly

        NativeSignature.signaturesAreEqual
            fixture.LoggerFactory
            "test"
            state
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
        let state =
            (IlMachineState.initial fixture.LoggerFactory ImmutableArray.Empty user).WithLoadedAssembly mid

        // Distinct Property rows of the same type, so this is a real comparison rather than an
        // identity check, and both sides resolve the same reference.
        NativeSignature.compareDecodedSignatures
            fixture.LoggerFactory
            "test"
            state
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
        let state =
            (IlMachineState.initial fixture.LoggerFactory ImmutableArray.Empty left)
                .WithLoadedAssembly(right)
                .WithLoadedAssembly(leftDefining)
                .WithLoadedAssembly
                rightDefining

        NativeSignature.compareDecodedSignatures fixture.LoggerFactory "test" state left leftSig right rightSig
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
        let state = IlMachineState.initial fixture.LoggerFactory ImmutableArray.Empty user

        NativeSignature.compareDecodedSignatures
            fixture.LoggerFactory
            "test"
            state
            user
            (signatureNamed "P")
            user
            (signatureNamed "Q")
        |> snd
        |> shouldEqual true
