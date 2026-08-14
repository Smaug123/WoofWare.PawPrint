namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <see cref="MethodOwner" />'s custom equality, which is the only new logic in the
/// change that introduced it — everything else was a rename driven by the compiler.
/// </summary>
/// <remarks>
/// <para>
/// Custom equality rather than structural for two reasons, and both are tested here because
/// neither is visible from reading the call sites. A declared owner compares on identity plus
/// instantiation and deliberately not on the whole <c>ConcreteType</c> record, preserving what
/// <c>MethodInfo.NominallyEqual</c> has always compared. And <c>AssemblyName</c> is a BCL class
/// with reference equality, so the dynamic case has to compare <c>FullName</c>.
/// </para>
/// <para>
/// The dynamic arms are unreachable in production today — nothing constructs a
/// <c>DynamicMethodsClass</c> owner until dynamic methods become executable — so this fixture is
/// what stops them being written wrong and staying wrong. A test is able to construct one
/// directly where the interpreter cannot.
/// </para>
/// </remarks>
[<TestFixture>]
module TestMethodOwner =

    let private assemblyName (name : string) : AssemblyName = AssemblyName name

    let private declaredOn (assembly : string) (rowId : int) : MethodOwner<GenericParamFromMetadata> =
        ConcreteType.make
            (assemblyName assembly)
            (MetadataTokens.TypeDefinitionHandle rowId)
            "Some.Namespace"
            $"Type%d{rowId}"
            ImmutableArray.Empty
        |> MethodOwner.DeclaredOn

    /// The trap the custom equality exists for: two separately-parsed `AssemblyName`s naming the
    /// same assembly are different *objects*, so the default reference comparison would call these
    /// different owners — and every dynamic method would then be unequal to itself across any two
    /// reads of its scope assembly's name.
    [<Test>]
    let ``two dynamic owners of the same assembly are equal, though their AssemblyNames are not`` () : unit =
        let left : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")

        let right : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")

        // The premise: without this, the test would pass for the wrong reason.
        match left, right with
        | MethodOwner.DynamicMethodsClass l, MethodOwner.DynamicMethodsClass r ->
            System.Object.ReferenceEquals (l, r) |> shouldEqual false
        | _ -> failwith "unreachable"

        left |> shouldEqual right
        hash left |> shouldEqual (hash right)

    [<Test>]
    let ``dynamic owners of different assemblies are unequal`` () : unit =
        let left : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")

        let right : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass (assemblyName "Other, Version=1.0.0.0")

        left |> shouldNotEqual right

    /// The two cases must never collide, whatever their payloads: a method declared by a type and
    /// one minted into a module's synthetic class are different methods even if everything else
    /// about them agrees.
    [<Test>]
    let ``a declared owner never equals a dynamic one`` () : unit =
        let declared = declaredOn "Guest, Version=1.0.0.0" 1

        let dynamic : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")

        declared |> shouldNotEqual dynamic
        dynamic |> shouldNotEqual declared

    [<Test>]
    let ``declared owners compare on identity`` () : unit =
        declaredOn "Guest, Version=1.0.0.0" 1
        |> shouldEqual (declaredOn "Guest, Version=1.0.0.0" 1)

        declaredOn "Guest, Version=1.0.0.0" 1
        |> shouldNotEqual (declaredOn "Guest, Version=1.0.0.0" 2)

        declaredOn "Guest, Version=1.0.0.0" 1
        |> shouldNotEqual (declaredOn "Other, Version=1.0.0.0" 1)

    /// `Assembly` and `Generics` are projected as total because both cases have a truthful answer;
    /// `TryDeclaringType` is an option because only one does. Pinned because the totality is what
    /// let roughly half the call sites of the old `DeclaringType` become renames rather than
    /// decisions, and a future edit that made either of them fabricate something for the dynamic
    /// case would silently undo that reasoning.
    [<Test>]
    let ``the total projections answer for a dynamic owner`` () : unit =
        let owner =
            MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")
            : MethodOwner<GenericParamFromMetadata>

        owner.Assembly.FullName
        |> shouldEqual (assemblyName "Guest, Version=1.0.0.0").FullName

        owner.Generics |> shouldEqual ImmutableArray.Empty
        owner.TryDeclaringType |> shouldEqual None

    [<Test>]
    let ``the total projections agree with the declaring type when there is one`` () : unit =
        let declaringType =
            ConcreteType.make
                (assemblyName "Guest, Version=1.0.0.0")
                (MetadataTokens.TypeDefinitionHandle 1)
                "Some.Namespace"
                "Type1"
                ImmutableArray.Empty

        let owner = MethodOwner.DeclaredOn declaringType

        owner.Assembly.FullName |> shouldEqual declaringType.Assembly.FullName
        owner.Generics |> shouldEqual declaringType.Generics
        owner.TryDeclaringType |> shouldEqual (Some declaringType)

    /// `describe` is what every diagnostic that used to interpolate `Namespace.Name` now calls, so
    /// it has to render *something* for a dynamic owner rather than throwing — a message that
    /// fails while being built is worse than the failure it was reporting. It must also not look
    /// like a type name, or a reader will go hunting for a type that does not exist.
    [<Test>]
    let ``describe renders both cases, and the dynamic one is not a plausible type name`` () : unit =
        MethodOwner.describe (declaredOn "Guest, Version=1.0.0.0" 1)
        |> shouldEqual "Some.Namespace.Type1"

        let dynamic =
            MethodOwner.describe (
                MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")
                : MethodOwner<GenericParamFromMetadata>
            )

        dynamic |> shouldContainText "Guest"
        // No identifier can contain these, so this cannot be mistaken for a namespace-qualified name.
        dynamic |> shouldContainText "<"
        dynamic |> shouldContainText " "

    /// The partial accessor is the marker for "this site needs a TypeDef row", so it must fail
    /// rather than invent one — and say which assembly, since that is the only locating fact a
    /// dynamic method has.
    [<Test>]
    let ``requireDeclaringType fails for a dynamic owner, naming the assembly`` () : unit =
        let owner =
            MethodOwner.DynamicMethodsClass (assemblyName "Guest, Version=1.0.0.0")
            : MethodOwner<GenericParamFromMetadata>

        let exn =
            Assert.Throws<exn> (fun () -> MethodOwner.requireDeclaringType "some operation" owner |> ignore)

        exn.Message |> shouldContainText "some operation"
        exn.Message |> shouldContainText "Guest"
