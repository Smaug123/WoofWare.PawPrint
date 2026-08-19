namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// Tests for <see cref="MethodOwner" />'s custom equality.
/// </summary>
/// <remarks>
/// <para>
/// A declared owner compares on identity plus instantiation, not on the whole
/// <c>ConcreteType</c> record (matching <c>MethodInfo.NominallyEqual</c>). <c>AssemblyName</c>
/// is a BCL class with reference equality, so the dynamic case has to compare <c>FullName</c>.
/// </para>
/// <para>
/// Nothing in production constructs a <c>DynamicMethodsClass</c> owner until dynamic methods
/// become executable, so the dynamic arms are covered only here.
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
            MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)

        let right : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)

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
            MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)

        let right : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass ((assemblyName "Other, Version=1.0.0.0").FullName)

        left |> shouldNotEqual right

    /// The two cases must never collide, whatever their payloads: a method declared by a type and
    /// one minted into a module's synthetic class are different methods even if everything else
    /// about them agrees.
    [<Test>]
    let ``a declared owner never equals a dynamic one`` () : unit =
        let declared = declaredOn "Guest, Version=1.0.0.0" 1

        let dynamic : MethodOwner<GenericParamFromMetadata> =
            MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)

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

    /// <summary>
    /// Equal owners must hash equally when their instantiations were built from separate arrays.
    /// </summary>
    /// <remarks>
    /// F#'s <c>=</c> on an <c>ImmutableArray</c> compares elementwise, but the array's own
    /// <c>GetHashCode</c> reflects the identity of its backing storage — so
    /// <c>HashCode.Combine</c>, which calls that, disagrees with the equality right beside it.
    /// Measured, on two three-element arrays with equal contents: <c>=</c> is true,
    /// <c>GetHashCode</c> differs. Non-generic owners hash correctly either way, so only the
    /// generic case observes the difference.
    /// </remarks>
    [<Test>]
    let ``equal generic owners hash equally`` () : unit =
        let identity =
            ResolvedTypeIdentity.ofTypeDefinition
                (assemblyName "Guest, Version=1.0.0.0")
                (MetadataTokens.TypeDefinitionHandle 1)

        let owner (generics : int list) : MethodOwner<int> =
            ConcreteType.makeFromIdentity identity "Some.Namespace" "Generic`2" (ImmutableArray.CreateRange generics)
            |> MethodOwner.DeclaredOn

        let left = owner [ 7 ; 8 ]
        let right = owner [ 7 ; 8 ]

        // The premise: distinct backing arrays, or this passes for the wrong reason.
        match left, right with
        | MethodOwner.DeclaredOn l, MethodOwner.DeclaredOn r ->
            System.Object.ReferenceEquals (l.Generics, r.Generics) |> shouldEqual false
        | _ -> failwith "unreachable"

        left |> shouldEqual right
        hash left |> shouldEqual (hash right)

        // ...and a differing instantiation is still a different owner: equality must consult the
        // generics.
        owner [ 7 ; 8 ] |> shouldNotEqual (owner [ 7 ; 9 ])

    /// `Assembly` and `Generics` are projected as total because both cases have a truthful answer;
    /// `TryDeclaringType` is an option because only one does. Neither total projection may
    /// fabricate anything for the dynamic case.
    [<Test>]
    let ``the total projections answer for a dynamic owner`` () : unit =
        let owner =
            MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)
            : MethodOwner<GenericParamFromMetadata>

        owner.AssemblyFullName
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

        owner.AssemblyFullName |> shouldEqual declaringType.AssemblyFullName
        owner.Generics |> shouldEqual declaringType.Generics
        owner.TryDeclaringType |> shouldEqual (Some declaringType)

    /// `describe` is used in diagnostics, so it has to render *something* for a dynamic owner
    /// rather than throwing — a message that fails while being built is worse than the failure it
    /// was reporting. It must also not look like a type name, or a reader will go hunting for a
    /// type that does not exist.
    [<Test>]
    let ``describe renders both cases, and the dynamic one is not a plausible type name`` () : unit =
        MethodOwner.describe (declaredOn "Guest, Version=1.0.0.0" 1)
        |> shouldEqual "Some.Namespace.Type1"

        let dynamic =
            MethodOwner.describe (
                MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)
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
            MethodOwner.DynamicMethodsClass ((assemblyName "Guest, Version=1.0.0.0").FullName)
            : MethodOwner<GenericParamFromMetadata>

        let exn =
            Assert.Throws<exn> (fun () -> MethodOwner.requireDeclaringType "some operation" owner |> ignore)

        exn.Message |> shouldContainText "some operation"
        exn.Message |> shouldContainText "Guest"
