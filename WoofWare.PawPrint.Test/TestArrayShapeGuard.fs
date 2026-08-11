namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the non-default-`ArrayShape` refusal in <c>TypeDefn.typeProvider</c>.
///
/// <c>TypeDefn.Array</c> records only the rank, so a shape carrying explicit sizes or a non-zero
/// lower bound would decode to the same <c>TypeDefn</c> as the plain array of that rank. That
/// equality is load-bearing for vtable slot matching, which compares concretised signatures, so
/// the decoder refuses such a shape rather than conflating it.
///
/// The subtlety these tests pin down is that the predicate is about the *encoding*, not the
/// meaning. ECMA-335 II.23.2.13 makes both counts optional, and <c>ArrayShape</c> faithfully
/// reports which was used: a blob with <c>numLoBounds = 0</c> decodes to <c>LowerBounds = []</c>,
/// while the vector every real compiler emits for <c>int[,]</c> decodes to <c>[0; 0]</c>. Those
/// denote the same type, but <c>MetaSig::CompareElementType</c> compares the counts and rejects the
/// pair before looking at the values (siginfo.cpp:4317), so they do not override one another.
/// Accepting both would reintroduce the conflation the guard exists to prevent; hence exactly one
/// encoding is accepted, and it is the one real compilers emit.
///
/// Two directions therefore need pinning, and each catches an opposite mistake: rejecting the
/// canonical <c>[0; 0]</c> would fail every multidimensional array in existence, and accepting the
/// omitted form would silently conflate two distinct signatures.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestArrayShapeGuard =

    let private provider : ISignatureTypeProvider<TypeDefn, unit> =
        TypeDefn.typeProvider (AssemblyName "TestAssembly")

    let private elementType : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Int32

    let private decode (rank : int) (sizes : int list) (lowerBounds : int list) : TypeDefn =
        let shape =
            ArrayShape (rank, ImmutableArray.CreateRange sizes, ImmutableArray.CreateRange lowerBounds)

        provider.GetArrayType (elementType, shape)

    /// The shape SRM hands us for a C#- or F#-emitted `int[,]`: no sizes, and one explicitly
    /// encoded zero lower bound per dimension. This is by far the most important case -- if the
    /// guard rejects this, every multidimensional array in every assembly fails to load. Measured
    /// over the runtime pack, FSharp.Core, Roslyn and this repo's test binaries, all 339
    /// multidimensional array signatures take this form.
    [<Test>]
    let ``canonically encoded int[,] is accepted`` () =
        decode 2 [] [ 0 ; 0 ] |> shouldEqual (TypeDefn.Array (elementType, 2))

    [<Test>]
    let ``canonically encoded rank-3 array is accepted`` () =
        decode 3 [] [ 0 ; 0 ; 0 ] |> shouldEqual (TypeDefn.Array (elementType, 3))

    /// The omitted lower-bound vector denotes the same *type* as the canonical encoding, but
    /// `MetaSig::CompareElementType` compares the counts, so the two do not override one another.
    /// `TypeDefn.Array` cannot record which encoding was used, so the non-canonical one is refused
    /// rather than silently unified with the canonical one.
    [<Test>]
    let ``omitted lower-bound vector is refused despite denoting the same type`` () =
        let exc = Assert.Throws<Exception> (fun () -> decode 2 [] [] |> ignore)

        exc.Message |> shouldContainText "non-canonical ArrayShape"

    /// A lower-bound vector shorter than the rank is likewise a distinct encoding.
    [<Test>]
    let ``under-length lower-bound vector is refused`` () =
        let exc = Assert.Throws<Exception> (fun () -> decode 2 [] [ 0 ] |> ignore)

        exc.Message |> shouldContainText "non-canonical ArrayShape"

    [<Test>]
    let ``non-zero lower bound is refused`` () =
        let exc = Assert.Throws<Exception> (fun () -> decode 2 [] [ 1 ; 0 ] |> ignore)

        exc.Message |> shouldContainText "non-canonical ArrayShape"
        exc.Message |> shouldContainText "TestAssembly"

    [<Test>]
    let ``explicit sizes are refused`` () =
        let exc =
            Assert.Throws<Exception> (fun () -> decode 2 [ 3 ; 4 ] [ 0 ; 0 ] |> ignore)

        exc.Message |> shouldContainText "non-canonical ArrayShape"

    /// A negative lower bound is legal in the encoding (it is a compressed *signed* integer) and is
    /// just as much a divergence as a positive one.
    [<Test>]
    let ``negative lower bound is refused`` () =
        let exc = Assert.Throws<Exception> (fun () -> decode 1 [] [ -1 ] |> ignore)

        exc.Message |> shouldContainText "non-canonical ArrayShape"

    /// The contract the rest of the system relies on: a shape is accepted exactly when it carries
    /// no sizes and exactly one zero lower bound per dimension, and when accepted the rank survives
    /// intact.
    [<Test>]
    let ``accepted exactly when the shape is canonically encoded`` () =
        let config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

        let property (rank : int, sizes : int list, lowerBounds : int list) : bool =
            let isCanonical =
                List.isEmpty sizes
                && List.length lowerBounds = rank
                && List.forall (fun bound -> bound = 0) lowerBounds

            match
                (try
                    Ok (decode rank sizes lowerBounds)
                 with _ ->
                     Error ())
            with
            | Ok decoded -> isCanonical && decoded = TypeDefn.Array (elementType, rank)
            | Error () -> not isCanonical

        let gen =
            gen {
                let! rank = Gen.choose (1, 4)

                // Deliberately include 0 in the alphabet of both lists: a generator that only ever
                // produced non-zero bounds could not distinguish "rejects non-zero bounds" from
                // "rejects any bounds at all", which is one of the two mistakes this guard has to
                // avoid.
                let! sizes = Gen.frequency [ 3, Gen.constant [] ; 1, Gen.listOf (Gen.choose (0, 3)) ]

                // Unbiased, a random list almost never has exactly `rank` entries, so the accepting
                // branch of the property would hardly ever be exercised and a guard that rejected
                // everything would still pass. Generate the canonical vector often enough that both
                // branches carry weight.
                let! lowerBounds =
                    Gen.frequency
                        [
                            3, Gen.constant (List.replicate rank 0)
                            1, Gen.listOf (Gen.choose (-2, 2))
                            1, Gen.listOf (Gen.constant 0)
                        ]

                return rank, sizes, lowerBounds
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)
