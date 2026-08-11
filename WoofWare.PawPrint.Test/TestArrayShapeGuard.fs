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
/// The subtlety these tests pin down is that "the shape carried bounds" is *not* the right
/// predicate. ECMA-335 II.23.2.13 makes both counts optional, and <c>ArrayShape</c> synthesises a
/// zero per dimension when <c>numLoBounds</c> is 0 -- so an ordinary <c>int[,]</c> arrives with
/// <c>LowerBounds = [0; 0]</c>. A guard rejecting non-empty <c>LowerBounds</c> would therefore
/// reject every multidimensional array in existence, which the round-trip test below would catch.
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

    /// The shape SRM hands us for a C#-emitted `int[,]`: no sizes, and a synthesised zero lower
    /// bound per dimension. This is by far the most important case -- if the guard rejects this,
    /// every multidimensional array in every assembly fails to load.
    [<Test>]
    let ``ordinary int[,] as SRM presents it is accepted`` () =
        decode 2 [] [ 0 ; 0 ] |> shouldEqual (TypeDefn.Array (elementType, 2))

    [<Test>]
    let ``shape with no bounds at all is accepted`` () =
        decode 2 [] [] |> shouldEqual (TypeDefn.Array (elementType, 2))

    /// An explicitly encoded all-zero lower bound denotes the same type as an omitted one, and is
    /// indistinguishable from it by the time `ArrayShape` reaches us. Accepting is the documented
    /// choice.
    [<Test>]
    let ``explicit all-zero lower bounds are accepted`` () =
        decode 3 [] [ 0 ; 0 ; 0 ] |> shouldEqual (TypeDefn.Array (elementType, 3))

    [<Test>]
    let ``non-zero lower bound is refused`` () =
        let exc = Assert.Throws<Exception> (fun () -> decode 2 [] [ 1 ; 0 ] |> ignore)

        exc.Message |> shouldContainText "non-default ArrayShape"
        exc.Message |> shouldContainText "TestAssembly"

    [<Test>]
    let ``explicit sizes are refused`` () =
        let exc =
            Assert.Throws<Exception> (fun () -> decode 2 [ 3 ; 4 ] [ 0 ; 0 ] |> ignore)

        exc.Message |> shouldContainText "non-default ArrayShape"

    /// A negative lower bound is legal in the encoding (it is a compressed *signed* integer) and is
    /// just as much a divergence as a positive one.
    [<Test>]
    let ``negative lower bound is refused`` () =
        let exc = Assert.Throws<Exception> (fun () -> decode 1 [] [ -1 ] |> ignore)

        exc.Message |> shouldContainText "non-default ArrayShape"

    /// The contract the rest of the system relies on: a shape is accepted exactly when it carries
    /// no sizes and no non-zero lower bound, and when accepted the rank survives intact.
    [<Test>]
    let ``accepted exactly when the shape is default`` () =
        let config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

        let property (rank : int, sizes : int list, lowerBounds : int list) : bool =
            let isDefault =
                List.isEmpty sizes && List.forall (fun bound -> bound = 0) lowerBounds

            match
                (try
                    Ok (decode rank sizes lowerBounds)
                 with _ ->
                     Error ())
            with
            | Ok decoded -> isDefault && decoded = TypeDefn.Array (elementType, rank)
            | Error () -> not isDefault

        let gen =
            gen {
                let! rank = Gen.choose (1, 4)
                // Deliberately include 0 in the alphabet of both lists: a generator that only ever
                // produced non-zero bounds could not distinguish "rejects non-zero bounds" from
                // "rejects any bounds at all", which is the exact bug this guard had to avoid.
                let! sizes = Gen.listOf (Gen.choose (0, 3))
                let! lowerBounds = Gen.listOf (Gen.choose (-2, 2))
                return rank, sizes, lowerBounds
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)
