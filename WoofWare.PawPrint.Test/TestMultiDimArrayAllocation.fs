namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `MultiDimArrayAllocation.totalElements` transcribes the dimension-length rules of CoreCLR's
/// `AllocateArrayEx`, so the host runtime is the oracle for it rather than a second transcription
/// of the same source, which would be free to share a misreading.
///
/// The oracle is reachable without allocating anything: every generated dimension vector contains
/// a zero, so a vector CoreCLR accepts produces an empty array whatever the other dimensions are.
/// That is what makes it safe to sweep values up to `Int32.MaxValue`, which is exactly where the
/// interesting rules live.
///
/// Negative lengths are outside the oracle's reach: managed `Array.CreateInstance` screens them
/// with an `ArgumentOutOfRangeException` before the QCall, so no managed call can put one in front
/// of `AllocateArrayEx`. They are pinned by example against the source instead.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestMultiDimArrayAllocation =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// What the host runtime does with these dimension lengths: the empty array's length, or the
    /// name and message of the exception it refused with.
    let private hostAnswer (lengths : int list) : Result<int, string * string> =
        try
            let created = Array.CreateInstance (typeof<byte>, List.toArray lengths)
            Ok created.Length
        with e ->
            Error (e.GetType().Name, e.Message)

    /// The same question asked of the classifier, in the same shape. The exception is the one the
    /// QCall handler would raise, named the way the host names it.
    let private classifierAnswer (lengths : int list) : Result<int, string * string> =
        match MultiDimArrayAllocation.totalElements (ImmutableArray.CreateRange lengths) with
        | Ok total -> Ok total
        | Error MultiDimArrayLengthError.TotalElementsExceedsInt32 ->
            Error ("OutOfMemoryException", "PawPrint backing store limit")
        | Error (MultiDimArrayLengthError.Negative _) -> Error ("OverflowException", "")
        | Error (MultiDimArrayLengthError.DimensionExceedsMaxLength _)
        | Error (MultiDimArrayLengthError.TotalElementsOverflow _) ->
            Error ("OutOfMemoryException", SzArrayAllocation.dimensionsExceededMessage)

    /// Values chosen around every boundary the rules name: zero, the `MaxArrayLength` boundary on
    /// both sides, `Int32.MaxValue`, and the square roots of `UInt32.MaxValue` where the running
    /// product tips over.
    let private interestingLength : Gen<int> =
        Gen.frequency
            [
                3, Gen.elements [ 0 ; 1 ; 2 ; 3 ; 7 ]
                3,
                Gen.elements
                    [
                        SzArrayAllocation.maxLength - 1
                        SzArrayAllocation.maxLength
                        SzArrayAllocation.maxLength + 1
                        Int32.MaxValue
                    ]
                3, Gen.elements [ 65535 ; 65536 ; 65537 ; 50000 ; 46340 ; 46341 ]
                2, Gen.choose (0, Int32.MaxValue)
            ]

    /// A vector of two to four dimensions with at least one zero, so that a vector the host
    /// accepts allocates nothing.
    let private lengthsWithAZero : Gen<int list> =
        gen {
            let! rank = Gen.choose (2, 4)
            let! lengths = Gen.listOfLength rank interestingLength
            let! zeroAt = Gen.choose (0, rank - 1)
            return lengths |> List.mapi (fun i v -> if i = zeroAt then 0 else v)
        }

    [<Test>]
    let ``agrees with the host runtime on dimension vectors containing a zero`` () =
        let property (lengths : int list) : bool =
            hostAnswer lengths = classifierAnswer lengths

        Check.One (config, Prop.forAll (Arb.fromGen lengthsWithAZero) property)

    /// The generator above always answers `Ok 0` when it answers `Ok`, so on its own it could not
    /// tell a classifier that returns the product from one that returns zero. This sweeps small
    /// vectors with no zero in them, where the product is the whole answer.
    [<Test>]
    let ``agrees with the host runtime on the element count of a small array`` () =
        let smallLengths : Gen<int list> =
            gen {
                let! rank = Gen.choose (2, 4)
                return! Gen.listOfLength rank (Gen.choose (0, 6))
            }

        let property (lengths : int list) : bool =
            hostAnswer lengths = classifierAnswer lengths

        Check.One (config, Prop.forAll (Arb.fromGen smallLengths) property)

    [<Test>]
    let ``MaxArrayLength itself is allowed, one above it is not`` () =
        MultiDimArrayAllocation.totalElements (ImmutableArray.Create (SzArrayAllocation.maxLength, 0))
        |> shouldEqual (Ok 0)

        MultiDimArrayAllocation.totalElements (ImmutableArray.Create (SzArrayAllocation.maxLength + 1, 0))
        |> shouldEqual (Error (MultiDimArrayLengthError.DimensionExceedsMaxLength (0, SzArrayAllocation.maxLength + 1)))

    /// `AllocateArrayEx` records an over-long dimension and raises it only after the whole walk,
    /// so a negative length anywhere wins — and the two raise different exceptions, which makes
    /// the ordering guest-visible. Beyond the host oracle's reach, since managed
    /// `Array.CreateInstance` screens negatives first.
    [<Test>]
    let ``a negative length beats an over-long one in an earlier dimension`` () =
        MultiDimArrayAllocation.totalElements (ImmutableArray.Create (SzArrayAllocation.maxLength + 1, -1))
        |> shouldEqual (Error (MultiDimArrayLengthError.Negative (1, -1)))

    /// A running product that overflows is checked inside the loop, so it is raised even though a
    /// later zero would have brought the product back down; a prefix that merely passes
    /// `Int32.MaxValue` is not.
    [<Test>]
    let ``a zero dimension rescues a large prefix but not an overflowing one`` () =
        MultiDimArrayAllocation.totalElements (ImmutableArray.Create (50000, 50000, 0))
        |> shouldEqual (Ok 0)

        MultiDimArrayAllocation.totalElements (ImmutableArray.Create (65536, 65536, 0))
        |> shouldEqual (Error (MultiDimArrayLengthError.TotalElementsOverflow 1))

    /// A product that fits in UInt32 but not Int32 is PawPrint's own limit, not a CoreCLR rule.
    [<Test>]
    let ``a product above Int32.MaxValue that does not overflow UInt32 is refused`` () =
        MultiDimArrayAllocation.totalElements (ImmutableArray.Create (SzArrayAllocation.maxLength, 2))
        |> shouldEqual (Error MultiDimArrayLengthError.TotalElementsExceedsInt32)
