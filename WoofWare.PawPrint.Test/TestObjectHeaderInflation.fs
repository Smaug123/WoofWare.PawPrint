namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ObjectHeaderInflation.inflate`, checked against CoreCLR's header layout (syncblk.h) stated
/// bit by bit, rather than against the function's own arithmetic.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestObjectHeaderInflation =

    let private indexOrHashBit = 0x08000000
    let private hashCodeBit = 0x04000000
    let private spinLockBit = 0x10000000
    let private indexMask = 0x03FFFFFF

    /// Header words the thin lock and the VM write, and arbitrary ones: an owner and recursion
    /// level in the thin lock's fields, under any combination of the high bits.
    let private wordGen : Gen<int32> =
        gen {
            let! threadId = Gen.oneof [ Gen.constant 0 ; Gen.choose (1, 0xFFFF) ]
            let! recursion = if threadId = 0 then Gen.constant 0 else Gen.choose (0, 63)
            let! high = Gen.elements [ 0 ; 0x20000000 ; 0x40000000 ; 0x80000000 ; 0x60000000 ]

            let! extra =
                Gen.elements
                    [
                        0
                        indexOrHashBit
                        spinLockBit
                        hashCodeBit
                        indexOrHashBit ||| hashCodeBit
                    ]

            let thin = high ||| extra ||| (recursion <<< 16) ||| threadId
            let! arbitrary = ArbMap.defaults |> ArbMap.generate<int32>
            return! Gen.frequency [ 4, Gen.constant thin ; 1, Gen.constant arbitrary ]
        }

    let private indexGen : Gen<int> =
        Gen.oneof [ Gen.choose (1, 16) ; Gen.choose (1, indexMask) ; Gen.constant indexMask ]

    [<Test>]
    let ``inflating installs the index, keeps the high bits, and hands over the thin lock's owner`` () : unit =
        let property (word : int32, index : int) : unit =
            let threadId = word &&& 0xFFFF
            let recursion = (word >>> 16) &&& 0x3F

            if word &&& (indexOrHashBit ||| spinLockBit) <> 0 then
                ObjectHeaderInflation.inflate index word
                |> shouldEqual (HeaderWordInflation.Unmodelled word)
            elif threadId = 0 && recursion <> 0 then
                Assert.Throws<exn> (fun () -> ObjectHeaderInflation.inflate index word |> ignore)
                |> ignore
            else
                match ObjectHeaderInflation.inflate index word with
                | HeaderWordInflation.Unmodelled _ -> failwith $"0x%08x{word} holds a thin lock, but was refused"
                | HeaderWordInflation.FromThinLock (owner, after) ->
                    owner
                    |> shouldEqual (
                        if threadId = 0 then
                            None
                        else
                            Some
                                {
                                    ManagedThreadId = threadId
                                    RecursionLevel = recursion
                                }
                    )

                    after &&& indexOrHashBit |> shouldEqual indexOrHashBit
                    after &&& hashCodeBit |> shouldEqual 0
                    after &&& spinLockBit |> shouldEqual 0
                    after &&& indexMask |> shouldEqual index

                    // The bits above the index, the hash-code bit and the index-or-hash bit are
                    // the GC's and the spin lock's, which the VM leaves alone.
                    let above = ~~~(indexOrHashBit ||| hashCodeBit ||| indexMask)
                    after &&& above |> shouldEqual (word &&& above)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 5000,
            Prop.forAll (Arb.fromGen (Gen.zip wordGen indexGen)) property
        )

    [<TestCase(0)>]
    [<TestCase(-1)>]
    [<TestCase(0x04000000)>]
    let ``an index outside the sync block table's range is refused`` (index : int) : unit =
        Assert.Throws<exn> (fun () -> ObjectHeaderInflation.inflate index 0 |> ignore)
        |> ignore
