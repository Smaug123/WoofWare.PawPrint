namespace WoofWare.PawPrint.Test

open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Storage-layer contract for static fields, checked against a reference model.
///
/// `TestThreadStatics` covers the *owner* component of a slot's key thoroughly, but its
/// isolation properties hold the declaring type and the field fixed while varying the owner.
/// Nothing there therefore separates storage that keys on all three components from storage
/// that quietly ignores one of the other two. This fixture varies all three.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStaticStorage =

    /// Three of each key component. Deliberately tiny: with 27 slots and writes drawn
    /// uniformly, a run revisits slots and lands on neighbours differing in exactly one
    /// component. A wide alphabet would put almost every write on a fresh slot, and storage
    /// that ignored a component would then agree with the model on nearly every draw.
    let private owners : StaticOwner[] =
        [|
            StaticOwner.Shared
            StaticOwner.OwnedBy (ThreadId 0)
            StaticOwner.OwnedBy (ThreadId 1)
        |]

    /// Nothing in `StaticStorage` dereferences either handle — they are opaque keys — so
    /// synthetic values keep these properties independent of any particular assembly.
    let private types : ConcreteTypeHandle[] =
        [|
            ConcreteTypeHandle.Concrete 1
            ConcreteTypeHandle.Concrete 2
            ConcreteTypeHandle.Concrete 3
        |]

    let private fields : ComparableFieldDefinitionHandle[] =
        // Row 0 is the null handle, so bias away from it.
        [| 1 ; 2 ; 3 |]
        |> Array.map (MetadataTokens.FieldDefinitionHandle >> ComparableFieldDefinitionHandle.Make)

    let private value (i : int) : CliType =
        CliType.Numeric (CliNumericType.Int32 i)

    /// Writes are drawn as *indices* into the alphabets, and the model is keyed on those
    /// indices rather than on the real key types. The model therefore cannot inherit a bug
    /// in `ConcreteTypeHandle`'s or `ComparableFieldDefinitionHandle`'s equality: if two
    /// distinct handles compared equal, the model would still tell them apart and the
    /// storage would not.
    let private genWrite : Gen<int * int * int * int> =
        gen {
            let! owner = Gen.choose (0, owners.Length - 1)
            let! ty = Gen.choose (0, types.Length - 1)
            let! field = Gen.choose (0, fields.Length - 1)
            let! v = Gen.choose (0, 5)
            return owner, ty, field, v
        }

    [<Test>]
    let ``every slot agrees with a model keyed on the whole triple`` () : unit =
        let property (writes : (int * int * int * int) list) : unit =
            let model : Map<int * int * int, int> =
                (Map.empty, writes)
                ||> List.fold (fun model (owner, ty, field, v) -> Map.add (owner, ty, field) v model)

            let storage : StaticStorage =
                (StaticStorage.empty, writes)
                ||> List.fold (fun storage (owner, ty, field, v) ->
                    StaticStorage.set owners.[owner] types.[ty] fields.[field] (value v) storage
                )

            // Every slot in the alphabet, not only the ones written: a slot that was never
            // written must still miss, which is what keeps zero-initialisation lazy.
            for owner in 0 .. owners.Length - 1 do
                for ty in 0 .. types.Length - 1 do
                    for field in 0 .. fields.Length - 1 do
                        StaticStorage.get owners.[owner] types.[ty] fields.[field] storage
                        |> shouldEqual (Map.tryFind (owner, ty, field) model |> Option.map value)

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen (Gen.listOf genWrite)) property)
