namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Storage-layer contract for `[ThreadStatic]` fields: a thread-static has one slot per
/// thread, an ordinary static has exactly one shared slot, and a managed pointer to a
/// thread-static addresses the slot of the thread that *took* it, not of whichever thread
/// later dereferences it.
///
/// The end-to-end counterparts are `sourcesPure/ThreadStaticIsolation.cs` and
/// `sourcesPure/ThreadStaticCctorInitialiser.cs`, plus the PawPrint-only
/// `sourcesImpure/ThreadStaticByrefAcrossThreads.cs` (see that file for why the real runtime
/// cannot be its oracle).
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestThreadStatics =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
    // its sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private state () : IlMachineState =
        // Factory intentionally undisposed: state.Logger outlives this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// A `ConcreteTypeHandle` and a `ComparableFieldDefinitionHandle` are opaque keys as far as
    /// the statics map is concerned; nothing in `setStatic`/`getStatic` dereferences them, so
    /// synthetic values keep the storage properties independent of any particular assembly.
    let private typeHandle (i : int) : ConcreteTypeHandle = ConcreteTypeHandle.Concrete i

    let private fieldHandle (i : int) : ComparableFieldDefinitionHandle =
        // Row 0 is the null handle, so bias away from it.
        ComparableFieldDefinitionHandle.Make (MetadataTokens.FieldDefinitionHandle (i + 1))

    let private value (i : int) : CliType =
        CliType.Numeric (CliNumericType.Int32 i)

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private genOwner : Gen<StaticOwner> =
        Gen.oneof
            [
                Gen.constant StaticOwner.Shared
                Gen.map (fun i -> StaticOwner.OwnedBy (ThreadId i)) (Gen.choose (0, 4))
            ]

    /// Two distinct owners.
    let private genDistinctOwners : Gen<StaticOwner * StaticOwner> =
        gen {
            let! first = genOwner
            let! second = genOwner |> Gen.filter (fun o -> o <> first)
            return first, second
        }

    // ------------------------------------------------------------------
    // Property 1: round-trip
    // ------------------------------------------------------------------

    [<Test>]
    let ``getStatic recovers what setStatic wrote, for any owner`` () : unit =
        let property (owner : StaticOwner, ty : int, field : int, v : int) : unit =
            let ty = typeHandle ty
            let field = fieldHandle field

            state ()
            |> IlMachineState.setStatic owner ty field (value v)
            |> IlMachineState.getStatic owner ty field
            |> shouldEqual (Some (value v))

        let gen =
            gen {
                let! owner = genOwner
                let! ty = Gen.choose (0, 3)
                let! field = Gen.choose (0, 3)
                let! v = ArbMap.defaults |> ArbMap.generate<int>
                return owner, ty, field, v
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------
    // Property 2: isolation between owners
    // ------------------------------------------------------------------

    [<Test>]
    let ``a write under one owner is invisible under every other owner`` () : unit =
        let property (writer : StaticOwner, reader : StaticOwner, ty : int, field : int, v : int) : unit =
            let tyHandle = typeHandle ty
            let fieldHandle = fieldHandle field

            let after =
                state () |> IlMachineState.setStatic writer tyHandle fieldHandle (value v)

            IlMachineState.getStatic reader tyHandle fieldHandle after |> shouldEqual None

            // ... and the writer's own slot is untouched by the reader's absence.
            IlMachineState.getStatic writer tyHandle fieldHandle after
            |> shouldEqual (Some (value v))

        let gen =
            gen {
                let! writer, reader = genDistinctOwners
                let! ty = Gen.choose (0, 3)
                let! field = Gen.choose (0, 3)
                let! v = ArbMap.defaults |> ArbMap.generate<int>
                return writer, reader, ty, field, v
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``interleaved writes under distinct owners each keep their own value`` () : unit =
        let property (writes : (StaticOwner * int) list) : unit =
            let ty = typeHandle 0
            let field = fieldHandle 0

            let after =
                writes
                |> List.fold
                    (fun state (owner, v) -> IlMachineState.setStatic owner ty field (value v) state)
                    (state ())

            // The last write under each owner is what that owner sees.
            let expected =
                writes |> List.fold (fun acc (owner, v) -> Map.add owner v acc) Map.empty

            for owner in expected |> Map.toSeq |> Seq.map fst do
                IlMachineState.getStatic owner ty field after
                |> shouldEqual (Some (value expected.[owner]))

        let gen =
            Gen.listOf (
                gen {
                    let! owner = genOwner
                    let! v = ArbMap.defaults |> ArbMap.generate<int>
                    return owner, v
                }
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------
    // Property 3: zero-init (a never-written owner misses, so the caller zeroes)
    // ------------------------------------------------------------------

    [<Test>]
    let ``an owner that never wrote misses, whatever other owners wrote`` () : unit =
        let property (writes : (StaticOwner * int) list, reader : StaticOwner) : unit =
            let ty = typeHandle 0
            let field = fieldHandle 0
            let writes = writes |> List.filter (fun (owner, _) -> owner <> reader)

            let after =
                writes
                |> List.fold
                    (fun state (owner, v) -> IlMachineState.setStatic owner ty field (value v) state)
                    (state ())

            IlMachineState.getStatic reader ty field after |> shouldEqual None

        let gen =
            gen {
                let! writes =
                    Gen.listOf (
                        gen {
                            let! owner = genOwner
                            let! v = ArbMap.defaults |> ArbMap.generate<int>
                            return owner, v
                        }
                    )

                let! reader = genOwner
                return writes, reader
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------
    // Property 4: `StaticOwner.forField`, and the parse-time detection behind it
    // ------------------------------------------------------------------

    let private fieldNamed (typeName : string) (fieldName : string) (assy : DumpedAssembly) : FieldInfo<_, _> =
        assy.TypeDefs.Values
        |> Seq.filter (fun ty -> ty.Name = typeName)
        |> Seq.collect (fun ty -> ty.Fields)
        |> Seq.filter (fun field -> field.Name = fieldName)
        |> Seq.exactlyOne

    let private detectionAssembly : DumpedAssembly =
        let source =
            """
using System;

public class Detection
{
    [ThreadStatic] public static int ThreadStaticField;
    public static int OrdinaryStaticField;

    // `[ThreadStatic]` on an instance field is legal to *write* (the attribute targets any
    // field) and is ignored by the runtime, so it must not be detected as thread-static.
    [ThreadStatic] public int ThreadStaticInstanceField;
    public int OrdinaryInstanceField;

    // An unrelated field attribute must not be mistaken for [ThreadStatic].
    [Obsolete] public static int ObsoleteStaticField;
}
"""

        let image =
            Roslyn.compileAssembly
                "ThreadStaticDetectionTestAssembly"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (image)
        global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None stream

    [<Test>]
    let ``IsThreadStatic is exactly static-and-attributed`` () : unit =
        let isThreadStatic (name : string) : bool =
            (fieldNamed "Detection" name detectionAssembly).IsThreadStatic

        isThreadStatic "ThreadStaticField" |> shouldEqual true
        isThreadStatic "OrdinaryStaticField" |> shouldEqual false
        // The instance-field guard: the runtime ignores `[ThreadStatic]` on instance fields.
        isThreadStatic "ThreadStaticInstanceField" |> shouldEqual false
        isThreadStatic "OrdinaryInstanceField" |> shouldEqual false
        isThreadStatic "ObsoleteStaticField" |> shouldEqual false

    [<Test>]
    let ``forField owns exactly the thread-static fields`` () : unit =
        let ownerFor (name : string) (thread : ThreadId) : StaticOwner =
            StaticOwner.forField thread (fieldNamed "Detection" name detectionAssembly)

        let property (threadId : int) : unit =
            let thread = ThreadId threadId

            ownerFor "ThreadStaticField" thread |> shouldEqual (StaticOwner.OwnedBy thread)

            ownerFor "OrdinaryStaticField" thread |> shouldEqual StaticOwner.Shared

            ownerFor "ThreadStaticInstanceField" thread |> shouldEqual StaticOwner.Shared

            ownerFor "OrdinaryInstanceField" thread |> shouldEqual StaticOwner.Shared
            ownerFor "ObsoleteStaticField" thread |> shouldEqual StaticOwner.Shared

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int>) property)

    /// Detection must work against the real CoreLib, whose `[ThreadStatic]` fields are what
    /// actually changes behaviour. `System.Threading.Lock+ThreadId::t_threadId` is the
    /// motivating one: with a single shared slot, thread B reads A's cached OS thread id and
    /// `Lock` waves it through as a re-entrant owner.
    [<Test>]
    let ``CoreLib's Lock ThreadId cache is detected as thread-static`` () : unit =
        let lockThreadIdType =
            corelib.TypeDefs.Values
            |> Seq.filter (fun ty -> ty.Name = "ThreadId")
            |> Seq.filter (fun ty -> ty.Fields |> List.exists (fun f -> f.Name = "t_threadId"))
            |> Seq.exactlyOne

        let cached = lockThreadIdType.Fields |> List.find (fun f -> f.Name = "t_threadId")
        cached.IsStatic |> shouldEqual true
        cached.IsThreadStatic |> shouldEqual true

        // A neighbouring instance field on the same type is not.
        let instance = lockThreadIdType.Fields |> List.find (fun f -> f.Name = "_id")
        instance.IsThreadStatic |> shouldEqual false

        // ... nor is an ordinary CoreLib static.
        let stringEmpty =
            bct.String.Fields |> List.find (fun f -> f.Name = "Empty" && f.IsStatic)

        stringEmpty.IsThreadStatic |> shouldEqual false

    // ------------------------------------------------------------------
    // Property 5: ByteStorageIdentity ordering distinguishes owners
    // ------------------------------------------------------------------

    let private genStaticFieldIdentity : Gen<ByteStorageIdentity> =
        gen {
            let! ty = Gen.choose (0, 2)
            let! field = Gen.choose (0, 2)
            let! owner = genOwner
            return ByteStorageIdentity.StaticField (typeHandle ty, fieldHandle field, owner)
        }

    [<Test>]
    let ``static-field byte storage identities differ exactly when their components differ`` () : unit =
        let property ((ty1, f1, o1), (ty2, f2, o2)) : unit =
            let left = ByteStorageIdentity.StaticField (typeHandle ty1, fieldHandle f1, o1)
            let right = ByteStorageIdentity.StaticField (typeHandle ty2, fieldHandle f2, o2)

            let componentsEqual = ty1 = ty2 && f1 = f2 && o1 = o2

            (left = right) |> shouldEqual componentsEqual
            (ByteStorageIdentity.compare left right = 0) |> shouldEqual componentsEqual

        let genComponents =
            gen {
                let! ty = Gen.choose (0, 2)
                let! field = Gen.choose (0, 2)
                let! owner = genOwner
                return ty, field, owner
            }

        let gen =
            gen {
                let! left = genComponents
                let! right = genComponents
                return left, right
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``ByteStorageIdentity compare stays a total order across owners`` () : unit =
        let sign (n : int) : int = System.Math.Sign n

        let property (a : ByteStorageIdentity, b : ByteStorageIdentity, c : ByteStorageIdentity) : unit =
            let cmp = ByteStorageIdentity.compare

            // Antisymmetry.
            sign (cmp a b) |> shouldEqual (-(sign (cmp b a)))

            // Reflexivity.
            cmp a a |> shouldEqual 0

            // Transitivity.
            if sign (cmp a b) <= 0 && sign (cmp b c) <= 0 then
                (sign (cmp a c) <= 0) |> shouldEqual true

        // Mix in a non-StaticField identity so the cross-rank arms stay covered.
        let genIdentity =
            Gen.oneof
                [
                    genStaticFieldIdentity
                    Gen.map (fun i -> ByteStorageIdentity.Array (ManagedHeapAddress i)) (Gen.choose (0, 2))
                    Gen.map (fun i -> ByteStorageIdentity.StackLocal (ThreadId i, FrameId 0, 0us)) (Gen.choose (0, 2))
                ]

        let gen =
            gen {
                let! a = genIdentity
                let! b = genIdentity
                let! c = genIdentity
                return a, b, c
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------
    // Property 6: the byref-capture property - the one the whole design hangs on
    // ------------------------------------------------------------------

    /// `readManagedByref` / `writeManagedByref` take no `ThreadId` at all: the slot a
    /// thread-static byref reaches is fixed by the byref itself, which is exactly the .NET
    /// semantics (`ldsflda` resolves to a concrete per-thread address when it executes; the
    /// resulting managed pointer is a plain address, not a late-bound "current thread's
    /// slot" indirection).
    ///
    /// A design that re-resolved the owner from the accessing thread would pass every other
    /// property in this file and fail here.
    [<Test>]
    let ``a byref to a thread-static reads the slot recorded in the byref`` () : unit =
        let property (owners : StaticOwner list, target : StaticOwner) : unit =
            let ty = typeHandle 0
            let field = fieldHandle 0

            // Give every owner a distinct value, so reading the wrong slot cannot coincide.
            let owners = (target :: owners) |> List.distinct

            let state =
                owners
                |> List.indexed
                |> List.fold
                    (fun state (i, owner) -> IlMachineState.setStatic owner ty field (value i) state)
                    (state ())

            let targetIndex = owners |> List.findIndex (fun o -> o = target)

            let ptr = ManagedPointerSource.Byref (ByrefRoot.StaticField (ty, field, target), [])

            IlMachineState.readManagedByref bct state ptr |> shouldEqual (value targetIndex)

        let gen =
            gen {
                let! owners = Gen.listOf genOwner
                let! target = genOwner
                return owners, target
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``a byref to a thread-static writes only the slot recorded in the byref`` () : unit =
        let property (owners : StaticOwner list, target : StaticOwner, written : int) : unit =
            let ty = typeHandle 0
            let field = fieldHandle 0
            let owners = (target :: owners) |> List.distinct

            let before =
                owners
                |> List.indexed
                |> List.fold
                    (fun state (i, owner) -> IlMachineState.setStatic owner ty field (value i) state)
                    (state ())

            let ptr = ManagedPointerSource.Byref (ByrefRoot.StaticField (ty, field, target), [])

            let after = IlMachineState.writeManagedByref before ptr (value written)

            for i, owner in List.indexed owners do
                let expected = if owner = target then written else i

                IlMachineState.getStatic owner ty field after
                |> shouldEqual (Some (value expected))

        let gen =
            gen {
                let! owners = Gen.listOf genOwner
                let! target = genOwner
                let! written = Gen.choose (1000, 2000)
                return owners, target, written
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen gen) property)

    /// The concrete shape of the end-to-end case: thread 1 takes the byref, thread 2
    /// dereferences it, and must see thread 1's value rather than its own.
    [<Test>]
    let ``a byref taken on one thread still addresses that thread's slot`` () : unit =
        let ty = typeHandle 0
        let field = fieldHandle 0
        let threadA = StaticOwner.OwnedBy (ThreadId 1)
        let threadB = StaticOwner.OwnedBy (ThreadId 2)

        let state =
            state ()
            |> IlMachineState.setStatic threadA ty field (value 12345)
            |> IlMachineState.setStatic threadB ty field (value 0)

        // Taken while thread A was running.
        let capturedOnA =
            ManagedPointerSource.Byref (ByrefRoot.StaticField (ty, field, threadA), [])

        // Dereferenced later, with no reference to thread B's context - and the answer is
        // still A's slot.
        IlMachineState.readManagedByref bct state capturedOnA
        |> shouldEqual (value 12345)

        let after = IlMachineState.writeManagedByref state capturedOnA (value 999)

        IlMachineState.getStatic threadA ty field after
        |> shouldEqual (Some (value 999))

        IlMachineState.getStatic threadB ty field after |> shouldEqual (Some (value 0))

    /// Two byrefs to the same field owned by different threads must not compare equal, or the
    /// aliasing/overlap analysis would treat one thread's slot as the other's storage.
    [<Test>]
    let ``byrefs to different threads' slots of the same field are distinct`` () : unit =
        let ty = typeHandle 0
        let field = fieldHandle 0

        let ofOwner (owner : StaticOwner) : ManagedPointerSource =
            ManagedPointerSource.Byref (ByrefRoot.StaticField (ty, field, owner), [])

        let a = ofOwner (StaticOwner.OwnedBy (ThreadId 1))
        let b = ofOwner (StaticOwner.OwnedBy (ThreadId 2))
        let shared = ofOwner StaticOwner.Shared

        (a = b) |> shouldEqual false
        (a = shared) |> shouldEqual false
        (b = shared) |> shouldEqual false
        (a = ofOwner (StaticOwner.OwnedBy (ThreadId 1))) |> shouldEqual true

        ManagedPointerSource.tryByteOffsetWithinSameRoot a b |> shouldEqual None

        ManagedPointerSource.tryByteOffsetWithinSameRoot a (ofOwner (StaticOwner.OwnedBy (ThreadId 1)))
        |> shouldEqual (Some 0L)
