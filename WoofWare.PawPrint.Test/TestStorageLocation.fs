namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Laws of `StorageLocation.overlapVerdict`.
///
/// `overlapVerdict` is pure over `LocationResolution`, which is the whole point of splitting
/// it out of `shouldCopyBackwards`: the interesting failure mode has nothing to do with how a
/// pointer resolves and everything to do with how two resolutions of *differing precision*
/// combine. Testing it here needs no `IlMachineState`, so the laws can be stated directly
/// rather than smuggled through a guest program.
///
/// Note what is deliberately *not* asserted: that `resolve` produces any particular
/// resolution. That would restate `byteLocation`, whose behaviour this stage does not change.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStorageLocation =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// A deliberately tiny alphabet. Every law below is about *equality* of coarse keys and
    /// *ordering* of byte offsets, never about a key's content, so three distinct coarse keys
    /// and two distinct byte storages suffice to reach both the equal and unequal cases. What
    /// proves the cases are actually reached is the distribution check at the end of each
    /// property, not the width of the alphabet.
    let private coarseAlphabet : StorageLocation.SharedStorageKey list =
        [
            StorageLocation.SharedStorageKey.ArrayCell (ManagedHeapAddress 1, 0)
            StorageLocation.SharedStorageKey.ArrayCell (ManagedHeapAddress 1, 1)
            StorageLocation.SharedStorageKey.HeapValue (ManagedHeapAddress 2)
        ]

    let private storageAlphabet : ByteStorageIdentity list =
        [
            ByteStorageIdentity.Array (ManagedHeapAddress 1)
            ByteStorageIdentity.Array (ManagedHeapAddress 2)
        ]

    /// Offsets and byte counts are drawn with an explicit `Gen.choose`, not from FsCheck's
    /// default `int`: under `Quick` that is size-bounded to roughly [-100, 100], and deriving
    /// a whole case from one such value would explore only ~100 distinct cases however high
    /// `MaxTest` is set. The distribution checks below caught exactly that — the first draft
    /// of this file drove everything from one `NonNegativeInt` seed and never once produced a
    /// `CopyBackwards` verdict.
    let private genPrecise : Gen<(ByteStorageIdentity * int64) option> =
        Gen.frequency
            [
                1, Gen.constant None
                2,
                gen {
                    let! storage = Gen.elements storageAlphabet
                    let! offset = Gen.choose (0, 11)
                    return Some (storage, int64 offset)
                }
            ]

    let private genResolution : Gen<StorageLocation.LocationResolution> =
        Gen.frequency
            [
                1, Gen.constant StorageLocation.LocationResolution.Unrelatable
                6,
                gen {
                    let! coarse = Gen.elements coarseAlphabet
                    let! precise = genPrecise
                    return StorageLocation.LocationResolution.Located (coarse, precise)
                }
            ]

    let private genCase : Gen<StorageLocation.LocationResolution * StorageLocation.LocationResolution * int> =
        gen {
            let! src = genResolution
            let! dest = genResolution
            let! byteCount = Gen.choose (0, 11)
            return src, dest, byteCount
        }

    let private preciseOf (r : StorageLocation.LocationResolution) : (ByteStorageIdentity * int64) option =
        match r with
        | StorageLocation.LocationResolution.Located (_, precise) -> precise
        | StorageLocation.LocationResolution.Unrelatable -> None

    /// The pre-refactor decision, transcribed from `CellAwareMemOps.shouldCopyBackwards` as it
    /// stood before this stage. An independent statement of the arithmetic, so that a slip in
    /// rewriting the `match` shows up as a disagreement rather than as a silently reordered
    /// guard.
    let private referenceBackwards
        (src : (ByteStorageIdentity * int64) option)
        (dest : (ByteStorageIdentity * int64) option)
        (byteCount : int)
        : bool
        =
        match src, dest with
        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) when srcStorage = destStorage ->
            srcOffset < destOffset && destOffset < srcOffset + int64 byteCount
        | _ -> false

    /// The law Codex's review of the plan was about: when two byrefs share a coarse key but
    /// either lacks a flat coordinate, the direction is *not derivable*, and the verdict must
    /// say so. A resolution type that dropped its coarse key once a precise one was available
    /// could not state this law at all, because the pair would be incomparable.
    [<Test>]
    let ``equal coarse keys with either side imprecise is undecidable`` () : unit =
        let mutable observed = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    match src, dest with
                    | StorageLocation.LocationResolution.Located (srcCoarse, srcPrecise),
                      StorageLocation.LocationResolution.Located (destCoarse, destPrecise) when
                        srcCoarse = destCoarse && (srcPrecise.IsNone || destPrecise.IsNone)
                        ->
                        observed <- observed + 1

                        match StorageLocation.overlapVerdict src dest byteCount with
                        | StorageLocation.OverlapVerdict.Undecidable key -> key |> shouldEqual srcCoarse
                        | other ->
                            failwith
                                $"expected Undecidable for shared coarse key %A{srcCoarse} with an imprecise side, got %A{other}"
                    | _ -> ()
                )

        Check.One (propertyConfig, property)

        // Distribution check: the law is vacuous unless the shared-key-imprecise shape is
        // actually generated. If this were 0 the property above would pass on an
        // implementation that never returns `Undecidable` at all.
        if observed = 0 then
            failwith "property never generated a shared-coarse-key pair with an imprecise side"

    /// `CopyBackwards` is the only verdict that can corrupt data if wrong, so it must be
    /// earned: both sides precise, one byte storage, and `dest` starting strictly inside
    /// `src`'s range.
    [<Test>]
    let ``backwards is claimed only for a genuine forward overlap`` () : unit =
        let mutable observed = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    match StorageLocation.overlapVerdict src dest byteCount with
                    | StorageLocation.OverlapVerdict.CopyBackwards ->
                        observed <- observed + 1

                        match preciseOf src, preciseOf dest with
                        | Some (srcStorage, srcOffset), Some (destStorage, destOffset) ->
                            srcStorage |> shouldEqual destStorage
                            (srcOffset < destOffset) |> shouldEqual true
                            (destOffset < srcOffset + int64 byteCount) |> shouldEqual true
                        | _ ->
                            failwith
                                $"CopyBackwards claimed without a precise offset on both sides: %A{src} / %A{dest}"
                    | _ -> ()
                )

        Check.One (propertyConfig, property)

        if observed = 0 then
            failwith "property never produced a CopyBackwards verdict, so the law is vacuous"

    /// A non-byref endpoint shares storage with nothing, so a copy involving one can always
    /// run forwards. This is the arm that must never reach `Undecidable`.
    [<Test>]
    let ``an unrelatable endpoint always copies forwards`` () : unit =
        let mutable observed = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    if
                        src = StorageLocation.LocationResolution.Unrelatable
                        || dest = StorageLocation.LocationResolution.Unrelatable
                    then
                        observed <- observed + 1

                        StorageLocation.overlapVerdict src dest byteCount
                        |> shouldEqual StorageLocation.OverlapVerdict.CopyForwards
                )

        Check.One (propertyConfig, property)

        if observed = 0 then
            failwith "property never generated an Unrelatable endpoint"

    /// Behaviour preservation: for the both-precise case, the new verdict must agree with the
    /// arithmetic the pre-refactor `shouldCopyBackwards` performed.
    [<Test>]
    let ``both-precise agrees with the pre-refactor decision`` () : unit =
        let mutable observedBackwards = 0
        let mutable observedForwards = 0

        let property =
            Prop.forAll
                (Arb.fromGen genCase)
                (fun (src, dest, byteCount) ->
                    match preciseOf src, preciseOf dest with
                    | Some _, Some _ ->
                        let expected = referenceBackwards (preciseOf src) (preciseOf dest) byteCount

                        let actual =
                            match StorageLocation.overlapVerdict src dest byteCount with
                            | StorageLocation.OverlapVerdict.CopyBackwards -> true
                            | StorageLocation.OverlapVerdict.CopyForwards -> false
                            | StorageLocation.OverlapVerdict.Undecidable key ->
                                failwith $"both sides precise, yet the verdict was Undecidable %A{key}"

                        actual |> shouldEqual expected

                        if expected then
                            observedBackwards <- observedBackwards + 1
                        else
                            observedForwards <- observedForwards + 1
                    | _ -> ()
                )

        Check.One (propertyConfig, property)

        // Both outcomes must occur, or the agreement is only being checked on one branch.
        if observedBackwards = 0 then
            failwith "never observed a both-precise overlapping pair"

        if observedForwards = 0 then
            failwith "never observed a both-precise non-overlapping pair"

/// Resolution of real byrefs through `StorageLocation.resolve`, on a real machine state.
///
/// The pure laws above take `LocationResolution` values as given; these tests pin what
/// `resolve` actually *produces* for the shape whose mis-resolution was a measured bug:
/// two `ByrefRoot.HeapObjectField` roots on one heap object. Under
/// `[StructLayout(LayoutKind.Explicit)]` on a class two such fields can genuinely
/// overlap, so they must resolve into the *same* storage container (the precise half)
/// and carry *equal* coarse keys (the coarse half).
///
/// The two halves fail independently, which is why both are asserted here. The guest
/// `SpanMemmoveOverlappingExplicitLayoutClassFields.cs` catches only the precise half:
/// its reference-free fields resolve precisely on both sides, so `overlapVerdict` takes
/// the both-precise arm and never consults a coarse key. Reverting only
/// `SharedStorageKey.HeapObjectField` to a per-field key would leave every guest green
/// while restoring the original defect's shape on the degradation path — the case most
/// likely to be silently wrong, because it fires exactly when precision is unavailable.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStorageLocationResolve =

    /// Parsed once for all tests; DumpedAssembly is immutable, so sharing it
    /// under ParallelScope.All is safe.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

    let private objectHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Object

    let private freshState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory System.Collections.Immutable.ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private intField (name : string) (offset : int) (value : int) : CliField =
        {
            Id = FieldId.Named name
            Name = name
            Contents = CliType.Numeric (CliNumericType.Int32 value)
            Offset = Some offset
            Type = int32Handle
            MarshallingDescriptor = None
        }

    /// Allocate a heap object whose two int fields sit at explicit offsets 0 and 2 —
    /// the explicit-layout-class shape in which two distinct fields genuinely share
    /// bytes 2..4.
    let private allocateOverlappingFieldObject (state : IlMachineState) : IlMachineState * ManagedHeapAddress =
        let contents =
            CliValueType.OfFields
                baseClassTypes
                concreteTypes
                objectHandle
                {
                    IsValueType = true
                    IsEnum = false
                    NominalAlignment = None
                    // Every field carries a `FieldOffset`, which is what explicit layout means; the
                    // two deliberately overlap.
                    LayoutKind = TypeLayoutKind.Explicit
                    Layout = Layout.Default
                    CharSet = CharSetMetadata.ofTypeAttributes baseClassTypes.Object.TypeAttributes
                }
                [ intField "A" 0 7 ; intField "B" 2 9 ]

        let obj : AllocatedNonArrayObject =
            {
                Contents = contents
                ConcreteType = objectHandle
            }

        let addr, heap = ManagedHeap.allocateNonArray obj state.ManagedHeap

        { state with
            ManagedHeap = heap
        },
        addr

    let private fieldByref (addr : ManagedHeapAddress) (name : string) : ManagedPointerSource =
        ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, FieldId.Named name), [])

    [<Test>]
    let ``two field roots on one object share one storage container at their layout offsets`` () : unit =
        let state, addr = allocateOverlappingFieldObject (freshState ())

        let resolvedA = StorageLocation.resolve baseClassTypes state (fieldByref addr "A")
        let resolvedB = StorageLocation.resolve baseClassTypes state (fieldByref addr "B")

        match resolvedA, resolvedB with
        | StorageLocation.LocationResolution.Located (coarseA, Some (containerA, offsetA)),
          StorageLocation.LocationResolution.Located (coarseB, Some (containerB, offsetB)) ->
            // The coarse half: "could these share storage" is answered per-object,
            // because field layout — the only thing that could prove two fields of one
            // object disjoint — is exactly what the coarse key cannot consult.
            coarseA |> shouldEqual coarseB
            coarseA |> shouldEqual (StorageLocation.SharedStorageKey.HeapObjectField addr)

            // The precise half: one heap object is one container, and each field is a
            // view into it at its layout offset.
            containerA |> shouldEqual (ByteStorageIdentity.HeapObject addr)
            containerB |> shouldEqual (ByteStorageIdentity.HeapObject addr)
            offsetA |> shouldEqual 0L
            offsetB |> shouldEqual 2L
        | other -> failwith $"expected both byrefs to resolve precisely, got %A{other}"

        // End-to-end at this level: a four-byte copy from A to B overlaps (src 0 < dest 2
        // < src + 4), so the verdict must be the backwards loop. Before the fix the two
        // sides carried distinct per-field containers and this was CopyForwards — the
        // measured corruption in SpanMemmoveOverlappingExplicitLayoutClassFields.cs.
        StorageLocation.overlapVerdict resolvedA resolvedB 4
        |> shouldEqual StorageLocation.OverlapVerdict.CopyBackwards

    [<Test>]
    let ``field roots on different objects stay distinct`` () : unit =
        let state, addr1 = allocateOverlappingFieldObject (freshState ())
        let state, addr2 = allocateOverlappingFieldObject state

        let resolved1 = StorageLocation.resolve baseClassTypes state (fieldByref addr1 "A")
        let resolved2 = StorageLocation.resolve baseClassTypes state (fieldByref addr2 "A")

        match resolved1, resolved2 with
        | StorageLocation.LocationResolution.Located (coarse1, Some (container1, _)),
          StorageLocation.LocationResolution.Located (coarse2, Some (container2, _)) ->
            // Collapsing per-field keys to per-object must not over-collapse: separate
            // allocations remain separate storage.
            (coarse1 = coarse2) |> shouldEqual false
            (container1 = container2) |> shouldEqual false
        | other -> failwith $"expected both byrefs to resolve precisely, got %A{other}"

        StorageLocation.overlapVerdict resolved1 resolved2 4
        |> shouldEqual StorageLocation.OverlapVerdict.CopyForwards

    [<Test>]
    let ``an unresolvable projection degrades to the shared coarse key, not to disjointness`` () : unit =
        let state, addr = allocateOverlappingFieldObject (freshState ())

        // A projection to a field the object does not have defeats
        // `tryProjectionByteOffset`, exercising the documented degradation: the walk
        // raises, the resolver catches, and the resolution keeps its coarse key with no
        // precise coordinate. No guest-constructible shape is known to defeat precision
        // for a heap-field root (field chains resolve against real templates, and
        // `ReinterpretAs` targets are concretized by construction), so the degradation
        // path is driven directly here rather than through a guest that would be
        // vacuous.
        let degraded =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapObjectField (addr, FieldId.Named "A"),
                [ ByrefProjection.Field (FieldId.Named "NoSuchField") ]
            )

        let resolvedDegraded = StorageLocation.resolve baseClassTypes state degraded
        let resolvedB = StorageLocation.resolve baseClassTypes state (fieldByref addr "B")

        // Precondition: the projection really did defeat precision. If resolution ever
        // learns to answer it, this test must fail loudly here rather than silently
        // asserting nothing about the degradation path.
        match resolvedDegraded with
        | StorageLocation.LocationResolution.Located (coarse, None) ->
            coarse |> shouldEqual (StorageLocation.SharedStorageKey.HeapObjectField addr)
        | other -> failwith $"expected the bad projection to lose precision but keep its coarse key, got %A{other}"

        // The verdict the coarse keys exist to force: same object, cross-field, no
        // precise coordinate on one side — undecidable, so the caller fails loud. With
        // per-field coarse keys this arm answered CopyForwards, silently reasserting the
        // disjointness the layout does not guarantee.
        StorageLocation.overlapVerdict resolvedDegraded resolvedB 4
        |> shouldEqual (
            StorageLocation.OverlapVerdict.Undecidable (StorageLocation.SharedStorageKey.HeapObjectField addr)
        )

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Byte

    let private byteType : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup byteHandle concreteTypes |> Option.get

    let private byteField (name : string) (offset : int) : CliField =
        {
            Id = FieldId.Named name
            Name = name
            Contents = CliType.Numeric (CliNumericType.UInt8 0uy)
            Offset = Some offset
            Type = byteHandle
            MarshallingDescriptor = None
        }

    /// The issue's repro type: two byte fields one byte apart.
    let private allocateAdjacentByteFieldObject (state : IlMachineState) : IlMachineState * ManagedHeapAddress =
        let contents =
            CliValueType.OfFields
                baseClassTypes
                concreteTypes
                objectHandle
                {
                    IsValueType = true
                    IsEnum = false
                    NominalAlignment = None
                    LayoutKind = TypeLayoutKind.Explicit
                    Layout = Layout.Default
                    CharSet = CharSetMetadata.ofTypeAttributes baseClassTypes.Object.TypeAttributes
                }
                [ byteField "A" 0 ; byteField "B" 1 ]

        let obj : AllocatedNonArrayObject =
            {
                Contents = contents
                ConcreteType = objectHandle
            }

        let addr, heap = ManagedHeap.allocateNonArray obj state.ManagedHeap

        { state with
            ManagedHeap = heap
        },
        addr

    /// Issue #993, at the level of the thing that actually resolves it.
    ///
    /// `Unsafe.AddByteOffset (ref o.B, Int32.MaxValue)` and
    /// `Unsafe.AddByteOffset (ref o.A, Int32.MinValue)` are `Int32.MaxValue + 1` and
    /// `Int32.MinValue` bytes from the object base — 2^32 apart, and real .NET reports them as
    /// different addresses. Folded into an `int`, `B`'s coordinate wrapped onto exactly
    /// `Int32.MinValue`, so the two resolved to *one* coordinate in *one* container: an
    /// arbitrarily strong claim of aliasing, from arithmetic that had silently lost a bit.
    [<Test>]
    let ``two byrefs 2^32 bytes apart do not resolve to one coordinate`` () : unit =
        let state, addr = allocateAdjacentByteFieldObject (freshState ())

        let displaced (name : string) (byteOffset : int) : ManagedPointerSource =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapObjectField (addr, FieldId.Named name),
                [
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset byteOffset
                ]
            )

        let resolvedA =
            StorageLocation.resolve baseClassTypes state (displaced "A" System.Int32.MinValue)

        let resolvedB =
            StorageLocation.resolve baseClassTypes state (displaced "B" System.Int32.MaxValue)

        match resolvedA, resolvedB with
        | StorageLocation.LocationResolution.Located (_, Some (containerA, offsetA)),
          StorageLocation.LocationResolution.Located (_, Some (containerB, offsetB)) ->
            // One object is one container, so the *container* halves agreeing is correct and is
            // not what was wrong; asserting it keeps the test honest about which half moved.
            containerA |> shouldEqual (ByteStorageIdentity.HeapObject addr)
            containerB |> shouldEqual (ByteStorageIdentity.HeapObject addr)

            offsetA |> shouldEqual -2147483648L
            offsetB |> shouldEqual 2147483648L
        | other -> failwith $"expected both byrefs to resolve precisely, got %A{other}"

    /// The consequence for the consumer that reads these coordinates to choose a copy direction.
    ///
    /// The destination here is 2^32 + 2 bytes past the source, so the two ranges are nowhere
    /// near each other and a forward loop is correct. Folded into an `int` the destination
    /// landed at *2*, two bytes inside a four-byte source range, and `overlapVerdict` claimed
    /// the backwards loop — a copy run in the wrong direction, which is the shape that corrupts
    /// data rather than merely reporting a wrong number.
    ///
    /// The chain is constructed directly because `appendProjection` coalesces adjacent
    /// `ByteOffset`s, so no sequence of `Unsafe.AddByteOffset` calls produces this exact shape;
    /// the shape a guest *does* produce (offsets separated by a `Field`, which does not
    /// coalesce) is covered end to end by `sourcesImpure/UnsafeByteOffsetInt32Overflow.cs`. What
    /// is being tested here is the resolver, and it sees chains from both routes.
    [<Test>]
    let ``a copy whose endpoints are 2^32 bytes apart is not an overlap`` () : unit =
        let state, addr = allocateAdjacentByteFieldObject (freshState ())

        let source =
            ManagedPointerSource.Byref (ByrefRoot.HeapObjectField (addr, FieldId.Named "A"), [])

        let destination =
            ManagedPointerSource.Byref (
                ByrefRoot.HeapObjectField (addr, FieldId.Named "A"),
                [
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset System.Int32.MaxValue
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset System.Int32.MaxValue
                    ByrefProjection.ReinterpretAs byteType
                    ByrefProjection.ByteOffset 4
                ]
            )

        let resolvedSource = StorageLocation.resolve baseClassTypes state source

        let resolvedDestination = StorageLocation.resolve baseClassTypes state destination

        // Premise, so a failure says which half broke: the destination really is 2^32 + 2 bytes
        // along. (`A` sits at offset 0, and the three cursors total 2^32 + 2.)
        match resolvedSource, resolvedDestination with
        | StorageLocation.LocationResolution.Located (_, Some (_, sourceOffset)),
          StorageLocation.LocationResolution.Located (_, Some (_, destinationOffset)) ->
            sourceOffset |> shouldEqual 0L
            destinationOffset |> shouldEqual 4294967298L
        | other -> failwith $"expected both byrefs to resolve precisely, got %A{other}"

        StorageLocation.overlapVerdict resolvedSource resolvedDestination 4
        |> shouldEqual StorageLocation.OverlapVerdict.CopyForwards
