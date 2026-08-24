namespace WoofWare.PawPrint.Test

open System
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// The rule by which a kernel accepts or refuses a user buffer before it
/// performs a read or write, and the two platforms' answers to it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUserBufferCheck =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 2000

    /// The limits worth exercising: x86-64's, arm64's, and degenerate ones that
    /// make the `length` term dominate.
    let private limits : uint64 list =
        [
            0UL
            1UL
            0x1000UL
            0x0000_7FFF_FFFF_F000UL
            0x0001_0000_0000_0000UL
            0x7FFF_FFFF_FFFF_FFFFUL
            UInt64.MaxValue
        ]

    /// Values chosen so that a range end lands exactly on a limit, one short of
    /// it, and one past it, rather than only in the interior.
    let private notableAddresses : uint64 list =
        [
            0UL
            1UL
            8UL
            0x0000_7FFF_FFFF_EFFFUL
            0x0000_7FFF_FFFF_F000UL
            0x0000_FFFF_FFFF_FFFFUL
            0x0001_0000_0000_0000UL
            0x7FFF_FFFF_FFFF_FFFEUL
            0x7FFF_FFFF_FFFF_FFFFUL
            0x8000_0000_0000_0000UL
            UInt64.MaxValue - 1UL
            UInt64.MaxValue
        ]

    let private notableLengths : uint64 list =
        [ 0UL ; 1UL ; 2UL ; 5UL ; 4096UL ; 0x7FFF_FFFF_FFFF_FFFFUL ; UInt64.MaxValue ]

    let private anyUInt64 : Gen<uint64> =
        gen {
            let! hi = Gen.choose (Int32.MinValue, Int32.MaxValue)
            let! lo = Gen.choose (Int32.MinValue, Int32.MaxValue)
            return (uint64 (uint32 hi) <<< 32) ||| uint64 (uint32 lo)
        }

    /// Uniformly-drawn triples cover the interior; the enumerated values above
    /// are what put a range end exactly on a boundary, which is where an
    /// off-by-one in the implementation's underflow-avoiding rearrangement
    /// would show.
    let private cases : Gen<uint64 * uint64 * uint64> =
        gen {
            let! highest = Gen.oneof [ Gen.elements limits ; anyUInt64 ]
            let! address = Gen.oneof [ Gen.elements notableAddresses ; anyUInt64 ]
            let! length = Gen.oneof [ Gen.elements notableLengths ; anyUInt64 ]
            return highest, address, length
        }

    /// The oracle, in arbitrary precision, so it cannot share a bug with the
    /// implementation: `bigint` addition cannot wrap, and the implementation
    /// must not either.
    let private naive (highest : uint64) (address : uint64) (length : uint64) : bool =
        bigint address + bigint length > bigint highest

    [<Test>]
    let ``agrees with arbitrary-precision arithmetic`` () : unit =
        let property (highest : uint64, address : uint64, length : uint64) : bool =
            let actual =
                UserBufferCheck.faultsBeforeOperation (UserBufferCheck.BeforeOperation highest) address length

            actual = naive highest address length

        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    /// The generator is supposed to produce both answers at every limit it
    /// draws. One that produced only refusals would make the property above
    /// true of an implementation that refuses everything.
    [<Test>]
    let ``the generated cases reach both answers`` () : unit =
        let answers =
            [
                for highest in limits do
                    for address in notableAddresses do
                        for length in notableLengths do
                            yield
                                UserBufferCheck.faultsBeforeOperation
                                    (UserBufferCheck.BeforeOperation highest)
                                    address
                                    length
            ]

        answers |> List.contains true |> shouldEqual true
        answers |> List.contains false |> shouldEqual true

    /// A range whose end exceeds `UInt64.MaxValue` must be a refusal rather
    /// than wrapping onto a low address the check would accept. This is the
    /// `sum >= (unsigned long)ptr` conjunct of x86-64's `__access_ok`.
    [<Test>]
    let ``a wrapping range is refused`` () : unit =
        let check = UserBufferCheck.BeforeOperation UInt64.MaxValue

        // Accepted only because they stop exactly at the top of the space.
        UserBufferCheck.faultsBeforeOperation check UInt64.MaxValue 0UL
        |> shouldEqual false

        UserBufferCheck.faultsBeforeOperation check (UInt64.MaxValue - 5UL) 5UL
        |> shouldEqual false

        UserBufferCheck.faultsBeforeOperation check 0UL UInt64.MaxValue
        |> shouldEqual false

        // One byte further wraps to zero, which is not a reason to accept it.
        UserBufferCheck.faultsBeforeOperation check UInt64.MaxValue 1UL
        |> shouldEqual true

        UserBufferCheck.faultsBeforeOperation check (UInt64.MaxValue - 4UL) 5UL
        |> shouldEqual true

        UserBufferCheck.faultsBeforeOperation check 1UL UInt64.MaxValue
        |> shouldEqual true

    [<Test>]
    let ``a platform that checks at copy time refuses nothing`` () : unit =
        let property (_highest : uint64, address : uint64, length : uint64) : bool =
            not (UserBufferCheck.faultsBeforeOperation UserBufferCheck.AtCopyTime address length)

        Check.One (config, Prop.forAll (Arb.fromGen cases) property)

    // ------------------------------------------------- the platforms' answers

    let private kernelOn (platform : SimulatedUnixPlatform) (limit : uint64) : EmulatedKernel =
        EmulatedKernel.initial
        |> EmulatedKernel.withUnixPlatformAndFileSystemType platform None
        |> EmulatedKernel.withUserAddressLimit limit

    /// macOS performs no up-front check at all: measured, every address at
    /// every size reads 0 from a descriptor with nothing to transfer. The
    /// machine's address-space limit is not consulted, so setting it to
    /// something absurd changes nothing.
    [<Test>]
    let ``Darwin checks at copy time`` () : unit =
        for limit in [ 1UL ; ObservedUserAddressLimit.X64FourLevelPaging ; UInt64.MaxValue ] do
            EmulatedKernel.userBufferCheck (kernelOn SimulatedUnixPlatform.macOsArm64 limit)
            |> shouldEqual UserBufferCheck.AtCopyTime

    /// The limit is the *machine's*, not the flavour's: the same Linux platform
    /// screens at whichever address space its host was configured with. Two
    /// GitHub runners of one image were measured disagreeing, which is why this
    /// is configuration rather than a constant.
    [<Test>]
    let ``Linux screens at the machine's limit`` () : unit =
        let observed =
            [
                ObservedUserAddressLimit.X64FourLevelPaging
                ObservedUserAddressLimit.X64FiveLevelPaging
                ObservedUserAddressLimit.Arm64FortyEightBit
            ]

        for limit in observed do
            EmulatedKernel.userBufferCheck (kernelOn SimulatedUnixPlatform.linuxX64 limit)
            |> shouldEqual (UserBufferCheck.BeforeOperation limit)

        // Every observed value is a real `TASK_SIZE_MAX`, so each is either a
        // power of two or one page below one. A typo in any of them shows here.
        for limit in observed do
            let isPowerOfTwo (value : uint64) =
                value <> 0UL && value &&& (value - 1UL) = 0UL

            (isPowerOfTwo limit || isPowerOfTwo (limit + 4096UL)) |> shouldEqual true

        // A machine with no user address space is not a machine.
        Assert.Throws<exn> (fun () ->
            EmulatedKernel.withUserAddressLimit 0UL EmulatedKernel.initial
            |> ignore<EmulatedKernel>
        )
        |> ignore<exn>

    /// The rows that separate x86-64's `TASK_SIZE_MAX` from arm64's. Getting
    /// this wrong in either direction is invisible to any test that only asks
    /// whether an obviously-bogus pointer faults.
    [<Test>]
    let ``Linux refuses a range that leaves the user address space`` () : unit =
        let check =
            EmulatedKernel.userBufferCheck (
                kernelOn SimulatedUnixPlatform.linuxX64 ObservedUserAddressLimit.X64FourLevelPaging
            )

        let faults (address : uint64) (length : uint64) : bool =
            UserBufferCheck.faultsBeforeOperation check address length

        // `(void*)-1` fails whatever the size.
        faults UInt64.MaxValue 5UL |> shouldEqual true
        faults UInt64.MaxValue 0UL |> shouldEqual true

        // The boundary is the *range end*, so the last accepted address depends
        // on the length asked for.
        faults 0x0000_7FFF_FFFF_F000UL 0UL |> shouldEqual false
        faults 0x0000_7FFF_FFFF_F000UL 1UL |> shouldEqual true
        faults 0x0000_7FFF_FFFF_EFFBUL 5UL |> shouldEqual false
        faults 0x0000_7FFF_FFFF_EFFCUL 5UL |> shouldEqual true

        // Strictly between x86-64's `TASK_SIZE_MAX` and arm64's 2^48, with room
        // for the length at both ends: an arm64 kernel accepts this range and
        // this one refuses it, so it is the row an architecture mix-up lands on
        // — and the reason the constant is measured rather than assumed.
        faults 0x0000_9000_0000_0000UL 5UL |> shouldEqual true
        faults 0x0000_FFFF_FFFF_FFFFUL 5UL |> shouldEqual true

        // Null and a low bit pattern are ordinary user addresses. Nothing about
        // this check refuses them; they fault, if at all, at the copy.
        faults 0UL 5UL |> shouldEqual false
        faults 8UL 5UL |> shouldEqual false

    // ------------------------------------------ classifying a buffer argument

    let private classify (arg : CliType) : BufferPointer =
        NativeSystemNative.bufferPointerArgument "test" "buffer" arg

    /// A guest's raw address reaches an entry point in either of two encodings,
    /// depending on whether it passed through a managed-reference conversion on
    /// the way. Both name the same address, so both must classify the same way;
    /// otherwise one route answers EFAULT and the other resolves a bit pattern
    /// to storage and aborts.
    [<Test>]
    let ``both encodings of a raw address agree`` () : unit =
        let expected = BufferPointer.RawAddress 8UL

        classify (CliType.RuntimePointer (CliRuntimePointer.Verbatim 8L))
        |> shouldEqual expected

        classify (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 8L)))
        |> shouldEqual expected

        classify (CliType.RuntimePointer (CliRuntimePointer.Managed (ManagedPointerSource.NativeIntPlaceholder 8L)))
        |> shouldEqual expected

        classify (
            CliType.Numeric (
                CliNumericType.NativeInt (NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder 8L))
            )
        )
        |> shouldEqual expected

    /// The null pointer is address zero rather than a case of its own, which is
    /// what lets `SystemNative_GetCwd` tell it from an unmapped address while
    /// every other caller collapses the two.
    [<Test>]
    let ``null classifies as address zero`` () : unit =
        let expected = BufferPointer.RawAddress 0UL

        classify (CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L))
        |> shouldEqual expected

        classify (CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
        |> shouldEqual expected

        classify (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)))
        |> shouldEqual expected

    /// `(byte*)-1` must arrive as the top of the address space rather than as a
    /// negative number narrowed or checked on the way in: it is the value the
    /// screen exists to reject.
    [<Test>]
    let ``a negative bit pattern is the top of the address space`` () : unit =
        classify (CliType.RuntimePointer (CliRuntimePointer.Verbatim -1L))
        |> shouldEqual (BufferPointer.RawAddress UInt64.MaxValue)

    /// A kernel of the given flavour on the commonest x86-64 machine. Only the
    /// screening behaviour is under test here, so the limit is arbitrary.
    let private kernelFor (platform : SimulatedUnixPlatform) : EmulatedKernel =
        kernelOn platform ObservedUserAddressLimit.X64FourLevelPaging

    let private storage : BufferPointer =
        BufferPointer.Storage (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 1), [])

    /// The difference of two pointers into separate storages, which PawPrint
    /// keeps synthetic because it has no number for it.
    let private crossStorageDifference : CliType =
        SyntheticCrossArrayOffset.make
            (ByteStorageIdentity.NativeMemory (NativeMemoryBlockId.NativeMemoryBlockId 1))
            0L
            (ByteStorageIdentity.NativeMemory (NativeMemoryBlockId.NativeMemoryBlockId 2))
            0L
        |> NativeIntSource.SyntheticCrossArrayOffset
        |> CliNumericType.NativeInt
        |> CliType.Numeric

    /// Classification is pure shape inspection and must stay total: entry points
    /// classify ahead of short-circuits that never inspect the buffer, so a
    /// value that cannot be a buffer still has to *classify* rather than abort.
    /// The refusals wait for something that actually needs the address.
    [<Test>]
    let ``a value with no address still classifies`` () : unit =
        match classify crossStorageDifference with
        | BufferPointer.Unstatable _ -> ()
        | other -> failwith $"expected a cross-storage difference to classify as Unstatable, got %O{other}"

    /// A platform that screens up front would have to compare an address that
    /// does not exist, so it refuses; one that screens nothing asks nothing and
    /// the call proceeds. Answering "in range" on the screening platform would
    /// be a guess, and a guest-visible one.
    [<Test>]
    let ``a value with no address is refused only where the platform screens`` () : unit =
        let unstatable = classify crossStorageDifference

        Assert.Throws<exn> (fun () ->
            NativeSystemNative.faultsBeforeOperation (kernelFor SimulatedUnixPlatform.linuxX64) unstatable 0
            |> ignore<bool>
        )
        |> ignore<exn>

        NativeSystemNative.faultsBeforeOperation (kernelFor SimulatedUnixPlatform.macOsArm64) unstatable 0
        |> shouldEqual false

        // Transferring through it refuses on every platform, though.
        Assert.Throws<exn> (fun () -> BufferPointer.dereferenceable unstatable |> ignore<ManagedPointerSource option>)
        |> ignore<exn>

    [<Test>]
    let ``a byref classifies as storage`` () : unit =
        classify (
            CliType.RuntimePointer (
                CliRuntimePointer.Managed (
                    ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 1), [])
                )
            )
        )
        |> shouldEqual storage

    /// Real allocated guest storage is never screened out: PawPrint's address
    /// space is a graph of typed cells, and a byref into it is a user address by
    /// construction whatever the simulated platform.
    [<Test>]
    let ``storage is never refused before the operation`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            for size in [ 0 ; 1 ; 5 ; Int32.MaxValue ] do
                NativeSystemNative.faultsBeforeOperation (kernelFor platform) storage size
                |> shouldEqual false

    /// ...whereas a raw address is screened, and only under the platform whose
    /// kernel screens at all.
    [<Test>]
    let ``a raw address is refused only where the platform screens`` () : unit =
        let wild = BufferPointer.RawAddress UInt64.MaxValue

        NativeSystemNative.faultsBeforeOperation (kernelFor SimulatedUnixPlatform.linuxX64) wild 5
        |> shouldEqual true

        NativeSystemNative.faultsBeforeOperation (kernelFor SimulatedUnixPlatform.linuxX64) wild 0
        |> shouldEqual true

        NativeSystemNative.faultsBeforeOperation (kernelFor SimulatedUnixPlatform.macOsArm64) wild 5
        |> shouldEqual false

        // Null is a raw address, and an entirely ordinary one.
        NativeSystemNative.faultsBeforeOperation
            (kernelFor SimulatedUnixPlatform.linuxX64)
            (BufferPointer.RawAddress 0UL)
            5
        |> shouldEqual false
