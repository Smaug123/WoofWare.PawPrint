namespace WoofWare.PawPrint.Test

open System
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ManagedPointerSource.tryContainerBase` states two runtime layout facts as
/// numbers: how many low bits of a container's address the runtime guarantees,
/// and how far into it the data starts. Nothing in the type system keeps those
/// honest, so both are measured here against the runtime running the test — and
/// compared in *both* directions, because a claim that is too strong is as wrong
/// as one that is too weak. Too weak refuses questions that have answers; too
/// strong answers questions that do not.
///
/// The oracle is a pinned `GCHandle`, whose `AddrOfPinnedObject` is documented to
/// give the first character of a string and the first element of an array — the
/// two addresses the model claims. Pinning is what makes the reading meaningful:
/// an unpinned object may move between the read and the assertion.
///
/// `docs/probes/byref-alignment/` has the same measurements at greater length, on
/// macOS arm64 and linux-x64, along with what they imply.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByrefAlignmentAgainstHost =

    /// Enough samples that a residue which merely *happened* to hold for one
    /// allocation cannot survive, and enough to see a bit that varies do so.
    let private samples : int = 200

    let private pinnedAddress (o : obj) : int64 =
        let handle = GCHandle.Alloc (o, GCHandleType.Pinned)

        try
            int64 (handle.AddrOfPinnedObject ())
        finally
            handle.Free ()

    /// The measurement the model is compared against: how many low bits of these
    /// addresses are the *same* in every sample, and what those bits are.
    ///
    /// A container start with `n` guaranteed zero bits, plus a fixed header, makes
    /// exactly the low `n` bits of the data address constant. So the widest width
    /// at which the samples agree is the alignment the runtime is really
    /// guaranteeing, and the value they agree on is the header's residue.
    let private determinedLowBits (addresses : int64 list) : int * int64 =
        // 6 is past any alignment this model could sensibly claim; if the samples
        // ever agreed that far the search would need widening, and the assertion
        // that the model matches would fail rather than silently cap.
        let widths =
            [ 0..6 ]
            |> List.filter (fun width ->
                let mask = (1L <<< width) - 1L

                addresses
                |> List.map (fun addr -> addr &&& mask)
                |> List.distinct
                |> List.length = 1
            )

        let width = List.max widths
        width, (List.head addresses &&& ((1L <<< width) - 1L))

    let private containerBaseOf (root : ByrefRoot) : ByrefContainerBase =
        match ManagedPointerSource.tryContainerBase (ManagedPointerSource.Byref (root, [])) with
        | Some containerBase -> containerBase
        | None ->
            failwith $"ManagedPointerSource.tryContainerBase makes no claim for %O{root}, so there is nothing to check."

    /// The claim and the measurement, checked against each other. `expectedResidue`
    /// is restated rather than derived so that a *matching* pair of wrong numbers —
    /// a header and an alignment that drifted together — still fails.
    let private checkAgainstHost
        (what : string)
        (root : ByrefRoot)
        (expectedResidue : int64)
        (addresses : int64 list)
        : unit
        =
        let claimed = containerBaseOf root
        let measuredBits, measuredResidue = determinedLowBits addresses

        if measuredBits <> claimed.AlignmentBits then
            failwith
                $"this runtime determines %d{measuredBits} low bits of a %s{what}'s data address, but ManagedPointerSource.tryContainerBase claims %d{claimed.AlignmentBits}. Claiming too few refuses masks that have answers; claiming too many answers masks that do not. The object layout this models has changed."

        let claimedResidue = claimed.HeaderBytes &&& ((1L <<< claimed.AlignmentBits) - 1L)

        if measuredResidue <> claimedResidue then
            failwith
                $"a %s{what}'s data sits at %d{measuredResidue} modulo %d{1L <<< measuredBits} on this runtime, but ManagedPointerSource.tryContainerBase's %d{claimed.HeaderBytes}-byte header makes it %d{claimedResidue}."

        measuredResidue |> shouldEqual expectedResidue

    [<Test>]
    let ``a string's characters are four modulo eight, and no better`` () : unit =
        // An 8-byte-aligned object plus a 12-byte header. This is the one container
        // whose data start is not itself aligned, and the residue is what
        // `UnicodeEncoding.GetByteCount`'s vectorised gate reads.
        [ 0 .. samples - 1 ]
        |> List.map (fun length -> pinnedAddress (System.String ('x', length)))
        |> checkAgainstHost "string" (ByrefRoot.StringCharAt (ManagedHeapAddress.ManagedHeapAddress 1, 0)) 4L

    [<Test>]
    let ``an array's elements are zero modulo eight, and no better`` () : unit =
        // A 16-byte header, which leaves the data 8-byte aligned.
        let root = ByrefRoot.ArrayElement (ManagedHeapAddress.ManagedHeapAddress 1, 0)

        [ 0 .. samples - 1 ]
        |> List.map (fun length -> pinnedAddress (Array.zeroCreate<byte> length))
        |> checkAgainstHost "byte array" root 0L

        [ 0 .. samples - 1 ]
        |> List.map (fun length -> pinnedAddress (Array.zeroCreate<int64> length))
        |> checkAgainstHost "int64 array" root 0L
