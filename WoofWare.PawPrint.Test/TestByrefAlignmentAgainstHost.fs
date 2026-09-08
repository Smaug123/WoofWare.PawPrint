namespace WoofWare.PawPrint.Test

open System
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ManagedPointerSource.tryContainerBase` states two runtime layout facts as
/// numbers: an object is 8-byte aligned, a string's characters sit 12 bytes into
/// it and an array's elements 16. Nothing in the type system keeps those honest,
/// so they are measured here against the runtime running the test.
///
/// The oracle is a pinned `GCHandle`, whose `AddrOfPinnedObject` is documented to
/// give the first character of a string and the first element of an array — the
/// two addresses the model claims. Pinning is what makes the reading meaningful:
/// an unpinned object may move between the read and the assertion.
///
/// `docs/probes/byref-alignment/` has the same measurements taken on macOS arm64
/// and linux-x64 at greater length, along with what they imply.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestByrefAlignmentAgainstHost =

    /// Enough samples that a residue which merely *happened* to hold for one
    /// allocation cannot survive, and enough for the 16-byte row below to see
    /// both of its values.
    let private samples : int = 200

    let private pinnedAddress (o : obj) : int64 =
        let handle = GCHandle.Alloc (o, GCHandleType.Pinned)

        try
            int64 (handle.AddrOfPinnedObject ())
        finally
            handle.Free ()

    [<Test>]
    let ``a string's characters are four modulo eight`` () : unit =
        // The claim being checked is `AlignmentBits = 3` with `HeaderBytes = 12`:
        // an 8-byte-aligned object plus a 12-byte header is 4 mod 8, every time.
        // This is the one container whose data start is not itself aligned, and
        // the residue is what `UnicodeEncoding.GetByteCount`'s vectorised gate
        // reads.
        for length in 0 .. samples - 1 do
            let residue = pinnedAddress (System.String ('x', length)) &&& 7L

            if residue <> 4L then
                failwith
                    $"a string of length %d{length} has its characters at %d{residue} mod 8 on this runtime, but ManagedPointerSource.tryContainerBase claims 12 bytes past an 8-byte-aligned object, which is 4. The object layout this models has changed."

    [<Test>]
    let ``an array's elements are zero modulo eight`` () : unit =
        // `HeaderBytes = 16` for an SZARRAY, which leaves the data 8-byte aligned.
        for length in 0 .. samples - 1 do
            for residue in
                [
                    pinnedAddress (Array.zeroCreate<byte> length) &&& 7L
                    pinnedAddress (Array.zeroCreate<char> length) &&& 7L
                    pinnedAddress (Array.zeroCreate<int64> length) &&& 7L
                ] do
                if residue <> 0L then
                    failwith
                        $"an array of length %d{length} has its elements at %d{residue} mod 8 on this runtime, but ManagedPointerSource.tryContainerBase claims a 16-byte header past an 8-byte-aligned object, which is 0. The object layout this models has changed."

    [<Test>]
    let ``sixteen-byte alignment really is undetermined`` () : unit =
        // The falsifier for the claim above: `AlignmentBits` is 3 and not 4, so a
        // mask of 15 is refused rather than answered. If every object were in fact
        // 16-byte aligned that refusal would be leaving a real answer on the table,
        // and this row is what would notice.
        let residues =
            [ 0 .. samples - 1 ]
            |> List.map (fun length -> pinnedAddress (System.String ('x', length)) &&& 15L)
            |> Set.ofList

        if Set.count residues < 2 then
            failwith
                $"every one of %d{samples} strings had its characters at the same address modulo 16 (%A{residues}). Objects would then be 16-byte aligned rather than 8-byte, and ManagedPointerSource.tryContainerBase is refusing masks of 15 that it could answer."

        // And they are the two an 8-byte-aligned object with a 12-byte header can
        // produce, rather than some third value that would mean the header had moved.
        residues |> shouldEqual (Set.ofList [ 4L ; 12L ])
