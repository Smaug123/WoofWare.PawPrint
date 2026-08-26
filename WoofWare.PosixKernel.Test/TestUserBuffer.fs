namespace WoofWare.PosixKernel.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The address check as a syscall sees it: over a buffer this kernel has
/// classified, rather than over a bare number.
///
/// The rows that matter are the two the classification exists for. `Opaque` and
/// `Addressless` both name memory this kernel cannot be handed the bytes of, and
/// they behave *differently* at the screen — one is a real address and passes,
/// the other is not an address and cannot be asked about. Collapsing them, in
/// either direction, is the mistake this fixture exists to catch.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUserBuffer =

    /// A machine that screens a buffer's address before performing the
    /// operation, as Linux does, with an arbitrary limit: only the screening
    /// behaviour is under test here.
    let private screening : UserBufferCheck =
        UserBufferCheck.BeforeOperation 0x0000_7FFF_FFFF_F000UL

    /// ...and one that does not, as Darwin does.
    let private atCopyTime : UserBufferCheck = UserBufferCheck.AtCopyTime

    let private both : UserBufferCheck list = [ screening ; atCopyTime ]

    [<Test>]
    let ``storage is never screened`` () : unit =
        // Not "screened and passed": real storage is inside the user address
        // space of every platform modelled here, so the range check is not
        // performed at all. Asserted at a length no address could accommodate,
        // which is what tells those two apart.
        for check in both do
            UserBufferCheck.faultsBeforeOperationFor check UserBuffer.Mapped System.UInt64.MaxValue
            |> shouldEqual (Ok false)

    [<Test>]
    let ``an opaque address passes every screen`` () : unit =
        // The whole point of the case: it is real mapped memory, so there is
        // nothing out of range about it, and it runs out of answer only where
        // bytes are wanted. A screen that refused it here would turn a call
        // that transfers nothing into a crash.
        for check in both do
            UserBufferCheck.faultsBeforeOperationFor check UserBuffer.Opaque 4096UL
            |> shouldEqual (Ok false)

    [<Test>]
    let ``an addressless buffer is refused only where the platform screens`` () : unit =
        // The asymmetry that stops the two unanswerable cases being one case.
        UserBufferCheck.faultsBeforeOperationFor screening UserBuffer.Addressless 4096UL
        |> shouldEqual (Error BufferRefusal.AddresslessAtScreen)

        UserBufferCheck.faultsBeforeOperationFor atCopyTime UserBuffer.Addressless 4096UL
        |> shouldEqual (Ok false)

    [<Test>]
    let ``an unmapped address is screened by the arithmetic`` () : unit =
        // The classified form must agree with the bare-number form it wraps, or
        // a caller that moved from one to the other changes what a guest reads.
        for address in
            [
                0UL
                1UL
                0x0000_7FFF_0000_0000UL
                0x0000_9000_0000_0000UL
                System.UInt64.MaxValue
            ] do
            for length in [ 0UL ; 1UL ; 4096UL ] do
                for check in both do
                    UserBufferCheck.faultsBeforeOperationFor check (UserBuffer.Unmapped address) length
                    |> shouldEqual (Ok (UserBufferCheck.faultsBeforeOperation check address length))

    [<Test>]
    let ``a null pointer is screened as the ordinary low address it is`` () : unit =
        // There is no `Null` case, and this is why: to a kernel, address 0 is a
        // low address that passes the range check like any other. The screens
        // that treat null specially belong to foreign-function layers, not here.
        UserBufferCheck.faultsBeforeOperationFor screening (UserBuffer.Unmapped 0UL) 4096UL
        |> shouldEqual (Ok false)

    [<Test>]
    let ``each refusal describes itself distinctly`` () : unit =
        // A client composes its half onto these, so two refusals that read alike
        // would be indistinguishable in a crash report.
        let described =
            [
                BufferRefusal.OpaqueAtTransfer
                BufferRefusal.AddresslessAtScreen
                BufferRefusal.AddresslessAtTransfer
            ]
            |> List.map BufferRefusal.describe

        described |> List.distinct |> List.length |> shouldEqual 3

        for text in described do
            text |> shouldNotEqual ""
