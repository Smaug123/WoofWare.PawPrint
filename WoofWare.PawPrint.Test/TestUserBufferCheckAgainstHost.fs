namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Measures how the *host* kernel screens a read buffer, and checks the model
/// against it.
///
/// What is checked is the shape of the rule — whether the kernel screens before
/// performing the operation, and whether it screens the range end or the
/// pointer — together with the model's arithmetic once handed this machine's
/// own limit. What is deliberately *not* checked is that limit's value: it
/// varies by machine (paging depth, virtual-address width), so demanding that
/// a host match the shipped default would be asserting that everyone runs the
/// same hardware. `EmulatedKernel.UserAddressLimit` carries it as configuration
/// for that reason, and `ObservedUserAddressLimit` records the values seen.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUserBufferCheckAgainstHost =

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private openFile(string path, int flags)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private readFd(int fd, nativeint buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private closeFd(int fd)

    [<Literal>]
    let private O_RDONLY = 0

    [<Literal>]
    let private EFAULT = 14

    /// Whether the host refuses `length` bytes at `address` on a descriptor with
    /// nothing to transfer.
    ///
    /// The file is empty, so there is no offset at which any byte could move:
    /// the kernel either refuses the range up front or reaches the end of the
    /// file and returns 0 without touching the buffer. That is what makes it
    /// safe to name arbitrary addresses here — nothing can be written through
    /// one. A positive return would mean that premise had broken, so it is a
    /// hard failure rather than a probe result.
    let private faultsAt (fd : int) (address : uint64) (length : uint64) : bool =
        Marshal.SetLastPInvokeError 0
        let transferred = readFd (fd, nativeint (int64 address), unativeint length)

        if transferred > 0n then
            failwith
                $"TestUserBufferCheckAgainstHost: read of %d{length} bytes at 0x%x{address} transferred %d{int transferred} bytes from a file that is supposed to be empty. The probe names addresses it does not own, so it must never reach a copy."

        if transferred = 0n then
            false
        else

        match Marshal.GetLastPInvokeError () with
        | e when e = EFAULT -> true
        | other ->
            failwith
                $"TestUserBufferCheckAgainstHost: read of %d{length} bytes at 0x%x{address} failed with errno %d{other}, which is neither success nor EFAULT. This probe can only interpret those two."

    /// The greatest range end this host accepts, for buffers of `length` bytes.
    ///
    /// `None` when nothing faults at any address, which is how a kernel with no
    /// up-front screen answers.
    let private measureLimit (fd : int) (length : uint64) : uint64 option =
        if not (faultsAt fd (UInt64.MaxValue - length) length) then
            None
        else

        if faultsAt fd 0UL length then
            failwith
                $"TestUserBufferCheckAgainstHost: this kernel refuses a %d{length}-byte buffer at address zero, so it screens something other than the range and the bisection below would be meaningless."

        // Invariant: `accepted` is accepted and `refused` is refused. The
        // predicate is monotone in the address for a fixed length, so bisection
        // converges on the boundary between them.
        let mutable accepted = 0UL
        let mutable refused = UInt64.MaxValue - length

        while refused - accepted > 1UL do
            let midpoint = accepted + (refused - accepted) / 2UL

            if faultsAt fd midpoint length then
                refused <- midpoint
            else
                accepted <- midpoint

        Some (accepted + length)

    let private withProbeDescriptor (action : int -> unit) : unit =
        let path = Path.GetTempFileName ()

        try
            FileInfo(path).Length |> shouldEqual 0L

            let fd = openFile (path, O_RDONLY)

            if fd < 0 then
                failwith
                    $"TestUserBufferCheckAgainstHost: could not open %s{path}: errno %d{Marshal.GetLastPInvokeError ()}"

            try
                action fd
            finally
                closeFd fd |> ignore<int>
        finally
            File.Delete path

    /// *Whether* this host screens up front, which is what the model derives
    /// from the flavour. The limit it screens at is a property of the machine
    /// and is deliberately not asserted here — see `EmulatedKernel.UserAddressLimit`.
    [<Test>]
    let ``the flavour decides whether this kernel screens up front`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            withProbeDescriptor (fun fd ->
                let screens = (measureLimit fd 1UL).IsSome

                screens
                |> shouldEqual (SimulatedUnixPlatform.screensUserBufferUpFront (HostPlatform.platformOf flavour))
            )
        )

    /// The screen is on the *range end* rather than on the pointer: raising the
    /// length by four lowers the last accepted address by exactly four. A model
    /// that compared the pointer alone would pass every constant-agnostic check
    /// but answer differently for every buffer longer than a byte.
    [<Test>]
    let ``this kernel screens the range end, not the pointer`` () : unit =
        HostPlatform.onUnixHost (fun _ ->
            withProbeDescriptor (fun fd ->
                match measureLimit fd 1UL, measureLimit fd 5UL with
                | None, None ->
                    Assert.Ignore "this kernel does not screen up front, so there is no boundary to compare"
                | Some atOne, Some atFive -> atFive |> shouldEqual atOne
                | one, five ->
                    failwith
                        $"this kernel screens at one length but not the other (1 byte: %O{one}, 5 bytes: %O{five}), which no rule modelled here can express."
            )
        )

    /// The model's arithmetic against a real kernel, at *this machine's* limit
    /// rather than at a modelled one. Configuring the model with the measured
    /// limit is what lets the two be compared without asserting that any
    /// particular machine's address space is the one PawPrint ships as default.
    [<Test>]
    let ``the model agrees with this kernel once given its limit`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            withProbeDescriptor (fun fd ->
                let check =
                    match measureLimit fd 1UL with
                    | None -> UserBufferCheck.AtCopyTime
                    | Some limit ->
                        EmulatedKernel.initial
                        |> EmulatedKernel.withUnixPlatformAndFileSystemType (HostPlatform.platformOf flavour) None
                        |> EmulatedKernel.withUserAddressLimit limit
                        |> EmulatedKernel.userBufferCheck

                let describe (refuses : bool) : string =
                    if refuses then "refuses" else "accepts"

                let agrees (address : uint64) (length : uint64) : unit =
                    let measured = faultsAt fd address length
                    let modelled = UserBufferCheck.faultsBeforeOperation check address length

                    if measured <> modelled then
                        failwith
                            $"read of %d{length} bytes at 0x%x{address}: this kernel %s{describe measured} it, the model %s{describe modelled} it."

                // Both ends of the space, and the boundary itself at three
                // lengths — including zero, which is where a kernel that
                // screened only non-empty buffers would diverge.
                agrees UInt64.MaxValue 0UL
                agrees UInt64.MaxValue 1UL
                agrees UInt64.MaxValue 5UL
                agrees 0UL 0UL
                agrees 0UL 5UL
                agrees 8UL 5UL

                match check with
                | UserBufferCheck.AtCopyTime -> ()
                | UserBufferCheck.BeforeOperation limit ->
                    agrees limit 0UL
                    agrees limit 1UL
                    agrees (limit - 1UL) 1UL
                    agrees (limit - 5UL) 5UL
                    agrees (limit - 4UL) 5UL
            )
        )
