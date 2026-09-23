namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PosixKernel

/// The entropy syscall of the flavour this test process runs on — `getrandom(2)`
/// on Linux, `getentropy(2)` on Darwin — asked the same questions as the model,
/// and required to give the same answer: the byte count or the errno. The bytes
/// themselves are entropy and are not compared.
///
/// Every row names either real storage or an address the kernel refuses to
/// write through, so no row can write anywhere the test does not own.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEntropyAgainstHost =

    [<DllImport("libc", EntryPoint = "getrandom", SetLastError = true)>]
    extern nativeint private getrandom(nativeint buffer, unativeint count, uint32 flags)

    [<DllImport("libc", EntryPoint = "getentropy", SetLastError = true)>]
    extern int private getentropy(nativeint buffer, unativeint length)

    /// Where a row's buffer is.
    [<RequireQualifiedAccess>]
    type private Where =
        /// `StorageBytes` bytes the test allocated.
        | Storage
        /// Address zero, which no process has mapped.
        | Null
        /// The last address there is, past every machine's user address space.
        | Wild

    [<Literal>]
    let private StorageBytes : int = 4096

    let private classify (where : Where) : UserBuffer =
        match where with
        | Where.Storage -> UserBuffer.Mapped
        | Where.Null -> UserBuffer.Unmapped 0UL
        | Where.Wild -> UserBuffer.Unmapped UInt64.MaxValue

    let private withStorage (action : nativeint -> unit) : unit =
        let storage = Marshal.AllocHGlobal StorageBytes

        try
            action storage
        finally
            Marshal.FreeHGlobal storage

    let private address (storage : nativeint) (where : Where) : nativeint =
        match where with
        | Where.Storage -> storage
        | Where.Null -> 0n
        | Where.Wild -> -1n

    let private everywhere : Where list = [ Where.Storage ; Where.Null ; Where.Wild ]

    [<Literal>]
    let private EINTR : int = 4

    /// The return value, or the errno a -1 left behind.
    let private hostAnswer (returned : int64) : Result<int64, int> =
        if returned >= 0L then
            Ok returned
        else
            Error (Marshal.GetLastPInvokeError ())

    [<Test>]
    let ``getrandom agrees with this kernel`` () : unit =
        match HostPlatform.flavour () with
        | Some SimulatedUnixFlavour.Linux -> ()
        | _ -> Assert.Ignore "getrandom(2) is a Linux system call"

        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.linuxX64

        let flagsToTry : uint32 list = [ 0u .. 16u ] @ [ 0x8000_0000u ; UInt32.MaxValue ]

        withStorage (fun storage ->
            for where in everywhere do
                for count in [ 0UL ; 5UL ; 16UL ; uint64 StorageBytes ] do
                    for flags in flagsToTry do
                        let measured =
                            hostAnswer (int64 (getrandom (address storage where, unativeint count, flags)))

                        let modelled =
                            match UnixEntropy.getRandom (classify where) count flags system with
                            | Ok (GetRandomAnswer.Completed bytes, _) -> Ok (int64 bytes.Length)
                            | Ok (GetRandomAnswer.Failed error, _) -> Error (UnixError.toRawErrno error)
                            | Error refusal ->
                                failwith $"%A{where}, %d{count} bytes, flags 0x%x{flags}: refused, %O{refusal}"

                        if measured <> modelled then
                            failwith
                                $"getrandom into %A{where}, %d{count} bytes, flags 0x%x{flags}: this kernel answered %O{measured} (a count, or an errno), the model %O{modelled}."
        )

    /// However much is asked for, one call moves at most this much. Measured
    /// with a buffer that really can hold it, which is 2 GiB of this process's
    /// memory for the length of the call.
    [<Test>]
    let ``getrandom's largest transfer is this kernel's`` () : unit =
        match HostPlatform.flavour () with
        | Some SimulatedUnixFlavour.Linux -> ()
        | _ -> Assert.Ignore "getrandom(2) is a Linux system call"

        // The limit is a whole number of pages, and the model states it for the
        // x86-64 page size. A host with larger pages has a different limit and
        // says nothing about this one.
        if Environment.SystemPageSize <> 4096 then
            Assert.Ignore
                $"this host's pages are %d{Environment.SystemPageSize} bytes; the modelled limit is for 4096-byte pages"

        // A page beyond the largest transfer, so that a kernel that moved more
        // would still be writing into storage rather than faulting.
        let size = UnixEntropy.getRandomMaxTransfer + 4096UL
        let storage = Marshal.AllocHGlobal (nativeint size)

        try
            // A signal arriving mid-copy ends the call early with however much
            // had moved (or EINTR), and the runtime hosting this test signals
            // its own threads. So a short answer is retried, a bounded number of
            // times, and the claim is only that some attempt moves exactly the
            // limit and none moves more.
            let attempts = 5

            let rec measure (remaining : int) (seen : Result<int64, int> list) : unit =
                if remaining = 0 then
                    failwith
                        $"getrandom of UInt64.MaxValue bytes never moved the modelled %d{UnixEntropy.getRandomMaxTransfer} bytes in %d{attempts} attempts; this kernel answered %A{List.rev seen} (counts, or errnos). A short count is what an interrupted call returns, so persistent short counts suggest a different limit."
                else

                match hostAnswer (int64 (getrandom (storage, unativeint UInt64.MaxValue, 0u))) with
                | Ok moved when uint64 moved = UnixEntropy.getRandomMaxTransfer -> ()
                | Ok moved when uint64 moved > UnixEntropy.getRandomMaxTransfer ->
                    failwith
                        $"getrandom of UInt64.MaxValue bytes moved %d{moved}, more than the modelled %d{UnixEntropy.getRandomMaxTransfer}."
                | Error errno when errno <> EINTR ->
                    failwith $"getrandom of UInt64.MaxValue bytes failed with errno %d{errno}."
                | interrupted -> measure (remaining - 1) (interrupted :: seen)

            measure attempts []
        finally
            Marshal.FreeHGlobal storage

    [<Test>]
    let ``getentropy agrees with this kernel`` () : unit =
        match HostPlatform.flavour () with
        | Some SimulatedUnixFlavour.Darwin -> ()
        | _ -> Assert.Ignore "getentropy(2) is a Darwin system call"

        let system : UnixSystem<int, string> =
            UnixSystem.initial SimulatedUnixPlatform.macOsArm64

        let lengths : uint64 list =
            [
                0UL
                1UL
                5UL
                UnixEntropy.getEntropyMaxLength - 1UL
                UnixEntropy.getEntropyMaxLength
                UnixEntropy.getEntropyMaxLength + 1UL
                uint64 StorageBytes
                UInt64.MaxValue
            ]

        withStorage (fun storage ->
            for where in everywhere do
                for length in lengths do
                    let measured =
                        hostAnswer (int64 (getentropy (address storage where, unativeint length)))

                    let modelled =
                        match UnixEntropy.getEntropy (classify where) length system with
                        | Ok (GetEntropyAnswer.Completed _, _) -> Ok 0L
                        | Ok (GetEntropyAnswer.Failed error, _) -> Error (UnixError.toRawErrno error)
                        | Error refusal -> failwith $"%A{where}, %d{length} bytes: refused, %O{refusal}"

                    if measured <> modelled then
                        failwith
                            $"getentropy into %A{where}, %d{length} bytes: this kernel answered %O{measured} (0, or an errno), the model %O{modelled}."
        )
