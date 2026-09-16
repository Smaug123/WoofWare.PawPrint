namespace WoofWare.PosixKernel.Test

open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PosixKernel

/// `Signal.isUnblockableUnder`'s column for the flavour this test process runs
/// on, measured against the machine itself: for every signo the kernel has,
/// add it to the calling thread's mask with `pthread_sigmask(3)`, read the
/// mask back, and compare membership with the model. `TestSignal` restates
/// both columns as literals; this is what keeps the restatement honest, the
/// way `TestSignalAgainstHost` does for the signo table.
///
/// Safe to run in the test host, unlike a `sigaction` sweep (see
/// `TestSignalAgainstHost`'s header for why that one is probe-pinned
/// instead): a mask is per-thread, so nothing outside this test's own thread
/// is touched; the original mask is restored after every signo; and the
/// signals whose membership we assert *absent* never enter the mask at all —
/// that is the fact under test.
///
/// The single-signal set is built by setting the signo's bit directly rather
/// than through `sigaddset(3)`, so that libc's `sigaddset`-level screening
/// cannot hide what the mask call itself does with the bit. The bit layout —
/// bit `signo - 1`, little-endian — is glibc's `unsigned long` array and
/// Darwin's `uint32_t` alike, on every architecture .NET supports.
[<TestFixture>]
module TestSignalMaskAgainstHost =

    [<DllImport("libc", SetLastError = true)>]
    extern int private pthread_sigmask(int how, byte[] set, byte[] oldSet)

    /// A comfortable margin over both platforms' `sigset_t`: glibc's is 128
    /// bytes and Darwin's is 4, and the callee reads only its own size.
    [<Literal>]
    let private sigsetBytes : int = 256

    /// `SIG_BLOCK` and `SIG_SETMASK` for this host: 0 and 2 on Linux, 1 and 3
    /// on Darwin (from each libc's `signal.h`).
    let private howFor (flavour : SimulatedUnixFlavour) : int * int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 0, 2
        | SimulatedUnixFlavour.Darwin -> 1, 3

    let private setBit (buffer : byte[]) (signo : int) : unit =
        buffer.[(signo - 1) / 8] <- buffer.[(signo - 1) / 8] ||| (1uy <<< ((signo - 1) % 8))

    let private getBit (buffer : byte[]) (signo : int) : bool =
        (buffer.[(signo - 1) / 8] >>> ((signo - 1) % 8)) &&& 1uy = 1uy

    /// Whether this host lets the calling thread block `signo`: block it,
    /// read the mask back, restore the original mask.
    let private hostCanBlock (flavour : SimulatedUnixFlavour) (signo : int) : bool =
        let sigBlock, sigSetMask = howFor flavour
        let one : byte[] = Array.zeroCreate sigsetBytes
        setBit one signo
        let original : byte[] = Array.zeroCreate sigsetBytes

        let rc = pthread_sigmask (sigBlock, one, original)

        if rc <> 0 then
            failwith $"pthread_sigmask(SIG_BLOCK, {{%d{signo}}}) failed with %d{rc}, which the model has no row for"

        try
            let current : byte[] = Array.zeroCreate sigsetBytes
            let rc = pthread_sigmask (sigBlock, null, current)

            if rc <> 0 then
                failwith $"pthread_sigmask read-back for signo %d{signo} failed with %d{rc}"

            getBit current signo
        finally
            pthread_sigmask (sigSetMask, original, null) |> ignore<int>

    [<Test>]
    let ``isUnblockableUnder agrees with this host's pthread_sigmask about every signo`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let numbering =
                SimulatedUnixPlatform.signalNumbering (HostPlatform.platformOf flavour)

            let disagreements =
                [ 1 .. Signal.highestSignoUnder numbering ]
                |> List.choose (fun signo ->
                    let signal =
                        match Signal.ofRawSignoUnder numbering signo with
                        | ValueSome signal -> signal
                        | ValueNone ->
                            failwith
                                $"%O{numbering}: signo %d{signo} is within the model's range but ofRawSignoUnder refused it"

                    let modelled = not (Signal.isUnblockableUnder numbering signal)
                    let measured = hostCanBlock flavour signo

                    if modelled = measured then
                        None
                    else
                        Some
                            $"signo %d{signo} (%O{signal}): model says blockable=%b{modelled}, host says %b{measured}"
                )

            if not (List.isEmpty disagreements) then
                let report = String.concat "; " disagreements

                failwith $"%O{numbering}: isUnblockableUnder disagrees with this host's pthread_sigmask: %s{report}"
        )
