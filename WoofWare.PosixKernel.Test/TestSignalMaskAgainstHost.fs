namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open FsUnitTyped
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

    let private wordOfBuffer (buffer : byte[]) : uint64 = BitConverter.ToUInt64 (buffer, 0)

    let private bufferOfWord (word : uint64) : byte[] =
        let buffer : byte[] = Array.zeroCreate sigsetBytes
        BitConverter.GetBytes(word).CopyTo (buffer, 0)
        buffer

    [<Test>]
    let ``UnixSignal.pthreadSigmask answers as this host's pthread_sigmask, call for call`` () : unit =
        // Random sequences of calls on this thread, each made on the host and
        // on the model, compared answer by answer and mask by mask. The sets
        // name only signals the test host can leave blocked for a moment,
        // SIGKILL's and SIGSTOP's bits, and the bits one flavour treats
        // specially: Linux's 32 and 33, which glibc screens, and Darwin's bit
        // 31, which names no signal and is kept.
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let numbering = SimulatedUnixPlatform.signalNumbering platform
            let sigBlock, sigSetMask = howFor flavour

            let bitOf (signal : Signal) : int =
                Signal.toRawSignoUnder numbering signal - 1

            let bits =
                [
                    Signal.SIGHUP
                    Signal.SIGINT
                    Signal.SIGUSR2
                    Signal.SIGTERM
                    Signal.SIGALRM
                    Signal.SIGKILL
                    Signal.SIGSTOP
                ]
                |> List.map bitOf
                |> List.append (
                    match flavour with
                    | SimulatedUnixFlavour.Linux -> [ 31 ; 32 ]
                    | SimulatedUnixFlavour.Darwin -> [ 31 ]
                )

            let hows = [ sigBlock ; sigBlock + 1 ; sigSetMask ; 100 ]
            let rng = Random 20261008
            let original : byte[] = Array.zeroCreate sigsetBytes

            if pthread_sigmask (sigBlock, null, original) <> 0 then
                failwith "could not read this thread's mask"

            try
                for _ in 1..200 do
                    if pthread_sigmask (sigSetMask, bufferOfWord 0UL, null) <> 0 then
                        failwith "could not clear this thread's mask"

                    let mutable system : UnixSystem<int, unit> =
                        UnixSystem.initial platform
                        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

                    for _ in 1..10 do
                        let how = hows.[rng.Next hows.Length]

                        let set =
                            if rng.Next 6 = 0 then
                                None
                            else
                                Some (
                                    bits
                                    |> List.filter (fun _ -> rng.Next 2 = 0)
                                    |> List.fold (fun w b -> w ||| (1UL <<< b)) 0UL
                                )

                        let old : byte[] = Array.zeroCreate sigsetBytes

                        let rc =
                            pthread_sigmask (how, (set |> Option.map bufferOfWord |> Option.toObj), old)

                        let modelSet =
                            set
                            |> Option.map (fun word ->
                                match SignalMask.ofWord numbering word with
                                | Ok mask -> mask
                                | Error refusal -> failwith (SignalMaskRefusal.describe refusal)
                            )

                        match UnixSignal.pthreadSigmask 0 how modelSet system with
                        | Ok (modelOld, after) ->
                            (how, set, rc, SignalMask.toWord modelOld)
                            |> shouldEqual (how, set, 0, wordOfBuffer old)

                            system <- after
                        | Error errno ->
                            let expected =
                                UnixError.toRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno

                            (how, set, rc) |> shouldEqual (how, set, expected)

                        let now : byte[] = Array.zeroCreate sigsetBytes
                        pthread_sigmask (sigBlock, null, now) |> ignore<int>

                        (how, set, SignalState.maskOf 0 (UnixSystem.signals system) |> SignalMask.toWord)
                        |> shouldEqual (how, set, wordOfBuffer now)
            finally
                pthread_sigmask (sigSetMask, original, null) |> ignore<int>
        )
