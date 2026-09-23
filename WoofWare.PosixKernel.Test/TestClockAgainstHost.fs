namespace WoofWare.PosixKernel.Test

open System.Diagnostics
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// Measures the host's `clock_gettime` and checks the model's per-flavour decoding of
/// a clock id against it: which ids are `EINVAL`, and which clocks report only whole
/// microseconds.
///
/// macOS runs it locally and Linux in CI, so each run falsifies one flavour's column.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestClockAgainstHost =

    /// `struct timespec` on both 64-bit hosts this runs on: two 8-byte fields.
    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private Timespec =
        val mutable Seconds : int64
        val mutable Nanoseconds : int64

    [<DllImport("libc", EntryPoint = "clock_gettime", SetLastError = true)>]
    extern int private hostClockGettime(int clockId, Timespec& reading)

    [<Literal>]
    let private EINVAL = 22

    /// What the host did with a clock id.
    [<RequireQualifiedAccess>]
    type private HostOutcome =
        | Read of Timespec
        | Failed of errno : int

    let private hostRead (clockId : int) : HostOutcome =
        let mutable reading = Timespec ()
        Marshal.SetLastPInvokeError 0

        if hostClockGettime (clockId, &reading) = 0 then
            HostOutcome.Read reading
        else
            HostOutcome.Failed (Marshal.GetLastPInvokeError ())

    let private sweptIds : int list =
        [ -4096 .. 4096 ] @ [ System.Int32.MinValue ; System.Int32.MaxValue ]

    let private machineOn (flavour : SimulatedUnixFlavour) : UnixMachineState =
        (UnixSystem.initial<int, string> (HostPlatform.platformOf flavour)).Machine

    [<Test>]
    let ``an id is EINVAL here exactly when the host says so, unless it is refused`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let machine = machineOn flavour

            let disagreements =
                sweptIds
                |> List.choose (fun clockId ->
                    match UnixClock.clockGettime clockId machine, hostRead clockId with
                    // A refusal is this kernel declining to model a clock; the host may
                    // answer it or not, and either is consistent with declining.
                    | Error _, _ -> None
                    | Ok (Error UnixError.EINVAL), HostOutcome.Failed errno when errno = EINVAL -> None
                    | Ok (Ok _), HostOutcome.Read _ -> None
                    | modelled, host -> Some $"id %d{clockId}: model %A{modelled}, host %A{host}"
                )

            disagreements |> shouldEqual []
        )

    /// Whether the host's readings of `clockId`, taken over at least `window`, ever carry
    /// a digit finer than a microsecond.
    ///
    /// A window rather than a count, because a coarse clock repeats its value until the
    /// next timer tick: it needs time to pass, not calls, to show several values.
    let private showsSubMicrosecondDigits (clockId : int) (window : System.TimeSpan) : bool =
        let stopwatch = Stopwatch.StartNew ()
        let mutable seen = false

        while not seen && stopwatch.Elapsed < window do
            match hostRead clockId with
            | HostOutcome.Read reading -> seen <- reading.Nanoseconds % 1000L <> 0L
            | HostOutcome.Failed errno -> failwith $"clock id %d{clockId} answered once and then failed with %d{errno}"

        seen

    [<Test>]
    let ``a clock reports whole microseconds here exactly when it does on the host`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            // Two clocks, the monotonic one and the realtime one, and one reading
            // of each whose sub-microsecond part is non-zero: a clock that keeps
            // whole microseconds drops it, and one that does not keeps it.
            let machine =
                machineOn flavour
                |> UnixMachineState.withBootTime (UnixTimestamp.ofSeconds 1_000_000L)
                |> UnixMachineState.advanceClock 1_000_000_123L

            for clockId in [ 0..16 ] do
                match UnixClock.clockGettime clockId machine with
                | Ok (Ok reading) ->
                    let modelKeepsNanoseconds = UnixTimestamp.nanoseconds reading % 1000 <> 0

                    // Probability of a false negative for a fine clock is nil; for a coarse
                    // clock at a 4 ms tick, 50 ms shows a dozen values, each of which would
                    // have to land on a whole microsecond.
                    let hostKeepsNanoseconds =
                        showsSubMicrosecondDigits clockId (System.TimeSpan.FromMilliseconds 50.0)

                    if modelKeepsNanoseconds <> hostKeepsNanoseconds then
                        failwith
                            $"%O{flavour} clock id %d{clockId}: the model keeps sub-microsecond digits = %b{modelKeepsNanoseconds}, the host = %b{hostKeepsNanoseconds}"
                | Ok (Error _)
                | Error _ -> ()
        )
