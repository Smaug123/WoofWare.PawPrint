namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Pins `ThreadStatus.managedThreadState`, the answer `ThreadNative_GetThreadState` gives.
/// The guests `ThreadStateObserved.cs` and `ThreadStateOfEntryThreadAfterMain.cs` check the
/// rows a guest can reach against real .NET; this checks the laws every row obeys, and the
/// rows no guest can reach.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestManagedThreadState =

    let private reported : System.Threading.ThreadState list =
        [
            System.Threading.ThreadState.Background
            System.Threading.ThreadState.Unstarted
            System.Threading.ThreadState.Stopped
            System.Threading.ThreadState.WaitSleepJoin
        ]

    let private has (bit : System.Threading.ThreadState) (s : System.Threading.ThreadState) : bool = s &&& bit = bit

    let private statusGen : Gen<ThreadStatus> =
        ArbMap.defaults |> ArbMap.generate<ThreadStatus>

    [<Test>]
    let ``Every status obeys CoreCLR's snapshot laws`` () : unit =
        let property
            (runtime : EmulatedRuntime, status : ThreadStatus, isBackground : bool, inWaitSleepJoin : bool)
            : unit
            =
            let s = ThreadStatus.managedThreadState runtime isBackground inWaitSleepJoin status

            let allReported =
                reported
                |> List.fold (fun acc bit -> acc ||| bit) System.Threading.ThreadState.Running

            // CoreCLR never reports StopRequested, SuspendRequested, Suspended, AbortRequested
            // or Aborted: .NET has no thread abort or suspension.
            s &&& ~~~allReported |> shouldEqual System.Threading.ThreadState.Running

            has System.Threading.ThreadState.Unstarted s
            |> shouldEqual (
                match status with
                | ThreadStatus.NotStarted _ -> true
                | _ -> false
            )

            has System.Threading.ThreadState.Stopped s
            |> shouldEqual (
                match status with
                | ThreadStatus.Terminated
                | ThreadStatus.WaitingForForegroundThreads -> true
                | _ -> false
            )

            // A dead thread's TS_Background is cleared; everywhere else the flag shows through.
            has System.Threading.ThreadState.Background s
            |> shouldEqual (isBackground && status <> ThreadStatus.Terminated)

            // The flag decides the Background bit and nothing else.
            let other =
                ThreadStatus.managedThreadState runtime (not isBackground) inWaitSleepJoin status

            s &&& ~~~System.Threading.ThreadState.Background
            |> shouldEqual (other &&& ~~~System.Threading.ThreadState.Background)

            if has System.Threading.ThreadState.Unstarted s then
                has System.Threading.ThreadState.WaitSleepJoin s |> shouldEqual false

            match runtime with
            | EmulatedRuntime.Net10 ->
                // A wait-handle wait reports WaitSleepJoin exactly when CoreCLR would make it
                // alertably.
                match status with
                | ThreadStatus.BlockedOnWaitHandle (_, _, alertability) ->
                    has System.Threading.ThreadState.WaitSleepJoin s
                    |> shouldEqual (alertability = WaitAlertability.Alertable)
                | _ -> ()
            | EmulatedRuntime.Net11 ->
                // WaitSleepJoin is the managed wait subsystem's flag and nothing else; every other
                // bit is .NET 10's.
                has System.Threading.ThreadState.WaitSleepJoin s |> shouldEqual inWaitSleepJoin

                s &&& ~~~System.Threading.ThreadState.WaitSleepJoin
                |> shouldEqual (
                    ThreadStatus.managedThreadState EmulatedRuntime.Net10 isBackground false status
                    &&& ~~~System.Threading.ThreadState.WaitSleepJoin
                )

        let gen =
            gen {
                let! runtime = Gen.elements EmulatedRuntime.supported
                let! status = statusGen
                let! isBackground = ArbMap.defaults |> ArbMap.generate<bool>

                // Only .NET 11's CoreLib can set the flag, and only on a started, live thread.
                let! inWaitSleepJoin =
                    match runtime, status with
                    | EmulatedRuntime.Net10, _
                    | _, ThreadStatus.NotStarted _
                    | _, ThreadStatus.Terminated -> Gen.constant false
                    | EmulatedRuntime.Net11, _ -> ArbMap.defaults |> ArbMap.generate<bool>

                return runtime, status, isBackground, inWaitSleepJoin
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)

    /// No guest can reach these: a `LowLevelMonitor` and a trivial-wait `Lock` are internal to
    /// CoreLib, the parking syscalls run on threads the guest does not hold, and the signal
    /// dispatcher's `Thread` is reachable only from a handler installed straight through
    /// `SystemNative_SetPosixSignalHandler`. Measured on real .NET 10 with a contended
    /// `LowLevelLock` and a contended `Lock(useTrivialWaits: true)` (both reached by
    /// reflection), with `flock(2)` blocked on a lock another descriptor holds, and with such a
    /// handler keeping `Thread.CurrentThread` (which then reads `Background` while idle): none
    /// is an alertable wait, so none reports `WaitSleepJoin`.
    [<Test>]
    let ``Non-alertable parks read as running`` () : unit =
        for status in
            [
                ThreadStatus.BlockedOnMonitorAcquire (LowLevelMonitorId 0)
                ThreadStatus.BlockedOnMonitorWait (LowLevelMonitorId 0, None)
                ThreadStatus.BlockedInSyscall
                ThreadStatus.BlockedOnWaitHandle (WaitHandleId 0, None, WaitAlertability.NonAlertable)
                ThreadStatus.Parked
            ] do
            for runtime in EmulatedRuntime.supported do
                ThreadStatus.managedThreadState runtime false false status
                |> shouldEqual System.Threading.ThreadState.Running

                ThreadStatus.managedThreadState runtime true false status
                |> shouldEqual System.Threading.ThreadState.Background
