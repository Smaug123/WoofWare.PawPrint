namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint

/// What PawPrint shows of a thread blocked in `Thread.Sleep` or `Thread.Join` on the runtime
/// under test. .NET 10 sleeps and joins in native waits, each with its own `ThreadStatus`; on
/// Unix, .NET 11 does both through its managed `WaitSubsystem`, which blocks on a
/// `LowLevelMonitor`, so the two look alike there.
[<RequireQualifiedAccess>]
module SleepAndJoinStatus =

    /// The name of the `ThreadStatus` case a sleeping thread is in, as a deadlock report spells it.
    let sleeping () : string =
        match FrameworkUnderTest.runtime () with
        | EmulatedRuntime.Net10 -> "BlockedOnSleep"
        | EmulatedRuntime.Net11 -> "BlockedOnMonitorWait"

    /// The name of the `ThreadStatus` case a joining thread is in, as a deadlock report spells it.
    let joining () : string =
        match FrameworkUnderTest.runtime () with
        | EmulatedRuntime.Net10 -> "BlockedOnJoin"
        | EmulatedRuntime.Net11 -> "BlockedOnMonitorWait"

    /// Whether `status` is that of a thread inside a finite `Thread.Sleep`.
    let isInFiniteSleep (status : ThreadStatus) : bool =
        match FrameworkUnderTest.runtime (), status with
        | EmulatedRuntime.Net10, ThreadStatus.BlockedOnSleep (Some _) -> true
        | EmulatedRuntime.Net11, ThreadStatus.BlockedOnMonitorWait (_, Some _) -> true
        | _, _ -> false
