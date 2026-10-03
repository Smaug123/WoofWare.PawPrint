namespace WoofWare.PawPrint.Test

open WoofWare.PawPrint
open WoofWare.PosixKernel

/// Tasks for a test that builds threads by hand rather than through the
/// interpreter's thread-creation paths.
[<RequireQualifiedAccess>]
module KernelTasks =

    /// `kernel` with a task for `thread`: itself if `thread` already has one (the
    /// leader always does), and otherwise with one spawned by the leader on
    /// processor 0.
    let ensure (thread : ThreadId) (kernel : EmulatedKernel) : EmulatedKernel =
        if Map.containsKey thread kernel.Tasks then
            kernel
        else
            match UnixTaskLifecycle.spawn kernel.Leader thread (CpuId 0) kernel.System with
            | Ok (_, system) -> EmulatedKernel.withUnix system kernel
            | Error error -> failwith $"spawning %O{thread} failed with %O{error}"
