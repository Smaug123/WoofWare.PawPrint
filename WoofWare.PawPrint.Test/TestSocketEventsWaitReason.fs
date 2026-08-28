namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ThreadStatus.BlockedOnSocketEvents` is the representation of a thread parked inside
/// `SystemNative_WaitForSocketEvents`. These tests construct it directly, which is how the other
/// thread-status state machines are tested (`TestLowLevelMonitor`, `TestWaitHandle`,
/// `TestSyncBlockMonitor`); `TestSocketEventsWait` is the other half, driving a real guest into
/// the status through the handler.
///
/// What they pin is the set of answers the status is *obliged* to give: two exhaustive
/// classifiers in `ThreadStatus`, the scheduler's treatment of an unrecognised blocked state, and
/// the diagnostic rendering. The status carries no payload, so *which* port a waiter waits on is
/// not observable from here at all — that lives in the kernel's `ParkedSocketWait`, and
/// `TestSocketEventsWait` is where the derivation from it is pinned. The absence of a deadline
/// field remains a design property no test can observe, and is enforced by review.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventsWaitReason =

    let private blocked : ThreadStatus = ThreadStatus.BlockedOnSocketEvents

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseState () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()
        IlMachineState.initial loggerFactory ImmutableArray.Empty corelib

    /// Frame-less stub: these tests only read `Status`, so a sentinel `ActiveMethodState` that is
    /// absent from the empty `MethodStates` map is right — anything reaching for a frame crashes
    /// loudly rather than reading a plausible-looking lie.
    let private stubThreadState (osThreadId : uint32) (status : ThreadStatus) : ThreadState =
        {
            MethodStates = Map.empty
            YieldDebt = Set.empty
            NextFrameId = 0
            ActiveMethodState = FrameId -1
            Status = status
            IsBackground = false
            IsRaisingForeignException = false
            Name = None
        }

    let private withThreads (threads : (ThreadId * ThreadStatus) list) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                threads
                |> List.mapi (fun i (tid, status) -> tid, stubThreadState (uint32 i + 1u) status)
                |> Map.ofList
        }

    /// A thread parked in a blocking `epoll_wait` has a live frame — under re-entrant parking the
    /// dispatcher leaves the native `WaitForSocketEvents` frame on the stack — so every caller
    /// that guards frame reads on this classifier must go on to read the frame.
    ///
    /// A `true` answer here would make `GuestLocation.positionOfThread` report `NoFrame` for a
    /// thread that has one, hiding the engine thread's position in exactly the deadlock report
    /// that needs it.
    [<Test>]
    let ``a thread blocked on socket events has a live frame`` () : unit =
        ThreadStatus.hasNoActiveFrame blocked |> shouldEqual false

    /// Pins the re-entrant parking choice: the handler leaves its native frame in place and does
    /// *not* advance the caller's program counter, so the reported offset is the raw PC.
    ///
    /// This has to be asserted as a constant rather than through behaviour. The active frame is a
    /// native frame, `MethodInfo.tryIlBody` returns `None` for one, so
    /// `GuestLocation.precedingCallOffset` finds no candidate and `reportableOffset` falls back to
    /// `IlOpIndex` — identically under either answer. There is therefore no state the interpreter
    /// can reach in which the two answers differ, and the pin exists so that switching the future
    /// handler to resume-style parking (advance the PC, pop the frame, park) has to come here and
    /// change this deliberately.
    [<Test>]
    let ``a thread blocked on socket events parked without advancing its program counter`` () : unit =
        ThreadStatus.parksPastTheBlockingCall blocked |> shouldEqual false

    /// Regression tripwire, not coverage of this change: `Scheduler.runnableThreads` and
    /// `Scheduler.hasAnyRunnable` match `Runnable` against a wildcard, so no way of writing the
    /// change that introduced this status could have made it schedulable. The mutant this exists
    /// for is a later edit adding `| ThreadStatus.BlockedOnSocketEvents _ -> tid :: acc` to
    /// `runnableThreads`' fold, which would resume a thread whose `epoll_wait` has not returned.
    ///
    /// The Runnable thread makes the assertion non-vacuous: `chooseNext` returning `None` because
    /// it never picks anything would pass a test that only checked the blocked thread was not
    /// chosen.
    [<Test>]
    let ``a thread blocked on socket events is never scheduled`` () : unit =
        let blockedThread = ThreadId 0
        let runnableThread = ThreadId 1

        let state =
            baseState ()
            |> withThreads [ blockedThread, blocked ; runnableThread, ThreadStatus.Runnable ]

        Scheduler.hasAnyRunnable state |> shouldEqual true

        // From either starting point, the only candidate is the Runnable thread.
        for lastRan in [ blockedThread ; runnableThread ] do
            let _, chosen = Scheduler.chooseNext lastRan state
            chosen |> shouldEqual (Some runnableThread)

    /// With nothing else to run, a thread waiting on a socket event port is a deadlock: no clock
    /// deadline can wake it (`WaitForSocketEvents` takes no timeout) and no other thread exists to
    /// make its port ready.
    [<Test>]
    let ``a thread blocked on socket events alone leaves nothing runnable`` () : unit =
        let blockedThread = ThreadId 0
        let state = baseState () |> withThreads [ blockedThread, blocked ]

        Scheduler.hasAnyRunnable state |> shouldEqual false
        Scheduler.chooseNext blockedThread state |> snd |> shouldEqual None

    /// The stuck-thread description has to name the port, or a deadlocked `SocketAsyncEngine`
    /// thread is indistinguishable from any other blocked thread in a process with several
    /// engines. What is pinned here is that the renderer *places* the port — a `WaitingFor`
    /// dropped on the floor would leave the line saying only that the thread is parked. Where
    /// the string comes from is `GuestLocation.ofState`, and `TestSocketEventsWait` drives a
    /// real guest through it.
    ///
    /// The position is the shape a real engine thread produces: the innermost frame is the
    /// framework's interop stub, which ships without a PDB, so the report walks out to the caller.
    [<Test>]
    let ``the stuck-thread description names the port`` () : unit =
        let rendered =
            GuestLocation.renderThread
                {
                    Thread = ThreadId 4
                    Status = blocked
                    WaitingFor = Some "on open file description 7"
                    Position =
                        GuestThreadPosition.CalledFrom (
                            {
                                Method = "Interop.Sys.WaitForSocketEvents"
                                IlOffset = 0
                            },
                            2,
                            {
                                Method = "SocketAsyncEngine.EventLoop"
                                IlOffset = 31
                            },
                            {
                                DocumentPath = "/build/SocketAsyncEngine.Unix.cs"
                                StartLine = 168
                                StartColumn = 17
                                EndLine = 168
                                EndColumn = 60
                            }
                        )
                }

        // Asserted whole rather than by substring: a `shouldContainText "7"` would also pass on a
        // renderer that dropped the port and happened to emit a 7 from an IL offset or line number.
        rendered
        |> shouldEqual (
            "thread 4 (BlockedOnSocketEvents on open file description 7) "
            + "in Interop.Sys.WaitForSocketEvents at IL offset 0, "
            + "called 2 frames out from SocketAsyncEngine.EventLoop at IL offset 31 "
            + "(/build/SocketAsyncEngine.Unix.cs:168)"
        )
