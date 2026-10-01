namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// A task inside a signal handler, for a test that needs a task to block
/// signals: a task's mask is its innermost handler frame's, and nothing else
/// sets one.
[<RequireQualifiedAccess>]
module HandlerFrames =

    /// The signal these helpers deliver to put a task in a handler: SIGPROF,
    /// 27 under both numberings. A test that uses them does not use it
    /// otherwise.
    let carrier : Signal = Signal.Other 27

    /// `state` with `task` inside a handler for `carrier`, whose `sa_mask` is
    /// `mask` and which has `SA_NODEFER`: `task`'s mask becomes its old mask
    /// with `mask` added, and `carrier` is in it only if `mask` names it. The
    /// carrier's disposition is put back as it was. `None` if the return to
    /// user mode that delivers the carrier would deliver anything else too.
    ///
    /// `leader` and `tasks` are as `SignalState.onReturnToUser` takes them.
    let tryEnterVia<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (carrier : Signal)
        (handler : 'Handler)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (mask : Set<Signal>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler> option
        =
        let before = SignalState.disposition carrier state

        let action =
            { SignalCatch.ofHandler handler with
                Mask = mask
                NoDefer = true
            }

        let sent =
            state
            |> SignalState.setDisposition carrier (SignalDisposition.Catch action)
            |> SignalState.enqueue
                {
                    Signal = carrier
                    Target = ValueSome task
                }

        match SignalState.onReturnToUser CoreDumps.Suppressed leader tasks task sent with
        | Ok (Some (SignalDelivery.RunHandlers [ frame ]), state) when frame.Entry.Signal = carrier ->
            Some (SignalState.setDisposition carrier before state)
        | _ -> None

    /// `tryEnterVia` with the usual `carrier`.
    let tryEnter<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (handler : 'Handler)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (mask : Set<Signal>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler> option
        =
        tryEnterVia carrier handler leader tasks task mask state

    /// `tryEnterVia`, failing the test if anything but the carrier would be
    /// delivered: set up the frames before the signals the test is about are
    /// generated.
    let enterVia<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (carrier : Signal)
        (handler : 'Handler)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (mask : Set<Signal>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        match tryEnterVia carrier handler leader tasks task mask state with
        | Some state -> state
        | None -> failwith $"expected the carrier %O{carrier} alone to be delivered to %O{task}"

    /// `enterVia` with the usual `carrier`.
    let enter<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (handler : 'Handler)
        (leader : 'Task)
        (tasks : Set<'Task>)
        (task : 'Task)
        (mask : Set<Signal>)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        enterVia carrier handler leader tasks task mask state

    /// `task` returns from its innermost handler.
    let leave<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (task : 'Task)
        (state : SignalState<'Task, 'Handler>)
        : SignalState<'Task, 'Handler>
        =
        match SignalState.framesOf task state with
        | innermost :: _ -> SignalState.sigreturn task innermost.Id state
        | [] -> failwith $"%O{task} is in no handler"

    /// `enter` for a task of `system`, whose leader and tasks it takes.
    let enterIn<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (handler : 'Handler)
        (task : 'Task)
        (mask : Set<Signal>)
        (system : UnixSystem<'Task, 'Handler>)
        : UnixSystem<'Task, 'Handler>
        =
        let tasks = system.Tasks |> Map.keys |> Set.ofSeq

        { system with
            Process =
                { system.Process with
                    Signals = enter handler system.Leader tasks task mask system.Process.Signals
                }
        }
