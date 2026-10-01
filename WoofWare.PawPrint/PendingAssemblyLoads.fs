namespace WoofWare.PawPrint

/// The assemblies one step of a thread loaded, in load order, of which `AppDomain.AssemblyLoad` has
/// not yet been raised for `Remaining`.
type private AssemblyLoadBatch =
    {
        /// Never empty: a batch with nothing left is removed.
        Remaining : string list
        /// The frame announcing the assembly before `Remaining`'s head, if it may still be running.
        /// CoreCLR raises the event once per load, one after another, so nothing more of this batch
        /// is announced until that frame has returned.
        AwaitingReturnOf : FrameId option
    }

/// <summary>
/// The assemblies a thread has loaded that <c>AppDomain.AssemblyLoad</c> has not yet been raised
/// for, by assembly definition full name.
/// </summary>
/// <remarks>
/// CoreCLR raises the event on the loading thread once each load completes, after releasing the
/// load lock (<c>Assembly::DeliverAsyncEvents</c>). PawPrint records what a thread's step loaded and
/// announces it at the start of that thread's next step, which is an interleaving CoreCLR also
/// permits: the loading thread can be preempted between finishing a load and raising its event.
///
/// A load made while an announcement is running belongs to a step executing inside it, so it is
/// announced inside it too, before the rest of the outer batch, as CoreCLR's nested delivery does.
/// That is why this is a stack of batches, innermost first.
/// </remarks>
type PendingAssemblyLoads =
    private
        {
            Batches : AssemblyLoadBatch list
        }

/// An assembly taken from the front of a thread's <c>PendingAssemblyLoads</c>, which the caller must
/// now either announce or skip; each says what is pending afterwards.
type TakenAssemblyLoad =
    private
        {
            Name : string
            Rest : string list
            Below : AssemblyLoadBatch list
        }

    /// The definition full name of the assembly to announce.
    member this.DefinitionFullName : string = this.Name

[<RequireQualifiedAccess>]
module PendingAssemblyLoads =
    let empty : PendingAssemblyLoads =
        {
            Batches = []
        }

    let isEmpty (pending : PendingAssemblyLoads) : bool = pending.Batches.IsEmpty

    /// Every assembly still to be announced, innermost batch first and each batch in load order.
    let toList (pending : PendingAssemblyLoads) : string list =
        pending.Batches |> List.collect (fun batch -> batch.Remaining)

    /// Record the assemblies one step of the thread loaded, in the order it loaded them.
    let record (loaded : string list) (pending : PendingAssemblyLoads) : PendingAssemblyLoads =
        match loaded with
        | [] -> pending
        | _ ->
            {
                Batches =
                    {
                        Remaining = loaded
                        AwaitingReturnOf = None
                    }
                    :: pending.Batches
            }

    /// <summary>
    /// The assembly to announce now, if one is due: none is while the frame announcing the
    /// previous assembly of the innermost batch is still live on the thread.
    /// </summary>
    /// <remarks>
    /// Only the innermost batch is ever consulted. Every outer batch is waiting on a frame below
    /// the one the innermost batch is (directly or through the step that recorded it) running
    /// inside, so if the innermost has nothing due, nor has any other.
    /// </remarks>
    let tryTake (isLive : FrameId -> bool) (pending : PendingAssemblyLoads) : TakenAssemblyLoad option =
        match pending.Batches with
        | [] -> None
        | batch :: below ->
            match batch.AwaitingReturnOf with
            | Some frame when isLive frame -> None
            | Some _
            | None ->
                match batch.Remaining with
                | [] ->
                    failwith
                        "logic error: an assembly-load batch with nothing remaining was left pending; it should have been removed when its last assembly was taken"
                | name :: rest ->
                    {
                        Name = name
                        Rest = rest
                        Below = below
                    }
                    |> Some

    /// The taken assembly is being announced by `frame`; the rest of its batch waits for `frame` to
    /// return.
    let announcedBy (frame : FrameId) (taken : TakenAssemblyLoad) : PendingAssemblyLoads =
        match taken.Rest with
        | [] ->
            {
                Batches = taken.Below
            }
        | rest ->
            {
                Batches =
                    {
                        Remaining = rest
                        AwaitingReturnOf = Some frame
                    }
                    :: taken.Below
            }

    /// The taken assembly is not being announced (nothing is subscribed, or it is corelib); the
    /// rest of its batch is due at once.
    let skipped (taken : TakenAssemblyLoad) : PendingAssemblyLoads =
        match taken.Rest with
        | [] ->
            {
                Batches = taken.Below
            }
        | rest ->
            {
                Batches =
                    {
                        Remaining = rest
                        AwaitingReturnOf = None
                    }
                    :: taken.Below
            }
