namespace WoofWare.PosixKernel

/// A process ID, as `getpid(2)` reports it: a positive `pid_t`.
///
/// Zero and the negative numbers are not process IDs. `kill(2)` reads them as
/// "my process group", "every process I may signal" and "that process group",
/// so a value of this type can never be mistaken for one of those.
[<Struct>]
type ProcessId =
    private
    | ProcessId of pid : int32

    override this.ToString () : string =
        match this with
        | ProcessId pid -> string<int32> pid

[<RequireQualifiedAccess>]
module ProcessId =
    /// The `pid_t` a kernel would report for this process.
    let toInt32 (pid : ProcessId) : int32 =
        match pid with
        | ProcessId pid -> pid

    /// The process ID `candidate` names, or `None` if it is not positive.
    ///
    /// Only the lower bound is checked. Each kernel also has a largest process
    /// ID it will hand out (Linux's `pid_max` is a sysctl, Darwin's `PID_MAX` is
    /// fixed at build time), and this does not enforce either.
    let parse (candidate : int32) : ProcessId option =
        if candidate <= 0 then None else Some (ProcessId candidate)

    /// As `parse`, but failing with `context` in the message if `candidate` is
    /// not positive.
    let parseOrFail (context : string) (candidate : int32) : ProcessId =
        match parse candidate with
        | Some pid -> pid
        | None ->
            failwith
                $"%s{context}: %d{candidate} is not a process ID; a process ID is positive (0 and the negative numbers name process groups, or every process, to kill(2))."

    /// `pid` itself, failing with `context` in the message if it is the one
    /// value of this type that `parse` could not have produced:
    /// `Unchecked.defaultof<ProcessId>`, whose ID is 0.
    let assertValid (context : string) (pid : ProcessId) : ProcessId = parseOrFail context (toInt32 pid)
