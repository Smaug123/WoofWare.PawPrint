namespace WoofWare.PosixKernel

/// This library's model does not cover what was asked: nobody has measured
/// what a real kernel does there, or the model cannot hold what the answer
/// needs. It is a feature request for this library, not a condition to handle
/// in normal operation.
///
/// Any call may throw it, and a minor version may add or remove the places
/// that do. The message is for a human, and has no stable structure.
///
/// A throw leaves nothing behind: every value this library takes is
/// immutable, so a client that catches this may carry on from the state it
/// passed in.
[<Sealed>]
type UnmodelledException internal (message : string) =
    inherit System.Exception (message)

/// Which exception a refusal is raised as at a public entry point. The
/// question is whether a client that follows the documentation could have
/// made the call: if it could, the model does not cover the call; if not, the
/// call is the client's mistake, and the BCL's own exception for that mistake
/// says so.
[<RequireQualifiedAccess>]
type internal RefusalKind =
    /// The model does not cover what was asked: `UnmodelledException`.
    | Unmodelled
    /// The value passed as `parameter` contradicts the rest of the call, or the
    /// machine: no machine could have it. `ArgumentException`.
    | Contradictory of parameter : string
    /// The value passed as `parameter` lies outside the range any machine
    /// could have. `ArgumentOutOfRangeException`.
    | OutOfRange of parameter : string
    /// The call is one the documentation rules out at this point.
    /// `InvalidOperationException`.
    | Misuse
    /// The flavour being simulated has no such call, which the client could
    /// have checked first. `PlatformNotSupportedException`.
    | NotOnPlatform

[<RequireQualifiedAccess>]
module internal Refusal =

    /// The exception a public entry point named `operation` throws for a
    /// refusal of `kind`, which `description` describes.
    let exceptionOf (operation : string) (kind : RefusalKind) (description : string) : exn =
        let message = $"%s{operation}: %s{description}"

        match kind with
        | RefusalKind.Unmodelled -> UnmodelledException message
        | RefusalKind.Contradictory parameter -> System.ArgumentException (message, parameter)
        | RefusalKind.OutOfRange parameter -> System.ArgumentOutOfRangeException (parameter, message)
        | RefusalKind.Misuse -> System.InvalidOperationException message
        | RefusalKind.NotOnPlatform -> System.PlatformNotSupportedException message

    /// The answer `result` holds, or, for a refusal, the exception
    /// `exceptionOf` gives for it: what a public entry point named `operation`
    /// does with what its internal function answered.
    let orRaise<'Answer, 'Refusal>
        (operation : string)
        (kind : 'Refusal -> RefusalKind)
        (describe : 'Refusal -> string)
        (result : Result<'Answer, 'Refusal>)
        : 'Answer
        =
        match result with
        | Ok answer -> answer
        | Error refusal -> raise (exceptionOf operation (kind refusal) (describe refusal))
