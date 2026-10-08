namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open WoofWare.PosixKernel

/// A socket option's value as the bytes a caller's buffer holds: the presets
/// are little-endian machines, so a C `int` is `BitConverter`'s layout.
[<RequireQualifiedAccess>]
module OptionValue =

    /// The four bytes of a C `int`.
    let ofInt (value : int) : ImmutableArray<byte> =
        ImmutableArray.Create<byte> (BitConverter.GetBytes value)

    /// The eight bytes of a `struct linger`.
    let ofLinger (onOff : int) (linger : int) : ImmutableArray<byte> =
        ImmutableArray.CreateRange (Array.append (BitConverter.GetBytes onOff) (BitConverter.GetBytes linger))

    /// A whole C `int` copied out, or `None` for any other number of bytes.
    let (|Int|_|) (copied : ImmutableArray<byte>) : int option =
        if copied.Length = 4 then
            Some (BitConverter.ToInt32 (copied.AsSpan ()))
        else
            None

    /// The `int` a `getsockopt` reported whole, failing the test otherwise.
    let readInt (answer : GetSockOptAnswer) : int =
        match answer with
        | GetSockOptAnswer.Reported (Int value) -> value
        | other -> failwith $"expected a whole int, got %A{other}"

    /// What a `getsockopt` reports for an option whose value is `value`,
    /// through a buffer that took `length` bytes of it.
    let reported (value : int) (length : uint32) : GetSockOptAnswer =
        GetSockOptAnswer.Reported (ImmutableArray.Create (ofInt value, 0, int length))
