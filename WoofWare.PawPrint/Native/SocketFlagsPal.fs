namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The `SocketFlags` numbering `SystemNative_Receive` and `SystemNative_Send`
/// take their flags argument in (`pal_networking.h`), and the shim's
/// conversion of it to the flavour's own flag word
/// (`ConvertSocketFlagsPalToPlatform`, `pal_networking.c`).
///
/// The shim admits seven flags, each under an `#ifdef` of the platform's
/// `MSG_*` name, and answers `Error_ENOTSUP` for a word holding any other bit
/// before it calls the kernel. So this is PawPrint's half of the boundary, and
/// the library gets the flavour's raw `recv(2)` and `send(2)` word, which it
/// screens for the flags it models.
[<RequireQualifiedAccess>]
module SocketFlagsPal =

    /// Each `SocketFlags_MSG_*` value of `pal_networking.h`, and the flag the
    /// shim converts it to.
    let table : (int * MessageFlag) list =
        [
            0x0001, MessageFlag.OutOfBand
            0x0002, MessageFlag.Peek
            0x0004, MessageFlag.DontRoute
            0x0100, MessageFlag.Truncate
            0x0200, MessageFlag.ControlTruncate
            0x1000, MessageFlag.DontWait
            0x2000, MessageFlag.ErrorQueue
        ]

    /// The flag word the shim passes to `recv(2)` or `send(2)` for the PAL word
    /// `palFlags`, in `flavour`'s numbering; or `None` where the shim answers
    /// `Error_ENOTSUP` without calling the kernel: a bit outside the table, or
    /// one whose flag `flavour`'s header does not define (Darwin has no
    /// `MSG_ERRQUEUE`, so the shim's `#ifdef` leaves it out of the mask).
    let toPlatform (flavour : SimulatedUnixFlavour) (palFlags : int) : int option =
        let supported =
            table
            |> List.choose (fun (pal, flag) ->
                MessageFlag.number flavour flag |> Option.map (fun number -> pal, number)
            )

        let mask = supported |> List.fold (fun mask (pal, _) -> mask ||| pal) 0

        if palFlags &&& ~~~mask <> 0 then
            None
        else
            supported
            |> List.fold (fun word (pal, number) -> if palFlags &&& pal <> 0 then word ||| number else word) 0
            |> Some
