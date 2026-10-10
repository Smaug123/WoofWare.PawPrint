namespace WoofWare.PawPrint

/// The `SocketShutdown` numbering `SystemNative_Shutdown` takes its `how` in
/// (`pal_networking_common.h`, the values of `System.Net.SocketShutdown`), and
/// the shim's conversion of it to `shutdown(2)`'s (`Common_Shutdown`).
///
/// The shim converts the three values and answers `Error_EINVAL` for any other
/// before it looks at the descriptor or calls the kernel. So this is
/// PawPrint's half of the boundary, and the library gets `shutdown(2)`'s raw
/// `how`, which it screens again as the kernel does.
[<RequireQualifiedAccess>]
module SocketShutdownPal =

    /// `shutdown(2)`'s `how` for the PAL value `palHow`, which is `SHUT_RD` (0),
    /// `SHUT_WR` (1) or `SHUT_RDWR` (2) on every flavour; or `None` where the
    /// shim answers `Error_EINVAL` without calling the kernel.
    let toPlatform (palHow : int) : int option =
        match palHow with
        // SocketShutdown_SHUT_READ -> SHUT_RD
        | 0 -> Some 0
        // SocketShutdown_SHUT_WRITE -> SHUT_WR
        | 1 -> Some 1
        // SocketShutdown_SHUT_BOTH -> SHUT_RDWR
        | 2 -> Some 2
        | _ -> None
