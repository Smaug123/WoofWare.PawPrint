namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The `OpenFlags` numbering `SystemNative_Open` takes its flags argument in
/// (`pal_io.h`), the screen the shim applies to it, and the flag word it hands
/// `open(2)` in its place.
///
/// The shim's `ConvertOpenFlags` (`pal_io.c`) answers EINVAL without reaching
/// the kernel for an access mode that is none of the three and for any bit it
/// does not know, and otherwise translates each PAL bit to the platform's own
/// `<fcntl.h>` bit. So this is PawPrint's half of the boundary, and the kernel
/// library gets the flavour's raw `open(2)` flags.
[<RequireQualifiedAccess>]
module OpenFlagsPal =

    // `pal_io.h`'s `OpenFlags`, which CoreLib's `Interop.Sys.OpenFlags`
    // restates.

    [<Literal>]
    let AccessModeMask = 0x000F

    [<Literal>]
    let ReadOnly = 0x0000

    [<Literal>]
    let WriteOnly = 0x0001

    [<Literal>]
    let ReadWrite = 0x0002

    [<Literal>]
    let CloseOnExec = 0x0010

    [<Literal>]
    let Create = 0x0020

    [<Literal>]
    let Exclusive = 0x0040

    [<Literal>]
    let Truncate = 0x0080

    [<Literal>]
    let Synchronous = 0x0100

    [<Literal>]
    let NoFollow = 0x0200

    /// The `<fcntl.h>` bits a flavour gives the flags the shim and `opendir(3)`
    /// pass, as measured by the kernel library's `open-flags.c` probe.
    type private Numbering =
        {
            CloseOnExec : int
            Create : int
            Exclusive : int
            Truncate : int
            Synchronous : int
            NoFollow : int
            Directory : int
        }

    let private numbering (platform : SimulatedUnixPlatform) : Numbering =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            // aarch64's <asm/fcntl.h> moves O_NOFOLLOW and O_DIRECTORY; the
            // rest is the generic numbering on both architectures. O_SYNC is
            // __O_SYNC|O_DSYNC.
            let noFollow, directory =
                match SimulatedUnixPlatform.architecture platform with
                | SimulatedUnixArchitecture.X64 -> 0x20000, 0x10000
                | SimulatedUnixArchitecture.Arm64 -> 0x8000, 0x4000

            {
                CloseOnExec = 0x80000
                Create = 0x40
                Exclusive = 0x80
                Truncate = 0x200
                Synchronous = 0x101000
                NoFollow = noFollow
                Directory = directory
            }
        | SimulatedUnixFlavour.Darwin ->
            {
                CloseOnExec = 0x1000000
                Create = 0x200
                Exclusive = 0x800
                Truncate = 0x400
                Synchronous = 0x80
                NoFollow = 0x100
                Directory = 0x100000
            }

    /// What the shim hands `open(2)` for this flags argument, in `platform`'s
    /// own `<fcntl.h>` numbering, or `None` where `ConvertOpenFlags` answers -1
    /// and the shim EINVAL without reaching the kernel.
    ///
    /// The access mode is checked against the C's own four-bit mask before any
    /// other bit, as it is there; both refusals are the same EINVAL.
    let decode (platform : SimulatedUnixPlatform) (flags : int) : int option =
        let numbering = numbering platform

        let access =
            match flags &&& AccessModeMask with
            | ReadOnly -> Some 0
            | WriteOnly -> Some 1
            | ReadWrite -> Some 2
            | _ -> None

        let known =
            AccessModeMask
            ||| CloseOnExec
            ||| Create
            ||| Exclusive
            ||| Truncate
            ||| Synchronous
            ||| NoFollow

        match access with
        | None -> None
        | Some _ when flags &&& ~~~known <> 0 -> None
        | Some access ->
            let bit (pal : int) (platformBit : int) : int =
                if flags &&& pal <> 0 then platformBit else 0

            access
            ||| bit CloseOnExec numbering.CloseOnExec
            ||| bit Create numbering.Create
            ||| bit Exclusive numbering.Exclusive
            ||| bit Truncate numbering.Truncate
            ||| bit Synchronous numbering.Synchronous
            ||| bit NoFollow numbering.NoFollow
            |> Some

    /// The flag word PawPrint's `opendir(3)` opens a directory with:
    /// `O_RDONLY|O_DIRECTORY|O_CLOEXEC` in `platform`'s numbering.
    ///
    /// glibc's and Darwin's `opendir` add `O_NONBLOCK`, which the kernel
    /// library does not model on `open(2)`. It changes nothing for a directory,
    /// which no read blocks on, so it is left out rather than refused.
    let directoryStream (platform : SimulatedUnixPlatform) : int =
        let numbering = numbering platform
        numbering.Directory ||| numbering.CloseOnExec

    /// The flag word minipal opens `/dev/urandom` with: `O_RDONLY|O_CLOEXEC` in
    /// `platform`'s numbering.
    let minipalUrandom (platform : SimulatedUnixPlatform) : int = (numbering platform).CloseOnExec
