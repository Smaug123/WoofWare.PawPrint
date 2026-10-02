namespace WoofWare.PosixKernel.Test

open WoofWare.PosixKernel

/// What one bit of `open(2)`'s flag word means on one platform, by this test
/// project's own reading of the probe (`open-flags.c`), independently of the
/// library's decoder.
[<RequireQualifiedAccess>]
type OpenFlagBit =
    | Create
    | Exclusive
    | Truncate
    | NoFollow
    | Directory
    | CloseOnExec
    | Synchronous
    /// `O_DSYNC`: part of `O_SYNC` when `Synchronous` is beside it, and
    /// otherwise a flag the library does not model.
    | DataSynchronous
    | Unmodelled of UnmodelledOpenFlag

/// Each platform's `open(2)` numbering, transcribed from the probe's output
/// (`docs/plans/2026-08-23-posix-kernel-extraction/open-flags.*.txt`), and the
/// flag words a parsed request stands for.
[<RequireQualifiedAccess>]
module internal OpenFlagWords =

    /// Every bit above the access mode that `platform` defines, with its name
    /// in the platform's `<fcntl.h>`. A bit not listed is one neither kernel
    /// defines.
    let table (platform : SimulatedUnixPlatform) : (int * string * OpenFlagBit) list =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux ->
            // The four bits aarch64's <asm/fcntl.h> moves, measured on each
            // architecture's kernel.
            let directory, noFollow, direct, largeFile =
                match SimulatedUnixPlatform.architecture platform with
                | SimulatedUnixArchitecture.X64 -> 0x10000, 0x20000, 0x4000, 0x8000
                | SimulatedUnixArchitecture.Arm64 -> 0x4000, 0x8000, 0x10000, 0x20000

            [
                0x40, "O_CREAT", OpenFlagBit.Create
                0x80, "O_EXCL", OpenFlagBit.Exclusive
                0x100, "O_NOCTTY", OpenFlagBit.Unmodelled UnmodelledOpenFlag.NoControllingTerminal
                0x200, "O_TRUNC", OpenFlagBit.Truncate
                0x400, "O_APPEND", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Append
                0x800, "O_NONBLOCK", OpenFlagBit.Unmodelled UnmodelledOpenFlag.NonBlocking
                0x1000, "O_DSYNC", OpenFlagBit.DataSynchronous
                0x2000, "O_ASYNC", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Asynchronous
                direct, "O_DIRECT", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Direct
                largeFile, "O_LARGEFILE", OpenFlagBit.Unmodelled UnmodelledOpenFlag.LargeFile
                directory, "O_DIRECTORY", OpenFlagBit.Directory
                noFollow, "O_NOFOLLOW", OpenFlagBit.NoFollow
                0x40000, "O_NOATIME", OpenFlagBit.Unmodelled UnmodelledOpenFlag.NoAccessTime
                0x80000, "O_CLOEXEC", OpenFlagBit.CloseOnExec
                0x100000, "__O_SYNC", OpenFlagBit.Synchronous
                0x200000, "O_PATH", OpenFlagBit.Unmodelled UnmodelledOpenFlag.PathOnly
                0x400000, "__O_TMPFILE", OpenFlagBit.Unmodelled UnmodelledOpenFlag.TemporaryFile
            ]
            |> List.sortBy (fun (bit, _, _) -> bit)
        | SimulatedUnixFlavour.Darwin ->
            [
                0x4, "O_NONBLOCK", OpenFlagBit.Unmodelled UnmodelledOpenFlag.NonBlocking
                0x8, "O_APPEND", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Append
                0x10, "O_SHLOCK", OpenFlagBit.Unmodelled UnmodelledOpenFlag.SharedLock
                0x20, "O_EXLOCK", OpenFlagBit.Unmodelled UnmodelledOpenFlag.ExclusiveLock
                0x40, "O_ASYNC", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Asynchronous
                0x80, "O_SYNC", OpenFlagBit.Synchronous
                0x100, "O_NOFOLLOW", OpenFlagBit.NoFollow
                0x200, "O_CREAT", OpenFlagBit.Create
                0x400, "O_TRUNC", OpenFlagBit.Truncate
                0x800, "O_EXCL", OpenFlagBit.Exclusive
                0x1000, "O_RESOLVE_BENEATH", OpenFlagBit.Unmodelled UnmodelledOpenFlag.ResolveBeneath
                0x2000, "O_UNIQUE", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Unique
                0x8000, "O_EVTONLY", OpenFlagBit.Unmodelled UnmodelledOpenFlag.EventOnly
                0x20000, "O_NOCTTY", OpenFlagBit.Unmodelled UnmodelledOpenFlag.NoControllingTerminal
                0x100000, "O_DIRECTORY", OpenFlagBit.Directory
                0x200000, "O_SYMLINK", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Symlink
                0x400000, "O_DSYNC", OpenFlagBit.DataSynchronous
                0x1000000, "O_CLOEXEC", OpenFlagBit.CloseOnExec
                0x8000000, "O_CLOFORK", OpenFlagBit.Unmodelled UnmodelledOpenFlag.CloseOnFork
                0x20000000, "O_NOFOLLOW_ANY", OpenFlagBit.Unmodelled UnmodelledOpenFlag.NoFollowAny
                0x40000000, "O_EXEC", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Execute
                0x80000000, "O_POPUP", OpenFlagBit.Unmodelled UnmodelledOpenFlag.Popup
            ]

    /// The one bit `platform` numbers `meaning` as.
    let bitOf (platform : SimulatedUnixPlatform) (meaning : OpenFlagBit) : int =
        match table platform |> List.filter (fun (_, _, m) -> m = meaning) with
        | [ bit, _, _ ] -> bit
        | other -> failwith $"OpenFlagWords.bitOf: %A{meaning} is numbered %d{other.Length} times on %O{platform}"

    /// The flag word `platform`'s C library would pass for `flags`: its access
    /// mode, and the bit of each flag that is set (both of Linux's `O_SYNC`
    /// bits, as `<fcntl.h>` defines it).
    let encode (platform : SimulatedUnixPlatform) (flags : OpenFlags) : int =
        let access =
            match flags.Access with
            | FileAccessMode.ReadOnly -> 0
            | FileAccessMode.WriteOnly -> 1
            | FileAccessMode.ReadWrite -> 2

        let bit (set : bool) (meaning : OpenFlagBit) : int =
            if set then bitOf platform meaning else 0

        let synchronous =
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Linux ->
                bit flags.Synchronous OpenFlagBit.Synchronous
                ||| bit flags.Synchronous OpenFlagBit.DataSynchronous
            | SimulatedUnixFlavour.Darwin -> bit flags.Synchronous OpenFlagBit.Synchronous

        access
        ||| bit flags.Create OpenFlagBit.Create
        ||| bit flags.Exclusive OpenFlagBit.Exclusive
        ||| bit flags.Truncate OpenFlagBit.Truncate
        ||| bit flags.NoFollow OpenFlagBit.NoFollow
        ||| bit flags.CloseOnExec OpenFlagBit.CloseOnExec
        ||| bit flags.Directory OpenFlagBit.Directory
        ||| synchronous

    /// `UnixNamespace.openPath`, given the flag word that stands for `flags`
    /// on the system's own platform.
    let openPath<'Task, 'Handler when 'Task : comparison and 'Handler : equality>
        (flags : OpenFlags)
        (path : PathArgumentBytes)
        (mode : int)
        (system : UnixSystem<'Task, 'Handler>)
        : Result<SyscallAnswer * UnixSystem<'Task, 'Handler>, OpenRefusal>
        =
        UnixNamespace.openPath (encode system.Machine.UnixPlatform flags) path mode system
