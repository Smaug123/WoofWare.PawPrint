namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Runtime.InteropServices
open WoofWare.PosixKernel

/// This host's own `fstatfs(2)`, read into the library's vocabulary: the oracle
/// for `UnixPathResolution.fstatfs`. The host knows every field, so every group
/// of the answer is `Ok`.
[<RequireQualifiedAccess>]
module HostFileSystemType =

    /// `struct statfs` is read as raw bytes at offsets measured 2026-09-26 with
    /// `offsetof`. On Linux (glibc, aarch64; x86-64 lays it out the same) it is
    /// 120 bytes: `f_type`, `f_bsize`, `f_blocks`, `f_bfree`, `f_bavail`,
    /// `f_files`, `f_ffree` at 0, 8, ... 48, `f_fsid` (two `int`s) at 56,
    /// `f_namelen` at 64, `f_frsize` at 72, `f_flags` at 80. On Darwin (arm64)
    /// it is 2168: `f_bsize` (`uint32_t`) at 0, `f_iosize` (`int32_t`) at 4, the
    /// five `uint64_t` counts at 8 to 40, `f_fsid` at 48, `f_owner` at 56,
    /// `f_type` at 60, `f_flags` at 64, `f_fssubtype` at 68, `f_fstypename`
    /// (16 bytes) at 72, `f_mntonname` and `f_mntfromname` (1024 each) at 88
    /// and 1112, `f_flags_ext` at 2136. The buffer is oversized for both.
    [<DllImport("libc", EntryPoint = "fstatfs", SetLastError = true)>]
    extern int private linuxFstatfs(int fd, byte[] buffer)

    /// Darwin's 64-bit-inode layout under every architecture. The unsuffixed
    /// `fstatfs` is that layout on arm64 only: on x86-64 (Rosetta included) it
    /// is the legacy struct, which measured 2026-09-24 puts "apfs" at 104
    /// rather than 72, while `fstatfs64` puts it at 72 on both.
    [<DllImport("libc", EntryPoint = "fstatfs64", SetLastError = true)>]
    extern int private darwinFstatfs(int fd, byte[] buffer)

    let private bufferSize : int = 4096

    let private cString (fd : int) (field : string) (buffer : byte[]) (offset : int) (length : int) : UnixByteString =
        let span = ReadOnlySpan (buffer, offset, length)
        let terminator = span.IndexOf 0uy

        if terminator < 0 then
            failwith $"host fstatfs(%d{fd}) reported a %s{field} with no NUL in its %d{length} bytes"

        match UnixByteString.ofBytes (ImmutableArray.Create<byte> (span.Slice(0, terminator).ToArray ())) with
        | Ok text -> text
        | Error defect -> failwith $"host fstatfs(%d{fd}) reported a %s{field} that is not a Unix string: %O{defect}"

    /// `fstatfs(fd)` on this host, as the library would state its answer, for
    /// a host of `flavour`.
    let statisticsFor (flavour : SimulatedUnixFlavour) (fd : int) : FileSystemStatisticsAnswer =
        if not BitConverter.IsLittleEndian then
            failwith "HostFileSystemType reads struct statfs as little-endian, and this host is not"

        let buffer = Array.zeroCreate<byte> bufferSize

        let result =
            match flavour with
            | SimulatedUnixFlavour.Linux -> linuxFstatfs (fd, buffer)
            | SimulatedUnixFlavour.Darwin -> darwinFstatfs (fd, buffer)

        if result <> 0 then
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrno errno with
            | Some error -> FileSystemStatisticsAnswer.Failed error
            | None -> failwith $"host fstatfs(%d{fd}) failed with errno %d{errno}, which has no portable name"
        else

        let int64At (offset : int) =
            BinaryPrimitives.ReadInt64LittleEndian (ReadOnlySpan (buffer, offset, 8))

        let uint64At (offset : int) =
            BinaryPrimitives.ReadUInt64LittleEndian (ReadOnlySpan (buffer, offset, 8))

        let int32At (offset : int) =
            BinaryPrimitives.ReadInt32LittleEndian (ReadOnlySpan (buffer, offset, 4))

        let uint32At (offset : int) =
            BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan (buffer, offset, 4))

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            FileSystemStatistics.Linux
                {
                    Type = int64At 0
                    Geometry =
                        Ok
                            {
                                BlockSize = int64At 8
                                FragmentSize = int64At 72
                                NameLengthLimit = int64At 64
                            }
                    Capacity =
                        Ok
                            {
                                Blocks = uint64At 16
                                FreeBlocks = uint64At 24
                                AvailableBlocks = uint64At 32
                                Files = uint64At 40
                                FreeFiles = uint64At 48
                            }
                    FileSystemId =
                        Ok
                            {
                                First = int32At 56
                                Second = int32At 60
                            }
                    Flags = Ok (int64At 80)
                }
            |> FileSystemStatisticsAnswer.Reported
        | SimulatedUnixFlavour.Darwin ->
            FileSystemStatistics.Darwin
                {
                    Type = uint32At 60
                    TypeName = cString fd "f_fstypename" buffer 72 16
                    Geometry =
                        Ok
                            {
                                BlockSize = uint32At 0
                                IoSize = int32At 4
                            }
                    Capacity =
                        Ok
                            {
                                Blocks = uint64At 8
                                FreeBlocks = uint64At 16
                                AvailableBlocks = uint64At 24
                                Files = uint64At 32
                                FreeFiles = uint64At 40
                            }
                    FileSystemId =
                        Ok
                            {
                                First = int32At 48
                                Second = int32At 52
                            }
                    Mount =
                        Ok
                            {
                                Owner = uint32At 56
                                Flags = uint32At 64
                                ExtendedFlags = uint32At 2136
                                SubType = uint32At 68
                                MountedOn = cString fd "f_mntonname" buffer 88 1024
                                MountedFrom = cString fd "f_mntfromname" buffer 1112 1024
                            }
                }
            |> FileSystemStatisticsAnswer.Reported

    /// The fields naming the filesystem `fd` is on, or the errno `fstatfs`
    /// failed with.
    let typeFieldsFor (flavour : SimulatedUnixFlavour) (fd : int) : Result<FileSystemTypeFields, UnixError> =
        match statisticsFor flavour fd with
        | FileSystemStatisticsAnswer.Reported statistics -> Ok (FileSystemStatistics.typeFields statistics)
        | FileSystemStatisticsAnswer.Failed error -> Error error
