namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.Runtime.InteropServices
open WoofWare.PosixKernel

/// This host's own `fstatfs(2)`, read into the library's vocabulary: the oracle
/// for `EmulatedFileSystemType.reportedFor`.
[<RequireQualifiedAccess>]
module HostFileSystemType =

    /// `struct statfs` is read as raw bytes at offsets measured 2026-09-23 with
    /// `offsetof`: on Linux (glibc, aarch64; x86-64 lays it out the same)
    /// `f_type` is the `long` at 0 of a 120-byte struct; on Darwin (arm64)
    /// `f_type` is the `uint32_t` at 60 and `f_fstypename` the 16-byte
    /// `char[]` at 72, of 2168. The buffer is oversized for both.
    [<DllImport("libc", EntryPoint = "fstatfs", SetLastError = true)>]
    extern int private linuxFstatfs(int fd, byte[] buffer)

    /// Darwin's 64-bit-inode layout under every architecture. The unsuffixed
    /// `fstatfs` is that layout on arm64 only: on x86-64 (Rosetta included) it
    /// is the legacy struct, which measured 2026-09-24 puts "apfs" at 104
    /// rather than 72, while `fstatfs64` puts it at 72 on both.
    [<DllImport("libc", EntryPoint = "fstatfs64", SetLastError = true)>]
    extern int private darwinFstatfs(int fd, byte[] buffer)

    let private bufferSize : int = 4096

    /// `fstatfs(fd)` on this host, as the library would state its answer, for
    /// a host of `flavour`.
    let answerFor (flavour : SimulatedUnixFlavour) (fd : int) : FileSystemTypeAnswer =
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
            | Some error -> FileSystemTypeAnswer.Failed error
            | None -> failwith $"host fstatfs(%d{fd}) failed with errno %d{errno}, which has no portable name"
        else

        match flavour with
        | SimulatedUnixFlavour.Linux ->
            FileSystemTypeFields.Linux (BinaryPrimitives.ReadInt64LittleEndian (ReadOnlySpan (buffer, 0, 8)))
            |> FileSystemTypeAnswer.Reported
        | SimulatedUnixFlavour.Darwin ->
            let fType = BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan (buffer, 60, 4))
            let nameField = ReadOnlySpan (buffer, 72, 16)
            let terminator = nameField.IndexOf 0uy

            if terminator < 0 then
                failwith $"host fstatfs(%d{fd}) reported an f_fstypename with no NUL in its 16 bytes"

            let name =
                match
                    UnixByteString.ofBytes (ImmutableArray.Create<byte> (nameField.Slice(0, terminator).ToArray ()))
                with
                | Ok name -> name
                | Error defect ->
                    failwith $"host fstatfs(%d{fd}) reported an f_fstypename that is not a Unix string: %O{defect}"

            FileSystemTypeAnswer.Reported (FileSystemTypeFields.Darwin (fType, name))
