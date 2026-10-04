namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PosixKernel

/// The fields of this host's own `struct stat` that the output struct of
/// .NET's `System.Native` `stat` does not carry, read in the library's
/// vocabulary: the oracle for
/// `FileStatus.LinkCount`, `SpecialFileDevice` and `FileFlags`.
type HostStatus =
    {
        LinkCount : int64
        SpecialFileDevice : int64
        FileFlags : uint32 option
    }

[<RequireQualifiedAccess>]
module HostStat =

    /// Where `st_nlink`, `st_rdev` and (Darwin) `st_flags` sit in a host's
    /// `struct stat`, and how wide each is.
    type private Layout =
        {
            LinkCountOffset : int
            LinkCountWidth : int
            DeviceOffset : int
            DeviceWidth : int
            FlagsOffset : int option
        }

    /// `struct stat` is read as raw bytes at offsets measured 2026-10-02 with
    /// `offsetof`: on Darwin (arm64), `st_nlink` is a `uint16_t` at 6,
    /// `st_rdev` an `int32_t` at 24 and `st_flags` a `uint32_t` at 116, of 144
    /// bytes; on Linux x86-64 (glibc) `st_nlink` is 8 bytes at 16 and
    /// `st_rdev` 8 at 40, of 144; on Linux aarch64 (glibc) `st_nlink` is 4
    /// bytes at 20 and `st_rdev` 8 at 32, of 128. Any other host is unmeasured.
    let private layoutFor (flavour : SimulatedUnixFlavour) : Layout option =
        match flavour, RuntimeInformation.ProcessArchitecture with
        | SimulatedUnixFlavour.Darwin, Architecture.Arm64 ->
            Some
                {
                    LinkCountOffset = 6
                    LinkCountWidth = 2
                    DeviceOffset = 24
                    DeviceWidth = 4
                    FlagsOffset = Some 116
                }
        | SimulatedUnixFlavour.Linux, Architecture.X64 ->
            Some
                {
                    LinkCountOffset = 16
                    LinkCountWidth = 8
                    DeviceOffset = 40
                    DeviceWidth = 8
                    FlagsOffset = None
                }
        | SimulatedUnixFlavour.Linux, Architecture.Arm64 ->
            Some
                {
                    LinkCountOffset = 20
                    LinkCountWidth = 4
                    DeviceOffset = 32
                    DeviceWidth = 8
                    FlagsOffset = None
                }
        | _ -> None

    [<DllImport("libc", EntryPoint = "lstat", SetLastError = true)>]
    extern int private hostLStat(string path, byte[] buffer)

    [<DllImport("libc", EntryPoint = "fstat", SetLastError = true)>]
    extern int private hostFStat(int fd, byte[] buffer)

    /// Whether this host's `struct stat` layout is one measured here.
    let isMeasured (flavour : SimulatedUnixFlavour) : bool = (layoutFor flavour).IsSome

    /// Skip the calling test on a host whose `struct stat` layout is
    /// unmeasured; otherwise run `action` against this host's flavour.
    let onMeasuredHost (action : SimulatedUnixFlavour -> unit) : unit =
        HostPlatform.onUnixHost (fun flavour ->
            match layoutFor flavour with
            | None ->
                Assert.Ignore
                    $"no measured struct stat layout for %O{flavour} on %O{RuntimeInformation.ProcessArchitecture}"
            | Some _ -> action flavour
        )

    let private read (flavour : SimulatedUnixFlavour) (what : string) (call : byte[] -> int) : HostStatus =
        if not BitConverter.IsLittleEndian then
            failwith "HostStat reads struct stat as little-endian, and this host is not"

        let layout =
            match layoutFor flavour with
            | Some layout -> layout
            | None -> failwith $"HostStat: no measured struct stat layout on this host; call onMeasuredHost first"

        let buffer = Array.zeroCreate<byte> 512

        if call buffer <> 0 then
            failwith $"host %s{what} failed: errno %d{Marshal.GetLastPInvokeError ()}"

        let unsignedAt (offset : int) (width : int) : int64 =
            match width with
            | 2 -> int64 (BinaryPrimitives.ReadUInt16LittleEndian (ReadOnlySpan (buffer, offset, 2)))
            | 4 -> int64 (BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan (buffer, offset, 4)))
            | 8 -> BinaryPrimitives.ReadInt64LittleEndian (ReadOnlySpan (buffer, offset, 8))
            | other -> failwith $"HostStat: no field is %d{other} bytes wide"

        {
            LinkCount = unsignedAt layout.LinkCountOffset layout.LinkCountWidth
            SpecialFileDevice = unsignedAt layout.DeviceOffset layout.DeviceWidth
            FileFlags =
                layout.FlagsOffset
                |> Option.map (fun offset -> BinaryPrimitives.ReadUInt32LittleEndian (ReadOnlySpan (buffer, offset, 4)))
        }

    /// `lstat(path)` on this host.
    let lstat (flavour : SimulatedUnixFlavour) (path : string) : HostStatus =
        read flavour $"lstat %s{path}" (fun buffer -> hostLStat (path, buffer))

    /// `fstat(fd)` on this host.
    let fstat (flavour : SimulatedUnixFlavour) (fd : int) : HostStatus =
        read flavour $"fstat %d{fd}" (fun buffer -> hostFStat (fd, buffer))

    /// The fields of a status the model reported, in the same shape.
    let ofModel (status : FileStatus) : HostStatus =
        {
            LinkCount = status.LinkCount
            SpecialFileDevice = status.SpecialFileDevice
            FileFlags = status.FileFlags
        }
