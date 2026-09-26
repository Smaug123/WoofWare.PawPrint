namespace WoofWare.PosixKernel.Test

open System
open System.Buffers.Binary
open System.Collections.Immutable
open System.IO
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// One step of a script run against descriptors onto one directory, on this
/// host's kernel and on the model at once. Each index picks among the live
/// descriptors, modulo how many there are.
[<RequireQualifiedAccess>]
type HostDirectoryStep =
    /// `open(d, O_RDONLY)`: a fresh description.
    | Open
    /// `dup` of a live descriptor: the same description.
    | Dup of which : int
    /// One `getdents` call through a live descriptor.
    | Read of which : int
    /// `lseek(fd, 0, SEEK_SET)` on a live descriptor.
    | Rewind of which : int
    | Close of which : int

/// `UnixNamespace.readDirectoryEntry` against this host's own `getdents64`
/// (Linux, on tmpfs) or `__getdirentries64` (Darwin, on APFS), called directly
/// rather than through libc.
///
/// The model's order among the names is its own, so nothing here compares an
/// order. What it compares is what every order must agree on: which names a
/// description yields between a rewind and end-of-directory, that a `dup`
/// shares the position and a second `open` does not, `d_ino` and `d_type` for
/// every entry, the dots included, and each flavour's errno for a removed
/// directory and for descriptors that are not directories.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestDirectoryReadAgainstHost =

    let private context : string = "TestDirectoryReadAgainstHost"

    // ------------------------------------------------------------- the host

    [<DllImport("libc", EntryPoint = "syscall", SetLastError = true)>]
    extern nativeint private linuxSyscall(nativeint number, int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "__getdirentries64", SetLastError = true)>]
    extern nativeint private darwinGetDirEntries(int fd, byte[] buffer, unativeint count, int64& basep)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_LStat", SetLastError = true)>]
    extern int private hostLStat(string path, byte[] output)

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "dup", SetLastError = true)>]
    extern int private hostDup(int fd)

    [<DllImport("libc", EntryPoint = "lseek", SetLastError = true)>]
    extern int64 private hostLSeek(int fd, int64 offset, int whence)

    [<DllImport("libc", EntryPoint = "mkdir", SetLastError = true)>]
    extern int private hostMkdir(string path, uint32 mode)

    [<DllImport("libc", EntryPoint = "rmdir", SetLastError = true)>]
    extern int private hostRmdir(string path)

    [<DllImport("libc", EntryPoint = "unlink", SetLastError = true)>]
    extern int private hostUnlink(string path)

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    let private hostSucceeded (what : string) (result : int) : unit =
        if result < 0 then
            failwith $"host %s{what} failed: errno %d{Marshal.GetLastPInvokeError ()}"

    /// One record as the host's syscall wrote it: `d_ino`, `d_type` and the
    /// name. Linux's `linux_dirent64` puts `d_type` at 18 and the name at 19;
    /// Darwin's `struct direntry` puts them at 20 and 21 (directory-layout.c).
    type private HostRecord =
        {
            Inode : uint64
            Type : byte
            Name : string
        }

    /// Linux's `getdents64` number is architecture-dependent; Darwin needs none.
    let private getdents64Number () : nativeint =
        match RuntimeInformation.ProcessArchitecture with
        | Architecture.X64 -> 217n
        | Architecture.Arm64 -> 61n
        | other -> failwith $"no getdents64 syscall number recorded for %O{other}"

    /// One call's worth of records, or the errno. `capacity` is below
    /// Darwin's 1024-byte threshold, above which its last four bytes would be a
    /// flags word rather than record space.
    let private hostRead
        (flavour : SimulatedUnixFlavour)
        (fd : int)
        (capacity : int)
        : Result<HostRecord list, UnixError>
        =
        let buffer = Array.zeroCreate<byte> capacity
        Marshal.SetLastPInvokeError 0

        let returned =
            match flavour with
            | SimulatedUnixFlavour.Linux -> linuxSyscall (getdents64Number (), fd, buffer, unativeint capacity)
            | SimulatedUnixFlavour.Darwin ->
                let mutable basep = 0L
                darwinGetDirEntries (fd, buffer, unativeint capacity, &basep)

        if returned < 0n then
            let raw = Marshal.GetLastPInvokeError ()

            match
                UnixError.ofRawErrnoUnder
                    (SimulatedUnixPlatform.rawErrnoNumbering (HostPlatform.platformOf flavour))
                    raw
            with
            | Some error -> Error error
            | None -> failwith $"host directory read failed with errno %d{raw}, which has no name"
        else

        let typeAt, nameAt =
            match flavour with
            | SimulatedUnixFlavour.Linux -> 18, 19
            | SimulatedUnixFlavour.Darwin -> 20, 21

        let rec parse (at : int) (acc : HostRecord list) =
            if at >= int returned then
                List.rev acc
            else
                let reclen =
                    int (BinaryPrimitives.ReadUInt16LittleEndian (ReadOnlySpan (buffer, at + 16, 2)))

                if reclen = 0 then
                    failwith "host directory record with d_reclen 0"

                let nameSpan = ReadOnlySpan (buffer, at + nameAt, reclen - nameAt)
                let terminator = nameSpan.IndexOf 0uy

                let record =
                    {
                        Inode = BinaryPrimitives.ReadUInt64LittleEndian (ReadOnlySpan (buffer, at, 8))
                        Type = buffer.[at + typeAt]
                        Name = Text.Encoding.UTF8.GetString (nameSpan.Slice (0, terminator))
                    }

                parse (at + reclen) (record :: acc)

        Ok (parse 0 [])

    /// `st_ino` through the shim's `SystemNative_LStat`, whose `FileStatus`
    /// has one layout on every platform (`Ino` at byte 104).
    let private hostInode (path : string) : uint64 =
        let output = Array.zeroCreate<byte> 256
        hostLStat (path, output) |> hostSucceeded $"lstat %s{path}"
        BitConverter.ToUInt64 (output, 104)

    /// A directory on this host whose filesystem is the one the library
    /// defaults to for `flavour`: `/dev/shm` for tmpfs on Linux, and the
    /// temporary directory for APFS on macOS.
    let private hostDirectoryFor (flavour : SimulatedUnixFlavour) : string option =
        let candidate =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "/dev/shm"
            | SimulatedUnixFlavour.Darwin -> Path.GetTempPath ()

        let wanted =
            EmulatedFileSystemType.defaultFor flavour
            |> EmulatedFileSystemType.fieldsFor flavour
            |> FileSystemTypeAnswer.Reported

        if not (Directory.Exists candidate) then
            None
        else

        let fd = hostOpen (candidate, 0, 0)

        if fd < 0 then
            None
        else

        try
            if HostFileSystemType.answerFor flavour fd = wanted then
                Some candidate
            else
                None
        finally
            hostClose fd |> ignore<int>

    let private onMeasurableHost (action : SimulatedUnixFlavour -> string -> unit) : unit =
        HostPlatform.onUnixHost (fun flavour ->
            match hostDirectoryFor flavour with
            | Some directory -> action flavour directory
            | None ->
                Assert.Ignore
                    $"this %O{flavour} host has no %O{EmulatedFileSystemType.defaultFor flavour} directory to measure"
        )

    // ------------------------------------------------------------ the model

    let private rooted (relative : string) : UnixPath =
        UnixPath.parseOrFail context $"/%s{relative}"

    let private reading : OpenFlags =
        {
            Access = FileAccessMode.ReadOnly
            Create = false
            Exclusive = false
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            Directory = false
        }

    let private creating : OpenFlags =
        { reading with
            Access = FileAccessMode.WriteOnly
            Create = true
        }

    let private completed
        (what : string)
        (answer : SyscallAnswer, system : UnixSystem<int, string>)
        : int64 * UnixSystem<int, string>
        =
        match answer with
        | SyscallAnswer.Completed value -> value, system
        | SyscallAnswer.Failed error -> failwith $"model %s{what} failed with %O{error}"

    let private modelInode (path : string) (system : UnixSystem<int, string>) : InodeNumber =
        match UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (rooted path) system with
        | Ok (FileStatusAnswer.Reported status) -> status.Inode
        | other -> failwith $"model stat %s{path}: %A{other}"

    // DT_DIR and DT_REG, the same number on both kernels.
    let private dtDirectory : byte = 4uy
    let private dtRegular : byte = 8uy

    // ------------------------------------------------------------ the script

    /// The directory's names: short and lower case, so that a case-insensitive
    /// APFS sees each as distinct and every record is small enough for the
    /// capacities below.
    let private entriesGen : Gen<(string * bool) list> =
        Gen.choose (0, 6)
        |> Gen.bind (fun count ->
            Gen.listOfLength count (Gen.elements [ true ; false ])
            |> Gen.map (List.mapi (fun i isDirectory -> $"n%d{i}", isDirectory))
        )

    let private stepGen : Gen<HostDirectoryStep> =
        let index = Gen.choose (0, 7)

        Gen.frequency
            [
                2, Gen.constant HostDirectoryStep.Open
                2, Gen.map HostDirectoryStep.Dup index
                8, Gen.map HostDirectoryStep.Read index
                2, Gen.map HostDirectoryStep.Rewind index
                1, Gen.map HostDirectoryStep.Close index
            ]

    /// One live descriptor in both worlds, and which description it shares.
    type private Live =
        {
            HostFd : int
            ModelFd : int
            Description : int
        }

    [<Test>]
    let ``a description yields every entry once between rewinds, shared across dup, as this host's does`` () : unit =
        onMeasurableHost (fun flavour hostBase ->
            let platform = HostPlatform.platformOf flavour

            // Linux returns one short record per call at 32 bytes; Darwin's
            // smallest accepted buffer is 64 bytes, which holds two.
            let capacity =
                match flavour with
                | SimulatedUnixFlavour.Linux -> 32
                | SimulatedUnixFlavour.Darwin -> 64

            let property (entries : (string * bool) list) (steps : HostDirectoryStep list) (removeAtEnd : bool) =
                let root =
                    Path.Combine (hostBase, "pawprint-getdents-" + Guid.NewGuid().ToString "N")

                hostMkdir (root, 0o755u) |> hostSucceeded "mkdir root"
                let hostD = Path.Combine (root, "d")
                hostMkdir (hostD, 0o755u) |> hostSucceeded "mkdir d"

                let mutable live : Live list = []

                try
                    let system : UnixSystem<int, string> = UnixSystem.initial platform

                    let system =
                        UnixNamespace.mkdir (rooted "d") 0o755 system |> completed "mkdir d" |> snd

                    let system =
                        (system, entries)
                        ||> List.fold (fun system (name, isDirectory) ->
                            if isDirectory then
                                hostMkdir (Path.Combine (hostD, name), 0o755u)
                                |> hostSucceeded $"mkdir %s{name}"

                                UnixNamespace.mkdir (rooted $"d/%s{name}") 0o755 system
                                |> completed $"mkdir %s{name}"
                                |> snd
                            else
                                File.Create(Path.Combine (hostD, name)).Dispose ()

                                let fd, system =
                                    UnixNamespace.openPath creating (rooted $"d/%s{name}") 0o644 system
                                    |> completed $"creat %s{name}"

                                match UnixDescriptor.close (int fd) system with
                                | Ok (SyscallAnswer.Completed _, system) -> system
                                | other -> failwith $"model close: %A{other}"
                        )

                    let everything = set ([ "." ; ".." ] @ List.map fst entries)
                    let isDirectory = Map.ofList ([ ".", true ; "..", true ] @ entries)

                    let hostPath (name : string) =
                        match name with
                        | "." -> hostD
                        | ".." -> root
                        | name -> Path.Combine (hostD, name)

                    let modelPath (name : string) =
                        match name with
                        | "." -> "d"
                        | ".." -> ""
                        | name -> $"d/%s{name}"

                    // What each description has yielded since it was opened or
                    // last rewound, in each world.
                    let mutable hostSeen : Map<int, string list> = Map.empty
                    let mutable modelSeen : Map<int, string list> = Map.empty
                    let mutable nextDescription = 0

                    let pick (which : int) : Live option =
                        match live with
                        | [] -> None
                        | _ -> Some (List.item (which % List.length live) live)

                    let checkComplete (description : int) =
                        let host = Map.tryFind description hostSeen |> Option.defaultValue []
                        let model = Map.tryFind description modelSeen |> Option.defaultValue []

                        if List.sort host <> List.sort model then
                            failwith $"description %d{description}: the host yielded %A{host} and the model %A{model}"

                        if List.length host <> Set.count everything || set host <> everything then
                            failwith
                                $"description %d{description}: the host yielded %A{host} before end-of-directory, not every entry once"

                    let openBoth (system : UnixSystem<int, string>) =
                        let hostFd = hostOpen (hostD, 0, 0)
                        hostSucceeded "open d" hostFd

                        let modelFd, system =
                            UnixNamespace.openPath reading (rooted "d") 0 system |> completed "open d"

                        live <-
                            live
                            @ [
                                {
                                    HostFd = hostFd
                                    ModelFd = int modelFd
                                    Description = nextDescription
                                }
                            ]

                        nextDescription <- nextDescription + 1
                        system

                    let system = openBoth system

                    let system =
                        (system, steps)
                        ||> List.fold (fun system step ->
                            match step with
                            | HostDirectoryStep.Open -> openBoth system
                            | HostDirectoryStep.Dup which ->
                                match pick which with
                                | None -> system
                                | Some target ->
                                    let hostFd = hostDup target.HostFd
                                    hostSucceeded "dup" hostFd

                                    let modelFd, system = UnixDescriptor.dup target.ModelFd system |> completed "dup"

                                    live <-
                                        live
                                        @ [
                                            { target with
                                                HostFd = hostFd
                                                ModelFd = int modelFd
                                            }
                                        ]

                                    system
                            | HostDirectoryStep.Rewind which ->
                                match pick which with
                                | None -> system
                                | Some target ->
                                    hostLSeek (target.HostFd, 0L, 0) |> shouldEqual 0L

                                    let answer, system =
                                        match UnixDescriptor.lseek target.ModelFd 0L 0 system with
                                        | Ok result -> result
                                        | Error refusal -> failwith $"model rewind refused: %A{refusal}"

                                    answer |> shouldEqual (SyscallAnswer.Completed 0L)
                                    hostSeen <- Map.remove target.Description hostSeen
                                    modelSeen <- Map.remove target.Description modelSeen
                                    system
                            | HostDirectoryStep.Close which ->
                                match pick which with
                                | None -> system
                                | Some target ->
                                    hostClose target.HostFd |> hostSucceeded "close"
                                    live <- live |> List.filter (fun l -> l.HostFd <> target.HostFd)

                                    match UnixDescriptor.close target.ModelFd system with
                                    | Ok (SyscallAnswer.Completed _, system) -> system
                                    | other -> failwith $"model close: %A{other}"
                            | HostDirectoryStep.Read which ->
                                match pick which with
                                | None -> system
                                | Some target ->
                                    let hostRecords =
                                        match hostRead flavour target.HostFd capacity with
                                        | Ok records -> records
                                        | Error error -> failwith $"host read failed with %O{error}"

                                    for record in hostRecords do
                                        record.Inode |> shouldEqual (hostInode (hostPath record.Name))

                                        record.Type
                                        |> shouldEqual (if isDirectory.[record.Name] then dtDirectory else dtRegular)

                                    // The model reads as many entries as the
                                    // host's call returned, one at a time; an
                                    // empty host answer is end-of-directory.
                                    let wanted = max 1 (List.length hostRecords)

                                    let rec modelReads (remaining : int) (acc : string list) system =
                                        if remaining = 0 then
                                            List.rev acc, system
                                        else

                                        match UnixNamespace.readDirectoryEntry target.ModelFd system with
                                        | Ok (ReadDirectoryAnswer.EndOfDirectory, system) -> List.rev acc, system
                                        | Ok (ReadDirectoryAnswer.Entry record, system) ->
                                            let name = record.Name.ToString ()
                                            record.Inode |> shouldEqual (modelInode (modelPath name) system)

                                            record.Kind
                                            |> shouldEqual (
                                                if isDirectory.[name] then
                                                    DirectoryEntryKind.Directory
                                                else
                                                    DirectoryEntryKind.RegularFile
                                            )

                                            modelReads (remaining - 1) (name :: acc) system
                                        | other -> failwith $"model read: %A{other}"

                                    let modelNames, system = modelReads wanted [] system

                                    match hostRecords with
                                    | [] ->
                                        if not modelNames.IsEmpty then
                                            failwith
                                                $"description %d{target.Description}: the host is at end-of-directory and the model yielded %A{modelNames}"

                                        checkComplete target.Description
                                    | _ ->
                                        if List.length modelNames <> List.length hostRecords then
                                            failwith
                                                $"description %d{target.Description}: the host yielded %A{hostRecords} and the model only %A{modelNames}"

                                        let add (name : string) (seen : Map<int, string list>) =
                                            Map.change
                                                target.Description
                                                (fun previous -> Some (name :: Option.defaultValue [] previous))
                                                seen

                                        hostSeen <-
                                            (hostSeen, hostRecords) ||> List.fold (fun seen r -> add r.Name seen)

                                        modelSeen <- (modelSeen, modelNames) ||> List.fold (fun seen n -> add n seen)

                                    system
                        )

                    if removeAtEnd then
                        // Empty the directory and remove it, then read every
                        // live descriptor once: ENOENT on Linux, end-of-directory
                        // on Darwin, whatever each had read.
                        for name, isDirectory in entries do
                            if isDirectory then
                                hostRmdir (Path.Combine (hostD, name)) |> hostSucceeded $"rmdir %s{name}"
                            else
                                hostUnlink (Path.Combine (hostD, name)) |> hostSucceeded $"unlink %s{name}"

                        hostRmdir hostD |> hostSucceeded "rmdir d"

                        let system =
                            (system, entries)
                            ||> List.fold (fun system (name, isDirectory) ->
                                let answer =
                                    if isDirectory then
                                        UnixNamespace.rmdir (rooted $"d/%s{name}") system
                                    else
                                        UnixNamespace.unlink (rooted $"d/%s{name}") system

                                answer |> completed $"remove %s{name}" |> snd
                            )

                        let system = UnixNamespace.rmdir (rooted "d") system |> completed "rmdir d" |> snd

                        for target in live do
                            let host =
                                match hostRead flavour target.HostFd capacity with
                                | Ok [] -> ReadDirectoryAnswer.EndOfDirectory
                                | Ok records -> failwith $"host read of a removed directory yielded %A{records}"
                                | Error error -> ReadDirectoryAnswer.Failed error

                            match UnixNamespace.readDirectoryEntry target.ModelFd system with
                            | Ok (model, _) -> model |> shouldEqual host
                            | Error refusal -> failwith $"model read of a removed directory refused: %A{refusal}"
                finally
                    for target in live do
                        hostClose target.HostFd |> ignore<int>

                    if Directory.Exists root then
                        Directory.Delete (root, true)

            Check.One (
                Config.QuickThrowOnFailure.WithMaxTest 60,
                Prop.forAll
                    (Arb.fromGen (
                        Gen.zip3 entriesGen (Gen.listOf stepGen |> Gen.resize 40) (Gen.elements [ true ; false ])
                    ))
                    (fun (e, s, r) -> property e s r)
            )
        )

    [<Test>]
    let ``every descriptor that is not a directory answers this host's errno`` () : unit =
        onMeasurableHost (fun flavour hostBase ->
            let platform = HostPlatform.platformOf flavour

            let root =
                Path.Combine (hostBase, "pawprint-getdents-" + Guid.NewGuid().ToString "N")

            hostMkdir (root, 0o755u) |> hostSucceeded "mkdir root"
            let hostFile = Path.Combine (root, "f")
            File.Create(hostFile).Dispose ()

            let hostPipe =
                let fds = Array.zeroCreate<int> 2
                hostPipe fds |> hostSucceeded "pipe"
                fds

            // AF_UNIX and SOCK_STREAM, 1 and 1 on both kernels.
            let hostSocket = hostSocket (1, 1, 0)
            hostSucceeded "socket" hostSocket
            let hostReadable = hostOpen (hostFile, 0, 0)
            hostSucceeded "open f O_RDONLY" hostReadable
            let hostWriteOnly = hostOpen (hostFile, 1, 0)
            hostSucceeded "open f O_WRONLY" hostWriteOnly

            try
                let system : UnixSystem<int, string> = UnixSystem.initial platform

                let fd, system =
                    UnixNamespace.openPath creating (rooted "f") 0o644 system |> completed "creat f"

                let system =
                    match UnixDescriptor.close (int fd) system with
                    | Ok (_, system) -> system
                    | Error refusal -> failwith $"%A{refusal}"

                let readable, system =
                    UnixNamespace.openPath reading (rooted "f") 0 system |> completed "open f"

                let writeOnly, system =
                    UnixNamespace.openPath
                        { reading with
                            Access = FileAccessMode.WriteOnly
                        }
                        (rooted "f")
                        0
                        system
                    |> completed "open f"

                let socket, system =
                    UnixSocket.createSocket SocketDomain.Unix SocketKind.Stream SocketProtocol.Unspecified system

                let hostAnswer (fd : int) : ReadDirectoryAnswer =
                    match hostRead flavour fd 4096 with
                    | Ok records -> failwith $"host read of a non-directory yielded %A{records}"
                    | Error error -> ReadDirectoryAnswer.Failed error

                let modelAnswer (fd : int) : ReadDirectoryAnswer =
                    match UnixNamespace.readDirectoryEntry fd system with
                    | Ok (answer, _) -> answer
                    | Error refusal -> failwith $"%A{refusal}"

                // The model's standard input and output are the ends of a pipe.
                [
                    "a readable regular file", hostReadable, int readable
                    "a write-only regular file", hostWriteOnly, int writeOnly
                    "a pipe's read end", hostPipe.[0], 0
                    "a pipe's write end", hostPipe.[1], 1
                    "a socket", hostSocket, socket
                    "a closed descriptor", 987, 987
                ]
                |> List.iter (fun (what, hostFd, modelFd) ->
                    let host = hostAnswer hostFd
                    let model = modelAnswer modelFd

                    if host <> model then
                        failwith $"%s{what}: this %O{flavour} host answers %A{host}, the model %A{model}"
                )
            finally
                for fd in [ hostPipe.[0] ; hostPipe.[1] ; hostSocket ; hostReadable ; hostWriteOnly ] do
                    hostClose fd |> ignore<int>

                Directory.Delete (root, true)
        )
