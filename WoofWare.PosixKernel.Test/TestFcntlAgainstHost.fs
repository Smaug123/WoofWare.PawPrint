namespace WoofWare.PosixKernel.Test

open System
open System.IO
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `fcntl(2)`'s status and descriptor flags, and `dup2(2)`, against the Linux
/// kernel this test runs on: CI's x86-64, whose numbering the committed probe
/// (measured on aarch64) does not cover, and aarch64 where it is run by hand.
///
/// Linux only. `fcntl` is variadic, and Darwin's arm64 ABI passes a variadic
/// argument on the stack where a P/Invoke passes it in a register, so a call
/// from here would not reach Darwin's kernel with its argument; Linux's x86-64
/// and aarch64 ABIs pass an `int` the same way either side. Darwin's column is
/// `TestFcntlMeasured`'s.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFcntlAgainstHost =

    [<DllImport("libc", EntryPoint = "fcntl", SetLastError = true)>]
    extern int private hostFcntl(int fd, int command, int argument)

    [<DllImport("libc", EntryPoint = "open", SetLastError = true)>]
    extern int private hostOpen(string path, int flags, int mode)

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "dup2", SetLastError = true)>]
    extern int private hostDup2(int oldFd, int newFd)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    /// Run `action` on this Linux host's preset, or skip.
    let private onLinux (action : SimulatedUnixPlatform -> unit) : unit =
        HostPlatform.onUnixHostPreset (fun platform ->
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin ->
                Assert.Ignore
                    "fcntl is variadic, which a P/Invoke cannot call on Darwin arm64; TestFcntlMeasured holds Darwin"
            | SimulatedUnixFlavour.Linux -> action platform
        )

    /// What a host call answered: "ok 0x..." or the model's name for its errno.
    let private hostWord (platform : SimulatedUnixPlatform) (result : int) : string =
        if result >= 0 then
            $"ok 0x%x{result}"
        else
            let errno = Marshal.GetLastPInvokeError ()

            match UnixError.ofRawErrnoUnder (SimulatedUnixPlatform.rawErrnoNumbering platform) errno with
            | Some error -> $"%A{error}"
            | None -> $"errno %d{errno}"

    /// The kinds compared: each makes a host descriptor and the model's.
    let private kinds : string list =
        [
            "file-rdwr"
            "file-rdonly"
            "dir-o_directory"
            "pipe-r"
            "pipe-w"
            "inet-stream"
            "unix-stream"
        ]

    /// A fresh host descriptor of `kind`, and every descriptor to close after.
    let private hostMake (platform : SimulatedUnixPlatform) (directory : string) (kind : string) : int * int list =
        let file = Path.Combine (directory, "f")
        let dir = Path.Combine (directory, "d")

        let directoryFlag =
            OpenFlagNumbering.linuxDirectory (SimulatedUnixPlatform.architecture platform)

        let fd, extra =
            match kind with
            | "file-rdwr" -> hostOpen (file, 2, 0), []
            | "file-rdonly" -> hostOpen (file, 0, 0), []
            | "dir-o_directory" -> hostOpen (dir, directoryFlag, 0), []
            | "pipe-r"
            | "pipe-w" ->
                let fds = [| -1 ; -1 |]
                hostPipe fds |> shouldEqual 0

                if kind = "pipe-r" then
                    fds.[0], [ fds.[1] ]
                else
                    fds.[1], [ fds.[0] ]
            | "inet-stream" -> hostSocket (2, 1, 0), []
            | "unix-stream" -> hostSocket (1, 1, 0), []
            | other -> failwith $"unknown kind %s{other}"

        if fd < 0 then
            failwith $"making %s{kind} on the host failed with errno %d{Marshal.GetLastPInvokeError ()}"

        fd, fd :: extra

    let private withHostDirectory (action : string -> unit) : unit =
        let directory =
            Path.Combine (Path.GetTempPath (), $"fcntl-%s{Guid.NewGuid().ToString ()}")

        Directory.CreateDirectory directory |> ignore
        File.WriteAllBytes (Path.Combine (directory, "f"), "abcdefgh"B)
        Directory.CreateDirectory (Path.Combine (directory, "d")) |> ignore

        try
            action directory
        finally
            Directory.Delete (directory, true)

    let private runFor (platform : SimulatedUnixPlatform) : FcntlWorld.Run =
        {
            Name = "host"
            Platform = platform
            Caller = Owners.root
            Lines = []
        }

    [<Test>]
    let ``F_GETFL of every kind is the host's`` () : unit =
        onLinux (fun platform ->
            withHostDirectory (fun directory ->
                for kind in kinds do
                    let fd, opened = hostMake platform directory kind
                    let host = hostWord platform (hostFcntl (fd, FcntlWorld.GetFl, 0))
                    List.iter (hostClose >> ignore) opened

                    match FcntlWorld.make kind (FcntlWorld.system (runFor platform)) with
                    | Some (fd, system) -> (kind, FcntlWorld.statusFlags fd system) |> shouldEqual (kind, host)
                    | None -> failwith $"the model makes no %s{kind}"
            )
        )

    /// Each bit on its own beside the word `F_GETFL` reported, on a fresh
    /// descriptor of each kind: what the host's `F_SETFL` answered and left,
    /// against the model's, wherever the model does not refuse the word. A
    /// refused word must hold a flag the model does not model.
    [<Test>]
    let ``F_SETFL of every bit on every kind answers and leaves what the host's does`` () : unit =
        onLinux (fun platform ->
            withHostDirectory (fun directory ->
                let mutable compared = 0

                for kind in kinds do
                    for bit in 0..31 do
                        let fd, opened = hostMake platform directory kind
                        let before = hostFcntl (fd, FcntlWorld.GetFl, 0)
                        let word = before ||| (1 <<< bit)
                        let set = hostWord platform (hostFcntl (fd, FcntlWorld.SetFl, word))
                        let after = hostWord platform (hostFcntl (fd, FcntlWorld.GetFl, 0))
                        List.iter (hostClose >> ignore) opened

                        match FcntlWorld.make kind (FcntlWorld.system (runFor platform)) with
                        | None -> failwith $"the model makes no %s{kind}"
                        | Some (fd, system) ->

                        match UnixDescriptor.fcntl fd FcntlWorld.SetFl word system with
                        | Error (FcntlRefusal.UnmodelledStatusFlags _) -> ()
                        | Error refusal -> failwith $"%s{kind} bit %d{bit}: %s{FcntlRefusal.describe refusal}"
                        | Ok (answer, system) ->
                            compared <- compared + 1

                            (kind, bit, FcntlWorld.word answer, FcntlWorld.statusFlags fd system)
                            |> shouldEqual (kind, bit, set, after)

                compared |> shouldBeGreaterThan 150
            )
        )

    /// F_SETFD of every bit, and F_GETFD after it.
    [<Test>]
    let ``F_SETFD keeps the bits the host's keeps`` () : unit =
        onLinux (fun platform ->
            withHostDirectory (fun directory ->
                for word in [ 0 ; -1 ] @ [ for bit in 0..31 -> 1 <<< bit ] do
                    let fd, opened = hostMake platform directory "file-rdwr"
                    let set = hostWord platform (hostFcntl (fd, FcntlWorld.SetFd, word))
                    let after = hostWord platform (hostFcntl (fd, FcntlWorld.GetFd, 0))
                    List.iter (hostClose >> ignore) opened

                    let fd, system =
                        FcntlWorld.make "file-rdwr" (FcntlWorld.system (runFor platform)) |> Option.get

                    let answer, system = FcntlWorld.fcntl fd FcntlWorld.SetFd word system

                    (word, FcntlWorld.word answer, FcntlWorld.descriptorFlags fd system)
                    |> shouldEqual (word, set, after)
            )
        )

    /// `dup2` onto an open, a free and the same descriptor, and from a closed
    /// one, and `F_DUPFD` from every minimum below a table with gaps: the
    /// answers and the descriptor flags after, as the host's.
    [<Test>]
    let ``dup2 and F_DUPFD answer and flag as the host's do`` () : unit =
        onLinux (fun platform ->
            withHostDirectory (fun directory ->
                // Well above anything the test host holds, so that the gaps are
                // the test's own.
                let baseFd = 900

                let hostTable () =
                    for i in 0..5 do
                        let fd, _ = hostMake platform directory "file-rdonly"
                        hostDup2 (fd, baseFd + i) |> shouldEqual (baseFd + i)
                        hostClose fd |> ignore

                    hostClose (baseFd + 2) |> ignore
                    hostClose (baseFd + 4) |> ignore

                let hostClean () =
                    for i in 0..20 do
                        hostClose (baseFd + i) |> ignore

                let modelTable () =
                    let system = FcntlWorld.system (runFor platform)
                    let fd, system = FcntlWorld.make "file-rdonly" system |> Option.get

                    let system =
                        (system, [ 0..5 ])
                        ||> List.fold (fun system i ->
                            match UnixDescriptor.dup2 fd (baseFd + i) system with
                            | Ok (_, system) -> system
                            | Error refusal -> failwith $"%s{Dup2Refusal.describe refusal}"
                        )

                    [ fd ; baseFd + 2 ; baseFd + 4 ]
                    |> List.fold
                        (fun system fd ->
                            match UnixDescriptor.close fd system with
                            | Ok (_, system) -> system
                            | Error refusal -> failwith $"%s{CloseRefusal.describe refusal}"
                        )
                        system

                for minimum in [ baseFd .. baseFd + 7 ] do
                    for command in [ FcntlWorld.DupFd ; FcntlWorld.dupFdCloexec platform ] do
                        hostTable ()
                        let created = hostFcntl (baseFd, command, minimum)
                        let host = hostWord platform created
                        let flags = hostWord platform (hostFcntl (created, FcntlWorld.GetFd, 0))
                        hostClean ()

                        let answer, system = FcntlWorld.fcntl baseFd command minimum (modelTable ())

                        let modelFlags =
                            match answer with
                            | SyscallAnswer.Completed fd -> FcntlWorld.descriptorFlags (int fd) system
                            | SyscallAnswer.Failed _ -> FcntlWorld.descriptorFlags -1 system

                        (minimum, command, FcntlWorld.word answer, modelFlags)
                        |> shouldEqual (minimum, command, host, flags)

                for oldFd, newFd in
                    [
                        baseFd, baseFd + 1
                        baseFd, baseFd + 2
                        baseFd, baseFd
                        baseFd + 2, baseFd + 1
                        baseFd + 2, baseFd + 2
                        baseFd, -1
                    ] do
                    hostTable ()
                    hostFcntl (baseFd + 1, FcntlWorld.SetFd, 1) |> ignore
                    let host = hostDup2 (oldFd, newFd)
                    let hostAnswer = if host >= 0 then $"ok %d{host}" else hostWord platform host
                    let flags = hostWord platform (hostFcntl (newFd, FcntlWorld.GetFd, 0))
                    hostClean ()

                    let system = modelTable ()
                    let _, system = FcntlWorld.fcntl (baseFd + 1) FcntlWorld.SetFd 1 system

                    match UnixDescriptor.dup2 oldFd newFd system with
                    | Ok (answer, system) ->
                        (oldFd, newFd, FcntlWorld.number answer, FcntlWorld.descriptorFlags newFd system)
                        |> shouldEqual (oldFd, newFd, hostAnswer, flags)
                    | Error refusal -> failwith $"%s{Dup2Refusal.describe refusal}"
            )
        )
