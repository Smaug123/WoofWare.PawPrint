namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `UnixSocket.socket` against the kernel running the suite, over the same
/// arguments as the checked-in sweeps, wherever the library answers rather than
/// refuses.
///
/// The sweeps were taken on one Linux kernel and one Darwin one. A Linux kernel
/// built differently -- CI's, with modules this one lacked -- is exactly where an
/// answer the library gives as the kernel's own could turn out to be that
/// build's, so this runs the whole grid again on whichever host it finds.
[<TestFixture>]
module TestSocketSyscallAgainstHost =

    [<DllImport("libc", EntryPoint = "socket", SetLastError = true)>]
    extern int private hostSocket(int domain, int kind, int protocol)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "getsockopt", SetLastError = true)>]
    extern int private hostGetSockOpt(int fd, int level, int optionName, int& value, int& optionLength)

    [<DllImport("libc", EntryPoint = "fcntl", SetLastError = true)>]
    extern int private hostFcntl(int fd, int command, int argument)

    [<Literal>]
    let private F_GETFL = 3

    /// `SOL_SOCKET`, `SO_TYPE` and `O_NONBLOCK`, which differ between the
    /// flavours.
    let private constants (flavour : SimulatedUnixFlavour) : int * int * int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 1, 3, 0x800
        | SimulatedUnixFlavour.Darwin -> 0xffff, 0x1008, 0x4

    /// What the host answered, in the vocabulary the library's answer is
    /// compared in: the created socket's type and non-blocking flag, or the raw
    /// errno.
    let private hostAnswer (flavour : SimulatedUnixFlavour) (domain : int) (kind : int) (protocol : int) =
        let solSocket, soType, nonBlock = constants flavour
        let fd = hostSocket (domain, kind, protocol)

        if fd < 0 then
            Error (Marshal.GetLastPInvokeError ())
        else
            try
                let mutable value = 0
                let mutable length = 4

                if hostGetSockOpt (fd, solSocket, soType, &value, &length) <> 0 then
                    failwith $"getsockopt(SO_TYPE) failed with errno %d{Marshal.GetLastPInvokeError ()}"

                let flags = hostFcntl (fd, F_GETFL, 0)

                if flags < 0 then
                    failwith $"fcntl(F_GETFL) failed with errno %d{Marshal.GetLastPInvokeError ()}"

                Ok (value, flags &&& nonBlock <> 0)
            finally
                hostClose fd |> ignore<int>

    [<Test>]
    let ``every call the library answers is answered the same by this host`` () : unit =
        HostPlatform.onUnixHost (fun flavour ->
            let platform = HostPlatform.platformOf flavour
            let system : UnixSystem<int, string> = UnixSystem.initial platform

            let numbering =
                match flavour with
                | SimulatedUnixFlavour.Linux -> RawErrnoNumbering.Linux
                | SimulatedUnixFlavour.Darwin -> RawErrnoNumbering.Darwin

            let file =
                match flavour with
                | SimulatedUnixFlavour.Linux -> "linux.tsv"
                | SimulatedUnixFlavour.Darwin -> "darwin.tsv"

            let mutable compared = 0
            let disagreements = ResizeArray ()

            for (domain, kind, protocol), _ in SocketSweep.load file do
                let library =
                    match UnixSocket.socket domain kind protocol system with
                    | Error _ -> None
                    | Ok (Error error) -> Some (Error (UnixError.toRawErrnoUnder numbering error))
                    | Ok (Ok (fd, after)) ->
                        let socket =
                            match FileDescriptorRegistry.tryFindTarget fd after.Process.FileDescriptors with
                            | Some (OpenFileTarget.Socket socketId) -> UnixMachineState.socket socketId after.Machine
                            | other -> failwith $"descriptor %d{fd} names %A{other}"

                        let soType =
                            match socket.Kind with
                            | SocketKind.Stream -> 1
                            | SocketKind.Datagram -> 2
                            | SocketKind.SeqPacket -> 5

                        Some (Ok (soType, UnixSocket.isNonBlocking fd after = Some true))

                match library with
                | None -> ()
                | Some library ->
                    compared <- compared + 1
                    let host = hostAnswer flavour domain kind protocol

                    if host <> library then
                        disagreements.Add
                            $"socket(%d{domain}, 0x%x{kind}, %d{protocol}): host %A{host}, library %A{library}"

            disagreements |> List.ofSeq |> List.truncate 20 |> shouldEqual []

            // Not vacuous: most of the sweep is answered.
            compared |> shouldBeGreaterThan 240_000
        )
