namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PosixKernel

/// `PipeBuffer`, put to a real pipe on the kernel running the suite, on the same
/// random non-blocking reads and writes: what each call took or returned, the
/// bytes held, and whether each end polls ready.
///
/// Each host falsifies its own column: macOS locally, Linux in CI. CI's Linux
/// is x86-64, which the recorded corpus (aarch64) does not cover.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipeBufferAgainstHost =

    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private PollFd =
        val mutable Fd : int
        val mutable Events : int16
        val mutable Revents : int16

    [<DllImport("libc", EntryPoint = "pipe", SetLastError = true)>]
    extern int private hostPipe(int[] fds)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private hostClose(int fd)

    [<DllImport("libc", EntryPoint = "write", SetLastError = true)>]
    extern nativeint private hostWrite(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "read", SetLastError = true)>]
    extern nativeint private hostRead(int fd, byte[] buffer, unativeint count)

    [<DllImport("libc", EntryPoint = "poll", SetLastError = true)>]
    extern int private hostPoll([<In ; Out>] PollFd[] fds, unativeint count, int timeout)

    // `fcntl(2)` and `ioctl(2)` are variadic, which a P/Invoke cannot call
    // portably (Apple's arm64 ABI passes variadic arguments on the stack), so
    // these two go through the runtime's own fixed-arity wrappers of them.
    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_FcntlSetIsNonBlocking")>]
    extern int private hostSetNonBlocking(nativeint fd, int isNonBlocking)

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_GetBytesAvailable")>]
    extern int private hostBytesAvailable(nativeint fd, int& available)

    [<Literal>]
    let private POLLIN = 0x1s

    [<Literal>]
    let private POLLOUT = 0x4s

    let private eagain (flavour : SimulatedUnixFlavour) : int =
        match flavour with
        | SimulatedUnixFlavour.Linux -> 11
        | SimulatedUnixFlavour.Darwin -> 35

    let private readyIn (fd : int) (events : int16) : bool =
        let mutable entry = PollFd ()
        entry.Fd <- fd
        entry.Events <- events
        let fds = [| entry |]

        if hostPoll (fds, 1un, 0) < 0 then
            failwith $"poll failed: errno %d{Marshal.GetLastPInvokeError ()}"

        (fds.[0].Revents &&& events) <> 0s

    [<Test>]
    let ``a real pipe and the model answer every call alike`` () : unit =
        HostPlatform.onUnixHostPreset (fun platform ->
            let flavour = SimulatedUnixPlatform.flavour platform

            let property (ops : PipeBufferOp list) : unit =
                let fds = Array.zeroCreate<int> 2

                if hostPipe fds <> 0 then
                    failwith $"pipe failed: errno %d{Marshal.GetLastPInvokeError ()}"

                try
                    for fd in fds do
                        if hostSetNonBlocking (nativeint fd, 1) <> 0 then
                            failwith $"could not make fd %d{fd} non-blocking"

                    let mutable buffer = PipeBuffer.empty platform
                    let mutable offered = 0

                    for i, op in List.indexed ops do
                        let where = $"%O{platform}, call %d{i} (%O{op})"

                        match op with
                        | PipeBufferOp.Write count ->
                            let bytes = TestPipeBuffer.payload offered count
                            let host = hostWrite (fds.[1], Seq.toArray bytes, unativeint count)
                            let errno = Marshal.GetLastPInvokeError ()
                            let accepted, b = PipeBuffer.write bytes buffer

                            let hostAccepted =
                                if host >= 0n then
                                    int host
                                elif errno = eagain flavour && count > 0 then
                                    0
                                else
                                    failwith $"%s{where}: the host's write failed with errno %d{errno}"

                            if hostAccepted <> accepted then
                                failwith $"%s{where}: the host took %d{hostAccepted}, the model %d{accepted}"

                            buffer <- b
                            offered <- offered + accepted
                        | PipeBufferOp.Read count ->
                            let target = Array.zeroCreate<byte> (max count 1)
                            let host = hostRead (fds.[0], target, unativeint count)
                            let errno = Marshal.GetLastPInvokeError ()
                            let got, b = PipeBuffer.read count buffer

                            let hostGot =
                                if host >= 0n then
                                    Array.sub target 0 (int host)
                                elif errno = eagain flavour && count > 0 then
                                    [||]
                                else
                                    failwith $"%s{where}: the host's read failed with errno %d{errno}"

                            if hostGot <> Seq.toArray got then
                                failwith
                                    $"%s{where}: the host read %d{hostGot.Length} bytes, the model %d{got.Length}, or their contents differ"

                            buffer <- b

                        let mutable available = 0

                        if hostBytesAvailable (nativeint fds.[0], &available) <> 0 then
                            failwith $"%s{where}: FIONREAD failed"

                        if available <> PipeBuffer.held buffer then
                            failwith $"%s{where}: the host holds %d{available}, the model %d{PipeBuffer.held buffer}"

                        let hostReadable = readyIn fds.[0] POLLIN
                        let hostWritable = readyIn fds.[1] POLLOUT

                        if
                            (hostReadable, hostWritable)
                            <> (PipeBuffer.readable buffer, PipeBuffer.writable buffer)
                        then
                            failwith
                                $"%s{where}: with %d{available} held, the host polls (readable %b{hostReadable}, writable %b{hostWritable}), the model (%b{PipeBuffer.readable buffer}, %b{PipeBuffer.writable buffer})"
                finally
                    hostClose fds.[0] |> ignore
                    hostClose fds.[1] |> ignore

            Check.One (
                Config.QuickThrowOnFailure.WithMaxTest 300,
                Prop.forAll (Arb.fromGen TestPipeBuffer.opsGen) property
            )
        )
