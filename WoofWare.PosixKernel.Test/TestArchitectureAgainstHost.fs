namespace WoofWare.PosixKernel.Test

open System
open System.Runtime.InteropServices
open NUnit.Framework
open WoofWare.PosixKernel

/// The facts the architecture and page size decide, asked of the kernel this
/// test process runs on and compared with the preset for its flavour and
/// architecture: macOS on arm64 locally, Linux on x86-64 in CI.
///
/// Each failure reports the host's answer, so one run on a machine whose
/// answer differs says what to correct.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestArchitectureAgainstHost =

    [<DllImport("libc", EntryPoint = "epoll_create1", SetLastError = true)>]
    extern int private epollCreate1(int flags)

    [<DllImport("libc", EntryPoint = "epoll_wait", SetLastError = true)>]
    extern int private epollWait(int epfd, nativeint events, int maxevents, int timeout)

    [<DllImport("libc", EntryPoint = "close")>]
    extern int private closeFd(int fd)

    [<Literal>]
    let private EFAULT : int = 14

    [<Literal>]
    let private EINVAL : int = 22

    [<Test>]
    let ``the page size is the preset's`` () : unit =
        HostPlatform.onUnixHostPreset (fun platform ->
            let modelled = SimulatedPageSize.bytes (SimulatedUnixPlatform.pageSize platform)

            if Environment.SystemPageSize <> modelled then
                failwith
                    $"this host's pages are %d{Environment.SystemPageSize} bytes, and the %O{platform} preset's are %d{modelled}. A kernel with these pages is one `SimulatedUnixPlatform.create` has not been told about: measure it with architecture-facts-linux.c and admit it."
        )

    /// Runs `action` on an empty epoll instance. Nothing is ever ready on it, so
    /// a zero-timeout `epoll_wait` copies nothing, whatever buffer it is handed,
    /// and the only answers it can give are the screens': a count of 0, EINVAL
    /// or EFAULT.
    let private withEmptyEpoll (action : SimulatedUnixPlatform -> (nativeint -> int -> Result<int, int>) -> unit) =
        HostPlatform.onUnixHostPreset (fun platform ->
            match SimulatedUnixPlatform.flavour platform with
            | SimulatedUnixFlavour.Darwin -> Assert.Ignore "epoll is a Linux interface"
            | SimulatedUnixFlavour.Linux -> ()

            let ep = epollCreate1 0

            if ep < 0 then
                failwith $"epoll_create1 failed with errno %d{Marshal.GetLastPInvokeError ()}"

            try
                let wait (buffer : nativeint) (maxEvents : int) : Result<int, int> =
                    let r = epollWait (ep, buffer, maxEvents, 0)

                    if r < 0 then
                        Error (Marshal.GetLastPInvokeError ())
                    elif r = 0 then
                        Ok r
                    else
                        failwith $"epoll_wait on an empty instance reported %d{r} events"

                action platform wait
            finally
                closeFd ep |> ignore<int>
        )

    /// The largest `maxevents` that is not EINVAL is the architecture's
    /// `EP_MAX_EVENTS`: that count is admitted and one more is not.
    [<Test>]
    let ``epoll's event bound is this architecture's`` () : unit =
        withEmptyEpoll (fun platform wait ->
            let buffer = Marshal.AllocHGlobal 64

            try
                let cap = LinuxEpollLimits.maxEvents (SimulatedUnixPlatform.architecture platform)

                match wait buffer cap, wait buffer (cap + 1) with
                | Ok 0, Error errno when errno = EINVAL -> ()
                | atCap, pastCap ->
                    failwith
                        $"%O{platform}: the modelled bound is %d{cap}, but this kernel answered %O{atCap} at it and %O{pastCap} one past it (Ok count, or Error errno)."
            finally
                Marshal.FreeHGlobal buffer
        )

    /// The byte range `epoll_wait` screens is `maxevents` elements of the
    /// architecture's `struct epoll_event`: the highest base address that is not
    /// EFAULT falls by exactly one element for each further event.
    [<Test>]
    let ``epoll screens elements of this architecture's size`` () : unit =
        withEmptyEpoll (fun platform wait ->
            let refuses (address : uint64) (maxEvents : int) : bool =
                match wait (nativeint (int64 address)) maxEvents with
                | Ok _ -> false
                | Error errno when errno = EFAULT -> true
                | Error errno -> failwith $"epoll_wait at 0x%x{address} answered errno %d{errno}"

            // Monotone: low addresses pass (nothing is copied), and every address
            // whose range reaches past the user address space is refused.
            let highestAccepted (maxEvents : int) : uint64 =
                let rec go (lo : uint64) (hi : uint64) =
                    if hi - lo <= 1UL then
                        lo
                    else
                        let mid = lo + (hi - lo) / 2UL
                        if refuses mid maxEvents then go lo mid else go mid hi

                if refuses 4096UL maxEvents || not (refuses UInt64.MaxValue maxEvents) then
                    failwith "epoll_wait's buffer screen is not the range check this test bisects"

                go 4096UL UInt64.MaxValue

            let one = highestAccepted 1
            let two = highestAccepted 2
            let measured = one - two

            let modelled =
                uint64 (LinuxEpollLimits.eventSize (SimulatedUnixPlatform.architecture platform))

            if measured <> modelled then
                failwith
                    $"%O{platform}: this kernel's screened stride is %d{measured} bytes (highest base 0x%x{one} for one event, 0x%x{two} for two), and the model's element is %d{modelled}."
        )
