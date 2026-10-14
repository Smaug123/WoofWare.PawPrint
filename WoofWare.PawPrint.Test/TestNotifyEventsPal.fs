namespace WoofWare.PawPrint.Test

open System
open System.Buffers.Binary
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `NotifyEventsPal` rests on upstream's claim that each `PAL_IN_*` equals the
/// Linux `IN_*` of its name, and that the shim passes the mask on untouched.
/// Nothing in the type system keeps either true, so both are re-read here from
/// the pinned `pal_io.h` and `pal_io.c`, and the Linux side of each value from
/// the library where it states one.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNotifyEventsPal =

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails.
    let private palSource (leaf : string) : string =
        let root = FrameworkUnderTest.requireRuntimeSource ()

        let path = Path.Combine (root, "src", "native", "libs", "System.Native", leaf)

        if not (File.Exists path) then
            failwith
                $"TestNotifyEventsPal: expected the pinned PAL io source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/System.Native, this transcription has lost its oracle."

        File.ReadAllText path

    /// The `NotifyEvents` enum of the pinned `pal_io.h`: `PAL_IN_ACCESS =
    /// 0x00000001,` and the rest, by the name after `PAL_IN_`.
    let private pinnedNotifyEvents () : Map<string, uint32> =
        let header = palSource "pal_io.h"

        let body =
            Regex.Match (header, @"typedef enum\s*\{(?<body>[^}]*)\}\s*NotifyEvents;", RegexOptions.Singleline)

        if not body.Success then
            failwith "TestNotifyEventsPal: the pinned pal_io.h no longer declares the NotifyEvents enum."

        Regex.Matches (body.Groups.["body"].Value, @"PAL_IN_(?<name>[A-Z_]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+)")
        |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToUInt32 (m.Groups.["value"].Value, 16))
        |> Map.ofSeq

    /// The mask bit a record the library queues carries, read from the bytes
    /// `read(2)` would return for it.
    let private recordMask (record : InotifyRecord) : uint32 =
        let bytes = InotifyRecord.toBytes record
        BinaryPrimitives.ReadUInt32LittleEndian (bytes.AsSpan().Slice (4, 4))

    /// The Linux value of each name `NotifyEvents` holds. The events and the bits
    /// a record carries are the library's own; the three flags a watch asks
    /// with the library parses but does not publish, so they are Linux's
    /// `<sys/inotify.h>`, as `inotify-probe.c` prints them.
    let private linux : (string * uint32) list =
        let watch = WatchDescriptor 1

        [
            "ACCESS", InotifyEvent.bit InotifyEvent.Access
            "MODIFY", InotifyEvent.bit InotifyEvent.Modify
            "ATTRIB", InotifyEvent.bit InotifyEvent.Attrib
            "MOVED_FROM", InotifyEvent.bit InotifyEvent.MovedFrom
            "MOVED_TO", InotifyEvent.bit InotifyEvent.MovedTo
            "CREATE", InotifyEvent.bit InotifyEvent.Create
            "DELETE", InotifyEvent.bit InotifyEvent.Delete
            "Q_OVERFLOW", recordMask InotifyRecord.QueueOverflow
            "IGNORED", recordMask (InotifyRecord.Ignored watch)
            "ONLYDIR", 0x01000000u
            "DONT_FOLLOW", 0x02000000u
            "EXCL_UNLINK", 0x04000000u
            "ISDIR",
            recordMask (InotifyRecord.Event (watch, Set.singleton InotifyEvent.Attrib, true, 0u, None))
            ^^^ InotifyEvent.bit InotifyEvent.Attrib
        ]

    [<Test>]
    let ``every NotifyEvents value is the Linux bit of its name, and reaches the kernel as it is`` () : unit =
        Net11Frontier.park
            ".NET 11's `NotifyEvents` declares a different set of names from .NET 10's, which `NotifyEventsPal` transcribes. Un-park when the PAL conversion follows each runtime's enum."

        let pinned = pinnedNotifyEvents ()

        // A name added upstream, or one dropped, means this table and the enum
        // no longer describe one thing.
        pinned
        |> Map.keys
        |> Set.ofSeq
        |> shouldEqual (linux |> List.map fst |> Set.ofList)

        for name, value in linux do
            pinned.[name] |> shouldEqual value
            NotifyEventsPal.toInotifyMask pinned.[name] |> shouldEqual value

    [<Test>]
    let ``upstream asserts each NotifyEvents value equal to the platform's`` () : unit =
        let source = palSource "pal_io.c"

        for name in pinnedNotifyEvents () |> Map.keys do
            if not (Regex.IsMatch (source, $@"c_static_assert\(PAL_IN_%s{name} == IN_%s{name}\);")) then
                failwith
                    $"TestNotifyEventsPal: pal_io.c no longer asserts PAL_IN_%s{name} == IN_%s{name}, which NotifyEventsPal passing the mask on untouched relies on."

    /// The body of `name` in the pinned `pal_io.c`.
    let private shimBody (name : string) : string =
        let m =
            Regex.Match (palSource "pal_io.c", $@"\b%s{name}\([^)]*\)\s*\{{(?<body>.*?)\n\}}", RegexOptions.Singleline)

        if not m.Success then
            failwith
                $"TestNotifyEventsPal: the pinned pal_io.c no longer defines %s{name}, so its handler has lost its oracle."

        m.Groups.["body"].Value

    [<Test>]
    let ``the shim passes the mask to inotify_add_watch, clearing IN_EXCL_UNLINK only where it is undeclared``
        ()
        : unit
        =
        let body = shimBody "SystemNative_INotifyAddWatch"

        if not (Regex.IsMatch (body, @"return inotify_add_watch\(ToFileDescriptor\(fd\), pathName, mask\);")) then
            failwith
                $"TestNotifyEventsPal: SystemNative_INotifyAddWatch no longer hands its mask to inotify_add_watch as it is. It now reads:%s{Environment.NewLine}%s{body}"

        // Every assignment to the mask, which must be the one clearing
        // IN_EXCL_UNLINK under `#if !HAVE_IN_EXCL_UNLINK`.
        let assignments = Regex.Matches (body, @"\bmask\s*[&|^]?=")

        if
            assignments.Count <> 1
            || not (
                Regex.IsMatch (
                    body,
                    @"#if !HAVE_IN_EXCL_UNLINK\s*mask &= ~\(\(uint32_t\)PAL_IN_EXCL_UNLINK\);\s*#endif"
                )
            )
        then
            failwith
                $"TestNotifyEventsPal: SystemNative_INotifyAddWatch changes its mask other than by clearing IN_EXCL_UNLINK where the C library lacks it. It now reads:%s{Environment.NewLine}%s{body}"

    /// The three handlers answer ENOTSUP under the Darwin flavour without asking
    /// the library, because that is what the shim does where it is built
    /// without inotify, as every Darwin build is.
    [<TestCase "SystemNative_INotifyInit">]
    [<TestCase "SystemNative_INotifyAddWatch">]
    [<TestCase "SystemNative_INotifyRemoveWatch">]
    let ``the shim answers ENOTSUP itself where it is built without inotify`` (name : string) : unit =
        let body = shimBody name

        if not (Regex.IsMatch (body, @"#else.*errno = ENOTSUP;\s*return -1;\s*#endif", RegexOptions.Singleline)) then
            failwith
                $"TestNotifyEventsPal: %s{name} no longer answers -1 with ENOTSUP where HAVE_INOTIFY is unset. It now reads:%s{Environment.NewLine}%s{body}"

    [<Test>]
    let ``the shim's init asks for IN_CLOEXEC alone`` () : unit =
        let body = shimBody "SystemNative_INotifyInit"

        if not (Regex.IsMatch (body, @"return inotify_init1\(IN_CLOEXEC\);")) then
            failwith
                $"TestNotifyEventsPal: SystemNative_INotifyInit no longer calls inotify_init1(IN_CLOEXEC), which its handler passes as InotifyInitFlags.CloseOnExec. It now reads:%s{Environment.NewLine}%s{body}"
