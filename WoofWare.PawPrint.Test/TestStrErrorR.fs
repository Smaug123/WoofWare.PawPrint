namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Runtime.InteropServices
open System.Text
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel
open WoofWare.PosixKernel.Test

/// `StrErrorR` transcribes what System.Native's `SystemNative_StrErrorR` does
/// against each C library, and `CLibrary` the text that library's `strerror_r`
/// answers. The oracles are the shim itself: its measured answers on both
/// flavours (`docs/plans/2026-08-23-posix-kernel-extraction/strerror-r.c`,
/// whose every row this regenerates from the model and compares, for each glibc
/// build and for Darwin's libc), and this host's own shim, asked directly.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStrErrorR =

    let private glibcBuilds : CLibrary list = HostCLibrary.glibcBuilds

    let private libraries : CLibrary list = glibcBuilds @ [ CLibrary.DarwinLibc ]

    /// The byte the probe fills its buffer with before every call.
    let private fill : byte = 0xAAuy

    /// The probe's buffer: twice the largest size it passes, so that a write
    /// past `size` would show.
    let private bufferLength : int = 2048

    /// One call as the probe sees it: what came back, and the buffer after it.
    type private Observed =
        {
            /// `BUFFER`, `NULL` or `OTHER`, in the probe's words.
            Return : string
            Buffer : byte[]
            /// The string at an `OTHER` return.
            Other : string option
            /// `kept` if errno still held what it was set to before the call,
            /// or the number it held after.
            Errno : string
        }

    /// The model's answer, played onto a freshly-filled buffer.
    let private observeModel (answer : StrErrorRAnswer) : Observed =
        let buffer = Array.create bufferLength fill

        match answer with
        | StrErrorRAnswer.RefusedSize ->
            {
                Return = "NULL"
                Buffer = buffer
                Other = None
                Errno = "kept"
            }
        | StrErrorRAnswer.LibraryText text ->
            {
                Return = "OTHER"
                Buffer = buffer
                Other = Some text
                Errno = "kept"
            }
        | StrErrorRAnswer.Buffer written ->
            written.CopyTo buffer

            {
                Return = "BUFFER"
                Buffer = buffer
                Other = None
                Errno = "kept"
            }
        | StrErrorRAnswer.NullAfterWriting written ->
            written.CopyTo buffer

            {
                Return = "NULL"
                Buffer = buffer
                Other = None
                Errno = "kept"
            }

    /// The probe's escaping: printable ASCII but the backslash as itself,
    /// every other byte as `\xNN`.
    let private escaped (bytes : byte seq) : string =
        let sb = StringBuilder ()

        for b in bytes do
            if b >= 0x20uy && b < 0x7fuy && b <> byte '\\' then
                sb.Append (char b) |> ignore
            else
                sb.Append $"\\x%02x{b}" |> ignore

        sb.ToString ()

    let private nulTerminated (bytes : byte[]) (limit : int) : byte[] =
        let length =
            match Array.tryFindIndex (fun b -> b = 0uy) (Array.truncate limit bytes) with
            | Some i -> i
            | None -> min limit bytes.Length

        Array.truncate length bytes

    let private textRow (n : int) (observed : Observed) : string =
        let changed =
            observed.Buffer
            |> Array.truncate 1024
            |> Array.filter (fun b -> b <> fill)
            |> Array.length

        let text =
            match observed.Other with
            | Some other -> escaped (Encoding.UTF8.GetBytes other)
            | None -> escaped (nulTerminated observed.Buffer 1024)

        $"TEXT\t%d{n}\t%s{observed.Return}\t%d{changed}\t%s{observed.Errno}\t%s{text}"

    let private sizeRow (n : int) (size : int) (observed : Observed) : string =
        let shown = if size <= 0 then 0 else min size 96
        let beyondFrom = if size <= 0 then 0 else size

        let beyond =
            [ beyondFrom .. min (beyondFrom + 63) (bufferLength - 1) ]
            |> List.exists (fun i -> observed.Buffer.[i] <> fill)

        let other =
            match observed.Other with
            | Some other -> "\t" + escaped (Encoding.UTF8.GetBytes other)
            | None -> ""

        let beyondWord = if beyond then "touched" else "untouched"
        $"SIZE\t%d{n}\t%d{size}\t%s{observed.Return}\t%s{beyondWord}\t%s{observed.Errno}\t%s{escaped (Array.truncate shown observed.Buffer)}%s{other}"

    let private extremes : int list =
        [
            Int32.MinValue
            Int32.MinValue + 1
            -0x20003
            -0x20002
            -0x20001
            -0x20000
            -0x10000
            Int32.MaxValue - 1
            Int32.MaxValue
        ]

    /// The numbers the probe's TEXT rows cover, in its order.
    let private textSweep : int list = extremes @ [ -300 .. 4096 ]

    /// The numbers the probe's SIZE rows cover, in its order.
    let private sizeSweep : int list = extremes @ [ -3 .. 140 ]

    let private fixedSizes : int list =
        [ Int32.MinValue ; -2 ; -1 ; 0 ; 1 ; 2 ; 3 ; 4 ; 8 ; 16 ; 1024 ]

    /// The text's length, from a call with room to spare, as the probe takes it.
    let private fullLength (observe : int -> int -> Observed) (n : int) : int =
        let observed = observe n 1024

        match observed.Other with
        | Some other -> Encoding.UTF8.GetByteCount other
        | None -> (nulTerminated observed.Buffer bufferLength).Length

    /// Every row the probe prints but `IDENTITY`, in its order, from `observe`.
    let private probeRows (observe : int -> int -> Observed) : string list =
        [
            for n in textSweep do
                textRow n (observe n 1024)
            for n in sizeSweep do
                let len = fullLength observe n

                for size in fixedSizes do
                    sizeRow n size (observe n size)

                for size in len - 2 .. len + 2 do
                    if size > 0 then
                        sizeRow n size (observe n size)
        ]

    let private measuredLines (library : CLibrary) : string list =
        let name =
            match library with
            | CLibrary.Glibc GlibcErrnoSet.ThroughEHWPOISON -> "WoofWare.PawPrint.Test.strerrorR.glibc.txt"
            | CLibrary.Glibc GlibcErrnoSet.ThroughEFTYPE -> "WoofWare.PawPrint.Test.strerrorR.glibc-eftype.txt"
            | CLibrary.DarwinLibc -> "WoofWare.PawPrint.Test.strerrorR.darwin.txt"

        let assembly = Assembly.GetExecutingAssembly ()

        use stream =
            match assembly.GetManifestResourceStream name with
            | null -> failwith $"embedded resource %s{name} not found"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> Array.toList
        |> List.filter (fun line -> not (line.StartsWith ("#", StringComparison.Ordinal)))

    let private modelObserve (library : CLibrary) (n : int) (size : int) : Observed =
        observeModel (StrErrorR.answer library n size)

    [<Test>]
    let ``every measured row is what the model answers`` () : unit =
        for library in libraries do
            let measured =
                measuredLines library
                |> List.filter (fun line -> not (line.StartsWith ("IDENTITY\t", StringComparison.Ordinal)))

            let modelled = probeRows (modelObserve library)

            modelled.Length |> shouldEqual measured.Length

            for expected, actual in List.zip measured modelled do
                if expected <> actual then
                    failwith
                        $"TestStrErrorR: under %O{library}, the shim was measured printing\n%s{expected}\nbut the model gives\n%s{actual}"

    /// The C library's own pointer is the same on every call (measured for
    /// every such row), where PawPrint hands out a fresh copy each time; this
    /// pins that the divergence is confined to the rows that return one.
    [<Test>]
    let ``the library's own text is returned only where the measurement says OTHER, and it was always the same pointer``
        ()
        : unit
        =
        for library in libraries do
            let lines = measuredLines library

            let others =
                lines
                |> List.choose (fun line ->
                    match line.Split '\t' with
                    | [| "TEXT" ; n ; "OTHER" ; _ ; _ ; _ |] -> Some (int n)
                    | _ -> None
                )

            let identities =
                lines
                |> List.choose (fun line ->
                    match line.Split '\t' with
                    | [| "IDENTITY" ; n ; verdict |] -> Some (int n, verdict)
                    | _ -> None
                )

            identities |> List.map fst |> shouldEqual others
            identities |> List.iter (fun (_, verdict) -> verdict |> shouldEqual "same")

            for n in textSweep do
                match StrErrorR.answer library n 1024 with
                | StrErrorRAnswer.LibraryText _ -> List.contains n others |> shouldEqual true
                | _ -> List.contains n others |> shouldEqual false

    /// Every error the library numbers has a text, and no error it does not
    /// number has one; and its numbering and its decoding agree.
    [<Test>]
    let ``errorText names exactly the errors the library numbers`` () : unit =
        for library in libraries do
            for error in UnixError.all do
                let number = CLibrary.numberOfError library error
                CLibrary.errorText library error |> Option.isSome |> shouldEqual number.IsSome

                // Decoding a number gives an error with that number: aliases
                // such as ENOTSUP and EOPNOTSUPP share one, so not always
                // `error` itself.
                match number with
                | Some n ->
                    CLibrary.errorOfNumber library n
                    |> Option.bind (CLibrary.numberOfError library)
                    |> shouldEqual (Some n)
                | None -> ()

    /// The two glibc builds differ by one row of the table: errno 134, which
    /// only the build against Linux 7.2's headers names. Everything else the
    /// shim answers, at every size, is the same.
    [<Test>]
    let ``the glibc builds answer alike but for errno 134`` () : unit =
        let older = CLibrary.Glibc GlibcErrnoSet.ThroughEHWPOISON
        let newer = CLibrary.Glibc GlibcErrnoSet.ThroughEFTYPE

        StrErrorR.answer newer 134 1024
        |> shouldEqual (StrErrorRAnswer.LibraryText "Inappropriate file type or format")

        StrErrorR.answer older 134 1024
        |> shouldEqual (
            StrErrorRAnswer.Buffer (ImmutableArray.Create<byte> (Encoding.ASCII.GetBytes "Unknown error 134\000"))
        )

        let property (n : int, size : int) : bool =
            n = 134 || StrErrorR.answer older n size = StrErrorR.answer newer n size

        let gen =
            gen {
                let! n =
                    Gen.oneof
                        [
                            ArbMap.defaults |> ArbMap.generate<int>
                            Gen.choose (-300, 4200)
                            Gen.choose (120, 140)
                            Gen.elements extremes
                        ]

                let! size = Gen.oneof [ Gen.choose (-3, 100) ; Gen.choose (Int32.MinValue, 1024) ]
                return n, size
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 4000, Prop.forAll (Arb.fromGen gen) property)

    /// The fallback past the sweep: the text for a number the library names
    /// nothing for is its fixed prefix and the number in decimal, which the
    /// sweep measured at both ends of `int32` and everywhere in [-300, 4096].
    [<Test>]
    let ``an unnamed number's text is the measured fallback, for every int32`` () : unit =
        let fallback =
            function
            | CLibrary.Glibc _ -> Regex @"^Unknown error (-?[0-9]+)$"
            | CLibrary.DarwinLibc -> Regex @"^Unknown error: (-?[0-9]+)$"

        let property (library : CLibrary) (n : int) : bool =
            let named = n = 0 || (CLibrary.errorOfNumber library n).IsSome
            // The two pseudo-errnos the shim answers itself.
            let pseudo = n = -0x20001 || n = -0x20002

            if named || pseudo then
                true
            else

            match StrErrorR.answer library n 1024 with
            | StrErrorRAnswer.Buffer written ->
                let text = Encoding.ASCII.GetString (written.AsSpan (0, written.Length - 1))
                let m = (fallback library).Match text

                written.[written.Length - 1] = 0uy
                && m.Success
                && Int32.Parse m.Groups.[1].Value = n
            | _ -> false

        let gen =
            Gen.oneof [ ArbMap.defaults |> ArbMap.generate<int> ; Gen.choose (-500, 5000) ]

        for library in libraries do
            Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) (property library))

    /// What a shorter buffer gets is what a long one gets, cut to fit: the
    /// first `size - 1` bytes and a NUL, or nothing at all for size 0; and
    /// which of NULL and the buffer comes back depends only on whether the
    /// text fitted, for an error the library names, under Darwin's libc.
    [<Test>]
    let ``a short buffer gets the long buffer's text, cut to fit`` () : unit =
        let property (library : CLibrary) (n : int) (size : int) : bool =
            match StrErrorR.answer library n 4096 with
            | StrErrorRAnswer.LibraryText text ->
                // GNU's own string ignores the size.
                StrErrorR.answer library n size = StrErrorRAnswer.LibraryText text
            | StrErrorRAnswer.Buffer full
            | StrErrorRAnswer.NullAfterWriting full ->
                let text = full.AsSpan(0, full.Length - 1).ToArray ()

                let expectedWritten =
                    if size = 0 then
                        [||]
                    else
                        Array.append (Array.truncate (size - 1) text) [| 0uy |]

                let fits = size > text.Length

                let named = n = 0 || (CLibrary.errorOfNumber library n).IsSome

                match StrErrorR.answer library n size with
                | StrErrorRAnswer.NullAfterWriting written ->
                    library = CLibrary.DarwinLibc
                    && named
                    && not fits
                    && written.AsSpan().SequenceEqual (ReadOnlySpan expectedWritten)
                | StrErrorRAnswer.Buffer written ->
                    (library.IsGlibc || not named || fits)
                    && written.AsSpan().SequenceEqual (ReadOnlySpan expectedWritten)
                | _ -> false
            | StrErrorRAnswer.RefusedSize -> false

        let gen =
            gen {
                let! n = Gen.oneof [ Gen.choose (-300, 4200) ; Gen.elements extremes ]
                let! size = Gen.choose (0, 80)
                return n, size
            }

        for library in libraries do
            Check.One (
                Config.QuickThrowOnFailure.WithMaxTest 4000,
                Prop.forAll (Arb.fromGen gen) (fun (n, size) -> property library n size)
            )

    [<Test>]
    let ``a negative size is refused before anything else`` () : unit =
        let gen =
            gen {
                let! n = ArbMap.defaults |> ArbMap.generate<int>
                let! size = Gen.oneof [ Gen.choose (Int32.MinValue, -1) ; Gen.elements [ -1 ; Int32.MinValue ] ]
                return n, size
            }

        for library in libraries do
            Check.One (
                Config.QuickThrowOnFailure.WithMaxTest 1000,
                Prop.forAll
                    (Arb.fromGen gen)
                    (fun (n, size) -> StrErrorR.answer library n size = StrErrorRAnswer.RefusedSize)
            )

    [<Test>]
    let ``the C library follows the platform's flavour`` () : unit =
        // By default, glibc as the mainstream distributions build it.
        CLibrary.ofPlatform SimulatedUnixPlatform.linuxX64
        |> shouldEqual (CLibrary.Glibc GlibcErrnoSet.ThroughEHWPOISON)

        CLibrary.ofPlatform SimulatedUnixPlatform.linuxArm64
        |> shouldEqual (CLibrary.Glibc GlibcErrnoSet.ThroughEHWPOISON)

        CLibrary.ofPlatform SimulatedUnixPlatform.macOsArm64
        |> shouldEqual CLibrary.DarwinLibc

        for library in glibcBuilds do
            CLibrary.errnoNumbering library |> shouldEqual RawErrnoNumbering.Linux

        CLibrary.errnoNumbering CLibrary.DarwinLibc
        |> shouldEqual RawErrnoNumbering.Darwin

    [<Test>]
    let ``a configured C library reaches the kernel, and one of the other flavour is refused`` () : unit =
        (KernelConfig.toKernel KernelConfig.Default).CLibrary
        |> shouldEqual (CLibrary.Glibc GlibcErrnoSet.ThroughEHWPOISON)

        for library in glibcBuilds do
            (KernelConfig.toKernel
                { KernelConfig.Default with
                    CLibrary = Some library
                })
                .CLibrary
            |> shouldEqual library

        (KernelConfig.toKernel
            { KernelConfig.Default with
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
            })
            .CLibrary
        |> shouldEqual CLibrary.DarwinLibc

        let refused =
            Assert.Throws<exn> (fun () ->
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        CLibrary = Some CLibrary.DarwinLibc
                    }
                |> ignore
            )

        refused.Message |> shouldContainText "KernelConfig.CLibrary"

        let refused =
            Assert.Throws<exn> (fun () ->
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        UnixPlatform = SimulatedUnixPlatform.macOsArm64
                        CLibrary = Some (CLibrary.Glibc GlibcErrnoSet.ThroughEFTYPE)
                    }
                |> ignore
            )

        refused.Message |> shouldContainText "KernelConfig.CLibrary"

    // ---------------------------------------------------------------------
    // Against this host's own shim.
    // ---------------------------------------------------------------------

    [<DllImport("libSystem.Native", EntryPoint = "SystemNative_StrErrorR")>]
    extern nativeint private hostStrErrorR(int platformErrno, nativeint buffer, int bufferSize)

    let private observeHost (n : int) (size : int) : Observed =
        let buffer = Marshal.AllocHGlobal bufferLength

        try
            Marshal.Copy (Array.create bufferLength fill, 0, buffer, bufferLength)
            Marshal.SetLastSystemError 4242
            let ret = hostStrErrorR (n, buffer, size)
            let errno = Marshal.GetLastSystemError ()
            let after = Array.zeroCreate<byte> bufferLength
            Marshal.Copy (buffer, after, 0, bufferLength)

            {
                Return =
                    if ret = 0n then "NULL"
                    elif ret = buffer then "BUFFER"
                    else "OTHER"
                Buffer = after
                Other =
                    if ret = 0n || ret = buffer then
                        None
                    else
                        Some (Marshal.PtrToStringUTF8 ret)
                Errno = if errno = 4242 then "kept" else string<int> errno
            }
        finally
            Marshal.FreeHGlobal buffer

    /// The modelled C library this host's shim runs against, asked of the host
    /// (see `HostCLibrary.detect`): the glibc build is the one whose answer for
    /// errno 134 the host gives, and every other row is then compared against
    /// that build's model. A host that is neither build fails rather than skips.
    let private hostLibrary () : CLibrary option = HostCLibrary.detect ()

    /// The probe's whole sweep, repeated against whatever shim this host has:
    /// the Darwin half on a dev box, the Linux half in CI.
    [<Test>]
    let ``every probe row is what this host's shim answers`` () : unit =
        match hostLibrary () with
        | None -> Assert.Ignore $"no modelled Unix to measure (%s{RuntimeInformation.OSDescription})"
        | Some library ->

        let host = probeRows observeHost
        let modelled = probeRows (modelObserve library)

        for expected, actual in List.zip host modelled do
            if expected <> actual then
                failwith
                    $"TestStrErrorR: under %O{library}, this host's shim prints\n%s{expected}\nbut the model gives\n%s{actual}"

    /// Arbitrary numbers and sizes, compared byte for byte against this host's
    /// shim, past the probe's sweep.
    [<Test>]
    let ``the model is this host's shim, for arbitrary numbers and sizes`` () : unit =
        match hostLibrary () with
        | None -> Assert.Ignore $"no modelled Unix to measure (%s{RuntimeInformation.OSDescription})"
        | Some library ->

        let gen =
            gen {
                let! n =
                    Gen.oneof
                        [
                            ArbMap.defaults |> ArbMap.generate<int>
                            Gen.choose (-500, 5000)
                            Gen.choose (0, 140)
                            Gen.elements extremes
                        ]

                let! size = Gen.oneof [ Gen.choose (-3, 100) ; Gen.choose (Int32.MinValue, 1024) ]
                return n, size
            }

        let property (n : int, size : int) : bool =
            let host = observeHost n size
            let model = modelObserve library n size

            if host = model then
                true
            else
                failwith
                    $"TestStrErrorR: under %O{library}, StrErrorR(%d{n}, _, %d{size}) on this host is %A{host}, the model %A{model}"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 3000, Prop.forAll (Arb.fromGen gen) property)
