namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `PATH_MAX` is the one length rule the resolver can never see: it binds the
/// pathname *as the caller passed it*, in bytes, before any parsing. `UnixPath`
/// has already lost that — it collapses repeated separators, records a trailing
/// one as a flag, and counts UTF-16 — so the rule lives at the syscall boundary,
/// in `PathArgument.parse`, and is tested here against that function directly
/// rather than through a resolution.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPathArgument =

    let private darwin : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.macOsArm64

    let private linux : PathLimits =
        SimulatedUnixPlatform.pathLimits SimulatedUnixPlatform.linuxX64

    /// The bytes a caller's `char*` would hold, without the NUL: a caller that
    /// stopped at the NUL has already dropped it, which is why the limit (which
    /// counts it) is compared against `pathMaxBytes - 1`.
    let private bytesOf (s : string) : ImmutableArray<byte> =
        UnixPathText.utf8.GetBytes s |> ImmutableArray.CreateRange

    /// A path of exactly `n` bytes that would resolve if it were short enough:
    /// "./" repeated, so every component is a "." and none can hit NAME_MAX.
    let private ofLength (n : int) : string =
        let unit = "./"
        let s = String.replicate ((n / 2) + 1) unit
        s.Substring (0, n)

    [<Test>]
    let ``the length probe really produces the length it claims`` () : unit =
        // Guards the guard: every assertion below is about a byte count, so a
        // helper that quietly produced 1022 bytes when asked for 1023 would make
        // the boundary tests pass against the wrong boundary.
        for n in [ 255 ; 1023 ; 1024 ; 4095 ; 4096 ] do
            (ofLength n |> bytesOf).Length |> shouldEqual n

    [<Test>]
    let ``PATH_MAX binds the argument at each platform's own boundary`` () : unit =
        // Measured: on macOS an argument of 1023 bytes resolves and 1024 is
        // ENAMETOOLONG; on Linux 4095 resolves and 4096 is ENAMETOOLONG. The
        // limit counts the NUL, which is why the usable length is one less than
        // the number in the header.
        let ok (limits : PathLimits) (n : int) : unit =
            match PathArgument.parse limits (bytesOf (ofLength n)) with
            | Ok (PathArgument.Parsed _) -> ()
            | other -> failwith $"a %d{n}-byte path was not parsed: %O{other}"

        let tooLong (limits : PathLimits) (n : int) : unit =
            PathArgument.parse limits (bytesOf (ofLength n))
            |> shouldEqual (Ok (PathArgument.Failed UnixError.ENAMETOOLONG))

        ok darwin 1023
        tooLong darwin 1024

        ok linux 4095
        tooLong linux 4096

        // ...and the platforms genuinely differ in between, which is the whole
        // reason this is derived from the flavour rather than a constant.
        ok linux 1024
        tooLong darwin 4095

    [<Test>]
    let ``the limit is bytes, not characters`` () : unit =
        // 512 two-byte characters are 1024 bytes but only 512 UTF-16 units, so a
        // `String.Length` implementation would accept this on Darwin.
        let multiByte = String.replicate 512 "é"
        (bytesOf multiByte).Length |> shouldEqual 1024

        PathArgument.parse darwin (bytesOf multiByte)
        |> shouldEqual (Ok (PathArgument.Failed UnixError.ENAMETOOLONG))

    [<Test>]
    let ``an over-long path is refused before its bytes are decoded`` () : unit =
        // Ordering: a real kernel checks the length in `getname`/`copyinstr` as
        // it copies the string in, long before anything interprets it. A path
        // this kernel cannot represent has no answer at all — so if the decode
        // ran first, a path a real kernel rejects cheaply would instead be a
        // refusal.
        let invalid =
            ImmutableArray.CreateRange (Seq.append (bytesOf (ofLength 2000)) [ 0xFFuy ])

        PathArgument.parse darwin invalid
        |> shouldEqual (Ok (PathArgument.Failed UnixError.ENAMETOOLONG))

        // The same bytes under a kernel whose PATH_MAX they fit *do* reach the
        // decode, and that is the refusal — which is what proves the check above
        // is doing the work rather than the input being harmless.
        PathArgument.parse linux invalid
        |> shouldEqual (Error PathArgumentRefusal.NotUtf8)

    [<Test>]
    let ``a lone invalid byte is refused rather than substituted`` () : unit =
        // The reason the decode is strict: U+FFFD would name a file literally
        // called "�", which a caller could have seeded, so a lenient decode
        // answers confidently about the wrong inode.
        PathArgument.parse linux (ImmutableArray.CreateRange [ 0x2Fuy ; 0xFFuy ])
        |> shouldEqual (Error PathArgumentRefusal.NotUtf8)

    [<Test>]
    let ``a path within the limit parses to what it says`` () : unit =
        match PathArgument.parse linux (bytesOf "/etc/hostname") with
        | Ok (PathArgument.Parsed path) -> UnixPath.toString path |> shouldEqual "/etc/hostname"
        | other -> failwith $"expected a parse, got %O{other}"

    [<Test>]
    let ``a forged limit is rejected rather than making every path too long`` () : unit =
        // The failure worth catching is not a crash: a defaulted `PathLimits`
        // has a PATH_MAX of zero, under which every path is over-long, so a
        // caller would get a confident ENAMETOOLONG for every path a guest ever
        // names.
        let exn =
            Assert.Throws<Exception> (fun () ->
                PathArgument.parse Unchecked.defaultof<PathLimits> (bytesOf "/etc")
                |> ignore<Result<PathArgument, PathArgumentRefusal>>
            )

        exn.Message |> shouldContainText "PathArgument.parse"

    [<Test>]
    let ``a defaulted byte array is rejected rather than read`` () : unit =
        // `default(ImmutableArray<byte>)` wraps a null array, so the length read
        // below would throw a bare NullReferenceException from inside the
        // parser. It is also not the same as an empty path, which the row below
        // shows is a legitimate argument.
        let exn =
            Assert.Throws<Exception> (fun () ->
                PathArgument.parse linux Unchecked.defaultof<ImmutableArray<byte>>
                |> ignore<Result<PathArgument, PathArgumentRefusal>>
            )

        exn.Message |> shouldContainText "ImmutableArray<byte>.Empty"

    [<Test>]
    let ``an empty argument parses rather than refusing`` () : unit =
        // `open("")` is ENOENT, which is an answer about resolution rather than
        // about the bytes, so this stage must let it through: refusing here
        // would turn a guest's ordinary mistake into a crash.
        match PathArgument.parse linux ImmutableArray<byte>.Empty with
        | Ok (PathArgument.Parsed path) -> UnixPath.isEmpty path |> shouldEqual true
        | other -> failwith $"expected an empty path, got %O{other}"
