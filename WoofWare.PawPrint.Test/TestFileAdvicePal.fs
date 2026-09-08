namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `FileAdvicePal` transcribes an upstream enum and the screen the shim applies
/// over it, so nothing in the type system keeps its numbers right. Its oracle is
/// upstream: the six values are re-derived here from the pinned `pal_io.h`, and
/// the two errnos the shim returns without reaching a kernel are re-read from
/// `pal_io.c`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFileAdvicePal =

    let private runtimeSrc : string option =
        match Environment.GetEnvironmentVariable "DOTNET_RUNTIME_SRC" with
        | null
        | "" -> None
        | dir -> Some dir

    /// The pinned runtime source only exists inside the Nix devshell, so a plain
    /// `dotnet test` in a non-Nix checkout skips rather than fails.
    let private requireRuntimeSrc () : string =
        match runtimeSrc with
        | Some dir -> dir
        | None ->
            Assert.Ignore
                "DOTNET_RUNTIME_SRC is unset; run under `nix develop` to check against pinned upstream sources."

            failwith "unreachable: Assert.Ignore did not throw"

    let private palPath (leaf : string) : string =
        let path =
            Path.Combine (requireRuntimeSrc (), "src", "native", "libs", "System.Native", leaf)

        if not (File.Exists path) then
            failwith
                $"TestFileAdvicePal: expected the pinned PAL io source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/System.Native, this transcription has lost its oracle."

        path

    /// `PAL_POSIX_FADV_NORMAL = 0,     /* ... */` and friends.
    let private palEntry : Regex =
        Regex (@"^\s+PAL_POSIX_FADV_(?<name>[A-Z]+)\s*=\s*(?<value>\d+),", RegexOptions.Multiline)

    let private pinnedAdvice () : Map<string, int> =
        let values =
            palEntry.Matches (File.ReadAllText (palPath "pal_io.h"))
            |> Seq.map (fun m -> m.Groups.["name"].Value, Int32.Parse m.Groups.["value"].Value)
            |> Map.ofSeq

        if values.Count <> 6 then
            failwith
                $"TestFileAdvicePal: read %d{values.Count} PAL_POSIX_FADV_ values from the pinned pal_io.h, expected 6. The enum's shape has changed; teach this test to read it."

        values

    let private pinned (name : string) : int =
        match Map.tryFind name (pinnedAdvice ()) with
        | Some value -> value
        | None ->
            failwith
                $"TestFileAdvicePal: the pinned pal_io.h has no PAL_POSIX_FADV_%s{name}. The enum has been renamed or reordered upstream."

    /// The advice each name denotes, as this library spells it.
    let private meaning : (string * FileAccessAdvice) list =
        [
            "NORMAL", FileAccessAdvice.Normal
            "RANDOM", FileAccessAdvice.Random
            "SEQUENTIAL", FileAccessAdvice.Sequential
            "WILLNEED", FileAccessAdvice.WillNeed
            "DONTNEED", FileAccessAdvice.DontNeed
            "NOREUSE", FileAccessAdvice.NoReuse
        ]

    [<Test>]
    let ``every advice decodes to the value upstream numbers it`` () : unit =
        for name, advice in meaning do
            FileAdvicePal.decode (pinned name) |> shouldEqual (Some advice)

    [<Test>]
    let ``the six numbers are distinct and contiguous from zero`` () : unit =
        // A transcription that collapsed two names onto one number would still
        // satisfy the row above for whichever name it kept.
        meaning |> List.map (fst >> pinned) |> shouldEqual [ 0 ; 1 ; 2 ; 3 ; 4 ; 5 ]

    [<Test>]
    let ``nothing outside the six decodes`` () : unit =
        let known = meaning |> List.map (fst >> pinned) |> Set.ofList

        for candidate in [ -1 ; 6 ; 7 ; 100 ; Int32.MinValue ; Int32.MaxValue ] do
            FileAdvicePal.decode candidate |> shouldEqual None

        // And nothing in the neighbourhood of the accepted range either, so a
        // decode that fell through to a default could not hide in the gaps.
        for candidate in -64 .. 64 do
            if Set.contains candidate known then
                FileAdvicePal.decode candidate |> shouldNotEqual None
            else
                FileAdvicePal.decode candidate |> shouldEqual None

    /// The two answers the shim gives without reaching any kernel, transcribed
    /// into `NativeSystemNative`'s handler: EINVAL for an advice its `switch`
    /// does not know, and ENOTSUP where the body is compiled out entirely.
    [<Test>]
    let ``the shim's own two answers are still what upstream returns`` () : unit =
        let source = File.ReadAllText (palPath "pal_io.c")

        let body =
            Regex.Match (
                source,
                @"int32_t SystemNative_PosixFAdvise\([^)]*\)\s*\{(?<body>.*?)\n\}",
                RegexOptions.Singleline
            )

        if not body.Success then
            failwith
                "TestFileAdvicePal: the pinned pal_io.c no longer defines SystemNative_PosixFAdvise, so this transcription has lost its oracle."

        let text = body.Groups.["body"].Value

        // The `default:` arm of the advice `switch`.
        if not (Regex.IsMatch (text, @"default:\s*return EINVAL;")) then
            failwith
                $"TestFileAdvicePal: SystemNative_PosixFAdvise no longer answers EINVAL from its switch's default arm. It now reads:%s{Environment.NewLine}%s{text}"

        // The `#else` arm, taken where HAVE_POSIX_ADVISE is not defined.
        if not (Regex.IsMatch (text, @"#else.*return ENOTSUP;", RegexOptions.Singleline)) then
            failwith
                $"TestFileAdvicePal: SystemNative_PosixFAdvise no longer answers ENOTSUP where posix_fadvise is absent. It now reads:%s{Environment.NewLine}%s{text}"

        // Both are raw `<errno.h>` numbers rather than PAL `Error` values: the
        // shim returns them straight, and its managed declaration takes
        // `SetLastError = false`. A `ConvertErrorPlatformToPal` here would mean
        // the handler must convert too.
        if Regex.IsMatch (text, "ConvertErrorPlatformToPal") then
            failwith
                $"TestFileAdvicePal: SystemNative_PosixFAdvise now converts its errno into the PAL numbering, which PawPrint's handler does not. It now reads:%s{Environment.NewLine}%s{text}"

    /// CoreLib's own copy of the enum, which is what a guest's `FileAdvice`
    /// argument actually is.
    [<Test>]
    let ``CoreLib's managed enum agrees with the header`` () : unit =
        let path =
            Path.Combine (
                requireRuntimeSrc (),
                "src",
                "libraries",
                "Common",
                "src",
                "Interop",
                "Unix",
                "System.Native",
                "Interop.PosixFAdvise.cs"
            )

        if not (File.Exists path) then
            failwith
                $"TestFileAdvicePal: expected CoreLib's FileAdvice declaration at %s{path}. If the sparse checkout in flake.nix no longer includes src/libraries/Common, this row has lost its oracle."

        let managed =
            Regex.Matches (File.ReadAllText path, @"POSIX_FADV_(?<name>[A-Z]+)\s*=\s*(?<value>\d+)")
            |> Seq.map (fun m -> m.Groups.["name"].Value, Int32.Parse m.Groups.["value"].Value)
            |> Map.ofSeq

        managed |> shouldEqual (pinnedAdvice ())
