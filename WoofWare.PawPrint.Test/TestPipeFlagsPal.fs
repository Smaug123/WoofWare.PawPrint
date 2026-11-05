namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `PipeFlagsPal` transcribes `SystemNative_Pipe`'s flag screen, which differs between runtimes.
/// Its oracle is each runtime's pinned `pal_io.c` and `pal_io.h`: the screen and the PAL values
/// are re-read from them, and the platform's `O_CLOEXEC` is `OpenFlagsPal`'s, which
/// `TestOpenFlagsPal` holds to each platform's headers.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPipeFlagsPal =

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private pinnedFile (runtime : EmulatedRuntime) (leaf : string) : string =
        let path =
            Path.Combine (
                FrameworkUnderTest.requireRuntimeSourceOf runtime,
                "src",
                "native",
                "libs",
                "System.Native",
                leaf
            )

        if not (File.Exists path) then
            failwith
                $"TestPipeFlagsPal: expected %O{runtime}'s pinned source at %s{path}. If the sparse checkout in flake.nix no longer includes it, PipeFlagsPal has lost its oracle."

        File.ReadAllText path

    /// Each `PAL_O_*` value the pinned `pal_io.h` defines, by the name after `PAL_O_`.
    let private palValues (runtime : EmulatedRuntime) : Map<string, int> =
        Regex.Matches (
            pinnedFile runtime "pal_io.h",
            @"^\s+PAL_O_(?<name>[A-Z_]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),",
            RegexOptions.Multiline
        )
        |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
        |> Map.ofSeq

    /// The body of the pinned `SystemNative_Pipe`, up to the brace that closes it in the first column.
    let private pipeBody (runtime : EmulatedRuntime) : string =
        let source = pinnedFile runtime "pal_io.c"
        let header = "\nint32_t SystemNative_Pipe(int32_t pipeFds[2], int32_t flags)\n{"
        let start = source.IndexOf (header, StringComparison.Ordinal)

        if start < 0 then
            failwith $"TestPipeFlagsPal: %O{runtime}'s pinned pal_io.c has no definition of SystemNative_Pipe"

        let finish = source.IndexOf ("\n}\n", start, StringComparison.Ordinal)
        source.Substring (start, finish - start)

    /// The two shapes of screen the shims have taken.
    type private Screen =
        /// A `switch (flags)` whose `case` labels are the words admitted, every other word reaching
        /// `default`'s EINVAL.
        | Exact of Set<int>
        /// `if ((flags & ~(...)) != 0)`'s EINVAL: every word within the mask is admitted.
        | Mask of int

    /// The screen at the head of the pinned `SystemNative_Pipe`, read from the C.
    let private screen (runtime : EmulatedRuntime) : Screen =
        let body = pipeBody runtime
        let values = palValues runtime

        let value (token : string) : int =
            match Int32.TryParse token with
            | true, literal -> literal
            | false, _ ->
                if not (token.StartsWith ("PAL_O_", StringComparison.Ordinal)) then
                    failwith $"TestPipeFlagsPal: %O{runtime}'s screen names %s{token}, which is no PAL_O_ value"

                match Map.tryFind (token.Substring "PAL_O_".Length) values with
                | Some v -> v
                | None -> failwith $"TestPipeFlagsPal: %O{runtime}'s pal_io.h does not define %s{token}"

        let mask =
            Regex.Match (
                body,
                @"if \(\(flags & ~\((?<names>[^)]*)\)\) != 0\)\s*\{\s*assert_msg[^;]*;\s*errno = EINVAL;"
            )

        let switch =
            Regex.Match (
                body,
                @"switch \(flags\)\s*\{(?<arms>.*?)default:\s*assert_msg[^;]*;\s*errno = EINVAL;",
                RegexOptions.Singleline
            )

        match mask.Success, switch.Success with
        | true, false ->
            mask.Groups.["names"].Value.Split '|'
            |> Array.map (fun name -> value (name.Trim ()))
            |> Array.fold (|||) 0
            |> Screen.Mask
        | false, true ->
            Regex.Matches (switch.Groups.["arms"].Value, @"case (?<label>\w+):")
            |> Seq.map (fun m -> value m.Groups.["label"].Value)
            |> Set.ofSeq
            |> Screen.Exact
        | _ ->
            failwith
                $"TestPipeFlagsPal: %O{runtime}'s SystemNative_Pipe screens its flags in neither shape this test reads; re-read the shim and teach the test its new screen:\n%s{pipeBody runtime}"

    let private admits (screen : Screen) (flags : int) : bool =
        match screen with
        | Screen.Exact words -> Set.contains flags words
        | Screen.Mask mask -> flags &&& ~~~mask = 0

    /// What the shim does for `flags` on `platform`, given the screen and PAL values read from the
    /// runtime's pinned source: `pipe2` with `O_CLOEXEC` if `PAL_O_CLOEXEC` is set, then `O_NONBLOCK`
    /// on the end each of `PAL_O_NONBLOCK_READ` and `PAL_O_NONBLOCK_WRITE` names, where the runtime
    /// defines them.
    let private reference
        (screen : Screen)
        (values : Map<string, int>)
        (platform : SimulatedUnixPlatform)
        (flags : int)
        : PipeRequest option
        =
        let has (name : string) : bool =
            match Map.tryFind name values with
            | Some v -> flags &&& v <> 0
            | None -> false

        if not (admits screen flags) then
            None
        else
            let closeOnExec =
                match OpenFlagsPal.decode platform OpenFlagsPal.CloseOnExec with
                | Some word -> word
                | None -> failwith "TestPipeFlagsPal: OpenFlagsPal refuses a bare PAL_O_CLOEXEC"

            Some
                {
                    Pipe2Flags = if has "CLOEXEC" then closeOnExec else 0
                    NonBlockingRead = has "NONBLOCK_READ"
                    NonBlockingWrite = has "NONBLOCK_WRITE"
                }

    [<Test>]
    let ``each runtime's screen has the shape it is known to have`` () : unit =
        screen EmulatedRuntime.Net10
        |> shouldEqual (Screen.Exact (Set.ofList [ 0 ; PipeFlagsPal.CloseOnExec ]))

        screen EmulatedRuntime.Net11
        |> shouldEqual (
            Screen.Mask (
                PipeFlagsPal.CloseOnExec
                ||| PipeFlagsPal.NonBlockingRead
                ||| PipeFlagsPal.NonBlockingWrite
            )
        )

    [<Test>]
    let ``each PAL value is the one the pinned header defines`` () : unit =
        for runtime in EmulatedRuntime.supported do
            palValues runtime |> Map.find "CLOEXEC" |> shouldEqual PipeFlagsPal.CloseOnExec

        let net11 = palValues EmulatedRuntime.Net11
        net11 |> Map.find "NONBLOCK_READ" |> shouldEqual PipeFlagsPal.NonBlockingRead
        net11 |> Map.find "NONBLOCK_WRITE" |> shouldEqual PipeFlagsPal.NonBlockingWrite

    /// The shim's nonblocking step, which the handler reproduces: after the pipe is made, the read
    /// end (`pipeFds[0]`) for `PAL_O_NONBLOCK_READ`, then the write end for `PAL_O_NONBLOCK_WRITE`
    /// only if the first succeeded, and both ends closed if either failed.
    [<Test>]
    let ``.NET 11 sets O_NONBLOCK on the end each flag names`` () : unit =
        let body = pipeBody EmulatedRuntime.Net11

        body
        |> shouldContainText
            "if ((flags & PAL_O_NONBLOCK_READ) != 0)\n        {\n            result = SystemNative_FcntlSetIsNonBlocking((intptr_t)pipeFds[0], 1);"

        body
        |> shouldContainText
            "if (result == 0 && (flags & PAL_O_NONBLOCK_WRITE) != 0)\n        {\n            result = SystemNative_FcntlSetIsNonBlocking((intptr_t)pipeFds[1], 1);"

        body
        |> shouldContainText
            "if (result != 0)\n        {\n            int tmpErrno = errno;\n            close(pipeFds[0]);\n            close(pipeFds[1]);\n            errno = tmpErrno;"

    [<Test>]
    let ``every flags word up to 0x10000 decodes as the shim screens it`` () : unit =
        for runtime in EmulatedRuntime.supported do
            let screen = screen runtime
            let values = palValues runtime

            for platform in platforms do
                for flags in 0..0xFFFF do
                    (runtime, platform, flags, PipeFlagsPal.decode runtime platform flags)
                    |> shouldEqual (runtime, platform, flags, reference screen values platform flags)

    [<Test>]
    let ``every flags word decodes as the shim screens it`` () : unit =
        // A word of the admitted bits, with sometimes one other bit set anywhere in the word.
        let flags : Gen<int> =
            gen {
                let! admitted = Gen.elements [ 0 ; 0x10 ; 0x400 ; 0x800 ; 0x410 ; 0x810 ; 0xC00 ; 0xC10 ]
                let! stray = Gen.oneof [ Gen.constant 0 ; Gen.choose (0, 31) |> Gen.map (fun bit -> 1 <<< bit) ]
                let! anything = Gen.choose (Int32.MinValue, Int32.MaxValue)
                return! Gen.elements [ admitted ||| stray ; anything ]
            }

        for runtime in EmulatedRuntime.supported do
            let screen = screen runtime
            let values = palValues runtime

            for platform in platforms do
                let property =
                    Prop.forAll
                        (Arb.fromGen flags)
                        (fun flags ->
                            PipeFlagsPal.decode runtime platform flags = reference screen values platform flags
                            |> Prop.label $"%O{runtime} %O{platform} 0x%x{flags}"
                        )

                Check.One (Config.QuickThrowOnFailure.WithMaxTest 10000, property)
