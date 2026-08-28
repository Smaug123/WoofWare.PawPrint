namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Text.RegularExpressions
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `SocketEventsPal` transcribes three upstream functions, so nothing in the
/// type system keeps its numbers right. Its oracle is upstream rather than the
/// library: the five `SocketEvents` values are re-derived here from the pinned
/// `pal_networking.h`, and the library has no opinion about them at all — it
/// holds epoll's conditions, and never sees .NET's encoding of them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSocketEventsPal =

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
                $"TestSocketEventsPal: expected the pinned PAL networking source at %s{path}. If the sparse checkout in flake.nix no longer includes src/native/libs/System.Native, this transcription has lost its oracle."

        path

    /// `SocketEvents_SA_READ = 0x01,` and friends.
    let private palEntry : Regex =
        Regex (@"^\s+SocketEvents_(?<name>SA_[A-Z]+)\s*=\s*0x(?<value>[0-9A-Fa-f]+),", RegexOptions.Multiline)

    let private pinnedSocketEvents () : Map<string, int> =
        let text = File.ReadAllText (palPath "pal_networking.h")

        let values =
            palEntry.Matches text
            |> Seq.map (fun m -> m.Groups.["name"].Value, Convert.ToInt32 (m.Groups.["value"].Value, 16))
            |> Map.ofSeq

        // `SA_NONE` is in the enum too, so six rather than five. Its absence
        // would mean the regex had drifted rather than that upstream had.
        if values.Count <> 6 then
            failwith
                $"TestSocketEventsPal: read %d{values.Count} SocketEvents values from the pinned pal_networking.h, expected 6 (SA_NONE and the five conditions). The enum's shape has changed; teach this test to read it."

        values

    let private pinned (name : string) : int =
        match Map.tryFind name (pinnedSocketEvents ()) with
        | Some value -> value
        | None ->
            failwith
                $"TestSocketEventsPal: the pinned pal_networking.h has no SocketEvents_%s{name}. The enum has been renamed or reordered upstream."

    // ---------------------------------------------------------------------
    // The alphabet itself.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``the five condition bits are upstream's`` () : unit =
        pinned "SA_NONE" |> shouldEqual 0
        pinned "SA_READ" |> shouldEqual 0x01
        pinned "SA_WRITE" |> shouldEqual 0x02
        pinned "SA_READCLOSE" |> shouldEqual 0x04
        pinned "SA_CLOSE" |> shouldEqual 0x08
        pinned "SA_ERROR" |> shouldEqual 0x10

    /// The wrapper's screen is `SupportedEvents`, which upstream spells as the
    /// OR of exactly these five. Checked as that OR rather than as `0x1F`, so
    /// that a `supported` narrowed to some other constant cannot agree with a
    /// literal copied out of it.
    [<Test>]
    let ``supported is upstream's SupportedEvents`` () : unit =
        let expected =
            pinned "SA_READ"
            ||| pinned "SA_WRITE"
            ||| pinned "SA_READCLOSE"
            ||| pinned "SA_CLOSE"
            ||| pinned "SA_ERROR"

        SocketEventsPal.supported |> shouldEqual expected

        // And that upstream really names those five in the screen, rather than
        // some subset that happens to OR to the same number today.
        let source = File.ReadAllText (palPath "pal_networking.c")

        let declaration =
            Regex.Match (source, @"const int32_t SupportedEvents = (?<rhs>[^;]+);")

        if not declaration.Success then
            failwith
                "TestSocketEventsPal: the pinned pal_networking.c no longer declares `const int32_t SupportedEvents`, so the screen has lost its oracle."

        let named =
            Regex.Matches (declaration.Groups.["rhs"].Value, @"SocketEvents_(SA_[A-Z]+)")
            |> Seq.map (fun m -> m.Groups.[1].Value)
            |> Set.ofSeq

        named
        |> shouldEqual (Set.ofList [ "SA_READ" ; "SA_WRITE" ; "SA_READCLOSE" ; "SA_CLOSE" ; "SA_ERROR" ])

    // ---------------------------------------------------------------------
    // `GetSocketEvents`, which has all five rows.
    // ---------------------------------------------------------------------

    /// Every `ReadinessLevel` there is: five booleans, so 32 of them. Small
    /// enough to enumerate, which beats sampling for a bit-for-bit table.
    let private allLevels : ReadinessLevel list =
        [
            for bits in 0..31 ->
                {
                    In = bits &&& 0x01 <> 0
                    Out = bits &&& 0x02 <> 0
                    RdHup = bits &&& 0x04 <> 0
                    Hup = bits &&& 0x08 <> 0
                    Err = bits &&& 0x10 <> 0
                }
        ]

    [<Test>]
    let ``ofReadiness is upstream's GetSocketEvents on every level`` () : unit =
        for level in allLevels do
            let expected =
                (if level.In then pinned "SA_READ" else 0)
                ||| (if level.Out then pinned "SA_WRITE" else 0)
                ||| (if level.RdHup then pinned "SA_READCLOSE" else 0)
                ||| (if level.Hup then pinned "SA_CLOSE" else 0)
                ||| (if level.Err then pinned "SA_ERROR" else 0)

            SocketEventsPal.ofReadiness level |> shouldEqual expected

    // ---------------------------------------------------------------------
    // `ConvertEventEPollToSocketAsync`, which folds before converting.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``delivery folds HUP into READ and WRITE`` () : unit =
        for level in allLevels do
            let folded =
                if level.Hup then
                    { level with
                        Hup = false
                        In = true
                        Out = true
                    }
                else
                    level

            SocketEventsPal.delivered level
            |> shouldEqual (SocketEventsPal.ofReadiness folded)

    /// The consequence of that fold, and the reason a guest never sees
    /// `SA_CLOSE` on Linux however the socket is registered.
    [<Test>]
    let ``no level delivers SA_CLOSE`` () : unit =
        let saClose = pinned "SA_CLOSE"

        for level in allLevels do
            SocketEventsPal.delivered level &&& saClose |> shouldEqual 0

    /// An idle stream socket's level, which is the row that makes the fold
    /// visible rather than merely stated: `OUT|HUP` is not `SA_WRITE`.
    [<Test>]
    let ``an idle socket's OUT and HUP deliver as READ and WRITE`` () : unit =
        { ReadinessLevel.none with
            Out = true
            Hup = true
        }
        |> SocketEventsPal.delivered
        |> shouldEqual (pinned "SA_READ" ||| pinned "SA_WRITE")

    // ---------------------------------------------------------------------
    // `GetEPollEvents`, less the two bits `epoll_ctl` does not keep.
    // ---------------------------------------------------------------------

    [<Test>]
    let ``each maskable bit sets its own field and no other`` () : unit =
        SocketEventsPal.toInterest "test" (pinned "SA_READ")
        |> shouldEqual
            {
                SocketEventInterest.In = true
                Out = false
                RdHup = false
            }

        SocketEventsPal.toInterest "test" (pinned "SA_WRITE")
        |> shouldEqual
            {
                SocketEventInterest.In = false
                Out = true
                RdHup = false
            }

        SocketEventsPal.toInterest "test" (pinned "SA_READCLOSE")
        |> shouldEqual
            {
                SocketEventInterest.In = false
                Out = false
                RdHup = true
            }

    /// The lossy half, and the whole reason the library's record has three
    /// fields: `epoll_ctl` forces `EPOLLERR|EPOLLHUP` into every stored mask,
    /// so asking for them registers exactly what not asking does (measured on
    /// Linux 6.18.5 through `/proc/self/fdinfo`,
    /// `docs/plans/2026-08-23-posix-kernel-extraction/fdinfo.c`).
    [<Test>]
    let ``asking for CLOSE and ERROR is the same registration as not asking`` () : unit =
        let unmaskable = pinned "SA_CLOSE" ||| pinned "SA_ERROR"

        SocketEventsPal.toInterest "test" unmaskable
        |> shouldEqual (SocketEventsPal.toInterest "test" 0)

        SocketEventsPal.toInterest "test" (pinned "SA_READ" ||| unmaskable)
        |> shouldEqual (SocketEventsPal.toInterest "test" (pinned "SA_READ"))

    /// 0x20 is written out rather than derived from `supported`: a `supported`
    /// widened by mutation must not be able to make this row pass by moving
    /// the boundary the row is testing.
    [<Test>]
    let ``a mask outside the five bits is refused`` () : unit =
        let exn =
            Assert.Throws<Exception> (fun () -> SocketEventsPal.toInterest "test" 0x20 |> ignore<SocketEventInterest>)

        exn.Message |> shouldContainText "the wrapper's EINVAL screen"

        Assert.Throws<Exception> (fun () -> SocketEventsPal.toInterest "test" 0x100 |> ignore<SocketEventInterest>)
        |> ignore<Exception>

        // The whole supported set is not refused, which is what stops the row
        // above from passing for the wrong reason.
        SocketEventsPal.toInterest "test" 0x1F
        |> shouldEqual
            {
                SocketEventInterest.In = true
                Out = true
                RdHup = true
            }
