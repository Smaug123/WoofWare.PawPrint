namespace WoofWare.PosixKernel.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `umask(2)`: which bits of its argument each flavour keeps, what the next
/// call answers, and how the kept mask applies when `open(O_CREAT)` and
/// `mkdir(2)` create.
///
/// The rows come from `docs/plans/2026-08-23-posix-kernel-extraction/umask-width.c`,
/// run on Linux 6.18.5 (aarch64, gcc:14, as root and as uid 1000) and on Darwin
/// 27.0 at uid 501; its output is beside it. Its sweeps are replayed here
/// exhaustively against the rule it compared the kernels with, and its
/// individually measured rows as literals.
///
/// No test here asks the host: the mask is process-global, so setting it in the
/// test host would race every test that creates a file.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUmask =

    let private context : string = "TestUmask"

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 1000

    let private mode (bits : int) : PermissionBits = PermissionBits.parseOrFail context bits

    let private platforms : SimulatedUnixPlatform list =
        [
            SimulatedUnixPlatform.linuxX64
            SimulatedUnixPlatform.linuxArm64
            SimulatedUnixPlatform.macOsArm64
        ]

    let private fresh (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    /// The bits the probe found each flavour keeps: `stored = argument & width`,
    /// with 0 mismatches over every 12-bit argument by both routes, and every
    /// bit from 12 to 31 ignored.
    let private measuredWidth (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 0o777
        | SimulatedUnixFlavour.Darwin -> 0o7777

    /// Call `umask` with each of `masks` in turn, answering what each call
    /// answered and the system after the last.
    let private chain (masks : int list) (system : UnixSystem<int, string>) : int list * UnixSystem<int, string> =
        let answers, system =
            masks
            |> List.fold
                (fun (answers, system) mask ->
                    let previous, system = UnixSystem.umask mask system
                    PermissionBits.toInt previous :: answers, system
                )
                ([], system)

        List.rev answers, system

    /// What `umask(mask)` leaves stored, read back as the probe read it: by a
    /// second call.
    let private readBack (platform : SimulatedUnixPlatform) (mask : int) : int =
        match chain [ mask ; 0 ] (fresh platform) with
        | [ _ ; back ], _ -> back
        | other, _ -> failwith $"two calls answered %A{other}"

    // ------------------------------------------------------------------ the stored width

    [<Test>]
    let ``each flavour keeps the bits the probe measured`` () : unit =
        for platform in platforms do
            SimulatedUnixPlatform.umaskStoredBits platform
            |> shouldEqual (mode (measuredWidth platform))

    [<Test>]
    let ``every twelve-bit argument is stored at the flavour's width`` () : unit =
        // The probe's SWEEP12 rows: 4096 arguments, 0 mismatches against
        // `argument & 0777` on Linux and `argument & 07777` on Darwin.
        for platform in platforms do
            for mask in 0..0o7777 do
                let back = readBack platform mask

                if back <> (mask &&& measuredWidth platform) then
                    failwith $"%O{platform}: umask(0o%04o{mask}) read back 0o%04o{back}"

    [<Test>]
    let ``every bit above the permission word is ignored`` () : unit =
        // The probe's HIGH rows: each of bits 12 to 31, alone and over four
        // low words, read back as the low word at the flavour's width.
        for platform in platforms do
            for low in [ 0 ; 0o7777 ; 0o022 ; 0o777 ] do
                for bit in 12..31 do
                    let mask = (1 <<< bit) ||| low

                    readBack platform mask |> shouldEqual (low &&& measuredWidth platform)

    [<Test>]
    let ``the probe's extreme arguments read back as it measured`` () : unit =
        // EXTREME, transcribed: the argument as the 32 bits the kernel took,
        // and what each flavour read back.
        let rows =
            [
                0xffffffff, 0o777, 0o7777
                0x80000000, 0o000, 0o0000
                0x7fffffff, 0o777, 0o7777
                0xfffff000, 0o000, 0o0000
                0x0000ffff, 0o777, 0o7777
                0x00010000 ||| 0o022, 0o022, 0o0022
            ]

        for mask, linux, darwin in rows do
            readBack SimulatedUnixPlatform.linuxX64 mask |> shouldEqual linux
            readBack SimulatedUnixPlatform.macOsArm64 mask |> shouldEqual darwin

    [<Test>]
    let ``a later call answers the earlier one's mask at the flavour's width`` () : unit =
        // ROW, transcribed. After high bits the next call answers only what was
        // kept; and 0o7777 over 0o777 is where the flavours part.
        for platform in platforms do
            chain [ 0 ; 0xfffff000 ||| 0o777 ; 0o022 ; 0 ] (fresh platform)
            |> fst
            |> shouldEqual [ 0o022 ; 0 ; 0o777 ; 0o022 ]

        chain [ 0o777 ; 0o7777 ; 0 ] (fresh SimulatedUnixPlatform.linuxX64)
        |> fst
        |> shouldEqual [ 0o022 ; 0o777 ; 0o777 ]

        chain [ 0o777 ; 0o7777 ; 0 ] (fresh SimulatedUnixPlatform.macOsArm64)
        |> fst
        |> shouldEqual [ 0o022 ; 0o777 ; 0o7777 ]

    [<Test>]
    let ``a fresh process has the mask the probe inherited`` () : unit =
        // `inherited 0022` on both.
        for platform in platforms do
            readBack platform 0o022 |> shouldEqual 0o022
            chain [ 0 ] (fresh platform) |> fst |> shouldEqual [ 0o022 ]

    /// A `umask` argument with the bits above the permission word often set.
    let private maskGen : Gen<int> =
        Gen.oneof
            [
                Gen.choose (0, 0o7777)
                ArbMap.defaults |> ArbMap.generate<int>
                Gen.elements [ 0 ; -1 ; System.Int32.MinValue ; System.Int32.MaxValue ; 0o777 ; 0o7777 ]
            ]

    [<Test>]
    let ``a chain of calls answers each previous mask, and changes nothing else`` () : unit =
        // CHAIN's rule, 20000 steps each on both kernels with 0 mismatches:
        // every call answers the previous argument at the flavour's width.
        let property (platformIndex : int, masks : int list) : unit =
            let platform = platforms.[platformIndex]
            let before = fresh platform
            let answers, after = chain masks before

            let expected =
                0o022 :: (masks |> List.map (fun mask -> mask &&& measuredWidth platform))

            answers |> shouldEqual (List.take (List.length masks) expected)

            let last = List.last expected

            after
            |> shouldEqual
                { before with
                    Process =
                        { before.Process with
                            Umask = mode last
                        }
                }

            UnixSystem.checkInvariants after |> shouldEqual []

        let gen =
            gen {
                let! platformIndex = Gen.choose (0, List.length platforms - 1)
                let! masks = Gen.listOf maskGen
                return platformIndex, masks
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``step answers umask as the function does`` () : unit =
        let property (platformIndex : int, earlier : int, mask : int) : unit =
            let _, system = UnixSystem.umask earlier (fresh platforms.[platformIndex])
            let previous, expected = UnixSystem.umask mask system

            match UnixSystem.step 0 (Syscall.UMask mask) system with
            | Ok (SyscallOutcome.Answered (SyscallAnswer.Completed answer), after) ->
                answer |> shouldEqual (int64 (PermissionBits.toInt previous))
                after |> shouldEqual expected
            | other -> failwith $"unexpected: %A{other}"

        let gen =
            gen {
                let! platformIndex = Gen.choose (0, List.length platforms - 1)
                let! earlier = maskGen
                let! mask = maskGen
                return platformIndex, earlier, mask
            }

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    // ------------------------------------------------------------------ configuring the mask

    [<Test>]
    let ``a configured mask is refused exactly when it holds a bit the flavour never stores`` () : unit =
        for platform in platforms do
            for bits in 0..0o7777 do
                let storable = bits &&& ~~~(measuredWidth platform) = 0

                if storable then
                    let system =
                        UnixSystem.initial<int, string> platform
                        |> Launched.bootWith (Launched.umask (mode bits)) UnixSystem.pipedStandardStreams 0 (CpuId 0)

                    system.Process.Umask |> shouldEqual (mode bits)
                    UnixSystem.checkInvariants system |> shouldEqual []
                else
                    Launched.launch platform UnixSystem.pipedStandardStreams 0 (CpuId 0)
                    |> ProcessLaunch.withUmask (mode bits)
                    |> Result.map ignore
                    |> shouldEqual (
                        Error (
                            UmaskRefusal.BitsNotStored (
                                mode bits,
                                mode (measuredWidth platform),
                                SimulatedUnixPlatform.flavour platform
                            )
                        )
                    )

    [<Test>]
    let ``checkInvariants reports a mask the flavour never stores`` () : unit =
        for platform in platforms do
            for bits in [ 0o4000 ; 0o2022 ; 0o1000 ; 0o7777 ] do
                let system = fresh platform

                let forged =
                    { system with
                        Process =
                            { system.Process with
                                Umask = mode bits
                            }
                    }

                let expected =
                    if bits &&& ~~~(measuredWidth platform) = 0 then
                        []
                    else
                        [ UnixSystemDefect.UmaskNotOfPlatform (mode bits, platform) ]

                UnixSystem.checkInvariants forged |> shouldEqual expected

    // ------------------------------------------------------------------ the mask applied

    let private creatingOpen : OpenFlags =
        {
            Access = FileAccessMode.WriteOnly
            Create = true
            Exclusive = true
            Truncate = false
            NoFollow = false
            CloseOnExec = false
            Synchronous = false
            DataSynchronous = false
            Directory = false
        }

    let private created (p : string) (system : UnixSystem<int, string>) : int =
        match
            UnixPathResolution.stat SymlinkPolicy.NoFollowFinal (PathArg.ofPath (UnixPath.parseOrFail context p)) system
        with
        | Ok (FileStatusAnswer.Reported status) -> status.Mode &&& 0o7777
        | other -> failwith $"%s{p}: %A{other}"

    /// The bits each syscall keeps from its mode before the mask, as the probe
    /// measured them at umask 0.
    let private openMask (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 0o7777
        | SimulatedUnixFlavour.Darwin -> 0o777

    let private mkdirMask (platform : SimulatedUnixPlatform) : int =
        match SimulatedUnixPlatform.flavour platform with
        | SimulatedUnixFlavour.Linux -> 0o1777
        | SimulatedUnixFlavour.Darwin -> 0o777

    [<Test>]
    let ``open and mkdir apply the mask umask stored, over every twelve-bit argument`` () : unit =
        // APPLY: every 12-bit umask argument under four modes, for `open` and
        // `mkdir`, 0 mismatches against `mode & SYSCALL_MASK & ~stored` on both
        // kernels (Linux as root and as uid 1000; this process is uid 1000, in
        // a plain directory it owns).
        let target = UnixPath.parseOrFail context "/e"

        for platform in platforms do
            for mask in 0..0o7777 do
                let _, system = UnixSystem.umask mask (fresh platform)
                let stored = mask &&& measuredWidth platform

                for requested in [ 0o7777 ; 0o666 ; 0o2775 ; 0o4755 ] do
                    let _, afterOpen = Answered.openPath creatingOpen target requested system
                    let opened = created "/e" afterOpen
                    let expectedOpen = requested &&& openMask platform &&& ~~~stored

                    if opened <> expectedOpen then
                        failwith
                            $"%O{platform}: umask(0o%04o{mask}), open(0o%04o{requested}) created 0o%04o{opened}, measured 0o%04o{expectedOpen}"

                    match Answered.mkdir (PathArg.ofPath target) requested system with
                    | SyscallAnswer.Completed _, afterMkdir ->
                        let made = created "/e" afterMkdir
                        let expectedMkdir = requested &&& mkdirMask platform &&& ~~~stored

                        if made <> expectedMkdir then
                            failwith
                                $"%O{platform}: umask(0o%04o{mask}), mkdir(0o%04o{requested}) created 0o%04o{made}, measured 0o%04o{expectedMkdir}"
                    | other -> failwith $"mkdir: %A{other}"

    [<Test>]
    let ``the mask is applied at its full stored width`` () : unit =
        // Darwin's `mkfifo` keeps all twelve bits of its mode, and there the
        // stored mask's high bits bite. Measured by the ownership plan's
        // `ownership-probe.c` on Darwin 27.0: under `umask 07777`,
        // `mkfifo(p, 07777)` and `mkfifo(p, 04775)` both create 0000, where
        // under `umask 0777` `mkfifo(p, 07777)` creates 07000. That is this
        // rule with a twelve-bit mode mask. This library models no `mkfifo`;
        // the rows pin that the mask is not narrowed to nine bits here.
        let everything = mode 0o7777

        PermissionBits.fromCreationMode everything (mode 0o7777) 0o7777
        |> shouldEqual (mode 0o0000)

        PermissionBits.fromCreationMode everything (mode 0o7777) 0o4775
        |> shouldEqual (mode 0o0000)

        PermissionBits.fromCreationMode everything (mode 0o0777) 0o7777
        |> shouldEqual (mode 0o7000)
