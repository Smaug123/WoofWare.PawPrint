namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PosixKernel

/// `KernelConfig` is everything a host may set about the simulated process
/// before a run, and `toKernel` is the only production path that writes those
/// fields onto a kernel. These are the rows about that layer itself — that its
/// defaults agree with the kernel's, that it reaches the field it names, and
/// that it validates rather than passing a bad value through — as opposed to
/// the rows about what any one field *means*, which belong with that field.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestKernelConfig =

    let private name (s : string) : DirectoryEntryName = DirectoryEntryName.parseOrFail "test" s

    let private absolute (s : string) : AbsoluteUnixPath = AbsoluteUnixPath.parseOrFail "test" s

    let private noBytes : ImmutableArray<byte> = ImmutableArray<byte>.Empty

    /// `outer/inner/` beside `outer/file`: enough for a row to set a current
    /// directory that the seed really contains.
    let private seed : Map<DirectoryEntryName, SeedEntry> =
        Map.ofList
            [
                name "outer",
                SeedEntry.directory (
                    Map.ofList
                        [
                            name "inner", SeedEntry.directory Map.empty
                            name "file", SeedEntry.file noBytes
                        ]
                )
            ]

    /// The inode a path names, resolved independently of the kernel — so a row
    /// asserting "the kernel held *this* inode" is checked against the graph
    /// rather than against the kernel's own answer.
    let private inodeOf (kernel : EmulatedKernel) (path : string) : InodeNumber =
        let vfs = kernel.FileSystem

        match
            PathWalk.resolveExisting
                (SimulatedUnixPlatform.pathLimits kernel.UnixPlatform)
                CallerPrivilege.Privileged
                (VirtualFileSystem.root vfs)
                SymlinkPolicy.Follow
                (UnixPath.parseOrFail "test" path)
                vfs
        with
        | Ok inode -> inode
        | Error error -> failwith $"could not resolve %s{path} in the test seed: %O{error}"

    [<Test>]
    let ``the instruction cost is configurable and validated`` () : unit =
        // The rate is guest-observable — a guest can measure it by counting work against
        // `Environment.TickCount64`, and it decides whether `SpinWait` reaches its blocking
        // rung — so it is part of the replay contract and belongs in `KernelConfig` rather than
        // being a constant a host cannot see.
        KernelConfig.Default.InstructionCostTicks
        |> shouldEqual EmulatedKernel.defaultInstructionCostTicks

        let configured =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    InstructionCostTicks = 10_000L
                }

        configured.InstructionCostTicks |> shouldEqual 10_000L

        // Zero would freeze the clock, so every guest waiting for time to pass would spin
        // forever: a hang rather than a wrong answer, and the sort of thing a host sweeping the
        // knob could reach by off-by-one. Rejected at the setter, like `ProcessorCount`.
        for bad in [ 0L ; -1L ] do
            let apply () =
                KernelConfig.toKernel
                    { KernelConfig.Default with
                        InstructionCostTicks = bad
                    }
                |> ignore<EmulatedKernel>

            Assert.Throws<Exception> (TestDelegate apply) |> ignore<Exception>

    /// The platform is the one field of `KernelConfig` that others are
    /// derived from, so a Darwin configuration must yield a kernel that is
    /// Darwin in every derived field, not one re-flavoured after the fact.
    /// Stated as the measured literals: a row that asked each derivation
    /// what to expect would agree with any derivation at all.
    [<Test>]
    let ``a Darwin configuration yields a kernel that is Darwin throughout`` () : unit =
        let kernel =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                }

        kernel.UnixPlatform |> shouldEqual SimulatedUnixPlatform.macOsArm64
        kernel.FileSystemType |> shouldEqual EmulatedFileSystemType.Apfs
        kernel.SoMaxConn |> shouldEqual 128
        kernel.EphemeralPortRange |> shouldEqual (49152us, 65535us)
        kernel.UserId |> shouldEqual 501u
        kernel.GroupId |> shouldEqual 20u
        EmulatedKernel.checkInvariants kernel |> shouldEqual []

        // ...and a configured value is carried as given, on either flavour.
        let configured =
            KernelConfig.toKernel
                { KernelConfig.Default with
                    UnixPlatform = SimulatedUnixPlatform.macOsArm64
                    EphemeralPortRange = Some (40000us, 40010us)
                    UserId = Some 1000u
                    GroupId = Some 1000u
                }

        configured.EphemeralPortRange |> shouldEqual (40000us, 40010us)
        configured.UserId |> shouldEqual 1000u
        configured.GroupId |> shouldEqual 1000u

        // The default configuration is Linux's, in every one of those fields.
        let linux = KernelConfig.toKernel KernelConfig.Default
        linux.UnixPlatform |> shouldEqual SimulatedUnixPlatform.linuxX64
        linux.EphemeralPortRange |> shouldEqual (32768us, 60999us)
        linux.UserId |> shouldEqual 1000u
        linux.GroupId |> shouldEqual 1000u

    [<Test>]
    let ``KernelConfig applies the current directory whatever else it sets`` () : unit =
        let config =
            { KernelConfig.Default with
                FileSystem = seed
                UnixPlatform = SimulatedUnixPlatform.macOsArm64
                CurrentDirectory = absolute "/outer/inner"
            }

        let kernel = KernelConfig.toKernel config

        kernel.CurrentDirectoryInode |> shouldEqual (inodeOf kernel "/outer/inner")
        EmulatedKernel.checkInvariants kernel |> shouldEqual []
