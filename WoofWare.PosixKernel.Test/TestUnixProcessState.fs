namespace WoofWare.PosixKernel.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// The process record, exercised directly rather than through a client.
///
/// Both type parameters are `SignalState`'s, and these rows instantiate them at
/// `int` and `string` — which is the point: naming a scheduling entity and
/// naming a signal handler are the client's business, and nothing here knows
/// what types a client uses for them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnixProcessState =

    let private rootInode : InodeNumber = InodeNumber 1L

    let private context : string = "TestUnixProcessState"

    /// A process holding nothing but its current directory: the least a client
    /// has to supply for any of these operations to mean something.
    let private empty : UnixProcessState<int, string> =
        {
            FileDescriptors = FileDescriptorRegistry.descriptorTable LaunchedStreams.registry
            Environment = []
            CurrentDirectoryInode = rootInode
            ProcessPath = None
            Credentials = Credentials.ofIds (UserId.parseOrFail context 1000u) (GroupId.parseOrFail context 1000u) []
            Umask = PermissionBits.parseOrFail context 0o022
            ProcessId = ProcessId.parseOrFail context 4242
            Signals = SignalState.initial SignalNumbering.Linux Set.empty
            CoreDumps = CoreDumps.Suppressed
        }

    /// A freshly made process, for the setters, which configure one before it
    /// boots.
    let private image : UnixBootImage<int, string> =
        UnixSystem.initial SimulatedUnixPlatform.linuxX64 UnixSystem.pipedStandardStreams 0 (CpuId 0)

    [<Test>]
    let ``the signal state is keyed by whatever the client names tasks`` () : unit =
        // The claim the two type parameters exist to make. `int` names a task and
        // `string` is a handler; a record that fixed either type to one client's
        // own would not compile here at all.
        let proc =
            { empty with
                Signals =
                    empty.Signals
                    |> HandlerFrames.enter "h" 7 (Set.ofList [ 7 ; 8 ]) 7 (Set.singleton Signal.SIGTERM)
            }

        SignalState.maskOf 7 proc.Signals
        |> Set.contains Signal.SIGTERM
        |> shouldEqual true

        SignalState.maskOf 8 proc.Signals
        |> Set.contains Signal.SIGTERM
        |> shouldEqual false

        SignalState.framesOf 7 proc.Signals
        |> List.map (fun frame -> frame.Action.Handler)
        |> shouldEqual [ "h" ]

    /// An environment entry: arbitrary non-NUL bytes, drawn from a small pool
    /// often enough that duplicates turn up, and including the shapes a
    /// `NAME=VALUE` reading would treat specially (no `=`, a leading `=`, empty).
    let private genEntry : Gen<UnixByteString> =
        let ofBytes (bytes : byte array) : UnixByteString =
            match UnixByteString.ofBytes (ImmutableArray.Create<byte> bytes) with
            | Ok s -> s
            | Error defect -> failwith $"generator produced a NUL: %s{UnixByteString.describe defect}"

        Gen.frequency
            [
                2,
                Gen.elements [ "A=1" ; "A=2" ; "A" ; "=A" ; "" ; "B==" ]
                |> Gen.map (fun s -> ofBytes (System.Text.Encoding.ASCII.GetBytes s))
                3,
                ArbMap.defaults
                |> ArbMap.generate<byte>
                |> Gen.filter (fun b -> b <> 0uy)
                |> Gen.listOf
                |> Gen.map (List.toArray >> ofBytes)
            ]

    [<Test>]
    let ``the environment is exactly the entries it was set to, in order`` () : unit =
        // Replacement, not an overlay: whatever the process held before is gone,
        // and nothing is merged, sorted or de-duplicated.
        let mutable withDuplicates = 0

        let property (before : UnixByteString list, after : UnixByteString list) : unit =
            let system =
                image
                |> UnixBootImage.withEnvironment context before
                |> UnixBootImage.withEnvironment context after
                |> UnixBootImage.boot

            system.Process.Environment |> shouldEqual after

            if List.length (List.distinct after) < List.length after then
                withDuplicates <- withDuplicates + 1

        let gen = Gen.zip (Gen.listOf genEntry) (Gen.listOf genEntry)
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

        // Duplicates are the case a map would silently collapse.
        withDuplicates > 20 |> shouldEqual true

    [<Test>]
    let ``a forged entry is refused under the caller's name for it`` () : unit =
        // The context string is the client's, not this library's: a host that has
        // to fix one of these knows the table by whatever its own configuration
        // calls it.
        let exn =
            Assert.Throws<exn> (fun () ->
                UnixBootImage.withEnvironment
                    "whatever the client calls it"
                    [ UnixByteString.empty ; Unchecked.defaultof<UnixByteString> ]
                    image
                |> ignore<UnixBootImage<int, string>>
            )

        exn.Message |> shouldContainText "whatever the client calls it"

    [<Test>]
    let ``a forged path is refused under the caller's name`` () : unit =
        // `AbsoluteUnixPath` hides its case, so the only invalid value a client
        // can produce is a defaulted one; this setter is where it stops.
        let exn =
            Assert.Throws<exn> (fun () ->
                UnixBootImage.withProcessPath
                    "the client's name for the path"
                    (Some Unchecked.defaultof<AbsoluteUnixPath>)
                    image
                |> ignore<UnixBootImage<int, string>>
            )

        exn.Message |> shouldContainText "the client's name for the path"

    [<Test>]
    let ``no path is an answer rather than a request for a default`` () : unit =
        let system =
            image
            |> UnixBootImage.withProcessPath context (Some (AbsoluteUnixPath.parseOrFail context "/bin/app"))
            |> UnixBootImage.withProcessPath context None
            |> UnixBootImage.boot

        system.Process.ProcessPath |> shouldEqual None

    [<Test>]
    let ``the process's privilege is its credentials' privilege`` () : unit =
        let property (credentials : Credentials) : unit =
            UnixProcessState.callerPrivilege
                { empty with
                    Credentials = credentials
                }
            |> shouldEqual (Credentials.privilege credentials)

        Check.One (
            Config.QuickThrowOnFailure.WithMaxTest 200,
            Prop.forAll (Arb.fromGen CredentialsGen.credentials) property
        )

    [<Test>]
    let ``the only inode the process itself holds is its current directory`` () : unit =
        // The descriptors' inodes are held by the descriptions they name, which
        // are the machine's (`UnixMachineState.heldInodes`).
        UnixProcessState.heldInodes empty |> shouldEqual (Set.singleton rootInode)
