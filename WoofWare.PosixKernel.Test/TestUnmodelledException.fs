namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// A call this library cannot answer throws: `UnmodelledException` where its
/// model does not cover what was asked, and the BCL's own exception where no
/// client following the documentation could have asked it. The refusals behind
/// those throws are internal values, so no public type is named for one.
///
/// Each public entry point is its internal `tryX` with the refusal raised:
/// these tests hold the two to agreeing, answer for answer and refusal for
/// throw, over representative families of calls.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnmodelledException =

    let private library : Assembly = typeof<UnixError>.Assembly

    /// The public types whose names end in `Refusal` or `Fault` and are not
    /// refusals: the client's own buffer, the errors of parsers, flavour rules.
    let private closedValues : Set<string> =
        Set.ofList
            [
                "BufferRefusal"
                "DirectorySourceRefusal"
                "Ipv4PrefixRefusal"
                "SignalMaskRefusal"
                "BindFault"
                "GetCwdDestinationFault"
            ]

    [<Test>]
    let ``no public type is named Refusal or Fault, but the closed values`` () : unit =
        let named =
            library.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> not t.IsNested)
            |> Array.map (fun (t : Type) -> LibraryNames.withoutArity t.Name)
            |> Array.filter (fun (name : string) -> name.EndsWith "Refusal" || name.EndsWith "Fault")
            |> Set.ofArray

        // Controls, so that this cannot pass by finding nothing.
        named |> shouldContain "BufferRefusal"
        named |> shouldContain "BindFault"

        let faults =
            [
                for name in Set.difference named closedValues do
                    yield
                        $"%s{name} is public. A refusal is internal, and a public entry point raises it (Refusal.orRaise); if %s{name} is a value a client holds rather than a refusal, add it to closedValues"
                for name in Set.difference closedValues named do
                    yield $"%s{name} is listed as a public closed value, and is not one. Remove it from the list"
            ]

        faults |> shouldEqual []

    /// The error type of a `Result`, if `t` is one.
    let private resultError (t : Type) : Type option =
        if t.IsGenericType && t.GetGenericTypeDefinition () = typedefof<Result<int, int>> then
            Some (t.GetGenericArguments ()).[1]
        else
            None

    /// The `Error`s a public function may answer, none of them a refusal: an
    /// errno, or the error of a parser or a classifier, which says what was
    /// wrong with the client's own input.
    let private publicErrors : Set<string> =
        Set.ofList
            [
                "UnixError"
                "AbsoluteUnixPathError"
                "FileNameError"
                "SimulatedUnixPlatformError"
                "SymlinkTargetError"
                "UnixByteStringDefect"
                "UnixPathError"
                "UnixPathTextDefect"
                "BufferRefusal"
                "Ipv4PrefixRefusal"
                "SignalMaskRefusal"
            ]

    [<Test>]
    let ``no public function answers a refusal`` () : unit =
        let answered =
            library.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> LibraryNames.kindOf t = Some SourceConstructFlags.Module)
            |> Array.collect (fun (t : Type) ->
                t.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
                |> Array.choose (fun (m : MethodInfo) ->
                    resultError m.ReturnType
                    |> Option.map (fun (error : Type) ->
                        $"%s{LibraryNames.dottedName t}.%s{LibraryNames.sourceName m}",
                        LibraryNames.withoutArity error.Name
                    )
                )
            )
            |> List.ofArray

        // A control: a function whose `Error` is an errno.
        answered |> shouldContain ("UnixSignal.sigaction", "UnixError")

        answered
        |> List.filter (fun (_, error : string) -> not (publicErrors.Contains error))
        |> List.map (fun (name : string, error : string) -> $"%s{name} answers Error of %s{error}")
        |> shouldEqual []

    /// The exception a public entry point throws for a refusal of `kind`.
    let private expectedType (kind : RefusalKind) : Type =
        match kind with
        | RefusalKind.Unmodelled -> typeof<UnmodelledException>
        | RefusalKind.Contradictory _ -> typeof<ArgumentException>
        | RefusalKind.OutOfRange _ -> typeof<ArgumentOutOfRangeException>
        | RefusalKind.Misuse -> typeof<InvalidOperationException>
        | RefusalKind.NotOnPlatform -> typeof<PlatformNotSupportedException>

    let private kindGen : Gen<RefusalKind> =
        let parameter = ArbMap.defaults |> ArbMap.generate<NonEmptyString> |> Gen.map (fun s -> s.Get)

        Gen.oneof
            [
                Gen.constant RefusalKind.Unmodelled
                parameter |> Gen.map RefusalKind.Contradictory
                parameter |> Gen.map RefusalKind.OutOfRange
                Gen.constant RefusalKind.Misuse
                Gen.constant RefusalKind.NotOnPlatform
            ]

    [<Test>]
    let ``orRaise answers an Ok's value, and throws a refusal as its kind's exception, naming the operation and the description``
        ()
        : unit
        =
        let gen =
            gen {
                let! operation = ArbMap.defaults |> ArbMap.generate<NonEmptyString>
                let! description = ArbMap.defaults |> ArbMap.generate<NonEmptyString>
                let! kind = kindGen
                let! value = ArbMap.defaults |> ArbMap.generate<int>
                let! refused = ArbMap.defaults |> ArbMap.generate<bool>
                return operation.Get, description.Get, kind, value, refused
            }

        let property (operation : string, description : string, kind : RefusalKind, value : int, refused : bool) =
            let result : Result<int, string> =
                if refused then Error description else Ok value

            let raised =
                try
                    Ok (Refusal.orRaise operation (fun _ -> kind) id result)
                with e ->
                    Error e

            match result, raised with
            | Ok expected, Ok answered -> answered |> shouldEqual expected
            | Ok _, Error e -> failwith $"orRaise threw for an Ok: %O{e}"
            | Error _, Ok answered -> failwith $"orRaise answered %d{answered} for a refusal"
            | Error _, Error e ->
                e.GetType () |> shouldEqual (expectedType kind)
                e.Message |> shouldContainText operation
                e.Message |> shouldContainText description

                match kind, e with
                | RefusalKind.Contradictory parameter, (:? ArgumentException as e)
                | RefusalKind.OutOfRange parameter, (:? ArgumentException as e) -> e.ParamName |> shouldEqual parameter
                | _ -> ()

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

    /// What one call did: answered, or refused with a kind and a description.
    [<RequireQualifiedAccess>]
    type private Observed =
        | Answered
        | Refused of RefusalKind

    /// Check that `call` (the public entry point) agrees with `tryCall` (its
    /// internal `Result`-returning function): the same answer where `tryCall`
    /// answers, and otherwise the exception `kind` names, carrying `describe`'s
    /// text. Answers what `tryCall` did, so a caller can check that a family
    /// reached both answers and refusals.
    let private agrees<'Answer, 'Refusal when 'Answer : equality>
        (kind : 'Refusal -> RefusalKind)
        (describe : 'Refusal -> string)
        (tryCall : unit -> Result<'Answer, 'Refusal>)
        (call : unit -> 'Answer)
        : Observed
        =
        let thrown =
            try
                Ok (call ())
            with e ->
                Error e

        match tryCall (), thrown with
        | Ok expected, Ok answered ->
            answered |> shouldEqual expected
            Observed.Answered
        | Ok expected, Error e -> failwith $"the internal call answered %A{expected}, and the public one threw %O{e}"
        | Error refusal, Ok answered ->
            failwith
                $"the internal call refused (%s{describe refusal}), and the public one answered %A{answered}"
        | Error refusal, Error e ->
            e.GetType () |> shouldEqual (expectedType (kind refusal))
            e.Message |> shouldContainText (describe refusal)
            Observed.Refused (kind refusal)

    /// Run `observe` over `count` inputs from `gen`, and check that every kind
    /// in `expected` was observed, so that a family cannot pass vacuously.
    let private covering<'Input> (count : int) (gen : Gen<'Input>) (observe : 'Input -> Observed) (expected : Observed list) =
        let seen =
            Gen.sample count gen
            |> Array.map observe
            |> Array.map (fun (observed : Observed) ->
                match observed with
                | Observed.Refused (RefusalKind.Contradictory _) -> Observed.Refused (RefusalKind.Contradictory "")
                | Observed.Refused (RefusalKind.OutOfRange _) -> Observed.Refused (RefusalKind.OutOfRange "")
                | observed -> observed
            )
            |> Set.ofArray

        Set.difference (Set.ofList expected) seen |> Set.toList |> shouldEqual []

    let private platforms : SimulatedUnixPlatform list =
        [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ]

    let private platformGen : Gen<SimulatedUnixPlatform> = Gen.elements platforms

    let private booted (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
        UnixSystem.initial<int, string> platform
        |> Launched.boot UnixSystem.pipedStandardStreams 0 (CpuId 0)

    let private smallInt : Gen<int> = Gen.choose (-3, 40)

    [<Test>]
    let ``socket throws exactly when trySocket refuses`` () : unit =
        let gen =
            gen {
                let! platform = platformGen
                let! domain = smallInt
                let! kind = smallInt
                let! protocol = Gen.oneof [ smallInt ; Gen.elements [ 1 ; 6 ; 17 ; 58 ; 132 ; 136 ; 262 ] ]
                return platform, domain, kind, protocol
            }

        covering
            400
            gen
            (fun (platform, domain, kind, protocol) ->
                let system = booted platform

                agrees
                    SocketRefusal.kind
                    SocketRefusal.describe
                    (fun () -> UnixSocket.trySocket domain kind protocol system)
                    (fun () -> UnixSocket.socket domain kind protocol system)
            )
            [ Observed.Answered ; Observed.Refused RefusalKind.Unmodelled ]

    [<Test>]
    let ``clockGettime throws exactly when tryClockGettime refuses`` () : unit =
        let gen = Gen.zip platformGen (Gen.choose (-5, 20))

        covering
            200
            gen
            (fun (platform, clockId) ->
                let system = booted platform

                agrees
                    ClockGettimeRefusal.kind
                    ClockGettimeRefusal.describe
                    (fun () -> UnixClock.tryClockGettime clockId system)
                    (fun () -> UnixClock.clockGettime clockId system)
            )
            [ Observed.Answered ; Observed.Refused RefusalKind.Unmodelled ]

    [<Test>]
    let ``the descriptor calls throw exactly when their try functions refuse`` () : unit =
        let fdGen = Gen.choose (-1, 5)

        let gen =
            gen {
                let! platform = platformGen
                let! choice = Gen.choose (0, 2)
                let! fd = fdGen
                let! other = fdGen
                let! word = Gen.oneof [ smallInt ; Gen.choose (0, 0x2000) ]
                return platform, choice, fd, other, word
            }

        covering
            600
            gen
            (fun (platform, choice, fd, other, word) ->
                let system = booted platform

                match choice with
                | 0 ->
                    agrees
                        FcntlRefusal.kind
                        FcntlRefusal.describe
                        (fun () -> UnixDescriptor.tryFcntl fd word other system)
                        (fun () -> UnixDescriptor.fcntl fd word other system)
                | 1 ->
                    agrees
                        Dup3Refusal.kind
                        Dup3Refusal.describe
                        (fun () -> UnixDescriptor.tryDup3 fd other word system)
                        (fun () -> UnixDescriptor.dup3 fd other word system)
                | _ ->
                    agrees
                        PosixFadviseRefusal.kind
                        PosixFadviseRefusal.describe
                        (fun () -> UnixDescriptor.tryPosixFadvise fd 0L 0L word system)
                        (fun () -> UnixDescriptor.posixFadvise fd 0L 0L word system)
            )
            [
                Observed.Answered
                Observed.Refused RefusalKind.Unmodelled
                Observed.Refused RefusalKind.NotOnPlatform
            ]

    [<Test>]
    let ``getresuid throws exactly when tryGetresuid refuses`` () : unit =
        let buffer =
            Gen.elements [ UserBuffer.Mapped ; UserBuffer.Unmapped 8UL ; UserBuffer.Opaque ; UserBuffer.Addressless ]

        let gen =
            gen {
                let! platform = platformGen
                let! real = buffer
                let! effective = buffer
                let! saved = buffer
                return platform, real, effective, saved
            }

        covering
            200
            gen
            (fun (platform, real, effective, saved) ->
                let system = booted platform

                agrees
                    GetIdsRefusal.kind
                    GetIdsRefusal.describe
                    (fun () -> UnixCredentials.tryGetresuid real effective saved system)
                    (fun () -> UnixCredentials.getresuid real effective saved system)
            )
            [
                Observed.Answered
                Observed.Refused RefusalKind.Unmodelled
                Observed.Refused RefusalKind.NotOnPlatform
            ]

    [<Test>]
    let ``the boot setters throw exactly when their try functions refuse`` () : unit =
        let gen =
            gen {
                let! platform = platformGen
                let! choice = Gen.choose (0, 2)
                let! a = Gen.oneof [ Gen.choose (-3, 70000) ; Gen.choose (-3, 3) ]
                let! b = Gen.oneof [ Gen.choose (-3, 70000) ; Gen.choose (-3, 3) ]
                let! present = ArbMap.defaults |> ArbMap.generate<bool>
                return platform, choice, a, b, present
            }

        covering
            600
            gen
            (fun (platform, choice, a, b, present) ->
                let image = UnixSystem.initial<int, string> platform

                match choice with
                | 0 ->
                    agrees
                        ProcessorCountRefusal.kind
                        ProcessorCountRefusal.describe
                        (fun () -> UnixBootImage.tryWithProcessorCount a image)
                        (fun () -> UnixBootImage.withProcessorCount a image)
                | 1 ->
                    let range = uint16 (abs a % 65536), uint16 (abs b % 65536)

                    agrees
                        EphemeralPortRangeRefusal.kind
                        EphemeralPortRangeRefusal.describe
                        (fun () -> UnixBootImage.tryWithEphemeralPortRange range image)
                        (fun () -> UnixBootImage.withEphemeralPortRange range image)
                | _ ->
                    let value = if present then Some a else None

                    agrees
                        TcpSendSpaceRefusal.kind
                        TcpSendSpaceRefusal.describe
                        (fun () -> UnixBootImage.tryWithTcpSendSpace value image)
                        (fun () -> UnixBootImage.withTcpSendSpace value image)
            )
            [
                Observed.Answered
                Observed.Refused RefusalKind.Unmodelled
                Observed.Refused (RefusalKind.Contradictory "")
                Observed.Refused (RefusalKind.OutOfRange "")
            ]

    let private syscallGen : Gen<Syscall> =
        let fd = Gen.choose (-1, 5)
        let word = Gen.oneof [ Gen.choose (-3, 40) ; Gen.choose (0, 0x2000) ]

        Gen.oneof
            [
                Gen.map3 (fun fd command argument -> Syscall.Fcntl (fd, command, argument)) fd word fd
                Gen.map3 (fun oldFd newFd flags -> Syscall.Dup3 (oldFd, newFd, flags)) fd fd word
                Gen.map3 (fun fd offset whence -> Syscall.LSeek (fd, int64 offset, whence)) fd word word
                Gen.map2 (fun fd operation -> Syscall.FLock (fd, operation)) fd word
                Gen.map2 (fun fd mode -> Syscall.FChMod (fd, mode)) fd word
                Gen.map (fun fd -> Syscall.Close fd) fd
                Gen.map2 (fun destination source -> Syscall.FileClone (destination, source)) fd fd
                Gen.map3 (fun inFd outFd flags -> Syscall.CopyFileRange (inFd, outFd, 4UL, flags)) fd fd word
            ]

    [<Test>]
    let ``step throws exactly when tryStep refuses, on a system and on a machine`` () : unit =
        let gen = Gen.zip platformGen syscallGen

        covering
            600
            gen
            (fun (platform, call) ->
                let system = booted platform

                let onSystem =
                    agrees
                        SyscallRefusal.kind
                        SyscallRefusal.describe
                        (fun () -> UnixSystem.tryStep 0 call system)
                        (fun () -> UnixSystem.step 0 call system)

                let machine = SimulatedMachine.ofSystem system
                let pid = UnixSystem.processId system

                let unwrap (answer : 'a option) : 'a =
                    match answer with
                    | Some answer -> answer
                    | None -> failwith $"expected the machine to hold process %O{pid}"

                let onMachine =
                    agrees
                        SyscallRefusal.kind
                        SyscallRefusal.describe
                        (fun () ->
                            match unwrap (SimulatedMachine.tryStep pid 0 call machine) with
                            | Ok outcome, machine -> Ok (outcome, machine)
                            | Error refusal, _ -> Error refusal
                        )
                        (fun () -> unwrap (SimulatedMachine.step pid 0 call machine))

                onMachine |> shouldEqual onSystem
                onSystem
            )
            [
                Observed.Answered
                Observed.Refused RefusalKind.Unmodelled
                Observed.Refused RefusalKind.NotOnPlatform
            ]

    /// The statistics a `statfs` reports throw where a field group was refused,
    /// as the field group is read: every group of every mount type, under each
    /// flavour, agrees with the internal `Result` it holds.
    [<Test>]
    let ``a refused statistics field throws when it is read`` () : unit =
        let mounts =
            [ EmulatedFileSystemType.Tmpfs ; EmulatedFileSystemType.Apfs ; EmulatedFileSystemType.Nfs ]

        let observed =
            [
                for platform in platforms do
                    for fileSystemType in mounts do
                        let flavour = SimulatedUnixPlatform.flavour platform

                        if EmulatedFileSystemType.isReportableUnder flavour fileSystemType then
                            let mount = EmulatedMount.defaultOf fileSystemType

                            match FileSystemStatistics.ofMount platform mount with
                            | FileSystemStatistics.Linux linux ->
                                yield
                                    agrees
                                        GeometryRefusal.kind
                                        GeometryRefusal.describe
                                        (fun () -> linux.GeometryResult)
                                        (fun () -> linux.Geometry)

                                yield
                                    agrees
                                        CapacityRefusal.kind
                                        CapacityRefusal.describe
                                        (fun () -> linux.CapacityResult)
                                        (fun () -> linux.Capacity)

                                yield
                                    agrees
                                        FileSystemIdRefusal.kind
                                        FileSystemIdRefusal.describe
                                        (fun () -> linux.FileSystemIdResult)
                                        (fun () -> linux.FileSystemId)

                                yield
                                    agrees
                                        MountFieldsRefusal.kind
                                        MountFieldsRefusal.describe
                                        (fun () -> linux.FlagsResult)
                                        (fun () -> linux.Flags)
                            | FileSystemStatistics.Darwin darwin ->
                                yield
                                    agrees
                                        GeometryRefusal.kind
                                        GeometryRefusal.describe
                                        (fun () -> darwin.GeometryResult)
                                        (fun () -> darwin.Geometry)

                                yield
                                    agrees
                                        CapacityRefusal.kind
                                        CapacityRefusal.describe
                                        (fun () -> darwin.CapacityResult)
                                        (fun () -> darwin.Capacity)

                                yield
                                    agrees
                                        FileSystemIdRefusal.kind
                                        FileSystemIdRefusal.describe
                                        (fun () -> darwin.FileSystemIdResult)
                                        (fun () -> darwin.FileSystemId)

                                yield
                                    agrees
                                        MountFieldsRefusal.kind
                                        MountFieldsRefusal.describe
                                        (fun () -> darwin.MountResult)
                                        (fun () -> darwin.Mount)
            ]
            |> Set.ofList

        observed
        |> shouldEqual (Set.ofList [ Observed.Answered ; Observed.Refused RefusalKind.Unmodelled ])

    [<Test>]
    let ``exitThread throws exactly when tryExitThread refuses`` () : unit =
        // Task 0 leads, and task 2 is parked in an `flock` on an event queue,
        // so the leader's exit is unmodelled, the parked task's is a misuse,
        // and task 1's is answered.
        let world (platform : SimulatedUnixPlatform) : UnixSystem<int, string> =
            let system = booted platform

            let create =
                match SimulatedUnixPlatform.flavour platform with
                | SimulatedUnixFlavour.Linux -> FileDescriptorRegistry.createEpoll
                | SimulatedUnixFlavour.Darwin -> FileDescriptorRegistry.createKqueue system.Process.ProcessId

            let fd, registry = create (UnixSystemState.fileDescriptors system)

            let queue =
                match FileDescriptorRegistry.tryFindWithId fd registry with
                | Some (id, _) -> id
                | None -> failwith $"fd %d{fd} names no description"

            UnixSystemState.withFileDescriptors registry system
            |> Tasks.ensure 1
            |> Tasks.ensure 2
            |> UnixWait.park
                2
                (ParkedSyscall.Flock
                    {
                        Requester = queue
                        Mode = FlockMode.Exclusive
                    })

        covering
            40
            (Gen.zip platformGen (Gen.choose (0, 2)))
            (fun (platform, task) ->
                let system = world platform

                agrees
                    ThreadExitRefusal.kind
                    ThreadExitRefusal.describe
                    (fun () -> UnixTaskLifecycle.tryExitThread task 0 system)
                    (fun () -> UnixTaskLifecycle.exitThread task 0 system)
            )
            [
                Observed.Answered
                Observed.Refused RefusalKind.Unmodelled
                Observed.Refused RefusalKind.Misuse
            ]
