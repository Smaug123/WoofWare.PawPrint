namespace WoofWare.PosixKernel.Test

open System
open System.Reflection
open System.Text.RegularExpressions
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open Microsoft.FSharp.Reflection
open NUnit.Framework
open WoofWare.PosixKernel

/// A refusal is open: its cases are hidden from clients, and its module's
/// `Reason` module has one partial pattern per case, which is how a client
/// names a reason. These tests hold each open refusal's patterns to its
/// cases, and hold the suffix `Refusal` to meaning an open union.
[<TestFixture>]
module TestOpenRefusals =

    /// The refusals whose cases are hidden. A union converted to an open one
    /// joins this list and leaves `unconverted`. The boot faults
    /// `CurrentDirectoryFault` and `FileSystemSeedFault` are refusals of the
    /// boot and launch setters despite their names, and are here too.
    let private openRefusals : Type list =
        [
            typeof<BindRefusal>
            typeof<BootTimeRefusal>
            typeof<CredentialsRefusal>
            typeof<CurrentDirectoryFault>
            typeof<EphemeralPortRangeRefusal>
            typeof<FileSystemSeedFault>
            typeof<InotifyLimitsRefusal>
            typeof<LaunchRefusal>
            typeof<LaunchTableRefusal>
            typeof<LeaderThreadIdRefusal>
            typeof<MountRefusal>
            typeof<Pipe2Refusal>
            typeof<PipeDeviceRefusal>
            typeof<ProcessIdRefusal>
            typeof<ProcessorCountRefusal>
            typeof<ProtectedFilesRefusal>
            typeof<SoMaxConnRefusal>
            typeof<TcpReceiveSpaceRefusal>
            typeof<TcpSendSpaceMaxRefusal>
            typeof<TcpSendSpaceRefusal>
            typeof<UmaskRefusal>
            typeof<UserAddressLimitRefusal>
        ]

    /// Public unions named `*Refusal` that are closed by what they are: a
    /// client's own buffer, the error of a parser, or a flavour's rule. They
    /// keep public cases.
    let private closedRefusals : Set<string> =
        Set.ofList
            [
                "BufferRefusal"
                "DirectorySourceRefusal"
                "Ipv4PrefixRefusal"
                "SignalMaskRefusal"
            ]

    /// Public unions named `*Refusal` that are to become open, and have not yet.
    /// This list only shrinks.
    let private unconverted : Set<string> =
        Set.ofList
            [
                "AcceptRefusal"
                "AccessRefusal"
                "BytesAvailableRefusal"
                "CapacityRefusal"
                "ChModRefusal"
                "ChOwnRefusal"
                "ClockGettimeRefusal"
                "CloneFileRefusal"
                "CloseRefusal"
                "ConnectRefusal"
                "CopyFileRangeRefusal"
                "DescriptionReleaseRefusal"
                "Dup2Refusal"
                "Dup3Refusal"
                "EndedWriteSignalRefusal"
                "EpollCreateRefusal"
                "EpollCtlRefusal"
                "EpollWaitRefusal"
                "ExecutionRefusal"
                "FChModAtRefusal"
                "FChModRefusal"
                "FChOwnAtRefusal"
                "FChOwnRefusal"
                "FLockRefusal"
                "FStatAtRefusal"
                "FStatRefusal"
                "FcntlRefusal"
                "FileCloneRefusal"
                "FileSystemIdRefusal"
                "GeometryRefusal"
                "GetCpuRefusal"
                "GetCwdRefusal"
                "GetEntropyRefusal"
                "GetGroupsRefusal"
                "GetIdsRefusal"
                "GetRandomRefusal"
                "GetSockNameRefusal"
                "InotifyAddWatchRefusal"
                "InotifyRefusal"
                "KeventRefusal"
                "KillRefusal"
                "KqueueRefusal"
                "LSeekRefusal"
                "LinkReadRefusal"
                "LinkRefusal"
                "ListenRefusal"
                "MkNodRefusal"
                "ModeChangeRefusal"
                "MountFieldsRefusal"
                "OpenRefusal"
                "OwnerChangeRefusal"
                "PReadRefusal"
                "PWriteRefusal"
                "PathRefusal"
                "PollRefusal"
                "PosixFadviseRefusal"
                "ProcessCreationRefusal"
                "ProcessEndRefusal"
                "ReadDirectoryRefusal"
                "ReadLinkRefusal"
                "ReadRefusal"
                "ReceiveRefusal"
                "RemovalRefusal"
                "RenameRefusal"
                "SendRefusal"
                "SetGroupsRefusal"
                "SetIdChangeRefusal"
                "SetIdsRefusal"
                "ShutdownRefusal"
                "SignalReceiverRefusal"
                "SigprocmaskRefusal"
                "SigsuspendRefusal"
                "SockaddrCopyRefusal"
                "SocketOptionRefusal"
                "SocketRefusal"
                "SpawnRefusal"
                "StatRefusal"
                "StickyRefusal"
                "SymlinkRefusal"
                "SyscallInterruptionRefusal"
                "SyscallRefusal"
                "ThreadExitRefusal"
                "ThreadKillRefusal"
                "TruncationRefusal"
                "UTimensAtRefusal"
                "UnlinkAtRefusal"
                "WaitStatusRefusal"
                "WriteRefusal"
            ]

    let private library : Assembly = typeof<UnixError>.Assembly

    let private openRefusalCases () : TestCaseData seq =
        openRefusals |> List.map (fun (t : Type) -> TestCaseData t) |> Seq.ofList

    /// Public as well as non-public: with `NonPublic` alone, a union whose
    /// cases are public answers none.
    let private everyCase : BindingFlags =
        BindingFlags.Public ||| BindingFlags.NonPublic

    let private hiddenCases (t : Type) : UnionCaseInfo list =
        FSharpType.GetUnionCases (t, everyCase) |> List.ofArray

    let private activePattern : Regex = Regex @"^\|(\w+)\|_\|$"

    /// The `Reason` module of an open refusal's module, if it has one.
    let private reasonModule (t : Type) : Type option =
        let name = $"%s{LibraryNames.dottedName t}.Reason"

        library.GetTypes ()
        |> Array.filter (fun (m : Type) ->
            LibraryNames.kindOf m = Some SourceConstructFlags.Module
            && LibraryNames.dottedName m = name
        )
        |> Array.tryExactlyOne

    /// The patterns of a `Reason` module, each by the name a client writes,
    /// instantiated for `t`'s type arguments where the union is generic.
    let private patterns (t : Type) (reason : Type) : (string * MethodInfo) list =
        reason.GetMethods (BindingFlags.Public ||| BindingFlags.Static ||| BindingFlags.DeclaredOnly)
        |> Array.choose (fun (m : MethodInfo) ->
            let matched = activePattern.Match m.Name

            if matched.Success then
                let m =
                    if m.IsGenericMethodDefinition then
                        m.MakeGenericMethod (t.GetGenericArguments ())
                    else
                        m

                Some (matched.Groups.[1].Value, m)
            else
                None
        )
        |> List.ofArray

    /// What a case's pattern hands back: nothing for a case with no fields,
    /// the field for one, and the fields as a tuple in declaration order for
    /// several.
    let private payloadType (case : UnionCaseInfo) : Type =
        match case.GetFields () with
        | [||] -> typeof<unit>
        | [| field |] -> field.PropertyType
        | fields -> FSharpType.MakeTupleType (fields |> Array.map (fun (f : PropertyInfo) -> f.PropertyType))

    let private payload (case : UnionCaseInfo) (fields : obj[]) : obj =
        match fields with
        | [||] -> box ()
        | [| field |] -> field
        | fields -> FSharpValue.MakeTuple (fields, payloadType case)

    [<TestCaseSource(nameof openRefusalCases)>]
    let ``an open refusal hides its cases and has exactly one pattern per case, of the case's name`` (t : Type) : unit =
        FSharpType.IsUnion t |> shouldEqual false
        FSharpType.IsUnion (t, everyCase) |> shouldEqual true

        let reason =
            match reasonModule t with
            | Some reason -> reason
            | None ->
                failwith $"expected %s{LibraryNames.dottedName t} to have a module %s{LibraryNames.dottedName t}.Reason"

        let patterns = patterns t reason

        patterns
        |> List.map fst
        |> List.sort
        |> shouldEqual (hiddenCases t |> List.map (fun (c : UnionCaseInfo) -> c.Name) |> List.sort)

        // A partial pattern returning `bool` or `voption` would not reach a
        // client on F# 8, or would hand back something else.
        for case in hiddenCases t do
            let _, m = patterns |> List.find (fun (name, _) -> name = case.Name)

            m.GetParameters ()
            |> Array.map (fun (p : ParameterInfo) -> p.ParameterType)
            |> shouldEqual [| t |]

            m.ReturnType
            |> shouldEqual (typedefof<int option>.MakeGenericType (payloadType case))

    [<TestCaseSource(nameof openRefusalCases)>]
    let ``each pattern answers its own case's fields in order, and no other case`` (t : Type) : unit =
        let reason =
            match reasonModule t with
            | Some reason -> reason
            | None ->
                failwith $"expected %s{LibraryNames.dottedName t} to have a module %s{LibraryNames.dottedName t}.Reason"

        let patterns = patterns t reason
        let cases = hiddenCases t
        // A control, so that this cannot pass by checking nothing.
        cases |> shouldNotEqual []

        for case in cases do
            let fieldsGen : Gen<obj[]> =
                case.GetFields ()
                |> Array.map (fun (f : PropertyInfo) -> (ArbMap.defaults.ArbFor f.PropertyType).Generator)
                |> List.ofArray
                |> Gen.sequenceToList
                |> Gen.map Array.ofList

            let property (fields : obj[]) : unit =
                let refusal = FSharpValue.MakeUnion (case, fields, true)

                let answers =
                    patterns
                    |> List.choose (fun (name : string, m : MethodInfo) ->
                        // `None` is null.
                        match m.Invoke ((null : obj), [| refusal |]) with
                        | null -> None
                        | answered -> Some (name, answered.GetType().GetProperty("Value").GetValue answered)
                    )

                match answers with
                | [ name, answered ] ->
                    name |> shouldEqual case.Name

                    if not (answered = payload case fields) then
                        failwith
                            $"%s{LibraryNames.dottedName t}.Reason.%s{name} answered %A{answered} for %A{refusal}, whose fields in order are %A{fields}"
                | answers ->
                    let names = answers |> List.map fst
                    failwith $"expected exactly the pattern %s{case.Name} to answer %A{refusal}, but %A{names} did"

            Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen fieldsGen) property)

    [<TestCaseSource(nameof openRefusalCases)>]
    let ``a client names each reason by its pattern, and cannot name the case`` (t : Type) : unit =
        let names = LibraryNames.namesInSource library
        let owner = LibraryNames.dottedName t
        // A control, so that this cannot pass by checking nothing.
        hiddenCases t |> shouldNotEqual []

        for case in hiddenCases t do
            Map.tryFind $"%s{owner}.Reason.%s{case.Name}" names |> shouldEqual (Some true)
            Map.tryFind $"%s{owner}.%s{case.Name}" names |> shouldEqual (Some false)

    [<Test>]
    let ``a public union named Refusal is closed by nature, or is open`` () : unit =
        let refusals =
            library.GetExportedTypes ()
            |> Array.filter (fun (t : Type) -> not t.IsNested && (LibraryNames.withoutArity t.Name).EndsWith "Refusal")
            |> List.ofArray

        let named (types : Type list) : Set<string> =
            types
            |> List.map (fun (t : Type) -> LibraryNames.withoutArity t.Name)
            |> Set.ofList

        let publicUnions = refusals |> List.filter FSharpType.IsUnion |> named

        let hiddenUnions =
            refusals
            |> List.filter (fun (t : Type) -> not (FSharpType.IsUnion t) && FSharpType.IsUnion (t, everyCase))
            |> named

        let declaredOpen = named openRefusals

        // Controls, so that this cannot pass by finding nothing.
        publicUnions |> shouldContain "BufferRefusal"
        hiddenUnions |> shouldContain "BindRefusal"

        Set.intersect closedRefusals unconverted |> Set.toList |> shouldEqual []

        let faults =
            [
                for name in Set.difference publicUnions (Set.union closedRefusals unconverted) do
                    yield
                        $"%s{name} is a public union named Refusal. Convert it to an open refusal (internal representation, a Reason module with one pattern per case, and an entry in openRefusals), or, if it is closed by nature rather than a gap in the model, add it to closedRefusals"
                for name in Set.difference (Set.union closedRefusals unconverted) publicUnions do
                    yield $"%s{name} is listed as a public union named Refusal, and is not one. Remove it from the list"
                for name in Set.difference hiddenUnions declaredOpen do
                    yield
                        $"%s{name} hides its cases and is not in openRefusals, so nothing checks its patterns. Add it there"
            ]

        faults |> shouldEqual []
