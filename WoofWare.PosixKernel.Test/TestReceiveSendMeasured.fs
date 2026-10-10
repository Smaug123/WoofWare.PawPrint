namespace WoofWare.PosixKernel.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `recv(2)` and `send(2)` held to sections H and O of `tcp-recv-send.c`
/// (docs/plans/2026-10-07-tcp-byte-transfer), measured on Linux 6.18.5 aarch64
/// and Darwin 27.0: the flag word's numbering, and what each call answers
/// through a descriptor nothing holds, a regular file, each end of a pipe and
/// an idle connected socket, through a mapped buffer, NULL and `(void*)-1`, at
/// lengths 4 and 0.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestReceiveSendMeasured =

    let private lines (flavour : SimulatedUnixFlavour) : string list =
        let name =
            match flavour with
            | SimulatedUnixFlavour.Linux -> "WoofWare.PosixKernel.Test.tcpRecvSend.linux.txt"
            | SimulatedUnixFlavour.Darwin -> "WoofWare.PosixKernel.Test.tcpRecvSend.darwin.txt"

        use stream =
            match Assembly.GetExecutingAssembly().GetManifestResourceStream name with
            | null -> failwith $"no embedded resource %s{name}"
            | stream -> stream

        use reader = new StreamReader (stream)

        reader.ReadToEnd().Split ('\n', StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray

    /// The tab-separated fields of each line of `flavour`'s run in `section`,
    /// without the section's name.
    let private rows (flavour : SimulatedUnixFlavour) (section : string) : string list list =
        lines flavour
        |> List.filter (fun line -> line.StartsWith (section + "\t"))
        |> List.map (fun line -> line.Split '\t' |> List.ofArray |> List.tail)

    let private flavours : SimulatedUnixFlavour list =
        [ SimulatedUnixFlavour.Linux ; SimulatedUnixFlavour.Darwin ]

    [<Test>]
    let ``each flavour's header numbers exactly the flags MessageFlag gives it, as MessageFlag numbers them``
        ()
        : unit
        =
        for flavour in flavours do
            let measured =
                rows flavour "H"
                |> List.map (fun row ->
                    match row with
                    | [ name ; number ] -> name, Convert.ToInt32 (number, 16)
                    | other -> failwith $"%O{flavour}: a header row %A{other}"
                )
                |> Map.ofList

            let modelled =
                MessageFlag.named
                |> List.choose (fun flag ->
                    MessageFlag.number flavour flag
                    |> Option.map (fun number -> MessageFlag.describe flag, number)
                )
                |> Map.ofList

            modelled |> shouldEqual measured

    [<Test>]
    let ``a flag word decodes to one flag per bit, which encode back to the word`` () : unit =
        let property (flavour : SimulatedUnixFlavour, word : int) : unit =
            let flags = MessageFlag.decode flavour word
            List.length flags |> shouldEqual (Numerics.BitOperations.PopCount (uint32 word))
            MessageFlag.encode flavour flags |> shouldEqual (Some word)

            for flag in flags do
                match flag with
                | MessageFlag.Unnamed bit ->
                    MessageFlag.named
                    |> List.filter (fun named -> MessageFlag.number flavour named = Some bit)
                    |> shouldEqual []
                | named ->
                    MessageFlag.decode flavour (MessageFlag.number flavour named |> Option.get)
                    |> shouldEqual [ named ]

        let gen =
            gen {
                let! flavour = Gen.elements flavours
                let! word = ArbMap.defaults |> ArbMap.generate<int>
                // Words made of the named flags too, which a random int rarely is.
                let! named = Gen.subListOf MessageFlag.named

                let ofNamed = named |> List.choose (MessageFlag.number flavour) |> List.fold (|||) 0

                let! pick = Gen.elements [ word ; ofNamed ; word ||| ofNamed ]
                return flavour, pick
            }

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 2000, Prop.forAll (Arb.fromGen gen) property)

    /// What one of `recv` and `send` came to, as the probe prints it.
    let private printed (answer : Result<int64, UnixError>) : string =
        match answer with
        | Ok count -> $"%d{count}"
        | Error error -> $"-1 %O{error}"

    [<Test>]
    let ``recv and send answer each descriptor, buffer and length as measured`` () : unit =
        for flavour in flavours do
            let run =
                FcntlWorld.runs
                |> List.find (fun run -> SimulatedUnixPlatform.flavour run.Platform = flavour)

            let system = FcntlWorld.system run

            let file, system =
                FcntlWorld.openWith (FcntlWorld.opening FileAccessMode.ReadWrite) "f" system

            let (pipeRead, pipeWrite), system = FcntlWorld.pipe 0 system
            let listener, system = KeventWorld.listenerAt 6200us system
            let client, system = KeventWorld.stream false system

            let system =
                match KeventWorld.connect client 6200us system with
                | ConnectOutcome.Completed, system -> system
                | other, _ -> failwith $"connect: %A{other}"

            let server, system = KeventWorld.accept listener system
            let _, system = UnixDescriptor.setNonBlocking client true system
            let bad = 900

            FileDescriptorRegistry.tryFindId bad (UnixSystemState.fileDescriptors system)
            |> shouldEqual None

            let target (name : string) : int =
                match name with
                | "badfd" -> bad
                | "file" -> file
                | "pipe-read" -> pipeRead
                | "pipe-write" -> pipeWrite
                | "socket-idle" -> client
                | other -> failwith $"the probe named target %s{other}"

            let buffer (name : string) : UserBuffer =
                match name with
                | "mapped" -> UserBuffer.Mapped
                | "null" -> UserBuffer.Unmapped 0UL
                | "minus1" -> UserBuffer.Unmapped UInt64.MaxValue
                | other -> failwith $"the probe named buffer %s{other}"

            let mutable system = system
            let mutable checkedRows = 0

            for row in rows flavour "O" do
                match row with
                | [ "p-fionread-after" ; count ] ->
                    UnixDescriptor.bytesAvailable server UserBuffer.Mapped system
                    |> shouldEqual (Ok (BytesAvailableAnswer.Reported (int count)))
                | [ targetName ; bufferName ; length ; recvText ; sendText ; sigpipe ] ->
                    let where = $"%O{flavour} %s{targetName} %s{bufferName} %s{length}"
                    let fd = target targetName
                    let buffer = buffer bufferName

                    let count =
                        match length.Split '=' with
                        | [| "len" ; n |] -> int n
                        | _ -> failwith $"%s{where}: no length"

                    sigpipe |> shouldEqual "sigpipe=0"

                    let received, after =
                        match UnixReadWrite.recv 0 fd buffer (uint64 count) 0 system with
                        | Ok (ReadOutcome.Answered (ReadAnswer.Completed bytes), after) ->
                            Ok (int64 bytes.Length), after
                        | Ok (ReadOutcome.Answered (ReadAnswer.Failed error), after) -> Error error, after
                        | other -> failwith $"%s{where}: recv came to %A{other}"

                    if printed received <> recvText.Substring "recv=".Length then
                        failwith $"%s{where}: recv answered %s{printed received}, measured %s{recvText}"

                    system <- after

                    let bytes = ImmutableArray.CreateRange (Seq.replicate count 'x'B)

                    match WriteOutcomes.admitThenSend 0 fd buffer bytes 0 system with
                    | Ok (WriteOutcome.Returns (answer, after)) ->
                        let sent =
                            match answer with
                            | WriteAnswer.Completed n -> Ok n
                            | WriteAnswer.Failed error -> Error error

                        if printed sent <> sendText.Substring "send=".Length then
                            failwith $"%s{where}: send answered %s{printed sent}, measured %s{sendText}"

                        system <- after
                    | Error (SendRefusal.ConnectionFault _) when
                        targetName = "socket-idle" && sendText = "send=-1 EFAULT"
                        ->
                        // A send whose buffer faults on a connected socket with
                        // room for it: EFAULT, taking nothing, on both. The
                        // model refuses rather than answer it, as it refuses a
                        // write whose copy would fault.
                        ()
                    | other -> failwith $"%s{where}: send came to %A{other}"

                    checkedRows <- checkedRows + 1
                | other -> failwith $"%O{flavour}: an O row %A{other}"

            checkedRows |> shouldEqual 30
