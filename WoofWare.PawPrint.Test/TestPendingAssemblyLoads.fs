namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `PendingAssemblyLoads` decides the order in which a thread announces the assemblies it loaded.
/// CoreCLR announces each load as it completes, running the handler to completion (and announcing
/// whatever the handler itself loads, inside it) before the next load. PawPrint records a whole
/// step's loads at once and announces them over later steps, so these tests run a nested program
/// of loads through the queue and compare the announcements with CoreCLR's order.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPendingAssemblyLoads =

    /// One assembly load. `Subscribed` is whether the event has a subscriber when the load is
    /// announced; `Handler` is what the subscriber runs, if it does.
    type private Load =
        {
            Name : string
            Subscribed : bool
            Handler : Step list
        }

    /// The loads one interpreter step makes, in load order.
    and private Step = Load list

    let private loadsGen : Gen<Step list> =
        let rec program (size : int) : Gen<Step list> =
            gen {
                let! stepCount = Gen.choose (0, 3)

                return!
                    Gen.listOfLength
                        stepCount
                        (gen {
                            let! loadCount = Gen.choose (0, 3)

                            return!
                                Gen.listOfLength
                                    loadCount
                                    (gen {
                                        let! subscribed = Gen.elements [ true ; true ; false ]

                                        let! handler = if size <= 0 then Gen.constant [] else program (size / 4)

                                        return
                                            {
                                                Name = ""
                                                Subscribed = subscribed
                                                Handler = handler
                                            }
                                    })
                        })
            }

        Gen.sized program

    /// Give every load a distinct name, so an announcement identifies the load it came from.
    let private name (program : Step list) : Step list =
        let mutable next = 0

        let rec program' (steps : Step list) : Step list =
            steps
            |> List.map (fun step ->
                step
                |> List.map (fun load ->
                    let n = next
                    next <- next + 1

                    { load with
                        Name = $"Assembly%d{n}"
                        Handler = program' load.Handler
                    }
                )
            )

        program' program

    /// CoreCLR's order: each load is announced as it completes, and its handler runs to completion
    /// before the next load. A load with no subscriber is not announced and runs nothing.
    let rec private coreClrOrder (program : Step list) : string list =
        program
        |> List.collect (fun step ->
            step
            |> List.collect (fun load ->
                if load.Subscribed then
                    load.Name :: coreClrOrder load.Handler
                else
                    []
            )
        )

    /// Run `program` as one thread would under `AssemblyLoadEvent`: at the start of every step,
    /// announce whatever `PendingAssemblyLoads` says is due, pushing a frame that runs the load's
    /// handler; otherwise run the active frame's next step, recording its loads, or return from
    /// the frame if it has none left.
    let private simulate (program : Step list) : string list * PendingAssemblyLoads =
        let loads =
            let rec all (steps : Step list) : Load list =
                steps
                |> List.collect (fun step -> step |> List.collect (fun load -> load :: all load.Handler))

            all program |> List.map (fun load -> load.Name, load) |> Map.ofList

        let rec go
            (frames : (FrameId * Step list) list)
            (nextFrame : int)
            (pending : PendingAssemblyLoads)
            (announced : string list)
            : string list * PendingAssemblyLoads
            =
            let isLive (frame : FrameId) : bool =
                frames |> List.exists (fun (f, _) -> f = frame)

            match PendingAssemblyLoads.tryTake isLive pending with
            | Some taken ->
                let load = loads.[taken.DefinitionFullName]

                if load.Subscribed then
                    let frame = FrameId nextFrame

                    go
                        ((frame, load.Handler) :: frames)
                        (nextFrame + 1)
                        (PendingAssemblyLoads.announcedBy frame taken)
                        (load.Name :: announced)
                else
                    go frames nextFrame (PendingAssemblyLoads.skipped taken) announced
            | None ->
                match frames with
                | [] -> List.rev announced, pending
                | (frame, step :: rest) :: below ->
                    let loaded = step |> List.map (fun load -> load.Name)
                    go ((frame, rest) :: below) nextFrame (PendingAssemblyLoads.record loaded pending) announced
                | (_, []) :: below -> go below nextFrame pending announced

        go [ FrameId 0, program ] 1 PendingAssemblyLoads.empty []

    [<Test>]
    let ``Assemblies are announced in the order CoreCLR announces them`` () : unit =
        let property (program : Step list) : unit =
            let program = name program
            let announced, pending = simulate program

            announced |> shouldEqual (coreClrOrder program)

            // Every load was either announced or skipped: nothing is left behind once the thread
            // has run out of frames.
            PendingAssemblyLoads.toList pending |> shouldEqual []

        Prop.forAll (Arb.fromGen loadsGen) property |> Check.QuickThrowOnFailure

    [<Test>]
    let ``Nothing more of a batch is due while the frame announcing its previous load is live`` () : unit =
        let taken =
            PendingAssemblyLoads.empty
            |> PendingAssemblyLoads.record [ "A" ; "B" ]
            |> PendingAssemblyLoads.tryTake (fun _ -> failwith "nothing is being awaited yet")
            |> Option.get

        taken.DefinitionFullName |> shouldEqual "A"

        let announcing = FrameId 7
        let pending = PendingAssemblyLoads.announcedBy announcing taken

        PendingAssemblyLoads.tryTake (fun frame -> frame = announcing) pending
        |> Option.isNone
        |> shouldEqual true

        let next = PendingAssemblyLoads.tryTake (fun _ -> false) pending |> Option.get
        next.DefinitionFullName |> shouldEqual "B"

        PendingAssemblyLoads.skipped next
        |> PendingAssemblyLoads.isEmpty
        |> shouldEqual true
