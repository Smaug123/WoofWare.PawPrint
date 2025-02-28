namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging
open WoofWare.DotnetRuntimeLocator

module Program =
    /// Returns the pointer to the resulting array on the heap.
    let allocateArgs (args : string list) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let argsAllocations, state =
            (state, args)
            ||> Seq.mapFold (fun state arg -> IlMachineState.allocate (ReferenceType.String arg) state
            // TODO: set the char values in memory
            )

        let arrayAllocation, state =
            IlMachineState.allocate
                (ReferenceType.Array (args.Length, Type.ReferenceType ReferenceType.ManagedObject))
                state
        // TODO: set the length of the array

        let state =
            ((state, 0), argsAllocations)
            ||> Seq.fold (fun (state, i) arg ->
                let state =
                    IlMachineState.setArrayValue arrayAllocation (CliObject.OfManagedObject arg) i state

                state, i + 1
            )
            |> fst

        arrayAllocation, state

    let reallyMain (argv : string[]) : int =
        let loggerFactory =
            LoggerFactory.Create (fun builder ->
                builder.AddConsole (fun options -> options.LogToStandardErrorThreshold <- LogLevel.Debug)
                |> ignore<ILoggingBuilder>
            )

        let logger = loggerFactory.CreateLogger "WoofWare.PawPrint.App"

        match argv |> Array.toList with
        | dllPath :: args ->
            let dotnetRuntimes =
                // TODO: work out which runtime it expects to use, parsing the runtimeconfig etc and using DotnetRuntimeLocator. For now we assume we're self-contained.
                // DotnetEnvironmentInfo.Get().Frameworks
                // |> Seq.map (fun fi -> Path.Combine (fi.Path, fi.Version.ToString ()))
                // |> ImmutableArray.CreateRange
                ImmutableArray.Create (FileInfo(dllPath).Directory.FullName)

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)
            let dumped = Assembly.read loggerFactory fileStream

            let entryPoint =
                match dumped.MainMethod with
                | None -> failwith $"No entry point in {dllPath}"
                | Some d -> d

            let mainMethod = dumped.Methods.[entryPoint]

            if mainMethod.Signature.GenericParameterCount > 0 then
                failwith "Refusing to execute generic main method"

            let state = IlMachineState.initial dotnetRuntimes dumped

            let arrayAllocation, state =
                match mainMethod.Signature.ParameterTypes |> Seq.toList with
                | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] ->
                    allocateArgs args state
                | _ -> failwith "Main method must take an array of strings; other signatures not yet implemented"

            match mainMethod.Signature.ReturnType with
            | TypeDefn.PrimitiveType PrimitiveType.Int32 -> ()
            | _ -> failwith "Main method must return int32; other types not currently supported"

            let state, mainThread =
                state
                |> IlMachineState.addThread
                    // TODO: we need to load the main method's class first, and that's a faff with the current layout
                    { MethodState.Empty mainMethod None with
                        Arguments = ImmutableArray.Create (CliObject.OfManagedObject arrayAllocation)
                    }

            let mutable state = state

            while true do
                state <- fst (AbstractMachine.executeOneStep loggerFactory state mainThread)

            0
        | _ ->
            logger.LogCritical "Supply exactly one DLL path"
            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
