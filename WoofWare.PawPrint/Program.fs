namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
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

    let run
        (loggerFactory : ILoggerFactory)
        (fileStream : Stream)
        (dotnetRuntimeDirs : ImmutableArray<string>)
        (argv : string list)
        =
        let logger = loggerFactory.CreateLogger "Program"

        let dumped = Assembly.read loggerFactory fileStream

        let entryPoint =
            match dumped.MainMethod with
            | None -> failwith $"No entry point in input DLL"
            | Some d -> d

        let mainMethod = dumped.Methods.[entryPoint]

        if mainMethod.Signature.GenericParameterCount > 0 then
            failwith "Refusing to execute generic main method"

        let state = IlMachineState.initial dotnetRuntimeDirs dumped

        let arrayAllocation, state =
            match mainMethod.Signature.ParameterTypes |> Seq.toList with
            | [ TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.String) ] ->
                allocateArgs argv state
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
                dumped.Name

        let mutable state = state

        while true do
            let state', whatWeDid =
                AbstractMachine.executeOneStep loggerFactory state mainThread

            state <- state'

            match whatWeDid with
            | WhatWeDid.Executed -> logger.LogInformation "Executed one step."
            | WhatWeDid.SuspendedForClassInit ->
                logger.LogInformation "Suspended execution of current method for class initialisation."
            | WhatWeDid.NotTellingYou -> logger.LogInformation "(Execution outcome missing.)"
            | WhatWeDid.BlockedOnClassInit threadBlockingUs ->
                logger.LogInformation "Unable to execute because class has not yet initialised."
