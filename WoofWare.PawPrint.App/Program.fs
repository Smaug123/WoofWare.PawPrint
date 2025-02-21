namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO
open WoofWare.DotnetRuntimeLocator

module Program =
    /// Returns the pointer to the resulting array on the heap.
    let allocateArgs (args : string list) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let argsAllocations, state =
            (state, args)
            ||> Seq.mapFold (fun state arg -> IlMachineState.Allocate (ReferenceType.String arg) state
            // TODO: set the char values in memory
            )

        let arrayAllocation, state =
            IlMachineState.Allocate
                (ReferenceType.Array (args.Length, Type.ReferenceType ReferenceType.ManagedObject))
                state
        // TODO: set the length of the array

        let state =
            ((state, 0), argsAllocations)
            ||> Seq.fold (fun (state, i) arg ->
                let state =
                    IlMachineState.SetArrayValue arrayAllocation (CliObject.OfManagedObject arg) i state

                state, i + 1
            )
            |> fst

        arrayAllocation, state

    let reallyMain (argv : string[]) : int =
        match argv |> Array.toList with
        | dllPath :: args ->
            let dotnetRuntimes =
                // TODO: work out which runtime it expects to use. For now we just use the first one we find.
                DotnetEnvironmentInfo.Get().Frameworks
                |> Seq.map (fun fi -> Path.Combine (fi.Path, fi.Version.ToString ()))
                |> Seq.toArray

            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)
            let dumped = Assembly.read fileStream

            let entryPoint =
                match dumped.MainMethod with
                | None -> failwith $"No entry point in {dllPath}"
                | Some d -> d

            let mainMethod = dumped.Methods.[entryPoint]

            if mainMethod.Signature.GenericParameterCount > 0 then
                failwith "Refusing to execute generic main method"

            let state = IlMachineState.Initial dumped

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
                |> IlMachineState.AddThread
                    { MethodState.Empty mainMethod None with
                        Arguments = ImmutableArray.Create (CliObject.OfManagedObject arrayAllocation)
                    }

            let mutable state = state

            while true do
                state <- AbstractMachine.executeOneStep dotnetRuntimes state mainThread

            0
        | _ ->
            Console.Error.WriteLine "Supply exactly one DLL path"
            1

    [<EntryPoint>]
    let main argv =
        try
            reallyMain argv
        with _ ->
            reraise ()
