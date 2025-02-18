namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.IO

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
            use fileStream = new FileStream (dllPath, FileMode.Open, FileAccess.Read)
            let dumped = Assembly.read fileStream
            let mainMethod = dumped.Methods.[dumped.MainMethod]

            if mainMethod.Signature.GenericParameterCount > 0 then
                failwith "Refusing to execute generic main method"

            let state = IlMachineState.Initial

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
                    {
                        LocalVariables = ImmutableArray.Empty
                        IlOpIndex = 0
                        EvaluationStack = EvalStack.Empty
                        Arguments = ImmutableArray.Create (CliObject.OfManagedObject arrayAllocation)
                        ExecutingMethod = dumped.Methods.[dumped.MainMethod]
                        LocalMemoryPool = ()
                        ReturnState = None
                    }

            let mutable state = state

            while true do
                state <- AbstractMachine.executeOneStep state dumped mainThread

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
