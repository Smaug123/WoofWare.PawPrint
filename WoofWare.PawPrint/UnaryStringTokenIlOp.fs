namespace WoofWare.PawPrint

open System.Reflection

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryStringTokenIlOp =
    let execute
        (baseClassTypes : BaseClassTypes<'a>)
        (op : UnaryStringTokenIlOp)
        (sh : StringToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            let addressToLoad, state =
                match state.InternedStrings.TryGetValue sh with
                | false, _ ->
                    let stringToAllocate = state.ActiveAssembly(thread).Strings sh

                    let dataAddr, state =
                        IlMachineState.allocateStringData stringToAllocate.Length state

                    let state = state |> IlMachineState.setStringData dataAddr stringToAllocate

                    let stringInstanceFields =
                        baseClassTypes.String.Fields
                        |> List.choose (fun field ->
                            if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                                Some (field.Name, field.Signature)
                            else
                                None
                        )
                        |> List.sortBy fst
                    // Assert that the string type has the fields we expect
                    if
                        stringInstanceFields
                        <> [
                            ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                            ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
                        ]
                    then
                        failwith
                            $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

                    let fields =
                        [
                            "_firstChar", CliType.OfChar state.ManagedHeap.StringArrayData.[dataAddr]
                            "_stringLength", CliType.Numeric (CliNumericType.Int32 stringToAllocate.Length)
                        ]

                    let addr, state =
                        IlMachineState.allocateManagedObject baseClassTypes.String fields state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (sh, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addressToLoad)) thread state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed
