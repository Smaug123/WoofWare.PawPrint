namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module internal UnaryStringTokenIlOp =
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
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

                    // String type is:
                    // https://github.com/dotnet/runtime/blob/f0168ee80ba9aca18a7e7140b2bb436defda623c/src/libraries/System.Private.CoreLib/src/System/String.cs#L26
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
                            {
                                Name = "_firstChar"
                                Contents = CliType.ofChar state.ManagedHeap.StringArrayData.[dataAddr]
                                Offset = None
                                Type =
                                    AllConcreteTypes.findExistingConcreteType
                                        state.ConcreteTypes
                                        (baseClassTypes.Char.Assembly,
                                         baseClassTypes.Char.Namespace,
                                         baseClassTypes.Char.Name,
                                         ImmutableArray.Empty)
                                    |> Option.get
                            }
                            {
                                Name = "_stringLength"
                                Contents = CliType.Numeric (CliNumericType.Int32 stringToAllocate.Length)
                                Offset = None
                                Type =
                                    AllConcreteTypes.findExistingConcreteType
                                        state.ConcreteTypes
                                        (baseClassTypes.Int32.Assembly,
                                         baseClassTypes.Int32.Namespace,
                                         baseClassTypes.Int32.Name,
                                         ImmutableArray.Empty)
                                    |> Option.get
                            }
                        ]
                        |> CliValueType.OfFields Layout.Default

                    let state, stringType =
                        DumpedAssembly.typeInfoToTypeDefn' baseClassTypes state._LoadedAssemblies baseClassTypes.String
                        |> IlMachineState.concretizeType
                            loggerFactory
                            baseClassTypes
                            state
                            baseClassTypes.Corelib.Name
                            ImmutableArray.Empty
                            ImmutableArray.Empty

                    let addr, state = IlMachineState.allocateManagedObject stringType fields state

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
