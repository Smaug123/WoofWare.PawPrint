namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

[<RequireQualifiedAccess>]
module internal UnaryMetadataIlOp =
    let execute
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (op : UnaryMetadataTokenIlOp)
        (metadataToken : MetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | Call ->
            let state, methodToCall, generics =
                match metadataToken with
                | MetadataToken.MethodSpecification h ->
                    let spec = (state.ActiveAssembly thread).MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token ->
                        state, (state.ActiveAssembly thread).Methods.[token], Some spec.Signature
                    | MetadataToken.MemberReference ref ->
                        let state, _, method =
                            IlMachineState.resolveMember loggerFactory (state.ActiveAssembly thread) ref state

                        match method with
                        | Choice2Of2 _field -> failwith "tried to Call a field"
                        | Choice1Of2 method -> state, method, Some spec.Signature
                    | k -> failwith $"Unrecognised kind: %O{k}"
                | MetadataToken.MemberReference h ->
                    let state, _, method =
                        IlMachineState.resolveMember loggerFactory (state.ActiveAssembly thread) h state

                    match method with
                    | Choice2Of2 _field -> failwith "tried to Call a field"
                    | Choice1Of2 method -> state, method, None

                | MetadataToken.MethodDef defn ->
                    let activeAssy = state.ActiveAssembly thread

                    match activeAssy.Methods.TryGetValue defn with
                    | true, method -> state, method, None
                    | false, _ -> failwith $"could not find method in {activeAssy.Name}"
                | k -> failwith $"Unrecognised kind: %O{k}"

            match
                IlMachineState.loadClass
                    loggerFactory
                    (fst methodToCall.DeclaringType)
                    (snd methodToCall.DeclaringType)
                    thread
                    state
            with
            | NothingToDo state ->
                state.WithThreadSwitchedToAssembly (snd methodToCall.DeclaringType) thread
                |> fst
                |> IlMachineState.callMethodInActiveAssembly loggerFactory thread generics methodToCall None
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit

        | Callvirt ->
            let method, generics =
                match metadataToken with
                | MetadataToken.MethodDef defn ->
                    let activeAssy = state.ActiveAssembly thread

                    match activeAssy.Methods.TryGetValue defn with
                    | true, method -> method, None
                    | false, _ -> failwith $"could not find method in {activeAssy.Name}"
                | _ -> failwith $"TODO (Callvirt): %O{metadataToken}"

            let currentObj =
                match IlMachineState.peekEvalStack thread state with
                | None -> failwith "nothing on stack when Callvirt called"
                | Some obj -> obj

            let methodToCall =
                match currentObj with
                | EvalStackValue.ManagedPointer src ->
                    match src with
                    | ManagedPointerSource.Null -> failwith "TODO: raise NullReferenceException"
                    | ManagedPointerSource.LocalVariable _ -> failwith "TODO (Callvirt): LocalVariable"
                    | ManagedPointerSource.Heap addr ->
                        match state.ManagedHeap.NonArrayObjects.TryGetValue addr with
                        | false, _ -> failwith "TODO (Callvirt): address"
                        | true, v ->
                            { new TypeInfoEval<_> with
                                member _.Eval ty =
                                    let matchingMethods =
                                        ty.Methods
                                        |> List.filter (fun mi ->
                                            mi.Name = method.Name && mi.Signature = method.Signature && not mi.IsStatic
                                        )

                                    match matchingMethods with
                                    | [] ->
                                        failwith
                                            "TODO: walk up the class hierarchy; eventually throw MissingMethodException"
                                    | [ m ] -> m
                                    | _ -> failwith $"multiple matching methods for {method.Name}"
                            }
                            |> v.Type.Apply
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
                | _ -> failwith $"TODO (Callvirt): can't identify type of {currentObj}"

            state.WithThreadSwitchedToAssembly (snd methodToCall.DeclaringType) thread
            |> fst
            |> IlMachineState.callMethodInActiveAssembly loggerFactory thread generics methodToCall None
        | Castclass -> failwith "TODO: Castclass unimplemented"
        | Newobj ->
            let state, assy, ctor =
                match metadataToken with
                | MethodDef md ->
                    let activeAssy = state.ActiveAssembly thread
                    let method = activeAssy.Methods.[md]
                    state, activeAssy.Name, method
                | MemberReference mr ->
                    let state, name, method =
                        IlMachineState.resolveMember loggerFactory (state.ActiveAssembly thread) mr state

                    match method with
                    | Choice1Of2 mr -> state, name, mr
                    | Choice2Of2 _field -> failwith "unexpectedly NewObj found a constructor which is a field"
                | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

            let ctorType, ctorAssembly = ctor.DeclaringType
            let ctorAssembly = state.LoadedAssembly ctorAssembly |> Option.get
            let ctorType = ctorAssembly.TypeDefs.[ctorType]

            let fields =
                ctorType.Fields
                |> List.map (fun field ->
                    // TODO: I guess the type itself can have generics, which should be passed in as this array?
                    let zeroedAllocation = CliType.zeroOf ImmutableArray.Empty field.Signature
                    field.Name, zeroedAllocation
                )

            let allocatedAddr, state =
                IlMachineState.allocateManagedObject ctorType fields state

            let state =
                state
                |> IlMachineState.pushToEvalStack'
                    (EvalStackValue.ManagedPointer (ManagedPointerSource.Heap allocatedAddr))
                    thread

            let state, whatWeDid =
                state.WithThreadSwitchedToAssembly assy thread
                |> fst
                |> IlMachineState.callMethodInActiveAssembly loggerFactory thread None ctor (Some allocatedAddr)

            match whatWeDid with
            | SuspendedForClassInit -> failwith "unexpectedly suspended while initialising constructor"
            | BlockedOnClassInit threadBlockingUs ->
                failwith "TODO: Newobj blocked on class init synchronization unimplemented"
            | Executed -> ()

            state, WhatWeDid.Executed
        | Newarr ->
            let currentState = state.ThreadState.[thread]
            let popped, newMethodState = MethodState.popFromStack currentState.MethodState

            let currentState =
                { currentState with
                    MethodStates = currentState.MethodStates.SetItem (currentState.ActiveMethodState, newMethodState)
                }

            let len =
                match popped with
                | EvalStackValue.Int32 v -> v
                | popped -> failwith $"unexpectedly popped value %O{popped} to serve as array len"

            let elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state.LoadedAssembly currentState.ActiveAssembly
                    |> Option.get
                    |> fun assy -> assy.TypeDefs.[defn]
                | x -> failwith $"TODO: Newarr element type resolution unimplemented for {x}"

            let baseType =
                elementType.BaseType
                |> TypeInfo.resolveBaseType
                    (fun (x : DumpedAssembly) -> x.Name)
                    (fun x y -> x.TypeDefs.[y])
                    baseClassTypes
                    elementType.Assembly

            let zeroOfType =
                match baseType with
                | ResolvedBaseType.Object ->
                    // initialise with null references
                    fun () -> CliType.ObjectRef None
                | ResolvedBaseType.Enum -> failwith "TODO: Newarr Enum array initialization unimplemented"
                | ResolvedBaseType.ValueType -> failwith "TODO: Newarr ValueType array initialization unimplemented"
                | ResolvedBaseType.Delegate -> failwith "TODO: Newarr Delegate array initialization unimplemented"

            let alloc, state = IlMachineState.allocateArray zeroOfType len state

            let state =
                { state with
                    ThreadState = state.ThreadState |> Map.add thread currentState
                }
                |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some alloc)) thread
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed
        | Box -> failwith "TODO: Box unimplemented"
        | Ldelema -> failwith "TODO: Ldelema unimplemented"
        | Isinst -> failwith "TODO: Isinst unimplemented"
        | Stfld -> failwith "TODO: Stfld unimplemented"
        | Stsfld ->
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: Stsfld - throw MissingFieldException"
            | true, field ->

            do
                let logger = loggerFactory.CreateLogger "Stsfld"
                let declaring = state.ActiveAssembly(thread).TypeDefs.[field.DeclaringType]

                logger.LogInformation (
                    "Storing in static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    declaring.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            let popped, state = IlMachineState.popEvalStack thread state

            let toStore =
                EvalStackValue.toCliTypeCoerced (CliType.zeroOf ImmutableArray.Empty field.Signature) popped

            let state =
                { state with
                    Statics = state.Statics.SetItem ((field.DeclaringType, activeAssy.Name), toStore)
                }
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        | Ldfld ->
            let state, assyName, field =
                match metadataToken with
                | MetadataToken.FieldDefinition f ->
                    state, (state.ActiveAssembly thread).Name, state.ActiveAssembly(thread).Fields.[f]
                | MetadataToken.MemberReference mr ->
                    let state, assyName, field =
                        IlMachineState.resolveMember loggerFactory (state.ActiveAssembly thread) mr state

                    match field with
                    | Choice1Of2 _method -> failwith "member reference was unexpectedly a method"
                    | Choice2Of2 field -> state, assyName, field
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            do
                let logger = loggerFactory.CreateLogger "Ldfld"
                let declaring = state.ActiveAssembly(thread).TypeDefs.[field.DeclaringType]

                logger.LogInformation (
                    "Storing in object field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    declaring.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            let currentObj, state = IlMachineState.popEvalStack thread state

            if field.Attributes.HasFlag FieldAttributes.Static then
                let staticField = state.Statics.[field.DeclaringType, assyName]
                let state = state |> IlMachineState.pushToEvalStack staticField thread
                state, WhatWeDid.Executed
            else

            let state =
                match currentObj with
                | EvalStackValue.Int32 i -> failwith "todo: int32"
                | EvalStackValue.Int64 int64 -> failwith "todo: int64"
                | EvalStackValue.NativeInt nativeIntSource -> failwith $"todo: nativeint {nativeIntSource}"
                | EvalStackValue.Float f -> failwith "todo: float"
                | EvalStackValue.ManagedPointer managedPointerSource ->
                    match managedPointerSource with
                    | ManagedPointerSource.LocalVariable (sourceThread, methodFrame, whichVar) ->
                        let currentValue =
                            state.ThreadState.[sourceThread].MethodStates.[methodFrame].LocalVariables
                                .[int<uint16> whichVar]

                        failwith $"todo: local variable {currentValue} {field}"
                    | ManagedPointerSource.Heap managedHeapAddress ->
                        match state.ManagedHeap.NonArrayObjects.TryGetValue managedHeapAddress with
                        | false, _ -> failwith $"todo: array {managedHeapAddress}"
                        | true, v -> IlMachineState.pushToEvalStack v.Fields.[field.Name] thread state
                    | ManagedPointerSource.Null -> failwith "TODO: raise NullReferenceException"
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith $"todo: {managedHeapAddress}"
                | EvalStackValue.UserDefinedValueType -> failwith "todo"

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

        | Ldflda -> failwith "TODO: Ldflda unimplemented"
        | Ldsfld ->
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to load from a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: Ldsfld - throw MissingFieldException"
            | true, field ->

            do
                let logger = loggerFactory.CreateLogger "Ldsfld"
                let declaring = state.ActiveAssembly(thread).TypeDefs.[field.DeclaringType]

                logger.LogInformation (
                    "Loading from static field {FieldAssembly}.{FieldDeclaringType}.{FieldName} (type {FieldType})",
                    declaring.Assembly.Name,
                    declaring.Name,
                    field.Name,
                    field.Signature
                )

            match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            let fieldValue, state =
                match state.Statics.TryGetValue ((field.DeclaringType, activeAssy.Name)) with
                | false, _ ->
                    // TODO: generics
                    let newVal = CliType.zeroOf ImmutableArray.Empty field.Signature

                    newVal,
                    { state with
                        Statics = state.Statics.SetItem ((field.DeclaringType, activeAssy.Name), newVal)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack fieldValue thread state
                |> IlMachineState.advanceProgramCounter thread

            state, WhatWeDid.Executed

        | Unbox_Any -> failwith "TODO: Unbox_Any unimplemented"
        | Stelem -> failwith "TODO: Stelem unimplemented"
        | Ldelem -> failwith "TODO: Ldelem unimplemented"
        | Initobj -> failwith "TODO: Initobj unimplemented"
        | Ldsflda ->
            // TODO: check whether we should throw FieldAccessException
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to load a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: Ldsflda - throw MissingFieldException"
            | true, field ->
                match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
                | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
                | NothingToDo state ->

                if TypeDefn.isManaged field.Signature then
                    match state.Statics.TryGetValue ((field.DeclaringType, activeAssy.Name)) with
                    | true, v ->
                        IlMachineState.pushToEvalStack v thread state
                        |> IlMachineState.advanceProgramCounter thread
                        |> Tuple.withRight WhatWeDid.Executed
                    | false, _ ->
                        let allocation, state =
                            state |> (failwith "TODO: Ldsflda static field allocation unimplemented")

                        state
                        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocation)) thread
                        |> Tuple.withRight WhatWeDid.Executed
                else
                    failwith "TODO: Ldsflda - push unmanaged pointer"
        | Ldftn -> failwith "TODO: Ldftn unimplemented"
        | Stobj -> failwith "TODO: Stobj unimplemented"
        | Constrained -> failwith "TODO: Constrained unimplemented"
        | Ldtoken -> failwith "TODO: Ldtoken unimplemented"
        | Cpobj -> failwith "TODO: Cpobj unimplemented"
        | Ldobj -> failwith "TODO: Ldobj unimplemented"
        | Sizeof -> failwith "TODO: Sizeof unimplemented"
        | Calli -> failwith "TODO: Calli unimplemented"
        | Unbox -> failwith "TODO: Unbox unimplemented"
        | Ldvirtftn -> failwith "TODO: Ldvirtftn unimplemented"
        | Mkrefany -> failwith "TODO: Mkrefany unimplemented"
        | Refanyval -> failwith "TODO: Refanyval unimplemented"
        | Jmp -> failwith "TODO: Jmp unimplemented"
