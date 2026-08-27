namespace ExnSpike

open System.Collections.Generic
open WoofWare.PawPrint

/// A census of the raw material an exception-escape analysis has to work from, measured against a
/// real assembly. Answers "how much of the answer is syntactically apparent, and where exactly
/// does the analysis need something it does not have?"
module Census =

    type private Counters () =
        let d = Dictionary<string, int> ()

        member _.Bump (k : string) =
            match d.TryGetValue k with
            | true, v -> d.[k] <- v + 1
            | _ -> d.[k] <- 1

        member _.Items =
            d
            |> Seq.sortByDescending (fun kv -> kv.Value)
            |> Seq.map (fun kv -> kv.Key, kv.Value)
            |> List.ofSeq

    let run (assy : DumpedAssembly) : unit =
        let ownerOfMethod =
            Dictionary<System.Reflection.Metadata.MethodDefinitionHandle, string> ()

        for KeyValue (_, ty) in assy.TypeDefs do
            for m in ty.Methods do
                match m.TryMetadata with
                | Some facts -> ownerOfMethod.[facts.Handle] <- $"{ty.Namespace}.{ty.Name}"
                | None -> ()

        let bodyKinds = Counters ()
        let throwOperandKinds = Counters ()
        let catchKinds = Counters ()
        let calleeKinds = Counters ()
        let thrownTypes = Counters ()
        let implicitCounts = Counters ()
        let opcodeKinds = Counters ()

        let mutable methodsTotal = 0
        let mutable ilMethods = 0
        let mutable throwSites = 0
        let mutable rethrowSites = 0
        let mutable methodsWithHandlers = 0
        let mutable callSites = 0
        let mutable indirectSites = 0
        let mutable methodsWithImplicit = 0

        let nameOfTypeToken (t : MetadataToken) : string option =
            match t with
            | MetadataToken.TypeDefinition h ->
                match assy.TypeDefs.TryGetValue h with
                | true, ty -> Some $"{ty.Namespace}.{ty.Name}"
                | _ -> None
            | MetadataToken.TypeReference h ->
                match assy.TypeRefs.TryGetValue h with
                | true, tr -> Some $"{tr.Namespace}.{tr.Name}"
                | _ -> None
            | MetadataToken.TypeSpecification _ -> Some "<TypeSpec>"
            | _ -> None

        let declaringTypeOfCallee (t : MetadataToken) : string option =
            match t with
            | MetadataToken.MethodDef h ->
                match ownerOfMethod.TryGetValue h with
                | true, n -> Some n
                | _ -> None
            | MetadataToken.MemberReference h ->
                match assy.Members.TryGetValue h with
                | true, mr -> nameOfTypeToken mr.Parent
                | _ -> None
            | MetadataToken.MethodSpecification _ -> Some "<MethodSpec>"
            | _ -> None

        for KeyValue (_, ty) in assy.TypeDefs do
            for m in ty.Methods do
                methodsTotal <- methodsTotal + 1

                match m.Body with
                | MethodBody.InternalCall -> bodyKinds.Bump "InternalCall"
                | MethodBody.PInvoke -> bodyKinds.Bump "PInvoke"
                | MethodBody.RuntimeProvided k -> bodyKinds.Bump $"RuntimeProvided({k})"
                | MethodBody.Abstract -> bodyKinds.Bump "Abstract"
                | MethodBody.Il instrs ->

                bodyKinds.Bump "Il"
                ilMethods <- ilMethods + 1
                let ops = instrs.Instructions |> List.toArray

                if not instrs.ExceptionRegions.IsEmpty then
                    methodsWithHandlers <- methodsWithHandlers + 1

                for r in instrs.ExceptionRegions do
                    match r with
                    | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata t, _) ->
                        match nameOfTypeToken t with
                        | Some n ->
                            catchKinds.Bump "resolved-to-name"
                            catchKinds.Bump $"  {n}"
                        | None -> catchKinds.Bump "unresolvable"
                    | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope _, _) ->
                        catchKinds.Bump "dynamic-scope"
                    | ExceptionRegion.Filter _ -> catchKinds.Bump "filter"
                    | ExceptionRegion.Finally _ -> catchKinds.Bump "finally"
                    | ExceptionRegion.Fault _ -> catchKinds.Bump "fault"

                let mutable sawImplicit = false

                for i in 0 .. ops.Length - 1 do
                    let op, _off = ops.[i]

                    match OpcodeFaults.ofIlOp op with
                    | OpcodeFaults.Unmodelled -> opcodeKinds.Bump "unmodelled opcode"
                    | OpcodeFaults.Raises [] -> ()
                    | OpcodeFaults.Raises xs ->
                        sawImplicit <- true

                        for x in xs do
                            implicitCounts.Bump (OpcodeFault.typeName x)

                    match op with
                    | IlOp.Nullary NullaryIlOp.Throw ->
                        throwSites <- throwSites + 1
                        let prev = if i > 0 then Some (fst ops.[i - 1]) else None

                        match prev with
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, MetadataOperand.FromMetadata t)) ->
                            match declaringTypeOfCallee t.Token with
                            | Some n ->
                                throwOperandKinds.Bump "newobj immediately before"
                                thrownTypes.Bump n
                            | None -> throwOperandKinds.Bump "newobj but unnameable"
                        | Some (IlOp.UnaryConst (UnaryConstIlOp.Ldloc _)) ->
                            throwOperandKinds.Bump "ldloc (needs dataflow)"
                        | Some (IlOp.UnaryConst (UnaryConstIlOp.Ldarg _)) ->
                            throwOperandKinds.Bump "ldarg (needs dataflow)"
                        | Some (IlOp.Nullary NullaryIlOp.Dup) -> throwOperandKinds.Bump "dup (needs dataflow)"
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Call, _)) ->
                            throwOperandKinds.Bump "call (needs callee return type)"
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, _)) ->
                            throwOperandKinds.Bump "ldfld (needs dataflow)"
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldsfld, _)) ->
                            throwOperandKinds.Bump "ldsfld (needs dataflow)"
                        | Some other -> throwOperandKinds.Bump $"other: %O{other}"
                        | None -> throwOperandKinds.Bump "no predecessor"
                    | IlOp.Nullary NullaryIlOp.Rethrow -> rethrowSites <- rethrowSites + 1
                    | IlOp.UnaryMetadataToken (mop, operand) ->
                        match mop with
                        // Only `calli` is indirect. `jmp` carries a method token like any other
                        // named call, and `Escape.run` follows it as one.
                        | UnaryMetadataTokenIlOp.Calli ->
                            indirectSites <- indirectSites + 1
                            calleeKinds.Bump $"%O{mop} (indirect: no metadata target)"
                        | UnaryMetadataTokenIlOp.Call
                        | UnaryMetadataTokenIlOp.Callvirt
                        | UnaryMetadataTokenIlOp.Jmp
                        | UnaryMetadataTokenIlOp.Newobj ->
                            callSites <- callSites + 1

                            match operand with
                            | MetadataOperand.FromDynamicScope _ -> calleeKinds.Bump "dynamic-scope operand"
                            | MetadataOperand.FromMetadata t ->
                                calleeKinds.Bump (
                                    match t.Token with
                                    | MetadataToken.MethodDef _ -> "MethodDef (same assembly)"
                                    | MetadataToken.MemberReference _ -> "MemberRef (parent decides; see below)"
                                    | MetadataToken.MethodSpecification _ -> "MethodSpec (generic instantiation)"
                                    | _ -> "other"
                                )

                            match mop with
                            | UnaryMetadataTokenIlOp.Callvirt -> calleeKinds.Bump "  ...of which callvirt"
                            | _ -> ()
                        | _ -> ()
                    | _ -> ()

                if sawImplicit then
                    methodsWithImplicit <- methodsWithImplicit + 1

        // MemberRef parents, which is what decides whether a MemberRef callee is a foreign call or
        // merely an instantiation of a local type.
        let memberRefParents = Counters ()

        for KeyValue (_, mr) in assy.Members do
            match mr.Parent with
            | MetadataToken.TypeDefinition _ -> memberRefParents.Bump "TypeDef (this assembly)"
            | MetadataToken.TypeReference h ->
                match assy.TypeRefs.TryGetValue h with
                | true, tr ->
                    match tr.ResolutionScope with
                    | TypeRefResolutionScope.Assembly _ -> memberRefParents.Bump "TypeRef scoped to an AssemblyRef"
                    | TypeRefResolutionScope.ModuleDef _
                    | TypeRefResolutionScope.ModuleRef _ -> memberRefParents.Bump "TypeRef scoped to a module"
                    | TypeRefResolutionScope.TypeRef _ -> memberRefParents.Bump "TypeRef nested in a TypeRef"
                | _ -> memberRefParents.Bump "TypeRef (unresolvable)"
            | MetadataToken.TypeSpecification _ -> memberRefParents.Bump "TypeSpec (an instantiation)"
            | MetadataToken.MethodDef _ -> memberRefParents.Bump "MethodDef (vararg call site)"
            | MetadataToken.ModuleReference _ -> memberRefParents.Bump "ModuleRef"
            | _ -> memberRefParents.Bump "other"

        let pct (n : int) (d : int) =
            if d = 0 then 0.0 else 100.0 * float n / float d

        printfn "methods: %d (IL bodies: %d, %.1f%%)" methodsTotal ilMethods (pct ilMethods methodsTotal)
        printfn ""
        printfn "-- body kinds --"

        for k, v in bodyKinds.Items do
            printfn "  %-40s %6d  %5.1f%%" k v (pct v methodsTotal)

        printfn ""

        printfn
            "IL methods with at least one implicitly-raising opcode: %d (%.1f%%)"
            methodsWithImplicit
            (pct methodsWithImplicit ilMethods)

        printfn
            "IL methods with any exception region: %d (%.1f%%)"
            methodsWithHandlers
            (pct methodsWithHandlers ilMethods)

        printfn
            "throw sites: %d ; rethrow sites: %d ; call sites: %d ; indirect transfers: %d"
            throwSites
            rethrowSites
            callSites
            indirectSites

        printfn ""
        printfn "-- what precedes a `throw` (can we name the thrown type syntactically?) --"

        for k, v in throwOperandKinds.Items do
            printfn "  %-50s %6d  %5.1f%%" k v (pct v throwSites)

        printfn ""
        printfn "-- exception regions --"

        for k, v in catchKinds.Items |> List.filter (fun (k, _) -> not (k.StartsWith "  ")) do
            printfn "  %-40s %6d" k v

        printfn ""
        printfn "-- top 10 catch types --"

        for k, v in
            catchKinds.Items
            |> List.filter (fun (k, _) -> k.StartsWith "  ")
            |> List.truncate 10 do
            printfn "  %-45s %6d" (k.Trim ()) v

        printfn ""
        printfn "-- callee token kinds --"

        for k, v in calleeKinds.Items do
            printfn "  %-50s %6d  %5.1f%%" k v (pct v callSites)

        printfn ""
        printfn "-- MemberRef parents, over the whole MemberRef table --"

        for k, v in memberRefParents.Items do
            printfn "  %-45s %6d" k v

        printfn ""
        printfn "-- implicit (opcode-raised) exception site counts --"

        for k, v in implicitCounts.Items do
            printfn "  %-45s %6d" k v

        for k, v in opcodeKinds.Items do
            printfn "  %-45s %6d" k v

        printfn ""
        printfn "-- top 12 directly-thrown types (newobj;throw) --"

        for k, v in thrownTypes.Items |> List.truncate 12 do
            printfn "  %-55s %6d" k v
