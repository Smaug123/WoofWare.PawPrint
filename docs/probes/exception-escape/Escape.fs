namespace ExnSpike

open System.Collections.Generic
open System.Reflection.Metadata
open Microsoft.Extensions.Logging.Abstractions
open WoofWare.PawPrint

/// An intraprocedural-plus-fixpoint "which exceptions escape this method?" analysis, built
/// against WoofWare.PawPrint.Domain only, over a single assembly's own MethodDefs.
///
/// This is a measurement instrument, not a product. It is deliberately *unsound in exactly
/// two named ways* so that the size of the remaining gap can be counted rather than guessed:
/// a call whose callee lives outside this assembly, and a `callvirt` whose target is not the
/// statically named method, both contribute `Unknown` rather than being resolved.
module Escape =

    /// What a method may let escape. `Unknown` is the top element: something happened that
    /// this instrument cannot see through, so no claim about absence is available.
    type EscapeSet =
        {
            Types : Set<string>
            Unknown : bool
        }

    let bottom : EscapeSet =
        {
            Types = Set.empty
            Unknown = false
        }

    let join (a : EscapeSet) (b : EscapeSet) : EscapeSet =
        {
            Types = Set.union a.Types b.Types
            Unknown = a.Unknown || b.Unknown
        }

    /// Why a method's answer is incomplete, counted so the instrument can report its own envelope.
    [<RequireQualifiedAccess>]
    type Incompleteness =
        | ForeignCallee
        | VirtualCall
        | GenericInstantiation
        | NoIlBody
        | UntypedThrow
        | Rethrow

    type Analysis =
        {
            /// Escaping set per method, at the fixpoint.
            Escaping : Dictionary<MethodDefinitionHandle, EscapeSet>
            Names : Dictionary<MethodDefinitionHandle, string>
            Reasons : Dictionary<Incompleteness, int>
            Rounds : int
        }

    /// Name-keyed subtype relation over this assembly's own TypeDefs. A base type that is a
    /// TypeRef leaves the chain (it points outside), which for CoreLib means only `System.Object`
    /// itself and a handful of interop roots.
    let private buildSupertypes (assy : DumpedAssembly) : Dictionary<string, Set<string>> =
        let nameOf (h : TypeDefinitionHandle) =
            match assy.TypeDefs.TryGetValue h with
            | true, ty -> Some $"{ty.Namespace}.{ty.Name}"
            | _ -> None

        let direct = Dictionary<string, string option> ()

        for KeyValue (h, ty) in assy.TypeDefs do
            let me = $"{ty.Namespace}.{ty.Name}"

            let parent =
                match ty.BaseType with
                | Some (BaseTypeInfo.TypeDef bh) -> nameOf bh
                | Some (BaseTypeInfo.TypeRef rh) ->
                    match assy.TypeRefs.TryGetValue rh with
                    | true, tr -> Some $"{tr.Namespace}.{tr.Name}"
                    | _ -> None
                | Some (BaseTypeInfo.TypeSpec _) -> None
                | None -> None

            direct.[me] <- parent
            ignore h

        let cache = Dictionary<string, Set<string>> ()

        let rec chain (n : string) : Set<string> =
            match cache.TryGetValue n with
            | true, v -> v
            | _ ->
                // Guard against a cycle in malformed metadata: seed with self before recursing.
                cache.[n] <- Set.singleton n

                let result =
                    match direct.TryGetValue n with
                    | true, Some p -> Set.add n (chain p)
                    | _ -> Set.singleton n

                cache.[n] <- result
                result

        for KeyValue (k, _) in direct do
            chain k |> ignore

        cache

    /// <param name="includeImplicit">
    /// Whether opcode-raised exceptions (a null dereference, an array bound, a division) count.
    /// Sound analysis must include them; the run with them switched off measures how much of the
    /// answer they swamp.
    /// </param>
    let run (includeImplicit : bool) (assy : DumpedAssembly) : Analysis =
        let supertypes = buildSupertypes assy

        let isSubtypeOf (candidate : string) (catchType : string) : bool =
            if catchType = "System.Object" || catchType = "System.Exception" then
                true
            elif candidate = catchType then
                true
            else
                match supertypes.TryGetValue candidate with
                | true, s -> Set.contains catchType s
                | _ -> false

        let ownerOfMethod = Dictionary<MethodDefinitionHandle, string> ()

        let allMethods =
            ResizeArray<MethodDefinitionHandle * TypeInfo<_, _> * MethodInfo<_, _, _>> ()

        for KeyValue (_, ty) in assy.TypeDefs do
            for m in ty.Methods do
                match m.TryMetadata with
                | Some facts ->
                    ownerOfMethod.[facts.Handle] <- $"{ty.Namespace}.{ty.Name}"
                    allMethods.Add (facts.Handle, ty, m)
                | None -> ()

        let names = Dictionary<MethodDefinitionHandle, string> ()

        for h, ty, m in allMethods do
            names.[h] <- $"{ty.Namespace}.{ty.Name}::{m.Name}"

        let reasons = Dictionary<Incompleteness, int> ()

        let bumpReason (r : Incompleteness) =
            match reasons.TryGetValue r with
            | true, v -> reasons.[r] <- v + 1
            | _ -> reasons.[r] <- 1

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
            | _ -> None

        // Per-method, precomputed once: the raises this body performs itself (offset-tagged),
        // and the same-assembly callees it invokes (offset-tagged, so catch scoping applies).
        // `RaisedHere` entries of `None` mean "some exception whose type we cannot name".
        let localRaises = Dictionary<MethodDefinitionHandle, (int * string option) list> ()

        let localCalls =
            Dictionary<MethodDefinitionHandle, (int * MethodDefinitionHandle) list> ()

        let foreignAt = Dictionary<MethodDefinitionHandle, int list> ()
        let regionsOf = Dictionary<MethodDefinitionHandle, ExceptionRegion list> ()

        for h, _ty, m in allMethods do
            match m.Body with
            | MethodBody.Il instrs ->
                let ops = instrs.Instructions |> List.toArray
                let raises = ResizeArray<int * string option> ()
                let calls = ResizeArray<int * MethodDefinitionHandle> ()
                let foreign = ResizeArray<int> ()

                for i in 0 .. ops.Length - 1 do
                    let op, off = ops.[i]

                    match op with
                    | IlOp.Nullary NullaryIlOp.Throw ->
                        let prev = if i > 0 then Some (fst ops.[i - 1]) else None

                        match prev with
                        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj, MetadataOperand.FromMetadata t)) ->
                            let n =
                                match t.Token with
                                | MetadataToken.MethodDef mh ->
                                    match ownerOfMethod.TryGetValue mh with
                                    | true, x -> Some x
                                    | _ -> None
                                | MetadataToken.MemberReference mrh ->
                                    match assy.Members.TryGetValue mrh with
                                    | true, mr -> nameOfTypeToken mr.Parent
                                    | _ -> None
                                | _ -> None

                            match n with
                            | Some _ -> ()
                            | None -> bumpReason Incompleteness.UntypedThrow

                            raises.Add (off, n)
                        | _ ->
                            bumpReason Incompleteness.UntypedThrow
                            raises.Add (off, None)
                    | IlOp.Nullary NullaryIlOp.Rethrow ->
                        bumpReason Incompleteness.Rethrow
                        raises.Add (off, None)
                    | IlOp.Nullary n ->
                        if includeImplicit then
                            for x in Implicit.ofNullary n do
                                raises.Add (off, Some x)
                    | IlOp.UnaryMetadataToken (mop, operand) ->
                        if includeImplicit then
                            for x in Implicit.ofUnaryMetadata mop do
                                raises.Add (off, Some x)

                        match mop with
                        | UnaryMetadataTokenIlOp.Call
                        | UnaryMetadataTokenIlOp.Callvirt
                        | UnaryMetadataTokenIlOp.Newobj ->
                            match operand with
                            | MetadataOperand.FromMetadata t ->
                                match t.Token with
                                | MetadataToken.MethodDef mh ->
                                    // A `callvirt` on a same-assembly MethodDef still may dispatch
                                    // to an override this instrument does not enumerate.
                                    match mop with
                                    | UnaryMetadataTokenIlOp.Callvirt ->
                                        bumpReason Incompleteness.VirtualCall
                                        foreign.Add off
                                    | _ -> ()

                                    calls.Add (off, mh)
                                | MetadataToken.MemberReference _ ->
                                    bumpReason Incompleteness.ForeignCallee
                                    foreign.Add off
                                | MetadataToken.MethodSpecification _ ->
                                    bumpReason Incompleteness.GenericInstantiation
                                    foreign.Add off
                                | _ ->
                                    bumpReason Incompleteness.ForeignCallee
                                    foreign.Add off
                            | MetadataOperand.FromDynamicScope _ ->
                                bumpReason Incompleteness.ForeignCallee
                                foreign.Add off
                        | _ -> ()
                    | _ -> ()

                localRaises.[h] <- List.ofSeq raises
                localCalls.[h] <- List.ofSeq calls
                foreignAt.[h] <- List.ofSeq foreign
                regionsOf.[h] <- List.ofSeq instrs.ExceptionRegions
            | _ ->
                bumpReason Incompleteness.NoIlBody
                localRaises.[h] <- []
                localCalls.[h] <- []
                foreignAt.[h] <- [ 0 ]
                regionsOf.[h] <- []

        // Does an exception of `ty` raised at `off` escape this method's handlers?
        // Conservative: a `finally`/`fault` never stops propagation; a `filter` might not run,
        // so it never stops propagation either.
        let escapesHandlers (regions : ExceptionRegion list) (off : int) (ty : string option) : bool =
            let caught =
                regions
                |> List.exists (fun r ->
                    match r with
                    | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata t, o) ->
                        if off >= o.TryOffset && off < o.TryOffset + o.TryLength then
                            match nameOfTypeToken t, ty with
                            | Some catchTy, Some raisedTy -> isSubtypeOf raisedTy catchTy
                            | Some catchTy, None -> catchTy = "System.Object" || catchTy = "System.Exception"
                            | None, _ -> false
                        else
                            false
                    | _ -> false
                )

            not caught

        let escaping = Dictionary<MethodDefinitionHandle, EscapeSet> ()

        for h, _, _ in allMethods do
            escaping.[h] <- bottom

        // Seed each method with its own raises, filtered through its handlers. This part does not
        // change under iteration, so compute it once.
        let seeds = Dictionary<MethodDefinitionHandle, EscapeSet> ()

        for h, _, _ in allMethods do
            let regions = regionsOf.[h]
            let mutable acc = bottom

            for off, ty in localRaises.[h] do
                if escapesHandlers regions off ty then
                    match ty with
                    | Some t ->
                        acc <-
                            { acc with
                                Types = Set.add t acc.Types
                            }
                    | None ->
                        acc <-
                            { acc with
                                Unknown = true
                            }

            for off in foreignAt.[h] do
                if escapesHandlers regions off None then
                    acc <-
                        { acc with
                            Unknown = true
                        }

            seeds.[h] <- acc

        let mutable changed = true
        let mutable rounds = 0

        while changed do
            changed <- false
            rounds <- rounds + 1

            for h, _, _ in allMethods do
                let regions = regionsOf.[h]
                let mutable acc = seeds.[h]

                for off, callee in localCalls.[h] do
                    match escaping.TryGetValue callee with
                    | true, calleeSet ->
                        for t in calleeSet.Types do
                            if escapesHandlers regions off (Some t) then
                                acc <-
                                    { acc with
                                        Types = Set.add t acc.Types
                                    }

                        if calleeSet.Unknown && escapesHandlers regions off None then
                            acc <-
                                { acc with
                                    Unknown = true
                                }
                    | _ -> ()

                let before = escaping.[h]

                if before.Types <> acc.Types || before.Unknown <> acc.Unknown then
                    escaping.[h] <- acc
                    changed <- true

        {
            Escaping = escaping
            Names = names
            Reasons = reasons
            Rounds = rounds
        }
