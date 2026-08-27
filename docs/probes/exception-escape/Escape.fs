namespace ExnSpike

open System.Collections.Generic
open System.Reflection.Metadata
open WoofWare.PawPrint

/// A type's identity for subtyping purposes. Keyed by TypeDef handle rather than by
/// namespace-and-name: a nested type carries an empty namespace and its own short name, so two
/// nested `E`s under different outer types would otherwise collide and inherit each other's base
/// chain.
[<RequireQualifiedAccess>]
type TyKey =
    /// Defined in the assembly under analysis.
    | Local of ComparableTypeDefinitionHandle
    /// Named from outside, or named by a TypeRef row. Coarser than identity — two rows may
    /// describe the same type — so it is only ever compared against clauses in the same assembly,
    /// never used to claim two things are the same type.
    | Foreign of ns : string * name : string

/// An interprocedural "which exceptions escape this method?" fixpoint, built against
/// WoofWare.PawPrint.Domain only, over a single assembly's own MethodDefs.
///
/// This is a measurement instrument, not a product. Its purpose is to say *where* an analyser
/// would need something it does not have, so every place it cannot see through widens to
/// `Unknown` and is counted under a named reason. A reason's site count is the size of that wall.
module Escape =

    /// What a method may let escape. `Unknown` is the top element: something happened that this
    /// instrument cannot see through, so no claim about absence is available.
    type EscapeSet =
        {
            Types : Set<TyKey>
            Unknown : bool
        }

    let bottom : EscapeSet =
        {
            Types = Set.empty
            Unknown = false
        }

    /// Why a method's answer is incomplete. Counted per *site*, so each number sizes the work
    /// that removing that wall would take.
    [<RequireQualifiedAccess>]
    type Incompleteness =
        /// A callee whose root type is defined in another assembly. Removing this wall means
        /// cross-assembly member resolution.
        | ForeignCallee
        /// A callee named by a MethodSpec, or by a MemberRef whose parent is an instantiation of a
        /// type in *this* assembly. Removing it means resolving an instantiation to its
        /// definition, not loading another image.
        | GenericInstantiation
        /// A `callvirt` or `ldvirtftn`: the statically named method may not be the one that runs.
        | VirtualCall
        /// `calli`, `jmp`, or a dynamic-scope operand: an indirect transfer with no metadata
        /// target at all.
        | IndirectCall
        /// An `InternalCall`, `PInvoke` or `RuntimeProvided` method. Removing this wall means a
        /// declared summary per native target.
        | NativeBody
        /// An abstract method. There is no body to summarise and no native implementation either;
        /// its answer is the join over its overrides, so this is a dispatch problem.
        | AbstractBody
        /// A `throw` whose operand this instrument cannot type.
        | UntypedThrow
        /// A `rethrow`: naming what it re-raises needs the enclosing handler's clause type.
        | Rethrow

    type Analysis =
        {
            Escaping : Dictionary<MethodDefinitionHandle, EscapeSet>
            Names : Dictionary<MethodDefinitionHandle, string>
            Reasons : Dictionary<Incompleteness, int>
            Rounds : int
        }

    let display (assy : DumpedAssembly) (k : TyKey) : string =
        match k with
        | TyKey.Local h ->
            match assy.TypeDefs.TryGetValue h.Get with
            | true, ty -> $"{ty.Namespace}.{ty.Name}"
            | _ -> "<unknown TypeDef>"
        | TyKey.Foreign (ns, n) -> if ns = "" then n else $"{ns}.{n}"

    /// Walk a `TypeDefn` to the nominal type at its root, past instantiations, arrays, pointers
    /// and modifiers. `None` when the root names no nominal type (a primitive, a generic
    /// parameter, `void`, a function pointer).
    let rec private rootOf (t : TypeDefn) : Choice<TypeRef, ResolvedTypeIdentity> option =
        match t with
        | TypeDefn.FromReference (tr, _) -> Some (Choice1Of2 tr)
        | TypeDefn.FromDefinition (id, _) -> Some (Choice2Of2 id)
        | TypeDefn.GenericInstantiation (generic, _) -> rootOf generic
        | TypeDefn.Array (elt, _) -> rootOf elt
        | TypeDefn.OneDimensionalArrayLowerBoundZero elt -> rootOf elt
        | TypeDefn.Pinned inner
        | TypeDefn.Pointer inner
        | TypeDefn.Byref inner -> rootOf inner
        | TypeDefn.Modified m -> rootOf m.Unmodified
        | TypeDefn.PrimitiveType _
        | TypeDefn.FunctionPointer _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _
        | TypeDefn.Void -> None

    /// <param name="includeImplicit">
    /// Whether opcode-raised exceptions (a null dereference, an array bound, a division) count.
    /// Sound analysis must include them; the run with them switched off measures how much of the
    /// answer they swamp.
    /// </param>
    let run (includeImplicit : bool) (assy : DumpedAssembly) : Analysis =
        let thisAssembly = assy.ThisAssemblyDefinition.Name

        // Indices. Built once; every lookup below is O(1) against one of these.
        let typeOfMethod = Dictionary<MethodDefinitionHandle, TypeDefinitionHandle> ()
        let byName = Dictionary<string * string, TypeDefinitionHandle> ()
        let allMethods = ResizeArray<MethodDefinitionHandle * MethodInfo<_, _, _>> ()
        let names = Dictionary<MethodDefinitionHandle, string> ()

        for KeyValue (th, ty) in assy.TypeDefs do
            // Only the first row wins for a duplicate (namespace, name); duplicates are exactly
            // the nested-type collision `TyKey` exists to avoid, and this index is used only to
            // canonicalise names that came from outside metadata, where a nested type cannot
            // arise.
            byName.TryAdd ((ty.Namespace, ty.Name), th) |> ignore

            for m in ty.Methods do
                match m.TryMetadata with
                | Some facts ->
                    typeOfMethod.[facts.Handle] <- th
                    allMethods.Add (facts.Handle, m)
                    names.[facts.Handle] <- $"{ty.Namespace}.{ty.Name}::{m.Name}"
                | None -> ()

        /// Canonicalise a type named by namespace and name: prefer this assembly's own TypeDef, so
        /// that an exception this table names by string and a `catch` clause naming the same type
        /// by token compare equal.
        let keyOfName (ns : string) (n : string) : TyKey =
            match byName.TryGetValue ((ns, n)) with
            | true, h -> TyKey.Local (ComparableTypeDefinitionHandle.Make h)
            | _ -> TyKey.Foreign (ns, n)

        let keyOfQualifiedName (qualified : string) : TyKey =
            let idx = qualified.LastIndexOf '.'

            if idx < 0 then
                keyOfName "" qualified
            else
                keyOfName (qualified.Substring (0, idx)) (qualified.Substring (idx + 1))

        // Base chain of every local TypeDef, keyed by handle. A chain leaves the assembly at its
        // first TypeRef base, which is recorded and then terminates the walk: a base type we
        // cannot read is a base chain we cannot follow.
        let supertypes = Dictionary<TypeDefinitionHandle, Set<TyKey>> ()

        let rec chain (h : TypeDefinitionHandle) : Set<TyKey> =
            match supertypes.TryGetValue h with
            | true, v -> v
            | _ ->

            // Seed with self before recursing, so malformed cyclic metadata terminates.
            supertypes.[h] <- Set.singleton (TyKey.Local (ComparableTypeDefinitionHandle.Make h))

            let result =
                match assy.TypeDefs.TryGetValue h with
                | false, _ -> Set.singleton (TyKey.Local (ComparableTypeDefinitionHandle.Make h))
                | true, ty ->
                    match ty.BaseType with
                    | Some (BaseTypeInfo.TypeDef bh) ->
                        Set.add (TyKey.Local (ComparableTypeDefinitionHandle.Make h)) (chain bh)
                    | Some (BaseTypeInfo.TypeRef rh) ->
                        match assy.TypeRefs.TryGetValue rh with
                        | true, tr ->
                            Set.ofList
                                [
                                    TyKey.Local (ComparableTypeDefinitionHandle.Make h)
                                    keyOfName tr.Namespace tr.Name
                                ]
                        | _ -> Set.singleton (TyKey.Local (ComparableTypeDefinitionHandle.Make h))
                    | Some (BaseTypeInfo.TypeSpec _)
                    | None -> Set.singleton (TyKey.Local (ComparableTypeDefinitionHandle.Make h))

            supertypes.[h] <- result
            result

        for KeyValue (h, _) in assy.TypeDefs do
            chain h |> ignore

        let systemException = keyOfName "System" "Exception"
        let systemObject = keyOfName "System" "Object"

        /// Does an exception whose type is `candidate` reach a `catch (catchType)`?
        let isSubtypeOf (candidate : TyKey) (catchType : TyKey) : bool =
            if catchType = systemException || catchType = systemObject then
                // These two absorb everything, so a clause naming one settles the question without
                // any chain walk. This is what lets a single-assembly instrument stay useful.
                true
            elif candidate = catchType then
                true
            else

            match candidate with
            | TyKey.Local h ->
                match supertypes.TryGetValue h.Get with
                | true, s -> Set.contains catchType s
                | _ -> false
            | TyKey.Foreign _ ->
                // The base chain of a type defined elsewhere is not readable from here. Refusing
                // to guess keeps the instrument sound; it is also the cross-assembly wall,
                // appearing in the subtype relation rather than in the call graph.
                false

        let reasons = Dictionary<Incompleteness, int> ()

        let bumpReason (r : Incompleteness) =
            match reasons.TryGetValue r with
            | true, v -> reasons.[r] <- v + 1
            | _ -> reasons.[r] <- 1

        let keyOfTypeToken (t : MetadataToken) : TyKey option =
            match t with
            | MetadataToken.TypeDefinition h -> Some (TyKey.Local (ComparableTypeDefinitionHandle.Make h))
            | MetadataToken.TypeReference h ->
                match assy.TypeRefs.TryGetValue h with
                | true, tr -> Some (keyOfName tr.Namespace tr.Name)
                | _ -> None
            | MetadataToken.TypeSpecification h ->
                match assy.TypeSpecs.TryGetValue h with
                | true, spec ->
                    match rootOf spec.Signature with
                    | Some (Choice1Of2 tr) -> Some (keyOfName tr.Namespace tr.Name)
                    | Some (Choice2Of2 _)
                    | None -> None
                | _ -> None
            | _ -> None

        /// Does the type named by this token live in another image? `None` when the token names no
        /// nominal type at all (a primitive receiver, an array, a generic parameter).
        let isForeignTypeToken (t : MetadataToken) : bool option =
            let ofTypeRef (tr : TypeRef) =
                match tr.ResolutionScope with
                // Scoped to an AssemblyRef, a TypeRef names a type in another image. Scoped to
                // this module it does not; nested in another TypeRef, the outer one decides, and
                // this instrument does not chase it, so treat it as foreign.
                | TypeRefResolutionScope.Assembly _ -> Some true
                | TypeRefResolutionScope.ModuleDef _
                | TypeRefResolutionScope.ModuleRef _ -> Some false
                | TypeRefResolutionScope.TypeRef _ -> Some true

            match t with
            | MetadataToken.TypeDefinition _ -> Some false
            | MetadataToken.TypeReference h ->
                match assy.TypeRefs.TryGetValue h with
                | true, tr -> ofTypeRef tr
                | _ -> None
            | MetadataToken.TypeSpecification h ->
                match assy.TypeSpecs.TryGetValue h with
                | true, spec ->
                    match rootOf spec.Signature with
                    | Some (Choice1Of2 tr) -> ofTypeRef tr
                    | Some (Choice2Of2 id) -> Some (id.AssemblyFullName <> thisAssembly.FullName)
                    | None -> None
                | _ -> None
            | _ -> None

        // Per-method, precomputed once. Everything is offset-tagged so catch scoping applies to
        // callee-propagated exceptions exactly as it does to locally-raised ones.
        let localRaises = Dictionary<MethodDefinitionHandle, (int * TyKey option) list> ()

        let localCalls =
            Dictionary<MethodDefinitionHandle, (int * MethodDefinitionHandle) list> ()

        let opaqueAt = Dictionary<MethodDefinitionHandle, int list> ()
        let regionsOf = Dictionary<MethodDefinitionHandle, ExceptionRegion list> ()

        for h, m in allMethods do
            match m.Body with
            | MethodBody.Il instrs ->
                let ops = instrs.Instructions |> List.toArray
                let raises = ResizeArray<int * TyKey option> ()
                let calls = ResizeArray<int * MethodDefinitionHandle> ()
                let opaque = ResizeArray<int> ()

                /// Record an operation this instrument cannot see through.
                let opaqueHere (off : int) (reason : Incompleteness) =
                    bumpReason reason
                    opaque.Add off

                for i in 0 .. ops.Length - 1 do
                    let op, off = ops.[i]

                    // 1. What the instruction raises by itself. An opcode this table declines to
                    //    classify is opaque whether or not implicit faults are switched on:
                    //    "unmodelled" is never a claim of safety.
                    match OpcodeFaults.ofIlOp op with
                    | OpcodeFaults.Unmodelled ->
                        match op with
                        | IlOp.Nullary NullaryIlOp.Rethrow -> opaqueHere off Incompleteness.Rethrow
                        | _ -> opaqueHere off Incompleteness.UntypedThrow
                    | OpcodeFaults.Raises xs ->
                        if includeImplicit then
                            for x in xs do
                                raises.Add (off, Some (keyOfQualifiedName (OpcodeFault.typeName x)))

                    // 2. What the instruction throws explicitly.
                    match op with
                    | IlOp.Nullary NullaryIlOp.Throw ->
                        let prev = if i > 0 then Some (fst ops.[i - 1]) else None

                        let thrown =
                            match prev with
                            | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Newobj,
                                                             MetadataOperand.FromMetadata t)) ->
                                match t.Token with
                                | MetadataToken.MethodDef mh ->
                                    match typeOfMethod.TryGetValue mh with
                                    | true, th -> Some (TyKey.Local (ComparableTypeDefinitionHandle.Make th))
                                    | _ -> None
                                | MetadataToken.MemberReference mrh ->
                                    match assy.Members.TryGetValue mrh with
                                    | true, mr -> keyOfTypeToken mr.Parent
                                    | _ -> None
                                | _ -> None
                            | _ -> None

                        match thrown with
                        | Some _ -> ()
                        | None -> opaqueHere off Incompleteness.UntypedThrow

                        raises.Add (off, thrown)
                    | _ -> ()

                    // 3. What the instruction calls.
                    match op with
                    | IlOp.UnaryMetadataToken (mop, operand) ->
                        // Count the virtual-dispatch wall from the opcode, independently of how
                        // the token happened to be encoded.
                        match mop with
                        | UnaryMetadataTokenIlOp.Callvirt
                        | UnaryMetadataTokenIlOp.Ldvirtftn -> opaqueHere off Incompleteness.VirtualCall
                        | _ -> ()

                        match mop with
                        | UnaryMetadataTokenIlOp.Calli
                        | UnaryMetadataTokenIlOp.Jmp -> opaqueHere off Incompleteness.IndirectCall
                        | UnaryMetadataTokenIlOp.Call
                        | UnaryMetadataTokenIlOp.Callvirt
                        | UnaryMetadataTokenIlOp.Newobj ->
                            match operand with
                            | MetadataOperand.FromDynamicScope _ -> opaqueHere off Incompleteness.IndirectCall
                            | MetadataOperand.FromMetadata t ->
                                match t.Token with
                                | MetadataToken.MethodDef mh -> calls.Add (off, mh)
                                | MetadataToken.MethodSpecification _ ->
                                    opaqueHere off Incompleteness.GenericInstantiation
                                | MetadataToken.MemberReference mrh ->
                                    // A MemberRef's *parent* decides which wall this is. In a
                                    // self-contained image like CoreLib nearly every one is an
                                    // instantiation of a local type, not a foreign call at all.
                                    let reason =
                                        match assy.Members.TryGetValue mrh with
                                        | true, mr ->
                                            match isForeignTypeToken mr.Parent with
                                            | Some true -> Incompleteness.ForeignCallee
                                            | Some false
                                            | None -> Incompleteness.GenericInstantiation
                                        | _ -> Incompleteness.ForeignCallee

                                    opaqueHere off reason
                                | _ -> opaqueHere off Incompleteness.ForeignCallee
                        | _ -> ()
                    | _ -> ()

                localRaises.[h] <- List.ofSeq raises
                localCalls.[h] <- List.ofSeq calls
                opaqueAt.[h] <- List.ofSeq opaque
                regionsOf.[h] <- List.ofSeq instrs.ExceptionRegions
            | MethodBody.Abstract ->
                bumpReason Incompleteness.AbstractBody
                localRaises.[h] <- []
                localCalls.[h] <- []
                opaqueAt.[h] <- [ 0 ]
                regionsOf.[h] <- []
            | MethodBody.InternalCall
            | MethodBody.PInvoke
            | MethodBody.RuntimeProvided _ ->
                bumpReason Incompleteness.NativeBody
                localRaises.[h] <- []
                localCalls.[h] <- []
                opaqueAt.[h] <- [ 0 ]
                regionsOf.[h] <- []

        /// Does an exception of `ty` raised at `off` escape this method's handlers?
        /// Conservative: `finally` and `fault` never stop propagation, and a `filter` may decline,
        /// so neither does that.
        let escapesHandlers (regions : ExceptionRegion list) (off : int) (ty : TyKey option) : bool =
            regions
            |> List.exists (fun r ->
                match r with
                | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata t, o) ->
                    if off >= o.TryOffset && off < o.TryOffset + o.TryLength then
                        match keyOfTypeToken t, ty with
                        | Some catchTy, Some raisedTy -> isSubtypeOf raisedTy catchTy
                        | Some catchTy, None ->
                            // An exception we could not name is absorbed only by a clause that
                            // absorbs everything.
                            catchTy = systemException || catchTy = systemObject
                        | None, _ -> false
                    else
                        false
                | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope _, _)
                | ExceptionRegion.Filter _
                | ExceptionRegion.Finally _
                | ExceptionRegion.Fault _ -> false
            )
            |> not

        let escaping = Dictionary<MethodDefinitionHandle, EscapeSet> ()

        for h, _ in allMethods do
            escaping.[h] <- bottom

        // A method's own contribution does not change under iteration, so compute it once.
        let seeds = Dictionary<MethodDefinitionHandle, EscapeSet> ()

        for h, _ in allMethods do
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

            for off in opaqueAt.[h] do
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

            for h, _ in allMethods do
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
