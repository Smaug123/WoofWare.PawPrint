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

    /// How far a branch instruction jumps from the end of itself, or `None` if it is not a branch.
    ///
    /// Exhaustive with no wildcard: a branch opcode added to `UnaryConstIlOp` must be classified
    /// here rather than silently reading as "not a branch", which would let a `throw` be typed from
    /// an instruction control never fell through from.
    let private branchDelta (op : UnaryConstIlOp) : int option =
        match op with
        | UnaryConstIlOp.Br d
        | UnaryConstIlOp.Brfalse d
        | UnaryConstIlOp.Brtrue d
        | UnaryConstIlOp.Beq d
        | UnaryConstIlOp.Blt d
        | UnaryConstIlOp.Ble d
        | UnaryConstIlOp.Bgt d
        | UnaryConstIlOp.Bge d
        | UnaryConstIlOp.Bne_un d
        | UnaryConstIlOp.Bge_un d
        | UnaryConstIlOp.Bgt_un d
        | UnaryConstIlOp.Ble_un d
        | UnaryConstIlOp.Blt_un d
        | UnaryConstIlOp.Leave d -> Some (int d)
        | UnaryConstIlOp.Br_s d
        | UnaryConstIlOp.Brfalse_s d
        | UnaryConstIlOp.Brtrue_s d
        | UnaryConstIlOp.Beq_s d
        | UnaryConstIlOp.Blt_s d
        | UnaryConstIlOp.Ble_s d
        | UnaryConstIlOp.Bgt_s d
        | UnaryConstIlOp.Bge_s d
        | UnaryConstIlOp.Bne_un_s d
        | UnaryConstIlOp.Bge_un_s d
        | UnaryConstIlOp.Bgt_un_s d
        | UnaryConstIlOp.Ble_un_s d
        | UnaryConstIlOp.Blt_un_s d
        | UnaryConstIlOp.Leave_s d -> Some (int d)
        | UnaryConstIlOp.Stloc _
        | UnaryConstIlOp.Stloc_s _
        | UnaryConstIlOp.Ldc_I8 _
        | UnaryConstIlOp.Ldc_I4 _
        | UnaryConstIlOp.Ldc_R4 _
        | UnaryConstIlOp.Ldc_R8 _
        | UnaryConstIlOp.Ldc_I4_s _
        | UnaryConstIlOp.Ldloc_s _
        | UnaryConstIlOp.Ldloca_s _
        | UnaryConstIlOp.Ldarga _
        | UnaryConstIlOp.Ldarg_s _
        | UnaryConstIlOp.Ldarga_s _
        | UnaryConstIlOp.Starg_s _
        | UnaryConstIlOp.Starg _
        | UnaryConstIlOp.Unaligned _
        | UnaryConstIlOp.Ldloc _
        | UnaryConstIlOp.Ldloca _
        | UnaryConstIlOp.Ldarg _ -> None

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

    /// How to run the analysis. A record rather than positional flags: there are four of them now,
    /// they are all `bool`-shaped, and three of the four are things a caller gets wrong silently.
    type Options =
        {
            /// Whether opcode-raised faults (a null dereference, an array bound, a division) count.
            /// A sound analysis must include them; the run with them off measures how much of the
            /// answer they swamp.
            IncludeImplicit : bool
            /// Whether a `.cctor` touching its own type's members picks up `TypeInitialization`
            /// from doing so. It should not — the initializer it would trigger is the one already
            /// running (ECMA-335 I.8.9.5) — but the unrefined run is kept so the refinement's size
            /// is measured rather than assumed.
            PruneSelfInitialisation : bool
            /// Whether an invocation whose target type has no `.cctor` picks up
            /// `TypeInitialization`. It cannot: there is no initializer to fail. Exact, and pure
            /// metadata.
            PruneAbsentInitialisers : bool
            /// Kinds dropped from the *report*. Deliberately unsound — see
            /// `OpcodeFaults.excludingKind`. A result produced with this non-empty may not be read
            /// as a proof that the dropped faults cannot happen.
            ExcludeKinds : FaultKind list
        }

    /// Everything on, nothing hidden: the honest answer.
    let sound : Options =
        {
            IncludeImplicit = true
            PruneSelfInitialisation = true
            PruneAbsentInitialisers = true
            ExcludeKinds = []
        }

    let run (options : Options) (assy : DumpedAssembly) : Analysis =
        let thisAssembly = assy.ThisAssemblyDefinition.Name
        let includeImplicit = options.IncludeImplicit
        let pruneSelfInitialisation = options.PruneSelfInitialisation

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

        // Which TypeDef declares each field, which methods are `.cctor`s, and which types have one
        // at all. All three serve the type-initialisation prunes below.
        let typeOfField = Dictionary<FieldDefinitionHandle, TypeDefinitionHandle> ()
        let cctorsOwner = Dictionary<MethodDefinitionHandle, TypeDefinitionHandle> ()
        let hasInitialiser = HashSet<TypeDefinitionHandle> ()

        for KeyValue (th, ty) in assy.TypeDefs do
            for f in ty.Fields do
                typeOfField.[f.Handle] <- th

            for m in ty.Methods do
                match m.TryMetadata with
                | Some facts when m.Name = ".cctor" && m.IsStatic ->
                    cctorsOwner.[facts.Handle] <- th
                    hasInitialiser.Add th |> ignore
                | _ -> ()

        /// Canonicalise a type named by namespace and name: prefer this assembly's own TypeDef, so
        /// that an exception this table names by string and a `catch` clause naming the same type
        /// by token compare equal.
        ///
        /// Only correct for a name that is *known* to denote a local type. A name that arrived from
        /// a TypeRef has a resolution scope saying otherwise, and `keyOfTypeRef` is what respects
        /// it.
        let keyOfName (ns : string) (n : string) : TyKey =
            match byName.TryGetValue ((ns, n)) with
            | true, h -> TyKey.Local (ComparableTypeDefinitionHandle.Make h)
            | _ -> TyKey.Foreign (ns, n)

        /// The identity of the type a TypeRef row names.
        ///
        /// A TypeRef carries a resolution scope, and only `ModuleDef` — "this module" — says the
        /// definition is here. Canonicalising on the name alone would key an *external*
        /// `System.NullReferenceException` to a same-named type the analysed assembly happens to
        /// declare, so a `catch` for that local shadow would absorb the real exception and the
        /// probe would report a false negative. A shadow named `System.Exception` would be worse
        /// still: `isSubtypeOf` treats that name as universal, so one local declaration would make
        /// every `catch` absorb everything.
        ///
        /// Declining to canonicalise costs precision in the other direction — a TypeRef through a
        /// facade to a type this assembly does define keys `Foreign` and no longer matches its own
        /// TypeDef — but that direction over-reports escapes, which is the safe one for an
        /// instrument whose whole purpose is to bound what escapes.
        let keyOfTypeRef (tr : TypeRef) : TyKey =
            match tr.ResolutionScope with
            | TypeRefResolutionScope.ModuleDef _ -> keyOfName tr.Namespace tr.Name
            | TypeRefResolutionScope.Assembly _
            | TypeRefResolutionScope.ModuleRef _
            | TypeRefResolutionScope.TypeRef _ -> TyKey.Foreign (tr.Namespace, tr.Name)

        // Is the image under analysis the one that *defines* the exceptions an opcode raises?
        let analysingCorelib = thisAssembly.Name = "System.Private.CoreLib"

        /// The identity of a type named by an `OpcodeFault` -- always a corelib type.
        ///
        /// Canonicalised to a local `TypeDef` only when this image is corelib. Elsewhere it stays
        /// `Foreign`, so that an assembly *shadowing* one of these names with a type of its own
        /// does not absorb the real fault: a `catch` for a local `System.NullReferenceException`
        /// keys to that local `TypeDef`, the fault keys to the foreign name, and they correctly do
        /// not match. An ordinary `catch (NullReferenceException)` in a normal assembly names the
        /// type by TypeRef, which keys foreign too, and still matches.
        ///
        /// A residual imprecision this does not fix: two different assemblies' same-named types
        /// still key alike. Distinguishing them means recording the *defining* assembly, which
        /// type forwarding puts out of reach without the cross-assembly resolution the probe
        /// deliberately lacks -- a TypeRef in System.Text.Json names System.Runtime, while the
        /// definition is in System.Private.CoreLib.
        let keyOfFaultName (qualified : string) : TyKey =
            let idx = qualified.LastIndexOf '.'
            let ns, n = qualified.Substring (0, idx), qualified.Substring (idx + 1)

            if analysingCorelib then
                keyOfName ns n
            else
                TyKey.Foreign (ns, n)

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
                            Set.ofList [ TyKey.Local (ComparableTypeDefinitionHandle.Make h) ; keyOfTypeRef tr ]
                        | _ -> Set.singleton (TyKey.Local (ComparableTypeDefinitionHandle.Make h))
                    | Some (BaseTypeInfo.TypeSpec _)
                    | None -> Set.singleton (TyKey.Local (ComparableTypeDefinitionHandle.Make h))

            supertypes.[h] <- result
            result

        for KeyValue (h, _) in assy.TypeDefs do
            chain h |> ignore

        // Through `keyOfFaultName`, so that an assembly declaring its own `System.Exception` does
        // not have every `catch` for it read as universal. Only corelib's own is.
        let systemException = keyOfFaultName "System.Exception"
        let systemObject = keyOfFaultName "System.Object"

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
                | true, tr -> Some (keyOfTypeRef tr)
                | _ -> None
            | MetadataToken.TypeSpecification h ->
                match assy.TypeSpecs.TryGetValue h with
                | true, spec ->
                    match rootOf spec.Signature with
                    | Some (Choice1Of2 tr) -> Some (keyOfTypeRef tr)
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

                // Offsets some branch can land on. A `throw` among them may be reached from
                // somewhere other than the instruction lexically above it, so the `newobj` there is
                // not necessarily what produced its operand -- two arms each constructing a
                // different exception and sharing one `throw` is the shape that breaks it.
                let branchTargets = HashSet<int> ()

                for op, off in instrs.Instructions do
                    let width = IlOp.NumberOfBytes op

                    match op with
                    | IlOp.UnaryConst c ->
                        match branchDelta c with
                        | Some delta -> branchTargets.Add (off + width + delta) |> ignore
                        | None -> ()
                    | IlOp.Switch deltas ->
                        for d in deltas do
                            branchTargets.Add (off + width + d) |> ignore
                    | _ -> ()

                // A handler's first instruction is entered by dispatch rather than by a branch, and
                // is just as much not-reached-from-above.
                for r in instrs.ExceptionRegions do
                    match r with
                    | ExceptionRegion.Catch (_, o) -> branchTargets.Add o.HandlerOffset |> ignore
                    | ExceptionRegion.Filter (filterOffset, o) ->
                        branchTargets.Add filterOffset |> ignore
                        branchTargets.Add o.HandlerOffset |> ignore
                    | ExceptionRegion.Finally o
                    | ExceptionRegion.Fault o -> branchTargets.Add o.HandlerOffset |> ignore

                let raises = ResizeArray<int * TyKey option> ()
                let calls = ResizeArray<int * MethodDefinitionHandle> ()
                let opaque = ResizeArray<int> ()

                // The type whose initializer this body *is*, when it is one. Inside it, touching
                // that same type's statics or calling its own methods cannot trigger the
                // initializer, because the initializer is the code doing the touching: the CLI
                // marks the type as initializing and lets the initializing thread straight through
                // (ECMA-335 I.8.9.5). Without this, a `.cctor` that writes its own fields picks up
                // `TypeInitialization` from `stsfld`, which then propagates to every call site of
                // its type -- a cycle the analysis inflicts on itself.
                let initialisingType =
                    if pruneSelfInitialisation then
                        match cctorsOwner.TryGetValue h with
                        | true, th -> Some th
                        | _ -> None
                    else
                        None

                /// Does this instruction's operand name a member of a type that has no `.cctor`?
                /// Then no initializer can fail on its account. Exact, and pure metadata — but only
                /// answerable for a local target, so a foreign or instantiated one stays
                /// conservative.
                let touchesTypeWithoutInitialiser (operand : MetadataOperand) : bool =
                    if not options.PruneAbsentInitialisers then
                        false
                    else

                    let ownerOf (t : MetadataToken) : TypeDefinitionHandle option =
                        match t with
                        | MetadataToken.FieldDefinition fh ->
                            match typeOfField.TryGetValue fh with
                            | true, th -> Some th
                            | _ -> None
                        | MetadataToken.MethodDef mh ->
                            match typeOfMethod.TryGetValue mh with
                            | true, th -> Some th
                            | _ -> None
                        | _ -> None

                    match operand with
                    | MetadataOperand.FromDynamicScope _ -> false
                    | MetadataOperand.FromMetadata t ->
                        match ownerOf t.Token with
                        | Some th -> not (hasInitialiser.Contains th)
                        | None -> false

                /// Is this instruction's operand a member of the type this `.cctor` initialises?
                let touchesOwnType (operand : MetadataOperand) : bool =
                    match initialisingType, operand with
                    | None, _ -> false
                    | Some own, MetadataOperand.FromDynamicScope _ -> false
                    | Some own, MetadataOperand.FromMetadata t ->
                        match t.Token with
                        | MetadataToken.FieldDefinition fh ->
                            match typeOfField.TryGetValue fh with
                            | true, th -> th = own
                            | _ -> false
                        | MetadataToken.MethodDef mh ->
                            match typeOfMethod.TryGetValue mh with
                            | true, th -> th = own
                            | _ -> false
                        | _ -> false

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
                            // Two reasons an instruction's `TypeInitialization` entry cannot fire:
                            // this body *is* the initializer it would trigger, or the type it
                            // touches has no initializer to trigger.
                            let noTypeInit =
                                match op with
                                // A `callvirt`'s token names where dispatch starts, not where it
                                // lands, so neither prune may read the owner off it: an interface
                                // has no `.cctor`, and pruning on that would be exactly wrong when
                                // the implementation is a value type whose initializer fails.
                                // `CctorCensus` refuses this for the same reason.
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Callvirt, _)
                                | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldvirtftn, _) -> false
                                | IlOp.UnaryMetadataToken (_, operand) ->
                                    touchesOwnType operand || touchesTypeWithoutInitialiser operand
                                | _ -> false

                            for x in xs do
                                if not (noTypeInit && x = OpcodeFault.TypeInitialization) then
                                    let x =
                                        if List.contains (OpcodeFault.kind x) options.ExcludeKinds then
                                            None
                                        else
                                            Some x

                                    match x with
                                    | Some x -> raises.Add (off, Some (keyOfFaultName (OpcodeFault.typeName x)))
                                    | None -> ()

                    // 2. What the instruction throws explicitly.
                    match op with
                    | IlOp.Nullary NullaryIlOp.Throw ->
                        // Only trust the instruction above when control can only have come from
                        // it.
                        let prev =
                            if i > 0 && not (branchTargets.Contains off) then
                                Some (fst ops.[i - 1])
                            else
                                None

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
                        // Count the virtual-dispatch wall from the opcode, independently of how the
                        // token happened to be encoded.
                        //
                        // `ldvirtftn` is *not* here despite selecting a method: it pushes a pointer
                        // and invokes nothing, so a body that does `ldvirtftn; pop; ret` has only
                        // the opcode's own null-receiver fault. Whatever uncertainty the pointer
                        // carries belongs to the `calli` that eventually uses it.
                        match mop with
                        | UnaryMetadataTokenIlOp.Callvirt -> opaqueHere off Incompleteness.VirtualCall
                        | _ -> ()

                        match mop with
                        // `calli` has no metadata target at all. `jmp` does, so it is followed
                        // below like any other named call.
                        | UnaryMetadataTokenIlOp.Calli -> opaqueHere off Incompleteness.IndirectCall
                        | UnaryMetadataTokenIlOp.Call
                        | UnaryMetadataTokenIlOp.Callvirt
                        | UnaryMetadataTokenIlOp.Jmp
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
