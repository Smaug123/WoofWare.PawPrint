namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

/// Where a method sits in a type's method table: which slot each declaration owns, and how many of
/// those slots form the vtable. This is slot *identity*; a MethodImpl changes what a slot holds
/// without changing which slot its body was declared at, so slot *content* is a separate question.
///
/// Kept apart from `NativeRuntimeTypeHelpers`, where the QCalls that ask these questions live,
/// because virtual dispatch asks them too and compiles well before those QCalls do.
module VirtualSlotLayout =

    /// The type a vtable slot's occupant was read from, reduced to what deciding the layout needs:
    /// the token space its signature is spelled in, the identity that orders ties by derivation, and
    /// the substitution its `!i` are read against. The base chain's entries carry a different
    /// substitution from the derived type's -- which is the whole difficulty of matching an override
    /// against the slot it fills.
    type SlotOwner =
        {
            AssemblyFullName : string
            Identity : ResolvedTypeIdentity
            Substitution : TypeConcretization.SubstitutionContext
            /// How to name this type in a diagnostic. Held rather than derived, because the walk that
            /// builds it knows whether it is looking at an instantiation or at a definition and the
            /// identity alone does not carry a name.
            Description : string
        }

    /// The owner of a slot read from a closed type.
    let private slotOwnerOfClosed (concreteType : ConcreteType<ConcreteTypeHandle>) : SlotOwner =
        {
            SlotOwner.AssemblyFullName = concreteType.AssemblyFullName
            SlotOwner.Identity = concreteType.Identity
            SlotOwner.Substitution = TypeConcretization.SubstitutionContext.ofClosed concreteType.Generics
            SlotOwner.Description = string concreteType
        }

    /// One entry of a type's instance vtable: the method currently occupying the slot, together
    /// with the type it was read from.
    type VtableSlot =
        {
            Method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
            DeclaredBy : SlotOwner
        }

    /// Does `candidate`, a non-newslot instance virtual declared on some derived type, fill the
    /// vtable slot currently occupied by `slot`?
    ///
    /// This is CoreCLR's *layout* rule (`MethodTableBuilder::LoaderFindMethodInParentClass`): same
    /// name, and an exact signature match under substitution -- return type included. It is
    /// deliberately stricter than PawPrint's *dispatch* rule in
    /// `IlMachineStateExecution.tryResolveVirtualImplementationForSlot`, which accepts an
    /// assignable return type and has variance carve-outs. That difference is not an oversight on
    /// either side: a covariant-return override is a genuinely new slot in CoreCLR (Roslyn emits it
    /// `newslot` plus a MethodImpl), so folding it into the base slot by return-assignability would
    /// make `GetMethods` report one method where .NET reports two.
    let private candidateFillsSlot
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (candidate : VtableSlot)
        (slot : VtableSlot)
        : IlMachineState * bool
        =
        if candidate.Method.Name <> slot.Method.Name then
            // The one rejection worth making before the signature comparison, because it is the one
            // that discards nearly every (candidate, slot) pair. Everything else the layout rule
            // requires -- the calling convention and `hasThis` in the header, the generic arity, the
            // parameter count -- `signaturesEquivalent` compares first, in that order.
            state, false
        else

        let comparand (slot : VtableSlot) : TypeConcretization.SignatureComparand =
            {
                Signature = slot.Method.Signature
                AssemblyFullName = slot.DeclaredBy.AssemblyFullName
                // A slot's occupant is read through the type it was found on, and the base chain's
                // entries carry a different substitution from the derived type's. That is the
                // substitution the comparison needs.
                DeclaringTypeGenerics = slot.DeclaredBy.Substitution
            }

        IlMachineState.signaturesEquivalent
            loggerFactory
            baseClassTypes
            state
            false
            (comparand candidate)
            (comparand slot)

    /// One side of the constraint comparison CoreCLR runs once it has chosen which parent slot a
    /// generic override fills.
    let private constraintComparand (slot : VtableSlot) : TypeConcretization.ConstraintComparand =
        {
            Parameters = slot.Method.Generics |> Seq.map snd |> List.ofSeq
            AssemblyFullName = slot.DeclaredBy.AssemblyFullName
            DeclaringTypeGenerics = slot.DeclaredBy.Substitution
        }

    /// The methods of a type that CoreCLR's `DeclaredMethodIterator` ranges over, paired with their
    /// metadata facts. Both halves of the method table are laid out from this one list, so that
    /// neither can disagree with the other about what the type declares.
    ///
    /// Two kinds of row are absent from it.
    ///
    /// A *synthesised* method has no MethodDef row, so it is not a declared method at all. The
    /// vtable walk excludes them only incidentally (a synthesised method is never `IsVirtual`);
    /// beyond the vtable, placing one would shift every later method's slot number by one. No test
    /// can cover the filter: nothing today puts a synthesised method into a `TypeInfo` (the
    /// construction sites in `Program.buildStartupFrame` and `StructMarshalStub` both build one for
    /// immediate execution), but `TypeInfo.Methods` is typed to hold either kind.
    ///
    /// A COM *vtable-gap marker* names empty slots in the COM interface vtable rather than declaring
    /// a method. `EnumerateClassMethods` recognises it by `IsMdRTSpecialName` plus a `_VtblGap` name
    /// prefix (methodtablebuilder.cpp:2749, corhdr.h:265-270) and `continue`s before it reaches
    /// `rgDeclaredMethods` (:2852-2921), recording the run length in a `SparseVTableMap` that only
    /// `FEATURE_COMINTEROP` reads -- so it occupies no slot in the CLR method table, virtual or
    /// otherwise. Dropping it here rather than in one walk alone is the point: tlbimp emits these as
    /// `virtual abstract` members of an interface, so a filter applied only past the vtable would
    /// leave the *vtable* inflated by one slot per gap, which moves `GetNumVirtuals` and with it the
    /// origin of everything after it.
    ///
    /// The name grammar is `_VtblGap` + optional digits + optionally `_` and at least one digit, and
    /// CoreCLR refuses to load the type for anything else (:2865-2907) rather than treating it as an
    /// ordinary method -- so a prefix match alone would accept images the runtime rejects. Upstream
    /// raises that as `COR_E_BADIMAGEFORMAT` with `IDS_CLASSLOAD_BADSPECIALMETHOD`, but what a guest
    /// (and the fabricated test) observes is a `TypeLoadException`.
    let private declaredMethodsOf
        (operation : string)
        (owner : SlotOwner)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : (MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * MetadataMethodFacts) list
        =
        // Exactly `void`, no custom modifier: a *blob* comparison, matching `ExactlyEqual`.
        let hasNullaryVoidSignature (method : MethodInfo<_, _, _>) : bool =
            method.Signature.ParameterTypes.IsEmpty
            && not (
                method.Signature.Header.Get.Attributes.HasFlag System.Reflection.Metadata.SignatureAttributes.Generic
            )
            && method.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.Default
            && method.Signature.ReturnType = MethodReturnType.Void

        // `void` once custom modifiers are looked through, which is the other question CoreCLR asks;
        // see `slotsBeyondVtableOfClosed` for why the two must stay separate.
        let returnsVoidThroughModifiers (method : MethodInfo<_, _, _>) : bool =
            match method.Signature.ReturnType with
            | MethodReturnType.Void -> true
            | MethodReturnType.Returns ty -> TypeDefn.stripCustomModifiers ty = TypeDefn.Void

        // `_VtblGap`, then the optional-number/count grammar upstream parses.
        let isWellFormedGapName (name : string) : bool =
            let suffix = name.Substring "_VtblGap".Length
            let afterLeadingDigits = suffix.TrimStart [| '0' .. '9' |]

            if afterLeadingDigits = "" then
                // "_VtblGap" or "_VtblGap<n>": a single empty slot, or the count-less form.
                true
            elif afterLeadingDigits.[0] <> '_' then
                false
            else
                let count = afterLeadingDigits.Substring 1
                count <> "" && count |> Seq.forall System.Char.IsAsciiDigit

        typeInfo.Methods
        |> List.choose (fun method ->
            match method.TryMetadata with
            | None -> None
            | Some facts ->
                if
                    facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName
                    && method.Name.StartsWith ("_VtblGap", System.StringComparison.Ordinal)
                then
                    if not (isWellFormedGapName method.Name) then
                        failwith
                            $"%s{operation}: method %s{method.Name} on %s{owner.Description} is marked RTSpecialName and begins `_VtblGap`, but the rest of the name is not the vtable-gap count grammar; CoreCLR rejects the type at load time (methodtablebuilder.cpp:2865-2907) rather than laying out a method table for it"

                    None
                else

                // The load-time rejections. They live here, rather than beside the placement that
                // needs them, so that they run for *every* type this walk touches -- including each
                // ancestor, since `vtableOfClosed` recurses through the base chain and asks each one
                // for its declared methods. A type whose base CoreCLR refuses to load cannot itself
                // be loaded, because building a MethodTable begins by building the parent's, so
                // validating only the leaf would let `GetSlot` answer for a derived type that cannot
                // exist.
                //
                // The scope is exactly the type and its base chain.
                // Those are the declarations that *contribute slots to the layout being computed*,
                // so a rejection anywhere in them means the numbers this function returns describe a
                // MethodTable that cannot exist. An implemented interface is a different matter:
                // CoreCLR does load one while building the type (`ResolveInterfaces`) and would
                // refuse the implementor if the interface were malformed, but no interface method
                // enters this slot table, so nothing computed here depends on it. Chasing that
                // dependency has no natural stopping point short of the whole type-load closure --
                // field types, generic constraints, and so on -- which is a different feature from
                // laying out a method table. A guest that asks about the malformed interface itself
                // is still refused, because this same function is what answers for it.
                //
                // The classification below keys *on* the
                // RTSpecialName flag, and that is only unambiguous because CoreCLR refuses to load
                // the shapes that would make it ambiguous. Same reason `vtableOfClosed` refuses a
                // non-newslot virtual that matches a `final` parent slot.

                // A `static virtual` is legal only on an interface: on a class or value type
                // `ValidateMethods` throws `IDS_CLASSLOAD_STATICVIRTUAL`
                // (methodtablebuilder.cpp:5124-5131). Only the `!IsInterface()` half is enforced
                // there -- upstream's comment beside it also says such methods "must be abstract",
                // but nothing checks that, and static virtuals with bodies have been legal since
                // .NET 7. Without this the method would simply be placed past the vtable, since
                // `PlaceVirtualMethods` skips it for being static.
                if method.IsStatic && method.IsVirtual && not typeInfo.IsInterface then
                    failwith
                        $"%s{operation}: method %s{method.Name} on %s{owner.Description} is both static and virtual, which is legal only on an interface; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5124-5131) rather than laying out a method table for it"

                if facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName then
                    if method.IsVirtual then
                        failwith
                            $"%s{operation}: method %s{method.Name} on %s{owner.Description} is marked RTSpecialName and virtual; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5001-5004) rather than laying out a method table for it"

                    if method.IsStatic then
                        if method.Name <> ".cctor" || not (hasNullaryVoidSignature method) then
                            failwith
                                $"%s{operation}: static method %s{method.Name} on %s{owner.Description} is marked RTSpecialName but is not exactly `static void .cctor()`; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5011-5019) rather than laying out a method table for it"
                    else if method.Name <> ".ctor" then
                        failwith
                            $"%s{operation}: instance method %s{method.Name} on %s{owner.Description} is marked RTSpecialName but is not named `.ctor`; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5023-5026) rather than laying out a method table for it"
                    elif not (returnsVoidThroughModifiers method) then
                        failwith
                            $"%s{operation}: constructor on %s{owner.Description} does not return void; CoreCLR rejects the type at load time (methodtablebuilder.cpp:5028-5037) rather than laying out a method table for it"

                Some (method, facts)
        )

    /// The assembly and metadata of the type a definition-level walk is laid out on.
    let private definitionMetadata
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : DumpedAssembly * TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let assembly =
            state.LoadedAssembly identity.AssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith $"%s{operation}: assembly %s{identity.AssemblyFullName} is not loaded"
            )

        assembly, Assembly.resolveTypeIdentityDefinition assembly identity

    /// The owner for a type read as its own definition: each `!i` denotes the type's own `i`th
    /// variable, which is the context CoreCLR builds a method table in.
    let ownerOfDefinition (operation : string) (state : IlMachineState) (identity : ResolvedTypeIdentity) : SlotOwner =
        let assembly, typeInfo = definitionMetadata operation state identity

        {
            SlotOwner.AssemblyFullName = identity.AssemblyFullName
            SlotOwner.Identity = identity
            SlotOwner.Substitution =
                TypeConcretization.SubstitutionContext.forDefinition identity typeInfo.Generics.Length
            SlotOwner.Description = TypeInfo.fullName (fun handle -> assembly.TypeDefs.[handle]) typeInfo
        }

    /// The type a nominal signature element names.
    let private nominalIdentityOfSpelling
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (assembly : DumpedAssembly)
        (ty : TypeDefn)
        : IlMachineState * ResolvedTypeIdentity
        =
        match ty with
        | TypeDefn.FromDefinition (identity, _) -> state, identity
        | TypeDefn.FromReference (typeRef, _) ->
            let state, _, resolved =
                IlMachineTypeResolution.resolveTypeFromRef loggerFactory assembly typeRef ImmutableArray.Empty state

            state, ResolvedTypeIdentity.ofDefinitionInAssembly resolved.AssemblyFullName resolved.TypeDefHandle
        | TypeDefn.PrimitiveType PrimitiveType.Object ->
            // A TypeSpec may spell System.Object as a bare `ELEMENT_TYPE_OBJECT`, which names the same
            // type as the nominal form; CoreCLR resolves the two alike (`CompareElementTypeToToken`,
            // siginfo.cpp:4915).
            let object = baseClassTypes.Object
            state, ResolvedTypeIdentity.ofDefinitionInAssembly object.AssemblyFullName object.TypeDefHandle
        | other ->
            failwith
                $"%s{operation}: a base type is spelled %O{other}, which names no type definition; an extends clause is a TypeDefOrRefOrSpec, so it resolves to a nominal type or to `object`"

    /// The base type a definition extends, and the arguments its extends clause applies to it, read in
    /// the extending type's own vocabulary.
    ///
    /// The base is deliberately *not* returned as something to lay out in that vocabulary: placement
    /// belongs to the base's own definition. These arguments are what re-reads the resulting slots for
    /// comparison against this type's methods.
    ///
    /// `None` is `System.Object`, which extends nothing.
    let private baseOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (owner : SlotOwner)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState * (ResolvedTypeIdentity * ImmutableArray<TypeConcretization.SubstitutionArgument>) option
        =
        match typeInfo.BaseType with
        | None -> state, None
        | Some baseTypeInfo ->

        let assembly, _ = definitionMetadata operation state owner.Identity

        // Only a TypeSpec can carry generic arguments: a TypeDef or TypeRef token names a type without
        // an instantiation, so a generic base is always spelled as a TypeSpec. That is why this
        // decomposes `BaseTypeInfo` itself rather than going through
        // `IlMachineRuntimeMetadata.resolveBaseTypeInfo`, whose TypeDefn for the other two arms is
        // rebuilt from the *resolved* type -- for a generic one that is the typical instantiation
        // `G<!0, !1>`, whose variables are G's own rather than this type's, and substituting those
        // into this type's context would re-index another type's variables.
        let state, baseIdentity, arguments =
            match baseTypeInfo with
            | BaseTypeInfo.TypeDef handle ->
                state,
                ResolvedTypeIdentity.ofDefinitionInAssembly owner.Identity.AssemblyFullName handle,
                ImmutableArray.Empty
            | BaseTypeInfo.TypeRef handle ->
                let state, _, resolved =
                    IlMachineTypeResolution.resolveTypeFromRef
                        loggerFactory
                        assembly
                        assembly.TypeRefs.[handle]
                        ImmutableArray.Empty
                        state

                state,
                ResolvedTypeIdentity.ofDefinitionInAssembly resolved.AssemblyFullName resolved.TypeDefHandle,
                ImmutableArray.Empty
            | BaseTypeInfo.TypeSpec handle ->
                match assembly.TypeSpecs.[handle].Signature with
                | TypeDefn.GenericInstantiation (generic, arguments) ->
                    let state, identity =
                        nominalIdentityOfSpelling loggerFactory baseClassTypes operation state assembly generic

                    state, identity, ImmutableArray.CreateRange arguments
                | nominal ->
                    let state, identity =
                        nominalIdentityOfSpelling loggerFactory baseClassTypes operation state assembly nominal

                    state, identity, ImmutableArray.Empty

        let baseAssembly, baseTypeDefinition =
            definitionMetadata operation state baseIdentity

        // The clause supplies one argument per variable the base declares, or the image would not
        // load. Checked because a decomposition bug above would otherwise surface as a signature
        // comparison reading past the end of a substitution.
        if arguments.Length <> baseTypeDefinition.Generics.Length then
            failwith
                $"%s{operation}: %s{owner.Description} extends %s{baseAssembly.Name.Name}/%O{baseIdentity.TypeDefinition.Get}, which declares %d{baseTypeDefinition.Generics.Length} generic parameter(s), but its extends clause supplies %d{arguments.Length} argument(s)"

        // The clause's arguments are spelled in the token space of the type that *writes* the clause,
        // which is this type's and not the base's.
        let arguments =
            (TypeConcretization.SubstitutionContext.forBase owner.AssemblyFullName arguments owner.Substitution)
                .Arguments

        state, Some (baseIdentity, arguments)

    /// The instance vtable of a type *definition*, base-first: index `i` is the method that occupies
    /// slot `i`. A type inherits its base's layout, replaces the entries its own non-newslot virtuals
    /// override, and appends a slot for each `newslot` virtual it introduces.
    ///
    /// This is the single definition of "which slot" in PawPrint: `GetSlot` is an index into this
    /// list and `GetNumVirtuals` is its length, so the two cannot disagree -- which matters,
    /// because the BCL *compares* them (`isVirtual = slot &lt; GetNumVirtuals(declaringType)`,
    /// RuntimeType.CoreCLR.cs:685-686).
    ///
    /// Laid out on the definition rather than on an instantiation because that is what CoreCLR does,
    /// and the difference is observable: `A&lt;T&gt;.M(T)` and `B&lt;T&gt;.M(string)` are distinct declarations
    /// occupying distinct slots, and closing them at `T = string` first would make an override of one
    /// appear to fill the other. Signatures are therefore compared with the type's variables left
    /// standing, each ancestor's read through the substitution its extends clause supplies.
    ///
    /// Note that MethodImpls are deliberately not consulted. A MethodImpl overwrites a slot's
    /// implementation but not the slot number its body was declared at
    /// (`MethodTableBuilder::SetVirtualMethodImpl` changes the Impl and not the Decl), so it
    /// belongs to slot *content* -- dispatch, and one day `GetMethodAt` -- rather than to slot
    /// identity.
    ///
    /// This is recomputed on every `GetSlot`/`GetNumVirtuals` query, and `PopulateMethods` issues
    /// one query per virtual method: the walk is not memoised, so populating a type is quadratic in
    /// its virtual count. A cache would be keyed on the definition, every instantiation of which
    /// shares this answer.
    let rec private vtableOfDefinitionOwner
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (owner : SlotOwner)
        : IlMachineState * VtableSlot list
        =
        let _, typeInfo = definitionMetadata operation state owner.Identity

        let state, baseType =
            baseOfDefinition loggerFactory baseClassTypes operation state owner typeInfo

        let state, baseSlots =
            match baseType with
            | None -> state, []
            | Some (baseIdentity, arguments) ->
                // The parent's table is built once, in the parent's own context: which slot each of its
                // methods occupies was decided before any argument was supplied, and `CopyParentVtable`
                // copies the result rather than rebuilding it (methodtablebuilder.cpp:1143). Re-running
                // that placement here would fold two of the parent's slots together whenever this
                // type's arguments make two of its distinct declarations coincide.
                let state, inherited =
                    ownerOfDefinition operation state baseIdentity
                    |> vtableOfDefinitionOwner loggerFactory baseClassTypes operation state

                // Matching this type's own methods against those slots is the separate question, and it
                // does need them read in this type's vocabulary.
                let inherited =
                    inherited
                    |> List.map (fun slot ->
                        { slot with
                            VtableSlot.DeclaredBy =
                                { slot.DeclaredBy with
                                    SlotOwner.Substitution =
                                        TypeConcretization.SubstitutionContext.rebase
                                            baseIdentity
                                            arguments
                                            slot.DeclaredBy.Substitution
                                }
                        }
                    )

                state, inherited

        // Shared with the walk past the vtable, so that the two cannot disagree about what the
        // type declares -- see `declaredMethodsOf` for what it drops and why. Upstream's
        // `PlaceVirtualMethods` takes exactly the declared *instance* virtuals from that same
        // list; a `static virtual` is placed past the vtable instead.
        let instanceVirtuals =
            declaredMethodsOf operation owner typeInfo
            |> List.filter (fun (method, _) -> not method.IsStatic && method.IsVirtual)
            |> List.map fst

        // Upstream is a single pass over the declared methods in MethodDef row order
        // (`DeclaredMethodIterator` over the array `EnumerateClassMethods` fills in row order),
        // and each method either replaces a parent slot or takes the next free one
        // (`MethodTableBuilder::PlaceVirtualMethods`, methodtablebuilder.cpp:5405-5482). So
        // overrides and fresh slots interleave, and it is *declaration* order -- not NewSlot --
        // that decides the order of the fresh ones. Partitioning on NewSlot and appending the
        // groups separately would agree only while fresh slots were the exclusive preserve of
        // NewSlot methods, which stopped being true the moment the unmatched case below started
        // allocating one. Measured on a fabricated type declaring an unmatched NewSlot virtual
        // before an unmatched non-NewSlot one: the host CLR gives the NewSlot method the lower
        // slot, and a NewSlot-grouped layout gets it backwards (TestFabricatedVtableLayout).
        //
        // Only the *parent's* slots are candidates for a match: upstream searches
        // `bmtParent->pParentMethodHash`, built once from the parent MethodTable
        // (methodtablebuilder.cpp:174-193) and never extended as this type's own methods are
        // placed. A slot appended by an earlier method of *this* type is therefore not something
        // a later one can land on.
        //
        // That is why the fold below carries the inherited slots and the fresh ones as two
        // values rather than one growing list. `inherited` only ever has entries *replaced*, so
        // it stays exactly the parent's vtable and the search cannot reach a fresh slot however
        // the search is written. Threading one list and capping the search at the parent's length
        // would compute the same answer, but this way the invariant is a property of the shape
        // rather than of remembering to cap; it also keeps appending O(1) rather than copying the
        // accumulated vtable per method, which for an interface -- where every member appends --
        // is the difference between a linear layout and a quadratic one.
        //
        // The restriction bites on legal metadata, not only on corrupt images. ECMA-335
        // II.22.26 stops a type repeating a method blob-for-blob, but `candidateFillsSlot`
        // compares *concretised* signatures -- which is what lets an ordinary override of a
        // generic base match at all -- and that conflates blobs which genuinely differ. The
        // worked example is `GenericConflation`1` in TestFabricatedVtableLayout: it declares
        // `Conflated(!0)` as NewSlot and `Conflated(string)` without it, and closing it at
        // `T = string` makes the second match the slot the first was just appended to. CoreCLR
        // lays slots out on the generic definition, where the two are distinct, and gives each
        // its own; a search that could see fresh slots would have the second replace the first
        // and the vtable would come out a slot short.
        let state, inherited, freshReversed =
            ((state, baseSlots, []), instanceVirtuals)
            ||> List.fold (fun (state, slots, fresh) method ->
                let candidate =
                    {
                        VtableSlot.Method = method
                        VtableSlot.DeclaredBy = owner
                    }

                let state, matched =
                    if method.IsNewSlot then
                        // "If the member is marked with a new slot we do not need to find it in
                        // the parent" -- it is asking for a slot of its own by construction.
                        state, []
                    else
                        // An interface reaches here with no inherited slots, so the search is
                        // empty and every method it declares appends -- which is exactly what
                        // upstream's `IsInterface` arm does, an interface having no parent whose
                        // slots it could reuse. That arm needs no special case here, but it does
                        // need the unmatched case below to allocate rather than fail: corelib's
                        // `INumberBase<T>` declares `System.IUtf8SpanFormattable.TryFormat` as
                        // `Private, Final, Virtual, HideBySig` with no NewSlot -- measured, the
                        // only such method in corelib -- and it takes this path.
                        ((state, []), List.indexed slots)
                        ||> List.fold (fun (state, acc) (i, slot) ->
                            let state, fills =
                                candidateFillsSlot loggerFactory baseClassTypes state candidate slot

                            state, (if fills then i :: acc else acc)
                        )

                // More than one slot can legitimately match: `A` declares `virtual M()`, `B :
                // A` declares `new virtual M()` with the identical signature, and `C : B`
                // overrides it. CoreCLR resolves this in `LoaderFindMethodInParentClass`, and
                // the tie-break lives in how that lookup's index is built rather than in the
                // lookup itself: `CreateMethodChainHash` walks the *parent's* slot table in
                // ascending slot order and inserts each slot's occupant at the **head** of its
                // name bucket, and `Lookup` returns the first entry in the bucket. So the entry
                // returned is the one inserted last, i.e. the occupant of the highest matching
                // slot -- the most-derived declaration, which is also C#'s meaning, since
                // `C.M` overrides the `M` that `B` introduced and leaves `A`'s alone. Slots are
                // appended as the walk descends, so that is the matching slot with the largest
                // index; the fold above prepends, so `matched` is already in descending index
                // order.
                // Every tie here is genuine, and highest-matching-slot is its answer. Laid out on an
                // instantiation a tie could instead be an *artifact* -- `A<T>.M(T)` and
                // `B<T>.M(string)` are distinct declarations that closing at `T = string` makes
                // identical -- and this walk used to refuse that shape rather than guess. Reading the
                // definition's variables as themselves removes the possibility, so there is nothing
                // left to separate.
                //
                // Two slots of the *same* owner can tie too, and that is not illegal metadata: with
                // `B<T>` declaring both `M(T)` and `M(string)`, a derived `D : B<string>` reading
                // B's slots sees two `M(string)`s. C# refuses to compile it (CS0462) but the CLR
                // loads it, and measured against the host on a fabricated image it gives the
                // reuse-slot override B's *second* slot -- the highest match, the same rule as
                // above. `TestFabricatedVtableLayout` pins it.
                match matched with
                | mostDerived :: _ ->
                    // CoreCLR refuses to load a type whose non-newslot virtual matches a
                    // `final` parent slot: having picked the override candidate out of the
                    // parent chain, `MethodTableBuilder::PlaceVirtualMethods` throws
                    // `IDS_CLASSLOAD_MI_FINAL_DECL` when `IsMdFinal(dwParentAttrs)`
                    // (methodtablebuilder.cpp:5445-5448). The check is against the single method
                    // the lookup returned, which is the most-derived match -- the same slot the
                    // tie-break above selects -- so testing the chosen occupant is upstream's
                    // rule and not an approximation of it.
                    //
                    // Filling the slot anyway would hand out a vtable layout for a type the real
                    // runtime would refuse to load, and every slot number derived from it would
                    // then be answering a question about a type that cannot exist. Roslyn cannot
                    // emit this shape, but -- like the unmatched-override case below -- assembly
                    // version skew can, by sealing a virtual in a base that a derived assembly
                    // was already compiled against.
                    let occupant = List.item mostDerived slots

                    if occupant.Method.IsFinal then
                        failwith
                            $"%s{operation}: virtual method %s{method.Name} on %s{owner.Description} is not marked newslot and matches vtable slot %i{mostDerived}, which is occupied by the final method %s{occupant.Method.Name} declared by %s{occupant.DeclaredBy.Description}; CoreCLR rejects this type at load time with a TypeLoadException rather than laying out a vtable for it"

                    // Matching signatures are not the whole of the layout rule for a *generic*
                    // method: CoreCLR compares the type parameters' constraints too, and refuses
                    // to load the type if the override demands more of a type argument than the
                    // method it overrides did (`MetaSig::CompareMethodConstraints`,
                    // methodtablebuilder.cpp:5449-5459).
                    //
                    // Like the `final` check above, this belongs *after* the most-derived match
                    // is chosen rather than inside the predicate that finds matches. A base
                    // chain may hold several slots this candidate matches by signature -- `A`
                    // declaring `virtual M<T>()`, `B` hiding it with a `new virtual M<T>()` that
                    // adds a constraint, `C` overriding `B`'s -- and only the one it actually
                    // fills has any say. Comparing against the others would reject ordinary C#.
                    //
                    // Roslyn copies a base method's constraints verbatim onto an override, so a
                    // genuine override always agrees here; assembly version skew and
                    // hand-authored IL are what can disagree.
                    let state, constraintsMatch =
                        if candidate.Method.Generics.IsEmpty then
                            state, true
                        else
                            IlMachineState.methodConstraintsMatch
                                loggerFactory
                                baseClassTypes
                                state
                                (constraintComparand candidate)
                                (constraintComparand occupant)

                    if not constraintsMatch then
                        failwith
                            $"%s{operation}: generic method %s{method.Name} on %s{owner.Description} fills vtable slot %i{mostDerived}, held by %s{occupant.Method.Name} declared by %s{occupant.DeclaredBy.Description}, but its type parameters' constraints do not permit it to override that slot; CoreCLR rejects this type at load time with a TypeLoadException rather than laying out a vtable for it"

                    state, (slots |> List.mapi (fun j slot -> if j = mostDerived then candidate else slot)), fresh
                | [] ->
                    // "Else, place the method in the next available empty vtable slot"
                    // (methodtablebuilder.cpp:5401). Both kinds of method arrive here: one
                    // marked NewSlot, which skipped the search and is asking for a slot of its
                    // own, and one *not* marked NewSlot whose search came up empty. Upstream
                    // makes no distinction between them -- both go to `AddVirtualMethod` -- and
                    // neither does this.
                    //
                    // The second kind is what F# emits constantly: the structural equality and
                    // comparison members of a union or record are `Public, Final, Virtual,
                    // HideBySig` with no NewSlot, so `Equals(T)` and `CompareTo(object,
                    // IComparer)` match nothing on `Object` and land here. Roslyn never emits
                    // it -- 0 of corelib's 1470 non-generic classes trigger it, measured.
                    //
                    // Appending is the whole of the rule, but it costs a diagnostic: a gap in
                    // `candidateFillsSlot` shows up as a spurious extra slot rather than a
                    // failure here, so what catches one is the slot-by-slot comparison against
                    // the host CLR's own `GetSlot` in TestVirtualMethodSlots -- a check on the
                    // layout rather than merely on its length, because a walk that appends one
                    // slot too many while dropping a real one has the right length.
                    state, slots, candidate :: fresh
            )

        // The fresh slots were accumulated head-first, so undo that once here rather than
        // copying the accumulated vtable on every append.
        let slots = inherited @ List.rev freshReversed

        state, slots

    /// The instance vtable of the generic definition `identity` -- the layout every instantiation of
    /// it shares.
    let vtableOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * VtableSlot list
        =
        ownerOfDefinition operation state identity
        |> vtableOfDefinitionOwner loggerFactory baseClassTypes operation state

    /// The instance vtable of a runtime type, base-first: index `i` is the method that occupies slot
    /// `i`.
    ///
    /// For a nominal type this is its *definition's* vtable, which is the same list for every
    /// instantiation -- see `vtableOfDefinition`, where the rule lives. A structural handle has no
    /// definition to ask: byrefs, pointers and function pointers are TypeDescs with no method table,
    /// and a synthesised array's slots are `System.Array`'s.
    let rec vtableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * VtableSlot list
        =
        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // Byrefs, pointers, and function pointers are TypeDescs in CoreCLR with no
            // MethodTable, so they have no vtable at all.
            state, []
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // Synthesised array MethodTables inherit their virtual slots from System.Array (and
            // through it, System.Object); the structural array handle itself introduces none.
            let state, baseHandle =
                IlMachineState.resolveBaseConcreteType loggerFactory baseClassTypes state concreteType

            match baseHandle with
            | None -> state, []
            | Some bh -> vtableOfClosed loggerFactory baseClassTypes operation state bh
        | ConcreteTypeHandle.Concrete _ ->
            // A nominal type's own generic arguments say nothing about which slot anything occupies:
            // CoreCLR hands the method-table builder its parent as a raw `SigPointer` into the
            // extends-clause blob with no substitution for the type being built
            // (methodtablebuilder.cpp:1330-1337), and clones the canonical method table for every
            // instantiation that shares code
            // (`Generics::CreateTypeHandleForNonCanonicalGenericInstantiation`, generics.cpp:159-495).
            // Measured over corelib, System.Linq, System.Text.Json, System.Collections.Concurrent and
            // System.Private.Uri: 2683 (definition, instantiation) pairs agree on the whole layout.
            let concreteTypeInfo, _ =
                IlMachineState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            vtableOfDefinition loggerFactory baseClassTypes operation state concreteTypeInfo.Identity

    /// The occupants of the region of a type's method table that follows its vtable, in slot order,
    /// so that the method at index `i` holds slot `numVirtuals + i`.
    ///
    /// This is `MethodTableBuilder::PlaceNonVirtualMethods` (methodtablebuilder.cpp:5255-5359).
    /// Slot numbers come from one monotonic counter shared with the vtable
    /// (`AddNonVirtualMethod` sets the index to `pSlotTable->GetSlotCount()`,
    /// methodtablebuilder.h:1532-1541), and only the parent's *virtual* slots are inherited --
    /// `CopyParentVtable` (methodtablebuilder.cpp:1143) stops at the parent's `GetNumVirtuals()` --
    /// so this region begins at exactly the type's own `GetNumVirtuals()`, however many slots its
    /// base had beyond its vtable. Upstream machine-checks that premise: `PlaceNonVirtualMethods`
    /// opens with `INDEBUG(bmtVT->SealVirtualSlotSection())` and every subsequent add re-seals, so
    /// a debug build asserts that nothing appends to the vtable once this has begun.
    ///
    /// Nothing renumbers a declared method afterwards. `PlaceInterfaceMethods` runs later but adds
    /// no slots -- it only fills in `bmtInterfaceSlotImpl` and the dispatch map. Do not be misled by
    /// the comment above its call site (methodtablebuilder.cpp:1676), which still describes
    /// creating "duplicate slots ... starting at dwCurrentDuplicateVtableSlot": that variable no
    /// longer exists anywhere in the file. The one later addition, `AddUnboxedMethod` for a value
    /// type's unboxed entrypoints (:7178), appends after everything placed from metadata.
    ///
    /// Two assumptions about what the metadata contains, both currently true and neither checked
    /// here. Runtime-async (`g_pConfig->RuntimeAsync()`, off by default) makes
    /// `EnumerateClassMethods` synthesise a second `bmtMDMethod` per Task-returning method, and
    /// those consume slots alongside the declared ones; and EnC adds MethodDescs entirely outside
    /// this file, which PawPrint may ignore because it does not support dynamic code at all (#853).
    ///
    /// The order is *not* MethodDef row order, and every step below is observable.
    /// Verified against the host CLR's own `RuntimeMethodHandle.GetSlot` for every method reflection
    /// can reach: 31064 methods over 2336 corelib types, 5499 over 1153 FSharp.Core types, and 352
    /// over closed generic instantiations, with no disagreement.
    let private slotsBeyondVtableOfDefinition
        (operation : string)
        (owner : SlotOwner)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : VtableSlot list
        =
        // The same list the vtable walk is laid out from, so the two cannot disagree about what the
        // type declares; `declaredMethodsOf` documents which rows it drops and why. In particular a
        // vtable-gap marker never reaches the validation below, which would otherwise see a
        // runtime-special-named method that is not a constructor and wrongly report that CoreCLR
        // rejects the type.
        let declared = declaredMethodsOf operation owner typeInfo

        // `PlaceVirtualMethods` places exactly the declared *instance* virtuals, so everything else
        // is still unplaced when `PlaceNonVirtualMethods` runs. A `static virtual` -- an interface
        // static abstract -- is therefore placed here, which is what upstream's
        // `AddNonVirtualMethod` assertion `!IsMdVirtual(...) || IsMdStatic(...)` asserts. Writing
        // this filter as "not virtual" would silently drop all 41 of `INumberBase<T>`'s static
        // members, which is why that interface is in the layout corpus.
        let unplaced =
            declared
            |> List.filter (fun (method, _) -> not (method.IsVirtual && not method.IsStatic))

        // CoreCLR recognises the two constructors by `IsMdRTSpecialName` *plus* an `ExactlyEqual`
        // match -- name and raw signature blob both -- against hard-coded `static void .cctor()` and
        // `instance void .ctor()` signatures. `declaredMethodsOf` has already refused any
        // runtime-special-named method that is neither, since CoreCLR refuses to load such a type.
        //
        // The flag is not implied by the name: a method merely *named* `.ctor`
        // without it skips that block entirely and is placed in the ordinary pass below.
        // `FakeCtorSecond` in TestFabricatedVtableLayout pins that against the host CLR. ECMA-335
        // II.10.5.1 requires constructors to carry `rtspecialname`, so such an image is invalid --
        // but CoreCLR loads it anyway, and CoreCLR is what this emulates.
        let isRuntimeSpecialName (facts : MetadataMethodFacts) : bool =
            facts.MethodAttributes.HasFlag MethodAttributes.RTSpecialName

        // "The signature carries `IMAGE_CEE_CS_CALLCONV_GENERIC`", which is the bit
        // `EnumerateClassMethods` reads to decide `hasGenericMethodArgs`
        // (methodtablebuilder.cpp:2794) -- *not* "the encoded generic arity is positive". ECMA-335
        // requires that arity to be at least 1 when the bit is set, so the two agree on every valid
        // image and no test here distinguishes them; on an invalid-but-loadable one with the bit set
        // and a count of zero, CoreCLR goes by the bit, and the method's pass below would differ.
        let isGenericSignature (method : MethodInfo<_, _, _>) : bool =
            method.Signature.Header.Get.Attributes.HasFlag System.Reflection.Metadata.SignatureAttributes.Generic

        // CoreCLR asks two different questions about a constructor's return type, and the answers
        // come apart on exactly one shape. `ValidateMethods` rejects a ctor whose return is not void
        // using `MetaSig::GetReturnType()`, which reaches `SigParser::PeekElemTypeClosed` and calls
        // `SkipCustomModifiers()` first (sigparser.h:225) -- so `modopt(X) void` *is* void there, and
        // such a type loads happily; measured on the host, which instantiates one. That question is
        // `declaredMethodsOf`'s, since it decides whether the type loads at all.
        //
        // This one is the other: `pDefaultCtor` is set by `ExactlyEqual` against the hard-coded
        // `instance void ()`, a raw *blob* comparison, in which a modifier makes the signature
        // different -- so the same constructor does not get the priority slot. `ModoptVoidCtor` in
        // TestFabricatedVtableLayout needs both, and collapsing either into the other kills it.
        //
        // Matching the blob also means the calling convention and generic arity are part of the
        // test, not just the arity: a vararg or (illegal-but-loadable) generic `.ctor()` is not the
        // default constructor either.
        let hasNullaryVoidSignature (method : MethodInfo<_, _, _>) : bool =
            method.Signature.ParameterTypes.IsEmpty
            && not (isGenericSignature method)
            && method.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.Default
            && method.Signature.ReturnType = MethodReturnType.Void

        let isClassConstructor ((method, facts) : MethodInfo<_, _, _> * MetadataMethodFacts) : bool =
            isRuntimeSpecialName facts
            && method.IsStatic
            && method.Name = ".cctor"
            && hasNullaryVoidSignature method

        let isDefaultConstructor ((method, facts) : MethodInfo<_, _, _> * MetadataMethodFacts) : bool =
            isRuntimeSpecialName facts
            && not method.IsStatic
            && method.Name = ".ctor"
            && hasNullaryVoidSignature method

        // Steps 1 and 2: the class constructor, then the parameterless instance constructor, ahead
        // of everything else whatever their MethodDef rows say. Upstream places them first because
        // `MethodTable::GetCCtorSlot` and `GetDefaultCtorSlot` are *defined* as those two positions.
        // `System.Type` is the corpus witness for both halves at once: it declares its `.cctor` at
        // row 2639, its default ctor at row 2438, and other methods from row 2431, so it
        // discriminates cctor-before-ctor *and* ctor-before-row-order. `Lazy`1` is the witness that
        // the rule still holds on a generic type, where every other method is placed in the first
        // pass below and could otherwise have swallowed the ctors with it.
        //
        // At most *one* row is hoisted for each. `ValidateMethods` records them by plain assignment
        // inside its loop -- `bmtVT->pCCtor = *it` (methodtablebuilder.cpp:5019) and
        // `bmtVT->pDefaultCtor = *it` (:5042) -- so when a type declares the same constructor twice,
        // which ECMA-335 II.22.26 forbids but CoreCLR loads anyway, the *last* matching row wins and
        // the earlier ones are placed in the ordinary pass like any other method. Measured: a type
        // with `Plain` then two identical `.ctor()` rows gives the last `.ctor` slot 4 and leaves the
        // earlier one at slot 6, *after* `Plain`. Hoisting both would move everything after them.
        let lastMatching (predicate : MethodInfo<_, _, _> * MetadataMethodFacts -> bool) =
            unplaced |> List.filter predicate |> List.tryLast

        let placedFirst =
            [ lastMatching isClassConstructor ; lastMatching isDefaultConstructor ]
            |> List.choose id

        let hoisted = placedFirst |> List.map (fun (method, _) -> method.IdentityKey)

        let stillUnplaced =
            unplaced
            |> List.filter (fun (method, _) -> not (hoisted |> List.contains method.IdentityKey))

        // Steps 3 and 4: two passes, each in row order. Upstream's vocabulary for them is worth
        // knowing, because it cuts across the name of this function: the first pass places methods
        // that need a *real vtable slot* and freezes `bmtVT->cVtableSlots` after itself, so only
        // pass-2 methods are what CoreCLR calls "non-vtable slots". Both regions are past
        // `GetNumVirtuals` and both are returned here. The boundary between them is deliberately
        // not exposed -- nothing PawPrint models reads `cVtableSlots` -- and the split is modelled
        // only because it decides the numbering.
        //
        // `fCanHaveNonVtableSlots` is false for a generic type and for an interface, so both place
        // everything in the first pass and leave the second empty. `mcInstantiated` is exactly "the
        // signature carries `IMAGE_CEE_CS_CALLCONV_GENERIC`" (methodtablebuilder.cpp:2794, 3235-3238):
        // the delegate and P/Invoke arms are tried first, but a generic method reaching one of them
        // is rejected outright by the `BFA_GENERIC_METHODS_INST` guard at :3273, so on a loadable
        // image the two coincide. `GenericParameterCount` is read from the same signature blob
        // rather than from the GenericParam rows, so this is that predicate and not a proxy for it.
        //
        // So on a non-generic class a generic method is numbered *ahead* of a non-generic one
        // declared earlier: `System.Version` puts its four generic methods at slots 12-15 and starts
        // everything else at 16, though its lowest-numbered row is among the latter.
        let canHaveNonVtableSlots = typeInfo.Generics.IsEmpty && not typeInfo.IsInterface

        let needsRealSlot ((method, _) : MethodInfo<_, _, _> * MetadataMethodFacts) : bool =
            not canHaveNonVtableSlots || isGenericSignature method

        let realSlots, rest = stillUnplaced |> List.partition needsRealSlot

        placedFirst @ realSlots @ rest
        |> List.map (fun (method, _) ->
            {
                VtableSlot.Method = method
                // Slots beyond the vtable are never inherited, so the declaring type is always this
                // one -- unlike a vtable slot, which routinely still holds a base type's method.
                VtableSlot.DeclaredBy = owner
            }
        )

    /// A closed type's whole method table, as CoreCLR's `bmtVT->pSlotTable`: the vtable proper,
    /// followed by the region `PlaceNonVirtualMethods` fills. Slot numbers run across the two
    /// without a break, and `cVirtualSlots` -- `MethodTable::GetNumVirtuals()` -- is the length of
    /// the first.
    ///
    /// Kept as two lists rather than one, with `slotIndexInTable` owning the arithmetic that joins
    /// them, because the two halves answer different questions and the BCL asks both: `GetSlot`
    /// indexes the concatenation while `GetNumVirtuals` is the prefix length, and
    /// `PopulateProperties` *compares* the two to decide whether an accessor is virtual. A single
    /// flat list would lose the boundary the comparison is about; making the caller add an offset
    /// would scatter that arithmetic across call sites.
    ///
    /// The second field is named for the boundary rather than for virtualness on purpose: it holds
    /// every `static virtual` the type declares, those being placed outside the vtable, so calling
    /// it "non-virtual" would be false of its contents.
    type MethodSlotTable =
        {
            /// Slots `0 .. Vtable.Length - 1`. This length is `MethodTable::GetNumVirtuals()`.
            Vtable : VtableSlot list
            /// Slots `Vtable.Length` upwards.
            BeyondVtable : VtableSlot list
        }

    let slotTableOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * MethodSlotTable
        =
        // Only the vtable walk recurses through the base chain; the region beyond it is this type's
        // alone, so it is computed once here rather than once per ancestor and discarded.
        let state, virtualSlots =
            vtableOfClosed loggerFactory baseClassTypes operation state concreteType

        match concreteType with
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // TypeDescs with no MethodTable, so no slots of either kind -- the same
            // reason `vtableOfClosed` gives them an empty vtable.
            state,
            {
                MethodSlotTable.Vtable = virtualSlots
                MethodSlotTable.BeyondVtable = []
            }
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ ->
            // A synthesised array MethodTable really does carry slots beyond its vtable, for the
            // intrinsic Get/Set/Address and the ctor, and PawPrint models none of them --
            // `introducedMethodsOfClosed` refuses the same question for the same reason. Answering
            // "none" would be a wrong answer rather than an absent one, so refuse. Unreachable from
            // `GetSlot` today: a method handle always resolves to a `Concrete` declaring type, there
            // being no way to mint one naming an array intrinsic.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{concreteType}; the array intrinsic methods (Get/Set/Address/.ctor) occupy slots beyond the vtable that PawPrint does not model"
        | ConcreteTypeHandle.Concrete _ ->
            let concreteTypeInfo, _ =
                IlMachineState.tryGetConcreteTypeInfo state concreteType
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{concreteType}"
                )

            let owner = ownerOfDefinition operation state concreteTypeInfo.Identity
            let _, typeInfo = definitionMetadata operation state concreteTypeInfo.Identity

            state,
            {
                MethodSlotTable.Vtable = virtualSlots
                MethodSlotTable.BeyondVtable = slotsBeyondVtableOfDefinition operation owner typeInfo
            }

    /// The whole method table of a generic definition, which is the table every instantiation of it
    /// shares. An open generic type definition has no other spelling, so this is what
    /// `RuntimeTypeHandle.GetNumVirtuals` and `RuntimeMethodHandle.GetSlot` answer from when the
    /// declaring type a guest names is the definition itself.
    let slotTableOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * MethodSlotTable
        =
        let state, virtualSlots =
            vtableOfDefinition loggerFactory baseClassTypes operation state identity

        let owner = ownerOfDefinition operation state identity
        let _, typeInfo = definitionMetadata operation state identity

        state,
        {
            MethodSlotTable.Vtable = virtualSlots
            MethodSlotTable.BeyondVtable = slotsBeyondVtableOfDefinition operation owner typeInfo
        }

    /// What identifies a vtable slot's occupant well enough to find it again: the full name of the
    /// assembly that declares the method, paired with the method's within-assembly identity.
    ///
    /// The assembly is not decoration. `MethodInfo.IdentityKey` is a MethodDef *row number*, which
    /// is unique only within its own module, and a vtable routinely spans assemblies -- a guest type
    /// deriving from `System.Object` has corelib's rows sitting underneath its own. Row 6 of the
    /// guest and row 6 of corelib are different methods that compare equal on `IdentityKey` alone.
    let slotIdentity
        (slot : VtableSlot)
        : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option)
        =
        slot.DeclaredBy.AssemblyFullName, slot.Method.IdentityKey

    /// The index of the slot occupied by the method with the given identity, or `None`.
    let slotIndexOfIdentity
        (target : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option))
        (slotIdentities :
            (string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option)) list)
        : int option
        =
        slotIdentities |> List.tryFindIndex (fun identity -> identity = target)

    /// The slot CoreCLR assigns a method in its declaring type's method table -- `MethodDesc::GetSlot`
    /// -- or `None` if the method holds no slot there at all.
    ///
    /// The one place the two halves of a `MethodSlotTable` are joined into a single numbering, which
    /// is the point of routing every query through here rather than letting callers add the offset.
    ///
    /// `None` is not "not virtual": every method a type declares in metadata occupies a slot, in one
    /// half or the other. It means the method is not this type's at all -- a synthesised method,
    /// which has no MethodDef row and so is never placed, or a lookup against the wrong type.
    let slotIndexInTable
        (target : string * (System.Reflection.Metadata.MethodDefinitionHandle option * SynthesisedMethod option))
        (table : MethodSlotTable)
        : int option
        =
        match slotIndexOfIdentity target (table.Vtable |> List.map slotIdentity) with
        | Some index -> Some index
        | None ->
            slotIndexOfIdentity target (table.BeyondVtable |> List.map slotIdentity)
            |> Option.map (fun index -> List.length table.Vtable + index)

    /// The size of the instance vtable for a closed type, matching CoreCLR's
    /// `MethodTable::GetNumVirtuals()`.
    let numVirtualsOfClosed
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (concreteType : ConcreteTypeHandle)
        : IlMachineState * int
        =
        // The length of `vtableOfClosed` by definition rather than an independently-computed sum,
        // because `PopulateMethods` compares it against `RuntimeMethodHandle.GetSlot`'s answer:
        // two walks that had to agree by discipline would disagree silently, and the symptom
        // would be a wrong `isVirtual` rather than a crash.
        let state, slots =
            vtableOfClosed loggerFactory baseClassTypes operation state concreteType

        state, List.length slots

    /// The size of the instance vtable of a generic definition, which is the size every
    /// instantiation of it inherits: slot layout is a property of the definition.
    let numVirtualsOfDefinition
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (identity : ResolvedTypeIdentity)
        : IlMachineState * int
        =
        let state, slots =
            vtableOfDefinition loggerFactory baseClassTypes operation state identity

        state, List.length slots

    let numVirtuals
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : IlMachineState * int
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at VirtualSlotLayout.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
            // CoreCLR's GetNumVirtuals asserts !typeHandle.IsGenericVariable(); the BCL's
            // RuntimeType.GetMethodCandidates strips generic variables before calling.
            // Reaching here means a managed-side invariant was violated.
            failwith
                $"%s{operation}: invoked on type-generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before calling"
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
            failwith
                $"%s{operation}: invoked on method-generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before calling"
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            // Slot layout is a property of the generic definition: `MethodTableBuilder` places
            // virtuals from the definition's own metadata, so every instantiation ends up with the
            // same numbering -- and for a reference type CoreCLR does not even recompute it, taking
            // `SetNumVirtuals` from the canonical instantiation and sharing its vtable chunks
            // (`Generics::CreateTypeHandleForNonCanonicalGenericInstantiation`, generics.cpp:205 and
            // :327-334). So this is the same number `numVirtualsOfClosed` answers for any `G<...>`,
            // and asking the definition is the only way to get it when the guest named no
            // instantiation.
            numVirtualsOfDefinition loggerFactory baseClassTypes operation state identity
        | RuntimeTypeHandleTarget.Closed handle ->
            numVirtualsOfClosed loggerFactory baseClassTypes operation state handle

    /// The methods a declaring type introduces, as CoreCLR's `IntroducedMethodIterator` walks
    /// them: the type's own MethodDef rows in metadata order, never an inherited one.
    ///
    /// Returns the defining assembly and the declaring target alongside them, because those are
    /// what `MethodHandleRegistry.getOrAllocateInternalHandle` needs to mint a handle and they
    /// differ between the closed and open-definition cases.
    ///
    /// `None` means "this type has no MethodTable, so it introduces nothing" — byref, pointer and
    /// function-pointer TypeDescs. Callers should emit the null sentinel so the managed
    /// `IntroducedMethodEnumerator` terminates immediately.
    let introducedMethodsOf
        (operation : string)
        (state : IlMachineState)
        (target : RuntimeTypeHandleTarget)
        : (string *
          RuntimeTypeHandleTarget *
          MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list) option
        =
        match target with
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete _ as handle) ->
            let concreteType, typeInfo =
                IlMachineState.tryGetConcreteTypeInfo state handle
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                )

            Some (concreteType.AssemblyFullName, target, typeInfo.Methods)
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
            // CoreCLR's typical instantiation of `G<>` is a MethodTable carrying the definition's
            // own TypeDef token, and its MethodDescChunks hold the definition's MethodDefs. So the
            // answer is the metadata method list read straight off the typedef: no instantiation is
            // needed, which is what makes this answerable where `numVirtuals` is not — that needs
            // to *match* signatures across the base chain, and this only needs to list them.
            let assembly =
                state.LoadedAssembly identity.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: assembly %s{identity.AssemblyFullName} is not loaded"
                )

            let typeInfo = Assembly.resolveTypeIdentityDefinition assembly identity

            Some (identity.AssemblyFullName, target, typeInfo.Methods)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Pointer _)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer _) ->
            // CoreCLR's IntroducedMethodIterator runs on a MethodTable; byrefs/pointers/function-
            // pointers are TypeDescs with no MethodTable, so GetFirstIntroducedMethod returns null
            // and the managed enumerator terminates without iterating.
            None
        | RuntimeTypeHandleTarget.GenericParameter _
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            // Also TypeVarTypeDescs, and CoreCLR agrees they introduce nothing:
            // `PopulateConstructors` returns an empty array for `IsGenericParameter`
            // (RuntimeType.CoreCLR.cs:755) rather than iterating.
            None
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.OneDimArrayZero _ as handle)
        | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Array _ as handle) ->
            // Synthesised array MethodTables have a small fixed set of introduced methods (Get/Set/
            // Address/the parameterless ctor). PawPrint does not yet model these; no test exercises
            // this path, so fail loudly to flag the gap rather than silently reporting zero.
            failwith
                $"TODO: %s{operation} for synthesised array handle %O{handle}; need to surface the array's intrinsic Get/Set/Address methods"
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at VirtualSlotLayout.fs:%s{__LINE__}; got %O{openConstructed}"
