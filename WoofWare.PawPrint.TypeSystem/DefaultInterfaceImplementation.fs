namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// The default interface bodies a call can land on, as CoreCLR's
/// `MethodTable::FindDefaultInterfaceImplementation` (methodtable.cpp) finds them, for an instance
/// method (`ConcreteVirtualDispatch`) and a static virtual (`StaticVirtualDispatch`) alike. A body
/// is found by identity, never by name and signature: on the method's own interface it is the method
/// itself, and on any other interface only one a MethodImpl names the method for.
///
/// Whatever loads an assembly or registers a concrete type on the way returns the state it leaves
/// behind; `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module internal DefaultInterfaceImplementation =

    let private operation = "default interface body search"

    /// A default body `FindDefaultInterfaceImplementation` considers, and the interface supplying it.
    type internal Candidate =
        {
            Interface : ConcreteTypeHandle
            Body : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        }

    let internal sameDefinition (state : TypeSystemState) (a : ConcreteTypeHandle) (b : ConcreteTypeHandle) : bool =
        match TypeSystemState.tryGetConcreteTypeInfo state a, TypeSystemState.tryGetConcreteTypeInfo state b with
        | Some (a, _), Some (b, _) -> a.Identity = b.Identity
        | _ -> false

    /// The definition of the interface method `method` instantiates.
    let internal definitionOf
        (state : TypeSystemState)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        match method.TryMetadata with
        | Some facts -> state.LoadedAssembly(method.DeclaringAssemblyFullName).Value.Methods.[facts.Handle]
        | None -> failwith $"%s{operation}: %s{method.Name} is synthesised, so it has no MethodDef row to search for"

    /// The body a MethodImpl on `level` supplies for `method`, a static method if `statics` holds and
    /// an instance one otherwise, on an interface instantiation `accepts` admits. Where several do,
    /// it is the one CoreCLR's search meets first: for an instance method the first body in
    /// `level`'s method table, in slot order (the MethodImpl search of
    /// `TryGetCandidateImplementation`), and for a static method the body of the first such
    /// MethodImpl row (`MethodTable::TryResolveVirtualStaticMethodOnThisType`). A type the search
    /// reaches that does not bind is the failure.
    let internal implementationOnType
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (level : ConcreteTypeHandle)
        (method : SlotIdentity)
        (statics : bool)
        (accepts : TypeSystemState -> ConcreteTypeHandle -> TypeSystemState * Result<bool, TypeBindingFailure>)
        (state : TypeSystemState)
        : TypeSystemState *
          Result<
              WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option,
              TypeBindingFailure
           >
        =
        match TypeSystemState.tryGetConcreteTypeInfo state level with
        | None -> state, Ok None
        | Some (levelTy, levelTypeInfo) ->

        let owner = ConcreteMethodTable.ownerOfDefinition operation state levelTy.Identity

        let assembly, _ =
            ConcreteMethodTable.definitionMetadata operation state levelTy.Identity

        let rows =
            levelTypeInfo.MethodImpls
            |> Seq.sortBy (fun (KeyValue (handle, _)) ->
                MetadataToken.toInt (MetadataToken.MethodImplementation handle)
            )
            |> Seq.map (fun (KeyValue (_, impl)) -> impl)
            |> List.ofSeq

        // The body `row` supplies, if it names `method` on an instantiation `accepts` admits.
        let bodyOf
            (state : TypeSystemState)
            (row : MethodImplParsed)
            : TypeSystemState *
              Result<
                  WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option,
                  TypeBindingFailure
               >
            =
            match
                ConcreteInterfaceDispatch.interfaceMethodImplDeclaration
                    loggerFactory
                    dotnetRuntimeDirs
                    baseClassTypes
                    operation
                    statics
                    state
                    levelTy
                    owner.Description
                    owner.Substitution
                    assembly
                    row.Declaration
            with
            | state, Error failure -> state, Error failure
            | state, Ok (Some (declaredOn, _, declared)) when declared = method ->
                match accepts state declaredOn with
                | state, Error failure -> state, Error failure
                | state, Ok true ->
                    ConcreteInterfaceDispatch.methodImplBody
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        owner
                        levelTypeInfo
                        assembly
                        row.Body
                    |> fun (state, body) -> state, Result.map Some body
                | state, Ok false -> state, Ok None
            | state, Ok _ -> state, Ok None

        if statics then
            let rec first
                (state : TypeSystemState)
                (rows : MethodImplParsed list)
                : TypeSystemState *
                  Result<
                      WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> option,
                      TypeBindingFailure
                   >
                =
                match rows with
                | [] -> state, Ok None
                | row :: rows ->
                    match bodyOf state row with
                    | state, Ok None -> first state rows
                    | found -> found

            first state rows
        else
            let state, bodies =
                (state, [], rows)
                |||> StateResult.fold (fun state acc row ->
                    match bodyOf state row with
                    | state, Ok (Some body) -> state, Ok (body :: acc)
                    | state, Ok None -> state, Ok acc
                    | state, Error failure -> state, Error failure
                )

            match bodies with
            | Error failure -> state, Error failure
            | Ok bodies ->

            match List.rev bodies with
            | [] -> state, Ok None
            | bodies ->
                match
                    ConcreteMethodTable.slotTableOfDefinition
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        levelTy.Identity
                with
                | state, Error failure -> state, Error failure
                | state, Ok table ->

                let slotOf
                    (body : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
                    : int
                    =
                    match
                        MethodTableLayout.slotIndexInTable (body.DeclaringAssemblyFullName, body.IdentityKey) table
                    with
                    | Some slot -> slot
                    | None ->
                        failwith
                            $"%s{operation}: the MethodImpl body %s{body.Name} holds no slot in the method table of %s{levelTy.Name}, which declares it"

                state, Ok (Some (List.minBy slotOf bodies))

    /// Whether `from` casts to `target`, which for two interfaces is identity or variance.
    let internal castsTo
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (target : ConcreteTypeHandle)
        (state : TypeSystemState)
        (from : ConcreteTypeHandle)
        : TypeSystemState * Result<bool, TypeBindingFailure>
        =
        TypeAssignability.isConcreteTypeAssignableTo loggerFactory dotnetRuntimeDirs baseClassTypes state from target

    /// What the interface `candidate` supplies for `method` of the instantiation `target`, if it
    /// supplies anything (`TryGetCandidateImplementation`): on `target` itself, the method's own body;
    /// on a variance-compatible instantiation of it, where variance is allowed, that body as that
    /// instantiation's; on a more specific interface, a MethodImpl of its own naming the method.
    let private candidateOn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allowVariance : bool)
        (target : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (candidate : ConcreteTypeHandle)
        (state : TypeSystemState)
        : TypeSystemState * Result<Candidate option, TypeBindingFailure>
        =
        let own () =
            if method.Body.IsAbstract then
                None
            else
                Some
                    {
                        Interface = candidate
                        Body = definitionOf state method
                    }

        if candidate = target then
            state, Ok (own ())
        else

        match castsTo loggerFactory dotnetRuntimeDirs baseClassTypes target state candidate with
        | state, Error failure -> state, Error failure
        | state, Ok false -> state, Ok None
        | state, Ok true ->

        if sameDefinition state candidate target then
            state, Ok (if allowVariance then own () else None)
        else

        let accepts
            (state : TypeSystemState)
            (declaredOn : ConcreteTypeHandle)
            : TypeSystemState * Result<bool, TypeBindingFailure>
            =
            if declaredOn = target then
                state, Ok true
            elif allowVariance && sameDefinition state declaredOn target then
                castsTo loggerFactory dotnetRuntimeDirs baseClassTypes target state declaredOn
            else
                state, Ok false

        let state, body =
            implementationOnType
                loggerFactory
                dotnetRuntimeDirs
                baseClassTypes
                candidate
                (method.DeclaringAssemblyFullName, method.IdentityKey)
                method.IsStatic
                accepts
                state

        state,
        body
        |> Result.map (
            Option.map (fun body ->
                {
                    Interface = candidate
                    Body = body
                }
            )
        )

    /// Add `current` to `candidates`, keeping only the most specific of those that cast to one
    /// another (`FindDefaultInterfaceImplementation`'s insertion loop, transcribed). `None` marks a
    /// candidate a more specific one displaced.
    let private insertCandidate
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allowVariance : bool)
        (current : Candidate)
        (state : TypeSystemState)
        (candidates : Candidate option list)
        : TypeSystemState * Result<Candidate option list, TypeBindingFailure>
        =
        // Walks the candidates in order, rebuilding the list: `seenMoreSpecific` says whether
        // `current` has already displaced one, and `insert` whether it still needs a place.
        let rec go
            (state : TypeSystemState)
            (seenMoreSpecific : bool)
            (insert : bool)
            (before : Candidate option list)
            (after : Candidate option list)
            : TypeSystemState * Result<Candidate option list * bool, TypeBindingFailure>
            =
            match after with
            | [] -> state, Ok (List.rev before, insert)
            | None :: rest -> go state seenMoreSpecific insert (None :: before) rest
            | Some existing :: rest ->
                if existing.Interface = current.Interface then
                    // A duplicate: nothing more to do.
                    state, Ok (List.rev before @ after, false)
                elif allowVariance && sameDefinition state existing.Interface current.Interface then
                    // Variant instantiations of one interface tie.
                    go state seenMoreSpecific insert (Some existing :: before) rest
                else

                match
                    castsTo loggerFactory dotnetRuntimeDirs baseClassTypes existing.Interface state current.Interface
                with
                | state, Error failure -> state, Error failure
                | state, Ok true ->
                    // `current` is more specific: it takes the first such place, and empties the rest.
                    let replaced = if seenMoreSpecific then None else Some current
                    go state true false (replaced :: before) rest
                | state, Ok false ->

                match
                    castsTo loggerFactory dotnetRuntimeDirs baseClassTypes current.Interface state existing.Interface
                with
                | state, Error failure -> state, Error failure
                | state, Ok true ->
                    // `existing` already stands for `current`.
                    state, Ok (List.rev before @ after, false)
                | state, Ok false -> go state seenMoreSpecific insert (Some existing :: before) rest

        match go state false true [] candidates with
        | state, Error failure -> state, Error failure
        | state, Ok (candidates, insert) -> state, Ok (if insert then candidates @ [ Some current ] else candidates)

    /// The default bodies for `method`, an interface method as the call instantiates it, which survive
    /// CoreCLR's search among `receiver` and the interfaces of every type on its chain
    /// (`MethodTable::FindDefaultInterfaceImplementation`), in the order that search leaves them. Each
    /// level contributes only the interfaces its parent's map does not already hold. `allowVariance` admits a
    /// variance-compatible instantiation of `method`'s interface, whose own body runs as that
    /// instantiation's, and counts two instantiations of one interface as equally specific.
    ///
    /// The survivors are distinct interfaces, none of which casts to another except as variance
    /// allows; what more than one of them means is the caller's question. A survivor's body may be
    /// abstract, which is a reabstraction.
    ///
    /// A type the search reaches that does not bind is the failure.
    let internal search
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allowVariance : bool)
        (receiver : ConcreteTypeHandle)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState * Result<Candidate list, TypeBindingFailure>
        =
        let state, target =
            match
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    method.RequiredDeclaringType.Identity
                    method.DeclaringTypeGenerics
            with
            | Some target -> state, target
            | None ->
                let target, concreteTypes =
                    AllConcreteTypes.add method.RequiredDeclaringType state.ConcreteTypes

                { state with
                    ConcreteTypes = concreteTypes
                },
                target

        let consider
            (state : TypeSystemState)
            (candidates : Candidate option list)
            (on : ConcreteTypeHandle)
            : TypeSystemState * Result<Candidate option list, TypeBindingFailure>
            =
            match candidateOn loggerFactory dotnetRuntimeDirs baseClassTypes allowVariance target method on state with
            | state, Error failure -> state, Error failure
            | state, Ok None -> state, Ok candidates
            | state, Ok (Some current) ->
                insertCandidate loggerFactory dotnetRuntimeDirs baseClassTypes allowVariance current state candidates

        // The receiver itself, when it is an interface.
        let state, candidates =
            match TypeSystemState.tryGetConcreteTypeInfo state receiver with
            | Some (_, typeInfo) when typeInfo.IsInterface -> consider state [] receiver
            | _ -> state, Ok []

        let rec levels
            (state : TypeSystemState)
            (candidates : Candidate option list)
            (level : ConcreteTypeHandle)
            : TypeSystemState * Result<Candidate option list, TypeBindingFailure>
            =
            // A level with no TypeDef row, an array, declares no interfaces of its own; its base does.
            let state, candidates =
                match TypeSystemState.tryGetConcreteTypeInfo state level with
                | None -> state, Ok candidates
                | Some _ ->

                match
                    ConcreteInterfaceDispatch.interfaceMapOf
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        operation
                        state
                        level
                with
                | state, Error failure -> state, Error failure
                | state, Ok map ->
                    (state, candidates, map |> List.filter (fun entry -> not entry.ImplementedByParent))
                    |||> StateResult.fold (fun state candidates entry -> consider state candidates entry.Interface)

            match candidates with
            | Error failure -> state, Error failure
            | Ok candidates ->

            match
                TypeSystemState.resolveBaseConcreteType loggerFactory dotnetRuntimeDirs baseClassTypes state level
            with
            | state, Error failure -> state, Error failure
            | state, Ok None -> state, Ok candidates
            | state, Ok (Some parent) -> levels state candidates parent

        match candidates with
        | Error failure -> state, Error failure
        | Ok candidates ->

        let state, candidates = levels state candidates receiver

        state, candidates |> Result.map (List.choose id)
