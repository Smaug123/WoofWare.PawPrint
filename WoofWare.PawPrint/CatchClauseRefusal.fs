namespace WoofWare.PawPrint

/// Where CoreCLR refuses a method for the type one of its own `catch` clauses names, which PawPrint,
/// having no compile step, cannot raise where CoreCLR does, and so stops the run instead.
[<RequireQualifiedAccess>]
module internal CatchClauseRefusal =

    /// <summary>
    /// Stops the run where CoreCLR refuses <paramref name="method"/>, of
    /// <paramref name="containingAssembly"/>, when it compiles it, or may: where a <c>catch</c>
    /// clause in metadata names a byref or <c>System.Void</c> (<c>TypeOperand.ofTypeToken</c>), and
    /// the JIT may compile the clause's protected block. CoreCLR raises
    /// <c>InvalidProgramException</c> in the caller before the method runs; PawPrint resolves a
    /// clause's type only during exception dispatch, and would otherwise run the method.
    /// </summary>
    /// <remarks>
    /// <para>
    /// The JIT resolves a clause's type when it imports the clause's handler, which it does once it
    /// imports a block of the protected region (<c>impVerifyEHBlock</c>). Measured: refused where
    /// the protected block is reachable, even behind a branch on a local, and compiled where nothing
    /// reaches it or a folded branch on a literal skips it. PawPrint does not model which branches
    /// the JIT folds, so a protected block that only some foldings skip stops the run too.
    /// </para>
    /// <para>
    /// A clause naming <c>System.Void</c> through an assembly not yet loaded is not found, since
    /// telling would mean loading it (<c>TypeOperand.ofSpelling</c>'s <c>Error</c>).
    /// </para>
    /// </remarks>
    let check
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (loadedAssemblies : LoadedAssemblies)
        (containingAssembly : DumpedAssembly)
        (method : MethodInfo<'typeGen, 'methodGen, 'methodVars>)
        : unit
        =
        match MethodInfo.tryIlBody method with
        | None -> ()
        | Some body ->

        let refused =
            body.ExceptionRegions
            |> Seq.choose (fun region ->
                match region with
                | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, offset) ->
                    match TypeOperand.ofTypeToken baseClassTypes loadedAssemblies containingAssembly token with
                    | Ok (Some refusal) -> Some (offset, refusal)
                    | Ok None
                    | Error _ -> None
                | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope _, _)
                | ExceptionRegion.Filter _
                | ExceptionRegion.Finally _
                | ExceptionRegion.Fault _ -> None
            )
            |> List.ofSeq

        if not refused.IsEmpty then
            // Bounds that hold however the JIT folds: `Maybe` folds nothing, and `Surely` crosses
            // only the branches no folding decides.
            let compiled =
                JitImport.compiledCode
                    body
                    (JitFolding.Optimises
                        {
                            Capabilities = Map.empty
                            OpaqueResults = Set.empty
                        })

            let protects (compiled : Set<int>) (offset : ExceptionOffset) : bool =
                compiled
                |> Set.exists (fun at -> at >= offset.TryOffset && at < offset.TryOffset + offset.TryLength)

            let name =
                match method.TryDeclaringType with
                | Some declaring -> $"%s{declaring.Namespace}.%s{declaring.Name}::%s{method.Name}"
                | None -> method.Name

            match refused |> List.tryFind (fun (offset, _) -> protects compiled.Surely offset) with
            | Some (offset, refusal) ->
                failwith
                    $"TODO: CoreCLR refuses %s{name} in %s{containingAssembly.Name.Name} when it compiles it, raising InvalidProgramException in its caller, because the catch clause of the protected block at IL_%04x{offset.TryOffset} names %O{refusal}; PawPrint has no compile step to raise that from, so it stops"
            | None ->

            match refused |> List.tryFind (fun (offset, _) -> protects compiled.Maybe offset) with
            | Some (offset, refusal) ->
                failwith
                    $"TODO: CoreCLR may refuse %s{name} in %s{containingAssembly.Name.Name} when it compiles it, raising InvalidProgramException in its caller, because the catch clause of the protected block at IL_%04x{offset.TryOffset} names %O{refusal}: it does if it compiles that block, which depends on branches it may fold; PawPrint models neither, so it stops"
            | None -> ()
