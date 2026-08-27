namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

/// Shared machinery for the CoreLib entry points a runtime host calls during startup:
/// `System.AppContext::Setup` and `System.Environment::InitializeCommandLineArgs`.
///
/// Both are ordinary managed IL that PawPrint interprets, and both are called by the VM
/// rather than by any guest. What the VM contributes, and the only thing PawPrint has to
/// synthesise, is native-heap argument buffers — `char*` and `char**` over NUL-terminated
/// UTF-16 — plus the lookup that finds the method. Those are here so the two callers cannot
/// drift on the pointer-cell conventions below, which are not locally obvious and which fail
/// in ways that look like a corrupt guest rather than a wrong buffer.
[<RequireQualifiedAccess>]
module HostStartupCall =

    /// Bytes in a `char**` slot. A native-int cell is 8 bytes wide, and `sizeof ptr[char]`
    /// in the guest's IL agrees, so the stride we write matches the stride the callee reads. A
    /// disagreement would leave every entry but the first unreadable, which is what
    /// `AppContextConfigProperties.cs` (five properties, all asserted) would catch.
    [<Literal>]
    let PointerSize = 8

    /// Write `s` as NUL-terminated UTF-16 into a fresh native-heap block, and return a
    /// pointer to its first code unit — a `char*` as CoreLib will consume it.
    ///
    /// Interior NULs are the caller's problem: a real host truncates at the first one when it
    /// assigns a `char_t*` into a `pal::string_t`, so a caller whose strings can contain one
    /// must have truncated already. That matters for anything used as a dictionary key,
    /// where truncating late would let two strings a real host merges into one arrive here as
    /// two.
    let allocateWideString (s : string) (state : IlMachineState) : ManagedPointerSource * IlMachineState =
        let ptr, state =
            IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized ((s.Length + 1) * 2) state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []) -> blockId
            | other ->
                failwith
                    $"logic error: allocateNativeMemory returned %O{other} rather than a byref to byte 0 of a fresh native block"

        // A .NET string is already UTF-16, so each `char` becomes one 2-byte cell
        // verbatim; no encoding decision arises.
        let pool =
            (IlMachineState.getNativeMemoryPool state, seq { 0 .. s.Length - 1 })
            ||> Seq.fold (fun pool i -> NativeMemoryPool.writeCell blockId (i * 2) (CliType.ofChar s.[i]) pool)

        // The terminator is left implicit: the block is zero-initialised, and a run of two
        // zero bytes is exactly the NUL code unit `wcslen` stops at.
        ptr, IlMachineState.setNativeMemoryPool pool state

    /// Build one `char**`: a block of pointer cells, one per string.
    ///
    /// The cells hold pointers, not synthesised bit patterns, so that when the guest's
    /// `ldind.i` reads a slot back it gets a pointer with its provenance intact rather than
    /// an integer we would then have to re-interpret.
    let allocatePointerArray
        (targets : ManagedPointerSource list)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let ptr, state =
            IlMachineState.allocateNativeMemory
                MemoryBlockInitialization.ZeroInitialized
                (List.length targets * PointerSize)
                state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []) -> blockId
            | other ->
                failwith
                    $"logic error: allocateNativeMemory returned %O{other} rather than a byref to byte 0 of a fresh native block"

        // Native-int rather than `CliType.RuntimePointer`, because indirect memory in this
        // machine is untyped: `stind.i` coerces a pointer to
        // `Numeric (NativeInt (ManagedPointer …))` (`EvalStack.toCliTypeCoerced`), and
        // `ldind.i` reads with a matching native-int template. Writing these cells the way a
        // guest's own `stind.i` would have written them is what lets the callee's `ldind.i`
        // find them; a `RuntimePointer` cell is the right shape for a *typed* pointer slot
        // such as a parameter, but it is not what an untyped indirect load expects.
        let pool =
            (IlMachineState.getNativeMemoryPool state, List.indexed targets)
            ||> List.fold (fun pool (i, target) ->
                NativeMemoryPool.writeCell
                    blockId
                    (i * PointerSize)
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer target)))
                    pool
            )

        ptr, IlMachineState.setNativeMemoryPool pool state

    /// The unique static `methodName` of arity `arity` on the CoreLib type
    /// `typeNamespace.typeName`.
    ///
    /// `purpose` completes the sentence "… ; PawPrint calls it to <purpose>", and appears in
    /// every rejection: these are the VM's own private entry points, so a lookup that finds
    /// none or several means CoreLib's shape has changed under us, and the report has to say
    /// what PawPrint wanted it for.
    ///
    /// Arity is matched as well as the name so that failure stays loud, and correctly
    /// attributed, if a future CoreLib keeps the name but changes the signature.
    let findCorelibStaticMethod
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeNamespace : string)
        (typeName : string)
        (methodName : string)
        (arity : int)
        (purpose : string)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let corelib = baseClassTypes.Corelib

        let candidateTypes =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, ty)) ->
                if ty.Namespace = typeNamespace && ty.Name = typeName then
                    Some ty
                else
                    None
            )
            |> Seq.toList

        let declaringType =
            match candidateTypes with
            | [ single ] -> single
            | [] ->
                failwith $"Could not find %s{typeNamespace}.%s{typeName} in CoreLib; PawPrint calls it to %s{purpose}."
            | _ :: _ :: _ ->
                failwith
                    $"Found several %s{typeNamespace}.%s{typeName} type definitions in CoreLib; PawPrint calls it to %s{purpose}."

        let candidates =
            declaringType.Methods
            |> List.filter (fun m -> m.Name = methodName && m.IsStatic && MethodInfo.arity m = arity)

        match candidates with
        | [ single ] -> single
        | [] ->
            failwith
                $"Could not find the static %i{arity}-argument %s{typeNamespace}.%s{typeName}::%s{methodName} in CoreLib; PawPrint calls it to %s{purpose}. If its signature has changed, the caller needs updating to match."
        | _ :: _ :: _ ->
            failwith
                $"Found several static %i{arity}-argument %s{typeNamespace}.%s{typeName}::%s{methodName} methods in CoreLib; expected exactly one. PawPrint calls it to %s{purpose}."

    /// Concretize `method` — which must be non-generic, on a non-generic type — and build the
    /// frame that calls it with `args`.
    ///
    /// `purpose` completes the same sentence as in `findCorelibStaticMethod`, and appears if
    /// the frame cannot be built.
    let buildFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (args : System.Collections.Immutable.ImmutableArray<CliType>)
        (purpose : string)
        (state : IlMachineState)
        : IlMachineState * MethodState
        =
        let state, concretized, _ =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                System.Collections.Immutable.ImmutableArray.Empty
                method
                None
                baseClassTypes.Corelib.DefinitionFullName
                System.Collections.Immutable.ImmutableArray.Empty
                state

        match
            MethodState.Empty
                state.ConcreteTypes
                baseClassTypes
                state._LoadedAssemblies
                baseClassTypes.Corelib
                concretized
                System.Collections.Immutable.ImmutableArray.Empty
                args
                None
        with
        | Ok methodState -> state, methodState
        | Error e ->
            failwith $"Failed to build a call frame for %s{method.Name}, which PawPrint calls to %s{purpose}: %O{e}"
