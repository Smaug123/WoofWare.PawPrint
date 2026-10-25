namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Shared machinery for the CoreLib entry points the VM calls itself rather than any guest:
/// `System.AppContext::Setup` and `System.Environment::InitializeCommandLineArgs` during
/// startup, and `System.Threading.Thread::StartCallback` at the bottom of every started thread.
///
/// All are ordinary managed IL that PawPrint interprets. What the VM contributes, and the only
/// thing PawPrint has to synthesise, is the lookup that finds the method (`CorelibEntryPoint`) and, for the startup
/// pair, native-heap argument buffers — `char*` and `char**` over NUL-terminated UTF-16, and the
/// `NativeReferenceCell` an `[UnmanagedCallersOnly]` entry point stores what it caught in. Those
/// are here so the callers cannot drift on the pointer-cell conventions below, which are not
/// locally obvious and which fail in ways that look like a corrupt guest rather than a wrong
/// buffer.
[<RequireQualifiedAccess>]
module HostStartupCall =

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
            IlMachineState.allocateNativeMemory
                MemoryBlockInitialization.ZeroInitialized
                NativeBlockAlignment.malloc
                ((s.Length + 1) * 2)
                state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref {
                                             Root = ByrefRoot.NativeMemoryByte (blockId, 0)
                                             Projections = []
                                         } -> blockId
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
                NativeBlockAlignment.malloc
                // A native-int cell is pointer-sized, and `sizeof ptr[char]` in the guest's IL
                // agrees, so the stride written matches the stride the callee reads. A
                // disagreement would leave every entry but the first unreadable, which is what
                // `AppContextConfigProperties.cs` (five properties, all asserted) would catch.
                (List.length targets * NativeReferenceCell.PointerSize)
                state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref {
                                             Root = ByrefRoot.NativeMemoryByte (blockId, 0)
                                             Projections = []
                                         } -> blockId
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
                    (i * NativeReferenceCell.PointerSize)
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ofManagedPointer target)))
                    pool
            )

        ptr, IlMachineState.setNativeMemoryPool pool state

    /// A pointer to a fresh VM local holding the object at `addr`, as the VM passes a `Thread*`.
    let allocateReferenceLocal
        (addr : ManagedHeapAddress)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let cell, pool =
            NativeReferenceCell.allocate (Some addr) (IlMachineState.getNativeMemoryPool state)

        cell.Pointer, IlMachineState.setNativeMemoryPool pool state

    /// A fresh `Exception*` out-slot holding null, as the VM's protected local does before the call.
    let allocateExceptionSlot (state : IlMachineState) : NativeReferenceCell * IlMachineState =
        let cell, pool =
            NativeReferenceCell.allocate None (IlMachineState.getNativeMemoryPool state)

        cell, IlMachineState.setNativeMemoryPool pool state

    /// What the callee stored in the out-slot `slot`: `None` if it left it null.
    let readExceptionSlot (slot : NativeReferenceCell) (state : IlMachineState) : ManagedHeapAddress option =
        NativeReferenceCell.read slot (IlMachineState.getNativeMemoryPool state)

    /// Concretize `method` — which must be non-generic, on a non-generic type — and build the
    /// frame that calls it with `args`, which for an instance method begin with `this`.
    /// Also returns the handle of the method's declaring type, so that a caller installing the
    /// frame as a thread's bottom frame can arm that type's class initialiser the way a call
    /// would.
    ///
    /// `purpose` completes the same sentence as in `CorelibEntryPoint.findCorelibStaticMethod`,
    /// and appears if the frame cannot be built.
    let buildFrame
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        (args : ImmutableArray<CliType>)
        (purpose : string)
        (state : IlMachineState)
        : IlMachineState * MethodState * ConcreteTypeHandle
        =
        let state, concretizedResult =
            ExecutionConcretization.concretizeMethodWithTypeGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                None
                baseClassTypes.Corelib.DefinitionFullName
                ImmutableArray.Empty
                state

        let concretized, declaringType = BindingStop.bound concretizedResult

        match
            MethodState.Empty
                state.TypeSystem.ConcreteTypes
                baseClassTypes
                state.TypeSystem._LoadedAssemblies
                baseClassTypes.Corelib
                concretized
                ImmutableArray.Empty
                args
                None
        with
        | Ok methodState -> state, methodState, declaringType
        | Error e ->
            failwith $"Failed to build a call frame for %s{method.Name}, which PawPrint calls to %s{purpose}: %O{e}"
