namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

module internal IntrinsicHelpers =
    /// Intrinsic provider type names whose `get_IsSupported` query is modelled as always false
    /// under PawPrint's current scalar-only virtual hardware profile.
    val scalarOnlyFalseIsSupportedIntrinsics : Set<string>

    /// CoreCLR's `MethodTable::IsValueTypeImpl`, as the reflection surface sees it: byrefs,
    /// pointers, function pointers and arrays are TypeDescs and answer false, and everything
    /// else is asked of its metadata. `operation` names the caller in the diagnostic raised
    /// when a nominal handle turns out to have no row in `AllConcreteTypes`.
    val isValueTypeHandleAsCoreClr :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        state : IlMachineState ->
        operation : string ->
        handle : ConcreteTypeHandle ->
            bool

    /// Walk a value-type's fields to decide whether its storage contains any managed references
    /// (object references or by-refs). The walk is in the concrete domain, so a field typed `T` is resolved
    /// against the instantiation `handle` names rather than against a generic parameter.
    val concreteTypeContainsReferences :
        loggerFactory : ILoggerFactory ->
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        state : IlMachineState ->
        handle : ConcreteTypeHandle ->
            IlMachineState * bool

    /// Add an element-count offset to a managed byref, preserving PawPrint's byte-view and
    /// reinterpretation invariants for arrays, strings, local memory, and existing byref views.
    val offsetManagedPointerByElements :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        state : IlMachineState ->
        elementType : ConcreteTypeHandle ->
        offset : int64 ->
        src : EvalStackValue ->
            EvalStackValue * IlMachineState

    /// Read the deterministic virtual CPU profile for the CoreLib vector type named by an
    /// intrinsic `get_IsHardwareAccelerated` method.
    val vectorAccelerationAvailable : declaringTypeName : string -> profile : HardwareIntrinsicsProfile -> bool

    /// Interpret an eval-stack value as a pointer argument accepted by CoreLib pointer intrinsics,
    /// rejecting unmanaged non-null addresses that PawPrint cannot dereference.
    val managedPointerOfPointerArgument : operation : string -> arg : EvalStackValue -> ManagedPointerSource

    /// Read one byte from a concrete CLI value, rejecting reference-like storage
    /// with byte-addressability diagnostics.
    val byteAtOffset : operation : string -> src : ManagedPointerSource -> byteOffset : int -> value : CliType -> byte

    /// Check whether a candidate `SpanHelpers.SequenceEqual` method is the byte-wise overload
    /// implemented by PawPrint.
    val isSpanHelpersByteSequenceEqual :
        state : IlMachineState ->
        methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> ->
            bool

    /// Execute PawPrint's byte-wise `SpanHelpers.SequenceEqual` intrinsic implementation,
    /// comparing bytes through managed-pointer byte views and pushing a Boolean result.
    val spanHelpersSequenceEqual :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> ->
        state : IlMachineState ->
            IlMachineState

    /// Interpret an eval-stack value as a managed byref argument, treating guest null references
    /// as PawPrint's null managed-pointer source.
    val popManagedByrefArgument : operation : string -> arg : EvalStackValue -> ManagedPointerSource

    /// Execute the `Span<T>(void*, int)` and `ReadOnlySpan<T>(void*, int)` constructor intrinsic,
    /// materialising the span fields from a pointer-backed source after validating element shape.
    val writePointerBackedSpanConstructor :
        loggerFactory : ILoggerFactory ->
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        wasConstructing : ConstructionState ->
        methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> ->
        state : IlMachineState ->
            IlMachineState

    /// Execute `Span<T>.ToString` or `ReadOnlySpan<T>.ToString`, projecting character spans into
    /// managed strings and returning a deterministic summary string for non-character spans.
    val spanToString :
        loggerFactory : ILoggerFactory ->
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> ->
        state : IlMachineState ->
            IlMachineState

    /// Execute `MemoryExtensions.Equals(ReadOnlySpan<char>, ReadOnlySpan<char>, StringComparison)`
    /// for the deterministic ordinal comparison modes PawPrint currently supports.
    val memoryExtensionsEquals :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> ->
        state : IlMachineState ->
            IlMachineState

    /// Execute the `Unsafe.CopyBlock` / `Unsafe.CopyBlockUnaligned` JIT intrinsics. Drives the
    /// shared cell-aware copy primitive forwards (cpblk has undefined behaviour on overlap per
    /// ECMA-335 III.3.30, so no overlap detection is performed); the cell-aware path preserves
    /// non-byte-addressable cell shapes and non-`Verbatim` numeric provenance through the copy.
    val executeUnsafeCopyBlock :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        operation : string ->
        state : IlMachineState ->
            IlMachineState

    /// Execute the `SpanHelpers.Memmove(ref byte, ref byte, nuint)` JIT intrinsic. Routes through
    /// the shared cell-aware copy primitive with `Memmove` direction policy so cell-aligned
    /// ranges anchored on cell-aware roots preserve non-`Verbatim` numeric provenance (e.g.
    /// `TypeHandlePtr`-tagged `IntPtr`s) and non-byte-addressable cell shapes across the copy.
    val executeSpanHelpersMemmove :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        operation : string ->
        state : IlMachineState ->
            IlMachineState

    /// Execute the `SpanHelpers.ClearWithoutReferences(ref byte, nuint)` JIT intrinsic. Routes
    /// through the shared cell-aware zeroing primitive, which preserves the CLI shape of each
    /// destination cell rather than scattering zero bytes across storage that may not be
    /// byte-addressable.
    val executeSpanHelpersClearWithoutReferences :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        currentThread : ThreadId ->
        operation : string ->
        state : IlMachineState ->
            IlMachineState
