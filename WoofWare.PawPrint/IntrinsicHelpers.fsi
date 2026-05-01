namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

module internal IntrinsicHelpers =
    /// Intrinsic provider type names whose `get_IsSupported` query is modelled as always false
    /// under PawPrint's current scalar-only virtual hardware profile.
    val scalarOnlyFalseIsSupportedIntrinsics : Set<string>

    /// Walk a value-type's fields to decide whether its storage contains any managed references,
    /// hiding the cycle-detection memo table used by the implementation.
    val typeInfoContainsReferences :
        loggerFactory : ILoggerFactory ->
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        state : IlMachineState ->
        typeInfo : TypeInfo<TypeDefn, TypeDefn> ->
            IlMachineState * bool

    /// Pop a `System.Type`/`System.RuntimeType` receiver from the guest evaluation stack and
    /// recover the runtime type-handle target it represents.
    val popRuntimeTypeHandle :
        currentThread : ThreadId -> state : IlMachineState -> RuntimeTypeHandleTarget * IlMachineState

    /// Add an element-count offset to a managed byref, preserving PawPrint's byte-view and
    /// reinterpretation invariants for arrays, strings, local memory, and existing byref views.
    val offsetManagedPointerByElements :
        baseClassTypes : BaseClassTypes<DumpedAssembly> ->
        state : IlMachineState ->
        elementType : ConcreteTypeHandle ->
        offset : int ->
        src : EvalStackValue ->
            EvalStackValue * IlMachineState

    /// Read the deterministic virtual CPU profile for the CoreLib vector type named by an
    /// intrinsic `get_IsHardwareAccelerated` method.
    val vectorAccelerationAvailable : declaringTypeName : string -> profile : HardwareIntrinsicsProfile -> bool

    /// Interpret an eval-stack value as a pointer argument accepted by CoreLib pointer intrinsics,
    /// rejecting unmanaged non-null addresses that PawPrint cannot dereference.
    val managedPointerOfPointerArgument : operation : string -> arg : EvalStackValue -> ManagedPointerSource

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
        wasConstructing : ManagedHeapAddress option ->
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
