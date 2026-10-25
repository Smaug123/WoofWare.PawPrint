namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

/// Whether storage of a type holds anything the GC tracks: what
/// `RuntimeHelpers.IsReferenceOrContainsReferences<T>()` answers.
[<RequireQualifiedAccess>]
module GcReferences =

    type private Status =
        | InProgress
        | Completed of bool

    /// CoreLib's primitive value types. Each is defined with a single instance field whose
    /// signature is the primitive type itself (`System.Int32.m_value : int32`), so walking their
    /// fields bottoms out only on the cycle check below. Short-circuiting is both cheaper and
    /// clearer than relying on that.
    let private primitiveValueTypeNames =
        set
            [
                "Boolean"
                "Byte"
                "SByte"
                "Char"
                "Int16"
                "UInt16"
                "Int32"
                "UInt32"
                "Int64"
                "UInt64"
                "IntPtr"
                "UIntPtr"
                "Single"
                "Double"
            ]

    // The walk is in the *concrete* domain: `handle` names an exact instantiation, so a field
    // typed `T` is concretized against the declaring type's own generic arguments rather than
    // against a manufactured `GenericTypeParameter` placeholder. That is what makes `Box<string>`
    // distinguishable from `Box<int>`, and it is why the memo table is keyed on the handle:
    // `ConcreteTypeHandle` already identifies identity *and* instantiation.
    let rec private containsReferencesIn
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (seenSoFar : ImmutableDictionary<ConcreteTypeHandle, Status>)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * ImmutableDictionary<ConcreteTypeHandle, Status> * bool
        =
        match handle with
        // An array is an object reference.
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> state, seenSoFar, true
        // A byref is a GC-tracked pointer, and CoreCLR counts it: the JIT answers
        // `IsReferenceOrContainsReferences<T>` with `varTypeIsGC(fromType) || fromLayout->HasGCPtr()`
        // (jit/importercalls.cpp:3624), and a layout's `m_gcPtrCount` includes its `TYP_BYREF`
        // slots (jit/layout.cpp:576). Hence the API's documented contract: "a value type that
        // contains references *or by-refs*".
        | ConcreteTypeHandle.Byref _ -> state, seenSoFar, true
        // Unmanaged pointers are not GC-tracked.
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> state, seenSoFar, false
        | ConcreteTypeHandle.Concrete _ ->

        match seenSoFar.TryGetValue handle with
        | true, InProgress ->
            // We've hit a cycle. Optimistically assume this path does not introduce a reference type.
            // If another path finds a reference type, its 'true' will override this.
            state, seenSoFar, false
        | true, Completed v ->
            // We've already calculated this; return the memoized result.
            state, seenSoFar, v
        | false, _ ->
            let concrete =
                AllConcreteTypes.lookup handle state.ConcreteTypes
                |> Option.defaultWith (fun () -> failwith $"type was not registered: %O{handle}")

            if
                AssemblyDefinitionName.isNamed "System.Private.CoreLib" concrete.AssemblyFullName
                && concrete.Namespace = "System"
                && primitiveValueTypeNames.Contains concrete.Name
            then
                state, seenSoFar.Add (handle, Completed false), false
            else

            let assy =
                state.LoadedAssembly concrete.AssemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"assembly %O{concrete.AssemblyFullName} of concrete type %O{handle} is not loaded"
                )

            let td = assy.TypeDefs.[concrete.Definition.Get]

            if not (LoadedTypeInfo.isValueType baseClassTypes state._LoadedAssemblies td) then
                // Short-circuit: if the type itself is a reference type, we're done.
                state, seenSoFar.Add (handle, Completed true), true
            else

            // It's a value type, so we must check its fields.
            // Mark as in progress before recursing.
            let seenSoFarWithInProgress = seenSoFar.Add (handle, InProgress)

            // Concretize each instance field's signature against *this* instantiation's generic
            // arguments. A field is declared in the same assembly as its declaring type, so that
            // assembly is the resolution scope for the signature's tokens. There is no method
            // generic context: a field signature cannot mention one.
            let stateAfterFieldResolution, nonStaticFields =
                ((state, []), td.Fields)
                ||> List.fold (fun (currentState, acc) field ->
                    if field.IsStatic then
                        currentState, acc
                    else
                        let newState, fieldHandle =
                            TypeSystemState.concretizeType
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                currentState
                                concrete.AssemblyFullName
                                concrete.Generics
                                ImmutableArray.Empty
                                field.Signature

                        newState, BindingStop.bound fieldHandle :: acc
                )

            // Recurse through the fields, correctly propagating state.
            let finalState, finalSeenSoFar, fieldsContainRefType =
                ((stateAfterFieldResolution, seenSoFarWithInProgress, false), nonStaticFields)
                ||> List.fold (fun (currentState, currentSeenSoFar, currentResult) field ->
                    if currentResult then
                        (currentState, currentSeenSoFar, true) // Short-circuit
                    else
                        let newState, newSeenSoFar, fieldResult =
                            containsReferencesIn
                                loggerFactory
                                dotnetRuntimeDirs
                                baseClassTypes
                                currentState
                                currentSeenSoFar
                                field

                        (newState, newSeenSoFar, currentResult || fieldResult)
                )

            // Mark as completed with the final result before returning.
            let finalSeenSoFar = finalSeenSoFar.SetItem (handle, Completed fieldsContainRefType)

            finalState, finalSeenSoFar, fieldsContainRefType

    /// Whether storage of `handle` holds a managed reference or a byref, walking the fields of
    /// value types transitively: true for a reference type, an array or a byref, false for an
    /// unmanaged pointer or a function pointer.
    let containsReferences
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : TypeSystemState * bool
        =
        let state, _, result =
            containsReferencesIn loggerFactory dotnetRuntimeDirs baseClassTypes state ImmutableDictionary.Empty handle

        state, result
