namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open Microsoft.Extensions.Logging

/// ECMA-335 III.4.1's `box`, as a function of a value and its type rather than of the opcode's
/// operand stack.
///
/// The `box` opcode is not the only place the runtime boxes: `constrained.` callvirt boxes a
/// value-type receiver whose method it inherits from Object/ValueType/Enum (III.2.1 case 3), and
/// the reflection-invocation QCall boxes a value-type return. Every one of those must apply the
/// same Nullable rule, so they share this module rather than each rebuilding the heap object.
[<RequireQualifiedAccess>]
module internal Boxing =
    /// Wrap a value-type value in a fresh heap object, returning the box's address.
    ///
    /// This is the plain value-type half of the `box` opcode: `Nullable<T>` must be normalised by
    /// the caller (ECMA-335 III.4.1 boxes a `Nullable<T>` as a `T`, or as null, so a box whose
    /// declared type *is* `Nullable<T>` is a shape this never produces, and a `Nullable<T>` handle
    /// is refused), and reference types are a no-op that the caller should not route here at all.
    /// `boxValue` is the whole rule, and is what a caller holding an arbitrary value type wants.
    ///
    /// Callers outside the `box` opcode exist because the CLR boxes in places that have no IL:
    /// the `RuntimeMethodHandle_InvokeMethod` QCall must box a value-type return
    /// (`InvokeUtil::CreateObjectAfterInvoke`, reflectioninvocation.cpp:678) before handing it back
    /// through its `ObjectHandleOnStack`.
    let boxValueType
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeHandle : ConcreteTypeHandle)
        (toBox : EvalStackValue)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let targetType, defn =
            AllConcreteTypes.tryTypeInfo state._LoadedAssemblies state.ConcreteTypes typeHandle
            |> Option.defaultWith (fun () ->
                failwith $"boxValueType: ConcreteTypeHandle %O{typeHandle} is not registered in AllConcreteTypes"
            )

        if not (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn) then
            failwith
                $"boxValueType: %s{defn.Namespace}.%s{defn.Name} is not a value type; boxing a reference type is a no-op and must not reach here"

        if InternalTypeKind.kind baseClassTypes targetType = InternalTypeKind.Nullable then
            failwith
                $"boxValueType: %O{typeHandle} is a Nullable`1, which never boxes as itself (ECMA-335 III.4.1); the caller must apply the Nullable rule, as `boxValue` does"

        let cvt, state =
            match toBox with
            | EvalStackValue.UserDefinedValueType cvt ->
                // Already have the CliValueType with the right field structure
                cvt, state
            | _ ->
                // Primitive value on the eval stack (Int32, Int64, Float, etc.)
                // Construct a CliValueType from the type definition's instance fields
                let targetAssembly =
                    state._LoadedAssemblies.ByDefinitionName targetType.AssemblyFullName

                let instanceFields =
                    defn.Fields
                    |> List.filter (fun field -> not (field.Attributes.HasFlag FieldAttributes.Static))

                let state, fieldValues =
                    ((state, []), instanceFields)
                    ||> List.fold (fun (state, acc) field ->
                        let state, fieldZero, fieldTypeHandle =
                            IlMachineState.cliTypeZeroOf
                                loggerFactory
                                baseClassTypes
                                targetAssembly
                                field.Signature
                                targetType.Generics
                                ImmutableArray.Empty
                                state

                        let coerced = EvalStackValue.toCliTypeCoerced fieldZero toBox

                        let cliField : CliField =
                            {
                                Id = FieldId.metadata typeHandle field.Handle field.Name
                                Name = field.Name
                                Contents = coerced
                                Offset = field.Offset
                                Type = fieldTypeHandle
                                MarshallingDescriptor = field.MarshallingDescriptor
                            }

                        state, cliField :: acc
                    )

                let cvt =
                    List.rev fieldValues
                    // Unreachable for an inline array (an N>1 inline array is never
                    // primitive-like, so it always arrives as `UserDefinedValueType` and takes the
                    // branch above), but routed through the shared expansion rather than relying
                    // on that being true here.
                    |> InlineArrayStorage.expand
                        (fun () -> $"%s{defn.Namespace}.%s{defn.Name}")
                        defn.Layout
                        (InlineArrayStorage.effectiveLength
                            (DumpedAssembly.isValueType baseClassTypes state._LoadedAssemblies defn)
                            defn.InlineArrayLength)
                    |> CliValueType.OfFields
                        baseClassTypes
                        state.ConcreteTypes
                        typeHandle
                        (DeclaredTypeFacts.ofTypeInfo baseClassTypes state._LoadedAssemblies defn)

                cvt, state

        IlMachineState.allocateManagedObject typeHandle cvt state

    /// Box a value whose type is the value type `typeHandle`, exactly as the `box` opcode would
    /// (ECMA-335 III.4.1), answering the reference that `box` pushes.
    ///
    /// A `Nullable<T>` never boxes as itself: one without a value boxes to null, and one with a
    /// value boxes its `T`. Every other value type boxes as itself. `toBox` is the value as it
    /// sits on the evaluation stack, so a primitive-like value type arrives flattened and a
    /// `Nullable<T>` arrives as `UserDefinedValueType`.
    ///
    /// Reference types are not accepted: boxing one is a no-op the caller can perform by itself.
    let boxValue
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeHandle : ConcreteTypeHandle)
        (toBox : EvalStackValue)
        (state : IlMachineState)
        : EvalStackValue * IlMachineState
        =
        let targetType =
            AllConcreteTypes.lookup typeHandle state.ConcreteTypes
            |> Option.defaultWith (fun () ->
                failwith $"boxValue: ConcreteTypeHandle %O{typeHandle} is not registered in AllConcreteTypes"
            )

        if InternalTypeKind.kind baseClassTypes targetType = InternalTypeKind.Nullable then
            match toBox with
            | EvalStackValue.UserDefinedValueType cvt ->
                let hasValueField =
                    IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "hasValue"

                match CliValueType.DereferenceFieldById hasValueField cvt with
                | CliType.Bool 0uy -> EvalStackValue.NullObjectRef, state
                | CliType.Bool _ ->
                    let underlyingTypeHandle = targetType.Generics.[0]

                    let valueField =
                        IlMachineState.requiredOwnInstanceFieldId state cvt.Declared "value"

                    let value = CliValueType.DereferenceFieldById valueField cvt

                    let addr, state =
                        match value with
                        | CliType.ValueType existingCvt ->
                            // The stored `T` already has its field structure, so there is nothing
                            // to rebuild; this also keeps a primitive-like `T` (an enum, IntPtr)
                            // from being flattened and re-wrapped on the way into the box.
                            IlMachineState.allocateManagedObject underlyingTypeHandle existingCvt state
                        | _ ->
                            boxValueType
                                loggerFactory
                                baseClassTypes
                                underlyingTypeHandle
                                (EvalStackValue.ofCliType value)
                                state

                    EvalStackValue.ObjectRef addr, state
                | other -> failwith $"boxValue: expected Bool for Nullable`1's hasValue field, got %O{other}"
            | other -> failwith $"boxValue: expected a Nullable`1 to arrive as UserDefinedValueType, got %O{other}"
        else
            let addr, state = boxValueType loggerFactory baseClassTypes typeHandle toBox state
            EvalStackValue.ObjectRef addr, state
