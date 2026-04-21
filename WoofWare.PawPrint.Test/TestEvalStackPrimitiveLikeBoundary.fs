namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestEvalStackPrimitiveLikeBoundary =

    // Load corelib once and concretise every primitive-like BCL type so that
    // OfFields can populate _PrimitiveLikeKind via the real lookup path.

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt ti

    let private wrap (declared : ConcreteTypeHandle) (fieldName : string) (contents : CliType) : CliValueType =
        {
            CliField.Name = fieldName
            Contents = contents
            Offset = None
            Type = declared
        }
        |> List.singleton
        |> CliValueType.OfFields bct allCt declared Layout.Default

    // -- IntPtr / UIntPtr flatten + rewrap --------------------------------------------------

    [<Test>]
    let ``IntPtr flattens to NativeInt on push`` () : unit =
        let intPtrHandle = handleFor bct.IntPtr

        let wrapped =
            wrap intPtrHandle "_value" (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 42L)))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim 42L) -> ()
        | other -> failwithf "Expected NativeInt(Verbatim 42L), got %A" other

    [<Test>]
    let ``UIntPtr flattens to NativeInt on push`` () : unit =
        let h = handleFor bct.UIntPtr

        let wrapped =
            wrap h "_value" (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 7L)))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.NativeInt (NativeIntSource.Verbatim 7L) -> ()
        | other -> failwithf "Expected NativeInt(Verbatim 7L), got %A" other

    [<Test>]
    let ``IntPtr round-trips through the stack boundary`` () : unit =
        let intPtrHandle = handleFor bct.IntPtr

        let contents =
            CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1234L))

        let wrapped = wrap intPtrHandle "_value" contents
        let stored = CliType.ValueType wrapped

        let roundTripped =
            EvalStackValue.toCliTypeCoerced stored (EvalStackValue.ofCliType stored)

        match roundTripped with
        | CliType.ValueType vt ->
            vt.PrimitiveLikeKind |> shouldEqual (Some PrimitiveLikeKind.FlattenToNativeInt)

            match CliValueType.TryExactlyOneField vt with
            | Some field ->
                match field.Contents with
                | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1234L)) -> ()
                | other -> failwithf "Expected wrapped NativeInt 1234, got %A" other
            | None -> failwith "rewrapped IntPtr had no single field"
        | other -> failwithf "Expected CliType.ValueType after rewrap, got %A" other

    // -- RuntimeTypeHandle (flattens to ObjectRef) ------------------------------------------

    [<Test>]
    let ``RuntimeTypeHandle flattens to ObjectRef on push`` () : unit =
        let h = handleFor bct.RuntimeTypeHandle
        let addr = ManagedHeapAddress.ManagedHeapAddress 99
        let wrapped = wrap h "m_type" (CliType.ObjectRef (Some addr))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.ObjectRef a -> a |> shouldEqual addr
        | other -> failwithf "Expected ObjectRef %O, got %A" addr other

    [<Test>]
    let ``RuntimeTypeHandle wrapping a null ref flattens to NullObjectRef`` () : unit =
        let h = handleFor bct.RuntimeTypeHandle
        let wrapped = wrap h "m_type" (CliType.ObjectRef None)

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.NullObjectRef -> ()
        | other -> failwithf "Expected NullObjectRef, got %A" other

    [<Test>]
    let ``RuntimeTypeHandle round-trips through the stack boundary`` () : unit =
        let h = handleFor bct.RuntimeTypeHandle
        let addr = ManagedHeapAddress.ManagedHeapAddress 7
        let wrapped = wrap h "m_type" (CliType.ObjectRef (Some addr))
        let stored = CliType.ValueType wrapped

        let roundTripped =
            EvalStackValue.toCliTypeCoerced stored (EvalStackValue.ofCliType stored)

        match roundTripped with
        | CliType.ValueType vt ->
            vt.PrimitiveLikeKind |> shouldEqual (Some PrimitiveLikeKind.FlattenToObjectRef)

            match CliValueType.TryExactlyOneField vt with
            | Some field ->
                match field.Contents with
                | CliType.ObjectRef (Some a) -> a |> shouldEqual addr
                | other -> failwithf "Expected wrapped ObjectRef %O, got %A" addr other
            | None -> failwith "rewrapped RuntimeTypeHandle had no single field"
        | other -> failwithf "Expected CliType.ValueType after rewrap, got %A" other

    // -- RuntimeFieldHandleInternal (flattens to RuntimePointer) ----------------------------

    [<Test>]
    let ``RuntimeFieldHandleInternal flattens to native int carrying a field-handle pointer`` () : unit =
        let h = handleFor bct.RuntimeFieldHandleInternal

        let wrapped =
            wrap h "m_handle" (CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle 17L))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.NativeInt (NativeIntSource.FieldHandlePtr 17L) -> ()
        | other -> failwithf "Expected NativeInt(FieldHandlePtr 17), got %A" other

    // -- Non-primitive-like structs are not flattened ---------------------------------------

    [<Test>]
    let ``Non-primitive-like struct is pushed as UserDefinedValueType`` () : unit =
        // TypedReference is nominally a value type but not in the primitive-like registry.
        let h = handleFor bct.TypedReference

        let wrapped = wrap h "Value" (CliType.Numeric (CliNumericType.Int32 42))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.UserDefinedValueType vt -> vt.PrimitiveLikeKind |> shouldEqual None
        | other -> failwithf "Expected UserDefinedValueType for non-primitive-like struct, got %A" other

    // -- Push-time invariant ---------------------------------------------------------------

    [<Test>]
    let ``Push' rejects a primitive-like struct wrapped as UserDefinedValueType`` () : unit =
        // Bypass the flattening `Push (CliType)` path and construct the invariant-violating
        // EvalStackValue directly. This must be rejected by `Push'`.
        let intPtrHandle = handleFor bct.IntPtr

        let wrapped =
            wrap intPtrHandle "_value" (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1L)))

        let illegal = EvalStackValue.UserDefinedValueType wrapped

        let thrown =
            try
                EvalStack.Push' illegal EvalStack.Empty |> ignore
                None
            with ex ->
                Some ex.Message

        match thrown with
        | Some message ->
            message |> shouldContainText "eval-stack invariant violated"
            message |> shouldContainText "primitive-like"
        | None -> failwith "expected Push' to reject primitive-like UserDefinedValueType"

    [<Test>]
    let ``Push' accepts a non-primitive-like UserDefinedValueType`` () : unit =
        let h = handleFor bct.TypedReference

        let wrapped = wrap h "Value" (CliType.Numeric (CliNumericType.Int32 42))
        let legal = EvalStackValue.UserDefinedValueType wrapped

        EvalStack.Push' legal EvalStack.Empty |> ignore

    // -- Invariant held across every primitive-like kind -----------------------------------

    [<Test>]
    let ``every primitive-like type flattens to a non-UserDefinedValueType on push`` () : unit =
        // For each entry in the primitive-like registry, build a storage value and verify
        // that pushing it never yields UserDefinedValueType — the executable form of the
        // plan's stack-boundary invariant.
        let cases : (string * CliValueType) list =
            [
                "IntPtr",
                wrap
                    (handleFor bct.IntPtr)
                    "_value"
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1L)))
                "UIntPtr",
                wrap
                    (handleFor bct.UIntPtr)
                    "_value"
                    (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 2L)))
                "RuntimeTypeHandle",
                wrap (handleFor bct.RuntimeTypeHandle) "m_type" (CliType.ObjectRef (Some (ManagedHeapAddress 3)))
                "RuntimeFieldHandleInternal",
                wrap
                    (handleFor bct.RuntimeFieldHandleInternal)
                    "m_handle"
                    (CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle 4L))
            ]

        for name, vt in cases do
            vt.PrimitiveLikeKind |> shouldNotEqual None |> (fun () -> ())

            match EvalStackValue.ofCliType (CliType.ValueType vt) with
            | EvalStackValue.UserDefinedValueType _ ->
                failwithf "primitive-like type %s flattened into UserDefinedValueType" name
            | _ -> ()

    [<Test>]
    let ``round-trip holds for every primitive-like kind`` () : unit =
        let cases : (string * CliType) list =
            [
                "IntPtr",
                CliType.ValueType (
                    wrap
                        (handleFor bct.IntPtr)
                        "_value"
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 101L)))
                )
                "UIntPtr",
                CliType.ValueType (
                    wrap
                        (handleFor bct.UIntPtr)
                        "_value"
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 202L)))
                )
                "RuntimeTypeHandle",
                CliType.ValueType (
                    wrap (handleFor bct.RuntimeTypeHandle) "m_type" (CliType.ObjectRef (Some (ManagedHeapAddress 303)))
                )
                "RuntimeFieldHandleInternal",
                CliType.ValueType (
                    wrap
                        (handleFor bct.RuntimeFieldHandleInternal)
                        "m_handle"
                        (CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle 404L))
                )
            ]

        for name, stored in cases do
            let roundTripped =
                EvalStackValue.toCliTypeCoerced stored (EvalStackValue.ofCliType stored)

            if roundTripped <> stored then
                failwithf "round-trip failed for %s: %A -> %A" name stored roundTripped
