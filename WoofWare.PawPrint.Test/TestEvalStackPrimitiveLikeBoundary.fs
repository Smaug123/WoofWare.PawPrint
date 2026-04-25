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

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private allCt : AllConcreteTypes =
        let concreteTypes = Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

        match bct.ByReference with
        | Some byReference when byReference.Generics.IsEmpty ->
            match AllConcreteTypes.findExistingNonGenericConcreteType concreteTypes byReference.Identity with
            | Some _ -> concreteTypes
            | None ->
                let byReferenceConcrete =
                    ConcreteType.makeFromIdentity
                        byReference.Identity
                        byReference.Namespace
                        byReference.Name
                        ImmutableArray.Empty

                AllConcreteTypes.add byReferenceConcrete concreteTypes |> snd
        | _ -> concreteTypes

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt ti

    let private wrap (declared : ConcreteTypeHandle) (fieldName : string) (contents : CliType) : CliValueType =
        {
            CliField.Id = FieldId.named fieldName
            CliField.Name = fieldName
            Contents = contents
            Offset = None
            Type = declared
        }
        |> List.singleton
        |> CliValueType.OfFields bct allCt declared Layout.Default

    let private wrapSingleField
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (contents : CliType)
        : CliValueType
        =
        let field =
            ti.Fields |> List.filter (fun field -> not field.IsStatic) |> List.exactlyOne

        wrap (handleFor ti) field.Name contents

    let private primitiveLikeStoredCases () : (string * CliType) list =
        let baseCases : (string * CliType) list =
            [
                "IntPtr",
                CliType.ValueType (
                    wrapSingleField
                        bct.IntPtr
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 101L)))
                )
                "UIntPtr",
                CliType.ValueType (
                    wrapSingleField
                        bct.UIntPtr
                        (CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 202L)))
                )
                "RuntimeTypeHandle",
                CliType.ValueType (
                    wrapSingleField bct.RuntimeTypeHandle (CliType.ObjectRef (Some (ManagedHeapAddress 303)))
                )
                "RuntimeMethodHandle",
                CliType.ValueType (
                    wrapSingleField bct.RuntimeMethodHandle (CliType.ObjectRef (Some (ManagedHeapAddress 404)))
                )
                "RuntimeMethodHandleInternal",
                CliType.ValueType (
                    wrapSingleField
                        bct.RuntimeMethodHandleInternal
                        (CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle 454L))
                )
                "RuntimeFieldHandle",
                CliType.ValueType (
                    wrapSingleField bct.RuntimeFieldHandle (CliType.ObjectRef (Some (ManagedHeapAddress 505)))
                )
                "RuntimeFieldHandleInternal",
                CliType.ValueType (
                    wrapSingleField
                        bct.RuntimeFieldHandleInternal
                        (CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle 606L))
                )
            ]

        match bct.ByReference with
        | Some byReference when byReference.Generics.IsEmpty ->
            let ptr =
                ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress 707), [])

            baseCases
            @ [
                "ByReference",
                CliType.ValueType (wrapSingleField byReference (CliType.RuntimePointer (CliRuntimePointer.Managed ptr)))
            ]
        | _ -> baseCases

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

            match (CliValueType.PrimitiveLikeField vt).Contents with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 1234L)) -> ()
            | other -> failwithf "Expected wrapped NativeInt 1234, got %A" other
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

            match (CliValueType.PrimitiveLikeField vt).Contents with
            | CliType.ObjectRef (Some a) -> a |> shouldEqual addr
            | other -> failwithf "Expected wrapped ObjectRef %O, got %A" addr other
        | other -> failwithf "Expected CliType.ValueType after rewrap, got %A" other

    [<Test>]
    let ``RuntimeMethodHandle flattens to ObjectRef on push`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 199

        let wrapped =
            wrapSingleField bct.RuntimeMethodHandle (CliType.ObjectRef (Some addr))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.ObjectRef a -> a |> shouldEqual addr
        | other -> failwithf "Expected ObjectRef %O, got %A" addr other

    [<Test>]
    let ``RuntimeFieldHandle flattens to ObjectRef on push`` () : unit =
        let addr = ManagedHeapAddress.ManagedHeapAddress 299
        let wrapped = wrapSingleField bct.RuntimeFieldHandle (CliType.ObjectRef (Some addr))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.ObjectRef a -> a |> shouldEqual addr
        | other -> failwithf "Expected ObjectRef %O, got %A" addr other

    // -- RuntimeFieldHandleInternal (flattens to RuntimePointer) ----------------------------

    [<Test>]
    let ``RuntimeFieldHandleInternal flattens to native int carrying a field-handle pointer`` () : unit =
        let h = handleFor bct.RuntimeFieldHandleInternal

        let wrapped =
            wrap h "m_handle" (CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle 17L))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.NativeInt (NativeIntSource.FieldHandlePtr 17L) -> ()
        | other -> failwithf "Expected NativeInt(FieldHandlePtr 17), got %A" other

    [<Test>]
    let ``RuntimeMethodHandleInternal flattens to native int carrying a method-handle pointer`` () : unit =
        let h = handleFor bct.RuntimeMethodHandleInternal

        let wrapped =
            wrap h "m_handle" (CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle 18L))

        match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
        | EvalStackValue.NativeInt (NativeIntSource.MethodHandlePtr 18L) -> ()
        | other -> failwithf "Expected NativeInt(MethodHandlePtr 18), got %A" other

    [<Test>]
    let ``ByReference flattens to ManagedPointer on push`` () : unit =
        match bct.ByReference with
        | None -> Assert.Inconclusive "corelib under test does not expose System.ByReference"
        | Some byReference when not byReference.Generics.IsEmpty ->
            Assert.Inconclusive
                "boundary fixture currently constructs only the non-generic System.ByReference storage form"
        | Some byReference ->
            let src =
                ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress 23), [])

            let wrapped =
                wrapSingleField byReference (CliType.RuntimePointer (CliRuntimePointer.Managed src))

            match EvalStackValue.ofCliType (CliType.ValueType wrapped) with
            | EvalStackValue.ManagedPointer actual -> actual |> shouldEqual src
            | other -> failwithf "Expected ManagedPointer %O, got %A" src other

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
        for name, stored in primitiveLikeStoredCases () do
            let vt =
                match stored with
                | CliType.ValueType vt -> vt
                | other -> failwithf "primitive-like storage case %s was unexpectedly not a value type: %A" name other

            vt.PrimitiveLikeKind |> shouldNotEqual None |> (fun () -> ())

            match EvalStackValue.ofCliType stored with
            | EvalStackValue.UserDefinedValueType _ ->
                failwithf "primitive-like type %s flattened into UserDefinedValueType" name
            | _ -> ()

    [<Test>]
    let ``round-trip holds for every primitive-like kind`` () : unit =
        for name, stored in primitiveLikeStoredCases () do
            let roundTripped =
                EvalStackValue.toCliTypeCoerced stored (EvalStackValue.ofCliType stored)

            if roundTripped <> stored then
                failwithf "round-trip failed for %s: %A -> %A" name stored roundTripped
