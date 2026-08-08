namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The `Interlocked` intrinsics select their width by matching the declared parameter
/// types, then read the value argument off the stack. These tests pin what that read
/// accepts.
///
/// It is deliberately *not* "the matching slot only". The CLI coerces between int32 and
/// the pointer-sized integer types at a call boundary — `impImplicitIorI4Cast`
/// (importer.cpp:2459) runs on every call argument, and on 64-bit `varTypeIsI(TYP_LONG)`
/// holds — so a 64-bit-wide integer really can arrive at an int32 parameter, and CoreCLR
/// narrows it. Refusing that would reject legal IL.
///
/// What must be refused is narrowing a *pointer* to int32. `conv.i4` would synthesise
/// bits for one, which fabricates the answer and, worse, assigns the pointer a
/// `PointerHashCounters` identity — perturbing every synthesised pointer value later in
/// the run. An int32 parameter cannot legally receive a pointer, so this is pure downside.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestIntrinsicValueArguments =

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private byref : ManagedPointerSource =
        ManagedPointerSource.Byref (ByrefRoot.HeapValue (ManagedHeapAddress.ManagedHeapAddress 7), [])

    /// Integer-shaped stack values whose bits PawPrint knows exactly, paired with those bits.
    /// Every one of these is a legal int32 argument under the CLI coercion above.
    let private genExactIntegerValue : Gen<EvalStackValue * int64> =
        Gen.oneof
            [
                ArbMap.defaults
                |> ArbMap.generate<int32>
                |> Gen.map (fun i -> EvalStackValue.Int32 (Int32Source.Verbatim i), int64<int32> i)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (fun i -> EvalStackValue.Int64 (Int64Source.Verbatim i), i)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (fun i -> EvalStackValue.NativeInt (NativeIntSource.Verbatim i), i)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (fun bits -> EvalStackValue.Int64 (Int64Source.OpaqueHashBits bits), bits)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (fun bits -> EvalStackValue.NativeInt (NativeIntSource.OpaqueHashBits bits), bits)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (fun bits ->
                    EvalStackValue.NativeInt (
                        NativeIntSource.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits)
                    ),
                    bits
                )
                Gen.constant (EvalStackValue.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null), 0L)
                Gen.constant (EvalStackValue.ManagedPointer ManagedPointerSource.Null, 0L)
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map (fun bits ->
                    EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder bits), bits
                )
            ]

    /// Pointer-shaped integer values: legal-looking to `conv.i4`, but their bits are an
    /// address PawPrint does not model. Both the bare native-int and the `conv.i8`-widened
    /// spellings, since either can reach a call site.
    let private genPointerShapedValue : Gen<EvalStackValue> =
        let sources : Gen<NativeIntSource> =
            Gen.oneof
                [
                    Gen.choose64 (0L, 40L) |> Gen.map NativeIntSource.MethodHandlePtr
                    Gen.choose64 (0L, 40L) |> Gen.map NativeIntSource.FieldHandlePtr
                    Gen.choose (0, 5)
                    |> Gen.map (fun n ->
                        NativeIntSource.MethodTablePtr (RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Concrete n))
                    )
                    Gen.constant (
                        NativeIntSource.TypeHandlePtr (
                            RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.Byref (ConcreteTypeHandle.Concrete 1))
                        )
                    )
                    Gen.constant (NativeIntSource.ManagedPointer byref)
                ]

        Gen.oneof
            [
                sources |> Gen.map EvalStackValue.NativeInt
                sources
                |> Gen.map (fun src -> EvalStackValue.Int64 (Int64Source.WidenedNativeInt (src, true)))
            ]

    // ---- int32 ----

    [<Test>]
    let ``int32 value argument narrows every exactly-known integer the way conv i4 does`` () : unit =
        // The oracle for the accepting half: where the helper answers at all, it answers
        // exactly what `conv.i4` would. The helper is not a different truncation, only a
        // narrower domain.
        let property ((value, bits) : EvalStackValue * int64) : unit =
            let actual = Intrinsics.int32ValueArgument "Interlocked.Add" value

            let viaConv, counters = EvalStackValue.convToInt32 value PointerHashCounters.empty

            viaConv |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim actual))
            actual |> shouldEqual (int32 bits)

            // And no identity was registered by either route, since nothing was synthesised.
            counters |> shouldEqual PointerHashCounters.empty

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genExactIntegerValue) property)

    [<Test>]
    let ``int32 value argument refuses a pointer rather than synthesising its bits`` () : unit =
        // The whole point of the change. `conv.i4` answers these by minting bits and
        // registering a pointer identity; the intrinsic must not, because an int32
        // parameter cannot legally receive a pointer.
        let property (value : EvalStackValue) : unit =
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    Intrinsics.int32ValueArgument "Interlocked.Add" value |> ignore
                )

            exn.Message |> shouldContainText "Interlocked.Add"

            // Confirm the delta is real: `conv.i4` does answer, and registers an identity
            // while doing so. That side effect is what the refusal exists to prevent.
            let _, counters = EvalStackValue.convToInt32 value PointerHashCounters.empty

            match value with
            | EvalStackValue.NativeInt (NativeIntSource.ManagedPointer _)
            | EvalStackValue.Int64 (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer _, _)) ->
                // A byref narrows to `NarrowedManagedPointer` instead: no synthesis, but
                // also no number, which is why the helper still refuses it.
                counters |> shouldEqual PointerHashCounters.empty
            | _ -> counters.Assigned.Count |> shouldEqual 1

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genPointerShapedValue) property)

    [<Test>]
    let ``int32 value argument refuses a truncated byref`` () : unit =
        // Already in the int32 slot, but with no numeric value PawPrint can state: `conv.i4`
        // kept the low half of an address that was never modelled. The diagnostic comes from
        // `Int32Source.value`, so it names the byref rather than the slot.
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                Intrinsics.int32ValueArgument
                    "Interlocked.Or"
                    (EvalStackValue.Int32 (Int32Source.NarrowedManagedPointer byref))
                |> ignore
            )

        exn.Message |> shouldContainText "Interlocked.Or"
        exn.Message |> shouldContainText "refusing to use managed pointer"

    [<Test>]
    let ``int32 value argument coerces ldnull to zero, where conv i4 refuses it`` () : unit =
        // The one shape where call-boundary coercion and the `conv.i4` opcode genuinely
        // disagree, so it cannot live in the oracle above. `impImplicitIorI4Cast` retypes a
        // zero `TYP_REF` constant to a pointer-sized integer outright ("We also allow an
        // implicit conversion of a ldnull into a TYP_I_IMPL(0)"), and narrows from there.
        // `conv.i4` on an object reference is invalid IL, so `convToInt32` rightly refuses.
        Intrinsics.int32ValueArgument "Interlocked.Add" EvalStackValue.NullObjectRef
        |> shouldEqual 0

        let convRefuses =
            Assert.Throws<System.Exception> (fun () ->
                EvalStackValue.convToInt32 EvalStackValue.NullObjectRef PointerHashCounters.empty
                |> ignore
            )

        convRefuses.Message
        |> shouldContainText "refusing to convert null object reference"

    [<Test>]
    let ``int32 value argument refuses a float`` () : unit =
        // `impImplicitR4orR8Cast` converts only between R4 and R8, so there is no
        // float-to-integer coercion at a call boundary to honour.
        let exn =
            Assert.Throws<System.Exception> (fun () ->
                Intrinsics.int32ValueArgument "Interlocked.Add" (EvalStackValue.Float 1.5)
                |> ignore
            )

        exn.Message |> shouldContainText "not floats"

    [<Test>]
    let ``int32 value argument refuses addresses PawPrint does not model`` () : unit =
        // CoreCLR would narrow an object reference or a byref, having a real address to
        // narrow. PawPrint has none, so it refuses rather than inventing one.
        for value in
            [
                EvalStackValue.ObjectRef (ManagedHeapAddress.ManagedHeapAddress 11)
                EvalStackValue.ManagedPointer byref
            ] do
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    Intrinsics.int32ValueArgument "Interlocked.Add" value |> ignore
                )

            exn.Message |> shouldContainText "an address PawPrint does not model"

    // ---- int64 ----

    [<Test>]
    let ``int64 value argument passes any Int64Source through unchanged`` () : unit =
        // Deliberately permissive about *which* Int64Source: `Interlocked.And` / `Or` route
        // the operand through `Int64Source.bitAnd` / `bitOr`, which synthesise hash bits for
        // a pointer-derived value rather than failing. Widening fabricates nothing, so there
        // is nothing here for this helper to refuse.
        let sources : Int64Source list =
            [
                Int64Source.Verbatim 42L
                Int64Source.OpaqueHashBits 0x40L
                Int64Source.WidenedNativeInt (NativeIntSource.MethodHandlePtr 17L, true)
            ]

        for src in sources do
            Intrinsics.int64ValueArgument "Interlocked.Or" (EvalStackValue.Int64 src)
            |> shouldEqual src

    [<Test>]
    let ``int64 value argument sign-extends an int32 stack value`` () : unit =
        // `impImplicitIorI4Cast` widens with zeroExtend = false, so this is sign extension.
        let property (i : int32) : unit =
            Intrinsics.int64ValueArgument "Interlocked.Add" (EvalStackValue.Int32 (Int32Source.Verbatim i))
            |> shouldEqual (Int64Source.Verbatim (int64<int32> i))

        Check.One (propertyConfig, Prop.forAll (ArbMap.defaults |> ArbMap.arbitrary<int32>) property)

    [<Test>]
    let ``int64 value argument keeps pointer provenance through the widening`` () : unit =
        // Widening is bit-preserving on 64-bit, so a pointer survives as a widened native int
        // rather than becoming opaque bits. This is what lets `Interlocked.And` mask a
        // pointer-derived location, and it must not silently decay to synthesised bits.
        let property (handle : int64) : unit =
            let src = NativeIntSource.MethodHandlePtr handle

            Intrinsics.int64ValueArgument "Interlocked.And" (EvalStackValue.NativeInt src)
            |> shouldEqual (Int64Source.WidenedNativeInt (src, true))

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen (Gen.choose64 (0L, 40L))) property)

    [<Test>]
    let ``int64 value argument coerces the pointer-sized shapes CoreCLR coerces`` () : unit =
        // `ldnull` and a null byref both arrive as zero; a real byref keeps its provenance
        // so the arithmetic can refuse it with a diagnostic that names the byref.
        Intrinsics.int64ValueArgument "Interlocked.Or" EvalStackValue.NullObjectRef
        |> shouldEqual (Int64Source.Verbatim 0L)

        Intrinsics.int64ValueArgument "Interlocked.Or" (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
        |> shouldEqual (Int64Source.Verbatim 0L)

        Intrinsics.int64ValueArgument
            "Interlocked.Or"
            (EvalStackValue.ManagedPointer (ManagedPointerSource.NativeIntPlaceholder 0x20L))
        |> shouldEqual (Int64Source.Verbatim 0x20L)

        Intrinsics.int64ValueArgument "Interlocked.Or" (EvalStackValue.ManagedPointer byref)
        |> shouldEqual (Int64Source.WidenedNativeInt (NativeIntSource.ManagedPointer byref, true))

    [<Test>]
    let ``int64 value argument refuses non-integer shapes`` () : unit =
        for value in
            [
                EvalStackValue.Float 1.5
                EvalStackValue.ObjectRef (ManagedHeapAddress.ManagedHeapAddress 11)
            ] do
            let exn =
                Assert.Throws<System.Exception> (fun () ->
                    Intrinsics.int64ValueArgument "Interlocked.Add" value |> ignore
                )

            exn.Message |> shouldContainText "expected an integer value argument"
