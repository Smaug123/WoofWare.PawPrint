namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `IntrinsicHelpers.offsetManagedPointerByElements` is the shared element-offset walk behind
/// `Unsafe.Add`, `Unsafe.Subtract`, `Span<T>.get_Item` and friends. Every branch of it combines the
/// caller's element `offset` with a stored index or byte offset that PawPrint represents as an
/// int32.
///
/// Real IL does that arithmetic at native-int width (`sizeof !!T; conv.i; mul; add`), so an int32
/// product or sum that wraps is a *modelling* overflow, not the CLI's — and a wrapped result is not
/// merely imprecise, it puts the byref on the wrong side of the source address. Concretely,
/// `Unsafe.Add(ref a[1], Int32.MaxValue)` should land `+8589934592` bytes from `&a[0]`; wrapping the
/// index to `Int32.MinValue` reports `-8589934592`.
///
/// PawPrint cannot represent such a byref (the roots store int32 indices and byte offsets), and the
/// project prefers a loud refusal to a silently wrong answer, so these assert that the in-range
/// arithmetic is exact and that the out-of-range arithmetic fails loudly rather than wrapping.
[<TestFixture>]
module TestElementOffsetOverflow =

    // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over its
    // sinks, and disposing while the assembly is still live would silently drop events.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private state () : IlMachineState =
        // Factory intentionally undisposed: state.Logger outlives this scope.
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ti

    let private int32Handle : ConcreteTypeHandle = handleFor bct.Int32
    let private int64Handle : ConcreteTypeHandle = handleFor bct.Int64

    let private allocateIntArray (length : int) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        IlMachineState.allocateArray
            (ConcreteTypeHandle.OneDimArrayZero int32Handle)
            (fun () -> CliType.Numeric (CliNumericType.Int32 0))
            length
            state

    /// A byref at `&a[index]`. The index is deliberately allowed to sit outside the array's stored
    /// bounds: PawPrint models an out-of-bounds byref symbolically and only refuses it on
    /// dereference, which is exactly what lets a large `Unsafe.Add` be expressed at all.
    let private elementByref (arr : ManagedHeapAddress) (index : int) : EvalStackValue =
        EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, index), []))

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Uniform over the whole int32 range, with extra weight on the boundary values themselves.
    /// FsCheck's default `int` arbitrary is *size-bounded* — under `Quick` it stays roughly within
    /// [-100, 100] — so drawing from it would never approach the limits this property is about.
    let private genFullRangeInt32 : Gen<int> =
        Gen.frequency
            [
                3, Gen.choose (System.Int32.MinValue, System.Int32.MaxValue)
                1,
                Gen.elements
                    [
                        System.Int32.MinValue
                        System.Int32.MinValue + 1
                        -1
                        0
                        1
                        System.Int32.MaxValue - 1
                        System.Int32.MaxValue
                    ]
            ]

    /// Element index and offset whose sum stays inside int32 *by construction*: the offset is drawn
    /// from exactly the interval that keeps `index + offset` representable, rather than by
    /// generating freely and filtering or folding. Filtering here would bias the sample towards
    /// small indices — precisely the region where no wrap can ever happen.
    let private genRepresentableCase : Gen<int * int> =
        gen {
            let! index = genFullRangeInt32

            // Computed in int64 so the endpoints themselves cannot wrap; each clamps back into
            // int32 because one side of every `max`/`min` already is an int32 bound.
            let lo =
                max (int64<int> System.Int32.MinValue) (int64<int> System.Int32.MinValue - int64<int> index)

            let hi =
                min (int64<int> System.Int32.MaxValue) (int64<int> System.Int32.MaxValue - int64<int> index)

            let! offset = Gen.choose (int32<int64> lo, int32<int64> hi)
            return index, offset
        }

    [<Test>]
    let ``in-range element walk lands on index plus offset`` () : unit =
        // The property is only meaningful if it actually visits sums near the int32 boundary: that
        // is where an unchecked `i + offset` would wrap, and where a guard that clamped too
        // aggressively would start refusing valid walks. Count what the generator really produced
        // and assert the distribution rather than trusting it.
        let mutable extremeIndex = 0
        let mutable extremeSum = 0
        let mutable total = 0

        // 2^30: comfortably "near the boundary" without making the assertion below a coin flip.
        let extreme = 1073741824L

        let property ((index, offset) : int * int) : unit =
            let sum = int64<int> index + int64<int> offset
            total <- total + 1

            if abs (int64<int> index) > extreme then
                extremeIndex <- extremeIndex + 1

            if abs sum > extreme then
                extremeSum <- extremeSum + 1

            let arr, st = allocateIntArray 4 (state ())

            let result =
                IntrinsicHelpers.offsetManagedPointerByElements
                    bct
                    st
                    int32Handle
                    (int64<int> offset)
                    (elementByref arr index)
                |> fst

            // The int64 oracle: the walk is plain integer addition on the element index, and the
            // generator has already established the answer is representable.
            let expected = int32<int64> sum

            result
            |> shouldEqual (
                EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, expected), []))
            )

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genRepresentableCase) property)

        // A uniform draw over int32 puts just over half the mass beyond ±2^30, so a generator that
        // had silently reverted to FsCheck's size-bounded default would fail these outright.
        if extremeIndex * 4 < total then
            failwith
                $"generator explored too few extreme indices: %d{extremeIndex} of %d{total} exceeded 2^30, so the property is not testing the boundary it claims to"

        if extremeSum * 4 < total then
            failwith
                $"generator explored too few extreme sums: %d{extremeSum} of %d{total} exceeded 2^30, so a wrap near the int32 limit could go unnoticed"

    [<Test>]
    let ``element walk accepts a widened offset whose result is representable`` () : unit =
        // The offset is a *native-int* element count, so it must not be narrowed to int32 before
        // the walk decides what it can represent. Negating `Int32.MinValue` (what
        // `Unsafe.Subtract(ref p, Int32.MinValue)` does) produces exactly this offset, and from
        // index -1 it lands on `Int32.MaxValue` — representable, and what real .NET computes.
        let arr, st = allocateIntArray 4 (state ())

        let result =
            IntrinsicHelpers.offsetManagedPointerByElements bct st int32Handle 2147483648L (elementByref arr -1)
            |> fst

        result
        |> shouldEqual (
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, System.Int32.MaxValue), [])
            )
        )

    [<Test>]
    let ``bit-pattern walk accepts an offset beyond the int32 range`` () : unit =
        // A bit-pattern byref carries its whole address in an int64, so it has no int32 index to
        // overflow and must accept offsets the anchored roots cannot. `Unsafe.Subtract(ref
        // Unsafe.NullRef<byte>(), Int32.MinValue)` is the reachable shape; real .NET yields the bit
        // pattern 0x80000000.
        let byteHandle = handleFor bct.Byte

        let result =
            IntrinsicHelpers.offsetManagedPointerByElements
                bct
                (state ())
                byteHandle
                2147483648L
                (EvalStackValue.ManagedPointer ManagedPointerSource.Null)
            |> fst

        match result with
        | EvalStackValue.ManagedPointer ptr ->
            ManagedPointerSource.tryBitPatternBits ptr
            |> shouldEqual (ValueSome 2147483648L)
        | other -> failwith $"expected a bit-pattern byref, got %O{other}"

    [<Test>]
    let ``element walk refuses an offset too wide for any anchored root`` () : unit =
        // Beyond +/-(2^32 - 1) no int32 index can absorb the walk, whatever the source index is.
        let arr, st = allocateIntArray 4 (state ())

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements bct st int32Handle 4294967296L (elementByref arr 0)
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "4294967296"

    [<Test>]
    let ``element walk refuses an index that overflows int32`` () : unit =
        // `Unsafe.Add(ref a[1], Int32.MaxValue)`: real .NET lands
        // +8589934592 bytes from &a[0]; wrapping would report -8589934592.
        let arr, st = allocateIntArray 4 (state ())

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements
                    bct
                    st
                    int32Handle
                    (int64<int> System.Int32.MaxValue)
                    (elementByref arr 1)
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "2147483648"

    [<Test>]
    let ``element walk refuses an index that underflows int32`` () : unit =
        let arr, st = allocateIntArray 4 (state ())

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements
                    bct
                    st
                    int32Handle
                    (int64<int> System.Int32.MinValue)
                    (elementByref arr -1)
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "-2147483649"

    [<Test>]
    let ``byte-cursor walk refuses a byte delta that overflows int32`` () : unit =
        // `sizeof(T)` differing from the array's element stride, with a trailing `ReinterpretAs`,
        // routes through the byte cursor instead of the cell index. There the wrap is in
        // `tSize * offset` rather than `i + offset`: 8 bytes by 300_000_000 elements is 2.4e9,
        // which does not fit in the int32 `ByrefProjection.ByteOffset`.
        let arr, st = allocateIntArray 4 (state ())

        let int64View =
            AllConcreteTypes.lookup int64Handle concreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Int64 was not concretised")

        let src =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (arr, 0),
                    [ ByrefProjection.ReinterpretAs int64View ]
                )
            )

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                IntrinsicHelpers.offsetManagedPointerByElements bct st int64Handle 300000000L src
                |> ignore
            )

        ex.Message |> shouldContainText "TODO: byref element offset"
        ex.Message |> shouldContainText "2400000000"

    [<Test>]
    let ``in-range byte-cursor walk still succeeds`` () : unit =
        // The control for the case above: a byte delta that does fit must keep working, so the
        // guard is not refusing the whole byte-cursor path outright.
        let arr, st = allocateIntArray 4 (state ())

        let int64View =
            AllConcreteTypes.lookup int64Handle concreteTypes
            |> Option.defaultWith (fun () -> failwith "System.Int64 was not concretised")

        let src =
            EvalStackValue.ManagedPointer (
                ManagedPointerSource.Byref (
                    ByrefRoot.ArrayElement (arr, 0),
                    [ ByrefProjection.ReinterpretAs int64View ]
                )
            )

        let result =
            IntrinsicHelpers.offsetManagedPointerByElements bct st int64Handle 1L src |> fst

        // One `long` forward is 8 bytes, and the array's stride is 4, so the byte cursor
        // normalises cleanly back onto cell 2 rather than leaving a trailing `ByteOffset`.
        match result with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (actual, index), projs)) ->
            actual |> shouldEqual arr
            index |> shouldEqual 2

            projs
            |> List.tryLast
            |> shouldEqual (Some (ByrefProjection.ReinterpretAs int64View))
        | other -> failwith $"expected a byte-view byref into the array, got %O{other}"

    /// Uniform over the whole int64 range, with extra weight on the values where two's-complement
    /// arithmetic behaves differently from the integers. `Int64.MinValue` is the one whose negation
    /// is not itself representable, which is exactly what `Unsafe.Subtract` forms.
    let private genFullRangeInt64 : Gen<int64> =
        Gen.frequency
            [
                3, Gen.choose64 (System.Int64.MinValue, System.Int64.MaxValue)
                1,
                Gen.elements
                    [
                        System.Int64.MinValue
                        System.Int64.MinValue + 1L
                        -4294967296L
                        -2147483649L
                        -1L
                        0L
                        1L
                        2147483648L
                        4294967296L
                        System.Int64.MaxValue - 1L
                        System.Int64.MaxValue
                    ]
            ]

    /// Element sizes that scale the offset differently: one byte does not scale it at all, four is
    /// narrower than a machine word, and eight is one.
    let private genElementSize : Gen<int> = Gen.elements [ 1 ; 4 ; 8 ]

    /// `(start + offset * size) mod 2^64`, read back as a signed int64. Computed in `bigint` so the
    /// oracle cannot share a wrap with the implementation it is checking.
    let private wrappedAddress (start : int64) (offset : int64) (size : int) : int64 =
        let modulus = bigint 1 <<< 64

        let raw =
            (((bigint start + bigint offset * bigint size) % modulus) + modulus) % modulus

        if raw >= (bigint 1 <<< 63) then
            int64 (raw - modulus)
        else
            int64 raw

    [<Test>]
    let ``bit-pattern walk is exact modulo 2^64 for every native-width offset`` () : unit =
        // A bit-pattern byref carries its whole address in an int64 and has no int32 root to
        // overflow, so this branch answers rather than refusing however far the walk goes. Its
        // arithmetic is therefore the only place a native-width element offset is *used* rather
        // than rejected, and `Unsafe.Add`/`Unsafe.Subtract` reach it with offsets of any
        // magnitude now that they no longer narrow to int32 first.
        //
        // Real IL wraps here: `mul` and `add`/`sub` are two's-complement at native width, so
        // wrapping is the answer rather than a modelling limit. Hence the `bigint` oracle: an
        // implementation that threw on overflow, or that carried the sum in a wider type, would
        // never produce a *wrong* wrap and so could only be caught by an oracle computed outside
        // the arithmetic under test.
        let handleOfSize (size : int) : ConcreteTypeHandle =
            match size with
            | 1 -> handleFor bct.Byte
            | 4 -> int32Handle
            | 8 -> int64Handle
            | other -> failwith $"no element handle of size %d{other} in this fixture"

        // The property is only about wrapping if it actually wraps. Count it rather than assume.
        let mutable wrapped = 0
        let mutable total = 0

        // One machine state for the whole property: a bit-pattern walk allocates nothing, so
        // there is nothing to keep fresh between iterations, and `state ()` builds a logger
        // factory that outlives the call.
        let st = state ()

        let property ((start, offset, size) : int64 * int64 * int) : unit =
            total <- total + 1
            let expected = wrappedAddress start offset size

            // A walk whose exact (unwrapped) answer differs from the modular one is a wrap.
            if bigint expected <> bigint start + bigint offset * bigint size then
                wrapped <- wrapped + 1

            let result =
                IntrinsicHelpers.offsetManagedPointerByElements
                    bct
                    st
                    (handleOfSize size)
                    offset
                    (EvalStackValue.ManagedPointer (ManagedPointerSource.ofBitPattern start))
                |> fst

            match result with
            | EvalStackValue.ManagedPointer ptr ->
                ManagedPointerSource.tryBitPatternBits ptr |> shouldEqual (ValueSome expected)
            | other -> failwith $"expected a bit-pattern byref, got %O{other}"

        let genCase =
            gen {
                let! start = genFullRangeInt64
                let! offset = genFullRangeInt64
                let! size = genElementSize
                return start, offset, size
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCase) property)

        // Two uniform int64 draws put the great majority of the mass outside the range where the
        // sum fits, so a generator that had quietly reverted to a size-bounded default would fail
        // this outright and the property would be asserting nothing about wrapping.
        if wrapped * 2 < total then
            failwith
                $"generator produced too few wrapping walks: %d{wrapped} of %d{total}, so the modular arithmetic is not being exercised"

    [<Test>]
    let ``bit-pattern walk and its negation return to the start`` () : unit =
        // `Unsafe.Subtract(ref p, n)` is `Unsafe.Add(ref p, -n)`, and PawPrint implements it by
        // negating at int64 width. Negating `Int64.MinValue` wraps it back to itself, so the
        // composition is what settles whether that is faithful: examining the negated offset alone
        // could not tell a correct wrap from a lost sign.
        let st = state ()

        let property ((start, offset) : int64 * int64) : unit =
            let walk (offset : int64) (src : EvalStackValue) : EvalStackValue =
                IntrinsicHelpers.offsetManagedPointerByElements bct st int32Handle offset src
                |> fst

            let src = EvalStackValue.ManagedPointer (ManagedPointerSource.ofBitPattern start)

            // Away and back: `-offset` is formed exactly as the `Unsafe.Subtract` arm forms it.
            walk -offset (walk offset src) |> shouldEqual src

        let genCase =
            gen {
                let! start = genFullRangeInt64
                let! offset = genFullRangeInt64
                return start, offset
            }

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genCase) property)

    [<Test>]
    let ``element offset argument reads its bits at native width`` () : unit =
        // `UnsafeIntrinsics.EmitAdd`/`EmitSubtract` generate one IL body per member, shared by the
        // int32, IntPtr and nuint overloads: the declared parameter's signedness never reaches the
        // arithmetic, only its bits do. So a `nuint` offset above `Int64.MaxValue` must arrive as
        // the negative int64 sharing its bits, and an int32 one must be sign-extended rather than
        // zero-extended.
        let read (value : EvalStackValue) : int64 =
            Intrinsics.elementOffsetArgument "test" value

        read (EvalStackValue.Int32 (Int32Source.Verbatim -5)) |> shouldEqual -5L

        read (EvalStackValue.Int32 (Int32Source.Verbatim System.Int32.MinValue))
        |> shouldEqual -2147483648L

        read (EvalStackValue.NativeInt (NativeIntSource.Verbatim 3000000000L))
        |> shouldEqual 3000000000L

        // `unchecked((nuint)(-1))`, i.e. 2^64-1: one element *forwards* under Subtract.
        read (EvalStackValue.NativeInt (NativeIntSource.Verbatim -1L))
        |> shouldEqual -1L

        read (EvalStackValue.NativeInt (NativeIntSource.Verbatim System.Int64.MinValue))
        |> shouldEqual System.Int64.MinValue

        // The int32 overload does not narrow its caller's stack value either, so these must not
        // come back as the int32 they would truncate to. Measured on real .NET with a
        // `DynamicMethod` that emits `ldarg.0; ldc.i8 <bits>; conv.i; call Add<int>(ref int,
        // int32)` and returns the byte delta: 0x100000001 walks 17179869188 bytes rather than the
        // 4 a narrowed `1` would give, and 0xFFFFFFFF walks forwards by 4294967295 elements rather
        // than back by one. The substituted body runs before argument coercion, so there is no
        // narrowing step for the declared int32 width to drive.
        read (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0x100000001L))
        |> shouldEqual 4294967297L

        read (EvalStackValue.NativeInt (NativeIntSource.Verbatim 0xFFFFFFFFL))
        |> shouldEqual 4294967295L

    [<Test>]
    let ``element offset argument refuses a pointer whose bits are not modelled`` () : unit =
        // An anchored byref has no numeric offset to report, and synthesising one would register a
        // pointer identity for a value the offset arithmetic would then multiply.
        let arr, _ = allocateIntArray 4 (state ())

        let ex =
            Assert.Throws<System.Exception> (fun () ->
                Intrinsics.elementOffsetArgument "Unsafe.Subtract" (elementByref arr 0)
                |> ignore
            )

        ex.Message |> shouldContainText "refusing to read"
        ex.Message |> shouldContainText "element offset"
