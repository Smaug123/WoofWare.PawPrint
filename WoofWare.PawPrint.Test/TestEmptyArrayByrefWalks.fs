namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// An empty array is still an array of something. Byref arithmetic over one must therefore
/// give the same answer as over a populated array of the same element type — the walk is a
/// question about the element type, not about how many cells happen to exist.
///
/// Before the stride was recorded on `ArrayShape`, it could only be recovered by measuring a
/// stored cell, so every one of these paths carried an empty-array fallback that substituted
/// something else: `sizeof(T)` from the *calling* method, or a zero that silently disabled
/// byte-offset normalisation. Those fallbacks disagreed with the populated case, which is
/// what these tests pin.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEmptyArrayByrefWalks =
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    let private state (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory) : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private handleFor (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ty

    let private concreteTypeFor (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteType<ConcreteTypeHandle> =
        match AllConcreteTypes.lookup (handleFor ty) concreteTypes with
        | Some t -> t
        | None -> failwith $"%s{ty.Name} is not concretized"

    /// Walk `offset` elements of `elementType` from index 0 of a fresh `int[len]`, through a
    /// byref carrying `projections`, and report the resulting cell index and projections.
    /// The array address is dropped so that the empty and populated cases are comparable.
    let private walk
        (len : int)
        (elementType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (projections : ByrefProjection list)
        (offset : int64)
        : int * ByrefProjection list
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let state = state loggerFactory

        let int32Handle = handleFor baseClassTypes.Int32

        let zero, state =
            IlMachineState.cliTypeZeroOfHandle state baseClassTypes int32Handle

        let arr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero int32Handle) (fun () -> zero) len state

        let src =
            ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 0), projections)
            |> EvalStackValue.ManagedPointer

        let result, _ =
            IntrinsicHelpers.offsetManagedPointerByElements baseClassTypes state (handleFor elementType) offset src

        match result with
        | EvalStackValue.ManagedPointer (ManagedPointerSource.Byref (ByrefRoot.ArrayElement (_, index), projs)) ->
            index, projs
        | other -> failwith $"expected an array-element byref, got %O{other}"

    [<Test>]
    let ``walking an empty array agrees with a populated one, at the element type`` () : unit =
        // `T` is the element type, so cell-index arithmetic applies to both.
        walk 0 baseClassTypes.Int32 [] 3L
        |> shouldEqual (walk 8 baseClassTypes.Int32 [] 3L)

    [<Test>]
    let ``walking an empty array agrees with a populated one, under a byte view`` () : unit =
        // The case the old empty-array fallback got wrong. `T` is `byte` while the array's
        // element is `int`, so eight elements of `T` is eight *bytes* — two cells, not eight.
        //
        // Substituting `sizeof(T)` for the stride of an empty array made the two sizes compare
        // equal, which sent this down the cell-index branch and produced cell 8: four times too
        // far, and silently, since an empty array has no cell whose contents would contradict
        // it. The populated array took the byte-cursor branch and correctly reached cell 2.
        let byteView =
            [ ByrefProjection.ReinterpretAs (concreteTypeFor baseClassTypes.Byte) ]

        let populated = walk 8 baseClassTypes.Byte byteView 8L
        let empty = walk 0 baseClassTypes.Byte byteView 8L

        empty |> shouldEqual populated

        // Pin the value too, so that "they agree" cannot be satisfied by both being wrong.
        fst populated |> shouldEqual 2

    [<Test>]
    let ``a byte-view walk that lands mid-cell keeps the remainder as a byte cursor`` () : unit =
        // Same reasoning one step further: 6 bytes is one whole cell plus 2, so normalisation
        // folds one cell and keeps the remainder. Distinguishes correct normalisation from a
        // rule that merely divides and discards.
        let byteView =
            [ ByrefProjection.ReinterpretAs (concreteTypeFor baseClassTypes.Byte) ]

        let populated = walk 8 baseClassTypes.Byte byteView 6L
        let empty = walk 0 baseClassTypes.Byte byteView 6L

        empty |> shouldEqual populated
        fst populated |> shouldEqual 1

        match snd populated with
        | [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset 2 ] -> ()
        | other -> failwith $"expected a trailing 2-byte cursor, got %O{other}"

    /// An array whose element stride is `stride`, with no cells. Corelib has no 3-byte
    /// element type, so the element zero is a fieldless struct with an explicit `Size`:
    /// `CliValueType.SizeOfFieldStorage` gives that exactly the width we want, which
    /// `allocateArray` then agrees with rather than rejecting.
    ///
    /// The declared handle is `System.Byte`'s only so that the fixture needs no type of its
    /// own; nothing here reads it back, and the walk under test consults the stride alone.
    let private emptyArrayWithStride (stride : int) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        let elementHandle = handleFor baseClassTypes.Byte

        let elementZero =
            CliValueType.OfFields baseClassTypes concreteTypes elementHandle (Layout.Custom (stride, 1)) CharSet.Ansi []
            |> CliType.ValueType

        CliType.sizeOf elementZero |> shouldEqual stride

        let allocation : AllocatedArray =
            {
                Shape =
                    {
                        ConcreteType = ConcreteTypeHandle.OneDimArrayZero elementHandle
                        Length = 0
                        Lengths = ImmutableArray.Create 0
                        ElementStride = stride
                        ElementZero = elementZero
                    }
                Elements = ImmutableArray.Empty
            }

        ManagedHeap.allocateArray allocation heap

    [<Test>]
    let ``a byte cursor at the int32 floor normalises without overflowing`` () : unit =
        // `Int32.MinValue` bytes into an array whose stride does not divide it. Folding used
        // to recover the residual as `n - cellAdvance * cellSize`, whose product is
        // -2147483649 here: outside int32, and `ManagedPointerSource` is `Checked`, so it
        // raised `OverflowException` over an intermediate even though the residual it was
        // computing is 1.
        //
        // Reachable only now that an empty array supplies a real stride to normalisation;
        // before, a zero stride meant the fold was skipped entirely.
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        let state = state loggerFactory

        let arr, heap = emptyArrayWithStride 3 state.ManagedHeap

        let state =
            { state with
                ManagedHeap = heap
            }

        let src =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arr, 0),
                [ ByrefProjection.ReinterpretAs (concreteTypeFor baseClassTypes.Byte) ]
            )

        let result =
            ManagedPointerByteView.addByteOffsetToByteView state System.Int32.MinValue src

        // -2147483648 = 3 * -715827883 + 1, with the residual in [0, 3) as floor division
        // requires.
        match result with
        | ManagedPointerSource.Byref (ByrefRoot.ArrayElement (_, index),
                                      [ ByrefProjection.ReinterpretAs _ ; ByrefProjection.ByteOffset residual ]) ->
            index |> shouldEqual -715827883
            residual |> shouldEqual 1
        | other -> failwith $"expected a folded array-element byref with a 1-byte residual, got %O{other}"
