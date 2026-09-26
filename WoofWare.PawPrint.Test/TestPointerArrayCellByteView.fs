namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Reading a cell of a pointer-element array (`int*[]`) through a byte cursor over the array's
/// data, which is what `MemoryMarshal.GetArrayDataReference(Array)` hands out.
///
/// A pointer cell has no byte image, so the only reads such a cursor can serve are the ones that
/// cover exactly one cell at the width of a native int: those hand back the pointer the cell
/// holds, provenance and all. `sourcesPure/PointerArrayCellThroughByteView.cs` checks the guest
/// sees the right pointee. This file checks the two things a guest cannot: that the value read is
/// *the same value* a plain `ldind.i` of the cell would push, and that the reads which would
/// need the cell's bytes are still refused rather than answered with invented ones. Real .NET
/// answers those, so a differential guest could only ever be parked.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPointerArrayCellByteView =

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

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int32

    let private intPtrHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.IntPtr

    let private byteType : ConcreteType<ConcreteTypeHandle> =
        ConcreteType.makeFromIdentity
            baseClassTypes.Byte.Identity
            baseClassTypes.Byte.Namespace
            baseClassTypes.Byte.Name
            ImmutableArray<ConcreteTypeHandle>.Empty

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private pointerStride : int = 8

    let private nativeIntTemplate : CliType =
        CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))

    /// `System.IntPtr` as `Unsafe.ReadUnaligned<IntPtr>` asks for it: the wrapper, not the bare
    /// native int it flattens to.
    let private intPtrTemplate : CliType =
        let zero, _, _ =
            CliType.zeroOf IAssemblyLoad.alreadyLoadedOnly concreteTypes loadedAssemblies baseClassTypes intPtrHandle

        zero

    let private pointerGen : Gen<CliRuntimePointer> =
        let local =
            gen {
                let! frame = Gen.choose (0, 20)
                let! slot = Gen.choose (0, 5)

                return
                    ManagedPointerSource.Byref
                        {
                            Root =
                                ByrefRoot.LocalVariable (ThreadId.ThreadId 0, FrameId.FrameId frame, uint16<int> slot)
                            Projections = []
                        }
                    |> CliRuntimePointer.Managed
            }

        Gen.oneof
            [
                ArbMap.defaults |> ArbMap.generate<int64> |> Gen.map CliRuntimePointer.Verbatim
                ArbMap.defaults
                |> ArbMap.generate<int64>
                |> Gen.map CliRuntimePointer.FieldRegistryHandle
                Gen.constant (CliRuntimePointer.Managed ManagedPointerSource.Null)
                local
                Gen.constant (CliRuntimePointer.TypeHandlePtr (RuntimeTypeHandleTarget.Closed int32Handle))
            ]

    /// An `int*[]` holding `cells`.
    let private pointerArray (cells : CliRuntimePointer list) : IlMachineState * ManagedHeapAddress =
        let state = state ()

        let arr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Pointer int32Handle))
                (fun () -> CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null))
                cells.Length
                state

        let state =
            cells
            |> List.indexed
            |> List.fold
                (fun state (i, cell) -> IlMachineThreadState.setArrayValue arr (CliType.RuntimePointer cell) i state)
                state

        state, arr

    /// A byte cursor rooted at element `root`, displaced `byteOffset` bytes: the shape
    /// `GetArrayDataReference(Array)` followed by `Unsafe.Add(ref byte, n)` produces.
    let private byteCursor
        (state : IlMachineState)
        (arr : ManagedHeapAddress)
        (root : int)
        (byteOffset : int)
        : ManagedPointerSource
        =
        ManagedPointerSource.Byref
            {
                Root = ByrefRoot.ArrayElement (arr, root)
                Projections = []
            }
        |> ManagedPointerByteView.addByteOffset state byteType byteOffset

    type private Case =
        {
            Cells : CliRuntimePointer list
            Root : int
            Target : int
        }

    let private caseGen : Gen<Case> =
        gen {
            let! length = Gen.choose (1, 4)
            let! cells = Gen.listOfLength length pointerGen
            let! root = Gen.choose (0, length - 1)
            let! target = Gen.choose (0, length - 1)

            return
                {
                    Cells = cells
                    Root = root
                    Target = target
                }
        }

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    let private messageOf (f : unit -> 'a) : string =
        let outcome =
            try
                f () |> ignore
                None
            with e ->
                Some e.Message

        match outcome with
        | None -> failwith "expected the access to be refused, but it succeeded"
        | Some message -> message

    /// The read agrees with a plain `ldind.i` of the target cell, which pushes
    /// `ofCliType cell` — so the pointer's provenance survives — and it comes back in the
    /// *template's* shape, so a caller asking for an `IntPtr` gets an `IntPtr` rather than the
    /// cell's `RuntimePointer`.
    [<TestCase true>]
    [<TestCase false>]
    let ``a cell-aligned native-int read through a byte cursor hands over the pointer`` (wrapped : bool) : unit =
        let template = if wrapped then intPtrTemplate else nativeIntTemplate

        let property (case : Case) : unit =
            let state, arr = pointerArray case.Cells

            let cursor =
                byteCursor state arr case.Root ((case.Target - case.Root) * pointerStride)

            let expected =
                CliType.RuntimePointer case.Cells.[case.Target]
                |> EvalStackValue.ofCliType
                |> EvalStackValue.toCliTypeCoerced template

            IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state cursor template
            |> shouldEqual expected

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)

    /// A native-int-wide read that starts partway into a pointer cell would need that cell's
    /// bytes, and there are none to give.
    [<Test>]
    let ``a misaligned native-int read of a pointer cell is refused`` () : unit =
        let gen =
            gen {
                let! case = caseGen
                let! residue = Gen.choose (1, pointerStride - 1)
                return case, residue
            }

        let property (case : Case, residue : int) : unit =
            let state, arr = pointerArray case.Cells

            let cursor =
                byteCursor state arr case.Root ((case.Target - case.Root) * pointerStride + residue)

            messageOf (fun () ->
                IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state cursor nativeIntTemplate
            )
            |> shouldContainText "refusing byte view over runtime pointer"

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// A read narrower than a cell, even one starting on the cell boundary, is a read of some of
    /// the pointer's bytes.
    [<Test>]
    let ``a cell-aligned read narrower than a pointer is refused`` () : unit =
        let gen =
            gen {
                let! case = caseGen

                let! template =
                    Gen.elements
                        [
                            CliType.Numeric (CliNumericType.Int32 0)
                            CliType.Numeric (CliNumericType.UInt16 0us)
                            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))
                        ]

                return case, template
            }

        let property (case : Case, template : CliType) : unit =
            let state, arr = pointerArray case.Cells

            let cursor =
                byteCursor state arr case.Root ((case.Target - case.Root) * pointerStride)

            messageOf (fun () -> IlMachineManagedByref.readManagedByrefBytesAs baseClassTypes state cursor template)
            |> shouldContainText "refusing byte view over runtime pointer"

        Check.One (config, Prop.forAll (Arb.fromGen gen) property)

    /// A pointer-wide read of a pointer cell as a *number* of the same width is a read of the
    /// pointer's bytes too, however the eval stack would later widen it.
    [<Test>]
    let ``a cell-aligned int64 read of a pointer cell is refused`` () : unit =
        let property (case : Case) : unit =
            let state, arr = pointerArray case.Cells

            let cursor =
                byteCursor state arr case.Root ((case.Target - case.Root) * pointerStride)

            messageOf (fun () ->
                IlMachineManagedByref.readManagedByrefBytesAs
                    baseClassTypes
                    state
                    cursor
                    (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L)))
            )
            |> shouldContainText "refusing byte view over runtime pointer"

        Check.One (config, Prop.forAll (Arb.fromGen caseGen) property)
