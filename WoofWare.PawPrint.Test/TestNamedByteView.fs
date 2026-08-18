namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A native int PawPrint models as an *identity* — a type handle, a method table pointer — has no
/// address, so it has never had a byte image either. `SignatureHelper.InternalAddRuntimeType` asks
/// for one anyway: with no module to spell a type as a metadata token it writes
/// `ELEMENT_TYPE_INTERNAL` and then copies the eight bytes of `type.TypeHandle.Value` into a
/// `Reflection.Emit` signature blob a byte at a time (SignatureHelper.cs:541-559), and hands the
/// blob straight back to the runtime.
///
/// So the bytes are only ever *moved*, never inspected — which is what makes naming them, rather
/// than inventing bits for them, an exact answer rather than an approximation. This file pins the
/// route those bytes travel: out of the handle through a byte cursor, across the evaluation stack
/// (which has no byte slot, so they widen to an int32 and back), and into a `byte[]` cell.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNamedByteView =

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

    let private handleOf (ty : TypeInfo<_, _>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes ty

    let private int32Handle : ConcreteTypeHandle = handleOf baseClassTypes.Int32
    let private byteHandle : ConcreteTypeHandle = handleOf baseClassTypes.Byte
    let private intPtrHandle : ConcreteTypeHandle = handleOf baseClassTypes.IntPtr

    let private byteConcreteType : ConcreteType<ConcreteTypeHandle> =
        AllConcreteTypes.lookup byteHandle concreteTypes |> Option.get

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// The handle whose bytes every test below follows.
    let private handleSource : NativeIntSource =
        NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed int32Handle)

    /// A second, distinct handle, so a test can tell "byte i of *this* handle" from "byte i of
    /// some handle".
    let private otherHandleSource : NativeIntSource =
        NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed byteHandle)

    let private byteTemplate : CliType =
        CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))

    /// The shape the guest's `IntPtr handle = type.TypeHandle.Value` local has: a single-field
    /// struct whose field is the handle.
    let private intPtrHolding (source : NativeIntSource) : CliValueType =
        [
            {
                Id = FieldId.named "_value"
                Name = "_value"
                Contents = CliType.Numeric (CliNumericType.NativeInt source)
                Offset = None
                Type = intPtrHandle
                MarshallingDescriptor = None
            }
        ]
        |> SynthesisedLayoutKind.ofFields baseClassTypes concreteTypes intPtrHandle Layout.Default CharSet.Ansi

    /// `(byte*)&handle` — a byte cursor `offset` bytes into the value at `addr`.
    let private byteCursor (addr : ManagedHeapAddress) (offset : int) : ManagedPointerSource =
        ManagedPointerSource.Byref (
            ByrefRoot.HeapValue addr,
            [
                ByrefProjection.ReinterpretAs byteConcreteType
                ByrefProjection.ByteOffset offset
            ]
        )

    [<Test>]
    let ``a byte cursor into a handle reads back that byte of that handle`` () : unit =
        let addr, st =
            IlMachineState.allocateManagedObject intPtrHandle (intPtrHolding handleSource) (state ())

        for offset in 0..7 do
            IlMachineState.readManagedByrefBytesAs baseClassTypes st (byteCursor addr offset) byteTemplate
            |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.NativeIntByte (handleSource, offset))))

    [<Test>]
    let ``the named byte identifies which handle it came from`` () : unit =
        // The discriminating check: an implementation that named the position but lost the source
        // would decode two different types to the same one, and `SignatureHelper` puts several
        // handles in one blob whenever a method has more than one non-primitive parameter.
        let addrA, st =
            IlMachineState.allocateManagedObject intPtrHandle (intPtrHolding handleSource) (state ())

        let addrB, st =
            IlMachineState.allocateManagedObject intPtrHandle (intPtrHolding otherHandleSource) st

        let readAt (addr : ManagedHeapAddress) (offset : int) : CliType =
            IlMachineState.readManagedByrefBytesAs baseClassTypes st (byteCursor addr offset) byteTemplate

        readAt addrA 3
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.NativeIntByte (handleSource, 3))))

        readAt addrB 3
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.NativeIntByte (otherHandleSource, 3))))

        readAt addrA 3 |> shouldNotEqual (readAt addrB 3)

    [<Test>]
    let ``reading the whole handle still yields the handle, not eight named bytes`` () : unit =
        // The control that keeps the byte route to sub-width accesses. A read whose width matches
        // the cell has always been served structurally, and must stay that way: coming back as
        // eight named bytes would lose the ability to use the value as a handle at all.
        let addr, st =
            IlMachineState.allocateManagedObject intPtrHandle (intPtrHolding handleSource) (state ())

        let wholeCell =
            ManagedPointerSource.Byref (ByrefRoot.HeapValue addr, [ ByrefProjection.Field (FieldId.named "_value") ])

        IlMachineState.readManagedByref baseClassTypes st wholeCell
        |> shouldEqual (CliType.Numeric (CliNumericType.NativeInt handleSource))

    [<Test>]
    let ``a named byte survives the evaluation stack round trip`` () : unit =
        // `ldind.u1` pushes an int32 and `stelem.i1` narrows it back, so a byte that has no number
        // has to cross a slot that only holds numbers. This is the pair that carries it.
        let stored =
            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.NativeIntByte (handleSource, 5)))

        let pushed = EvalStackValue.ofCliType stored

        pushed
        |> shouldEqual (EvalStackValue.Int32 (Int32Source.NativeIntByte (handleSource, 5)))

        EvalStackValue.toCliTypeCoerced byteTemplate pushed |> shouldEqual stored

    [<Test>]
    let ``a named byte on the stack has no number for anything that wants one`` () : unit =
        let pushed = EvalStackValue.Int32 (Int32Source.NativeIntByte (handleSource, 5))

        (fun () -> Int32Source.value "test" (Int32Source.NativeIntByte (handleSource, 5)) |> ignore)
        |> shouldFail<exn>

        // Storing it anywhere wider is refused: the widening would need the seven bytes either
        // side, which belong to the same handle but are not this value.
        (fun () ->
            EvalStackValue.toCliTypeCoerced (CliType.Numeric (CliNumericType.Int16 0s)) pushed
            |> ignore
        )
        |> shouldFail<exn>

        (fun () ->
            EvalStackValue.toCliTypeCoerced (CliType.Numeric (CliNumericType.Int32 0)) pushed
            |> ignore
        )
        |> shouldFail<exn>

    [<Test>]
    let ``a named byte stored in a byte array reads back through the array byte view`` () : unit =
        // `m_signature[m_currSig++] = phandle[i]` puts the byte here, and PawPrint's QCall
        // boundary reads the blob back out through exactly this route.
        let st = state ()

        let arrayAddr, st =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero byteHandle) (fun () -> byteTemplate) 4 st

        let named =
            CliType.Numeric (CliNumericType.UInt8 (UInt8Source.NativeIntByte (handleSource, 6)))

        let st = IlMachineState.setArrayValue arrayAddr named 2 st

        let cursor =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arrayAddr, 2),
                [
                    ByrefProjection.ReinterpretAs byteConcreteType
                    ByrefProjection.ByteOffset 0
                ]
            )

        IlMachineState.readManagedByrefBytesAs baseClassTypes st cursor byteTemplate
        |> shouldEqual named

        // The cells either side are untouched ordinary zero bytes, so the array is not wholly
        // poisoned by one named cell.
        let plainCursor (index : int) : ManagedPointerSource =
            ManagedPointerSource.Byref (
                ByrefRoot.ArrayElement (arrayAddr, index),
                [
                    ByrefProjection.ReinterpretAs byteConcreteType
                    ByrefProjection.ByteOffset 0
                ]
            )

        IlMachineState.readManagedByrefBytesAs baseClassTypes st (plainCursor 1) byteTemplate
        |> shouldEqual byteTemplate

        IlMachineState.readManagedByrefBytesAs baseClassTypes st (plainCursor 3) byteTemplate
        |> shouldEqual byteTemplate
