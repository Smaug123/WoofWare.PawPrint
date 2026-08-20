namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// A primitive load strictly narrower than the struct its byref names reads the struct's
/// leading bytes, whatever the struct's field decomposition. `executeLdind` routes such loads
/// through `readManagedByrefBytesAs` (the same byte walk nonzero displacements take);
/// `sourcesPure/StructLeadingByteView.cs` covers the routing end to end, and this file pins the
/// hinge that routing leans on: the byte walk serves a *plain* byref — no projections, so the
/// `resolveCell` trailing-view branch is not in play — pointed at a wider struct cell.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStructLeadingByteView =

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

    let private int64Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Int64

    let private uint16Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.UInt16

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// A struct whose single field is an 8-byte integer: the shape
    /// `IndexOfAnyAsciiSearcher`'s bitmaps decompose into.
    let private oneLongValueType (state : IlMachineState) (value : int64) : CliValueType =
        [
            {
                Id = FieldId.named "_00"
                Name = "_00"
                Contents = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value))
                Offset = None
                Type = int64Handle
                MarshallingDescriptor = None
            }
        ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int64Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    /// `{ ushort A; ushort B; uint C }`: a four-byte window at offset 0 spans A and B.
    let private twoShortsValueType (state : IlMachineState) (a : uint16) (b : uint16) : CliValueType =
        let shortField (name : string) (value : uint16) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = CliType.Numeric (CliNumericType.UInt16 value)
                Offset = None
                Type = uint16Handle
                MarshallingDescriptor = None
            }

        [
            shortField "A" a
            shortField "B" b
            {
                Id = FieldId.named "C"
                Name = "C"
                Contents = CliType.Numeric (CliNumericType.Int32 0)
                Offset = None
                Type = int32Handle
                MarshallingDescriptor = None
            }
        ]
        |> SynthesisedLayoutKind.ofFields
            baseClassTypes
            state.ConcreteTypes
            int32Handle
            Layout.Default
            System.Runtime.InteropServices.CharSet.Ansi

    /// A plain byref — empty projection chain — at an array element holding `element`.
    let private plainByrefAt (element : CliType) : IlMachineState * ManagedPointerSource =
        let state = state ()

        let arr, state =
            IlMachineState.allocateArray (ConcreteTypeHandle.OneDimArrayZero int32Handle) (fun () -> element) 1 state

        state, ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arr, 0), [])

    [<Test>]
    let ``a one-byte template over an eight-byte-field struct reads the leading byte`` () : unit =
        let machine = state ()

        let state, ptr =
            plainByrefAt (CliType.ValueType (oneLongValueType machine 0x0807060504030201L))

        IlMachineManagedByref.readManagedByrefBytesAs
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
        |> shouldEqual (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 1uy)))

    [<Test>]
    let ``a four-byte template over an eight-byte-field struct reads the leading four bytes`` () : unit =
        let machine = state ()

        let state, ptr =
            plainByrefAt (CliType.ValueType (oneLongValueType machine 0x0807060504030201L))

        IlMachineManagedByref.readManagedByrefBytesAs
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x04030201))

    [<Test>]
    let ``a four-byte template spanning two ushort fields splices them`` () : unit =
        let machine = state ()

        let state, ptr =
            plainByrefAt (CliType.ValueType (twoShortsValueType machine 0x3412us 0x7856us))

        IlMachineManagedByref.readManagedByrefBytesAs
            baseClassTypes
            state
            ptr
            (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (CliType.Numeric (CliNumericType.Int32 0x78563412))
