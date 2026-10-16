namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `Interlocked.CompareExchange(int*, int, int)`, .NET 11's compare-exchange on an object's header,
/// performed by `Intrinsics.performPrimitive` directly. A guest cannot reach the overload yet: it is
/// internal, so a guest finds it by reflection, which on .NET 11 stops at the thin lock.
///
/// The pointer's provenance never decides the access's width: an `int*` taken from a byte buffer
/// addresses four bytes, as `ldind.i4` and `stind.i4` through the same pointer do. And the pointer
/// reaches the self-call in either of the forms PawPrint gives an unmanaged pointer: a native int
/// carrying a byref, as `&local` makes one, or the managed pointer that the method's `ldarg.0`
/// reloads from its argument slot.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestPointerCompareExchange =

    let private primitive : IntrinsicPrimitive =
        IntrinsicPrimitive.AtomicCompareExchange (AtomicOperand.Int32, AtomicLocation.Pointer)

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        BaseClassTypes.ofCorelib ThreadFixtures.corelib

    /// How the pointer argument reaches the self-call.
    type private PointerForm =
        /// A native int carrying a byref: the caller's `&local` or `fixed`.
        | NativeInt
        /// A managed pointer: what `ldarg.0` reloads from the overload's argument slot.
        | Reloaded

    /// What the pointer addresses.
    type private Storage =
        /// The first four elements of a `byte[]`, little-endian.
        | Bytes
        /// An element of an `int[]`.
        | Int32Cell

    /// A machine with its entry thread, and the method `performPrimitive` is told it performs:
    /// the host CoreLib's `ref int` overload, which has the pointer overload's arity, since
    /// .NET 10 has no pointer overload.
    let private machine () =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let state, frame = ThreadFixtures.aFrame (ThreadFixtures.bare ())
        let state, thread = IlMachineState.addThread frame state

        let raw =
            baseClassTypes.Corelib.TryGetTopLevelTypeDef "System.Threading" "Interlocked"
            |> Option.get
            |> fun ty -> ty.Methods
            |> List.find (fun m ->
                m.Name = "CompareExchange"
                && m.Signature.ParameterTypes = [
                    TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.Int32)
                    TypeDefn.PrimitiveType PrimitiveType.Int32
                    TypeDefn.PrimitiveType PrimitiveType.Int32
                ]
            )

        let state, methodToCall, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                raw
                ImmutableArray.Empty
                state

        loggerFactory, state, thread, methodToCall

    let private byteValue (b : byte) : CliType =
        CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim b))

    let private concretise loggerFactory (ty : TypeInfo<GenericParamFromMetadata, TypeDefn>) (state : IlMachineState) =
        LoadedTypeInfo.typeInfoToTypeDefn' baseClassTypes state.TypeSystem._LoadedAssemblies ty
        |> IlMachineState.concretizeType
            loggerFactory
            baseClassTypes
            state
            baseClassTypes.Corelib.DefinitionFullName
            ImmutableArray.Empty
            ImmutableArray.Empty

    /// Store `initial` in `storage`: the array whose element 0 a pointer to it addresses.
    let private place loggerFactory (storage : Storage) (initial : int32) (state : IlMachineState) =
        match storage with
        | Storage.Bytes ->
            let state, byteType = concretise loggerFactory baseClassTypes.Byte state

            let array, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero byteType)
                    (fun () -> byteValue 0uy)
                    4
                    state

            let state =
                (state, BitConverter.GetBytes initial |> Array.indexed)
                ||> Array.fold (fun state (i, b) -> IlMachineState.setArrayValue array (byteValue b) i state)

            array, state
        | Storage.Int32Cell ->
            let state, intType = concretise loggerFactory baseClassTypes.Int32 state

            let array, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero intType)
                    (fun () -> CliType.Numeric (CliNumericType.Int32 0))
                    1
                    state

            array, IlMachineState.setArrayValue array (CliType.Numeric (CliNumericType.Int32 initial)) 0 state

    /// The `int32` now in `storage` at `array`.
    let private read (storage : Storage) (array : ManagedHeapAddress) (state : IlMachineState) : int32 =
        match storage with
        | Storage.Bytes ->
            [| 0..3 |]
            |> Array.map (fun i ->
                match IlMachineState.getArrayValue array i state with
                | CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim b)) -> b
                | other -> failwith $"byte %d{i} of the buffer is %O{other}"
            )
            |> fun bytes -> BitConverter.ToInt32 (bytes, 0)
        | Storage.Int32Cell ->
            match IlMachineState.getArrayValue array 0 state with
            | CliType.Numeric (CliNumericType.Int32 i) -> i
            | other -> failwith $"the int cell is %O{other}"

    let private pointer (form : PointerForm) (location : AddressedByref) : EvalStackValue =
        match form with
        | PointerForm.NativeInt -> EvalStackValue.NativeInt (NativeIntSource.ManagedPointer location)
        | PointerForm.Reloaded -> EvalStackValue.ManagedPointer (ManagedPointerSource.Byref location)

    [<Test>]
    let ``a pointer compare-exchange returns the old int32 and stores only on a match, in four bytes`` () : unit =
        let gen =
            gen {
                let! initial = ArbMap.defaults |> ArbMap.generate<int32>
                let! value = ArbMap.defaults |> ArbMap.generate<int32>

                let! comparand = Gen.oneof [ Gen.constant initial ; ArbMap.defaults |> ArbMap.generate<int32> ]

                let! form = Gen.elements [ PointerForm.NativeInt ; PointerForm.Reloaded ]
                let! storage = Gen.elements [ Storage.Bytes ; Storage.Int32Cell ]
                return initial, value, comparand, form, storage
            }

        let property (initial, value, comparand, form, storage) =
            let loggerFactory, state, thread, methodToCall = machine ()
            let array, state = place loggerFactory storage initial state

            let location : AddressedByref =
                {
                    Root = ByrefRoot.ArrayElement (array, 0)
                    Projections = []
                }

            let state =
                state
                |> IlMachineState.pushToEvalStack' (pointer form location) thread
                |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 value)) thread
                |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 comparand)) thread

            match
                Intrinsics.performPrimitive loggerFactory baseClassTypes primitive methodToCall thread false state
            with
            | IntrinsicResult.Completed after ->
                let returned, after = IlMachineState.popEvalStack thread after
                returned |> shouldEqual (EvalStackValue.Int32 (Int32Source.Verbatim initial))

                read storage array after
                |> shouldEqual (if initial = comparand then value else initial)
            | other -> failwith $"%A{form} pointer into %A{storage}: expected completion, got %A{other}"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 200, Prop.forAll (Arb.fromGen gen) property)

    [<TestCase(true)>]
    [<TestCase(false)>]
    let ``a null pointer raises NullReferenceException`` (asNativeInt : bool) : unit =
        let loggerFactory, state, thread, methodToCall = machine ()

        let nullPointer =
            if asNativeInt then
                EvalStackValue.NativeInt (NativeIntSource.Verbatim 0L)
            else
                EvalStackValue.ManagedPointer ManagedPointerSource.Null

        let state =
            state
            |> IlMachineState.pushToEvalStack' nullPointer thread
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) thread
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) thread

        match Intrinsics.performPrimitive loggerFactory baseClassTypes primitive methodToCall thread false state with
        | IntrinsicResult.RaiseException (_, exceptionType, _) ->
            exceptionType.Identity
            |> shouldEqual baseClassTypes.NullReferenceException.Identity
        | other -> failwith $"expected NullReferenceException, got %A{other}"
