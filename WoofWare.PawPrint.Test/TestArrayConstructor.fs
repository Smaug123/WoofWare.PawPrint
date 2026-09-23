namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection.Emit
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `ArrayConstructor` and `ArrayConstruction` together are what a `newobj` of an array's
/// constructor does, whichever route reached it. The host runtime is the oracle, asked through a
/// `DynamicMethod` whose body is that very `newobj`: the JIT hands the arguments to
/// `AllocateArrayEx` untouched, so unlike `Array.CreateInstance` nothing managed screens them
/// first, and negative lengths and lower bounds reach the rules under test.
///
/// Every element type bottoms out in `byte`, and the generated arguments are chosen so that
/// anything the host accepts is small: each length is negative, at most 3, or above
/// `MaxArrayLength`, except that a multi-dimensional array may have any length in a dimension
/// provided another dimension is zero.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestArrayConstructor =

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    /// Parsed once for all tests; DumpedAssembly is immutable, so sharing it under
    /// ParallelScope.All is safe.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll (LoadedAssemblies.ofAssemblies [ corelib ]) baseClassTypes AllConcreteTypes.Empty

    let private byteHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle concreteTypes baseClassTypes.Byte

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    /// A type built from `byte` by making arrays of it.
    type ArrayTypeShape =
        | Byte
        | SzArray of ArrayTypeShape
        | MultiDim of ArrayTypeShape * rank : int

    let rec private hostType (shape : ArrayTypeShape) : Type =
        match shape with
        | ArrayTypeShape.Byte -> typeof<byte>
        | ArrayTypeShape.SzArray element -> (hostType element).MakeArrayType ()
        // `MakeArrayType 1` is `byte[*]`, the rank-1 multi-dimensional array.
        | ArrayTypeShape.MultiDim (element, rank) -> (hostType element).MakeArrayType rank

    let rec private handle (shape : ArrayTypeShape) : ConcreteTypeHandle =
        match shape with
        | ArrayTypeShape.Byte -> byteHandle
        | ArrayTypeShape.SzArray element -> ConcreteTypeHandle.OneDimArrayZero (handle element)
        | ArrayTypeShape.MultiDim (element, rank) -> ConcreteTypeHandle.Array (handle element, rank)

    let rec private hostTypeOfHandle (h : ConcreteTypeHandle) : Type =
        match h with
        | ConcreteTypeHandle.OneDimArrayZero element -> (hostTypeOfHandle element).MakeArrayType ()
        | ConcreteTypeHandle.Array (element, rank) -> (hostTypeOfHandle element).MakeArrayType rank
        | _ when h = byteHandle -> typeof<byte>
        | other -> failwith $"test built no type %O{other}"

    /// An array type, nested at most three deep, weighted towards szarrays of szarrays so that the
    /// jagged constructors are well represented.
    let private arrayTypeShape : Gen<ArrayTypeShape> =
        let rec element (depth : int) : Gen<ArrayTypeShape> =
            if depth = 0 then
                Gen.constant ArrayTypeShape.Byte
            else
                Gen.frequency
                    [
                        1, Gen.constant ArrayTypeShape.Byte
                        3, element (depth - 1) |> Gen.map ArrayTypeShape.SzArray
                        2,
                        gen {
                            let! inner = element (depth - 1)
                            let! rank = Gen.choose (1, 3)
                            return ArrayTypeShape.MultiDim (inner, rank)
                        }
                    ]

        gen {
            let! inner = element 2
            let! rank = Gen.choose (1, 3)
            return! Gen.elements [ ArrayTypeShape.SzArray inner ; ArrayTypeShape.MultiDim (inner, rank) ]
        }

    /// What an array constructor built, in terms both runtimes can answer: the array's type, its
    /// bounds, and -- for a single-dimensional array of arrays -- what its first and last elements
    /// are.
    type private Built =
        {
            Type : Type
            LowerBounds : int list
            Lengths : int list
            First : Built option
            Last : Built option
        }

    let rec private describeHost (array : Array) : Built =
        let ends =
            if array.Rank = 1 && array.GetType().GetElementType().IsArray && array.Length > 0 then
                let at (index : int) : Built option =
                    match array.GetValue index with
                    | :? Array as inner -> Some (describeHost inner)
                    | null -> None
                    | other -> failwith $"array of arrays held %O{other}"

                at (array.GetLowerBound 0), at (array.GetUpperBound 0)
            else
                None, None

        {
            Type = array.GetType ()
            LowerBounds = List.init array.Rank array.GetLowerBound
            Lengths = List.init array.Rank array.GetLength
            First = fst ends
            Last = snd ends
        }

    let rec private describePawPrint (heap : ManagedHeap) (address : ManagedHeapAddress) : Built =
        let shape =
            ManagedHeap.tryGetArrayShape address heap
            |> Option.defaultWith (fun () -> failwith $"%O{address} is not an array")

        let lengths = List.ofSeq shape.Lengths

        let ends =
            match shape.ConcreteType with
            | ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.OneDimArrayZero _)
            | ConcreteTypeHandle.OneDimArrayZero (ConcreteTypeHandle.Array _) when shape.Length > 0 ->
                let at (index : int) : Built option =
                    match ManagedHeap.getArrayValue address index heap with
                    | CliType.ObjectRef (Some inner) -> Some (describePawPrint heap inner)
                    | CliType.ObjectRef None -> None
                    | other -> failwith $"array of arrays held %O{other}"

                at 0, at (shape.Length - 1)
            | _ -> None, None

        {
            Type = hostTypeOfHandle shape.ConcreteType
            LowerBounds = lengths |> List.map (fun _ -> 0)
            Lengths = lengths
            First = fst ends
            Last = snd ends
        }

    /// The host's `newobj` of `arrayType`'s constructor taking `arguments.Length` arguments.
    let private hostNewobj (arrayType : Type) (arguments : int[]) : Result<Built, string * string> =
        let ctor =
            arrayType.GetConstructor (Array.replicate arguments.Length typeof<int>)
            |> Option.ofObj
            |> Option.defaultWith (fun () ->
                failwith $"%O{arrayType} has no constructor of %d{arguments.Length} arguments"
            )

        let method = DynamicMethod ("NewArray", typeof<obj>, [| typeof<int[]> |])
        let il = method.GetILGenerator ()

        for i in 0 .. arguments.Length - 1 do
            il.Emit OpCodes.Ldarg_0
            il.Emit (OpCodes.Ldc_I4, i)
            il.Emit OpCodes.Ldelem_I4

        il.Emit (OpCodes.Newobj, ctor)
        il.Emit OpCodes.Ret

        let construct = method.CreateDelegate typeof<Func<int[], obj>> :?> Func<int[], obj>

        try
            Ok (describeHost (construct.Invoke arguments :?> Array))
        with e ->
            Error (e.GetType().FullName, e.Message)

    /// PawPrint's answer to the same `newobj`, in the same shape: what `ArrayConstruction` put on
    /// the heap, or the exception `ArrayConstructor.exceptionFor` names with the message the host
    /// would give it.
    let private pawPrintNewobj (arrayType : ConcreteTypeHandle) (arguments : int[]) : Result<Built, string * string> =
        let ctor =
            ArrayConstructor.withParameterCount arrayType arguments.Length
            |> Option.defaultWith (fun () ->
                failwith $"%O{arrayType} declares no constructor of %d{arguments.Length} arguments"
            )

        match ArrayConstructor.plan ctor (ImmutableArray.CreateRange arguments) with
        | Error error ->
            let exceptionType, message = ArrayConstructor.exceptionFor baseClassTypes error
            let fullName = $"%s{exceptionType.Namespace}.%s{exceptionType.Name}"

            let message =
                message
                |> Option.defaultWith (fun () -> (Activator.CreateInstance (Type.GetType fullName) :?> exn).Message)

            Error (fullName, message)
        | Ok (ArrayAllocation.MultiDim (element, dimensions)) when
            dimensions |> Seq.exists (fun dimension -> dimension.LowerBound <> 0)
            ->
            // The heap cannot hold a non-zero lower bound, so this compares the plan alone.
            Ok
                {
                    Type = hostTypeOfHandle (ConcreteTypeHandle.Array (element, dimensions.Length))
                    LowerBounds = dimensions |> Seq.map (fun dimension -> dimension.LowerBound) |> List.ofSeq
                    Lengths = dimensions |> Seq.map (fun dimension -> dimension.Length) |> List.ofSeq
                    First = None
                    Last = None
                }
        | Ok allocation ->
            let address, state = ArrayConstruction.allocate baseClassTypes allocation (state ())
            Ok (describePawPrint state.ManagedHeap address)

    /// A length whose acceptance allocates at most three elements.
    let private smallOrRefusedLength : Gen<int> =
        Gen.frequency
            [
                4, Gen.choose (0, 3)
                2, Gen.elements [ -1 ; -2 ; Int32.MinValue ]
                2, Gen.elements [ SzArrayAllocation.maxLength + 1 ; Int32.MaxValue ]
            ]

    /// The boundaries the multi-dimensional rules name, for a dimension that another zero
    /// dimension keeps from being allocated.
    let private anyLength : Gen<int> =
        Gen.frequency
            [
                3, smallOrRefusedLength
                3,
                Gen.elements
                    [
                        SzArrayAllocation.maxLength - 1
                        SzArrayAllocation.maxLength
                        65535
                        65536
                        65537
                        46341
                    ]
                1, Gen.choose (0, Int32.MaxValue)
            ]

    let private lowerBound : Gen<int> =
        Gen.frequency
            [
                4, Gen.elements [ 0 ; 1 ; -1 ; 5 ]
                3, Gen.elements [ Int32.MaxValue ; Int32.MaxValue - 1 ; Int32.MaxValue - 2 ; Int32.MinValue ]
                1, Gen.choose (Int32.MinValue, Int32.MaxValue)
            ]

    /// Arguments for `ctor`, of the shape its parameters take.
    let private argumentsFor (ctor : ArrayConstructor) : Gen<int[]> =
        match ctor with
        | ArrayConstructor.SzArray (_, depth) -> Gen.arrayOfLength depth smallOrRefusedLength
        | ArrayConstructor.MultiDim (_, rank, lowerBounds) ->
            gen {
                let! lengths =
                    Gen.oneof
                        [
                            Gen.arrayOfLength rank smallOrRefusedLength
                            // Every dimension non-empty, which the other two rarely produce.
                            Gen.arrayOfLength rank (Gen.choose (1, 3))
                            gen {
                                let! lengths = Gen.arrayOfLength rank anyLength
                                let! zeroAt = Gen.choose (0, rank - 1)
                                return lengths |> Array.mapi (fun i length -> if i = zeroAt then 0 else length)
                            }
                        ]

                if lowerBounds then
                    let! bounds = Gen.arrayOfLength rank lowerBound
                    return Array.init (2 * rank) (fun i -> if i % 2 = 0 then bounds.[i / 2] else lengths.[i / 2])
                else
                    return lengths
            }

    let private constructorCall : Gen<ArrayTypeShape * int[]> =
        gen {
            let! shape = arrayTypeShape
            let! ctor = Gen.elements (ArrayConstructor.declaredOn (handle shape))
            let! arguments = argumentsFor ctor
            return shape, arguments
        }

    [<Test>]
    let ``an array type declares the constructors the host's does`` () : unit =
        let property (shape : ArrayTypeShape) : bool =
            let hostCounts =
                (hostType shape).GetConstructors ()
                |> Array.map (fun ctor ->
                    let parameters = ctor.GetParameters ()

                    if
                        parameters
                        |> Array.exists (fun parameter -> parameter.ParameterType <> typeof<int>)
                    then
                        failwith $"%O{ctor} takes something other than int32"

                    parameters.Length
                )
                |> Array.sort
                |> List.ofArray

            let declaredCounts =
                ArrayConstructor.declaredOn (handle shape)
                |> List.map ArrayConstructor.parameterCount
                |> List.sort

            hostCounts = declaredCounts

        Check.One (config, Prop.forAll (Arb.fromGen arrayTypeShape) property)

    [<Test>]
    let ``constructing an array agrees with the host's newobj`` () : unit =
        let property (shape : ArrayTypeShape, arguments : int[]) : bool =
            hostNewobj (hostType shape) arguments = pawPrintNewobj (handle shape) arguments

        Check.One (config, Prop.forAll (Arb.fromGen constructorCall) property)

    /// The property above is only as good as the outcomes its generator reaches, and most of the
    /// rules under test are reached by a narrow band of arguments. This asks the host what a large
    /// sample of the generator's calls do, and requires every outcome to turn up. Measured over
    /// 20000 draws, the rarest outcome (a jagged constructor's nested arrays) is 2.4% of them, so
    /// a sample of 4000 misses one with probability below 1e-40.
    [<Test>]
    let ``the generator reaches every outcome`` () : unit =
        let outcomes =
            Gen.sample 4000 constructorCall
            |> Array.map (fun (shape, arguments) ->
                match hostNewobj (hostType shape) arguments with
                | Error (exceptionType, _) -> exceptionType
                | Ok built when built.First.IsSome -> "jagged"
                | Ok built when built.LowerBounds |> List.exists (fun bound -> bound <> 0) -> "lower bound"
                | Ok built when
                    built.Type.IsSZArray
                    && (
                        match shape with
                        | ArrayTypeShape.MultiDim _ -> true
                        | _ -> false
                    )
                    ->
                    "rank-1 multi-dimensional as szarray"
                | Ok built when built.Lengths.Length > 1 && List.forall (fun length -> length > 0) built.Lengths ->
                    "non-empty multi-dimensional"
                | Ok _ -> "other"
            )
            |> Set.ofArray

        let expected =
            set
                [
                    "System.OverflowException"
                    "System.OutOfMemoryException"
                    "System.ArgumentOutOfRangeException"
                    "jagged"
                    "lower bound"
                    "rank-1 multi-dimensional as szarray"
                    "non-empty multi-dimensional"
                ]

        Set.difference expected outcomes |> Set.toList |> shouldEqual []
