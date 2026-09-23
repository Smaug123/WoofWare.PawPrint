namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// One of the constructors CoreCLR synthesises on an array type (`ArrayClass`, vm/array.cpp:248
/// and :480-508), which are the only constructors an array has. A `newobj` names one through a
/// `MemberReference` whose parent is the array's `TypeSpec`; an `[UnsafeAccessor(Constructor)]`
/// returning the array binds one by signature. Every parameter of every one of them is an
/// `int32`, so on its array type each is identified by how many it takes.
[<RequireQualifiedAccess>]
type ArrayConstructor =
    /// `element[]::.ctor(int32 × depth)`. The first argument is the array's length; each further
    /// argument is the length of one more level of szarrays nested inside it, every one of which
    /// is allocated. So `element` is itself a szarray nested at least `depth - 1` deep: `int[][]`
    /// has `.ctor(int32)` and `.ctor(int32, int32)`, while `int[]` and `int[,][]` have only the
    /// first.
    | SzArray of element : ConcreteTypeHandle * depth : int
    /// `element[,…]::.ctor(int32 × rank)`, one length per dimension with every lower bound zero;
    /// or, when `lowerBounds`, `.ctor(int32 × 2·rank)`, a lower bound and then a length for each
    /// dimension in turn.
    | MultiDim of element : ConcreteTypeHandle * rank : int * lowerBounds : bool

/// What an array constructor allocates, once CoreCLR has accepted its arguments.
[<RequireQualifiedAccess>]
type ArrayAllocation =
    /// A szarray of `element` with `length` elements. Each element is a fresh allocation of
    /// `eachElement`, or `element`'s zero when that is `None`. An `eachElement` allocates an array
    /// whose type is `element`.
    | SzArray of element : ConcreteTypeHandle * length : int * eachElement : ArrayAllocation option
    /// A multi-dimensional array of `element`, of rank `dimensions.Length`.
    | MultiDim of element : ConcreteTypeHandle * dimensions : ImmutableArray<ArrayDimension>

/// Why CoreCLR refuses an array constructor's arguments. The single-dimensional and
/// multi-dimensional walks in `AllocateArrayEx` (`vm/gchelpers.cpp:804`) apply different rules,
/// so each keeps its own vocabulary.
[<RequireQualifiedAccess>]
type ArrayConstructorError =
    /// The length of a szarray: the array itself, or a nested level a jagged constructor
    /// allocates.
    | SzArrayLength of SzArrayLengthError
    /// The dimensions of a multi-dimensional array, which for rank 1 means one whose lower bound
    /// is not zero.
    | MultiDimLength of MultiDimArrayLengthError

[<RequireQualifiedAccess>]
module ArrayConstructor =
    /// How many consecutive levels of szarray `ty` is: 0 for `int`, 2 for `int[][]`, and 1 for
    /// `int[,][]`, whose element is not a szarray.
    let rec private szArrayNesting (ty : ConcreteTypeHandle) : int =
        match ty with
        | ConcreteTypeHandle.OneDimArrayZero element -> 1 + szArrayNesting element
        | _ -> 0

    /// Every constructor CoreCLR declares on `arrayType`, in declaration order; empty if it is not
    /// an array. `ArrayClass` declares one per level of szarray nesting on a szarray, and exactly
    /// two on any other array, rank 1 included.
    let declaredOn (arrayType : ConcreteTypeHandle) : ArrayConstructor list =
        match arrayType with
        | ConcreteTypeHandle.OneDimArrayZero element ->
            [ 1 .. szArrayNesting element + 1 ]
            |> List.map (fun depth -> ArrayConstructor.SzArray (element, depth))
        | ConcreteTypeHandle.Array (element, rank) ->
            [
                ArrayConstructor.MultiDim (element, rank, false)
                ArrayConstructor.MultiDim (element, rank, true)
            ]
        | ConcreteTypeHandle.Concrete _
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ -> []

    /// The array type the constructor is declared on.
    let arrayType (ctor : ArrayConstructor) : ConcreteTypeHandle =
        match ctor with
        | ArrayConstructor.SzArray (element, _) -> ConcreteTypeHandle.OneDimArrayZero element
        | ArrayConstructor.MultiDim (element, rank, _) -> ConcreteTypeHandle.Array (element, rank)

    let parameterCount (ctor : ArrayConstructor) : int =
        match ctor with
        | ArrayConstructor.SzArray (_, depth) -> depth
        | ArrayConstructor.MultiDim (_, rank, false) -> rank
        | ArrayConstructor.MultiDim (_, rank, true) -> 2 * rank

    /// The constructor's signature as `ArrayClass::GenerateArrayAccessorCallSig` (vm/array.cpp:69)
    /// writes it: an instance method returning void, taking `int32`s only.
    let signature (ctor : ArrayConstructor) : TypeMethodSignature<TypeDefn> =
        let count = parameterCount ctor

        {
            Header =
                SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.Instance)
                |> ComparableSignatureHeader.Make
            ParameterTypes = List.replicate count (TypeDefn.PrimitiveType PrimitiveType.Int32)
            GenericParameterCount = 0
            RequiredParameterCount = count
            ReturnType = MethodReturnType.Void
        }

    /// The constructor of `arrayType` that takes `count` arguments, if it declares one.
    let withParameterCount (arrayType : ConcreteTypeHandle) (count : int) : ArrayConstructor option =
        declaredOn arrayType |> List.tryFind (fun ctor -> parameterCount ctor = count)

    /// What `AllocateArrayEx` (`vm/gchelpers.cpp:804`) allocates for these constructor arguments,
    /// or the first rule it refuses them by, checked in the order it checks them.
    ///
    /// A jagged constructor validates a nested level only while allocating that level's first
    /// element, so a zero length at one level means the levels below it are never looked at:
    /// `new int[0][-1]` spelled as `int[][]::.ctor(0, -1)` is an empty array. A rank-1
    /// multi-dimensional array whose lower bound is zero is allocated as a szarray, which is the
    /// type the caller gets back.
    ///
    /// `arguments` must have as many entries as the constructor has parameters.
    let plan
        (ctor : ArrayConstructor)
        (arguments : ImmutableArray<int>)
        : Result<ArrayAllocation, ArrayConstructorError>
        =
        if arguments.Length <> parameterCount ctor then
            failwith
                $"BUG: array constructor %O{ctor} takes %d{parameterCount ctor} arguments, but was handed %d{arguments.Length}"

        let szArray (element : ConcreteTypeHandle) (length : int) : Result<ArrayAllocation, ArrayConstructorError> =
            match SzArrayAllocation.checkLength length with
            | Some error -> Error (ArrayConstructorError.SzArrayLength error)
            | None -> Ok (ArrayAllocation.SzArray (element, length, None))

        match ctor with
        | ArrayConstructor.SzArray (element, _) ->
            let rec level
                (element : ConcreteTypeHandle)
                (lengths : int list)
                : Result<ArrayAllocation, ArrayConstructorError>
                =
                match lengths with
                | [] -> failwith "BUG: a jagged array constructor ran out of lengths"
                | [ length ] -> szArray element length
                | length :: nested ->
                    match szArray element length with
                    | Error error -> Error error
                    | Ok allocation when length = 0 -> Ok allocation
                    | Ok _ ->

                    match element with
                    | ConcreteTypeHandle.OneDimArrayZero inner ->
                        level inner nested
                        |> Result.map (fun each -> ArrayAllocation.SzArray (element, length, Some each))
                    | other ->
                        failwith
                            $"BUG: a jagged array constructor has a level left to allocate, but its element type %O{other} is not a szarray"

            level element (List.ofSeq arguments)
        | ArrayConstructor.MultiDim (element, 1, lowerBounds) when not lowerBounds || arguments.[0] = 0 ->
            szArray element arguments.[arguments.Length - 1]
        | ArrayConstructor.MultiDim (element, rank, lowerBounds) ->
            let dimensions =
                Seq.init
                    rank
                    (fun i ->
                        if lowerBounds then
                            {
                                LowerBound = arguments.[2 * i]
                                Length = arguments.[2 * i + 1]
                            }
                        else
                            {
                                LowerBound = 0
                                Length = arguments.[i]
                            }
                    )
                |> ImmutableArray.CreateRange

            match MultiDimArrayAllocation.totalElementsOfDimensions dimensions with
            | Error error -> Error (ArrayConstructorError.MultiDimLength error)
            | Ok _ -> Ok (ArrayAllocation.MultiDim (element, dimensions))

    /// The exception CoreCLR raises for this refusal, and the message it carries (`None` meaning
    /// the parameterless constructor's own default).
    ///
    /// Named through `BaseClassTypes` rather than as an `OpcodeFault`: the constructor is a callee,
    /// so what it raises reaches the `newobj` by the call edge rather than being the instruction's
    /// own fault.
    let exceptionFor
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (error : ArrayConstructorError)
        : TypeInfo<GenericParamFromMetadata, TypeDefn> * string option
        =
        match error with
        | ArrayConstructorError.SzArrayLength error ->
            let fault, message = SzArrayAllocation.faultFor error
            OpcodeFault.resolve baseClassTypes fault, message
        | ArrayConstructorError.MultiDimLength error -> MultiDimArrayAllocation.exceptionFor baseClassTypes error

[<RequireQualifiedAccess>]
module ArrayConstruction =
    /// Allocate what `ArrayConstructor.plan` answered, returning the outermost array.
    let rec allocate
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (allocation : ArrayAllocation)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        match allocation with
        | ArrayAllocation.SzArray (element, length, eachElement) ->
            let zero, state =
                IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes element

            let array, state =
                IlMachineThreadState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero element)
                    (fun () -> zero)
                    length
                    state

            match eachElement with
            | None -> array, state
            | Some each ->
                let mutable state = state

                for i = 0 to length - 1 do
                    let inner, afterInner = allocate baseClassTypes each state
                    state <- IlMachineThreadState.setArrayValue array (CliType.ObjectRef (Some inner)) i afterInner

                array, state
        | ArrayAllocation.MultiDim (element, dimensions) ->
            for i = 0 to dimensions.Length - 1 do
                if dimensions.[i].LowerBound <> 0 then
                    failwith
                        $"TODO: array constructor gives dimension %d{i} the non-zero lower bound %d{dimensions.[i].LowerBound}; PawPrint's heap models zero lower bounds only"

            // With every lower bound zero, `plan` answers a rank-1 array as a szarray.
            if dimensions.Length < 2 then
                failwith
                    $"BUG: a multi-dimensional allocation of rank %d{dimensions.Length} with zero lower bounds; a rank-1 one is a szarray"

            let zero, state =
                IlMachineTypeResolution.cliTypeZeroOfHandle state baseClassTypes element

            IlMachineThreadState.allocateMultiDimArray
                (ConcreteTypeHandle.Array (element, dimensions.Length))
                (fun () -> zero)
                (dimensions
                 |> Seq.map (fun dimension -> dimension.Length)
                 |> ImmutableArray.CreateRange)
                state
