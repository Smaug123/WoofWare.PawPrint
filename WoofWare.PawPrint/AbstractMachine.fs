namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.FSharp.Core

type ThreadId = | ThreadId of int

type ManagedHeapAddress = | ManagedHeapAddress of int

type EvalStackValue =
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | Float of float
    /// allowed to be null
    | ManagedPointer of ManagedHeapAddress option
    | ObjectRef of ManagedHeapAddress
    | TransientPointer of int
    | UserDefinedValueType

type BasicCliObject =
    /// Can be assigned the null value 0
    | ObjectReference of ManagedHeapAddress option
    | PointerType of unit option
    | Int32 of int32
    | Int64 of int64
    | NativeInt of int64
    | NativeFloat of float

type CliObject =
    private
    | Basic of BasicCliObject
    | Bool of byte
    /// A UTF-16 code unit, i.e. two bytes. We store the most significant one first.
    | Char of byte * byte
    | UInt8 of uint8
    | UInt16 of uint16
    | Int8 of int8
    | Int16 of int16
    | Float32 of float32
    | Float64 of float32

    /// In fact any non-zero value will do for True, but we'll use 1
    static member OfBool (b : bool) = CliObject.Bool (if b then 1uy else 0uy)

    static member OfChar (c : char) =
        CliObject.Char (byte (int c / 256), byte (int c % 256))

    static member OfManagedObject (ptr : ManagedHeapAddress) =
        CliObject.Basic (BasicCliObject.ObjectReference (Some ptr))

type ReferenceType =
    | String of string
    | ManagedObject
    | Array of len : int * containedType : Type

    static member SizeOnHeap (r : ReferenceType) =
        match r with
        | ReferenceType.String s -> 2 * s.Length
        | ReferenceType.ManagedObject -> 8
        | ReferenceType.Array (len, ty) -> Type.SizeOf ty * len + 4 // for the len

and Type =
    | ReferenceType of ReferenceType
    | ValueType

    static member SizeOf (t : Type) : int =
        match t with
        | ReferenceType t -> ReferenceType.SizeOnHeap t
        | ValueType -> failwith "todo"


type EvalStack =
    {
        Values : EvalStackValue list
    }

    static member Empty : EvalStack =
        {
            Values = []
        }

    static member Push (v : CliObject) (stack : EvalStack) =
        let v =
            match v with
            | CliObject.Basic (BasicCliObject.Int32 i) -> EvalStackValue.Int32 i
            | CliObject.Basic (BasicCliObject.Int64 i) -> EvalStackValue.Int64 i
            | CliObject.Basic (BasicCliObject.NativeInt i) -> failwith "TODO"
            | CliObject.Basic (BasicCliObject.NativeFloat i) -> failwith "TODO"
            | CliObject.Basic (BasicCliObject.ObjectReference i) -> EvalStackValue.ManagedPointer i
            | CliObject.Basic (BasicCliObject.PointerType i) -> failwith "TODO"
            // Zero-extend unsigned int8/unsigned int16/bool/char
            | CliObject.Bool b -> int32 b |> EvalStackValue.Int32
            | CliObject.Char (high, low) -> int32 high * 256 + int32 low |> EvalStackValue.Int32
            | CliObject.UInt8 b -> int32 b |> EvalStackValue.Int32
            | CliObject.UInt16 b -> int32 b |> EvalStackValue.Int32
            // Sign-extend types int8 and int16
            | CliObject.Int8 b -> int32 b |> EvalStackValue.Int32
            | CliObject.Int16 b -> int32 b |> EvalStackValue.Int32
            | Float32 f -> failwith "todo"
            | Float64 f -> failwith "todo"

        {
            Values = v :: stack.Values
        }

type MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliObject ImmutableArray
        IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliObject ImmutableArray
        ExecutingMethod : MethodInfo
        /// We don't implement the local memory pool right now
        LocalMemoryPool : unit
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodState option
    }

    static member AdvanceProgramCounter (state : MethodState) =
        { state with
            IlOpIndex = state.IlOpIndex + 1
        }

    static member LoadArgument (index : int) (state : MethodState) : MethodState =
        // Correct CIL guarantees that we are loading an argument from an index that exists.
        { state with
            EvaluationStack = state.EvaluationStack |> EvalStack.Push state.Arguments.[index]
        }

type ThreadState =
    {
        // TODO: thread-local storage, synchronisation state, exception handling context
        MethodState : MethodState
    }

    static member New (methodState : MethodState) =
        {
            MethodState = methodState
        }

type ManagedHeap =
    {
        /// We store the size of the allocation too.
        Types : Map<ManagedHeapAddress, ReferenceType * int>
        Contents : ImmutableArray<byte option>
        FirstAvailableAddress : int
    }

    static member Empty : ManagedHeap =
        {
            Types = Map.empty
            // We'll leave the null reference empty.
            Contents = ImmutableArray.Create None
            FirstAvailableAddress = 1
        }

    static member Allocate (ty : ReferenceType) (heap : ManagedHeap) : ManagedHeapAddress * ManagedHeap =
        let size = ReferenceType.SizeOnHeap ty

        assert (heap.Contents.Length = heap.FirstAvailableAddress)
        let contents = heap.Contents.AddRange (Seq.replicate size None)

        let heap =
            {
                FirstAvailableAddress = heap.FirstAvailableAddress + size
                Types = heap.Types |> Map.add (ManagedHeapAddress heap.FirstAvailableAddress) (ty, size)
                Contents = contents
            }

        ManagedHeapAddress heap.FirstAvailableAddress, heap

    static member SetValue
        (alloc : ManagedHeapAddress)
        (offset : int)
        (v : CliObject)
        (heap : ManagedHeap)
        : ManagedHeap
        =
        let ty, _ = heap.Types.[alloc]
        let size = ReferenceType.SizeOnHeap ty
        let (ManagedHeapAddress a) = alloc

        let v =
            match v with
            | CliObject.Basic (BasicCliObject.ObjectReference o) ->
                if size <> 8 then
                    failwith
                        $"precondition failed! trying to write mismatched size 8 to array whose elements are size %i{size}"

                match o with
                | None -> Array.replicate 8 (Some 0uy)
                | Some (ManagedHeapAddress ptr) -> System.BitConverter.GetBytes (uint64 ptr) |> Array.map Some
            | _ -> failwith $"TODO: %O{v}"

        { heap with
            Contents = heap.Contents.RemoveRange(a + offset, size).InsertRange (a + offset, v)
        }

type IlMachineState =
    {
        NextThreadId : int
        EvalStacks : Map<ThreadId, EvalStack>
        // CallStack : StackFrame list
        /// Multiple managed heaps are allowed, but we hopefully only need one.
        ManagedHeap : ManagedHeap
        ThreadState : Map<ThreadId, ThreadState>
    }

    static member Initial : IlMachineState =
        {
            NextThreadId = 0
            EvalStacks = Map.empty
            // CallStack = []
            ManagedHeap = ManagedHeap.Empty
            ThreadState = Map.empty
        }

    static member AddThread (newThreadState : MethodState) (state : IlMachineState) : IlMachineState * ThreadId =
        let thread = ThreadId state.NextThreadId

        let newState =
            {
                NextThreadId = state.NextThreadId + 1
                EvalStacks = state.EvalStacks |> Map.add thread EvalStack.Empty
                // CallStack = state.CallStack
                ManagedHeap = state.ManagedHeap
                ThreadState = state.ThreadState |> Map.add thread (ThreadState.New newThreadState)
            }

        newState, thread

    static member Allocate (o : ReferenceType) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let alloc, heap = ManagedHeap.Allocate o state.ManagedHeap

        alloc,
        { state with
            ManagedHeap = heap
        }

    static member SetArrayValue
        (arrayAllocation : ManagedHeapAddress)
        (v : CliObject)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        // TODO: actually we need to skip the first four bytes because they hold the length
        let heap = ManagedHeap.SetValue arrayAllocation index v state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    static member AdvanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some (state : ThreadState) ->
                            { state with
                                MethodState = state.MethodState |> MethodState.AdvanceProgramCounter
                            }
                            |> Some
                    )
        }

    static member LoadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun state ->
                        match state with
                        | None -> failwith "expected state"
                        | Some state ->
                            { state with
                                MethodState = state.MethodState |> MethodState.LoadArgument index
                            }
                            |> Some
                    )
        }

[<RequireQualifiedAccess>]
module AbstractMachine =
    let internal executeNullary
        (state : IlMachineState)
        (currentThread : ThreadId)
        (dumped : DumpedAssembly)
        (op : NullaryIlOp)
        : IlMachineState
        =
        match op with
        | Nop -> state |> IlMachineState.AdvanceProgramCounter currentThread
        | LdArg0 ->
            state
            |> IlMachineState.LoadArgument currentThread 0
            |> IlMachineState.AdvanceProgramCounter currentThread
        | LdArg1 ->
            state
            |> IlMachineState.LoadArgument currentThread 1
            |> IlMachineState.AdvanceProgramCounter currentThread
        | LdArg2 ->
            state
            |> IlMachineState.LoadArgument currentThread 2
            |> IlMachineState.AdvanceProgramCounter currentThread
        | LdArg3 ->
            state
            |> IlMachineState.LoadArgument currentThread 3
            |> IlMachineState.AdvanceProgramCounter currentThread
        | Ldloc_0 -> failwith "todo"
        | Ldloc_1 -> failwith "todo"
        | Ldloc_2 -> failwith "todo"
        | Ldloc_3 -> failwith "todo"
        | Pop -> failwith "todo"
        | Dup -> failwith "todo"
        | Ret -> failwith "todo"
        | LdcI4_0 -> failwith "todo"
        | LdcI4_1 -> failwith "todo"
        | LdcI4_2 -> failwith "todo"
        | LdcI4_3 -> failwith "todo"
        | LdcI4_4 -> failwith "todo"
        | LdcI4_5 -> failwith "todo"
        | LdcI4_6 -> failwith "todo"
        | LdcI4_7 -> failwith "todo"
        | LdcI4_8 -> failwith "todo"
        | LdcI4_m1 -> failwith "todo"
        | LdNull -> failwith "todo"
        | Ceq -> failwith "todo"
        | Cgt -> failwith "todo"
        | Cgt_un -> failwith "todo"
        | Clt -> failwith "todo"
        | Clt_un -> failwith "todo"
        | Stloc_0 -> failwith "todo"
        | Stloc_1 -> failwith "todo"
        | Stloc_2 -> failwith "todo"
        | Stloc_3 -> failwith "todo"
        | Sub -> failwith "todo"
        | Sub_ovf -> failwith "todo"
        | Sub_ovf_un -> failwith "todo"
        | Add -> failwith "todo"
        | Add_ovf -> failwith "todo"
        | Add_ovf_un -> failwith "todo"
        | Mul -> failwith "todo"
        | Mul_ovf -> failwith "todo"
        | Mul_ovf_un -> failwith "todo"
        | Div -> failwith "todo"
        | Div_un -> failwith "todo"
        | Shr -> failwith "todo"
        | Shr_un -> failwith "todo"
        | Shl -> failwith "todo"
        | And -> failwith "todo"
        | Or -> failwith "todo"
        | Xor -> failwith "todo"
        | Conv_I -> failwith "todo"
        | Conv_I1 -> failwith "todo"
        | Conv_I2 -> failwith "todo"
        | Conv_I4 -> failwith "todo"
        | Conv_I8 -> failwith "todo"
        | Conv_R4 -> failwith "todo"
        | Conv_R8 -> failwith "todo"
        | Conv_U -> failwith "todo"
        | Conv_U1 -> failwith "todo"
        | Conv_U2 -> failwith "todo"
        | Conv_U4 -> failwith "todo"
        | Conv_U8 -> failwith "todo"
        | LdLen -> failwith "todo"
        | Endfilter -> failwith "todo"
        | Endfinally -> failwith "todo"
        | Rethrow -> failwith "todo"
        | Throw -> failwith "todo"

    let private executeUnaryMetadata
        (op : UnaryMetadataTokenIlOp)
        (metadataToken : MetadataToken)
        (state : IlMachineState)
        (dumped : DumpedAssembly)
        (thread : ThreadId)
        : IlMachineState
        =
        match op with
        | Call ->
            let handle =
                match metadataToken.Kind with
                | HandleKind.MethodSpecification -> MethodSpecificationHandle.op_Explicit metadataToken
                | k -> failwith $"Unrecognised kind: %O{k}"

            let method =
                dumped.Methods.[MethodDefinitionHandle.op_Explicit dumped.MethodSpecs.[handle].Method]

            failwith "TODO: now do this!"
            state
        | Callvirt -> failwith "todo"
        | Castclass -> failwith "todo"
        | Newobj -> failwith "todo"
        | Newarr -> failwith "todo"
        | Box -> failwith "todo"
        | Ldelema -> failwith "todo"
        | Isinst -> failwith "todo"
        | Stfld -> failwith "todo"
        | Stsfld -> failwith "todo"
        | Ldfld -> failwith "todo"
        | Ldflda -> failwith "todo"
        | Ldsfld -> failwith "todo"
        | Unbox_Any -> failwith "todo"
        | Stelem -> failwith "todo"
        | Ldelem -> failwith "todo"

    let executeOneStep (state : IlMachineState) (dumped : DumpedAssembly) (thread : ThreadId) : IlMachineState =
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> executeNullary state thread dumped op
        | UnaryConst unaryConstIlOp -> failwith "todo"
        | UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            executeUnaryMetadata unaryMetadataTokenIlOp bytes state dumped thread
        | Switch immutableArray -> failwith "todo"
        | UnaryStringToken (unaryStringTokenIlOp, stringHandle) -> failwith "todo"
