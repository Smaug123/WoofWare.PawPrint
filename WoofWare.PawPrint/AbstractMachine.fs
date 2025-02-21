namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
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

    static member Empty (method : MethodInfo) (returnState : MethodState option) =
        {
            EvaluationStack = EvalStack.Empty
            LocalVariables =
                // TODO: use method.LocalsInit
                ImmutableArray.Empty
            IlOpIndex = 0
            Arguments = Array.zeroCreate method.Parameters.Length |> ImmutableArray.ToImmutableArray
            ExecutingMethod = method
            LocalMemoryPool = ()
            ReturnState = returnState
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
        InternedStrings : ImmutableDictionary<StringToken, ManagedHeapAddress>
        ActiveAssemblyName : string
        LoadedAssemblies : Map<string, DumpedAssembly>
    }

    member this.ActiveAssembly = this.LoadedAssemblies.[this.ActiveAssemblyName]

    static member Initial (entryAssembly : DumpedAssembly) : IlMachineState =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name

        {
            NextThreadId = 0
            EvalStacks = Map.empty
            // CallStack = []
            ManagedHeap = ManagedHeap.Empty
            ThreadState = Map.empty
            InternedStrings = ImmutableDictionary.Empty
            ActiveAssemblyName = assyName
            LoadedAssemblies = Map.ofList [ assyName, entryAssembly ]
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
                InternedStrings = state.InternedStrings
                ActiveAssemblyName = state.ActiveAssemblyName
                LoadedAssemblies = state.LoadedAssemblies
            }

        newState, thread

    static member Allocate (o : ReferenceType) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let alloc, heap = ManagedHeap.Allocate o state.ManagedHeap

        alloc,
        { state with
            ManagedHeap = heap
        }

    static member PushToStack (o : CliObject) (thread : ThreadId) (state : IlMachineState) =
        { state with
            EvalStacks =
                state.EvalStacks
                |> Map.change
                    thread
                    (fun s ->
                        match s with
                        | None -> failwith "tried to push to stack of nonexistent thread"
                        | Some stack -> EvalStack.Push o stack |> Some
                    )
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
        | Localloc -> failwith "todo"
        | Stind_I -> failwith "todo"
        | Stind_I1 -> failwith "todo"
        | Stind_I2 -> failwith "todo"
        | Stind_I4 -> failwith "todo"
        | Stind_I8 -> failwith "todo"
        | Stind_R4 -> failwith "todo"
        | Stind_R8 -> failwith "todo"
        | Ldind_i -> failwith "todo"
        | Ldind_i1 -> failwith "todo"
        | Ldind_i2 -> failwith "todo"
        | Ldind_i4 -> failwith "todo"
        | Ldind_i8 -> failwith "todo"
        | Ldind_u1 -> failwith "todo"
        | Ldind_u2 -> failwith "todo"
        | Ldind_u4 -> failwith "todo"
        | Ldind_u8 -> failwith "todo"
        | Ldind_r4 -> failwith "todo"
        | Ldind_r8 -> failwith "todo"
        | Rem -> failwith "todo"
        | Rem_un -> failwith "todo"
        | Volatile -> failwith "todo"
        | Tail -> failwith "todo"
        | Conv_ovf_i_un -> failwith "todo"
        | Conv_ovf_u_un -> failwith "todo"
        | Conv_ovf_i1_un -> failwith "todo"
        | Conv_ovf_u1_un -> failwith "todo"
        | Conv_ovf_i2_un -> failwith "todo"
        | Conv_ovf_u2_un -> failwith "todo"
        | Conv_ovf_i4_un -> failwith "todo"
        | Conv_ovf_u4_un -> failwith "todo"
        | Conv_ovf_i8_un -> failwith "todo"
        | Conv_ovf_u8_un -> failwith "todo"
        | Conv_ovf_i -> failwith "todo"
        | Conv_ovf_u -> failwith "todo"
        | Neg -> failwith "todo"
        | Not -> failwith "todo"
        | Ldind_ref -> failwith "todo"
        | Stind_ref -> failwith "todo"
        | Ldelem_i -> failwith "todo"
        | Ldelem_i1 -> failwith "todo"
        | Ldelem_u1 -> failwith "todo"
        | Ldelem_i2 -> failwith "todo"
        | Ldelem_u2 -> failwith "todo"
        | Ldelem_i4 -> failwith "todo"
        | Ldelem_u4 -> failwith "todo"
        | Ldelem_i8 -> failwith "todo"
        | Ldelem_u8 -> failwith "todo"
        | Ldelem_r4 -> failwith "todo"
        | Ldelem_r8 -> failwith "todo"
        | Ldelem_ref -> failwith "todo"
        | Stelem_i -> failwith "todo"
        | Stelem_i1 -> failwith "todo"
        | Stelem_u1 -> failwith "todo"
        | Stelem_i2 -> failwith "todo"
        | Stelem_u2 -> failwith "todo"
        | Stelem_i4 -> failwith "todo"
        | Stelem_u4 -> failwith "todo"
        | Stelem_i8 -> failwith "todo"
        | Stelem_u8 -> failwith "todo"
        | Stelem_r4 -> failwith "todo"
        | Stelem_r8 -> failwith "todo"
        | Stelem_ref -> failwith "todo"

    let private executeUnaryMetadata
        (dotnetRuntimeDirs : string[])
        (op : UnaryMetadataTokenIlOp)
        (metadataToken : MetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState
        =
        match op with
        | Call ->
            let state, methodToCall =
                match metadataToken with
                | MetadataToken.MethodSpecification h ->
                    let spec = state.ActiveAssembly.MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token -> state, state.ActiveAssembly.Methods.[token]
                    | k -> failwith $"Unrecognised kind: %O{k}"
                | MetadataToken.MemberReference h ->
                    let mem = state.ActiveAssembly.Members.[h]

                    let memberSig =
                        match mem.Signature with
                        | MemberSignature.Field _ -> failwith "Trying to CALL a field?!"
                        | MemberSignature.Method method -> method

                    let memberName : string = state.ActiveAssembly.Strings mem.Name

                    let parent =
                        match mem.Parent with
                        | MetadataToken.TypeReference typeRef -> state.ActiveAssembly.TypeRefs.[typeRef]
                        | parent -> failwith $"Unexpected: {parent}"

                    match parent.ResolutionScope with
                    | AssemblyReference r ->
                        let state, assy, newAssyName =
                            let assemblyRef = state.ActiveAssembly.AssemblyReferences.[r]
                            let assemblyName = state.ActiveAssembly.Strings assemblyRef.Name

                            match state.LoadedAssemblies.TryGetValue assemblyName with
                            | true, v -> state, v, assemblyName
                            | false, _ ->
                                let assy =
                                    dotnetRuntimeDirs
                                    |> Seq.choose (fun dir ->
                                        let file = Path.Combine (dir, assemblyName + ".dll")

                                        try
                                            use f = File.OpenRead file
                                            Console.Error.WriteLine $"Loading {file}"
                                            Assembly.read f |> Some
                                        with :? FileNotFoundException ->
                                            None
                                    )
                                    |> Seq.exactlyOne

                                { state with
                                    LoadedAssemblies = state.LoadedAssemblies |> Map.add assemblyName assy
                                },
                                assy,
                                assemblyName

                        let nsPath =
                            state.ActiveAssembly.Strings(parent.Namespace).Split '.' |> Array.toList

                        let targetNs = assy.NonRootNamespaces.[nsPath]

                        let targetType =
                            targetNs.TypeDefinitions
                            |> Seq.choose (fun td ->
                                let ty = assy.TypeDefs.[td]

                                if ty.Name = state.ActiveAssembly.Strings parent.Name then
                                    Some ty
                                else
                                    None
                            )
                            |> Seq.exactlyOne

                        let availableMethods =
                            targetType.Methods
                            |> List.filter (fun mi -> mi.Name = memberName)
                            |> List.filter (fun mi -> mi.Signature = memberSig)

                        let method =
                            match availableMethods with
                            | [] -> failwith $"Could not find member {memberName} with the right signature in CALL"
                            | [ x ] -> x
                            | _ -> failwith $"Multiple overloads matching signature for call to {memberName}!"

                        { state with
                            ActiveAssemblyName = newAssyName
                        },
                        method
                    | k -> failwith $"Unexpected: {k}"
                | MetadataToken.MethodDef defn -> state, state.ActiveAssembly.Methods.[defn]
                | k -> failwith $"Unrecognised kind: %O{k}"

            let threadState =
                let threadState = state.ThreadState.[thread]

                { threadState with
                    MethodState = MethodState.Empty methodToCall (Some threadState.MethodState)
                }

            { state with
                ThreadState = state.ThreadState |> Map.add thread threadState
            }

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
        | Initobj -> failwith "todo"
        | Ldsflda -> failwith "todo"
        | Ldftn -> failwith "todo"
        | Stobj -> failwith "todo"
        | Constrained -> failwith "todo"
        | Ldtoken -> failwith "todo"
        | Cpobj -> failwith "todo"
        | Ldobj -> failwith "todo"

    let private executeUnaryStringToken
        (op : UnaryStringTokenIlOp)
        (sh : StringToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            let addressToLoad, state =
                match state.InternedStrings.TryGetValue sh with
                | false, _ ->
                    let toAllocate = state.ActiveAssembly.Strings sh
                    let addr, state = IlMachineState.Allocate (ReferenceType.String toAllocate) state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (sh, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.PushToStack
                    (CliObject.Basic (BasicCliObject.ObjectReference (Some addressToLoad)))
                    thread
                    state
            // +1 for the opcode, +4 for the bytes of the handle.
            // TODO: some opcodes are multiple bytes! Should deal with that.
            let mutable state = state

            for i = 0 to 4 do
                state <- IlMachineState.AdvanceProgramCounter thread state

            state

    let executeOneStep (dotnetRuntimePath : string[]) (state : IlMachineState) (thread : ThreadId) : IlMachineState =
        let instruction = state.ThreadState.[thread].MethodState

        Console.Error.WriteLine
            $"[DBG] Executing one step! Now executing: {instruction.IlOpIndex} in {instruction.ExecutingMethod.Name}"

        match instruction.ExecutingMethod.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> executeNullary state thread op
        | UnaryConst unaryConstIlOp -> failwith "todo"
        | UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            executeUnaryMetadata dotnetRuntimePath unaryMetadataTokenIlOp bytes state thread
        | Switch immutableArray -> failwith "todo"
        | UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            executeUnaryStringToken unaryStringTokenIlOp stringHandle state thread
