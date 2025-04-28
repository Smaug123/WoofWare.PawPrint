namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
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
    /// This is the 'O' type.
    | ObjectReference of ManagedHeapAddress option
    /// This is the '&' type.
    | PointerType of ManagedHeapAddress option
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

    static member Pop (stack : EvalStack) : EvalStackValue * EvalStack =
        match stack.Values with
        | [] -> failwith "eval stack was empty on pop instruction"
        | v :: rest ->
            let stack =
                {
                    Values = rest
                }

            v, stack

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


type MethodReturnState =
    {
        JumpTo : MethodState
        WasInitialising : (TypeDefinitionHandle * AssemblyName) option
    }

and MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliObject ImmutableArray
        /// Index into the stream of IL bytes.
        IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliObject ImmutableArray
        ExecutingMethod : WoofWare.PawPrint.MethodInfo
        /// We don't implement the local memory pool right now
        LocalMemoryPool : unit
        /// On return, we restore this state. This should be Some almost always; an exception is the entry point.
        ReturnState : MethodReturnState option
    }

    static member jumpProgramCounter (bytes : int) (state : MethodState) =
        { state with
            IlOpIndex = state.IlOpIndex + bytes
        }

    static member advanceProgramCounter (state : MethodState) =
        MethodState.jumpProgramCounter (IlOp.NumberOfBytes state.ExecutingMethod.Locations.[state.IlOpIndex]) state

    static member loadArgument (index : int) (state : MethodState) : MethodState =
        // Correct CIL guarantees that we are loading an argument from an index that exists.
        { state with
            EvaluationStack = state.EvaluationStack |> EvalStack.Push state.Arguments.[index]
        }

    static member popFromStack (localVariableIndex : int) (state : MethodState) : MethodState =
        if localVariableIndex >= state.LocalVariables.Length then
            failwith
                $"Tried to access zero-indexed local variable %i{localVariableIndex} but only %i{state.LocalVariables.Length} exist"

        if localVariableIndex < 0 || localVariableIndex >= 65535 then
            failwith $"Incorrect CIL encountered: local variable index has value %i{localVariableIndex}"

        let popped, newStack = EvalStack.Pop state.EvaluationStack

        let desiredValue =
            match state.LocalVariables.[localVariableIndex] with
            | Basic (BasicCliObject.Int32 _) ->
                match popped with
                | EvalStackValue.Int32 i -> CliObject.Basic (BasicCliObject.Int32 i)
                | EvalStackValue.Int64 int64 -> failwith "todo"
                | EvalStackValue.NativeInt int64 -> failwith "todo"
                | EvalStackValue.Float f -> failwith "todo"
                | EvalStackValue.ManagedPointer managedHeapAddressOption -> failwith "todo"
                | EvalStackValue.ObjectRef managedHeapAddress -> failwith "todo"
                | EvalStackValue.TransientPointer i -> failwith "todo"
                | EvalStackValue.UserDefinedValueType -> failwith "todo"
            | Basic (BasicCliObject.Int64 _) -> failwith "todo"
            | Basic (BasicCliObject.NativeFloat _) -> failwith "todo"
            | Basic (BasicCliObject.NativeInt _) -> failwith "todo"
            | Basic (BasicCliObject.ObjectReference _) -> failwith "todo"
            | Basic (BasicCliObject.PointerType _) -> failwith "todo"
            | Bool b -> failwith "todo"
            | Char (b, b1) -> failwith "todo"
            | UInt8 b -> failwith "todo"
            | UInt16 s -> failwith "todo"
            | Int8 b -> failwith "todo"
            | Int16 s -> failwith "todo"
            | Float32 f -> failwith "todo"
            | Float64 f -> failwith "todo"

        { state with
            EvaluationStack = newStack
            LocalVariables = state.LocalVariables.SetItem (localVariableIndex, desiredValue)
        }

    static member Empty (method : WoofWare.PawPrint.MethodInfo) (returnState : MethodReturnState option) =
        let localVariableSig =
            match method.LocalVars with
            | None -> ImmutableArray.Empty
            | Some vars -> vars
        // I think valid code should remain valid if we unconditionally localsInit - it should be undefined
        // to use an uninitialised value? Not checked this; TODO.
        let localVars =
            localVariableSig
            |> Seq.map (fun var ->
                match var with
                | TypeDefn.PrimitiveType primitiveType ->
                    match primitiveType with
                    | PrimitiveType.Void -> failwith "todo"
                    | PrimitiveType.Boolean -> CliObject.Bool 0uy
                    | PrimitiveType.Char -> failwith "todo"
                    | PrimitiveType.SByte -> failwith "todo"
                    | PrimitiveType.Byte -> failwith "todo"
                    | PrimitiveType.Int16 -> failwith "todo"
                    | PrimitiveType.UInt16 -> failwith "todo"
                    | PrimitiveType.Int32 -> CliObject.Basic (BasicCliObject.Int32 0)
                    | PrimitiveType.UInt32 -> failwith "todo"
                    | PrimitiveType.Int64 -> CliObject.Basic (BasicCliObject.Int64 0L)
                    | PrimitiveType.UInt64 -> failwith "todo"
                    | PrimitiveType.Single -> failwith "todo"
                    | PrimitiveType.Double -> failwith "todo"
                    | PrimitiveType.String -> failwith "todo"
                    | PrimitiveType.TypedReference -> failwith "todo"
                    | PrimitiveType.IntPtr -> failwith "todo"
                    | PrimitiveType.UIntPtr -> failwith "todo"
                    | PrimitiveType.Object -> failwith "todo"
                | TypeDefn.Array (elt, shape) -> failwith "todo"
                | TypeDefn.Pinned typeDefn -> failwith "todo"
                | TypeDefn.Pointer typeDefn -> failwith "todo"
                | TypeDefn.Byref typeDefn -> failwith "todo"
                | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
                | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
                | TypeDefn.FromReference signatureTypeKind -> CliObject.Basic (BasicCliObject.ObjectReference None)
                | TypeDefn.FromDefinition signatureTypeKind -> failwith "todo"
                | TypeDefn.GenericInstantiation (generic, args) -> failwith "todo"
                | TypeDefn.FunctionPointer typeMethodSignature -> failwith "todo"
                | TypeDefn.GenericTypeParameter index -> failwith "todo"
                | TypeDefn.GenericMethodParameter index -> failwith "todo"
            )
            |> ImmutableArray.CreateRange

        {
            EvaluationStack = EvalStack.Empty
            LocalVariables = localVars
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
        ActiveAssembly : AssemblyName
    }

    static member New (activeAssy : AssemblyName) (methodState : MethodState) =
        {
            MethodState = methodState
            ActiveAssembly = activeAssy
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

type WhatWeDid =
    | Executed
    /// We didn't run what you wanted, because we have to do class initialisation first.
    | SuspendedForClassInit
    | NotTellingYou
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId

/// Represents the state of a type's initialization in the CLI
type TypeInitState =
    | InProgress of ThreadId // Being initialized by this thread
    | Initialized

/// Tracks the initialization state of types across assemblies
type TypeInitTable = ImmutableDictionary<TypeDefinitionHandle * AssemblyName, TypeInitState>

[<RequireQualifiedAccess>]
module TypeInitTable =
    let beginInitialising
        (thread : ThreadId)
        (typeDef : TypeDefinitionHandle * AssemblyName)
        (t : TypeInitTable)
        : TypeInitTable
        =
        match t.TryGetValue typeDef with
        | false, _ -> t.Add (typeDef, TypeInitState.InProgress thread)
        | true, v -> failwith "Logic error: tried initialising a type which has already started initialising"

    let markInitialised
        (thread : ThreadId)
        (typeDef : TypeDefinitionHandle * AssemblyName)
        (t : TypeInitTable)
        : TypeInitTable
        =
        match t.TryGetValue typeDef with
        | false, _ -> failwith "Logic error: completing initialisation of a type which never started initialising"
        | true, TypeInitState.Initialized ->
            failwith "Logic error: completing initialisation of a type which has already finished initialising"
        | true, TypeInitState.InProgress thread2 ->
            if thread <> thread2 then
                failwith
                    "Logic error: completed initialisation of a type on a different thread to the one which started it!"
            else
                t.SetItem (typeDef, TypeInitState.Initialized)

type IlMachineState =
    {
        NextThreadId : int
        // CallStack : StackFrame list
        /// Multiple managed heaps are allowed, but we hopefully only need one.
        ManagedHeap : ManagedHeap
        ThreadState : Map<ThreadId, ThreadState>
        InternedStrings : ImmutableDictionary<StringToken, ManagedHeapAddress>
        /// Keyed by FullName. (Sometimes an assembly has a PublicKey when we read it from the disk, but we
        /// only have a reference to it by an AssemblyName without a PublicKey.)
        _LoadedAssemblies : ImmutableDictionary<string, DumpedAssembly>
        /// Tracks initialization state of types across assemblies
        TypeInitTable : TypeInitTable
        Statics : ImmutableDictionary<TypeDefinitionHandle * AssemblyName, ManagedHeapAddress>
        DotnetRuntimeDirs : string ImmutableArray
    }

    member this.WithLoadedAssembly (name : AssemblyName) (value : DumpedAssembly) =
        { this with
            _LoadedAssemblies = this._LoadedAssemblies.Add (name.FullName, value)
        }

    member this.LoadedAssembly (name : AssemblyName) : DumpedAssembly option =
        match this._LoadedAssemblies.TryGetValue name.FullName with
        | false, _ -> None
        | true, v -> Some v

    /// Returns also the original assembly name.
    member this.WithThreadSwitchedToAssembly (assy : AssemblyName) (thread : ThreadId) : IlMachineState * AssemblyName =
        let mutable existing = Unchecked.defaultof<AssemblyName>

        let newState =
            this.ThreadState
            |> Map.change
                thread
                (fun s ->
                    match s with
                    | None -> failwith $"expected thread {thread} to be in a state already; internal logic error"
                    | Some s ->
                        existing <- s.ActiveAssembly

                        { s with
                            ActiveAssembly = assy
                        }
                        |> Some
                )

        { this with
            ThreadState = newState
        },
        existing

    member this.ActiveAssembly (thread : ThreadId) =
        let active = this.ThreadState.[thread].ActiveAssembly

        match this.LoadedAssembly active with
        | Some v -> v
        | None ->
            let available = this._LoadedAssemblies.Keys |> String.concat " ; "

            failwith
                $"Somehow we believe the active assembly is {active}, but only had the following available: {available}"

type StateLoadResult =
    | NothingToDo of IlMachineState
    | FirstLoadThis of IlMachineState

[<RequireQualifiedAccess>]
module IlMachineState =
    type private Dummy = class end

    let loadAssembly
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (r : AssemblyReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * AssemblyName
        =
        let assemblyRef = referencedInAssembly.AssemblyReferences.[r]
        let assemblyName = assemblyRef.Name

        match state.LoadedAssembly assemblyName with
        | Some v -> state, v, assemblyName
        | None ->
            let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

            let assy =
                state.DotnetRuntimeDirs
                |> Seq.choose (fun dir ->
                    let file = Path.Combine (dir, assemblyName.Name + ".dll")

                    try
                        use f = File.OpenRead file
                        logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", file)
                        Assembly.read loggerFactory f |> Some
                    with :? FileNotFoundException ->
                        None
                )
                |> Seq.exactlyOne

            state.WithLoadedAssembly assemblyName assy, assy, assemblyName

    let rec internal resolveTypeFromName
        (loggerFactory : ILoggerFactory)
        (ns : string option)
        (name : string)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo
        =
        match ns with
        | None -> failwith "what are the semantics here"
        | Some ns ->

        match assy.TypeDef ns name with
        | Some typeDef -> state, assy, typeDef
        | None ->

        match assy.TypeRef ns name with
        | Some typeRef -> resolveTypeFromRef loggerFactory assy typeRef state
        | None ->

        match assy.ExportedType (Some ns) name with
        | Some export -> resolveTypeFromExport loggerFactory assy export state
        | None -> failwith $"TODO: {ns} {name}"

    and resolveTypeFromExport
        (loggerFactory : ILoggerFactory)
        (fromAssembly : DumpedAssembly)
        (ty : WoofWare.PawPrint.ExportedType)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo
        =
        match ty.Data with
        | NonForwarded _ -> failwith "Somehow didn't find type definition but it is exported"
        | ForwardsTo assy ->
            let state, targetAssy, _ = loadAssembly loggerFactory fromAssembly assy state
            resolveTypeFromName loggerFactory ty.Namespace ty.Name targetAssy state

    and resolveTypeFromRef
        (loggerFactory : ILoggerFactory)
        (referencedInAssembly : DumpedAssembly)
        (target : TypeRef)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo
        =
        match target.ResolutionScope with
        | AssemblyReference r ->
            let state, assy, newAssyName =
                loadAssembly loggerFactory referencedInAssembly r state

            let nsPath = target.Namespace.Split '.' |> Array.toList

            let targetNs = assy.NonRootNamespaces.[nsPath]

            let targetType =
                targetNs.TypeDefinitions
                |> Seq.choose (fun td ->
                    let ty = assy.TypeDefs.[td]

                    if ty.Name = target.Name && ty.Namespace = target.Namespace then
                        Some ty
                    else
                        None
                )
                |> Seq.toList

            match targetType with
            | [ t ] -> state, assy, t
            | _ :: _ :: _ -> failwith $"Multiple matching type definitions! {nsPath} {target.Name}"
            | [] ->
                match assy.ExportedType (Some target.Namespace) target.Name with
                | None -> failwith $"Failed to find type {nsPath} {target.Name} in {assy.Name.FullName}!"
                | Some ty -> resolveTypeFromExport loggerFactory assy ty state
        | k -> failwith $"Unexpected: {k}"

    and resolveType
        (loggerFactory : ILoggerFactory)
        (ty : TypeReferenceHandle)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo
        =
        let target = assy.TypeRefs.[ty]

        resolveTypeFromRef loggerFactory assy target state

    let resolveTypeFromSpec
        (loggerFactory : ILoggerFactory)
        (ty : TypeSpecificationHandle)
        (assy : DumpedAssembly)
        (state : IlMachineState)
        : IlMachineState * DumpedAssembly * WoofWare.PawPrint.TypeInfo
        =
        failwith "TODO"

    let rec loadClass
        (loggerFactory : ILoggerFactory)
        (typeDefHandle : TypeDefinitionHandle)
        (assemblyName : AssemblyName)
        (currentThread : ThreadId)
        (state : IlMachineState)
        : StateLoadResult
        =
        if typeDefHandle.IsNil then
            failwith "Called `loadClass` with a nil typedef"

        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType

        match state.TypeInitTable.TryGetValue ((typeDefHandle, assemblyName)) with
        | true, TypeInitState.Initialized ->
            // Type already initialized; nothing to do
            StateLoadResult.NothingToDo state
        | true, TypeInitState.InProgress tid when tid = currentThread ->
            // We're already initializing this type on this thread; just proceed with the initialisation, no extra
            // class loading required.
            StateLoadResult.NothingToDo state
        | true, TypeInitState.InProgress _ ->
            // This is usually signalled by WhatWeDid.Blocked
            failwith "TODO: this thread has to wait for the other thread to finish initialisation"
        | false, _ ->
            // We have work to do!

            let state, origAssyName =
                state.WithThreadSwitchedToAssembly assemblyName currentThread

            let sourceAssembly = state.LoadedAssembly assemblyName |> Option.get

            let typeDef =
                match sourceAssembly.TypeDefs.TryGetValue typeDefHandle with
                | false, _ -> failwith $"Failed to find type definition {typeDefHandle} in {assemblyName.Name}"
                | true, v -> v

            logger.LogDebug ("Resolving type {TypeDefNamespace}.{TypeDefName}", typeDef.Namespace, typeDef.Name)

            // First mark as in-progress to detect cycles
            let state =
                { state with
                    TypeInitTable =
                        state.TypeInitTable
                        |> TypeInitTable.beginInitialising currentThread (typeDefHandle, assemblyName)
                }

            // Check if the type has a base type that needs initialization
            let firstDoBaseClass =
                match typeDef.BaseType with
                | Some baseTypeInfo ->
                    // Determine if base type is in the same or different assembly
                    match baseTypeInfo with
                    | ForeignAssemblyType (baseAssemblyName, baseTypeHandle) ->
                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to foreign assembly {ForeignAssemblyName}",
                            typeDef.Namespace,
                            typeDef.Name,
                            baseAssemblyName.Name
                        )

                        match loadClass loggerFactory baseTypeHandle baseAssemblyName currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | TypeDef typeDefinitionHandle ->
                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to this assembly, typedef",
                            typeDef.Namespace,
                            typeDef.Name
                        )

                        match loadClass loggerFactory typeDefinitionHandle assemblyName currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | TypeRef typeReferenceHandle ->
                        let state, assy, targetType =
                            resolveType loggerFactory typeReferenceHandle (state.ActiveAssembly currentThread) state

                        logger.LogDebug (
                            "Resolved base type of {TypeDefNamespace}.{TypeDefName} to a typeref in assembly {ResolvedAssemblyName}, {BaseTypeNamespace}.{BaseTypeName}",
                            typeDef.Namespace,
                            typeDef.Name,
                            assy.Name.Name,
                            targetType.Namespace,
                            targetType.Name
                        )

                        match loadClass loggerFactory targetType.TypeDefHandle assy.Name currentThread state with
                        | FirstLoadThis state -> Error state
                        | NothingToDo state -> Ok state
                    | TypeSpec typeSpecificationHandle -> failwith "todo"
                | None -> Ok state // No base type (or it's System.Object)

            match firstDoBaseClass with
            | Error state -> FirstLoadThis state
            | Ok state ->

            // Find the class constructor (.cctor) if it exists
            let cctor =
                typeDef.Methods
                |> List.tryFind (fun method -> method.Name = ".cctor" && method.IsStatic && method.Parameters.IsEmpty)

            match cctor with
            | Some ctorMethod ->
                // Call the class constructor! Note that we *don't* use `callMethodInActiveAssembly`, because that
                // performs class loading, but we're already in the middle of loading this class.
                // TODO: factor out the common bit.
                let currentThreadState = state.ThreadState.[currentThread]

                let newMethodState =
                    MethodState.Empty
                        ctorMethod
                        (Some
                            {
                                JumpTo = currentThreadState.MethodState |> MethodState.advanceProgramCounter
                                WasInitialising = Some (typeDefHandle, assemblyName)
                            })

                { state with
                    ThreadState =
                        state.ThreadState
                        |> Map.add
                            currentThread
                            { currentThreadState with
                                MethodState = newMethodState
                            }
                }
                |> FirstLoadThis
            | None ->
                // No constructor, just continue.
                // Mark the type as initialized.
                let state =
                    { state with
                        TypeInitTable =
                            state.TypeInitTable
                            |> TypeInitTable.markInitialised currentThread (typeDefHandle, assemblyName)
                    }

                // Restore original assembly context if needed
                state.WithThreadSwitchedToAssembly origAssyName currentThread
                |> fst
                |> NothingToDo

    let callMethodInActiveAssembly
        (loggerFactory : ILoggerFactory)
        (thread : ThreadId)
        (methodToCall : WoofWare.PawPrint.MethodInfo)
        (state : IlMachineState)
        : IlMachineState * WhatWeDid
        =
        let threadState = state.ThreadState.[thread]

        match state.TypeInitTable.TryGetValue methodToCall.DeclaringType with
        | false, _ ->
            match
                loadClass loggerFactory (fst methodToCall.DeclaringType) (snd methodToCall.DeclaringType) thread state
            with
            | NothingToDo state ->
                // TODO: factor this out, it's the same as the Initialized flow
                let newThreadState =
                    { threadState with
                        MethodState =
                            MethodState.Empty
                                methodToCall
                                (Some
                                    {
                                        JumpTo = threadState.MethodState |> MethodState.advanceProgramCounter
                                        WasInitialising = None
                                    })
                    }

                { state with
                    ThreadState = state.ThreadState |> Map.add thread newThreadState
                },
                WhatWeDid.Executed
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
        | true, TypeInitState.Initialized ->
            let newThreadState =
                { threadState with
                    MethodState =
                        MethodState.Empty
                            methodToCall
                            (Some
                                {
                                    JumpTo = threadState.MethodState |> MethodState.advanceProgramCounter
                                    WasInitialising = None
                                })
                }

            { state with
                ThreadState = state.ThreadState |> Map.add thread newThreadState
            },
            WhatWeDid.Executed
        | true, InProgress threadId -> state, WhatWeDid.BlockedOnClassInit threadId


    let initial (dotnetRuntimeDirs : ImmutableArray<string>) (entryAssembly : DumpedAssembly) : IlMachineState =
        let assyName = entryAssembly.ThisAssemblyDefinition.Name

        {
            NextThreadId = 0
            // CallStack = []
            ManagedHeap = ManagedHeap.Empty
            ThreadState = Map.empty
            InternedStrings = ImmutableDictionary.Empty
            _LoadedAssemblies = ImmutableDictionary.Empty
            Statics = ImmutableDictionary.Empty
            TypeInitTable = ImmutableDictionary.Empty
            DotnetRuntimeDirs = dotnetRuntimeDirs
        }
            .WithLoadedAssembly
            assyName
            entryAssembly

    let addThread
        (newThreadState : MethodState)
        (newThreadAssy : AssemblyName)
        (state : IlMachineState)
        : IlMachineState * ThreadId
        =
        let thread = ThreadId state.NextThreadId

        let newState =
            { state with
                NextThreadId = state.NextThreadId + 1
                ThreadState =
                    state.ThreadState
                    |> Map.add thread (ThreadState.New newThreadAssy newThreadState)
            }

        newState, thread

    let allocate (o : ReferenceType) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
        let alloc, heap = ManagedHeap.Allocate o state.ManagedHeap

        alloc,
        { state with
            ManagedHeap = heap
        }

    let pushToEvalStack (o : CliObject) (thread : ThreadId) (state : IlMachineState) =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun threadState ->
                        match threadState with
                        | None -> failwith "Logic error: tried to push to stack of a nonexistent thread"
                        | Some threadState ->
                            { threadState with
                                ThreadState.MethodState.EvaluationStack =
                                    threadState.MethodState.EvaluationStack |> EvalStack.Push o
                            }
                            |> Some
                    )
        }

    // TODO: which stack do we actually want to push to?
    let pushToStackCoerced (o : EvalStackValue) (targetType : TypeDefn) (thread : ThreadId) (state : IlMachineState) =
        { state with
            ThreadState =
                state.ThreadState
                |> Map.change
                    thread
                    (fun threadState ->
                        match threadState with
                        | None -> failwith "Logic error: tried to push to stack of a nonexistent thread"
                        | Some threadState ->
                            { threadState with
                                ThreadState.MethodState.EvaluationStack =
                                    threadState.MethodState.EvaluationStack |> EvalStack.Push (failwith "TODO")
                            }
                            |> Some
                    )
        }

    let popFromStackToLocalVariable
        (thread : ThreadId)
        (localVariableIndex : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let threadState =
            match Map.tryFind thread state.ThreadState with
            | None -> failwith "Logic error: tried to pop from stack of nonexistent thread"
            | Some threadState -> threadState

        let methodState =
            MethodState.popFromStack localVariableIndex threadState.MethodState

        { state with
            ThreadState =
                state.ThreadState
                |> Map.add
                    thread
                    { threadState with
                        MethodState = methodState
                    }
        }

    let setArrayValue
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

    let advanceProgramCounter (thread : ThreadId) (state : IlMachineState) : IlMachineState =
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
                                MethodState = state.MethodState |> MethodState.advanceProgramCounter
                            }
                            |> Some
                    )
        }

    let jumpProgramCounter (thread : ThreadId) (bytes : int) (state : IlMachineState) : IlMachineState =
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
                                MethodState = state.MethodState |> MethodState.jumpProgramCounter bytes
                            }
                            |> Some
                    )
        }

    let loadArgument (thread : ThreadId) (index : int) (state : IlMachineState) : IlMachineState =
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
                                MethodState = state.MethodState |> MethodState.loadArgument index
                            }
                            |> Some
                    )
        }

type ExecutionResult =
    | Terminated of IlMachineState * terminatingThread : ThreadId
    | Stepped of IlMachineState * WhatWeDid

[<RequireQualifiedAccess>]
module AbstractMachine =
    type private Dummy = class end

    let internal executeNullary
        (state : IlMachineState)
        (currentThread : ThreadId)
        (op : NullaryIlOp)
        : ExecutionResult
        =
        match op with
        | Nop ->
            (IlMachineState.advanceProgramCounter currentThread state, WhatWeDid.Executed)
            |> ExecutionResult.Stepped
        | LdArg0 ->
            state
            |> IlMachineState.loadArgument currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg1 ->
            state
            |> IlMachineState.loadArgument currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg2 ->
            state
            |> IlMachineState.loadArgument currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdArg3 ->
            state
            |> IlMachineState.loadArgument currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_0 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[0]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_1 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[1]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_2 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[2]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Ldloc_3 ->
            let localVar = state.ThreadState.[currentThread].MethodState.LocalVariables.[3]

            state
            |> IlMachineState.pushToEvalStack localVar currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Pop -> failwith "todo"
        | Dup -> failwith "todo"
        | Ret ->
            let threadStateAtEndOfMethod = state.ThreadState.[currentThread]

            match threadStateAtEndOfMethod.MethodState.ReturnState with
            | None -> ExecutionResult.Terminated (state, currentThread)
            | Some returnState ->

            let state =
                match returnState.WasInitialising with
                | None -> state
                | Some finishedInitialising ->
                    { state with
                        TypeInitTable =
                            state.TypeInitTable
                            |> TypeInitTable.markInitialised currentThread finishedInitialising
                    }

            // Return to previous stack frame
            let state =
                { state with
                    ThreadState =
                        state.ThreadState
                        |> Map.add
                            currentThread
                            { threadStateAtEndOfMethod with
                                MethodState = returnState.JumpTo
                            }
                }

            match threadStateAtEndOfMethod.MethodState.EvaluationStack.Values with
            | [] ->
                // no return value
                (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
            | [ retVal ] ->
                let retType =
                    threadStateAtEndOfMethod.MethodState.ExecutingMethod.Signature.ReturnType

                state
                |> IlMachineState.pushToStackCoerced retVal retType currentThread
                |> Tuple.withRight WhatWeDid.Executed
                |> ExecutionResult.Stepped
            | vals ->
                failwith
                    "Unexpected interpretation result has a local evaluation stack with more than one element on RET"

        | LdcI4_0 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 0)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_1 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_2 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 2)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_3 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 3)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_4 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 4)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_5 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 5)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_6 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 6)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_7 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 7)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_8 ->
            state
            |> IlMachineState.pushToEvalStack (CliObject.Basic (BasicCliObject.Int32 8)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_m1 -> failwith "todo"
        | LdNull -> failwith "todo"
        | Ceq -> failwith "todo"
        | Cgt -> failwith "todo"
        | Cgt_un -> failwith "todo"
        | Clt -> failwith "todo"
        | Clt_un -> failwith "todo"
        | Stloc_0 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 0
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_1 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 1
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_2 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 2
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | Stloc_3 ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread 3
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
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
        | Cpblk -> failwith "todo"
        | Initblk -> failwith "todo"
        | Conv_ovf_u1 -> failwith "todo"
        | Conv_ovf_u2 -> failwith "todo"
        | Conv_ovf_u4 -> failwith "todo"
        | Conv_ovf_u8 -> failwith "todo"
        | Conv_ovf_i1 -> failwith "todo"
        | Conv_ovf_i2 -> failwith "todo"
        | Conv_ovf_i4 -> failwith "todo"
        | Conv_ovf_i8 -> failwith "todo"
        | Break -> failwith "todo"
        | Conv_r_un -> failwith "todo"
        | Arglist -> failwith "todo"
        | Ckfinite -> failwith "todo"
        | Readonly -> failwith "todo"
        | Refanytype -> failwith "todo"

    let private resolveMember
        (loggerFactory : ILoggerFactory)
        (assy : DumpedAssembly)
        (m : MemberReferenceHandle)
        (state : IlMachineState)
        : IlMachineState * AssemblyName * WoofWare.PawPrint.MethodInfo
        =
        // TODO: do we need to initialise the parent class here?
        let mem = assy.Members.[m]

        let memberSig =
            match mem.Signature with
            | MemberSignature.Field _ -> failwith "tried to resolveMember on a field; not yet implemented"
            | MemberSignature.Method method -> method

        let memberName : string = assy.Strings mem.Name

        let state, assy, targetType =
            match mem.Parent with
            | MetadataToken.TypeReference parent -> IlMachineState.resolveType loggerFactory parent assy state
            | MetadataToken.TypeSpecification parent ->
                IlMachineState.resolveTypeFromSpec loggerFactory parent assy state
            | parent -> failwith $"Unexpected: {parent}"

        let availableMethods =
            targetType.Methods
            |> List.filter (fun mi -> mi.Name = memberName)
            |> List.filter (fun mi -> mi.Signature = memberSig)

        let method =
            match availableMethods with
            | [] ->
                failwith
                    $"Could not find member {memberName} with the right signature on {targetType.Namespace}.{targetType.Name}"
            | [ x ] -> x
            | _ ->
                failwith
                    $"Multiple overloads matching signature for call to {targetType.Namespace}.{targetType.Name}'s {memberName}!"

        state, assy.Name, method

    let private executeUnaryMetadata
        (loggerFactory : ILoggerFactory)
        (op : UnaryMetadataTokenIlOp)
        (metadataToken : MetadataToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | Call ->
            // TODO: make an abstraction for "call this method" that wraps up all the `loadClass` stuff too
            let state, methodToCall =
                match metadataToken with
                | MetadataToken.MethodSpecification h ->
                    // TODO: do we need to initialise the parent class here?
                    let spec = (state.ActiveAssembly thread).MethodSpecs.[h]

                    match spec.Method with
                    | MetadataToken.MethodDef token -> state, (state.ActiveAssembly thread).Methods.[token]
                    | k -> failwith $"Unrecognised kind: %O{k}"
                | MetadataToken.MemberReference h ->
                    let state, _, method =
                        resolveMember loggerFactory (state.ActiveAssembly thread) h state

                    state, method
                | MetadataToken.MethodDef defn -> state, (state.ActiveAssembly thread).Methods.[defn]
                | k -> failwith $"Unrecognised kind: %O{k}"

            state.WithThreadSwitchedToAssembly (snd methodToCall.DeclaringType) thread
            |> fst
            |> IlMachineState.callMethodInActiveAssembly loggerFactory thread methodToCall

        | Callvirt -> failwith "todo"
        | Castclass -> failwith "todo"
        | Newobj ->
            // TODO: allocate the object, and pass it as the first argument to the constructor. Check the rest of what
            // newobj is supposed to do, and do it.
            let state, assy, ctor =
                match metadataToken with
                | MethodDef md ->
                    let activeAssy = state.ActiveAssembly thread
                    let method = activeAssy.Methods.[md]
                    state, activeAssy.Name, method
                | MemberReference mr -> resolveMember loggerFactory (state.ActiveAssembly thread) mr state
                | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

            state.WithThreadSwitchedToAssembly assy thread
            |> fst
            |> IlMachineState.callMethodInActiveAssembly loggerFactory thread ctor
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
        | Ldsflda ->
            // TODO: check whether we should throw FieldAccessException
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to load a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: throw MissingFieldException"
            | true, field ->
                match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
                | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
                | NothingToDo state ->

                if TypeDefn.isManaged field.Signature then
                    match state.Statics.TryGetValue ((field.DeclaringType, activeAssy.Name)) with
                    | true, v ->
                        IlMachineState.pushToEvalStack
                            (CliObject.Basic (BasicCliObject.PointerType (Some v)))
                            thread
                            state
                        |> IlMachineState.advanceProgramCounter thread
                        |> Tuple.withRight WhatWeDid.Executed
                    | false, _ ->
                        let allocation, state = state |> IlMachineState.allocate (failwith "")

                        state
                        |> IlMachineState.pushToEvalStack
                            (CliObject.Basic (BasicCliObject.PointerType (Some allocation)))
                            thread
                        |> Tuple.withRight WhatWeDid.Executed
                else
                    failwith "TODO: push unmanaged pointer"
        | Ldftn -> failwith "todo"
        | Stobj -> failwith "todo"
        | Constrained -> failwith "todo"
        | Ldtoken -> failwith "todo"
        | Cpobj -> failwith "todo"
        | Ldobj -> failwith "todo"
        | Sizeof -> failwith "todo"
        | Calli -> failwith "todo"
        | Unbox -> failwith "todo"
        | Ldvirtftn -> failwith "todo"
        | Mkrefany -> failwith "todo"
        | Refanyval -> failwith "todo"
        | Jmp -> failwith "todo"

    let private executeUnaryStringToken
        (op : UnaryStringTokenIlOp)
        (sh : StringToken)
        (state : IlMachineState)
        (thread : ThreadId)
        : IlMachineState * WhatWeDid
        =
        match op with
        | UnaryStringTokenIlOp.Ldstr ->
            let addressToLoad, state =
                match state.InternedStrings.TryGetValue sh with
                | false, _ ->
                    let toAllocate = state.ActiveAssembly(thread).Strings sh
                    let addr, state = IlMachineState.allocate (ReferenceType.String toAllocate) state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (sh, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack
                    (CliObject.Basic (BasicCliObject.ObjectReference (Some addressToLoad)))
                    thread
                    state

            state
            |> IlMachineState.advanceProgramCounter thread
            |> Tuple.withRight WhatWeDid.Executed

    let private executeUnaryConst
        (state : IlMachineState)
        (currentThread : ThreadId)
        (op : UnaryConstIlOp)
        : IlMachineState * WhatWeDid
        =
        match op with
        | Stloc s ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread (int s)
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Stloc_s b ->
            state
            |> IlMachineState.popFromStackToLocalVariable currentThread (int b)
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_I8 int64 -> failwith "todo"
        | Ldc_I4 i -> failwith "todo"
        | Ldc_R4 f -> failwith "todo"
        | Ldc_R8 f -> failwith "todo"
        | Ldc_I4_s b -> failwith "todo"
        | Br i -> failwith "todo"
        | Br_s b ->
            state
            |> IlMachineState.advanceProgramCounter currentThread
            |> IlMachineState.jumpProgramCounter currentThread (int b)
            |> Tuple.withRight WhatWeDid.Executed
        | Brfalse_s b -> failwith "todo"
        | Brtrue_s b -> failwith "todo"
        | Brfalse i -> failwith "todo"
        | Brtrue i -> failwith "todo"
        | Beq_s b -> failwith "todo"
        | Blt_s b -> failwith "todo"
        | Ble_s b -> failwith "todo"
        | Bgt_s b -> failwith "todo"
        | Bge_s b -> failwith "todo"
        | Beq i -> failwith "todo"
        | Blt i -> failwith "todo"
        | Ble i -> failwith "todo"
        | Bgt i -> failwith "todo"
        | Bge i -> failwith "todo"
        | Bne_un_s b -> failwith "todo"
        | Bge_un_s b -> failwith "todo"
        | Bgt_un_s b -> failwith "todo"
        | Ble_un_s b -> failwith "todo"
        | Blt_un_s b -> failwith "todo"
        | Bne_un i -> failwith "todo"
        | Bge_un i -> failwith "todo"
        | Bgt_un i -> failwith "todo"
        | Ble_un i -> failwith "todo"
        | Blt_un i -> failwith "todo"
        | Ldloc_s b -> failwith "todo"
        | Ldloca_s b -> failwith "todo"
        | Ldarga s -> failwith "todo"
        | Ldarg_s b -> failwith "todo"
        | Ldarga_s b -> failwith "todo"
        | Leave i -> failwith "todo"
        | Leave_s b -> failwith "todo"
        | Starg_s b -> failwith "todo"
        | Starg s -> failwith "todo"
        | Unaligned b -> failwith "todo"
        | Ldloc s -> failwith "todo"
        | Ldloca s -> failwith "todo"
        | Ldarg s -> failwith "todo"

    let executeOneStep (loggerFactory : ILoggerFactory) (state : IlMachineState) (thread : ThreadId) : ExecutionResult =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ -> failwith "Wanted to execute a nonexistent instruction"
        | true, executingInstruction ->

        logger.LogInformation (
            "Executing one step (index {ExecutingIlOpIndex} in method {ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            instruction.ExecutingMethod.Name,
            executingInstruction
        )

        match instruction.ExecutingMethod.Locations.[instruction.IlOpIndex] with
        | IlOp.Nullary op -> executeNullary state thread op
        | IlOp.UnaryConst unaryConstIlOp -> executeUnaryConst state thread unaryConstIlOp |> ExecutionResult.Stepped
        | IlOp.UnaryMetadataToken (unaryMetadataTokenIlOp, bytes) ->
            executeUnaryMetadata loggerFactory unaryMetadataTokenIlOp bytes state thread
            |> ExecutionResult.Stepped
        | IlOp.Switch immutableArray -> failwith "todo"
        | IlOp.UnaryStringToken (unaryStringTokenIlOp, stringHandle) ->
            executeUnaryStringToken unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
