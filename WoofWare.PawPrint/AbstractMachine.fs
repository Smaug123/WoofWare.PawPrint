namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging
open Microsoft.FSharp.Core

type FieldSlot =
    {
        FieldName : string
        FieldSize : int
    }

type ManagedObject =
    {
        Fields : (string * CliType) list
        SyncBlock : unit
    }

type ReferenceType =
    | String of string
    | ManagedObject of fields : FieldSlot list
    | Array of len : int * containedType : Type

and Type =
    | ReferenceType of ReferenceType
    | ValueType

[<RequireQualifiedAccess>]
module Type =
    let rec sizeOnHeapRef (r : ReferenceType) : int =
        match r with
        | ReferenceType.String s ->
            // UTF-16, so two bytes per char
            2 * s.Length
        | ReferenceType.ManagedObject fields -> fields |> Seq.sumBy (fun slot -> slot.FieldSize)
        | ReferenceType.Array (len, ty) -> sizeOf ty * len + 4 // for the len

    and sizeOf (t : Type) : int =
        match t with
        | ReferenceType t -> sizeOnHeapRef t
        | ValueType -> failwith "todo"

    let sizeOfTypeDefn (assy : DumpedAssembly) (t : WoofWare.PawPrint.TypeDefn) : int =
        match t with
        | TypeDefn.PrimitiveType prim ->
            match prim with
            | PrimitiveType.Void -> failwith "todo"
            | PrimitiveType.Boolean -> failwith "todo"
            | PrimitiveType.Char -> failwith "todo"
            | PrimitiveType.SByte -> failwith "todo"
            | PrimitiveType.Byte -> failwith "todo"
            | PrimitiveType.Int16 -> failwith "todo"
            | PrimitiveType.UInt16 -> failwith "todo"
            | PrimitiveType.Int32 -> 4
            | PrimitiveType.UInt32 -> failwith "todo"
            | PrimitiveType.Int64 -> failwith "todo"
            | PrimitiveType.UInt64 -> failwith "todo"
            | PrimitiveType.Single -> failwith "todo"
            | PrimitiveType.Double -> failwith "todo"
            | PrimitiveType.String -> failwith "todo"
            | PrimitiveType.TypedReference -> failwith "todo"
            | PrimitiveType.IntPtr -> failwith "todo"
            | PrimitiveType.UIntPtr -> failwith "todo"
            | PrimitiveType.Object -> failwith "todo"
        | TypeDefn.FromDefinition (handle, kind) ->
            match kind with
            | SignatureTypeKind.Unknown -> failwith "todo"
            | SignatureTypeKind.Class -> 8
            | SignatureTypeKind.ValueType ->
                let ty = assy.TypeDefs.[handle]
                failwith $"TODO: %O{ty}"
            | s -> raise (System.ArgumentOutOfRangeException ())
        | _ -> failwith $"oh no: %O{t}"

    let ofTypeInfo (assy : DumpedAssembly) (t : WoofWare.PawPrint.TypeInfo) : Type =
        // TODO: is value type?
        t.Fields
        |> List.map (fun field ->
            {
                FieldName = field.Name
                FieldSize = sizeOfTypeDefn assy field.Signature
            }
        )
        |> ReferenceType.ManagedObject
        |> Type.ReferenceType

type MethodReturnState =
    {
        JumpTo : MethodState
        WasInitialisingType : (TypeDefinitionHandle * AssemblyName) option
    }

and MethodState =
    {
        // TODO: local variables are initialised to 0 if the localsinit flag is set for the method
        LocalVariables : CliType ImmutableArray
        /// Index into the stream of IL bytes.
        IlOpIndex : int
        EvaluationStack : EvalStack
        Arguments : CliType ImmutableArray
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

    static member popFromStackToVariable (localVariableIndex : int) (state : MethodState) : MethodState =
        if localVariableIndex >= state.LocalVariables.Length then
            failwith
                $"Tried to access zero-indexed local variable %i{localVariableIndex} but only %i{state.LocalVariables.Length} exist"

        if localVariableIndex < 0 || localVariableIndex >= 65535 then
            failwith $"Incorrect CIL encountered: local variable index has value %i{localVariableIndex}"

        let popped, newStack = EvalStack.Pop state.EvaluationStack

        let desiredValue =
            match state.LocalVariables.[localVariableIndex] with
            | CliType.Numeric numeric ->
                match numeric with
                | CliNumericType.Int32 _ ->
                    match popped with
                    | EvalStackValue.Int32 i -> CliType.Numeric (CliNumericType.Int32 i)
                    | i -> failwith $"TODO: %O{i}"
                | CliNumericType.Int64 int64 -> failwith "todo"
                | CliNumericType.NativeInt int64 -> failwith "todo"
                | CliNumericType.NativeFloat f -> failwith "todo"
                | CliNumericType.Int8 b -> failwith "todo"
                | CliNumericType.Int16 s -> failwith "todo"
                | CliNumericType.UInt8 b -> failwith "todo"
                | CliNumericType.UInt16 s -> failwith "todo"
                | CliNumericType.Float32 f -> failwith "todo"
                | CliNumericType.Float64 f -> failwith "todo"
            | CliType.ObjectRef _ ->
                match popped with
                | EvalStackValue.ManagedPointer addr -> CliType.ObjectRef addr
                | i -> failwith $"TODO: %O{i}"
            | CliType.Bool _ ->
                match popped with
                | EvalStackValue.Int32 i ->
                    // Bools are zero-extended
                    CliType.Bool (i % 256 |> byte)
                | i -> failwith $"TODO: %O{i}"
            | i -> failwith $"TODO: %O{i}"

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
                    | PrimitiveType.Boolean -> CliType.Bool 0uy
                    | PrimitiveType.Char -> failwith "todo"
                    | PrimitiveType.SByte -> failwith "todo"
                    | PrimitiveType.Byte -> failwith "todo"
                    | PrimitiveType.Int16 -> failwith "todo"
                    | PrimitiveType.UInt16 -> failwith "todo"
                    | PrimitiveType.Int32 -> CliType.Numeric (CliNumericType.Int32 0)
                    | PrimitiveType.UInt32 -> failwith "todo"
                    | PrimitiveType.Int64 -> CliType.Numeric (CliNumericType.Int64 0L)
                    | PrimitiveType.UInt64 -> failwith "todo"
                    | PrimitiveType.Single -> failwith "todo"
                    | PrimitiveType.Double -> failwith "todo"
                    | PrimitiveType.String -> failwith "todo"
                    | PrimitiveType.TypedReference -> failwith "todo"
                    | PrimitiveType.IntPtr -> failwith "todo"
                    | PrimitiveType.UIntPtr -> failwith "todo"
                    | PrimitiveType.Object -> CliType.ObjectRef None
                | TypeDefn.Array (elt, shape) -> failwith "todo"
                | TypeDefn.Pinned typeDefn -> failwith "todo"
                | TypeDefn.Pointer typeDefn -> failwith "todo"
                | TypeDefn.Byref typeDefn -> failwith "todo"
                | TypeDefn.OneDimensionalArrayLowerBoundZero elements -> failwith "todo"
                | TypeDefn.Modified (original, afterMod, modificationRequired) -> failwith "todo"
                | TypeDefn.FromReference _ -> CliType.ObjectRef None
                | TypeDefn.FromDefinition (_, signatureTypeKind) -> failwith "todo"
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

type WhatWeDid =
    | Executed
    /// We didn't run what you wanted, because we have to do class initialisation first.
    | SuspendedForClassInit
    /// We can't proceed until this thread has finished the class initialisation work it's doing.
    | BlockedOnClassInit of threadBlockingUs : ThreadId

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
        Statics : ImmutableDictionary<TypeDefinitionHandle * AssemblyName, CliType>
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
    /// The type is loaded; you can proceed.
    | NothingToDo of IlMachineState
    /// We didn't manage to load the requested type, because that type itself requires first loading something.
    /// The state we give you is ready to load that something.
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
                |> Seq.toList

            match assy with
            | [] -> failwith $"Could not find a readable DLL in any runtime dir with name %s{assemblyName.Name}.dll"
            | _ :: _ :: _ -> failwith $"Found multiple DLLs in runtime dirs with name %s{assemblyName.Name}.dll"
            | [ assy ] ->

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
                                WasInitialisingType = Some (typeDefHandle, assemblyName)
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
                                        WasInitialisingType = None
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
                                    WasInitialisingType = None
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

    // let allocate (o : CliObject) (state : IlMachineState) : ManagedHeapAddress * IlMachineState =
    //     let alloc, heap = ManagedHeap.Allocate o state.ManagedHeap

    //     let state =
    //         { state with
    //             ManagedHeap = heap
    //         }

    //     alloc, state

    let allocateArray
        (elementType : WoofWare.PawPrint.TypeInfo)
        (len : int)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let zeroElement (_ : int) : CliType = failwith "TODO"
        let initialisation = zeroElement |> Seq.init len |> ImmutableArray.CreateRange

        let o : AllocatedArray =
            {
                Length = len
                Elements = initialisation
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.AllocateArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let allocateStringData (len : int) (state : IlMachineState) : int * IlMachineState =
        let addr, heap = state.ManagedHeap |> ManagedHeap.AllocateString len

        let state =
            { state with
                ManagedHeap = heap
            }

        addr, state

    let setStringData (addr : int) (contents : string) (state : IlMachineState) : IlMachineState =
        let heap = ManagedHeap.SetStringData addr contents state.ManagedHeap

        { state with
            ManagedHeap = heap
        }

    let allocateManagedObject
        (typeInfo : WoofWare.PawPrint.TypeInfo)
        (fields : (string * CliType) list)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let o =
            {
                Fields = Map.ofList fields
                Type = typeInfo
            }

        let alloc, heap = state.ManagedHeap |> ManagedHeap.AllocateNonArray o

        let state =
            { state with
                ManagedHeap = heap
            }

        alloc, state

    let pushToEvalStack (o : CliType) (thread : ThreadId) (state : IlMachineState) =
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
            MethodState.popFromStackToVariable localVariableIndex threadState.MethodState

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
        (v : CliType)
        (index : int)
        (state : IlMachineState)
        : IlMachineState
        =
        let heap = ManagedHeap.SetArrayValue arrayAllocation index v state.ManagedHeap

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
                match returnState.WasInitialisingType with
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
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 0)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_1 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 1)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_2 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 2)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_3 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 3)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_4 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 4)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_5 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 5)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_6 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 6)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_7 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 7)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
            |> ExecutionResult.Stepped
        | LdcI4_8 ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 8)) currentThread
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
            // TODO: Pass the allocation as the first argument to the constructor. Check the rest of what
            // newobj is supposed to do, and do it.
            let state, assy, ctor =
                match metadataToken with
                | MethodDef md ->
                    let activeAssy = state.ActiveAssembly thread
                    let method = activeAssy.Methods.[md]
                    state, activeAssy.Name, method
                | MemberReference mr -> resolveMember loggerFactory (state.ActiveAssembly thread) mr state
                | x -> failwith $"Unexpected metadata token for constructor: %O{x}"

            let ctorType, ctorAssembly = ctor.DeclaringType
            let ctorAssembly = state.LoadedAssembly ctorAssembly |> Option.get
            let ctorType = ctorAssembly.TypeDefs.[ctorType]

            let fields =
                ctorType.Fields
                |> List.map (fun field ->
                    let zeroedAllocation =
                        match field.Signature with
                        | TypeDefn.PrimitiveType ty -> failwith "todo"
                        | TypeDefn.Array _ -> failwith "todo"
                        | TypeDefn.Pinned _ -> failwith "todo"
                        | TypeDefn.Pointer _ -> failwith "todo"
                        | TypeDefn.Byref _ -> failwith "todo"
                        | TypeDefn.OneDimensionalArrayLowerBoundZero _ -> failwith "todo"
                        | TypeDefn.Modified _ -> failwith "todo"
                        | TypeDefn.FromReference _ -> failwith "todo"
                        | TypeDefn.FromDefinition _ -> failwith "todo"
                        | TypeDefn.GenericInstantiation _ -> failwith "todo"
                        | TypeDefn.FunctionPointer _ -> failwith "todo"
                        | TypeDefn.GenericTypeParameter _ -> failwith "todo"
                        | TypeDefn.GenericMethodParameter _ -> failwith "todo"

                    field.Name, zeroedAllocation
                )

            let allocatedAddr, state =
                IlMachineState.allocateManagedObject ctorType fields state

            let state =
                state
                |> IlMachineState.pushToEvalStack (CliType.OfManagedObject allocatedAddr) thread

            let state, whatWeDid =
                state.WithThreadSwitchedToAssembly assy thread
                |> fst
                |> IlMachineState.callMethodInActiveAssembly loggerFactory thread ctor

            state, whatWeDid
        | Newarr ->
            let currentState = state.ThreadState.[thread]
            let popped, newStack = EvalStack.Pop currentState.MethodState.EvaluationStack

            let currentState =
                { currentState with
                    ThreadState.MethodState.EvaluationStack = newStack
                }

            let len =
                match popped with
                | EvalStackValue.Int32 v -> v
                | popped -> failwith $"unexpectedly popped value %O{popped} to serve as array len"

            let elementType =
                match metadataToken with
                | MetadataToken.TypeDefinition defn ->
                    state.LoadedAssembly currentState.ActiveAssembly
                    |> Option.get
                    |> fun assy -> assy.TypeDefs.[defn]
                | x -> failwith $"TODO: {x}"

            failwith $"TODO: {elementType.Name}"
        | Box -> failwith "todo"
        | Ldelema -> failwith "todo"
        | Isinst -> failwith "todo"
        | Stfld -> failwith "todo"
        | Stsfld ->
            let fieldHandle =
                match metadataToken with
                | MetadataToken.FieldDefinition f -> f
                | t -> failwith $"Unexpectedly asked to store to a non-field: {t}"

            let activeAssy = state.ActiveAssembly thread

            match activeAssy.Fields.TryGetValue fieldHandle with
            | false, _ -> failwith "TODO: throw MissingFieldException"
            | true, field ->

            match IlMachineState.loadClass loggerFactory field.DeclaringType activeAssy.Name thread state with
            | FirstLoadThis state -> state, WhatWeDid.SuspendedForClassInit
            | NothingToDo state ->

            let popped, evalStack =
                EvalStack.Pop state.ThreadState.[thread].MethodState.EvaluationStack

            let toStore =
                match popped with
                | EvalStackValue.ManagedPointer addr -> CliType.ObjectRef addr
                | _ -> failwith "TODO"

            let newThreadState =
                { state.ThreadState.[thread] with
                    ThreadState.MethodState.EvaluationStack = evalStack
                }

            let state =
                { state with
                    Statics = state.Statics.SetItem ((field.DeclaringType, activeAssy.Name), toStore)
                    ThreadState = state.ThreadState |> Map.add thread newThreadState
                }

            state, WhatWeDid.Executed

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
                        IlMachineState.pushToEvalStack v thread state
                        |> IlMachineState.advanceProgramCounter thread
                        |> Tuple.withRight WhatWeDid.Executed
                    | false, _ ->
                        let allocation, state = state |> (failwith "")

                        state
                        |> IlMachineState.pushToEvalStack (CliType.ObjectRef (Some allocation)) thread
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
        (stringType : WoofWare.PawPrint.TypeInfo)
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
                    let stringToAllocate = state.ActiveAssembly(thread).Strings sh

                    let dataAddr, state =
                        IlMachineState.allocateStringData stringToAllocate.Length state

                    let state = state |> IlMachineState.setStringData dataAddr stringToAllocate

                    let stringInstanceFields =
                        stringType.Fields
                        |> List.choose (fun field ->
                            if int (field.Attributes &&& FieldAttributes.Static) = 0 then
                                Some (field.Name, field.Signature)
                            else
                                None
                        )
                        |> List.sortBy fst
                    // Assert that the string type has the fields we expect
                    if
                        stringInstanceFields
                        <> [
                            ("_firstChar", TypeDefn.PrimitiveType PrimitiveType.Char)
                            ("_stringLength", TypeDefn.PrimitiveType PrimitiveType.Int32)
                        ]
                    then
                        failwith
                            $"unexpectedly don't know how to initialise a string: got fields %O{stringInstanceFields}"

                    let fields =
                        [
                            "_firstChar", CliType.OfChar state.ManagedHeap.StringArrayData.[dataAddr]
                            "_stringLength", CliType.Numeric (CliNumericType.Int32 stringToAllocate.Length)
                        ]

                    let addr, state = IlMachineState.allocateManagedObject stringType fields state

                    addr,
                    { state with
                        InternedStrings = state.InternedStrings.Add (sh, addr)
                    }
                | true, v -> v, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addressToLoad)) thread state

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
        | Ldc_I4 i ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 i)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
        | Ldc_R4 f -> failwith "todo"
        | Ldc_R8 f -> failwith "todo"
        | Ldc_I4_s b ->
            state
            |> IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int8 b)) currentThread
            |> IlMachineState.advanceProgramCounter currentThread
            |> Tuple.withRight WhatWeDid.Executed
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

    let executeOneStep
        (loggerFactory : ILoggerFactory)
        (stringType : WoofWare.PawPrint.TypeInfo)
        (state : IlMachineState)
        (thread : ThreadId)
        : ExecutionResult
        =
        let logger = loggerFactory.CreateLogger typeof<Dummy>.DeclaringType
        let instruction = state.ThreadState.[thread].MethodState

        match instruction.ExecutingMethod.Locations.TryGetValue instruction.IlOpIndex with
        | false, _ -> failwith "Wanted to execute a nonexistent instruction"
        | true, executingInstruction ->

        let executingInType =
            match state.LoadedAssembly (snd instruction.ExecutingMethod.DeclaringType) with
            | None -> "<unloaded assembly>"
            | Some assy ->
                match assy.TypeDefs.TryGetValue (fst instruction.ExecutingMethod.DeclaringType) with
                | true, v -> v.Name
                | false, _ -> "<unrecognised type>"

        logger.LogInformation (
            "Executing one step (index {ExecutingIlOpIndex} in method {ExecutingMethodType}.{ExecutingMethodName}): {ExecutingIlOp}",
            instruction.IlOpIndex,
            executingInType,
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
            executeUnaryStringToken stringType unaryStringTokenIlOp stringHandle state thread
            |> ExecutionResult.Stepped
