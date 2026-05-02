namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging
open WoofWare.PawPrint.ExternImplementations

type NativeCallContext =
    {
        LoggerFactory : ILoggerFactory
        Implementations : ISystem_Environment_Env
        BaseClassTypes : BaseClassTypes<DumpedAssembly>
        Thread : ThreadId
        State : IlMachineState
        Instruction : MethodState
        TargetAssembly : DumpedAssembly
        TargetType : TypeInfo<GenericParamFromMetadata, TypeDefn>
    }

[<RequireQualifiedAccess>]
module NativeCall =
    let tryQCallEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.NativeImport with
        | Some import when import.ModuleName = "QCall" -> Some import.EntryPointName
        | _ -> None

    let qCallAssemblyToAssemblyFullName (operation : string) (state : IlMachineState) (arg : EvalStackValue) : string =
        match arg with
        | EvalStackValue.UserDefinedValueType vt ->
            let assemblyField =
                IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_assembly"

            match
                CliValueType.DereferenceFieldById assemblyField vt
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)) ->
                assemblyFullName
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) ->
                failwith $"TODO: %s{operation} with null QCallAssembly should throw ArgumentNullException"
            | other -> failwith $"%s{operation}: expected AssemblyHandle in QCallAssembly._assembly, got %O{other}"
        | other -> failwith $"%s{operation}: expected QCallAssembly value type, got %O{other}"

    let qCallTypeHandleToRuntimeTypeHandleTarget
        (operation : string)
        (state : IlMachineState)
        (arg : EvalStackValue)
        : RuntimeTypeHandleTarget
        =
        match arg with
        | EvalStackValue.UserDefinedValueType vt ->
            let handleField =
                IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_handle"

            match CliValueType.DereferenceFieldById handleField vt |> CliType.unwrapPrimitiveLike with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
            | other -> failwith $"%s{operation}: expected TypeHandlePtr in QCallTypeHandle._handle, got %O{other}"
        | other -> failwith $"%s{operation}: expected QCallTypeHandle value type, got %O{other}"

    let qCallTypeHandleToConcreteTypeHandle
        (operation : string)
        (state : IlMachineState)
        (arg : EvalStackValue)
        : ConcreteTypeHandle
        =
        match qCallTypeHandleToRuntimeTypeHandleTarget operation state arg with
        | RuntimeTypeHandleTarget.Closed cth -> cth
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
            failwith
                $"%s{operation}: expected closed RuntimeTypeHandleTarget in QCallTypeHandle._handle, but got open generic"

    let gcHandleKindOfEvalStackValue (operation : string) (arg : EvalStackValue) : GcHandleKind =
        let value =
            match arg with
            | EvalStackValue.Int32 i -> i
            | other -> failwith $"%s{operation}: expected GCHandleType enum as int32, got %O{other}"

        match value with
        | 0 -> GcHandleKind.Weak
        | 1 -> GcHandleKind.WeakTrackResurrection
        | 2 -> GcHandleKind.Normal
        | 3 -> GcHandleKind.Pinned
        | 6 -> GcHandleKind.Dependent
        | other -> failwith $"%s{operation}: unsupported GC handle kind %i{other}"

    let objectTargetOfEvalStackValue (operation : string) (arg : EvalStackValue) : ManagedHeapAddress option =
        match arg with
        | EvalStackValue.NullObjectRef -> None
        | EvalStackValue.ObjectRef addr -> Some addr
        | other -> failwith $"%s{operation}: expected object reference, got %O{other}"

    let gcHandleAddressOfEvalStackValue (operation : string) (arg : EvalStackValue) : GcHandleAddress =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr handle) -> handle
        | other -> failwith $"%s{operation}: expected GC handle pointer, got %O{other}"

    let pushGcHandleAddress (handle : GcHandleAddress) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr handle)) thread state

    let pushObjectTarget
        (target : ManagedHeapAddress option)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        IlMachineState.pushToEvalStack (CliType.ObjectRef target) thread state

    let cliUInt32 (value : uint32) : CliType =
        // PawPrint models CLI UInt32 as the same 4-byte stack/storage cell as
        // Int32 while preserving the low 32 bits; see PrimitiveType.UInt32.
        CliType.Numeric (CliNumericType.Int32 (int32 value))

    let int32Argument (operation : string) (arg : CliType) : int =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int32 i) -> i
        | other -> failwith $"%s{operation}: expected Int32 argument, got %O{other}"

    let fieldHandleIdOfRuntimeFieldHandleInternal (operation : string) (arg : CliType) : int64 option =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> Some id
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr id)) -> Some id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | other ->
            failwith
                $"%s{operation}: expected RuntimeFieldHandleInternal containing a field-registry handle, got %O{other}"

    let managedPointerOfPointerArgument (operation : string) (argName : string) (arg : CliType) : ManagedPointerSource =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.Managed ptr) -> ptr
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> ManagedPointerSource.Null
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ptr)) -> ptr
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> ManagedPointerSource.Null
        | other -> failwith $"%s{operation}: expected %s{argName} to be a managed pointer argument, got %O{other}"

    let private requiredCharConcreteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Char.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Char is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Char handle %O{handle} not found")

    let private readUtf16Char
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (charConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (charIndex : int)
        : char
        =
        let ptr =
            ManagedPointerByteView.addByteOffset baseClassTypes state charConcreteType (charIndex * 2) ptr

        match IlMachineState.readManagedByrefBytesAs state ptr (CliType.ofChar (char 0)) with
        | CliType.Char (high, low) -> char (int high * 256 + int low)
        | other -> failwith $"%s{operation}: UTF-16 char read returned non-char value %O{other}"

    let readNullTerminatedUtf16
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : string
        =
        match ptr with
        | ManagedPointerSource.Null ->
            failwith $"TODO: %s{operation} with null UTF-16 pointer should throw ArgumentNullException"
        | ManagedPointerSource.Byref _ ->
            let charConcreteType = requiredCharConcreteType operation baseClassTypes state

            let rec loop (charIndex : int) (chars : char list) : string =
                if charIndex > 32767 then
                    failwith $"%s{operation}: unterminated UTF-16 string exceeded PawPrint's 32767-char scan limit"

                let c = readUtf16Char operation baseClassTypes state charConcreteType ptr charIndex

                if c = char 0 then
                    chars |> List.rev |> Array.ofList |> System.String
                else
                    loop (charIndex + 1) (c :: chars)

            loop 0 []

    let stringHandleOnStackTarget
        (operation : string)
        (state : IlMachineState)
        (argName : string)
        (arg : CliType)
        : ManagedPointerSource
        =
        match arg with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_ptr"

            let ptrValue = CliValueType.DereferenceFieldById ptrField vt
            managedPointerOfPointerArgument operation $"{argName}._ptr" ptrValue
        | other -> failwith $"%s{operation}: expected %s{argName} to be StringHandleOnStack, got %O{other}"

    let objectHandleOnStackTarget
        (operation : string)
        (state : IlMachineState)
        (argName : string)
        (arg : CliType)
        : ManagedPointerSource
        =
        match arg with
        | CliType.ValueType vt ->
            let ptrField = IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_ptr"

            let ptrValue = CliValueType.DereferenceFieldById ptrField vt
            managedPointerOfPointerArgument operation $"{argName}._ptr" ptrValue
        | other -> failwith $"%s{operation}: expected %s{argName} to be ObjectHandleOnStack, got %O{other}"

    let methodTableOfEvalStackValue (operation : string) (arg : EvalStackValue) : ConcreteTypeHandle =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed typeHandle))
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr typeHandle) -> typeHandle
        | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity)) ->
            failwith $"%s{operation}: expected closed MethodTable pointer argument, got open generic %O{identity}"
        | other -> failwith $"%s{operation}: expected MethodTable pointer argument, got %O{other}"

    let runtimeTypeHandleTargetOfRuntimeTypeRef
        (operation : string)
        (state : IlMachineState)
        (runtimeTypeRef : EvalStackValue)
        : RuntimeTypeHandleTarget
        =
        let runtimeTypeAddr =
            match runtimeTypeRef with
            | EvalStackValue.ObjectRef addr -> addr
            | other -> failwith $"%s{operation}: expected ObjectRef for RuntimeType argument, got %O{other}"

        let heapObj = ManagedHeap.get runtimeTypeAddr state.ManagedHeap

        // RuntimeType.m_handle is typed as IntPtr (primitive-like); unwrap to reach the inner NativeInt.
        let handleField =
            IlMachineState.requiredOwnInstanceFieldId state heapObj.ConcreteType "m_handle"

        match
            AllocatedNonArrayObject.DereferenceFieldById handleField heapObj
            |> CliType.unwrapPrimitiveLike
        with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> target
        | other -> failwith $"%s{operation}: expected TypeHandlePtr in RuntimeType.m_handle, got %O{other}"

    let typeAssemblyName
        (operation : string)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : System.Reflection.AssemblyName
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> identity.Assembly
        | RuntimeTypeHandleTarget.Closed concreteTypeHandle ->
            // Unwrap Byref/Pointer/Array to reach the element type's Concrete handle.
            // In .NET, typeof(T[]).Assembly == typeof(T).Assembly, so arrays follow the
            // same rule: return the element type's assembly.
            let rec unwrapToConcreteHandle (h : ConcreteTypeHandle) : ConcreteTypeHandle =
                match h with
                | ConcreteTypeHandle.Concrete _ -> h
                | ConcreteTypeHandle.Byref inner -> unwrapToConcreteHandle inner
                | ConcreteTypeHandle.Pointer inner -> unwrapToConcreteHandle inner
                | ConcreteTypeHandle.OneDimArrayZero inner -> unwrapToConcreteHandle inner
                | ConcreteTypeHandle.Array (inner, _) -> unwrapToConcreteHandle inner

            let concreteHandle = unwrapToConcreteHandle concreteTypeHandle

            let concreteType =
                AllConcreteTypes.lookup concreteHandle state.ConcreteTypes
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: could not find concrete type for handle %O{concreteTypeHandle} (unwrapped to %O{concreteHandle})"
                )

            concreteType.Assembly

    let failUnimplemented (ctx : NativeCallContext) : ExecutionResult =
        let instruction = ctx.Instruction
        let state = ctx.State

        let implKind =
            if instruction.ExecutingMethod.IsCliInternal then
                "InternalCall"
            elif instruction.ExecutingMethod.IsPinvokeImpl then
                match instruction.ExecutingMethod.NativeImport with
                | Some import -> $"PInvokeImpl %s{import.ModuleName}!%s{import.EntryPointName}"
                | None -> "PInvokeImpl"
            elif instruction.ExecutingMethod.ImplAttributes.HasFlag System.Reflection.MethodImplAttributes.Runtime then
                "Runtime"
            else
                $"Unknown (ImplAttributes=%O{instruction.ExecutingMethod.ImplAttributes})"

        let rec formatTypeHandle (cth : ConcreteTypeHandle) : string =
            match AllConcreteTypes.lookup cth state.ConcreteTypes with
            | Some ct -> $"{ct.Namespace}.{ct.Name}"
            | None ->
                match cth with
                | ConcreteTypeHandle.Byref inner -> $"&({formatTypeHandle inner})"
                | ConcreteTypeHandle.Pointer inner -> $"*({formatTypeHandle inner})"
                | ConcreteTypeHandle.OneDimArrayZero inner -> $"{formatTypeHandle inner}[]"
                | ConcreteTypeHandle.Array (inner, rank) ->
                    let dims = if rank <= 1 then "*" else String.replicate (rank - 1) ","
                    $"{formatTypeHandle inner}[{dims}]"
                | ConcreteTypeHandle.Concrete i -> string i

        let paramStr =
            instruction.ExecutingMethod.Signature.ParameterTypes
            |> Seq.map formatTypeHandle
            |> String.concat ", "

        let retStr =
            match instruction.ExecutingMethod.Signature.ReturnType with
            | MethodReturnType.Void -> "void"
            | MethodReturnType.Returns retType -> formatTypeHandle retType

        failwith
            $"Unimplemented native method (%s{implKind}): %s{ctx.TargetAssembly.Name.Name} %s{ctx.TargetType.Namespace}.%s{ctx.TargetType.Name}::%s{instruction.ExecutingMethod.Name}(%s{paramStr}) -> %s{retStr}. Add a mock implementation in ExternImplementations."
