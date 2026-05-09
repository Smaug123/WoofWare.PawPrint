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

    let qCallAssemblyToAssemblyFullName (operation : string) (state : IlMachineState) (arg : CliType) : string =
        match arg with
        | CliType.ValueType vt ->
            let assemblyField =
                IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_assembly"

            match
                CliValueType.DereferenceFieldById assemblyField vt
                |> CliType.unwrapPrimitiveLike
            with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.AssemblyHandle assemblyFullName)) ->
                assemblyFullName
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
                // QCallAssembly is a value type; CoreLib represents a null
                // assembly by storing IntPtr.Zero in this field.
                failwith $"TODO: %s{operation} refuses to dereference null QCallAssembly._assembly IntPtr"
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

    /// Decode a `QCallModule` value-type argument to the assembly full name of the wrapped
    /// `RuntimeModule`. CoreCLR's `QCallModule` carries `(_ptr, _module)` where `_module` is
    /// the result of `RuntimeModule.GetUnderlyingNativeHandle()` — i.e. `m_pData`, which we
    /// represent as `NativeIntSource.ModuleHandle`.
    let qCallModuleToAssemblyFullName (operation : string) (state : IlMachineState) (arg : EvalStackValue) : string =
        match arg with
        | EvalStackValue.UserDefinedValueType vt ->
            let moduleField =
                IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_module"

            match CliValueType.DereferenceFieldById moduleField vt |> CliType.unwrapPrimitiveLike with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ModuleHandle assemblyFullName)) ->
                assemblyFullName
            | other -> failwith $"%s{operation}: expected ModuleHandle in QCallModule._module, got %O{other}"
        | other -> failwith $"%s{operation}: expected QCallModule value type, got %O{other}"

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
        | RuntimeTypeHandleTarget.GenericParameter _ ->
            failwith
                $"%s{operation}: expected closed RuntimeTypeHandleTarget in QCallTypeHandle._handle, but got generic parameter"

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

    let requiredCharConcreteType
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
                    // Defensive PawPrint bound against scanning guest memory
                    // forever for unterminated strings. This is not a CLR
                    // resource-name limit.
                    failwith $"%s{operation}: unterminated UTF-16 string exceeded PawPrint's 32767-char scan limit"

                let c = readUtf16Char operation baseClassTypes state charConcreteType ptr charIndex

                if c = char 0 then
                    chars |> List.rev |> Array.ofList |> System.String
                else
                    loop (charIndex + 1) (c :: chars)

            loop 0 []

    let private requiredByteConcreteType
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        : ConcreteType<ConcreteTypeHandle>
        =
        let handle =
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes baseClassTypes.Byte.Identity
            |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Byte is not concretized")

        AllConcreteTypes.lookup handle state.ConcreteTypes
        |> Option.defaultWith (fun () -> failwith $"%s{operation}: concrete System.Byte handle %O{handle} not found")

    let private readUtf8Byte
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (byteConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (byteIndex : int)
        : byte
        =
        let ptr =
            ManagedPointerByteView.addByteOffset baseClassTypes state byteConcreteType byteIndex ptr

        match IlMachineState.readManagedByrefBytesAs state ptr (CliType.Numeric (CliNumericType.UInt8 0uy)) with
        | CliType.Numeric (CliNumericType.UInt8 b) -> b
        | other -> failwith $"%s{operation}: UTF-8 byte read returned non-byte value %O{other}"

    let readNullTerminatedUtf8
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : string
        =
        match ptr with
        | ManagedPointerSource.Null ->
            failwith $"TODO: %s{operation} with null UTF-8 pointer should throw ArgumentNullException"
        | ManagedPointerSource.Byref _ ->
            let byteConcreteType = requiredByteConcreteType operation baseClassTypes state

            let rec loop (byteIndex : int) (bytes : byte list) : string =
                if byteIndex > 65535 then
                    // Defensive PawPrint bound against scanning guest memory
                    // forever for unterminated strings.
                    failwith $"%s{operation}: unterminated UTF-8 string exceeded PawPrint's 65535-byte scan limit"

                let b = readUtf8Byte operation baseClassTypes state byteConcreteType ptr byteIndex

                if b = 0uy then
                    bytes |> List.rev |> Array.ofList |> System.Text.Encoding.UTF8.GetString
                else
                    loop (byteIndex + 1) (b :: bytes)

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
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : System.Reflection.AssemblyName
        =
        match typeHandleTarget with
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> identity.Assembly
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, _) ->
            // A generic parameter belongs to the same assembly as its declaring type.
            declaringType.Assembly
        | RuntimeTypeHandleTarget.Closed concreteTypeHandle ->
            // Unwrap Byref/Pointer/Array to reach the element type's Concrete handle.
            // In .NET, typeof(T[]).Assembly == typeof(T).Assembly, so arrays follow the
            // same rule: return the element type's assembly.
            //
            // Function-pointer TypeDescs have no underlying element type. CoreCLR's
            // ComputeLoaderModuleWorker (coreclr/vm/clsload.cpp) walks the type list
            // [retType; arg1; arg2; ...] and assigns the loader module on the first
            // iteration only — the return type's loader module wins; later params
            // never overwrite it. Mirror that here: void return → corelib (since void
            // lives in corelib); concrete return → that type's assembly.
            let corelib = baseClassTypes.Corelib.Name

            let rec assemblyOf (h : ConcreteTypeHandle) : System.Reflection.AssemblyName =
                match h with
                | ConcreteTypeHandle.Concrete _ ->
                    AllConcreteTypes.lookup h state.ConcreteTypes
                    |> Option.map (fun ct -> ct.Assembly)
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: could not find concrete type for handle %O{h}"
                    )
                | ConcreteTypeHandle.Byref inner
                | ConcreteTypeHandle.Pointer inner
                | ConcreteTypeHandle.OneDimArrayZero inner
                | ConcreteTypeHandle.Array (inner, _) -> assemblyOf inner
                | ConcreteTypeHandle.FunctionPointer signature ->
                    match signature.ReturnType with
                    | ConcreteFunctionPointerReturnType.Void -> corelib
                    | ConcreteFunctionPointerReturnType.Returns ret ->
                        // Custom modifiers do not change the loader module; flatten to a plain
                        // handle and walk it the same way we walk the rest of the type tree.
                        assemblyOf (ConcreteSignatureType.toHandle ret.UnderlyingType)

            assemblyOf concreteTypeHandle

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
                | ConcreteTypeHandle.FunctionPointer sg ->
                    let rec formatSigType (s : ConcreteSignatureType) : string =
                        match s with
                        | ConcreteSignatureType.Concrete h -> formatTypeHandle h
                        | ConcreteSignatureType.Byref e -> $"&({formatWithMods e})"
                        | ConcreteSignatureType.Pointer e -> $"*({formatWithMods e})"
                        | ConcreteSignatureType.OneDimArrayZero e -> $"{formatWithMods e}[]"
                        | ConcreteSignatureType.Array (e, rank) ->
                            let dims = if rank <= 1 then "*" else String.replicate (rank - 1) ","
                            $"{formatWithMods e}[{dims}]"
                        | ConcreteSignatureType.FunctionPointer fp ->
                            formatTypeHandle (ConcreteTypeHandle.FunctionPointer fp)

                    and formatWithMods (wm : ConcreteTypeWithModifiers) : string =
                        let mods =
                            wm.Modifiers
                            |> List.map (fun (modHandle, isReq) ->
                                let kw = if isReq then "modreq" else "modopt"
                                $"{kw}({formatTypeHandle modHandle})"
                            )

                        match mods with
                        | [] -> formatSigType wm.UnderlyingType
                        | _ -> formatSigType wm.UnderlyingType + " " + String.concat " " mods

                    let parameters = sg.ParameterTypes |> Seq.map formatWithMods |> String.concat ","

                    let ret =
                        match sg.ReturnType with
                        | ConcreteFunctionPointerReturnType.Void -> "void"
                        | ConcreteFunctionPointerReturnType.Returns retType -> formatWithMods retType

                    $"delegate*<{parameters}->{ret}>"
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
