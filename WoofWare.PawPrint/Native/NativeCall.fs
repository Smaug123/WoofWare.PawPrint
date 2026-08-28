namespace WoofWare.PawPrint

open Microsoft.Extensions.Logging

type NativeCallContext =
    {
        LoggerFactory : ILoggerFactory
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
        match ctx.Instruction.ExecutingMethod.TryNativeImport with
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

    /// Like <c>qCallTypeHandleToRuntimeTypeHandleTarget</c>, but tolerates the
    /// null-handle case that CoreCLR represents as <c>_handle = IntPtr.Zero</c>
    /// (the encoding produced by passing <c>default(RuntimeTypeHandle)</c> through
    /// the <c>QCallTypeHandle</c> ctor, e.g. when CoreCLR's
    /// <c>RuntimeMethodHandle.IsCAVisibleFromDecoratedType</c> is called without
    /// a decorated source type). Returns <c>None</c> in that case.
    let qCallTypeHandleToRuntimeTypeHandleTargetOption
        (operation : string)
        (state : IlMachineState)
        (arg : EvalStackValue)
        : RuntimeTypeHandleTarget option
        =
        match arg with
        | EvalStackValue.UserDefinedValueType vt ->
            let handleField =
                IlMachineState.requiredOwnInstanceFieldId state vt.Declared "_handle"

            match CliValueType.DereferenceFieldById handleField vt |> CliType.unwrapPrimitiveLike with
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.TypeHandlePtr target)) -> Some target
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
            | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) ->
                None
            | other ->
                failwith $"%s{operation}: expected TypeHandlePtr or null in QCallTypeHandle._handle, got %O{other}"
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
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly ->
            RuntimeTypeHandleTarget.refuseMetadataQuery operation scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed _ as openConstructed ->
            failwith
                $"TODO: open constructed types are not handled at Native/NativeCall.fs:%s{__LINE__}; got %O{openConstructed}"
        | RuntimeTypeHandleTarget.Closed cth -> cth
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _ ->
            failwith
                $"%s{operation}: expected closed RuntimeTypeHandleTarget in QCallTypeHandle._handle, but got open generic"
        | RuntimeTypeHandleTarget.GenericParameter _ ->
            failwith
                $"%s{operation}: expected closed RuntimeTypeHandleTarget in QCallTypeHandle._handle, but got generic parameter"
        | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
            failwith
                $"%s{operation}: expected closed RuntimeTypeHandleTarget in QCallTypeHandle._handle, but got method generic parameter"

    let gcHandleKindOfEvalStackValue (operation : string) (arg : EvalStackValue) : GcHandleKind =
        let value =
            match arg with
            | EvalStackValue.Int32 (Int32Source.Verbatim i) -> i
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

    /// The GC handle entry points take the raw handle, not a tagged view of one:
    /// `GCHandle` strips its pinned marker via `GetHandleValue` and `WeakReference`
    /// strips its tag bits before calling `InternalSet`, so a tagged handle
    /// arriving here means the guest skipped a mask and we must not guess.
    let gcHandleAddressOfEvalStackValue (operation : string) (arg : EvalStackValue) : GcHandleAddress =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, 0L)) -> handle
        | EvalStackValue.NativeInt (NativeIntSource.GcHandlePtr (handle, tag)) ->
            failwith $"%s{operation}: expected an untagged GC handle, got %O{handle} carrying tag bits 0x%x{tag}"
        | other -> failwith $"%s{operation}: expected GC handle pointer, got %O{other}"

    let pushGcHandleAddress (handle : GcHandleAddress) (thread : ThreadId) (state : IlMachineState) : IlMachineState =
        IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.gcHandlePtr handle)) thread state

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

    /// A `uint16_t` parameter, such as the `port` of `SystemNative_SetPort`.
    ///
    /// Distinct from `int32Argument` because PawPrint keeps a CLI `UInt16` in a
    /// two-byte cell of its own rather than widening it to `Int32` the way it
    /// does a `UInt32`, so a `ushort` P/Invoke parameter never arrives as an
    /// `Int32` and that reader rejects it outright.
    let uint16Argument (operation : string) (arg : CliType) : uint16 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.UInt16 i) -> i
        | other -> failwith $"%s{operation}: expected UInt16 argument, got %O{other}"

    /// A `uint32_t` parameter, such as the `address` of
    /// `SystemNative_SetIPv4Address`.
    ///
    /// A CLI `UInt32` shares the four-byte cell of an `Int32` while preserving the
    /// low 32 bits (see `cliUInt32` above), so this is that cell reinterpreted
    /// rather than a case of its own — the inverse of `cliUInt32`, and the reason
    /// a negative `Int32` here is a `uint32` above `Int32.MaxValue` rather than an
    /// error.
    let uint32Argument (operation : string) (arg : CliType) : uint32 = uint32 (int32Argument operation arg)

    /// An `int64_t` parameter, such as the `fileOffset` of `SystemNative_PRead`.
    /// Accepts a 32-bit value too, since a guest hand-rolling the P/Invoke may
    /// hand over a widened constant, and every `int64` parameter PawPrint
    /// receives is one whose value a narrower cell represents exactly.
    let int64Argument (operation : string) (arg : CliType) : int64 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim i)) -> i
        | CliType.Numeric (CliNumericType.Int32 i) -> int64 i
        | other -> failwith $"%s{operation}: expected Int64 argument, got %O{other}"

    /// Extract the registry id from the m_handle of a `RuntimeFieldHandleInternal`, or `None` for
    /// the null sentinel. `default(RuntimeFieldHandleInternal)` reaches PawPrint as whichever zero
    /// shape the zero-initialising path produced -- a verbatim zero or a null managed pointer, in
    /// either the runtime-pointer or the native-int tag -- so all four spellings map to `None`,
    /// exactly as in `methodHandleIdOfRuntimeMethodHandleInternal` below.
    let fieldHandleIdOfRuntimeFieldHandleInternal (operation : string) (arg : CliType) : int64 option =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.FieldRegistryHandle id) -> Some id
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> None
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.FieldHandlePtr id)) -> Some id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other ->
            failwith
                $"%s{operation}: expected RuntimeFieldHandleInternal containing a field-registry handle, got %O{other}"

    /// Extract the registry id from the m_handle of a `RuntimeMethodHandleInternal`. Accepts both
    /// the canonical `RuntimePointer (MethodRegistryHandle id)` form and the `NativeInt
    /// (MethodHandlePtr id)` form that primitive-like rewrapping produces when the value is
    /// stored through an `IntPtr`-shaped byref (see EvalStack rewrap rules). `Verbatim 0L` in
    /// either tag means "null sentinel" — the BCL writes that when iteration is exhausted.
    let methodHandleIdOfRuntimeMethodHandleInternal (operation : string) (arg : CliType) : int64 option =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.RuntimePointer (CliRuntimePointer.MethodRegistryHandle id) -> Some id
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L) -> None
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.MethodHandlePtr id)) -> Some id
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> None
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null)) -> None
        | other ->
            failwith
                $"%s{operation}: expected RuntimeMethodHandleInternal containing a method-registry handle, got %O{other}"

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
            ManagedPointerByteView.addByteOffset state charConcreteType (charIndex * 2) ptr

        match IlMachineState.readManagedByrefBytesAs baseClassTypes state ptr (CliType.ofChar (char 0)) with
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
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read UTF-16 string from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
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

    /// Read exactly `length` UTF-16 code units from `ptr`, returning the resulting
    /// `string`. Counterpart to `readNullTerminatedUtf16` for callers whose source
    /// passes an explicit length alongside the pointer (e.g. the
    /// `ThreadNative_InformThreadNameChange` QCall, whose CoreCLR signature is
    /// `(ThreadHandle, char* name, int32 len) -> void`).
    ///
    /// `length = 0` returns `""` regardless of `ptr` (so this helper safely
    /// handles both `Thread.Name = ""` — non-null pointer, len=0 — and
    /// `Thread.Name = null` — null pointer, len=0); callers that need to
    /// distinguish those two cases must inspect the pointer themselves before
    /// invoking. `length < 0` is a guest contract violation and fails loudly.
    /// A null pointer with `length > 0` is also a contract violation and
    /// fails loudly rather than dereferencing the null.
    let readLengthPrefixedUtf16
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (length : int)
        : string
        =
        if length < 0 then
            failwith $"%s{operation}: UTF-16 length %d{length} is negative"
        elif length = 0 then
            ""
        else
            match ptr with
            | ManagedPointerSource.Null ->
                failwith
                    $"%s{operation}: cannot read UTF-16 string of length %d{length} from a null pointer; callers must pass length=0 when the pointer is null"
            | ManagedPointerSource.NativeIntPlaceholder bits ->
                failwith
                    $"%s{operation}: cannot read UTF-16 string from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
            | ManagedPointerSource.Byref _ ->
                let charConcreteType = requiredCharConcreteType operation baseClassTypes state
                let chars = Array.zeroCreate<char> length

                for i in 0 .. length - 1 do
                    chars.[i] <- readUtf16Char operation baseClassTypes state charConcreteType ptr i

                System.String chars

    let requiredByteConcreteType
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

    let private readNamedByte
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (byteConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (byteIndex : int)
        : UInt8Source
        =
        let ptr = ManagedPointerByteView.addByteOffset state byteConcreteType byteIndex ptr

        match
            IlMachineState.readManagedByrefBytesAs
                baseClassTypes
                state
                ptr
                (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
        with
        | CliType.Numeric (CliNumericType.UInt8 b) -> b
        | other -> failwith $"%s{operation}: byte read returned non-byte value %O{other}"

    let private readNumericByte
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (byteConcreteType : ConcreteType<ConcreteTypeHandle>)
        (ptr : ManagedPointerSource)
        (byteIndex : int)
        : byte
        =
        readNamedByte operation baseClassTypes state byteConcreteType ptr byteIndex
        |> UInt8Source.value $"%s{operation}: byte read"

    /// The bytes of a NUL-terminated C string, without the terminator and
    /// without interpreting them.
    ///
    /// Separate from `readNullTerminatedUtf8` because not every caller wants
    /// the lenient decode: `Encoding.UTF8.GetString` silently replaces an
    /// invalid sequence with U+FFFD, which for a *filename* means quietly
    /// naming a different file than the guest asked for. A caller that must not
    /// alias reads the bytes and decodes them strictly.
    let readNullTerminatedBytes
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : byte array
        =
        match ptr with
        | ManagedPointerSource.Null ->
            failwith $"TODO: %s{operation} with null UTF-8 pointer should throw ArgumentNullException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read UTF-8 string from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ ->
            let byteConcreteType = requiredByteConcreteType operation baseClassTypes state

            let rec loop (byteIndex : int) (bytes : byte list) : byte array =
                if byteIndex > 65535 then
                    // Defensive PawPrint bound against scanning guest memory
                    // forever for unterminated strings.
                    failwith $"%s{operation}: unterminated UTF-8 string exceeded PawPrint's 65535-byte scan limit"

                let b =
                    readNumericByte operation baseClassTypes state byteConcreteType ptr byteIndex

                if b = 0uy then
                    bytes |> List.rev |> Array.ofList
                else
                    loop (byteIndex + 1) (b :: bytes)

            loop 0 []

    /// `readNullTerminatedBytes`, but scanning at most `maxBytes` bytes of guest
    /// memory — for callers whose own boundary imposes a limit.
    ///
    /// Returns the bytes before the terminator when one is found within that
    /// span, and otherwise exactly `maxBytes` bytes ("at least this long"); the
    /// rule for what is too long stays with the caller.
    ///
    /// This exists because a real `stat` does not scan for a terminator
    /// indefinitely either. `getname()` on Linux and `copyinstr` on Darwin copy
    /// at most `PATH_MAX` bytes and report ENAMETOOLONG if no NUL appears in
    /// them — so an unterminated buffer is an ordinary error a guest can
    /// provoke, not the interpreter abort that reading past it would give.
    let readNullTerminatedBytesWithin
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (maxBytes : int)
        : byte array
        =
        if maxBytes < 1 then
            failwith $"%s{operation}: cannot scan %d{maxBytes} bytes for a terminator; the bound must be positive."

        match ptr with
        | ManagedPointerSource.Null ->
            failwith $"TODO: %s{operation} with null UTF-8 pointer should throw ArgumentNullException"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read UTF-8 string from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ ->
            let byteConcreteType = requiredByteConcreteType operation baseClassTypes state

            let rec loop (byteIndex : int) (bytes : byte list) : byte array =
                if byteIndex >= maxBytes then
                    bytes |> List.rev |> Array.ofList
                else

                let b =
                    readNumericByte operation baseClassTypes state byteConcreteType ptr byteIndex

                if b = 0uy then
                    bytes |> List.rev |> Array.ofList
                else
                    loop (byteIndex + 1) (b :: bytes)

            loop 0 []

    let readNullTerminatedUtf8
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        : string
        =
        readNullTerminatedBytes operation baseClassTypes state ptr
        |> System.Text.Encoding.UTF8.GetString

    /// Read exactly <paramref name="byteCount"/> bytes from the guest, without looking for a null
    /// terminator, and without interpreting them. Callers that receive a length must not scan past
    /// it: CoreCLR's metadata strings are counted rather than terminated (see <c>MdUtf8String</c>,
    /// which carries an explicit byte length), and a custom-attribute blob is not text at all.
    ///
    /// Every byte must hold a number; a byte that names a native int is refused. Take
    /// <c>readCountedNamedBytes</c> where that shape is meaningful.
    let readCountedBytes
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (byteCount : int)
        : byte array
        =
        if byteCount < 0 then
            failwith $"%s{operation}: negative byte count %d{byteCount}"
        elif byteCount = 0 then
            // A zero-length buffer is legitimately pinned from an empty span, which the
            // guest presents as a null pointer; don't dereference it.
            Array.empty
        else

        match ptr with
        | ManagedPointerSource.Null ->
            failwith
                $"%s{operation}: cannot read %d{byteCount} bytes from a null pointer; callers must pass byteCount=0 when the pointer is null"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read bytes from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ ->
            let byteConcreteType = requiredByteConcreteType operation baseClassTypes state

            Array.init
                byteCount
                (fun byteIndex -> readNumericByte operation baseClassTypes state byteConcreteType ptr byteIndex)

    /// Read exactly <paramref name="byteCount"/> bytes from the guest without demanding that
    /// every one of them be a number.
    ///
    /// A byte that names a native int rather than holding a number is how a `Reflection.Emit`
    /// signature blob carries a type its `SignatureHelper` had no module to spell as a token
    /// (`ELEMENT_TYPE_INTERNAL`). Callers that can say something useful about such a byte take
    /// this; everything else takes <c>readCountedBytes</c>, which refuses one by name.
    let readCountedNamedBytes
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (ptr : ManagedPointerSource)
        (byteCount : int)
        : UInt8Source[]
        =
        if byteCount = 0 then
            [||]
        else

        match ptr with
        | ManagedPointerSource.Null ->
            failwith
                $"%s{operation}: cannot read %d{byteCount} bytes from a null pointer; callers must pass byteCount=0 when the pointer is null"
        | ManagedPointerSource.NativeIntPlaceholder bits ->
            failwith
                $"%s{operation}: cannot read bytes from fake non-null byref @ 0x%x{bits}; the placeholder must never be dereferenced"
        | ManagedPointerSource.Byref _ ->
            let byteConcreteType = requiredByteConcreteType operation baseClassTypes state

            Array.init
                byteCount
                (fun byteIndex -> readNamedByte operation baseClassTypes state byteConcreteType ptr byteIndex)

    /// Allocate a managed <c>byte[]</c> holding <paramref name="storage"/> and return the array
    /// object itself. Use this when the guest receives the array as an object — e.g. a QCall
    /// writing through an <c>ObjectHandleOnStack</c>; use <c>allocateBlobByteArray</c> when it
    /// receives a pointer into the buffer instead.
    let allocateManagedByteArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (storage : byte array)
        (state : IlMachineState)
        : ManagedHeapAddress * IlMachineState
        =
        let byteHandle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Byte

        let arrayAddr, state =
            IlMachineState.allocateArray
                (ConcreteTypeHandle.OneDimArrayZero byteHandle)
                (fun () -> CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy)))
                storage.Length
                state

        let state =
            ((state, 0), storage)
            ||> Array.fold (fun (state, index) b ->
                IlMachineState.setArrayValue
                    arrayAddr
                    (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim b)))
                    index
                    state,
                index + 1
            )
            |> fst

        arrayAddr, state

    /// Allocate a managed <c>byte[]</c> backing buffer for an unmanaged-looking blob and return
    /// a byref to its first element. Shared by callers that need to materialise <c>ConstArray</c>
    /// or null-terminated UTF-8 results across the native boundary.
    let allocateBlobByteArray
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (storage : byte array)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let arrayAddr, state = allocateManagedByteArray baseClassTypes storage state
        ManagedPointerSource.Byref (ByrefRoot.ArrayElement (arrayAddr, 0), []), state

    /// Allocate a block on the simulated process's native heap (the pool that backs
    /// <c>malloc</c> / <c>Marshal.AllocHGlobal</c>), fill it with <paramref name="bytes"/>, and
    /// return a pointer to its base address.
    ///
    /// Use this — rather than <c>allocateBlobByteArray</c> — whenever the guest is going to
    /// <c>free</c> the result, which is the case for every entry point modelled on a native
    /// function that returns <c>strdup</c>'d or <c>malloc</c>'d memory. Only a native-heap block
    /// base is a legal argument to <c>SystemNative_Free</c>; a byref into a managed
    /// <c>byte[]</c> is refused there (correctly — freeing it would be nonsense).
    let allocateNativeHeapBlob
        (operation : string)
        (bytes : byte array)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        // ZeroInitialized rather than Uninitialized: every byte is overwritten
        // immediately below, so there is nothing for the use-of-uninit detector to
        // catch, and a zeroed block gives a saner picture if a future caller
        // under-fills its allocation.
        let ptr, state =
            IlMachineState.allocateNativeMemory MemoryBlockInitialization.ZeroInitialized bytes.Length state

        let blockId =
            match ptr with
            | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (blockId, 0), []) -> blockId
            | other ->
                failwith
                    $"%s{operation}: allocateNativeMemory returned an unexpected pointer shape (%O{other}); this is an interpreter bug"

        let state =
            state.MapKernel (fun kernel ->
                { kernel with
                    NativeMemoryPool = NativeMemoryPool.writeBytes blockId 0 bytes kernel.NativeMemoryPool
                }
            )

        ptr, state

    /// Resolve the pointer a <c>free</c>-shaped entry point was handed to the native-heap
    /// block it may release: <c>Ok None</c> for the <c>free(NULL)</c> no-op, <c>Ok (Some
    /// block)</c> for an allocation's base address, and <c>Error</c> — carrying a description
    /// of what was wrong, for the caller to prefix with its own name — for anything else.
    ///
    /// C <c>free</c> is undefined unless its argument is exactly an address a
    /// <c>malloc</c>-shaped entry point returned, so an interior pointer like <c>base + 4</c>
    /// is refused rather than taken to mean the whole block: accepting it would mask the guest
    /// memory-corruption bug that produced it.
    let tryResolveNativeHeapFreeTarget (ptr : ManagedPointerSource) : Result<NativeMemoryBlockId option, string> =
        // Accumulated in `int64` because this file is not `Checked`: an `int` fold could wrap
        // an interior pointer back onto zero and so report the block base, which is precisely
        // the corruption this refusal exists to expose (issue #993).
        let rec projectionByteOffset (acc : int64) (ps : ByrefProjection list) : Result<int64, ByrefProjection> =
            match ps with
            | [] -> Ok acc
            | ByrefProjection.ReinterpretAs _ :: rest -> projectionByteOffset acc rest
            | ByrefProjection.ByteOffset n :: rest -> projectionByteOffset (acc + int64<int> n) rest
            | (ByrefProjection.Field _ as field) :: _ -> Error field

        match ptr with
        | ManagedPointerSource.Null -> Ok None
        | ManagedPointerSource.Byref (ByrefRoot.NativeMemoryByte (block, rootByteOffset), projs) ->
            match projectionByteOffset (int64<int> rootByteOffset) projs with
            | Ok 0L -> Ok (Some block)
            | Ok offset ->
                Error
                    $"refusing to free interior native-heap pointer at byte offset %d{offset} into %O{block} (only an allocation's base address may be freed)"
            | Error field ->
                Error
                    $"refusing to free native-heap pointer with non-byte projection %O{field} into %O{block} (only an allocation's base address may be freed)"
        | other -> Error $"expected null or a native-heap pointer, got %O{other}"

    /// Allocate a native-heap block holding the UTF-8 encoding of <paramref name="value"/>
    /// followed by a single trailing null byte, and return a pointer to its base address.
    /// This is PawPrint's <c>strdup</c>: the resulting pointer is a C string the guest may
    /// read and then <c>free</c> (which is exactly what CoreLib's
    /// <c>Utf8StringMarshaller.Free</c> does with the return value of a
    /// <c>StringMarshalling.Utf8</c> <c>LibraryImport</c>).
    let allocateNativeHeapNullTerminatedUtf8
        (operation : string)
        (value : string)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let bytes = System.Text.Encoding.UTF8.GetBytes value
        let storage = Array.zeroCreate<byte> (bytes.Length + 1)
        Array.blit bytes 0 storage 0 bytes.Length

        allocateNativeHeapBlob operation storage state

    /// Allocate a managed <c>byte[]</c> holding the UTF-8 encoding of <paramref name="value"/>
    /// followed by a single trailing null byte, and return a byref to its first element. The
    /// resulting pointer is suitable for managed code that expects a C-style null-terminated
    /// UTF-8 string (e.g. CoreLib's <c>MdUtf8String(void*)</c> ctor, which calls
    /// <c>string.strlen</c> on the pointer).
    ///
    /// The buffer lives on the *managed* heap, so the guest must not <c>free</c> it. Where the
    /// native function being modelled hands ownership to the caller, use
    /// <c>allocateNativeHeapNullTerminatedUtf8</c> instead.
    let allocateNullTerminatedUtf8
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (value : string)
        (state : IlMachineState)
        : ManagedPointerSource * IlMachineState
        =
        let bytes = System.Text.Encoding.UTF8.GetBytes value
        let storage = Array.zeroCreate<byte> (bytes.Length + 1)
        Array.blit bytes 0 storage 0 bytes.Length

        allocateBlobByteArray baseClassTypes storage state

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

    /// Decode the byref a `StackCrawlMarkHandle` wraps. Its `_ptr` is `Unsafe.AsPointer(ref
    /// stackMark)` over a `StackCrawlMark` local, so the byref names the frame that declared the
    /// mark; `StackCrawlMark.resolveCaller` needs exactly that frame.
    let stackCrawlMarkHandleTarget
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
        | other -> failwith $"%s{operation}: expected %s{argName} to be StackCrawlMarkHandle, got %O{other}"

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
        EvalStackValue.requireMethodTable operation arg

    /// Decode a `void*`/`TypeHandle` argument to the underlying RuntimeTypeHandleTarget. Unlike
    /// `methodTableOfEvalStackValue`, this preserves the full target so callers that legitimately
    /// receive an open generic definition or generic parameter can dispatch on it (e.g. CoreLib's
    /// `TypeHandle.GetCorElementType` QCall).
    let runtimeTypeHandleTargetOfEvalStackValue (operation : string) (arg : EvalStackValue) : RuntimeTypeHandleTarget =
        match arg with
        | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr target)
        | EvalStackValue.NativeInt (NativeIntSource.MethodTablePtr target) -> target
        | other -> failwith $"%s{operation}: expected TypeHandle/MethodTable pointer argument, got %O{other}"

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

        DynamicScopeOperand.runtimeTypeHandleTargetOfRuntimeType operation state runtimeTypeAddr

    /// The definition identity of the assembly that declares the type this handle names.
    let typeAssemblyFullName
        (operation : string)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (typeHandleTarget : RuntimeTypeHandleTarget)
        : string
        =
        match typeHandleTarget with
        // An instantiation lives in the assembly that declares its definition, exactly as a
        // closed one does (`typeof(List<int>).Assembly` is corelib, not the caller's).
        // `CreateMinimalMethodTable` calls `SetModule(pContainingModule)` (methodtable.cpp:687), so
        // the containing module -- and hence the assembly -- is one of the few facts the synthetic
        // type carries, rather than one it would need a metadata row to answer.
        | RuntimeTypeHandleTarget.DynamicMethodsClass scopeAssembly -> scopeAssembly
        | RuntimeTypeHandleTarget.OpenConstructed (identity, _)
        | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity -> identity.AssemblyFullName
        | RuntimeTypeHandleTarget.GenericParameter (declaringType, _)
        | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, _, _) ->
            // A generic parameter belongs to the same assembly as its declaring type.
            declaringType.AssemblyFullName
        | RuntimeTypeHandleTarget.Closed concreteTypeHandle ->
            // Unwrap Byref/Pointer/Array to reach the element type's assembly.
            // In .NET, typeof(T[]).Assembly == typeof(T).Assembly, so arrays follow
            // the element rule. Function pointers anchor to their return type's
            // assembly (CoreCLR `MethodTable::GetAssembly` for FnPtr); for `void`
            // returns, that assembly is corelib (where System.Void lives).
            let rec assemblyFullNameOfHandle (h : ConcreteTypeHandle) : string =
                match h with
                | ConcreteTypeHandle.Concrete _ ->
                    let concreteType =
                        AllConcreteTypes.lookup h state.ConcreteTypes
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: could not find concrete type for handle %O{concreteTypeHandle} (unwrapped to %O{h})"
                        )

                    concreteType.AssemblyFullName
                | ConcreteTypeHandle.Byref inner -> assemblyFullNameOfHandle inner
                | ConcreteTypeHandle.Pointer inner -> assemblyFullNameOfHandle inner
                | ConcreteTypeHandle.OneDimArrayZero inner -> assemblyFullNameOfHandle inner
                | ConcreteTypeHandle.Array (inner, _) -> assemblyFullNameOfHandle inner
                | ConcreteTypeHandle.FunctionPointer signature ->
                    match signature.ReturnType with
                    | MethodReturnType.Void -> baseClassTypes.Void.AssemblyFullName
                    | MethodReturnType.Returns ret -> assemblyFullNameOfHandle ret

            assemblyFullNameOfHandle concreteTypeHandle

    let failUnimplemented (ctx : NativeCallContext) : NativeHandlerResult =
        let instruction = ctx.Instruction
        let state = ctx.State

        let implKind =
            match instruction.ExecutingMethod.Body with
            | MethodBody.InternalCall -> "InternalCall"
            | MethodBody.PInvoke ->
                match instruction.ExecutingMethod.TryNativeImport with
                | Some import -> $"PInvokeImpl %s{import.ModuleName}!%s{import.EntryPointName}"
                | None -> "PInvokeImpl"
            | MethodBody.RuntimeProvided behaviour ->
                match behaviour with
                | RuntimeBehaviour.DelegateCtor -> "Runtime (delegate .ctor)"
                | RuntimeBehaviour.DelegateInvoke -> "Runtime (delegate Invoke)"
                | RuntimeBehaviour.StructMarshalStub -> "Runtime (struct-marshal stub)"
                | RuntimeBehaviour.UnsafeAccessor (kind, targetName, hasTypeNameOverrides) ->
                    let nameStr =
                        match targetName with
                        | Some n -> $"\"%s{n}\""
                        | None -> "<attributed method name>"

                    let overrides =
                        if hasTypeNameOverrides then
                            ", types named by [UnsafeAccessorType]"
                        else
                            ""

                    $"Runtime (UnsafeAccessor: kind=%O{kind}, target=%s{nameStr}%s{overrides})"
                | RuntimeBehaviour.UnsafeAccessorInvalidKind raw -> $"Runtime (UnsafeAccessor naming no kind: %d{raw})"
                | RuntimeBehaviour.Unrecognised name -> $"Runtime (unrecognised: %s{name})"
            | MethodBody.Abstract -> "Abstract"
            | MethodBody.Il _ -> "IL"

        let rec formatTypeHandle (cth : ConcreteTypeHandle) : string =
            match AllConcreteTypes.lookup cth state.ConcreteTypes with
            | Some ct -> $"{ct.Namespace}.{ct.Name}"
            | None ->
                match cth with
                | ConcreteTypeHandle.Byref inner -> $"&({formatTypeHandle inner})"
                | ConcreteTypeHandle.Pointer inner -> $"*({formatTypeHandle inner})"
                | ConcreteTypeHandle.FunctionPointer signature ->
                    let argStr =
                        signature.ParameterTypes |> Seq.map formatTypeHandle |> String.concat ", "

                    let retStr =
                        match signature.ReturnType with
                        | MethodReturnType.Void -> "void"
                        | MethodReturnType.Returns ret -> formatTypeHandle ret

                    $"fnptr({argStr}->{retStr})"
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
            $"Unimplemented native method (%s{implKind}): %s{ctx.TargetAssembly.Name.Name} %s{ctx.TargetType.Namespace}.%s{ctx.TargetType.Name}::%s{instruction.ExecutingMethod.Name}(%s{paramStr}) -> %s{retStr}. Implement it as a handler in Native/ and register that handler in NativeDispatch."
