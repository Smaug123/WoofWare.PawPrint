namespace WoofWare.PawPrint

/// A base unmanaged calling convention, which CoreCLR names by a type of
/// `System.Runtime.CompilerServices` (`DECLARE_BASE_CALL_CONVS`, `vm/callconvbuilder.cpp`).
[<RequireQualifiedAccess>]
type UnmanagedCallConvBase =
    | Cdecl
    | Stdcall
    | Thiscall
    | Fastcall
    | Swift

/// A modifier of an unmanaged calling convention, which CoreCLR names by a type of
/// `System.Runtime.CompilerServices` (`DECLARE_MOD_CALL_CONVS`, `vm/callconvbuilder.cpp`).
[<RequireQualifiedAccess>]
type UnmanagedCallConvModifier =
    | SuppressGcTransition
    | MemberFunction

/// The conventions a list of calling-convention types names, as CoreCLR's `CallConvBuilder` holds
/// them: at most one base convention, and any modifiers. A type it does not recognise adds nothing.
type UnmanagedCallConvs =
    {
        /// `None` where no base convention is named, for which CoreCLR takes the platform's default
        /// (`GetDefaultUnmanagedCallingConvention`: C on Unix, Stdcall on Windows), or its
        /// member-function variant.
        Base : UnmanagedCallConvBase option
        Modifiers : Set<UnmanagedCallConvModifier>
    }

/// The calling convention CoreCLR compiles an `[UnmanagedCallersOnly]` method for, from what the
/// attribute names (`getUnmanagedCallConvForMethod`, `vm/jitinterface.cpp`).
[<RequireQualifiedAccess>]
type CallersOnlyConvention =
    /// The conventions the attribute's `CallConvs` names, where it names no second base convention.
    | Named of UnmanagedCallConvs
    /// The platform's default, without modifiers: the attribute names no `CallConvs`, or names a
    /// second base convention, either of which makes
    /// `CallConv::TryGetCallingConventionFromUnmanagedCallersOnly` answer false.
    | PlatformDefault
    /// CoreCLR cannot parse the attribute's blob, and the compilation of the method raises the
    /// exception of the failure's `HRESULT` (a `COMException` for most such blobs, an
    /// `OverflowException` where a string runs past the blob's end).
    | Unreadable of reason : string
    /// `CallConvs = null`: CoreCLR reads the elements of the null array it parsed, and the
    /// compilation of the method faults in the runtime, which kills the process.
    | NullArray

[<RequireQualifiedAccess>]
module UnmanagedCallingConvention =
    /// No convention named.
    let empty : UnmanagedCallConvs =
        {
            Base = None
            Modifiers = Set.empty
        }

    let private bases : (string * UnmanagedCallConvBase) list =
        [
            "CallConvCdecl", UnmanagedCallConvBase.Cdecl
            "CallConvStdcall", UnmanagedCallConvBase.Stdcall
            "CallConvThiscall", UnmanagedCallConvBase.Thiscall
            "CallConvFastcall", UnmanagedCallConvBase.Fastcall
            "CallConvSwift", UnmanagedCallConvBase.Swift
        ]

    let private modifiers : (string * UnmanagedCallConvModifier) list =
        [
            "CallConvSuppressGCTransition", UnmanagedCallConvModifier.SuppressGcTransition
            "CallConvMemberFunction", UnmanagedCallConvModifier.MemberFunction
        ]

    /// Whether the serialised type name `serialised` names the type `System.Runtime.CompilerServices.<name>`:
    /// it is that name alone, as for a type of the attribute's own assembly, or that name followed by
    /// a comma and an assembly, any assembly (`BeginsWithTypeName`).
    let private namesType (name : string) (serialised : string) : bool =
        let full = "System.Runtime.CompilerServices." + name

        serialised.StartsWith (full, System.StringComparison.Ordinal)
        && (serialised.Length = full.Length || serialised.[full.Length] = ',')

    /// Add the type whose serialised, assembly-qualified name is `typeName` (`None` for a null
    /// element) to `conventions`, as `CallConvBuilder::AddFullyQualifiedTypeName` does; `None` where
    /// it names a second base convention, which CoreCLR takes as an error.
    let addFullyQualifiedTypeName
        (typeName : string option)
        (conventions : UnmanagedCallConvs)
        : UnmanagedCallConvs option
        =
        match typeName with
        | None -> Some conventions
        | Some typeName ->

        match bases |> List.tryFind (fun (name, _) -> namesType name typeName) with
        | Some (_, convention) ->
            match conventions.Base with
            | Some _ -> None
            | None ->
                Some
                    { conventions with
                        Base = Some convention
                    }
        | None ->

        match modifiers |> List.tryFind (fun (name, _) -> namesType name typeName) with
        | Some (_, modifier) ->
            Some
                { conventions with
                    Modifiers = Set.add modifier conventions.Modifiers
                }
        | None -> Some conventions

    /// Whether `runtime`'s `UnmanagedCallersOnlyAttribute` declares the `AssociatedSourceType` named
    /// argument, which its VM accepts in the attribute's blob (`namedArgs`,
    /// `CallConv::TryGetCallingConventionFromUnmanagedCallersOnly`).
    let declaresAssociatedSourceType (runtime : EmulatedRuntime) : bool =
        match runtime with
        | EmulatedRuntime.Net10 -> false
        | EmulatedRuntime.Net11 -> true

    /// The calling convention `runtime`'s CoreCLR compiles a method for whose
    /// `[UnmanagedCallersOnly]` is `application`, where the method's signature has the default or
    /// vararg calling convention, which ECMA-335 gives every method definition. CoreCLR reads
    /// another convention, which only hand-written IL can give one, from the signature instead.
    let ofCallersOnly
        (runtime : EmulatedRuntime)
        (application : UnmanagedCallersOnlyApplication)
        : CallersOnlyConvention
        =
        match application with
        | UnmanagedCallersOnlyApplication.Malformed reason -> CallersOnlyConvention.Unreadable reason
        | UnmanagedCallersOnlyApplication.Parsed (_, true) when not (declaresAssociatedSourceType runtime) ->
            CallersOnlyConvention.Unreadable
                $"The blob names AssociatedSourceType, which %A{runtime}'s attribute does not declare"
        | UnmanagedCallersOnlyApplication.Parsed (callConvs, _) ->

        match callConvs with
        | UnmanagedCallersOnlyCallConvs.Unnamed -> CallersOnlyConvention.PlatformDefault
        | UnmanagedCallersOnlyCallConvs.NullArray -> CallersOnlyConvention.NullArray
        | UnmanagedCallersOnlyCallConvs.Types typeNames ->
            (Some empty, typeNames)
            ||> List.fold (fun conventions typeName -> conventions |> Option.bind (addFullyQualifiedTypeName typeName))
            |> Option.map CallersOnlyConvention.Named
            |> Option.defaultValue CallersOnlyConvention.PlatformDefault

    /// Whether CoreCLR's JIT for `target` compiles calls by Swift's calling convention
    /// (`SWIFT_SUPPORT`): on Arm64 it does, and on x64 it does for the Unix ABI
    /// (`UNIX_AMD64_ABI`) only. `JitTarget` names no operating system, and this answers for a
    /// Unix runtime; Windows x64 is not modelled.
    let compilesSwiftCalls (target : JitTarget) : bool =
        match target with
        | JitTarget.Arm64
        | JitTarget.X64 -> true

    /// Whether CoreCLR's JIT for `target` compiles a method whose calling convention is `convention`
    /// as Swift's: a base convention of Swift, whatever modifiers come with it (the member-function
    /// variant of Swift's is Swift's), on a target that compiles Swift calls.
    let isSwift (target : JitTarget) (convention : CallersOnlyConvention) : bool =
        match convention with
        | CallersOnlyConvention.Named {
                                          Base = Some UnmanagedCallConvBase.Swift
                                      } -> compilesSwiftCalls target
        | CallersOnlyConvention.Named _
        | CallersOnlyConvention.PlatformDefault
        | CallersOnlyConvention.Unreadable _
        | CallersOnlyConvention.NullArray -> false

/// What CoreCLR's `PInvoke::MarshalingRequired` (`vm/dllimport.cpp`) makes of one type of the
/// signature of an `[UnmanagedCallersOnly]` method, which needs no marshalling to be valid
/// (`COMDelegate::ThrowIfInvalidUnmanagedCallersOnlyUsage`).
[<RequireQualifiedAccess>]
type SignatureTypeMarshalling =
    /// It needs no marshalling, unless the parameter carries `[MarshalAs]`
    /// (`UnmanagedCallersOnly.marshalAsRequires`).
    | NotRequired
    /// It needs marshalling, which CoreCLR decides without loading anything.
    | Required
    /// CoreCLR loads this type in full (`GetTypeHandleThrowing`), and then finds it needs
    /// marshalling: it instantiates a class.
    | RequiredOnceLoaded of loaded : TypeDefn
    /// CoreCLR loads this value type in full, and then decides by what it holds and how it is laid
    /// out: it needs marshalling where it is not blittable and not an enum (or, with runtime
    /// marshalling disabled, where it holds a reference or has an auto-layout part), where it is
    /// not valid for generic marshalling, or where it holds an `Int128` (or, on .NET 11, a decimal
    /// floating-point type).
    | DecidedByLayoutOnceLoaded of loaded : TypeDefn

/// What CoreCLR refuses of an `[UnmanagedCallersOnly]` method, which only native code may call.
[<RequireQualifiedAccess>]
module UnmanagedCallersOnly =
    /// Whether `runtime`'s CoreCLR refuses to make a delegate over an `[UnmanagedCallersOnly]`
    /// method where the JIT compiles the delegate's construction and sees the method the target's
    /// address instruction names: .NET 10's `COMDelegate::GetDelegateCtor` throws
    /// `NotSupportedException` (`NotSupported_UnmanagedCallersOnlyTarget`) before it walks any
    /// signature; .NET 11's no longer checks, and makes the delegate.
    let refusesDelegateTarget (runtime : EmulatedRuntime) : bool =
        match runtime with
        | EmulatedRuntime.Net10 -> true
        | EmulatedRuntime.Net11 -> false

    /// The modifiers `MarshalingRequired` looks for on a value type a pointer points to, which make
    /// the pointer need marshalling where runtime marshalling is enabled.
    let private copyConstructorModifiers : Set<string * string> =
        Set.ofList
            [
                "Microsoft.VisualC", "NeedsCopyConstructorModifier"
                "System.Runtime.CompilerServices", "IsCopyConstructed"
            ]

    /// The namespace and name of a type `assembly` spells by a reference or a definition.
    let private spelledName (assembly : DumpedAssembly) (spelling : TypeDefn) : (string * string) option =
        match spelling with
        | TypeDefn.FromReference (typeRef, _) -> Some (typeRef.Namespace, typeRef.Name)
        | TypeDefn.FromDefinition (identity, _) when identity.AssemblyFullName = assembly.DefinitionFullName ->
            let definition = assembly.TypeDefs.[identity.TypeDefinition.Get]
            Some (definition.Namespace, definition.Name)
        | _ -> None

    /// The names of the required modifiers on `spelling`, outermost first, and what they modify.
    let rec private requiredModifiers
        (assembly : DumpedAssembly)
        (spelling : TypeDefn)
        : (string * string) list * TypeDefn
        =
        match spelling with
        | TypeDefn.Modified modified ->
            let names, unmodified = requiredModifiers assembly modified.Unmodified

            let own =
                if modified.IsRequired then
                    Option.toList (spelledName assembly modified.Modifier)
                else
                    []

            own @ names, unmodified
        | spelling -> [], spelling

    /// What `PInvoke::MarshalingRequired` makes of `spelling`, a type of the signature of an
    /// `[UnmanagedCallersOnly]` method of `assembly` (its return type, `TypeDefn.Void` for none, or a
    /// parameter's), where runtime marshalling is enabled for `assembly` or not (it carries no
    /// `[DisableRuntimeMarshalling]`, `Module::IsRuntimeMarshallingEnabled`). It reads past custom
    /// modifiers, as `PeekElemType` does.
    let signatureTypeMarshalling
        (assembly : DumpedAssembly)
        (runtimeMarshalling : bool)
        (spelling : TypeDefn)
        : SignatureTypeMarshalling
        =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.Void -> SignatureTypeMarshalling.NotRequired
        | TypeDefn.PrimitiveType primitive ->
            match primitive with
            | PrimitiveType.Boolean
            | PrimitiveType.Char ->
                if runtimeMarshalling then
                    SignatureTypeMarshalling.Required
                else
                    SignatureTypeMarshalling.NotRequired
            | PrimitiveType.String
            | PrimitiveType.Object
            | PrimitiveType.TypedReference -> SignatureTypeMarshalling.Required
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr -> SignatureTypeMarshalling.NotRequired
        | TypeDefn.Pointer pointee ->
            let modifiers, pointee = requiredModifiers assembly pointee

            let pointsToValueType =
                match pointee with
                | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.ValueType)
                | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.ValueType) -> true
                | _ -> false

            if
                runtimeMarshalling
                && pointsToValueType
                && modifiers |> List.exists copyConstructorModifiers.Contains
            then
                SignatureTypeMarshalling.Required
            else
                SignatureTypeMarshalling.NotRequired
        | TypeDefn.FunctionPointer _ -> SignatureTypeMarshalling.NotRequired
        | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.ValueType)
        | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.ValueType) as valueType ->
            SignatureTypeMarshalling.DecidedByLayoutOnceLoaded valueType
        | TypeDefn.GenericInstantiation (root, _) as instantiation ->
            match TypeDefn.stripCustomModifiers root with
            | TypeDefn.FromReference (_, System.Reflection.Metadata.SignatureTypeKind.ValueType)
            | TypeDefn.FromDefinition (_, System.Reflection.Metadata.SignatureTypeKind.ValueType) ->
                SignatureTypeMarshalling.DecidedByLayoutOnceLoaded instantiation
            | _ -> SignatureTypeMarshalling.RequiredOnceLoaded instantiation
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.Byref _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Pinned _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> SignatureTypeMarshalling.Required
        | TypeDefn.Modified _ -> failwith "BUG: stripCustomModifiers left a modifier"

    /// Whether a parameter's `[MarshalAs]`, as its FieldMarshal row says, makes it need marshalling
    /// where runtime marshalling is enabled or not: where it is, any blob CoreCLR fails to parse or
    /// that names a native type other than `NATIVE_TYPE_DEFAULT` does (`ParseNativeTypeInfo`);
    /// where it is disabled, CoreCLR does not read it.
    let marshalAsRequires (runtimeMarshalling : bool) (marshal : ParameterMarshal) : bool =
        match marshal with
        | ParameterMarshal.Absent -> false
        | ParameterMarshal.Present None -> runtimeMarshalling
        | ParameterMarshal.Present (Some info) ->
            runtimeMarshalling && info.NativeType <> NativeTypeParamInfo.NativeTypeDefault
