namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

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
        /// `None` where no base convention is named.
        Base : UnmanagedCallConvBase option
        Modifiers : Set<UnmanagedCallConvModifier>
    }

/// What CoreCLR reads as the unmanaged calling convention of a call of unmanaged code (a P/Invoke,
/// or `calli` by an unmanaged calling convention), or of an `[UnmanagedCallersOnly]` method
/// (`getUnmanagedCallConv`, `vm/jitinterface.cpp`), as the JIT compiles the call or the method.
[<RequireQualifiedAccess>]
type UnmanagedConventionRead =
    /// It reads this base convention, the platform's default where nothing it reads names one,
    /// with the modifiers it hands the JIT alongside: the member-function modifier, which makes the
    /// base convention its member-function variant
    /// (`GetMemberFunctionUnmanagedCallingConventionVariant`, the same convention for Thiscall and
    /// Swift), and, for a call, the GC-transition modifier where it has the JIT suppress the
    /// transition (`pSuppressGCTransition`).
    | Reads of baseConvention : UnmanagedCallConvBase * modifiers : Set<UnmanagedCallConvModifier>
    /// A signature by the `unmanaged` calling convention whose return type's modifiers name a second
    /// base convention: CoreCLR raises `InvalidProgramException` (`IDS_EE_MULTIPLE_CALLCONV_UNSUPPORTED`),
    /// failing the compilation. Only a call site's signature, or a P/Invoke's own, gives this.
    | MultipleBases
    /// CoreCLR cannot parse the blob of the attribute that names the conventions, and the
    /// compilation raises the exception of the failure's `HRESULT` (a `COMException` for most such
    /// blobs, an `OverflowException` where a string runs past the blob's end).
    | Unreadable of reason : string
    /// The attribute says `CallConvs = null`: CoreCLR reads the elements of the null array it
    /// parsed, and the compilation faults in the runtime, which kills the process.
    | NullArray

[<RequireQualifiedAccess>]
module UnmanagedCallingConvention =
    /// No convention named.
    let empty : UnmanagedCallConvs =
        {
            Base = None
            Modifiers = Set.empty
        }

    /// The convention CoreCLR takes where nothing names a base convention
    /// (`GetDefaultUnmanagedCallingConvention`): C on the Unix runtime the analysis answers for.
    let platformDefault : UnmanagedCallConvBase = UnmanagedCallConvBase.Cdecl

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

    /// The namespace of the calling-convention types (`CMOD_CALLCONV_NAMESPACE`).
    let private conventionNamespace : string = "System.Runtime.CompilerServices"

    /// Add the convention of the first of `bases` or `modifiers` whose name `matches` to
    /// `conventions`, as `ProcessName` does; `None` where it is a second base convention, which
    /// CoreCLR takes as an error. A name matching none adds nothing.
    let private add (matches : string -> bool) (conventions : UnmanagedCallConvs) : UnmanagedCallConvs option =
        match bases |> List.tryFind (fun (name, _) -> matches name) with
        | Some (_, convention) ->
            match conventions.Base with
            | Some _ -> None
            | None ->
                Some
                    { conventions with
                        Base = Some convention
                    }
        | None ->

        match modifiers |> List.tryFind (fun (name, _) -> matches name) with
        | Some (_, modifier) ->
            Some
                { conventions with
                    Modifiers = Set.add modifier conventions.Modifiers
                }
        | None -> Some conventions

    /// Whether the serialised type name `serialised` names the type `System.Runtime.CompilerServices.<name>`:
    /// it is that name alone, as for a type of the attribute's own assembly, or that name followed by
    /// a comma and an assembly, any assembly (`BeginsWithTypeName`).
    let private namesType (name : string) (serialised : string) : bool =
        let full = conventionNamespace + "." + name

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
        | Some typeName -> add (fun name -> namesType name typeName) conventions

    /// The conventions the serialised type names of an attribute's `CallConvs` name, or `None` where
    /// they name a second base convention (`TryGetCallingConventionFromTypeArray`).
    let private ofTypeNames (typeNames : string option list) : UnmanagedCallConvs option =
        (Some empty, typeNames)
        ||> List.fold (fun conventions typeName -> conventions |> Option.bind (addFullyQualifiedTypeName typeName))

    /// The base convention and modifiers CoreCLR hands the JIT for `conventions`, or `None` where
    /// they name neither a base convention nor the member-function modifier
    /// (`CallConvBuilder::GetCurrentCallConv` answering `UnsetValue`). The member-function modifier
    /// alone stands for the platform's default.
    let private current
        (conventions : UnmanagedCallConvs)
        : (UnmanagedCallConvBase * Set<UnmanagedCallConvModifier>) option
        =
        match conventions.Base with
        | Some convention -> Some (convention, conventions.Modifiers)
        | None when conventions.Modifiers.Contains UnmanagedCallConvModifier.MemberFunction ->
            Some (platformDefault, conventions.Modifiers)
        | None -> None

    /// As `current`, taking the platform's default where `conventions` name no base convention.
    let private currentOrDefault
        (conventions : UnmanagedCallConvs)
        : UnmanagedCallConvBase * Set<UnmanagedCallConvModifier>
        =
        current conventions
        |> Option.defaultValue (platformDefault, conventions.Modifiers)

    /// What CoreCLR reads from `conventions`, taking the platform's default where they name no base
    /// convention.
    let private readOrDefault (conventions : UnmanagedCallConvs) : UnmanagedConventionRead =
        UnmanagedConventionRead.Reads (currentOrDefault conventions)

    /// The namespace and name of the type a modifier in a signature of `assembly` names, where it
    /// is a type reference or definition (`GetNameOfTypeRefOrDef`); `None` for any other spelling,
    /// which CoreCLR passes over.
    let private modifierName (assembly : DumpedAssembly) (modifier : TypeDefn) : (string * string) option =
        match modifier with
        | TypeDefn.FromReference (typeRef, _) -> Some (typeRef.Namespace, typeRef.Name)
        | TypeDefn.FromDefinition (identity, _) ->
            // A signature's type definition token names a row of its own module's table.
            if identity.AssemblyFullName <> assembly.DefinitionFullName then
                failwith
                    $"BUG: a signature of %s{assembly.DefinitionFullName} names a type definition of %s{identity.AssemblyFullName}"

            let definition = assembly.TypeDefs.[identity.TypeDefinition.Get]
            Some (definition.Namespace, definition.Name)
        | _ -> None

    /// The conventions the modifiers before the return type of `signature`, spelled in `assembly`,
    /// name, outermost first (`CallConv::TryGetUnmanagedCallingConventionFromModOpt`): CoreCLR reads
    /// each optional modifier of `System.Runtime.CompilerServices` by its name alone, passing over a
    /// required one. `Error` where a modifier names a second base convention, with the conventions
    /// those before it named.
    let ofModifiers
        (assembly : DumpedAssembly)
        (signature : TypeMethodSignature<TypeDefn>)
        : Result<UnmanagedCallConvs, UnmanagedCallConvs>
        =
        let rec walk (conventions : UnmanagedCallConvs) (spelling : TypeDefn) =
            match spelling with
            | TypeDefn.Modified modified ->
                let named =
                    if modified.IsRequired then
                        None
                    else
                        match modifierName assembly modified.Modifier with
                        | Some (ns, name) when ns = conventionNamespace -> Some name
                        | _ -> None

                match named with
                | None -> walk conventions modified.Unmodified
                | Some typeName ->
                    match add (fun name -> name = typeName) conventions with
                    | Some conventions -> walk conventions modified.Unmodified
                    | None -> Error conventions
            | _ -> Ok conventions

        match signature.ReturnType with
        | MethodReturnType.Returns returned -> walk empty returned
        | MethodReturnType.Void -> Ok empty

    /// The calling convention of a native variable argument list (`IMAGE_CEE_CS_CALLCONV_NATIVEVARARG`),
    /// which `SignatureCallingConvention` does not name.
    let private nativeVarArgs : SignatureCallingConvention =
        LanguagePrimitives.EnumOfValue<byte, SignatureCallingConvention> 0xBuy

    /// What CoreCLR reads as the convention of `signature`, spelled in `assembly`, by an unmanaged
    /// calling convention: a `calli`'s call-site signature, or a P/Invoke's own where it names one
    /// (`getUnmanagedCallConvForSig`). A legacy convention names its base convention outright, and
    /// CoreCLR reads no modifier; only the `unmanaged` convention names its conventions by modifiers.
    let ofCallSite (assembly : DumpedAssembly) (signature : TypeMethodSignature<TypeDefn>) : UnmanagedConventionRead =
        match signature.Header.Get.CallingConvention with
        | SignatureCallingConvention.CDecl -> UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Cdecl, Set.empty)
        | SignatureCallingConvention.StdCall -> UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Stdcall, Set.empty)
        | SignatureCallingConvention.ThisCall ->
            UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Thiscall, Set.empty)
        | SignatureCallingConvention.FastCall ->
            UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Fastcall, Set.empty)
        | SignatureCallingConvention.Unmanaged ->
            match ofModifiers assembly signature with
            | Ok conventions -> readOrDefault conventions
            | Error _ -> UnmanagedConventionRead.MultipleBases
        | convention when convention = nativeVarArgs ->
            UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Cdecl, Set.empty)
        | convention -> failwith $"BUG: %O{convention} is not an unmanaged calling convention"

    /// The attribute that names a P/Invoke's calling conventions where its import does not.
    let private unmanagedCallConv : string =
        "System.Runtime.InteropServices.UnmanagedCallConvAttribute"

    /// The attribute that has the JIT suppress the GC transition of a call of a P/Invoke.
    let private suppressGcTransition : string =
        "System.Runtime.InteropServices.SuppressGCTransitionAttribute"

    /// The base convention `definition`'s import names, or `None` where it names none: it has no
    /// import, or its bits say `WinApi`, zero or a value out of range, all of which CoreCLR reads as
    /// naming none (`GetCallConvValueForPInvokeCallConv`).
    let private importConvention
        (definition : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : UnmanagedCallConvBase option
        =
        match definition.TryNativeImport with
        | None -> None
        | Some import ->
            match import.Attributes &&& MethodImportAttributes.CallingConventionMask with
            | MethodImportAttributes.CallingConventionCDecl -> Some UnmanagedCallConvBase.Cdecl
            | MethodImportAttributes.CallingConventionStdCall -> Some UnmanagedCallConvBase.Stdcall
            | MethodImportAttributes.CallingConventionThisCall -> Some UnmanagedCallConvBase.Thiscall
            | MethodImportAttributes.CallingConventionFastCall -> Some UnmanagedCallConvBase.Fastcall
            | _ -> None

    /// What CoreCLR reads as the convention of a call of `method`, a P/Invoke of `assembly`
    /// (`getUnmanagedCallConvForMethod`). Where its signature names an unmanaged convention, that
    /// one (`ofCallSite`). Otherwise the first of these that names one
    /// (`PInvoke::GetCallingConvention_IgnoreErrors`): its import's; its first
    /// `[UnmanagedCallConv]`'s, which counts for nothing where it names a second base convention;
    /// its signature's modifiers', up to any naming a second base convention; and otherwise the
    /// platform's default. An `[UnmanagedCallConv]` CoreCLR cannot parse fails the reading.
    let ofPInvoke (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : UnmanagedConventionRead =
        let definition = assembly.Methods.[method]

        match definition.Signature.Header.Get.CallingConvention with
        | SignatureCallingConvention.Default
        | SignatureCallingConvention.VarArgs ->
            // CoreCLR has the JIT suppress the GC transition where the P/Invoke carries
            // `[SuppressGCTransition]` (`HasSuppressGCTransitionAttribute`), or where the
            // `[UnmanagedCallConv]` it reads names the modifier, whichever convention it then takes;
            // never for the modifiers of its signature.
            let reads
                (suppressed : bool)
                (convention : UnmanagedCallConvBase, modifiers : Set<UnmanagedCallConvModifier>)
                =
                let modifiers = Set.remove UnmanagedCallConvModifier.SuppressGcTransition modifiers

                let modifiers =
                    if suppressed then
                        Set.add UnmanagedCallConvModifier.SuppressGcTransition modifiers
                    else
                        modifiers

                UnmanagedConventionRead.Reads (convention, modifiers)

            let suppressed =
                not (NamedAttribute.ofMethod suppressGcTransition assembly method).IsEmpty

            match importConvention definition with
            | Some convention -> reads suppressed (convention, Set.empty)
            | None ->

            // What the attribute names, where CoreCLR reads it: the convention it hands the JIT, if
            // any, and the attribute's modifiers.
            let ofAttribute =
                match NamedAttribute.ofMethod unmanagedCallConv assembly method with
                | [] -> Ok None
                | first :: _ ->
                    match CustomAttribute.parseUnmanagedCallConv first.Value with
                    | UnmanagedCallConvApplication.Malformed reason -> Error (UnmanagedConventionRead.Unreadable reason)
                    | UnmanagedCallConvApplication.Parsed CallConvsArgument.Unnamed -> Ok None
                    | UnmanagedCallConvApplication.Parsed CallConvsArgument.NullArray ->
                        Error UnmanagedConventionRead.NullArray
                    | UnmanagedCallConvApplication.Parsed (CallConvsArgument.Types typeNames) ->
                        Ok (
                            ofTypeNames typeNames
                            |> Option.map (fun conventions -> current conventions, conventions)
                        )

            match ofAttribute with
            | Error read -> read
            | Ok attribute ->

            let suppressed =
                suppressed
                || match attribute with
                   | Some (_, conventions) ->
                       conventions.Modifiers.Contains UnmanagedCallConvModifier.SuppressGcTransition
                   | None -> false

            match attribute with
            | Some (Some named, _) -> reads suppressed named
            | Some (None, _)
            | None ->

            // A second base convention among the modifiers ends the reading, keeping what came
            // before it.
            match ofModifiers assembly definition.Signature with
            | Ok conventions
            | Error conventions -> reads suppressed (currentOrDefault conventions)
        | _ -> ofCallSite assembly definition.Signature

    /// Whether CoreCLR, describing `method`, a P/Invoke of `assembly`, as it begins its walk for
    /// whether a call of it needs marshalling, refuses the conventions it names
    /// (`PInvokeStaticSigInfo::InitCallConv`), raising `TypeLoadException`. It refuses them where its
    /// import names no convention and its first `[UnmanagedCallConv]` names a second base
    /// convention; where its signature's modifiers name a second base convention, whatever the
    /// signature's calling convention; and where the import or the attribute names a convention and
    /// the modifiers name another, a member-function variant counting as a convention of its own.
    /// An `[UnmanagedCallConv]` that does not parse, or says `CallConvs = null`, fails the JIT's
    /// reading of the convention first (`ofPInvoke`), and this answers `false` for it.
    let describingRefuses (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        let definition = assembly.Methods.[method]

        // The convention as `CallConvBuilder::GetCurrentCallConv` answers it, which CoreCLR
        // compares: the base convention, and whether the member-function modifier makes it a
        // variant of its own.
        let extension (convention : UnmanagedCallConvBase, modifiers : Set<UnmanagedCallConvModifier>) =
            let variant =
                modifiers.Contains UnmanagedCallConvModifier.MemberFunction
                && convention <> UnmanagedCallConvBase.Thiscall
                && convention <> UnmanagedCallConvBase.Swift

            convention, variant

        let named =
            match importConvention definition with
            | Some convention -> Ok (Some (convention, Set.empty))
            | None ->
                match NamedAttribute.ofMethod unmanagedCallConv assembly method with
                | [] -> Ok None
                | first :: _ ->
                    match CustomAttribute.parseUnmanagedCallConv first.Value with
                    | UnmanagedCallConvApplication.Parsed (CallConvsArgument.Types typeNames) ->
                        match ofTypeNames typeNames with
                        | Some conventions -> Ok (current conventions)
                        | None -> Error ()
                    | UnmanagedCallConvApplication.Parsed CallConvsArgument.Unnamed
                    | UnmanagedCallConvApplication.Parsed CallConvsArgument.NullArray
                    | UnmanagedCallConvApplication.Malformed _ -> Ok None

        match named, ofModifiers assembly definition.Signature with
        | Error (), _
        | _, Error _ -> true
        | Ok (Some named), Ok modifiers ->
            match current modifiers with
            | Some fromModifiers -> extension named <> extension fromModifiers
            | None -> false
        | Ok None, Ok _ -> false

    /// Whether `runtime`'s `UnmanagedCallersOnlyAttribute` declares the `AssociatedSourceType` named
    /// argument, which its VM accepts in the attribute's blob (`namedArgs`,
    /// `CallConv::TryGetCallingConventionFromUnmanagedCallersOnly`).
    let declaresAssociatedSourceType (runtime : EmulatedRuntime) : bool =
        match runtime with
        | EmulatedRuntime.Net10 -> false
        | EmulatedRuntime.Net11 -> true

    /// What `runtime`'s CoreCLR reads as the convention of a method whose `[UnmanagedCallersOnly]`
    /// is `application`, where the method's signature has the default or vararg calling convention,
    /// which ECMA-335 gives every method definition (`getUnmanagedCallConvForMethod`). Where the
    /// attribute names no `CallConvs`, or names a second base convention,
    /// `CallConv::TryGetCallingConventionFromUnmanagedCallersOnly` answers false and CoreCLR takes
    /// the platform's default; this never answers `MultipleBases`. CoreCLR reads another
    /// convention, which only hand-written IL can give the method, from the signature instead.
    let ofCallersOnly
        (runtime : EmulatedRuntime)
        (application : UnmanagedCallersOnlyApplication)
        : UnmanagedConventionRead
        =
        match application with
        | UnmanagedCallersOnlyApplication.Malformed reason -> UnmanagedConventionRead.Unreadable reason
        | UnmanagedCallersOnlyApplication.Parsed (_, true) when not (declaresAssociatedSourceType runtime) ->
            UnmanagedConventionRead.Unreadable
                $"The blob names AssociatedSourceType, which %A{runtime}'s attribute does not declare"
        | UnmanagedCallersOnlyApplication.Parsed (callConvs, _) ->

        match callConvs with
        | CallConvsArgument.Unnamed -> UnmanagedConventionRead.Reads (platformDefault, Set.empty)
        | CallConvsArgument.NullArray -> UnmanagedConventionRead.NullArray
        | CallConvsArgument.Types typeNames ->
            match ofTypeNames typeNames with
            | Some conventions ->
                // CoreCLR suppresses no GC transition for the method's own convention.
                let convention, modifiers = currentOrDefault conventions

                UnmanagedConventionRead.Reads (
                    convention,
                    Set.remove UnmanagedCallConvModifier.SuppressGcTransition modifiers
                )
            | None -> UnmanagedConventionRead.Reads (platformDefault, Set.empty)

    /// Whether CoreCLR's JIT for `target` compiles calls by Swift's calling convention
    /// (`SWIFT_SUPPORT`): on Arm64 it does, and on x64 it does for the Unix ABI
    /// (`UNIX_AMD64_ABI`) only. `JitTarget` names no operating system, and this answers for a
    /// Unix runtime; Windows x64 is not modelled.
    let compilesSwiftCalls (target : JitTarget) : bool =
        match target with
        | JitTarget.Arm64
        | JitTarget.X64 -> true

    /// Whether CoreCLR's JIT for `target` compiles a call or a method whose convention CoreCLR reads
    /// as `read` by Swift's convention: a base convention of Swift, whatever modifiers come with it,
    /// on a target that compiles Swift calls.
    let isSwift (target : JitTarget) (read : UnmanagedConventionRead) : bool =
        match read with
        | UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Swift, _) -> compilesSwiftCalls target
        | UnmanagedConventionRead.Reads _
        | UnmanagedConventionRead.MultipleBases
        | UnmanagedConventionRead.Unreadable _
        | UnmanagedConventionRead.NullArray -> false

    /// Whether CoreCLR's JIT refuses a call of unmanaged code whose convention CoreCLR reads as
    /// `read` and whose signature, the method's or the call site's, is `signature`, as it reads the
    /// convention (`BADCODE("thiscall with 0 arguments")`, `impCheckForPInvokeCall`): by Thiscall's
    /// convention, or its member-function variant, which is the same, the call must pass an
    /// argument besides any `this`. The JIT fails the compilation (`CORJIT_BADCODE`), and the VM
    /// raises `InvalidProgramException`, after the JIT loads the return type and before it meets
    /// anything else of the signature, wherever the call is.
    let refusesThiscallWithoutArguments
        (read : UnmanagedConventionRead)
        (signature : TypeMethodSignature<TypeDefn>)
        : bool
        =
        match read with
        | UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Thiscall, _) -> signature.ParameterTypes.IsEmpty
        | UnmanagedConventionRead.Reads _
        | UnmanagedConventionRead.MultipleBases
        | UnmanagedConventionRead.Unreadable _
        | UnmanagedConventionRead.NullArray -> false

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
    /// the pointer need marshalling where runtime marshalling is enabled, by the full names CoreCLR
    /// compares (`IsTypeRefOrDef`).
    let private copyConstructorModifiers : Set<string> =
        Set.ofList
            [
                "Microsoft.VisualC.NeedsCopyConstructorModifier"
                "System.Runtime.CompilerServices.IsCopyConstructed"
            ]

    /// The full name of a type `assembly` spells by a reference or a definition: its namespace and
    /// name joined with a dot, or its name alone where the namespace is empty, which is how CoreCLR
    /// compares a name with one the metadata splits (`IsTypeRefOrDef`).
    let private spelledName (assembly : DumpedAssembly) (spelling : TypeDefn) : string option =
        let joined (ns : string) (name : string) : string =
            if ns = "" then name else ns + "." + name

        match spelling with
        | TypeDefn.FromReference (typeRef, _) -> Some (joined typeRef.Namespace typeRef.Name)
        | TypeDefn.FromDefinition (identity, _) when identity.AssemblyFullName = assembly.DefinitionFullName ->
            let definition = assembly.TypeDefs.[identity.TypeDefinition.Get]
            Some (joined definition.Namespace definition.Name)
        | _ -> None

    /// The names of the required modifiers on `spelling`, outermost first, and what they modify.
    let rec private requiredModifiers (assembly : DumpedAssembly) (spelling : TypeDefn) : string list * TypeDefn =
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

    /// The index of the parameter that CoreCLR passes the locale ID in, as `GetLCIDParameterIndex`
    /// reads it from the first of `attributes`, a method's `[LCIDConversion]` applications: the
    /// blob's one `int` argument, or -1, meaning none, where the method carries none or CoreCLR
    /// cannot parse the blob (`ParseKnownCaArgs`). Any index but -1 needs marshalling.
    let lcidParameterIndex (attributes : WoofWare.PawPrint.CustomAttribute list) : int =
        match attributes with
        | [] -> -1
        | first :: _ ->
            match CustomAttribute.readFixedArgs [ CustomAttribArgShape.Primitive PrimitiveType.Int32 ] first.Value with
            | Ok ([ CustomAttribFixedArg.I4 index ], _) -> index
            | Ok _
            | Error _ -> -1
