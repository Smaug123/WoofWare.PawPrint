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
