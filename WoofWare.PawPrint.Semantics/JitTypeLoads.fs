namespace WoofWare.PawPrint

/// Where a type appears to the JIT compiling a method, for what it loads of the type there
/// (`JitTypeLoads.load`). CoreCLR's JIT interface hands the JIT only fully loaded types, loading
/// each to `CLASS_LOADED` (`vm/jitinterface.cpp`), and a load that fails fails the compilation:
/// the exception comes out of the call to the method being compiled, past every one of its
/// handlers.
[<RequireQualifiedAccess>]
type JitTypePosition =
    /// A local the method declares (`lvaInitTypeRef`: `getArgType`, then `getArgClass` for an
    /// object reference).
    | Local
    /// A parameter of the method being compiled, as its locals (`lvaInitArgs`).
    | OwnParameter
    /// A parameter of the method being compiled where that method is called by Swift's calling
    /// convention (`[UnmanagedCallersOnly]` naming `CallConvSwift`), on a JIT target that compiles
    /// such calls (`SWIFT_SUPPORT`: Arm64, and x64 other than Windows'): as `OwnParameter`, and a
    /// pointer's or byref's target and a function pointer's signature too, which the JIT reads for
    /// the Swift-specific arguments (`lvaInitSpecialSwiftParam`).
    | SwiftOwnParameter
    /// The return type of the method being compiled (`getMethodInfo`, by `ConvToJitSig`).
    | OwnReturn
    /// A parameter of a method a call passes arguments to (`impPopCallArgs`: `getArgType` only).
    | CalleeParameter
    /// The return type of a method an instruction names (`getCallInfo` or `getMethodSig`, by
    /// `ConvToJitSig`).
    | CalleeReturn
    /// The return type or a parameter of a method whose address CoreCLR prepares as a function
    /// pointer, which may pun one type for another, so that it loads every value type in the
    /// signature first (`MethodDesc::PrepareForUseAsAFunctionPointer`).
    | FunctionPointerSignature
    /// As `FunctionPointerSignature`, for a method whose address CoreCLR prepares only when the JIT
    /// asks for the address itself (`getFunctionFixedEntryPoint`, as it morphs the `ldftn`). It
    /// does not ask where it folds the address away, nor where a generic dictionary supplies it in
    /// code shared between instantiations, so these are loads it may make or not
    /// (`JitTypeLoad.Possibly`). Where it inlines the method into another, it asks while compiling
    /// that one, so that a load failing here fails the other's compilation instead.
    | RequestedFunctionPointerSignature
    /// The type of a field an instruction accesses (`getFieldInfo`).
    | FieldType
    /// The type a type token names, including a `catch` clause's (`resolveToken`).
    | TypeToken
    /// A type argument of a method a method specification instantiates (`resolveToken`).
    | MethodArgument
    /// The type a member token names as the member's owner: a method or field definition's
    /// declaring type, a member reference's parent (`resolveToken`).
    | DeclaringType

/// What the JIT loads of a type it meets (`JitTypeLoads.load`).
[<RequireQualifiedAccess>]
type JitTypeLoad =
    /// The type, in full: a reference anywhere in the load that names nothing fails it.
    | InFull of TypeDefn
    /// The type, in full, if the type reference at its head (an instantiation's generic type)
    /// names a type definition. A reference into an assembly that is there, to a type it lacks,
    /// is passed over; one into an assembly that is not there fails, as binding it must load the
    /// assembly (`ClassLoader::ResolveTokenToTypeDefThrowing`).
    | InFullIfDefined of TypeDefn
    /// What the JIT may load or not, depending on how it compiles the method: if at all, as the
    /// load inside says.
    | Possibly of JitTypeLoad

/// What becomes of the address an `ldftn` pushes.
[<RequireQualifiedAccess>]
type MethodAddressUse =
    /// It is the method argument of the delegate constructor that the `newobj` right after the
    /// `ldftn` calls, which the JIT reads straight off the `ldftn` (`fgOptimizeDelegateConstructor`).
    /// Making the delegate may then load the value types of the signature in full, for the
    /// `newobj` rather than the `ldftn`: CoreCLR sets up a thunk to shuffle the arguments of a
    /// delegate over a static method (`COMDelegate::GetDelegateCtor`), which `JitTypeLoads` does
    /// not describe.
    | DelegateTarget
    /// The `pop` right after the `ldftn` drops it, and with it the JIT's request for it.
    | Discarded
    /// Anything else: it is a function pointer.
    | FunctionPointer

/// When CoreCLR prepares the method an `ldftn` or `ldvirtftn` names as a function pointer
/// (`JitTypePosition.FunctionPointerSignature`), relative to the JIT reading its return type.
[<RequireQualifiedAccess>]
type FunctionPointerPreparation =
    /// While the JIT resolves the instruction (`getCallInfo`), before it reads the return type:
    /// for `ldvirtftn`, and for an `ldftn` of a method a method specification instantiates, whatever
    /// becomes of the address.
    | BeforeReturnType
    /// If the JIT asks for the address itself, after it reads the return type: for any other `ldftn`
    /// whose address is a function pointer (`JitTypePosition.RequestedFunctionPointerSignature`).
    | IfAddressRequested
    /// Not at all: for any other `ldftn`.
    | Unprepared

/// What an instruction does with the method or field it names, as far as which types of the
/// member's signature the JIT loads depends on it (`JitTypeLoads.memberPositions`).
[<RequireQualifiedAccess>]
type MemberOperand =
    /// `call`, `callvirt`, `newobj` or `calli` of managed code.
    | Call
    /// A call of unmanaged code: of a method CoreCLR implements by a P/Invoke, or `calli` by an
    /// unmanaged calling convention.
    | UnmanagedCall
    /// `jmp`.
    | Jump
    /// `ldftn` or `ldvirtftn` (`JitTypeLoads.preparation`).
    | Address of FunctionPointerPreparation
    /// An instruction that loads or stores a field, or takes its address.
    | FieldAccess
    /// `ldtoken`.
    | Token

/// Where the JIT meets types in the signature of the member an instruction names
/// (`JitTypeLoads.memberPositions`).
[<RequireQualifiedAccess>]
type MemberPositions =
    /// At these positions, in the order it meets them.
    | Described of JitTypePosition list
    /// At positions `JitTypeLoads` does not describe: an unmanaged call's. What the JIT reads of its
    /// signature depends on whether it makes the transition to unmanaged code in the caller's own
    /// code (`impCheckForPInvokeCall`), on the VM's walk of the signature for whether the call
    /// needs marshalling (`PInvoke::MarshalingRequired`), and on the calling convention, Swift's
    /// reading more (`impPopArgsForSwiftCall`).
    | Undescribed

/// Which types the JIT loads when it compiles a method, by where each appears.
[<RequireQualifiedAccess>]
module JitTypeLoads =

    /// Whether a spelling is a value type in the sense of a signature's element type: a type
    /// reference or definition that says it is one, or an instantiation of one. A type variable
    /// counts, because the JIT reads the argument's kind and whoever instantiated the method
    /// loaded the argument already. A kind the spelling does not record, which no signature
    /// leaves out, counts too.
    let rec private isValueType (spelling : TypeDefn) : bool =
        match spelling with
        | TypeDefn.FromReference (_, kind)
        | TypeDefn.FromDefinition (_, kind) -> kind <> System.Reflection.Metadata.SignatureTypeKind.Class
        | TypeDefn.GenericInstantiation (generic, _) -> isValueType generic
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> true
        | TypeDefn.Modified modified -> isValueType modified.Unmodified
        | TypeDefn.PrimitiveType _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Pinned _
        | TypeDefn.Pointer _
        | TypeDefn.Byref _
        | TypeDefn.FunctionPointer _
        | TypeDefn.Void -> false

    /// What the JIT loads where `spelling` appears at `position`, or `None` if it loads nothing
    /// there that could fail to load.
    ///
    /// A primitive type, `string`, `object` and `TypedReference` among them, is CoreLib's and
    /// always loaded, so it is `None`. Loading a pointer, a byref or a function pointer loads what
    /// it points to or what its signature names; loading an array loads its element type.
    ///
    /// A type variable stands for its argument, which whoever instantiated the method or type
    /// loaded in full already, so loading it again cannot fail; it is answered as loaded in full
    /// wherever CoreCLR would load an argument of the right kind.
    let rec load (position : JitTypePosition) (spelling : TypeDefn) : JitTypeLoad option =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void -> None
        | spelling ->

        match position with
        // `getArgType` loads a value type or a type variable's argument; `getArgClass` loads what
        // `getArgType` called an object reference: a class, an interface, an array. Neither loads
        // a pointer's target, a byref's or a function pointer's signature.
        | JitTypePosition.Local
        | JitTypePosition.OwnParameter ->
            let spelling =
                match spelling with
                | TypeDefn.Pinned pinned -> TypeDefn.stripCustomModifiers pinned
                | spelling -> spelling

            match spelling with
            | TypeDefn.PrimitiveType _
            | TypeDefn.Pointer _
            | TypeDefn.Byref _
            | TypeDefn.FunctionPointer _ -> None
            | spelling -> Some (JitTypeLoad.InFull spelling)
        // `getArgType` alone: only a value type.
        | JitTypePosition.CalleeParameter ->
            if isValueType spelling then
                Some (JitTypeLoad.InFull spelling)
            else
                None
        // As `OwnParameter`, with `getArgClass` of what `getArgType` calls a pointer as well: a
        // pointer, a byref or a function pointer, whose type loads its target or its signature.
        | JitTypePosition.SwiftOwnParameter -> Some (JitTypeLoad.InFull spelling)
        | JitTypePosition.FunctionPointerSignature ->
            match spelling with
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _ -> Some (JitTypeLoad.InFull spelling)
            | spelling when isValueType spelling -> Some (JitTypeLoad.InFullIfDefined spelling)
            | _ -> None
        | JitTypePosition.RequestedFunctionPointerSignature ->
            load JitTypePosition.FunctionPointerSignature spelling
            |> Option.map JitTypeLoad.Possibly
        // `ConvToJitSig`, `getFieldInfo` and `resolveToken` load every type but a primitive.
        | JitTypePosition.OwnReturn
        | JitTypePosition.CalleeReturn
        | JitTypePosition.FieldType
        | JitTypePosition.TypeToken
        | JitTypePosition.MethodArgument
        | JitTypePosition.DeclaringType -> Some (JitTypeLoad.InFull spelling)

    /// When CoreCLR prepares as a function pointer the method that `op`, an `ldftn` or
    /// `ldvirtftn`, names by `token`, whose address has the use `addressUse`. `addressUse` says
    /// nothing of an `ldvirtftn`, whose address CoreCLR prepares whatever becomes of it.
    let preparation
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        (addressUse : MethodAddressUse)
        : FunctionPointerPreparation
        =
        match op, token, addressUse with
        | UnaryMetadataTokenIlOp.Ldvirtftn, _, _
        | UnaryMetadataTokenIlOp.Ldftn, MetadataToken.MethodSpecification _, _ ->
            FunctionPointerPreparation.BeforeReturnType
        | UnaryMetadataTokenIlOp.Ldftn, _, MethodAddressUse.FunctionPointer ->
            FunctionPointerPreparation.IfAddressRequested
        | UnaryMetadataTokenIlOp.Ldftn, _, MethodAddressUse.DelegateTarget
        | UnaryMetadataTokenIlOp.Ldftn, _, MethodAddressUse.Discarded -> FunctionPointerPreparation.Unprepared
        | _ -> failwith $"JitTypeLoads.preparation was given %O{op}, which takes no method's address"

    /// Where in the signature of the member an instruction names the JIT meets a type, in the order
    /// it meets them, besides the member's owner
    /// (`JitTypePosition.DeclaringType`), which it loads first, for every member token, `ldtoken`'s
    /// included. A function pointer's signature is its return type and then its parameters.
    let memberPositions (operand : MemberOperand) : MemberPositions =
        match operand with
        | MemberOperand.Call ->
            MemberPositions.Described [ JitTypePosition.CalleeReturn ; JitTypePosition.CalleeParameter ]
        | MemberOperand.UnmanagedCall -> MemberPositions.Undescribed
        | MemberOperand.Jump -> MemberPositions.Described [ JitTypePosition.CalleeReturn ]
        | MemberOperand.Address FunctionPointerPreparation.BeforeReturnType ->
            MemberPositions.Described [ JitTypePosition.FunctionPointerSignature ; JitTypePosition.CalleeReturn ]
        | MemberOperand.Address FunctionPointerPreparation.IfAddressRequested ->
            MemberPositions.Described
                [
                    JitTypePosition.CalleeReturn
                    JitTypePosition.RequestedFunctionPointerSignature
                ]
        | MemberOperand.Address FunctionPointerPreparation.Unprepared ->
            MemberPositions.Described [ JitTypePosition.CalleeReturn ]
        | MemberOperand.FieldAccess -> MemberPositions.Described [ JitTypePosition.FieldType ]
        | MemberOperand.Token -> MemberPositions.Described []
