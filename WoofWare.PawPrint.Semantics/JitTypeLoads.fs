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
    /// pointer's or byref's target too, which the JIT reads for the Swift-specific arguments
    /// (`lvaInitSpecialSwiftParam`).
    | SwiftOwnParameter
    /// The return type of the method being compiled (`getMethodInfo`, by `ConvToJitSig`).
    | OwnReturn
    /// A parameter of a method a call passes arguments to (`impPopCallArgs`: `getArgType` only).
    | CalleeParameter
    /// A parameter of an unmanaged method called by Swift's calling convention, which the JIT
    /// reads for the Swift-specific arguments, a pointer's or byref's target included
    /// (`impPopArgsForSwiftCall`: `getArgType`, then `getArgClass` for a pointer or byref).
    | SwiftCalleeParameter
    /// The return type of a method an instruction names (`getCallInfo` or `getMethodSig`, by
    /// `ConvToJitSig`).
    | CalleeReturn
    /// The return type or a parameter of a method whose address CoreCLR prepares as a function
    /// pointer, which may pun one type for another, so that it loads every value type in the
    /// signature first (`MethodDesc::PrepareForUseAsAFunctionPointer`).
    | FunctionPointerSignature
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
    /// When the JIT asks for the address (`getFunctionFixedEntryPoint`, as it morphs the
    /// `ldftn`), after: for any other `ldftn` whose address is a function pointer.
    | AfterReturnType
    /// Not at all: for any other `ldftn`.
    | Unprepared

/// What an instruction does with the method or field it names, as far as which types of the
/// member's signature the JIT loads depends on it (`JitTypeLoads.memberPositions`).
[<RequireQualifiedAccess>]
type MemberOperand =
    /// `call`, `callvirt`, `newobj` or `calli` of a method, called other than by Swift's calling
    /// convention.
    | Call
    /// A call of an unmanaged method by Swift's calling convention, on a JIT target that compiles
    /// such calls (`SWIFT_SUPPORT`: Arm64, and x64 other than Windows'), where the JIT makes the
    /// transition to unmanaged code in the caller's own code (`impCheckForPInvokeCall`). It does
    /// not in a handler, in a `try` that has a `catch` on a 64-bit target, in code it compiles
    /// for debugging, in a block it expects to run rarely, or where the VM says the call needs
    /// marshalling; there the call is `Call`, which loads less.
    | SwiftCall
    /// `jmp`.
    | Jump
    /// `ldftn` or `ldvirtftn` (`JitTypeLoads.preparation`).
    | Address of FunctionPointerPreparation
    /// An instruction that loads or stores a field, or takes its address.
    | FieldAccess
    /// `ldtoken`.
    | Token

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
    let load (position : JitTypePosition) (spelling : TypeDefn) : JitTypeLoad option =
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
        // As `OwnParameter`, with `getArgClass` of a pointer or byref, whose type loads its target.
        | JitTypePosition.SwiftOwnParameter ->
            match spelling with
            | TypeDefn.FunctionPointer _ -> None
            | spelling -> Some (JitTypeLoad.InFull spelling)
        // `getArgType`, and `getArgClass` of a pointer or byref, whose type loads its target.
        | JitTypePosition.SwiftCalleeParameter ->
            match spelling with
            | TypeDefn.Pointer _
            | TypeDefn.Byref _ -> Some (JitTypeLoad.InFull spelling)
            | spelling when isValueType spelling -> Some (JitTypeLoad.InFull spelling)
            | _ -> None
        | JitTypePosition.FunctionPointerSignature ->
            match spelling with
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _ -> Some (JitTypeLoad.InFull spelling)
            | spelling when isValueType spelling -> Some (JitTypeLoad.InFullIfDefined spelling)
            | _ -> None
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
            FunctionPointerPreparation.AfterReturnType
        | UnaryMetadataTokenIlOp.Ldftn, _, MethodAddressUse.DelegateTarget
        | UnaryMetadataTokenIlOp.Ldftn, _, MethodAddressUse.Discarded -> FunctionPointerPreparation.Unprepared
        | _ -> failwith $"JitTypeLoads.preparation was given %O{op}, which takes no method's address"

    /// The positions in the signature of the member an instruction names where the JIT meets a
    /// type, in the order it meets them, besides the member's owner
    /// (`JitTypePosition.DeclaringType`), which it loads first, for every member token, `ldtoken`'s
    /// included. A function pointer's signature is its return type and then its parameters.
    let memberPositions (operand : MemberOperand) : JitTypePosition list =
        match operand with
        | MemberOperand.Call -> [ JitTypePosition.CalleeReturn ; JitTypePosition.CalleeParameter ]
        | MemberOperand.SwiftCall -> [ JitTypePosition.CalleeReturn ; JitTypePosition.SwiftCalleeParameter ]
        | MemberOperand.Jump -> [ JitTypePosition.CalleeReturn ]
        | MemberOperand.Address FunctionPointerPreparation.BeforeReturnType ->
            [ JitTypePosition.FunctionPointerSignature ; JitTypePosition.CalleeReturn ]
        | MemberOperand.Address FunctionPointerPreparation.AfterReturnType ->
            [ JitTypePosition.CalleeReturn ; JitTypePosition.FunctionPointerSignature ]
        | MemberOperand.Address FunctionPointerPreparation.Unprepared -> [ JitTypePosition.CalleeReturn ]
        | MemberOperand.FieldAccess -> [ JitTypePosition.FieldType ]
        | MemberOperand.Token -> []
