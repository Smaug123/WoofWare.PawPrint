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

/// How an instruction that takes a method's address uses it.
[<RequireQualifiedAccess>]
type MethodAddressUse =
    /// As the method argument of the delegate constructor that the `newobj` next to it calls,
    /// which the JIT reads straight off the `ldftn` (`fgOptimizeDelegateConstructor`). Making the
    /// delegate may then load the value types of the signature in full, for the `newobj` rather
    /// than the `ldftn`: CoreCLR sets up a thunk to shuffle the arguments of a delegate over a
    /// static method (`COMDelegate::GetDelegateCtor`), which `JitTypeLoads` does not describe.
    | DelegateTarget
    /// Any other way: stored, passed to a method, or the delegate constructor reached otherwise.
    | FunctionPointer

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

        if true then
            Some (JitTypeLoad.InFull spelling)
        else

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

    /// The positions in the signature of the member that `op`, naming `token`, names where the JIT
    /// meets a type, besides the member's owner (`JitTypePosition.DeclaringType`), which it loads
    /// for every member token, `ldtoken`'s included. Empty for an instruction naming a type.
    ///
    /// A call passes arguments, so its callee's parameters count; `jmp` reads only the return
    /// type, and `ldtoken` neither. `ldftn` and `ldvirtftn` read the return type, and CoreCLR
    /// prepares the method as a function pointer (`JitTypePosition.FunctionPointerSignature`)
    /// unless `addressUse` is `DelegateTarget` for an `ldftn` of a method no method specification
    /// instantiates: `getCallInfo` prepares a method `ldvirtftn` names, or one a method
    /// specification instantiates, whatever becomes of its address. `addressUse` says nothing of
    /// any other instruction.
    let memberPositions
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        (addressUse : MethodAddressUse)
        : JitTypePosition list
        =
        match op with
        | UnaryMetadataTokenIlOp.Call
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Newobj -> [ JitTypePosition.CalleeReturn ; JitTypePosition.CalleeParameter ]
        | UnaryMetadataTokenIlOp.Jmp -> [ JitTypePosition.CalleeReturn ]
        | UnaryMetadataTokenIlOp.Ldftn ->
            match token, addressUse with
            | MetadataToken.MethodSpecification _, _
            | _, MethodAddressUse.FunctionPointer ->
                [ JitTypePosition.CalleeReturn ; JitTypePosition.FunctionPointerSignature ]
            | _, MethodAddressUse.DelegateTarget -> [ JitTypePosition.CalleeReturn ]
        | UnaryMetadataTokenIlOp.Ldvirtftn ->
            [ JitTypePosition.CalleeReturn ; JitTypePosition.FunctionPointerSignature ]
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stsfld -> [ JitTypePosition.FieldType ]
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Ldelema
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Ldelem
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> []
