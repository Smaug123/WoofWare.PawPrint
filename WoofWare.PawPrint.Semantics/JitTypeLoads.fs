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
    /// convention, as CoreCLR reads it from the method's `[UnmanagedCallersOnly]`, on a JIT target
    /// that compiles such calls (`UnmanagedCallingConvention.isSwift`): as `OwnParameter`, and a
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
    /// As `FunctionPointerSignature`, for a method CoreCLR prepares only on a condition the
    /// compilation decides, so these are loads the JIT may make or not (`JitTypeLoad.Possibly`).
    /// One condition is that the JIT asks for the address itself (`getFunctionFixedEntryPoint`, as
    /// it morphs the `ldftn`), which it does not where it folds the address away, nor where a
    /// generic dictionary supplies it in code shared between instantiations; where it inlines the
    /// method into another, it asks while compiling that one, so that a load failing here fails
    /// the other's compilation instead. The other is that the JIT resolves which implementation of
    /// a static virtual method a `constrained.` prefix names (`getCallInfo`), which it does not
    /// where the type is one of the type variables of code shared between instantiations.
    | PossibleFunctionPointerSignature
    /// The return type or a parameter of a delegate's `Invoke` method, or of the method a delegate
    /// open over it is made over (`JitTypeLoads.delegateClosure`), which CoreCLR walks to make a thunk
    /// that shuffles the arguments of each call, loading every value type approximately to lay them
    /// out (`COMDelegate::GetDelegateCtor`, `SetupShuffleThunk`, `ArgIterator`). It walks the
    /// `Invoke` method's signature and then the target's, each its return type and then its
    /// parameters, as each method's own definition spells it.
    /// CoreCLR keeps the thunk on the delegate type, so a later delegate of that type walks neither;
    /// where the IL is verifiable, each value type of the target's signature is one of the `Invoke`
    /// signature's, so walking it again would load nothing new.
    | ShuffleThunkSignature
    /// A parameter of a method a call of unmanaged code passes arguments to, which the VM's walk of
    /// the signature for whether the call needs marshalling may reach (`PInvoke::MarshalingRequired`,
    /// `vm/dllimport.cpp`). The walk loads a value type or an instantiation in full; a pointer, a
    /// function pointer or a primitive it passes over without loading anything, and it ends at any
    /// other type without loading it. The JIT asks for the walk only where it may make the
    /// transition to unmanaged code in the caller's own code (`impCheckForPInvokeCall`), so these
    /// are loads it may make or not (`JitTypeLoad.Possibly`).
    | MarshallingWalk
    /// A parameter of a method a call by Swift's calling convention passes arguments to, on a JIT
    /// target that compiles such calls (`SWIFT_SUPPORT`: Arm64, and x64 other than Windows'), where
    /// the JIT makes the transition to unmanaged code in the caller's own code: it reads what a
    /// pointer points to and a function pointer's signature, for the Swift-specific arguments
    /// (`impPopArgsForSwiftCall`). It does so only where the walk finds that nothing in the
    /// signature needs marshalling, so these are loads it may make or not (`JitTypeLoad.Possibly`).
    | SwiftCalleeParameter
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
    /// The type, approximately (`CLASS_LOAD_APPROXPARENTS`): an instantiation's generic definition
    /// and, of its arguments, the value types, each approximately; a definition's base and
    /// interfaces approximately and the types of its value-type fields, but nothing a later level
    /// loads, such as an interface's type arguments or a class argument of an instantiation.
    | Approximately of TypeDefn
    /// What the JIT may load or not, depending on how it compiles the method: if at all, as the
    /// load inside says.
    | Possibly of JitTypeLoad

/// What becomes of the address an `ldftn` pushes.
[<RequireQualifiedAccess>]
type MethodAddressUse =
    /// It is the method argument of the delegate constructor that the `newobj` right after the
    /// `ldftn` calls, which the JIT reads straight off the `ldftn` (`fgOptimizeDelegateConstructor`).
    /// Making a delegate open over its target then loads, for the `newobj` rather than the `ldftn`,
    /// the types at `JitTypePosition.ShuffleThunkSignature` (`JitTypeLoads.delegateClosure`).
    | DelegateTarget
    /// The `pop` right after the `ldftn` drops it, and with it the JIT's request for it.
    | Discarded
    /// Anything else: it is a function pointer.
    | FunctionPointer

/// Whether a delegate closes over the first argument of the method it is made over, which decides
/// whether CoreCLR makes a thunk to shuffle the arguments of each call
/// (`COMDelegate::GetDelegateCtor`).
[<RequireQualifiedAccess>]
type DelegateClosure =
    /// Its `Invoke` method takes as many arguments as the target does, counting an instance
    /// method's `this`: the target is a static method, or an instance method whose receiver each
    /// call passes. A thunk drops the delegate from each call's arguments, so CoreCLR walks both
    /// signatures to make one (`JitTypePosition.ShuffleThunkSignature`).
    | Open
    /// Its `Invoke` method takes any other number of arguments, one fewer where the IL is
    /// verifiable: the delegate holds the target's first argument, an instance method's receiver
    /// or a static method's first parameter, and passes it in the delegate's place, needing no thunk.
    | Closed

/// Whether an `ldftn` has a `constrained.` prefix, naming the type whose implementation of a static
/// virtual method it takes the address of.
[<RequireQualifiedAccess>]
type AddressConstraint =
    /// No prefix.
    | Unconstrained
    /// A `constrained.` prefix.
    | Constrained

/// When CoreCLR prepares the method an `ldftn` or `ldvirtftn` names as a function pointer
/// (`JitTypePosition.FunctionPointerSignature`), relative to the JIT reading its return type.
[<RequireQualifiedAccess>]
type FunctionPointerPreparation =
    /// While the JIT resolves the instruction (`getCallInfo`), before it reads the return type:
    /// for `ldvirtftn`, and for an `ldftn` of a method a method specification instantiates, whatever
    /// becomes of the address, unless a `constrained.` prefix names the implementation.
    | BeforeReturnType
    /// As `BeforeReturnType`, if the JIT resolves which implementation the `constrained.` prefix
    /// names: for an `ldftn` of a static virtual method a method specification instantiates
    /// (`JitTypePosition.PossibleFunctionPointerSignature`).
    | IfConstraintResolved
    /// If the JIT asks for the address itself, after it reads the return type: for any other `ldftn`
    /// whose address is a function pointer (`JitTypePosition.PossibleFunctionPointerSignature`).
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
    | UnmanagedCall of UnmanagedCall
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
    /// At positions given together with the types of the signature (`JitTypeLoads.unmanagedMeetings`):
    /// an unmanaged call's, as CoreCLR's walk of its signature ends at the first type that needs
    /// marshalling.
    | BySignature

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
        | JitTypePosition.PossibleFunctionPointerSignature ->
            load JitTypePosition.FunctionPointerSignature spelling
            |> Option.map JitTypeLoad.Possibly
        | JitTypePosition.ShuffleThunkSignature ->
            if isValueType spelling then
                Some (JitTypeLoad.Approximately spelling)
            else
                None
        // The walk ends at a type variable, an element type its table does not pass over.
        | JitTypePosition.MarshallingWalk ->
            match spelling with
            | TypeDefn.GenericTypeParameter _
            | TypeDefn.GenericMethodParameter _ -> None
            | TypeDefn.GenericInstantiation _ -> Some (JitTypeLoad.Possibly (JitTypeLoad.InFull spelling))
            | spelling when isValueType spelling -> Some (JitTypeLoad.Possibly (JitTypeLoad.InFull spelling))
            | _ -> None
        // A value type `CalleeParameter` loads already; a byref ends the walk, so the JIT never
        // reads one here.
        | JitTypePosition.SwiftCalleeParameter ->
            match spelling with
            | TypeDefn.Pointer _
            | TypeDefn.FunctionPointer _ -> Some (JitTypeLoad.Possibly (JitTypeLoad.InFull spelling))
            | _ -> None
        // `ConvToJitSig`, `getFieldInfo` and `resolveToken` load every type but a primitive.
        | JitTypePosition.OwnReturn
        | JitTypePosition.CalleeReturn
        | JitTypePosition.FieldType
        | JitTypePosition.TypeToken
        | JitTypePosition.MethodArgument
        | JitTypePosition.DeclaringType -> Some (JitTypeLoad.InFull spelling)

    /// Whether a delegate whose `Invoke` method has `invokeParameters` parameters closes over the
    /// first argument of a target with `targetParameters` parameters, static or not
    /// (`COMDelegate::GetDelegateCtor`).
    let delegateClosure (invokeParameters : int) (targetParameters : int) (targetIsStatic : bool) : DelegateClosure =
        let targetArguments =
            if targetIsStatic then
                targetParameters
            else
                targetParameters + 1

        if invokeParameters = targetArguments then
            DelegateClosure.Open
        else
            DelegateClosure.Closed

    /// When CoreCLR prepares as a function pointer the method that `op`, an `ldftn` or
    /// `ldvirtftn`, names by `token`, with or without a `constrained.` prefix, whose address has
    /// the use `addressUse`. `addressUse` says nothing of an `ldvirtftn`, whose address CoreCLR
    /// prepares whatever becomes of it.
    let preparation
        (op : UnaryMetadataTokenIlOp)
        (token : MetadataToken)
        (prefix : AddressConstraint)
        (addressUse : MethodAddressUse)
        : FunctionPointerPreparation
        =
        match op, prefix, token, addressUse with
        | UnaryMetadataTokenIlOp.Ldvirtftn, _, _, _
        | UnaryMetadataTokenIlOp.Ldftn, AddressConstraint.Unconstrained, MetadataToken.MethodSpecification _, _ ->
            FunctionPointerPreparation.BeforeReturnType
        | UnaryMetadataTokenIlOp.Ldftn, AddressConstraint.Constrained, MetadataToken.MethodSpecification _, _ ->
            FunctionPointerPreparation.IfConstraintResolved
        | UnaryMetadataTokenIlOp.Ldftn, _, _, MethodAddressUse.FunctionPointer ->
            FunctionPointerPreparation.IfAddressRequested
        | UnaryMetadataTokenIlOp.Ldftn, _, _, MethodAddressUse.DelegateTarget
        | UnaryMetadataTokenIlOp.Ldftn, _, _, MethodAddressUse.Discarded -> FunctionPointerPreparation.Unprepared
        | _ -> failwith $"JitTypeLoads.preparation was given %O{op}, which takes no method's address"

    /// Where in the signature of the member an instruction names the JIT meets a type, in the order
    /// it meets them, besides the member's owner
    /// (`JitTypePosition.DeclaringType`), which it loads first, for every member token, `ldtoken`'s
    /// included. A function pointer's signature is its return type and then its parameters.
    let memberPositions (operand : MemberOperand) : MemberPositions =
        match operand with
        | MemberOperand.Call ->
            MemberPositions.Described [ JitTypePosition.CalleeReturn ; JitTypePosition.CalleeParameter ]
        | MemberOperand.UnmanagedCall _ -> MemberPositions.BySignature
        | MemberOperand.Jump -> MemberPositions.Described [ JitTypePosition.CalleeReturn ]
        | MemberOperand.Address FunctionPointerPreparation.BeforeReturnType ->
            MemberPositions.Described [ JitTypePosition.FunctionPointerSignature ; JitTypePosition.CalleeReturn ]
        | MemberOperand.Address FunctionPointerPreparation.IfConstraintResolved ->
            MemberPositions.Described
                [
                    JitTypePosition.PossibleFunctionPointerSignature
                    JitTypePosition.CalleeReturn
                ]
        | MemberOperand.Address FunctionPointerPreparation.IfAddressRequested ->
            MemberPositions.Described
                [
                    JitTypePosition.CalleeReturn
                    JitTypePosition.PossibleFunctionPointerSignature
                ]
        | MemberOperand.Address FunctionPointerPreparation.Unprepared ->
            MemberPositions.Described [ JitTypePosition.CalleeReturn ]
        | MemberOperand.FieldAccess -> MemberPositions.Described [ JitTypePosition.FieldType ]
        | MemberOperand.Token -> MemberPositions.Described []

    /// How the VM's walk of an unmanaged call's signature for whether the call needs marshalling
    /// meets one type of it (`PInvoke::MarshalingRequired`), as far as the type's spelling decides.
    [<RequireQualifiedAccess>]
    type private WalkStep =
        /// It may go on past the type, having loaded it if it is a value type
        /// (`JitTypePosition.MarshallingWalk`); for a value type, its layout may yet end the walk.
        | MayPass
        /// It ends at the type without loading it.
        | EndsBefore
        /// It loads the type and ends there.
        | EndsAt

    /// How the walk meets `spelling`, with runtime marshalling enabled or not.
    let private walkStep (runtimeMarshalling : bool) (spelling : TypeDefn) : WalkStep =
        match TypeDefn.stripCustomModifiers spelling with
        | TypeDefn.Void
        | TypeDefn.Pointer _
        | TypeDefn.FunctionPointer _ -> WalkStep.MayPass
        | TypeDefn.PrimitiveType primitive ->
            match primitive with
            | PrimitiveType.Boolean
            | PrimitiveType.Char ->
                if runtimeMarshalling then
                    WalkStep.EndsBefore
                else
                    WalkStep.MayPass
            | PrimitiveType.String
            | PrimitiveType.Object
            | PrimitiveType.TypedReference -> WalkStep.EndsBefore
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
            | PrimitiveType.UIntPtr -> WalkStep.MayPass
        // An instantiation of a class needs marshalling, which the walk finds only once it has
        // loaded the instantiation.
        | TypeDefn.GenericInstantiation _ as instantiation ->
            if isValueType instantiation then
                WalkStep.MayPass
            else
                WalkStep.EndsAt
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _ as named ->
            if isValueType named then
                WalkStep.MayPass
            else
                WalkStep.EndsBefore
        | TypeDefn.Byref _
        | TypeDefn.Array _
        | TypeDefn.OneDimensionalArrayLowerBoundZero _
        | TypeDefn.Pinned _
        | TypeDefn.GenericTypeParameter _
        | TypeDefn.GenericMethodParameter _ -> WalkStep.EndsBefore
        | TypeDefn.Modified _ -> failwith "BUG: TypeDefn.stripCustomModifiers left a modifier"

    /// Whether the JIT compiling `call`, a call of unmanaged code whose convention CoreCLR reads,
    /// may ask the VM to begin its walk of the signature for whether the call needs marshalling,
    /// which it does only where it may make the transition to unmanaged code in the calling
    /// method's own code (`impCheckForPInvokeCall`): not for a call in a handler or a protected
    /// `try`, nor in a method of an assembly that disables optimisation, nor by Fastcall's
    /// convention or its member-function variant, which only a stub makes the transition for, nor
    /// of a P/Invoke that takes a variable argument list or does not preserve its signature
    /// (`CEEInfo::pInvokeMarshalingRequired`, `PInvoke::MarshalingRequired`).
    let private beginsWalk (call : UnmanagedCall) : bool =
        call.Placement = UnmanagedCallPlacement.Unprotected
        && not call.CallerDisablesOptimisation
        && (
            match call.Convention with
            | UnmanagedConventionRead.Reads (UnmanagedCallConvBase.Fastcall, _) -> false
            | UnmanagedConventionRead.Reads _ -> true
            | UnmanagedConventionRead.MultipleBases
            | UnmanagedConventionRead.Unreadable _
            | UnmanagedConventionRead.NullArray -> false
        )
        && match call.Target with
           | UnmanagedTarget.PInvoke facts -> facts.PreservesSignature && not facts.TakesVariableArguments
           | UnmanagedTarget.FunctionPointer _ -> true

    /// Whether the JIT compiling `call`, a call of unmanaged code, may have the VM refuse, as its
    /// walk of the signature begins, the calling conventions of the P/Invoke the call calls
    /// (`PInvokeMarshalling.RefusesConventions`), raising `TypeLoadException` and failing the
    /// compilation. Whether it begins the walk turns on block weights, so this is a raise it may make
    /// or not.
    let unmanagedWalkRefuses (call : UnmanagedCall) : bool =
        beginsWalk call
        && match call.Target with
           | UnmanagedTarget.PInvoke facts -> facts.RefusesConventions
           | UnmanagedTarget.FunctionPointer _ -> false

    /// Where the JIT for `target` compiling `call`, a call of unmanaged code, meets the types of
    /// `signature`, the signature of the method it calls or the call site's, in the order it meets
    /// them, for an instruction whose operand `memberPositions` answers `MemberPositions.BySignature`.
    ///
    /// It meets the return type, and then reads the call's convention, which fails the compilation
    /// where CoreCLR cannot read one (`UnmanagedConventionRead`): then it meets nothing more.
    /// Otherwise it meets each parameter as any call passes it. Where it may begin the VM's walk of
    /// the signature for whether the call needs marshalling (`unmanagedWalkRefuses` says where that
    /// may refuse the call instead), the walk meets the return type and then each parameter up to
    /// the first that needs marshalling, unless the P/Invoke sets the last error or converts a
    /// locale, which need marshalling first; and if that may find none, by Swift's convention on a
    /// target that compiles it, the JIT meets each parameter again.
    let unmanagedMeetings
        (target : JitTarget)
        (call : UnmanagedCall)
        (signature : TypeMethodSignature<TypeDefn>)
        : (JitTypePosition * TypeDefn) list
        =
        let at (position : JitTypePosition) : (JitTypePosition * TypeDefn) list =
            signature.ParameterTypes |> List.map (fun parameter -> position, parameter)

        let returned, returnedSpelling =
            match signature.ReturnType with
            | MethodReturnType.Void -> [], TypeDefn.Void
            | MethodReturnType.Returns returned -> [ JitTypePosition.CalleeReturn, returned ], returned

        match call.Convention with
        | UnmanagedConventionRead.MultipleBases
        | UnmanagedConventionRead.Unreadable _
        | UnmanagedConventionRead.NullArray -> returned
        | UnmanagedConventionRead.Reads _ ->

        let walks =
            beginsWalk call
            && match call.Target with
               | UnmanagedTarget.PInvoke facts ->
                   not facts.RefusesConventions
                   && not facts.SetsLastError
                   && not facts.ConvertsLocale
               | UnmanagedTarget.FunctionPointer _ -> true

        let runtimeMarshalling, marshalled =
            match call.Target with
            | UnmanagedTarget.PInvoke facts -> facts.RuntimeMarshalling, facts.MarshalledPositions
            | UnmanagedTarget.FunctionPointer runtimeMarshalling -> runtimeMarshalling, Set.empty

        // The walk's step at each position, 0 the return type and then each parameter from 1. A
        // marshalling the metadata spells ends it, with runtime marshalling, once it has met the
        // type at that position.
        let steps =
            (returnedSpelling :: signature.ParameterTypes)
            |> List.mapi (fun position spelling ->
                match walkStep runtimeMarshalling spelling with
                | WalkStep.MayPass when runtimeMarshalling && marshalled.Contains position -> WalkStep.EndsAt
                | step -> step
            )

        // The parameters the walk meets: those before the first step that ends it, and that one if
        // it loads the type there.
        let walked =
            let rec reached (steps : (WalkStep * TypeDefn) list) : TypeDefn list =
                match steps with
                | [] -> []
                | (WalkStep.MayPass, spelling) :: rest -> spelling :: reached rest
                | (WalkStep.EndsAt, spelling) :: _ -> [ spelling ]
                | (WalkStep.EndsBefore, _) :: _ -> []

            match steps with
            | [] -> failwith "BUG: a signature has no return position"
            | WalkStep.MayPass :: parameters -> reached (List.zip parameters signature.ParameterTypes)
            | _ -> []

        let mayFindNoMarshalling =
            steps |> List.forall (fun step -> step = WalkStep.MayPass)

        let swift =
            if
                UnmanagedCallingConvention.isSwift target call.Convention
                && walks
                && mayFindNoMarshalling
            then
                at JitTypePosition.SwiftCalleeParameter
            else
                []

        let walk =
            if walks then
                walked |> List.map (fun parameter -> JitTypePosition.MarshallingWalk, parameter)
            else
                []

        returned @ walk @ swift @ at JitTypePosition.CalleeParameter
