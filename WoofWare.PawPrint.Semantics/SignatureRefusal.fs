namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// Why CoreCLR refuses to hand the JIT a method signature, as it converts the signature for the
/// JIT (`ConvToJitSig`, `vm/jitinterface.cpp`): the compilation that reads it fails, and its
/// exception comes out of the call to the method being compiled, past every one of its handlers.
[<RequireQualifiedAccess>]
type SignatureRefusal =
    /// A variable argument list, managed or native, which CoreCLR on Unix does not support
    /// (`TARGET_UNIX`, `IDS_EE_VARARG_NOT_SUPPORTED`): it raises `InvalidProgramException`.
    | VariableArguments

/// Which method signatures CoreCLR refuses to hand the JIT, and where the JIT reads one.
[<RequireQualifiedAccess>]
module SignatureRefusal =

    /// The calling convention of a native variable argument list (`IMAGE_CEE_CS_CALLCONV_NATIVEVARARG`),
    /// which `SignatureCallingConvention` does not name.
    let private nativeVarArgs : SignatureCallingConvention =
        LanguagePrimitives.EnumOfValue<byte, SignatureCallingConvention> 0xBuy

    /// Why CoreCLR, on a Unix runtime, refuses `signature`, a method's or a call site's, or `None`.
    let ofSignature<'Types> (signature : TypeMethodSignature<'Types>) : SignatureRefusal option =
        let convention = signature.Header.Get.CallingConvention

        if convention = SignatureCallingConvention.VarArgs || convention = nativeVarArgs then
            Some SignatureRefusal.VariableArguments
        else
            None

    /// Whether the JIT has CoreCLR convert the signature of the method an instruction with operand
    /// `operand` names, or of its call site for `calli`: resolving a call or a method's address
    /// (`getCallInfo`), `calli`'s call site (`findSig`) and `jmp`'s target (`getMethodSig`) do,
    /// `ldtoken` and a field access do not. The JIT converts the signature of the method it compiles
    /// too, before anything else of it (`getMethodInfo`).
    let readsMemberSignature (operand : MemberOperand) : bool =
        match operand with
        | MemberOperand.Call
        | MemberOperand.UnmanagedCall
        | MemberOperand.Jump
        | MemberOperand.Address _ -> true
        | MemberOperand.FieldAccess
        | MemberOperand.Token -> false
