namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

/// Whether a call of unmanaged code is made by Swift's calling convention, as far as PawPrint
/// decides it (`UnmanagedConvention.ofPInvoke`, `UnmanagedConvention.ofCallSite`). Swift's
/// convention has CoreCLR's JIT read more of the call's signature
/// (`JitTypePosition.SwiftCalleeParameter`).
[<RequireQualifiedAccess>]
type UnmanagedConvention =
    /// Swift's.
    | Swift
    /// A convention other than Swift's.
    | NotSwift
    /// Swift's or another: the call names Swift's convention alongside another one, or is a
    /// P/Invoke with an `[UnmanagedCallConv]` attribute, whose conventions PawPrint does not read.
    | MaybeSwift

/// Which calling convention CoreCLR calls unmanaged code by, as far as whether it is Swift's.
[<RequireQualifiedAccess>]
module UnmanagedConvention =

    /// The namespace of the types whose names, as modifiers, name calling conventions
    /// (`CMOD_CALLCONV_NAMESPACE`, `vm/callconvbuilder.cpp`).
    let private conventionNamespace : string = "System.Runtime.CompilerServices"

    /// The names of the base calling conventions, as against the modifiers of one, such as
    /// `CallConvSuppressGCTransition` (`DECLARE_BASE_CALL_CONVS`, `vm/callconvbuilder.cpp`).
    let private baseConventions : Set<string> =
        Set.ofList
            [
                "CallConvCdecl"
                "CallConvStdcall"
                "CallConvThiscall"
                "CallConvFastcall"
                "CallConvSwift"
            ]

    /// The attribute that sets a P/Invoke's calling convention when its import says `WinApi`.
    let private unmanagedCallConv : string =
        "System.Runtime.InteropServices.UnmanagedCallConvAttribute"

    /// The namespace and name of the type a modifier in a signature of `assembly` names.
    let private modifierName (assembly : DumpedAssembly) (modifier : TypeDefn) : (string * string) option =
        match modifier with
        | TypeDefn.FromReference (typeRef, _) -> Some (typeRef.Namespace, typeRef.Name)
        | TypeDefn.FromDefinition (identity, _) ->
            let definition = assembly.TypeDefs.[identity.TypeDefinition.Get]
            Some (definition.Namespace, definition.Name)
        | _ -> None

    /// The base calling conventions that the optional modifiers on `returned`, a return type a
    /// signature of `assembly` spells, name. CoreCLR reads them by name alone, and passes over a
    /// required modifier (`CallConv::TryGetUnmanagedCallingConventionFromModOpt`).
    let rec private namedConventions (assembly : DumpedAssembly) (returned : TypeDefn) : string list =
        match returned with
        | TypeDefn.Modified modified ->
            let rest = namedConventions assembly modified.Unmodified

            match modified.IsRequired, modifierName assembly modified.Modifier with
            | false, Some (ns, name) when ns = conventionNamespace && baseConventions.Contains name -> name :: rest
            | _ -> rest
        | _ -> []

    /// The convention a signature of `assembly` names by the modifiers on its return type.
    let private ofModifiers
        (assembly : DumpedAssembly)
        (signature : TypeMethodSignature<TypeDefn>)
        : UnmanagedConvention
        =
        let named =
            match signature.ReturnType with
            | MethodReturnType.Returns returned -> namedConventions assembly returned |> List.distinct
            | MethodReturnType.Void -> []

        match named with
        | [ "CallConvSwift" ] -> UnmanagedConvention.Swift
        | named when List.contains "CallConvSwift" named -> UnmanagedConvention.MaybeSwift
        | _ -> UnmanagedConvention.NotSwift

    /// The convention of a call of unmanaged code through a function pointer, a `calli` whose
    /// call-site signature, spelled in `assembly`, is `signature` (`getUnmanagedCallConvForSig`,
    /// `vm/jitinterface.cpp`): only one by the `unmanaged` calling convention names its convention
    /// by modifiers.
    let ofCallSite (assembly : DumpedAssembly) (signature : TypeMethodSignature<TypeDefn>) : UnmanagedConvention =
        match signature.Header.Get.CallingConvention with
        | SignatureCallingConvention.Unmanaged -> ofModifiers assembly signature
        | _ -> UnmanagedConvention.NotSwift

    /// The convention of a call of `method`, a P/Invoke of `assembly`
    /// (`getUnmanagedCallConvForMethod`, `PInvoke::GetCallingConvention_IgnoreErrors`): its
    /// signature's, if that names an unmanaged convention; otherwise its import's, unless that says
    /// `WinApi`; otherwise its `[UnmanagedCallConv]` attribute's, if it has one, and otherwise its
    /// signature's modifiers'.
    let ofPInvoke (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : UnmanagedConvention =
        let definition = assembly.Methods.[method]

        match definition.Signature.Header.Get.CallingConvention with
        | SignatureCallingConvention.Default
        | SignatureCallingConvention.VarArgs ->
            let importConvention =
                definition.TryNativeImport
                |> Option.map (fun import -> import.Attributes &&& MethodImportAttributes.CallingConventionMask)

            match importConvention with
            | Some convention when convention <> MethodImportAttributes.CallingConventionWinApi ->
                UnmanagedConvention.NotSwift
            | _ when not (NamedAttribute.ofMethod unmanagedCallConv assembly method).IsEmpty ->
                UnmanagedConvention.MaybeSwift
            | _ -> ofModifiers assembly definition.Signature
        | _ -> ofCallSite assembly definition.Signature
