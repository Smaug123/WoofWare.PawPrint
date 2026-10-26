namespace WoofWare.PawPrint

open System.Reflection
open System.Reflection.Metadata

/// Where a call of unmanaged code is in its method's body, as far as it decides whether CoreCLR's
/// JIT may make the transition to unmanaged code in the method's own code
/// (`impCanPInvokeInlineCallSite`).
[<RequireQualifiedAccess>]
type UnmanagedCallPlacement =
    /// In no handler or filter, and in no `try` that has a catch or filter handler.
    | Unprotected
    /// In a handler or a filter, or in a `try` that has a catch or filter handler, or inside one:
    /// on a 64-bit target, as every `JitTarget` is, the JIT never makes the transition there
    /// (`USE_PER_FRAME_PINVOKE_INIT`).
    | Protected

/// What CoreCLR reads of a P/Invoke to decide, before walking its signature, that a call of it
/// needs marshalling, and how it walks the signature (`PInvoke::MarshalingRequired`).
type PInvokeMarshalling =
    {
        /// Whether it returns its native result as it is (`PreserveSig`), rather than raising for
        /// a failing HRESULT.
        PreservesSignature : bool
        /// Whether it saves the native error afterwards (`SetLastError`).
        SetsLastError : bool
        /// Whether it passes the current culture's locale as an argument (`LCIDConversion`).
        ConvertsLocale : bool
        /// Whether it takes a variable argument list.
        TakesVariableArguments : bool
        /// Whether it is synchronized (`MethodImplAttributes.Synchronized`), which the VM refuses
        /// as it begins to describe it for the walk, raising `TypeLoadException`
        /// (`IDS_EE_NOSYNCHRONIZED`, `InitializeSigInfoAndPopulatePInvokeMethodDesc`).
        Synchronized : bool
        /// Whether the VM, describing it as the walk begins, refuses the calling conventions it
        /// names, raising `TypeLoadException` (`UnmanagedCallingConvention.describingRefuses`).
        RefusesConventions : bool
        /// Whether its assembly leaves runtime marshalling enabled (no `DisableRuntimeMarshalling`).
        RuntimeMarshalling : bool
        /// The positions, 0 its return type and then each parameter from 1, whose marshalling its
        /// metadata spells in a way that CoreCLR reads as other than the default (`MarshalAs`).
        MarshalledPositions : Set<int>
    }

/// What a call of unmanaged code calls, as far as CoreCLR reads it to decide whether the call
/// needs marshalling (`PInvoke::MarshalingRequired`).
[<RequireQualifiedAccess>]
type UnmanagedTarget =
    /// A P/Invoke.
    | PInvoke of PInvokeMarshalling
    /// A function pointer, which `calli` calls by an unmanaged calling convention; runtime
    /// marshalling is the calling module's.
    | FunctionPointer of runtimeMarshalling : bool

/// A call of unmanaged code, as far as what CoreCLR's JIT reads of its signature depends on it
/// (`JitTypeLoads.unmanagedMeetings`).
type UnmanagedCall =
    {
        /// What CoreCLR reads as the call's calling convention.
        Convention : UnmanagedConventionRead
        /// Where the call is in its method's body.
        Placement : UnmanagedCallPlacement
        /// Whether the calling method's assembly disables optimisation (`DebuggableAttribute`), so
        /// that the JIT compiles it as debuggable code, never making the transition to unmanaged
        /// code in it (`impCanPInvokeInline`).
        CallerDisablesOptimisation : bool
        /// What the call calls.
        Target : UnmanagedTarget
    }

/// Where a call of unmanaged code is in its method's body.
[<RequireQualifiedAccess>]
module UnmanagedCallPlacement =

    /// Where the instruction at IL offset `offset` is, in a body whose exception regions are
    /// `regions`. Each region is one clause; a `try` with several handlers is several clauses over
    /// the same range.
    let ofRegions (regions : WoofWare.PawPrint.ExceptionRegion seq) (offset : int) : UnmanagedCallPlacement =
        let within (start : int) (length : int) : bool =
            start <= offset && offset < start + length

        let protects (region : WoofWare.PawPrint.ExceptionRegion) : bool =
            match region with
            | WoofWare.PawPrint.ExceptionRegion.Catch (_, range) ->
                within range.TryOffset range.TryLength
                || within range.HandlerOffset range.HandlerLength
            // A filter's own code runs up to its handler, which follows it.
            | WoofWare.PawPrint.ExceptionRegion.Filter (filterOffset, range) ->
                within range.TryOffset range.TryLength
                || within filterOffset (range.HandlerOffset - filterOffset)
                || within range.HandlerOffset range.HandlerLength
            | WoofWare.PawPrint.ExceptionRegion.Finally range
            | WoofWare.PawPrint.ExceptionRegion.Fault range -> within range.HandlerOffset range.HandlerLength

        if Seq.exists protects regions then
            UnmanagedCallPlacement.Protected
        else
            UnmanagedCallPlacement.Unprotected

/// What a call of unmanaged code calls.
[<RequireQualifiedAccess>]
module UnmanagedTarget =

    let private disableRuntimeMarshalling : string =
        "System.Runtime.CompilerServices.DisableRuntimeMarshallingAttribute"

    let private lcidConversion : string =
        "System.Runtime.InteropServices.LCIDConversionAttribute"

    /// The first byte of a marshalling blob that CoreCLR reads as the default native type
    /// (`NATIVE_TYPE_DEFAULT`, which is `NATIVE_TYPE_MAX`).
    let private defaultNativeType : byte = 0x50uy

    /// Whether `method`, a P/Invoke of `assembly`, passes a locale as an argument: its first
    /// `LCIDConversion` attribute names, as its one fixed argument, a parameter index other than -1.
    /// CoreCLR reads -1 where the attribute is missing or its blob does not parse
    /// (`GetLCIDParameterIndex`).
    let private convertsLocale (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        match NamedAttribute.ofMethod lcidConversion assembly method with
        | [] -> false
        | attribute :: _ ->
            let blob = attribute.Value

            blob.Length >= 6
            && blob.[0] = 1uy
            && blob.[1] = 0uy
            && System.BitConverter.ToInt32 (blob.AsSpan (2, 4)) <> -1

    let private runtimeMarshalling (assembly : DumpedAssembly) : bool =
        (NamedAttribute.ofAssembly disableRuntimeMarshalling assembly).IsEmpty

    /// What CoreCLR reads of `method`, a P/Invoke of `assembly`.
    let ofPInvoke
        (assembly : DumpedAssembly)
        (method : System.Reflection.Metadata.MethodDefinitionHandle)
        : UnmanagedTarget
        =
        let definition = assembly.Methods.[method]
        let metadata = assembly.PeReader.GetMetadataReader ()

        let implAttributes =
            match definition with
            | MethodInfo.Metadata (_, facts) -> facts.ImplAttributes
            | MethodInfo.Synthesised _ ->
                failwith $"BUG: %O{method} of %s{assembly.DefinitionFullName} is a P/Invoke without metadata"

        let setsLastError =
            match definition.TryNativeImport with
            | Some import -> import.Attributes.HasFlag MethodImportAttributes.SetLastError
            | None -> false

        // CoreCLR finds a parameter's marshalling row by the parameter alone, whatever its
        // `HasFieldMarshal` flag says, and reads a row whose blob is empty, or does not begin with
        // the default native type, as spelling a marshalling (`ParseNativeTypeInfo`). A row whose
        // blob index is zero looks here like no row, and is taken as none, which keeps the loads the
        // walk may make.
        let marshalled =
            metadata.GetMethodDefinition(method).GetParameters ()
            |> Seq.map metadata.GetParameter
            |> Seq.filter (fun (parameter : Parameter) ->
                let descriptor = parameter.GetMarshallingDescriptor ()

                if descriptor.IsNil then
                    false
                else
                    let blob = metadata.GetBlobBytes descriptor
                    blob.Length = 0 || blob.[0] <> defaultNativeType
            )
            |> Seq.map (fun (parameter : Parameter) -> parameter.SequenceNumber)
            |> Set.ofSeq

        UnmanagedTarget.PInvoke
            {
                PreservesSignature = implAttributes.HasFlag MethodImplAttributes.PreserveSig
                SetsLastError = setsLastError
                ConvertsLocale = convertsLocale assembly method
                TakesVariableArguments =
                    definition.Signature.Header.Get.CallingConvention = System.Reflection.Metadata.SignatureCallingConvention.VarArgs
                Synchronized = implAttributes.HasFlag MethodImplAttributes.Synchronized
                RefusesConventions = UnmanagedCallingConvention.describingRefuses assembly method
                RuntimeMarshalling = runtimeMarshalling assembly
                MarshalledPositions = marshalled
            }

    /// What CoreCLR reads of a function pointer that a `calli` of `assembly` calls.
    let ofFunctionPointer (assembly : DumpedAssembly) : UnmanagedTarget =
        UnmanagedTarget.FunctionPointer (runtimeMarshalling assembly)
