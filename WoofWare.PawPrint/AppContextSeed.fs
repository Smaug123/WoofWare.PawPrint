namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// A signature of `System.AppContext::Setup` which PawPrint has checked against a real CoreLib
/// and knows how to build a call for.
///
/// The cases are exactly the validated set. A signature outside it is refused rather than called
/// with whatever arguments its parameter list suggests, because a changed `Setup` may have
/// changed its contract too (net11's added out-slot is a value the host must read back), and
/// only a check against the image that declares it can say how.
[<RequireQualifiedAccess>]
type internal SetupShape =
    /// `static void Setup(char** pNames, char** pValues, int count)`, as .NET 10's CoreCLR CoreLib
    /// declares it.
    | ThreeArg

[<RequireQualifiedAccess>]
module internal SetupShape =

    let private (|CharPtrPtr|_|) (ty : TypeDefn) : unit option =
        match ty with
        | TypeDefn.Pointer (TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.Char)) -> Some ()
        | _ -> None

    /// A static, non-generic method with the default managed calling convention.
    let private plainStaticHeader : SignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)

    /// The shape `signature` has, or `None` if it has none of them. The whole signature is
    /// compared, header and return type included.
    let classify (signature : TypeMethodSignature<TypeDefn>) : SetupShape option =
        if signature.Header.Get <> plainStaticHeader then
            None
        else

        match signature.ReturnType, signature.ParameterTypes with
        | MethodReturnType.Void, [ CharPtrPtr ; CharPtrPtr ; TypeDefn.PrimitiveType PrimitiveType.Int32 ] ->
            Some SetupShape.ThreeArg
        | _ -> None

    /// The signatures `classify` recognises, for a refusal to name.
    let describeKnown : string =
        "static void Setup(char** pNames, char** pValues, int count)"

/// Seeds `System.AppContext` the way a real runtime host does, so that feature switches
/// declared in `runtimeconfig.json` — `System.Diagnostics.Tracing.EventSource.IsSupported`
/// and friends — are in place before any guest or BCL code can latch them.
///
/// CoreCLR does this from `CorHost2::CreateAppDomainWithManager`, which calls
///
///     internal static unsafe void AppContext.Setup(char** pNames, char** pValues, int count)
///
/// with two arrays of NUL-terminated UTF-16 strings that `hostpolicy` allocated. `Setup` is
/// ordinary managed IL — it news up a `Dictionary<string, object>`, walks the arrays doing
/// pointer arithmetic, and `new string(char*)`s each entry — so PawPrint runs CoreLib's own
/// code here. The host's contribution, and the only thing this module synthesises, is the
/// two `char**` buffers.
///
/// This module only *builds the call*; installing and pumping it is `Program.prepare`'s
/// business, because that is where the entry thread's frame lifecycle is managed.
///
/// The call is built for a `SetupShape`: CoreLib's `Setup` is found by name and its signature
/// classified, so a CoreLib that declares some other signature is refused by name rather than
/// called with arguments built for a different one.
[<RequireQualifiedAccess>]
module AppContextSeed =

    /// What PawPrint wants `AppContext::Setup` for, phrased to complete "PawPrint calls it
    /// to …" in `HostStartupCall`'s rejections.
    [<Literal>]
    let private Purpose =
        "install the host's configuration properties, which is what AppContext.GetData reads"

    /// `method`'s signature as a reader of a refusal wants it: every part of it that
    /// `SetupShape.classify` looks at.
    let private describeSignature
        (corelib : DumpedAssembly)
        (method : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string
        =
        let scope = GenericScope.ofMethod method
        let signature = method.Signature
        let header = signature.Header.Get

        let parameters =
            signature.ParameterTypes
            |> List.map (IlFormatting.renderTypeDefn corelib scope)
            |> String.concat ", "

        let generics =
            if signature.GenericParameterCount = 0 then
                ""
            else
                $"<%i{signature.GenericParameterCount} generic parameters>"

        let instance = if header.IsInstance then "instance " else ""

        let ret = IlFormatting.renderMethodReturnType corelib scope signature.ReturnType

        $"%s{instance}%O{header.CallingConvention} %s{method.Name}%s{generics}(%s{parameters}) : %s{ret}"

    /// CoreLib's `System.AppContext::Setup`, and the shape its signature was classified into.
    /// Refuses, naming the signature found, when there is no static `Setup`, when there are
    /// several, or when the one there is has a signature outside the `SetupShape`s.
    let internal locateSetup
        (corelib : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> * SetupShape
        =
        let appContext =
            HostStartupCall.findCorelibType corelib "System" "AppContext" Purpose

        let candidates =
            appContext.Methods |> List.filter (fun m -> m.Name = "Setup" && m.IsStatic)

        match candidates with
        | [ setup ] ->
            match SetupShape.classify setup.Signature with
            | Some shape -> setup, shape
            | None ->
                failwith
                    $"CoreLib's System.AppContext::Setup has the signature `%s{describeSignature corelib setup}`, which is not one PawPrint knows how to call (it knows `%s{SetupShape.describeKnown}`); PawPrint calls it to %s{Purpose}."
        | [] ->
            failwith $"Could not find a static System.AppContext::Setup in CoreLib; PawPrint calls it to %s{Purpose}."
        | _ :: _ :: _ ->
            let found =
                candidates
                |> List.map (fun m -> $"`%s{describeSignature corelib m}`")
                |> String.concat ", "

            failwith
                $"Found several static System.AppContext::Setup methods in CoreLib (%s{found}); expected exactly one. PawPrint calls it to %s{Purpose}."

    /// Build the call to `AppContext.Setup` that seeds `properties`, returning the machine
    /// state with the argument buffers allocated and a frame ready to be installed and run.
    ///
    /// `None` when there is nothing to seed, which skips the call rather than making it with a
    /// count of zero; the difference is not observable to the guest.
    ///
    /// The native blocks allocated here are never freed. `hostpolicy`'s arrays
    /// outlive the call too, and a guest is entitled to have kept a `char*` into one — so
    /// freeing them would turn a legal (if strange) guest into a use-after-free report.
    let prepareCall
        (loggerFactory : ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (properties : AppContextProperties)
        (state : IlMachineState)
        : (IlMachineState * MethodState) option
        =
        if AppContextProperties.isEmpty properties then
            // Skipping differs from calling with a count of zero internally — `Setup`
            // assigns a fresh dictionary to `s_dataStore`, where skipping leaves it null —
            // but not observably: `GetData` returns null for a null store, and `SetData`
            // lazily installs one. So this buys the cheaper path without changing what a
            // guest can see.
            None
        else

        // Sorted, because `Map.toList` is ordered by key: the layout of the `char**` arrays
        // is then a function of the property set alone, not of any traversal order, which is
        // what makes two runs with the same `HostConfig` produce identical machine states.
        let entries = AppContextProperties.toMap properties |> Map.toList

        let namePointers, state =
            (state, entries)
            ||> List.mapFold (fun state (name, _) -> HostStartupCall.allocateWideString name state)

        let valuePointers, state =
            (state, entries)
            ||> List.mapFold (fun state (_, value) -> HostStartupCall.allocateWideString value state)

        let pNames, state = HostStartupCall.allocatePointerArray namePointers state
        let pValues, state = HostStartupCall.allocatePointerArray valuePointers state

        let setup, shape = locateSetup baseClassTypes.Corelib

        let args =
            match shape with
            | SetupShape.ThreeArg ->
                ImmutableArray.CreateRange
                    [
                        CliType.RuntimePointer (CliRuntimePointer.Managed pNames)
                        CliType.RuntimePointer (CliRuntimePointer.Managed pValues)
                        CliType.Numeric (CliNumericType.Int32 (List.length entries))
                    ]

        let state, frame, _declaringType =
            HostStartupCall.buildFrame loggerFactory baseClassTypes setup args Purpose state

        Some (state, frame)
