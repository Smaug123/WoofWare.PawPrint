namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata

/// The signature of CoreLib's `System.Threading.Thread::StartCallback`, the method CoreCLR enters
/// at the bottom of every thread the guest starts (`KickOffThread_Worker`), as classified by
/// `StartCallbackShape.classify`. The shape decides what PawPrint passes it.
[<RequireQualifiedAccess>]
type internal StartCallbackShape =
    /// `void StartCallback()`, an instance method of the started `Thread`, as .NET 10's CoreCLR
    /// CoreLib declares it.
    | Instance
    /// `static void StartCallback(Thread* pThread)`, an `[UnmanagedCallersOnly]` method, as
    /// .NET 11's CoreCLR CoreLib declares it: the VM passes the address of its own protected local
    /// holding the started `Thread`.
    | ThreadPointer

[<RequireQualifiedAccess>]
module internal StartCallbackShape =

    let private instanceHeader : SignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.Instance)

    let private staticHeader : SignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)

    /// The shape `signature` has, or `None` if it has neither. The whole signature is compared,
    /// header and return type included. `corelibThread` is the `System.Threading.Thread` that the
    /// CoreLib declaring `signature` defines.
    let classify
        (corelibThread : ResolvedTypeIdentity)
        (signature : TypeMethodSignature<TypeDefn>)
        : StartCallbackShape option
        =
        let header = signature.Header.Get

        match signature.ReturnType, signature.ParameterTypes with
        | MethodReturnType.Void, [] when header = instanceHeader -> Some StartCallbackShape.Instance
        | MethodReturnType.Void, [ TypeDefn.Pointer (TypeDefn.FromDefinition (identity, SignatureTypeKind.Class)) ] when
            header = staticHeader && identity = corelibThread
            ->
            Some StartCallbackShape.ThreadPointer
        | _ -> None

    /// The signatures `classify` recognises, for a refusal to name.
    let describeKnown : string =
        "instance void StartCallback()` or `static void StartCallback(System.Threading.Thread* pThread)"

/// Finding CoreLib's `Thread::StartCallback` and building the bottom frame of a started thread on
/// it.
[<RequireQualifiedAccess>]
module internal ThreadStartCallback =

    let private purpose : string = "enter a thread the guest has just started"

    /// CoreLib's `System.Threading.Thread::StartCallback`, and the shape its signature was
    /// classified into. Refuses when there is no `StartCallback`, when there are several, or when
    /// the one there is has neither shape.
    let locate
        (corelib : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> *
          StartCallbackShape
        =
        let threadType =
            CorelibEntryPoint.findCorelibType corelib "System.Threading" "Thread" purpose

        match threadType.Methods |> List.filter (fun m -> m.Name = "StartCallback") with
        | [ startCallback ] ->
            match StartCallbackShape.classify threadType.Identity startCallback.Signature with
            | Some shape -> startCallback, shape
            | None ->
                failwith
                    $"CoreLib's System.Threading.Thread::StartCallback has the signature %O{startCallback.Signature}, which is not one PawPrint knows how to call (it knows `%s{StartCallbackShape.describeKnown}`); PawPrint calls it to %s{purpose}."
        | [] ->
            failwith
                $"Could not find System.Threading.Thread::StartCallback in CoreLib; PawPrint calls it to %s{purpose}."
        | _ :: _ :: _ ->
            failwith
                $"Found several System.Threading.Thread::StartCallback methods in CoreLib; expected exactly one. PawPrint calls it to %s{purpose}."

    /// The bottom frame of the thread whose `Thread` object is at `threadAddr`, calling CoreLib's
    /// `StartCallback` with the arguments its shape takes, and the handle of its declaring type.
    let buildFrame
        (loggerFactory : Microsoft.Extensions.Logging.ILoggerFactory)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (threadAddr : ManagedHeapAddress)
        (state : IlMachineState)
        : IlMachineState * MethodState * ConcreteTypeHandle
        =
        let startCallback, shape = locate baseClassTypes.Corelib

        let args, state =
            match shape with
            | StartCallbackShape.Instance -> [ CliType.ObjectRef (Some threadAddr) ], state
            | StartCallbackShape.ThreadPointer ->
                let pThread, state = HostStartupCall.allocateReferenceLocal threadAddr state
                [ CliType.RuntimePointer (CliRuntimePointer.Managed pThread) ], state

        HostStartupCall.buildFrame
            loggerFactory
            baseClassTypes
            startCallback
            (ImmutableArray.CreateRange args)
            purpose
            state
