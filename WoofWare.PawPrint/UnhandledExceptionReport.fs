namespace WoofWare.PawPrint

open System

/// <summary>
/// Describing a guest's unhandled exception, for PawPrint's developer-facing diagnostics.
/// </summary>
/// <remarks>
/// <para>
/// A read over the heap of a finished run: it runs no guest code and changes no state. So it
/// reports what the exception object holds rather than what the guest would compute from it: the
/// raw <c>Exception._message</c> field rather than the virtual <c>Exception.Message</c>, which
/// CoreLib's own types override (<c>ArgumentException</c> appends the parameter name, and
/// <c>Exception</c> itself synthesises "Exception of type 'X' was thrown." when the field is null).
/// </para>
/// <para>
/// The layout follows <c>Exception.ToString</c>, which is what CoreCLR prints after
/// "Unhandled exception.", so that it reads familiarly; but it is not that string and must not be
/// compared against it. The frames are PawPrint's own, rendered as <c>GuestLocation</c> renders
/// them, with source positions where the guest has a PDB.
/// </para>
/// <para>
/// Nothing here may throw on an unexpected heap shape: it runs on failure paths, where an
/// exception of its own would replace the diagnostic it exists to give.
/// </para>
/// </remarks>
[<RequireQualifiedAccess>]
module UnhandledExceptionReport =

    /// How far down an <c>_innerException</c> chain to follow. A guest can build a cycle through
    /// reflection, and a diagnostic must terminate regardless.
    [<Literal>]
    let private maxInnerExceptionDepth = 32

    /// CoreLib's `System.Exception` and its concrete handle, or None if either is absent from
    /// `state` — as in a skeletal state that never loaded CoreLib, or one where nothing has
    /// concretised `Exception`, in which case no object on the heap can be one.
    let private tryExceptionType
        (state : IlMachineState)
        : (TypeInfo<GenericParamFromMetadata, TypeDefn> * ConcreteTypeHandle) option
        =
        state._LoadedAssemblies.DefinitionNamesInLoadOrder
        |> Seq.tryFind (AssemblyDefinitionName.isNamed "System.Private.CoreLib")
        |> Option.bind state.LoadedAssembly
        |> Option.bind (fun corelib ->
            corelib.TypeDefs.Values
            |> Seq.tryFind (fun ti -> ti.Namespace = "System" && ti.Name = "Exception")
        )
        |> Option.bind (fun exceptionType ->
            AllConcreteTypes.findExistingNonGenericConcreteType state.ConcreteTypes exceptionType.Identity
            |> Option.map (fun handle -> exceptionType, handle)
        )

    /// The identities of the `System.Exception` fields this report reads.
    type private ExceptionFields =
        {
            Message : FieldId
            InnerException : FieldId
            StackTrace : FieldId
        }

    let private exceptionFields
        (exceptionType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (exceptionHandle : ConcreteTypeHandle)
        : ExceptionFields
        =
        let field (name : string) : FieldId =
            FieldIdentity.requiredOwnInstanceField exceptionType name
            |> FieldIdentity.fieldId exceptionHandle

        {
            Message = field "_message"
            InnerException = field "_innerException"
            StackTrace = field "_stackTrace"
        }

    let private typeName (state : IlMachineState) (handle : ConcreteTypeHandle) : string =
        match AllConcreteTypes.lookup handle state.ConcreteTypes with
        | None -> $"<type %O{handle}>"
        | Some ty ->
            match state.LoadedAssembly ty.AssemblyFullName with
            | Some assy -> Assembly.fullName assy ty.Identity
            | None when String.IsNullOrEmpty ty.Namespace -> ty.Name
            | None -> $"%s{ty.Namespace}.%s{ty.Name}"

    /// The field's value if `obj`'s storage holds exactly that field. Every class deriving from
    /// `System.Exception` holds `Exception`'s own fields under `Exception`'s declaring handle,
    /// and nothing else does, so their presence is what identifies an exception object.
    let private tryReadField (field : FieldId) (obj : AllocatedNonArrayObject) : CliType option =
        let present =
            CliValueType.TryAllFields obj.Contents
            |> List.exists (fun f -> FieldId.exactlyEqual field f.Id)

        if present then
            Some (AllocatedNonArrayObject.DereferenceFieldById field obj)
        else
            None

    let private renderFrame
        (state : IlMachineState)
        (frame : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : string list
        =
        let guestFrame : GuestFrame =
            {
                Method = string<MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>> frame.Method
                IlOffset = frame.IlOffset
            }

        let line =
            match GuestLocation.trySourceOf state frame.Method frame.IlOffset with
            | Some source -> $"   at %s{GuestLocation.renderFrame guestFrame} (%O{source})"
            | None -> $"   at %s{GuestLocation.renderFrame guestFrame}"

        if frame.IsLastFrameFromForeignExceptionStackTrace then
            [ line ; "--- End of stack trace from previous location ---" ]
        else
            [ line ]

    /// The lines describing the object at `addr` and everything reachable from it through
    /// `_innerException`, outermost first. `trace` is the outermost object's frames when the
    /// caller has them, as it does for the unhandled exception itself: the dispatch that just
    /// ended carries them. Without it, an exception's frames are read from its frozen
    /// `_stackTrace`, which is where an inner exception's are.
    let rec private describeObject
        (state : IlMachineState)
        (exceptionFields : ExceptionFields option)
        (depth : int)
        (addr : ManagedHeapAddress)
        (trace : ExceptionStackFrame<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> list option)
        : string list
        =
        let exceptionObject =
            match HeapObserver.tryGetNonArrayObject addr state.ManagedHeap, exceptionFields with
            | Some obj, Some fields ->
                tryReadField fields.Message obj
                |> Option.map (fun message -> obj, fields, message)
            | _, _ -> None

        let heading, inner, frozenFrames =
            match exceptionObject with
            | None ->
                let heading =
                    match HeapObserver.tryGetNonArrayObject addr state.ManagedHeap with
                    | Some obj -> $"%s{typeName state obj.ConcreteType} (not a System.Exception)"
                    | None ->
                        match HeapObserver.tryGetArray addr state.ManagedHeap with
                        | Some arr -> $"%s{typeName state arr.Shape.ConcreteType} (an array, not a System.Exception)"
                        | None -> $"<no live object at %O{addr}>"

                heading, [], []
            | Some (obj, fields, message) ->
                let name = typeName state obj.ConcreteType

                let heading =
                    match message with
                    | CliType.ObjectRef None -> $"%s{name} (_message is null)"
                    | CliType.ObjectRef (Some messageAddr) ->
                        match HeapObserver.getStringContents messageAddr state.ManagedHeap with
                        | Some text -> $"%s{name}: %s{text}"
                        | None -> $"%s{name} (_message is %O{messageAddr}, which holds no string)"
                    | other -> $"%s{name} (_message holds %O{other})"

                let inner =
                    match tryReadField fields.InnerException obj with
                    | Some (CliType.ObjectRef (Some innerAddr)) ->
                        if depth >= maxInnerExceptionDepth then
                            [
                                $" ---> <_innerException chain truncated after %d{maxInnerExceptionDepth} levels>"
                            ]
                        else
                            match describeObject state exceptionFields (depth + 1) innerAddr None with
                            | [] -> []
                            | innerHeading :: innerRest ->
                                [
                                    yield $" ---> %s{innerHeading}"
                                    yield! innerRest
                                    yield "   --- End of inner exception stack trace ---"
                                ]
                    | _ -> []

                let frozenFrames =
                    match tryReadField fields.StackTrace obj with
                    | Some (CliType.ObjectRef (Some token)) ->
                        state.FrozenStackTraces |> Map.tryFind token |> Option.defaultValue []
                    | _ -> []

                heading, inner, frozenFrames

        let frames = trace |> Option.defaultValue frozenFrames

        [
            yield heading
            yield! inner
            yield! frames |> List.collect (renderFrame state)
        ]

    /// <summary>
    /// Describe <paramref name="exn" />, an exception that ended a run unhandled, from the
    /// heap of <paramref name="state" />, the state the run ended in.
    /// </summary>
    /// <remarks>
    /// Names the thrown object's runtime type and its <c>_message</c>, then each
    /// <c>_innerException</c> in turn, then the frames the exception unwound through. A thrown
    /// object that is not a <c>System.Exception</c>, which IL can throw though C# cannot, is
    /// described by its type alone.
    /// </remarks>
    let describe
        (state : IlMachineState)
        (exn : CliException<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : string
        =
        let exceptionFields =
            tryExceptionType state
            |> Option.map (fun (ty, handle) -> exceptionFields ty handle)

        describeObject state exceptionFields 0 exn.ExceptionObject (Some exn.StackTrace)
        |> String.concat Environment.NewLine
