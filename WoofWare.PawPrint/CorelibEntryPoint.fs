namespace WoofWare.PawPrint

/// The CoreLib types and methods the VM looks up by name to call itself, rather than any guest:
/// its startup entry points, a started thread's `StartCallback`, and the
/// `[UnmanagedCallersOnly]` helpers it calls from native code.
[<RequireQualifiedAccess>]
module CorelibEntryPoint =

    /// The unique type definition `typeNamespace.typeName` in `corelib`. `purpose` is as for
    /// `findCorelibStaticMethod`.
    let findCorelibType
        (corelib : DumpedAssembly)
        (typeNamespace : string)
        (typeName : string)
        (purpose : string)
        : WoofWare.PawPrint.TypeInfo<GenericParamFromMetadata, TypeDefn>
        =
        let candidateTypes =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, ty)) ->
                if ty.Namespace = typeNamespace && ty.Name = typeName then
                    Some ty
                else
                    None
            )
            |> Seq.toList

        match candidateTypes with
        | [ single ] -> single
        | [] -> failwith $"Could not find %s{typeNamespace}.%s{typeName} in CoreLib; PawPrint calls it to %s{purpose}."
        | _ :: _ :: _ ->
            failwith
                $"Found several %s{typeNamespace}.%s{typeName} type definitions in CoreLib; PawPrint calls it to %s{purpose}."

    let private findCorelibMethod
        (isStatic : bool)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeNamespace : string)
        (typeName : string)
        (methodName : string)
        (arity : int)
        (purpose : string)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        let staticness = if isStatic then "static" else "instance"

        let declaringType =
            findCorelibType baseClassTypes.Corelib typeNamespace typeName purpose

        let candidates =
            declaringType.Methods
            |> List.filter (fun m -> m.Name = methodName && m.IsStatic = isStatic && MethodInfo.arity m = arity)

        match candidates with
        | [ single ] -> single
        | [] ->
            failwith
                $"Could not find the %s{staticness} %i{arity}-argument %s{typeNamespace}.%s{typeName}::%s{methodName} in CoreLib; PawPrint calls it to %s{purpose}. If its signature has changed, the caller needs updating to match."
        | _ :: _ :: _ ->
            failwith
                $"Found several %s{staticness} %i{arity}-argument %s{typeNamespace}.%s{typeName}::%s{methodName} methods in CoreLib; expected exactly one. PawPrint calls it to %s{purpose}."

    /// The unique static `methodName` of arity `arity` on the CoreLib type
    /// `typeNamespace.typeName`.
    ///
    /// `purpose` completes the sentence "… ; PawPrint calls it to <purpose>", and appears in
    /// every rejection: these are the VM's own private entry points, so a lookup that finds
    /// none or several means CoreLib's shape has changed under us, and the report has to say
    /// what PawPrint wanted it for.
    ///
    /// Arity is matched as well as the name so that failure stays loud, and correctly
    /// attributed, if a future CoreLib keeps the name but changes the signature.
    let findCorelibStaticMethod
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeNamespace : string)
        (typeName : string)
        (methodName : string)
        (arity : int)
        (purpose : string)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        findCorelibMethod true baseClassTypes typeNamespace typeName methodName arity purpose
