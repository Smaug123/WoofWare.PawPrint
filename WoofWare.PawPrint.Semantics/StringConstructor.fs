namespace WoofWare.PawPrint

/// Why a constructor of `System.String` has no `String.Ctor` for CoreCLR to run.
[<RequireQualifiedAccess>]
type StringConstructorFault =
    /// No static `Ctor` of `System.String` takes the constructor's parameters.
    | NoCtor
    /// Several do, differing only in what they return.
    | SeveralCtors
    /// The one that does returns something other than a string, so it cannot stand for the
    /// constructor, whose `newobj` leaves the new string.
    | CtorReturns of MethodReturnType<TypeDefn>

/// What CoreCLR runs for a constructor of `System.String`. Every one is an FCall with no body of
/// its own; `vm/ecall.cpp` (`PopulateManagedStringConstructors`) makes each one's implementation
/// the static `String.Ctor` with the same parameters, which returns the new string, and the JIT
/// compiles a `newobj` of one as a call of that `Ctor`, allocating nothing and passing no `this`
/// (`jit/importer.cpp`, CEE_NEWOBJ).
[<RequireQualifiedAccess>]
module StringConstructor =

    /// The static `String.Ctor` that CoreCLR runs for `ctor`, a constructor of `stringType`
    /// (CoreLib's `System.String`).
    let implementation
        (stringType : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (ctor : MethodInfo<'typeGenerics, 'methodGenerics, 'methodVars>)
        : Result<MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>, StringConstructorFault>
        =
        if ctor.Name <> ".ctor" || ctor.IsStatic then
            failwith $"StringConstructor.implementation was given %s{ctor.Name}, which is not a constructor"

        let parameters =
            (MethodInfo.requireRawSignature "String constructor" ctor).ParameterTypes

        let candidates =
            stringType.Methods
            |> List.filter (fun candidate ->
                candidate.Name = "Ctor"
                && candidate.IsStatic
                && (MethodInfo.requireRawSignature "String.Ctor" candidate).ParameterTypes = parameters
            )

        match candidates with
        | [] -> Error StringConstructorFault.NoCtor
        | _ :: _ :: _ -> Error StringConstructorFault.SeveralCtors
        | [ single ] ->
            match (MethodInfo.requireRawSignature "String.Ctor" single).ReturnType with
            | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String) -> Ok single
            | other -> Error (StringConstructorFault.CtorReturns other)
