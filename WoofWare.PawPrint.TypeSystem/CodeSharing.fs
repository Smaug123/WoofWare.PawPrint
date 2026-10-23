namespace WoofWare.PawPrint

/// Whether CoreCLR compiles a method once for many instantiations: "shared generic code", whose
/// type arguments it reads at run time from a generic dictionary rather than knowing as the JIT
/// compiles. A few failures surface at a different instruction in shared code from exact code, so
/// an instruction whose outcome depends on that asks here.
///
/// CoreCLR shares the code of an instantiation that has an argument whose canonical form contains
/// `System.__Canon` (`ClassLoader::IsSharableInstantiation`, generics.cpp). The canonical form of a
/// reference type is `__Canon` itself; that of a value type keeps the type and canonicalises its own
/// arguments (`ClassLoader::CanonicalizeGenericArg`), so `S<string>` is shared and `S<int>` is not.
[<RequireQualifiedAccess>]
module CodeSharing =

    /// Whether `handle`, as a generic argument, canonicalises to a type containing `__Canon`:
    /// whether it is a reference type, or a value type with such an argument.
    let rec canonicalisesToCanon
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (handle : ConcreteTypeHandle)
        : bool
        =
        match handle with
        | ConcreteTypeHandle.OneDimArrayZero _
        | ConcreteTypeHandle.Array _ -> true
        | ConcreteTypeHandle.Byref _
        | ConcreteTypeHandle.Pointer _
        | ConcreteTypeHandle.FunctionPointer _ ->
            // CoreCLR's type loader refuses these as generic arguments
            // (`Generics::CheckInstantiation`, generics.cpp), so no instantiation that runs has one.
            failwith
                $"BUG: CodeSharing: %O{handle} is a generic argument, but CoreCLR admits no byref, pointer or function pointer as one"
        | ConcreteTypeHandle.Concrete _ ->
            if TypeSystemState.isReferenceTypeHandle baseClassTypes "CodeSharing" state handle then
                true
            else
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | Some concreteType -> concreteType.Generics |> Seq.exists (canonicalisesToCanon baseClassTypes state)
                | None -> failwith $"BUG: CodeSharing: concrete type handle %O{handle} is not registered"

    /// Whether a type that a body spells names a type variable whose argument in `typeArguments`,
    /// the declaring type's, or `methodArguments`, the method's, makes CoreCLR share the body's code
    /// (`canonicalisesToCanon`): whether the shared code sees the type in its canonical form, and
    /// looks it up at run time.
    let rec namesSharedArgument
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (typeArguments : ConcreteTypeHandle list)
        (methodArguments : ConcreteTypeHandle list)
        (spelling : TypeDefn)
        : bool
        =
        let shared (arguments : ConcreteTypeHandle list) (index : int) : bool =
            match List.tryItem index arguments with
            | Some argument -> canonicalisesToCanon baseClassTypes state argument
            | None ->
                failwith $"BUG: %O{spelling} names type variable %d{index}, which the instance has no argument for"

        let recurse = namesSharedArgument baseClassTypes state typeArguments methodArguments

        match spelling with
        | TypeDefn.GenericTypeParameter index -> shared typeArguments index
        | TypeDefn.GenericMethodParameter index -> shared methodArguments index
        | TypeDefn.GenericInstantiation (root, arguments) -> Seq.append [ root ] arguments |> Seq.exists recurse
        | TypeDefn.Array (element, _)
        | TypeDefn.OneDimensionalArrayLowerBoundZero element
        | TypeDefn.Pointer element
        | TypeDefn.Byref element
        | TypeDefn.Pinned element -> recurse element
        | TypeDefn.Modified modified -> recurse modified.Unmodified
        | TypeDefn.FunctionPointer signature ->
            let spelled =
                match signature.ReturnType with
                | MethodReturnType.Void -> signature.ParameterTypes
                | MethodReturnType.Returns ty -> ty :: signature.ParameterTypes

            List.exists recurse spelled
        | TypeDefn.FromReference _
        | TypeDefn.FromDefinition _
        | TypeDefn.PrimitiveType _
        | TypeDefn.Void -> false

    /// Whether CoreCLR runs `method` as shared generic code: whether its declaring type's
    /// instantiation or its own has an argument that canonicalises to `__Canon`.
    let isShared
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : TypeSystemState)
        (method : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : bool
        =
        Seq.append method.DeclaringTypeGenerics method.Generics
        |> Seq.exists (canonicalisesToCanon baseClassTypes state)
