namespace WoofWare.PawPrint

open System

[<RequireQualifiedAccess>]
module IntrinsicMethodKeys =
    /// A method's identity as `Intrinsics.call` matches it and as its refusals name it.
    type IntrinsicMethodKey =
        {
            /// The definition identity of the declaring assembly, which is what a `MethodInfo`
            /// already carries.
            DeclaringAssemblyFullName : string
            DeclaringTypeFullName : string
            MethodName : string
            ParameterShapes : string list

            /// The shape of what the method returns, in the same vocabulary as `ParameterShapes`.
            /// Overloads that differ only here are distinct methods with distinct bodies:
            /// `System.Int128` declares sixteen `op_Explicit` overloads that all take a single
            /// `System.Int128`.
            ReturnShape : MethodReturnType<string>
        }

    let methodKey
        (state : IlMachineState)
        (methodToCall : WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : IntrinsicMethodKey
        =
        let declaringAssy =
            match state.LoadedAssembly methodToCall.DeclaringAssemblyFullName with
            | Some assy -> assy
            | None ->
                failwith
                    $"Intrinsic method key requested for method whose declaring assembly is not loaded: %O{methodToCall}"

        let declaringType =
            declaringAssy.TypeDefs.[methodToCall.RequiredDeclaringType.Definition.Get]

        let concreteTypeShape (handle : ConcreteTypeHandle) : string =
            match handle with
            | ConcreteTypeHandle.Concrete _ ->
                match AllConcreteTypes.lookup handle state.ConcreteTypes with
                | Some ct ->
                    if String.IsNullOrEmpty ct.Namespace then
                        ct.Name
                    else
                        $"%s{ct.Namespace}.%s{ct.Name}"
                | None -> failwith $"Intrinsic method key requested for unknown concrete type handle: %O{handle}"
            | ConcreteTypeHandle.Byref _ -> "&"
            | ConcreteTypeHandle.Pointer _ -> "*"
            | ConcreteTypeHandle.FunctionPointer _ -> "fnptr"
            | ConcreteTypeHandle.OneDimArrayZero _ -> "[]"
            | ConcreteTypeHandle.Array (_, rank) -> $"[%i{rank}]"

        {
            DeclaringAssemblyFullName = methodToCall.DeclaringAssemblyFullName
            DeclaringTypeFullName = TypeInfo.fullName (fun h -> declaringAssy.TypeDefs.[h]) declaringType
            MethodName = methodToCall.Name
            ParameterShapes = methodToCall.Signature.ParameterTypes |> List.map concreteTypeShape
            ReturnShape =
                // Note that `MethodReturnType.Void` is strictly the bare `void` column: a `void`
                // under custom modifiers, which is how C# spells every `init` accessor, decodes as
                // `Returns` (see `TypeMethodSignature`'s own docstring). Mirroring the decoded
                // column rather than re-classifying it keeps this key saying what the blob said.
                match methodToCall.Signature.ReturnType with
                | MethodReturnType.Void -> MethodReturnType.Void
                | MethodReturnType.Returns handle -> MethodReturnType.Returns (concreteTypeShape handle)
        }

    let formatMethodKey (key : IntrinsicMethodKey) : string =
        let parameters = key.ParameterShapes |> String.concat ", "

        let returns =
            match key.ReturnShape with
            | MethodReturnType.Void -> "void"
            | MethodReturnType.Returns shape -> shape

        $"%s{AssemblyDefinitionName.simpleName key.DeclaringAssemblyFullName} %s{key.DeclaringTypeFullName}.%s{key.MethodName}(%s{parameters}) : %s{returns}"
