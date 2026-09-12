namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

[<RequireQualifiedAccess>]
module StackShapeOfMethod =

    /// The analysis's key for a method, or `None` for a body the interpreter synthesises itself:
    /// the marshal stub keeps conversion results on its stack between re-entries, and the entry
    /// placeholder is not a guest body at all.
    let keyOf (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>) : StackShapeKey option =
        match method with
        | MethodInfo.Metadata (_, facts) ->
            let row =
                MetadataTokens.GetRowNumber (MethodDefinitionHandle.op_Implicit facts.Handle : EntityHandle)

            Some (
                StackShapeKey.Metadata (
                    method.DeclaringAssemblyFullName,
                    row,
                    List.ofSeq method.DeclaringTypeGenerics,
                    List.ofSeq method.Generics
                )
            )
        | MethodInfo.Synthesised (_, SynthesisedMethod.DynamicMethod handle) -> Some (StackShapeKey.Dynamic handle)
        | MethodInfo.Synthesised (_, SynthesisedMethod.StructMarshalStub)
        | MethodInfo.Synthesised (_, SynthesisedMethod.EntryPointPlaceholder) -> None

    let private shapeOfConcreteType (state : IlMachineState) (handle : ConcreteTypeHandle) : SlotShape =
        match handle with
        | ConcretePrimitive state.ConcreteTypes PrimitiveType.Single -> SlotShape.Float FloatWidth.Single
        | ConcretePrimitive state.ConcreteTypes PrimitiveType.Double -> SlotShape.Float FloatWidth.Double
        | _ -> SlotShape.Other

    /// The instantiation the body is running under, as the shapes of its generic parameters.
    let private bindingOf
        (state : IlMachineState)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        : GenericBinding
        =
        let bind (generics : ImmutableArray<ConcreteTypeHandle>) (index : int) : SlotShape =
            if index >= 0 && index < generics.Length then
                shapeOfConcreteType state generics.[index]
            else
                SlotShape.Other

        {
            TypeParameter = bind method.DeclaringTypeGenerics
            MethodParameter = bind method.Generics
        }

    let private inputsOf
        (assembly : DumpedAssembly)
        (binding : GenericBinding)
        (isStatic : bool)
        (mode : CompilationMode)
        (signature : TypeMethodSignature<TypeDefn>)
        (body : MethodInstructions<TypeDefn>)
        (tokens : Map<int, TokenShape>)
        : StackShapeInputs
        =
        let declared =
            signature.ParameterTypes
            |> List.map (StackShapeTokens.shapeOfTypeDefn assembly binding)

        {
            Arguments = ImmutableArray.CreateRange (if isStatic then declared else SlotShape.Other :: declared)
            Locals =
                match body.LocalVars with
                | None -> ImmutableArray.Empty
                | Some vars ->
                    vars
                    |> Seq.map (StackShapeTokens.shapeOfTypeDefn assembly binding)
                    |> ImmutableArray.CreateRange
            ReturnsValue =
                (StackShapeTokens.returnShape assembly binding GenericSubstitution.None signature.ReturnType).IsSome
            Tokens = tokens
            Mode = mode
        }

    /// A body from a PE image: everything comes from the owning assembly's metadata.
    let private analyseMetadata
        (state : IlMachineState)
        (method : MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle>)
        (facts : MetadataMethodFacts)
        : StackShape
        =
        let assembly =
            state.LoadedAssembly method.DeclaringAssemblyFullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"stack shape: assembly %s{method.DeclaringAssemblyFullName} of executing method %s{method.Name} is not loaded"
            )

        let definition = assembly.Methods.[facts.Handle]

        let body =
            match MethodInfo.tryIlBody definition with
            | Some body -> body
            | None ->
                failwith
                    $"stack shape: executing method %s{method.Name} has an IL body, but its definition in %s{assembly.DefinitionFullName} has none"

        let binding = bindingOf state method

        // Only what control can reach is read, as CoreCLR's importer reads only what it
        // imports: a token on dead code may name what cannot be resolved.
        let tokens =
            StackShapeTokens.ofBody assembly binding (StackShape.reachable body) body

        StackShape.analyse
            (inputsOf
                assembly
                binding
                definition.IsStatic
                (StackShapeTokens.compilationModeOf assembly facts.Handle)
                definition.Signature
                body
                tokens)
            body

    /// The signature of a `DynamicMethod` that has not yet been minted, read from the guest
    /// object itself: its `_parameterTypes` array and `_returnType`.
    let private unmintedCalleeShape
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (operation : string)
        (state : IlMachineState)
        (dynamicMethodObject : ManagedHeapAddress)
        : Result<TokenShape, string>
        =
        let dm = ManagedHeap.get dynamicMethodObject state.ManagedHeap

        let arguments =
            match
                AllocatedNonArrayObject.DereferenceField "_parameterTypes" dm
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.ObjectRef None -> Ok 0
            | CliType.ObjectRef (Some parameterTypes) ->
                Ok (ManagedHeap.getArrayShape parameterTypes state.ManagedHeap).Length
            | other -> Error $"%s{operation}: DynamicMethod._parameterTypes is %O{other}, not an array reference"

        let returns =
            match
                AllocatedNonArrayObject.DereferenceField "_returnType" dm
                |> CliType.unwrapPrimitiveLikeDeep
            with
            | CliType.ObjectRef None -> Ok None
            | CliType.ObjectRef (Some returnType) ->
                match
                    NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                        operation
                        state
                        (EvalStackValue.ObjectRef returnType)
                with
                | RuntimeTypeHandleTarget.Closed handle ->
                    let isVoid =
                        match IlMachineState.tryGetConcreteTypeInfo state handle with
                        | Some (concreteType, _) -> concreteType.Identity = baseClassTypes.Void.Identity
                        | None -> false

                    if isVoid then
                        Ok None
                    else
                        Ok (Some (shapeOfConcreteType state handle))
                | _ -> Ok (Some SlotShape.Other)
            | other -> Error $"%s{operation}: DynamicMethod._returnType is %O{other}, not a reference"

        match arguments, returns with
        | Ok arguments, Ok returns -> Ok (TokenShape.Callee (arguments, returns))
        | Error e, _
        | _, Error e -> Error e

    /// The effect of one `DynamicScope` operand, read the way the instruction itself will read it.
    let private dynamicTokenShape
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : DynamicMethodHandle)
        (op : UnaryMetadataTokenIlOp)
        (index : int)
        : Result<TokenShape option, string>
        =
        let operation = $"stack shape of %O{op}"

        match op with
        | UnaryMetadataTokenIlOp.Call ->
            // An entry that is not a dynamic method (a guest can put a `RuntimeMethodHandle`
            // there after the mint) is the call's own problem when it executes.
            match DynamicScopeOperand.tryDynamicMethod baseClassTypes operation index state handle with
            | Error why -> Error why
            | Ok (DynamicMethodResolution.Resolved callee) ->
                let definition =
                    MethodHandleRegistry.resolveDynamicMethod callee state.MethodHandles
                    |> Option.defaultWith (fun () ->
                        failwith $"%s{operation}: %O{callee} is not registered in the method-handle registry"
                    )

                let scopeAssembly =
                    state.LoadedAssembly (definition.GetScopeAssemblyFullName ())
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: scope assembly %s{definition.GetScopeAssemblyFullName ()} is not loaded"
                    )

                let signature =
                    MethodSignatureDecoding.decode
                        scopeAssembly.Name
                        (scopeAssembly.PeReader.GetMetadataReader ())
                        (definition.GetSignature () |> Seq.toArray)
                    |> TypeMethodSignature.make

                Ok (
                    Some (
                        StackShapeTokens.calleeShape
                            scopeAssembly
                            GenericBinding.AtDefinition
                            GenericSubstitution.None
                            signature
                    )
                )
            | Ok (DynamicMethodResolution.NeedsMinting callee) ->
                unmintedCalleeShape baseClassTypes operation state callee |> Result.map Some
        | UnaryMetadataTokenIlOp.Callvirt
        | UnaryMetadataTokenIlOp.Calli
        | UnaryMetadataTokenIlOp.Newobj
        | UnaryMetadataTokenIlOp.Jmp ->
            // The decoder refuses a minted body carrying a scope operand on these, so none
            // executes; see `IlDecoding.scopeOperandKind`.
            failwith
                $"BUG: %O{op} with a DynamicScope operand reached the stack-shape analysis, but IlDecoding refuses such a body at mint"
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldsfld ->
            match DynamicScopeOperand.field baseClassTypes operation index state handle with
            | Error (_, why) -> Error why
            | Ok field ->
                let assembly =
                    state.LoadedAssembly (field.GetAssemblyFullName ())
                    |> Option.defaultWith (fun () ->
                        failwith
                            $"%s{operation}: assembly %s{field.GetAssemblyFullName ()} of a scope field is not loaded"
                    )

                match assembly.Fields.TryGetValue (field.GetFieldDefinitionHandle().Get) with
                | false, _ ->
                    Error $"%s{operation}: the scope field's definition row is not in %s{assembly.DefinitionFullName}"
                | true, fieldInfo ->

                // The handle names the field on a closed type, so a field whose declared type is
                // the declaring type's parameter takes the shape of that argument.
                let typeParameter (index : int) : SlotShape =
                    match field.GetDeclaringTypeHandle () with
                    | RuntimeTypeHandleTarget.Closed declaring ->
                        match IlMachineState.tryGetConcreteTypeInfo state declaring with
                        | Some (concreteType, _) when index >= 0 && index < concreteType.Generics.Length ->
                            shapeOfConcreteType state concreteType.Generics.[index]
                        | _ -> SlotShape.Other
                    | _ -> SlotShape.Other

                Ok (
                    Some (
                        TokenShape.Field (
                            StackShapeTokens.shapeOfTypeDefnBinding
                                assembly
                                typeParameter
                                (fun _ -> SlotShape.Other)
                                fieldInfo.Signature
                        )
                    )
                )
        | UnaryMetadataTokenIlOp.Ldobj
        | UnaryMetadataTokenIlOp.Unbox_Any
        | UnaryMetadataTokenIlOp.Ldelem ->
            match DynamicScopeOperand.closedType baseClassTypes operation index state handle with
            | Error (_, why) -> Error why
            | Ok closed -> Ok (Some (TokenShape.Type (shapeOfConcreteType state closed)))
        | UnaryMetadataTokenIlOp.Castclass
        | UnaryMetadataTokenIlOp.Isinst
        | UnaryMetadataTokenIlOp.Newarr
        | UnaryMetadataTokenIlOp.Box
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Ldelema
        | UnaryMetadataTokenIlOp.Stfld
        | UnaryMetadataTokenIlOp.Stsfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stelem
        | UnaryMetadataTokenIlOp.Initobj
        | UnaryMetadataTokenIlOp.Ldftn
        | UnaryMetadataTokenIlOp.Stobj
        | UnaryMetadataTokenIlOp.Constrained
        | UnaryMetadataTokenIlOp.Ldtoken
        | UnaryMetadataTokenIlOp.Cpobj
        | UnaryMetadataTokenIlOp.Sizeof
        | UnaryMetadataTokenIlOp.Ldvirtftn
        | UnaryMetadataTokenIlOp.Mkrefany
        | UnaryMetadataTokenIlOp.Refanyval -> Ok None

    /// A body minted by `Reflection.Emit`: the signature and locals come from the definition the
    /// mint registered, and each token's effect from the `DynamicScope` entry it names.
    let private analyseDynamic
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (state : IlMachineState)
        (handle : DynamicMethodHandle)
        : StackShape
        =
        let definition =
            MethodHandleRegistry.resolveDynamicMethod handle state.MethodHandles
            |> Option.defaultWith (fun () ->
                failwith
                    $"stack shape: executing dynamic method %O{handle} is not registered in the method-handle registry"
            )

        let scopeAssembly =
            state.LoadedAssembly (definition.GetScopeAssemblyFullName ())
            |> Option.defaultWith (fun () ->
                failwith $"stack shape: scope assembly %s{definition.GetScopeAssemblyFullName ()} is not loaded"
            )

        let signature =
            MethodSignatureDecoding.decode
                scopeAssembly.Name
                (scopeAssembly.PeReader.GetMetadataReader ())
                (definition.GetSignature () |> Seq.toArray)
            |> TypeMethodSignature.make

        // `localsInit` does not affect the stack; the analysis reads only instructions, locals
        // and regions.
        let body = definition.GetBody () |> MintedDynamicMethodBody.withLocalsInit false

        // Only what control can reach is read, as CoreCLR's importer reads only what it
        // imports: a token on dead code may name an entry the interpreter cannot resolve, and
        // the body ran fine before because that instruction never executed.
        let reachable = StackShape.reachable body

        let sourceAssembly (sourced : SourcedMetadataToken) : DumpedAssembly =
            state.LoadedAssembly sourced.SourceAssembly.FullName
            |> Option.defaultWith (fun () ->
                failwith
                    $"stack shape: token source assembly %O{sourced.SourceAssembly} of a minted body is not loaded"
            )

        // An entry the instruction will refuse when it runs gets no shape here: the instruction
        // is recorded as untyped, and what happens when it executes is its own business.
        let tokens =
            body.Instructions
            |> List.choose (fun (instruction, offset) ->
                match instruction with
                | IlOp.UnaryMetadataToken _ when not (reachable.Contains offset) -> None
                | IlOp.UnaryMetadataToken (op, MetadataOperand.FromDynamicScope index) ->
                    match dynamicTokenShape baseClassTypes state handle op index with
                    | Error _ -> None
                    | Ok None -> None
                    | Ok (Some shape) -> Some (offset, shape)
                | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata sourced) ->
                    StackShapeTokens.ofMetadataToken
                        (sourceAssembly sourced)
                        GenericBinding.AtDefinition
                        op
                        sourced.Token
                    |> Option.map (fun shape -> offset, shape)
                | IlOp.Nullary _
                | IlOp.UnaryConst _
                | IlOp.UnaryStringToken _
                | IlOp.Switch _ -> None
            )
            |> Map.ofList

        // A dynamic method is always static, and is compiled the way its scope's module is.
        StackShape.analyse
            (inputsOf
                scopeAssembly
                GenericBinding.AtDefinition
                true
                (StackShapeTokens.dynamicCompilationModeOf scopeAssembly)
                signature
                body
                tokens)
            body

    let private isDebugBuild : bool =
#if DEBUG
        true
#else
        false
#endif

    /// Check the frame about to execute an instruction against its body's stack shape,
    /// computing the shape on the body's first execution, which is the moment CoreCLR would JIT
    /// it. In a Debug build the runtime stack must have the analysis's depth for the instruction,
    /// with a float in every slot the analysis says is one and in no other; an instruction the
    /// analysis found invalid on every path stops the run rather than executing.
    let beforeInstruction
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (thread : ThreadId)
        (state : IlMachineState)
        : IlMachineState
        =
        let frame = state.ThreadState.[thread].MethodState

        match keyOf frame.ExecutingMethod with
        | None -> state
        | Some key ->

        let shape, state =
            match Map.tryFind key state.StackShapes with
            | Some shape -> shape, state
            | None ->
                let shape =
                    match frame.ExecutingMethod with
                    | MethodInfo.Metadata (_, facts) -> analyseMetadata state frame.ExecutingMethod facts
                    | MethodInfo.Synthesised (_, SynthesisedMethod.DynamicMethod handle) ->
                        analyseDynamic baseClassTypes state handle
                    | MethodInfo.Synthesised (_, SynthesisedMethod.StructMarshalStub)
                    | MethodInfo.Synthesised (_, SynthesisedMethod.EntryPointPlaceholder) ->
                        failwith "BUG: keyOf gave a synthesised non-dynamic body a stack-shape key"

                shape,
                { state with
                    StackShapes = Map.add key shape state.StackShapes
                }

        match Map.tryFind frame.IlOpIndex shape.Entry with
        | None ->
            match Map.tryFind frame.IlOpIndex shape.Invalid with
            | Some error when error.IsConflict ->
                // Two paths disagree here. Either the IL is invalid, in which case CoreCLR's
                // importer refuses it and PawPrint runs it as it always has, or one path is an
                // arm the importer never imports (a branch on an intrinsic, say), in which case
                // CoreCLR runs it. Neither is worth stopping over: the join and what follows it
                // are simply untyped, and nothing is asserted.
                state
            | Some (StackShapeError.MissingTokenShape _) ->
                // The token could not be read ahead of time; the instruction raises for the
                // guest, or fails, on its own terms.
                state
            | Some error ->
                // An instruction that cannot run on any path: CoreCLR's importer refuses it when
                // it imports it, which is at the latest when control reaches it.
                failwith
                    $"stack shape: %s{frame.ExecutingMethod.Name} (%O{key}) is executing offset %d{frame.IlOpIndex}, which is invalid IL: %O{error}"
            | None ->
                if shape.Reachable.Contains frame.IlOpIndex then
                    // Reachable only through an offset the analysis could not type.
                    state
                else
                    failwith
                        $"stack shape: %s{frame.ExecutingMethod.Name} (%O{key}) is executing offset %d{frame.IlOpIndex}, which the analysis found unreachable"
        | Some expected ->

        if isDebugBuild then
            let actual = frame.EvaluationStack.Values

            if List.length actual <> expected.Length then
                failwith
                    $"stack shape: %s{frame.ExecutingMethod.Name} (%O{key}) at offset %d{frame.IlOpIndex} has %d{List.length actual} value(s) on its evaluation stack, but the analysis expects %d{expected.Length} (%O{expected}); the stack is %O{actual}"

            // The width of a float is not yet a fact the stack carries, so only its kind is
            // checked: a float where the analysis says one, and nowhere else.
            List.zip actual expected
            |> List.iteri (fun slot (value, shape) ->
                let isFloat =
                    match value with
                    | EvalStackValue.Float _ -> true
                    | _ -> false

                let saysFloat =
                    match shape with
                    | SlotShape.Float _ -> true
                    | SlotShape.Other -> false

                if isFloat <> saysFloat then
                    failwith
                        $"stack shape: %s{frame.ExecutingMethod.Name} (%O{key}) at offset %d{frame.IlOpIndex} has %O{value} in slot %d{slot} from the top, but the analysis says %O{shape}; the stack is %O{actual}, expected %O{expected}"
            )

        state
