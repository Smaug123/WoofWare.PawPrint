namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

open NativeRuntimeTypeHelpers

/// QCalls declared on `System.Reflection.Emit.RuntimeModuleBuilder`, each of which adds to a dynamic
/// module's metadata what CoreCLR's emitter adds (vm/commodule.cpp), and hands back the token.
[<RequireQualifiedAccess>]
module NativeRuntimeModuleBuilder =
    /// `mdtTypeRef`, the table byte of a `TypeRef` token.
    let private typeRefTable : int = 0x01

    /// `mdtMemberRef`, the table byte of a `MemberRef` token.
    let private memberRefTable : int = 0x0A

    /// `mdtTypeSpec`, the table byte of a `TypeSpec` token.
    let private typeSpecTable : int = 0x1B

    let tryExecuteQCall (entryPoint : string) (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            entryPoint,
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "ModuleBuilder_GetTypeRef",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeModuleBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.UInt16)
            CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", refedGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) when
            moduleGenerics.IsEmpty && refedGenerics.IsEmpty
            ->
            let operation = "ModuleBuilder_GetTypeRef"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held =
                match state.TypeSystem._LoadedAssemblies.TryDynamicScope moduleName with
                | Some scope -> scope
                | None ->
                    failwith
                        $"%s{operation}: %s{moduleName} is not a dynamic assembly, and only a RuntimeModuleBuilder's module reaches this QCall"

            let fullName =
                match NativeCall.managedPointerOfPointerArgument operation "strFullName" instruction.Arguments.[1] with
                | ManagedPointerSource.Null ->
                    failwith
                        $"TODO: %s{operation}: a null type name, for which CoreCLR raises ArgumentNullException; the managed caller passes Type.FullName, which it never lets be null"
                | namePtr -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            let refedName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[2] |> EvalStackValue.ofCliType)

            let tkResolution = NativeCall.int32Argument operation instruction.Arguments.[3]
            let name = EmitScopeReferences.unescapeTypeName fullName

            if refedName = moduleName then
                failwith
                    $"TODO: %s{operation}: a TypeRef from %s{moduleName} to its own type %s{name}, which CoreCLR finds by name among the module's TypeDefs; the managed caller asks for one only for a type of this module that is not a TypeBuilder, which only CreateType makes"

            // A non-zero RID is the TypeRef of the enclosing type, which an earlier call handed out:
            // the reference is to a nested type. A zero one asks for a top-level type, scoped by the
            // AssemblyRef of the assembly that defines it.
            let resolution, scope, emittedFor =
                if tkResolution &&& 0x00FFFFFF <> 0 then
                    if tkResolution >>> 24 <> typeRefTable then
                        failwith
                            $"%s{operation}: resolution token 0x%08x{tkResolution} for %s{name} is not a TypeRef; the managed caller passes only its enclosing type's TypeRef"

                    match EmitScope.tryTypeRefRid (tkResolution &&& 0x00FFFFFF) held with
                    | Some enclosing -> ResolutionScope.TypeRef enclosing, held, None
                    | None ->
                        failwith
                            $"%s{operation}: resolution token 0x%08x{tkResolution} for %s{name} names no TypeRef row of %s{moduleName}"
                else
                    let refed =
                        state.LoadedAssembly refedName
                        |> Option.defaultWith (fun () ->
                            failwith $"%s{operation}: the module of %s{name}, %s{refedName}, is not loaded"
                        )

                    let request = AssemblyRefRequest.ofDefinition (refed.PeReader.GetMetadataReader ())
                    let assemblyRef, scope = EmitScopeReferences.defineAssemblyRef request held
                    ResolutionScope.AssemblyRef assemblyRef, scope, Some (assemblyRef, refed)

            let typeRef, extended =
                EmitScopeReferences.defineTypeRefByName resolution name scope

            let state =
                // Found rather than added, so there is nothing to re-read.
                if Object.ReferenceEquals (extended, held) then
                    state
                else
                    state.WithEmitScope ctx.LoggerFactory moduleName extended |> fst

            // `ModuleBuilder_GetTypeRef` records the assembly in the module's rid map, where a row
            // already recorded keeps its first assembly.
            let state =
                match emittedFor with
                | None -> state
                | Some (assemblyRef, refed) -> state.WithAssemblyRefTarget moduleName assemblyRef refed

            state
            |> IlMachineState.pushToEvalStack
                (CliType.Numeric (CliNumericType.Int32 ((typeRefTable <<< 24) ||| typeRef.Rid)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "ModuleBuilder_GetMemberRefOfMethodInfo",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeModuleBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            CorelibType state.TypeSystem.ConcreteTypes ("System", "RuntimeMethodHandleInternal", methodGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) when
            moduleGenerics.IsEmpty && methodGenerics.IsEmpty
            ->
            let operation = "ModuleBuilder_GetMemberRefOfMethodInfo"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held =
                match state.TypeSystem._LoadedAssemblies.TryDynamicScope moduleName with
                | Some scope -> scope
                | None ->
                    failwith
                        $"%s{operation}: %s{moduleName} is not a dynamic assembly, and only a RuntimeModuleBuilder's module reaches this QCall"

            let tr = NativeCall.int32Argument operation instruction.Arguments.[1]

            let identity =
                NativeRuntimeMethodHandle.resolveMetadataIdentityFromArg operation state instruction.Arguments.[2]

            let declaringName = identity.GetAssemblyFullName ()

            let declaring =
                state.LoadedAssembly declaringName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: %s{declaringName} is not loaded")

            let reader = declaring.PeReader.GetMetadataReader ()

            let definition =
                reader.GetMethodDefinition (identity.GetMethodDefinitionHandle().Get)

            let name = reader.GetString definition.Name
            let declaringType = reader.GetTypeDefinition (definition.GetDeclaringType ())

            let described =
                $"%s{reader.GetString declaringType.Namespace}.%s{reader.GetString declaringType.Name}::%s{name} of %s{declaringName}"

            if declaringName = moduleName then
                failwith
                    $"TODO: %s{operation}: a reference from %s{moduleName} to its own method %s{name}, for which CoreCLR hands back the method's MethodDef token; only a created type of the dynamic module has a RuntimeMethodInfo, and TypeBuilder_TermCreateClass is not implemented"

            // An instantiated generic method's reference is to its definition: CoreCLR reads the name
            // and signature of the MethodDesc's MethodDef. No assembly PawPrint loads is collectible, so
            // the emitter's collectible-assembly check never applies.
            let signature = reader.GetBlobContent definition.Signature

            if EmitScopeReferences.methodSignatureNamesTypeByToken signature then
                failwith
                    $"TODO: %s{operation}: the signature of %s{described} names a type by token, which CoreCLR's TranslateSigWithScope rewrites to name a TypeRef of %s{moduleName}, adding TypeRef and AssemblyRef rows; PawPrint copies only a signature that names no type by token"

            let parent =
                match tr >>> 24 with
                | table when table = typeRefTable ->
                    match EmitScope.tryTypeRefRid (tr &&& 0x00FFFFFF) held with
                    | Some rid -> MemberRefParent.TypeRef rid
                    | None ->
                        failwith
                            $"%s{operation}: parent token 0x%08x{tr} for %s{described} names no TypeRef row of %s{moduleName}"
                | table when table = typeSpecTable ->
                    failwith
                        $"TODO: %s{operation}: parent token 0x%08x{tr} for %s{described} is a TypeSpec, which the managed caller makes for a generic type with ModuleBuilder_GetTokenFromTypeSpec; an emit scope has no TypeSpec table yet"
                | _ ->
                    failwith
                        $"%s{operation}: parent token 0x%08x{tr} for %s{described} is neither a TypeRef nor a TypeSpec; the managed caller passes its declaring type's token, which is a TypeDef only for a type of this module"

            let memberRef, extended =
                EmitScopeReferences.defineMemberRef parent name signature held

            let state =
                // Found rather than added, so there is nothing to re-read.
                if Object.ReferenceEquals (extended, held) then
                    state
                else
                    state.WithEmitScope ctx.LoggerFactory moduleName extended |> fst

            state
            |> IlMachineState.pushToEvalStack
                (CliType.Numeric (CliNumericType.Int32 ((memberRefTable <<< 24) ||| memberRef.Rid)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | _ -> None
