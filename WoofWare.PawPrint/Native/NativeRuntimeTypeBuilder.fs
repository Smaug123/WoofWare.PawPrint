namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection

open NativeRuntimeTypeHelpers

/// QCalls declared on `System.Reflection.Emit.RuntimeTypeBuilder`, each of which adds to a dynamic
/// module's metadata what CoreCLR's emitter adds (vm/comdynamic.cpp), and hands back the token.
[<RequireQualifiedAccess>]
module NativeRuntimeTypeBuilder =
    /// `mdtTypeDef`, the table byte of a `TypeDef` token.
    let private typeDefTable : int = 0x02

    /// `mdtMethodDef`, the table byte of a `MethodDef` token.
    let private methodDefTable : int = 0x06

    /// The dynamic module's scope, which only a `RuntimeModuleBuilder`'s module has.
    let private dynamicScope (operation : string) (moduleName : string) (state : IlMachineState) : EmitScope =
        match state.TypeSystem._LoadedAssemblies.TryDynamicScope moduleName with
        | Some scope -> scope
        | None ->
            failwith
                $"%s{operation}: %s{moduleName} is not a dynamic assembly, and only a RuntimeModuleBuilder's module reaches this QCall"

    /// The type a method's parent token names, as `RegMeta::DefineMethod` reads it: a nil token
    /// (`mdTokenNil` or `mdTypeDefNil`, which `ModuleBuilder`'s global type passes) is the module's
    /// `<Module>` (`IsGlobalMethodParent`), and any other is a `TypeDef` of the module.
    let private methodParent (operation : string) (token : int) (scope : EmitScope) : TypeDefRid =
        let rid =
            if token = 0 || token = (typeDefTable <<< 24) then
                1
            elif token >>> 24 &&& 0xFF = typeDefTable then
                token &&& 0x00FFFFFF
            else
                failwith
                    $"%s{operation}: the parent 0x%08x{token} is not a TypeDef token, and the managed caller passes only its TypeBuilder's"

        match EmitScope.tryTypeDefRid rid scope with
        | Some typeDef -> typeDef
        | None -> failwith $"%s{operation}: the parent 0x%08x{token} names no TypeDef row of the module's scope"

    /// The type a `TypeDefOrRef` token names in `scope`. `None` for a nil token (RID 0), as
    /// `IsNilToken` reads one.
    let private typeDefOrRef
        (operation : string)
        (what : string)
        (token : int)
        (scope : EmitScope)
        : TypeDefOrRef option
        =
        let rid = token &&& 0x00FFFFFF

        if rid = 0 then
            None
        else

        let target =
            match token >>> 24 &&& 0xFF with
            | 0x01 -> EmitScope.tryTypeRefRid rid scope |> Option.map TypeDefOrRef.TypeRef
            | 0x02 -> EmitScope.tryTypeDefRid rid scope |> Option.map TypeDefOrRef.TypeDef
            | 0x1B ->
                failwith
                    $"TODO: %s{operation}: %s{what} is the TypeSpec 0x%08x{token}, which ModuleBuilder_GetTokenFromTypeSpec hands out, and an emit scope has no TypeSpec table yet"
            | _ ->
                failwith
                    $"%s{operation}: %s{what} 0x%08x{token} is not a TypeDef, TypeRef or TypeSpec token, which the managed caller's GetTypeTokenInternal alone hands out"

        match target with
        | Some target -> Some target
        | None -> failwith $"%s{operation}: %s{what} 0x%08x{token} names no row of the module's scope"

    /// The method a `MethodDef` or `MemberRef` token names in `scope`, which is what the managed
    /// caller's `GetMethodMetadataToken` hands out for a method.
    let private methodDefOrRef (operation : string) (what : string) (token : int) (scope : EmitScope) : MethodDefOrRef =
        let rid = token &&& 0x00FFFFFF

        let target =
            match token >>> 24 &&& 0xFF with
            | 0x06 -> EmitScope.tryMethodDefRid rid scope |> Option.map MethodDefOrRef.MethodDef
            | 0x0A -> EmitScope.tryMemberRefRid rid scope |> Option.map MethodDefOrRef.MemberRef
            | _ ->
                failwith
                    $"%s{operation}: %s{what} 0x%08x{token} is not a MethodDef or MemberRef token, which the managed caller's GetMethodMetadataToken alone hands out"

        match target with
        | Some target -> target
        | None -> failwith $"%s{operation}: %s{what} 0x%08x{token} names no row of the module's scope"

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
        | "TypeBuilder_DefineType",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.UInt16)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            CorelibType state.TypeSystem.ConcreteTypes ("System.Reflection", "TypeAttributes", attributesGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) when
            moduleGenerics.IsEmpty && attributesGenerics.IsEmpty
            ->
            let operation = "TypeBuilder_DefineType"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected six native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state

            let fullName =
                match NativeCall.managedPointerOfPointerArgument operation "fullname" instruction.Arguments.[1] with
                | ManagedPointerSource.Null ->
                    failwith
                        $"%s{operation}: a null type name; the managed caller has already read the name's first character"
                | namePtr -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            let parent =
                typeDefOrRef operation "the parent" (NativeCall.int32Argument operation instruction.Arguments.[2]) held

            let attributes =
                enum<TypeAttributes> (NativeCall.int32Argument operation instruction.Arguments.[3])

            let enclosing = NativeCall.int32Argument operation instruction.Arguments.[4]

            if enclosing &&& 0x00FFFFFF <> 0 then
                failwith
                    $"TODO: %s{operation}: %s{fullName} is nested in 0x%08x{enclosing}, which CoreCLR's DefineNestedType records in a NestedClass row, and an emit scope has no NestedClass table yet"

            // The managed caller passes its interface tokens with one nil token after them, or no
            // array at all for a type with no interfaces; `_SetImplements` reads up to the first nil.
            let interfaces =
                match
                    NativeCall.managedPointerOfPointerArgument operation "interfaceTokens" instruction.Arguments.[5]
                with
                | ManagedPointerSource.Null -> []
                | tokens ->
                    let int32Type =
                        AllConcreteTypes.findExistingNonGenericConcreteType
                            state.TypeSystem.ConcreteTypes
                            ctx.BaseClassTypes.Int32.Identity
                        |> Option.defaultWith (fun () -> failwith $"%s{operation}: System.Int32 is not concretized")

                    let rec read (index : int) (acc : TypeDefOrRef list) : TypeDefOrRef list =
                        // The managed array holds one token for each of a type's interfaces, and a
                        // type with this many has been refused long before.
                        if index > 0xFFFF then
                            failwith $"%s{operation}: no nil token among the first %d{index} interface tokens"

                        let token =
                            int32AtPointer
                                operation
                                ctx.BaseClassTypes
                                state
                                (ManagedPointerByteView.addByteOffset state int32Type (index * 4) tokens)

                        match typeDefOrRef operation $"interface %d{index}" token held with
                        | None -> List.rev acc
                        | Some target -> read (index + 1) (target :: acc)

                    read 0 []

            match EmitScopeDefinitions.defineTopLevelTypeDef fullName attributes parent interfaces held with
            | TypeDefDefinition.Duplicate _ ->
                // `COMDynamicWrite::DefineType` raises `Argument_DuplicateTypeName` for the
                // emitter's `META_S_DUPLICATE`.
                NativeHandlerResult.raiseExceptionWithMessage
                    ctx.BaseClassTypes.ArgumentException
                    (Some "Duplicate type name within an assembly.")
                    state
                |> Some
            | TypeDefDefinition.Defined (typeDef, extended) ->
                // `AddAvailableClassDontHaveLock` makes the type findable by name; the image read
                // back from the scope holds it, and the type system refuses to load it until it is
                // created.
                let state, _ = state.WithEmitScope ctx.LoggerFactory moduleName extended

                state
                |> IlMachineState.pushToEvalStack
                    (CliType.Numeric (CliNumericType.Int32 ((typeDefTable <<< 24) ||| typeDef.Rid)))
                    ctx.Thread
                |> NativeHandlerResult.completed
                |> Some
        | "TypeBuilder_DefineMethod",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.UInt16)
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            CorelibType state.TypeSystem.ConcreteTypes ("System.Reflection", "MethodAttributes", attributesGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32) when
            moduleGenerics.IsEmpty && attributesGenerics.IsEmpty
            ->
            let operation = "TypeBuilder_DefineMethod"

            if instruction.Arguments.Length <> 6 then
                failwith $"%s{operation}: expected six native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state

            let owner =
                methodParent operation (NativeCall.int32Argument operation instruction.Arguments.[1]) held

            let name =
                match NativeCall.managedPointerOfPointerArgument operation "wszName" instruction.Arguments.[2] with
                | ManagedPointerSource.Null ->
                    failwith $"%s{operation}: a null method name; the managed caller refuses a null name"
                | namePtr -> NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state namePtr

            // The managed caller passes its signature buffer, which may be longer than the signature.
            let signature =
                NativeCall.readCountedBytes
                    operation
                    ctx.BaseClassTypes
                    state
                    (NativeCall.managedPointerOfPointerArgument operation "pSignature" instruction.Arguments.[3])
                    (NativeCall.int32Argument operation instruction.Arguments.[4])
                |> ImmutableArray.CreateRange

            let attributes =
                enum<MethodAttributes> (NativeCall.int32Argument operation instruction.Arguments.[5])

            // `DefinePInvokeMethod` defines the row first and gives it its import afterwards, through
            // `TypeBuilder_SetPInvokeData`; an image holding a P/Invoke row without an import is not
            // one `MethodInfo.read` can read.
            if attributes.HasFlag MethodAttributes.PinvokeImpl then
                failwith
                    $"TODO: %s{operation}: %s{name} is a P/Invoke, whose import TypeBuilder_SetPInvokeData writes in an ImplMap row after this; an emit scope has no ImplMap table yet, and MethodInfo.read refuses a P/Invoke row without one"

            let method, extended =
                EmitScopeDefinitions.defineMethod owner name signature attributes held

            let state, _ = state.WithEmitScope ctx.LoggerFactory moduleName extended

            state
            |> IlMachineState.pushToEvalStack
                (CliType.Numeric (CliNumericType.Int32 ((methodDefTable <<< 24) ||| method.Rid)))
                ctx.Thread
            |> NativeHandlerResult.completed
            |> Some
        | "TypeBuilder_SetMethodImpl",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            CorelibType state.TypeSystem.ConcreteTypes ("System.Reflection", "MethodImplAttributes", attributesGenerics) ],
          MethodReturnType.Void when moduleGenerics.IsEmpty && attributesGenerics.IsEmpty ->
            let operation = "TypeBuilder_SetMethodImpl"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state
            let token = NativeCall.int32Argument operation instruction.Arguments.[1]

            let method =
                if token >>> 24 &&& 0xFF <> methodDefTable then
                    failwith
                        $"%s{operation}: 0x%08x{token} is not a MethodDef token, and the managed caller passes only its MethodBuilder's"

                match EmitScope.tryMethodDefRid (token &&& 0x00FFFFFF) held with
                | Some method -> method
                | None -> failwith $"%s{operation}: 0x%08x{token} names no MethodDef row of the module's scope"

            let attributes =
                enum<MethodImplAttributes> (NativeCall.int32Argument operation instruction.Arguments.[2])

            // A row with no body is one `MethodInfo.read` reads only as abstract, an InternalCall,
            // runtime-provided, or managed IL; real .NET holds any of these until `CreateType`.
            let row = (EmitScope.methodDefs held).[method.Rid - 1]
            let codeType = attributes &&& MethodImplAttributes.CodeTypeMask

            if
                row.Body.IsNone
                && not (row.Flags.HasFlag MethodAttributes.Abstract)
                && not (attributes.HasFlag MethodImplAttributes.InternalCall)
                && codeType <> MethodImplAttributes.Runtime
                && (codeType <> MethodImplAttributes.IL
                    || attributes &&& MethodImplAttributes.ManagedMask <> MethodImplAttributes.Managed)
            then
                failwith
                    $"TODO: %s{operation}: the implementation flags %O{attributes} for MethodDef 0x%08x{token}, which has no body; real .NET holds such a row until CreateType, but MethodInfo.read refuses a bodiless method that is not managed IL"

            let extended = EmitScopeDefinitions.setMethodImplFlags method attributes held
            let state, _ = state.WithEmitScope ctx.LoggerFactory moduleName extended

            NativeHandlerResult.completed state |> Some
        | "TypeBuilder_DefineMethodImpl",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void when moduleGenerics.IsEmpty ->
            let operation = "TypeBuilder_DefineMethodImpl"

            if instruction.Arguments.Length <> 4 then
                failwith $"%s{operation}: expected four native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state
            let typeToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            // The managed caller passes its TypeBuilder's own token; only `ModuleBuilder`'s global
            // type has a nil one, and nothing reaches `DefineMethodOverride` on it.
            let owner =
                if typeToken >>> 24 &&& 0xFF <> typeDefTable || typeToken &&& 0x00FFFFFF = 0 then
                    failwith
                        $"%s{operation}: 0x%08x{typeToken} is not a non-nil TypeDef token, and the managed caller passes only its TypeBuilder's"

                match EmitScope.tryTypeDefRid (typeToken &&& 0x00FFFFFF) held with
                | Some owner -> owner
                | None -> failwith $"%s{operation}: 0x%08x{typeToken} names no TypeDef row of the module's scope"

            let body =
                methodDefOrRef operation "the body" (NativeCall.int32Argument operation instruction.Arguments.[2]) held

            let declaration =
                methodDefOrRef
                    operation
                    "the declaration"
                    (NativeCall.int32Argument operation instruction.Arguments.[3])
                    held

            // `RegMeta::DefineMethodImpl` looks for a duplicate only under `MDDupMethodDef`, which a
            // dynamic module's emitter does not set, so each call is a new row.
            let extended =
                EmitScope.addMethodImpl
                    {
                        Class = owner
                        Body = body
                        Declaration = declaration
                    }
                    held

            let state, _ = state.WithEmitScope ctx.LoggerFactory moduleName extended

            NativeHandlerResult.completed state |> Some
        | "TypeBuilder_SetParentType",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void when moduleGenerics.IsEmpty ->
            let operation = "TypeBuilder_SetParentType"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state
            let typeToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            // `CreateTypeNoLock` calls this only when both tokens are non-nil, so never for the global
            // type or an interface.
            let owner =
                if typeToken >>> 24 &&& 0xFF <> typeDefTable || typeToken &&& 0x00FFFFFF = 0 then
                    failwith
                        $"%s{operation}: 0x%08x{typeToken} is not a non-nil TypeDef token, and the managed caller passes only its TypeBuilder's"

                match EmitScope.tryTypeDefRid (typeToken &&& 0x00FFFFFF) held with
                | Some owner -> owner
                | None -> failwith $"%s{operation}: 0x%08x{typeToken} names no TypeDef row of the module's scope"

            let parentToken = NativeCall.int32Argument operation instruction.Arguments.[2]

            let parent =
                match typeDefOrRef operation "the parent" parentToken held with
                | Some parent -> parent
                | None ->
                    failwith
                        $"%s{operation}: the parent 0x%08x{parentToken} is nil, and the managed caller passes only a non-nil one"

            // `RegMeta::SetTypeParent` overwrites the column and checks nothing else; the extension
            // check refuses it for a created type, which the managed caller's `ThrowIfCreated` and
            // `IsCreated` keep from reaching here.
            let extended = EmitScope.setTypeDefExtends owner parent held
            let state, _ = state.WithEmitScope ctx.LoggerFactory moduleName extended

            NativeHandlerResult.completed state |> Some
        | "TypeBuilder_SetMethodIL",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Byte)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (CorelibType state.TypeSystem.ConcreteTypes ("System.Reflection.Emit",
                                                                         "ExceptionHandler",
                                                                         handlerGenerics))
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            ConcretePointer (ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Void when moduleGenerics.IsEmpty && handlerGenerics.IsEmpty ->
            let operation = "TypeBuilder_SetMethodIL"

            if instruction.Arguments.Length <> 12 then
                failwith $"%s{operation}: expected twelve native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state
            let methodToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            let method =
                if methodToken >>> 24 &&& 0xFF <> methodDefTable then
                    failwith
                        $"%s{operation}: 0x%08x{methodToken} is not a MethodDef token, and the managed caller passes only its MethodBuilder's"

                match EmitScope.tryMethodDefRid (methodToken &&& 0x00FFFFFF) held with
                | Some method -> method
                | None -> failwith $"%s{operation}: 0x%08x{methodToken} names no MethodDef row of the module's scope"

            // `Interop.BOOL`: any nonzero value is true.
            let initLocals = NativeCall.int32Argument operation instruction.Arguments.[2] <> 0

            let readBytes (what : string) (pointerIndex : int) (lengthIndex : int) : byte array =
                let length = NativeCall.int32Argument operation instruction.Arguments.[lengthIndex]

                let pointer =
                    NativeCall.managedPointerOfPointerArgument operation what instruction.Arguments.[pointerIndex]

                NativeCall.readCountedBytes $"%s{operation}: %s{what}" ctx.BaseClassTypes state pointer length

            let code = readBytes "body" 3 4
            let localSignature = readBytes "localSig" 5 6

            // The native parameter is a `UINT16`, so only the low 16 bits of the managed `int` arrive.
            let maxStack =
                uint16 (NativeCall.int32Argument operation instruction.Arguments.[7] &&& 0xFFFF)

            let exceptionCount = NativeCall.int32Argument operation instruction.Arguments.[9]

            if exceptionCount <> 0 then
                failwith
                    $"TODO: %s{operation}: 0x%08x{methodToken} of %s{moduleName} has %d{exceptionCount} exception handlers, whose section of the method body is not implemented"

            // The token fixups are relocations for an image saved to disk: they move no byte of a
            // dynamic module's body in memory, so nothing here reads them.

            // An empty local signature is the two bytes 0 0, which `CreateType` never passes: it passes
            // `IMAGE_CEE_CS_CALLCONV_LOCAL_SIG` and a count even for no locals, and the emitter gives
            // that a `StandAloneSig` row like any other.
            let localSignatureRow, held =
                if localSignature = [| 0uy ; 0uy |] then
                    None, held
                else
                    let rid, held =
                        EmitScopeReferences.defineStandAloneSig (ImmutableArray.Create<byte> localSignature) held

                    Some rid, held

            let body =
                EmitScopeDefinitions.methodBody initLocals maxStack (ImmutableArray.Create<byte> code) localSignatureRow

            // CoreCLR sets the method's RVA only for code that is not empty, so an abstract method,
            // whose code is, keeps no body; its local signature's row is added all the same.
            let extended =
                if code.Length = 0 then
                    held
                else
                    EmitScope.setMethodBody method body held

            let state, _ = state.WithEmitScope ctx.LoggerFactory moduleName extended

            NativeHandlerResult.completed state |> Some
        | "TypeBuilder_TermCreateClass",
          "System.Private.CoreLib",
          "System.Reflection.Emit",
          "RuntimeTypeBuilder",
          [ CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", moduleGenerics)
            ConcretePrimitive state.TypeSystem.ConcreteTypes PrimitiveType.Int32
            CorelibType state.TypeSystem.ConcreteTypes ("System.Runtime.CompilerServices",
                                                        "ObjectHandleOnStack",
                                                        handleGenerics) ],
          MethodReturnType.Void when moduleGenerics.IsEmpty && handleGenerics.IsEmpty ->
            let operation = "TypeBuilder_TermCreateClass"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let moduleName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let held = dynamicScope operation moduleName state
            let typeToken = NativeCall.int32Argument operation instruction.Arguments.[1]

            let retType =
                NativeCall.objectHandleOnStackTarget operation state "retType" instruction.Arguments.[2]

            if typeToken &&& 0x00FFFFFF = 0 then
                failwith
                    $"TODO: %s{operation}: the nil token 0x%08x{typeToken} is the global type of %s{moduleName}, which ModuleBuilder.CreateGlobalFunctions creates; creating <Module> is not implemented"

            let created =
                if typeToken >>> 24 &&& 0xFF <> typeDefTable then
                    failwith
                        $"%s{operation}: 0x%08x{typeToken} is not a TypeDef token, and the managed caller passes only its TypeBuilder's"

                match EmitScope.tryTypeDefRid (typeToken &&& 0x00FFFFFF) held with
                | Some created -> created
                | None -> failwith $"%s{operation}: 0x%08x{typeToken} names no TypeDef row of the module's scope"

            // `ReflectionModule::AddClass` marks the type created before the class loader loads it,
            // and PawPrint's loader refuses a type of a dynamic module that is not created. The
            // managed caller returns early for a created type, so a second creation is a fault here.
            let state = state.WithCreatedType moduleName created

            let assembly =
                state.LoadedAssembly moduleName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: %s{moduleName} is not loaded")

            let typeInfo =
                assembly.TypeDefs.[System.Reflection.Metadata.Ecma335.MetadataTokens.TypeDefinitionHandle created.Rid]

            if not typeInfo.Generics.IsEmpty then
                failwith
                    $"%s{operation}: %s{typeInfo.Namespace}.%s{typeInfo.Name} of %s{moduleName} has generic parameters, which a dynamic module's scope cannot yet hold"

            // CoreCLR's `LoadTypeHandleForTypeKey` loads the type fully here, raising
            // `TypeLoadException` for a type it rejects. PawPrint, as for any type, builds the type's
            // layout when it is first needed, and refuses there what CoreCLR would reject; a guest
            // that catches `TypeLoadException` from `CreateType` is not modelled.
            let stk =
                LoadedTypeInfo.signatureTypeKind ctx.BaseClassTypes state.TypeSystem._LoadedAssemblies typeInfo

            let state, handle =
                IlMachineState.concretizeType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    state
                    moduleName
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.FromDefinition (typeInfo.Identity, stk))

            let runtimeType, state =
                IlMachineState.getOrAllocateType
                    ctx.LoggerFactory
                    ctx.BaseClassTypes
                    (RuntimeTypeHandleTarget.Closed handle)
                    state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    (ManagedPointerSource.requireAddressed retType)
                    (CliType.ObjectRef (Some runtimeType))

            NativeHandlerResult.completed state |> Some
        | _ -> None
