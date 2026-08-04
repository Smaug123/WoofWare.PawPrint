namespace WoofWare.PawPrint

open System.Collections.Immutable
open Microsoft.Extensions.Logging

open NativeRuntimeTypeHelpers

[<RequireQualifiedAccess>]
module NativeRuntimeTypeFCall =
    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            ctx.TargetAssembly.Name.Name,
            ctx.TargetType.Namespace,
            ctx.TargetType.Name,
            instruction.ExecutingMethod.Name,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        | "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "MethodTable",
          "GetNumInstanceFieldBytes",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.UInt32) ->
            let operation = "MethodTable.GetNumInstanceFieldBytes"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let methodTableArg, state = IlMachineState.popEvalStack ctx.Thread state
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let bytes, state =
                MethodTableProjection.numInstanceFieldBytes ctx.BaseClassTypes state methodTableFor

            let state =
                IlMachineState.pushToEvalStack (NativeCall.cliUInt32 bytes) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System.Runtime.CompilerServices",
          "MethodTable",
          "GetPrimitiveCorElementType",
          [],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "CorElementType",
                                                                      corElementTypeGenerics)) when
            corElementTypeGenerics.IsEmpty
            ->
            let operation = "MethodTable.GetPrimitiveCorElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let methodTableArg, state = IlMachineState.popEvalStack ctx.Thread state
            let methodTableFor = NativeCall.methodTableOfEvalStackValue operation methodTableArg

            let elementType =
                primitiveMethodTableCorElementType operation ctx.BaseClassTypes state methodTableFor

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetFields",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr)
            ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetFields"

            if instruction.Arguments.Length <> 3 then
                failwith $"%s{operation}: expected three native arguments, got %d{instruction.Arguments.Length}"

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let resultBuffer =
                NativeCall.managedPointerOfPointerArgument operation "result buffer" instruction.Arguments.[1]

            let countPtr =
                NativeCall.managedPointerOfPointerArgument operation "count pointer" instruction.Arguments.[2]

            let capacity = int32AtPointer operation ctx.BaseClassTypes state countPtr

            let state, fieldHandleIds =
                match typeHandleTarget with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic type definition %O{identity}; expected behavior is to enumerate the canonical type's non-literal fields"
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    // A generic parameter (type- or method-level) has no instance fields of its
                    // own — it's a TypeVarTypeDesc in CoreCLR, not a field-bearing type. CoreCLR's
                    // RuntimeTypeHandle.GetFields returns an empty array for typeof(T).GetFields().
                    state, []
                | RuntimeTypeHandleTarget.Closed typeHandle ->
                    walkClosedTypeHandleFields ctx.LoggerFactory ctx.BaseClassTypes operation typeHandle state

            let count = List.length fieldHandleIds

            let state =
                if count > capacity then
                    writeInt32AtPointer ctx.BaseClassTypes state countPtr count
                else
                    let state =
                        ((state, 0), fieldHandleIds)
                        ||> List.fold (fun (state, index) fieldHandleId ->
                            writeFieldHandleElement operation ctx.BaseClassTypes state resultBuffer index fieldHandleId,
                            index + 1
                        )
                        |> fst

                    writeInt32AtPointer ctx.BaseClassTypes state countPtr count

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool (count <= capacity)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetUtf8NameInternal",
          [ ConcretePointer (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                               "System.Runtime.CompilerServices",
                                                               "MethodTable",
                                                               methodTableGenerics)) ],
          MethodReturnType.Returns (ConcretePointer (ConcreteVoid state.ConcreteTypes)) when methodTableGenerics.IsEmpty ->
            // CoreCLR's RuntimeTypeHandle::GetUtf8Name (runtimehandles.cpp:732) is an FCall
            // that reads the type's UTF-8 name straight out of the metadata string heap:
            // `GetNameOfTypeDef(pMT->GetCl(), &name, NULL)`. That is the TypeDef row's Name
            // column, so it is the *short* name — no namespace, no declaring type for a
            // nested type, and the arity mangling of a generic type is retained
            // (`IList`1`). For a generic instantiation the MethodTable's `GetCl()` is the
            // open definition's token, so `IList<int>` also answers `IList`1`.
            //
            // PawPrint materialises the name as a freshly-allocated null-terminated UTF-8
            // byte[] and returns a byref to it; the managed wrapper wraps that in
            // MdUtf8String, which strlens the pointer to discover the byte length. Mirrors
            // the RuntimeMethodHandle / RuntimeFieldHandle GetUtf8NameInternal handlers.
            let operation = "RuntimeTypeHandle.GetUtf8NameInternal"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let typeHandle =
                NativeCall.methodTableOfEvalStackValue operation (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            // Only a type with a TypeDef row has a name to read. CoreCLR asserts exactly
            // this (`_ASSERTE(!IsNilToken(tkTypeDef))`) and relies on the managed wrapper
            // RuntimeTypeHandle.GetUtf8Name (RuntimeHandles.cs:667-673) having already
            // thrown ArgumentException for TypeDescs and for arrays — whose MethodTables
            // are synthesised and carry a nil token. Reaching here with one of those shapes
            // means the wrapper's guard was bypassed, so fail rather than invent a name.
            let name =
                match typeHandle with
                | ConcreteTypeHandle.Concrete _ ->
                    match IlMachineState.tryGetConcreteTypeInfo state typeHandle with
                    | Some (_, typeInfo) -> typeInfo.Name
                    | None -> failwith $"%s{operation}: concrete type handle was not registered: %O{typeHandle}"
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ ->
                    failwith
                        $"%s{operation}: array type %O{typeHandle} reached the FCall; arrays have no TypeDef row, and the managed wrapper throws ArgumentException for them before this point"
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _ ->
                    failwith
                        $"%s{operation}: TypeDesc handle %O{typeHandle} reached the FCall; the managed wrapper throws ArgumentException for `IsTypeDesc` before this point"

            let namePtr, state =
                NativeCall.allocateNullTerminatedUtf8 ctx.BaseClassTypes name state

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.ManagedPointer namePtr) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetInterfaces",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteTypeHandle.OneDimArrayZero (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                                                          "System",
                                                                                                          "Type",
                                                                                                          returnTypeGenerics))) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetInterfaces"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let state =
                requireEmptyInterfaceMap ctx.LoggerFactory ctx.BaseClassTypes operation state typeHandleTarget

            let arrayAddr, state =
                allocateEmptyTypeArray ctx.LoggerFactory ctx.BaseClassTypes state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some arrayAddr)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetCorElementType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "CorElementType",
                                                                      corElementTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && corElementTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetCorElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let elementType = corElementType operation ctx.BaseClassTypes state typeHandleTarget

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 elementType)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetToken",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetToken"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let token =
                typeDefinitionTokenOfRuntimeTypeHandleTarget operation state typeHandleTarget

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 token)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "IsGenericVariable",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.IsGenericVariable"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let isGenericVariable =
                match target with
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> true
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                | RuntimeTypeHandleTarget.Closed _ -> false

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool isGenericVariable) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetGenericVariableIndex",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            // CoreCLR's public RuntimeTypeHandle.GetGenericVariableIndex wrapper guards this
            // InternalCall with an IsGenericVariable check that throws InvalidOperationException
            // for non-parameter targets. Reaching here on a non-parameter target means the
            // wrapper's invariant was violated, so fail loudly.
            let operation = "RuntimeTypeHandle.GetGenericVariableIndex"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let index =
                match target with
                | RuntimeTypeHandleTarget.GenericParameter (_, position)
                | RuntimeTypeHandleTarget.MethodGenericParameter (_, _, position) -> position
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                | RuntimeTypeHandleTarget.Closed _ ->
                    failwith
                        $"%s{operation} called on non-parameter target %O{target}: managed wrapper should have rejected this"

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 index)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringMethod",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "IRuntimeMethodInfo",
                                                                      methodInfoGenerics)) when
            runtimeTypeGenerics.IsEmpty && methodInfoGenerics.IsEmpty
            ->
            // GetDeclaringMethod returns null for type-level generic parameters and
            // non-parameter targets, and the declaring IRuntimeMethodInfo for
            // method-level generic parameters.
            let operation = "RuntimeTypeHandle.GetDeclaringMethod"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            match target with
            | RuntimeTypeHandleTarget.GenericParameter _
            | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
            | RuntimeTypeHandleTarget.Closed _ ->
                // Type-level generic parameters and non-parameter targets return null.
                let state = NativeCall.pushObjectTarget None ctx.Thread state
                NativeHandlerResult.completed state |> Some
            | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                failwith
                    $"TODO: %s{operation} for method generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}; need to allocate/return IRuntimeMethodInfo"
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetDeclaringType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetDeclaringType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let declaringTypeAddr, state =
                declaringRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget declaringTypeAddr ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "ContainsGenericVariables",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            runtimeTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.ContainsGenericVariables"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let result = containsGenericVariables operation state typeHandleTarget

            let state = IlMachineState.pushToEvalStack (CliType.ofBool result) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetBaseType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetBaseType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let baseTypeAddr, state =
                baseRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget baseTypeAddr ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetElementType",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeType",
                                                                      returnTypeGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnTypeGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetElementType"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let elementTypeAddr, state =
                elementRuntimeType ctx.LoggerFactory ctx.BaseClassTypes state typeHandleTarget

            let state = NativeCall.pushObjectTarget elementTypeAddr ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetAssembly",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeAssembly",
                                                                      runtimeAssemblyGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeAssemblyGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetAssembly"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName =
                NativeCall.typeAssemblyName operation ctx.BaseClassTypes state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeAssembly ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetModule",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeModule",
                                                                      runtimeModuleGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            let operation = "RuntimeTypeHandle.GetModule"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName =
                NativeCall.typeAssemblyName operation ctx.BaseClassTypes state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeModule ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetAssemblyIfExists",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeAssembly",
                                                                      runtimeAssemblyGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeAssemblyGenerics.IsEmpty
            ->
            // .NET 10 InternalCall fast path: returns the cached RuntimeAssembly for the type, or
            // null if the runtime hasn't materialised one yet. PawPrint always has the assembly
            // available, so we can produce the same RuntimeAssembly the slow path would produce.
            let operation = "RuntimeTypeHandle.GetAssemblyIfExists"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName =
                NativeCall.typeAssemblyName operation ctx.BaseClassTypes state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeAssembly ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetModuleIfExists",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "RuntimeModule",
                                                                      runtimeModuleGenerics)) when
            runtimeTypeGenerics.IsEmpty && runtimeModuleGenerics.IsEmpty
            ->
            // .NET 10 InternalCall fast path: same shape as GetAssemblyIfExists.
            let operation = "RuntimeTypeHandle.GetModuleIfExists"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let typeHandleTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let assemblyName =
                NativeCall.typeAssemblyName operation ctx.BaseClassTypes state typeHandleTarget

            let addr, state =
                getOrAllocateRuntimeModule ctx.LoggerFactory ctx.BaseClassTypes assemblyName state

            let state =
                IlMachineState.pushToEvalStack (CliType.ObjectRef (Some addr)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetElementTypeHandle",
          [ ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            // .NET 10 InternalCall: takes the underlying TypeHandle native handle (IntPtr) and
            // returns the element TypeHandle as an IntPtr (zero for non-array/pointer/byref types).
            // The managed wrapper RuntimeTypeHandle.GetElementType maps a zero result to null.
            let operation = "RuntimeTypeHandle.GetElementTypeHandle"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let handleArg, state = IlMachineState.popEvalStack ctx.Thread state

            let target = NativeCall.runtimeTypeHandleTargetOfEvalStackValue operation handleArg

            let elementTypeSource : NativeIntSource =
                match target with
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition _
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ ->
                    // GetElementType returns null for non-array/pointer/byref types.
                    NativeIntSource.Verbatim 0L
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Concrete _
                    | ConcreteTypeHandle.FunctionPointer _ -> NativeIntSource.Verbatim 0L
                    | ConcreteTypeHandle.Byref inner
                    | ConcreteTypeHandle.Pointer inner
                    | ConcreteTypeHandle.OneDimArrayZero inner ->
                        NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed inner)
                    | ConcreteTypeHandle.Array (inner, _) ->
                        NativeIntSource.TypeHandlePtr (RuntimeTypeHandleTarget.Closed inner)

            let state =
                IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt elementTypeSource) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "CanCastTo",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", sourceGenerics)
            ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", targetGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Boolean) when
            sourceGenerics.IsEmpty && targetGenerics.IsEmpty
            ->
            // RuntimeTypeHandle.CanCastTo is the InternalCall boundary that backs
            // RuntimeType.IsAssignableFrom (and therefore Type.IsAssignableTo) on .NET 9.
            // Delegate to the existing concrete-type cast oracle.
            let operation = "RuntimeTypeHandle.CanCastTo"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let sourceRef, state = IlMachineState.popEvalStack ctx.Thread state
            let state = IlMachineState.loadArgument ctx.Thread 1 state
            let targetRef, state = IlMachineState.popEvalStack ctx.Thread state

            let sourceTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state sourceRef

            let targetTarget =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state targetRef

            let sourceHandle =
                match sourceTarget with
                | RuntimeTypeHandleTarget.Closed handle -> handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic source type definition %O{identity}; need to model variance/identity rules for unbound generics"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    failwith
                        $"TODO: %s{operation} for generic parameter source #%i{position} of %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: %s{operation} for method generic parameter source #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            let targetHandle =
                match targetTarget with
                | RuntimeTypeHandleTarget.Closed handle -> handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic target type definition %O{identity}; need to model variance/identity rules for unbound generics"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    failwith
                        $"TODO: %s{operation} for generic parameter target #%i{position} of %O{declaringType.TypeDefinition.Get}"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"TODO: %s{operation} for method generic parameter target #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            // Reflection-only rule from CanCastToWorker(nullableCast: true): T is assignable
            // to Nullable<T> when queried via reflection, even though the runtime IL cast
            // disagrees. The asymmetric direction (Nullable<T> -> T) does not hold and is
            // left to the standard cast oracle.
            let nullableTargetMatchesSource =
                match targetHandle with
                | ConcreteTypeHandle.Concrete _ ->
                    match AllConcreteTypes.lookup targetHandle state.ConcreteTypes with
                    | Some targetCt when
                        InternalTypeKind.kind ctx.BaseClassTypes targetCt = InternalTypeKind.Nullable
                        && targetCt.Generics.Length = 1
                        ->
                        targetCt.Generics.[0] = sourceHandle
                    | _ -> false
                | ConcreteTypeHandle.Byref _
                | ConcreteTypeHandle.Pointer _
                | ConcreteTypeHandle.FunctionPointer _
                | ConcreteTypeHandle.OneDimArrayZero _
                | ConcreteTypeHandle.Array _ -> false

            let state, isAssignable =
                if nullableTargetMatchesSource then
                    state, true
                else
                    IlMachineState.isConcreteTypeAssignableTo
                        ctx.LoggerFactory
                        ctx.BaseClassTypes
                        state
                        sourceHandle
                        targetHandle

            let state =
                IlMachineState.pushToEvalStack (CliType.ofBool isAssignable) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetAttributes",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System.Reflection",
                                                                      "TypeAttributes",
                                                                      typeAttributesGenerics)) when
            runtimeTypeGenerics.IsEmpty && typeAttributesGenerics.IsEmpty
            ->
            // RuntimeTypeHandle.GetAttributes is the InternalCall boundary backing
            // RuntimeType.GetAttributeFlagsImpl, which is what Type.Attributes calls.
            // CoreCLR's implementation (runtimehandles.cpp ::GetAttributes) returns
            // tdPublic (1) for any TypeDesc — generic variables, byrefs, pointers,
            // function pointers — and otherwise returns the MethodTable's TypeAttributes.
            // Arrays are not TypeDesc in CoreCLR; their synthesized MethodTable carries
            // Public | Sealed | Serializable.
            let operation = "RuntimeTypeHandle.GetAttributes"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let attributes : int32 =
                match target with
                | RuntimeTypeHandleTarget.GenericParameter _
                | RuntimeTypeHandleTarget.MethodGenericParameter _ -> int System.Reflection.TypeAttributes.Public
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    let assembly =
                        state.LoadedAssembly identity.Assembly
                        |> Option.defaultWith (fun () ->
                            failwith
                                $"%s{operation}: assembly for open generic type definition is not loaded: %s{identity.AssemblyFullName}"
                        )

                    let typeInfo = assembly.TypeDefs.[identity.TypeDefinition.Get]
                    int typeInfo.TypeAttributes
                | RuntimeTypeHandleTarget.Closed handle ->
                    match handle with
                    | ConcreteTypeHandle.Byref _
                    | ConcreteTypeHandle.Pointer _
                    | ConcreteTypeHandle.FunctionPointer _ -> int System.Reflection.TypeAttributes.Public
                    | ConcreteTypeHandle.OneDimArrayZero _
                    | ConcreteTypeHandle.Array _ ->
                        // tdPublic | tdSealed | tdSerializable. The Serializable enum
                        // member is deprecated for new managed code, but the bit is the
                        // documented runtime convention for synthesized array MethodTables.
                        int (
                            System.Reflection.TypeAttributes.Public
                            ||| System.Reflection.TypeAttributes.Sealed
                        )
                        ||| 0x2000
                    | ConcreteTypeHandle.Concrete _ ->
                        let concreteType =
                            AllConcreteTypes.lookup handle state.ConcreteTypes
                            |> Option.defaultWith (fun () ->
                                failwith $"%s{operation}: concrete type handle was not registered: %O{handle}"
                            )

                        let assembly =
                            state.LoadedAssembly concreteType.Assembly
                            |> Option.defaultWith (fun () ->
                                failwith
                                    $"%s{operation}: assembly for concrete type is not loaded: %s{concreteType.Assembly.FullName}"
                            )

                        let typeInfo = assembly.TypeDefs.[concreteType.Definition.Get]
                        int typeInfo.TypeAttributes

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 attributes)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetNumVirtuals",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            runtimeTypeGenerics.IsEmpty
            ->
            // RuntimeType.GetMethodCandidates allocates a `bool[numVirtuals]` overrides
            // map, so this number must be the size of the instance vtable for the type:
            // sum of (Virtual + NewSlot, instance) methods declared on the type and on
            // every ancestor up to System.Object. CoreCLR's runtimehandles.cpp returns
            // pMT->GetNumVirtuals() (or 0 when there is no MethodTable, e.g. byrefs and
            // pointers).
            let operation = "RuntimeTypeHandle.GetNumVirtuals"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let state, count =
                numVirtuals ctx.LoggerFactory ctx.BaseClassTypes operation state target

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 count)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetFirstIntroducedMethod",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib", "System", "RuntimeType", runtimeTypeGenerics) ],
          MethodReturnType.Returns (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                                      "System",
                                                                      "RuntimeMethodHandleInternal",
                                                                      returnGenerics)) when
            runtimeTypeGenerics.IsEmpty && returnGenerics.IsEmpty
            ->
            // First half of the IntroducedMethodEnumerator pair: returns the bare
            // RuntimeMethodHandleInternal pointing at the first method declared by `type`'s
            // MethodTable, or zero if there are none. The BCL pairs this with
            // GetNextIntroducedMethod to walk every introduced slot in metadata order
            // (RuntimeHandles.cs:347-390). Inherited methods are NOT surfaced; callers walk the
            // base-type chain themselves (see RuntimeType.GetMethodCandidates).
            let operation = "RuntimeTypeHandle.GetFirstIntroducedMethod"
            let state = IlMachineState.loadArgument ctx.Thread 0 state
            let runtimeTypeRef, state = IlMachineState.popEvalStack ctx.Thread state

            let target =
                NativeCall.runtimeTypeHandleTargetOfRuntimeTypeRef operation state runtimeTypeRef

            let handle =
                match target with
                | RuntimeTypeHandleTarget.Closed handle -> handle
                | RuntimeTypeHandleTarget.OpenGenericTypeDefinition identity ->
                    failwith
                        $"TODO: %s{operation} for open generic type definition %O{identity}; need to walk metadata-level methods on the open type"
                | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
                    // CoreCLR's GetMethodCandidates strips generic variables via GetBaseType
                    // before iterating; reaching here means a managed-side invariant was violated.
                    failwith
                        $"%s{operation}: invoked on type-generic parameter #%i{position} of %O{declaringType.TypeDefinition.Get}; the BCL is expected to strip generic variables via GetBaseType before iterating"
                | RuntimeTypeHandleTarget.MethodGenericParameter (declaringType, declaringMethod, position) ->
                    failwith
                        $"%s{operation}: invoked on method-generic parameter #%i{position} of method %O{declaringMethod.Get} on %O{declaringType.TypeDefinition.Get}"

            let returnValue, state =
                match introducedMethodsOfClosed operation state handle with
                | None
                | Some (_, []) ->
                    let zero =
                        MethodHandleRegistry.zeroInternalHandle ctx.BaseClassTypes state.ConcreteTypes

                    zero, state
                | Some (declaringType, first :: _) ->
                    let value, reg =
                        MethodHandleRegistry.getOrAllocateInternalHandle
                            ctx.BaseClassTypes
                            state.ConcreteTypes
                            declaringType
                            first
                            state.MethodHandles

                    let state =
                        { state with
                            MethodHandles = reg
                        }

                    value, state

            let state =
                IlMachineState.pushToEvalStack (CliType.ValueType returnValue) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | "System.Private.CoreLib",
          "System",
          "RuntimeTypeHandle",
          "GetNextIntroducedMethod",
          [ ConcreteByref (ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                                             "System",
                                                             "RuntimeMethodHandleInternal",
                                                             refGenerics)) ],
          MethodReturnType.Void when refGenerics.IsEmpty ->
            // Second half of the IntroducedMethodEnumerator pair. Reads the byref'd handle,
            // advances to the next introduced method on the same declaring type (in metadata
            // order), and writes the new handle through the byref. A null/zero handle is written
            // when the iteration is exhausted (RuntimeHandles.cs:359-370).
            let operation = "RuntimeTypeHandle.GetNextIntroducedMethod"

            let methodPtr =
                NativeCall.managedPointerOfPointerArgument operation "method" instruction.Arguments.[0]

            let currentValue =
                IlMachineState.readManagedByref ctx.BaseClassTypes state methodPtr

            // RuntimeMethodHandleInternal wraps a single IntPtr-shaped m_handle. The byref came
            // from a managed local of struct type, so primitive-like rewrapping during the
            // write/read round-trip can surface the registry id either as a runtime pointer (the
            // form GetFirst returns) or as a NativeInt with a MethodHandlePtr source (the form
            // produced after passing through an IntPtr field). The shared helper accepts both.
            let currentId =
                match NativeCall.methodHandleIdOfRuntimeMethodHandleInternal operation currentValue with
                | Some id -> id
                | None -> 0L

            if currentId = 0L then
                failwith
                    $"%s{operation}: byref already held a null RuntimeMethodHandleInternal; the BCL's IntroducedMethodEnumerator only calls GetNextIntroducedMethod when the current handle is non-null"

            let methodHandle =
                MethodHandleRegistry.resolveMethodFromId currentId state.MethodHandles
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: registry id %d{currentId} did not resolve to a known MethodHandle"
                )

            // The registry only stores handles whose declaring type was Concrete (GetFirst emits
            // the null sentinel for TypeDesc handles), so `None` here would mean the iterator was
            // resumed against a handle whose declaring type can no longer produce methods.
            let declaringType, methods =
                introducedMethodsOfClosed operation state methodHandle.DeclaringType
                |> Option.defaultWith (fun () ->
                    failwith
                        $"%s{operation}: registry handle %d{currentId} resolves to declaring type %O{methodHandle.DeclaringType}, which does not enumerate introduced methods"
                )

            let currentMetadataHandle = methodHandle.GetMethodDefinitionHandle ()

            let nextValue, state =
                let rec findNext (xs : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn> list) =
                    match xs with
                    | [] ->
                        failwith
                            $"%s{operation}: current method (token %O{currentMetadataHandle}) was not found in declaring type's introduced-methods list"
                    | head :: tail ->
                        if ComparableMethodDefinitionHandle.Make head.Handle = currentMetadataHandle then
                            tail
                        else
                            findNext tail

                match findNext methods with
                | [] ->
                    let zero =
                        MethodHandleRegistry.zeroInternalHandle ctx.BaseClassTypes state.ConcreteTypes

                    zero, state
                | nextMethod :: _ ->
                    let value, reg =
                        MethodHandleRegistry.getOrAllocateInternalHandle
                            ctx.BaseClassTypes
                            state.ConcreteTypes
                            declaringType
                            nextMethod
                            state.MethodHandles

                    let state =
                        { state with
                            MethodHandles = reg
                        }

                    value, state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    methodPtr
                    (CliType.ValueType nextValue)

            NativeHandlerResult.completed state |> Some
        | _ -> None
