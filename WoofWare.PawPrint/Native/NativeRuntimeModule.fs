namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

open NativeRuntimeTypeHelpers

/// <summary>
/// QCalls declared on <c>System.Reflection.RuntimeModule</c>, which ask about the module's
/// own contents rather than resolving a token the caller supplies. The
/// <c>ModuleHandle_*</c> entry points — including the token resolvers, which live in
/// <c>NativeRuntimeTypeQCall</c> next to the type-resolution machinery they share — are
/// elsewhere.
/// </summary>
[<RequireQualifiedAccess>]
module NativeRuntimeModule =
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
        | "RuntimeModule_GetTypes",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeModule",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", qCallModuleGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "ObjectHandleOnStack",
                                             objectHandleGenerics) ],
          MethodReturnType.Void when qCallModuleGenerics.IsEmpty && objectHandleGenerics.IsEmpty ->
            // `RuntimeModule_GetTypes` (coreclr/vm/commodule.cpp:689), the QCall behind
            // `Module.GetTypes()` and `Assembly.DefinedTypes`. CoreCLR enumerates the module's
            // TypeDef table, loads each row with `ClassLoader::LoadTypeDefOrRefThrowing` under
            // `PermitUninstDefOrRef`, and writes back an array of each loaded type's
            // `GetManagedClassObject()`.
            //
            // CoreCLR wraps each row's load in an `EX_TRY`, collects the throwables, and ends with
            // a `ReflectionTypeLoadException` carrying the partially-populated array. That is
            // deliberately *not* reproduced: a row PawPrint cannot concretize is a gap in PawPrint,
            // not a malformed guest assembly, and reporting it as the latter would let the guest
            // swallow it in a `catch` and carry on with a silently short array. So such a row
            // aborts the run instead, with whatever diagnostic concretization gives for it.
            let operation = "RuntimeModule_GetTypes"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let retTypes =
                NativeCall.objectHandleOnStackTarget operation state "retTypes" instruction.Arguments.[1]

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: module's assembly %s{assemblyFullName} is not loaded"
                )

            // Row 1 is the global `<Module>` type, which CoreCLR's enumerator skips: every
            // `EnumTypeDefInit` starts the walk at rid 2 ("Skip over the global model typedef",
            // md/runtime/mdinternalro.cpp:193-217). Skipped by row number rather than by name,
            // exactly as CoreCLR does — the name of that row is not what makes it the global type.
            //
            // Sorted rather than taken in enumeration order because `TypeDefs` is an
            // `ImmutableDictionary`, whose enumeration order is not contractual. Row number is the
            // metadata order: the TypeDef table's rids run 1..N with no gaps, and `TypeDefs` is
            // built by walking `MetadataReader.TypeDefinitions` (Assembly.fs), so this reproduces
            // the sequence CoreCLR's `EnumNext` yields. The order is guest-visible —
            // `Module.GetTypes()` hands the array straight back.
            let rows : TypeDefinitionHandle list =
                assembly.TypeDefs.Keys
                |> Seq.map (fun handle -> MetadataTokens.GetRowNumber (TypeDefinitionHandle.op_Implicit handle), handle)
                |> Seq.filter (fun (rowNumber, _) -> rowNumber > 1)
                |> Seq.sortBy fst
                |> Seq.map snd
                |> List.ofSeq

            // `PermitUninstDefOrRef` is what `allowOpenGenericDefinition = true` says here: a
            // generic row comes back as the open generic definition rather than as any
            // instantiation of it, which is also true of a row nested inside a generic, since it
            // inherits its enclosing type's formals. The empty type and method instantiations are
            // the same thing said again — a TypeDef row carries no caller-supplied generic
            // context. This is the call `ModuleHandle_ResolveType` makes for a TypeDef token, so
            // the two agree by construction.
            let state, typeAddresses =
                ((state, []), rows)
                ||> List.fold (fun (state, acc) handle ->
                    let state, typeDefn =
                        IlMachineState.lookupTypeDefn ctx.BaseClassTypes state assembly handle

                    let state, target =
                        IlMachineState.runtimeTypeHandleTargetForTypeToken
                            ctx.LoggerFactory
                            ctx.BaseClassTypes
                            assembly
                            true
                            ImmutableArray.Empty
                            ImmutableArray.Empty
                            typeDefn
                            state

                    let addr, state =
                        IlMachineState.getOrAllocateType ctx.LoggerFactory ctx.BaseClassTypes target state

                    state, addr :: acc
                )

            let typeAddresses = List.rev typeAddresses

            // `AllocateObjectArray(dwNumTypeDefs, CoreLibBinder::GetClass(CLASS__CLASS))`, and
            // `CLASS__CLASS` is `System.RuntimeType` (vm/corelib.h:151) — matching the
            // `RuntimeType[]` the managed caller declares.
            let state, _, runtimeTypeElementHandle =
                concretizeNonGenericCorelibType ctx.LoggerFactory ctx.BaseClassTypes state "System" "RuntimeType"

            let arrayAddr, state =
                IlMachineState.allocateArray
                    (ConcreteTypeHandle.OneDimArrayZero runtimeTypeElementHandle)
                    (fun () -> CliType.ObjectRef None)
                    (List.length typeAddresses)
                    state

            let state =
                ((state, 0), typeAddresses)
                ||> List.fold (fun (state, index) typeAddr ->
                    let state =
                        IlMachineState.setArrayValue arrayAddr (CliType.ObjectRef (Some typeAddr)) index state

                    state, index + 1
                )
                |> fst

            // Written unconditionally, including for an empty array. Unlike
            // `RuntimeTypeHandle_GetInterfaces`, whose managed wrapper pre-initialises its local
            // to `[]`, `RuntimeModule.GetDefinedTypes` starts from `null` and returns `types!`
            // (RuntimeModule.cs:421-427), so a skipped write is a NullReferenceException in the
            // guest.
            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retTypes
                    (CliType.ObjectRef (Some arrayAddr))

            NativeHandlerResult.completed state |> Some
        | "RuntimeModule_GetScopeName",
          "System.Private.CoreLib",
          "System.Reflection",
          "RuntimeModule",
          [ CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices", "QCallModule", qCallModuleGenerics)
            CorelibType state.ConcreteTypes ("System.Runtime.CompilerServices",
                                             "StringHandleOnStack",
                                             stringHandleGenerics) ],
          MethodReturnType.Void when qCallModuleGenerics.IsEmpty && stringHandleGenerics.IsEmpty ->
            // `RuntimeModule_GetScopeName` (coreclr/vm/commodule.cpp:604), behind
            // `Module.ScopeName` and so `Module.ToString()`. CoreCLR answers
            // `GetMDImport()->GetScopeProps(&szName, 0)`, which is the `Name` column of the
            // module's own `Module` metadata row.
            let operation = "RuntimeModule_GetScopeName"

            if instruction.Arguments.Length <> 2 then
                failwith $"%s{operation}: expected two native arguments, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let retString =
                NativeCall.stringHandleOnStackTarget operation state "retString" instruction.Arguments.[1]

            let assembly =
                state.LoadedAssembly assemblyFullName
                |> Option.defaultWith (fun () ->
                    failwith $"%s{operation}: module's assembly %s{assemblyFullName} is not loaded"
                )

            // CoreCLR's leading `IsValidToken(GetModuleFromScope())` guard, which reports
            // `COR_E_BADIMAGEFORMAT` for an image whose `Module` table is empty, is absent here:
            // that table is mandatory and has exactly one row, and an image lacking it would have
            // failed when its metadata was first read rather than reaching a QCall.
            // Reported verbatim, including an empty string. CoreCLR performs no emptiness check
            // here -- it hands `GetScopeProps`'s name straight to `retString.Set` -- so an image
            // whose Module row names the empty string is one whose `ScopeName` really is "".
            // Unlike `AssemblyNative_GetSimpleName`, which does refuse: that reads the *Assembly*
            // row, where CoreCLR's own `_ASSERTE` treats an empty name as a corrupted image.
            let scopeName = assembly.ScopeName

            let nameAddr, state =
                if System.String.IsNullOrEmpty scopeName then
                    // `StringObject::NewString` returns the shared empty-string instance for a
                    // zero-length string (object.cpp:651), so an empty scope name is
                    // reference-equal to `string.Empty` and to itself across reads.
                    IlMachineState.internCanonicalEmptyString ctx.LoggerFactory ctx.BaseClassTypes state
                else
                    // Allocated afresh per call otherwise: CoreCLR does not intern a QCall's
                    // string result, so two reads of a non-empty `ScopeName` are
                    // reference-distinct.
                    IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes scopeName state

            let state =
                IlMachineState.writeManagedByrefWithBase
                    ctx.BaseClassTypes
                    state
                    retString
                    (CliType.ObjectRef (Some nameAddr))

            NativeHandlerResult.completed state |> Some
        | _ -> None
