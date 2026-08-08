namespace WoofWare.PawPrint

/// <summary>
/// QCalls on <c>System.ModuleHandle</c> that answer questions about the module's image
/// rather than resolving anything out of it. The <c>ModuleHandle_Resolve*</c> entry points
/// live in <c>NativeRuntimeTypeQCall</c> instead, next to the type-resolution machinery
/// they share.
/// </summary>
[<RequireQualifiedAccess>]
module NativeModuleHandle =
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
        | "ModuleHandle_GetMDStreamVersion",
          "System.Private.CoreLib",
          "System",
          "ModuleHandle",
          [ ConcreteType state.ConcreteTypes ("System.Private.CoreLib",
                                              "System.Runtime.CompilerServices",
                                              "QCallModule",
                                              qCallModuleGenerics) ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) when
            qCallModuleGenerics.IsEmpty
            ->
            let operation = "ModuleHandle_GetMDStreamVersion"

            if instruction.Arguments.Length <> 1 then
                failwith $"%s{operation}: expected one native argument, got %d{instruction.Arguments.Length}"

            let assemblyFullName =
                NativeCall.qCallModuleToAssemblyFullName
                    operation
                    state
                    (instruction.Arguments.[0] |> EvalStackValue.ofCliType)

            let assembly =
                state.LoadedAssembly' assemblyFullName
                |> Option.defaultWith (fun () -> failwith $"%s{operation}: assembly %s{assemblyFullName} is not loaded")

            // CoreCLR returns `pModule->GetMDImport()->GetMetadataStreamVersion()`, which both
            // metadata-import implementations define as `m_Schema.m_minor | (m_Schema.m_major
            // << 16)` — the major and minor bytes of the table stream header, packed into one
            // Int32 with the major in the *senior* half. So a 2.0 image reports 0x20000, which
            // is `MD_STREAM_VER_2` in metadata.h, and a 1.x one 0x1000x (`MD_STREAM_VER_1X`).
            //
            // The packing lives here rather than on `DumpedAssembly.MetadataTableStreamVersion`
            // for the same reason `GetFlags`'s `afPublicKey` synthesis does: that member says
            // what the file format says, and this is one particular CoreCLR API's encoding of
            // it, reproduced at the seam that reproduces CoreCLR APIs.
            //
            // Note this is keyed by a module while PawPrint models one module per assembly, so
            // it resolves through the assembly. That is what every other `QCallModule` handler
            // here does, and for `Assembly.GetName()` — which asks the *manifest* module — it
            // is exactly right regardless.
            let version = assembly.MetadataTableStreamVersion
            let packed = version.Minor ||| (version.Major <<< 16)

            let state =
                IlMachineState.pushToEvalStack (CliType.Numeric (CliNumericType.Int32 packed)) ctx.Thread state

            NativeHandlerResult.completed state |> Some
        | _ -> None
