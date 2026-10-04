namespace WoofWare.PawPrint

open System.Reflection.Metadata

/// A function of the C runtime's maths library that `System.Math` and `System.MathF` declare as
/// an FCall, named as the two classes name it.
[<RequireQualifiedAccess>]
type MathFunction =
    | Acos
    | Acosh
    | Asin
    | Asinh
    | Atan
    | Atanh
    | Atan2
    | Cbrt
    | Ceiling
    | Cos
    | Cosh
    | Exp
    | Floor
    | FusedMultiplyAdd
    | Log
    | Log2
    | Log10
    | Pow
    | Sin
    | Sinh
    | Sqrt
    | Tan
    | Tanh

/// A library of native code that the framework ships with its CoreLib, built from the same runtime
/// source, which CoreLib's P/Invokes call.
[<RequireQualifiedAccess>]
type FrameworkShim =
    /// `libSystem.Native`: the operating system's calls, each answering a result or an error code.
    | SystemNative
    /// `libSystem.Globalization.Native`: ICU's, which CoreCLR links into itself.
    | GlobalizationNative

/// A method CoreCLR implements in native code (an `InternalCall` or a P/Invoke) whose behaviour
/// is known.
///
/// The cases are operations, not methods. Which CoreLib method is which operation is read from the
/// image (`NativeMethod.recognise`), so a CoreLib of another runtime version is classified from
/// its own methods.
[<RequireQualifiedAccess>]
type NativeMethod =
    /// A function of the C runtime's maths library, over doubles (`System.Math`) or singles
    /// (`System.MathF`). The JIT may instead expand a call to one into an instruction.
    | MathFunction of MathFunction * FloatWidth
    /// A function of one of the framework's shim libraries, called by a P/Invoke that copies every
    /// value it passes and returns as its bytes: CoreLib disables runtime marshalling, and each value
    /// is a number, a pointer, a function pointer, an enum, or a value type holding only such
    /// values. The call does nothing besides: it returns the function's own result rather than an
    /// HRESULT to throw from, sets no last error, adds no locale argument, and passes a fixed
    /// argument list by a convention that takes no `this`.
    | ShimFunction of FrameworkShim * entryPoint : string
    /// A method of CoreLib's that the contract table (`NativeContractTable`) describes: an FCall,
    /// or a QCall called by a P/Invoke whose stub copies every value as its bytes and does nothing
    /// besides, as for `ShimFunction`.
    | Tabulated of NativeContractRow

[<RequireQualifiedAccess>]
module NativeMethod =

    let private corelib : string = "System.Private.CoreLib"

    /// The library name by which CoreLib's P/Invokes call CoreCLR's own functions.
    let private qcall : string = "QCall"

    /// Each function, with the number of arguments it takes.
    let private mathFunctions : Map<string * int, MathFunction> =
        [
            "Acos", 1, MathFunction.Acos
            "Acosh", 1, MathFunction.Acosh
            "Asin", 1, MathFunction.Asin
            "Asinh", 1, MathFunction.Asinh
            "Atan", 1, MathFunction.Atan
            "Atanh", 1, MathFunction.Atanh
            "Atan2", 2, MathFunction.Atan2
            "Cbrt", 1, MathFunction.Cbrt
            "Ceiling", 1, MathFunction.Ceiling
            "Cos", 1, MathFunction.Cos
            "Cosh", 1, MathFunction.Cosh
            "Exp", 1, MathFunction.Exp
            "Floor", 1, MathFunction.Floor
            "FusedMultiplyAdd", 3, MathFunction.FusedMultiplyAdd
            "Log", 1, MathFunction.Log
            "Log2", 1, MathFunction.Log2
            "Log10", 1, MathFunction.Log10
            "Pow", 2, MathFunction.Pow
            "Sin", 1, MathFunction.Sin
            "Sinh", 1, MathFunction.Sinh
            "Sqrt", 1, MathFunction.Sqrt
            "Tan", 1, MathFunction.Tan
            "Tanh", 1, MathFunction.Tanh
        ]
        |> List.map (fun (name, arity, fn) -> (name, arity), fn)
        |> Map.ofList

    /// The shims, by the library names CoreLib's P/Invokes give them on Unix.
    let private shims : Map<string, FrameworkShim> =
        Map.ofList
            [
                "libSystem.Native", FrameworkShim.SystemNative
                "libSystem.Globalization.Native", FrameworkShim.GlobalizationNative
            ]

    /// The attribute whose presence on an assembly makes its P/Invokes pass every value as its bytes.
    let private disableRuntimeMarshalling : string =
        "System.Runtime.CompilerServices.DisableRuntimeMarshallingAttribute"

    /// The attribute that sets a P/Invoke's calling convention when its import says `WinApi`.
    let private unmanagedCallConv : string =
        "System.Runtime.InteropServices.UnmanagedCallConvAttribute"

    /// The attribute that has a P/Invoke pass the current culture's locale ID as an extra argument.
    let private lcidConversion : string =
        "System.Runtime.InteropServices.LCIDConversionAttribute"

    /// A number, a pointer or a function pointer, which a P/Invoke passes as its bytes.
    let rec private isScalar (spelling : TypeDefn) : bool =
        match spelling with
        | TypeDefn.PrimitiveType primitive ->
            match primitive with
            | PrimitiveType.String
            | PrimitiveType.Object
            | PrimitiveType.TypedReference -> false
            | _ -> true
        | TypeDefn.Pointer _
        | TypeDefn.FunctionPointer _ -> true
        | TypeDefn.Modified modified -> isScalar modified.Unmodified
        | _ -> false

    /// The definition in `assembly` of a value type it spells, if it is one of its own.
    let private ownValueType (assembly : DumpedAssembly) (spelling : TypeDefn) : TypeInfo<_, _> option =
        match spelling with
        | TypeDefn.FromDefinition (identity, SignatureTypeKind.ValueType) when
            identity.AssemblyFullName = assembly.DefinitionFullName
            ->
            Some assembly.TypeDefs.[identity.TypeDefinition.Get]
        | _ -> None

    /// A value type that holds nothing but numbers, pointers, function pointers and other such
    /// value types, laid out sequentially or explicitly: what CoreCLR's
    /// `GetDisabledMarshallerType` (`vm/mlinfo.cpp`) copies as its bytes, since it holds no
    /// reference, has no auto-layout part, meets the generics restriction and holds no `Int128`.
    /// Some value types CoreCLR copies are refused (one with an enum field, say), and a P/Invoke
    /// naming one is left unrecognised.
    let rec private holdsOnlyBytes (assembly : DumpedAssembly) (definition : TypeInfo<_, _>) : bool =
        TypeLayoutKind.ofTypeAttributes definition.TypeAttributes <> TypeLayoutKind.Auto
        && not (
            definition.Namespace = "System"
            && (definition.Name = "Int128" || definition.Name = "UInt128")
        )
        && definition.Fields
           |> List.forall (fun field ->
               field.IsStatic
               || isScalar field.Signature
               || (ownValueType assembly field.Signature |> Option.exists (holdsOnlyBytes assembly))
           )

    /// Whether a type definition is an enum, which a signature passes as its underlying number.
    let private isEnum (assembly : DumpedAssembly) (definition : TypeInfo<_, _>) : bool =
        let isSystemEnum (ns : string, name : string) = ns = "System" && name = "Enum"

        match definition.BaseType with
        | Some (BaseTypeInfo.TypeDef handle) ->
            let baseType = assembly.TypeDefs.[handle]
            isSystemEnum (baseType.Namespace, baseType.Name)
        | Some (BaseTypeInfo.TypeRef handle) ->
            let baseType = assembly.TypeRefs.[handle]
            isSystemEnum (baseType.Namespace, baseType.Name)
        | Some (BaseTypeInfo.TypeSpec _)
        | None -> false

    /// Whether a P/Invoke of `assembly`, which disables runtime marshalling, passes or returns a
    /// value of this type as its bytes.
    let private copiedAsIs (assembly : DumpedAssembly) (spelling : TypeDefn) : bool =
        isScalar spelling
        || ownValueType assembly spelling
           |> Option.exists (fun definition -> isEnum assembly definition || holdsOnlyBytes assembly definition)

    /// Whether CoreCLR builds the stub for `method`, a P/Invoke of `assembly`, which disables runtime
    /// marshalling, without throwing for what it declares besides the types it passes: the method
    /// returns its own result (`PreserveSig`), takes a fixed argument list, sets no last error,
    /// converts no locale argument (`LCIDConversion`), and calls by a convention that takes no
    /// `this` and that its import alone gives (no `UnmanagedCallConv`, and no modifier on its
    /// return type, from which CoreCLR would read a convention).
    let private hasPlainStub (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : bool =
        let definition = assembly.Methods.[method]

        match definition, definition.TryNativeImport with
        | MethodInfo.Metadata (_, facts), Some import ->
            let convention =
                import.Attributes
                &&& System.Reflection.MethodImportAttributes.CallingConventionMask

            facts.ImplAttributes.HasFlag System.Reflection.MethodImplAttributes.PreserveSig
            && definition.Signature.Header.Get.CallingConvention = SignatureCallingConvention.Default
            && not (import.Attributes.HasFlag System.Reflection.MethodImportAttributes.SetLastError)
            && (convention = System.Reflection.MethodImportAttributes.CallingConventionWinApi
                || convention = System.Reflection.MethodImportAttributes.CallingConventionCDecl
                || convention = System.Reflection.MethodImportAttributes.CallingConventionStdCall)
            && (NamedAttribute.ofMethod unmanagedCallConv assembly method).IsEmpty
            && (NamedAttribute.ofMethod lcidConversion assembly method).IsEmpty
            && (
                match definition.Signature.ReturnType with
                | MethodReturnType.Returns (TypeDefn.Modified _) -> false
                | MethodReturnType.Returns _
                | MethodReturnType.Void -> true
            )
        | MethodInfo.Synthesised _, _
        | _, None -> false

    /// The operation `method` performs, when it is a native method of `assembly`, a CoreLib, that
    /// this module describes. Recognition is by class, name and signature, or by the library and
    /// entry point a P/Invoke names, so it holds for any CoreLib that keeps those; a method this
    /// does not recognise is `None`.
    let recognise (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : NativeMethod option =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        match definition.Body with
        | MethodBody.PInvoke when
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && definition.Signature.GenericParameterCount = 0
            && declaringType.Generics.IsEmpty
            && not (NamedAttribute.ofAssembly disableRuntimeMarshalling assembly).IsEmpty
            && hasPlainStub assembly method
            ->
            let returnsAsIs =
                match definition.Signature.ReturnType with
                | MethodReturnType.Void -> true
                | MethodReturnType.Returns returned -> copiedAsIs assembly returned

            match definition.TryNativeImport with
            | Some import when
                returnsAsIs
                && definition.Signature.ParameterTypes |> List.forall (copiedAsIs assembly)
                ->
                match Map.tryFind import.ModuleName shims with
                | Some shim -> Some (NativeMethod.ShimFunction (shim, import.EntryPointName))
                // CoreCLR binds CoreLib's P/Invokes into `QCall` to its own functions by entry point
                // (`vm/qcallentrypoints.cpp`), without loading a library.
                | None when import.ModuleName = qcall ->
                    Map.tryFind import.EntryPointName (NativeContractTable.qcalls.Force ())
                    |> Option.map NativeMethod.Tabulated
                | None -> None
            | _ -> None
        | MethodBody.InternalCall when
            assembly.ThisAssemblyDefinition.Name.Name = corelib
            && not declaringType.IsNested
            && definition.IsStatic
            && definition.Signature.GenericParameterCount = 0
            ->
            let width =
                match declaringType.Namespace, declaringType.Name with
                | "System", "Math" -> Some (FloatWidth.Double, PrimitiveType.Double)
                | "System", "MathF" -> Some (FloatWidth.Single, PrimitiveType.Single)
                | _ -> None

            // A row names an FCall by its name alone, as `vm/ecalllist.h` binds most, so it names one
            // only when its type declares no other FCall of that name.
            let tabulated () =
                let fcallsNamed =
                    declaringType.Methods
                    |> List.filter (fun candidate ->
                        match candidate.Body with
                        | MethodBody.InternalCall -> candidate.Name = definition.Name
                        | _ -> false
                    )

                match fcallsNamed with
                | [ _ ] when declaringType.Generics.IsEmpty ->
                    Map.tryFind
                        (declaringType.Namespace + "." + declaringType.Name, definition.Name)
                        (NativeContractTable.fcalls.Force ())
                    |> Option.map NativeMethod.Tabulated
                | _ -> None

            let mathFunction =
                match width with
                | None -> None
                | Some (width, primitive) ->
                    let float = TypeDefn.PrimitiveType primitive

                    if
                        definition.Signature.ParameterTypes |> List.forall ((=) float)
                        && definition.Signature.ReturnType = MethodReturnType.Returns float
                    then
                        Map.tryFind (definition.Name, definition.Signature.ParameterTypes.Length) mathFunctions
                        |> Option.map (fun fn -> NativeMethod.MathFunction (fn, width))
                    else
                        None

            mathFunction |> Option.orElseWith tabulated
        | _ -> None

    /// What `native` can do to its caller.
    let contract (native : NativeMethod) : NativeContract =
        match native with
        // CoreCLR's FCall returns the C runtime's result (`COMDouble` and `COMSingle`,
        // floatdouble.cpp and floatsingle.cpp), and the instruction the JIT may emit instead
        // computes the same: a domain error or an unrepresentable result is a value (NaN, an
        // infinity), not a fault.
        | NativeMethod.MathFunction _ ->
            {
                Raises = []
                CanReturn = true
                Result = ResultNullness.NotAReference
            }
        // Native code raises no managed exception: a fault in it ends the process, and so does an
        // exception thrown by a managed callback it calls, which does not cross the native frames
        // back to the caller. Binding the call raises nothing either, on the assumption that the
        // framework's shims are present and export what its CoreLib imports; were one missing, the
        // program's `ResolvingUnmanagedDll` handlers would run, and could throw anything.
        | NativeMethod.ShimFunction _ ->
            {
                Raises = []
                CanReturn = true
                Result = ResultNullness.NotAReference
            }
        | NativeMethod.Tabulated row -> row.Contract
