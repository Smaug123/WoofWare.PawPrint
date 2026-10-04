namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `NativeMethod` recognises methods CoreCLR implements in native code whose behaviour is known,
/// and states what each can do to its caller. These tests hold the recognition to the CoreLibs'
/// own metadata and the contracts to real .NET.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeMethod =

    /// The width of `method`'s floats, if it is an FCall on CoreLib's `System.Math` (doubles) or
    /// `System.MathF` (singles) whose parameters and result are all of that width: every one of
    /// those is a C runtime function, as `Math.CoreCLR.cs` and `MathF.CoreCLR.cs` declare them.
    let private floatOnlyMathFCall (corelib : DumpedAssembly) (method : MethodInfo<_, _, _>) : FloatWidth option =
        let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

        let width =
            match declaringType.Namespace, declaringType.Name, declaringType.IsNested with
            | "System", "Math", false -> Some (FloatWidth.Double, PrimitiveType.Double)
            | "System", "MathF", false -> Some (FloatWidth.Single, PrimitiveType.Single)
            | _ -> None

        match width, method.Body with
        | Some (width, primitive), MethodBody.InternalCall ->
            let isFloat (ty : TypeDefn) = ty = TypeDefn.PrimitiveType primitive

            if
                method.Signature.ParameterTypes |> List.forall isFloat
                && method.Signature.ReturnType = MethodReturnType.Returns (TypeDefn.PrimitiveType primitive)
            then
                Some width
            else
                None
        | _ -> None

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    let private caseName (value : 'a) : string =
        let case, _ = FSharpValue.GetUnionFields (value, typeof<'a>)
        case.Name

    [<TestCaseSource(nameof coreLibs)>]
    let ``recognises exactly the Math and MathF FCalls that take and return only floats`` (which : string) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        let recognised =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    match NativeMethod.recognise corelib handle, floatOnlyMathFCall corelib method with
                    | None, None -> ()
                    | Some (NativeMethod.MathFunction (fn, recognisedWidth) as native), Some width ->
                        // The case is the method, spelled as its C# name.
                        caseName fn |> shouldEqual method.Name
                        recognisedWidth |> shouldEqual width
                        yield native
                    | Some (NativeMethod.ShimFunction _), None -> ()
                    | Some native, _ ->
                        failwith $"%s{method.Name} is recognised as %A{native}, but is not a float-only Math FCall"
                    | None, Some _ ->
                        failwith
                            $"%s{corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get].Name}.%s{method.Name} is a float-only Math FCall that is not recognised"
            ]

        // Each function at each width is exactly one method.
        let every =
            [
                for case in FSharpType.GetUnionCases typeof<MathFunction> do
                    let fn = FSharpValue.MakeUnion (case, [||]) :?> MathFunction

                    for width in [ FloatWidth.Single ; FloatWidth.Double ] do
                        yield NativeMethod.MathFunction (fn, width)
            ]

        List.sort recognised |> shouldEqual (List.sort every)

    /// An image that calls itself System.Private.CoreLib, whose `System.Math` declares methods of
    /// the maths functions' names: one with a maths function's signature, and others that differ
    /// from one in a single respect each.
    let private fabricateMath () : byte[] =
        let name = "System.Private.CoreLib"
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)
        let modul = builder.DefineDynamicModule name

        let ty =
            modul.DefineType (
                "System.Math",
                TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed
            )

        let staticMethod = MethodAttributes.Public ||| MethodAttributes.Static

        let fcall (name : string) (returns : Type) (parameters : Type[]) : MethodBuilder =
            let m = ty.DefineMethod (name, staticMethod, returns, parameters)
            m.SetImplementationFlags MethodImplAttributes.InternalCall
            m

        fcall "Sin" typeof<double> [| typeof<double> |] |> ignore<MethodBuilder>

        fcall "Pow" typeof<double> [| typeof<double> ; typeof<int> |]
        |> ignore<MethodBuilder>

        fcall "Cos" typeof<single> [| typeof<double> |] |> ignore<MethodBuilder>

        fcall "Tan" typeof<double> [| typeof<double>.MakePointerType () |]
        |> ignore<MethodBuilder>

        let generic = fcall "Exp" typeof<double> [| typeof<double> |]

        generic.DefineGenericParameters [| "T" |]
        |> ignore<GenericTypeParameterBuilder[]>

        let withIl =
            ty.DefineMethod ("Floor", staticMethod, typeof<double>, [| typeof<double> |])

        let il = withIl.GetILGenerator ()
        il.Emit OpCodes.Ldarg_0
        il.Emit OpCodes.Ret

        ty.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    /// The shim a P/Invoke's library names, as CoreLib's `Interop.Libraries` names them on Unix.
    let private shimNamed (moduleName : string) : FrameworkShim option =
        match moduleName with
        | "libSystem.Native" -> Some FrameworkShim.SystemNative
        | "libSystem.Globalization.Native" -> Some FrameworkShim.GlobalizationNative
        | _ -> None

    [<TestCaseSource(nameof coreLibs)>]
    let ``recognises every P/Invoke into the framework's shims, and no other`` (which : string) : unit =
        let corelib = TestIntrinsicBody.coreLib which
        let mutable shimCalls = 0

        for KeyValue (handle, method) in corelib.Methods do
            let expected =
                match method.Body, method.TryNativeImport with
                | MethodBody.PInvoke, Some import ->
                    shimNamed import.ModuleName
                    |> Option.map (fun shim -> NativeMethod.ShimFunction (shim, import.EntryPointName))
                | _ -> None

            match NativeMethod.recognise corelib handle, expected with
            | Some (NativeMethod.MathFunction _), None -> ()
            | recognised, expected ->
                if recognised <> expected then
                    let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                    failwith
                        $"%s{declaringType.Name}.%s{method.Name} is recognised as %A{recognised}, but should be %A{expected}"

                if expected.IsSome then
                    shimCalls <- shimCalls + 1

        // Each CoreLib calls its shims from hundreds of P/Invokes; this rules out a CoreLib that
        // names them otherwise, which would leave the loop above nothing to check.
        shimCalls |> shouldBeGreaterThan 100

    /// An image named `name`, which disables runtime marshalling if `disablesMarshalling`, whose
    /// `Interop` class declares P/Invokes into the shims: some whose stub copies every value as its
    /// bytes and nothing more, and others that differ from one of those in a single respect each.
    let private fabricateShimCalls (name : string) (disablesMarshalling : bool) : byte[] =
        let builder = PersistedAssemblyBuilder (AssemblyName name, typeof<obj>.Assembly)

        if disablesMarshalling then
            builder.SetCustomAttribute (
                CustomAttributeBuilder (
                    typeof<Runtime.CompilerServices.DisableRuntimeMarshallingAttribute>.GetConstructor [||],
                    [||]
                )
            )

        let modul = builder.DefineDynamicModule name

        let mode = modul.DefineEnum ("Mode", TypeAttributes.Public, typeof<int>)
        mode.DefineLiteral ("Read", 0) |> ignore<FieldBuilder>
        let mode = mode.CreateType ()

        /// A value type with these instance fields, each at offset 0 if it is laid out explicitly.
        let structure (name : string) (layout : TypeAttributes) (fields : Type list) : Type =
            let ty =
                modul.DefineType (name, TypeAttributes.Public ||| TypeAttributes.Sealed ||| layout, typeof<ValueType>)

            fields
            |> List.iteri (fun i field ->
                let built = ty.DefineField ($"Field%d{i}", field, FieldAttributes.Public)

                if layout = TypeAttributes.ExplicitLayout then
                    built.SetOffset 0
            )

            ty.CreateType ()

        let pair = structure "Pair" TypeAttributes.SequentialLayout [ typeof<int> ]

        let outer =
            structure "Outer" TypeAttributes.SequentialLayout [ pair ; typeof<byte>.MakePointerType () ]

        let overlaid =
            structure "Overlaid" TypeAttributes.ExplicitLayout [ typeof<int> ; typeof<single> ]

        let autoLaid = structure "AutoLaid" TypeAttributes.AutoLayout [ typeof<int> ]

        let holdsObject =
            structure "HoldsObject" TypeAttributes.SequentialLayout [ typeof<int> ; typeof<obj> ]

        let holdsEnum = structure "HoldsEnum" TypeAttributes.SequentialLayout [ mode ]

        let wide =
            structure "System.Int128" TypeAttributes.SequentialLayout [ typeof<uint64> ; typeof<uint64> ]

        let withStatic =
            let ty =
                modul.DefineType (
                    "WithStatic",
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            ty.DefineField ("Value", typeof<int>, FieldAttributes.Public)
            |> ignore<FieldBuilder>

            ty.DefineField ("Shared", typeof<obj>, FieldAttributes.Public ||| FieldAttributes.Static)
            |> ignore<FieldBuilder>

            ty.CreateType ()

        let holder = (modul.DefineType ("Holder", TypeAttributes.Public)).CreateType ()

        let ty =
            modul.DefineType ("Interop", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let declare
            (name : string)
            (library : string)
            (preserveSig : bool)
            (header : CallingConventions)
            (convention : Runtime.InteropServices.CallingConvention)
            (returnModifiers : Type[])
            (returns : Type)
            (parameters : Type[])
            : MethodBuilder
            =
            let m =
                ty.DefinePInvokeMethod (
                    name,
                    library,
                    $"Shim_%s{name}",
                    MethodAttributes.Public
                    ||| MethodAttributes.Static
                    ||| MethodAttributes.PinvokeImpl,
                    header,
                    returns,
                    null,
                    returnModifiers,
                    parameters,
                    null,
                    null,
                    convention,
                    Runtime.InteropServices.CharSet.Ansi
                )

            m.SetImplementationFlags (
                if preserveSig then
                    MethodImplAttributes.PreserveSig
                else
                    MethodImplAttributes.IL
            )

            m

        let pinvoke (name : string) (library : string) (preserveSig : bool) (returns : Type) (parameters : Type[]) =
            declare
                name
                library
                preserveSig
                CallingConventions.Standard
                Runtime.InteropServices.CallingConvention.Winapi
                [||]
                returns
                parameters
            |> ignore<MethodBuilder>

        /// A P/Invoke into `libSystem.Native` from an `int` to an `int`, declared in one other respect
        /// as `differ` declares it.
        let differing (name : string) (differ : MethodBuilder -> unit) =
            declare
                name
                "libSystem.Native"
                true
                CallingConventions.Standard
                Runtime.InteropServices.CallingConvention.Winapi
                [||]
                typeof<int>
                [| typeof<int> |]
            |> differ

        let native = "libSystem.Native"
        pinvoke "Numbers" native true typeof<int> [| typeof<int64> ; typeof<bool> |]
        pinvoke "Pointers" native true typeof<nativeint> [| typeof<byte>.MakePointerType () |]
        pinvoke "Nothing" native true typeof<Void> [||]
        pinvoke "Structure" native true pair [| pair |]
        pinvoke "Nested" native true typeof<int> [| outer |]
        pinvoke "Overlaid" native true typeof<int> [| overlaid |]
        pinvoke "StaticField" native true typeof<int> [| withStatic |]
        pinvoke "TakesEnum" native true mode [| mode |]
        pinvoke "AutoLaid" native true typeof<int> [| autoLaid |]
        pinvoke "HoldsObject" native true typeof<int> [| holdsObject |]
        pinvoke "HoldsEnum" native true typeof<int> [| holdsEnum |]
        pinvoke "Wide" native true typeof<int> [| wide |]
        pinvoke "Foreign" native true typeof<int> [| typeof<Guid> |]

        pinvoke "Globalization" "libSystem.Globalization.Native" true typeof<int> [| typeof<char>.MakePointerType () |]

        pinvoke "Translated" native false typeof<int> [| typeof<int> |]
        pinvoke "TakesObject" native true typeof<int> [| typeof<obj> |]
        pinvoke "TakesClass" native true typeof<int> [| holder |]
        pinvoke "TakesString" native true typeof<int> [| typeof<string> |]
        pinvoke "ReturnsObject" native true typeof<obj> [||]
        pinvoke "TakesByref" native true typeof<int> [| typeof<int>.MakeByRefType () |]
        pinvoke "Runtime" "QCall" true typeof<int> [| typeof<int> |]
        pinvoke "System" "libc" true typeof<int> [| typeof<int> |]

        let attribute (attributeType : Type) (arguments : obj[]) (fields : (string * obj) list) =
            CustomAttributeBuilder (
                attributeType.GetConstructor (arguments |> Array.map (fun argument -> argument.GetType ())),
                arguments,
                fields
                |> List.map (fun (field, _) -> attributeType.GetField field)
                |> Array.ofList,
                fields |> List.map snd |> Array.ofList
            )

        differing
            "SetsLastError"
            (fun m ->
                attribute
                    typeof<Runtime.InteropServices.DllImportAttribute>
                    [| box native |]
                    [
                        "EntryPoint", box "Shim_SetsLastError"
                        "CallingConvention", box Runtime.InteropServices.CallingConvention.Winapi
                        "SetLastError", box true
                    ]
                |> m.SetCustomAttribute
            )

        differing
            "ConvertsLocale"
            (fun m ->
                attribute typeof<Runtime.InteropServices.LCIDConversionAttribute> [| box 0 |] []
                |> m.SetCustomAttribute
            )

        differing
            "ConventionByAttribute"
            (fun m ->
                attribute typeof<Runtime.InteropServices.UnmanagedCallConvAttribute> [||] []
                |> m.SetCustomAttribute
            )

        let variant (name : string) (header : CallingConventions) convention (returnModifiers : Type[]) =
            declare name native true header convention returnModifiers typeof<int> [| typeof<int> |]
            |> ignore<MethodBuilder>

        variant "VarArgs" CallingConventions.VarArgs Runtime.InteropServices.CallingConvention.Winapi [||]

        variant "ThisCall" CallingConventions.Standard Runtime.InteropServices.CallingConvention.ThisCall [||]

        variant "FastCall" CallingConventions.Standard Runtime.InteropServices.CallingConvention.FastCall [||]

        variant
            "ConventionByModifier"
            CallingConventions.Standard
            Runtime.InteropServices.CallingConvention.Winapi
            [| typeof<Runtime.CompilerServices.CallConvCdecl> |]

        variant "Cdecl" CallingConventions.Standard Runtime.InteropServices.CallingConvention.Cdecl [||]
        variant "StdCall" CallingConventions.Standard Runtime.InteropServices.CallingConvention.StdCall [||]

        let generic =
            ty.DefinePInvokeMethod (
                "Generic",
                native,
                "Shim_Generic",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.PinvokeImpl,
                CallingConventions.Standard,
                typeof<int>,
                [| typeof<int> |],
                Runtime.InteropServices.CallingConvention.Winapi,
                Runtime.InteropServices.CharSet.Ansi
            )

        generic.SetImplementationFlags MethodImplAttributes.PreserveSig

        generic.DefineGenericParameters [| "T" |]
        |> ignore<GenericTypeParameterBuilder[]>

        ty.CreateType () |> ignore<Type>

        let genericType =
            modul.DefineType ("GenericInterop", TypeAttributes.Public ||| TypeAttributes.Abstract)

        genericType.DefineGenericParameters [| "T" |]
        |> ignore<GenericTypeParameterBuilder[]>

        let inGenericType =
            genericType.DefinePInvokeMethod (
                "InGenericType",
                native,
                "Shim_InGenericType",
                MethodAttributes.Public
                ||| MethodAttributes.Static
                ||| MethodAttributes.PinvokeImpl,
                CallingConventions.Standard,
                typeof<int>,
                [| typeof<int> |],
                Runtime.InteropServices.CallingConvention.Winapi,
                Runtime.InteropServices.CharSet.Ansi
            )

        inGenericType.SetImplementationFlags MethodImplAttributes.PreserveSig
        genericType.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``only a CoreLib P/Invoke into a shim that copies every value as its bytes is that shim's function`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let recognised (name : string) (disablesMarshalling : bool) : (string * NativeMethod option) list =
            use stream = new MemoryStream (fabricateShimCalls name disablesMarshalling)
            let image = Assembly.read loggerFactory None stream

            [
                for KeyValue (handle, method) in image.Methods do
                    // The classes' default constructors.
                    if method.Name <> ".ctor" then
                        yield method.Name, NativeMethod.recognise image handle
            ]
            |> List.sort

        let shim (library : FrameworkShim) (name : string) : NativeMethod option =
            Some (NativeMethod.ShimFunction (library, $"Shim_%s{name}"))

        recognised "System.Private.CoreLib" true
        |> shouldEqual
            [
                "AutoLaid", None
                "Cdecl", shim FrameworkShim.SystemNative "Cdecl"
                "ConventionByAttribute", None
                "ConventionByModifier", None
                "ConvertsLocale", None
                "FastCall", None
                "Foreign", None
                "Generic", None
                "Globalization", shim FrameworkShim.GlobalizationNative "Globalization"
                "HoldsEnum", None
                "HoldsObject", None
                "InGenericType", None
                "Nested", shim FrameworkShim.SystemNative "Nested"
                "Nothing", shim FrameworkShim.SystemNative "Nothing"
                "Numbers", shim FrameworkShim.SystemNative "Numbers"
                "Overlaid", shim FrameworkShim.SystemNative "Overlaid"
                "Pointers", shim FrameworkShim.SystemNative "Pointers"
                "ReturnsObject", None
                "Runtime", None
                "SetsLastError", None
                "StaticField", shim FrameworkShim.SystemNative "StaticField"
                "StdCall", shim FrameworkShim.SystemNative "StdCall"
                "Structure", shim FrameworkShim.SystemNative "Structure"
                "System", None
                "TakesByref", None
                "TakesClass", None
                "TakesEnum", shim FrameworkShim.SystemNative "TakesEnum"
                "TakesObject", None
                "TakesString", None
                "ThisCall", None
                "Translated", None
                "VarArgs", None
                "Wide", None
            ]

        // With runtime marshalling on, a value type may be converted on its way (a `DateTime` to an
        // OLE date, which can overflow), so no call is known.
        recognised "System.Private.CoreLib" false |> List.choose snd |> shouldEqual []

        // The same calls from any other assembly bind through its own resolver, which may be the
        // program's code.
        recognised "Elsewhere" true |> List.choose snd |> shouldEqual []

    /// A program that has native code call a managed function that throws, inside a `try` that
    /// would catch it: the C library's `qsort`, comparing through a function pointer to an
    /// `[UnmanagedCallersOnly]` method, or to a delegate marshalled to native code.
    let private throwingCallback : string =
        String.concat
            "\n"
            [
                "using System;"
                "using System.Runtime.InteropServices;"
                "class CallbackFault : Exception { }"
                "static unsafe class Program"
                "{"
                "    [UnmanagedCallersOnly]"
                "    static int Throwing(void* a, void* b) => throw new CallbackFault();"
                "    delegate int Compare(void* a, void* b);"
                "    static int ThrowingManaged(void* a, void* b) => throw new CallbackFault();"
                "    static readonly Compare Kept = ThrowingManaged;"
                "    static int Main(string[] args)"
                "    {"
                "        var qsort = (delegate* unmanaged<void*, nuint, nuint, void*, void>)"
                "            NativeLibrary.GetExport(NativeLibrary.GetMainProgramHandle(), \"qsort\");"
                "        void* compare = args[0] == \"unmanaged-callers-only\""
                "            ? (void*)(delegate* unmanaged<void*, void*, int>)&Throwing"
                "            : (void*)Marshal.GetFunctionPointerForDelegate(Kept);"
                "        int* values = stackalloc int[] { 3, 1, 2, 0 };"
                "        try"
                "        {"
                "            qsort(values, 4, sizeof(int), compare);"
                "            return 0;"
                "        }"
                "        catch (CallbackFault)"
                "        {"
                "            return 1;"
                "        }"
                "    }"
                "}"
            ]

    /// A shim function's contract says it raises nothing, though some shim functions take a
    /// function pointer and call it: an exception the function they call throws ends the process
    /// rather than reaching the caller.
    [<TestCase "unmanaged-callers-only">]
    [<TestCase "marshalled-delegate">]
    let ``an exception thrown into native code ends the process, on this runtime`` (callback : string) : unit =
        let image = Roslyn.compile [ throwingCallback ]

        match RealRuntime.executeWithRealRuntime [| callback |] image with
        | RealRuntimeResult.UnhandledException report -> report |> shouldContainText "CallbackFault"
        | other -> failwith $"expected the callback's exception to end the process, got %A{other}"

    [<Test>]
    let ``only an FCall with a maths function's own signature is that function`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (fabricateMath ())
        let image = Assembly.read loggerFactory None stream

        let recognised =
            [
                for KeyValue (handle, method) in image.Methods do
                    yield method.Name, NativeMethod.recognise image handle
            ]
            |> List.sort

        recognised
        |> shouldEqual
            [
                "Cos", None
                "Exp", None
                "Floor", None
                "Pow", None
                "Sin", Some (NativeMethod.MathFunction (MathFunction.Sin, FloatWidth.Double))
                "Tan", None
            ]

    /// Values that exercise a floating-point function's edges: zeros, infinities, NaN, the
    /// subnormal and largest magnitudes, and a few ordinary points either side of 1.
    let private doubles : double list =
        [
            0.0
            -0.0
            1.0
            -1.0
            0.5
            -0.5
            2.0
            Math.PI
            Double.Epsilon
            -Double.Epsilon
            Double.MaxValue
            Double.MinValue
            Double.PositiveInfinity
            Double.NegativeInfinity
            Double.NaN
            1e-310
        ]

    let private singles : single list =
        [
            0.0f
            -0.0f
            1.0f
            -1.0f
            0.5f
            -0.5f
            2.0f
            MathF.PI
            Single.Epsilon
            -Single.Epsilon
            Single.MaxValue
            Single.MinValue
            Single.PositiveInfinity
            Single.NegativeInfinity
            Single.NaN
            1e-40f
        ]

    /// A delegate whose JIT-compiled body calls `method` directly, so that the call is expanded
    /// as the JIT expands it at an ordinary call site, where reflection would call the FCall.
    let private directCall (method : Reflection.MethodInfo) : Delegate =
        let parameters = method.GetParameters () |> Array.map (fun p -> p.ParameterType)

        let dynamicMethod =
            DynamicMethod ($"call_%s{method.Name}", method.ReturnType, parameters)

        let il = dynamicMethod.GetILGenerator ()

        for i in 0 .. parameters.Length - 1 do
            il.Emit (OpCodes.Ldarg, int16 i)

        il.Emit (OpCodes.Call, method)
        il.Emit OpCodes.Ret

        let delegateType =
            Linq.Expressions.Expression.GetDelegateType (Array.append parameters [| method.ReturnType |])

        dynamicMethod.CreateDelegate delegateType

    let private faultType (fault : PrimitiveFault) : Type =
        match fault with
        | PrimitiveFault.NullReference -> typeof<NullReferenceException>
        | PrimitiveFault.DataMisaligned -> typeof<DataMisalignedException>

    [<Test>]
    let ``every native method raises only what its contract says, on this runtime`` () : unit =
        let corelib = TestIntrinsicBody.coreLib "host"
        let mutable calls = 0

        for KeyValue (handle, method) in corelib.Methods do
            match NativeMethod.recognise corelib handle with
            // Calling one with values chosen here could do anything to this process; the callback
            // test holds what its contract rests on to the runtime instead.
            | None
            | Some (NativeMethod.ShimFunction _) -> ()
            | Some (NativeMethod.MathFunction (_, width) as native) ->
                let contract = NativeMethod.contract native
                let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                let runtimeType =
                    typeof<obj>.Assembly.GetType ($"%s{declaringType.Namespace}.%s{declaringType.Name}", true)

                let values : obj list =
                    match width with
                    | FloatWidth.Double -> doubles |> List.map box
                    | FloatWidth.Single -> singles |> List.map box

                let parameterTypes =
                    method.Signature.ParameterTypes
                    |> List.map (fun _ -> (List.head values).GetType ())
                    |> Array.ofList

                let target =
                    runtimeType.GetMethod (
                        method.Name,
                        BindingFlags.Public ||| BindingFlags.NonPublic ||| BindingFlags.Static,
                        parameterTypes
                    )

                if isNull target then
                    failwith $"%s{runtimeType.Name}.%s{method.Name} is not on the host's CoreLib"

                let direct = directCall target

                let rec arguments (arity : int) : obj list list =
                    if arity = 0 then
                        [ [] ]
                    else
                        [
                            for v in values do
                                for rest in arguments (arity - 1) do
                                    yield v :: rest
                        ]

                let raised (call : unit -> obj) : Type option =
                    try
                        call () |> ignore
                        None
                    with
                    | :? TargetInvocationException as e -> Some (e.InnerException.GetType ())
                    | e -> Some (e.GetType ())

                for args in arguments parameterTypes.Length do
                    let args = Array.ofList args

                    let invocations : (unit -> obj) list =
                        [
                            (fun () -> target.Invoke ((null : obj), args))
                            (fun () -> direct.DynamicInvoke args)
                        ]

                    for call in invocations do
                        calls <- calls + 1

                        match raised call with
                        | None -> contract.CanReturn |> shouldEqual true
                        | Some thrown ->
                            if not (contract.Raises |> List.exists (fun (fault, _) -> faultType fault = thrown)) then
                                failwith
                                    $"%s{runtimeType.Name}.%s{method.Name}%A{args} raised %s{thrown.Name}, which its contract %A{contract} leaves out"

                // A method returning a float returns no reference.
                if target.ReturnType.IsValueType then
                    contract.Result |> shouldEqual ResultNullness.NotAReference

        // 20 unary, 2 binary and 1 ternary function, at two widths, called two ways.
        calls |> shouldEqual (2 * 2 * (20 * 16 + 2 * 16 * 16 + 16 * 16 * 16))
