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
                    | Some native, Some width ->
                        match native with
                        | NativeMethod.MathFunction (fn, recognisedWidth) ->
                            // The case is the method, spelled as its C# name.
                            caseName fn |> shouldEqual method.Name
                            recognisedWidth |> shouldEqual width
                            yield native
                    | Some native, None ->
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
            | None -> ()
            | Some native ->
                let contract = NativeMethod.contract native
                let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                let runtimeType =
                    typeof<obj>.Assembly.GetType ($"%s{declaringType.Namespace}.%s{declaringType.Name}", true)

                let values : obj list =
                    match native with
                    | NativeMethod.MathFunction (_, FloatWidth.Double) -> doubles |> List.map box
                    | NativeMethod.MathFunction (_, FloatWidth.Single) -> singles |> List.map box

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
