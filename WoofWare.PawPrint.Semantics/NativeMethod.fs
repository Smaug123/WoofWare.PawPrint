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

[<RequireQualifiedAccess>]
module NativeMethod =

    let private corelib : string = "System.Private.CoreLib"

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

    /// The operation `method` performs, when it is a native method of `assembly`, a CoreLib, that
    /// this module describes. Recognition is by class, name and signature, so it holds for any
    /// CoreLib that keeps the method's signature; a method this does not recognise is `None`.
    let recognise (assembly : DumpedAssembly) (method : MethodDefinitionHandle) : NativeMethod option =
        let definition = assembly.Methods.[method]

        let declaringType =
            assembly.TypeDefs.[definition.RequiredDeclaringType.Definition.Get]

        match definition.Body with
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
        | _ -> None

    /// What `native` can do to its caller.
    let contract (native : NativeMethod) : IntrinsicContract =
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
