namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `StringConstructor` says which `String.Ctor` CoreCLR runs for each constructor of
/// `System.String`. These tests hold it to the CoreLibs' own metadata, and to a fabricated type for
/// the ways a constructor can lack one.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStringConstructor =

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    [<TestCaseSource(nameof coreLibs)>]
    let ``every String constructor is an FCall run as its own String.Ctor`` (which : string) : unit =
        let corelib = TestIntrinsicBody.coreLib which
        let stringType = (BaseClassTypes.ofCorelib corelib).String

        let constructors =
            stringType.Methods |> List.filter (fun method -> method.Name = ".ctor")

        // `vm/ecall.cpp`'s `NumberOfStringConstructors`.
        constructors.Length |> shouldEqual 9

        let implementations =
            constructors
            |> List.map (fun constructor ->
                match constructor.Body with
                | MethodBody.InternalCall -> ()
                | _ -> failwith $"String::.ctor%A{constructor.Signature.ParameterTypes} is not an FCall"

                match StringConstructor.implementation stringType constructor with
                | Ok implementation ->
                    implementation.Name |> shouldEqual "Ctor"
                    implementation.IsStatic |> shouldEqual true

                    implementation.Signature.ParameterTypes
                    |> shouldEqual constructor.Signature.ParameterTypes

                    (MethodInfo.requireMetadata "String.Ctor" implementation).Handle
                | Error fault -> failwith $"String::.ctor%A{constructor.Signature.ParameterTypes} has none: %A{fault}"
            )

        // No two constructors share one.
        implementations
        |> List.distinct
        |> List.length
        |> shouldEqual constructors.Length

    /// An image whose class `Text` has constructors from `int`, `long`, `byte`, `short` and `char`,
    /// and these methods named `Ctor`: static ones from `int` to `string`, from `long` to `string`
    /// and to `object`, and from `byte` to `object`, and an instance one from `char` to `string`.
    let private fabricateText () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "Strings", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "Strings"
        let ty = modul.DefineType ("Text", TypeAttributes.Public)

        for parameter in [ typeof<int> ; typeof<int64> ; typeof<byte> ; typeof<int16> ; typeof<char> ] do
            let constructor =
                ty.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [| parameter |])

            constructor.GetILGenerator().Emit OpCodes.Ret

        let ctor (attributes : MethodAttributes) (returns : Type) (parameter : Type) =
            let method =
                ty.DefineMethod ("Ctor", MethodAttributes.Public ||| attributes, returns, [| parameter |])

            let il = method.GetILGenerator ()
            il.Emit OpCodes.Ldnull
            il.Emit OpCodes.Ret

        ctor MethodAttributes.Static typeof<string> typeof<int>
        ctor MethodAttributes.Static typeof<string> typeof<int64>
        ctor MethodAttributes.Static typeof<obj> typeof<int64>
        ctor MethodAttributes.Static typeof<obj> typeof<byte>
        ctor MethodAttributes.PrivateScope typeof<string> typeof<char>

        ty.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``a constructor has its String.Ctor only if exactly one static Ctor takes its parameters and returns a string``
        ()
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = new MemoryStream (fabricateText ())
        let image = Assembly.read loggerFactory None stream

        let text = image.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = "Text")

        let outcome =
            [
                for method in text.Methods do
                    if method.Name = ".ctor" then
                        let parameter = List.exactlyOne method.Signature.ParameterTypes

                        let outcome =
                            StringConstructor.implementation text method
                            |> Result.map (fun implementation ->
                                implementation.Signature.ParameterTypes, implementation.Signature.ReturnType
                            )

                        yield parameter, outcome
            ]
            |> Map.ofList

        let primitive = TypeDefn.PrimitiveType

        outcome
        |> shouldEqual (
            Map.ofList
                [
                    primitive PrimitiveType.Int32,
                    Ok ([ primitive PrimitiveType.Int32 ], MethodReturnType.Returns (primitive PrimitiveType.String))
                    primitive PrimitiveType.Int64, Error StringConstructorFault.SeveralCtors
                    primitive PrimitiveType.Byte,
                    Error (
                        StringConstructorFault.CtorReturns (MethodReturnType.Returns (primitive PrimitiveType.Object))
                    )
                    primitive PrimitiveType.Int16, Error StringConstructorFault.NoCtor
                    // The `Ctor` from `char` is an instance method.
                    primitive PrimitiveType.Char, Error StringConstructorFault.NoCtor
                ]
        )

        let notAConstructor = text.Methods |> List.find (fun method -> method.Name = "Ctor")

        Assert.Throws<exn> (fun () -> StringConstructor.implementation text notAConstructor |> ignore)
        |> ignore<exn>
