namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestMethodReturnType =

    let private loadAssemblyFromSource
        (assemblyName : string)
        (source : string)
        : Microsoft.Extensions.Logging.ILoggerFactory * BaseClassTypes<DumpedAssembly> * DumpedAssembly * IlMachineState
        =
        let image =
            Roslyn.compileAssembly assemblyName Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelibPath = typeof<obj>.Assembly.Location

        let corelib =
            global.WoofWare.PawPrint.AssemblyApi.readFile loggerFactory corelibPath

        let baseClassTypes = Corelib.getBaseTypes corelib

        use assemblyStream = new MemoryStream (image)

        let assembly =
            global.WoofWare.PawPrint.AssemblyApi.read loggerFactory None assemblyStream

        let state : IlMachineState =
            let initialState =
                IlMachineState.initial loggerFactory ImmutableArray.Empty assembly

            let state = initialState.WithLoadedAssembly corelib.Name corelib

            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies baseClassTypes state.ConcreteTypes
            }

        loggerFactory, baseClassTypes, assembly, state

    let private findMethod
        (declaringTypeName : string)
        (methodName : string)
        (assembly : DumpedAssembly)
        : WoofWare.PawPrint.MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>
        =
        assembly.Methods.Values
        |> Seq.find (fun method -> method.DeclaringType.Name = declaringTypeName && method.Name = methodName)

    let private source =
        """
public static class ReturnShapes
{
    public static void DoesNotReturn()
    {
    }

    public static int ReturnsInt()
    {
        return 1;
    }
}
"""

    [<Test>]
    let ``void return is parsed as no return value`` () =
        let _, _, assembly, _ = loadAssemblyFromSource "MethodReturnTypeTestAssembly" source
        let method = findMethod "ReturnShapes" "DoesNotReturn" assembly

        match method.Signature.ReturnType with
        | MethodReturnType.Void -> ()
        | other -> Assert.Fail $"Expected void return, got %O{other}"

    [<Test>]
    let ``non-void return is parsed as returned type`` () =
        let _, _, assembly, _ = loadAssemblyFromSource "MethodReturnTypeTestAssembly" source
        let method = findMethod "ReturnShapes" "ReturnsInt" assembly

        match method.Signature.ReturnType with
        | MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32) -> ()
        | other -> Assert.Fail $"Expected int32 return, got %O{other}"

    [<Test>]
    let ``mapping a void signature does not map a fake return type`` () =
        let _, _, assembly, _ = loadAssemblyFromSource "MethodReturnTypeTestAssembly" source
        let method = findMethod "ReturnShapes" "DoesNotReturn" assembly
        let mutable mapperCalls = 0

        let _, mapped =
            TypeMethodSignature.map
                ()
                (fun () ty ->
                    mapperCalls <- mapperCalls + 1
                    (), ty
                )
                method.Signature

        Assert.That (mapperCalls, Is.EqualTo 0)

        match mapped.ReturnType with
        | MethodReturnType.Void -> ()
        | other -> Assert.Fail $"Expected mapped void return, got %O{other}"

    [<Test>]
    let ``concretizing a void method preserves no return value`` () =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "MethodReturnTypeTestAssembly" source

        let method =
            findMethod "ReturnShapes" "DoesNotReturn" assembly
            |> MethodInfo.mapTypeGenerics (fun (param, _) -> TypeDefn.GenericTypeParameter param.SequenceNumber)

        let _, concretizedMethod, _ =
            ExecutionConcretization.concretizeMethodWithAllGenerics
                loggerFactory
                baseClassTypes
                ImmutableArray.Empty
                method
                ImmutableArray.Empty
                state

        match concretizedMethod.Signature.ReturnType with
        | MethodReturnType.Void -> ()
        | other -> Assert.Fail $"Expected concretized void return, got %O{other}"
