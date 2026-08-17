namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open FsUnitTyped
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

            let state = initialState.WithLoadedAssembly corelib

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
        |> Seq.find (fun method ->
            method.RequiredDeclaringType.Name = declaringTypeName
            && method.Name = methodName
        )

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

public sealed class ModifiedReturnShapes
{
    private int[] cells = new int[1];

    // `set_InitOnly` returns `void modreq(System.Runtime.CompilerServices.IsExternalInit)`.
    public int InitOnly { get; init; }

    // `RefToCell` returns `int32& modreq(System.Runtime.InteropServices.InAttribute)`.
    public ref readonly int RefToCell()
    {
        return ref cells[0];
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

    /// A decoded signature mirrors its blob, so the modifier is still there. The metadata questions
    /// that read a decoded signature are blob comparisons — CoreCLR's `ExactlyEqual`, which is what
    /// decides whether a `.ctor` becomes a type's default constructor, and vtable slot matching,
    /// which compares a return's modifiers alongside its handle — and a modifier makes two
    /// signatures different to them. Folding it away here would take that information from them:
    /// `TestFabricatedVtableLayout`'s modopt-void constructor is what notices.
    [<Test>]
    let ``a decoded init-only setter keeps the custom modifier on its return`` () =
        let _, _, assembly, _ = loadAssemblyFromSource "MethodReturnTypeTestAssembly" source
        let method = findMethod "ModifiedReturnShapes" "set_InitOnly" assembly

        match method.Signature.ReturnType with
        | MethodReturnType.Returns (TypeDefn.Modified modified) ->
            TypeDefn.stripCustomModifiers modified.Unmodified |> shouldEqual TypeDefn.Void
            modified.IsRequired |> shouldEqual true
        | other -> Assert.Fail $"Expected a modified void return, got %O{other}"

    [<Test>]
    let ``a decoded ref readonly return keeps the custom modifier on its return`` () =
        let _, _, assembly, _ = loadAssemblyFromSource "MethodReturnTypeTestAssembly" source
        let method = findMethod "ModifiedReturnShapes" "RefToCell" assembly

        match method.Signature.ReturnType with
        | MethodReturnType.Returns (TypeDefn.Modified modified) ->
            modified.Unmodified
            |> shouldEqual (TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.Int32))
        | other -> Assert.Fail $"Expected a modified byref return, got %O{other}"

    /// The other half: concretisation is where the modifier is looked through, so an `init`
    /// accessor's *runtime* signature returns nothing. Everything that executes a method reads this
    /// form — `returnStackFrame`'s check that a void method returns with an empty evaluation stack
    /// most of all.
    [<Test>]
    let ``concretizing an init-only setter yields no return value`` () =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "MethodReturnTypeTestAssembly" source

        let method =
            findMethod "ModifiedReturnShapes" "set_InitOnly" assembly
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

    /// The control for the test above: a modifier on a return type that *does* come back must not
    /// make the method look void. Both returns carry a `modreq`; only one of them decorates `void`.
    [<Test>]
    let ``concretizing a ref readonly return still yields a return value`` () =
        let loggerFactory, baseClassTypes, assembly, state =
            loadAssemblyFromSource "MethodReturnTypeTestAssembly" source

        let method =
            findMethod "ModifiedReturnShapes" "RefToCell" assembly
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
        | MethodReturnType.Returns (ConcreteTypeHandle.Byref _) -> ()
        | other -> Assert.Fail $"Expected concretized byref return, got %O{other}"

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
