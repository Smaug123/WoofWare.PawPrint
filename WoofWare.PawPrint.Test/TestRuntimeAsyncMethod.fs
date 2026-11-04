namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `RuntimeAsyncMethod.isAsyncCall`, which .NET 11's `RuntimeMethodHandle.IsAsyncMethod` FCall
/// answers, checked against real .NET 11 over every method two framework assemblies declare.
[<TestFixture>]
module TestRuntimeAsyncMethod =

    /// Both are compiled with runtime async, so each declares methods that carry
    /// `MethodImplAttributes.Async` and return a promise type, which are not `AsyncCall`. Only
    /// CoreLib may declare one that returns anything else, which is.
    let private assemblyNames = [ "System.Private.CoreLib" ; "System.Net.Http" ]

    /// Takes `<assembly name>:<MethodDef row count>` arguments, and prints a line for each method that
    /// carries `MethodImplAttributes.Async` or that `IsAsyncMethod` calls async: its assembly, its
    /// MethodDef row, and whether it is async.
    let private oracleSource : string =
        """
using System;
using System.Reflection;

public static class Program
{
    public static int Main(string[] args)
    {
        var isAsyncMethod = typeof(RuntimeMethodHandle).GetMethod("IsAsyncMethod", BindingFlags.NonPublic | BindingFlags.Static);
        var handleType = isAsyncMethod.GetParameters()[0].ParameterType;
        var makeHandle = handleType.GetConstructor(BindingFlags.NonPublic | BindingFlags.Instance, null, new[] { typeof(IntPtr) }, null);
        foreach (var arg in args)
        {
            var parts = arg.Split(':');
            var module = Assembly.Load(parts[0]).ManifestModule;
            int rows = int.Parse(parts[1]);
            for (int row = 1; row <= rows; row++)
            {
                var method = module.ResolveMethod(0x06000000 | row);
                var handle = makeHandle.Invoke(new object[] { method.MethodHandle.Value });
                bool isAsync = (bool)isAsyncMethod.Invoke(null, new[] { handle });
                bool declaresAsync = ((int)method.MethodImplementationFlags & 0x2000) != 0;
                if (isAsync || declaresAsync)
                {
                    Console.WriteLine(parts[0] + " " + row + " " + (isAsync ? "async" : "not-async"));
                }
            }
        }
        return 0;
    }
}
"""

    [<Test>]
    let ``PawPrint calls async exactly the methods real .NET 11 does`` () : unit =
        match FrameworkUnderTest.runtime () with
        | EmulatedRuntime.Net10 ->
            Assert.Ignore ".NET 10's CoreLib does not declare `RuntimeMethodHandle.IsAsyncMethod`"
        | EmulatedRuntime.Net11 -> ()

        let _, loggerFactory = LoggerFactory.makeTest ()
        let frameworkDirectory = FrameworkUnderTest.sharedFrameworkDirectory ()

        let images =
            assemblyNames
            |> List.map (fun name -> Assembly.readFile loggerFactory (Path.Combine (frameworkDirectory, name + ".dll")))

        let expected =
            let run =
                RealRuntime.executeConcurrently
                    OracleFrameworks.NetCoreApp
                    AppContextProperties.empty
                    []
                    [
                        {
                            Image = Roslyn.compile [ oracleSource ]
                            Args =
                                List.map2
                                    (fun name (image : DumpedAssembly) -> $"%s{name}:%d{image.Methods.Count}")
                                    assemblyNames
                                    images
                        }
                    ]
                |> List.exactlyOne

            run.Result |> shouldEqual (RealRuntimeResult.NormalExit 0)

            run.Stdout.Split ('\n', System.StringSplitOptions.RemoveEmptyEntries)
            |> Array.map _.Trim()
            |> Set.ofArray

        let actual =
            let _, lines =
                ((LoadedAssemblies.ofAssemblies images, []), List.zip assemblyNames images)
                ||> List.fold (fun (assemblies, lines) (name, image) ->
                    (assemblies, lines)
                    |> Seq.foldBack
                        (fun (method : MethodInfo<_, _, _>) (assemblies, lines) ->
                            let facts = MethodInfo.requireMetadata "TestRuntimeAsyncMethod" method

                            let assemblies, isAsync =
                                RuntimeAsyncMethod.isAsyncCall
                                    loggerFactory
                                    [ frameworkDirectory ]
                                    images.Head
                                    image
                                    facts
                                    assemblies

                            let isAsync =
                                match isAsync with
                                | Ok isAsync -> isAsync
                                | Error failure -> failwith (TypeBindingFailure.describe failure)

                            let declaresAsync =
                                facts.ImplAttributes.HasFlag RuntimeAsyncMethod.asyncImplAttribute

                            if isAsync || declaresAsync then
                                let row =
                                    MetadataTokens.GetRowNumber (
                                        MethodDefinitionHandle.op_Implicit facts.Handle : EntityHandle
                                    )

                                let verdict = if isAsync then "async" else "not-async"
                                assemblies, $"%s{name} %d{row} %s{verdict}" :: lines
                            else
                                assemblies, lines
                        )
                        image.Methods.Values
                )

            Set.ofList lines

        // Neither answer is vacuous: each assembly list holds methods of both kinds.
        expected |> Set.exists _.EndsWith(" async") |> shouldEqual true
        expected |> Set.exists _.EndsWith(" not-async") |> shouldEqual true

        Set.difference expected actual |> shouldBeEmpty
        Set.difference actual expected |> shouldBeEmpty

    /// A row names a promise type when it names `System.Threading.Tasks.Task`, `ValueTask`,
    /// ``Task`1`` or ``ValueTask`1``, and only when the row binds to CoreLib's.
    [<Test>]
    let ``a method returns a promise type only when it returns CoreLib's`` () : unit =
        let source =
            """
using System.Threading.Tasks;

namespace System.Threading.Tasks
{
    public class Task { }
}

public static class Methods
{
    public static Task OwnTask() => null;
    public static ValueTask NonGenericValueTask() => default;
    public static Task<int> GenericTask() => null;
    public static ValueTask<int> GenericValueTask() => default;
    public static int Normal() => 0;
    public static void Nothing() { }
}
"""

        let _, loggerFactory = LoggerFactory.makeTest ()
        let frameworkDirectory = FrameworkUnderTest.sharedFrameworkDirectory ()

        let image =
            Roslyn.compileAssembly "PromiseReturns" OutputKind.DynamicallyLinkedLibrary [] [ source ]
            |> fun bytes -> Assembly.read loggerFactory (Some "PromiseReturns.dll") (new MemoryStream (bytes))

        let coreLib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDirectory, "System.Private.CoreLib.dll"))

        let methods =
            image.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = "Methods") |> _.Methods

        let _, kinds =
            ((LoadedAssemblies.ofAssemblies [ coreLib ; image ], Map.empty), methods)
            ||> List.fold (fun (assemblies, kinds) method ->
                let facts = MethodInfo.requireMetadata "TestRuntimeAsyncMethod" method

                match
                    RuntimeAsyncMethod.classifyReturn
                        loggerFactory
                        [ frameworkDirectory ]
                        coreLib
                        image
                        facts.RawSignature.ReturnType
                        assemblies
                with
                | assemblies, Ok kind -> assemblies, Map.add method.Name kind kinds
                | _, Error failure -> failwith (TypeBindingFailure.describe failure)
            )

        kinds
        |> shouldEqual (
            Map.ofList
                [
                    "OwnTask", MethodReturnKind.NormalMethod
                    "NonGenericValueTask", MethodReturnKind.NonGenericTaskReturningMethod
                    "GenericTask", MethodReturnKind.GenericTaskReturningMethod
                    "GenericValueTask", MethodReturnKind.GenericTaskReturningMethod
                    "Normal", MethodReturnKind.NormalMethod
                    "Nothing", MethodReturnKind.NormalMethod
                ]
        )
