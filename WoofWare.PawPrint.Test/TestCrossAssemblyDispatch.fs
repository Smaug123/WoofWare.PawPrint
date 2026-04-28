namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
module TestCrossAssemblyDispatch =

    type private ObservedLdstr =
        {
            SourceAssemblyFullName : string
            Token : StringToken
            Contents : string
        }

    let private readAssembly (assemblyName : string) (bytes : byte array) : DumpedAssembly =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "inspection_assembly", assemblyName ]

        let peImage = new MemoryStream (bytes)

        AssemblyApi.read loggerFactory (Some $"{assemblyName}.dll") peImage

    let private readAssemblies (assemblies : CrossAssemblySpec list) : Map<string, DumpedAssembly> =
        CrossAssemblyHarness.compileAssemblies assemblies |> Map.map readAssembly

    let private findMethod (assembly : DumpedAssembly) (typeName : string) (methodName : string) =
        assembly.TypeDefs.Values
        |> Seq.collect (fun typeInfo -> typeInfo.Methods)
        |> Seq.filter (fun methodInfo -> methodInfo.DeclaringType.Name = typeName && methodInfo.Name = methodName)
        |> Seq.exactlyOne

    let private exactlyOne (label : string) (values : 'a list) : 'a =
        match values with
        | [ value ] -> value
        | _ -> failwith $"Expected exactly one {label}, but found {values.Length}"

    let private ldstrOperands
        (assembly : DumpedAssembly)
        (typeName : string)
        (methodName : string)
        : ObservedLdstr list
        =
        let methodInfo = findMethod assembly typeName methodName

        match methodInfo.Instructions with
        | None -> failwith $"Expected {assembly.Name.Name}.{typeName}.{methodName} to have an IL body"
        | Some instructions ->
            instructions.Instructions
            |> List.choose (fun (op, _offset) ->
                match op with
                | IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, token) ->
                    Some
                        {
                            SourceAssemblyFullName = token.SourceAssembly.FullName
                            Token = token.Token
                            Contents = assembly.Strings token.Token
                        }
                | _ -> None
            )

    let private inspectLdstrByContents
        (assemblies : Map<string, DumpedAssembly>)
        (assemblyName : string)
        (typeName : string)
        (methodName : string)
        (expectedContents : string)
        : ObservedLdstr
        =
        let assembly = assemblies.[assemblyName]

        let operands = ldstrOperands assembly typeName methodName

        for operand in operands do
            operand.SourceAssemblyFullName |> shouldEqual assembly.Name.FullName

        operands
        |> List.filter (fun operand -> operand.Contents = expectedContents)
        |> exactlyOne $"{assemblyName}.{typeName}.{methodName} ldstr with contents {expectedContents}"

    let private assertDifferentLiteralSameToken
        (assemblies : CrossAssemblySpec list)
        (leftAssemblyName : string)
        (leftTypeName : string)
        (leftMethodName : string)
        (leftContents : string)
        (rightAssemblyName : string)
        (rightTypeName : string)
        (rightMethodName : string)
        (rightContents : string)
        : unit
        =
        let dumped = readAssemblies assemblies

        let left =
            inspectLdstrByContents dumped leftAssemblyName leftTypeName leftMethodName leftContents

        let right =
            inspectLdstrByContents dumped rightAssemblyName rightTypeName rightMethodName rightContents

        left.Token |> shouldEqual right.Token
        left.Contents |> shouldNotEqual right.Contents

    let private assertEqualLiteralDifferentTokens
        (assemblies : CrossAssemblySpec list)
        (leftAssemblyName : string)
        (leftTypeName : string)
        (leftMethodName : string)
        (leftContents : string)
        (rightAssemblyName : string)
        (rightTypeName : string)
        (rightMethodName : string)
        (rightContents : string)
        : unit
        =
        let dumped = readAssemblies assemblies

        let left =
            inspectLdstrByContents dumped leftAssemblyName leftTypeName leftMethodName leftContents

        let right =
            inspectLdstrByContents dumped rightAssemblyName rightTypeName rightMethodName rightContents

        left.Contents |> shouldEqual right.Contents
        right.Contents |> shouldEqual rightContents
        left.Token |> shouldNotEqual right.Token

    [<Test>]
    let ``interface dispatch uses implementation assembly for method body metadata`` () : unit =
        let assemblies =
            [
                CrossAssemblySpec.library
                    "CrossAssemblyDispatch.Contract"
                    []
                    [
                        """
namespace CrossAssemblyDispatch;

public interface IProbe
{
    int Measure();
}

public static class ContractStrings
{
    public static string FirstUserString()
    {
        return "bad";
    }
}
"""
                    ]
                CrossAssemblySpec.library
                    "CrossAssemblyDispatch.Implementation"
                    [ "CrossAssemblyDispatch.Contract" ]
                    [
                        """
using CrossAssemblyDispatch;

namespace CrossAssemblyDispatch.Implementation;

public sealed class Probe : IProbe
{
    public int Measure()
    {
        string value = Identity("implementation");
        return value.Length;
    }

    private static string Identity(string value)
    {
        return value;
    }
}
"""
                    ]
                CrossAssemblySpec.entryPoint
                    "CrossAssemblyDispatch.Entry"
                    [ "CrossAssemblyDispatch.Contract" ; "CrossAssemblyDispatch.Implementation" ]
                    [
                        """
using CrossAssemblyDispatch;
using CrossAssemblyDispatch.Implementation;

class Program
{
    static int Main(string[] argv)
    {
        IProbe probe = new Probe();
        int measured = probe.Measure();
        return measured == 14 ? 0 : measured;
    }
}
"""
                    ]
            ]

        assertDifferentLiteralSameToken
            assemblies
            "CrossAssemblyDispatch.Contract"
            "ContractStrings"
            "FirstUserString"
            "bad"
            "CrossAssemblyDispatch.Implementation"
            "Probe"
            "Measure"
            "implementation"

        {
            Assemblies = assemblies
            EntryAssemblyName = "CrossAssemblyDispatch.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``ldstr intern cache does not collide on same token from different assemblies`` () : unit =
        let assemblies =
            [
                CrossAssemblySpec.library
                    "LdstrInterning.First"
                    []
                    [
                        """
namespace LdstrInterning;

public static class First
{
    public static string Value()
    {
        return "contract";
    }
}
"""
                    ]
                CrossAssemblySpec.library
                    "LdstrInterning.Second"
                    []
                    [
                        """
namespace LdstrInterning;

public static class Second
{
    public static string Value()
    {
        return "implementation";
    }
}
"""
                    ]
                CrossAssemblySpec.entryPoint
                    "LdstrInterning.Entry"
                    [ "LdstrInterning.First" ; "LdstrInterning.Second" ]
                    [
                        """
using LdstrInterning;

class Program
{
    static int Main(string[] argv)
    {
        string first = First.Value();
        string second = Second.Value();

        if (first.Length != 8) return 100 + first.Length;
        return second.Length == 14 ? 0 : second.Length;
    }
}
"""
                    ]
            ]

        assertDifferentLiteralSameToken
            assemblies
            "LdstrInterning.First"
            "First"
            "Value"
            "contract"
            "LdstrInterning.Second"
            "Second"
            "Value"
            "implementation"

        {
            Assemblies = assemblies
            EntryAssemblyName = "LdstrInterning.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest

    [<Test>]
    let ``ldstr interns equal literals across assemblies by value`` () : unit =
        let assemblies =
            [
                CrossAssemblySpec.library
                    "LdstrInterningShared.First"
                    []
                    [
                        """
namespace LdstrInterningShared;

public static class First
{
    public static string Value()
    {
        string padding = "padding";
        if (padding.Length == 0) return padding;
        return "shared";
    }
}
"""
                    ]
                CrossAssemblySpec.library
                    "LdstrInterningShared.Second"
                    []
                    [
                        """
namespace LdstrInterningShared;

public static class Second
{
    public static string Value()
    {
        return "shared";
    }
}
"""
                    ]
                CrossAssemblySpec.entryPoint
                    "LdstrInterningShared.Entry"
                    [ "LdstrInterningShared.First" ; "LdstrInterningShared.Second" ]
                    [
                        """
using LdstrInterningShared;

class Program
{
    static int Main(string[] argv)
    {
        string first = First.Value();
        string second = Second.Value();

        return object.ReferenceEquals(first, second) ? 0 : 1;
    }
}
"""
                    ]
            ]

        assertEqualLiteralDifferentTokens
            assemblies
            "LdstrInterningShared.First"
            "First"
            "Value"
            "shared"
            "LdstrInterningShared.Second"
            "Second"
            "Value"
            "shared"

        {
            Assemblies = assemblies
            EntryAssemblyName = "LdstrInterningShared.Entry"
            ExpectedReturnCode = 0
            NativeImpls = MockEnv.make ()
        }
        |> CrossAssemblyHarness.runTest
