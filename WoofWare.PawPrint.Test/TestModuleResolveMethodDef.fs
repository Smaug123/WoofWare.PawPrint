namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// <c>Module.ResolveMethod</c> on a MethodDef token, against the real runtime. CoreCLR ignores the
/// caller's instantiations for a MethodDef and answers with the typical definition: the method on
/// its declaring type's generic definition, and a generic method's definition, the very
/// <c>MethodInfo</c> that the declaring type's <c>GetMethod</c> returns.
/// </summary>
/// <remarks>
/// Except for a generic virtual method on a value type, generic or not: there CoreCLR answers with
/// a generic method which is not the definition, whose handle is not the definition's, and which is
/// not equal to it, as it does for a MemberRef (see <c>TestModuleResolveMethodMemberRef</c>).
/// PawPrint does not model that answer, and refuses it. A non-generic virtual method on a value type
/// is answered with the definition.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestModuleResolveMethodDef =

    /// `Main` resolves each MethodDef token it is given, in the context given beside it, and
    /// classifies the answer: in `classify` mode it returns the one token's classification, and in
    /// `check` mode it returns 0 if every token's is the one given beside it, or one more than the
    /// index of the first that is not.
    let private source =
        """
using System;
using System.Reflection;

public interface IConvert
{
    U GenericVirtual<U>();
    void PlainVirtual();
}

public class Holder<T> : IConvert
{
    public U GenericVirtual<U>() => default!;
    public void PlainVirtual() { }
}

public struct SBox<T> : IConvert
{
    public U GenericVirtual<U>() => default!;
    public void PlainVirtual() { }
    public U GenericNonVirtual<U>() => default!;
    public void PlainNonVirtual() { }
}

public struct Plain : IConvert
{
    public U GenericVirtual<U>() => default!;
    public void PlainVirtual() { }
    public U GenericNonVirtual<U>() => default!;
    public void PlainNonVirtual() { }
}

// Derives from a type in an assembly that nothing else in this program loads, so that resolving
// one of its methods is the first thing to touch that assembly.
public class OnXml : System.Xml.XmlDocument
{
    public void PlainOnXml() { }
    public virtual U GenericVirtualOnXml<U>() => default!;
}

public static class Program
{
    static int Classify(int token, string context)
    {
        Type[] typeArgs = context == "t=int" ? new[] { typeof(int) } : null;
        MethodBase m = typeof(Program).Module.ResolveMethod(token, typeArgs, null);
        MethodInfo definition = m.DeclaringType.GetMethod(m.Name);
        int code = 0;
        if (m.DeclaringType.IsGenericTypeDefinition) code |= 1;
        if (m.IsGenericMethodDefinition) code |= 2;
        if (ReferenceEquals(m, definition)) code |= 4;
        if (m.MethodHandle == definition.MethodHandle) code |= 8;
        if (m.IsGenericMethod) code |= 16;
        if (m.Equals(definition)) code |= 32;
        return code;
    }

    public static int Main(string[] args)
    {
        if (args[0] == "classify")
            return Classify(int.Parse(args[1]), args[2]);

        for (int i = 0; 1 + 3 * i < args.Length; i++)
        {
            int token = int.Parse(args[1 + 3 * i]);
            if (Classify(token, args[2 + 3 * i]) != int.Parse(args[3 + 3 * i]))
                return i + 1;
        }

        return 0;
    }
}
"""

    let private image : Lazy<byte[]> = lazy (Roslyn.compile [ source ])

    /// The token of the MethodDef `methodName` on the type `typeName`, spelled as in metadata.
    let private methodDefToken (image : byte[]) (typeName : string) (methodName : string) : int =
        use peReader = new PEReader (new MemoryStream (image))
        let reader = peReader.GetMetadataReader ()

        let tokens =
            [
                for typeHandle in reader.TypeDefinitions do
                    let typeDef = reader.GetTypeDefinition typeHandle

                    if reader.GetString typeDef.Name = typeName then
                        for methodHandle in typeDef.GetMethods () do
                            if reader.GetString (reader.GetMethodDefinition methodHandle).Name = methodName then
                                yield
                                    MetadataTokens.GetToken (
                                        MethodDefinitionHandle.op_Implicit methodHandle : EntityHandle
                                    )
            ]

        match tokens with
        | [ token ] -> token
        | tokens -> failwith $"test bug: %d{tokens.Length} MethodDefs are named %s{typeName}.%s{methodName}"

    let private realRuntime (args : string list) : int =
        match RealRuntime.executeWithRealRuntime (Array.ofList args) (image.Force ()) with
        | RealRuntimeResult.NormalExit exitCode -> exitCode
        | other -> failwith $"expected the real runtime to exit normally, got %A{other}"

    let private underPawPrint (name : string) (args : string list) : RunOutcome =
        let _messages, loggerFactory =
            LoggerFactory.makeTestWithProperties [ "source_file", name ]

        use _loggerFactoryResource = loggerFactory

        let config =
            let config =
                HostConfig.Default (FrameworkUnderTest.runtimeDirsFor KernelConfig.Default.UnixPlatform)

            { config with
                Guest =
                    { config.Guest with
                        Argv = args
                    }
            }

        use peImage = new MemoryStream (image.Force ())
        BoundedRun.run loggerFactory name (Some name) peImage config |> ExpectRun.ended

    // Classifications, as `Classify` computes them: 1 for a declaring type that is a generic type
    // definition, 2 for a generic method definition, 4 for the very `MethodInfo` that the declaring
    // type's `GetMethod` returns, 8 for that method's handle, 16 for a generic method, and 32 for a
    // method equal to it.
    let private definition = 2 + 4 + 8 + 16 + 32
    let private plain = 4 + 8 + 32

    /// The cases PawPrint answers: the type, the method, the context the MethodDef is resolved in,
    /// and what the real runtime was measured to answer.
    let private answered : (string * string * string * int) list =
        [
            "Holder`1", "GenericVirtual", "none", 1 + definition
            // A MethodDef ignores the caller's instantiation.
            "Holder`1", "GenericVirtual", "t=int", 1 + definition
            "Holder`1", "PlainVirtual", "none", 1 + plain
            "SBox`1", "PlainVirtual", "none", 1 + plain
            "SBox`1", "PlainVirtual", "t=int", 1 + plain
            "SBox`1", "GenericNonVirtual", "none", 1 + definition
            "SBox`1", "PlainNonVirtual", "none", 1 + plain
            "Plain", "PlainVirtual", "none", plain
            "Plain", "GenericNonVirtual", "none", definition
            "Plain", "PlainNonVirtual", "none", plain
        ]

    /// The generic virtual methods on value types, which PawPrint refuses, and what the real runtime
    /// was measured to answer.
    let private genericVirtualOnValueType : (string * string * string * int) list =
        [
            "SBox`1", "GenericVirtual", "none", 1 + 16
            "SBox`1", "GenericVirtual", "t=int", 1 + 16
            "Plain", "GenericVirtual", "none", 16
        ]

    /// Methods on a class whose base is in an assembly nothing else loads, each resolved by a run of
    /// its own so that the resolution is the first thing to touch that assembly, and what the real
    /// runtime was measured to answer.
    let private firstTouch : (string * string * string * int) list =
        [
            "OnXml", "PlainOnXml", "none", plain
            "OnXml", "GenericVirtualOnXml", "none", definition
        ]

    let private checkArgs (cases : (string * string * string * int) list) : string list =
        [
            yield "check"
            for typeName, methodName, context, expected in cases do
                yield string (methodDefToken (image.Force ()) typeName methodName)
                yield context
                yield string expected
        ]

    [<Test>]
    let ``The real runtime answers as measured`` () : unit =
        for typeName, methodName, context, measured in answered @ genericVirtualOnValueType @ firstTouch do
            let token = methodDefToken (image.Force ()) typeName methodName
            let actual = realRuntime [ "classify" ; string token ; context ]

            if actual <> measured then
                failwith
                    $"%s{typeName}.%s{methodName} in context %s{context}: measured %d{measured}, but the real runtime now gives %d{actual}"

    [<Test>]
    let ``PawPrint resolves a MethodDef to the method CoreCLR does`` () : unit =
        let args = checkArgs answered
        realRuntime args |> shouldEqual 0

        match underPawPrint "ModuleResolveMethodDef" args with
        | RunOutcome.NormalExit (terminalState, _, _)
        | RunOutcome.ProcessExit (terminalState, _, _) ->
            match terminalState.LatchedExitCode with
            | 0 -> ()
            | failing ->
                let typeName, methodName, context, expected = answered.[failing - 1]

                failwith
                    $"%s{typeName}.%s{methodName} in context %s{context}: the real runtime classifies it %d{expected}; PawPrint does not"
        | other -> failwith $"expected the guest to exit, got %A{other}"

    [<TestCase 0>]
    [<TestCase 1>]
    [<TestCase 2>]
    let ``PawPrint refuses a generic virtual method on a value type`` (index : int) : unit =
        let typeName, methodName, _, _ = genericVirtualOnValueType.[index]

        let exn =
            Assert.Catch (fun () ->
                underPawPrint
                    $"ModuleResolveMethodDefGenericVirtualOnValueType%d{index}"
                    (checkArgs [ genericVirtualOnValueType.[index] ])
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "ModuleHandle.ResolveMethod"
        exn.Message |> shouldContainText "unboxing stub"
        exn.Message |> shouldContainText typeName
        exn.Message |> shouldContainText methodName

    [<TestCase 0>]
    [<TestCase 1>]
    let ``PawPrint resolves a MethodDef on a class whose base is in an unloaded assembly`` (index : int) : unit =
        let typeName, methodName, context, expected = firstTouch.[index]

        match underPawPrint $"ModuleResolveMethodDefFirstTouch%d{index}" (checkArgs [ firstTouch.[index] ]) with
        | RunOutcome.NormalExit (terminalState, _, _)
        | RunOutcome.ProcessExit (terminalState, _, _) ->
            if terminalState.LatchedExitCode <> 0 then
                failwith
                    $"%s{typeName}.%s{methodName} in context %s{context}: the real runtime classifies it %d{expected}; PawPrint does not"
        | other -> failwith $"expected the guest to exit, got %A{other}"
