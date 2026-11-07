namespace WoofWare.PawPrint.Test

open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// <c>Module.ResolveMethod</c> on a MethodSpec token, against the real runtime. CoreCLR answers with
/// the method the spec names, instantiated at the spec's arguments read in the caller's context:
/// the very <c>MethodInfo</c> that <c>MakeGenericMethod</c> returns for those arguments, on a
/// shared instantiation and on a value type's generic virtual method as anywhere else.
/// </summary>
/// <remarks>
/// A spec whose arguments, or whose MemberRef's parent, name a generic parameter the caller's context
/// does not supply raises <c>BadImageFormatException</c>, which <c>RuntimeModule.ResolveMethod</c>
/// wraps in <c>ArgumentException</c>. One whose arguments violate the method's constraints raises
/// <c>VerificationException</c>, which PawPrint refuses.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestModuleResolveMethodSpec =

    /// Each method of `Sites`, and `Caller`, `CallerS`, `InContext` and `MakeInMethod`, calls exactly
    /// one method through a MethodSpec. `Main` resolves each token it is given, in the context given
    /// beside it, and returns 0 if every answer is the one given beside it, or one more than the index
    /// of the first that is not. An answer is either a key of `Expected`, which the resolved method
    /// must be the very `MethodInfo` of, or `!` and the names of the exception resolving must raise and
    /// of its inner exceptions, separated by `/`. A `BadImageFormatException` among them must carry
    /// the HResult and message CoreCLR gives it.
    let private source =
        """
using System;
using System.Linq;
using System.Reflection;

public interface IConvert
{
    U GenericVirtual<U>();
}

public class Holder<T>
{
    public U Convert<U>() => default!;
    public static U Make<U>() => default!;
    public T InContext() => Convert<T>();
    public int ParentInContext() => Convert<int>();
    public static W MakeInMethod<W>() => Make<W>();
}

public struct SBox<T> : IConvert
{
    public U GenericVirtual<U>() => default!;
    public U GenericNonVirtual<U>() => default!;
}

public struct Plain : IConvert
{
    public U GenericVirtual<U>() => default!;
}

public static class Statics
{
    public static U Generic<U>() => default!;
    public static V Caller<V>() => Generic<V>();
    public static V Two<V, W>() => default!;
    public static V Constrained<V>() where V : struct => default!;
    public static W CallerS<W>() where W : struct => Constrained<W>();
}

// Each derives from a type in an assembly that nothing else in this program loads, so that
// resolving a MethodSpec of one of their methods can be the first thing to touch that assembly.
public class OnXml : System.Xml.XmlDocument
{
    public static U Make<U>() => default!;
}

public class XmlHolder<T> : System.Xml.XmlDocument
{
    public static U Make<U>() => default!;
}

public static class Sites
{
    public static void GenericOfInt() => Statics.Generic<int>();
    public static void GenericOfString() => Statics.Generic<string>();
    public static void Two() => Statics.Two<int, string>();
    public static void HolderConvert() => new Holder<int>().Convert<long>();
    public static void HolderOfStringConvert() => new Holder<string>().Convert<string>();
    public static void HolderMake() => Holder<int>.Make<long>();
    public static void SBoxGenericVirtual() => new SBox<int>().GenericVirtual<long>();
    public static void SBoxGenericNonVirtual() => new SBox<int>().GenericNonVirtual<long>();
    public static void PlainGenericVirtual() => new Plain().GenericVirtual<long>();
    public static void ArrayEmpty() => Array.Empty<int>();
    public static void EnumerableEmpty() => Enumerable.Empty<string>();
    public static void OnXmlMake() => OnXml.Make<int>();
    public static void XmlHolderMake() => XmlHolder<int>.Make<long>();
}

public static class Program
{
    static MethodInfo Expected(string key)
    {
        switch (key)
        {
            case "Statics.Generic<int>": return typeof(Statics).GetMethod("Generic").MakeGenericMethod(typeof(int));
            case "Statics.Generic<string>": return typeof(Statics).GetMethod("Generic").MakeGenericMethod(typeof(string));
            case "Statics.Two<int,string>": return typeof(Statics).GetMethod("Two").MakeGenericMethod(typeof(int), typeof(string));
            case "Statics.Constrained<int>": return typeof(Statics).GetMethod("Constrained").MakeGenericMethod(typeof(int));
            case "Holder<int>.Convert<long>": return typeof(Holder<int>).GetMethod("Convert").MakeGenericMethod(typeof(long));
            case "Holder<int>.Convert<int>": return typeof(Holder<int>).GetMethod("Convert").MakeGenericMethod(typeof(int));
            case "Holder<string>.Convert<string>": return typeof(Holder<string>).GetMethod("Convert").MakeGenericMethod(typeof(string));
            case "Holder<int>.Make<long>": return typeof(Holder<int>).GetMethod("Make").MakeGenericMethod(typeof(long));
            case "Holder<int>.Make<string>": return typeof(Holder<int>).GetMethod("Make").MakeGenericMethod(typeof(string));
            case "SBox<int>.GenericVirtual<long>": return typeof(SBox<int>).GetMethod("GenericVirtual").MakeGenericMethod(typeof(long));
            case "SBox<int>.GenericNonVirtual<long>": return typeof(SBox<int>).GetMethod("GenericNonVirtual").MakeGenericMethod(typeof(long));
            case "Plain.GenericVirtual<long>": return typeof(Plain).GetMethod("GenericVirtual").MakeGenericMethod(typeof(long));
            case "Array.Empty<int>": return typeof(Array).GetMethod("Empty").MakeGenericMethod(typeof(int));
            case "Enumerable.Empty<string>": return typeof(Enumerable).GetMethod("Empty").MakeGenericMethod(typeof(string));
            case "OnXml.Make<int>": return typeof(OnXml).GetMethod("Make").MakeGenericMethod(typeof(int));
            case "XmlHolder<int>.Make<long>": return typeof(XmlHolder<int>).GetMethod("Make").MakeGenericMethod(typeof(long));
            default: throw new Exception("test bug: no expected method " + key);
        }
    }

    static string Describe(Exception e) =>
        e.GetType().Name + (e.InnerException == null ? "" : "/" + Describe(e.InnerException));

    static bool Faithful(Exception e)
    {
        if (e is BadImageFormatException
            && (e.HResult != unchecked((int)0x8007000B)
                || e.Message != "An attempt was made to load a program with an incorrect format.\n (0x8007000B)"))
            return false;
        return e.InnerException == null || Faithful(e.InnerException);
    }

    static bool Answers(int token, string context, string expected)
    {
        Type[] typeArgs = context.Contains("t=int") ? new[] { typeof(int) } : null;
        Type[] methodArgs =
            context.Contains("m=int") ? new[] { typeof(int) }
            : context.Contains("m=string") ? new[] { typeof(string) }
            : null;

        MethodBase m;
        try
        {
            m = typeof(Program).Module.ResolveMethod(token, typeArgs, methodArgs);
        }
        catch (Exception e)
        {
            return expected == "!" + Describe(e) && Faithful(e);
        }

        if (expected.StartsWith("!"))
            return false;

        MethodInfo made = Expected(expected);
        return ReferenceEquals(m, made)
            && m.MethodHandle == made.MethodHandle
            && m.Equals(made)
            && m.IsGenericMethod
            && !m.IsGenericMethodDefinition;
    }

    public static int Main(string[] args)
    {
        for (int i = 0; 3 * i < args.Length; i++)
        {
            if (!Answers(int.Parse(args[3 * i]), args[3 * i + 1], args[3 * i + 2]))
                return i + 1;
        }

        return 0;
    }
}
"""

    let private image : Lazy<byte[]> = lazy (Roslyn.compile [ source ])

    /// The token of the one MethodSpec that the method `methodName` on the type `typeName` calls.
    let private methodSpecToken (image : byte[]) (typeName : string) (methodName : string) : int =
        use peReader = new PEReader (new MemoryStream (image))
        let reader = peReader.GetMetadataReader ()

        let tokens =
            [
                for typeHandle in reader.TypeDefinitions do
                    let typeDef = reader.GetTypeDefinition typeHandle

                    if reader.GetString typeDef.Name = typeName then
                        for methodHandle in typeDef.GetMethods () do
                            let methodDef = reader.GetMethodDefinition methodHandle

                            if reader.GetString methodDef.Name = methodName then
                                let il = (peReader.GetMethodBody methodDef.RelativeVirtualAddress).GetILBytes ()

                                // A `call` or `callvirt` whose operand is a MethodSpec token (table 0x2B).
                                for offset in 0 .. il.Length - 5 do
                                    if (il.[offset] = 0x28uy || il.[offset] = 0x6Fuy) && il.[offset + 4] = 0x2Buy then
                                        yield System.BitConverter.ToInt32 (il, offset + 1)
            ]
            |> List.distinct

        match tokens with
        | [ token ] -> token
        | tokens -> failwith $"test bug: %s{typeName}.%s{methodName} calls %d{tokens.Length} MethodSpecs"

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

    /// The cases PawPrint answers: the type and method whose MethodSpec is resolved, the context it
    /// is resolved in, and the method the real runtime was measured to answer with.
    let private answered : (string * string * string * string) list =
        [
            "Sites", "GenericOfInt", "none", "Statics.Generic<int>"
            // The context is not consulted when the spec's arguments need none.
            "Sites", "GenericOfInt", "m=string", "Statics.Generic<int>"
            "Sites", "GenericOfString", "none", "Statics.Generic<string>"
            "Sites", "Two", "none", "Statics.Two<int,string>"
            "Statics", "Caller", "m=int", "Statics.Generic<int>"
            "Statics", "Caller", "m=string", "Statics.Generic<string>"
            "Statics", "CallerS", "m=int", "Statics.Constrained<int>"
            "Sites", "HolderConvert", "none", "Holder<int>.Convert<long>"
            "Sites", "HolderOfStringConvert", "none", "Holder<string>.Convert<string>"
            "Sites", "HolderMake", "none", "Holder<int>.Make<long>"
            "Holder`1", "InContext", "t=int", "Holder<int>.Convert<int>"
            "Holder`1", "ParentInContext", "t=int", "Holder<int>.Convert<int>"
            "Holder`1", "MakeInMethod", "t=int m=string", "Holder<int>.Make<string>"
            "Sites", "SBoxGenericVirtual", "none", "SBox<int>.GenericVirtual<long>"
            "Sites", "SBoxGenericNonVirtual", "none", "SBox<int>.GenericNonVirtual<long>"
            "Sites", "PlainGenericVirtual", "none", "Plain.GenericVirtual<long>"
            "Sites", "ArrayEmpty", "none", "Array.Empty<int>"
            "Sites", "EnumerableEmpty", "none", "Enumerable.Empty<string>"
            // The spec's argument, `!!0`, needs a method context.
            "Statics", "Caller", "none", "!ArgumentException/BadImageFormatException"
            // The spec's argument, `!0`, needs a type context, which a method context does not give.
            "Holder`1", "InContext", "none", "!ArgumentException/BadImageFormatException"
            "Holder`1", "InContext", "m=int", "!ArgumentException/BadImageFormatException"
            // The spec's argument is closed, but its MemberRef's parent, `Holder<!0>`, needs a type
            // context.
            "Holder`1", "ParentInContext", "none", "!ArgumentException/BadImageFormatException"
            // Arguments are read before constraints are checked.
            "Statics", "CallerS", "none", "!ArgumentException/BadImageFormatException"
        ]

    /// Methods on classes whose base is in an assembly nothing else loads, each resolved by a run of
    /// its own so that the resolution is the first thing to touch that assembly, and the method the
    /// real runtime was measured to answer with: a MethodSpec over a MethodDef, and one over a
    /// MemberRef.
    let private firstTouch : (string * string * string * string) list =
        [
            "Sites", "OnXmlMake", "none", "OnXml.Make<int>"
            "Sites", "XmlHolderMake", "none", "XmlHolder<int>.Make<long>"
        ]

    /// The cases PawPrint refuses, the exception the real runtime was measured to raise, and text
    /// PawPrint's refusal must contain.
    let private refused : (string * string * string * string * string) list =
        [ "Statics", "CallerS", "m=string", "!VerificationException", "violates" ]

    let private checkArgs (cases : (string * string * string * string) list) : string list =
        [
            for typeName, methodName, context, expected in cases do
                yield string (methodSpecToken (image.Force ()) typeName methodName)
                yield context
                yield expected
        ]

    let private refusedCase (index : int) : string * string * string * string =
        let typeName, methodName, context, expected, _ = refused.[index]
        typeName, methodName, context, expected

    [<Test>]
    let ``The real runtime answers as measured`` () : unit =
        let cases = answered @ firstTouch @ List.init refused.Length refusedCase

        match realRuntime (checkArgs cases) with
        | 0 -> ()
        | failing ->
            let typeName, methodName, context, measured = cases.[failing - 1]

            failwith
                $"%s{typeName}.%s{methodName}'s MethodSpec in context %s{context}: measured %s{measured}, but the real runtime now answers otherwise"

    [<Test>]
    let ``PawPrint resolves a MethodSpec to the method CoreCLR does`` () : unit =
        match underPawPrint "ModuleResolveMethodSpec" (checkArgs answered) with
        | RunOutcome.NormalExit (terminalState, _, _)
        | RunOutcome.ProcessExit (terminalState, _, _) ->
            match terminalState.LatchedExitCode with
            | 0 -> ()
            | failing ->
                let typeName, methodName, context, expected = answered.[failing - 1]

                failwith
                    $"%s{typeName}.%s{methodName}'s MethodSpec in context %s{context}: the real runtime answers %s{expected}; PawPrint does not"
        | other -> failwith $"expected the guest to exit, got %A{other}"

    [<TestCase 0>]
    let ``PawPrint refuses a MethodSpec that CoreCLR rejects`` (index : int) : unit =
        let typeName, methodName, _, expected, refusal = refused.[index]
        let token = methodSpecToken (image.Force ()) typeName methodName

        let exn =
            Assert.Catch (fun () ->
                underPawPrint $"ModuleResolveMethodSpecRefused%d{index}" (checkArgs [ refusedCase index ])
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "ModuleHandle.ResolveMethod"
        exn.Message |> shouldContainText (expected.TrimStart '!')
        exn.Message |> shouldContainText refusal
        exn.Message |> shouldContainText $"0x%08x{token}"

    [<TestCase 0>]
    [<TestCase 1>]
    let ``PawPrint resolves a MethodSpec on a class whose base is in an unloaded assembly`` (index : int) : unit =
        let typeName, methodName, context, expected = firstTouch.[index]

        match underPawPrint $"ModuleResolveMethodSpecFirstTouch%d{index}" (checkArgs [ firstTouch.[index] ]) with
        | RunOutcome.NormalExit (terminalState, _, _)
        | RunOutcome.ProcessExit (terminalState, _, _) ->
            if terminalState.LatchedExitCode <> 0 then
                failwith
                    $"%s{typeName}.%s{methodName}'s MethodSpec in context %s{context}: the real runtime answers %s{expected}; PawPrint does not"
        | other -> failwith $"expected the guest to exit, got %A{other}"
