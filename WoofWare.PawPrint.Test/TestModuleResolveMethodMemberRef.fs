namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// <summary>
/// <c>Module.ResolveMethod</c> on a MemberRef token, against the real runtime. CoreCLR
/// (runtimehandles.cpp, <c>ModuleHandle_ResolveMethod</c>; memberload.cpp,
/// <c>GetMethodDescFromMemberDefOrRefOrSpec</c>) substitutes the caller's type and method
/// instantiations into the MemberRef's parent and signature only, and then instantiates the method
/// over its own formal parameters: a non-generic method comes back as itself whatever the caller
/// passed, and a generic one as its definition on the parent's instantiation, never instantiated
/// over the caller's method arguments.
/// </summary>
/// <remarks>
/// Where the parent's instantiation is not its own canonical form (<c>Holder&lt;string&gt;</c>,
/// whose code CoreCLR shares as <c>Holder&lt;__Canon&gt;</c>), CoreCLR's answer for a generic method
/// is not the definition: reflection reports it generic, over its own parameter, but not a generic
/// method definition. PawPrint does not model canonical forms, and refuses those. Nor is the answer
/// the definition for a generic virtual method on a value type, even one that is its own canonical
/// form (<c>SBox&lt;int&gt;</c>), where CoreCLR's method table holds the method's unboxing stub;
/// PawPrint refuses those too.
/// </remarks>
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestModuleResolveMethodMemberRef =

    /// Each method the guest references is referenced once, by one MemberRef, so that its name finds
    /// the MemberRef's token. `Main` resolves each token it is given and classifies the answer: in
    /// `classify` mode it returns the one token's classification, and in `check` mode it returns 0 if
    /// every token's is the one given beside it, or one more than the index of the first that is not.
    let private source =
        """
using System;
using System.Reflection;

public interface IConvertOnClass { U ConvertVirtualOnClass<U>(); }

public interface IConvertOnStruct
{
    U ConvertVirtual<U>();
    void PlainVirtual();
}

public class Holder<T> : IConvertOnClass
{
    public U ConvertVirtualOnClass<U>() => default!;
    public U ConvertOnInt<U>(T v) => default!;
    public void PlainOnInt() { }
    public U ConvertOnString<U>(T v) => default!;
    public U ConvertInContext<U>(T v) => default!;
    public void PlainInContext() { }
}

public struct SBox<T> : IConvertOnStruct
{
    public U ConvertOnStruct<U>(T v) => default!;
    public U ConvertVirtual<U>() => default!;
    public void PlainVirtual() { }
}

public static class Program
{
    // Never run: these are here for the MemberRefs they make.
    static void References()
    {
        new Holder<int>().ConvertOnInt<string>(1);
        new Holder<int>().PlainOnInt();
        new Holder<string>().ConvertOnString<int>("");
        new SBox<int>().ConvertOnStruct<int>(1);
        new Holder<int>().ConvertVirtualOnClass<int>();
        new SBox<int>().ConvertVirtual<int>();
        new SBox<int>().PlainVirtual();
        InGeneric<long>();
    }

    static void InGeneric<V>()
    {
        new Holder<V>().ConvertInContext<V>(default!);
        new Holder<V>().PlainInContext();
    }

    static Type[] TypeContext(string context) =>
        context == "t=long,m=int" ? new[] { typeof(long) } : null;

    static Type[] MethodContext(string context) =>
        context switch
        {
            "none" => null,
            "m=int" => new[] { typeof(int) },
            "t=long,m=int" => new[] { typeof(int) },
            "m=string" => new[] { typeof(string) },
            _ => throw new ArgumentException(context),
        };

    static readonly Type[] DeclaringTypes = { typeof(Holder<int>), typeof(Holder<string>), typeof(SBox<int>) };

    static int Classify(int token, string context)
    {
        MethodBase m;
        try
        {
            m = typeof(Program).Module.ResolveMethod(token, TypeContext(context), MethodContext(context));
        }
        catch (ArgumentException e) when (e.InnerException is BadImageFormatException)
        {
            return 128;
        }

        int code = Array.IndexOf(DeclaringTypes, m.DeclaringType) + 1;
        if (m.IsGenericMethod) code |= 8;
        if (m is MethodInfo mi && mi.IsGenericMethodDefinition) code |= 16;
        if (m.IsGenericMethod && m.GetGenericArguments()[0].IsGenericParameter) code |= 32;
        if (ReferenceEquals(m, m.DeclaringType.GetMethod(m.Name))) code |= 64;
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

    /// The token of the one MemberRef named `name`.
    let private memberRefToken (image : byte[]) (name : string) : int =
        use peReader = new PEReader (new MemoryStream (image))
        let reader = peReader.GetMetadataReader ()

        let tokens =
            [
                for handle in reader.MemberReferences do
                    if reader.GetString (reader.GetMemberReference handle).Name = name then
                        yield MetadataTokens.GetToken (MemberReferenceHandle.op_Implicit handle : EntityHandle)
            ]

        match tokens with
        | [ token ] -> token
        | tokens -> failwith $"test bug: %d{tokens.Length} MemberRefs are named %s{name}"

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

    // Classifications, as `Classify` computes them: the declaring type's index in `DeclaringTypes`
    // plus one, then 8 for a generic method, 16 for a generic method definition, 32 for one over its
    // own parameter, and 64 for the very `MethodInfo` its declaring type's `GetMethod` returns.
    let private definitionOnHolderOfInt = 1 + 8 + 16 + 32 + 64
    let private plainOnHolderOfInt = 1 + 64

    /// The cases PawPrint answers: each MemberRef's name, the context it is resolved in, and what
    /// the real runtime was measured to answer.
    let private answered : (string * string * int) list =
        [
            "PlainOnInt", "none", plainOnHolderOfInt
            // The method context is not the method's instantiation.
            "PlainOnInt", "m=int", plainOnHolderOfInt
            "ConvertOnInt", "none", definitionOnHolderOfInt
            "ConvertOnInt", "m=int", definitionOnHolderOfInt
            "ConvertOnInt", "t=long,m=int", definitionOnHolderOfInt
            "ConvertOnStruct", "none", 3 + 8 + 16 + 32 + 64
            // The context substitutes into the parent, `Holder<!!0>`.
            "ConvertInContext", "m=int", definitionOnHolderOfInt
            "PlainInContext", "m=int", plainOnHolderOfInt
            // Virtual methods, generic on a class and non-generic on a struct.
            "ConvertVirtualOnClass", "none", definitionOnHolderOfInt
            "PlainVirtual", "none", 3 + 64
        ]

    /// The cases where CoreCLR's answer is on a shared canonical form, which PawPrint refuses.
    let private canonical : (string * string * int) list =
        [
            "ConvertOnString", "none", 2 + 8 + 32
            "ConvertInContext", "m=string", 2 + 8 + 32
        ]

    /// The case where CoreCLR's answer is a generic virtual method on a value type, which is not the
    /// definition even on a type that is its own canonical form, and which PawPrint refuses.
    let private genericVirtualOnStruct : string * string * int =
        "ConvertVirtual", "none", 3 + 8 + 32

    [<Test>]
    let ``The real runtime answers as measured`` () : unit =
        for name, context, measured in answered @ canonical @ [ genericVirtualOnStruct ] do
            let token = memberRefToken (image.Force ()) name
            let actual = realRuntime [ "classify" ; string token ; context ]

            if actual <> measured then
                failwith
                    $"%s{name} in context %s{context}: measured %d{measured}, but the real runtime now gives %d{actual}"

    let private checkArgs (cases : (string * string * int) list) : string list =
        [
            yield "check"
            for name, context, expected in cases do
                yield string (memberRefToken (image.Force ()) name)
                yield context
                yield string expected
        ]

    [<Test>]
    let ``PawPrint resolves a MemberRef to the method CoreCLR does`` () : unit =
        let args = checkArgs answered
        realRuntime args |> shouldEqual 0

        match underPawPrint "ModuleResolveMethodMemberRef" args with
        | RunOutcome.NormalExit (terminalState, _, _)
        | RunOutcome.ProcessExit (terminalState, _, _) ->
            match terminalState.LatchedExitCode with
            | 0 -> ()
            | failing ->
                let name, context, expected = answered.[failing - 1]

                failwith
                    $"%s{name} in context %s{context}: the real runtime classifies it %d{expected}; PawPrint does not"
        | other -> failwith $"expected the guest to exit, got %A{other}"

    [<TestCase 0>]
    [<TestCase 1>]
    let ``PawPrint refuses a generic method on a shared canonical form`` (index : int) : unit =
        let name, context, _ = canonical.[index]

        let exn =
            Assert.Catch (fun () ->
                underPawPrint $"ModuleResolveMethodMemberRefCanonical%d{index}" (checkArgs [ canonical.[index] ])
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "ModuleHandle.ResolveMethod"
        exn.Message |> shouldContainText "System.__Canon"
        exn.Message |> shouldContainText name

    [<Test>]
    let ``PawPrint refuses a generic virtual method on a value type`` () : unit =
        let name, _, _ = genericVirtualOnStruct

        let exn =
            Assert.Catch (fun () ->
                underPawPrint
                    "ModuleResolveMethodMemberRefGenericVirtualOnStruct"
                    (checkArgs [ genericVirtualOnStruct ])
                |> ignore<RunOutcome>
            )

        exn.Message |> shouldContainText "ModuleHandle.ResolveMethod"
        exn.Message |> shouldContainText "unboxing stub"
        exn.Message |> shouldContainText name
