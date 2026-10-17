namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `ThreadStartCallback` enters a started thread through CoreLib's `Thread::StartCallback` only
/// when its signature is one of the `StartCallbackShape`s, and refuses every other signature.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestStartCallbackShape =

    /// The `System.Threading.Thread` that `classify` is told is CoreLib's own.
    let private threadIdentity : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofDefinitionInAssembly "FabricatedCorelib" (MetadataTokens.TypeDefinitionHandle 2)

    /// Another type in the same image.
    let private otherIdentity : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofDefinitionInAssembly "FabricatedCorelib" (MetadataTokens.TypeDefinitionHandle 3)

    let private threadPtr : TypeDefn =
        TypeDefn.Pointer (TypeDefn.FromDefinition (threadIdentity, SignatureTypeKind.Class))

    /// Parameter types to build signatures from: `Thread*`, and near misses of it.
    let private parameterAlphabet : TypeDefn list =
        [
            threadPtr
            TypeDefn.Pointer (TypeDefn.FromDefinition (otherIdentity, SignatureTypeKind.Class))
            TypeDefn.Pointer (TypeDefn.FromDefinition (threadIdentity, SignatureTypeKind.ValueType))
            TypeDefn.FromDefinition (threadIdentity, SignatureTypeKind.Class)
            TypeDefn.Byref (TypeDefn.FromDefinition (threadIdentity, SignatureTypeKind.Class))
            TypeDefn.PrimitiveType PrimitiveType.IntPtr
        ]

    let private staticHeader : SignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)

    let private instanceHeader : SignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.Instance)

    /// Headers with the generic parameter count a decoder would pair with each.
    let private headers : (SignatureHeader * int) list =
        [
            staticHeader, 0
            instanceHeader, 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.VarArgs, SignatureAttributes.None), 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Unmanaged, SignatureAttributes.None), 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.Generic), 1
            SignatureHeader (
                SignatureKind.Method,
                SignatureCallingConvention.Default,
                SignatureAttributes.Instance ||| SignatureAttributes.Generic
            ),
            1
        ]

    let private returnTypes : MethodReturnType<TypeDefn> list =
        [
            MethodReturnType.Void
            MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32)
        ]

    let private parameterLists : TypeDefn list list =
        [
            yield []
            for p in parameterAlphabet do
                yield [ p ]

                for q in parameterAlphabet do
                    yield [ p ; q ]
        ]

    [<Test>]
    let ``classify recognises exactly net10's and net11's signatures`` () =
        let mutable recognised = 0

        for (header, genericCount) in headers do
            for ret in returnTypes do
                for parameters in parameterLists do
                    let signature : TypeMethodSignature<TypeDefn> =
                        {
                            Header = ComparableSignatureHeader.Make header
                            ParameterTypes = parameters
                            GenericParameterCount = genericCount
                            RequiredParameterCount = parameters.Length
                            ReturnType = ret
                        }

                    let expected =
                        if ret <> MethodReturnType.Void then
                            None
                        elif header = instanceHeader && parameters.IsEmpty then
                            Some StartCallbackShape.Instance
                        elif header = staticHeader && parameters = [ threadPtr ] then
                            Some StartCallbackShape.ThreadPointer
                        else
                            None

                    let actual = StartCallbackShape.classify threadIdentity signature

                    if actual <> expected then
                        failwith $"classify %O{signature} gave %O{actual}; expected %O{expected}"

                    if actual.IsSome then
                        recognised <- recognised + 1

        // Vacuity guard: the enumeration really does contain both accepted signatures.
        recognised |> shouldEqual 2

    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    [<TestCaseSource(nameof runtimes)>]
    let ``each runtime's CoreLib declares that runtime's StartCallback`` (runtime : EmulatedRuntime) =
        let corelibPath = FrameworkUnderTest.requireCoreLibPathOf runtime
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use stream = File.OpenRead corelibPath
        let corelib = Assembly.read loggerFactory (Some corelibPath) stream

        let expected =
            match runtime with
            | EmulatedRuntime.Net10 -> StartCallbackShape.Instance
            | EmulatedRuntime.Net11 -> StartCallbackShape.ThreadPointer

        let startCallback, shape = ThreadStartCallback.locate corelib
        shape |> shouldEqual expected
        startCallback.Name |> shouldEqual "StartCallback"

    /// An image declaring `System.Threading.Thread` with the given members, standing in for a
    /// CoreLib.
    let private fabricateCorelib (members : string) : DumpedAssembly =
        let source =
            $$"""
namespace System.Threading
{
    public class Thread
    {
        {{members}}
    }
}
"""

        let image =
            Roslyn.compileAssembly "FabricatedCorelib" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        Assembly.read loggerFactory (Some "FabricatedCorelib") (new MemoryStream (image))

    [<Test>]
    let ``a fabricated net11 StartCallback is recognised`` () =
        // Control for the refusals below: the fabrication is read the way the real CoreLib is.
        let corelib =
            fabricateCorelib "private static unsafe void StartCallback(Thread* pThread) {}"

        ThreadStartCallback.locate corelib
        |> snd
        |> shouldEqual StartCallbackShape.ThreadPointer

    [<TestCase("private void StartCallback(int x) {}")>]
    [<TestCase("private static unsafe void StartCallback(object* pThread) {}")>]
    [<TestCase("private static void StartCallback() {}")>]
    let ``a StartCallback of another signature is refused, naming the known ones`` (members : string) =
        let corelib = fabricateCorelib members

        let e =
            Assert.Throws<Exception> (fun () -> ThreadStartCallback.locate corelib |> ignore)

        e.Message |> shouldContainText "not one PawPrint knows how to call"
        e.Message |> shouldContainText StartCallbackShape.describeKnown

    [<Test>]
    let ``a CoreLib with no StartCallback is refused`` () =
        let corelib = fabricateCorelib "private void Other() {}"

        let e =
            Assert.Throws<Exception> (fun () -> ThreadStartCallback.locate corelib |> ignore)

        e.Message
        |> shouldContainText "Could not find System.Threading.Thread::StartCallback"
