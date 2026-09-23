namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection.Metadata
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint

/// `AppContextSeed` calls CoreLib's `System.AppContext::Setup` only when its signature is one of
/// the `SetupShape`s, and refuses every other signature by name.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSetupShape =

    let private charPtrPtr : TypeDefn =
        TypeDefn.Pointer (TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.Char))

    let private int32 : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Int32

    /// Parameter types to build signatures from: the three `Setup` has, and near misses of each.
    let private parameterAlphabet : TypeDefn list =
        [
            charPtrPtr
            int32
            TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.Char)
            TypeDefn.Pointer (TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.Byte))
            TypeDefn.PrimitiveType PrimitiveType.Int64
            TypeDefn.Pointer (TypeDefn.PrimitiveType PrimitiveType.UInt32)
        ]

    let private staticHeader : SignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)

    /// Headers with the generic parameter count a decoder would pair with each.
    let private headers : (SignatureHeader * int) list =
        [
            staticHeader, 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.Instance), 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.VarArgs, SignatureAttributes.None), 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Unmanaged, SignatureAttributes.None), 0
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.Generic), 1
        ]

    let private returnTypes : MethodReturnType<TypeDefn> list =
        [ MethodReturnType.Void ; MethodReturnType.Returns int32 ]

    let rec private parameterLists (maxLength : int) : TypeDefn list list =
        if maxLength = 0 then
            [ [] ]
        else
            let shorter = parameterLists (maxLength - 1)

            let longest =
                shorter
                |> List.filter (fun l -> l.Length = maxLength - 1)
                |> List.collect (fun l -> parameterAlphabet |> List.map (fun p -> l @ [ p ]))

            shorter @ longest

    [<Test>]
    let ``classify recognises exactly net10's three-argument signature`` () =
        // Every signature over the alphabet with up to five parameters: the net10 shape, the
        // net11 and Mono arities, and every one-type-off near miss of each position.
        let mutable recognised = 0

        for (header, genericCount) in headers do
            for ret in returnTypes do
                for parameters in parameterLists 5 do
                    let signature : TypeMethodSignature<TypeDefn> =
                        {
                            Header = ComparableSignatureHeader.Make header
                            ParameterTypes = parameters
                            GenericParameterCount = genericCount
                            RequiredParameterCount = parameters.Length
                            ReturnType = ret
                        }

                    let expected =
                        if
                            header = staticHeader
                            && ret = MethodReturnType.Void
                            && parameters = [ charPtrPtr ; charPtrPtr ; int32 ]
                        then
                            Some SetupShape.ThreeArg
                        else
                            None

                    let actual = SetupShape.classify signature

                    if actual <> expected then
                        failwith $"classify %O{signature} gave %O{actual}; expected %O{expected}"

                    if actual.IsSome then
                        recognised <- recognised + 1

        // Vacuity guard: the enumeration really does contain the accepted signature.
        recognised |> shouldEqual 1

    let private loadImage (name : string) (image : byte[]) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        Assembly.read loggerFactory (Some name) (new MemoryStream (image))

    /// An image declaring `System.AppContext` with the given members, standing in for a CoreLib.
    let private fabricateCorelib (members : string) : DumpedAssembly =
        let source =
            $$"""
using System;
using System.Runtime.InteropServices;

namespace System
{
    public class AppContext
    {
        {{members}}
    }
}
"""

        let image =
            Roslyn.compileAssembly "FabricatedCorelib" OutputKind.DynamicallyLinkedLibrary [] [ source ]

        loadImage "FabricatedCorelib" image

    let private refusal (corelib : DumpedAssembly) : string =
        let e =
            Assert.Throws<Exception> (fun () -> AppContextSeed.locateSetup corelib |> ignore)

        e.Message

    [<Test>]
    let ``the host's CoreLib declares the three-argument Setup`` () =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use _loggerFactoryResource = loggerFactory
        use stream = File.OpenRead corelibPath
        let corelib = Assembly.read loggerFactory (Some corelibPath) stream

        let setup, shape = AppContextSeed.locateSetup corelib
        shape |> shouldEqual SetupShape.ThreeArg
        setup.Name |> shouldEqual "Setup"

    [<Test>]
    let ``a fabricated net10 Setup is recognised`` () =
        // Control for the refusals below: the fabrication is read the way the real CoreLib is.
        let corelib =
            fabricateCorelib "internal static unsafe void Setup(char** pNames, char** pValues, int count) {}"

        let _, shape = AppContextSeed.locateSetup corelib
        shape |> shouldEqual SetupShape.ThreeArg

    [<Test>]
    let ``net11's four-argument Setup is refused, naming its signature`` () =
        let corelib =
            fabricateCorelib
                "[UnmanagedCallersOnly] internal static unsafe void Setup(char** pNames, char** pValues, int count, Exception* pException) {}"

        let message = refusal corelib

        message
        |> shouldContainText
            "has the signature `Default Setup(ptr[ptr[char]], ptr[ptr[char]], int32, ptr[ref[System.Exception]]) : void`"

    [<Test>]
    let ``Mono's five-argument Setup is refused, naming its signature`` () =
        let corelib =
            fabricateCorelib
                "internal static unsafe void Setup(char** pNames, uint* pNameLengths, char** pValues, uint* pValueLengths, int count) {}"

        refusal corelib
        |> shouldContainText
            "has the signature `Default Setup(ptr[ptr[char]], ptr[uint32], ptr[ptr[char]], ptr[uint32], int32) : void`"

    [<Test>]
    let ``a three-argument Setup with other parameter types is refused`` () =
        let corelib =
            fabricateCorelib "internal static unsafe void Setup(char* pNames, char* pValues, int count) {}"

        refusal corelib
        |> shouldContainText "has the signature `Default Setup(ptr[char], ptr[char], int32) : void`"

    [<Test>]
    let ``a three-argument Setup returning a value is refused`` () =
        let corelib =
            fabricateCorelib "internal static unsafe int Setup(char** pNames, char** pValues, int count) => 0;"

        refusal corelib
        |> shouldContainText "has the signature `Default Setup(ptr[ptr[char]], ptr[ptr[char]], int32) : int32`"

    [<Test>]
    let ``several static Setups are refused, naming each`` () =
        let corelib =
            fabricateCorelib
                """
                internal static unsafe void Setup(char** pNames, char** pValues, int count) {}
                internal static unsafe void Setup(char** pNames, char** pValues, int count, Exception* pException) {}
                """

        let message = refusal corelib

        message
        |> shouldContainText "Found several static System.AppContext::Setup methods"

        message
        |> shouldContainText "`Default Setup(ptr[ptr[char]], ptr[ptr[char]], int32) : void`"

        message
        |> shouldContainText "`Default Setup(ptr[ptr[char]], ptr[ptr[char]], int32, ptr[ref[System.Exception]]) : void`"

    [<Test>]
    let ``an instance Setup is not the host's entry point`` () =
        let corelib =
            fabricateCorelib "internal unsafe void Setup(char** pNames, char** pValues, int count) {}"

        refusal corelib
        |> shouldContainText "Could not find a static System.AppContext::Setup in CoreLib"

    [<Test>]
    let ``an AppContext with no static Setup is refused`` () =
        let corelib = fabricateCorelib "internal static void NotSetup() {}"

        refusal corelib
        |> shouldContainText "Could not find a static System.AppContext::Setup in CoreLib"
