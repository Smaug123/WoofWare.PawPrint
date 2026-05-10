namespace WoofWare.PawPrint.Test

open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Verifies that <c>MethodInfo.read</c> classifies <c>[UnsafeAccessor]</c>
/// <c>extern static</c> methods as <c>RuntimeProvided (UnsafeAccessor ...)</c>
/// rather than failing the RVA=0-no-flags hardening. The C# 12+ attribute
/// produces a metadata shape (RVA=0, ImplAttributes=IL, no PinvokeImpl/Abstract
/// flags) that ECMA-335 II.22.26 does not list, but which the runtime
/// recognises and synthesises the body for.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeAccessorRead =

    let private compileWithAccessors () : DumpedAssembly =
        let source =
            """
using System.Runtime.CompilerServices;

namespace UnsafeAccessorTest
{
    public class Container
    {
        [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "TargetStatic")]
        public static extern int CallStatic(object obj);

        [UnsafeAccessor(UnsafeAccessorKind.Method)]
        public static extern void CallInstance(object obj);

        [UnsafeAccessor(UnsafeAccessorKind.Field, Name = "_field")]
        public static extern ref int GetField(object obj);
    }
}
"""

        let bytes =
            Roslyn.compileAssembly
                "UnsafeAccessorReadTest"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        TypeIdentityTestHelpers.dumpedAssembly None bytes

    let private findMethod (assy : DumpedAssembly) (methodName : string) =
        let ty =
            TypeIdentityTestHelpers.getTopLevelTypeDef assy "UnsafeAccessorTest" "Container"

        ty.Methods
        |> List.tryFind (fun m -> m.Name = methodName)
        |> Option.defaultWith (fun () -> failwithf "Missing method %s" methodName)

    [<Test>]
    let ``StaticMethod kind with explicit Name is recognised`` () =
        let assy = compileWithAccessors ()
        let method = findMethod assy "CallStatic"

        match method.Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, name)) ->
            kind |> shouldEqual UnsafeAccessorKind.StaticMethod
            name |> shouldEqual (Some "TargetStatic")
        | other -> failwithf "Expected UnsafeAccessor (StaticMethod, Some \"TargetStatic\") but got %A" other

    [<Test>]
    let ``Method kind without Name defaults to None`` () =
        let assy = compileWithAccessors ()
        let method = findMethod assy "CallInstance"

        match method.Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, name)) ->
            kind |> shouldEqual UnsafeAccessorKind.Method
            name |> shouldEqual None
        | other -> failwithf "Expected UnsafeAccessor (Method, None) but got %A" other

    [<Test>]
    let ``Field kind with explicit Name is recognised`` () =
        let assy = compileWithAccessors ()
        let method = findMethod assy "GetField"

        match method.Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, name)) ->
            kind |> shouldEqual UnsafeAccessorKind.Field
            name |> shouldEqual (Some "_field")
        | other -> failwithf "Expected UnsafeAccessor (Field, Some \"_field\") but got %A" other
