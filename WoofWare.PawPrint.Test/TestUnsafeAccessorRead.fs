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

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = null)]
        public static extern void ExplicitlyNullName(object obj);

        [UnsafeAccessor(UnsafeAccessorKind.Method, Name = "")]
        public static extern void ExplicitlyEmptyName(object obj);
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
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, name, hasTypeNameOverrides)) ->
            kind |> shouldEqual UnsafeAccessorKind.StaticMethod
            name |> shouldEqual (Some "TargetStatic")
            hasTypeNameOverrides |> shouldEqual false
        | other -> failwithf "Expected UnsafeAccessor (StaticMethod, Some \"TargetStatic\") but got %A" other

    [<Test>]
    let ``Method kind without Name defaults to None`` () =
        let assy = compileWithAccessors ()
        let method = findMethod assy "CallInstance"

        match method.Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, name, hasTypeNameOverrides)) ->
            kind |> shouldEqual UnsafeAccessorKind.Method
            name |> shouldEqual None
            hasTypeNameOverrides |> shouldEqual false
        | other -> failwithf "Expected UnsafeAccessor (Method, None) but got %A" other

    [<Test>]
    let ``Field kind with explicit Name is recognised`` () =
        let assy = compileWithAccessors ()
        let method = findMethod assy "GetField"

        match method.Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (kind, name, hasTypeNameOverrides)) ->
            kind |> shouldEqual UnsafeAccessorKind.Field
            name |> shouldEqual (Some "_field")
            hasTypeNameOverrides |> shouldEqual false
        | other -> failwithf "Expected UnsafeAccessor (Field, Some \"_field\") but got %A" other

    /// A `[UnsafeAccessorType("...")]` on a parameter or on the return names the target type by
    /// assembly-qualified string, so the declaration's *signature* does not name the types dispatch
    /// must use. Every one of CoreLib's own `[UnsafeAccessor]` declarations is of that shape, so a
    /// dispatcher that could not tell the difference would resolve them against `System.Object`.
    let private compileWithTypeNameOverrides () : DumpedAssembly =
        let source =
            """
using System.Runtime.CompilerServices;

namespace UnsafeAccessorTest
{
    public class Overridden
    {
        [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Get")]
        public static extern int OnParameter([UnsafeAccessorType("Some.Other.Type, Some.Other.Assembly")] object obj);

        [UnsafeAccessor(UnsafeAccessorKind.Constructor)]
        [return: UnsafeAccessorType("Some.Other.Type, Some.Other.Assembly")]
        public static extern object OnReturn();

        [UnsafeAccessor(UnsafeAccessorKind.StaticMethod, Name = "Get")]
        public static extern int Plain(object obj);
    }
}
"""

        let bytes =
            Roslyn.compileAssembly
                "UnsafeAccessorTypeNameTest"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ source ]

        TypeIdentityTestHelpers.dumpedAssembly None bytes

    let private typeNameOverrideOf (assy : DumpedAssembly) (methodName : string) : bool =
        let ty =
            TypeIdentityTestHelpers.getTopLevelTypeDef assy "UnsafeAccessorTest" "Overridden"

        let method =
            ty.Methods
            |> List.tryFind (fun m -> m.Name = methodName)
            |> Option.defaultWith (fun () -> failwithf "Missing method %s" methodName)

        match method.Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (_, _, hasTypeNameOverrides)) ->
            hasTypeNameOverrides
        | other -> failwithf "Expected UnsafeAccessor but got %A" other

    [<Test>]
    let ``UnsafeAccessorType on a parameter is detected`` () =
        compileWithTypeNameOverrides ()
        |> fun assy -> typeNameOverrideOf assy "OnParameter" |> shouldEqual true

    [<Test>]
    let ``UnsafeAccessorType on the return is detected`` () =
        // The return's attributes live on the Param row with sequence number 0, which
        // `Parameter.readAll` drops; the scan must read the raw rows rather than that list.
        compileWithTypeNameOverrides ()
        |> fun assy -> typeNameOverrideOf assy "OnReturn" |> shouldEqual true

    [<Test>]
    let ``A declaration with no UnsafeAccessorType is not flagged`` () =
        compileWithTypeNameOverrides ()
        |> fun assy -> typeNameOverrideOf assy "Plain" |> shouldEqual false

    /// An explicitly supplied `Name = null` is not the same as an absent `Name`: CoreCLR's
    /// `TryParseUnsafeAccessorAttribute` keys the "use the attributed method's name" default off the
    /// named argument's *presence*, and copies a supplied value verbatim -- copying a null yields
    /// the empty string. So the two must not both read as `None`, or an explicitly-null accessor
    /// would silently bind its own name. `sourcesPure/UnsafeAccessorNameIsExplicitlyNull.cs` pins
    /// what the difference does at dispatch.
    [<Test>]
    let ``an explicitly null Name reads as the empty string, not as absent`` () =
        let assy = compileWithAccessors ()

        match (findMethod assy "ExplicitlyNullName").Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (_, name, _)) -> name |> shouldEqual (Some "")
        | other -> failwithf "Expected UnsafeAccessor but got %A" other

    [<Test>]
    let ``an explicitly empty Name reads as the empty string`` () =
        let assy = compileWithAccessors ()

        match (findMethod assy "ExplicitlyEmptyName").Body with
        | MethodBody.RuntimeProvided (RuntimeBehaviour.UnsafeAccessor (_, name, _)) -> name |> shouldEqual (Some "")
        | other -> failwithf "Expected UnsafeAccessor but got %A" other
