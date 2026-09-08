namespace WoofWare.PawPrint.Test

open NUnit.Framework
open FsUnitTyped
open WoofWare.PawPrint

/// `IntrinsicMethodKey` identifies a method by assembly, declaring type, name, parameter shapes and
/// return shape. The return shape is there because CoreLib's conversion operators are an overload
/// set that differs in nothing else: `System.Int128` declares sixteen `op_Explicit` overloads that
/// each take a single `System.Int128`, eleven of them a one-instruction field read and the rest real
/// conversion algorithms. An allowlist that could not tell them apart would admit all sixteen on the
/// strength of having reviewed one.
[<TestFixture>]
module TestIntrinsicReturnShape =

    let private corelib =
        "System.Private.CoreLib, Version=10.0.0.0, Culture=neutral, PublicKeyToken=7cec85d7bea7798e"

    let private key
        (declaringTypeFullName : string)
        (methodName : string)
        (parameterShapes : string list)
        (returns : MethodReturnType<string>)
        : IntrinsicMethodKeys.IntrinsicMethodKey
        =
        {
            DeclaringAssemblyFullName = corelib
            DeclaringTypeFullName = declaringTypeFullName
            MethodName = methodName
            ParameterShapes = parameterShapes
            ReturnShape = returns
        }

    let private int128Explicit (returns : MethodReturnType<string>) : IntrinsicMethodKeys.IntrinsicMethodKey =
        key "System.Int128" "op_Explicit" [ "System.Int128" ] returns

    [<Test>]
    let ``the reviewed Int128 narrowing to Int64 is allowlisted`` () =
        int128Explicit (MethodReturnType.Returns "System.Int64")
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual true

    /// The five `op_Explicit` overloads whose bodies are not a field read. Each is identical to the
    /// allowlisted one in every component of the key except the return shape, so each of these is a
    /// direct check that the discriminator is what is doing the work.
    [<TestCase "System.Double">]
    [<TestCase "System.Single">]
    [<TestCase "System.Half">]
    [<TestCase "System.Decimal">]
    [<TestCase "System.UInt128">]
    let ``an unreviewed Int128 conversion is not allowlisted by the Int64 entry`` (returns : string) =
        int128Explicit (MethodReturnType.Returns returns)
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual false

    /// The other ten truncating overloads are not allowlisted either. They have the same body shape
    /// as the `Int64` one, but "same shape" is not "reviewed", and nothing exercises them.
    [<TestCase "System.Int32">]
    [<TestCase "System.UInt64">]
    [<TestCase "System.Byte">]
    [<TestCase "System.Char">]
    [<TestCase "System.IntPtr">]
    let ``a truncating Int128 conversion that was not reviewed is not allowlisted`` (returns : string) =
        int128Explicit (MethodReturnType.Returns returns)
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual false

    [<Test>]
    let ``a void return does not satisfy a pattern demanding Int64`` () =
        int128Explicit MethodReturnType.Void
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual false

    /// Almost every entry in the allowlist constrains nothing about the return shape, because an
    /// overload set that already differs in its parameters is fully identified without it. Those
    /// entries have to keep matching whatever the return column says -- this is the regression the
    /// new field could most easily have caused.
    [<TestCase "System.Boolean">]
    [<TestCase "System.Int32">]
    [<TestCase "&">]
    let ``an entry with no return pattern matches regardless of the return shape`` (returns : string) =
        key "System.Int128" "op_Equality" [ "System.Int128" ; "System.Int128" ] (MethodReturnType.Returns returns)
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual true

    [<Test>]
    let ``an entry with no return pattern matches a void return too`` () =
        key "System.Int128" ".ctor" [ "System.UInt64" ; "System.UInt64" ] MethodReturnType.Void
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual true

    /// The return shape does not rescue a key that fails on any other component, so the new
    /// conjunct cannot have weakened the match.
    [<Test>]
    let ``the right return shape does not admit a wrong parameter list`` () =
        key "System.Int128" "op_Explicit" [ "System.UInt128" ] (MethodReturnType.Returns "System.Int64")
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual false

    [<Test>]
    let ``the right return shape does not admit a wrong declaring type`` () =
        key "System.UInt128" "op_Explicit" [ "System.Int128" ] (MethodReturnType.Returns "System.Int64")
        |> Intrinsics.isSafeIntrinsic
        |> shouldEqual false

    [<Test>]
    let ``the formatted key names the return shape`` () =
        // The unimplemented-intrinsic message is what a reader uses to find the method that stopped
        // a guest. Without the return shape, all sixteen `op_Explicit` overloads print identically,
        // which is precisely the confusion the key now avoids.
        int128Explicit (MethodReturnType.Returns "System.Double")
        |> Intrinsics.formatMethodKey
        |> shouldEqual "System.Private.CoreLib System.Int128.op_Explicit(System.Int128) : System.Double"

    [<Test>]
    let ``the formatted key spells a void return`` () =
        key "System.Int128" ".ctor" [ "System.UInt64" ; "System.UInt64" ] MethodReturnType.Void
        |> Intrinsics.formatMethodKey
        |> shouldEqual "System.Private.CoreLib System.Int128..ctor(System.UInt64, System.UInt64) : void"
