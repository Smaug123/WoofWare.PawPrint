namespace WoofWare.PawPrint.Test

open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestNativeKernel32 =
    let private errorEnvVarNotFound : int = 203

    type private EnvironmentVariableCase =
        {
            BufferSize : int
            Value : string option
        }

    let private genEnvironmentVariableCase : Gen<EnvironmentVariableCase> =
        let genValue =
            Gen.frequency
                [
                    1, Gen.constant None
                    4,
                    Gen.choose (0, 260)
                    |> Gen.map (fun length -> Some (System.String ('x', length)))
                ]

        gen {
            let! value = genValue

            let requiredSize =
                value |> Option.map (fun value -> value.Length + 1) |> Option.defaultValue 1

            let! bufferSize =
                Gen.frequency
                    [
                        1, Gen.constant 0
                        2, Gen.choose (0, max 0 (requiredSize - 1))
                        2, Gen.choose (requiredSize, requiredSize + 32)
                    ]

            return
                {
                    BufferSize = bufferSize
                    Value = value
                }
        }

    let private propertyConfig : Config = Config.QuickThrowOnFailure.WithMaxTest 500

    let private assertPlan
        (bufferSize : int)
        (value : string option)
        (expectedReturnLength : uint32)
        (expectedLastError : int)
        (expectedValueToWrite : string option)
        : unit
        =
        let actual = NativeKernel32.planGetEnvironmentVariableW bufferSize value

        actual.ReturnLength |> shouldEqual expectedReturnLength
        actual.LastError |> shouldEqual expectedLastError
        actual.ValueToWrite |> shouldEqual expectedValueToWrite

    [<Test>]
    let ``GetEnvironmentVariableW plan handles exact buffer edges`` () : unit =
        assertPlan 0 None 0u errorEnvVarNotFound None
        assertPlan 0 (Some "") 1u 0 None
        assertPlan 1 (Some "") 0u 0 (Some "")
        assertPlan 3 (Some "abc") 4u 0 None
        assertPlan 4 (Some "abc") 3u 0 (Some "abc")

    [<Test>]
    let ``GetEnvironmentVariableW plan matches Win32 buffer contract`` () : unit =
        let mutable missing = 0
        let mutable tooSmall = 0
        let mutable fits = 0

        let property (case : EnvironmentVariableCase) : unit =
            let actual = NativeKernel32.planGetEnvironmentVariableW case.BufferSize case.Value

            match case.Value with
            | None ->
                missing <- missing + 1
                actual.ReturnLength |> shouldEqual 0u
                actual.LastError |> shouldEqual errorEnvVarNotFound
                actual.ValueToWrite |> shouldEqual None
            | Some value ->
                let requiredSize = value.Length + 1

                if case.BufferSize < requiredSize then
                    tooSmall <- tooSmall + 1
                    actual.ReturnLength |> shouldEqual (uint32 requiredSize)
                    actual.LastError |> shouldEqual 0
                    actual.ValueToWrite |> shouldEqual None
                else
                    fits <- fits + 1
                    actual.ReturnLength |> shouldEqual (uint32 value.Length)
                    actual.LastError |> shouldEqual 0
                    actual.ValueToWrite |> shouldEqual (Some value)

        Check.One (propertyConfig, Prop.forAll (Arb.fromGen genEnvironmentVariableCase) property)

        missing > 20 |> shouldEqual true
        tooSmall > 50 |> shouldEqual true
        fits > 50 |> shouldEqual true
