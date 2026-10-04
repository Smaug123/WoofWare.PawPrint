namespace WoofWare.PawPrint.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// An `Assumption` summarises a CoreLib method by name and signature. These tests hold each to the
/// CoreLibs: the method must be there, as the contract describes it.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestAssumption =

    let coreLibs : TestCaseData list = TestIntrinsicBody.coreLibs

    [<TestCaseSource(nameof coreLibs)>]
    let ``every assumption summarises one method of the CoreLib, the one it names`` (which : string) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        let summarised =
            [
                for KeyValue (handle, method) in corelib.Methods do
                    match Assumption.summarises corelib handle with
                    | Some assumption -> yield assumption, method
                    | None -> ()
            ]

        summarised |> List.map fst |> Set.ofList |> shouldEqual Assumption.all
        summarised.Length |> shouldEqual Assumption.all.Count

        for assumption, method in summarised do
            match assumption with
            | Assumption.CoreLibResourceLookup ->
                let declaringType = corelib.TypeDefs.[method.RequiredDeclaringType.Definition.Get]

                $"%s{declaringType.Namespace}.%s{declaringType.Name}::%s{method.Name}"
                |> shouldEqual "System.SR::InternalGetResourceString"

                method.IsStatic |> shouldEqual true

                method.Signature.ParameterTypes
                |> shouldEqual [ TypeDefn.PrimitiveType PrimitiveType.String ]

                method.Signature.ReturnType
                |> shouldEqual (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.String))

            match method.Body with
            | MethodBody.Il _ -> ()
            | other -> failwith $"%A{assumption} summarises a method whose body is %A{other}, not IL"

    [<TestCaseSource(nameof coreLibs)>]
    let ``every exception an assumption's contract raises is a CoreLib exception type`` (which : string) : unit =
        let corelib = TestIntrinsicBody.coreLib which

        for assumption in Assumption.all do
            for name in Assumption.raises assumption do
                match corelib.TryGetTopLevelTypeDef name.Namespace name.Name with
                | Some _ -> ()
                | None -> failwith $"%A{assumption} raises %O{name}, which this CoreLib does not declare"

                // The host's CoreLib says what it derives from.
                let hostType = typeof<obj>.Assembly.GetType name.FullName

                if isNull hostType || not (typeof<Exception>.IsAssignableFrom hostType) then
                    failwith $"%A{assumption} raises %O{name}, which is not an exception type of the host's CoreLib"
