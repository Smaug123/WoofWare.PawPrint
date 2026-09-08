namespace WoofWare.PawPrint.Test

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The canonicalisation contract of `RuntimeTypeHandleTarget.composite` and `functionPointer`:
/// a shape over a closed element is the closed shape, so that `TypeHandleRegistry`, which keys
/// guest `Type` identity on the target, never holds two spellings of one type.
[<TestFixture>]
module TestRuntimeTypeHandleTarget =

    let private identity (row : int) : ResolvedTypeIdentity =
        ResolvedTypeIdentity.ofDefinitionInAssembly "Sample, Version=1.0.0.0" (MetadataTokens.TypeDefinitionHandle row)

    let private method (row : int) : ComparableMethodDefinitionHandle =
        ComparableMethodDefinitionHandle.Make (MetadataTokens.MethodDefinitionHandle row)

    let private header : ComparableSignatureHeader =
        SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
        |> ComparableSignatureHeader.Make

    let private signature (parameters : 'a list) (returnType : MethodReturnType<'a>) : TypeMethodSignature<'a> =
        {
            Header = header
            ParameterTypes = parameters
            GenericParameterCount = 0
            RequiredParameterCount = List.length parameters
            ReturnType = returnType
        }

    let private genShape : Gen<CompositeShape> =
        Gen.oneof
            [
                Gen.constant CompositeShape.Byref
                Gen.constant CompositeShape.Pointer
                Gen.constant CompositeShape.OneDimArrayZero
                Gen.map CompositeShape.Array (Gen.choose (1, 4))
            ]

    let rec private genConcrete (size : int) : Gen<ConcreteTypeHandle> =
        if size <= 0 then
            Gen.map ConcreteTypeHandle.Concrete (Gen.choose (0, 5))
        else
            Gen.oneof
                [
                    Gen.map ConcreteTypeHandle.Concrete (Gen.choose (0, 5))
                    Gen.map2 CompositeShape.applyConcrete genShape (genConcrete (size - 1))
                    gen {
                        let! pars = Gen.listOfLength 2 (genConcrete (size - 1))

                        let! ret =
                            Gen.frequency
                                [
                                    1, Gen.constant MethodReturnType.Void
                                    2, Gen.map MethodReturnType.Returns (genConcrete (size - 1))
                                ]

                        return ConcreteTypeHandle.FunctionPointer (signature pars ret)
                    }
                ]

    let private genVariable : Gen<RuntimeTypeHandleTarget> =
        Gen.oneof
            [
                Gen.map2
                    (fun row pos -> RuntimeTypeHandleTarget.GenericParameter (identity row, pos))
                    (Gen.choose (1, 3))
                    (Gen.choose (0, 2))
                Gen.map3
                    (fun row m pos -> RuntimeTypeHandleTarget.MethodGenericParameter (identity row, method m, pos))
                    (Gen.choose (1, 3))
                    (Gen.choose (1, 3))
                    (Gen.choose (0, 2))
            ]

    /// A target that is not `Closed`, built through the constructors under test so that it is
    /// canonical.
    let rec private genOpen (size : int) : Gen<RuntimeTypeHandleTarget> =
        if size <= 0 then
            genVariable
        else
            Gen.oneof
                [
                    genVariable
                    Gen.map2 RuntimeTypeHandleTarget.composite genShape (genOpen (size - 1))
                    gen {
                        // At least one open type, so the constructor keeps it open.
                        let! first = genOpen (size - 1)
                        let! second = genTarget (size - 1)

                        let! ret =
                            Gen.frequency
                                [
                                    1, Gen.constant MethodReturnType.Void
                                    2, Gen.map MethodReturnType.Returns (genTarget (size - 1))
                                ]

                        return RuntimeTypeHandleTarget.functionPointer (signature [ first ; second ] ret)
                    }
                    gen {
                        let! row = Gen.choose (1, 3)
                        let! first = genOpen (size - 1)
                        let! second = genTarget (size - 1)
                        return RuntimeTypeHandleTarget.openConstructed (identity row) [ first ; second ]
                    }
                ]

    /// Canonical targets only: every shape goes through the constructor under test.
    and private genTarget (size : int) : Gen<RuntimeTypeHandleTarget> =
        Gen.oneof [ Gen.map RuntimeTypeHandleTarget.Closed (genConcrete size) ; genOpen size ]

    let private isClosed (target : RuntimeTypeHandleTarget) : bool =
        match target with
        | RuntimeTypeHandleTarget.Closed _ -> true
        | _ -> false

    let private check (property : 'a -> unit) (gen : Gen<'a>) : unit =
        Check.One (Config.QuickThrowOnFailure.WithMaxTest 500, Prop.forAll (Arb.fromGen gen) property)

    [<Test>]
    let ``ofConcrete inverts applyConcrete`` () =
        Gen.zip genShape (genConcrete 3)
        |> check (fun (shape, element) ->
            CompositeShape.ofConcrete (CompositeShape.applyConcrete shape element)
            |> shouldEqual (Some (shape, element))
        )

    [<Test>]
    let ``ofConcrete answers None exactly for the shapes with no single element`` () =
        genConcrete 3
        |> check (fun handle ->
            let expected =
                match handle with
                | ConcreteTypeHandle.Concrete _
                | ConcreteTypeHandle.FunctionPointer _ -> None
                | _ ->
                    match CompositeShape.ofConcrete handle with
                    | Some _ as answer -> answer
                    | None -> failwith $"a shaped handle %O{handle} must decompose"

            CompositeShape.ofConcrete handle |> shouldEqual expected
        )

    [<Test>]
    let ``composite collapses a closed element and preserves an open one`` () =
        Gen.zip genShape (genTarget 3)
        |> check (fun (shape, element) ->
            let built = RuntimeTypeHandleTarget.composite shape element

            match element with
            | RuntimeTypeHandleTarget.Closed handle ->
                built
                |> shouldEqual (RuntimeTypeHandleTarget.Closed (CompositeShape.applyConcrete shape handle))
            | _ -> built |> shouldEqual (RuntimeTypeHandleTarget.Composite (shape, element))
        )

    [<Test>]
    let ``functionPointer is closed exactly when every type is closed`` () =
        gen {
            let! pars = Gen.listOfLength 3 (genTarget 2)

            let! ret =
                Gen.frequency
                    [
                        1, Gen.constant MethodReturnType.Void
                        3, Gen.map MethodReturnType.Returns (genTarget 2)
                    ]

            return signature pars ret
        }
        |> check (fun signature ->
            let allClosed =
                List.forall isClosed signature.ParameterTypes
                && (
                    match signature.ReturnType with
                    | MethodReturnType.Void -> true
                    | MethodReturnType.Returns ty -> isClosed ty
                )

            match RuntimeTypeHandleTarget.functionPointer signature with
            | RuntimeTypeHandleTarget.Closed (ConcreteTypeHandle.FunctionPointer closed) ->
                allClosed |> shouldEqual true

                closed.ParameterTypes
                |> List.map RuntimeTypeHandleTarget.Closed
                |> shouldEqual signature.ParameterTypes

                closed.Header |> shouldEqual signature.Header
            | RuntimeTypeHandleTarget.FunctionPointer preserved ->
                allClosed |> shouldEqual false
                preserved |> shouldEqual signature
            | other -> failwith $"unexpected %O{other}"
        )

    [<Test>]
    let ``every target the constructors build is well-formed`` () =
        genTarget 4 |> check RuntimeTypeHandleTarget.assertWellFormed

    [<Test>]
    let ``a Composite over a closed element is refused as non-canonical`` () =
        Gen.zip genShape (genConcrete 2)
        |> check (fun (shape, handle) ->
            let spelled =
                RuntimeTypeHandleTarget.Composite (shape, RuntimeTypeHandleTarget.Closed handle)

            let message =
                try
                    RuntimeTypeHandleTarget.assertWellFormed spelled
                    failwith "expected refusal"
                with e ->
                    e.Message

            message |> shouldContainText "not canonical"
        )

    [<Test>]
    let ``a Composite whose element is non-canonical is refused`` () =
        Gen.zip3 genShape genShape (genConcrete 2)
        |> check (fun (outer, inner, handle) ->
            // Canonical at the top and non-canonical one level down: the walk has to recurse.
            let spelled =
                RuntimeTypeHandleTarget.Composite (
                    outer,
                    RuntimeTypeHandleTarget.OpenConstructed (
                        identity 1,
                        [
                            RuntimeTypeHandleTarget.Composite (inner, RuntimeTypeHandleTarget.Closed handle)
                            RuntimeTypeHandleTarget.GenericParameter (identity 2, 0)
                        ]
                    )
                )

            let message =
                try
                    RuntimeTypeHandleTarget.assertWellFormed spelled
                    failwith "expected refusal"
                with e ->
                    e.Message

            message |> shouldContainText "not canonical"
        )

    [<Test>]
    let ``a FunctionPointer with every type closed is refused as non-canonical`` () =
        gen {
            let! pars = Gen.listOfLength 2 (genConcrete 2)

            let! ret =
                Gen.frequency
                    [
                        1, Gen.constant MethodReturnType.Void
                        2, Gen.map MethodReturnType.Returns (genConcrete 2)
                    ]

            return
                signature
                    (List.map RuntimeTypeHandleTarget.Closed pars)
                    (MethodReturnType.map () (fun () h -> (), RuntimeTypeHandleTarget.Closed h) ret
                     |> snd)
        }
        |> check (fun signature ->
            let message =
                try
                    RuntimeTypeHandleTarget.assertWellFormed (RuntimeTypeHandleTarget.FunctionPointer signature)
                    failwith "expected refusal"
                with e ->
                    e.Message

            message |> shouldContainText "not canonical"
        )

    [<Test>]
    let ``ToString renders every target`` () =
        genTarget 4
        |> check (fun target -> string target |> String.length |> shouldBeGreaterThan 0)
