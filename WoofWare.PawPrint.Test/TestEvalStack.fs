namespace WoofWare.PawPrint.Test

open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEvalStack =

    let private runtimePointerTarget : CliType =
        CliType.RuntimePointer (CliRuntimePointer.Verbatim 0L)

    let private assertDoesNotReturnObjectRef (popped : EvalStackValue) : unit =
        let outcome : Choice<CliType, exn> =
            try
                EvalStackValue.toCliTypeCoerced runtimePointerTarget popped |> Choice1Of2
            with e ->
                Choice2Of2 e

        match outcome with
        | Choice1Of2 (CliType.ObjectRef returned) ->
            failwith $"Bug: coercing %O{popped} to RuntimePointer returned ObjectRef(%O{returned})"
        | Choice1Of2 (CliType.RuntimePointer _) -> ()
        | Choice1Of2 other -> failwith $"Unexpected result from RuntimePointer coercion: %O{other}"
        | Choice2Of2 _ -> ()

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target does not return ObjectRef for NullObjectRef`` () : unit =
        assertDoesNotReturnObjectRef EvalStackValue.NullObjectRef

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target does not return ObjectRef for ObjectRef`` () : unit =
        ManagedHeapAddress.ManagedHeapAddress 42
        |> EvalStackValue.ObjectRef
        |> assertDoesNotReturnObjectRef

    [<Test>]
    let ``toCliTypeCoerced RuntimePointer target preserves type handle pointer provenance`` () : unit =
        let typeHandle = ConcreteTypeHandle.Concrete 42

        match
            EvalStackValue.toCliTypeCoerced
                runtimePointerTarget
                (EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr typeHandle))
        with
        | CliType.RuntimePointer (CliRuntimePointer.TypeHandlePtr actual) when actual = typeHandle -> ()
        | other -> failwith $"Expected RuntimePointer(TypeHandlePtr %O{typeHandle}), got %O{other}"

    [<Test>]
    let ``RuntimePointer carrying type handle pointer flattens back to native int`` () : unit =
        let typeHandle = ConcreteTypeHandle.Concrete 42

        match EvalStackValue.ofCliType (CliType.RuntimePointer (CliRuntimePointer.TypeHandlePtr typeHandle)) with
        | EvalStackValue.NativeInt (NativeIntSource.TypeHandlePtr actual) when actual = typeHandle -> ()
        | other -> failwith $"Expected NativeInt(TypeHandlePtr %O{typeHandle}), got %O{other}"
