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
