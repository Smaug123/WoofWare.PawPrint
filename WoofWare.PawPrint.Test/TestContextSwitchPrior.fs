namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the `ContextSwitchPrior` classifier. Totality of the per-op
/// match is enforced by the F# compiler, so these tests pin the band of a
/// representative-plus-exhaustive sample to guard against silent re-banding
/// when a new constructor lands or someone moves an existing op between
/// bands without thinking.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestContextSwitchPrior =

    /// A dummy metadata token, only used to construct `IlOp` values for tests.
    /// The classifier deliberately ignores token contents.
    let private dummyAssembly : AssemblyName =
        AssemblyName "dummy-for-context-switch-prior-tests"

    /// A TypeDefinition token at row 1, which is the implicit `<Module>` global
    /// type that every assembly has. Choice of token kind/row is arbitrary —
    /// the classifier ignores the token entirely.
    let private token : SourcedMetadataToken =
        SourcedMetadataToken.ofInt dummyAssembly 0x02000001

    /// A user-string token at row 1; same reasoning as above.
    let private stringToken : SourcedStringToken =
        SourcedStringToken.ofInt dummyAssembly 0x70000001

    // ---------- Band weight monotonicity ----------

    /// The whole point of having bands is that they admit a useful ordering
    /// by "likelihood of context-switch-worthiness". If the weight function
    /// ever loses that ordering, the bands no longer mean what their names
    /// say they mean.
    [<Test>]
    let ``weight is strictly increasing across the bands`` () : unit =
        let w b = ContextSwitchPrior.weight b
        w ContextSwitchPrior.Never |> shouldEqual 0.0

        (w ContextSwitchPrior.Never < w ContextSwitchPrior.InterpreterOnly)
        |> shouldEqual true

        (w ContextSwitchPrior.InterpreterOnly < w ContextSwitchPrior.RarelyGuestVisible)
        |> shouldEqual true

        (w ContextSwitchPrior.RarelyGuestVisible < w ContextSwitchPrior.AlwaysGuestVisible)
        |> shouldEqual true

        // Weights live in [0, 1] so a scheduler can interpret them as a
        // probability without further normalisation.
        (w ContextSwitchPrior.AlwaysGuestVisible <= 1.0) |> shouldEqual true
        (w ContextSwitchPrior.Never >= 0.0) |> shouldEqual true

    // ---------- Anchor sets for nullary ops ----------

    /// Pure-eval-stack / constant-push / frame-local-miscellany ops. Listed
    /// representative-exhaustive so this test fails loudly if any of these
    /// drifts into a heavier band.
    let private nullaryNever : NullaryIlOp list =
        [
            NullaryIlOp.Nop
            NullaryIlOp.Pop
            NullaryIlOp.Dup
            NullaryIlOp.LdcI4_0
            NullaryIlOp.LdcI4_1
            NullaryIlOp.LdcI4_2
            NullaryIlOp.LdcI4_3
            NullaryIlOp.LdcI4_4
            NullaryIlOp.LdcI4_5
            NullaryIlOp.LdcI4_6
            NullaryIlOp.LdcI4_7
            NullaryIlOp.LdcI4_8
            NullaryIlOp.LdcI4_m1
            NullaryIlOp.LdNull
            NullaryIlOp.Cgt
            NullaryIlOp.Cgt_un
            NullaryIlOp.Clt
            NullaryIlOp.Clt_un
            NullaryIlOp.Conv_I1
            NullaryIlOp.Conv_I2
            NullaryIlOp.Conv_I4
            NullaryIlOp.Conv_I8
            NullaryIlOp.Conv_R4
            NullaryIlOp.Conv_R8
            NullaryIlOp.Conv_U1
            NullaryIlOp.Conv_U2
            NullaryIlOp.Conv_U4
            NullaryIlOp.Conv_U8
            NullaryIlOp.Conv_r_un
            NullaryIlOp.Volatile
            NullaryIlOp.Tail
            NullaryIlOp.Readonly
            NullaryIlOp.Localloc
            NullaryIlOp.Refanytype
            NullaryIlOp.Arglist
            NullaryIlOp.Break
        ]

    /// Ops whose only effect is on interpreter-internal mutable bookkeeping
    /// (`PointerHashState`, or the array-shape reads on the byte-view anchor
    /// path). Guest IL has no opcode that directly addresses these structures.
    let private nullaryInterpreterOnly : NullaryIlOp list =
        [
            // Reads `PointerHashState` to decide whether synthesised bits are a
            // given handle's assigned address; a sibling thread materialising that
            // handle first flips the answer.
            NullaryIlOp.Ceq
            // Arithmetic that may materialise hash bits when an operand is
            // `Int64Source.WidenedNativeInt`.
            NullaryIlOp.Add
            NullaryIlOp.Sub
            NullaryIlOp.Mul
            NullaryIlOp.Neg
            NullaryIlOp.Not
            NullaryIlOp.Shl
            NullaryIlOp.Shr
            NullaryIlOp.Shr_un
            NullaryIlOp.And
            NullaryIlOp.Or
            NullaryIlOp.Xor
            // Anchor a byte-view on a plain-array byref via
            // `anchorByteViewIfPlainArrayByref`, which reads the array's shape
            // (`ManagedHeap.getArrayShape`) and never a cell.
            NullaryIlOp.Conv_I
            NullaryIlOp.Conv_U
        ]

    /// Ops that *could* cause a guest-visible effect on a given execution but
    /// usually don't. Locals/args (frame-private unless the slot's address
    /// has escaped); trapping arithmetic (overflow/divide-by-zero rare);
    /// exception ops (the exception object is usually thread-local);
    /// bottom-frame `Ret` (most Rets are nested).
    let private nullaryRarelyGuestVisible : NullaryIlOp list =
        [
            NullaryIlOp.LdArg0
            NullaryIlOp.LdArg1
            NullaryIlOp.LdArg2
            NullaryIlOp.LdArg3
            NullaryIlOp.Ldloc_0
            NullaryIlOp.Ldloc_1
            NullaryIlOp.Ldloc_2
            NullaryIlOp.Ldloc_3
            NullaryIlOp.Stloc_0
            NullaryIlOp.Stloc_1
            NullaryIlOp.Stloc_2
            NullaryIlOp.Stloc_3
            NullaryIlOp.Add_ovf
            NullaryIlOp.Add_ovf_un
            NullaryIlOp.Sub_ovf
            NullaryIlOp.Sub_ovf_un
            NullaryIlOp.Mul_ovf
            NullaryIlOp.Mul_ovf_un
            NullaryIlOp.Div
            NullaryIlOp.Div_un
            NullaryIlOp.Rem
            NullaryIlOp.Rem_un
            NullaryIlOp.Conv_ovf_i
            NullaryIlOp.Conv_ovf_u
            NullaryIlOp.Conv_ovf_i1
            NullaryIlOp.Conv_ovf_i2
            NullaryIlOp.Conv_ovf_i4
            NullaryIlOp.Conv_ovf_i8
            NullaryIlOp.Conv_ovf_u1
            NullaryIlOp.Conv_ovf_u2
            NullaryIlOp.Conv_ovf_u4
            NullaryIlOp.Conv_ovf_u8
            NullaryIlOp.Conv_ovf_i_un
            NullaryIlOp.Conv_ovf_u_un
            NullaryIlOp.Conv_ovf_i1_un
            NullaryIlOp.Conv_ovf_i2_un
            NullaryIlOp.Conv_ovf_i4_un
            NullaryIlOp.Conv_ovf_i8_un
            NullaryIlOp.Conv_ovf_u1_un
            NullaryIlOp.Conv_ovf_u2_un
            NullaryIlOp.Conv_ovf_u4_un
            NullaryIlOp.Conv_ovf_u8_un
            NullaryIlOp.Ckfinite
            NullaryIlOp.Throw
            NullaryIlOp.Rethrow
            NullaryIlOp.Ret
            NullaryIlOp.Endfilter
            NullaryIlOp.Endfinally
        ]

    /// Ops that every execution reads or writes shared guest-addressable
    /// state through. Indirect access through arbitrary managed pointers,
    /// array element ops, block copy/init.
    let private nullaryAlwaysGuestVisible : NullaryIlOp list =
        [
            NullaryIlOp.LdLen
            NullaryIlOp.Ldind_ref
            NullaryIlOp.Ldind_i
            NullaryIlOp.Ldind_i1
            NullaryIlOp.Ldind_i2
            NullaryIlOp.Ldind_i4
            NullaryIlOp.Ldind_i8
            NullaryIlOp.Ldind_u1
            NullaryIlOp.Ldind_u2
            NullaryIlOp.Ldind_u4
            NullaryIlOp.Ldind_u8
            NullaryIlOp.Ldind_r4
            NullaryIlOp.Ldind_r8
            NullaryIlOp.Stind_ref
            NullaryIlOp.Stind_I
            NullaryIlOp.Stind_I1
            NullaryIlOp.Stind_I2
            NullaryIlOp.Stind_I4
            NullaryIlOp.Stind_I8
            NullaryIlOp.Stind_R4
            NullaryIlOp.Stind_R8
            NullaryIlOp.Ldelem_i
            NullaryIlOp.Ldelem_i1
            NullaryIlOp.Ldelem_u1
            NullaryIlOp.Ldelem_i2
            NullaryIlOp.Ldelem_u2
            NullaryIlOp.Ldelem_i4
            NullaryIlOp.Ldelem_u4
            NullaryIlOp.Ldelem_i8
            NullaryIlOp.Ldelem_u8
            NullaryIlOp.Ldelem_r4
            NullaryIlOp.Ldelem_r8
            NullaryIlOp.Ldelem_ref
            NullaryIlOp.Stelem_i
            NullaryIlOp.Stelem_i1
            NullaryIlOp.Stelem_u1
            NullaryIlOp.Stelem_i2
            NullaryIlOp.Stelem_u2
            NullaryIlOp.Stelem_i4
            NullaryIlOp.Stelem_u4
            NullaryIlOp.Stelem_i8
            NullaryIlOp.Stelem_u8
            NullaryIlOp.Stelem_r4
            NullaryIlOp.Stelem_r8
            NullaryIlOp.Stelem_ref
            NullaryIlOp.Cpblk
            NullaryIlOp.Initblk
        ]

    [<Test>]
    let ``nullary ops banded Never classify as Never`` () : unit =
        for op in nullaryNever do
            ContextSwitchPrior.ofNullary op |> shouldEqual ContextSwitchPrior.Never

    [<Test>]
    let ``nullary ops banded InterpreterOnly classify as InterpreterOnly`` () : unit =
        for op in nullaryInterpreterOnly do
            ContextSwitchPrior.ofNullary op
            |> shouldEqual ContextSwitchPrior.InterpreterOnly

    [<Test>]
    let ``nullary ops banded RarelyGuestVisible classify as RarelyGuestVisible`` () : unit =
        for op in nullaryRarelyGuestVisible do
            ContextSwitchPrior.ofNullary op
            |> shouldEqual ContextSwitchPrior.RarelyGuestVisible

    [<Test>]
    let ``nullary ops banded AlwaysGuestVisible classify as AlwaysGuestVisible`` () : unit =
        for op in nullaryAlwaysGuestVisible do
            ContextSwitchPrior.ofNullary op
            |> shouldEqual ContextSwitchPrior.AlwaysGuestVisible

    // ---------- Anchor sets for unary const ops ----------

    let private unaryConstNever : UnaryConstIlOp list =
        [
            UnaryConstIlOp.Ldc_I4 42
            UnaryConstIlOp.Ldc_I8 123L
            UnaryConstIlOp.Ldc_R4 1.5f
            UnaryConstIlOp.Ldc_R8 2.5
            UnaryConstIlOp.Ldc_I4_s 4y
            UnaryConstIlOp.Br 8
            UnaryConstIlOp.Br_s 8y
            UnaryConstIlOp.Brfalse 8
            UnaryConstIlOp.Brfalse_s 8y
            UnaryConstIlOp.Brtrue 8
            UnaryConstIlOp.Brtrue_s 8y
            UnaryConstIlOp.Blt 8
            UnaryConstIlOp.Blt_s 8y
            UnaryConstIlOp.Ble 8
            UnaryConstIlOp.Ble_s 8y
            UnaryConstIlOp.Bgt 8
            UnaryConstIlOp.Bgt_s 8y
            UnaryConstIlOp.Bge 8
            UnaryConstIlOp.Bge_s 8y
            UnaryConstIlOp.Bge_un 8
            UnaryConstIlOp.Bge_un_s 8y
            UnaryConstIlOp.Bgt_un 8
            UnaryConstIlOp.Bgt_un_s 8y
            UnaryConstIlOp.Ble_un 8
            UnaryConstIlOp.Ble_un_s 8y
            UnaryConstIlOp.Blt_un 8
            UnaryConstIlOp.Blt_un_s 8y
            UnaryConstIlOp.Leave 8
            UnaryConstIlOp.Leave_s 8y
            UnaryConstIlOp.Unaligned 0uy
        ]

    /// The equality branches route through the same `EvalStackValueComparisons.ceq` as the
    /// nullary `Ceq`, so they read `PointerHashState` and answer differently depending on
    /// whether a sibling thread has materialised the handle being compared against. The
    /// *ordering* branches never consult it, and stay in the Never set above.
    let private unaryConstInterpreterOnly : UnaryConstIlOp list =
        [
            UnaryConstIlOp.Beq 8
            UnaryConstIlOp.Beq_s 8y
            UnaryConstIlOp.Bne_un 8
            UnaryConstIlOp.Bne_un_s 8y
        ]

    let private unaryConstRarelyGuestVisible : UnaryConstIlOp list =
        [
            UnaryConstIlOp.Stloc 7us
            UnaryConstIlOp.Stloc_s 3uy
            UnaryConstIlOp.Ldloc 0us
            UnaryConstIlOp.Ldloc_s 0uy
            UnaryConstIlOp.Ldloca 0us
            UnaryConstIlOp.Ldloca_s 0uy
            UnaryConstIlOp.Ldarg 0us
            UnaryConstIlOp.Ldarg_s 0uy
            UnaryConstIlOp.Ldarga 0us
            UnaryConstIlOp.Ldarga_s 0uy
            UnaryConstIlOp.Starg 0us
            UnaryConstIlOp.Starg_s 0uy
        ]

    [<Test>]
    let ``unary const ops banded Never classify as Never`` () : unit =
        for op in unaryConstNever do
            ContextSwitchPrior.ofUnaryConst op |> shouldEqual ContextSwitchPrior.Never

    [<Test>]
    let ``unary const ops banded InterpreterOnly classify as InterpreterOnly`` () : unit =
        for op in unaryConstInterpreterOnly do
            ContextSwitchPrior.ofUnaryConst op
            |> shouldEqual ContextSwitchPrior.InterpreterOnly

    [<Test>]
    let ``unary const ops banded RarelyGuestVisible classify as RarelyGuestVisible`` () : unit =
        for op in unaryConstRarelyGuestVisible do
            ContextSwitchPrior.ofUnaryConst op
            |> shouldEqual ContextSwitchPrior.RarelyGuestVisible

    // ---------- Anchor sets for unary metadata ops ----------

    let private metadataAlwaysGuestVisible : UnaryMetadataTokenIlOp list =
        [
            UnaryMetadataTokenIlOp.Call
            UnaryMetadataTokenIlOp.Calli
            UnaryMetadataTokenIlOp.Callvirt
            UnaryMetadataTokenIlOp.Jmp
            UnaryMetadataTokenIlOp.Newobj
            UnaryMetadataTokenIlOp.Newarr
            UnaryMetadataTokenIlOp.Stfld
            UnaryMetadataTokenIlOp.Ldfld
            UnaryMetadataTokenIlOp.Ldflda
            UnaryMetadataTokenIlOp.Stsfld
            UnaryMetadataTokenIlOp.Ldsfld
            UnaryMetadataTokenIlOp.Ldsflda
            UnaryMetadataTokenIlOp.Stelem
            UnaryMetadataTokenIlOp.Ldelem
            UnaryMetadataTokenIlOp.Ldelema
            UnaryMetadataTokenIlOp.Stobj
            UnaryMetadataTokenIlOp.Ldobj
            UnaryMetadataTokenIlOp.Cpobj
            UnaryMetadataTokenIlOp.Initobj
            UnaryMetadataTokenIlOp.Box
            UnaryMetadataTokenIlOp.Unbox
            UnaryMetadataTokenIlOp.Unbox_Any
            UnaryMetadataTokenIlOp.Castclass
            UnaryMetadataTokenIlOp.Isinst
            UnaryMetadataTokenIlOp.Ldvirtftn
        ]

    let private metadataRarelyGuestVisible : UnaryMetadataTokenIlOp list =
        [
            // `Refanyval` throws `InvalidCastException` on type mismatch.
            UnaryMetadataTokenIlOp.Refanyval
        ]

    let private metadataInterpreterOnly : UnaryMetadataTokenIlOp list =
        [
            UnaryMetadataTokenIlOp.Ldftn
            UnaryMetadataTokenIlOp.Sizeof
            UnaryMetadataTokenIlOp.Constrained
            UnaryMetadataTokenIlOp.Mkrefany
            UnaryMetadataTokenIlOp.Ldtoken
        ]

    [<Test>]
    let ``metadata ops banded AlwaysGuestVisible classify as AlwaysGuestVisible`` () : unit =
        for op in metadataAlwaysGuestVisible do
            ContextSwitchPrior.ofUnaryMetadata op
            |> shouldEqual ContextSwitchPrior.AlwaysGuestVisible

    [<Test>]
    let ``metadata ops banded RarelyGuestVisible classify as RarelyGuestVisible`` () : unit =
        for op in metadataRarelyGuestVisible do
            ContextSwitchPrior.ofUnaryMetadata op
            |> shouldEqual ContextSwitchPrior.RarelyGuestVisible

    [<Test>]
    let ``metadata ops banded InterpreterOnly classify as InterpreterOnly`` () : unit =
        for op in metadataInterpreterOnly do
            ContextSwitchPrior.ofUnaryMetadata op
            |> shouldEqual ContextSwitchPrior.InterpreterOnly

    // ---------- Routing at the top-level classifier ----------

    [<Test>]
    let ``ofIlOp dispatches to nullary classifier`` () : unit =
        ContextSwitchPrior.ofIlOp (IlOp.Nullary NullaryIlOp.Nop)
        |> shouldEqual ContextSwitchPrior.Never

        ContextSwitchPrior.ofIlOp (IlOp.Nullary NullaryIlOp.Stind_I4)
        |> shouldEqual ContextSwitchPrior.AlwaysGuestVisible

        ContextSwitchPrior.ofIlOp (IlOp.Nullary NullaryIlOp.Add)
        |> shouldEqual ContextSwitchPrior.InterpreterOnly

        ContextSwitchPrior.ofIlOp (IlOp.Nullary NullaryIlOp.Add_ovf)
        |> shouldEqual ContextSwitchPrior.RarelyGuestVisible

    [<Test>]
    let ``ofIlOp dispatches to unary-const classifier`` () : unit =
        ContextSwitchPrior.ofIlOp (IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 0))
        |> shouldEqual ContextSwitchPrior.Never

        ContextSwitchPrior.ofIlOp (IlOp.UnaryConst (UnaryConstIlOp.Ldloc 3us))
        |> shouldEqual ContextSwitchPrior.RarelyGuestVisible

    [<Test>]
    let ``ofIlOp dispatches to unary-metadata classifier`` () : unit =
        ContextSwitchPrior.ofIlOp (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token))
        |> shouldEqual ContextSwitchPrior.AlwaysGuestVisible

        ContextSwitchPrior.ofIlOp (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Sizeof, token))
        |> shouldEqual ContextSwitchPrior.InterpreterOnly

    [<Test>]
    let ``ofIlOp dispatches to unary-string classifier`` () : unit =
        ContextSwitchPrior.ofIlOp (
            IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, StringOperand.FromMetadata stringToken)
        )
        |> shouldEqual ContextSwitchPrior.InterpreterOnly

    [<Test>]
    let ``Switch classifies as Never`` () : unit =
        ContextSwitchPrior.ofIlOp (IlOp.Switch (ImmutableArray.Create<int32> ()))
        |> shouldEqual ContextSwitchPrior.Never
