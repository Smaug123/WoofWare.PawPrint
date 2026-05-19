namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Tests for the IL opcode visibility classifier. Totality is enforced by
/// exhaustive matches in the classifier itself, so these tests pin the
/// classification of a representative sample plus the full set of ops that we
/// have an opinion on, to guard against silent re-classification when a new
/// constructor lands.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpVisibility =

    /// A dummy metadata token, only used to construct `IlOp` values for tests.
    /// The classifier deliberately ignores token contents.
    let private dummyAssembly : AssemblyName = AssemblyName "dummy-for-visibility-tests"

    /// A TypeDefinition token at row 1, which is the implicit `<Module>` global
    /// type that every assembly has. Choice of token kind/row is arbitrary —
    /// `OpVisibility.classify` ignores the token entirely.
    let private token : SourcedMetadataToken =
        SourcedMetadataToken.ofInt dummyAssembly 0x02000001

    /// A user-string token at row 1; same reasoning as above.
    let private stringToken : SourcedStringToken =
        SourcedStringToken.ofInt dummyAssembly 0x70000001

    // ---------- Anchor sets for nullary ops ----------

    /// Nullary ops whose only effect is on frame-local state. Sampling, not
    /// exhaustive: the full enumeration is in the classifier itself.
    let private nullaryThreadLocalSample : NullaryIlOp list =
        [
            NullaryIlOp.Nop
            NullaryIlOp.LdArg0
            NullaryIlOp.Ldloc_2
            NullaryIlOp.Pop
            NullaryIlOp.Dup
            NullaryIlOp.LdcI4_5
            NullaryIlOp.LdNull
            NullaryIlOp.Stloc_1
            NullaryIlOp.Ceq
            NullaryIlOp.Cgt
            NullaryIlOp.Cgt_un
            NullaryIlOp.Clt
            NullaryIlOp.Clt_un
            NullaryIlOp.Conv_I
            NullaryIlOp.Conv_I4
            NullaryIlOp.Conv_I8
            NullaryIlOp.Conv_U
            NullaryIlOp.Conv_R4
            NullaryIlOp.Conv_R8
            NullaryIlOp.Conv_r_un
            NullaryIlOp.Endfilter
            NullaryIlOp.Endfinally
            NullaryIlOp.Localloc
            NullaryIlOp.Volatile
            NullaryIlOp.Tail
            NullaryIlOp.Readonly
            NullaryIlOp.Refanytype
            NullaryIlOp.Arglist
            NullaryIlOp.Break
        ]

    /// Nullary ops that touch the shared heap, dereference arbitrary pointers,
    /// or raise a CLR-defined exception (which allocates an exception object
    /// on the shared heap). Listed exhaustively (modulo trivial expansion of
    /// the LdInd / StInd / LdElem / StElem families) because each entry is
    /// load-bearing for the scheduler.
    let private nullaryGloballyVisible : NullaryIlOp list =
        [
            // Heap / indirect access
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
            // Trapping arithmetic / conversions: each may allocate a
            // CLR-defined runtime exception (OverflowException,
            // DivideByZeroException, ArithmeticException).
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
            // Explicit raise / re-raise: Throw allocates an NRE if the
            // operand is null; Rethrow reads through the active exception
            // object on the shared heap.
            NullaryIlOp.Throw
            NullaryIlOp.Rethrow
            // Bottom-frame Ret terminates the thread, mutating
            // `state.ThreadState` and waking any joiner. The classifier
            // can't tell bottom-frame from nested.
            NullaryIlOp.Ret
            // Arithmetic that may materialise `WidenedNativeInt` operands
            // into the shared `PointerHashCounters`; counter-assignment
            // order is guest-observable.
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
        ]

    [<Test>]
    let ``sampled thread-local nullary ops classify ThreadLocal`` () : unit =
        for op in nullaryThreadLocalSample do
            OpVisibility.classifyNullary op |> shouldEqual Visibility.ThreadLocal

    [<Test>]
    let ``listed globally-visible nullary ops classify GloballyVisible`` () : unit =
        for op in nullaryGloballyVisible do
            OpVisibility.classifyNullary op |> shouldEqual Visibility.GloballyVisible

    // ---------- Anchor sets for unary const ops ----------

    let private unaryConstThreadLocalSample : UnaryConstIlOp list =
        [
            UnaryConstIlOp.Stloc 7us
            UnaryConstIlOp.Stloc_s 3y
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
            UnaryConstIlOp.Beq 8
            UnaryConstIlOp.Beq_s 8y
            UnaryConstIlOp.Blt 8
            UnaryConstIlOp.Blt_s 8y
            UnaryConstIlOp.Ble 8
            UnaryConstIlOp.Ble_s 8y
            UnaryConstIlOp.Bgt 8
            UnaryConstIlOp.Bgt_s 8y
            UnaryConstIlOp.Bge 8
            UnaryConstIlOp.Bge_s 8y
            UnaryConstIlOp.Bne_un 8
            UnaryConstIlOp.Bne_un_s 8y
            UnaryConstIlOp.Bge_un 8
            UnaryConstIlOp.Bge_un_s 8y
            UnaryConstIlOp.Bgt_un 8
            UnaryConstIlOp.Bgt_un_s 8y
            UnaryConstIlOp.Ble_un 8
            UnaryConstIlOp.Ble_un_s 8y
            UnaryConstIlOp.Blt_un 8
            UnaryConstIlOp.Blt_un_s 8y
            UnaryConstIlOp.Ldloc_s 0uy
            UnaryConstIlOp.Ldloca_s 0uy
            UnaryConstIlOp.Ldarga 0us
            UnaryConstIlOp.Ldarg_s 0uy
            UnaryConstIlOp.Ldarga_s 0uy
            UnaryConstIlOp.Leave 8
            UnaryConstIlOp.Leave_s 8y
            UnaryConstIlOp.Starg_s 0uy
            UnaryConstIlOp.Starg 0us
            UnaryConstIlOp.Unaligned 0uy
            UnaryConstIlOp.Ldloc 0us
            UnaryConstIlOp.Ldloca 0us
            UnaryConstIlOp.Ldarg 0us
        ]

    [<Test>]
    let ``every unary const op classifies ThreadLocal`` () : unit =
        // The classifier currently treats *every* UnaryConst op as ThreadLocal.
        // If a new UnaryConst constructor ever needs to be globally visible,
        // this test will need updating; until then it's a useful guard.
        for op in unaryConstThreadLocalSample do
            OpVisibility.classifyUnaryConst op |> shouldEqual Visibility.ThreadLocal

    // ---------- Anchor sets for unary metadata ops ----------

    let private metadataGloballyVisible : UnaryMetadataTokenIlOp list =
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
            // Ldtoken allocates / mutates the runtime handle cache on first
            // use (RuntimeTypeHandle / RuntimeFieldHandle / RuntimeMethodHandle
            // objects are realised as heap objects with stable identity).
            UnaryMetadataTokenIlOp.Ldtoken
            // Refanyval allocates an InvalidCastException when the typed
            // reference's embedded type doesn't match the metadata token.
            UnaryMetadataTokenIlOp.Refanyval
        ]

    let private metadataThreadLocal : UnaryMetadataTokenIlOp list =
        [
            UnaryMetadataTokenIlOp.Ldftn
            UnaryMetadataTokenIlOp.Sizeof
            UnaryMetadataTokenIlOp.Constrained
            UnaryMetadataTokenIlOp.Mkrefany
        ]

    [<Test>]
    let ``heap-touching and call metadata ops classify GloballyVisible`` () : unit =
        for op in metadataGloballyVisible do
            OpVisibility.classifyUnaryMetadata op |> shouldEqual Visibility.GloballyVisible

    [<Test>]
    let ``pure metadata-token ops classify ThreadLocal`` () : unit =
        for op in metadataThreadLocal do
            OpVisibility.classifyUnaryMetadata op |> shouldEqual Visibility.ThreadLocal

    // ---------- Routing at the top-level classifier ----------

    [<Test>]
    let ``top-level classifier routes to nullary classifier`` () : unit =
        OpVisibility.classify (IlOp.Nullary NullaryIlOp.Nop)
        |> shouldEqual Visibility.ThreadLocal

        OpVisibility.classify (IlOp.Nullary NullaryIlOp.Stind_I4)
        |> shouldEqual Visibility.GloballyVisible

    [<Test>]
    let ``top-level classifier routes to unary-const classifier`` () : unit =
        OpVisibility.classify (IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 0))
        |> shouldEqual Visibility.ThreadLocal

    [<Test>]
    let ``top-level classifier routes to unary-metadata classifier`` () : unit =
        OpVisibility.classify (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Ldfld, token))
        |> shouldEqual Visibility.GloballyVisible

        OpVisibility.classify (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Sizeof, token))
        |> shouldEqual Visibility.ThreadLocal

    [<Test>]
    let ``top-level classifier routes to unary-string classifier`` () : unit =
        // Ldstr is globally visible because the first load of a literal
        // allocates the managed String and mutates the interning table;
        // later loads observe that shared object identity.
        OpVisibility.classify (IlOp.UnaryStringToken (UnaryStringTokenIlOp.Ldstr, stringToken))
        |> shouldEqual Visibility.GloballyVisible

    [<Test>]
    let ``Switch classifies ThreadLocal`` () : unit =
        OpVisibility.classify (IlOp.Switch (ImmutableArray.Create<int32> ()))
        |> shouldEqual Visibility.ThreadLocal
