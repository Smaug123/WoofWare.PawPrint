# Stack-shape analysis: depths first, float widths at joins second

## Why

CoreCLR's importer types the spill temp of a stack slot at a block boundary as
`TYP_DOUBLE` whenever *any* predecessor delivers a double there, and inserts a
cast on every predecessor that delivers a float32 (`importer.cpp`, the
`reimportSpillClique` logic in `impImportBlock`). So the width of a float in a
stack slot at a join is a static property of the IL, joined over every incoming
path, and the path that executed is not enough to know it. An interpreter that
tags each runtime value with the width of the path that produced it (#1429)
answers `16777216` for the float32 arm of an emitted method whose other arm
pushes a double, where CoreCLR answers `16777218`.

Only hand-written or emitted IL reaches this: Roslyn and fsc insert `conv.r8` on
the float32 arm of a mixed conditional. It is still a divergence, and the fix is
the analysis CoreCLR itself performs: a dataflow over the body's control-flow
graph, computed when the body is first executed, which is when CoreCLR would JIT
it.

## Staging

The analysis splits along a line that matters for what it has to know.

*Depth* needs only how many values each instruction pops and pushes. For every
opcode but a call that is fixed; for a call it is the callee's arity, which is a
signature blob in the module that owns the body (or, for a body minted by
`Reflection.Emit`, the `DynamicScope` entry the call names). Nothing has to be
resolved or loaded, and the answer is the same at every instantiation of a
generic method, so one analysis serves a definition.

*Width* needs the type of what a field load, a call or an `ldobj` pushes, which
means reading types out of tokens, substituting the generic arguments a call
site supplies, and deciding whether a `TypeRef` named `System.Single` is the
primitive. That is where the design decisions with blast radius live, so it
comes second, on top of a depth analysis that the Guest suite has already
checked against every interpreted instruction.

### Stage 1 (this PR): depths

`WoofWare.PawPrint.Semantics/StackShape.fs` computes the stack depth on entry
to every instruction reachable from the method's entry or a handler entry,
joining over every path. Its contract:

* An instruction that cannot run on any path (an underflow, a branch outside
  the body, an argument or local index the signature lacks) is recorded in
  `Invalid` against its own offset. The rest of the body stays typed, and the
  interpreter refuses execution of that offset alone: CoreCLR's importer
  refuses such an instruction when it imports it, and it imports only what it
  reaches, folding a branch whose condition it can evaluate (a literal, an
  intrinsic such as `IsSupported`).
* A join two paths reach with different depths is a *conflict*. It is either
  invalid IL or one arm the importer never imports, and the analysis cannot
  tell which, so it does not try: the join is recorded and delivers an
  *unknown* depth, which propagates. What follows only from the join is untyped
  (`Reachable` but in neither `Entry` nor `Invalid`), a later join the
  conflict feeds is untyped too rather than classified from its other arms,
  and the interpreter runs all of it unchecked. Unknown supersedes what the
  first arm to arrive found on its own, since the second arm may be the one
  the importer imports.
* A call whose token the interpreter cannot read ahead of time is
  `MissingTokenShape`, which is not a claim about the IL; the instruction runs
  and fails, or raises, on its own terms.

The interpreter computes and caches the analysis per definition on a body's
first execution, and in a Debug build asserts before every interpreted
instruction that the runtime stack depth equals the analysis's prediction. The
Guest suite is therefore the oracle for the whole stack-effect table.

Not modelled, by design: which arms the importer folds away. The analysis
follows every branch. Where that produces a conflict the join is left untyped,
which is the correct degraded answer for depth; a wrong answer is impossible,
because two arms that agree on depth deliver the same depth whichever the
importer imports.

### Stage 2: float widths at joins

On top of stage 1, each slot gets a shape (`Float Single`, `Float Double`,
`Other`), joined over CoreCLR's spill cliques rather than per target
(`impWalkSpillCliqueFromPred`): every block reachable through the
predecessor/successor relation shares one temp. `Promotions` are the offsets
and slots at which a float32 still arrives where the clique settled as double,
which are exactly the edges CoreCLR casts.

Decisions for that stage, recorded here so the PR does not relitigate them:

* A `TypeRef` named `System.Single` or `System.Double` is the primitive when
  its resolution scope is a framework assembly, judged by name, exactly as the
  interpreter's `ConcretePrimitive` judges a concrete type. Nothing is resolved
  or loaded before the body runs: eager resolution is an effect the interpreter
  never had, and it makes "which offsets are reachable" decide whether a method
  can execute at all, which is the wrong altitude for a width analysis.
* Branches on a literal of the same basic block are folded as the importer
  folds them (`gtFoldExpr` runs at Tier-0 too; only debuggable code and MinOpts
  skip it), because otherwise a folded-away double arm widens a join CoreCLR
  keeps single. Folding of intrinsics, `typeof` comparisons and inlined
  constants is *not* modelled: an emitted mixed-width join under such a branch
  keeps path-local width, and that bound is stated rather than chased.
* Shapes are per instantiation (an argument of type `T` is a float32 in
  `M<float>`), so the cache key grows the generic arguments.

### Stage 3: apply the promotions

#1429 rebased onto stage 2: on arriving at a promotion point with a
`EvalStackFloat.Single` in a promoted slot, widen it, and extend the Debug
assertion to widths. #1430 (`conv.r.un; conv.r4` fusion) sits under it.
