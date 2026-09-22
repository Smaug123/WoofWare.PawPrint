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

### Stage 1 (#1443, merged): depths

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

### Stage 2 (#1445, merged): float widths at joins

On top of stage 1, each slot gets a shape (`Float Single`, `Float Double`,
`Other`), joined over CoreCLR's spill cliques rather than per target
(`impWalkSpillCliqueFromPred`): every block reachable through the
predecessor/successor relation shares one temp. `Promotions` are the offsets
and slots at which a float32 still arrives where the clique settled as double,
which are exactly the edges CoreCLR casts.

The Debug assertion grows a slot-kind check: a float in every slot the analysis
says is one and in no other, which the Guest suite checks against every
interpreted instruction. The width itself is not yet a fact the stack carries,
so it is checked in stage 3.

Decisions for this stage, agreed before it was built:

* A `TypeRef` named `System.Single` or `System.Double` is the primitive when
  its resolution scope is a framework assembly, judged by name, exactly as the
  interpreter's `ConcretePrimitive` judges a concrete type. Nothing is resolved
  or loaded before the body runs: eager resolution is an effect the interpreter
  never had, and it makes "which offsets are reachable" decide whether a method
  can execute at all, which is the wrong altitude for a width analysis.
* Branches on literals are *not* folded; a join whose width they can decide is
  refused. The importer folds a branch whose operands its block computes from
  literals alone (`gtFoldExpr`, at every tier but not in debuggable code),
  importing only the arm taken, so whether such a join is widened depends on how
  the body was compiled. Rather than model that (which tier, which assembly
  stamp, which constants fold under which widening and overflow rules, and a
  re-JIT at a higher tier typing the join differently again), the analysis types
  the flow graph with every arm imported, as debuggable code does. Folding only
  removes edges and the deliveries of code reached only through them, so that
  answer is every compilation's answer except where a float32 meets a double: a
  promotion. A promotion that a branch on literals can reach, through succession
  or through sharing a spill temp with something it reaches, is recorded as
  `WidthDependsOnFoldedBranch`, and the interpreter refuses to execute it rather
  than guess a width. What follows only from it is unknown. A literal is an
  integer, float, `null`, token or `sizeof` literal, or the result of a
  token-less operation on literals; a value arriving at a block's first
  instruction is a spill temp to the importer, and no literal, and a `br` to the
  very next instruction starts no block, since the JIT merges the blocks before
  importing. Measured on the 10.0.7 shared framework: 259 of 129,000 methods
  branch directly on a same-block `ldc`/`ldnull`, and 2 of those use any `.r4`
  opcode, so refusal should be rare. Other folds are *not* modelled: intrinsics,
  `typeof` comparisons, `box` patterns, inlined constants, algebraic identities
  with one runtime operand (`gtFoldExprSpecial`) and comparisons of a local with
  itself. The analysis treats such a branch as importing both arms, so a join
  under it where a float32 on the live arm meets a double on the arm CoreCLR
  folds away is promoted, and widened once stage 3 applies promotions, where
  CoreCLR keeps it single. Compilers rarely leave a value on the stack across
  such a branch, and that bound is stated rather than chased.
* Slots are joined over CoreCLR's spill cliques rather than per target. The
  clique is a property of the importer's flow graph: it spans blocks the
  importer never imports, so the two successors of a dead conditional share a
  temp, transitively. A
  conflict (a depth mismatch, a float meeting a non-float) makes the join
  unknown, as in stage 1, together with every offset sharing one of its spill
  temps: they share the temp whose type is undecidable.
* Shapes are per instantiation (an argument of type `T` is a float32 in
  `M<float>`), so the cache key grows the generic arguments.

### Stage 3 (this PR): apply the promotions

#1429 on top of stage 2 and #1430 (`conv.r.un; conv.r4` fusion, merged): the
stack float carries its width, and on arriving at a promotion point with an
`EvalStackFloat.Single` in a promoted slot the interpreter widens it. The Debug
assertion checks widths too: a float of the analysis's width in every slot it
says is a float. An offset the analysis leaves unknown (a conflict, or what
follows a refused join) gets no promotion and no check, so it keeps the width
of whichever path executed.
