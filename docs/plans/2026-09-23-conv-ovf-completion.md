# Completing the `conv.ovf.*` family

Status: plan, 2026-09-23. Motivated by the ASP.NET ladder's rung K, whose first stop is
`conv.ovf.i8` from a `float64` in Kestrel's `TimeExtensions.ToTicks(TimeSpan, long)`.

## The gap

Of the twenty checked integer conversions, ten are `failwith "TODO: … unimplemented"` in
`NullaryIlOp.fs`: `conv.ovf.i2`, `conv.ovf.i8`, `conv.ovf.u8`, and `conv.ovf.{i1,i2,u2,u4,i8,u8,u}.un`.
The decoder, `OpcodeFaults`, `ContextSwitchPrior` and `StackShape` already know all twenty; only
execution is missing.

The ten that exist are one helper each (`convOvfI4`, `convOvfU2`, …), and seven of them — every
target narrower than 64 bits — are the same function with a different range: classify the source
(int32, int64, native int, float; refuse every tagged shape), read it as signed or unsigned, and
range-check it, with each float bound derived and commented by hand (`f >= 65536.0 || f <= -1.0`).
The three native-width ones (`conv.ovf.i`, `.u`, `.i.un`) differ in substance: they pass pointer
provenance through.

## Options

**A. Ten more hand-written helpers** in the existing style. Smallest conceptual change, but about
450 more lines of the same classification, and ten more sets of hand-derived float bounds — the
part most likely to be wrong, since each bound's strictness depends on which side truncation
rounds toward.

**B. One numeric core that every checked conversion desugars to.** A target is data (its inclusive
range as exact integers), a reading is data (`Signed` for `conv.ovf.<to>`, `Unsigned` for
`.un`), and the check is one function: read the source as an exact integer under the reading —
or, for a float, truncate toward zero and take the exact integer (NaN and infinities overflow) —
and test it against the range. Float bounds stop being hand-derived: an exact comparison of the
truncated value has no boundary to get wrong. The ten new opcodes are built on it, and so are the
seven existing narrowing ones, leaving no second implementation of the same rule. The
native-width trio keep their bespoke provenance handling.

**C. B, and also rewrite the native-width trio onto the core.** Their numeric arms are three lines
each; their substance is the pointer policy, which the core would not simplify. Churn in heavily
reviewed code for no reduction.

**Chosen: B.** It is the only option that removes the family's main error source rather than
multiplying it, and migrating the seven existing narrowing helpers is what stops B from being a
half-migration. Blast radius is bounded by the oracle below, which covers old and new opcodes
alike.

## Tagged sources

A checked conversion that *cannot* overflow for a given source shape is, for that shape, exactly
the unchecked conversion, so it gets the unchecked conversion's provenance policy verbatim:

- `conv.ovf.i8` on any integer or pointer source is `conv.i8` (a signed 32- or 64-bit source
  always fits in int64);
- `conv.ovf.u8.un` on any integer or pointer source is `conv.u8`;
- `conv.ovf.u.un` on any integer or pointer source is `conv.u`.

Where overflow *is* possible, the check needs the source's bits, and a tagged source has none
PawPrint can vouch for. So `conv.ovf.u8`, `conv.ovf.i8.un` (both overflow on a set top bit) and
every narrowing target refuse tagged sources with a `failwith`, as the narrowing family already
does (see `docs/plans/2026-08-08-narrowing-conv-pointer-hash.md`, "`conv.ovf.*` stays refusing").
The one exception, as today, is `NativeInt (ManagedPointer Null)`, which the guest has already
asked for as the number zero. `conv.ovf.i8.un` on an int32 zero-extends and cannot overflow.

Float sources ignore `.un` (ECMA-335 Partition III, `conv.ovf.<to type>.un`): a float is signed by construction. The oracle
checks that rather than this document.

## Tests

- **Host oracle, all twenty opcodes.** For each opcode and each numeric source stack type (int32,
  int64, native int, float64, float32), the host executes the opcode itself, emitted into a
  `DynamicMethod` whose return type is the stack type the opcode pushes — so the host also says
  how the result is extended into its slot. PawPrint runs the same opcode through
  `NullaryIlOp.execute`, and the two must agree on the pushed value or on overflow. Inputs are
  every target's boundaries and their neighbours (as integers of each width and as floats, with
  `Math.BitIncrement`/`BitDecrement` either side), plus random draws banded around each boundary.
  Written before the implementation, and observed failing on the ten missing opcodes.
- **Provenance policy.** For the three pass-through cases, a tagged source converts to exactly
  what the unchecked opcode gives. For the refusing cases, each tagged shape throws.
- **A guest** in `sourcesPure/` exercising every new opcode Roslyn emits from `checked(...)`, on
  both the success and the `OverflowException` path, cross-checked against real .NET.
