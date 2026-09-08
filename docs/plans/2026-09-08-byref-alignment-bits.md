# Answering `and` on a byref from the object, not from the data start

## The question a guest asks

`UnicodeEncoding.GetByteCount` gates its vectorised loop on an alignment test
(`UnicodeEncoding.cs:401`):

```csharp
if ((bigEndian ^ BitConverter.IsLittleEndian) &&
    (unchecked((long)chars) & 7) == 0 &&      // TARGET_64BIT; the 32-bit arm masks 3
    charLeftOver == 0)
```

`chars` is a `char*` into a pinned string, so this is an 8-byte alignment test
on a byref. The same idiom appears throughout the vectorised BCL.

## What PawPrint said before this change

Five expressions on `&s[0]` of a `string`, run on real .NET and under the
interpreter:

| expression | real .NET | PawPrint |
| --- | --- | --- |
| `(nint)p & 7` | 4 | **0 — a wrong answer, silently** |
| `(nint)p & 3` | 0 | 0 |
| `(nint)p & 1` | 0 | 0 |
| `(int)p & 3` | 0 | 0 |
| `(long)p & 7` | 4 | **abort** |

Two distinct defects with one cause.

`andManagedPointerAddressBits` masked the *in-container byte offset* as though
it were the address, with no alignment reasoning at all: for index 0 the offset
is 0, so every mask answered 0. `andNarrowedManagedPointerBits` — the `conv.i4`
path — did it properly through `TaggedPointerBits`, and the 64-bit width had no
managed-pointer arm at all, so it fell through to `Int64Source.bitAnd`, which
materialises hash bits and refuses a byref.

## The measured facts

`docs/probes/byref-alignment/AlignmentProbe.cs`, 200 samples per row, on
macOS arm64 and linux-x64. The two agree on every row.

| | measured |
| --- | --- |
| object pointer | 8-byte aligned, 200/200 |
| `&s[0]` of a string | `addr & 7 == 4`, 200/200; delta from the object pointer 12 (`OffsetToStringData`) |
| `&a[0]` of an array | `addr & 7 == 0`, 200/200; delta 16 |
| `&s[0]` modulo 16 | **varies** — 4 and 12 both occur |
| the FASTLOOP gate | `((long)&s[0] & 7) == 0` for **0** of 200 strings |

So a string's char data is never 8-byte aligned, and that is a fact, not an
accident of one run: an 8-byte-aligned object plus a fixed 12-byte header is 4
mod 8. It also holds on the large object heap.

The old model described the *data start* — "4-byte aligned, unknown base" — and
that is sound but loses exactly the bit the guest is asking about.
`addr & 15` is genuinely undetermined, because objects are 8-byte aligned and
not 16; no model without a simulated address space can answer it.

## What this change does

Model a byref as **an 8-byte-aligned object plus a known byte offset that
includes the container's header**: `(3, 12 + 2k)` for a string char pointer,
`(3, 16 + k·stride)` for an array element. `TaggedPointerBits.bitAndOffsetFromAlignedBase`
already has precisely this shape and needed no change — it was being handed the
wrong base.

The alignment and the offset are returned together, by one function, because
pairing them by hand at the call site is what produced the wrong answer: one
caller paired them and the other did not.

All three widths — `int`, `nint`, `int64` — route through that one decision
procedure, so a mask is answered identically however the guest spelled the cast.

Under the new model the five rows above all agree with real .NET, and `& 15`
still refuses, which is the correct answer rather than a gap.

## What was deliberately not done

**Synthesising concrete object addresses.** It would answer everything,
including `& 15` and `p2 - p1`, by giving each object a deterministic fake
address. It also ends byref provenance as a *guarantee*: once addresses exist,
arithmetic can synthesise a pointer that aliases another object, and the
property that makes this memory model checkable goes with it. PawPrint does not
simulate an address space, and this change does not start.

**Answering alignment by policy** (always reporting "unaligned" so guests take
scalar fallbacks). It is cheap and would unblock the same code, but it is a
fabricated answer, and a guest that prints `(nint)p & 7` would get a wrong
number — the defect this change exists to remove. For strings the measured
answer happens to be "never 8-aligned" anyway, so the honest model unblocks
exactly as much.

**`or` and `xor` on a byref.** Neither has a managed-pointer arm, and both
still refuse. `and` is the masking idiom; nothing reaches the other two.

## Consequence for the Fantomas probe

`fantomas --check` under the interpreter reached
`Fantomas.FCS.Lexer.singleQuoteString` and stopped on the `(long)chars & 7`
above. With the gate answered honestly it evaluates false, exactly as on real
.NET, and the guest takes the scalar loop.
