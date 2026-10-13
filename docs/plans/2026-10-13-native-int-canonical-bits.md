# One spelling for a native int of known bits

## Problem

A native int whose bits PawPrint knows can be held in two ways:

- `NativeIntSource.Verbatim n`, which integer arithmetic produces;
- `NativeIntSource.ManagedPointer p`, where `p` is `ManagedPointerSource.Null`
  (bits 0) or `ManagedPointerSource.NativeIntPlaceholder n` (bits n).

`CliType.zeroOfPrimitive` plants the second spelling for every default
`IntPtr` and `UIntPtr`, and `add` and `sub` on it produce placeholders. CoreCLR
cannot tell the spellings apart, since they are the same bits, but PawPrint
can, and it answers differently:

- `div`, `div.un`, `rem`, `rem.un`, `and`, `or`, `xor`, `shl`, `shr`,
  `shr.un` and `mul.ovf.un` refuse the pointer spelling. Stage 1 of the host
  oracle (`TestOpcodeFaultsOnPawPrint`) parks them for it.
- `div` answers `0 / null` and `null / null` with 0, where CoreCLR raises
  `DivideByZeroException`.
- `xor` of the null spelling with `Verbatim n` gives `OpaqueHashBits n`.
- `ldind`, `stind`, `ldind.ref` and `stind.ref` raise `NullReferenceException`
  through the null spelling, but fail in the interpreter through `Verbatim 0`.
- About forty sites (comparisons, branches, `NativeIntSource.isZero`, every
  native that takes `IntPtr.Zero`) carry an arm for each spelling.

## Survey

Changing `zeroOfPrimitive` alone does not remove the pointer spelling. It also
arrives from:

- a pointer-typed local or field, whose zero (`CliType.zeroOf`) is
  `RuntimePointer (Managed Null)`;
- `conv.i`, `conv.u` and the checked native-int conversions of a null byref or
  of `ldnull`;
- natives that return a NULL pointer or write one to an out-parameter;
- `Unsafe.AsRef(void*)`, `Unsafe.NullRef`, `Unsafe.As(null)`, and
  `Unsafe.Add` or `AddByteOffset` landing on 0;
- `Interlocked.Exchange` and `CompareExchange` of null into a `ref nint`;
- byref arithmetic that lands on a bit pattern (`BinaryArithmetic`'s
  `addOffsetToManagedPtr` and `subOffsetFromManagedPtr`).

Every native that takes `IntPtr.Zero` already accepts both spellings, and no
consumer depends on the pointer spelling except the four indirections above.

## Decision

A native int never holds a `ManagedPointer` whose bits are known: whatever
puts a value into a native int puts `Verbatim` bits there instead. The
alternative was to normalise at the head of each of the eleven instructions,
which is smaller, but leaves both spellings and their bridges in place, and
every new consumer would have to remember the normalisation.

## Stages

1. `ldind`, `stind`, `ldind.ref` and `stind.ref` raise
   `NullReferenceException` through `Verbatim 0`, which is `*(int*)0`. One
   classifier, `EvalStackValue.isNullAddress`, names the null addresses for
   these and for `ldobj`, `stobj` and `initobj`, which already accepted it.
2. Every producer above yields `Verbatim`, and a Debug assertion at the
   evaluation stack checks that no native int holds a pointer of known bits.
   The eleven instructions are unparked, and `div` loses its wrong answers.
3. The bridge arms for the pointer spelling are deleted.
