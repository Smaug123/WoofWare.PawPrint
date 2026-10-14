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
That was found by reading the arms that name `ManagedPointerSource.Null`, and
it missed one: `ceq` answered the null pointer against a cross-storage offset
through an arm for any pointer, but refused `Verbatim 0` there. A property in
`TestBinaryArithmetic` now checks every comparison and binary arithmetic
operation: wherever the null pointer has an answer, `Verbatim 0` must have the
same one.

## Decision

A native int never holds a `ManagedPointer` whose bits are known: whatever
puts a value into a native int puts `Verbatim` bits there instead. The
alternative was to normalise at the head of each of the eleven instructions,
which is smaller, but leaves both spellings and their bridges in place, and
every new consumer would have to remember the normalisation.

## Stages

1. Done (#1910). `ldind`, `stind`, `ldind.ref` and `stind.ref` raise
   `NullReferenceException` through `Verbatim 0`, which is `*(int*)0`. One
   classifier, `EvalStackValue.isNullAddress`, names the null addresses for
   these and for `ldobj`, `stobj` and `initobj`, which already accepted it.
2. Done (#1922). Every producer above builds its native int with
   `NativeIntSource.ofManagedPointer`, which turns a pointer of known bits
   into `Verbatim`, and `EvalStack.Push'` refuses a native int that holds one
   as a pointer, in every build. The eleven instructions are unparked. `div`
   on a byref of known bits divides the bits, which removes its two wrong
   answers; a null byref is still legal, so those arms remain reachable.
3. Done. `NativeIntSource.ManagedPointer` carries an `AddressedByref` rather
   than a `ManagedPointerSource`, so a native int of known bits cannot be
   built at all, and `Push'` no longer checks. The arms that matched a native
   int holding a null pointer or a placeholder are gone, as is the property in
   `TestBinaryArithmetic` that compared the two spellings, since only one can
   be written. A null byref on the stack, `EvalStackValue.ManagedPointer
   Null`, is still legal, and becomes `Verbatim 0` wherever it is held as a
   native int.
