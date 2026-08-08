# The method arm of the `Signature_Init` QCall

## What is blocked, verified rather than assumed

`SprintfBasic` is parked (`TestFSharpPureCases.unimplemented`) on

```
TODO: Signature_Init method signature parsing is not implemented;
got non-null Numeric (NativeInt (MethodHandlePtr 25L))
```

thrown by `requireNullMethodHandle` (`Native/NativeSignature.fs:95`) from the `Signature_Init`
handler (`:273`). Un-parking and running confirms that this is still where it stops
(`F# pure tests("SprintfBasic")`, 8 other cases in the fixture pass).

The blocker is not F#-specific. A four-line C# guest

```csharp
MethodInfo m = typeof (Program).GetMethod ("Twice", ...);
if (m.ReturnType != typeof (int)) return 2;
```

stops at exactly the same `failwith`, so the gap is reachable in isolation and does not need
`sprintf` to exercise it.

## Where the chain actually goes

`RuntimeMethodInfo.Signature` (`RuntimeMethodInfo.CoreCLR.cs:87`) lazily builds
`new Signature(this, m_declaringType)`, the two-argument constructor that leaves
`_returnTypeORfieldType` null, so the QCall must run the *full* parse arm: calling convention,
return type, and the `_arguments` array.

Spiking the method arm (working tree of this branch, uncommitted) shows what each further
reflection member costs:

| Guest expression | Verdict after this change |
| --- | --- |
| `m.ReturnType` (incl. `void`) | passes |
| `m.CallingConvention` (`Standard`, `Standard\|HasThis`) | passes |
| `m.GetParameters()` | next blocker: `RuntimeMethodHandle.GetMethodDef` (InternalCall, unimplemented) |
| ... with that spiked too | next blocker: `MetadataImport.Enum` for token type `0x08000000` (mdtParamDef) with a MethodDef parent — i.e. `EnumParams` |

So **`SprintfBasic` stays parked**; its park comment gets rewritten to name `GetMethodDef` and
`EnumParams` instead of `Signature_Init`. Those are two further primitives and belong in their own
changes (`GetMethodDef` is a near-trivial FCall; `EnumParams` is a new `MetadataImport.Enum` token
type). This PR ships the `Signature_Init` method arm plus a guest test that covers exactly what it
buys.

## Upstream shape being mirrored

`Signature_Init` (`src/coreclr/vm/runtimehandles.cpp:1585`):

```c
if (pMethodDesc != NULL)      pMethodDesc->GetSig(&pCorSig, &cCorSig);
else if (pFieldDesc != NULL)  pFieldDesc->GetSig(&pCorSig, &cCorSig);
_ASSERTE(pCorSig != NULL && cCorSig > 0);
gc.pSig->_sig = pCorSig; gc.pSig->_csig = cCorSig; gc.pSig->_pMethod = pMethodDesc;
if (gc.pSig->_returnTypeORfieldType == NULL) {
    // SigTypeContext = declType's class instantiation + pMethodDesc->LoadMethodInstantiation()
    if (callConv == IMAGE_CEE_CS_CALLCONV_FIELD) { ...field arm... }
    else {
        gc.pSig->SetCallingConvention(msig.GetCallingConventionInfo());
        gc.pSig->SetReturnType(msig.GetRetTypeHandleThrowing().GetManagedClassObject());
        // AllocateSzArray(RuntimeType[], msig.NumFixedArgs()) -- allocated even for 0 args
        for each fixed arg: gc.pSig->SetArgument(i, ...);
    }
}
```

Two details worth pinning down because they are easy to get wrong:

* `SetCallingConvention` (`runtimehandles.h:455`) is a **translation**, not the raw ECMA byte:
  `IMAGE_CEE_CS_CALLCONV_VARARG` → `CALLCONV_VarArgs` (0x2), everything else →
  `CALLCONV_Standard` (0x1), then `|= 0x20` for HASTHIS and `|= 0x40` for EXPLICITTHIS. Note
  `IMAGE_CEE_CS_CALLCONV_DEFAULT` is 0x0 while `CallingConventions.Standard` is 0x1.
* the field arm never calls `SetCallingConvention` at all, so a field-backed `Signature` leaves
  `_managedCallingConventionAndArgIteratorFlags` at 0 (see "pre-existing divergence" below).

## Decisions

### 1. Where do the parameter and return types come from?

* **A (chosen): PawPrint's already-parsed `MethodInfo.Signature`** — `ParameterTypes : TypeDefn list`
  and `ReturnType : MethodReturnType<TypeDefn>` — fed through `IlMachineState.concretizeType` and
  `getOrAllocateType`, exactly as `runtimeTypeForField` already does for the field arm.
* B: parse the COR blob bytes with a `BlobReader`, mirroring `MetaSig`.

A is the "transform canonical data into the right form" option: it reuses the one signature parser
the interpreter already trusts, so a method's reflected parameter types cannot disagree with the
types the interpreter binds calls against. B would create a second, independently-drifting
signature interpretation for the sake of surface similarity to C++. B is genuinely needed one day
for `Signature(void* pCorSig, int cCorSig, RuntimeType)` (a raw blob with no MethodDesc), which is
a separate, currently-unreached constructor that keeps failing loudly.

### 2. `_sig` / `_csig`

* **A (chosen): add `PeByteRangePointerSource.MethodSignatureBlob of ComparableMethodDefinitionHandle`**
  and point `_sig` at the MethodDef's signature blob, mirroring `peByteRangeForFieldSignatureBlob`.
  The compiler then forces the three existing match sites to be revisited
  (`ManagedPointerSource.tryContainerAlignmentBits`, `readPeByteRangeBytesAs`,
  `NativeSignature.resolveSignatureBlobHandle`); alignment bits stay `None` for the same reason the
  field variant does (a `#Blob`-heap offset PawPrint does not track).
* B: leave `_sig` null / `_csig` 0.

B is smaller but stores a value real .NET asserts cannot occur (`_ASSERTE(pCorSig != NULL && cCorSig > 0)`),
and would make the downstream readers (`GetParameterOffsetInternal`, `Signature_AreEqual`,
`GetCustomModifiersAtOffset`) fail with "not a PE byte range" instead of their own honest
"non-FIELD calling convention" TODO. A keeps every diagnostic pointing at the real gap.

### 3. Generic methods

`MetadataMethodIdentity` carries `DeclaringType : ConcreteTypeHandle` and
`MethodGenerics : ConcreteTypeHandle list`, which is the `SigTypeContext` we need — for *closed*
instantiations. For a generic method **definition** (the handle the introduced-method iterator
mints, `MethodGenerics = []`), CoreCLR resolves the signature against the typical instantiation,
whose method generic parameters a `ConcreteTypeHandle` cannot represent — the same limit that parks
`MakeGenericMethodOpenArgument.cs`.

* **A (chosen): fail loudly** when `methodInfo.Generics.Length <> MethodGenerics.Length`, naming the
  method and both counts.
* B: substitute something plausible (e.g. `System.Object`) so the call succeeds.

B would silently hand the guest wrong `RuntimeType`s for `T`. A is "prefer crashing over documented
divergence", and the failure names the representational gap rather than a symptom.

### 4. Dispatch between the three input shapes

CoreCLR's precedence is `pMethodDesc`, then `pFieldDesc`, then a caller-supplied raw blob. PawPrint
should classify explicitly:

```
match methodHandle, fieldHandle with
| Some m, None    -> method arm
| None,   Some f  -> field arm
| Some _, Some _  -> failwith (both non-null; no managed caller does this)
| None,   None    -> failwith (TODO: raw pCorSig blob, as today)
```

Refusing "both non-null" is deliberately *stricter* than CoreCLR, which would silently prefer the
method. No managed constructor passes both, so a value that reached here would be a PawPrint bug,
and preferring one input would hide it.

This changes one existing test. `TestNativeSignature.fs:462`, ``Signature_Init rejects mixed field
handle and method handle inputs``, currently passes `NativeInt (Verbatim 1L)` as a "non-null method
handle" and asserts the `requireNullMethodHandle` message. Its *intent* (both handles non-null is
refused) survives; it needs a genuine registry handle and the new message. Flagging this because
changing an existing test deserves scrutiny: the assertion being relaxed is
"method-shaped input is unimplemented", which is precisely what this change implements.

### 5. `Signature_GetCustomModifiersAtOffset`

That handler builds its type context from `_declaringType` alone and passes empty method generics,
under a comment saying "`_sig` byref is only populated for field-shaped signatures" — which this
change falsifies. It stays unreachable from a guest until `GetMethodDef`/`EnumParams` land, so:

* **A (chosen): read `_pMethod`; when non-null, resolve it and pass its method generics**, mirroring
  `SignatureNative::GetTypeContext` (`runtimehandles.h:388`), and drop the stale comment.
* B: keep it as-is and only fix the comment, letting a generic method's modifier token fault inside
  `concretizeType` with an index error.

A is ~10 lines and is what the C++ does; B leaves a knowingly-wrong type context behind a comment.
Confirmed against the C++ rather than inferred: `Signature_GetCustomModifiersAtOffset`
(`runtimehandles.cpp:1484`) opens with `gc.pSig->GetTypeContext(&typeContext)`, and that method
(`runtimehandles.h:388`) branches on `_pMethod` exactly as A does.

## Non-goals

* `RuntimeMethodHandle.GetMethodDef` and `MetadataImport`'s mdtParamDef enumeration — the next two
  blockers, each its own change.
* `Signature.GetParameterOffsetInternal`'s non-FIELD calling conventions (already a loud TODO).
* The raw-blob `Signature(void*, int, RuntimeType)` constructor arm.
* `_keepAlive`: CoreCLR sets it only for a collectible `LoaderAllocator`, and PawPrint has no
  collectible assemblies.

## Pre-existing divergence found while reading (deliberately deferred to its own PR)

`fillFieldSignature` writes the **raw ECMA byte** 0x6 into
`_managedCallingConventionAndArgIteratorFlags`, but CoreCLR's field arm never calls
`SetCallingConvention`, so a real field-backed `Signature` leaves that field 0. `(CallingConventions)6`
is `VarArgs | 0x4`, which is not a legal value. It is unobservable today — `FieldInfo` exposes no
`CallingConvention`, and neither `HasThis()` (6 & 0x20 = 0) nor `GetArgIteratorFlags()` (6 >> 8 = 0)
changes — so there is no guest-visible test for it, only a `TestNativeSignature` unit assertion.
Worth fixing; agreed to be its own PR rather than smuggled in here.

## Test plan

Unit (`TestNativeSignature.fs`), driving `tryExecuteQCall` directly — this is the only level at
which `_arguments`, `_sig` and `_pMethod` are observable at all today:

* a static two-parameter method: `_returnTypeORfieldType`, each `_arguments` element, `_csig`
  matching the MethodDef blob length, `_sig` resolving back through `resolveSignatureBlobHandle`,
  `_pMethod` echoing the input handle;
* a void, zero-argument method: `_arguments` is an allocated length-0 array, not null (CoreCLR
  always allocates), and `_returnTypeORfieldType` is `System.Void`;
* the calling-convention translation across static / instance;
* both-handles-non-null is refused;
* a generic method definition is refused with the counts in the message.

End-to-end (`sourcesPure/`), covering exactly what the spike proved reachable: `ReturnType` for a
value type, a reference type and `void`, and `CallingConvention` for a static and an instance
method. Deliberately no `GetParameters()` — that would park the file on the *next* primitive and
stop checking this one.

Mutation checks to run before calling it done: swap `callConvStandard`/`callConvVarArgs`, drop the
`HasThis` bit, return `_arguments` as null, and reverse the parameter fill order — each must kill at
least one test.

## Results

Shipped as planned, with one addition the mutation pass forced out (below).

Guest coverage: `sourcesPure/ReflectionMethodSignature.cs`, differential against real .NET via
`TestPureCases.runTest`. It reads `ReturnType` for a value type, a reference type, an array and
`void`, and `CallingConvention` for a static and an instance method, plus negative controls on the
VarArgs and ExplicitThis bits. Its methods also carry `ref`/`out` and generic-instantiation
parameters: `_arguments` is filled eagerly by the QCall, so those parameter shapes are concretized
on every one of these calls even though `GetParameters()` is not reachable yet.

Unit coverage: `TestNativeSignature.fs` gained a `MethodSignatureHost` type in its fixture source
plus 11 cases (return type and per-index argument types, void/nullary, the calling-convention
translation, `_sig`/`_csig`, `_pMethod`, the generic-method-definition refusal, both-handles-non-null,
and the null-field-handle spellings below). `TestNullaryIlOp.fs` pins `MethodSignatureBlob`'s
"no alignment claim", matching the field variant.

### Mutation results

| Mutant | Killed by |
| --- | --- |
| swap `callConvStandard` / `callConvVarArgs` | both unit calling-convention cases + guest (returns 6) |
| drop the `HasThis` bit | unit `Instance` case + guest (returns 8) |
| leave `_arguments` null | two unit cases + guest |
| reverse the parameter fill order | unit argument-order case + guest |
| skip the `_pMethod` write-back | unit `_pMethod` case + guest |
| write 0 into `_csig` | unit `_sig`/`_csig` case + guest |
| drop the generic-method-definition guard | unit refusal case + guest |
| narrow `fieldHandleIdOfRuntimeFieldHandleInternal` back | 8 unit cases + guest |

### A bug the guest test caught that the unit tests could not

Switching to the explicit four-way dispatch (decision 4) means classifying *both* handles before
choosing an arm, where the earlier shape only looked at the field handle if the method handle was
absent. `NativeCall.fieldHandleIdOfRuntimeFieldHandleInternal` recognised only two of the four
spellings of a null `RuntimeFieldHandleInternal` — and the one a real guest produces for
`default(RuntimeFieldHandleInternal)`, `NativeInt (ManagedPointer Null)`, was not among them, so it
threw instead of answering "no field handle". Its sibling
`methodHandleIdOfRuntimeMethodHandleInternal` already accepted all four; the asymmetry was latent
until a caller classified a field handle it was not going to use.

Fixed by widening the field classifier to match its sibling, which is the "keep the classifier's
contract truthful and load-bearing" rule: a caller dispatching on "did I get a field handle?" needs
an honest answer for every spelling of null. Every caller already treats `None` as "null handle" and
fails with its own diagnostic, so nothing else changes.

Two things are worth recording about how this was found. The unit tests all passed with the bug
present, because the fixture's null field handle used a spelling the classifier happened to list —
a unit test is only as faithful as the value it feeds in. And the end-to-end case was passing when
last run *before* the dispatch rewrite; it was the mutation pass, which re-ran it, that surfaced
the regression. `nullFieldHandleSpellings` now drives a test over all four shapes so the gap cannot
reopen silently.
