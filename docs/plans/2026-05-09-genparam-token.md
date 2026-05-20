# Plan: metadata-token plumbing for type-generic parameters

## Why

`RuntimeTypeHandle.GetToken` (NativeRuntimeType.fs:2432) is the single
InternalCall that backs every "what's the metadata token of this `Type`?"
property in the BCL: `Type.MetadataToken`, `RuntimeType.MetadataToken`,
`MemberInfo.MetadataToken`, and the `MetadataImport.GetGenericParamProps`
prologue all call it. Today it crashes with
`failwith "TODO: ... metadata token for generic parameter ..."`
(NativeRuntimeType.fs:443-445) the moment a generic-parameter `RuntimeType`
flows through `typeDefinitionTokenOfRuntimeTypeHandleTarget`.

ECMA-335 §II.22.20 reserves table tag 0x2A for the GenericParam table; the
token is `0x2A000000 | rid` where `rid` is the 1-based row number. The
`MetadataToken.GenericParameter` DU case already exists (Tokens.fs:57),
`MetadataToken.ofInt` decodes 0x2A tokens correctly (Tokens.fs:124), and
`MetadataToken.toInt` re-encodes them (Tokens.fs:179). The piece that's
missing is plumbing the underlying `GenericParameterHandle` into the place
where `RuntimeTypeHandleTarget.GenericParameter` is consumed, so that
`GetToken` can produce a real 0x2A token instead of failing.

This plan covers only that plumbing. It is the smallest change that
(a) lets `RuntimeType.MetadataToken` succeed for type-parameter `RuntimeType`s, and
(b) makes `MetadataImport._GetGenericParamProps` (a follow-up PR) implementable
without further metadata-shape changes. Together those two pieces unblock
running the IL body of `Type.GenericParameterAttributes` cleanly — the original
motivation, parked from the abandoned `track-a-generic-param-6-3-baseType`
branch.

## What I learned

- `RuntimeTypeHandleTarget.GenericParameter` carries
  `(declaringType : ResolvedTypeIdentity, position : int)` only — the original
  `GenericParameterHandle` is lost on its way through the target DU. The
  comment at NativeRuntimeType.fs:437-440 explicitly flags this and points at
  "a later stage will plumb the GenericParameterHandle into the DU."
- `GenericParameter` (WoofWare.PawPrint.Domain/GenericParameter.fs:36-46)
  is a flat record of `Name : string` and `SequenceNumber : int`. There are
  no consumers that construct `GenericParameter` outside `readAll`; every
  other reader only touches `.Name` / `.SequenceNumber`. Adding a third
  field is mechanically safe.
- `readAll` (GenericParameter.fs:117-118) already has the unshadowed
  `GenericParameterHandle` in scope — it just gets shadowed by `let param =
  metadata.GetGenericParameter param`. Renaming the outer variable and
  populating a new field is a one-line change.
- `MethodInfo.fs:780`, `FieldInfo.fs:67`, `TypeInfo.fs:296` all call
  `GenericParameter.readAll`. Each is fed by a `MetadataReader` that owns
  the resulting `GenericParameterHandle`; the handle is only meaningful
  relative to that reader's assembly. So the `Handle` field needs the same
  hygiene as other handle-bearing types: store it in a comparable wrapper
  but treat the value as scoped to the owning assembly's metadata.
- The Comparable*Handle pattern (ComparableTypeDefinitionHandle.fs,
  ComparableFieldDefinitionHandle.fs, ComparableMethodDefinitionHandle.fs)
  is uniform: `private { _Inner }`, `Make`, `Get`, custom equality and
  comparison via `_Inner.GetHashCode()`. The new
  `ComparableGenericParameterHandle` should follow it verbatim.
- `typeDefinitionTokenOfRuntimeTypeHandleTarget` (NativeRuntimeType.fs:431)
  is the only consumer of the `GenericParameter` arm in token-emission
  paths. Other arms (e.g. `IsGenericVariable` at 2454, `GetGenericVariableIndex`
  at 2479, `DeclaringType` at 540) already handle parameters fine without
  needing the handle.
- The TypeInfo lookup for the declaring type is already a known motion:
  `RuntimeTypeHandle.GetDeclaringType` does it via
  `state.LoadedAssembly'`/`assembly.TypeDefs.[declaringType.TypeDefinition.Get]`
  (see NativeRuntimeType.fs:540-565). The new `GetToken` arm can reuse
  the same pattern to reach `typeInfo.Generics.[position]`.

## Design

Add `Handle : ComparableGenericParameterHandle` to `GenericParameter`. Populate
it in `GenericParameter.readAll`. Use it in the `GenericParameter` arm of
`typeDefinitionTokenOfRuntimeTypeHandleTarget` to construct
`MetadataToken.GenericParameter handle` and return its int form.

Why this shape rather than e.g. extending `RuntimeTypeHandleTarget` itself:

- `GenericParameter` is the canonical domain object describing what a generic
  parameter *is*. Its identity in metadata terms is exactly its
  `GenericParameterHandle` (rowid in the GenericParam table). Carrying that
  identity on the record matches the rule in AGENTS.md:
  "preserve the distinction between identity and view/projection."
- `RuntimeTypeHandleTarget.GenericParameter` is a runtime-side handle that
  already knows the declaring type and the position; adding a redundant
  `GenericParameterHandle` to it would mean two sources of truth, and we'd
  have to keep them in sync at every allocation site. Looking up the
  parameter on the declaring `TypeInfo` once we need the handle is cheap and
  matches how other parameter-aware paths are written.
- Method-generic parameters are out of scope here (Method-generic
  `RuntimeType` doesn't exist yet in `RuntimeTypeHandleTarget`). The same
  `Handle` field will serve them when they land, since `readAll` is
  shared between type-generic and method-generic parameter reading
  (MethodInfo.fs:780, TypeInfo.fs:296).

The plumbed-through handle never crosses an assembly boundary inside this
PR: the `GenericParameter` instance is read from the declaring type's
metadata reader and consumed at sites that already know which assembly that
is. So we don't need to wrap it in a `Sourced...Handle` analogue;
`ComparableGenericParameterHandle` alone is enough.

## Implementation steps

1. **`WoofWare.PawPrint.Domain/ComparableGenericParameterHandle.fs`** (new file).
   Copy `ComparableTypeDefinitionHandle.fs` verbatim, swap
   `TypeDefinitionHandle` → `GenericParameterHandle` and the type name.
2. **`WoofWare.PawPrint.Domain/WoofWare.PawPrint.Domain.fsproj`**:
   insert `<Compile Include="ComparableGenericParameterHandle.fs" />` between
   `ComparableSignatureHeader.fs` (line 21) and `TypeIdentity.fs` (line 22),
   matching the existing Comparable cluster's order.
3. **`WoofWare.PawPrint.Domain/GenericParameter.fs`**:
    - Add `Handle : ComparableGenericParameterHandle` to the
      `GenericParameter` record (line 36-46), with a one-line XML doc.
    - In `readAll` (line 117), rename the closure parameter from `param` to
      `paramHandle` so the original `GenericParameterHandle` is no longer
      shadowed by `let param = metadata.GetGenericParameter param`. Populate
      `Handle = ComparableGenericParameterHandle.Make paramHandle` in the
      record literal at line 161-165.
4. **`WoofWare.PawPrint/Native/NativeRuntimeType.fs`**:
   replace the `failwith` at line 443-445 with the real implementation.
   Pattern (matching the `GetDeclaringType` arm at lines 540-565):

   ```fsharp
   | RuntimeTypeHandleTarget.GenericParameter (declaringType, position) ->
       let assembly =
           state.LoadedAssembly' declaringType.AssemblyFullName
           |> Option.defaultWith (fun () ->
               failwith
                   $"%s{operation}: assembly for generic parameter declaring type is not loaded: %s{declaringType.AssemblyFullName}"
           )
       let typeInfo = assembly.TypeDefs.[declaringType.TypeDefinition.Get]
       if position >= typeInfo.Generics.Length then
           failwith
               $"%s{operation}: generic parameter position %i{position} out of range for %s{typeInfo.Namespace}.%s{typeInfo.Name} (has %i{typeInfo.Generics.Length} generics)"
       let param, _md = typeInfo.Generics.[position]
       MetadataToken.toInt (MetadataToken.GenericParameter param.Handle.Get)
   ```

   The position bounds check is a defensive assert; it should be unreachable
   given that `getOrAllocateType` only allocates parameter targets via
   `RuntimeTypeHandleTarget.GenericParameter` when the position exists. If
   we trust the construction-site invariant, drop the check; if we want
   "fail fast, fail loud," keep it. I lean keep, in the spirit of
   AGENTS.md's debug-assert posture.

5. **No change** to the other consumers of `GenericParameter`: every site
   that currently destructures with `(par, _)` or reads `.Name` /
   `.SequenceNumber` continues to compile unchanged because we're adding a
   field, not replacing existing ones.

6. **Test** — add `WoofWare.PawPrint.Test/sourcesPure/RuntimeTypeMetadataTokenGenericParameter.cs`.
   Per the project's pure-test convention, the file is auto-discovered, runs
   on both PawPrint and the real runtime, and passes when both produce the
   same exit code. Suggested body:

   ```csharp
   using System;
   using System.Collections.Generic;

   namespace RuntimeTypeMetadataTokenGenericParameter
   {
       class Box<T> { }

       class Pair<TKey, TValue> { }

       class Program
       {
           static int Main(string[] args)
           {
               // The first generic parameter of a locally-defined type
               // is the first 0x2A row written by Roslyn for this assembly,
               // so its token is 0x2A000001 (or higher; we don't hardcode).
               // The real-runtime cross-check will catch mismatches; we
               // just need each assertion to take a distinct exit code so
               // the failure mode pinpoints which property diverged.

               int boxT = typeof(Box<>).GetGenericArguments()[0].MetadataToken;
               int pairK = typeof(Pair<,>).GetGenericArguments()[0].MetadataToken;
               int pairV = typeof(Pair<,>).GetGenericArguments()[1].MetadataToken;

               // Tag bits identify GenericParam (0x2A).
               if ((boxT >> 24) != 0x2A) return 1;
               if ((pairK >> 24) != 0x2A) return 2;
               if ((pairV >> 24) != 0x2A) return 3;

               // Distinct rows for distinct parameters.
               if (boxT == pairK) return 4;
               if (pairK == pairV) return 5;
               if (boxT == pairV) return 6;

               // Same Type instance returns the same token.
               if (typeof(Box<>).GetGenericArguments()[0].MetadataToken != boxT) return 7;

               // CoreLib types have stable, well-known parameters too;
               // smoke-test that List<>'s T is also a 0x2A token.
               if ((typeof(List<>).GetGenericArguments()[0].MetadataToken >> 24) != 0x2A) return 8;

               return 0;
           }
       }
   }
   ```

   This is a structural test, not a hard-coded golden one — its only
   load-bearing assumption is "tokens are 0x2A-tagged, distinct across
   distinct parameters, identical for the same parameter." The pure-test
   harness's real-runtime cross-check then upgrades that to "the *exact*
   row numbers also match the host" by virtue of both runtimes producing
   exit code 0 for the same source.

7. **Format** with `nix develop -c dotnet fantomas .` and commit. Branch off
   main, push, run `codex review --base main` per CLAUDE.md.

## Correctness oracle

- **Stage-internal**: the new test compiles and returns exit code 0 on both
  PawPrint and the real runtime. Today the same source crashes PawPrint at
  the `failwith "TODO: ..."` site, so this is a genuine pass/fail signal.
- **Cross-assembly**: `RuntimeTypeHandleAttributes.cs` already exercises a
  `typeof(List<>).GetGenericArguments()[0].Attributes` path — that path
  routes through `RuntimeTypeHandle::GetAttributes`, which today returns
  `tdPublic` without consulting the token. After this PR, the same target
  flows through `GetToken` via any caller that asks for it. Re-running the
  full pure suite is the regression net.
- **Stretch (not required for this PR)**: a property test in
  `WoofWare.PawPrint.Test` that, given a list of corelib generic types,
  asserts `pawprint(t.GetGenericArguments()[i].MetadataToken) ==
  realRuntime(t.GetGenericArguments()[i].MetadataToken)` for every (t, i).
  This is the right shape — see gospel.md §4 — but isn't in scope here
  because the pure-test cross-check already gives us essentially the same
  oracle for one assembly's worth of types, and standing up a property
  harness for the corelib is its own piece of work. I'll note this as a
  gap rather than land it half-built.

## Out of scope

- **Method-generic parameters.** No `RuntimeTypeHandleTarget` arm exists
  for them today; when one does, the same `Handle` field on
  `GenericParameter` will serve, but the routing change in `GetToken` is
  separate.
- **`MetadataImport._GetGenericParamProps` (β).** Once `Handle` is on
  `GenericParameter`, this InternalCall becomes a small follow-up — it
  decodes a 0x2A token via `MetadataToken.ofInt`, looks up the row, and
  reads its `Attributes`. Out of scope for this PR.
- **Letting `Type.GenericParameterAttributes` IL run (γ).** This depends on β.
  The abandoned force-intrinsic mechanism on
  `track-a-generic-param-6-3-baseType` was the wrong layer to land at; γ
  reverts that approach and lets the public property's own IL body call
  through `_GetGenericParamProps` cleanly.
- **`Module.ResolveType` for 0x2A tokens.** Not needed for the consumers
  in scope. If the BCL ever reaches `Module.ResolveType(0x2A...)`, that's
  a separate small InternalCall implementation against the same
  `MetadataToken.GenericParameter` shape.
- **Cross-assembly generic parameters.** Tokens are scoped to the assembly
  whose metadata defines the parameter; the `RuntimeTypeHandleTarget.GenericParameter`
  arm already carries `declaringType.AssemblyFullName`, which is the
  correct scope, and the lookup uses that scope. No new sourcing helper
  needed.

## Risks and open questions

- **Should the `Handle` field be on `GenericParameter` or on
  `GenericParamMetadata`?** Both are part of the
  `GenericParamFromMetadata = GenericParameter * GenericParamMetadata`
  pair. I went with `GenericParameter` because the handle *is* identity,
  not metadata-of-the-parameter; the metadata record is for "what does the
  declaration say *about* the parameter" (variance, constraints). But a
  reasonable reviewer could argue either way. Open to flipping it.
- **Position vs. handle bounds check** in step 4: keep or drop. I lean
  keep, but it's defensive. If we're confident `getOrAllocateType`'s
  invariant holds, the check is dead weight.
- **Comparable wrapper vs. raw handle.** Every other handle in the domain
  layer goes through a `Comparable*Handle` wrapper, even when the only
  consumer of comparison is incidental. I followed the convention; if
  there's a reason it's been resisted for `GenericParameterHandle`
  specifically (I didn't find one), happy to use the raw handle and skip
  step 1-2.
- **Test naming.** `RuntimeTypeMetadataTokenGenericParameter.cs` is
  long but discoverable; `GenericParameterMetadataToken.cs` is shorter
  and groups under existing `GenericParameter*` files. Minor.
