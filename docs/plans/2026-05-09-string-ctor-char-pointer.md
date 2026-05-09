# Plan: Implement `String..ctor(char*)`

## Context

`String..ctor(char*)` is a CoreLib InternalCall constructor that builds a managed `string` from a null-terminated UTF-16 character pointer. In the .NET BCL it is declared as `extern unsafe String(char* value)` with `[MethodImpl(MethodImplOptions.InternalCall)]`. WoofWare.PawPrint does not implement it, so any guest code using `new string(p)` over a `char*` traps. This is the user's "Blocker 13".

The fix is mostly mechanical: PawPrint already has every primitive needed — a UTF-16 null-terminator scanner over `ManagedPointerSource`, a managed-string allocator, and a native-dispatch seam for InternalCall String methods. The only missing piece is the per-method handler and a small helper to redirect the result of an `Newobj`-driven constructor frame to a different heap address (because strings are allocated to a fixed length, so we can't mutate the placeholder in place — we must allocate a fresh, correctly-sized string and arrange for *that* address to be pushed when the constructor frame returns).

## Approach

1. **Native handler in `NativeString.fs`**, alongside the existing `FastAllocateString` case. Match on assembly `System.Private.CoreLib`, namespace `System`, type `String`, method `.ctor`, parameter list `[ char* ]`, return type `void`.
2. The handler:
   - Reads the second method argument (`Arguments.[0]` is `this`; `Arguments.[1]` is the `char*`).
   - Coerces it to a `ManagedPointerSource` via the existing `NativeCall.managedPointerOfPointerArgument` helper. That helper already maps both `Verbatim 0L` and a literal null managed pointer to `ManagedPointerSource.Null`, and unwraps `NativeIntSource.ManagedPointer p` to `p`.
   - If null, the resulting string is empty (CoreCLR's `String.Ctor(char* ptr)` returns `Empty` when `ptr == null`; see `dotnet/.../System/String.cs:124-141`). Otherwise, calls `NativeCall.readNullTerminatedUtf16` to scan UTF-16 chars until `\0`. That helper is already correctly bounded (32767-char defensive scan limit) and already produces an F# `string`.
   - Allocates a fresh managed string of the right length via `IlMachineState.allocateManagedString` to get a new `ManagedHeapAddress`.
   - **Crucially**, replaces `WasConstructingObj` on the current frame's `ReturnState` with the new address. The placeholder allocated by `executeNewobj` (`UnaryMetadataObjectOps.fs:150`) stays on the heap as garbage but isn't referenced. After the native handler returns `Stepped Executed`, `AbstractMachine` calls `returnStackFrame` (`AbstractMachine.fs:182`), which inspects `WasConstructingObj` (`IlMachineThreadState.fs:182-211`) and pushes the address it finds. By the time we get there, that's our new string, not the placeholder.
3. **A small new helper** in `IlMachineThreadState.fs` (or `IlMachineState.fs` re-export) to mutate `ReturnState.WasConstructingObj` on the active frame. This uses `ThreadState.mapFrame` and is roughly:
   ```fsharp
   let withReplacedConstructedObject (newAddr : ManagedHeapAddress) (thread : ThreadId) (state : IlMachineState) : IlMachineState
   ```
   It fails loudly if `ReturnState` is `None` or if `WasConstructingObj` is already `None` — both would indicate this helper was called outside a constructor frame, which is a logic error.

This keeps the change small, reuses existing infrastructure, and matches the existing dispatch idiom in `NativeString.fs`. It does not require touching `executeNewobj` or `callMethod`.

## Files to modify

- `WoofWare.PawPrint/IlMachineThreadState.fs` — add the `withReplacedConstructedObject` helper (placed near the existing `setFrame` helpers around line 13–22).
- `WoofWare.PawPrint/IlMachineState.fs` — re-export the new helper alongside other re-exports (around line 80).
- `WoofWare.PawPrint/Native/NativeString.fs` — add the `.ctor(char*)` case to the pattern match in `tryExecute` (around line 19–46).
- `WoofWare.PawPrint.Test/sourcesPure/StringCtorCharPointer.cs` — new test (auto-discovered by the `sourcesPure` mechanism).

## Implementation sketch

In `NativeString.fs`, alongside `FastAllocateString`:

```fsharp
| "System.Private.CoreLib",
  "System",
  "String",
  ".ctor",
  [ ConcretePointer (ConcretePrimitive state.ConcreteTypes PrimitiveType.Char) ],
  MethodReturnType.Void ->
    let operation = "String..ctor(char*)"
    if instruction.Arguments.Length <> 2 then
        failwith
            $"%s{operation}: expected 2 args (this, char*) after matching signature, got %d{instruction.Arguments.Length}"

    let ptr =
        NativeCall.managedPointerOfPointerArgument operation "value" instruction.Arguments.[1]

    let contents =
        match ptr with
        | ManagedPointerSource.Null -> ""
        | _ ->
            NativeCall.readNullTerminatedUtf16 operation ctx.BaseClassTypes state ptr

    let newAddr, state =
        IlMachineState.allocateManagedString ctx.LoggerFactory ctx.BaseClassTypes contents state

    state
    |> IlMachineState.withReplacedConstructedObject newAddr ctx.Thread
    |> fun state -> (state, WhatWeDid.Executed) |> ExecutionResult.Stepped
    |> Some
```

Note: `Arguments.[0]` is `this` (the placeholder string ref); we deliberately ignore it. The `readNullTerminatedUtf16` helper already throws on unsupported source variants; we don't add new variant handling here.

## Edge cases

- **Null pointer (`(char*)0`).** Returns `""`. Matches CoreCLR's `Ctor(char*)` source. The user's `string s = new string((char*)null)` test should yield an empty string.
- **Non-null `NativeIntSource.Verbatim` (raw native address).** Falls through `managedPointerOfPointerArgument` to its `failwith`. This is consistent with current PawPrint scope: there is no model for arbitrary unmanaged memory, and this constructor variant is realistically only callable from guest code via `fixed` over a managed array (which produces `ManagedPointerSource.Byref _`) or via `null` (handled above). If a guest program ever does pass a raw address, the failure message will clearly indicate the missing capability.
- **Unterminated input.** `readNullTerminatedUtf16` enforces a 32767-char defensive scan ceiling and fails with a clear message — consistent behavior, not specific to this caller.
- **`stackalloc char[N]` followed by `new string(p)`.** Produces a `Byref` over a `LocalMemoryByte` root; the `readNullTerminatedUtf16` `Byref _` arm handles this transparently via `ManagedPointerByteView.addByteOffset` arithmetic.

## Test

`WoofWare.PawPrint.Test/sourcesPure/StringCtorCharPointer.cs` — exit code 0 on success. Cover:

- `fixed (char* p = chars)` over a managed `char[]` ending in `'\0'` → `new string(p)` equals the expected literal.
- Empty input: `char[] { '\0' }` → empty string.
- `new string((char*)null)` → empty string.
- `stackalloc char[]` containing terminated UTF-16 → matches expected literal.

The test files in `sourcesPure/` are auto-registered (see `TestPureCases.fs`); no manual wiring needed unless the test ends up needing to be on the `unimplemented` list.

If anything about the *current* `unimplemented` set should reference this work (e.g. tests that were blocked on `String..ctor(char*)`), that's a follow-up — let's not bundle it.

## Verification

1. Build: `nix develop -c dotnet build WoofWare.PawPrint.slnx`.
2. Format: `nix develop -c dotnet fantomas .`.
3. Run the new test: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --filter "Name~StringCtorCharPointer" --verbosity normal`.
4. Full suite to check for regressions: `nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal`.
5. After committing on a non-main branch: invoke `codex review --base main` per the project workflow in `CLAUDE.md`, address findings.

## Out of scope (intentional)

- `String..ctor(char*, int, int)` and the `sbyte*` overloads — they're separate InternalCalls.
- Reading from arbitrary `NativeIntSource.Verbatim` addresses — requires a model for unmanaged memory that doesn't currently exist (see exploration notes).
- Refactoring `readNullTerminatedUtf16` to allow `Null` (it currently `failwith`s with a TODO referencing `ArgumentNullException`). The String ctor wants different null behavior (return `""`, not throw), so we handle null *before* delegating, which leaves the helper's existing contract intact for its other callers (`AssemblyNative_GetResource`, `NativeKernel32`).
