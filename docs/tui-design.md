# PawPrint Debugger TUI: Design Document

## Philosophy

The purpose of this TUI is to make the entire machine state observable at a glance, and to support the deterministic nature of the runtime (stepping forward, and eventually backward). Every piece of state in `IlMachineState` should be reachable from the UI. The most commonly needed information is visible without navigation; less-frequently-needed state is one keypress away.

## Layout: Primary View

The primary view is a four-pane layout, with a thin status bar at the top and a command bar at the bottom.

```
┌─ Status Bar ──────────────────────────────────────────────────────────────┐
│ Step #4217  Thread 0 (of 3)  Frame 2/5  [Namespace.Class::Method<T>]     │
├─── IL Listing (left, 60%) ──────────┬─── State Panel (right, 40%) ───────┤
│                                     │ ┌─ Eval Stack ───────────────────┐ │
│   IL_0000: Nop                      │ │ 3: ObjectRef @42              │ │
│   IL_0001: Ldarg_0                  │ │ 2: Int32 7                    │ │
│   IL_0002: Ldfld String _name       │ │ 1: Int64 -1L                  │ │
│ ► IL_0007: Callvirt String.get_Length│ │ 0: &arg[1]                    │ │
│   IL_000C: Stloc_0                  │ ├─ Locals ──────────────────────┤ │
│   IL_000D: Ldloc_0                  │ │ 0: Int32 42                   │ │
│   IL_000E: Ldc_I4_0                 │ │ 1: ObjectRef @17              │ │
│   IL_000F: Ble_s IL_001A            │ │ 2: Bool true                  │ │
│                                     │ ├─ Arguments ───────────────────┤ │
│   ; exception region:               │ │ 0: ObjectRef @3  (this)       │ │
│   ; try IL_0010..IL_0018            │ │ 1: Int32 5                    │ │
│   ; catch [InvalidOp] IL_0018..IL_0020│ │ 2: ObjectRef null             │ │
│                                     │ └───────────────────────────────┘ │
├─── Call Stack (bottom) ─────────────┴────────────────────────────────────┤
│ ► 2  CSharpExample.Foo::Bar<Int32>(Int32, String)     IL_0007  Callvirt │
│   1  CSharpExample.Program::Helper(Foo)               IL_0023  Call     │
│   0  CSharpExample.Program::Main(String[])            IL_0042  Call     │
├─ Command Bar ────────────────────────────────────────────────────────────┤
│ [s]tep  [n]ext  [o]ut  [r]un  [t]hreads  [h]eap  [S]tatics  [a]sm  ?  │
└──────────────────────────────────────────────────────────────────────────┘
```

### Pane descriptions

**Status Bar** (1 line): Global step counter, selected thread ID and total thread count, current frame index and depth, and the fully-qualified method name (with generic parameters concretized). Always visible.

**IL Listing** (left, main pane): Disassembly of the current method body. The current PC is marked with `►`. Instructions are rendered using `IlOp.Format`. Branch targets are shown as resolved absolute offsets (e.g. `Ble_s IL_001A`). Exception regions relevant to the current PC are annotated as comments below the listing (try ranges, catch types, finally regions). The listing scrolls to keep the PC visible with ~3 lines of context above.

For `UnaryMetadataToken` ops, resolve the token to a human-readable name: `Call Namespace.Class::Method(ArgType)` rather than raw token numbers. For `UnaryStringToken` ops, show the string literal inline (truncated): `Ldstr "hello wor..."`.

**State Panel** (right, split into three sub-panels):

- **Eval Stack**: Top-of-stack at bottom (index 0), growing upward. Each entry shows its `EvalStackValue` variant and a compact rendering of the value. Object references show the heap address. Managed pointers show their source (e.g. `&local[2]`, `&arg[0]`, `&@42.fieldName`). Value types are shown as `{TypeName ...}` with a `→` indicator that they can be expanded.

- **Locals**: Indexed list of local variables with their `CliType` rendering. If `LocalVars` type info is available from the method body, show the declared type next to the index.

- **Arguments**: Indexed list of arguments. Argument 0 for instance methods is annotated `(this)`. Parameter names from `MethodInfo.Parameters` are shown where available.

**Call Stack** (bottom strip, ~3-5 lines): All frames in `MethodStates`, newest at top. Each line shows frame index, declaring type + method name with generics, current IL offset, and the instruction at that offset. The active frame is marked with `►`. Selecting a different frame (with up/down) updates the IL Listing and State Panel to show that frame's state.

**Command Bar** (1 line): Shows available key bindings for the current view.

## Execution Controls

| Key | Action |
|-----|--------|
| `s` | **Step Into**: Execute one IL instruction on the selected thread. If it's a `Call`/`Callvirt`/`Newobj`, enter the callee. |
| `n` | **Step Over**: Execute the current instruction, running through any calls it makes, and stop when control returns to the current frame. |
| `o` | **Step Out**: Execute until the current frame returns. |
| `r` | **Run**: Execute continuously until a breakpoint, exception, or termination. |
| `Ctrl+r` | **Run All Threads**: Step the machine (scheduler picks thread) until a breakpoint, exception, or termination. |
| `b` | **Toggle Breakpoint**: Set/clear a breakpoint at the currently highlighted IL offset. |
| `B` | **Breakpoint List**: Open overlay showing all breakpoints. |

Thread selection (in the primary view):

| Key | Action |
|-----|--------|
| `[` / `]` | Select previous/next thread. |
| `t` | Open **Threads** overlay (see below). |

Frame selection:

| Key | Action |
|-----|--------|
| `↑` / `↓` in call stack | Select a different frame to inspect. |

## Overlay Views

These replace the main content area when activated, and dismiss with `Esc` or `q`.

### Threads Overlay (`t`)

```
┌─ Threads ────────────────────────────────────────────────────────────────┐
│                                                                          │
│  ID  Status          Current Method                        IL Offset    │
│  ──  ──────────────  ──────────────────────────────────    ─────────    │
│ ► 0  Running         Program.Main(String[])                IL_0042      │
│   1  BlockedOnInit   (waiting on Thread 0: List<T>..cctor) IL_0003      │
│   2  Running         Foo.Compute<Int32>(Int32)             IL_0019      │
│                                                                          │
│ [Enter] select  [s] step this thread  [Esc] back                        │
└──────────────────────────────────────────────────────────────────────────┘
```

Shows all threads from `IlMachineState.ThreadState`. Status is derived from:
- `Running`: thread exists, not blocked
- `BlockedOnInit`: determined from `WhatWeDid.BlockedOnClassInit`
- `SuspendedForClassInit`: determined from `WhatWeDid.SuspendedForClassInit`

### Heap Inspector (`h`)

```
┌─ Managed Heap ───────────────────────────────────────────────────────────┐
│                                                                          │
│  Filter: [objects ▼]  Search: [________]                                │
│                                                                          │
│  Addr  Type                           Sync     Fields/Summary           │
│  ────  ────                           ────     ──────────────           │
│  @1    System.String                  Free     "Hello, World!"          │
│  @2    System.Int32[]                 Free     Length=5  [0,1,2,3,4]    │
│  @3    CSharpExample.Foo              Free     _name=@1 _count=42      │
│  @4    System.String                  Free     "test"                   │
│  @5    System.RuntimeType             Free     <handle for List<Int>>   │
│  @17   System.Object[]                Locked(T0,1) Length=3            │
│                                                                          │
│ [Enter] inspect  [f] filter  [/] search  [Esc] back                    │
└──────────────────────────────────────────────────────────────────────────┘
```

Three sub-views selectable with the filter dropdown:
- **Objects**: `ManagedHeap.NonArrayObjects` -- shows `ConcreteType` name, sync block state, and a compact field summary
- **Arrays**: `ManagedHeap.Arrays` -- shows element type, length, and first few elements
- **Strings**: Objects whose type resolves to `System.String` -- shows the string content from `StringArrayData`

Pressing Enter on an object opens a **detail view**:

```
┌─ Object @3: CSharpExample.Foo ───────────────────────────────────────────┐
│                                                                           │
│  Type: CSharpExample.Foo (ConcreteTypeHandle #7)                         │
│  Sync: Free                                                               │
│  Base: System.Object                                                      │
│                                                                           │
│  Fields:                                                                  │
│    _name     : ObjectRef @1  → System.String "Hello, World!"             │
│    _count    : Int32 42                                                   │
│    _inner    : ValueType {Point: X=Int32 10, Y=Int32 20}                 │
│    _list     : ObjectRef @12 → System.Collections.Generic.List<Int32>    │
│                                                                           │
│  Referenced by:                                                           │
│    Thread 0, Frame 0, Arg 0                                              │
│    Thread 0, Frame 2, Local 1                                            │
│    Heap @20 field "_parent"                                              │
│                                                                           │
│ [Enter] follow ref  [r] find references  [Esc] back                     │
└───────────────────────────────────────────────────────────────────────────┘
```

Key feature: **follow references**. Pressing Enter on an `ObjectRef` field navigates to that heap object. This allows walking the object graph interactively. The `r` key does a reverse lookup: scan all roots (thread locals/args/stack, static fields, other heap objects) that reference this address.

### Statics Overlay (`S`)

```
┌─ Static Fields ──────────────────────────────────────────────────────────┐
│                                                                          │
│  Search: [________]                                                     │
│                                                                          │
│  Type                                    Init State                     │
│  ────                                    ──────────                     │
│ ► System.String                          Initialized                    │
│   System.Collections.Generic.List<Int32> InProgress (Thread 0)          │
│   CSharpExample.Constants                Initialized                    │
│                                                                          │
│  Fields for: System.String                                              │
│    Empty  : ObjectRef @1 → ""                                           │
│                                                                          │
│ [Enter] expand  [Esc] back                                              │
└──────────────────────────────────────────────────────────────────────────┘
```

Lists all types that have entries in `_Statics`, merged with `TypeInitTable` to show initialization state. Selecting a type expands its static fields below with their current values.

### Assemblies Overlay (`a`)

```
┌─ Loaded Assemblies ──────────────────────────────────────────────────────┐
│                                                                          │
│  Assembly                            Types  Methods  Path               │
│  ────────                            ─────  ───────  ────               │
│  CSharpExample                       3      12       /path/to/dll       │
│  System.Private.CoreLib              847    6234     /path/to/dll       │
│  System.Runtime                      0      0        /path/to/dll       │
│                                                                          │
│ [Enter] browse types  [Esc] back                                        │
└──────────────────────────────────────────────────────────────────────────┘
```

Entering an assembly shows its type list. Entering a type shows its methods, fields, implemented interfaces, base type, and generic parameters.

### Concrete Type Registry (`T`)

```
┌─ Concrete Types ─────────────────────────────────────────────────────────┐
│                                                                          │
│  Search: [________]                                                     │
│                                                                          │
│  Handle  Type                                                           │
│  ──────  ────                                                           │
│  #0      System.Object                                                  │
│  #1      System.String                                                  │
│  #2      System.Int32                                                   │
│  #7      System.Collections.Generic.List<System.Int32>                  │
│  #8      System.Collections.Generic.List<System.String>                 │
│                                                                          │
│ [Enter] details  [Esc] back                                             │
└──────────────────────────────────────────────────────────────────────────┘
```

Shows `AllConcreteTypes.Mapping`. Generic arguments are rendered with their concrete type names rather than handles. Entering a type shows its fields, methods, base type, and which heap objects are instances of it.

### Exception Detail View

When the machine is in an exception-propagating state (`ExceptionContinuation = PropagatingException`), automatically highlight this in the status bar:

```
│ Step #4217  Thread 0  EXCEPTION: System.InvalidOperationException @55    │
```

Pressing `e` opens the exception detail:

```
┌─ Exception ──────────────────────────────────────────────────────────────┐
│                                                                          │
│  Type: System.InvalidOperationException                                 │
│  Object: @55                                                            │
│  Message: "Collection was modified during enumeration"                   │
│                                                                          │
│  Stack trace:                                                           │
│    List<Int32>.Enumerator.MoveNext()     IL_0023                        │
│    Program.ProcessItems(List<Int32>)     IL_0047                        │
│    Program.Main(String[])                IL_0012                        │
│                                                                          │
│  Exception regions in current frame:                                    │
│    try IL_0010..IL_0050                                                 │
│      catch [InvalidOperationException] IL_0050..IL_0070  ← will catch  │
│      finally IL_0070..IL_0080                                           │
│                                                                          │
│ [Enter] inspect object  [Esc] back                                      │
└──────────────────────────────────────────────────────────────────────────┘
```

## Value Rendering

A consistent rendering scheme for `CliType` values throughout all views:

| CliType | Rendering |
|---------|-----------|
| `Numeric (Int32 v)` | `Int32 42` |
| `Numeric (Int64 v)` | `Int64 -1L` |
| `Numeric (NativeInt (Verbatim v))` | `NativeInt 0x00FF` |
| `Numeric (NativeFloat v)` | `Float64 3.14` |
| `Numeric (Float32 v)` | `Float32 1.5f` |
| `Bool b` | `Bool true` / `Bool false` |
| `Char (h, l)` | `Char 'A'` (decoded) |
| `ObjectRef None` | `null` |
| `ObjectRef (Some addr)` | `@42 → TypeName` (type looked up from heap) |
| `RuntimePointer (Managed src)` | `&local[2]`, `&arg[0]`, `&@42.field`, `&@42[3]` |
| `RuntimePointer (Verbatim v)` | `ptr 0x00FF` |
| `ValueType vt` | `{TypeName: field1=v1, field2=v2}` (truncated if wide) |

For `ObjectRef` values pointing to `System.String`, append the string content: `@1 → String "hello"`.

For `EvalStackValue`, the rendering is the same but uses the eval-stack-width types (e.g. `EvalStackValue.Int32`, `EvalStackValue.ObjectRef`, etc.).

## Interned Strings View (`i`)

```
┌─ Interned Strings ───────────────────────────────────────────────────────┐
│                                                                          │
│  Token     Heap Addr  Value                                             │
│  ─────     ─────────  ─────                                             │
│  0x0001    @1         "Hello, World!"                                   │
│  0x0002    @4         "test"                                            │
│  0x0003    @9         ""                                                │
│                                                                          │
│ [Enter] inspect on heap  [Esc] back                                     │
└──────────────────────────────────────────────────────────────────────────┘
```

## Search / Navigation

| Key | Action |
|-----|--------|
| `/` | Search within current view (IL listing: search by opcode name or offset; heap: search by address or type name; statics: search by type name) |
| `g` | Go to IL offset (in IL listing view) |
| `G` | Go to heap address (opens heap inspector at that address) |
| `Tab` | Cycle focus between panes in the primary view |

## Determinism Features

Since the runtime is fully deterministic, the TUI should expose this.

**Step counter**: A monotonically-increasing global step counter displayed in the status bar. This identifies a unique point in the execution trace.

**Future: Time-travel controls** (to be designed when the infrastructure supports it):

| Key | Action |
|-----|--------|
| `P` | Step backward (reverse one instruction). Mnemonic: "previous". (`S` is unavailable: Statics overlay.) |
| `N` | Step back over |
| `O` | Step back out |

These would require snapshotting or replay from the beginning, but since the runtime is deterministic, replay from a saved snapshot + step count is always possible. The UI should be designed with space for these controls from the start.

**Execution log** (`l`): A scrollable log of the last N executed instructions across all threads:

```
┌─ Execution Log ──────────────────────────────────────────────────────────┐
│                                                                          │
│  Step  Thread  Method                          IL       Op              │
│  ────  ──────  ──────                          ──       ──              │
│  4214  T0      Program.Main                    IL_003E  Ldloc_0         │
│  4215  T0      Program.Main                    IL_003F  Callvirt        │
│  4216  T0      List<Int32>.get_Count           IL_0000  Nop             │
│  4217  T0      List<Int32>.get_Count           IL_0001  Ldarg_0         │
│                                                                          │
│ [Enter] jump to step  [f] filter by thread  [Esc] back                  │
└──────────────────────────────────────────────────────────────────────────┘
```

## Breakpoint Types

Beyond simple IL-offset breakpoints:

- **Address breakpoint**: Break when a specific heap address is read or written
- **Field breakpoint**: Break when a specific static field changes
- **Type init breakpoint**: Break when a specific type's `.cctor` begins
- **Condition breakpoint**: Break when the top of the eval stack matches a predicate (e.g. `top == Int32 0`)
- **Thread breakpoint**: Break when a specific thread is scheduled

These are managed via the breakpoint list (`B`).

## Key Binding Summary

| Key | Context | Action |
|-----|---------|--------|
| `s` | Primary | Step into |
| `n` | Primary | Step over |
| `o` | Primary | Step out |
| `r` | Primary | Run to breakpoint/end |
| `Ctrl+r` | Primary | Run all threads |
| `b` | IL listing | Toggle breakpoint |
| `B` | Any | Breakpoint list |
| `t` | Any | Threads overlay |
| `h` | Any | Heap inspector |
| `S` | Any | Statics overlay |
| `a` | Any | Assemblies overlay |
| `T` | Any | Concrete types |
| `i` | Any | Interned strings |
| `l` | Any | Execution log |
| `e` | Any | Exception detail (when active) |
| `[` / `]` | Primary | Prev/next thread |
| `↑` / `↓` | Call stack | Select frame |
| `Tab` | Primary | Cycle pane focus |
| `/` | Any | Search within view |
| `g` | IL listing | Go to offset |
| `G` | Any | Go to heap address |
| `Esc` / `q` | Overlay | Close overlay |
| `?` | Any | Help |

## Data Flow

The TUI is a pure function of `IlMachineState` plus a small amount of UI-local state (selected thread, selected frame, scroll positions, breakpoints, search filters). On each step:

1. User presses a step key.
2. TUI calls the appropriate execution function on the `IlMachineState`, receiving back an `ExecutionResult`.
3. TUI pattern-matches on the result:
   - `Stepped (newState, Executed)` -- normal advance.
   - `Stepped (newState, SuspendedForClassInit)` -- show in status bar.
   - `Stepped (newState, BlockedOnClassInit threadId)` -- show in status bar, maybe auto-switch to blocking thread.
   - `Terminated (newState, threadId)` -- show termination message.
4. After each step, the TUI inspects `IlMachineState` for breakpoint hits (comparing current thread/offset against the breakpoint list) and exception state (`ExceptionContinuation = PropagatingException`). These are not separate `ExecutionResult` cases; they are derived from the post-step machine state.
5. The new `IlMachineState` replaces the old one; the TUI re-renders.

The TUI state itself is a record:

```fsharp
{ MachineState : IlMachineState
  SelectedThread : ThreadId
  SelectedFrame : int
  StepCount : uint64
  Breakpoints : Breakpoint list
  View : ViewState  // which overlay is open, scroll positions, search text, etc.
  ExecutionLog : (uint64 * ThreadId * string * int * IlOp) list  // most recent N entries; oldest dropped on overflow
}
```

All of this is immutable. Each user interaction produces a new TUI state. This means the TUI itself is deterministic and could in principle be snapshot/restored alongside the machine state.
