---
name: pawprint-debugger-server
description: Use when debugging WoofWare.PawPrint guest execution interactively through the WoofWare.PawPrint.App HTTP debugger server, especially for infinite loops, scheduler/thread-state inspection, step-by-step IL execution, heap/object inspection, or when an agent needs to query PawPrint's state instead of running the interpreter to completion.
---

# PawPrint Debugger Server

## Overview

Use PawPrint's app-level HTTP debugger server when normal `Program.run` or `dotnet test` would run too far, hang, or hide the state needed to diagnose a guest-runtime bug. The server owns one mutable debugger session and exposes bounded stepping plus read-only snapshots of threads, frames, stacks, loaded assemblies, and heap objects.

## Start The Server

Build or publish the guest DLL first. For the repo's playground:

```bash
nix develop -c dotnet publish --self-contained --configuration Release --runtime osx-arm64 CSharpExample/
```

Start the debugger. It always binds `127.0.0.1` on an ephemeral port and prints both the selected URL and a bearer token to stdout.

```bash
nix develop -c dotnet run --project WoofWare.PawPrint.App/WoofWare.PawPrint.App.fsproj -- \
  --debug-server \
  CSharpExample/bin/Release/net9.0/osx-arm64/publish/CSharpExample.dll
```

For a test source, compile or publish the relevant DLL using the existing test/playground flow rather than modifying the debugger. Use the printed URL and token for all requests.

### Pure-Test Source Workflow

For a `WoofWare.PawPrint.Test/sourcesPure/*.cs` case, prefer compiling through the test project's Roslyn helper instead of making a temporary SDK project. The pure-test harness uses `Roslyn.compile`, which produces `PawPrintTestAssembly`; an SDK-published project can produce different IL and fail on an unrelated path.

One practical way to do this from the repo root is an F# script like:

```fsharp
#I "WoofWare.PawPrint.Test/bin/Debug/net9.0"
#r "WoofWare.PawPrint.Test.dll"

open System.IO
open WoofWare.PawPrint.Test

let source = File.ReadAllText "WoofWare.PawPrint.Test/sourcesPure/CastclassFailures.cs"
File.WriteAllBytes ("/tmp/pawprint-debug/CastclassFailures.dll", Roslyn.compile [ source ])
```

`WoofWare.PawPrint.App` currently asks `DotnetRuntimeLocator` for runtime paths from the DLL path, so a standalone Roslyn DLL also needs a sibling `*.runtimeconfig.json`. A minimal net9 config is enough:

```json
{
  "runtimeOptions": {
    "tfm": "net9.0",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "9.0.0"
    }
  }
}
```

## Query Endpoints

Use bounded operations. Do not call an unbounded run loop against a suspected infinite loop.

```bash
curl -s -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/state
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' 'http://127.0.0.1:PORT/step?count=1'
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' 'http://127.0.0.1:PORT/run?maxSteps=10000'
curl -s -H 'Authorization: Bearer TOKEN' -H 'Content-Type: application/json' -X POST --data '{"maxSteps":10000,"until":{"kind":"anyThread","condition":{"kind":"repeatedActiveLocation","repeatCount":3}}}' http://127.0.0.1:PORT/run-until
curl -s -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/thread/0
curl -s -H 'Authorization: Bearer TOKEN' 'http://127.0.0.1:PORT/thread/0/stack-summary?edgeFrames=12&topMethods=8'
curl -s -H 'Authorization: Bearer TOKEN' 'http://127.0.0.1:PORT/thread/0/active-method/il?context=8'
curl -s -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/heap/1
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' http://127.0.0.1:PORT/reset
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' http://127.0.0.1:PORT/stop
```

Endpoint summary:

- `GET /state`: session status, step count, loaded assemblies, thread summaries, heap counts.
- `POST /step?count=N`: execute up to `N` scheduler steps and return each event plus the new summary.
- `POST /run?maxSteps=N`: execute at most `N` steps and return recent events. Use this to move forward safely, not to prove termination. If `/stop` cancels an active run, the response includes `cancelled: true`.
- `POST /run-until`: execute until a bounded predicate tree matches, the session finishes/deadlocks, `/stop` cancels the run, or `maxSteps` is reached. The JSON body has `maxSteps`, optional `recordLimit`, and `until`. Predicate trees support `and`, `or`, `not`, `thread`, `anyThread`, `sessionStatusChanged`, `activeMethodMatches`, `frameDepthAtLeast`, `threadStatusChanged`, and `repeatedActiveLocation`; `repeatedActiveLocation` counts repeated visits to the same `(thread, method, IL offset, instruction)` tuple during that bounded run.
- `GET /thread/{id}`: full frame list for a thread, including active frame, IL offset, current instruction, eval stack, args, and locals.
- `GET /thread/{id}/stack-summary`: compact stack summary for deep stacks. Optional query parameters: `edgeFrames` (default 12, max 100) and `topMethods` (default 8, max 100).
- `GET /thread/{id}/active-method/il`: IL for the thread's active frame, including resolved metadata-token text and the active instruction. Optional query parameter: `context` instructions before/after the active offset (omitted means full method, max 500).
- `GET /heap/{address}`: inspect an object or array at a managed heap address. Use structured `objectAddress` fields from stack, argument, local, and array-element values when available.
- `POST /reset`: recreate the debugger session from the original DLL and arguments.
- `POST /stop`: stop the server cleanly.

The sandbox may block localhost HTTP calls. If `curl` fails with `Operation not permitted`, rerun the exact same command with escalated permissions; do not request a persisted broad `curl` prefix rule.

For empty POST requests, include `-d ''` so `curl` sends `Content-Length: 0`.

## Debugging Workflow

1. Start with `/state` to record loaded assemblies, runnable/blocked threads, active frame, and current step count.
2. Use `/step?count=1` until you understand which instruction or class-initialization transition is happening.
3. Once the pattern is clear, use `/run?maxSteps=N` with a modest bound such as `100`, `1000`, or `10000`, or use `/run-until` when you can state the stopping condition as a predicate.
4. If execution repeats, prefer `/run-until` with `repeatedActiveLocation` or `frameDepthAtLeast` before querying `/thread/{id}/stack-summary`. Compare `frameCount`, `topMethods`, edge frames, and the active frame summary. Use `/thread/{id}` only when you need full eval stacks, args, or locals.
5. Follow heap references with `/heap/{address}` when stack/locals include an `objectAddress` field.
6. Use `/reset` before trying a different stepping strategy on the same DLL.
7. Use `/stop` before ending the task unless the user explicitly asked to leave the server running.

For very deep stacks, prefer `/thread/{id}/stack-summary`. If you need full frame details anyway, `/thread/{id}` can be enormous; save it to a temp file and summarize with `jq` instead of dumping it into the conversation:

```bash
curl -sS -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/thread/0 \
  -o /tmp/pawprint-thread.json

jq '{
  activeFrame,
  frameCount:(.frames|length),
  active:(.frames[]|select(.active)),
  topMethods:(.frames|group_by(.method)|map({method:.[0].method,count:length})|sort_by(-.count)[:8]),
  firstFrames:(.frames[:12]|map({id,method,ilOffset,instruction})),
  lastFrames:(.frames[-12:]|map({id,method,ilOffset,instruction}))
}' /tmp/pawprint-thread.json
```

You can inspect the IL body of the current method with `/thread/{id}/active-method/il?context=8` (which gets 8 instructions before and after the current instruction).
For more complex IL disassembly tasks, use the standalone WoofWare.PawPrint.IlDump tool.

When the active method is a JIT intrinsic, use `/run-until` to stop at the first repeated active location, then inspect `/thread/{id}/active-method/il?context=8`. Some CoreLib intrinsic stubs are intentionally self-recursive because the real JIT replaces them. If PawPrint identifies the method as intrinsic but `Intrinsics.call` returns `None`, falling back to that IL can create an infinite stack-growth loop. A repeated tuple like `(thread 0, System.Private.CoreLib.AdvSimd.get_IsSupported, IL_0000, UnaryMetadataToken.Call)` is an example: the IL body is `call AdvSimd.get_IsSupported; ret`.

Example predicate body for an intrinsic loop:

```json
{
  "maxSteps": 10000,
  "recordLimit": 20,
  "until": {
    "kind": "anyThread",
    "condition": {
      "kind": "or",
      "conditions": [
        { "kind": "repeatedActiveLocation", "repeatCount": 3 },
        { "kind": "frameDepthAtLeast", "depth": 100 },
        {
          "kind": "activeMethodMatches",
          "match": { "kind": "contains", "value": "AdvSimd.get_IsSupported" }
        }
      ]
    }
  }
}
```
## Interpreting Responses

`session.status` is one of:

- `running`: more scheduler steps may be available.
- `finished`: PawPrint reached `NormalExit`, `ProcessExit`, or guest unhandled exception; see `outcome`.
- `deadlocked`: no runnable threads remain; inspect `stuckThreads` and `/thread/{id}`.

Step events distinguish normal IL execution from scheduler-visible transitions:

- `instruction` with `Executed`: one instruction or native boundary completed.
- `instruction` with `SuspendedForClassInit`: PawPrint pushed a class constructor frame before continuing.
- `instruction` with `BlockedOnClassInit`: the selected thread is blocked behind another thread's class initialization. Use `blockedOnClassInitThread` to identify the blocker when present.
- `workerTerminated`: a non-entry thread reached its final `ret`.
- `completed`: the program has exited or thrown an unhandled guest exception.
- `deadlocked`: the scheduler cannot choose a runnable thread.

## Reporting Findings

When reporting a debugger session, include:

- The exact server command and DLL path.
- The printed server URL, but not the bearer token.
- The bounded commands used (`/step`, `/run`, `/thread`, `/heap`) and their important response fields.
- The first repeated `(thread, method, ilOffset, instruction)` tuple if diagnosing a loop.
- Any heap addresses followed and what they contained.
- Whether a bounded `/run` response reported `cancelled: true`.
- Whether the session was stopped with `/stop`.
