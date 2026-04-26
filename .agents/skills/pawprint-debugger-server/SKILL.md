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

## Query Endpoints

Use bounded operations. Do not call an unbounded run loop against a suspected infinite loop.

```bash
curl -s -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/state
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' 'http://127.0.0.1:PORT/step?count=1'
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' 'http://127.0.0.1:PORT/run?maxSteps=10000'
curl -s -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/thread/0
curl -s -H 'Authorization: Bearer TOKEN' http://127.0.0.1:PORT/heap/1
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' http://127.0.0.1:PORT/reset
curl -s -H 'Authorization: Bearer TOKEN' -X POST -d '' http://127.0.0.1:PORT/stop
```

Endpoint summary:

- `GET /state`: session status, step count, loaded assemblies, thread summaries, heap counts.
- `POST /step?count=N`: execute up to `N` scheduler steps and return each event plus the new summary.
- `POST /run?maxSteps=N`: execute at most `N` steps and return recent events. Use this to move forward safely, not to prove termination.
- `GET /thread/{id}`: full frame list for a thread, including active frame, IL offset, current instruction, eval stack, args, and locals.
- `GET /heap/{address}`: inspect an object or array at a managed heap address. Use object addresses printed as `<object #N>` by stack/field values.
- `POST /reset`: recreate the debugger session from the original DLL and arguments.
- `POST /stop`: stop the server cleanly.

The sandbox may block localhost HTTP calls. If `curl` fails with `Operation not permitted`, rerun the exact same command with escalated permissions; do not request a persisted broad `curl` prefix rule.

For empty POST requests, include `-d ''` so `curl` sends `Content-Length: 0`.

## Debugging Workflow

1. Start with `/state` to record loaded assemblies, runnable/blocked threads, active frame, and current step count.
2. Use `/step?count=1` until you understand which instruction or class-initialization transition is happening.
3. Once the pattern is clear, use `/run?maxSteps=N` with a modest bound such as `100`, `1000`, or `10000`.
4. If execution repeats, query `/thread/{id}` before and after a bounded run. Compare `method`, `ilOffset`, `instruction`, stack depth, locals, and active frame.
5. Follow heap references with `/heap/{address}` when stack/locals show `ObjectRef(<object #N>)`.
6. Use `/reset` before trying a different stepping strategy on the same DLL.
7. Use `/stop` before ending the task unless the user explicitly asked to leave the server running.

## Interpreting Responses

`session.status` is one of:

- `running`: more scheduler steps may be available.
- `finished`: PawPrint reached `NormalExit`, `ProcessExit`, or guest unhandled exception; see `outcome`.
- `deadlocked`: no runnable threads remain; inspect `stuckThreads` and `/thread/{id}`.

Step events distinguish normal IL execution from scheduler-visible transitions:

- `instruction` with `Executed`: one instruction or native boundary completed.
- `instruction` with `SuspendedForClassInit`: PawPrint pushed a class constructor frame before continuing.
- `instruction` with `BlockedOnClassInit`: the selected thread is blocked behind another thread's class initialization.
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
- Whether the session was stopped with `/stop`.
