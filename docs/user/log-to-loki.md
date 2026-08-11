# PawPrint Local Loki Logging

*Authorship: LLM*

Enable JSON-lines file logging by setting `PAWPRINT_LOG_DIR`:

```bash
env PAWPRINT_LOG_DIR=/tmp/pawprint-logs PAWPRINT_LOG_LEVEL=Debug \
  nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal
```

The directory named by `PAWPRINT_LOG_DIR` is only a root. PawPrint creates a fresh run directory under it when `LoggingConfig.fromEnv` is called:

```text
/tmp/pawprint-logs/<utc timestamp>-pid<pid>-<guid>/*.jsonl
```

The app and end-to-end test logger construct their logging config once and reuse it, so all sinks created by a single process share one generated run directory. Callers that invoke `LoggingConfig.fromEnv` multiple times get a fresh run directory for each returned config.

Each logger sink writes to a GUID-suffixed file opened with `CreateNew`, so concurrent test processes and NUnit-parallel tests do not reuse or truncate each other's files. The sink opens the file only while appending an event, so an undisposed test logger factory does not pin a file handle. `PAWPRINT_LOG_RUN_ID` is recorded as `user_run_id` inside each event, but it is not used for physical paths.

Use `docs/observability/alloy.river` with a local Alloy process configured to send to `http://localhost:3100/loki/api/v1/push`. The sample config labels only `component` and `level`; `run_id`, `user_run_id`, and `logger` are sent as structured metadata to avoid high-cardinality labels. It leaves the original JSON event as the Loki log line, so query-time parsing can still inspect `fields` and `properties`. PawPrint-owned fields are top-level; caller-supplied static properties such as `source_file` and `entry_assembly` are nested under `properties` so they cannot collide with reserved event fields such as `level` or `message`.

The sample Alloy config assumes `PAWPRINT_LOG_DIR=/tmp/pawprint-logs`. If you use a different root, update the `__path__` glob in `docs/observability/alloy.river` to match.

## Guest source locations in the instruction trace

At `PAWPRINT_LOG_LEVEL=Trace` the interpreter emits one event per interpreted IL instruction. When the executing method's assembly was built with debug information, that event also carries the source the compiler attributed to the instruction:

```json
{"message": "Executing one step (index 7, max 12, in method Sut.Triple at File0.cs:5): ...",
 "fields": {"ExecutingIlOpIndex": 7, "MaxIlOpIndex": 12, "SourceFile": "File0.cs", "SourceLine": 5}}
```

(abridged: the event also carries the executing method's type and name, and the decoded instruction)

`SourceLine` is a number, so ranges work: `jq 'select(.fields.SourceLine >= 40 and .fields.SourceLine <= 50)'`.

Most steps have neither field. That is the ordinary case rather than a failure: the shared framework ships without PDBs, so every instruction inside it — the overwhelming majority of any run — has no source to name, and those events use a message template without the two holes. Filter on `message_template` if you want to separate the two.

`SourceFile` is the document path exactly as the compiler recorded it, which names paths on whichever machine built the assembly.
