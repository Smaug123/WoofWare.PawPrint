# PawPrint Local Loki Logging

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
