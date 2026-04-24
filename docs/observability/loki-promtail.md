# PawPrint Local Loki Logging

Promtail is EOL as of March 2, 2026; Grafana now recommends Alloy for new setups. This repo still includes a Promtail example because it is useful for a local Loki stack and matches the current logging shape.

Enable JSON-lines file logging by setting `PAWPRINT_LOG_DIR`:

```bash
env PAWPRINT_LOG_DIR=/tmp/pawprint-logs PAWPRINT_LOG_LEVEL=Debug \
  nix develop -c dotnet test WoofWare.PawPrint.Test/WoofWare.PawPrint.Test.fsproj --verbosity normal
```

The directory named by `PAWPRINT_LOG_DIR` is only a root. PawPrint creates a fresh run directory under it for each process:

```text
/tmp/pawprint-logs/<utc timestamp>-pid<pid>-<guid>/*.jsonl
```

Each logger sink writes to a GUID-suffixed file opened with `CreateNew`, so concurrent test processes and NUnit-parallel tests do not reuse or truncate each other's files. `PAWPRINT_LOG_RUN_ID` is recorded as `user_run_id` inside each event, but it is not used for physical paths.

Use `docs/observability/promtail.yaml` with a local Promtail process configured to send to `http://localhost:3100/loki/api/v1/push`. The sample config labels only `component` and `level`; `run_id`, `user_run_id`, and `logger` are sent as structured metadata to avoid high-cardinality labels.
