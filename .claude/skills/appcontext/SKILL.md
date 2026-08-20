---
name: appcontext
description: How PawPrint seeds AppContext feature switches from runtimeconfig.json. Use when touching AppContextSeed.fs, RuntimeConfig.parse, or a BCL feature switch a guest reads through AppContext.GetData/TryGetSwitch.
---

# AppContext and `runtimeconfig.json`

BCL feature switches (`System.Diagnostics.Tracing.EventSource.IsSupported`, `System.Globalization.Invariant`, …) are read through `AppContext.GetData`/`TryGetSwitch`. Nothing in managed code populates that store: on CoreCLR the VM calls `AppContext.Setup(char**, char**, int)` from `CorHost2::CreateAppDomainWithManager`, with arrays `hostpolicy` built from `runtimeOptions.configProperties`.

PawPrint does the same in `AppContextSeed.fs`, called from `Program.prepare`.
The design has three properties:

- **The library never performs IO to read the file.** `RuntimeConfig.parse` is pure `byte[] -> Result<AppContextProperties, string>`; the filesystem read lives in the App (`HostRuntimeConfig.fs`). A library that read the host's disk would make a replay depend on the machine that produced it, and the test harness compiles guests straight to a `MemoryStream` where no sibling file exists. It takes bytes rather than a string because the encoding rules are part of hostpolicy's behaviour: `parse_file` mmaps the file, skips only a UTF-8 BOM, and parses the rest as UTF-8, so a UTF-16 config is one a real host refuses.
- **`Setup` runs CoreLib's own IL.** We synthesise only the two `char**` buffers, which is the host's job; the dictionary, the pointer walk and the `new string(char*)` calls are all CoreLib's. Do not be tempted to write `s_dataStore` directly or intercept the accessors.
- **Seeding precedes the entry type's `.cctor` pump**, not merely `Main`, because switches latch into `static readonly` fields on first read. `sourcesImpure/AppContextSeededBeforeCctor.cs` pins this.

What we seed is *only* `configProperties`. A real host also populates `TRUSTED_PLATFORM_ASSEMBLIES`, `APP_CONTEXT_BASE_DIRECTORY` and seven more from deps resolution and its filesystem layout, which PawPrint has neither of; that gap is recorded in [docs/divergences.md](../../../docs/divergences.md). Do not be tempted to close it by synthesising plausible values.

Values that are reals, arrays or objects are *refused* rather than approximated: `hostpolicy` renders them with rapidjson's `Writer` (Grisu2 plus `dtoa.h`'s `Prettify`, so `1e2` becomes `100.0`), which PawPrint does not reproduce. Both files are in the pinned runtime source under `$DOTNET_RUNTIME_SRC/src/native/`.

