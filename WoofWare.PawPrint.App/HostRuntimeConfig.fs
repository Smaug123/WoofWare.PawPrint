namespace WoofWare.PawPrint

open System.IO

/// The CLI's half of the `runtimeconfig.json` story: the filesystem read.
///
/// The parsing lives in the library (`RuntimeConfig`), which is pure — a replay must not
/// depend on the machine that produced it, and the test harness compiles its guests straight
/// to a `MemoryStream`, where there is no sibling file to read at all. Touching the disk is
/// something only the host does, so it lives here.
[<RequireQualifiedAccess>]
module HostRuntimeConfig =

    /// Read the AppContext properties for a guest assembly from the `runtimeconfig.json` the
    /// SDK emits beside it, as `hostpolicy` does.
    ///
    /// A missing file yields no properties: PawPrint is routinely pointed at a bare dll, and
    /// `runtime_config_t::ensure_parsed` likewise treats "not existing" as success. A file
    /// that is present but invalid *is* an error, and throws — silently treating it as empty
    /// would drop the guest's feature switches and leave it running with quietly different
    /// behaviour. This present/absent split is exactly why `RuntimeConfig.parse` rejects a
    /// document with no `runtimeOptions` while this function tolerates no document at all.
    let forAssembly (dllPath : string) : AppContextProperties =
        let configPath = RuntimeConfig.pathForAssembly dllPath

        if not (File.Exists configPath) then
            AppContextProperties.empty
        else

        match RuntimeConfig.parse (File.ReadAllText configPath) with
        | Ok properties -> properties
        | Error e -> failwith $"Could not read %s{configPath}: %s{e}"
