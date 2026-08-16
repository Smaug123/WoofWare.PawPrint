namespace WoofWare.PawPrint

open System.IO

/// The CLI's half of the `runtimeconfig.json` story: the filesystem reads.
///
/// The parsing lives in the library (`RuntimeConfig`), which is pure — a replay must not
/// depend on the machine that produced it, and the test harness compiles its guests straight
/// to a `MemoryStream`, where there is no sibling file to read at all. Touching the disk is
/// something only the host does, so it lives here.
[<RequireQualifiedAccess>]
module HostRuntimeConfig =

    /// Properties from `<assembly>.runtimeconfig.dev.json`, which hostpolicy reads *before*
    /// the main config and lets the main config override.
    ///
    /// A failure *hostpolicy itself* would shrug off yields no properties rather than an
    /// error, matching `ensure_dev_config_parsed`: a missing file is `return true`, a file
    /// that will not parse makes `ensure_parsed` emit a verbose trace and carry on, and a
    /// `runtimeOptions` of the wrong shape has `parse_opts`' return value discarded at the
    /// call site. Hostpolicy draws no distinction between a file it cannot mmap and one it
    /// cannot parse — both are `parse_file` returning false — so neither do we.
    ///
    /// That does *not* cover a file hostpolicy reads happily and PawPrint cannot reproduce
    /// (`RuntimeConfigError.NotReproducible`); a real launch acts on such a file, so
    /// dropping it would silently change the guest's property set.
    let private devPropertiesFor (dllPath : string) : AppContextProperties =
        let devPath = RuntimeConfig.devPathForAssembly dllPath

        let contents =
            try
                if File.Exists devPath then
                    Some (File.ReadAllBytes devPath)
                else
                    None
            with _ ->
                // Catch everything: no way of failing to obtain these bytes is fatal, and an
                // enumeration of exception types is a list to get wrong. `File.Exists` above
                // is a check-then-use, so the file may also have vanished in between.
                None

        match contents with
        | None -> AppContextProperties.empty
        | Some bytes ->

        match RuntimeConfig.parse bytes with
        | Ok properties -> properties
        | Error (RuntimeConfigError.HostWouldReject _) -> AppContextProperties.empty
        | Error (RuntimeConfigError.NotReproducible message) ->
            // Only hostpolicy's own failures are ignorable here, and this is not one of them:
            // a real host reads this file and acts on it, so dropping it would launch the
            // guest with a different property set (and would hide the duplicate-name check,
            // since a dev sidecar naming a host-owned property is fatal to a real launch).
            failwith $"Could not read %s{devPath}: %s{message}"

    /// Read the AppContext properties for a guest assembly from the `runtimeconfig.json` the
    /// SDK emits beside it, as `hostpolicy` does.
    ///
    /// A missing main config yields no properties, because PawPrint is routinely pointed at a
    /// bare dll. `runtime_config_t::ensure_parsed` also treats "not existing" as success, but
    /// do not read that as agreement: a real launch then finds no framework reference, decides
    /// the app is self-contained, and dies looking for `hostpolicy` beside it, so no guest ever
    /// observes the no-config case. This is a deliberate divergence, recorded in
    /// docs/divergences.md, not a match.
    /// A file that is present but invalid *is* an error, and throws — silently treating it as
    /// empty would drop the guest's feature switches and leave it running with quietly
    /// different behaviour. This present/absent split is exactly why `RuntimeConfig.parse`
    /// rejects a document with no `runtimeOptions` while this function tolerates no document
    /// at all.
    let forAssembly (dllPath : string) : AppContextProperties =
        let configPath = RuntimeConfig.pathForAssembly dllPath

        // Attempt the read and treat only a genuine absence as absence, because that is the
        // only case hostpolicy forgives: `ensure_parsed` calls `pal::fullpath`, which is
        // `realpath` and returns false *just* for ENOENT, and anything that resolves goes on
        // to `parse_file`. So a path that exists but is not a readable file — a directory of
        // that name, most obviously — is one a real host resolves, fails to mmap, and refuses
        // to launch on. `File.Exists` would call it a missing sidecar and start the guest with
        // no properties at all.
        let contents =
            try
                Some (File.ReadAllBytes configPath)
            with
            | :? FileNotFoundException
            | :? DirectoryNotFoundException -> None

        let main =
            match contents with
            | None -> AppContextProperties.empty
            | Some bytes ->

            match RuntimeConfig.parse bytes with
            | Ok properties -> properties
            | Error e -> failwith $"Could not read %s{configPath}: %s{e.Message}"

        // Through `combine` rather than `overlay`, because the hosting layer's names are only
        // detectable once both files are in hand: a real host merges them into one property
        // set and finds the duplicate there. That is also why a host-owned name in the dev
        // sidecar is fatal despite this function ignoring that file's *parse* failures — the
        // real host keeps such a property and dies on it rather than dropping it.
        match RuntimeConfig.combine (devPropertiesFor dllPath) main with
        | Ok properties -> properties
        | Error e -> failwith $"Could not use the runtime configuration beside %s{dllPath}: %s{e.Message}"
