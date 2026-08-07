namespace WoofWare.PawPrint

open System
open System.IO
open System.Text.Json

/// The properties a runtime host seeds `System.AppContext` with before any guest code runs.
///
/// On a real runtime these come from the app's `runtimeconfig.json`: `hostpolicy` reads
/// `runtimeOptions.configProperties` and hands the result to `AppContext.Setup` as a pair of
/// `char**` arrays. By the time managed code can see them every value is a string, whatever
/// its JSON type was — `AppContext.GetData` returns `object`, but the only thing ever stored
/// is a string. That is why this is a `Map<string, string>` and not a map to some JSON value
/// type: the string *is* the property, and the JSON type only decides which string.
///
/// Construct one with `RuntimeConfig.parse` (which applies hostpolicy's mapping) or with
/// `AppContextProperties.ofMap` (for a host that knows its own mind, chiefly tests). The
/// representation is private so that the only way to get one from a config file is through
/// the parser.
///
/// Invariant: no name or value contains an interior NUL. hostpolicy stores both in a
/// `pal::string_t` assigned from a `char_t*`, which truncates there, so a NUL is not a
/// character a property can contain — and the difference is observable, because two names
/// that differ only after a NUL are *one* property to a real host.
type AppContextProperties =
    private
        {
            Values : Map<string, string>
        }

    override this.ToString () : string =
        this.Values
        |> Map.toSeq
        |> Seq.map (fun (k, v) -> $"%s{k}=%s{v}")
        |> String.concat ", "
        |> sprintf "{%s}"

[<RequireQualifiedAccess>]
module AppContextProperties =

    /// No properties at all.
    ///
    /// Note that this is *not* the same as "what a real .NET app starts with": a real host
    /// always seeds at least `TRUSTED_PLATFORM_ASSEMBLIES` and friends. It is PawPrint's
    /// honest default — there is no host filesystem layout to derive those from — and it is
    /// what a guest sees when nobody supplied a `runtimeconfig.json`.
    let empty : AppContextProperties =
        {
            Values = Map.empty
        }

    /// Truncate at the first NUL, as hostpolicy's `pal::string_t` assignment from a
    /// `char_t*` does.
    let internal truncateAtNul (s : string) : string =
        match s.IndexOf '\000' with
        | -1 -> s
        | i -> s.Substring (0, i)

    /// Build a property set directly, applying the same NUL truncation a real host would.
    ///
    /// Two names that collide only after truncation are rejected rather than silently
    /// merged: from a `Map` there is no document order to say which should win, so the
    /// caller's intent is genuinely ambiguous. `RuntimeConfig.parse` does not go through
    /// here — it has document order, and resolves such a collision last-wins.
    let ofMap (values : Map<string, string>) : AppContextProperties =
        let truncated =
            values
            |> Map.toList
            |> List.map (fun (k, v) -> truncateAtNul k, truncateAtNul v)

        let distinctNames = truncated |> List.map fst |> List.distinct

        if distinctNames.Length <> truncated.Length then
            let collisions =
                truncated
                |> List.countBy fst
                |> List.filter (fun (_, n) -> n > 1)
                |> List.map fst

            failwith
                $"AppContextProperties.ofMap: names %A{collisions} are distinct only after an interior NUL, which a real host truncates at, so they would be a single AppContext property. Supply one value per truncated name."

        {
            Values = Map.ofList truncated
        }

    let toMap (properties : AppContextProperties) : Map<string, string> = properties.Values

    let isEmpty (properties : AppContextProperties) : bool = Map.isEmpty properties.Values

[<RequireQualifiedAccess>]
module RuntimeConfig =

    /// The path the SDK emits an assembly's runtime configuration to: the assembly's own
    /// path with its extension replaced by `.runtimeconfig.json`. Pure, so that the caller
    /// that actually touches the filesystem stays a one-liner and this convention stays
    /// testable.
    let pathForAssembly (assemblyPath : string) : string =
        Path.ChangeExtension (assemblyPath, ".runtimeconfig.json")

    /// Render one `configProperties` value the way `hostpolicy` does.
    ///
    /// `runtime_config.cpp` takes `GetString()` for a JSON string and otherwise re-serialises
    /// the value with a `rapidjson::Writer`. We reproduce that exactly for strings, booleans,
    /// integers and `null`, and refuse everything else — see `refusal` below for why.
    let private renderValue (name : string) (value : JsonElement) : Result<string, string> =
        let refusal (kind : string) (wouldBe : string) : Result<string, string> =
            Error
                $"runtimeconfig.json property '%s{name}' has a %s{kind} value, which PawPrint does not know how to convert to an AppContext property string. A real host re-serialises it with rapidjson's Writer (%s{wouldBe}); reproducing that formatting exactly is not yet implemented, and seeding an approximation would silently give the guest a different value from the one it would see on CoreCLR. Remove the property, or express it as a string, boolean or integer."

        match value.ValueKind with
        | JsonValueKind.String ->
            // `GetString()` decodes the escapes, so the guest sees the value, not its JSON
            // spelling. The caller truncates at the first NUL, matching where hostpolicy
            // assigns this `char_t*` into a `pal::string_t`.
            match value.GetString () with
            | null ->
                // Unreachable: `GetString` only returns null for JsonValueKind.Null.
                Error $"logic error: runtimeconfig.json property '%s{name}' reported kind String but decoded to null"
            | s -> Ok s
        | JsonValueKind.True -> Ok "true"
        | JsonValueKind.False -> Ok "false"
        | JsonValueKind.Null -> Ok "null"
        | JsonValueKind.Number ->
            // rapidjson keeps a number as an integer only when it was written without a
            // fraction or an exponent *and* it fits in int64/uint64; otherwise it holds a
            // double and the Writer emits Grisu2 + `dtoa.h`'s `Prettify`. Classify on the
            // source spelling first, because `5.0` is a double to rapidjson however integral
            // its value is.
            let raw = value.GetRawText ()

            let looksIntegral = not (raw.Contains '.' || raw.Contains 'e' || raw.Contains 'E')

            if not looksIntegral then
                refusal "non-integer numeric" "e.g. 1e2 becomes 100.0, and 5.0 stays 5.0"
            else

            // `-0` is an integer to rapidjson, which has no negative zero, so it renders as
            // `0`. Going through int64/uint64 gets that canonicalisation for free.
            match value.TryGetInt64 () with
            | true, i -> Ok (string<int64> i)
            | false, _ ->
                match value.TryGetUInt64 () with
                | true, u -> Ok (string<uint64> u)
                | false, _ ->
                    // Wider than uint64, so rapidjson stores it as a double after all.
                    refusal
                        "integer-shaped but out-of-range numeric"
                        "it exceeds int64/uint64, so a real host holds it as a double: e.g. 99999999999999999999 becomes 100000000000000000000.0"
        | JsonValueKind.Array -> refusal "array" """e.g. [1, "two"] becomes [1,"two"]"""
        | JsonValueKind.Object -> refusal "object" """e.g. {"a": 1} becomes {"a":1}"""
        | kind -> Error $"runtimeconfig.json property '%s{name}' has unexpected JSON kind %O{kind}"

    /// The *first* member of `obj` with this name.
    ///
    /// First, not last, because that is what rapidjson's `FindMember` returns, and duplicate
    /// members are legal JSON that neither parser rejects. `JsonElement.TryGetProperty`
    /// returns the *last*, so using it here would silently disagree with a real host about
    /// which `runtimeOptions` block is in force.
    let private tryFindFirstMember (name : string) (obj : JsonElement) : JsonElement option =
        obj.EnumerateObject ()
        |> Seq.tryPick (fun property -> if property.Name = name then Some property.Value else None)

    /// Read the AppContext properties out of the contents of a `runtimeconfig.json`.
    ///
    /// Pure: the caller supplies the text, because a library that read the host filesystem
    /// would make a replay depend on the machine that produced it (and the test harness
    /// compiles its guests straight to a `MemoryStream`, so there is no file to read).
    ///
    /// Mirrors `runtime_config_t::ensure_parsed`/`parse_opts`. A file that exists but has no
    /// `runtimeOptions` is *invalid* there (`ensure_parsed` falls through to `return false`),
    /// so it is an error here too; a *missing* file is not an error, but that is the caller's
    /// distinction to make and `HostRuntimeConfig` makes it. `"runtimeOptions": null` is
    /// explicitly accepted as "no options". Absent `configProperties` simply means no
    /// properties.
    ///
    /// Malformed JSON, a section of the wrong shape, and a value whose hostpolicy rendering
    /// we do not reproduce are all errors: a misconfigured file should be loud, not silently
    /// equivalent to an empty one.
    let parse (contents : string) : Result<AppContextProperties, string> =
        let options =
            JsonDocumentOptions (
                // The host parses with `kParseCommentsFlag` (json_parser.cpp), so a config
                // with comments is one a real runtime accepts. It does *not* pass
                // `kParseTrailingCommasFlag`, and neither do we (that is the default).
                CommentHandling = JsonCommentHandling.Skip
            )

        let doc =
            try
                Ok (JsonDocument.Parse (contents, options))
            with :? JsonException as e ->
                Error $"could not parse runtimeconfig.json: %s{e.Message}"

        match doc with
        | Error e -> Error e
        | Ok doc ->

        use doc = doc

        if doc.RootElement.ValueKind <> JsonValueKind.Object then
            Error $"runtimeconfig.json must have a JSON object at its root, but found %O{doc.RootElement.ValueKind}"
        else

        match tryFindFirstMember "runtimeOptions" doc.RootElement with
        | None ->
            Error
                "runtimeconfig.json has no 'runtimeOptions' section, which a real host treats as an invalid configuration (runtime_config.cpp, ensure_parsed). An absent runtimeconfig.json is fine; a present one must have the section."
        | Some runtimeOptions ->

        // `parse_opts` returns success immediately for a null value, so an explicit
        // `"runtimeOptions": null` is a valid config that supplies nothing.
        if runtimeOptions.ValueKind = JsonValueKind.Null then
            Ok AppContextProperties.empty
        else if runtimeOptions.ValueKind <> JsonValueKind.Object then
            Error
                $"runtimeconfig.json 'runtimeOptions' must be an object or null, but found %O{runtimeOptions.ValueKind}"
        else

        match tryFindFirstMember "configProperties" runtimeOptions with
        | None -> Ok AppContextProperties.empty
        | Some configProperties ->

        if configProperties.ValueKind <> JsonValueKind.Object then
            // hostpolicy calls `GetObject()` on this without checking, which is undefined
            // behaviour for any other kind. There is no behaviour to match, so refuse.
            Error
                $"runtimeconfig.json 'runtimeOptions.configProperties' must be an object, but found %O{configProperties.ValueKind}"
        else

        // Resolve duplicates *before* rendering, in two passes, because hostpolicy's loop
        // body is `m_properties[name] = render(value)`: it renders every occurrence but only
        // the last survives. A single fold that rendered as it went would reject a file whose
        // *effective* value is perfectly renderable, e.g. `"P": 1.5, "P": "final"`.
        //
        // The name is truncated at its first NUL before being used as a key, because that is
        // where hostpolicy's `pal::string_t` assignment truncates — so two names differing
        // only after a NUL really are one property, and the later wins rather than colliding.
        let effective =
            (Map.empty, configProperties.EnumerateObject ())
            ||> Seq.fold (fun acc property ->
                Map.add (AppContextProperties.truncateAtNul property.Name) property.Value acc
            )

        (Ok Map.empty, Map.toSeq effective)
        ||> Seq.fold (fun acc (name, value) ->
            match acc with
            | Error _ -> acc
            | Ok acc ->
                renderValue name value
                |> Result.map (fun rendered -> Map.add name (AppContextProperties.truncateAtNul rendered) acc)
        )
        |> Result.map (fun values ->
            // Already truncated and deduplicated above, so `ofMap`'s collision check cannot
            // fire here; going through it keeps the invariant in one place.
            AppContextProperties.ofMap values
        )
