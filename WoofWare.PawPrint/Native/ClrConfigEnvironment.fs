namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// How CoreCLR's `CLRConfig` (`coreclr/utilcode/clrconfig.cpp`) reads a
/// `DOTNET_`/`COMPlus_` configuration knob out of the process environment,
/// which is not the same as looking the variable up: see `tryGetValue`.
[<RequireQualifiedAccess>]
module ClrConfigEnvironment =
    /// Whether `strtoul` skips `c` as leading whitespace: `isspace` in the C
    /// locale, which is exactly these six characters.
    ///
    /// Not `Char.IsWhiteSpace`, which also accepts U+00A0 and friends: on Unix a
    /// knob's value reaches `strtoul` as UTF-8 bytes, so a no-break space is the
    /// two bytes 0xC2 0xA0 and stops the parse rather than being skipped.
    /// Measured on both kernels with U+00A0 and U+2003 ahead of a
    /// `DisableConfigCache` value.
    let isCLocaleSpace (c : char) : bool =
        c = ' ' || c = '\t' || c = '\n' || c = '\011' || c = '\012' || c = '\r'

    /// Parse a CLRConfig DWORD env-var value the way CoreCLR does for
    /// `EnableEventLog` — `u16_strtoul(val, &endPtr, 16)` with the
    /// success condition `errno != ERANGE && endPtr != val` (see
    /// `GetConfigDWORD` in `clrconfig.cpp:228`). The radix is 16
    /// because `EnableEventLog` is declared via `RETAIL_CONFIG_DWORD_INFO`
    /// with no `ParseIntegerAsBase10` flag (`clrconfigvalues.h:580`).
    ///
    /// The Unix PAL's `PAL_wcstoul` (`pal/src/cruntime/wchar.cpp:281–324`)
    /// is a thin wrapper around glibc `strtoul`, which on a 64-bit host
    /// works in `unsigned long` (64-bit). On `HOST_64BIT` the PAL post-
    /// processes the result: if `strtoul` returned > UINT32_MAX and the
    /// input was *positive*, it clamps to `UINT32_MAX` and sets
    /// `errno = ERANGE`; if the input was *negative*, it leaves the
    /// value untouched and lets the final `(ULONG)res` cast truncate to
    /// the low 32 bits (because that mirrors Windows' 32-bit `long`
    /// behaviour). This means a guest setting
    /// `DOTNET_EnableEventLog=-100000001` reads as enabled on real
    /// CoreCLR — the 64-bit two's-complement wrap leaves the low 32
    /// bits at `0xFFFFFFFF`, non-zero.
    ///
    /// We return `None` in CoreCLR's two failure arms only:
    ///   * `endPtr == val` — no digits were consumed.
    ///   * `errno == ERANGE` — either the magnitude exceeded `uint64`
    ///     (`strtoul` itself sets ERANGE, regardless of sign) or the
    ///     magnitude fit in 64 bits but was positive and exceeded
    ///     `UINT32_MAX` (PAL's HOST_64BIT post-processing arm). The
    ///     caller treats `None` as the default `0`, i.e. disabled.
    ///
    /// `IsEventSourceLoggingEnabled` only asks whether the parsed value
    /// is non-zero, but the parser is shaped to surface the full DWORD
    /// so future knobs that care about the numeric magnitude can reuse it.
    ///
    let tryParseDword (raw : string) : uint32 option =
        let isHexDigit (c : char) : bool =
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F')

        // Skip leading whitespace, as `wcstoul` does.
        let trimmed =
            let mutable start = 0

            while start < raw.Length && isCLocaleSpace raw.[start] do
                start <- start + 1

            raw.Substring start

        if System.String.IsNullOrEmpty trimmed then
            None
        else
            // A single optional `+` / `-` sign.
            let signStart, negate =
                match trimmed.[0] with
                | '+' -> 1, false
                | '-' -> 1, true
                | _ -> 0, false

            // Optional `0x` / `0X` radix prefix — only treated as a prefix
            // when at least one hex digit follows it. Otherwise the `0`
            // is itself the parsed digit and the `x` becomes the
            // stop character (matching `wcstoul`'s longest-valid-prefix
            // semantics).
            let bodyStart =
                if
                    signStart + 2 < trimmed.Length
                    && trimmed.[signStart] = '0'
                    && (trimmed.[signStart + 1] = 'x' || trimmed.[signStart + 1] = 'X')
                    && isHexDigit trimmed.[signStart + 2]
                then
                    signStart + 2
                else
                    signStart

            let mutable idx = bodyStart

            // Consume the longest hex-digit prefix and ignore everything
            // after it (so `1garbage` parses as 1, matching `wcstoul`).
            while idx < trimmed.Length && isHexDigit trimmed.[idx] do
                idx <- idx + 1

            if idx = bodyStart then
                None
            else
                let hexBody = trimmed.Substring (bodyStart, idx - bodyStart)

                // Parse the magnitude as `uint64` to capture values that fit
                // in `unsigned long` but exceed `UInt32.MaxValue`.
                match
                    System.UInt64.TryParse (
                        hexBody,
                        System.Globalization.NumberStyles.HexNumber,
                        System.Globalization.CultureInfo.InvariantCulture
                    )
                with
                | false, _ ->
                    // Magnitude exceeds `uint64`, so glibc `strtoul`
                    // itself sets `errno = ERANGE`. PAL_wcstoul never
                    // clears that errno (its HOST_64BIT post-processing
                    // only adds an additional ERANGE arm for positive
                    // 32-bit overflows), so `GetConfigDWORD` rejects
                    // via the errno arm for both signs.
                    None
                | true, magnitude ->
                    if (not negate) && magnitude > uint64 System.UInt32.MaxValue then
                        // Positive value whose magnitude exceeds
                        // UINT32_MAX. PAL_wcstoul's HOST_64BIT branch
                        // clamps to UINT32_MAX and sets `errno = ERANGE`,
                        // which `GetConfigDWORD` then rejects.
                        None
                    else
                        // Negation happens in `unsigned long` (mod 2^64)
                        // inside strtoul, and the final `(ULONG)res`
                        // cast truncates to the low 32 bits. We mirror
                        // both steps explicitly.
                        let wrapped = if negate then 0UL - magnitude else magnitude
                        Some (uint32 wrapped)


    [<Literal>]
    let private dotnetPrefix : string = "DOTNET_"

    [<Literal>]
    let private complusPrefix : string = "COMPlus_"

    /// The bit of `ProbabilisticNameSet`, CLRConfig's 256-bit Bloom filter of
    /// knob names, that the `count` code units of `chars` from `start` select;
    /// with `count` 0, every code unit from `start` to the end.
    ///
    /// This is `HashiStringNKnownLower80` (coreclr/inc/utilcode.h) modulo 256:
    /// djb2 with xor, over UTF-16 code units, ASCII lower case folded to upper
    /// and every other code unit taken as it is.
    let private nameBit (chars : string) (start : int) (count : int) : int =
        let stop = if count = 0 then chars.Length else start + count
        let mutable hash = 5381u

        for i in start .. stop - 1 do
            let c = chars.[i]

            let c =
                if c >= 'a' && c <= 'z' then
                    uint32 c &&& ~~~0x20u
                else
                    uint32 c

            hash <- ((hash <<< 5) + hash) ^^^ c

        int (hash % 256u)

    /// `c` upper-cased as `SString::CaseCompareHelper` does when comparing it
    /// with an ASCII character: ASCII by the simple rule, anything else by
    /// `minipal_toupper_invariant`. Of the non-ASCII code units, that table
    /// maps exactly two to ASCII, U+0131 to `I` and U+017F to `S`; every other
    /// one stays outside ASCII, so it is left as it is here, which is enough
    /// to tell that it differs from any ASCII character.
    let private upperForAsciiComparison (c : char) : char =
        if c >= 'a' && c <= 'z' then char (int c - 32)
        elif c = char 0x131 then 'I'
        elif c = char 0x17F then 'S'
        else c

    /// Whether `entry` begins with `prefix`, compared as `SString::_wcsnicmp`
    /// compares them. Measured on both kernels: an entry
    /// `COMPLU\u017F_PROCESSOR_COUNT=6` is recorded as the knob `PROCESSOR_COUNT`.
    let private hasPrefix (entry : string) (prefix : string) : bool =
        entry.Length >= prefix.Length
        && Seq.forall2
            (fun (e : char) (p : char) -> e = p || upperForAsciiComparison e = upperForAsciiComparison p)
            (entry.Substring (0, prefix.Length))
            prefix

    /// The bits `CLRConfig::Initialize` sets from `environment`, the entries
    /// in order as `GetEnvironmentStringsW` hands them over.
    ///
    /// The walk stops at the first empty entry, because it looks for the end
    /// of the block as a NUL where an entry should start. Of the entries
    /// before that, it records those that begin with `COMPlus_` or `DOTNET_`
    /// (compared case-insensitively) and have an `=`, under the name between
    /// the prefix and the first `=`.
    let private recordedBits (operation : string) (environment : UnixByteString list) : Set<int> =
        environment
        |> List.map (EnvironmentPal.decodeOrFail operation)
        |> List.takeWhile (fun entry -> entry <> "")
        |> List.choose (fun entry ->
            // The walk only looks further at an entry whose first code unit
            // lower-cases to `c` or `d`; no non-ASCII code unit does.
            let first =
                if entry.Length > 0 then
                    upperForAsciiComparison entry.[0]
                else
                    char 0

            match entry.IndexOf '=' with
            | -1 -> None
            | equals ->
                let prefix =
                    if first = 'C' && hasPrefix entry complusPrefix then
                        Some complusPrefix
                    elif first = 'D' && hasPrefix entry dotnetPrefix then
                        Some dotnetPrefix
                    else
                        None

                // A prefix holds no `=`, so the name is never negative in
                // length; an empty one hashes everything after the prefix,
                // `=` and value included, as `Add(name, 0)` does.
                prefix
                |> Option.map (fun prefix -> nameBit entry prefix.Length (equals - prefix.Length))
        )
        |> Set.ofList

    /// The value of `DOTNET_<name>`, or failing that `COMPlus_<name>`, as the
    /// PAL's lookup finds it, or `None` if neither is set to a non-empty value.
    let private lookUpInFull (operation : string) (environment : UnixByteString list) (name : string) : string option =
        let tryVariable (variable : string) : string option =
            match EnvironmentPal.tryGetValue operation variable environment with
            | Some value when value.Length > 0 -> Some value
            | _ -> None

        // The fallback is gated on the first lookup's length being zero, which
        // a variable set to the empty string also reports: so
        // `DOTNET_X=` with `COMPlus_X=9` reads as 9.
        match tryVariable (dotnetPrefix + name) with
        | Some value -> Some value
        | None -> tryVariable (complusPrefix + name)

    /// The value CoreCLR's `CLRConfig` reads for the knob `name` (the part
    /// after the `DOTNET_`/`COMPlus_` prefix), or `None` if it reads the knob
    /// as unset. `operation` names the caller in a failure.
    ///
    /// This is the variable `DOTNET_<name>`, or failing that `COMPlus_<name>`,
    /// as `EnvironmentPal.tryGetValue` finds it, with an empty value counting
    /// as unset: except that at startup `CLRConfig::Initialize` walks the
    /// environment block recording which knob names occur, and a knob that
    /// walk did not record reads as unset without being looked up. The walk
    /// stops at the first empty entry, so a knob set only after one is unset
    /// to `CLRConfig`, although `Environment.GetEnvironmentVariable` sees it.
    ///
    /// The record is a 256-bit Bloom filter, so a knob that merely shares a bit
    /// with a recorded one is looked up too, and found if it is set after the
    /// empty entry. This models the filter exactly rather than as a set of
    /// names. A non-zero `DisableConfigCache` knob, which is itself looked up
    /// before the walk and so without it, turns the walk off entirely.
    ///
    /// A `name` of 55 UTF-16 code units or more is unset, because
    /// `EnvGetString` refuses a name its fixed buffer cannot hold with either
    /// prefix.
    ///
    /// Measured by `execve`-ing the real runtime with a hand-built `envp`, on
    /// Darwin 25.6 and Linux 6.18.5; `TestClrConfigEnvironment` holds the rows.
    let tryGetValue (operation : string) (environment : UnixByteString list) (name : string) : string option =
        if isNull name then
            nullArg (nameof name)

        if name.Length >= 55 then
            None
        else

        let cacheDisabled =
            match
                lookUpInFull operation environment "DisableConfigCache"
                |> Option.bind tryParseDword
            with
            | Some value -> value <> 0u
            | None -> false

        if
            cacheDisabled
            || Set.contains (nameBit name 0 0) (recordedBits operation environment)
        then
            lookUpInFull operation environment name
        else
            None
