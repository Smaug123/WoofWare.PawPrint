namespace WoofWare.PawPrint

open WoofWare.PosixKernel

/// The process environment as CoreCLR's Unix PAL presents it to CoreLib
/// (`pal/src/misc/environ.cpp`): a name-to-value view over the `envp` the
/// kernel holds, and the rules a host's configuration of that `envp` must
/// satisfy.
///
/// The PAL snapshots `environ` once at startup (`EnvironInitialize`), in order
/// and entry for entry, and every environment API CoreLib reaches consults that
/// snapshot. PawPrint services no `SetEnvironmentVariableW`, so no guest can
/// change it, and the kernel's exec-time `envp` is the snapshot for the whole
/// run.
[<RequireQualifiedAccess>]
module EnvironmentPal =

    /// The bytes a host-supplied environment entry stands for, which are its
    /// UTF-8 encoding; or why no real process could hold that entry.
    ///
    /// Refuses a null entry, an entry containing a NUL (which would end it
    /// early), and an entry containing an unpaired UTF-16 surrogate (which has no
    /// UTF-8 encoding, and which the PAL's decoder never produces, so no guest
    /// could observe one on real .NET). The error describes the problem for a
    /// caller to prefix with its own context.
    ///
    /// Anything else is accepted, including an entry with no `=`, an entry
    /// beginning with `=`, and an empty entry: a real `envp` can hold each of
    /// those, and the lookup and the environment block say what the guest sees
    /// of them.
    let tryEncodeEntry (entry : string) : Result<UnixByteString, string> =
        if isNull entry then
            Error "a null entry, which is not a string an environment could hold"
        else
            match UnixByteString.ofString entry with
            | Ok bytes -> Ok bytes
            | Error defect -> Error $"the entry %A{entry}, which %s{UnixPathText.describe defect}"

    /// The entry `NAME=VALUE`, which the environment APIs read back as the
    /// variable `name` holding `value`.
    ///
    /// Throws if no entry reads back that way: a null name or value, an empty
    /// name (`=VALUE` is an entry `GetEnvironmentVariables` discards and no lookup
    /// can name), or a name containing `=` (every reader splits an entry at its
    /// first `=`, so it would see a different name and value). A NUL or an
    /// unpaired surrogate is refused later, when the entry is installed, as it is
    /// in any entry; see `tryEncodeEntry`.
    let nameValueEntry (name : string) (value : string) : string =
        if isNull name then
            failwith "EnvironmentPal.nameValueEntry: the name is null"
        elif isNull value then
            failwith $"EnvironmentPal.nameValueEntry: the value of %s{name} is null"
        elif name = "" then
            failwith
                $"EnvironmentPal.nameValueEntry: the name is empty, so the entry would read `=%s{value}`, which no lookup can name"
        elif name.Contains '=' then
            failwith
                $"EnvironmentPal.nameValueEntry: the name %s{name} contains '=', so every reader would split the entry at that '=' and see a different name"
        else
            name + "=" + value

    let private equalsSign : byte = byte '='

    /// The name under which a lookup finds `entry`: its bytes before the first
    /// `=`, or the whole entry if it has none.
    let entryName (entry : UnixByteString) : UnixByteString =
        let bytes = UnixByteString.toBytes entry

        match bytes.IndexOf equalsSign with
        | -1 -> entry
        | index -> UnixByteString.slice 0 index entry

    /// The value bytes the PAL's `GetEnvironmentVariableA` finds for `name` in
    /// `environment`, or `None` if it finds none.
    ///
    /// `name` is first converted to UTF-8 the way `GetEnvironmentVariableW`
    /// converts it, substituting U+FFFD for each unpaired surrogate, so a name
    /// holding one matches an entry whose name holds U+FFFD. An empty name, and
    /// a name containing `=`, find nothing. Otherwise the first entry that is
    /// exactly `name` (whose value is then empty) or that begins with `name`
    /// followed by `=` supplies the value, and later entries of the same name are
    /// never consulted. Names are compared byte for byte, so case-sensitively.
    let tryFindValue (name : string) (environment : UnixByteString list) : UnixByteString option =
        if isNull name then
            nullArg (nameof name)

        // A NUL would end the PAL's C string early, so it would look up only the
        // part before it. No caller can supply one: the guest's name arrives
        // NUL-terminated, and PawPrint's own lookups are literals.
        if name.Contains (char 0) then
            invalidArg (nameof name) $"EnvironmentPal.tryFindValue: the name %A{name} contains a NUL"

        // `Encoding.UTF8` substitutes U+FFFD for each unpaired surrogate, as the
        // PAL's `WideCharToMultiByte(CP_ACP, 0, ...)` does (minipal's
        // `EncoderReplacementFallback`, one U+FFFD per unpaired code unit).
        let nameBytes = System.Text.Encoding.UTF8.GetBytes name

        if nameBytes.Length = 0 || System.Array.IndexOf (nameBytes, equalsSign) >= 0 then
            None
        else

        let matches (entry : UnixByteString) : UnixByteString option =
            let bytes = UnixByteString.toBytes entry

            if bytes.Length < nameBytes.Length then
                None
            else

            let mutable i = 0

            while i < nameBytes.Length && bytes.[i] = nameBytes.[i] do
                i <- i + 1

            if i < nameBytes.Length then
                None
            elif bytes.Length = nameBytes.Length then
                Some UnixByteString.empty
            elif bytes.[nameBytes.Length] = equalsSign then
                Some (UnixByteString.slice (nameBytes.Length + 1) (bytes.Length - nameBytes.Length - 1) entry)
            else
                None

        List.tryPick matches environment

    /// `bytes` as UTF-16, the way the PAL's `MultiByteToWideChar` decodes the
    /// environment for CoreLib; `operation` prefixes the failure.
    ///
    /// Fails on bytes that are not strictly UTF-8. The PAL substitutes for them
    /// by rules PawPrint does not model, and PawPrint's own configuration can
    /// never produce them (see `tryEncodeEntry`), so meeting some means the
    /// kernel's environment was set by another route.
    let decodeOrFail (operation : string) (bytes : UnixByteString) : string =
        match UnixByteString.tryToString bytes with
        | Some s -> s
        | None ->
            failwith
                $"%s{operation}: the environment holds %s{UnixByteString.toEscaped bytes}, which is not valid UTF-8; PawPrint does not model how the PAL decodes that"

    /// `tryFindValue`, decoded as the PAL decodes a value for CoreLib.
    let tryGetValue (operation : string) (name : string) (environment : UnixByteString list) : string option =
        tryFindValue name environment
        |> Option.map (decodeOrFail $"%s{operation}, looking up %s{name}")
