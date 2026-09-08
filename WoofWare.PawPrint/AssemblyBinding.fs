namespace WoofWare.PawPrint

open System
open System.IO
open System.Reflection
open System.Reflection.Metadata
open Microsoft.Extensions.Logging

/// <summary>
/// The version a load request asked for, as CoreCLR's binder sees it.
/// </summary>
/// <remarks>
/// <c>AssemblyName.Version</c> carries <c>int</c> components, and the managed side narrows each
/// to <c>uint16</c> on the way into <c>AssemblyNative_InternalLoad</c>, writing <c>0xFFFF</c>
/// for a component the request left out (<c>NativeAssemblyNameParts.SetVersion</c>). A
/// <c>Version=10.0</c> request therefore arrives as <c>10.0.65535.65535</c>, and a failure to
/// bind it is reported under that spelling too -- measured on .NET 10: <c>Version=99.1</c> fails
/// as <c>'..., Version=99.1.65535.65535, ...'</c>.
/// </remarks>
type RequestedAssemblyVersion =
    {
        Major : uint16
        Minor : uint16
        Build : uint16
        Revision : uint16
    }

[<RequireQualifiedAccess>]
module RequestedAssemblyVersion =
    /// The component value that means "not specified".
    [<Literal>]
    let Unspecified = 0xFFFFus

    /// The `System.Version` the display name prints: every component verbatim, so an unspecified
    /// one shows as `65535`.
    let toVersion (v : RequestedAssemblyVersion) : Version =
        Version (int v.Major, int v.Minor, int v.Build, int v.Revision)

/// What a guest asked `Assembly.Load` (or an assembly-qualified `Type.GetType`) for.
type AssemblyLoadRequest =
    {
        /// Compared to a candidate's simple name ignoring case, as the binder keys its
        /// trusted-platform-assemblies table: measured on .NET 10, `system.security.claims`
        /// binds the framework's `System.Security.Claims`.
        SimpleName : string
        Version : RequestedAssemblyVersion
        /// `None` for the neutral culture. A named culture asks for a satellite assembly,
        /// which lives at `<culture>/<name>.dll` beside where the neutral one would be.
        Culture : string option
        /// Shown in a failure's message and `FileName`, and never consulted for binding:
        /// measured on .NET 10, `PublicKeyToken=0000000000000000` still binds the framework's
        /// `System.Security.Claims`. A request made with a full key has had the token derived
        /// from it already.
        PublicKeyToken : byte[] option
        /// The `AssemblyNameFlags` bits. The content type decides whether anything can bind at
        /// all; the rest reach only the display name.
        Flags : int
    }

/// <summary>
/// What one process remembers of the binds it has already tried, and consults before binding
/// again. Both halves are guest-observable, each measured on .NET 10.
/// </summary>
/// <remarks>
/// A request that has bound answers the same assembly again without the binder being asked:
/// that is the AppDomain's cache of bound specs, which also holds the entry assembly's own
/// identity and the identity every metadata reference was bound under, and compares specs
/// exactly (<c>BaseAssemblySpec::CompareEx</c>) -- the simple name case-sensitively, the version
/// up to the first component a request left out. A request that has failed poisons every later
/// request sharing its <em>failure key</em>, <c>AssemblyName::GetDisplayName(INCLUDE_VERSION)</c>:
/// the simple name (ignoring case), the version when its major was given, the culture and the
/// token -- but not the architecture or the content type. So a miss for
/// <c>System.Security.Claims, processorArchitecture=x86</c> makes the plain
/// <c>System.Security.Claims</c> a miss for the rest of the process, while a miss for
/// <c>Version=99.0.0.0</c> or <c>Culture=fr</c> leaves it alone. The bound specs are consulted
/// first, which is why a plain request that bound before survives a later architecture miss
/// for the same name.
/// </remarks>
type AssemblyBindCache =
    private
        {
            /// Failure keys of the requests that missed.
            Failed : Set<string>
            /// Every part of the requests that bound, to the definition identity each bound to.
            Bound : Map<string, string>
            /// Definition identities of the assemblies a request read from disk. CoreCLR caches
            /// the *request's* spec for those, not the assembly's own identity -- measured: a
            /// plain request that bound `System.Security.Claims` leaves its full identity
            /// unanswered after a miss under that identity -- whereas the entry assembly and
            /// every reference are cached under their own.
            LoadedByName : Set<string>
        }

[<RequireQualifiedAccess>]
module AssemblyBindCache =
    let empty : AssemblyBindCache =
        {
            Failed = Set.empty
            Bound = Map.empty
            LoadedByName = Set.empty
        }

    let private tokenText (request : AssemblyLoadRequest) : string =
        match request.PublicKeyToken with
        | None -> ""
        | Some token -> Convert.ToHexString token

    /// `AssemblyName::GetDisplayName(INCLUDE_VERSION)`, as far as what it distinguishes: the
    /// binder's failure cache is keyed by it. That is the simple name, the version when its
    /// major was given (an unspecified major drops the whole segment, so a request versioned
    /// `65535.1.2.3` shares its key with an unversioned one), the culture, and the token.
    /// Measured on .NET 10: the key ignores the simple name's case, and a miss under
    /// `PublicKeyToken=0000000000000000` leaves the tokenless request alone.
    let private failureKey (request : AssemblyLoadRequest) : string =
        let version =
            let v = request.Version

            if v.Major = RequestedAssemblyVersion.Unspecified then
                ""
            else
                $"%d{v.Major}.%d{v.Minor}.%d{v.Build}.%d{v.Revision}"

        let culture =
            match request.Culture with
            | None -> ""
            | Some culture -> culture.ToLowerInvariant ()

        $"%s{request.SimpleName.ToLowerInvariant ()}|%s{version}|%s{culture}|%s{tokenText request}"

    /// The version as `BaseAssemblySpec::CompareEx` compares it: component by component, and no
    /// further than the first one the request left out -- so `65535.1.2.3` is the unversioned
    /// request, and `10.0` is `10.0.65535.3`. Measured on .NET 10: after `Version=10.0` bound
    /// and an architecture miss poisoned the name, `Version=10.0.65535.3` still answers.
    let private comparedVersion (v : RequestedAssemblyVersion) : uint16 list =
        [ v.Major ; v.Minor ; v.Build ; v.Revision ]
        |> List.takeWhile (fun c -> c <> RequestedAssemblyVersion.Unspecified)

    /// Every part of the request, as spelled, for the successes the process remembers: the
    /// AppDomain's cache of bound specs compares simple names case-sensitively (measured on
    /// .NET 10: after `System.Buffers` bound and `system.buffers, processorArchitecture=x86`
    /// missed, `System.Buffers` still answers and `SYSTEM.BUFFERS` does not). A culture never
    /// set and the neutral culture spelled out are two specs to `CompareEx`.
    let private requestKey (request : AssemblyLoadRequest) : string =
        let version =
            comparedVersion request.Version |> List.map string |> String.concat "."

        let culture =
            match request.Culture with
            | None -> "<none>"
            | Some culture -> culture

        $"%s{request.SimpleName}|%s{version}|%s{culture}|%s{tokenText request}|%d{request.Flags}"

    /// `BaseAssemblySpec::CompareEx` between the request and an identity the runtime recorded
    /// when it loaded something by another route: the entry assembly's own, or the one a
    /// metadata reference named. Name, token and every flag exactly; the version component by
    /// component, stopping at the first one both left out; the culture exactly, with a culture
    /// never set matching none.
    let private specEquals (request : AssemblyLoadRequest) (identity : AssemblyName) : bool =
        let identityToken =
            match identity.GetPublicKeyToken () with
            | null -> None
            | token when token.Length = 0 -> None
            | token -> Some token

        let sameToken =
            match request.PublicKeyToken, identityToken with
            | None, None -> true
            | Some a, Some b -> a = b
            | _ -> false

        // `AssemblyName.Flags` leaves the content type to its own property; the spec's flags
        // word carries both. No architecture term: neither a manifest row nor a display name
        // the BCL formats carries one, so an identity recorded by another route never has it,
        // and a request that names one is unequal to every such identity through `Flags`.
        let identityFlags = int identity.Flags ||| (int identity.ContentType <<< 9)

        let sameVersion =
            let identityComponents =
                match identity.Version with
                | null -> List.replicate 4 RequestedAssemblyVersion.Unspecified
                | version ->
                    [ version.Major ; version.Minor ; version.Build ; version.Revision ]
                    |> List.map (fun c ->
                        if c < 0 then
                            RequestedAssemblyVersion.Unspecified
                        else
                            uint16 c
                    )

            let requestedComponents =
                [
                    request.Version.Major
                    request.Version.Minor
                    request.Version.Build
                    request.Version.Revision
                ]

            let rec compare (requested : uint16 list) (found : uint16 list) : bool =
                match requested, found with
                | [], [] -> true
                | r :: requested, f :: found ->
                    if r <> f then false
                    elif r = RequestedAssemblyVersion.Unspecified then true
                    else compare requested found
                | _ -> false

            compare requestedComponents identityComponents

        let sameCulture =
            match request.Culture with
            | None -> false
            | Some culture -> String.Equals (culture, identity.CultureName, StringComparison.Ordinal)

        String.Equals (request.SimpleName, identity.Name, StringComparison.Ordinal)
        && sameToken
        && request.Flags = identityFlags
        && sameVersion
        && sameCulture

    /// The definition identity the runtime would answer this request with from the specs it
    /// recorded by other routes, if any: a reference bound under an identity the request
    /// equals, or an assembly loaded by the host whose own identity it equals.
    let tryFindRecordedByOtherRoute
        (request : AssemblyLoadRequest)
        (assemblies : LoadedAssemblies)
        (cache : AssemblyBindCache)
        : string option
        =
        let byReference =
            assemblies.ReferenceBindings
            |> List.tryPick (fun (reference, definition) ->
                if specEquals request reference then
                    Some definition
                else
                    None
            )

        match byReference with
        | Some definition -> Some definition
        | None ->
            assemblies.DefinitionNamesInLoadOrder
            |> Seq.tryFind (fun definition ->
                not (Set.contains definition cache.LoadedByName)
                && specEquals request (assemblies.ByDefinitionName definition).Name
            )

    /// The definition identity an identical request bound to before, if one did.
    let tryFindBound (request : AssemblyLoadRequest) (cache : AssemblyBindCache) : string option =
        Map.tryFind (requestKey request) cache.Bound

    /// Has a request sharing this one's failure key missed before?
    let hasFailed (request : AssemblyLoadRequest) (cache : AssemblyBindCache) : bool =
        Set.contains (failureKey request) cache.Failed

    let withBound
        (request : AssemblyLoadRequest)
        (definitionName : string)
        (readFromDisk : bool)
        (cache : AssemblyBindCache)
        : AssemblyBindCache
        =
        { cache with
            Bound = Map.add (requestKey request) definitionName cache.Bound
            LoadedByName =
                if readFromDisk then
                    Set.add definitionName cache.LoadedByName
                else
                    cache.LoadedByName
        }

    let withFailed (request : AssemblyLoadRequest) (cache : AssemblyBindCache) : AssemblyBindCache =
        { cache with
            Failed = Set.add (failureKey request) cache.Failed
        }

[<RequireQualifiedAccess>]
type AssemblyBindResult =
    /// The load context with the assembly registered, and the canonical instance for its
    /// definition identity.
    | Bound of LoadedAssemblies * DumpedAssembly
    /// Nothing PawPrint can see answers to the request. CoreCLR reports this as
    /// `FileNotFoundException` whether the name was unknown altogether or known only at a
    /// lower version than asked for.
    | NotFound

/// <summary>
/// Binding an assembly by display name, as CoreCLR's default load context does it.
/// </summary>
/// <remarks>
/// A reference from one assembly's metadata to another is bound by <c>TypeResolution</c>; this
/// module answers the other way an assembly gets named, by a string a guest built at run time.
/// The two share one disk probe so that a name reaches the same file whichever way it arrived.
/// </remarks>
[<RequireQualifiedAccess>]
module AssemblyBinding =

    /// <summary>
    /// <c>AssemblyBinderCommon::IsCompatibleAssemblyVersion</c>: may a request for
    /// <paramref name="requested"/> be satisfied by an assembly versioned
    /// <paramref name="found"/>?
    /// </summary>
    /// <remarks>
    /// Component by component from the major: a request that leaves a component out is
    /// satisfied whatever the rest of the found version is; a request for more than was found
    /// is not; a request for less is, regardless of the lesser-order components; and equal
    /// components defer to the next. A found component of <c>65535</c> is itself "unspecified",
    /// and a specific request for that component fails against it.
    ///
    /// Measured on .NET 10 against the framework's <c>10.0.0.0</c>: <c>Version=4.0.0.0</c> and
    /// <c>Version=10.0</c> bind, <c>Version=99.0.0.0</c> and <c>Version=99.1</c> do not.
    /// </remarks>
    let isCompatibleVersion (requested : RequestedAssemblyVersion) (found : Version) : bool =
        let foundComponent (c : int) : uint16 =
            if c < 0 || c > 0xFFFF then
                failwith
                    $"isCompatibleVersion: found version %O{found} has a component outside the manifest's USHORT column"

            uint16 c

        let rec go (pairs : (uint16 * uint16) list) : bool =
            match pairs with
            | [] -> true
            | (req, fnd) :: rest ->
                if req = RequestedAssemblyVersion.Unspecified then
                    true
                elif fnd = RequestedAssemblyVersion.Unspecified || req > fnd then
                    false
                elif req < fnd then
                    true
                else
                    go rest

        go
            [
                requested.Major, foundComponent found.Major
                requested.Minor, foundComponent found.Minor
                requested.Build, foundComponent found.Build
                requested.Revision, foundComponent found.Revision
            ]

    /// <summary>
    /// Read <c>&lt;simpleName&gt;.dll</c> from the first of <paramref name="dotnetRuntimeDirs"/>
    /// that holds it, looking in the <paramref name="culture"/> subdirectory of each when a
    /// culture is named.
    /// </summary>
    /// <remarks>
    /// The first hit is the binding, so every later directory goes unread; reading them anyway
    /// would let a directory we were never going to bind against fail the load, because anything
    /// but <c>FileNotFoundException</c> escapes.
    ///
    /// The file is found by listing the directory and matching the whole file name ignoring
    /// case, because CoreCLR's table of trusted platform assemblies is keyed by lower-cased
    /// simple name. Listing rather than asking the filesystem for the exact name keeps the
    /// answer the same on a case-sensitive host and a case-insensitive one, and asks the same
    /// question whatever the request's own casing: two files that differ only by case are a
    /// collision this refuses, however the request spells the name.
    /// </remarks>
    let tryReadFromRuntimeDirs
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (culture : string option)
        (simpleName : string)
        : DumpedAssembly option
        =
        let logger = loggerFactory.CreateLogger "AssemblyBinding"
        let fileName = simpleName + ".dll"

        dotnetRuntimeDirs
        |> Seq.tryPick (fun dir ->
            let dir =
                match culture with
                | None -> dir
                | Some culture -> Path.Combine (dir, culture)

            if not (Directory.Exists dir) then
                None
            else

            let matches =
                // Every file, not `*.dll`: a case-sensitive host's pattern match would drop
                // `Foo.DLL` before the comparison below ever saw it.
                Directory.EnumerateFiles dir
                |> Seq.filter (fun candidate ->
                    String.Equals (Path.GetFileName candidate, fileName, StringComparison.OrdinalIgnoreCase)
                )
                // Filesystem enumeration order is not reproducible; the report below must be.
                |> Seq.sort
                |> List.ofSeq

            match matches with
            | [] -> None
            | [ single ] ->
                logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", single)
                Assembly.readFile loggerFactory single |> Some
            | several ->
                failwith
                    $"TODO: %s{dir} holds %d{List.length several} files named %s{fileName} differing only by case (%A{several}); CoreCLR's trusted-platform-assemblies table would keep whichever its host enumerated last, which PawPrint does not reproduce"
        )

    /// The binder proper, once the caches have been consulted.
    let private bindUncached
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (request : AssemblyLoadRequest)
        (assemblies : LoadedAssemblies)
        : AssemblyBindResult
        =
        // afContentType_WindowsRuntime: nothing the binder holds has that content type, so the
        // request is a miss whether or not the simple name is known. Measured on .NET 10:
        // `System.Security.Claims, ContentType=WindowsRuntime` is reported not found.
        if request.Flags &&& 0xE00 = 0x200 then
            AssemblyBindResult.NotFound
        else

        // A simple name is a name, not a path: a separator in it, or a rooted one, is a miss
        // (measured on .NET 10: `/tmp/target`, `../target` and `a/b` are each reported not
        // found), and the probe below must never see one, since `Path.Combine` would follow it
        // out of the runtime directories.
        let isPathLike (segment : string) : bool =
            segment.IndexOfAny [| '/' ; '\\' |] >= 0 || Path.IsPathRooted segment

        if isPathLike request.SimpleName || (request.Culture |> Option.exists isPathLike) then
            AssemblyBindResult.NotFound
        else

        let describe = $"binding '%s{request.SimpleName}'"

        // The candidate's own architecture, which CoreCLR validates on every load
        // (`Assembly::Init`), requested or not. PawPrint hosts processor-agnostic IL: an
        // image built for a 32-bit architecture is `BadImageFormatException` on every 64-bit
        // platform it simulates, and one built for a 64-bit architecture loads only on a
        // runtime of that architecture -- the native runtime's own identity, which PawPrint
        // does not model (see `PEImageKind.peKindAndMachine` on ReadyToRun images).
        let requireAgnostic (candidate : DumpedAssembly) : unit =
            match
                PEImageKind.peKindAndMachine describe candidate.PEImageHeaders
                |> PEImageKind.architectureOfImage describe
            with
            | ImageArchitecture.Msil -> ()
            | ImageArchitecture.I386
            | ImageArchitecture.Arm as found ->
                failwith
                    $"TODO: %s{describe} found %s{candidate.Name.FullName} built for %O{found}, which CoreCLR refuses with BadImageFormatException on a 64-bit runtime; that exception is not one this binder raises"
            | ImageArchitecture.Amd64
            | ImageArchitecture.Arm64 as found ->
                failwith
                    $"TODO: %s{describe} found %s{candidate.Name.FullName} built for %O{found}; whether that loads depends on the architecture of the executing runtime, which PawPrint does not model"

        // afPA_Mask: the `ProcessorArchitecture` the request names, if any. The field is read
        // the way CoreCLR reads a manifest's (`GetProcessorArchitectureFromAssemblyFlags`),
        // by testing bits in order rather than by value, so IA64 (0x30) and ARM (0x50) are
        // MSIL -- measured on .NET 10, both bind the framework's System.Security.Claims.
        let requestedArchitecture : ImageArchitecture option =
            let field = request.Flags &&& 0x70

            if field = 0 then None
            elif field &&& 0x10 <> 0 then Some ImageArchitecture.Msil
            elif field &&& 0x20 <> 0 then Some ImageArchitecture.I386
            else Some ImageArchitecture.Amd64

        // `IsValidArchitecture`, which `BindByName` asks before it looks at any candidate:
        // MSIL and none outright, otherwise only the architecture of the process itself.
        // Every platform PawPrint simulates is 64-bit, so x86 is never valid, and a miss
        // before anything is read.
        if requestedArchitecture = Some ImageArchitecture.I386 then
            AssemblyBindResult.NotFound
        else

        // `TestCandidateRefMatchesDef` then compares a valid request with the candidate's own
        // architecture, which `requireAgnostic` has pinned to MSIL: AMD64 never matches it.
        // Measured, x86 and AMD64 are both misses against the framework's agnostic images.
        let architectureAccepts : bool =
            match requestedArchitecture with
            | None
            | Some ImageArchitecture.Msil -> true
            | Some _ -> false

        let sameName (candidate : DumpedAssembly) : bool =
            String.Equals (candidate.Name.Name, request.SimpleName, StringComparison.OrdinalIgnoreCase)

        // A culture never set and the neutral culture spelled out both bind the neutral
        // assembly (`AssemblySpec` treats both as neutral); they part company only in the spec
        // cache, above.
        let sameCulture (candidate : DumpedAssembly) : bool =
            match request.Culture with
            | None
            | Some "" -> String.IsNullOrEmpty candidate.Name.CultureName
            | Some culture -> String.Equals (candidate.Name.CultureName, culture, StringComparison.OrdinalIgnoreCase)

        // CoreCLR refuses to load an assembly whose manifest sets any bit of
        // `afContentType_Mask` (0xE00): the WindowsRuntime value and every reserved one alike
        // ("The given assembly name was invalid"), whatever the request asked for. Read off
        // the row's own flags word: `AssemblyName.ContentType` folds the reserved values back
        // to `Default`.
        let requireDefaultContentType (candidate : DumpedAssembly) : unit =
            let rawFlags =
                int (candidate.PeReader.GetMetadataReader().GetAssemblyDefinition().Flags)

            if rawFlags &&& 0xE00 <> 0 then
                failwith
                    $"TODO: %s{describe} found %s{candidate.Name.FullName}, whose manifest flags 0x%08X{rawFlags} set a content type; CoreCLR refuses to load such an image (FileLoadException, 'The given assembly name was invalid'), which is not an outcome this binder reports"

        let foundVersion (candidate : DumpedAssembly) : Version =
            match candidate.Name.Version with
            | null ->
                failwith
                    $"AssemblyBinding.tryBind: %s{candidate.Name.FullName} has no version, but every manifest row has one"
            | version -> version

        let alreadyLoaded =
            assemblies.DefinitionNamesInLoadOrder
            |> Seq.map assemblies.ByDefinitionName
            |> Seq.tryFind (fun candidate -> sameName candidate && sameCulture candidate)

        match alreadyLoaded with
        | Some candidate ->
            // "Can't give higher version than already bound" (assemblybindercommon.cpp): the
            // context holds one assembly per simple name, so an incompatible one is a miss
            // rather than a reason to probe for another.
            requireDefaultContentType candidate
            requireAgnostic candidate

            if
                architectureAccepts
                && isCompatibleVersion request.Version (foundVersion candidate)
            then
                AssemblyBindResult.Bound (assemblies, candidate)
            else
                AssemblyBindResult.NotFound
        | None ->

        match tryReadFromRuntimeDirs loggerFactory dotnetRuntimeDirs request.Culture request.SimpleName with
        | None -> AssemblyBindResult.NotFound
        | Some read ->
            // CoreCLR initialises the image before it compares identities, so an image it
            // cannot load fails as such whatever its manifest says.
            requireDefaultContentType read
            requireAgnostic read

            // The file was chosen by its name; the *manifest* must agree
            // (`TestCandidateRefMatchesDef`): `Alias.dll` declaring itself `Real` does not
            // answer to `Alias`, and a file found in a culture's subdirectory must declare that
            // culture.
            if not (sameName read && sameCulture read) then
                AssemblyBindResult.NotFound
            elif not architectureAccepts then
                AssemblyBindResult.NotFound
            elif not (isCompatibleVersion request.Version (foundVersion read)) then
                AssemblyBindResult.NotFound
            else

            let assemblies = assemblies.WithLoadedAssembly read
            AssemblyBindResult.Bound (assemblies, assemblies.ByDefinitionName read.Name.FullName)

    /// <summary>
    /// Bind <paramref name="request"/> the way <c>AssemblyNative_InternalLoad</c> does in the
    /// default load context: a request that bound before answers the same assembly, a request
    /// whose failure key missed before misses again, and otherwise an assembly already loaded
    /// under that simple name and culture wins if its version satisfies the request, else the
    /// runtime directories are probed. The returned cache remembers the outcome.
    /// </summary>
    let tryBind
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (request : AssemblyLoadRequest)
        (assemblies : LoadedAssemblies)
        (cache : AssemblyBindCache)
        : AssemblyBindCache * AssemblyBindResult
        =
        let recorded =
            match AssemblyBindCache.tryFindBound request cache with
            | Some definitionName -> Some definitionName
            | None -> AssemblyBindCache.tryFindRecordedByOtherRoute request assemblies cache

        match recorded with
        | Some definitionName ->
            match assemblies.TryByDefinitionName definitionName with
            | Some bound -> cache, AssemblyBindResult.Bound (assemblies, bound)
            | None ->
                failwith
                    $"AssemblyBinding.tryBind: '%s{request.SimpleName}' bound to %s{definitionName} earlier in this run, which is no longer loaded; assemblies are never unloaded"
        | None ->

        if AssemblyBindCache.hasFailed request cache then
            cache, AssemblyBindResult.NotFound
        else

        match bindUncached loggerFactory dotnetRuntimeDirs request assemblies with
        | AssemblyBindResult.Bound (loaded, bound) ->
            let readFromDisk = not (assemblies.ContainsDefinition bound.Name)

            AssemblyBindCache.withBound request bound.Name.FullName readFromDisk cache,
            AssemblyBindResult.Bound (loaded, bound)
        | AssemblyBindResult.NotFound -> AssemblyBindCache.withFailed request cache, AssemblyBindResult.NotFound
