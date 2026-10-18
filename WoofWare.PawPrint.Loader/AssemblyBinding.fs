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

    /// <summary>
    /// The version the binder compares, as <c>AssemblySpec::PopulateAssemblyNameData</c> hands it
    /// over: the requested components when the major was given, and <c>0.0.0.0</c> otherwise.
    /// </summary>
    /// <remarks>
    /// So an unversioned request does not match an assembly whose own major is <c>65535</c>,
    /// which a found component of that value, being "unspecified", never satisfies.
    /// </remarks>
    let forBinding (v : RequestedAssemblyVersion) : RequestedAssemblyVersion =
        if v.Major = Unspecified then
            {
                Major = 0us
                Minor = 0us
                Build = 0us
                Revision = 0us
            }
        else
            v

    /// The `System.Version` the display name prints: every component verbatim, so an unspecified
    /// one shows as `65535`.
    let toVersion (v : RequestedAssemblyVersion) : Version =
        Version (int v.Major, int v.Minor, int v.Build, int v.Revision)

/// <summary>
/// What of a guest's `Assembly.Load` (or assembly-qualified `Type.GetType`) request decides the
/// binding, once the request has been admitted.
/// </summary>
/// <remarks>
/// The rest of the request reaches only the display name a failure reports. Its public key token
/// is never consulted -- measured on .NET 10, `PublicKeyToken=0000000000000000` still binds the
/// framework's `System.Security.Claims` -- and an admitted request's culture is neutral.
/// </remarks>
type AssemblyLoadRequest =
    {
        /// Compared to a candidate's simple name ignoring case, as the binder keys its
        /// trusted-platform-assemblies table: measured on .NET 10, `system.security.claims`
        /// binds the framework's `System.Security.Claims`.
        SimpleName : string
        Version : RequestedAssemblyVersion
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
/// The two share one disk probe, <see cref="AssemblyProbe"/>, so that a name reaches the same
/// file whichever way it arrived.
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

    /// `BaseAssemblySpec::IsCoreLib`, the runtime's "rather loose idea of a reference to
    /// CoreLib": the simple name is CoreLib's own — alone, or with a comma-led suffix, which is
    /// how an `AssemblyName` whose `Name` field holds a whole display string arrives — or it is
    /// CoreLib's file-name spelling exactly, all ignoring case. Nothing else in the spec is
    /// consulted. Measured on .NET 10: `System.Private.CoreLib,garbage` answers the system
    /// assembly, and `System.Private.CoreLib.dll,x` does not.
    let isCoreLibName (simpleName : string) : bool =
        let coreLib = "System.Private.CoreLib"

        String.Equals (simpleName, coreLib + ".dll", StringComparison.OrdinalIgnoreCase)
        || (simpleName.StartsWith (coreLib, StringComparison.OrdinalIgnoreCase)
            && (simpleName.Length = coreLib.Length || simpleName.[coreLib.Length] = ','))

    /// Is every character of <paramref name="name"/> in ASCII?
    let isAscii (name : string) : bool =
        name |> Seq.forall (fun c -> c <= '\u007F')

    /// <summary>
    /// Might CoreCLR's case-insensitive comparison of simple names, which folds character by
    /// character by rules of its own, equate <paramref name="candidate"/> with the ASCII
    /// <paramref name="request"/> where an ordinal comparison ignoring case does not?
    /// </summary>
    /// <remarks>
    /// Only if the candidate is not ASCII (between two ASCII names the rules agree) and has the
    /// request's length (the folding is per character). Conservative: it says yes to pairs the
    /// rules may still tell apart, so that a caller refusing on it refuses every pair they don't.
    /// </remarks>
    let mayFoldToSameName (candidate : string) (request : string) : bool =
        not (isAscii candidate) && candidate.Length = request.Length

    /// <summary>
    /// <c>BINDER_SPACE::AssemblyName::IsNeutralCulture</c>: a culture never set, the empty
    /// culture, and the name <c>neutral</c> in any case all name the neutral culture.
    /// </summary>
    let isNeutralCulture (culture : string option) : bool =
        match culture with
        | None -> true
        | Some culture ->
            culture = ""
            || String.Equals (culture, "neutral", StringComparison.OrdinalIgnoreCase)

    /// <summary>
    /// <c>BaseAssemblySpec::IsCoreLibSatellite</c>: a simple name of
    /// <c>System.Private.CoreLib.resources</c>, alone or with a comma-led suffix, ignoring case,
    /// whose key-or-token blob is byte for byte <paramref name="coreLibPublicKey"/>. CoreCLR binds
    /// such a request by a path of its own rather than by the binder.
    /// </summary>
    /// <remarks>
    /// The blob is compared whatever <c>afPublicKey</c> says, so CoreLib's full key handed over
    /// as a token counts too.
    /// </remarks>
    let isCoreLibSatellite (simpleName : string) (publicKeyOrToken : byte[] option) (coreLibPublicKey : byte[]) : bool =
        let satellite = "System.Private.CoreLib.resources"

        let nameMatches =
            simpleName.StartsWith (satellite, StringComparison.OrdinalIgnoreCase)
            && (simpleName.Length = satellite.Length || simpleName.[satellite.Length] = ',')

        match publicKeyOrToken with
        | Some blob -> nameMatches && blob = coreLibPublicKey
        | None -> false

    /// <summary>
    /// Why PawPrint will not bind a request whose <c>AssemblyNameFlags</c> word is
    /// <paramref name="flags"/>, or <c>None</c> if it will.
    /// </summary>
    /// <remarks>
    /// Only <c>afPublicKey</c> (0x1) is admitted, which says whether the blob is a key or a token
    /// and has been dealt with by the time a token is in hand. A processor architecture, MSIL
    /// included, is refused: CoreCLR misses on one that is not the process's own, and on MSIL
    /// against an image built for a particular machine (<c>TestCandidateRefMatchesDef</c>), and
    /// either miss poisons the binder's failure cache for every later request of the same name,
    /// version, culture and token, whatever it names -- a cache PawPrint does not model. A
    /// content type is refused because a WindowsRuntime request misses and poisons the cache in
    /// the same way, and <c>Retargetable</c> or anything else because what the binder makes of it
    /// has not been measured.
    /// </remarks>
    let unsupportedFlags (flags : int) : string option =
        let afPublicKey = 0x1
        let others = flags &&& ~~~afPublicKey

        if others = 0 then
            None
        else
            Some $"AssemblyNameFlags 0x%08X{flags} set bits 0x%08X{others} beyond the public-key bit"

    /// <summary>
    /// Bind <paramref name="request"/> the way <c>AssemblyNative_InternalLoad</c> does in the
    /// default load context, once a CoreLib-named request (<see cref="isCoreLibName"/>) has been
    /// answered with the system assembly and the request has been admitted: an assembly already
    /// loaded under that simple name answers if its version satisfies the request (and misses
    /// otherwise), and failing that the runtime directories are probed.
    /// </summary>
    /// <remarks>
    /// CoreCLR remembers both the requests that bound and those that failed, and consults both
    /// before binding again. Neither is modelled, because neither is observable for the requests
    /// this accepts: the runtime directories do not change during a run and a loaded assembly is
    /// never unloaded, so binding afresh gives the answer the cache would, except after a miss
    /// whose cause the failure cache's key leaves out -- a processor architecture or, on .NET 10, a
    /// content type, which <see cref="unsupportedFlags"/> refuses.
    /// </remarks>
    let tryBind
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (request : AssemblyLoadRequest)
        (assemblies : LoadedAssemblies)
        : AssemblyBindResult
        =
        // `DefaultAssemblyBinder::BindAssemblyByNameWorker` asserts the same on its side.
        System.Diagnostics.Debug.Assert (not (isCoreLibName request.SimpleName))

        // A simple name is a name, not a path: a separator in it, or a rooted one, is a miss
        // (measured on .NET 10: `/tmp/target`, `../target` and `a/b` are each reported not
        // found), and the probe below must never see one, since `Path.Combine` would follow it
        // out of the runtime directories.
        let isPathLike (segment : string) : bool =
            segment.IndexOfAny [| '/' ; '\\' |] >= 0 || Path.IsPathRooted segment

        if isPathLike request.SimpleName then
            AssemblyBindResult.NotFound
        else

        let describe = $"binding '%s{request.SimpleName}'"

        let sameName (candidate : DumpedAssembly) : bool =
            String.Equals (candidate.Name.Name, request.SimpleName, StringComparison.OrdinalIgnoreCase)

        // An admitted request asks for the neutral culture, so a candidate that declares a culture
        // is a satellite and does not answer to it. A manifest may spell the neutral culture out.
        let isNeutral (candidate : DumpedAssembly) : bool =
            isNeutralCulture (Option.ofObj candidate.Name.CultureName)

        // What CoreCLR would refuse to load, or might bind where the comparisons below cannot tell:
        // an image marked 32-bit-required alone is x86-only, which no platform PawPrint simulates
        // runs (`COR_IS_32BIT_REQUIRED`), a reference assembly is never loaded for execution
        // (`PEAssembly::ValidateForExecution`), and a manifest name outside ASCII may fold to the
        // request under CoreCLR's rules.
        let requireAdmissible (candidate : DumpedAssembly) : unit =
            let metadata = candidate.PeReader.GetMetadataReader ()

            // `GetCustomAttributeByName` on the manifest matches the attribute's type by name alone,
            // whichever assembly defines it.
            let isReferenceAssembly =
                metadata.GetAssemblyDefinition().GetCustomAttributes ()
                |> Seq.exists (fun handle ->
                    CustomAttribute.constructorParentName
                        metadata
                        (fun () -> $"the manifest of %s{candidate.DefinitionFullName}")
                        (metadata.GetCustomAttribute handle).Constructor = Some (
                        "System.Runtime.CompilerServices",
                        "ReferenceAssemblyAttribute"
                    )
                )

            if isReferenceAssembly then
                failwith
                    $"TODO: %s{describe} found %s{candidate.DefinitionFullName}, a reference assembly, which CoreCLR refuses to load for execution (BadImageFormatException), not an outcome this binder reports"

            let bitness =
                candidate.PEImageHeaders.CorFlags
                &&& (System.Reflection.PortableExecutable.CorFlags.Requires32Bit
                     ||| System.Reflection.PortableExecutable.CorFlags.Prefers32Bit)

            if bitness = System.Reflection.PortableExecutable.CorFlags.Requires32Bit then
                failwith
                    $"TODO: %s{describe} found %s{candidate.DefinitionFullName}, an image marked 32-bit-required, which CoreCLR refuses to load on a 64-bit platform (BadImageFormatException), not an outcome this binder reports"

            if mayFoldToSameName candidate.Name.Name request.SimpleName then
                failwith
                    $"TODO: %s{describe} found %s{candidate.DefinitionFullName}, whose simple name is not ASCII and which CoreCLR's case folding may equate with the request; that folding is not modelled"

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
                    $"TODO: %s{describe} found %s{candidate.DefinitionFullName}, whose manifest flags 0x%08X{rawFlags} set a content type; CoreCLR refuses to load such an image (FileLoadException, 'The given assembly name was invalid'), which is not an outcome this binder reports"

        let foundVersion (candidate : DumpedAssembly) : Version =
            match candidate.Name.Version with
            | null ->
                failwith
                    $"AssemblyBinding.tryBind: %s{candidate.DefinitionFullName} has no version, but every manifest row has one"
            | version -> version

        // CoreCLR's binder never consults a dynamic assembly, so one neither answers a request
        // nor shadows an image on disk.
        let loaded =
            assemblies.DefinitionNamesInLoadOrder
            |> Seq.filter (assemblies.IsDynamic >> not)
            |> Seq.map assemblies.ByDefinitionName
            |> List.ofSeq

        match
            loaded
            |> List.tryFind (fun candidate -> mayFoldToSameName candidate.Name.Name request.SimpleName)
        with
        | Some candidate ->
            failwith
                $"TODO: %s{describe} with %s{candidate.DefinitionFullName} loaded, whose simple name is not ASCII and which CoreCLR's case folding may equate with the request; that folding is not modelled"
        | None -> ()

        let alreadyLoaded =
            loaded
            |> List.tryFind (fun candidate -> sameName candidate && isNeutral candidate)

        match alreadyLoaded with
        | Some candidate ->
            // "Can't give higher version than already bound" (assemblybindercommon.cpp): the
            // context holds one assembly per simple name, so an incompatible one is a miss
            // rather than a reason to probe for another.
            requireDefaultContentType candidate
            requireAdmissible candidate

            if isCompatibleVersion (RequestedAssemblyVersion.forBinding request.Version) (foundVersion candidate) then
                AssemblyBindResult.Bound (assemblies, candidate)
            else
                AssemblyBindResult.NotFound
        | None ->

        match AssemblyProbe.tryReadFromRuntimeDirs loggerFactory dotnetRuntimeDirs request.SimpleName with
        | None -> AssemblyBindResult.NotFound
        | Some read ->
            // CoreCLR initialises the image before it compares identities, so an image it
            // cannot load fails as such whatever its manifest says.
            requireDefaultContentType read
            requireAdmissible read

            // The file was chosen by its name; the *manifest* must agree
            // (`TestCandidateRefMatchesDef`): `Alias.dll` declaring itself `Real` does not
            // answer to `Alias`, and a file declaring a culture does not answer a neutral request.
            if not (sameName read && isNeutral read) then
                AssemblyBindResult.NotFound
            elif
                not (isCompatibleVersion (RequestedAssemblyVersion.forBinding request.Version) (foundVersion read))
            then
                AssemblyBindResult.NotFound
            else

            let assemblies = assemblies.WithLoadedAssembly read
            AssemblyBindResult.Bound (assemblies, assemblies.ByDefinitionName read.DefinitionFullName)
