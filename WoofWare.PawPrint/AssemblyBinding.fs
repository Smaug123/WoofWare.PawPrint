namespace WoofWare.PawPrint

open System
open System.IO
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
    /// A file whose name matches only ignoring case is accepted, because CoreCLR's table of
    /// trusted platform assemblies is keyed by lower-cased simple name. Deciding that by listing
    /// the directory rather than by asking the filesystem keeps the answer the same on a
    /// case-sensitive host and a case-insensitive one.
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

            let exact = Path.Combine (dir, fileName)

            try
                logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", exact)
                Assembly.readFile loggerFactory exact |> Some
            with
            | :? FileNotFoundException
            | :? DirectoryNotFoundException ->
                if not (Directory.Exists dir) then
                    None
                else

                let caseInsensitive =
                    Directory.EnumerateFiles (dir, "*.dll")
                    |> Seq.filter (fun candidate ->
                        String.Equals (Path.GetFileName candidate, fileName, StringComparison.OrdinalIgnoreCase)
                    )
                    // Filesystem enumeration order is not reproducible; the report below must be.
                    |> Seq.sort
                    |> List.ofSeq

                match caseInsensitive with
                | [] -> None
                | [ single ] ->
                    logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", single)
                    Assembly.readFile loggerFactory single |> Some
                | several ->
                    failwith
                        $"TODO: %s{dir} holds %d{List.length several} files named %s{fileName} differing only by case (%A{several}); CoreCLR's trusted-platform-assemblies table would keep whichever its host enumerated last, which PawPrint does not reproduce"
        )

    /// <summary>
    /// Bind <paramref name="request"/> the way <c>AssemblyNative_InternalLoad</c> does in the
    /// default load context: an assembly already loaded under that simple name and culture wins
    /// if its version satisfies the request, and otherwise the runtime directories are probed.
    /// </summary>
    let tryBind
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

        let sameName (candidate : DumpedAssembly) : bool =
            String.Equals (candidate.Name.Name, request.SimpleName, StringComparison.OrdinalIgnoreCase)

        let sameCulture (candidate : DumpedAssembly) : bool =
            match request.Culture with
            | None -> String.IsNullOrEmpty candidate.Name.CultureName
            | Some culture -> String.Equals (candidate.Name.CultureName, culture, StringComparison.OrdinalIgnoreCase)

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
            if isCompatibleVersion request.Version (foundVersion candidate) then
                AssemblyBindResult.Bound (assemblies, candidate)
            else
                AssemblyBindResult.NotFound
        | None ->

        match tryReadFromRuntimeDirs loggerFactory dotnetRuntimeDirs request.Culture request.SimpleName with
        | None -> AssemblyBindResult.NotFound
        | Some read ->
            if not (isCompatibleVersion request.Version (foundVersion read)) then
                AssemblyBindResult.NotFound
            else

            let assemblies = assemblies.WithLoadedAssembly read
            AssemblyBindResult.Bound (assemblies, assemblies.ByDefinitionName read.Name.FullName)
