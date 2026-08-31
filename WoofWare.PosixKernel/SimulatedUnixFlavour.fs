namespace WoofWare.PosixKernel

/// <summary>
/// Which Unix we are simulating.
/// </summary>
/// <remarks>
/// This is essentially a bundling of the many different ways various Unix platforms can differ.
/// (For example, it includes errno numbering, permission bit handling on symlinks, whether <c>stat</c>
/// reports creation times, etc.)
/// </remarks>
[<RequireQualifiedAccess>]
type SimulatedUnixFlavour =
    /// <summary>Linux.</summary>
    | Linux
    /// <summary>Darwin, i.e. macOS.</summary>
    /// <remarks><c>uname -r</c> reports the Darwin kernel release rather than the macOS product version.</remarks>
    | Darwin
