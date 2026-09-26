namespace WoofWare.PawPrint

open System
open System.IO
open Microsoft.Extensions.Logging

/// Finding the file that holds an assembly, given only its simple name.
[<RequireQualifiedAccess>]
module AssemblyProbe =

    /// <summary>
    /// The directory a runtime-dir entry names, as a path the filesystem can list: the empty
    /// entry is the current directory.
    /// </summary>
    /// <remarks>
    /// <c>Path.GetDirectoryName "Guest.dll"</c> is <c>""</c>, so a bare file name's own
    /// directory arrives spelled that way. <c>Directory.EnumerateFiles</c> refuses the empty
    /// path, where <c>Path.Combine ("", name)</c> is <c>name</c> relative to the current
    /// directory.
    /// </remarks>
    let runtimeDirPath (dir : string) : string = if dir = "" then "." else dir

    /// <summary>
    /// Read <c>&lt;simpleName&gt;.dll</c> from the first of <paramref name="dotnetRuntimeDirs"/>
    /// that holds it, matching the file name ignoring case. A directory that does not exist
    /// holds nothing, and neither does an entry that cannot be opened, such as a dangling
    /// symlink; a directory that exists but cannot be listed is an error.
    /// </summary>
    /// <remarks>
    /// CoreCLR's table of trusted platform assemblies compares simple names ignoring case
    /// (<c>SimpleNameToFileNameMapTraits</c>), and its extension check ignores case too, so a
    /// request for <c>system.runtime</c> reaches <c>System.Runtime.dll</c> on any host. Two files
    /// in one directory whose names differ only by case are refused: CoreCLR keeps whichever the
    /// host listed first, and PawPrint does not reproduce the host's order.
    /// </remarks>
    let tryReadFromRuntimeDirs
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (simpleName : string)
        : DumpedAssembly option
        =
        let logger = loggerFactory.CreateLogger "AssemblyProbe"
        let fileName = simpleName + ".dll"

        // `tryPick`, not `choose |> tryHead`: the first hit is the binding, so every later dir
        // must go unread. Reading them anyway is not merely wasted parsing (though it is that
        // too, and a runtime dir list often holds a whole second framework) -- it lets a
        // directory we were never going to bind against fail the load.
        dotnetRuntimeDirs
        |> Seq.tryPick (fun dir ->
            let dir = runtimeDirPath dir

            // The directory is listed rather than asked for the exact name, so that the answer
            // is the same on a case-sensitive host and a case-insensitive one, and the same
            // however the request spells the name. Every file, not `*.dll`: a case-sensitive
            // host's pattern match would drop `Foo.DLL` before the comparison saw it.
            //
            // Only a directory that is not there holds nothing. `Directory.Exists` would also say
            // false for one that is there but cannot be searched, and skipping that would bind
            // from a later directory -- possibly another CoreLib flavour -- so any other failure
            // to list escapes.
            let matches =
                try
                    Directory.EnumerateFiles dir
                    |> Seq.filter (fun candidate ->
                        String.Equals (Path.GetFileName candidate, fileName, StringComparison.OrdinalIgnoreCase)
                    )
                    // Enumeration order is not reproducible; the refusal below names them in order.
                    |> Seq.sort
                    |> List.ofSeq
                with :? DirectoryNotFoundException ->
                    []

            match matches with
            | [] -> None
            | [ single ] ->
                logger.LogInformation ("Loading assembly from file {AssemblyFileLoadPath}", single)

                // Listed is not the same as openable: a dangling symlink lists under its name and
                // then cannot be read, and holds nothing, so the probe goes on to the next dir.
                try
                    Assembly.readFile loggerFactory single |> Some
                with :? FileNotFoundException ->
                    None
            | several ->
                failwith
                    $"TODO: %s{dir} holds %d{List.length several} files named %s{fileName} differing only by case (%A{several}); CoreCLR binds whichever the host listed first among its trusted platform assemblies, an order PawPrint does not reproduce"
        )
