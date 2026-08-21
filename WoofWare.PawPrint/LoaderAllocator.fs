namespace WoofWare.PawPrint

/// A loader allocator: the arena CoreCLR loads a type, method or assembly into, and the unit at
/// which loaded code can be freed. Every `MethodTable`, `MethodDesc` and `Assembly` has one
/// (`LoaderAllocator`, coreclr/vm/loaderallocator.hpp:296).
///
/// One case, because PawPrint has one arena. Adding a second is what makes every use of
/// <see cref="LoaderAllocator.isCollectible"/> a compile error, which is the point: the answers
/// derived from this are guest-visible, so they must all be revisited together rather than
/// discovered one bug report at a time.
[<RequireQualifiedAccess>]
type LoaderAllocator =
    /// CoreCLR's `GlobalLoaderAllocator` (loaderallocator.hpp:905): the process-wide arena that
    /// backs every assembly outside a collectible `AssemblyLoadContext`, and so everything PawPrint
    /// loads.
    | Global

[<RequireQualifiedAccess>]
module LoaderAllocator =
    /// Whether the code in this arena can be unloaded, which is `LoaderAllocator::IsCollectible`
    /// (loaderallocator.hpp:592). Guest-visible: `Type.IsCollectible`, `MethodBase.IsCollectible`
    /// and `Assembly.IsCollectible` are each a QCall that reports it.
    ///
    /// PawPrint answers `false` for everything, and that is a fact about the arena rather than a
    /// simplification. Every collectible arena in CoreCLR is an `AssemblyLoaderAllocator`, whose
    /// constructor is the only thing that passes `true` (`LoaderAllocator(true)`,
    /// loaderallocator.hpp:942), and it is constructed in exactly two places:
    ///
    ///   * `AssemblyNative_InitializeAssemblyLoadContext`, for a collectible
    ///     `AssemblyLoadContext` (assemblynative.cpp:1197);
    ///   * `Assembly::CreateDynamic` (assembly.cpp:458), for
    ///     `AssemblyBuilder.DefineDynamicAssembly` with `AssemblyBuilderAccess.RunAndCollect`,
    ///     reached through the `AppDomain_CreateDynamicAssembly` QCall.
    ///
    /// PawPrint implements neither, nor any other `AssemblyLoadContext` native — `Assembly.Load`
    /// included — so a guest has no route to a second arena and `true` is unreachable by
    /// construction.
    ///
    /// Those two QCalls are therefore what to revisit here, and nothing before one of them lands.
    let isCollectible (allocator : LoaderAllocator) : bool =
        match allocator with
        | LoaderAllocator.Global -> false
