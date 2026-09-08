namespace WoofWare.PawPrint

open System.Collections.Immutable

/// A digest algorithm the emulated OpenSSL shim can run: what a `const EVP_MD*` names.
[<RequireQualifiedAccess>]
type EvpDigestAlgorithm = | Sha256

[<RequireQualifiedAccess>]
module EvpDigestAlgorithm =
    /// `EVP_MD_get_size`: the byte length of a finished digest.
    let digestSize (algorithm : EvpDigestAlgorithm) : int =
        match algorithm with
        | EvpDigestAlgorithm.Sha256 -> Sha256.DigestSize

/// Identifies one live `EVP_MD_CTX`. The number is what the guest holds as its `IntPtr`, so
/// it is minted from a counter rather than from anything the host could vary.
[<Struct>]
type EvpMdCtxHandle = | EvpMdCtxHandle of int64

/// A running digest computation: the algorithm and its state, kept together so that they
/// cannot disagree.
[<RequireQualifiedAccess>]
type EvpDigestState = | Sha256 of Sha256State

[<RequireQualifiedAccess>]
module EvpDigestState =
    /// `EVP_DigestInit_ex`: a fresh computation for `algorithm`.
    let fresh (algorithm : EvpDigestAlgorithm) : EvpDigestState =
        match algorithm with
        | EvpDigestAlgorithm.Sha256 -> EvpDigestState.Sha256 Sha256.empty

    let algorithm (state : EvpDigestState) : EvpDigestAlgorithm =
        match state with
        | EvpDigestState.Sha256 _ -> EvpDigestAlgorithm.Sha256

    /// `EVP_DigestUpdate`.
    let update (bytes : ImmutableArray<byte>) (state : EvpDigestState) : EvpDigestState =
        match state with
        | EvpDigestState.Sha256 sha -> EvpDigestState.Sha256 (Sha256.update bytes sha)

    /// The digest of everything fed so far, of length `EvpDigestAlgorithm.digestSize`. Pure:
    /// whether the context may continue is the context's business, not the computation's.
    let digest (state : EvpDigestState) : ImmutableArray<byte> =
        match state with
        | EvpDigestState.Sha256 sha -> Sha256.finish sha

/// What one `EVP_MD_CTX` is doing.
[<RequireQualifiedAccess>]
type EvpDigestContext =
    /// Between `EVP_DigestInit_ex` and `EVP_DigestFinal_ex`: accepts updates.
    | Running of EvpDigestState
    /// After `EVP_DigestFinal_ex`. OpenSSL permits no further `EVP_DigestUpdate` on such a
    /// context, only a re-initialisation, so the only thing kept is which algorithm it ran.
    | Finalised of EvpDigestAlgorithm

[<RequireQualifiedAccess>]
module EvpDigestContext =
    /// `EVP_DigestInit_ex`: whatever the context was doing, it now runs a fresh `algorithm`.
    let reset (algorithm : EvpDigestAlgorithm) : EvpDigestContext =
        EvpDigestContext.Running (EvpDigestState.fresh algorithm)

    let algorithm (context : EvpDigestContext) : EvpDigestAlgorithm =
        match context with
        | EvpDigestContext.Running state -> EvpDigestState.algorithm state
        | EvpDigestContext.Finalised algorithm -> algorithm

    /// The running computation, for an operation OpenSSL defines only on an initialised,
    /// not-yet-finalised context. `operation` and `handle` name the offender when it is not.
    let private running (operation : string) (handle : EvpMdCtxHandle) (context : EvpDigestContext) : EvpDigestState =
        match context with
        | EvpDigestContext.Running state -> state
        | EvpDigestContext.Finalised algorithm ->
            failwith
                $"%s{operation}: EVP_MD_CTX %O{handle} has already been finalised (it ran %O{algorithm}). OpenSSL permits no further digest operation on such a context except EVP_DigestInit_ex; CoreLib always resets before reusing one, so this is a hand-rolled P/Invoke."

    /// `EVP_DigestUpdate`.
    let update
        (operation : string)
        (handle : EvpMdCtxHandle)
        (bytes : ImmutableArray<byte>)
        (context : EvpDigestContext)
        : EvpDigestContext
        =
        EvpDigestContext.Running (EvpDigestState.update bytes (running operation handle context))

    /// `EVP_DigestFinal_ex`: the digest, and the context as it is afterwards.
    let finalise
        (operation : string)
        (handle : EvpMdCtxHandle)
        (context : EvpDigestContext)
        : ImmutableArray<byte> * EvpDigestContext
        =
        let state = running operation handle context
        EvpDigestState.digest state, EvpDigestContext.Finalised (EvpDigestState.algorithm state)

    /// `EVP_DigestFinal_ex` on a copy of the context: the digest so far, with the context left
    /// running.
    let current (operation : string) (handle : EvpMdCtxHandle) (context : EvpDigestContext) : ImmutableArray<byte> =
        EvpDigestState.digest (running operation handle context)

/// Every live `EVP_MD_CTX`, keyed by the handle the guest holds.
type EvpDigestRegistry =
    private
        {
            NextHandle : int64
            Contexts : Map<EvpMdCtxHandle, EvpDigestContext>
        }

[<RequireQualifiedAccess>]
module EvpDigestRegistry =
    let empty () : EvpDigestRegistry =
        {
            // From 1, so that no handle is `IntPtr.Zero`: `SafeEvpMdCtxHandle.IsInvalid` is
            // exactly that comparison.
            NextHandle = 1L
            Contexts = Map.empty
        }

    let private mint (context : EvpDigestContext) (registry : EvpDigestRegistry) : EvpMdCtxHandle * EvpDigestRegistry =
        let handle = EvpMdCtxHandle registry.NextHandle

        let registry =
            {
                NextHandle = registry.NextHandle + 1L
                Contexts = registry.Contexts |> Map.add handle context
            }

        handle, registry

    /// `EVP_MD_CTX_new` followed by `EVP_DigestInit_ex`: a fresh context running `algorithm`.
    let create (algorithm : EvpDigestAlgorithm) (registry : EvpDigestRegistry) : EvpMdCtxHandle * EvpDigestRegistry =
        mint (EvpDigestContext.reset algorithm) registry

    /// The context behind `handle`. Fails, naming `operation` and the handle, for a handle this
    /// registry never minted or has since destroyed: on a real run that is a use of freed or
    /// invented memory.
    let get (operation : string) (handle : EvpMdCtxHandle) (registry : EvpDigestRegistry) : EvpDigestContext =
        match registry.Contexts |> Map.tryFind handle with
        | Some context -> context
        | None ->
            failwith
                $"%s{operation}: EVP_MD_CTX %O{handle} is not a live digest context: it was never created by EvpMdCtxCreate/EvpMdCtxCopyEx, or has been destroyed. A real run would read freed or invented memory here."

    /// Replace the context behind `handle`, which must be live.
    let set
        (operation : string)
        (handle : EvpMdCtxHandle)
        (context : EvpDigestContext)
        (registry : EvpDigestRegistry)
        : EvpDigestRegistry
        =
        // Existence is checked first so that a store to a dead handle fails the same way a read
        // does, rather than resurrecting it.
        get operation handle registry |> ignore<EvpDigestContext>

        { registry with
            Contexts = registry.Contexts |> Map.add handle context
        }

    /// `EVP_MD_CTX_copy_ex` into a fresh context: a second handle whose computation continues
    /// independently of the original's.
    let copy
        (operation : string)
        (handle : EvpMdCtxHandle)
        (registry : EvpDigestRegistry)
        : EvpMdCtxHandle * EvpDigestRegistry
        =
        mint (get operation handle registry) registry

    /// `EVP_MD_CTX_free`. Fails for a handle that is not live: that is a double free.
    let destroy (operation : string) (handle : EvpMdCtxHandle) (registry : EvpDigestRegistry) : EvpDigestRegistry =
        get operation handle registry |> ignore<EvpDigestContext>

        { registry with
            Contexts = registry.Contexts |> Map.remove handle
        }

    /// Whether `handle` names a context that has been created and not yet destroyed.
    let isLive (handle : EvpMdCtxHandle) (registry : EvpDigestRegistry) : bool =
        registry.Contexts |> Map.containsKey handle
