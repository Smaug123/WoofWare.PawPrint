namespace WoofWare.PawPrint

open System
open System.Buffers.Binary
open System.Collections.Immutable

/// The `libSystem.Security.Cryptography.Native.OpenSsl` shim: the EVP digest entry points
/// `System.Security.Cryptography` reaches on Linux (`Interop.EVP.cs`,
/// `Interop.EVP.DigestAlgs.cs`), plus the `CryptoNative_EnsureOpenSslInitialized` call that
/// `Interop.CryptoInitializer`'s class constructor makes before any of them (every other
/// `Interop.Crypto` member is preceded by that class constructor).
///
/// Only SHA-256 is modelled. The shim's `const EVP_MD*` for it is a fixed sentinel, and every
/// `EVP_MD_CTX` is an entry in `IlMachineState.EvpDigests`, whose handle the guest holds as
/// an `IntPtr`.
///
/// The C side (`pal_evp.c`) is not in the pinned runtime checkout, so the contracts here are
/// OpenSSL's documented `EVP_Digest*` ones (1 on success, 0 on failure; `EVP_DigestFinal_ex`
/// writes the digest and its length; `EVP_MAX_MD_SIZE` is 64) and what the linux-x64 pack's
/// `LiteHash` and `OneShotHashProvider` IL actually does with the answers.
[<RequireQualifiedAccess>]
module NativeCryptoNative =
    let private tryCryptoNativeEntryPoint (ctx : NativeCallContext) : string option =
        match ctx.Instruction.ExecutingMethod.TryNativeImport with
        | Some import when import.ModuleName = "libSystem.Security.Cryptography.Native.OpenSsl" ->
            Some import.EntryPointName
        | _ -> None

    /// `EVP_MAX_MD_SIZE` from `<openssl/evp.h>`: the largest digest any `EVP_MD` produces,
    /// which `Interop.Crypto.EVP_MAX_MD_SIZE` caches and `LiteHash` sizes its buffers by.
    [<Literal>]
    let private evpMaxMdSize = 64

    /// The `const EVP_MD*` handed out for SHA-256. Any nonzero fixed value would do, since
    /// the guest only ever stores it and hands it back; this one spells "MDSHA256" in ASCII
    /// so that it is recognisable in a dump, and cannot collide with the context handles,
    /// which count up from 1.
    [<Literal>]
    let private sha256EvpMd = 0x4D44534841323536L

    let private pushInt32 (value : int) (ctx : NativeCallContext) (state : IlMachineState) : NativeHandlerResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.Int32 (Int32Source.Verbatim value)) ctx.Thread
        |> NativeHandlerResult.completed

    let private pushIntPtr (bits : int64) (ctx : NativeCallContext) (state : IlMachineState) : NativeHandlerResult =
        state
        |> IlMachineState.pushToEvalStack' (EvalStackValue.NativeInt (NativeIntSource.Verbatim bits)) ctx.Thread
        |> NativeHandlerResult.completed

    /// The bits of an `IntPtr` argument that this shim itself handed out earlier (an `EVP_MD`
    /// sentinel or an `EVP_MD_CTX` handle), or zero for a null.
    let private intPtrBits (operation : string) (argName : string) (arg : CliType) : int64 =
        match CliType.unwrapPrimitiveLikeDeep arg with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim bits)) -> bits
        | CliType.RuntimePointer (CliRuntimePointer.Verbatim bits) -> bits
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.ManagedPointer ManagedPointerSource.Null))
        | CliType.RuntimePointer (CliRuntimePointer.Managed ManagedPointerSource.Null) -> 0L
        | other ->
            failwith
                $"%s{operation}: expected %s{argName} to be an IntPtr this shim handed out (an EVP_MD from CryptoNative_EvpSha256, or an EVP_MD_CTX from CryptoNative_EvpMdCtxCreate/CopyEx), got %O{other}"

    /// The algorithm a `const EVP_MD*` argument names.
    let private algorithmOfEvpMd (operation : string) (argName : string) (arg : CliType) : EvpDigestAlgorithm =
        match intPtrBits operation argName arg with
        | bits when bits = sha256EvpMd -> EvpDigestAlgorithm.Sha256
        | 0L ->
            failwith
                $"%s{operation}: %s{argName} is a null EVP_MD. CoreLib never passes one: HashAlgorithmToEvp throws for a name it has no EVP_MD for, so this is a hand-rolled P/Invoke. Pass the EVP_MD CryptoNative_EvpSha256 returns."
        | bits ->
            failwith
                $"%s{operation}: %s{argName} is 0x%x{bits}, which is not an EVP_MD this shim handed out. Only CryptoNative_EvpSha256's is modelled."

    /// The context an `EVP_MD_CTX*` argument names, for an entry point that dereferences it.
    let private ctxHandleOfArgument (operation : string) (argName : string) (arg : CliType) : EvpMdCtxHandle =
        match intPtrBits operation argName arg with
        | 0L ->
            failwith
                $"%s{operation}: %s{argName} is a null EVP_MD_CTX. CoreLib never marshals one: CheckValidOpenSslHandle rejects an invalid SafeEvpMdCtxHandle at creation, so this is a hand-rolled P/Invoke, and a real run would fault here."
        | bits -> EvpMdCtxHandle bits

    let private withRegistry (registry : EvpDigestRegistry) (state : IlMachineState) : IlMachineState =
        { state with
            EvpDigests = registry
        }

    /// `*md = digest; *s = digest.Length`, as `EVP_DigestFinal_ex` leaves its two
    /// out-parameters.
    ///
    /// Both must name storage: `md` is written unconditionally by OpenSSL, and `s` by the shim
    /// on success. CoreLib always passes a pinned span and a local for them; what a real run
    /// does with a null `s` is not in the pinned source, so it is refused rather than guessed.
    let private writeDigest
        (ctx : NativeCallContext)
        (operation : string)
        (md : BufferPointer)
        (s : BufferPointer)
        (digest : ImmutableArray<byte>)
        (state : IlMachineState)
        : IlMachineState
        =
        let mdStorage = NativeSystemNative.requireUnscreenedStorage operation "md" md
        let sStorage = NativeSystemNative.requireUnscreenedStorage operation "s" s

        let size = Array.zeroCreate<byte> 4
        BinaryPrimitives.WriteUInt32LittleEndian (Span<byte> size, uint32 digest.Length)

        state
        |> NativeSystemNative.writeBytesThrough ctx operation mdStorage digest
        |> NativeSystemNative.writeBytesThrough ctx operation sStorage (ImmutableArray.CreateRange size)

    /// A digest algorithm accessor this shim does not model. Named, so that a guest asking for
    /// MD5 is told that MD5 is the gap rather than answered with SHA-256's `EVP_MD` or with a
    /// null one. A null is no safer a stand-in: `HashAlgorithmSupported` reports MD5, SHA-1,
    /// SHA-384 and SHA-512 as supported whatever these accessors answer, and
    /// `HashAlgorithmToEvp` passes the pointer on unchecked, so a null would surface as a
    /// failure somewhere further in rather than as "not supported on this platform".
    let private unmodelledAlgorithm (entryPoint : string) (algorithm : string) : NativeHandlerResult option =
        failwith
            $"%s{entryPoint}: the %s{algorithm} digest is not modelled; only SHA-256 (CryptoNative_EvpSha256) is. Implement %s{algorithm} in EvpDigestRegistry.fs and NativeCryptoNative.fs."

    let tryExecute (ctx : NativeCallContext) : NativeHandlerResult option =
        let state = ctx.State
        let instruction = ctx.Instruction

        match
            tryCryptoNativeEntryPoint ctx,
            instruction.ExecutingMethod.Signature.ParameterTypes,
            instruction.ExecutingMethod.Signature.ReturnType
        with
        // `int32_t CryptoNative_EnsureOpenSslInitialized(void)`: libcrypto's threading and
        // error-string setup, run once by `Interop.CryptoInitializer`'s class constructor, which
        // throws `InvalidOperationException` on anything but 0. There is no libcrypto to set up.
        | Some "CryptoNative_EnsureOpenSslInitialized",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pushInt32 0 ctx state |> Some
        // `int32_t CryptoNative_GetMaxMdSize(void)`: `EVP_MAX_MD_SIZE`.
        | Some "CryptoNative_GetMaxMdSize",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            pushInt32 evpMaxMdSize ctx state |> Some
        // `const EVP_MD* CryptoNative_EvpSha256(void)`: `EVP_sha256()`.
        | Some "CryptoNative_EvpSha256",
          [],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            pushIntPtr sha256EvpMd ctx state |> Some
        | Some ("CryptoNative_EvpMd5" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "MD5"
        | Some ("CryptoNative_EvpSha1" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHA-1"
        | Some ("CryptoNative_EvpSha384" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHA-384"
        | Some ("CryptoNative_EvpSha512" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHA-512"
        | Some ("CryptoNative_EvpSha3_256" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHA3-256"
        | Some ("CryptoNative_EvpSha3_384" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHA3-384"
        | Some ("CryptoNative_EvpSha3_512" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHA3-512"
        | Some ("CryptoNative_EvpShake128" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHAKE128"
        | Some ("CryptoNative_EvpShake256" as entryPoint), [], MethodReturnType.Returns _ ->
            unmodelledAlgorithm entryPoint "SHAKE256"
        // `int32_t CryptoNative_EvpMdSize(const EVP_MD* md)`: `EVP_MD_get_size`.
        | Some "CryptoNative_EvpMdSize",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CryptoNative_EvpMdSize"
            let algorithm = algorithmOfEvpMd operation "md" instruction.Arguments.[0]
            pushInt32 (EvpDigestAlgorithm.digestSize algorithm) ctx state |> Some
        // `EVP_MD_CTX* CryptoNative_EvpMdCtxCreate(const EVP_MD* type)`: `EVP_MD_CTX_new`
        // then `EVP_DigestInit_ex(ctx, type, NULL)`. The managed wrapper wraps the answer in a
        // `SafeEvpMdCtxHandle`, whose `IsInvalid` is a comparison with `IntPtr.Zero`.
        | Some "CryptoNative_EvpMdCtxCreate",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "CryptoNative_EvpMdCtxCreate"
            let algorithm = algorithmOfEvpMd operation "type" instruction.Arguments.[0]

            let EvpMdCtxHandle bits, registry =
                EvpDigestRegistry.create algorithm state.EvpDigests

            withRegistry registry state |> pushIntPtr bits ctx |> Some
        // `EVP_MD_CTX* CryptoNative_EvpMdCtxCopyEx(const EVP_MD_CTX* ctx)`: a fresh context
        // that `EVP_MD_CTX_copy_ex` has made a duplicate of `ctx`.
        | Some "CryptoNative_EvpMdCtxCopyEx",
          [ ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.IntPtr) ->
            let operation = "CryptoNative_EvpMdCtxCopyEx"
            let source = ctxHandleOfArgument operation "ctx" instruction.Arguments.[0]

            let EvpMdCtxHandle bits, registry =
                EvpDigestRegistry.copy operation source state.EvpDigests

            withRegistry registry state |> pushIntPtr bits ctx |> Some
        // `void CryptoNative_EvpMdCtxDestroy(EVP_MD_CTX* ctx)`: `EVP_MD_CTX_free`, which is
        // documented as a no-op on NULL.
        | Some "CryptoNative_EvpMdCtxDestroy", [ ConcreteIntPtr state.ConcreteTypes ], MethodReturnType.Void ->
            let operation = "CryptoNative_EvpMdCtxDestroy"

            match intPtrBits operation "ctx" instruction.Arguments.[0] with
            | 0L -> NativeHandlerResult.completed state |> Some
            | bits ->
                let registry =
                    EvpDigestRegistry.destroy operation (EvpMdCtxHandle bits) state.EvpDigests

                withRegistry registry state |> NativeHandlerResult.completed |> Some
        // `int32_t CryptoNative_EvpDigestReset(EVP_MD_CTX* ctx, const EVP_MD* type)`:
        // `EVP_DigestInit_ex(ctx, type, NULL)`, which discards whatever the context was doing.
        | Some "CryptoNative_EvpDigestReset",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcreteIntPtr state.ConcreteTypes ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CryptoNative_EvpDigestReset"
            let handle = ctxHandleOfArgument operation "ctx" instruction.Arguments.[0]
            let algorithm = algorithmOfEvpMd operation "type" instruction.Arguments.[1]

            let registry =
                EvpDigestRegistry.set operation handle (EvpDigestContext.reset algorithm) state.EvpDigests

            withRegistry registry state |> pushInt32 1 ctx |> Some
        // `int32_t CryptoNative_EvpDigestUpdate(EVP_MD_CTX* ctx, const void* d, int32_t cnt)`:
        // `EVP_DigestUpdate(ctx, d, (size_t)cnt)`.
        | Some "CryptoNative_EvpDigestUpdate",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32 ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CryptoNative_EvpDigestUpdate"
            let handle = ctxHandleOfArgument operation "ctx" instruction.Arguments.[0]
            let count = NativeCall.int32Argument operation instruction.Arguments.[2]

            if count < 0 then
                failwith
                    $"%s{operation}: EVP_MD_CTX %O{handle} was given cnt %d{count}, which is negative. The shim casts that to a size_t of several exabytes and OpenSSL reads that far; CoreLib passes a span's length, so this is a hand-rolled P/Invoke."

            // Zero bytes read nothing, so the buffer is never resolved: `LiteHash.Append` returns
            // before the call for an empty span, but a hand-rolled P/Invoke may pass `(NULL, 0)`,
            // which OpenSSL accepts.
            let bytes =
                if count = 0 then
                    ImmutableArray<byte>.Empty
                else
                    let buffer =
                        NativeSystemNative.bufferPointerArgument operation "d" instruction.Arguments.[1]

                    let storage = NativeSystemNative.requireUnscreenedStorage operation "d" buffer
                    NativeSystemNative.readBytesThrough ctx operation storage count state

            let context =
                EvpDigestRegistry.get operation handle state.EvpDigests
                |> EvpDigestContext.update operation handle bytes

            let registry = EvpDigestRegistry.set operation handle context state.EvpDigests
            withRegistry registry state |> pushInt32 1 ctx |> Some
        // `int32_t CryptoNative_EvpDigestFinalEx(EVP_MD_CTX* ctx, uint8_t* md, uint32_t* s)`:
        // `EVP_DigestFinal_ex`, after which the context accepts only a reset.
        | Some "CryptoNative_EvpDigestFinalEx",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CryptoNative_EvpDigestFinalEx"
            let handle = ctxHandleOfArgument operation "ctx" instruction.Arguments.[0]

            let md =
                NativeSystemNative.bufferPointerArgument operation "md" instruction.Arguments.[1]

            let s =
                NativeSystemNative.bufferPointerArgument operation "s" instruction.Arguments.[2]

            let digest, context =
                EvpDigestRegistry.get operation handle state.EvpDigests
                |> EvpDigestContext.finalise operation handle

            let registry = EvpDigestRegistry.set operation handle context state.EvpDigests

            withRegistry registry state
            |> writeDigest ctx operation md s digest
            |> pushInt32 1 ctx
            |> Some
        // `int32_t CryptoNative_EvpDigestCurrent(const EVP_MD_CTX* ctx, uint8_t* md, uint32_t* s)`:
        // `EVP_DigestFinal_ex` on an `EVP_MD_CTX_copy_ex` of the context, so the context itself
        // keeps running.
        | Some "CryptoNative_EvpDigestCurrent",
          [ ConcreteIntPtr state.ConcreteTypes ; ConcretePointer _ ; ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CryptoNative_EvpDigestCurrent"
            let handle = ctxHandleOfArgument operation "ctx" instruction.Arguments.[0]

            let md =
                NativeSystemNative.bufferPointerArgument operation "md" instruction.Arguments.[1]

            let s =
                NativeSystemNative.bufferPointerArgument operation "s" instruction.Arguments.[2]

            let digest =
                EvpDigestRegistry.get operation handle state.EvpDigests
                |> EvpDigestContext.current operation handle

            state |> writeDigest ctx operation md s digest |> pushInt32 1 ctx |> Some
        // `int32_t CryptoNative_EvpDigestOneShot(const EVP_MD* type, const void* source,
        // int32_t sourceSize, uint8_t* md, uint32_t* mdSize)`: init, update and final on a
        // context of its own.
        | Some "CryptoNative_EvpDigestOneShot",
          [ ConcreteIntPtr state.ConcreteTypes
            ConcretePointer _
            ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32
            ConcretePointer _
            ConcretePointer _ ],
          MethodReturnType.Returns (ConcretePrimitive state.ConcreteTypes PrimitiveType.Int32) ->
            let operation = "CryptoNative_EvpDigestOneShot"
            let algorithm = algorithmOfEvpMd operation "type" instruction.Arguments.[0]
            let sourceSize = NativeCall.int32Argument operation instruction.Arguments.[2]

            if sourceSize < 0 then
                failwith
                    $"%s{operation}: sourceSize %d{sourceSize} is negative. CoreLib passes a span's length, so this is a hand-rolled P/Invoke, and what the shim answers for it is not in the pinned source."

            // An empty `ReadOnlySpan` pins to a null reference, so `source` is null exactly
            // when `sourceSize` is 0 on CoreLib's own path; nothing is read in that case.
            let bytes =
                if sourceSize = 0 then
                    ImmutableArray<byte>.Empty
                else
                    let buffer =
                        NativeSystemNative.bufferPointerArgument operation "source" instruction.Arguments.[1]

                    let storage = NativeSystemNative.requireUnscreenedStorage operation "source" buffer
                    NativeSystemNative.readBytesThrough ctx operation storage sourceSize state

            let md =
                NativeSystemNative.bufferPointerArgument operation "md" instruction.Arguments.[3]

            let mdSize =
                NativeSystemNative.bufferPointerArgument operation "mdSize" instruction.Arguments.[4]

            let digest =
                EvpDigestState.fresh algorithm
                |> EvpDigestState.update bytes
                |> EvpDigestState.digest

            state |> writeDigest ctx operation md mdSize digest |> pushInt32 1 ctx |> Some
        | _ -> None
