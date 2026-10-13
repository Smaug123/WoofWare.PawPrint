namespace WoofWare.PosixKernel

/// How far a syscall got: it is over, or it has stopped at the point where the
/// kernel copies from the caller's memory.
///
/// A call stops there because the client may not be able to produce the bytes
/// without failing: a foreign-function layer whose memory is not a flat array
/// has to resolve the pointer, and resolving it can be a mistake in itself.
/// Every answer the call gives *without* reading that memory comes first, so
/// that the client reads it only where a real kernel would.
///
/// Resuming a `'Paused` with what it asks for answers `'Answer`, the same type
/// `Answered` carries, or, where the call copies again after that, another
/// `CallProgress` with the same `'Answer`. Each paused type says what to copy:
/// one that copies bytes has a `copyRange`, and its module the function that
/// resumes it.
[<RequireQualifiedAccess>]
type CallProgress<'Paused, 'Answer> =
    /// The call is over, without reading (any more of) the caller's memory.
    | Answered of answer : 'Answer
    /// The call has stopped where the kernel copies from the caller's memory.
    /// `paused` holds the system and everything the call has decided so far,
    /// so nothing else can be made to happen in between.
    | NeedsCopyIn of paused : 'Paused

/// The bytes of a buffer the caller passed that a paused call copies next:
/// `ByteCount` bytes, starting `FirstByte` bytes into the buffer.
type CopyRange =
    {
        /// How far into the caller's buffer the bytes start. Zero unless part of
        /// the buffer was copied before, as by a write that slept partway.
        FirstByte : int
        /// How many bytes the call copies: exactly this many must be supplied.
        ByteCount : int
    }
