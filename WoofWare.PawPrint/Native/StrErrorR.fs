namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Text
open WoofWare.PosixKernel

/// What System.Native's `SystemNative_StrErrorR(platformErrno, buffer,
/// bufferSize)` does: which pointer it returns, and what it leaves in the
/// caller's buffer.
[<RequireQualifiedAccess>]
type StrErrorRAnswer =
    /// NULL, with the buffer untouched: the shim refuses a negative size
    /// before anything else.
    | RefusedSize
    /// A NUL-terminated string of the C library's own, with the buffer
    /// untouched and the size ignored: GNU `strerror_r`'s answer for a number
    /// glibc names an error for (and for 0). Real glibc returns the same
    /// read-only pointer on every call.
    | LibraryText of text : string
    /// The buffer, after `written` was copied to its start. `written` is the
    /// text cut to `bufferSize - 1` bytes and NUL-terminated, or empty for a
    /// size of 0.
    | Buffer of written : ImmutableArray<byte>
    /// NULL, after `written` was copied to the buffer's start, cut as for
    /// `Buffer`: XSI `strerror_r`'s ERANGE, for a named error whose text did
    /// not fit. The caller then reads the buffer.
    | NullAfterWriting of written : ImmutableArray<byte>

[<RequireQualifiedAccess>]
module StrErrorR =

    /// `strlcpy`, `snprintf("%s")` and `strerror_r` all leave a buffer of
    /// `size` bytes holding as much of `text` as fits before a NUL, and write
    /// nothing at all into a buffer of size 0 (measured on both flavours at
    /// every size from 0 to the text's length plus two).
    let private cut (text : string) (bufferSize : int) : ImmutableArray<byte> * bool =
        let bytes = Encoding.ASCII.GetBytes text

        if bufferSize = 0 then
            ImmutableArray.Empty, false
        else
            let kept = min bytes.Length (bufferSize - 1)
            let written = Array.zeroCreate<byte> (kept + 1)
            Array.blit bytes 0 written 0 kept
            ImmutableArray.Create<byte> written, kept = bytes.Length

    /// The shim's answer, built against `library` and run on it.
    ///
    /// `platformErrno` is in the library's own numbering
    /// (`CLibrary.errorOfNumber`), which is what CoreLib passes: the raw errno
    /// a call left, or `SystemNative_ConvertErrorPalToPlatform`'s answer.
    ///
    /// The shim's own arms come first (`StrErrorR`, pal_error_common.h): a
    /// negative size is NULL; -0x20001 (`EHOSTNOTFOUND`) is
    /// `gai_strerror(EAI_NONAME)` and -0x20002 (`ESOCKETERROR`) "Unknown
    /// socket error", each copied into the buffer, which is returned. Every
    /// other number goes to `strerror_r`.
    let answer (library : CLibrary) (platformErrno : int) (bufferSize : int) : StrErrorRAnswer =
        if bufferSize < 0 then
            StrErrorRAnswer.RefusedSize
        elif platformErrno = -0x20001 then
            StrErrorRAnswer.Buffer (fst (cut (CLibrary.nameNotKnownText library) bufferSize))
        elif platformErrno = -0x20002 then
            StrErrorRAnswer.Buffer (fst (cut "Unknown socket error" bufferSize))
        else

        let named =
            if platformErrno = 0 then
                Some (CLibrary.successText library)
            else
                match CLibrary.errorOfNumber library platformErrno with
                | None -> None
                | Some error ->
                    match CLibrary.errorText library error with
                    | Some text -> Some text
                    | None ->
                        failwith
                            $"StrErrorR.answer: %d{platformErrno} decodes to %O{error} under %O{library}, which has no text for it (this is a transcription bug in CLibrary)."

        match library, named with
        | CLibrary.Glibc _, Some text -> StrErrorRAnswer.LibraryText text
        | CLibrary.Glibc _, None ->
            // `snprintf(buf, buflen, "Unknown error %d")`, into the buffer.
            StrErrorRAnswer.Buffer (fst (cut (CLibrary.unknownErrorText library platformErrno) bufferSize))
        | CLibrary.DarwinLibc, Some text ->
            match cut text bufferSize with
            | written, true -> StrErrorRAnswer.Buffer written
            | written, false -> StrErrorRAnswer.NullAfterWriting written
        | CLibrary.DarwinLibc, None ->
            // EINVAL, which the shim treats as success whether or not the text
            // fitted.
            StrErrorRAnswer.Buffer (fst (cut (CLibrary.unknownErrorText library platformErrno) bufferSize))
