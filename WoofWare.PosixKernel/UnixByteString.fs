namespace WoofWare.PosixKernel

open System
open System.Buffers
open System.Collections.Immutable
open System.Text
open System.Text.Unicode

/// Why a sequence of bytes is not a `UnixByteString`.
[<RequireQualifiedAccess>]
type UnixByteStringDefect =
    /// A NUL at this byte index.
    ///
    /// A Unix kernel takes its strings NUL-terminated across the `char*`
    /// boundary, so a NUL cannot appear within one: everything after it would be
    /// invisible to the kernel.
    | ContainsNul of index : int

/// Shared by `UnixByteString`'s own members and by its module: a member
/// cannot call a function declared after the type it is attached to.
module private UnixByteStringInternal =
    /// `Unchecked.defaultof<UnixByteString>` and C# `default` carry a
    /// *default* `ImmutableArray`, whose underlying array is null, and
    /// `.Length` on that throws rather than answering 0. Every read of the bytes
    /// goes through here, so the failure names the cause instead of surfacing as a
    /// bare `NullReferenceException` from somewhere further in.
    let checkedBytes (bytes : ImmutableArray<byte>) : ImmutableArray<byte> =
        if bytes.IsDefault then
            failwith
                "UnixByteString: value carries no bytes at all, so it can only have come from `Unchecked.defaultof` or C# `default`; construct one with UnixByteString.ofBytes or UnixByteString.ofString instead."

        bytes

    /// Lexicographic on the unsigned bytes, with a proper prefix sorting first.
    /// Total across differing lengths, which `ImmutableArray`'s own
    /// `IStructuralComparable` is not: that one throws on two arrays of
    /// unequal length, and a directory is a map keyed on exactly these.
    let compareBytes (left : ImmutableArray<byte>) (right : ImmutableArray<byte>) : int =
        let left = checkedBytes left
        let right = checkedBytes right
        let shared = min left.Length right.Length

        let mutable i = 0
        let mutable result = 0

        while result = 0 && i < shared do
            result <- compare left.[i] right.[i]
            i <- i + 1

        if result <> 0 then
            result
        else
            compare left.Length right.Length

    let equalBytes (left : ImmutableArray<byte>) (right : ImmutableArray<byte>) : bool =
        let left = checkedBytes left
        let right = checkedBytes right

        if left.Length <> right.Length then
            false
        else

        let mutable i = 0
        let mutable equal = true

        while equal && i < left.Length do
            equal <- left.[i] = right.[i]
            i <- i + 1

        equal

    /// FNV-1a over the bytes. Computed on demand rather than stored: F# `Map`
    /// is a balanced tree and uses comparison, so nothing in the hot path hashes a
    /// name, and a stored hash would be one more field a forged value could make
    /// inconsistent with the bytes beside it.
    let hashBytes (bytes : ImmutableArray<byte>) : int =
        let bytes = checkedBytes bytes
        let mutable hash = 2166136261u

        for i in 0 .. bytes.Length - 1 do
            hash <- (hash ^^^ uint32 bytes.[i]) * 16777619u

        int hash

    /// Decode as strict UTF-8, or `None` if the bytes are not strict UTF-8.
    let tryDecode (bytes : ImmutableArray<byte>) : string option =
        let bytes = checkedBytes bytes
        // Every UTF-8 sequence yields at most as many UTF-16 code units as it has
        // bytes: one-, two- and three-byte sequences give one, and the four-byte
        // sequences give a surrogate pair. So the source length always suffices.
        let destination = Array.zeroCreate<char> bytes.Length
        let mutable bytesRead = 0
        let mutable charsWritten = 0

        let status =
            Utf8.ToUtf16 (bytes.AsSpan (), destination.AsSpan (), &bytesRead, &charsWritten, false, true)

        match status with
        | OperationStatus.Done -> Some (String (destination, 0, charsWritten))
        | OperationStatus.InvalidData
        | OperationStatus.NeedMoreData -> None
        | OperationStatus.DestinationTooSmall ->
            // Answering `None` here would say "not UTF-8" about bytes that are.
            failwith
                $"UnixByteString.tryToString: decoding %d{bytes.Length} bytes overflowed a %d{destination.Length}-code-unit buffer, which cannot happen since UTF-8 never expands into UTF-16. This is a bug in UnixByteString."
        | status -> failwith $"UnixByteString.tryToString: unrecognised decoder status %O{status}."

    /// Total, injective rendering: strict UTF-8 runs verbatim, any other byte
    /// becomes `\xNN`, and a literal backslash doubles.
    let escape (bytes : ImmutableArray<byte>) : string =
        let bytes = checkedBytes bytes
        let builder = StringBuilder bytes.Length
        // As in `tryDecode`: never more code units than there are bytes.
        let destination = Array.zeroCreate<char> bytes.Length
        let mutable i = 0

        while i < bytes.Length do
            let mutable bytesRead = 0
            let mutable charsWritten = 0

            // Decoding the remainder consumes the longest strictly-valid prefix
            // and reports where it stopped, so the scan advances a whole
            // sequence at a time rather than a byte at a time.
            let status =
                Utf8.ToUtf16 (bytes.AsSpan().Slice i, destination.AsSpan (), &bytesRead, &charsWritten, false, true)

            for j in 0 .. charsWritten - 1 do
                let c = destination.[j]

                if c = '\\' then
                    builder.Append "\\\\" |> ignore<StringBuilder>
                else
                    builder.Append c |> ignore<StringBuilder>

            i <- i + bytesRead

            match status with
            | OperationStatus.Done -> ()
            | OperationStatus.InvalidData
            | OperationStatus.NeedMoreData ->
                // `i` is now the first byte the decoder refused. Escape that one
                // byte and resume: a later byte may well start a valid sequence.
                builder.Append('\\').Append('x').Append (bytes.[i].ToString "X2")
                |> ignore<StringBuilder>

                i <- i + 1
            | OperationStatus.DestinationTooSmall ->
                failwith
                    $"UnixByteString.toEscaped: decoding %d{bytes.Length - i} bytes overflowed a %d{destination.Length}-code-unit buffer, which cannot happen since UTF-8 never expands. This is a bug in UnixByteString."
            | status -> failwith $"UnixByteString.toEscaped: unrecognised decoder status %O{status}."

        builder.ToString ()

/// A NUL-free sequence of bytes: what a Unix kernel means by "a string".
///
/// This carries no encoding. A Unix filesystem stores the bytes a caller handed
/// it, so a name need not be text at all, and `tryToString` is a partial
/// function for exactly that reason.
///
/// Construct via `UnixByteString.ofBytes` or `UnixByteString.ofString`.
[<Struct>]
[<CustomEquality>]
[<CustomComparison>]
type UnixByteString =
    private
        {
            Bytes : ImmutableArray<byte>
        }

    /// The escaped rendering; see `UnixByteString.toEscaped`.
    ///
    /// Not round-trippable through any parser here: it exists so that a
    /// diagnostic can name a byte string that has no .NET string.
    override this.ToString () : string =
        UnixByteStringInternal.escape this.Bytes

    override this.Equals (other : obj) : bool =
        match other with
        | :? UnixByteString as other -> UnixByteStringInternal.equalBytes this.Bytes other.Bytes
        | _ -> false

    override this.GetHashCode () : int =
        UnixByteStringInternal.hashBytes this.Bytes

    interface IEquatable<UnixByteString> with
        member this.Equals (other : UnixByteString) : bool =
            UnixByteStringInternal.equalBytes this.Bytes other.Bytes

    interface IComparable<UnixByteString> with
        member this.CompareTo (other : UnixByteString) : int =
            UnixByteStringInternal.compareBytes this.Bytes other.Bytes

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? UnixByteString as other -> UnixByteStringInternal.compareBytes this.Bytes other.Bytes
            // `IComparable`'s contract: every instance sorts after null.
            | null -> 1
            | _ ->
                raise (
                    ArgumentException (
                        $"cannot compare a UnixByteString with %s{other.GetType().FullName}",
                        nameof other
                    )
                )

[<RequireQualifiedAccess>]
module UnixByteString =
    /// Human-readable rendering of a rejection.
    let describe (defect : UnixByteStringDefect) : string =
        match defect with
        | UnixByteStringDefect.ContainsNul index ->
            $"contains a NUL at byte index %d{index}, which cannot survive a C string boundary"

    /// Take these bytes as a Unix byte string, or explain why they are not one.
    ///
    /// Throws if `bytes` is a *default* `ImmutableArray` rather than
    /// an empty one: that is a forged value rather than data, and no byte string a
    /// guest could supply produces it.
    let ofBytes (bytes : ImmutableArray<byte>) : Result<UnixByteString, UnixByteStringDefect> =
        if bytes.IsDefault then
            raise (
                ArgumentException (
                    "a default ImmutableArray is not an empty byte string; it has no underlying array at all",
                    nameof bytes
                )
            )

        let index = bytes.IndexOf 0uy

        if index >= 0 then
            Error (UnixByteStringDefect.ContainsNul index)
        else
            Ok
                {
                    Bytes = bytes
                }

    /// Encode this .NET string as UTF-8 and take the result as a Unix byte string,
    /// or explain why the string has no such encoding.
    ///
    /// This is the constructor for host-supplied configuration, where a path is
    /// written as an F# literal. A guest-supplied path arrives as bytes and goes
    /// through `ofBytes` instead.
    ///
    /// An unpaired surrogate is refused here rather than encoded: it has no UTF-8
    /// encoding, so a strict encoder would throw and a lenient one would silently
    /// substitute U+FFFD and name a different file.
    let ofString (candidate : string) : Result<UnixByteString, UnixPathTextDefect> =
        if isNull candidate then
            nullArg (nameof candidate)

        match UnixPathText.firstDefect candidate with
        | Some defect -> Error defect
        | None ->
            Ok
                {
                    Bytes = UnixPathText.utf8.GetBytes candidate |> ImmutableArray.CreateRange
                }

    /// The bytes, as a Unix kernel would hand them back.
    /// Has no NUL terminator; callers that need a C string append it themselves.
    let toBytes (s : UnixByteString) : ImmutableArray<byte> =
        UnixByteStringInternal.checkedBytes s.Bytes

    /// How many bytes this is.
    let length (s : UnixByteString) : int =
        (UnixByteStringInternal.checkedBytes s.Bytes).Length

    /// The .NET string these bytes name, if they name one at all.
    ///
    /// `None` exactly when the bytes are not strictly-valid UTF-8.
    ///
    /// The Darwin flavour happens to admit exactly the names for which this
    /// is `Some`, but that is a chosen approximation of APFS's rule rather than
    /// APFS's rule: real APFS also refuses Unicode noncharacters and over-long
    /// combining sequences, which this accepts. Do not read a `Some` here as
    /// "a real Darwin would bind this".
    let tryToString (s : UnixByteString) : string option =
        UnixByteStringInternal.tryDecode s.Bytes

    /// A rendering for diagnostics: total, and injective, so two distinct byte
    /// strings never print alike.
    ///
    /// Strictly-valid UTF-8 runs verbatim; every other byte becomes `\xNN`;
    /// a literal backslash doubles. Use this rather than `tryToString` in an
    /// error message: a diagnostic that cannot render the thing it is diagnosing
    /// is no use, and the bytes that fail to decode are exactly the interesting ones.
    let toEscaped (s : UnixByteString) : string = UnixByteStringInternal.escape s.Bytes

    /// Re-check the invariant of a value that may not have come from a constructor,
    /// throwing (with a message containing `context`) if it does not hold.
    ///
    /// The constructors already ensure the invariant holds, so there is no need to
    /// call this if you know where the value came from.
    let assertValid (context : string) (s : UnixByteString) : UnixByteString =
        // The only value this can reject is `Unchecked.defaultof` / C# `default`,
        // whose payload is a default ImmutableArray: the private representation and
        // the constructors' NUL check stop every other route. The `IsDefault` test
        // has to come first, because `.Length` on that payload throws.
        if s.Bytes.IsDefault then
            failwith
                $"%s{context}: this UnixByteString carries no bytes at all, so it can only have come from `Unchecked.defaultof` or C# `default`; construct one with UnixByteString.ofBytes instead."

        let index = s.Bytes.IndexOf 0uy

        if index >= 0 then
            failwith
                $"%s{context}: %s{describe (UnixByteStringDefect.ContainsNul index)}. A UnixByteString that fails its own invariant can only have come from `Unchecked.defaultof` or C# `default`; construct one with UnixByteString.ofBytes instead."

        s
