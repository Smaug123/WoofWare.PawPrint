namespace WoofWare.PosixKernel.Test

open System
open FsUnitTyped
open NUnit.Framework
open WoofWare.PosixKernel

/// `SockaddrField.reachedBy`, which is the one piece of arithmetic the layout
/// descriptors carry.
///
/// It is public and takes a publicly-constructible record, so unlike the closed
/// `SockaddrFamilyField` it can be handed values no struct has. The rows below
/// are mostly about that: what the real descriptors answer is covered by the
/// entry points that consult them.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSockaddrField =

    /// The boundary is inclusive at the field's *end*: a length that reaches the
    /// last byte reaches the field, and one byte less does not.
    [<Test>]
    let ``a length reaches a field exactly when it covers its last byte`` () : unit =
        let field =
            {
                Offset = 4
                Width = 4
            }

        for length in [ 0 ; 1 ; 4 ; 7 ] do
            SockaddrField.reachedBy field length |> shouldEqual false

        for length in [ 8 ; 9 ; 128 ; Int32.MaxValue ] do
            SockaddrField.reachedBy field length |> shouldEqual true

    /// A zero-width field is reached by any length that reaches its offset,
    /// which is what makes the comparison "covers the last byte" rather than
    /// "is strictly past the offset".
    [<Test>]
    let ``a zero-width field is reached at its own offset`` () : unit =
        let field =
            {
                Offset = 4
                Width = 0
            }

        SockaddrField.reachedBy field 3 |> shouldEqual false
        SockaddrField.reachedBy field 4 |> shouldEqual true

    /// A negative length fails every field. A layer that casts such a length to
    /// an unsigned type makes the bound enormous instead, so this answers for
    /// what the caller declared rather than for what a cast would produce.
    [<Test>]
    let ``a negative length reaches nothing`` () : unit =
        let atZero =
            {
                Offset = 0
                Width = 0
            }

        for length in [ -1 ; -16 ; Int32.MinValue ] do
            SockaddrField.reachedBy InternetSockaddr.port length |> shouldEqual false
            SockaddrField.reachedBy atZero length |> shouldEqual false

    /// A field whose end is past `Int32.MaxValue` is not reached, rather than
    /// wrapping onto a low bound that every length satisfies. Adding the two
    /// would answer `true` here.
    [<Test>]
    let ``a field whose end overflows is reached by nothing`` () : unit =
        let field =
            {
                Offset = Int32.MaxValue
                Width = 1
            }

        for length in [ 0 ; 16 ; Int32.MaxValue ] do
            SockaddrField.reachedBy field length |> shouldEqual false

    /// A descriptor with a negative offset or width describes no part of any
    /// struct, and is refused rather than answered for.
    [<Test>]
    let ``a malformed descriptor is a caller bug`` () : unit =
        let malformed =
            [
                {
                    Offset = -1
                    Width = 4
                }
                {
                    Offset = 4
                    Width = -1
                }
                {
                    Offset = Int32.MinValue
                    Width = Int32.MinValue
                }
            ]

        for field in malformed do
            let e =
                Assert.Throws<exn> (fun () -> SockaddrField.reachedBy field 16 |> ignore<bool>)

            e.Message |> shouldContainText "describes no part of any struct"

    /// The family predicate is the same arithmetic, and answers the same:
    /// Darwin's one byte at offset 1 needs a length of 2, as Linux's two bytes
    /// at offset 0 do — the one input on which the two flavours agree despite
    /// disagreeing about the field.
    [<Test>]
    let ``the family predicate agrees with the shared arithmetic`` () : unit =
        for platform in [ SimulatedUnixPlatform.linuxX64 ; SimulatedUnixPlatform.macOsArm64 ] do
            let descriptor = SimulatedUnixPlatform.sockaddrFamilyField platform

            let asField =
                {
                    Offset = SockaddrFamilyField.offset descriptor
                    Width = SockaddrFamilyField.width descriptor
                }

            for length in [ -1 ; 0 ; 1 ; 2 ; 3 ; 16 ] do
                SockaddrFamilyField.reachedBy descriptor length
                |> shouldEqual (SockaddrField.reachedBy asField length)

            SockaddrFamilyField.reachedBy descriptor 1 |> shouldEqual false
            SockaddrFamilyField.reachedBy descriptor 2 |> shouldEqual true
