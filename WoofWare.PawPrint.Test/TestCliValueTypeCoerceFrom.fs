namespace WoofWare.PawPrint.Test

open System.Collections.Generic
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `CliValueType.CoerceFrom` rebuilds a value type from a source's contents while preserving the
/// target's shape. For explicit-layout structs with overlapping fields, `CliValueType.ToBytes`
/// resolves overlaps by replaying field writes in `EditedAtTime` order — so the rebuild must
/// carry per-field timestamps across. This test locks the invariant in place.
[<TestFixture>]
module TestCliValueTypeCoerceFrom =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        use stream = File.OpenRead corelibPath
        Assembly.read loggerFactory (Some corelibPath) stream

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary.CreateRange [ KeyValuePair (corelib.Name.FullName, corelib) ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    /// Stand-in for "some non-primitive-like value type" — in real usage, a value type with an
    /// explicit overlapping-field layout is always user-defined (the `[StructLayout(Explicit)]`
    /// pattern) and carries its own `ConcreteTypeHandle` derived from a user-assembly
    /// `ResolvedTypeIdentity`. We can't fabricate such an identity from an external test (the
    /// constructor is `internal`) and running a full concretisation of a synthetic assembly
    /// would be overkill for a pure-algebra test, so we reuse `bct.TypedReference`'s handle:
    /// it's already in `allCt`, it's a value type, and crucially it's *not* in the primitive-like
    /// wrapper registry (`PrimitiveLikeStruct.kind` lists `IntPtr`/`UIntPtr`/the runtime handles/
    /// `ByReference`, and nothing else). The only part of `_Declared` that `CoerceFrom`
    /// inspects is that registry classification; the handle's nominal identity is otherwise just
    /// a label in error messages.
    let private declaredHandle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.TypedReference

    let private int32Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int32

    let private int64Handle : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt bct.Int64

    /// Two fields overlapping at offset 0: `A` covers bytes 0-3, `B` covers bytes 0-7. Declaration
    /// order is [A; B] so that the corruption (timestamps reset to 0) would make `B` win every
    /// overlap — opposite of the correct "last-written-at-offset-0 is `A`" semantics.
    let private buildOverlapping () : CliValueType =
        let a : CliField =
            {
                Name = "A"
                Contents = CliType.Numeric (CliNumericType.Int32 0)
                Offset = Some 0
                Type = int32Handle
            }

        let b : CliField =
            {
                Name = "B"
                Contents = CliType.Numeric (CliNumericType.Int64 0L)
                Offset = Some 0
                Type = int64Handle
            }

        let layout : Layout = Layout.Custom (size = 8, packingSize = 0)

        CliValueType.OfFields bct allCt declaredHandle layout [ a ; b ]

    [<Test>]
    let ``CoerceFrom preserves overlap write order for explicit-layout unions`` () : unit =
        let initial = buildOverlapping ()

        // Write to A last so its `EditedAtTime` exceeds B's. Under `ToBytes`, B replays first,
        // then A overwrites bytes 0-3: the low 4 bytes must carry the Int32 we just wrote.
        let after =
            CliValueType.WithFieldSet "A" (CliType.Numeric (CliNumericType.Int32 0x01020304)) initial

        let expectedBytes = CliValueType.ToBytes after

        let identityCoerce (_targetContents : CliType) (sourceContents : CliType) : CliType = sourceContents

        let roundTripped = CliValueType.CoerceFrom identityCoerce after after

        let actualBytes = CliValueType.ToBytes roundTripped

        actualBytes |> shouldEqual expectedBytes

    /// Sanity check: the fixture really produces distinct expected vs. corrupt bytes. If this
    /// test ever degenerates (both orderings collide), the overlap-preservation test would
    /// silently stop proving anything.
    [<Test>]
    let ``overlap fixture bytes differ between ts-sorted and declaration-sorted replays`` () : unit =
        let after =
            buildOverlapping ()
            |> CliValueType.WithFieldSet "A" (CliType.Numeric (CliNumericType.Int32 0x01020304))

        let tsOrdered = CliValueType.ToBytes after

        let declarationOrdered : byte[] =
            let buf : byte[] = Array.zeroCreate 8
            let aBytes = CliType.ToBytes (CliType.Numeric (CliNumericType.Int32 0x01020304))
            let bBytes = CliType.ToBytes (CliType.Numeric (CliNumericType.Int64 0L))

            for i = 0 to aBytes.Length - 1 do
                buf.[i] <- aBytes.[i]

            for i = 0 to bBytes.Length - 1 do
                buf.[i] <- bBytes.[i]

            buf

        tsOrdered |> shouldNotEqual declarationOrdered
