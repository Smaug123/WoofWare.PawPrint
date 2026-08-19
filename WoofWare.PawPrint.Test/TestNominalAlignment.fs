namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open System.Runtime.CompilerServices
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `Int128` and `UInt128` demand 16-byte alignment of whatever contains them. CoreCLR does not
/// derive that from their fields — they are two `ulong`s, which would give 8 — but stamps it onto
/// the type by name in `MethodTableBuilder::CheckForSystemTypes` (methodtablebuilder.cpp:10576),
/// which runs only for corelib (`GetModule()->IsSystem()`, :11181).
///
/// Issue #992. The `Vector` family is stamped by the same mechanism and is deliberately *not*
/// modelled here: `Vector256`/`Vector512` are 32/64 on x64 but 16 on arm64, so they need a
/// target-architecture decision that nothing yet forces. `Int128`/`UInt128` are 16 on every
/// 64-bit target, which is what lets the guest `StructLayoutInt128Alignment.cs` be an ordinary
/// differential test rather than a host-conditional one.
[<TestFixture>]
module TestNominalAlignment =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt ti

    /// A stand-in declared type for the synthesised containers below. It must not itself be one of
    /// the nominally-aligned types, or the container's own answer would come from the stamp rather
    /// than from its fields; `TypedReference` is not stamped.
    let private declaredHandle : ConcreteTypeHandle = handleFor bct.TypedReference

    let private typeDef (ns : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TryGetTopLevelTypeDef ns name
        |> Option.defaultWith (fun () -> failwith $"expected %s{ns}.%s{name} in corelib")

    // ------------------------------------------------------------------
    // The rule itself, on `DeclaredTypeFacts`.
    // ------------------------------------------------------------------

    [<Test>]
    let ``Int128 and UInt128 carry a nominal alignment of 16`` () : unit =
        for name in [ "Int128" ; "UInt128" ] do
            let facts = DeclaredTypeFacts.ofTypeInfo bct loaded (typeDef "System" name)
            facts.NominalAlignment |> shouldEqual (Some 16)

    [<Test>]
    let ``an ordinary corelib value type carries no nominal alignment`` () : unit =
        // `Decimal` is the sharpest control available: it is 16 bytes like `Int128`, and is
        // likewise a corelib struct of fixed-width integers, but CoreCLR stamps nothing on it, so
        // it demands only 8. Measured on the host below by the corpus sweep.
        for name in [ "Decimal" ; "Int64" ; "Guid" ] do
            let facts = DeclaredTypeFacts.ofTypeInfo bct loaded (typeDef "System" name)
            facts.NominalAlignment |> shouldEqual None

    [<Test>]
    let ``the stamp is gated on the defining assembly being corelib`` () : unit =
        // `CheckForSystemTypes` runs only when `GetModule()->IsSystem()`
        // (methodtablebuilder.cpp:11181), so a guest assembly may define its own `System.Int128`
        // and it is an ordinary struct. Without the gate PawPrint would silently over-align it.
        //
        // This goes at `nominalAlignment` rather than at `ofTypeInfo`, because the same doctored
        // `TypeInfo` cannot survive `ofTypeInfo`'s base-chain walk: it would fail with "assembly
        // not loaded" and pass this assertion for a reason that has nothing to do with the gate.
        let int128 = typeDef "System" "Int128"

        let foreign : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            { int128 with
                Assembly = AssemblyName "SomeGuestAssembly"
            }

        DeclaredTypeFacts.nominalAlignment bct foreign |> shouldEqual None
        // The positive control: same name, same namespace, same everything but the assembly. Without
        // it, a `nominalAlignment` that returned `None` unconditionally would pass the line above.
        DeclaredTypeFacts.nominalAlignment bct int128 |> shouldEqual (Some 16)

    // ------------------------------------------------------------------
    // That the stamp actually reaches `SizeOf`, through the production pipeline.
    // ------------------------------------------------------------------

    /// Concretising and zero-initialising both *mint* handles, so the updated registry has to come
    /// back out with the value: a handle minted into a registry that is then dropped dangles, and
    /// the next lookup fails with "not found in AllConcreteTypes".
    let private buildValueType
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : CliType * ConcreteTypeHandle * AllConcreteTypes
        =
        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = allCt
                LoadedAssemblies = loaded
                BaseTypes = bct
            }

        let handle, ctx =
            TypeConcretization.concretizeType
                ctx
                IAssemblyLoad.alreadyLoadedOnly
                corelib.DefinitionFullName
                ImmutableArray.Empty
                ImmutableArray.Empty
                (TypeDefn.FromDefinition (ti.Identity, SignatureTypeKind.ValueType))

        let value, concreteTypes, _ =
            CliType.zeroOf IAssemblyLoad.alreadyLoadedOnly ctx.ConcreteTypes ctx.LoadedAssemblies bct handle

        value, handle, concreteTypes

    [<Test>]
    let ``a zero-initialised Int128 is 16 bytes and demands 16-byte alignment`` () : unit =
        for name in [ "Int128" ; "UInt128" ] do
            match buildValueType (typeDef "System" name) with
            | CliType.ValueType vt, _, _ ->
                let size = CliValueType.SizeOf vt
                size.Size |> shouldEqual 16
                size.Alignment |> shouldEqual 16
            | other, _, _ -> failwith $"expected %s{name} to be a value type, got %O{other}"

    /// A container of `{ long; Int128 }`, built through `OfFields` exactly as the interpreter
    /// builds one. The container carries no stamp of its own, so 16 can only have arrived by
    /// propagation through the field's reported alignment — which is the mechanism CoreCLR uses
    /// (`GetFieldPlacementInfo`, classlayoutinfo.cpp:112).
    [<Test>]
    let ``a struct embedding an Int128 is 16-aligned and 32 bytes`` () : unit =
        let field (name : string) (contents : CliType) (ty : ConcreteTypeHandle) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = contents
                Offset = None
                Type = ty
                MarshallingDescriptor = None
            }

        let int128, int128Handle, concreteTypes = buildValueType (typeDef "System" "Int128")

        let facts : DeclaredTypeFacts =
            {
                IsValueType = true
                IsEnum = false
                NominalAlignment = None
                LayoutKind = TypeLayoutKind.Sequential
                Layout = Layout.Default
                CharSet = CharSet.Ansi
            }

        let container =
            CliValueType.OfFields
                bct
                concreteTypes
                declaredHandle
                facts
                [
                    field "L" (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))) (handleFor bct.Int64)
                    field "I" int128 int128Handle
                ]

        let size = CliValueType.SizeOf container
        // The `Int128` lands at 16 rather than at 8, so the struct is 32 rather than 24.
        size.Size |> shouldEqual 32
        size.Alignment |> shouldEqual 16
        CliValueType.DereferenceFieldAt 16 16 container |> ignore<CliType>

    /// The invariant that makes "does the stamp round the type's own size?" a question with no
    /// consequences: for every type CoreCLR stamps, the fields already derive a whole number of
    /// stamps. `SizeOf` requires that rather than assuming it, so a future row that broke it says
    /// so instead of silently picking one of the two readings.
    [<Test>]
    let ``a stamped type whose fields do not fill its stamp is refused`` () : unit =
        let facts : DeclaredTypeFacts =
            {
                IsValueType = true
                IsEnum = false
                // 16, over a single `int` field: 4 bytes derived, so not a whole number of stamps.
                NominalAlignment = Some 16
                LayoutKind = TypeLayoutKind.Sequential
                Layout = Layout.Default
                CharSet = CharSet.Ansi
            }

        let field : CliField =
            {
                Id = FieldId.named "x"
                Name = "x"
                Contents = CliType.Numeric (CliNumericType.Int32 0)
                Offset = None
                Type = handleFor bct.Int32
                MarshallingDescriptor = None
            }

        let exc =
            Assert.Throws<Exception> (fun () ->
                CliValueType.OfFields bct allCt declaredHandle facts [ field ]
                |> CliValueType.SizeOf
                |> ignore<SizeofResult>
            )

        exc.Message |> shouldContainText "not a multiple of it"

    // ------------------------------------------------------------------
    // The outside oracle: real CoreCLR, over the whole corelib corpus.
    // ------------------------------------------------------------------

    /// Sizing probe. `Unsafe.SizeOf<Probe<T>>()` reveals how much alignment a `T` field demands of
    /// its container, which is the quantity this change is about and which no BCL API exposes
    /// directly.
    [<Struct ; StructLayout(LayoutKind.Sequential)>]
    type private Probe<'T when 'T : struct and 'T :> ValueType and 'T : (new : unit -> 'T)> =
        val mutable Pre : byte
        val mutable Value : 'T

    let private sizeOfMethod : MethodInfo =
        typeof<Unsafe>.GetMethod (nameof Unsafe.SizeOf, BindingFlags.Public ||| BindingFlags.Static)

    let private hostSizeOf (t : Type) : int =
        let noReceiver : obj = null

        let boxed : obj =
            sizeOfMethod.MakeGenericMethod([| t |]).Invoke (noReceiver, Array.empty<obj>)

        unbox<int> boxed

    let private probeType : Type = typedefof<Probe<int>>

    let private hostProbeSizeOf (t : Type) : int =
        hostSizeOf (probeType.MakeGenericType [| t |])

    /// PawPrint's answer to the same question the probe asks the host: how big is `{ byte; T }`?
    let private pawPrintProbeSize
        (concreteTypes : AllConcreteTypes)
        (contents : CliType)
        (ty : ConcreteTypeHandle)
        : int
        =
        let facts : DeclaredTypeFacts =
            {
                IsValueType = true
                IsEnum = false
                NominalAlignment = None
                LayoutKind = TypeLayoutKind.Sequential
                Layout = Layout.Default
                CharSet = CharSet.Ansi
            }

        let field (name : string) (contents : CliType) (ty : ConcreteTypeHandle) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = contents
                Offset = None
                Type = ty
                MarshallingDescriptor = None
            }

        let probe : CliValueType =
            CliValueType.OfFields
                bct
                concreteTypes
                declaredHandle
                facts
                [
                    field "Pre" (CliType.Numeric (CliNumericType.UInt8 (UInt8Source.Verbatim 0uy))) (handleFor bct.Byte)
                    field "Value" contents ty
                ]

        (CliValueType.SizeOf probe).Size

    /// Types the sweep must skip, each with its reason. Deliberately a hard-coded list rather than a
    /// computed predicate ("skip anything containing a vector"): a predicate would silently absorb
    /// the next divergence, where a name that no longer diverges — or a new one that does — should
    /// make this test go red and be looked at.
    ///
    /// The one entry is the deferred half of issue #992, and is the evidence that the `Vector`
    /// rows are target-dependent rather
    /// than merely untested: `AhoCorasick` holds an `AsciiState`, which holds a `Vector256<byte>`.
    /// On this arm64 host that vector demands 16, making `AhoCorasick` 80 bytes; on x64 it demands
    /// 32, making the very same type 96. PawPrint derives 8 and gets 72 on both. Adding a
    /// `Vector256` row therefore cannot be done without first deciding which target PawPrint's
    /// layout claims to be — the row would be wrong on one of the two machines this suite runs on.
    let private excludedFromSweep : Map<string, string> =
        [
            "System.Buffers.AhoCorasick",
            "transitively contains a Vector256<byte>, whose nominal alignment is target-dependent and deferred (#992)"
        ]
        |> Map.ofList

    /// The outside oracle. Reading the expected layout out of PawPrint's own metadata graph would
    /// only show it is self-consistent; this asks real CoreCLR — which laid these very types out in
    /// this very process — for both the size of each corelib value type and the size of a struct
    /// containing one. The second is the interesting half: it is sensitive to the alignment a field
    /// demands, which is exactly what a nominal stamp changes and what `Unsafe.SizeOf<T>` alone
    /// cannot see.
    [<Test>]
    let ``value-type layout agrees with the host CLR across corelib`` () : unit =
        let hostCorelib = typeof<obj>.Assembly

        let hostTypes =
            hostCorelib.GetTypes ()
            |> Array.filter (fun t -> t.IsValueType && not t.IsGenericTypeDefinition && not t.IsNested)
            |> Array.map (fun t -> (t.Namespace, t.Name), t)
            |> Array.distinctBy fst
            |> Map.ofArray

        let mutable compared = 0
        let mutable wideSeen = 0
        let divergences = ResizeArray<string> ()

        for KeyValue (_, ti) in corelib.TypeDefs do
            let key =
                if String.IsNullOrEmpty ti.Namespace then
                    ti.Name
                else
                    $"%s{ti.Namespace}.%s{ti.Name}"

            let hostType =
                if not ti.Generics.IsEmpty || excludedFromSweep.ContainsKey key then
                    None
                else
                    Map.tryFind
                        ((if String.IsNullOrEmpty ti.Namespace then
                              null
                          else
                              ti.Namespace),
                         ti.Name)
                        hostTypes

            match hostType with
            | None -> ()
            | Some hostType ->

            // `System.Void` is a value type to metadata but may not be used as a type argument, so
            // the probe cannot be instantiated over it. By-ref-like types cannot be a field of an
            // ordinary struct at all, which is likewise what the probe asks.
            if
                hostType.IsByRefLike
                || hostType.IsGenericType
                || hostType = typeof<Void>
                || not (DumpedAssembly.isValueType bct loaded ti)
            then
                ()
            else

            let ours =
                try
                    match buildValueType ti with
                    | CliType.ValueType vt, handle, concreteTypes ->
                        Some (
                            (CliValueType.SizeOf vt).Size,
                            pawPrintProbeSize concreteTypes (CliType.ValueType vt) handle
                        )
                    | _ -> None
                with _ ->
                    // A type PawPrint cannot build at all is out of scope for this comparison; the
                    // suite has other coverage for that. It is not silently skipped though — an
                    // exhaustive corpus with zero successes would trip the vacuity guard below.
                    None

            match ours with
            | None -> ()
            | Some (ourSize, ourProbe) ->
                let theirSize = hostSizeOf hostType
                let theirProbe = hostProbeSizeOf hostType
                compared <- compared + 1

                if theirProbe - theirSize > 8 then
                    // A type demanding more than pointer alignment: the shape this change is about.
                    wideSeen <- wideSeen + 1

                if ourSize <> theirSize || ourProbe <> theirProbe then
                    divergences.Add
                        $"%s{key}: PawPrint size=%d{ourSize} probe=%d{ourProbe}, host size=%d{theirSize} probe=%d{theirProbe}"

        if divergences.Count > 0 then
            failwithf
                "%d corelib value types are laid out differently:\n%s"
                divergences.Count
                (String.Join ("\n", divergences))

        // Guard against the sweep degenerating: a corpus that found no types, or no *widely
        // aligned* types, would pass however wrong the model was. `Int128`/`UInt128` are the only
        // non-generic corelib types that demand 16, so the second guard is exactly them.
        if compared < 100 then
            failwithf "expected to compare at least 100 corelib value types, only reached %d" compared

        if wideSeen < 2 then
            failwithf
                "expected the sweep to include Int128 and UInt128, which demand more than pointer alignment; saw %d such types"
                wideSeen
