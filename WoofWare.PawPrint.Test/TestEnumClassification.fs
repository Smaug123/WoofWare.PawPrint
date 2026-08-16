namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.Reflection
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Which value types PawPrint calls enums, and which of those it flattens onto the eval stack.
///
/// Issue #996: deciding enum-ness structurally, from the CLR-reserved field name
/// `value__`, is defeated by `struct Fake { public int value__; }`. It is nominal — the
/// immediate base type is `System.Enum` — and arrives at `CliValueType.OfFields` as
/// `DeclaredTypeFacts.IsEnum`. The width restriction is structural: an enum over
/// `bool`/`char`/native int is legal (ECMA-335 II.14.3) but is *not* flattened, because
/// `IlMachineRuntimeMetadata.unboxMaterialisesFlattened` reports it unflattened and `unboxPermitted`
/// would fail loudly on a legal unbox if the two disagreed.
[<TestFixture>]
module TestEnumClassification =

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

    /// A stand-in declared type for the synthesised values below. Its own identity is irrelevant:
    /// `ClassifyPrimitiveLike` consults it only to check the closed set of BCL wrapper structs, and
    /// `TypedReference` is not in that set (asserted by `TestPrimitiveLikeStructRegistry`).
    let private declaredHandle : ConcreteTypeHandle = handleFor bct.TypedReference

    /// Build the value a declared type with this single `value__` field would have, told whether it
    /// is nominally an enum.
    let private classify (isEnum : bool) (contents : CliType) : PrimitiveLikeKind option =
        let field : CliField =
            {
                Id = FieldId.named "value__"
                Name = "value__"
                Contents = contents
                Offset = None
                Type = handleFor bct.Int32
                MarshallingDescriptor = None
            }

        let facts : DeclaredTypeFacts =
            {
                IsValueType = true
                IsEnum = isEnum
                NominalAlignment = None
                LayoutKind = TypeLayoutKind.Sequential
                Layout = Layout.Default
                CharSet = CharSet.Ansi
            }

        (CliValueType.OfFields bct allCt declaredHandle facts [ field ]).PrimitiveLikeKind

    // ------------------------------------------------------------------
    // The nominal half: what #996 is about.
    // ------------------------------------------------------------------

    [<Test>]
    let ``a value__-shaped struct that is not an enum is not flattened`` () : unit =
        classify false (CliType.Numeric (CliNumericType.Int32 0)) |> shouldEqual None

    [<Test>]
    let ``a value__-shaped struct that is an enum is flattened`` () : unit =
        classify true (CliType.Numeric (CliNumericType.Int32 0))
        |> shouldEqual (Some PrimitiveLikeKind.EnumLike)

    /// The outside oracle. Reading the expectation out of PawPrint's own metadata graph would only
    /// prove it is self-consistent, so this asks the host CLR the same question about the same
    /// types: `Type.IsEnum` is CoreCLR's own answer, computed by its type loader rather than by
    /// anything in this repo.
    [<Test>]
    let ``nominal enum classification agrees with the host CLR across corelib`` () : unit =
        let hostCorelib = typeof<obj>.Assembly

        let hostTypes =
            hostCorelib.GetTypes ()
            |> Array.filter (fun t -> not t.IsGenericTypeDefinition && not t.IsNested)
            |> Array.map (fun t -> (t.Namespace, t.Name), t)
            |> Array.distinctBy fst
            |> Map.ofArray

        let mutable compared = 0
        let mutable enumsSeen = 0

        for KeyValue (_, ti) in corelib.TypeDefs do
            match
                Map.tryFind
                    ((if String.IsNullOrEmpty ti.Namespace then
                          null
                      else
                          ti.Namespace),
                     ti.Name)
                    hostTypes
            with
            | None -> ()
            | Some hostType ->
                if ti.Generics.IsEmpty then
                    let ours = DumpedAssembly.isEnum bct loaded ti
                    compared <- compared + 1

                    if hostType.IsEnum then
                        enumsSeen <- enumsSeen + 1

                    if ours <> hostType.IsEnum then
                        failwithf
                            "disagreed about %s.%s: PawPrint says isEnum=%b, the host CLR says %b"
                            ti.Namespace
                            ti.Name
                            ours
                            hostType.IsEnum

        // Guard against the comparison silently degenerating: a name-matching sweep that found no
        // enums would pass vacuously however wrong the classifier was.
        if compared < 500 then
            failwithf "expected to compare hundreds of corelib types, only reached %d" compared

        if enumsSeen < 20 then
            failwithf "expected corelib to contribute many enums to the comparison, saw %d" enumsSeen

    // ------------------------------------------------------------------
    // The structural half: which underlying types are flattened.
    // ------------------------------------------------------------------

    [<Test>]
    let ``an enum over each fixed-width integer is flattened`` () : unit =
        let cases : CliType list =
            [
                CliType.Numeric (CliNumericType.Int8 0y)
                CliType.Numeric (CliNumericType.UInt8 0uy)
                CliType.Numeric (CliNumericType.Int16 0s)
                CliType.Numeric (CliNumericType.UInt16 0us)
                CliType.Numeric (CliNumericType.Int32 0)
                CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))
            ]

        for contents in cases do
            classify true contents |> shouldEqual (Some PrimitiveLikeKind.EnumLike)

    [<Test>]
    let ``an enum over bool, char or a native int is not flattened`` () : unit =
        // ECMA-335 II.14.3 permits these and the CLR loads them; C# cannot declare one but
        // Reflection.Emit can. `unboxMaterialisesFlattened` reports them unflattened, so widening
        // the rule here would make `unboxPermitted` refuse an unbox that works today.
        let cases : CliType list =
            [
                CliType.Bool 0uy
                CliType.Char (0uy, 0uy)
                CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L))
            ]

        for contents in cases do
            classify true contents |> shouldEqual None

    /// `EnumUnderlyingIsFlattenable` lists `CliNumericType` cases; `unboxMaterialisesFlattened`
    /// lists `BaseClassTypes` rows. They are two spellings of one rule, and nothing else makes them
    /// agree, so a change to either that forgot the other would go unnoticed. This walks the
    /// nominal list and requires the structural one to accept exactly it.
    [<Test>]
    let ``the flattenable widths agree with the unbox rule's nominal list`` () : unit =
        let flattenable : (TypeInfo<GenericParamFromMetadata, TypeDefn> * PrimitiveType) list =
            [
                bct.SByte, PrimitiveType.SByte
                bct.Byte, PrimitiveType.Byte
                bct.Int16, PrimitiveType.Int16
                bct.UInt16, PrimitiveType.UInt16
                bct.Int32, PrimitiveType.Int32
                bct.UInt32, PrimitiveType.UInt32
                bct.Int64, PrimitiveType.Int64
                bct.UInt64, PrimitiveType.UInt64
            ]

        let notFlattenable : (TypeInfo<GenericParamFromMetadata, TypeDefn> * PrimitiveType) list =
            [
                bct.Boolean, PrimitiveType.Boolean
                bct.Char, PrimitiveType.Char
                bct.IntPtr, PrimitiveType.IntPtr
                bct.UIntPtr, PrimitiveType.UIntPtr
            ]

        // The pairing above is the test's own claim, so check it rather than trust it — and check
        // it through the production concretiser, which is what actually decides which corelib type
        // a `PrimitiveType` denotes.
        let ctx : TypeConcretization.ConcretizationContext<DumpedAssembly> =
            {
                ConcreteTypes = allCt
                LoadedAssemblies = loaded
                BaseTypes = bct
            }

        for ti, prim in flattenable @ notFlattenable do
            let handle, _ =
                TypeConcretization.concretizeType
                    ctx
                    IAssemblyLoad.alreadyLoadedOnly
                    corelib.Name
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    (TypeDefn.PrimitiveType prim)

            handle |> shouldEqual (handleFor ti)

        for _, prim in flattenable do
            classify true (CliType.zeroOfPrimitive allCt bct prim)
            |> shouldEqual (Some PrimitiveLikeKind.EnumLike)

        for _, prim in notFlattenable do
            classify true (CliType.zeroOfPrimitive allCt bct prim) |> shouldEqual None

    // ------------------------------------------------------------------
    // `DeclaredTypeFacts`, which carries the nominal answer.
    // ------------------------------------------------------------------

    /// The handle registries build these without a load context, via `ofCorelibType`. That
    /// shortcut rests on corelib referencing no other assembly, so this checks it against the
    /// answer a full load context gives.
    [<Test>]
    let ``ofCorelibType agrees with ofTypeInfo for every type the registries build`` () : unit =
        let built : TypeInfo<GenericParamFromMetadata, TypeDefn> list =
            [
                bct.IntPtr
                bct.UIntPtr
                bct.RuntimeType
                bct.RuntimeTypeHandle
                bct.RuntimeFieldHandle
                bct.RuntimeFieldHandleInternal
                bct.RuntimeFieldInfoStub
                bct.RuntimeMethodHandle
                bct.RuntimeMethodHandleInternal
                bct.RuntimeMethodInfoStub
                bct.String
            ]

        for ti in built do
            DeclaredTypeFacts.ofCorelibType bct ti
            |> shouldEqual (DeclaredTypeFacts.ofTypeInfo bct loaded ti)

            // Not a tautology worth skipping: these all reach `ClassifyPrimitiveLike`, and an
            // `IsEnum` of true would trip the enum field-shape assertion in `OfFields`.
            (DeclaredTypeFacts.ofCorelibType bct ti).IsEnum |> shouldEqual false

    [<Test>]
    let ``ofCorelibType refuses a type it cannot answer for`` () : unit =
        // The test assembly is not corelib, so its base chain may leave corelib and the
        // single-assembly shortcut would be unsound.
        let foreign : TypeInfo<GenericParamFromMetadata, TypeDefn> =
            { bct.Int32 with
                Assembly = AssemblyName "SomeOtherAssembly"
            }

        let exc =
            Assert.Throws<Exception> (fun () ->
                DeclaredTypeFacts.ofCorelibType bct foreign |> ignore<DeclaredTypeFacts>
            )

        exc.Message |> shouldContainText "not in corelib"

    [<Test>]
    let ``OfFields rejects facts claiming an enum whose fields are not an enum's`` () : unit =
        let facts : DeclaredTypeFacts =
            {
                IsValueType = true
                IsEnum = true
                NominalAlignment = None
                LayoutKind = TypeLayoutKind.Sequential
                Layout = Layout.Default
                CharSet = CharSet.Ansi
            }

        let field (name : string) : CliField =
            {
                Id = FieldId.named name
                Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 0)
                Offset = None
                Type = handleFor bct.Int32
                MarshallingDescriptor = None
            }

        let exc =
            Assert.Throws<Exception> (fun () ->
                CliValueType.OfFields bct allCt declaredHandle facts [ field "A" ; field "B" ]
                |> ignore<CliValueType>
            )

        exc.Message |> shouldContainText "ECMA-335 II.14.3"
