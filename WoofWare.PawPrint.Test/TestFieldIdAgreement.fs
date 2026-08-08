namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Two independent pieces of the interpreter compute the `FieldId` of an instance field, and a
/// field access only works if they agree:
///
///  * `IlMachineRuntimeMetadata.collectAllInstanceFields` lays out an object's storage. It walks
///    the base chain and keys each field to the `ConcreteTypeHandle` of the type that *declares*
///    it -- so an inherited field on a `Derived&lt;a,b,c&gt;` is keyed to `Base&lt;a,b&gt;`, not to
///    `Derived&lt;a,b,c&gt;`.
///  * `ExecutionConcretization.concretizeFieldDeclaringType` keys an `ldfld`/`stfld`/`ldflda`
///    access to the declaring type named by the access site's token, concretized in that frame.
///
/// If those ever disagree the access fails with "field not found", and nothing about the failure
/// says which of the two was wrong. This fixture is the oracle for their agreement: it needs no
/// guest program and no reachable IL path, so it can cover shapes (deep chains, reordered and
/// partially-applied base arguments, name-shadowed fields) long before any guest reaches them.
///
/// Note what this deliberately does *not* assume: that the declaring handle equals the object's
/// own type. It must not -- that is the whole point of an inherited field -- so the invariant is
/// stated as "the declaring handle names the type that really declares this field definition, and
/// round-trips through the access-site concretization unchanged".
[<TestFixture>]
module TestFieldIdAgreement =

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private corelibPath : string = typeof<obj>.Assembly.Location
    let private runtimeDir : string = Path.GetDirectoryName corelibPath

    /// The FSharp.Core this test project already runs against. Chosen over a nuget-cache lookup
    /// so the fixture has no environmental preconditions; `PrintfFormat`5 : PrintfFormat`4` in
    /// here is the shape that motivated the fixture.
    let private fsharpCorePath : string = typeof<int list>.Assembly.Location

    let private corelib : DumpedAssembly = Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    /// A hierarchy chosen to stress every way a derived type's generic arguments can be
    /// rearranged on the way to its base's field declarations. `DerivedAddsParam` is the
    /// `PrintfFormat`5 : PrintfFormat`4` shape from the bug report.
    let private corpusSource : string =
        """
namespace PawPrint.FieldIdAgreement;

public class Base<A, B>
{
    public A First;
    public B Second;
    public int Count;
}

public class NonGenericBase
{
    public object Ref;
}

public class DerivedAddsParam<A, B, C> : Base<A, B>
{
    public C Extra;
}

public class DerivedReorders<A, B> : Base<B, A>
{
    public int Own;
}

public class DerivedPartlyConcrete<A> : Base<A, string>
{
    public A Mine;
}

public class DerivedNestsBaseInItsOwnArgs<A> : Base<Base<A, int>, A>
{
    public A Nested;
}

public class Middle<A> : Base<A, int>
{
    public A M;
}

public class Leaf<A> : Middle<Base<A, string>>
{
    public int L;
}

public class ClosedDerived : Base<int, string>
{
    public byte B;
}

public class GenericFromNonGeneric<A> : NonGenericBase
{
    public A Val;
}

/// Declares a field with the same *name* as its base's, so the two can only be told apart by
/// declaring type. A name-keyed fallback would call this ambiguous.
public class ShadowsField<A> : Base<A, int>
{
    public new int Count;
}

public class Outer<A>
{
    public A OuterVal;

    public class Inner<B> : Base<A, B>
    {
        public B InnerVal;
    }
}
"""

    let private corpusAssembly : DumpedAssembly =
        let bytes =
            Roslyn.compileAssembly
                "PawPrint.FieldIdAgreement"
                Microsoft.CodeAnalysis.OutputKind.DynamicallyLinkedLibrary
                []
                [ corpusSource ]

        use stream = new MemoryStream (bytes)
        AssemblyApi.read loggerFactory (Some "PawPrint.FieldIdAgreement.dll") stream

    let private fsharpCore : DumpedAssembly =
        Assembly.readFile loggerFactory fsharpCorePath

    let private baseState : IlMachineState =
        let dirs =
            ImmutableArray.CreateRange [ runtimeDir ; Path.GetDirectoryName fsharpCorePath ]

        let state = IlMachineState.initial loggerFactory dirs corelib

        let state =
            { state with
                ConcreteTypes = Corelib.concretizeAll state._LoadedAssemblies bct AllConcreteTypes.Empty
            }

        state.WithLoadedAssembly(corpusAssembly).WithLoadedAssembly fsharpCore

    // ------------------------------------------------------------------------------------
    // Building closed instantiations
    // ------------------------------------------------------------------------------------

    /// Fold a `LoadedAssemblies` that has grown into the state, one assembly at a time, so the
    /// state's own bookkeeping runs rather than being bypassed by a record update.
    let private absorbLoads (state : IlMachineState) (loaded : LoadedAssemblies) : IlMachineState =
        (state, loaded.DefinitionNames)
        ||> Seq.fold (fun state name ->
            if state._LoadedAssemblies.ContainsDefinition (AssemblyName name) then
                state
            else
                state.WithLoadedAssembly (loaded.ByDefinitionName name)
        )

    /// Load every assembly the base chain of `typeInfo` reaches. Production does this inside
    /// `concretizeMethod`; a fixture that walks into the type graph directly has to do it for
    /// itself, and without it any type whose base is named through a facade (which is every
    /// inheriting type in a `netstandard`-targeting FSharp.Core) cannot even be classified as
    /// value type or reference type.
    let private withBaseAssemblies
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        : IlMachineState
        =
        let assy = state._LoadedAssemblies.ByDefinitionName typeInfo.Assembly.FullName

        Concretization.ensureTypeDefinitionBaseAssembliesLoaded
            (IlMachineState.loader loggerFactory state)
            state._LoadedAssemblies
            assy
            typeInfo.TypeDefHandle
        |> absorbLoads state

    /// A closed `TypeDefn` for `typeInfo` instantiated at `args`, which must have exactly the
    /// type's generic arity. Assumes `withBaseAssemblies` has already run for `typeInfo`.
    let private closedTypeDefn
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (args : TypeDefn list)
        : TypeDefn
        =
        let kind = DumpedAssembly.signatureTypeKind bct state._LoadedAssemblies typeInfo

        let definition = TypeDefn.FromDefinition (typeInfo.Identity, kind)

        match args with
        | [] -> definition
        | _ -> TypeDefn.GenericInstantiation (definition, ImmutableArray.CreateRange args)

    /// Concretize `typeInfo` at `args` and make the result ready to lay out: every assembly the
    /// base chain and the field types reach is loaded first, so a failure from here on is about
    /// the invariant rather than about a missing facade.
    let private layoutReady
        (state : IlMachineState)
        (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (args : TypeDefn list)
        : IlMachineState * ConcreteTypeHandle
        =
        let state = withBaseAssemblies state typeInfo
        let ty = closedTypeDefn state typeInfo args

        let state, handle =
            IlMachineTypeResolution.concretizeType
                loggerFactory
                bct
                state
                typeInfo.Assembly
                ImmutableArray.Empty
                ImmutableArray.Empty
                ty

        let loaded, concreteTypes =
            Concretization.ensureBaseAssembliesLoadedForConcreteHandle
                (IlMachineState.loader loggerFactory state)
                bct
                (System.Collections.Generic.HashSet ())
                state._LoadedAssemblies
                state.ConcreteTypes
                handle

        let state =
            absorbLoads
                { state with
                    ConcreteTypes = concreteTypes
                }
                loaded

        state, handle

    // ------------------------------------------------------------------------------------
    // The invariant
    // ------------------------------------------------------------------------------------

    /// Every violation this fixture can detect, as data rather than an assertion, so a single
    /// sweep reports all of them rather than stopping at the first.
    type private Violation =
        {
            Field : FieldId
            /// The type whose storage was being laid out.
            LaidOutFor : ConcreteTypeHandle
            Complaint : string
        }

        override this.ToString () : string =
            $"laying out %O{this.LaidOutFor}: field '%O{this.Field}': %s{this.Complaint}"

    /// Check every instance field of `handle`'s storage against the two-producer agreement.
    /// Returns the (possibly assembly-loading) state alongside the violations found.
    let private violationsFor (state : IlMachineState) (handle : ConcreteTypeHandle) : IlMachineState * Violation list =
        let state, fields =
            IlMachineState.collectAllInstanceFields loggerFactory bct state handle

        ((state, []), fields)
        ||> List.fold (fun (state, acc) (field : CliField) ->
            let complain (state : IlMachineState) (complaint : string) =
                state,
                {
                    Field = field.Id
                    LaidOutFor = handle
                    Complaint = complaint
                }
                :: acc

            match FieldId.tryDeclaringType field.Id, FieldId.tryFieldDefinition field.Id with
            | None, _
            | _, None ->
                // `FieldId.Named` carries no metadata identity, so neither producer could have
                // built it. Storage laid out from metadata must never contain one.
                complain state "storage laid out from metadata contains a name-keyed field identity"
            | Some declaringHandle, Some fieldDefinition ->

            match AllConcreteTypes.lookup declaringHandle state.ConcreteTypes with
            | None -> complain state $"declaring-type handle %O{declaringHandle} is not registered in AllConcreteTypes"
            | Some declaringType ->

            let declaringAssembly =
                state._LoadedAssemblies.ByDefinitionName declaringType.Identity.AssemblyFullName

            let declaringTypeDef =
                declaringAssembly.TypeDefs.[declaringType.Identity.TypeDefinition.Get]

            // P1: the declaring handle names the type that really declares this field definition.
            // A base field wrongly keyed to the *derived* handle fails here, because the derived
            // TypeDef does not declare that FieldDefinitionHandle.
            let declared =
                declaringTypeDef.Fields
                |> List.tryFind (fun f -> ComparableFieldDefinitionHandle.Make f.Handle = fieldDefinition)

            match declared with
            | None ->
                complain
                    state
                    $"declaring type %s{declaringTypeDef.Namespace}.%s{declaringTypeDef.Name} does not declare this field definition"
            | Some declaredField ->

            if declaredField.IsStatic then
                complain state "instance-field storage is keyed to a static field definition"
            else if
                declaredField.Name <> field.Id.Name
                && not (field.Id.Name.StartsWith (declaredField.Name + "["))
            then
                // Inline-array repeats legitimately carry a suffixed storage name; everything
                // else must match its metadata name exactly.
                complain
                    state
                    $"identity names the field '%s{field.Id.Name}', metadata calls it '%s{declaredField.Name}'"
            else

            // P2: rebuild the field exactly as `IlMachineMemberResolution.resolveMemberWithGenerics`
            // hands it to the access-site concretization -- declaring type identity from metadata,
            // generic arguments mapped back out of the concrete handle -- and check that the
            // access site would compute the very handle storage used.
            let genericArgs =
                declaringType.Generics
                |> Seq.map (fun h ->
                    Concretization.concreteHandleToTypeDefn bct h state.ConcreteTypes state._LoadedAssemblies
                )
                |> ImmutableArray.CreateRange

            let asResolved : FieldInfo<TypeDefn, TypeDefn> =
                declaredField |> FieldInfo.mapTypeGenerics (fun i _ -> genericArgs.[i])

            let state, accessSiteHandle, _ =
                ExecutionConcretization.concretizeFieldDeclaringType
                    loggerFactory
                    bct
                    ImmutableArray.Empty
                    ImmutableArray.Empty
                    asResolved
                    state

            if accessSiteHandle <> declaringHandle then
                complain
                    state
                    $"an access site would key this field to %O{accessSiteHandle}, but storage keyed it to %O{declaringHandle}"
            else
                state, acc
        )

    /// P3: an inherited field's identity does not depend on which derived type you reached it
    /// through. Laying out the declaring type on its own must produce the identical `FieldId`.
    let private inheritedIdentitiesAreStable
        (state : IlMachineState)
        (handle : ConcreteTypeHandle)
        : IlMachineState * Violation list
        =
        let state, fields =
            IlMachineState.collectAllInstanceFields loggerFactory bct state handle

        ((state, []), fields)
        ||> List.fold (fun (state, acc) (field : CliField) ->
            match FieldId.tryDeclaringType field.Id with
            | None -> state, acc
            | Some declaringHandle ->

            if declaringHandle = handle then
                // Declared on the type itself; nothing to cross-check.
                state, acc
            else

            let state, declaringFields =
                IlMachineState.collectAllInstanceFields loggerFactory bct state declaringHandle

            let present =
                declaringFields |> List.exists (fun f -> FieldId.exactlyEqual f.Id field.Id)

            if present then
                state, acc
            else
                state,
                {
                    Field = field.Id
                    LaidOutFor = handle
                    Complaint =
                        $"inherited from %O{declaringHandle}, but laying that type out on its own produces no such identity"
                }
                :: acc
        )

    // ------------------------------------------------------------------------------------
    // Corpus generation
    // ------------------------------------------------------------------------------------

    let private corpusType (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corpusAssembly.TryGetTopLevelTypeDef "PawPrint.FieldIdAgreement" name
        |> Option.defaultWith (fun () ->
            failwith $"PawPrint.FieldIdAgreement.%s{name} missing from the corpus assembly"
        )

    /// Nested types are not top-level, and metadata gives them an empty namespace, so they can
    /// only be found by enclosing type.
    let private corpusNestedType (outer : string) (inner : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        let outerType = corpusType outer

        corpusAssembly.TypeDefs
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun (t : TypeInfo<GenericParamFromMetadata, TypeDefn>) ->
            t.Name = inner && t.DeclaringType = outerType.TypeDefHandle
        )
        |> Seq.tryHead
        |> Option.defaultWith (fun () -> failwith $"nested type %s{outer}/%s{inner} missing from the corpus assembly")

    /// Every corpus type, paired with its generic arity.
    let private corpusTypes : (TypeInfo<GenericParamFromMetadata, TypeDefn> * int) list =
        [
            corpusType "Base`2", 2
            corpusType "NonGenericBase", 0
            corpusType "DerivedAddsParam`3", 3
            corpusType "DerivedReorders`2", 2
            corpusType "DerivedPartlyConcrete`1", 1
            corpusType "DerivedNestsBaseInItsOwnArgs`1", 1
            corpusType "Middle`1", 1
            corpusType "Leaf`1", 1
            corpusType "ClosedDerived", 0
            corpusType "GenericFromNonGeneric`1", 1
            corpusType "ShadowsField`1", 1
            corpusType "Outer`1", 1
            // `Outer<A>.Inner<B>` has arity 2: a nested type's generic parameters include the
            // enclosing type's.
            corpusNestedType "Outer`1" "Inner`1", 2
        ]

    /// Closed type arguments to instantiate the corpus at. Deliberately mixes primitives,
    /// reference types, arrays and a generic instantiation of a corpus type, since a generic
    /// argument that is itself a generic instantiation is the case where a lossy
    /// handle-to-`TypeDefn` round trip would show up.
    let private genTypeArg : Gen<TypeDefn> =
        let leaves =
            [
                TypeDefn.PrimitiveType PrimitiveType.Int32
                TypeDefn.PrimitiveType PrimitiveType.Int64
                TypeDefn.PrimitiveType PrimitiveType.String
                TypeDefn.PrimitiveType PrimitiveType.Object
                TypeDefn.PrimitiveType PrimitiveType.Boolean
                TypeDefn.OneDimensionalArrayLowerBoundZero (TypeDefn.PrimitiveType PrimitiveType.Int32)
            ]
            |> Gen.elements

        let nested =
            gen {
                let! a = leaves
                let! b = leaves
                return closedTypeDefn baseState (corpusType "Base`2") [ a ; b ]
            }

        Gen.frequency [ 4, leaves ; 1, nested ]

    let private genInstantiation : Gen<TypeInfo<GenericParamFromMetadata, TypeDefn> * TypeDefn list> =
        gen {
            let! typeInfo, arity = Gen.elements corpusTypes
            let! args = Gen.listOfLength arity genTypeArg
            return typeInfo, args
        }

    // ------------------------------------------------------------------------------------
    // Tests
    // ------------------------------------------------------------------------------------

    [<Test>]
    let ``storage and access-site field identities agree, over generated instantiations`` () : unit =
        let property (typeInfo : TypeInfo<GenericParamFromMetadata, TypeDefn>, args : TypeDefn list) : bool =
            let state, handle = layoutReady baseState typeInfo args

            let state, violations = violationsFor state handle
            let _, stability = inheritedIdentitiesAreStable state handle

            match violations @ stability with
            | [] -> true
            | problems ->
                let rendered = problems |> List.map string |> String.concat "\n  "
                let renderedArgs = args |> List.map string |> String.concat "; "

                failwith $"%s{typeInfo.Namespace}.%s{typeInfo.Name} instantiated at [%s{renderedArgs}]:\n  %s{rendered}"

        Check.One (Config.QuickThrowOnFailure.WithMaxTest 400, Prop.forAll (Arb.fromGen genInstantiation) property)

    /// Every non-interface FSharp.Core type. Deliberately not narrowed to intra-assembly
    /// inheritance: a base named by `TypeRef` (`: System.Exception`, `: System.Attribute`,
    /// `: FSharpFunc&lt;_,_&gt;`) is the cross-assembly case, and `System.Exception` alone brings
    /// a dozen inherited fields whose identities have to be keyed to corelib's handle for it.
    let private sweepableFsharpCoreTypes : TypeInfo<GenericParamFromMetadata, TypeDefn> list =
        fsharpCore.TypeDefs
        |> Seq.map (fun kvp -> kvp.Value)
        |> Seq.filter (fun t -> not t.IsInterface && t.BaseType.IsSome)
        |> Seq.sortBy (fun t -> t.Namespace, t.Name)
        |> List.ofSeq

    /// The corpus above is under this fixture's control, so it can only ever pin shapes we
    /// thought to write down. FSharp.Core is not: this sweeps every type in it that inherits
    /// from another of its own types, which is where the reported failure lived
    /// (`PrintfFormat`5 : PrintfFormat`4`).
    [<Test>]
    let ``storage and access-site field identities agree across FSharp.Core`` () : unit =
        let mutable state = baseState
        let mutable swept = 0
        let mutable skipped : (string * string) list = []
        let mutable violations : Violation list = []

        for typeInfo in sweepableFsharpCoreTypes do
            // Instantiate uniformly at Int32. Nothing about the invariant depends on *which*
            // closed arguments are chosen -- the corpus property above varies them -- and a
            // single choice keeps this sweep cheap enough to run over the whole assembly.
            let args =
                List.replicate typeInfo.Generics.Length (TypeDefn.PrimitiveType PrimitiveType.Int32)

            try
                let s, handle = layoutReady state typeInfo args
                let s, found = violationsFor s handle
                let s, stability = inheritedIdentitiesAreStable s handle
                state <- s
                swept <- swept + 1
                violations <- violations @ found @ stability
            with e ->
                // Layout can fail for reasons with nothing to do with this invariant (an
                // unimplemented zero value, an unresolvable base chain, an instantiation that
                // violates a constraint we do not check). Those are not evidence either way, but
                // they must stay visible: a silent skip would let this sweep decay to covering
                // nothing while still passing.
                skipped <- (typeInfo.Namespace + "." + typeInfo.Name, e.Message) :: skipped

        TestContext.Out.WriteLine
            $"swept %d{swept} of %d{sweepableFsharpCoreTypes.Length} FSharp.Core type(s); %d{skipped.Length} could not be laid out"

        for name, reason in List.truncate 20 (List.rev skipped) do
            TestContext.Out.WriteLine $"  skipped %s{name}: %s{reason}"

        match violations with
        | [] -> ()
        | problems ->
            let rendered = problems |> List.map string |> String.concat "\n  "
            failwith $"%d{problems.Length} field-identity disagreement(s) in FSharp.Core:\n  %s{rendered}"

        // Guard against the sweep silently degenerating: if a future change makes most types
        // unlayoutable, this test would otherwise keep passing while covering nothing.
        if swept < 500 then
            failwith
                $"only %d{swept} FSharp.Core type(s) could be laid out, out of %d{sweepableFsharpCoreTypes.Length}; this sweep is no longer covering anything meaningful"

    /// `PrintfFormat`5 : PrintfFormat`4` is the exact shape from the bug report -- the derived
    /// type adds a fifth type parameter and its base takes the first four -- and `value` is the
    /// exact field. Pinned by name as well as by the sweep above, so that a future change to
    /// what the sweep enumerates cannot quietly drop it.
    [<Test>]
    let ``PrintfFormat's inherited value field is keyed to the type that declares it`` () : unit =
        let derived =
            fsharpCore.TryGetTopLevelTypeDef "Microsoft.FSharp.Core" "PrintfFormat`5"
            |> Option.defaultWith (fun () -> failwith "Microsoft.FSharp.Core.PrintfFormat`5 not found in FSharp.Core")

        let baseType =
            fsharpCore.TryGetTopLevelTypeDef "Microsoft.FSharp.Core" "PrintfFormat`4"
            |> Option.defaultWith (fun () -> failwith "Microsoft.FSharp.Core.PrintfFormat`4 not found in FSharp.Core")

        let args =
            [
                TypeDefn.PrimitiveType PrimitiveType.String
                TypeDefn.PrimitiveType PrimitiveType.Object
                TypeDefn.PrimitiveType PrimitiveType.String
                TypeDefn.PrimitiveType PrimitiveType.String
                TypeDefn.PrimitiveType PrimitiveType.Int32
            ]

        let state, derivedHandle = layoutReady baseState derived args

        // The base is instantiated at the derived type's *first four* arguments.
        let state, baseHandle = layoutReady state baseType (List.truncate 4 args)

        let state, fields =
            IlMachineState.collectAllInstanceFields loggerFactory bct state derivedHandle

        let valueField =
            fields |> List.filter (fun f -> f.Name = "value") |> List.exactlyOne

        // The load-bearing claim: the identity is keyed to PrintfFormat`4<a,b,c,d>, *not* to the
        // PrintfFormat`5<a,b,c,d,e> whose storage it lives in.
        FieldId.tryDeclaringType valueField.Id |> shouldEqual (Some baseHandle)
        derivedHandle |> shouldNotEqual baseHandle

        let _, violations = violationsFor state derivedHandle
        violations |> List.map string |> shouldEqual []
