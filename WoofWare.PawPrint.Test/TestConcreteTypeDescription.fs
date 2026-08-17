namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `AllConcreteTypes.describe` is the renderer every concrete-type diagnostic goes through, and its
/// output is the entire product: no execution path depends on what a message says, so these tests
/// are the only thing that holds its rendering to a contract.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestConcreteTypeDescription =

    let private corelib : DumpedAssembly =
        // The factory is intentionally undisposed: the returned DumpedAssembly.Logger closes over
        // its sinks, and disposing while the assembly is still live would silently drop events.
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private assemblies : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ]

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private baseConcreteTypes : AllConcreteTypes =
        Corelib.concretizeAll assemblies bct AllConcreteTypes.Empty

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle baseConcreteTypes ti

    let private typeDefNamed (ns : string) (name : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TypeDefs.Values
        |> Seq.filter (fun ti -> ti.Namespace = ns && ti.Name = name)
        |> Seq.toList
        |> function
            | [ ti ] -> ti
            | [] -> failwith $"corelib has no TypeDef %s{ns}.%s{name}"
            | tis -> failwith $"corelib has %d{List.length tis} TypeDefs named %s{ns}.%s{name}"

    /// Register `ti` instantiated at `args`, so a test can hand the renderer exactly the argument
    /// list it means to, including nestings the corelib graph does not itself contain.
    let private instantiate
        (concreteTypes : AllConcreteTypes)
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (args : ConcreteTypeHandle list)
        : ConcreteTypeHandle * AllConcreteTypes
        =
        ConcreteType.makeFromIdentity ti.Identity ti.Namespace ti.Name (ImmutableArray.CreateRange args)
        |> fun ct -> AllConcreteTypes.add ct concreteTypes

    /// The nested type `leaf` declared inside the corelib type named `parent`. A nested type's own
    /// `Namespace` is empty, so `typeDefNamed` cannot find one.
    let private nestedTypeDefNamed (parent : string) (leaf : string) : TypeInfo<GenericParamFromMetadata, TypeDefn> =
        corelib.TypeDefs.Values
        |> Seq.filter (fun ti ->
            ti.Name = leaf
            && ti.IsNested
            && (
                match corelib.TypeDefs.TryGetValue ti.DeclaringType with
                | true, declaring -> declaring.Name = parent
                | false, _ -> false
            )
        )
        |> Seq.toList
        |> function
            | [ ti ] -> ti
            | [] -> failwith $"corelib has no nested TypeDef %s{parent}+%s{leaf}"
            | tis -> failwith $"corelib has %d{List.length tis} nested TypeDefs named %s{parent}+%s{leaf}"

    /// A function-pointer handle. Every field of `TypeMethodSignature` takes part in the handle's
    /// identity, so each is settable here.
    let private fnptr
        (callingConvention : SignatureCallingConvention)
        (genericParameterCount : int)
        (requiredParameterCount : int)
        (parameters : ConcreteTypeHandle list)
        : ConcreteTypeHandle
        =
        ConcreteTypeHandle.FunctionPointer
            {
                Header =
                    SignatureHeader (SignatureKind.Method, callingConvention, SignatureAttributes.None)
                    |> ComparableSignatureHeader.Make
                ParameterTypes = parameters
                GenericParameterCount = genericParameterCount
                RequiredParameterCount = requiredParameterCount
                ReturnType = MethodReturnType.Void
            }

    /// The integer a `Concrete` handle renders as.
    let private idOf (handle : ConcreteTypeHandle) : int =
        match handle with
        | ConcreteTypeHandle.Concrete id -> id
        | other -> failwith $"expected a Concrete handle, got %O{other}"

    let private stringId : int = idOf (handleFor bct.String)
    let private int32Id : int = idOf (handleFor bct.Int32)

    let private describe (concreteTypes : AllConcreteTypes) (handle : ConcreteTypeHandle) : string =
        AllConcreteTypes.describe assemblies concreteTypes handle

    [<Test>]
    let ``a generic instantiation renders its type arguments`` () : unit =
        let listOfString, concreteTypes =
            instantiate baseConcreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ handleFor bct.String ]

        describe concreteTypes listOfString
        |> shouldEqual
            $"System.Collections.Generic.List`1<System.String#%d{stringId}>#%d{idOf listOfString} [System.Private.CoreLib]"

    /// Only the head names its assembly: the identity a reader needs on an argument is its
    /// `#handle`, and repeating the assembly on every one buries the difference between two
    /// renderings under boilerplate identical to both.
    [<Test>]
    let ``type arguments are not assembly-qualified`` () : unit =
        let listOfString, concreteTypes =
            instantiate baseConcreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ handleFor bct.String ]

        let rendered = describe concreteTypes listOfString

        rendered.Split("[System.Private.CoreLib]").Length - 1 |> shouldEqual 1

    [<Test>]
    let ``generic arguments nest`` () : unit =
        let listOfInt, concreteTypes =
            instantiate baseConcreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ handleFor bct.Int32 ]

        let dictionary, concreteTypes =
            instantiate
                concreteTypes
                (typeDefNamed "System.Collections.Generic" "Dictionary`2")
                [ handleFor bct.String ; listOfInt ]

        describe concreteTypes dictionary
        |> shouldEqual
            $"System.Collections.Generic.Dictionary`2<System.String#%d{stringId}, System.Collections.Generic.List`1<System.Int32#%d{int32Id}>#%d{idOf listOfInt}>#%d{idOf dictionary} [System.Private.CoreLib]"

    /// Two instantiations of one generic type must be distinguishable *by their type arguments*,
    /// which is what makes a report comparing two instantiations legible.
    ///
    /// Asserted on the argument names rather than as `describe a <> describe b`: the trailing
    /// `(concrete N)` differs between any two handles, so inequality holds even when the arguments
    /// are dropped entirely.
    [<Test>]
    let ``instantiations of the same type are distinguished by their arguments`` () : unit =
        let listOfString, concreteTypes =
            instantiate baseConcreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ handleFor bct.String ]

        let listOfInt, concreteTypes =
            instantiate concreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ handleFor bct.Int32 ]

        let renderedString = describe concreteTypes listOfString
        let renderedInt = describe concreteTypes listOfInt

        renderedString |> shouldContainText "System.String"
        renderedString |> shouldNotContainText "System.Int32"
        renderedInt |> shouldContainText "System.Int32"
        renderedInt |> shouldNotContainText "System.String"

    /// A nested type's `Namespace` is empty and its name is just the leaf, so `List`1+Enumerator`
    /// and `Queue`1+Enumerator` are both `Enumerator` unless the declaring-type path is kept. Both
    /// exist in corelib, which is why this pair rather than a synthetic one.
    [<Test>]
    let ``nested types keep their declaring-type path`` () : unit =
        let listEnumerator, concreteTypes =
            instantiate baseConcreteTypes (nestedTypeDefNamed "List`1" "Enumerator") [ handleFor bct.Int32 ]

        let queueEnumerator, concreteTypes =
            instantiate concreteTypes (nestedTypeDefNamed "Queue`1" "Enumerator") [ handleFor bct.Int32 ]

        let renderedList = describe concreteTypes listEnumerator
        renderedList |> shouldContainText "List`1+Enumerator"
        renderedList |> shouldNotContainText "Queue"

        describe concreteTypes queueEnumerator |> shouldContainText "Queue`1+Enumerator"

    /// Two types sharing a full name across assemblies -- which also covers two versions of one
    /// assembly, where even the short assembly name matches -- must not render alike as arguments.
    /// The `#handle` separates them; the name cannot.
    [<Test>]
    let ``arguments sharing a full name across assemblies render differently`` () : unit =
        let guest =
            TypeIdentityTestHelpers.compileLibrary "GuestAsm" [] [ "namespace System { public class String { } }" ]
            |> TypeIdentityTestHelpers.dumpedAssembly None

        let assemblies = LoadedAssemblies.ofAssemblies [ corelib ; guest ]

        let guestString =
            guest.TypeDefs.Values
            |> Seq.filter (fun ti -> ti.Namespace = "System" && ti.Name = "String")
            |> Seq.exactlyOne

        let guestHandle, concreteTypes = instantiate baseConcreteTypes guestString []

        let listOfGuest, concreteTypes =
            instantiate concreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ guestHandle ]

        let listOfCorelib, concreteTypes =
            instantiate concreteTypes (typeDefNamed "System.Collections.Generic" "List`1") [ handleFor bct.String ]

        let renderedGuest = AllConcreteTypes.describe assemblies concreteTypes listOfGuest

        let renderedCorelib =
            AllConcreteTypes.describe assemblies concreteTypes listOfCorelib

        // Both arguments are spelled `System.String`, so only the handle can tell them apart.
        renderedGuest |> shouldContainText "System.String#"
        renderedCorelib |> shouldContainText "System.String#"

        let argumentOf (rendered : string) : string =
            rendered.Substring (rendered.IndexOf '<', rendered.IndexOf '>' - rendered.IndexOf '<')

        argumentOf renderedGuest |> shouldNotEqual (argumentOf renderedCorelib)

    /// `lookup` returns `None` for every structural handle by design, so "unregistered" would be a
    /// false accusation against the interpreter rather than a description of these handles.
    [<TestCase("byref")>]
    [<TestCase("pointer")>]
    [<TestCase("szarray")>]
    [<TestCase("array")>]
    let ``structural handles render structurally`` (kind : string) : unit =
        let element = handleFor bct.String

        let handle, expectedSuffix =
            match kind with
            | "byref" -> ConcreteTypeHandle.Byref element, "&"
            | "pointer" -> ConcreteTypeHandle.Pointer element, "*"
            | "szarray" -> ConcreteTypeHandle.OneDimArrayZero element, "[]"
            | "array" -> ConcreteTypeHandle.Array (element, 2), "[,]"
            | other -> failwith $"unknown structural kind %s{other}"

        let rendered = describe baseConcreteTypes handle

        rendered |> shouldContainText "System.String"
        rendered |> shouldContainText expectedSuffix
        rendered |> shouldNotContainText "unregistered"

    /// A structural wrapper is still the type being reported, so it keeps the head's
    /// qualification.
    [<Test>]
    let ``a structural handle keeps its assembly qualification`` () : unit =
        ConcreteTypeHandle.Byref (handleFor bct.String)
        |> describe baseConcreteTypes
        |> shouldEqual $"System.String#%d{stringId} [System.Private.CoreLib]&"

    /// The global namespace is the empty string in metadata, so a renderer that joins
    /// unconditionally emits `.Widget` for a type declared outside any namespace.
    [<Test>]
    let ``a type in the global namespace has no leading dot`` () : unit =
        // `<Module>` is the one TypeDef every assembly has in the global namespace. It is not among
        // the types `Corelib.concretizeAll` registers, so it has to be registered here.
        let handle, concreteTypes =
            instantiate baseConcreteTypes (typeDefNamed "" "<Module>") []

        describe concreteTypes handle
        |> shouldEqual $"<Module>#%d{idOf handle} [System.Private.CoreLib]"

    [<Test>]
    let ``an unregistered handle says so rather than throwing`` () : unit =
        ConcreteTypeHandle.Concrete System.Int32.MaxValue
        |> describe baseConcreteTypes
        |> shouldContainText "unregistered concrete type"

    /// A structural chain is peeled iteratively rather than recursed, so it is never truncated
    /// however deep it goes, and two chains over different elements stay distinguishable.
    [<Test>]
    let ``deeply wrapped structural types are rendered in full and stay distinct`` () : unit =
        let wrap (element : ConcreteTypeHandle) : ConcreteTypeHandle =
            List.replicate 200 ()
            |> List.fold (fun h () -> ConcreteTypeHandle.OneDimArrayZero h) element

        let overString = describe baseConcreteTypes (wrap (handleFor bct.String))
        let overInt = describe baseConcreteTypes (wrap (handleFor bct.Int32))

        overString |> shouldContainText "System.String"
        overString |> shouldNotContainText "nested deeper than"
        overString |> shouldNotEqual overInt

    /// A declaring-type chain is walked iteratively and refuses to revisit a row, so neither depth
    /// nor a cycle in a guest's `NestedClass` table can overflow the stack while a diagnostic is
    /// being formatted. Twenty levels exceeds `describeDepthBudget`, so this also pins the marker.
    [<Test>]
    let ``a deeply nested type is bounded rather than overflowing`` () : unit =
        let source =
            // C# forbids a nested type sharing its enclosing type's name, so each level is
            // numbered.
            let opens =
                [ 1..20 ] |> List.map (fun i -> $"public class N%d{i} {{") |> String.concat " "

            let closes = List.replicate 20 "}" |> String.concat " "
            $"namespace Deep {{ %s{opens} public class Leaf {{ }} %s{closes} }}"

        let guest =
            TypeIdentityTestHelpers.compileLibrary "DeepAsm" [] [ source ]
            |> TypeIdentityTestHelpers.dumpedAssembly None

        let assemblies = LoadedAssemblies.ofAssemblies [ corelib ; guest ]

        let leaf =
            guest.TypeDefs.Values
            |> Seq.filter (fun ti -> ti.Name = "Leaf")
            |> Seq.exactlyOne

        let handle, concreteTypes = instantiate baseConcreteTypes leaf []

        let rendered = AllConcreteTypes.describe assemblies concreteTypes handle
        rendered |> shouldContainText "Leaf"
        rendered |> shouldContainText "..+"

    /// Two function pointers differing only in one of these fields must not render alike: each is
    /// part of `TypeMethodSignature`, and so of the handle's identity.
    [<Test>]
    let ``function pointers differing only in calling convention render differently`` () : unit =
        let args = [ handleFor bct.String ]

        describe baseConcreteTypes (fnptr SignatureCallingConvention.CDecl 0 1 args)
        |> shouldNotEqual (describe baseConcreteTypes (fnptr SignatureCallingConvention.StdCall 0 1 args))

    [<Test>]
    let ``function pointers differing only in generic arity render differently`` () : unit =
        let args = [ handleFor bct.String ]

        describe baseConcreteTypes (fnptr SignatureCallingConvention.Default 0 1 args)
        |> shouldNotEqual (describe baseConcreteTypes (fnptr SignatureCallingConvention.Default 2 1 args))

    /// A vararg signature lists more parameters than it requires, so `RequiredParameterCount` is
    /// the only thing separating these two.
    [<Test>]
    let ``function pointers differing only in required parameter count render differently`` () : unit =
        let args = [ handleFor bct.String ; handleFor bct.Int32 ]

        describe baseConcreteTypes (fnptr SignatureCallingConvention.VarArgs 0 1 args)
        |> shouldNotEqual (describe baseConcreteTypes (fnptr SignatureCallingConvention.VarArgs 0 2 args))
