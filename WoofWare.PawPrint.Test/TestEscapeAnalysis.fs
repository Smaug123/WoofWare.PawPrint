namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open FsUnitTyped
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// The escape analysis over a fixture whose every method states what must, and must not, escape
/// it. Each expectation was written from the analysis's stated envelope before it was run.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEscapeAnalysis =

    let private source =
        """
using System;

namespace Fixture;

public static class Cases
{
    public static void ThrowsDirectly() { throw new InvalidOperationException("boom"); }

    public static void CaughtExactly()
    {
        try { ThrowsDirectly(); }
        catch (InvalidOperationException) { }
    }

    // SystemException is CoreLib's, so seeing that it covers InvalidOperationException needs the
    // base chain of a type in another assembly.
    public static void CaughtByBase()
    {
        try { ThrowsDirectly(); }
        catch (SystemException) { }
    }

    public static void UnrelatedCatch()
    {
        try { ThrowsDirectly(); }
        catch (ArgumentException) { }
    }

    public static void FinallyDoesNotCatch()
    {
        try { ThrowsDirectly(); }
        finally { GC.KeepAlive(null); }
    }

    public static void PropagatesOneHop() { UnrelatedCatch(); }

    // Entering it takes a monitor, which an interrupted wait abandons before any IL runs.
    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.Synchronized)]
    public static void Synchronised() { }

    public static void Unsynchronised() { }

    public static void CatchesInterruption()
    {
        try { Synchronised(); }
        catch (System.Threading.ThreadInterruptedException) { }
    }

    public static void TwoSources(bool b)
    {
        if (b) { ThrowsDirectly(); }
        else { throw new FormatException(); }
    }

    public static void CatchesBoth(bool b)
    {
        try { TwoSources(b); }
        catch (Exception) { }
    }

    public static int Leaf(int a) => a;

    public static void Recursive(int n)
    {
        if (n <= 0) { ThrowsDirectly(); }
        else { Recursive(n - 1); }
    }

    public static void Rethrows()
    {
        try { ThrowsDirectly(); }
        catch (Exception) { throw; }
    }

    public static void ThrowsLocalDerived() { throw new Derived(); }

    public static void CaughtByLocalBase()
    {
        try { ThrowsLocalDerived(); }
        catch (LocalBase) { }
    }

    static InvalidOperationException MakeInvalid() => new InvalidOperationException("made");

    // Only the helper's declared return type is known of what is thrown.
    public static void ThrowsHelperResult() { throw MakeInvalid(); }

    public static void HelperCaughtByBase()
    {
        try { ThrowsHelperResult(); }
        catch (SystemException) { }
    }

    // ObjectDisposedException derives from InvalidOperationException, so it catches only some of
    // what the helper may return.
    public static void HelperNotCaughtBySubclass()
    {
        try { ThrowsHelperResult(); }
        catch (ObjectDisposedException) { }
    }

    public static string CallsVirtual(Animal a) => a.Speak();

    // C# calls an instance method with callvirt; a non-virtual target is not dispatched.
    public static void CallsNonVirtualViaCallvirt(Sealed s) { s.Boom(); }

    // Cases has no type initializer, so calling into it cannot fail on one.
    public static int CallsLeaf() => Leaf(1);

    public static void CoreLibThrowHelper(object o) { ArgumentNullException.ThrowIfNull(o); }

    // Nothing says what the parameter is at run time.
    public static void ThrowsParameter(Exception e) { throw e; }
}

// A static virtual is dispatched on the type argument: the default body is not what runs for
// ThrowingParse.
public interface IParse
{
    static virtual int Parse() => 1;
}

public sealed class ThrowingParse : IParse
{
    public static int Parse() => throw new FormatException();
}

public static class StaticVirtualCases
{
    public static int CallParse<T>() where T : IParse => T.Parse();
}

// Each instantiation has its own initializer: initializing G<string> runs G<int>'s, which can fail.
public class G<T>
{
    public static int Value = 1 / G<int>.Value;
}

public class LocalBase : Exception { }

public class Derived : LocalBase { }

public class Animal
{
    public virtual string Speak() => "...";
}

public sealed class Sealed
{
    public void Boom() { throw new FormatException(); }
}

public static class Boom
{
    public static readonly int Value = int.Parse("not a number");

    public static int M() => Value;
}

public static class CctorCases
{
    public static int CallsBoom() => Boom.M();
}

public class Shadowed
{
    public int Field;
}

public static class ShadowCases
{
    // A catch for a locally declared System.NullReferenceException must not absorb the one the
    // runtime raises.
    public static int DereferencesNull(Shadowed s)
    {
        try { return s.Field; }
        catch (System.NullReferenceException) { return -1; }
    }
}
"""

    /// The local shadow the last case catches.
    let private shadow =
        """
namespace System;

public class NullReferenceException : Exception { }
"""

    /// What a fixture method's answer must hold. A thrown type is written `=T` for `Exactly T` and
    /// `<:T` for `SubtypeOf T`.
    type private Expectation =
        {
            Method : string * string
            Contains : string list
            Excludes : string list
            Unknown : bool option
        }

    let private expect (ty : string) (name : string) : Expectation =
        {
            Method = ty, name
            Contains = []
            Excludes = []
            Unknown = None
        }

    let private expectations : Expectation list =
        let ioe = "=System.InvalidOperationException"

        [
            { expect "Fixture.Cases" "ThrowsDirectly" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "CaughtExactly" with
                Excludes = [ ioe ]
            }
            { expect "Fixture.Cases" "CaughtByBase" with
                Excludes = [ ioe ]
            }
            { expect "Fixture.Cases" "UnrelatedCatch" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "FinallyDoesNotCatch" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "PropagatesOneHop" with
                Contains = [ ioe ]
            }
            { expect "Fixture.Cases" "Synchronised" with
                Contains = [ "=System.Threading.ThreadInterruptedException" ]
            }
            { expect "Fixture.Cases" "Unsynchronised" with
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
            }
            { expect "Fixture.Cases" "CatchesInterruption" with
                Excludes = [ "=System.Threading.ThreadInterruptedException" ]
            }
            { expect "Fixture.Cases" "TwoSources" with
                Contains = [ ioe ; "=System.FormatException" ]
            }
            // `catch (Exception)` absorbs everything, including what could not be named.
            { expect "Fixture.Cases" "CatchesBoth" with
                Excludes = [ ioe ; "=System.FormatException" ; "=System.StackOverflowException" ]
                Unknown = Some false
            }
            { expect "Fixture.Cases" "Leaf" with
                Unknown = Some false
            }
            { expect "Fixture.Cases" "Recursive" with
                Contains = [ ioe ]
            }
            // The `throw;` is in the handler, outside the region its catch protects.
            { expect "Fixture.Cases" "Rethrows" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "ThrowsLocalDerived" with
                Contains = [ "=Fixture.Derived" ]
            }
            { expect "Fixture.Cases" "CaughtByLocalBase" with
                Excludes = [ "=Fixture.Derived" ]
            }
            { expect "Fixture.Cases" "ThrowsHelperResult" with
                Contains = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "HelperCaughtByBase" with
                Excludes = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "HelperNotCaughtBySubclass" with
                Contains = [ "<:System.InvalidOperationException" ]
            }
            { expect "Fixture.Cases" "CallsVirtual" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "CallsNonVirtualViaCallvirt" with
                Contains = [ "=System.FormatException" ]
            }
            { expect "Fixture.Cases" "CallsLeaf" with
                Excludes = [ "=System.TypeInitializationException" ]
            }
            { expect "Fixture.Cases" "ThrowsParameter" with
                Unknown = Some true
            }
            { expect "Fixture.Cases" "CoreLibThrowHelper" with
                Contains = [ "=System.ArgumentNullException" ]
            }
            { expect "Fixture.StaticVirtualCases" "CallParse" with
                Unknown = Some true
            }
            { expect "Fixture.G`1" ".cctor" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            // `Boom`'s initializer fails, and calling `M` runs it first.
            { expect "Fixture.CctorCases" "CallsBoom" with
                Contains = [ "=System.TypeInitializationException" ]
            }
            { expect "Fixture.ShadowCases" "DereferencesNull" with
                Contains = [ "=System.NullReferenceException" ]
            }
        ]

    /// An answer as the expectations spell it: `=T` for `Exactly T`, `<:T` for `SubtypeOf T`.
    let private render (analysis : EscapeAnalysisState) (escapes : Escapes) : Set<string> =
        escapes.Types
        |> Seq.map (fun thrown ->
            match thrown with
            | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
            | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
        )
        |> Set.ofSeq

    /// The method of this name on the type of this full name, in `assembly`.
    let private methodNamed (assembly : DumpedAssembly) (typeName : string) (methodName : string) : MethodKey =
        assembly.Methods
        |> Seq.pick (fun (KeyValue (handle, method)) ->
            if
                method.Name = methodName
                && TypeInfo.fullName
                    (fun h -> assembly.TypeDefs.[h])
                    assembly.TypeDefs.[method.RequiredDeclaringType.Definition.Get] = typeName
            then
                Some (MethodKey.make assembly handle)
            else
                None
        )

    /// An analysis over CoreLib and `assemblies`, with `bind` applied to the load context.
    let private analysisOver
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        let frameworkDir = FrameworkUnderTest.sharedFrameworkDirectory ()
        let runtimeDirs = FrameworkUnderTest.runtimeDirs ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (Path.Combine (frameworkDir, "System.Private.CoreLib.dll"))

        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies (corelib :: assemblies) |> bind

        EscapeAnalysis.create
            loggerFactory
            runtimeDirs
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

    [<Test>]
    let ``each fixture method's escaping exceptions are as stated`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let image =
            Roslyn.compileAssembly "EscapeFixture" OutputKind.DynamicallyLinkedLibrary [] [ source ; shadow ]

        let fixture =
            Assembly.read loggerFactory (Some "EscapeFixture.dll") (new MemoryStream (image))

        let mutable analysis = analysisOver [ fixture ] id
        let failures = ResizeArray<string> ()

        for expectation in expectations do
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed fixture (fst expectation.Method) (snd expectation.Method))

            analysis <- next
            let shown = render analysis escapes

            let describe () =
                let ty, name = expectation.Method
                $"%s{ty}::%s{name}: %A{Set.toList shown}, unknown %b{escapes.Unknown}"

            for wanted in expectation.Contains do
                if not (shown.Contains wanted) then
                    failures.Add $"%s{describe ()} lacks %s{wanted}"

            for unwanted in expectation.Excludes do
                if shown.Contains unwanted then
                    failures.Add $"%s{describe ()} has %s{unwanted}"

            match expectation.Unknown with
            | Some unknown when unknown <> escapes.Unknown ->
                failures.Add $"%s{describe ()}, expected unknown %b{unknown}"
            | _ -> ()

        if failures.Count > 0 then
            failures |> String.concat Environment.NewLine |> failwith

    /// A client compiled against one version of a provider, run against another that lacks what it
    /// uses. The JIT binds a body's tokens before the body runs, so the failure comes out of the
    /// caller and the client's own `catch` cannot stop it.
    [<Test>]
    let ``what the provider no longer has fails to bind, past the method's own handlers`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let version1 =
            """
namespace Provider;

public class Parent
{
    public static int Value = 1;
    public static void Gone() { }
    public static void Varargs(__arglist) { }
}

public class GoneType { }

public class GoneException : System.Exception { }
"""

        let version2 =
            """
namespace Provider;

public class Parent
{
    public static void Varargs(__arglist) { }
}
"""

        let client =
            """
namespace Client;

public static class Uses
{
    public static int Read() => Provider.Parent.Value;
    public static void CallGone() { Provider.Parent.Gone(); }
    public static void CaughtCallGone()
    {
        try { Provider.Parent.Gone(); }
        catch (System.MissingMethodException) { }
    }
    public static object UseGoneType() => new Provider.GoneType();
    public static bool IsGone(object o) => o is Provider.GoneType;
    public static object ListOfGone() => new System.Collections.Generic.List<Provider.GoneType>();
    public static bool LocalOfGone()
    {
        Provider.GoneType x = null;
        return x != null;
    }
    public static int CatchGone(int x)
    {
        try { return 1 / x; }
        catch (Provider.GoneException) { return 42; }
    }
    static void Accept(Provider.GoneType x) { }
    public static void PassGone() { Accept(null); }
    public static void PassGoneAsVararg() { Provider.Parent.Varargs(__arglist((Provider.GoneType)null)); }
    public static bool CaughtLocalOfGone()
    {
        try
        {
            Provider.GoneType x = null;
            return x != null;
        }
        catch (System.TypeLoadException) { return false; }
    }
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let providerImage1 = compile "Provider" [] version1
        let clientAssembly = read "Client" (compile "Client" [ providerImage1 ] client)

        let answers (provider : DumpedAssembly) : string -> Set<string> =
            let providerReference =
                clientAssembly.AssemblyReferences.Values
                |> Seq.find (fun r -> r.Name.Name = "Provider")

            let mutable analysis =
                analysisOver
                    [ clientAssembly ; provider ]
                    (fun loaded -> fst (loaded.WithBoundReference providerReference provider))

            fun methodName ->
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Uses" methodName)

                analysis <- next
                render analysis escapes

        let against1 = answers (read "Provider" providerImage1)
        let against2 = answers (read "Provider" (compile "Provider" [] version2))

        for methodName, failure in
            [
                "Read", "=System.MissingFieldException"
                "CallGone", "=System.MissingMethodException"
                "CaughtCallGone", "=System.MissingMethodException"
                "UseGoneType", "=System.TypeLoadException"
                // A type token of its own.
                "IsGone", "=System.TypeLoadException"
                // A type argument of the member's parent, whose definition does resolve.
                "ListOfGone", "=System.TypeLoadException"
                // Named by no instruction, only by the type of a local.
                "LocalOfGone", "=System.TypeLoadException"
                "CaughtLocalOfGone", "=System.TypeLoadException"
                // Named only by a `catch` clause.
                "CatchGone", "=System.TypeLoadException"
                // Named only by the signature of the method called.
                "PassGone", "=System.TypeLoadException"
                // Named only by a vararg call site's extra arguments, which no definition declares.
                "PassGoneAsVararg", "=System.TypeLoadException"
            ] do
            let bound = against1 methodName
            let unbound = against2 methodName

            if bound.Contains failure then
                failwith $"%s{methodName} against the provider it was compiled against: %A{Set.toList bound}"

            if not (unbound.Contains failure) then
                failwith $"%s{methodName} against the provider lacking what it uses: %A{Set.toList unbound}"

    /// Loads each image of `images` by its simple name, so that one refers to another.
    type private ImagesContext (images : Map<string, byte[]>) =
        inherit System.Runtime.Loader.AssemblyLoadContext ("Images", isCollectible = true)

        override this.Load (name : AssemblyName) : Assembly =
            match images.TryFind name.Name with
            | Some image -> this.LoadFromStream (new MemoryStream (image))
            | None -> null

    [<Test>]
    let ``a module initializer runs when another module first binds to what it declares`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let provider =
            """
namespace Provider;

static class Init
{
    [System.Runtime.CompilerServices.ModuleInitializer]
    internal static void Run() => throw new System.InvalidOperationException();
}

public static class P
{
    public static int Field;
    public static void M() { }
    public static void CallsM() { M(); }
}

public class Base { }

public class Foreign { }
"""

        let client =
            """
namespace Client;

public class Derived : Provider.Base
{
    public static void Static() { }
}

public class Local<T> { }

public class DerivedOfForeign : Local<Provider.Foreign>
{
    public static void Static() { }
}

public static class Uses
{
    public static void CallM() { Provider.P.M(); }
    public static int CaughtCallM()
    {
        try { Provider.P.M(); return 0; }
        catch (System.TypeInitializationException) { return 1; }
    }
    public static int ReadField() => Provider.P.Field;
    public static object NewDerived() => new Derived();
    public static System.Type TypeOfDerived() => typeof(Derived);
    public static void CallDerivedStatic() { Derived.Static(); }
    public static void CallDerivedOfForeignStatic() { DerivedOfForeign.Static(); }
    public static int Local() => 1;
}
"""

        let compile (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileAssembly
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
                [ text ]

        let providerImage = compile "Provider" [] provider
        let clientImage = compile "Client" [ providerImage ] client

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let providerAssembly = read "Provider" providerImage
        let clientAssembly = read "Client" clientImage

        let providerReference =
            clientAssembly.AssemblyReferences.Values
            |> Seq.find (fun r -> r.Name.Name = "Provider")

        let mutable analysis =
            analysisOver
                [ clientAssembly ; providerAssembly ]
                (fun loaded -> fst (loaded.WithBoundReference providerReference providerAssembly))

        let reportsInitialisation (assembly : DumpedAssembly) (typeName : string) (methodName : string) : bool =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly typeName methodName)

            analysis <- next
            render analysis escapes |> Set.contains "=System.TypeInitializationException"

        // Each in a context of its own, since a module initializer that failed fails every later
        // binding the same way.
        let escapesOnRealRuntime (methodName : string) : bool =
            let context =
                new ImagesContext (Map.ofList [ "Provider", providerImage ; "Client", clientImage ])

            try
                let uses = context.LoadFromAssemblyName(AssemblyName "Client").GetType "Client.Uses"

                try
                    uses.GetMethod(methodName).Invoke ((null : obj), Array.empty<obj>)
                    |> ignore<obj>

                    false
                with :? TargetInvocationException ->
                    true
            finally
                context.Unload ()

        for methodName in
            [
                "CallM"
                "CaughtCallM"
                "ReadField"
                "NewDerived"
                // Provider is reached only through Derived's base type.
                "TypeOfDerived"
                "CallDerivedStatic"
                // Provider is reached only through an argument of that base type.
                "CallDerivedOfForeignStatic"
            ] do
            escapesOnRealRuntime methodName |> shouldEqual true

            if not (reportsInitialisation clientAssembly "Client.Uses" methodName) then
                failwith $"%s{methodName} binds into Provider, whose module initializer throws"

        escapesOnRealRuntime "Local" |> shouldEqual false
        reportsInitialisation clientAssembly "Client.Uses" "Local" |> shouldEqual false
        // A module's own code runs only once its initializer has.
        reportsInitialisation providerAssembly "Provider.P" "CallsM"
        |> shouldEqual false

    /// `int32[,]`'s constructor taking lower bounds and lengths raises `ArgumentOutOfRangeException`
    /// for bounds whose upper end overflows; the one taking lengths alone does not. C# spells
    /// neither, so the calls are emitted directly.
    [<Test>]
    let ``a multidimensional array constructor taking lower bounds can reject them`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "ArrayCtors.dll",
            metadata.GetOrAddGuid (Guid "3c9d1b2a-4e5f-4a6b-9c8d-7e6f5a4b3c2d"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        metadata.AddAssembly (
            metadata.GetOrAddString "ArrayCtors",
            Version (1, 0, 0, 0),
            Unchecked.defaultof<StringHandle>,
            Unchecked.defaultof<BlobHandle>,
            Unchecked.defaultof<AssemblyFlags>,
            AssemblyHashAlgorithm.None
        )
        |> ignore<AssemblyDefinitionHandle>

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let objectRef =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString "System",
                metadata.GetOrAddString "Object"
            )

        let rankTwo =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .TypeSpecificationSignature()
                .Array (
                    (fun element -> element.Int32 ()),
                    (fun shape -> shape.Shape (2, ImmutableArray.Empty, ImmutableArray.Create (0, 0)))
                )

            metadata.AddTypeSpecification (metadata.GetOrAddBlob blob)

        let constructor (arity : int) : MemberReferenceHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = true)
                .Parameters (
                    arity,
                    (fun returnType -> returnType.Void ()),
                    (fun parameters ->
                        for _ in 1..arity do
                            parameters.AddParameter().Type().Int32 ()
                    )
                )

            metadata.AddMemberReference (
                (TypeSpecificationHandle.op_Implicit rankTwo : EntityHandle),
                metadata.GetOrAddString ".ctor",
                metadata.GetOrAddBlob blob
            )

        let body (arity : int) : int =
            let code = InstructionEncoder (BlobBuilder ())

            for _ in 1..arity do
                code.LoadConstantI4 1

            code.OpCode ILOpCode.Newobj
            code.Token (MemberReferenceHandle.op_Implicit (constructor arity) : EntityHandle)
            code.OpCode ILOpCode.Pop
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        let staticVoid =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature()
                .Parameters (0, (fun returnType -> returnType.Void ()), ignore<ParametersEncoder>)

            metadata.GetOrAddBlob blob

        let addMethod (name : string) (arity : int) =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                staticVoid,
                body arity,
                MetadataTokens.ParameterHandle 1
            )

        let lowerBounds = addMethod "LowerBounds" 4
        addMethod "Lengths" 2 |> ignore<MethodDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            lowerBounds
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Arrays",
            (TypeReferenceHandle.op_Implicit objectRef : EntityHandle),
            MetadataTokens.FieldDefinitionHandle 1,
            lowerBounds
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>

        let assembly =
            Assembly.read loggerFactory (Some "ArrayCtors.dll") (new MemoryStream (image.ToArray ()))

        let mutable analysis = analysisOver [ assembly ] id

        let answer (name : string) =
            let next, escapes =
                EscapeAnalysis.escapes analysis (methodNamed assembly "W.Arrays" name)

            analysis <- next
            render analysis escapes

        let withBounds = answer "LowerBounds"
        let withoutBounds = answer "Lengths"
        withBounds |> shouldContain "=System.ArgumentOutOfRangeException"
        withBounds |> shouldContain "=System.OverflowException"
        withoutBounds |> shouldNotContain "=System.ArgumentOutOfRangeException"
        withoutBounds |> shouldContain "=System.OverflowException"

    /// Where the object a `throw` raises comes from, in an emitted method.
    [<RequireQualifiedAccess>]
    type private Raise =
        /// `newobj object::.ctor; throw`.
        | Constructed
        /// `call object Make(); throw`, whose static type is only `object`.
        | Returned
        /// `call object MakeException(); throw`: an exception, whose static type is only `object`.
        | ReturnedException
        /// `call Throw()`, which throws a constructed object.
        | Callee
        /// `ldnull; throw`, whose operand's type the analysis does not know.
        | Untyped

    /// The `catch` clause, if any, around an emitted method's raise.
    [<RequireQualifiedAccess>]
    type private Clause =
        | Uncaught
        | Catch of ns : string * name : string

    let private raises : Raise list =
        [
            Raise.Constructed
            Raise.Returned
            Raise.ReturnedException
            Raise.Callee
            Raise.Untyped
        ]

    let private clauses : Clause list =
        [
            Clause.Uncaught
            Clause.Catch ("System", "Exception")
            Clause.Catch ("System.Runtime.CompilerServices", "RuntimeWrappedException")
            Clause.Catch ("System", "Object")
        ]

    let private emittedMethodName (raise : Raise) (clause : Clause) : string =
        match clause with
        | Clause.Uncaught -> $"%A{raise}"
        | Clause.Catch (_, name) -> $"%A{raise}_%s{name}"

    /// An assembly `W.Throws` with one static method per raise and clause, plus the helpers `Make`,
    /// `MakeException` and `Throw`, carrying one `RuntimeCompatibilityAttribute` per blob in `attributes`, in order.
    let private emitThrows (attributes : byte[] list) : byte[] =
        let metadata = MetadataBuilder ()
        let ilStream = BlobBuilder ()
        let bodies = MethodBodyStreamEncoder ilStream

        metadata.AddModule (
            0,
            metadata.GetOrAddString "Throws.dll",
            metadata.GetOrAddGuid (Guid "5a1e7c3b-9d2f-4b8e-a6c4-1f3e5d7b9a2c"),
            Unchecked.defaultof<GuidHandle>,
            Unchecked.defaultof<GuidHandle>
        )
        |> ignore<ModuleDefinitionHandle>

        let assemblyDefinition =
            metadata.AddAssembly (
                metadata.GetOrAddString "Throws",
                Version (1, 0, 0, 0),
                Unchecked.defaultof<StringHandle>,
                Unchecked.defaultof<BlobHandle>,
                Unchecked.defaultof<AssemblyFlags>,
                AssemblyHashAlgorithm.None
            )

        let corelibName = typeof<obj>.Assembly.GetName ()

        let corelibRef =
            metadata.AddAssemblyReference (
                metadata.GetOrAddString corelibName.Name,
                corelibName.Version,
                Unchecked.defaultof<StringHandle>,
                metadata.GetOrAddBlob (corelibName.GetPublicKeyToken ()),
                Unchecked.defaultof<AssemblyFlags>,
                Unchecked.defaultof<BlobHandle>
            )

        let typeRef (ns : string) (name : string) : EntityHandle =
            metadata.AddTypeReference (
                (AssemblyReferenceHandle.op_Implicit corelibRef : EntityHandle),
                metadata.GetOrAddString ns,
                metadata.GetOrAddString name
            )
            |> TypeReferenceHandle.op_Implicit

        let objectRef = typeRef "System" "Object"

        let signature (isInstance : bool) (returnsObject : bool) : BlobHandle =
            let blob = BlobBuilder ()

            BlobEncoder(blob)
                .MethodSignature(isInstanceMethod = isInstance)
                .Parameters (
                    0,
                    (fun returnType ->
                        if returnsObject then
                            returnType.Type().Object ()
                        else
                            returnType.Void ()
                    ),
                    ignore<ParametersEncoder>
                )

            metadata.GetOrAddBlob blob

        let constructorOf (ty : EntityHandle) : EntityHandle =
            metadata.AddMemberReference (ty, metadata.GetOrAddString ".ctor", signature true false)
            |> MemberReferenceHandle.op_Implicit

        let objectConstructor = constructorOf objectRef
        let exceptionConstructor = constructorOf (typeRef "System" "Exception")

        let compatibilityConstructor =
            constructorOf (typeRef "System.Runtime.CompilerServices" "RuntimeCompatibilityAttribute")

        for blob in attributes do
            metadata.AddCustomAttribute (
                (AssemblyDefinitionHandle.op_Implicit assemblyDefinition : EntityHandle),
                compatibilityConstructor,
                metadata.GetOrAddBlob blob
            )
            |> ignore<CustomAttributeHandle>

        // Method definitions are numbered in the order they are added: `Make`, `MakeException`,
        // `Throw`, then the cases in the order of `raises` and `clauses`.
        let make = MetadataTokens.MethodDefinitionHandle 1
        let makeException = MetadataTokens.MethodDefinitionHandle 2
        let throw = MetadataTokens.MethodDefinitionHandle 3

        let emitRaise (code : InstructionEncoder) (raise : Raise) : unit =
            match raise with
            | Raise.Constructed ->
                code.OpCode ILOpCode.Newobj
                code.Token objectConstructor
                code.OpCode ILOpCode.Throw
            | Raise.Returned ->
                code.Call make
                code.OpCode ILOpCode.Throw
            | Raise.ReturnedException ->
                code.Call makeException
                code.OpCode ILOpCode.Throw
            | Raise.Callee -> code.Call throw
            | Raise.Untyped ->
                code.OpCode ILOpCode.Ldnull
                code.OpCode ILOpCode.Throw

        let addMethod (name : string) (returnsObject : bool) (body : int) : unit =
            metadata.AddMethodDefinition (
                MethodAttributes.Public ||| MethodAttributes.Static,
                MethodImplAttributes.IL,
                metadata.GetOrAddString name,
                signature false returnsObject,
                body,
                MetadataTokens.ParameterHandle 1
            )
            |> ignore<MethodDefinitionHandle>

        let makeBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Newobj
            code.Token objectConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        addMethod "Make" true makeBody

        let makeExceptionBody =
            let code = InstructionEncoder (BlobBuilder ())
            code.OpCode ILOpCode.Newobj
            code.Token exceptionConstructor
            code.OpCode ILOpCode.Ret
            bodies.AddMethodBody code

        addMethod "MakeException" true makeExceptionBody

        let throwBody =
            let code = InstructionEncoder (BlobBuilder ())
            emitRaise code Raise.Constructed
            bodies.AddMethodBody code

        addMethod "Throw" false throwBody

        for raise in raises do
            for clause in clauses do
                let body =
                    match clause with
                    | Clause.Uncaught ->
                        let code = InstructionEncoder (BlobBuilder ())
                        emitRaise code raise
                        code.OpCode ILOpCode.Ret
                        bodies.AddMethodBody code
                    | Clause.Catch (ns, name) ->
                        let flow = ControlFlowBuilder ()
                        let code = InstructionEncoder (BlobBuilder (), flow)
                        let tryStart = code.DefineLabel ()
                        let handlerStart = code.DefineLabel ()
                        let handlerEnd = code.DefineLabel ()
                        code.MarkLabel tryStart
                        emitRaise code raise
                        code.Branch (ILOpCode.Leave_s, handlerEnd)
                        code.MarkLabel handlerStart
                        code.OpCode ILOpCode.Pop
                        code.Branch (ILOpCode.Leave_s, handlerEnd)
                        code.MarkLabel handlerEnd
                        code.OpCode ILOpCode.Ret
                        flow.AddCatchRegion (tryStart, handlerStart, handlerStart, handlerEnd, typeRef ns name)
                        bodies.AddMethodBody code

                addMethod (emittedMethodName raise clause) false body

        metadata.AddTypeDefinition (
            TypeAttributes.Class,
            Unchecked.defaultof<StringHandle>,
            metadata.GetOrAddString "<Module>",
            Unchecked.defaultof<EntityHandle>,
            MetadataTokens.FieldDefinitionHandle 1,
            make
        )
        |> ignore<TypeDefinitionHandle>

        metadata.AddTypeDefinition (
            TypeAttributes.Public
            ||| TypeAttributes.Class
            ||| TypeAttributes.Abstract
            ||| TypeAttributes.Sealed,
            metadata.GetOrAddString "W",
            metadata.GetOrAddString "Throws",
            objectRef,
            MetadataTokens.FieldDefinitionHandle 1,
            make
        )
        |> ignore<TypeDefinitionHandle>

        let peBuilder =
            ManagedPEBuilder (
                PEHeaderBuilder (imageCharacteristics = Characteristics.Dll),
                MetadataRootBuilder metadata,
                ilStream
            )

        let image = BlobBuilder ()
        peBuilder.Serialize image |> ignore<BlobContentId>
        image.ToArray ()

    /// Run each of `methods` of `W.Throws` in `image` on the real runtime, answering which let an
    /// exception escape.
    let private escapingOnRealRuntime (image : byte[]) (methods : string list) : Map<string, bool> =
        let context =
            System.Runtime.Loader.AssemblyLoadContext ("Throws", isCollectible = true)

        try
            let throws = context.LoadFromStream(new MemoryStream (image)).GetType "W.Throws"

            methods
            |> List.map (fun name ->
                let escaped =
                    try
                        throws.GetMethod(name).Invoke ((null : obj), Array.empty<obj>) |> ignore<obj>
                        false
                    with :? TargetInvocationException ->
                        true

                name, escaped
            )
            |> Map.ofList
        finally
            context.Unload ()

    /// A `RuntimeCompatibility` blob: the prolog, a named-argument count, and the arguments.
    let private compatibilityBlob (count : int16) (arguments : byte list list) : byte[] =
        [ 0x01uy ; 0x00uy ; byte count ; byte (count >>> 8) ] @ List.concat arguments
        |> Array.ofList

    /// A named argument: its field or property tag, its serialization type, its name and its value.
    let private namedArgument (kind : byte) (serialization : byte) (name : string) (value : byte list) : byte list =
        let name = Text.Encoding.UTF8.GetBytes name
        [ kind ; serialization ; byte name.Length ] @ List.ofArray name @ value

    let private wrapProperty (value : byte list) : byte list =
        namedArgument 0x54uy 0x02uy "WrapNonExceptionThrows" value

    [<Test>]
    let ``whether an assembly wraps what it throws is read as CoreCLR reads it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let cases : (string * byte[] list) list =
            [
                "no attribute", []
                "true", [ compatibilityBlob 1s [ wrapProperty [ 1uy ] ] ]
                "false", [ compatibilityBlob 1s [ wrapProperty [ 0uy ] ] ]
                "any nonzero byte", [ compatibilityBlob 1s [ wrapProperty [ 2uy ] ] ]
                "set as a field",
                [
                    compatibilityBlob 1s [ namedArgument 0x53uy 0x02uy "WrapNonExceptionThrows" [ 1uy ] ]
                ]
                "set twice", [ compatibilityBlob 2s [ wrapProperty [ 1uy ] ; wrapProperty [ 1uy ] ] ]
                "then an unknown argument",
                [
                    compatibilityBlob 2s [ wrapProperty [ 1uy ] ; namedArgument 0x54uy 0x02uy "Other" [ 1uy ] ]
                ]
                "no arguments", [ compatibilityBlob 0s [] ]
                "no count", [ [| 0x01uy ; 0x00uy |] ]
                "a negative count", [ compatibilityBlob -1s [ wrapProperty [ 1uy ] ] ]
                "as an int32",
                [
                    compatibilityBlob
                        1s
                        [
                            namedArgument 0x54uy 0x08uy "WrapNonExceptionThrows" [ 1uy ; 0uy ; 0uy ; 0uy ]
                        ]
                ]
                "under another case",
                [
                    compatibilityBlob 1s [ namedArgument 0x54uy 0x02uy "wrapNonExceptionThrows" [ 1uy ] ]
                ]
                "without its value", [ compatibilityBlob 1s [ wrapProperty [] ] ]
                "with trailing bytes", [ Array.append (compatibilityBlob 1s [ wrapProperty [ 1uy ] ]) [| 0xAAuy |] ]
                "under a bad prolog",
                [
                    Array.append [| 0x00uy ; 0x00uy |] (compatibilityBlob 1s [ wrapProperty [ 1uy ] ]).[2..]
                ]
                "false, then true",
                [
                    compatibilityBlob 1s [ wrapProperty [ 0uy ] ]
                    compatibilityBlob 1s [ wrapProperty [ 1uy ] ]
                ]
                "true, then false",
                [
                    compatibilityBlob 1s [ wrapProperty [ 1uy ] ]
                    compatibilityBlob 1s [ wrapProperty [ 0uy ] ]
                ]
            ]

        let probe =
            emittedMethodName Raise.Constructed (Clause.Catch ("System", "Exception"))

        let answers =
            cases
            |> List.map (fun (description, attributes) ->
                let image = emitThrows attributes
                // A `catch (Exception)` stops the object only if the assembly wraps it.
                let runtime = not (escapingOnRealRuntime image [ probe ]).[probe]

                let ours =
                    Assembly.read loggerFactory (Some "Throws.dll") (new MemoryStream (image))
                    |> RuntimeCompatibility.wrapsNonExceptionThrows

                description, runtime, ours
            )

        for description, runtime, ours in answers do
            if runtime <> ours then
                failwith $"%s{description}: the runtime wraps %b{runtime}, we say %b{ours}"

        // Both answers occur, so neither side is constant.
        answers
        |> List.map (fun (_, runtime, _) -> runtime)
        |> List.distinct
        |> List.length
        |> shouldEqual 2

    [<Test>]
    let ``a thrown object that is not an exception is caught as the catching assembly sees it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let cases =
            [
                for raise in raises do
                    for clause in clauses do
                        raise, clause, emittedMethodName raise clause
            ]

        for wraps in [ false ; true ] do
            let image =
                emitThrows (
                    if wraps then
                        [ compatibilityBlob 1s [ wrapProperty [ 1uy ] ] ]
                    else
                        []
                )

            let assembly =
                Assembly.read loggerFactory (Some "Throws.dll") (new MemoryStream (image))

            RuntimeCompatibility.wrapsNonExceptionThrows assembly |> shouldEqual wraps

            // `ldnull; throw` raises a `NullReferenceException` at run time, which says nothing
            // about how an unknown exception is caught, so only the analysis is asked about those.
            let runtime =
                cases
                |> List.filter (fun (raise, _, _) -> raise <> Raise.Untyped)
                |> List.map (fun (_, _, name) -> name)
                |> escapingOnRealRuntime image

            let mutable analysis = analysisOver [ assembly ] id

            for raise, clause, name in cases do
                let next, escapes =
                    EscapeAnalysis.escapes analysis (methodNamed assembly "W.Throws" name)

                analysis <- next

                let describe () =
                    let wrapping = if wraps then "wraps" else "does not wrap"
                    $"%s{name} in an assembly that %s{wrapping}: %A{render analysis escapes}, unknown %b{escapes.Unknown}"

                match raise with
                | Raise.Untyped ->
                    let absorbed =
                        match clause with
                        | Clause.Catch ("System", "Object") -> true
                        | Clause.Catch ("System", "Exception") -> wraps
                        | _ -> false

                    if escapes.Unknown = absorbed then
                        failwith $"%s{describe ()}; expected unknown %b{not absorbed}"
                | _ ->
                    let objectEscapes =
                        render analysis escapes
                        |> Set.exists (fun shown -> shown = "=System.Object" || shown = "<:System.Object")

                    // Everything that escapes is reported. A returned object's type is known only
                    // to be `object`, so it may or may not be an exception, and whatever could not
                    // stop both is reported to let it escape; otherwise the answer is exact.
                    let exact =
                        match raise with
                        | Raise.Returned
                        | Raise.ReturnedException -> false
                        | _ -> true

                    if
                        (runtime.[name] && not objectEscapes)
                        || (exact && objectEscapes <> runtime.[name])
                    then
                        failwith $"%s{describe ()}; the runtime lets it escape: %b{runtime.[name]}"
