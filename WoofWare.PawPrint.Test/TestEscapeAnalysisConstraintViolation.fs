namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// The escape analysis against the host's CoreCLR on generic instantiations that a constraint
/// rejects. A library `Lib` is compiled twice, with its type parameters' constraints and without,
/// and a client is compiled against the build without them, so that the client's IL spells
/// instantiations and method specifications that the build with them rejects. Run against that
/// build, the host's JIT raises `TypeLoadException` (a type) or `VerificationException` (a
/// method) before the method naming one runs, or, where it looks the type up at run time, raises
/// it inside the method; the analysis must say the same.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestEscapeAnalysisConstraintViolation =

    /// One set of constraints on a type parameter, as C# spells it after `where T :`.
    type private ConstraintSet =
        {
            Name : string
            Clause : string
        }

    let private constraintSets : ConstraintSet list =
        [
            {
                Name = "Class"
                Clause = "class"
            }
            {
                Name = "Struct"
                Clause = "struct"
            }
            {
                Name = "New"
                Clause = "new()"
            }
            {
                Name = "IFoo"
                Clause = "IFoo"
            }
            {
                Name = "Base"
                Clause = "Base"
            }
            {
                Name = "IComparable"
                Clause = "System.IComparable"
            }
            {
                Name = "IEquatableOfSelf"
                Clause = "System.IEquatable<T>"
            }
            {
                Name = "EnumerableOfObject"
                Clause = "System.Collections.Generic.IEnumerable<object>"
            }
            {
                Name = "ListOfObject"
                Clause = "System.Collections.Generic.IList<object>"
            }
            {
                Name = "ClassIFoo"
                Clause = "class, IFoo"
            }
            {
                Name = "StructIFoo"
                Clause = "struct, IFoo"
            }
            {
                Name = "IFooNew"
                Clause = "IFoo, new()"
            }
            {
                Name = "IFooAllowsRef"
                Clause = "IFoo, allows ref struct"
            }
            {
                Name = "Enum"
                Clause = "System.Enum"
            }
        ]

    /// A type argument, as the client spells it in C#.
    let private arguments : (string * string) list =
        [
            "Int", "int"
            "String", "string"
            "NullableInt", "int?"
            "Object", "object"
            "DayOfWeek", "System.DayOfWeek"
            "Foo", "Foo"
            "NoCtor", "NoCtor"
            "AbstractFoo", "AbstractFoo"
            "FooStruct", "FooStruct"
            "RefFoo", "RefFoo"
            "ListOfString", "System.Collections.Generic.List<string>"
            "ListOfInt", "System.Collections.Generic.List<int>"
            "StringArray", "string[]"
            "DayOfWeekArray", "System.DayOfWeek[]"
        ]

    /// Where a case's IL spells the instantiation.
    [<RequireQualifiedAccess>]
    type private Position =
        /// `call` of a static method whose member reference's parent is the instantiation.
        | StaticCall
        /// `newobj` of its constructor.
        | Newobj
        /// `ldtoken` of it.
        | Ldtoken
        /// `isinst` of it.
        | Isinst
        /// `newarr` of it.
        | Newarr
        /// `newobj` of a class deriving from it.
        | BaseClass
        /// The parameter type, a struct instantiation, of a method the case calls.
        | CalleeParameter
        /// `ldtoken` of a struct with an instance field of a struct instantiation.
        | ValueField
        /// `call` of a method specification instantiating a constrained generic method.
        | MethodSpecCall
        /// `ldftn` of such a method specification, making a delegate.
        | MethodSpecLdftn
        /// `call` of a static method of the instantiation over a type variable, in a generic
        /// method the case calls instantiated with the argument.
        | SharedStaticCall
        /// `ldtoken` of the instantiation over a type variable, in such a generic method.
        | SharedLdtoken
        /// `call` of a method specification over a type variable, in such a generic method.
        | SharedMethodSpec

    let private positions : Position list =
        [
            Position.StaticCall
            Position.Newobj
            Position.Ldtoken
            Position.Isinst
            Position.Newarr
            Position.BaseClass
            Position.CalleeParameter
            Position.ValueField
            Position.MethodSpecCall
            Position.MethodSpecLdftn
            Position.SharedStaticCall
            Position.SharedLdtoken
            Position.SharedMethodSpec
        ]

    /// The positions at which the analysis does not yet report a violation the host raises, which
    /// it must then not name at all, so that a position leaves this set as the analysis learns to
    /// report violations there.
    let private unreported : Set<Position> = Set.ofList positions

    /// Whether the generic method a case of this position calls is a definition whose own
    /// instructions CoreCLR's JIT checks with the method's type variables as arguments
    /// (`CEEInfo::GetMethodForSecurity`), which the analysis does not model and so may answer
    /// "unknown" for.
    let private checkedOpen (position : Position) : bool =
        match position with
        | Position.SharedStaticCall
        | Position.SharedMethodSpec -> true
        | Position.StaticCall
        | Position.Newobj
        | Position.Ldtoken
        | Position.Isinst
        | Position.Newarr
        | Position.BaseClass
        | Position.CalleeParameter
        | Position.ValueField
        | Position.MethodSpecCall
        | Position.MethodSpecLdftn
        | Position.SharedLdtoken -> false

    type private Case =
        {
            Position : Position
            Set : ConstraintSet
            /// The argument's name and its C# spelling.
            Argument : string * string
        }

        member this.Name : string =
            $"%O{this.Position}_%s{this.Set.Name}_%s{fst this.Argument}"

    let private cases : Case list =
        [
            for position in positions do
                for set in constraintSets do
                    for argument in arguments do
                        yield
                            {
                                Position = position
                                Set = set
                                Argument = argument
                            }
        ]

    /// `Lib`, with its constraints or without. Without, each type parameter allows a byref-like
    /// argument, so that the client can spell one.
    let private librarySource (constrained : bool) : string =
        let clause (set : ConstraintSet) =
            if constrained then
                $"where T : %s{set.Clause}"
            else
                "where T : allows ref struct"

        [
            yield "namespace Lib;"
            yield "public interface IFoo { }"
            yield "public class Base { }"
            yield "public static class Methods"
            yield "{"

            for set in constraintSets do
                yield $"    public static void %s{set.Name}<T>() %s{clause set} {{ }}"

            yield "}"

            for set in constraintSets do
                yield $"public class C%s{set.Name}<T> %s{clause set} {{ public static void Touch() {{ }} }}"
                yield $"public struct S%s{set.Name}<T> %s{clause set} {{ }}"
        ]
        |> String.concat "\n"

    /// The client: each case a method of `Client.Cases` returning 0 where its body runs to the end,
    /// and 1 where its body raises something, which it catches. A failure to compile it, or the
    /// generic method it calls, escapes it instead.
    let private clientSource : string =
        let body (case : Case) : string list =
            let set = case.Set.Name
            let argument = snd case.Argument
            let name = case.Name

            // The case itself, running `statement` inside a `catch` of every exception.
            let caseMethod (statement : string) : string =
                $"public static int %s{name}() {{ try {{ %s{statement} return 0; }} catch (System.Exception) {{ return 1; }} }}"

            // A generic method running `statement` so, and the case calling it with the argument.
            let viaGeneric (statement : string) : string list =
                [
                    $"public static int Generic%s{name}<T>() where T : allows ref struct {{ try {{ %s{statement} return 0; }} catch (System.Exception) {{ return 1; }} }}"
                    $"public static int %s{name}() => Generic%s{name}<%s{argument}>();"
                ]

            let instantiation = $"Lib.C%s{set}<%s{argument}>"

            match case.Position with
            | Position.StaticCall -> [ caseMethod (instantiation + ".Touch();") ]
            | Position.Newobj -> [ caseMethod ("Keep(new " + instantiation + "());") ]
            | Position.Ldtoken -> [ caseMethod ("Keep(typeof(" + instantiation + "));") ]
            | Position.Isinst -> [ caseMethod ("Keep(Seen is " + instantiation + ");") ]
            | Position.Newarr -> [ caseMethod ("Keep(new " + instantiation + "[0]);") ]
            | Position.BaseClass ->
                [
                    $"public class Derived%s{name} : %s{instantiation} {{ }}"
                    caseMethod ("Keep(new Derived" + name + "());")
                ]
            | Position.CalleeParameter ->
                [
                    $"public static void Takes%s{name}(Lib.S%s{set}<%s{argument}> s) {{ }}"
                    caseMethod ("Takes" + name + "(default);")
                ]
            | Position.ValueField ->
                [
                    $"public struct Holds%s{name} {{ public Lib.S%s{set}<%s{argument}> F; }}"
                    caseMethod ("Keep(typeof(Holds" + name + "));")
                ]
            | Position.MethodSpecCall -> [ caseMethod $"Lib.Methods.%s{set}<%s{argument}>();" ]
            | Position.MethodSpecLdftn -> [ caseMethod $"Keep(new System.Action(Lib.Methods.%s{set}<%s{argument}>));" ]
            | Position.SharedStaticCall -> viaGeneric $"Lib.C%s{set}<T>.Touch();"
            | Position.SharedLdtoken -> viaGeneric $"Keep(typeof(Lib.C%s{set}<T>));"
            | Position.SharedMethodSpec -> viaGeneric $"Lib.Methods.%s{set}<T>();"

        [
            yield "namespace Client;"
            yield "public class Foo : Lib.Base, Lib.IFoo { public Foo() { } }"
            yield "public class NoCtor : Lib.IFoo { private NoCtor() { } }"
            yield "public abstract class AbstractFoo : Lib.IFoo { }"
            yield "public struct FooStruct : Lib.IFoo { }"
            yield "public ref struct RefFoo : Lib.IFoo { }"
            yield "public static class Cases"
            yield "{"
            yield "    public static object Seen = new object();"
            yield
                "    [System.Runtime.CompilerServices.MethodImpl(System.Runtime.CompilerServices.MethodImplOptions.NoInlining)]"
            yield "    public static void Keep(object o) { }"

            for case in cases do
                for line in body case do
                    yield "    " + line

            yield "}"
        ]
        |> String.concat "\n"

    /// What running a case on the host's CoreCLR does.
    [<RequireQualifiedAccess>]
    type private Outcome =
        /// Its body runs to the end.
        | Runs
        /// Its body raises something, which its own `catch` stops.
        | Caught
        /// Compiling it, or the generic method it calls, raises the exception of this full name,
        /// before the body raising it runs.
        | Escapes of exceptionName : string

    let private typeLoad = "System.TypeLoadException"
    let private verification = "System.Security.VerificationException"

    let private compile (name : string) (references : byte[] list) (text : string) : byte[] =
        Roslyn.compileAssembly
            name
            OutputKind.DynamicallyLinkedLibrary
            (references
             |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
            [ text ]

    let private hostCoreLib () : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()

        Assembly.readFile
            loggerFactory
            (Path.Combine (FrameworkUnderTest.sharedFrameworkDirectory (), "System.Private.CoreLib.dll"))

    /// The architecture the framework under test's JIT compiles for.
    let private hostTarget () : JitTarget =
        match Runtime.InteropServices.RuntimeInformation.ProcessArchitecture with
        | Runtime.InteropServices.Architecture.Arm64 -> JitTarget.Arm64
        | Runtime.InteropServices.Architecture.X64 -> JitTarget.X64
        | other -> failwith $"No JIT table describes the host's architecture, %O{other}"

    /// An analysis over the host's CoreLib and `assemblies`, with `bind` applied to the load
    /// context, allowed every assumption, on a CPU with no instruction sets.
    let private analysisOver
        (assemblies : DumpedAssembly list)
        (bind : LoadedAssemblies -> LoadedAssemblies)
        : EscapeAnalysisState
        =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let corelib = hostCoreLib ()
        let baseClassTypes = BaseClassTypes.ofCorelib corelib
        let loaded = LoadedAssemblies.ofAssemblies (corelib :: assemblies) |> bind

        EscapeAnalysis.create
            loggerFactory
            (FrameworkUnderTest.runtimeDirs ())
            (hostTarget ())
            HardwareIntrinsicsProfile.ScalarOnly
            Assumption.all
            {
                ConcreteTypes = Corelib.concretizeAll loaded baseClassTypes AllConcreteTypes.Empty
                LoadedAssemblies = loaded
                BaseTypes = baseClassTypes
            }

    let private render (analysis : EscapeAnalysisState) (escapes : Escapes) : Set<string> =
        escapes.Types
        |> Seq.map (fun thrown ->
            match thrown with
            | ThrownType.Exactly ty -> "=" + EscapeAnalysis.typeName analysis ty
            | ThrownType.SubtypeOf ty -> "<:" + EscapeAnalysis.typeName analysis ty
        )
        |> Set.ofSeq

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

    /// Every case, run on the host's CoreCLR against the constrained `Lib`, and asked of the
    /// analysis over the same assemblies. Where the host raises a violation before the body runs,
    /// the analysis must name that exception, or, for a generic method CoreCLR checks with its own
    /// type variables as arguments, may answer "unknown"; elsewhere it must name neither
    /// exception.
    [<Test>]
    let ``a violated constraint raises before the body runs exactly where the host's JIT raises it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let constrained = compile "Lib" [] (librarySource true)
        let unconstrained = compile "Lib" [] (librarySource false)
        let clientImage = compile "Client" [ unconstrained ] clientSource

        let hostOutcomes : Map<string, Outcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("ConstraintViolations", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Lib" -> context.LoadFromStream (new MemoryStream (constrained))
                    | _ -> null
                )
            )

            try
                let type' =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Cases"

                cases
                |> List.map (fun case ->
                    let outcome =
                        try
                            match type'.GetMethod(case.Name).Invoke ((null : obj), [||]) with
                            | :? int as returned when returned = 0 -> Outcome.Runs
                            | :? int as returned when returned = 1 -> Outcome.Caught
                            | other -> failwith $"%s{case.Name} returned %O{other}"
                        with :? TargetInvocationException as e ->
                            Outcome.Escapes (e.InnerException.GetType().FullName)

                    case.Name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let libraryAssembly = read "Lib" constrained

        let mutable analysis =
            analysisOver
                [ clientAssembly ; libraryAssembly ]
                (fun loaded ->
                    let reference =
                        clientAssembly.AssemblyReferences.Values
                        |> Seq.find (fun r -> r.Name.Name = "Lib")

                    fst (loaded.WithBoundReference reference libraryAssembly)
                )

        let violations = Set.ofList [ typeLoad ; verification ]

        let failures =
            [
                for case in cases do
                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Cases" case.Name)

                    analysis <- next
                    let shown = render analysis escapes

                    let named =
                        violations
                        |> Set.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                    let describe () =
                        $"%s{case.Name}: the host's outcome is %A{hostOutcomes.[case.Name]}; the analysis names %A{Set.toList shown}, unknown %b{escapes.Unknown}"

                    match hostOutcomes.[case.Name] with
                    | Outcome.Escapes raised when violations.Contains raised ->
                        let agrees =
                            if unreported.Contains case.Position then
                                named.IsEmpty
                            else
                                named = Set.singleton raised
                                || (checkedOpen case.Position
                                    && escapes.Unknown
                                    && named.IsSubsetOf (Set.singleton raised))

                        if not agrees then
                            yield describe ()
                    | Outcome.Escapes other -> yield $"%s{case.Name}: the host raises %s{other}, which no case means to"
                    | Outcome.Runs
                    | Outcome.Caught ->
                        if not named.IsEmpty then
                            yield describe ()
            ]

        // Every position must have cases the host rejects and cases it accepts, or the agreement
        // above says nothing about one of the two directions there.
        let vacuous =
            [
                for position in positions do
                    let outcomes =
                        cases
                        |> List.filter (fun case -> case.Position = position)
                        |> List.map (fun case -> hostOutcomes.[case.Name])

                    let escaping =
                        outcomes
                        |> List.filter (fun outcome ->
                            match outcome with
                            | Outcome.Escapes _ -> true
                            | Outcome.Runs
                            | Outcome.Caught -> false
                        )
                        |> List.length

                    if escaping = 0 then
                        yield $"%O{position}: the host rejects no case"
                    elif escaping = outcomes.Length && not (checkedOpen position) then
                        yield $"%O{position}: the host rejects every case"
            ]

        match vacuous @ failures with
        | [] -> ()
        | problems ->
            failwith (
                $"%d{failures.Length} of %d{cases.Length} cases disagree with the host, %d{vacuous.Length} positions are vacuous:\n"
                + String.concat "\n" problems
            )

    /// A constraint that a type argument violates, on a type parameter whose other constraint names
    /// a type in an assembly that is not there. CoreCLR's full load of the instantiation loads the
    /// generic definition's constraints before it checks the instantiation's, so the missing
    /// assembly's `FileNotFoundException` comes out, and the violation's `TypeLoadException` never
    /// does; so too for a generic method of a type whose generic methods' constraints name it.
    [<Test>]
    let ``a load that misses fails before the constraint it would check`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let missing =
            compile "Missing" [] "namespace Missing; public interface IMissing { }"

        let library (constrained : bool) : string =
            if constrained then
                """
namespace Lib;
public class Both<T> where T : class, Missing.IMissing { public static void Touch() { } }
public class BothMethod<T> where T : class { public static void Touch() { } public static void Gm<U>() where U : Missing.IMissing { } }
"""
            else
                """
namespace Lib;
public class Both<T> { public static void Touch() { } }
public class BothMethod<T> { public static void Touch() { } public static void Gm<U>() { } }
"""

        let constrained = compile "Lib" [ missing ] (library true)
        let unconstrained = compile "Lib" [] (library false)

        let clientImage =
            compile
                "Client"
                [ unconstrained ]
                """
namespace Client;
public static class Cases
{
    public static int BothInt() { try { Lib.Both<int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int BothString() { try { Lib.Both<string>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int BothMethodInt() { try { Lib.BothMethod<int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
}
"""

        let names = [ "BothInt" ; "BothString" ; "BothMethodInt" ]
        let fileNotFound = "System.IO.FileNotFoundException"

        let hostOutcomes : Map<string, Outcome> =
            let context =
                Runtime.Loader.AssemblyLoadContext ("ConstraintAfterMiss", isCollectible = true)

            context.add_Resolving (
                Func<_, _, _> (fun (context : Runtime.Loader.AssemblyLoadContext) (name : AssemblyName) ->
                    match name.Name with
                    | "Lib" -> context.LoadFromStream (new MemoryStream (constrained))
                    | _ -> null
                )
            )

            try
                let type' =
                    context.LoadFromStream(new MemoryStream (clientImage)).GetType "Client.Cases"

                names
                |> List.map (fun name ->
                    let outcome =
                        try
                            match type'.GetMethod(name).Invoke ((null : obj), [||]) with
                            | :? int as returned when returned = 0 -> Outcome.Runs
                            | :? int as returned when returned = 1 -> Outcome.Caught
                            | other -> failwith $"%s{name} returned %O{other}"
                        with :? TargetInvocationException as e ->
                            Outcome.Escapes (e.InnerException.GetType().FullName)

                    name, outcome
                )
                |> Map.ofList
            finally
                context.Unload ()

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let libraryAssembly = read "Lib" constrained

        let mutable analysis =
            analysisOver
                [ clientAssembly ; libraryAssembly ]
                (fun loaded ->
                    let reference =
                        clientAssembly.AssemblyReferences.Values
                        |> Seq.find (fun r -> r.Name.Name = "Lib")

                    fst (loaded.WithBoundReference reference libraryAssembly)
                )

        let failures =
            [
                for name in names do
                    if hostOutcomes.[name] <> Outcome.Escapes fileNotFound then
                        yield $"%s{name}: the host's outcome is %A{hostOutcomes.[name]}, not %s{fileNotFound}"

                    let next, escapes =
                        EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Cases" name)

                    analysis <- next
                    let shown = render analysis escapes

                    if not (shown.Contains $"=%s{fileNotFound}") || shown.Contains $"=%s{typeLoad}" then
                        yield $"%s{name}: the analysis names %A{Set.toList shown}, unknown %b{escapes.Unknown}"
            ]

        if not failures.IsEmpty then
            failures |> String.concat "\n" |> failwith
