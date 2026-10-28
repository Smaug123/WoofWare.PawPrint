namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis
open NUnit.Framework
open WoofWare.PawPrint
open WoofWare.PawPrint.Analysis

/// The escape analysis against the real runtime under test on generic instantiations that a
/// constraint rejects. A library `Lib` is compiled twice, with its type parameters' constraints and
/// without, and a client is compiled against the build without them, so that the client's IL spells
/// instantiations and method specifications that the build with them rejects. Run against that
/// build, as its own process, the runtime's JIT raises `TypeLoadException` (a type) or
/// `VerificationException` (a method) before the method naming one runs, or, where it looks the
/// type up at run time, raises it inside the method; the analysis must say the same.
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
        /// `ldtoken` of a generic class over a type variable whose base is the instantiation,
        /// closed, in a generic method the case calls with `string`, whose code is shared.
        | SharedClosedBase

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
            Position.SharedClosedBase
        ]

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
        | Position.SharedLdtoken
        | Position.SharedClosedBase -> false

    /// Whether a case of this position calls a generic method, which the analysis is also asked
    /// about as a definition by itself.
    let private callsGeneric (position : Position) : bool =
        match position with
        | Position.SharedStaticCall
        | Position.SharedMethodSpec
        | Position.SharedLdtoken
        | Position.SharedClosedBase -> true
        | Position.StaticCall
        | Position.Newobj
        | Position.Ldtoken
        | Position.Isinst
        | Position.Newarr
        | Position.BaseClass
        | Position.CalleeParameter
        | Position.ValueField
        | Position.MethodSpecCall
        | Position.MethodSpecLdftn -> false

    /// The constraints of the generic method a case of a shared position calls, and what that
    /// method's body spells over its type variable in place of the bare variable.
    type private Within =
        {
            Outer : ConstraintSet
            /// The spelling's name and its C#, over the method's type variable `T`.
            Spelled : string * string
        }

    type private Case =
        {
            Position : Position
            Set : ConstraintSet
            /// The argument's name and its C# spelling.
            Argument : string * string
            /// `None` for a generic method constrained only to allow a byref-like argument, whose
            /// body spells the bare variable.
            Within : Within option
        }

        member this.Name : string =
            match this.Within with
            | None -> $"%O{this.Position}_%s{this.Set.Name}_%s{fst this.Argument}"
            | Some within -> $"Within%O{this.Position}_%s{this.Set.Name}_%s{within.Outer.Name}_%s{fst within.Spelled}"

    /// A type argument satisfying each set of constraints in C#, which a generic method so
    /// constrained is called with. Where the set allows a byref-like argument, it is one, which
    /// CoreCLR rejects where its parameter does not, though the variable standing for it satisfies
    /// that parameter.
    let private witness (set : ConstraintSet) : string * string =
        match set.Name with
        | "Class"
        | "IComparable"
        | "IEquatableOfSelf" -> "String", "string"
        | "Struct" -> "Int", "int"
        | "New"
        | "IFoo"
        | "Base"
        | "ClassIFoo"
        | "IFooNew" -> "Foo", "Foo"
        | "IFooAllowsRef" -> "RefFoo", "RefFoo"
        | "EnumerableOfObject"
        | "ListOfObject" -> "ListOfObject", "System.Collections.Generic.List<object>"
        | "StructIFoo" -> "FooStruct", "FooStruct"
        | "Enum" -> "DayOfWeek", "System.DayOfWeek"
        | other -> failwith $"no witness is chosen for the constraint set %s{other}"

    /// What the body of a generic method of a `Within` case spells over its type variable. A
    /// variable allowing a byref-like argument cannot be the argument of a CoreLib generic, so its
    /// method spells only the bare variable.
    let private spellingsWithin (outer : ConstraintSet) : (string * string) list =
        [
            yield "T", "T"

            if outer.Name <> "IFooAllowsRef" then
                yield "ListOfT", "System.Collections.Generic.List<T>"
                yield "PairOfT", "System.Collections.Generic.KeyValuePair<T, int>"
        ]

    /// The positions whose generic method is also called constrained, with each set of constraints:
    /// where the method's own constraints imply its body's, CoreCLR's checks pass whatever
    /// instantiates it, and the analysis may know so.
    let private positionsWithin : Position list =
        [
            Position.SharedStaticCall
            Position.SharedLdtoken
            Position.SharedMethodSpec
        ]

    /// Pairs of constraint sets, each by name, the first a generic method's own and the second the
    /// set of what its body instantiates with its bare type variable, where the first demands at
    /// least as much as the second by a rule the analysis states: so the analysis must know the
    /// method's answer by itself. A variable constrained to a class other than `Object`, `ValueType`
    /// and `Enum` satisfies `class`; `struct` gives `new()`; and a type constraint is satisfied by
    /// one of the variable's own, or by an interface one of those inherits.
    let private impliedWithin : Set<string * string> =
        Set.ofList
            [
                "Class", "Class"
                "Struct", "Struct"
                "Struct", "New"
                "New", "New"
                "Base", "Class"
                "Base", "Base"
                "IFoo", "IFoo"
                "ClassIFoo", "Class"
                "ClassIFoo", "IFoo"
                "StructIFoo", "Struct"
                "StructIFoo", "New"
                "StructIFoo", "IFoo"
                "IFooNew", "New"
                "IFooNew", "IFoo"
                "IComparable", "IComparable"
                "IEquatableOfSelf", "IEquatableOfSelf"
                "EnumerableOfObject", "EnumerableOfObject"
                "ListOfObject", "ListOfObject"
                "ListOfObject", "EnumerableOfObject"
                "Enum", "Enum"
            ]

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
                                Within = None
                            }

            for position in positionsWithin do
                for set in constraintSets do
                    for outer in constraintSets do
                        for spelled in spellingsWithin outer do
                            yield
                                {
                                    Position = position
                                    Set = set
                                    Argument = witness outer
                                    Within =
                                        Some
                                            {
                                                Outer = outer
                                                Spelled = spelled
                                            }
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
                let clause =
                    match case.Within with
                    | None -> "allows ref struct"
                    | Some within -> within.Outer.Clause

                [
                    $"public static int Generic%s{name}<T>() where T : %s{clause} {{ try {{ %s{statement} return 0; }} catch (System.Exception) {{ return 1; }} }}"
                    $"public static int %s{name}() => Generic%s{name}<%s{argument}>();"
                ]

            // What the generic method's body spells over its type variable.
            let spelled =
                match case.Within with
                | None -> "T"
                | Some within -> snd within.Spelled

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
            | Position.SharedStaticCall -> viaGeneric $"Lib.C%s{set}<%s{spelled}>.Touch();"
            | Position.SharedLdtoken -> viaGeneric $"Keep(typeof(Lib.C%s{set}<%s{spelled}>));"
            | Position.SharedMethodSpec -> viaGeneric $"Lib.Methods.%s{set}<%s{spelled}>();"
            | Position.SharedClosedBase ->
                [
                    $"public class DerivedOfGeneric%s{name}<U> : %s{instantiation} where U : allows ref struct {{ }}"
                    $"public static int Generic%s{name}<T>() where T : allows ref struct {{ try {{ Keep(typeof(DerivedOfGeneric%s{name}<T>)); return 0; }} catch (System.Exception) {{ return 1; }} }}"
                    $"public static int %s{name}() => Generic%s{name}<string>();"
                ]

        [
            // A constraint set's clause names `Lib`'s types bare.
            yield "using Lib;"
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

    /// What running a case on the real runtime under test does.
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

    /// `Client.Driver`, whose `Main` runs each method of `Client.Cases` that its first argument's
    /// file names, one to a line, and writes to its second argument's file a line for each: its
    /// name, a tab, and `Runs`, `Caught`, or `Escapes:` and the full name of what escaped it. It
    /// asks for each method by name alone, since reading the signature of a method whose parameter
    /// is a violating instantiation loads that type.
    let private driverSource : string =
        """
namespace Client;
public static class Driver
{
    public static void Main(string[] args)
    {
        using var writer = new System.IO.StreamWriter(args[1]);
        foreach (var name in System.IO.File.ReadAllLines(args[0]))
        {
            var method = typeof(Cases).GetMethod(name);
            string outcome;
            try { outcome = (int)method.Invoke(null, null) == 0 ? "Runs" : "Caught"; }
            catch (System.Reflection.TargetInvocationException e) { outcome = "Escapes:" + e.InnerException.GetType().FullName; }
            writer.WriteLine(name + "\t" + outcome);
        }
    }
}
"""

    /// The client, `source` and the driver, compiled against `references`.
    let private compileClient (references : byte[] list) (source : string) : byte[] =
        Roslyn.compileAssembly
            "Client"
            OutputKind.ConsoleApplication
            (references
             |> List.map (fun image -> MetadataReference.CreateFromImage (ImmutableArray.CreateRange image)))
            [ source ; driverSource ]

    /// What running each of the methods `names` of `Client.Cases` does on the framework under
    /// test's real runtime, as its own process, with `libraries` (each a name and an image) beside
    /// the client.
    let private hostOutcomes
        (libraries : (string * byte[]) list)
        (clientImage : byte[])
        (names : string list)
        : Map<string, Outcome>
        =
        let directory =
            Path.Combine (Path.GetTempPath (), "pawprint-constraints-" + Path.GetRandomFileName ())

        Directory.CreateDirectory directory |> ignore<DirectoryInfo>

        try
            for name, image in libraries do
                File.WriteAllBytes (Path.Combine (directory, name + ".dll"), image)

            let clientPath = Path.Combine (directory, "Client.dll")
            File.WriteAllBytes (clientPath, clientImage)
            let requested = Path.Combine (directory, "cases.txt")
            File.WriteAllLines (requested, names)
            let outcomes = Path.Combine (directory, "outcomes.txt")

            match RealRuntime.executeAssemblyInPlace [| requested ; outcomes |] clientPath with
            | RealRuntimeResult.NormalExit 0 -> ()
            | other -> failwith $"the driver ended %A{other}"

            File.ReadAllLines outcomes
            |> Array.map (fun line ->
                match line.Split '\t' with
                | [| name ; "Runs" |] -> name, Outcome.Runs
                | [| name ; "Caught" |] -> name, Outcome.Caught
                | [| name ; escaped |] when escaped.StartsWith "Escapes:" ->
                    name, Outcome.Escapes (escaped.Substring "Escapes:".Length)
                | _ -> failwith $"the driver wrote %s{line}"
            )
            |> Map.ofArray
        finally
            RealRuntime.deleteScratchTree directory

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

    /// An analysis over the framework under test's CoreLib and `assemblies`, with `bind` applied to
    /// the load context, allowed every assumption, on a CPU with no instruction sets.
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

    /// Every case, run on the real runtime under test against the constrained `Lib`, and asked of
    /// the analysis over the same assemblies. Where the runtime raises a violation before the body
    /// runs, the analysis must name that exception, or, for a generic method CoreCLR checks with
    /// its own type variables as arguments, may answer "unknown"; elsewhere it must name neither
    /// exception.
    [<Test>]
    let ``a violated constraint raises before the body runs exactly where the host's JIT raises it`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let constrained = compile "Lib" [] (librarySource true)
        let unconstrained = compile "Lib" [] (librarySource false)
        let clientImage = compileClient [ unconstrained ] clientSource

        let hostOutcomes =
            hostOutcomes [ "Lib", constrained ] clientImage (cases |> List.map _.Name)

        for case in cases do
            if not (hostOutcomes.ContainsKey case.Name) then
                failwith $"the driver did not run %s{case.Name}"

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

        // The positions where the analysis knows what some constrained generic method raises,
        // asked about by itself.
        let mutable knownWithin : Set<Position> = Set.empty

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

                    // Asked about by itself, the generic method stands for every instantiation. Where
                    // what it names is closed, the violation is the same in each, so its answer must
                    // be exactly the instance's. Unconstrained, some instantiations the constrained
                    // `Lib` rejects, and its answer must not be known. Constrained, its answer, if
                    // known, holds of every instantiation, so of the one the case makes too.
                    if callsGeneric case.Position then
                        let next, escapes =
                            EscapeAnalysis.escapes
                                analysis
                                (methodNamed clientAssembly "Client.Cases" $"Generic%s{case.Name}")

                        analysis <- next
                        let shown = render analysis escapes

                        let named =
                            violations
                            |> Set.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                        let raised =
                            match hostOutcomes.[case.Name] with
                            | Outcome.Escapes raised when violations.Contains raised -> Set.singleton raised
                            | Outcome.Escapes _
                            | Outcome.Runs
                            | Outcome.Caught -> Set.empty

                        let agrees =
                            match case.Position, case.Within with
                            | Position.SharedClosedBase, _ -> named = raised
                            | _, None -> escapes.Unknown
                            | _, Some within ->
                                if not escapes.Unknown then
                                    knownWithin <- Set.add case.Position knownWithin

                                let mustKnow =
                                    fst within.Spelled = "T"
                                    && impliedWithin.Contains (within.Outer.Name, case.Set.Name)

                                if mustKnow then
                                    not escapes.Unknown && named = raised
                                else
                                    escapes.Unknown || named = raised

                        if not agrees then
                            yield
                                $"Generic%s{case.Name}, by itself: %A{Set.toList shown}, unknown %b{escapes.Unknown}; the instance's outcome is %A{hostOutcomes.[case.Name]}"
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

                // Nor does the agreement of a generic method's answer by itself say anything where
                // that answer is never known.
                for position in positionsWithin do
                    if not (knownWithin.Contains position) then
                        yield $"%O{position}: the analysis knows no constrained generic method's answer by itself"
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
    /// does; so too for a generic method of a type whose generic methods' constraints name it. So
    /// too where the definition's own load misses, at its base or at the type of a value-type field,
    /// before its instantiation's constraints, which the argument violates, are checked. And a class
    /// whose value-type field's type is missing misses before its base, which violates a
    /// constraint, is checked, though the base comes first among what its full load brings.
    [<Test>]
    let ``a load that misses fails before the constraint it would check`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let missing =
            compile
                "Missing"
                []
                "namespace Missing; public interface IMissing { } public class BaseMissing { } public struct StructMissing { }"

        let library (constrained : bool) : string =
            if constrained then
                """
namespace Lib;
public class Both<T> where T : class, Missing.IMissing { public static void Touch() { } }
public class BothMethod<T> where T : class { public static void Touch() { } public static void Gm<U>() where U : Missing.IMissing { } }
public class Based<T> : Missing.BaseMissing where T : class { public static void Touch() { } }
public class Fielded<T> where T : class { public Missing.StructMissing F; public static void Touch() { } }
public class Strict<U> where U : class { }
"""
            else
                """
namespace Lib;
public class Both<T> { public static void Touch() { } }
public class BothMethod<T> { public static void Touch() { } public static void Gm<U>() { } }
public class Based<T> { public static void Touch() { } }
public class Fielded<T> { public static void Touch() { } }
public class Strict<U> { }
"""

        let constrained = compile "Lib" [ missing ] (library true)
        let unconstrained = compile "Lib" [] (library false)

        let clientImage =
            compileClient
                [ unconstrained ; missing ]
                """
namespace Client;
public class Ordered : Lib.Strict<int> { public Missing.StructMissing F; public static void Touch() { } }
public static class Cases
{
    public static int BothInt() { try { Lib.Both<int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int BothString() { try { Lib.Both<string>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int BothMethodInt() { try { Lib.BothMethod<int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int BasedInt() { try { Lib.Based<int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int FieldedInt() { try { Lib.Fielded<int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int OrderedTouch() { try { Ordered.Touch(); return 0; } catch (System.Exception) { return 1; } }
}
"""

        let names =
            [
                "BothInt"
                "BothString"
                "BothMethodInt"
                "BasedInt"
                "FieldedInt"
                "OrderedTouch"
            ]

        let fileNotFound = "System.IO.FileNotFoundException"

        // `Missing` is not beside the client.
        let hostOutcomes = hostOutcomes [ "Lib", constrained ] clientImage names

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

    /// A program to run on the host and ask the analysis about.
    type private Scenario =
        {
            Name : string
            /// Each library's name, its source for the client to compile against, and its source
            /// to run against, compiled in order, each against those before it.
            Libraries : (string * string * string) list
            /// The client's source, which defines `Client.Cases`.
            Client : string
            /// The methods of `Client.Cases` to run, each taking nothing and returning an `int`.
            Cases : string list
            /// Whether the analysis must know each case's answer.
            Exact : bool
            /// Generic methods to ask the analysis about by themselves, each with the case that calls
            /// it: of `Client.Cases` by name, or of another type of `Client` as `Type::Method`.
            Generics : (string * string) list
        }

    /// Where `scenario` disagrees with the host: a case whose answer names a violation the host
    /// does not raise, or does not name the one it does unless "unknown" where the scenario
    /// allows it; a generic method by
    /// itself whose known answer is not its case's; or an analysis that throws.
    let private disagreements (scenario : Scenario) : string list =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let compileAll (pick : string * string -> string) : (string * byte[]) list =
            ([], scenario.Libraries)
            ||> List.fold (fun built (name, against, runs) ->
                built @ [ name, compile name (List.map snd built) (pick (against, runs)) ]
            )

        let against = compileAll fst
        let runs = compileAll snd
        let clientImage = compileClient (List.map snd against) scenario.Client
        let hostOutcomes = hostOutcomes runs clientImage scenario.Cases

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let libraries = runs |> List.map (fun (name, image) -> name, read name image)

        let analysis =
            analysisOver
                (clientAssembly :: List.map snd libraries)
                (fun loaded ->
                    (loaded, libraries)
                    ||> List.fold (fun loaded (name, library) ->
                        match
                            clientAssembly.AssemblyReferences.Values
                            |> Seq.tryFind (fun r -> r.Name.Name = name)
                        with
                        | Some reference -> fst (loaded.WithBoundReference reference library)
                        | None -> loaded
                    )
                )
            |> ref

        let violations = Set.ofList [ typeLoad ; verification ]

        let raisedBy (case : string) : Set<string> =
            match hostOutcomes.[case] with
            | Outcome.Escapes raised when violations.Contains raised -> Set.singleton raised
            | Outcome.Escapes _
            | Outcome.Runs
            | Outcome.Caught -> Set.empty

        // A method of `Client.Cases` by its name, or of another type of `Client` as `Type::Method`.
        let ask (methodName : string) : Result<Escapes * Set<string>, string> =
            let typeName, memberName =
                match methodName.Split "::" with
                | [| typeName ; memberName |] -> "Client." + typeName, memberName
                | _ -> "Client.Cases", methodName

            try
                let next, escapes =
                    EscapeAnalysis.escapes analysis.Value (methodNamed clientAssembly typeName memberName)

                analysis.Value <- next
                let shown = render next escapes

                Ok (
                    escapes,
                    violations
                    |> Set.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")
                )
            with e ->
                Error $"%s{scenario.Name}.%s{methodName}: the analysis throws %s{e.Message}"

        [
            for case in scenario.Cases do
                match ask case with
                | Error message -> yield message
                | Ok (escapes, named) ->
                    let raised = raisedBy case

                    let agrees =
                        if scenario.Exact then
                            not escapes.Unknown && named = raised
                        else
                            named = raised || (escapes.Unknown && named.IsSubsetOf raised)

                    if not agrees then
                        yield
                            $"%s{scenario.Name}.%s{case}: the host's outcome is %A{hostOutcomes.[case]}; the analysis names %A{Set.toList named}, unknown %b{escapes.Unknown}"

            for generic, case in scenario.Generics do
                match ask generic with
                | Error message -> yield message
                | Ok (escapes, named) ->
                    if not (escapes.Unknown || named = raisedBy case) then
                        yield
                            $"%s{scenario.Name}.%s{generic}, by itself: %A{Set.toList named}; %s{case}'s outcome is %A{hostOutcomes.[case]}"
        ]

    /// Programs whose constraint checks reach a type the analysis follows less directly than most,
    /// against the host: a `calli` in a generic method, whose signature is over the method's own
    /// type variables; a generic argument whose base is gone; a function pointer to a method whose
    /// signature names a struct that is gone, which the JIT does not load; a struct field's or a
    /// parameter's type whose argument carries a custom modifier; an abstract class over a generic
    /// method's variable, against an interface constraint that has gained an abstract static
    /// member, in code shared between instantiations; and a byref-like argument carrying a custom
    /// modifier, of a method whose address is taken and of a type whose token is loaded, which no
    /// longer allow one.
    [<Test>]
    let ``constraint checks of loads the analysis follows less directly agree with the host`` () : unit =
        let scenarios =
            [
                {
                    Name = "Calli"
                    Libraries = []
                    Client =
                        """
namespace Client;
public static unsafe class Cases
{
    static int Id(int x) => x;
    public static T Apply<T>(delegate*<T, T> f, T x) where T : unmanaged => f(x);
    public static int CallsApply() { try { return Apply<int>(&Id, 3) == 3 ? 0 : 1; } catch (System.Exception) { return 1; } }
}
"""
                    Cases = [ "CallsApply" ]
                    Exact = true
                    Generics = [ "Apply", "CallsApply" ]
                }
                {
                    Name = "BaseGone"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public class B { } public class C<T> where T : class { public static void Touch() { } }",
                            "namespace Lib; public class C<T> where T : class { public static void Touch() { } }"
                        ]
                    Client =
                        """
namespace Client;
public class Derived<T> : Lib.B { }
public static class Cases
{
    public static int Generic<T>() { try { Lib.C<Derived<T>>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<int>();
}
"""
                    Cases = [ "CallsGeneric" ]
                    // Constructing the `TypeLoadException` makes virtual calls the analysis does not
                    // follow.
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "SignatureTypeGone"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public struct Gone<T> { } public class Stays { }",
                            "namespace Lib; public class Stays { }"
                        ]
                    Client =
                        """
namespace Client;
public static class Holder<T> { public static void Take(Lib.Gone<T> g) { } }
public static unsafe class Cases
{
    public static int AddressOfTake() { try { delegate*<Lib.Gone<int>, void> p = &Holder<int>.Take; return p == null ? 1 : 0; } catch (System.Exception) { return 1; } }
}
"""
                    Cases = [ "AddressOfTake" ]
                    Exact = true
                    Generics = []
                }
                {
                    Name = "ModifiedFieldArgument"
                    Libraries = []
                    Client =
                        """
namespace Client;
public unsafe struct HoldsPointers { public System.ValueTuple<delegate*<out int, void>[]> F; }
public static class Cases
{
    static void Takes(HoldsPointers h) { }
    public static int PassesHolder() { try { Takes(default); return 0; } catch (System.Exception) { return 1; } }
}
"""
                    Cases = [ "PassesHolder" ]
                    // A type argument carrying a custom modifier, the analysis does not instantiate.
                    Exact = false
                    Generics = []
                }
                {
                    Name = "ModifiedArgument"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public struct Pair<A, B> { }",
                            "namespace Lib; public struct Pair<A, B> where B : struct { }"
                        ]
                    Client =
                        """
namespace Client;
public static unsafe class Cases
{
    static void Takes(Lib.Pair<int, delegate*<out int, void>[]> p) { }
    static void TakesOver<T>(Lib.Pair<T, delegate*<out int, void>[]> p) { }
    public static int PassesPair() { try { Takes(default); return 0; } catch (System.Exception) { return 1; } }
    public static int Generic<T>() { try { TakesOver<T>(default); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<int>();
}
"""
                    Cases = [ "PassesPair" ; "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "AbstractOverVariable"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public interface I<T> { } public static class Methods { public static void Go<U, V>() where U : I<V> { } }",
                            "namespace Lib; public interface I<T> { static abstract void S(); } public static class Methods { public static void Go<U, V>() where U : I<V> { } }"
                        ]
                    Client =
                        """
namespace Client;
public abstract class A<T> : Lib.I<T> { }
public static class Cases
{
    public static void Generic<T>() { Lib.Methods.Go<A<T>, T>(); }
    public static int CallsGeneric() { Generic<string>(); return 0; }
}
"""
                    Cases = [ "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "ModifiedByRefLike"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public static class Methods { public static void M<T>() where T : allows ref struct { } } public class C<T> where T : allows ref struct { public static void Touch() { } }",
                            "namespace Lib; public static class Methods { public static void M<T>() { } } public class C<T> { public static void Touch() { } }"
                        ]
                    Client =
                        """
namespace Client;
public ref struct R<X> { }
public static unsafe class Cases
{
    public static int TakesAddress() { delegate*<void> p = &Lib.Methods.M<R<delegate*<out int, void>[]>>; return p == null ? 1 : 0; }
    public static int TakesToken() { System.Type t = typeof(Lib.C<R<delegate*<out int, void>[]>>); return t == null ? 1 : 0; }
}
"""
                    Cases = [ "TakesAddress" ; "TakesToken" ]
                    Exact = false
                    Generics = []
                }
            ]

        match scenarios |> List.collect disagreements with
        | [] -> ()
        | problems -> problems |> String.concat "\n" |> failwith

    /// Programs whose violated constraint a generic method's own form, or every instantiation of
    /// it, meets only through part of what it names, against the host: a closed argument beside
    /// one over the method's type variable, of a method or of a type; a closed method argument of a
    /// method of a type over the variable; generic structs whose full loads reach each other, one
    /// of them reaching a violation; a callee's return type whose full load reaches one; and the
    /// address of a method instantiated with an argument carrying a custom modifier, which the
    /// analysis does not instantiate, of an unconstrained method and of one the argument violates.
    [<Test>]
    let ``a violation reached through part of what a generic method names agrees with the host`` () : unit =
        let scenarios =
            [
                {
                    Name = "PartlyOpenMethod"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public static class M { public static void Two<A, B>() { } }",
                            "namespace Lib; public static class M { public static void Two<A, B>() where B : class { } }"
                        ]
                    Client =
                        """
namespace Client;
public static class Cases
{
    public static int Generic<T>() { try { Lib.M.Two<T, int>(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<string>();
}
"""
                    Cases = [ "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "PartlyOpenType"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public class P<A, B> { public static void Touch() { } }",
                            "namespace Lib; public class P<A, B> where B : class { public static void Touch() { } }"
                        ]
                    Client =
                        """
namespace Client;
public static class Cases
{
    public static int Generic<T>() { try { Lib.P<T, int>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<string>();
}
"""
                    Cases = [ "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "OpenParentClosedMethod"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public class Q<A> { public static void M<B>() { } }",
                            "namespace Lib; public class Q<A> { public static void M<B>() where B : class { } }"
                        ]
                    Client =
                        """
namespace Client;
public static class Cases
{
    public static int Generic<T>() { try { Lib.Q<T>.M<int>(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<string>();
}
"""
                    Cases = [ "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "ClosedPartThroughCycle"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public struct Bad<T> { }",
                            "namespace Lib; public struct Bad<T> where T : class { }"
                        ]
                    Client =
                        """
namespace Client;
public struct SA<T> { public static SB<T> F; public static Lib.Bad<int> G; }
public struct SB<T> { public static SA<T> F; }
public static class Cases
{
    public static void Keep(object o) { }
    public static int GenericA<T>() { try { Keep(typeof(SA<T>)); return 0; } catch (System.Exception) { return 1; } }
    public static int GenericB<T>() { try { Keep(typeof(SB<T>)); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsA() => GenericA<int>();
    public static int CallsB() => GenericB<int>();
}
"""
                    Cases = [ "CallsA" ; "CallsB" ]
                    Exact = false
                    Generics = [ "GenericA", "CallsA" ; "GenericB", "CallsB" ]
                }
                {
                    Name = "ParameterThroughCycle"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public struct W<T> { }",
                            "namespace Lib; public struct W<T> where T : class { }"
                        ]
                    Client =
                        """
namespace Client;
public struct SX<T> { public static SY<T> F; public static Lib.W<T> G; }
public struct SY<T> { public static SX<T> F; }
public static class Cases
{
    public static void Keep(object o) { }
    public static int GenericX<T>() { try { Keep(typeof(SX<T>)); return 0; } catch (System.Exception) { return 1; } }
    public static int GenericY<T>() { try { Keep(typeof(SY<T>)); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsX() => GenericX<int>();
    public static int CallsY() => GenericY<int>();
}
"""
                    Cases = [ "CallsX" ; "CallsY" ]
                    Exact = false
                    Generics = [ "GenericX", "CallsX" ; "GenericY", "CallsY" ]
                }
                {
                    Name = "ClosedPartOfReturn"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public struct Bad<T> { }",
                            "namespace Lib; public struct Bad<T> where T : class { }"
                        ]
                    Client =
                        """
namespace Client;
public struct Holds<T> { public Lib.Bad<int> F; }
public static class Helper { public static Holds<T> Make<T>() => default; }
public static class Cases
{
    public static int Generic<T>() { try { Helper.Make<T>(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<string>();
}
"""
                    Cases = [ "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "SubstitutedClosedPart"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public struct S<T> { }",
                            "namespace Lib; public struct S<T> where T : class { }"
                        ]
                    Client =
                        """
namespace Client;
public static class Helper<A> { public static Lib.S<A> Make<B>() => default; }
public static class Cases
{
    public static int Generic<T>() { try { Helper<int>.Make<T>(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<string>();
}
"""
                    Cases = [ "CallsGeneric" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ]
                }
                {
                    Name = "RejectedCallee"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public class C<T> { public static void Touch() { } } public static class M { public static void Run<T>() { C<T>.Touch(); } }",
                            "namespace Lib; public class C<T> where T : class { public static void Touch() { } } public static class M { public static void Run<T>() where T : class { C<T>.Touch(); } }"
                        ]
                    Client =
                        """
namespace Client;
public static class Cases
{
    public static int CallsRun() { Lib.M.Run<int>(); return 0; }
}
"""
                    Cases = [ "CallsRun" ]
                    Exact = false
                    Generics = []
                }
                {
                    Name = "ArrayAccessor"
                    Libraries = []
                    Client =
                        """
namespace Client;
public static class Cases
{
    public static T Get<T>(T[,] a) => a[0, 0];
    public static void Set<T>(T[,] a, T v) { a[0, 0] = v; }
    public static int CallsGet() { try { return Get<int>(new int[1, 1]); } catch (System.Exception) { return 1; } }
    public static int CallsSet() { try { Set<int>(new int[1, 1], 0); return 0; } catch (System.Exception) { return 1; } }
}
"""
                    Cases = [ "CallsGet" ; "CallsSet" ]
                    Exact = false
                    Generics = [ "Get", "CallsGet" ; "Set", "CallsSet" ]
                }
                {
                    Name = "OpaqueArgument"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public class C<U> { public static void Touch() { } } public static class M { public static void Run<U>() { } }",
                            "namespace Lib; public class C<U> where U : struct { public static void Touch() { } } public static class M { public static void Run<U>() where U : struct { } }"
                        ]
                    Client =
                        """
namespace Client;
public static unsafe class Cases
{
    public static int Generic<T>() { try { Lib.C<delegate*<T, void>[]>.Touch(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGeneric() => Generic<int>();
    public static int GenericMethod<T>() { try { Lib.M.Run<delegate*<T, void>[]>(); return 0; } catch (System.Exception) { return 1; } }
    public static int CallsGenericMethod() => GenericMethod<int>();
}
"""
                    Cases = [ "CallsGeneric" ; "CallsGenericMethod" ]
                    Exact = false
                    Generics = [ "Generic", "CallsGeneric" ; "GenericMethod", "CallsGenericMethod" ]
                }
                {
                    // Enough fields that the client's member references outnumber the library's.
                    Name = "ForeignFields"
                    Libraries =
                        let fields =
                            [ 0..30 ]
                            |> List.map (fun index -> $"public static System.ValueTuple<int> F%d{index};")
                            |> String.concat " "

                        let library = "namespace Lib; public static class Fields { " + fields + " }"
                        [ "Lib", library, library ]
                    Client =
                        let reads =
                            [ 0..30 ]
                            |> List.map (fun index -> $"Lib.Fields.F%d{index}.Item1")
                            |> String.concat " + "

                        """
namespace Client;
public static class Cases
{
    public static int ReadsFields() { try { return ("""
                        + reads
                        + """) * 0; } catch (System.Exception) { return 1; } }
}
"""
                    Cases = [ "ReadsFields" ]
                    Exact = false
                    Generics = []
                }
                {
                    Name = "ModifiedMethodArgument"
                    Libraries =
                        [
                            "Lib",
                            "namespace Lib; public static class Methods { public static void M<T>() { } }",
                            "namespace Lib; public static class Methods { public static void M<T>() where T : struct { } }"
                        ]
                    Client =
                        """
namespace Client;
public static unsafe class Cases
{
    static void M<T>() { }
    public static int TakesAddress()
    {
        try { delegate*<void> p = &M<System.Collections.Generic.List<delegate*<out int, void>[]>>; return p == null ? 1 : 0; }
        catch (System.Exception) { return 1; }
    }
    public static int TakesConstrainedAddress()
    {
        try { delegate*<void> p = &Lib.Methods.M<System.Collections.Generic.List<delegate*<out int, void>[]>>; return p == null ? 1 : 0; }
        catch (System.Exception) { return 1; }
    }
}
"""
                    Cases = [ "TakesAddress" ; "TakesConstrainedAddress" ]
                    Exact = false
                    Generics = []
                }
            ]

        match scenarios |> List.collect disagreements with
        | [] -> ()
        | problems -> problems |> String.concat "\n" |> failwith

    /// A method of a generic type reading the type's own field, of a struct over the type's parameter, through
    /// the field's definition token rather than a reference to it on the type's instantiation over
    /// its parameters: IL that C# does not write, but that CoreCLR runs, reading the field of the
    /// instantiation the method runs in. The library is emitted, the client compiled against it.
    [<Test>]
    let ``reading a generic type's own field by its definition token agrees with the host`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let libraryImage =
            let builder =
                Reflection.Emit.PersistedAssemblyBuilder (Reflection.AssemblyName "Lib", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Lib"

            let holder =
                modul.DefineType ("Holder`1", Reflection.TypeAttributes.Public ||| Reflection.TypeAttributes.Class)

            let parameter = (holder.DefineGenericParameters [| "T" |]).[0]

            // `ValueTuple<T>`, a value type, which the JIT loads in full reading the field.
            let fieldType = typedefof<ValueTuple<int>>.MakeGenericType [| parameter :> Type |]

            holder.DefineField ("F", fieldType, Reflection.FieldAttributes.Public)
            |> ignore<Reflection.Emit.FieldBuilder>

            holder.DefineDefaultConstructor Reflection.MethodAttributes.Public
            |> ignore<Reflection.Emit.ConstructorBuilder>

            let read =
                holder.DefineMethod ("Read", Reflection.MethodAttributes.Public, fieldType, Type.EmptyTypes)

            let il = read.GetILGenerator ()
            il.Emit Reflection.Emit.OpCodes.Ldarg_0
            // The field's definition token, the module's first field, spelled out: given the
            // field's builder, the emitter names its declaring type's instantiation instead.
            il.Emit (Reflection.Emit.OpCodes.Ldfld, 0x04000001)
            il.Emit Reflection.Emit.OpCodes.Ret

            holder.CreateType () |> ignore<Type>
            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let clientImage =
            compileClient
                [ libraryImage ]
                """
namespace Client;
public static class Cases
{
    public static int CallsRead() { try { return new Holder<int>().Read().Item1; } catch (System.Exception) { return 1; } }
}
"""

        let hostOutcomes = hostOutcomes [ "Lib", libraryImage ] clientImage [ "CallsRead" ]
        Assert.That (hostOutcomes.["CallsRead"], Is.EqualTo Outcome.Runs)

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let libraryAssembly = read "Lib" libraryImage

        let analysis =
            analysisOver
                [ clientAssembly ; libraryAssembly ]
                (fun loaded ->
                    let reference =
                        clientAssembly.AssemblyReferences.Values
                        |> Seq.find (fun r -> r.Name.Name = "Lib")

                    fst (loaded.WithBoundReference reference libraryAssembly)
                )

        let violations = Set.ofList [ typeLoad ; verification ]

        let named (analysis : EscapeAnalysisState) (escapes : Escapes) : Set<string> =
            let shown = render analysis escapes
            violations |> Set.filter (fun name -> shown.Contains $"=%s{name}")

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Cases" "CallsRead")

        Assert.That (named analysis escapes, Is.Empty)

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed libraryAssembly "Holder`1" "Read")

        Assert.That (named analysis escapes, Is.Empty)

    /// IL that C# does not write, which CoreCLR compiles, against the host. A method of a generic
    /// type calls the type's own generic methods through method specifications over their
    /// definition tokens rather than references to them on the type's instantiation: CoreCLR takes
    /// a definition token to name the method on the type's typical instantiation, whose parameter
    /// an argument constrained to it does not satisfy, and whose own parameter a signature names.
    /// And a generic method instantiates another with a pointer to its type variable, which CoreCLR
    /// admits in no instantiation, rejecting it compiling the caller even in code shared between
    /// instantiations. The library is emitted, the client compiled against it.
    [<Test>]
    let ``generic methods instantiated as C# does not write agree with the host`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()
        let badImage = "System.BadImageFormatException"

        let libraryImage =
            let builder =
                Reflection.Emit.PersistedAssemblyBuilder (Reflection.AssemblyName "Lib", typeof<obj>.Assembly)

            let modul = builder.DefineDynamicModule "Lib"

            let holder =
                modul.DefineType ("Holder`1", Reflection.TypeAttributes.Public ||| Reflection.TypeAttributes.Class)

            let parameter = (holder.DefineGenericParameters [| "T" |]).[0]

            holder.DefineDefaultConstructor Reflection.MethodAttributes.Public
            |> ignore<Reflection.Emit.ConstructorBuilder>

            // `static void M<U>() where U : T`.
            let constrained =
                holder.DefineMethod (
                    "M",
                    Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static,
                    typeof<Void>,
                    Type.EmptyTypes
                )

            let methodParameter = (constrained.DefineGenericParameters [| "U" |]).[0]
            methodParameter.SetBaseTypeConstraint (parameter :> Type)
            constrained.GetILGenerator().Emit Reflection.Emit.OpCodes.Ret

            // `static ValueTuple<T> Make<U>()`, a value type over the type's parameter, which the JIT
            // loads in full compiling a call.
            let made = typedefof<ValueTuple<int>>.MakeGenericType [| parameter :> Type |]

            let make =
                holder.DefineMethod (
                    "Make",
                    Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static,
                    made,
                    Type.EmptyTypes
                )

            make.DefineGenericParameters [| "U" |]
            |> ignore<Reflection.Emit.GenericTypeParameterBuilder[]>

            let makeIl = make.GetILGenerator ()
            let result = makeIl.DeclareLocal made
            makeIl.Emit (Reflection.Emit.OpCodes.Ldloca_S, result)
            makeIl.Emit (Reflection.Emit.OpCodes.Initobj, made)
            makeIl.Emit (Reflection.Emit.OpCodes.Ldloc, result)
            makeIl.Emit Reflection.Emit.OpCodes.Ret

            // Each instance method calls one of those with the type's parameter.
            for name, callee, returns in [ "Run", constrained, false ; "RunMake", make, true ] do
                let run =
                    holder.DefineMethod (name, Reflection.MethodAttributes.Public, typeof<Void>, Type.EmptyTypes)

                let il = run.GetILGenerator ()
                il.Emit (Reflection.Emit.OpCodes.Call, callee.MakeGenericMethod [| parameter :> Type |])

                if returns then
                    il.Emit Reflection.Emit.OpCodes.Pop

                il.Emit Reflection.Emit.OpCodes.Ret

            holder.CreateType () |> ignore<Type>

            let plain =
                modul.DefineType (
                    "Plain",
                    Reflection.TypeAttributes.Public
                    ||| Reflection.TypeAttributes.Abstract
                    ||| Reflection.TypeAttributes.Sealed
                )

            let unconstrained =
                plain.DefineMethod (
                    "P",
                    Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static,
                    typeof<Void>,
                    Type.EmptyTypes
                )

            unconstrained.DefineGenericParameters [| "V" |]
            |> ignore<Reflection.Emit.GenericTypeParameterBuilder[]>

            unconstrained.GetILGenerator().Emit Reflection.Emit.OpCodes.Ret

            // `static void G<T>() { P<T*>(); }`.
            let pointing =
                plain.DefineMethod (
                    "G",
                    Reflection.MethodAttributes.Public ||| Reflection.MethodAttributes.Static,
                    typeof<Void>,
                    Type.EmptyTypes
                )

            let variable = (pointing.DefineGenericParameters [| "T" |]).[0]
            let pointingIl = pointing.GetILGenerator ()

            pointingIl.Emit (
                Reflection.Emit.OpCodes.Call,
                unconstrained.MakeGenericMethod [| variable.MakePointerType () |]
            )

            pointingIl.Emit Reflection.Emit.OpCodes.Ret
            plain.CreateType () |> ignore<Type>

            use stream = new MemoryStream ()
            builder.Save stream
            stream.ToArray ()

        let clientImage =
            compileClient
                [ libraryImage ]
                """
namespace Client;
public static class Cases
{
    public static int CallsRun() { new Holder<int>().Run(); return 0; }
    public static int CallsRunMake() { new Holder<int>().RunMake(); return 0; }
    public static int CallsPointing() { Plain.G<string>(); return 0; }
}
"""

        let names = [ "CallsRun" ; "CallsRunMake" ; "CallsPointing" ]
        let hostOutcomes = hostOutcomes [ "Lib", libraryImage ] clientImage names
        Assert.That (hostOutcomes.["CallsRun"], Is.EqualTo (Outcome.Escapes verification))
        Assert.That (hostOutcomes.["CallsPointing"], Is.EqualTo (Outcome.Escapes badImage))

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let libraryAssembly = read "Lib" libraryImage

        // The emitter spells the calls as the test means it to.
        let byDefinition =
            libraryAssembly.MethodSpecs.Values
            |> Seq.filter (fun spec ->
                match spec.Method with
                | MetadataToken.MethodDef _ -> true
                | _ -> false
            )
            |> Seq.length

        Assert.That (byDefinition, Is.EqualTo 3)

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
                // Each instance, through its caller, and each definition for every instantiation:
                // each names what the host raises, or is unknown, naming nothing else.
                for case, definitionType, definition in
                    [
                        "CallsRun", "Holder`1", "Run"
                        "CallsRunMake", "Holder`1", "RunMake"
                        "CallsPointing", "Plain", "G"
                    ] do
                    let raised =
                        match hostOutcomes.[case] with
                        | Outcome.Escapes raised -> Set.singleton raised
                        | Outcome.Runs
                        | Outcome.Caught -> Set.empty

                    for name, key in
                        [
                            case, methodNamed clientAssembly "Client.Cases" case
                            $"%s{definitionType}::%s{definition}", methodNamed libraryAssembly definitionType definition
                        ] do
                        let named =
                            try
                                let next, escapes = EscapeAnalysis.escapes analysis key
                                analysis <- next
                                let shown = render next escapes

                                let named =
                                    Set.ofList [ typeLoad ; verification ; badImage ]
                                    |> Set.filter (fun exceptionName -> shown.Contains $"=%s{exceptionName}")

                                if named = raised || (escapes.Unknown && named.IsSubsetOf raised) then
                                    None
                                else
                                    Some $"%s{name}: %A{Set.toList named}, unknown %b{escapes.Unknown}"
                            with e ->
                                Some $"%s{name}: the analysis throws %s{e.Message}"

                        match named with
                        | Some failure -> yield $"%s{failure}; %s{case}'s outcome is %A{hostOutcomes.[case]}"
                        | None -> ()
            ]

        if not failures.IsEmpty then
            failures |> String.concat "\n" |> failwith

    /// A method, inlined into its caller, taking the address of a method whose signature an update
    /// has made violate a constraint. CoreCLR prepares the method as a function pointer where the
    /// JIT asks for the address, compiling the caller it inlines the method into, so the caller's
    /// own handlers do not catch what that raises. The real runtime under test runs it, as its own
    /// process, in a collectible load context, whose code CoreCLR compiles fully optimised from the
    /// first call.
    [<Test>]
    let ``a violation met preparing an address raises where the method taking it is inlined`` () : unit =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let library (constrained : bool) : string =
            if constrained then
                "namespace Lib; public struct S<T> where T : class { }"
            else
                "namespace Lib; public struct S<T> { }"

        let client =
            """
using System.Runtime.CompilerServices;
namespace Client;
public static class Target<T> { public static void Take(Lib.S<T> s) { } }
public static unsafe class Takes
{
    [MethodImpl(MethodImplOptions.NoInlining)]
    public static void Use(void* p) { }
    [MethodImpl(MethodImplOptions.AggressiveInlining)]
    public static void* InlinedAddress() => (void*)(delegate*<Lib.S<int>, void>)&Target<int>.Take;
}
public static class Cases
{
    public static string CallsInlinedAddress()
    {
        try { unsafe { Takes.Use(Takes.InlinedAddress()); } return null; }
        catch (System.Exception e) { return e.GetType().FullName; }
    }
}
"""

        // Optimised, so that the JIT inlines: a Debug build disables its optimisation.
        let compileOptimised (name : string) (references : byte[] list) (text : string) : byte[] =
            Roslyn.compileOptimizedAssemblyWithReferences
                name
                OutputKind.DynamicallyLinkedLibrary
                (references
                 |> List.map (fun image ->
                     MetadataReference.CreateFromImage (ImmutableArray.CreateRange image) :> MetadataReference
                 ))
                [ text ]

        let constrained = compileOptimised "Lib" [] (library true)
        let unconstrained = compileOptimised "Lib" [] (library false)
        let clientImage = compileOptimised "Client" [ unconstrained ] client

        // Loads the client from the directory its first argument names into a collectible load
        // context, runs `CallsInlinedAddress`, and writes to its second argument's file `Runs`, or
        // `Escapes:` and the full name of what escaped.
        let driver =
            Roslyn.compileAssembly
                "Driver"
                OutputKind.ConsoleApplication
                []
                [
                    """
namespace Driver;
public static class Program
{
    public static void Main(string[] args)
    {
        var directory = args[0];
        var context = new System.Runtime.Loader.AssemblyLoadContext("Probe", isCollectible: true);
        context.Resolving += (c, name) =>
            name.Name == "Lib" ? c.LoadFromAssemblyPath(System.IO.Path.Combine(directory, "Lib.dll")) : null;
        var client = context.LoadFromAssemblyPath(System.IO.Path.Combine(directory, "Client.dll"));
        string outcome;
        try { client.GetType("Client.Cases").GetMethod("CallsInlinedAddress").Invoke(null, null); outcome = "Runs"; }
        catch (System.Reflection.TargetInvocationException e) { outcome = "Escapes:" + e.InnerException.GetType().FullName; }
        System.IO.File.WriteAllText(args[1], outcome);
    }
}
"""
                ]

        // What running `CallsInlinedAddress` does on the framework under test's real runtime, as its
        // own process.
        let hostOutcome : string =
            let directory =
                Path.Combine (Path.GetTempPath (), "pawprint-inlined-address-" + Path.GetRandomFileName ())

            let probe = Path.Combine (directory, "probe")
            Directory.CreateDirectory probe |> ignore<DirectoryInfo>

            try
                File.WriteAllBytes (Path.Combine (probe, "Lib.dll"), constrained)
                File.WriteAllBytes (Path.Combine (probe, "Client.dll"), clientImage)
                let driverPath = Path.Combine (directory, "Driver.dll")
                File.WriteAllBytes (driverPath, driver)
                let outcome = Path.Combine (directory, "outcome.txt")

                match RealRuntime.executeAssemblyInPlace [| probe ; outcome |] driverPath with
                | RealRuntimeResult.NormalExit 0 -> File.ReadAllText outcome
                | other -> failwith $"the driver ended %A{other}"
            finally
                RealRuntime.deleteScratchTree directory

        Assert.That (hostOutcome, Is.EqualTo $"Escapes:%s{typeLoad}")

        let read (name : string) (image : byte[]) : DumpedAssembly =
            Assembly.read loggerFactory (Some $"%s{name}.dll") (new MemoryStream (image))

        let clientAssembly = read "Client" clientImage
        let libraryAssembly = read "Lib" constrained

        let analysis =
            analysisOver
                [ clientAssembly ; libraryAssembly ]
                (fun loaded ->
                    let reference =
                        clientAssembly.AssemblyReferences.Values
                        |> Seq.find (fun r -> r.Name.Name = "Lib")

                    fst (loaded.WithBoundReference reference libraryAssembly)
                )

        let analysis, escapes =
            EscapeAnalysis.escapes analysis (methodNamed clientAssembly "Client.Cases" "CallsInlinedAddress")

        Assert.That (render analysis escapes, Does.Contain $"=%s{typeLoad}")
