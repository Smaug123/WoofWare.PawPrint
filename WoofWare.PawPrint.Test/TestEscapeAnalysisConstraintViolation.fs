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
                            | _, Some _ ->
                                if not escapes.Unknown then
                                    knownWithin <- Set.add case.Position knownWithin

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
            compileClient
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
            /// Generic methods of `Client.Cases` to ask the analysis about by themselves, each with
            /// the case that calls it.
            Generics : (string * string) list
        }

    /// Where `scenario` disagrees with the host: a case whose answer names a violation the host
    /// does not raise, or does not name the one it does unless "unknown"; a generic method by
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

        let ask (methodName : string) : Result<Escapes * Set<string>, string> =
            try
                let next, escapes =
                    EscapeAnalysis.escapes analysis.Value (methodNamed clientAssembly "Client.Cases" methodName)

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

                    if not (named = raised || (escapes.Unknown && named.IsSubsetOf raised)) then
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
    /// signature names a struct that is gone, which the JIT does not load; and a struct field's or
    /// a parameter's type whose argument carries a custom modifier.
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
                    Generics = [ "Generic", "CallsGeneric" ]
                }
            ]

        match scenarios |> List.collect disagreements with
        | [] -> ()
        | problems -> problems |> String.concat "\n" |> failwith
