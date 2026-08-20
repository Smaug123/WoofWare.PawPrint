namespace WoofWare.PawPrint.Test

open System
open System.Collections.Immutable
open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// Slot content for a MethodImpl declaration that crosses an assembly boundary, which is the shape
/// a single-assembly corpus structurally cannot reach.
///
/// Two defects lived behind exactly this gap, both of them in comparisons that are only wrong across
/// token spaces:
///
/// An ancestor's instantiation was compared by its *spelling*, and a spelling carries the assembly
/// that wrote it. `BG : AG&lt;int&gt;` in the library records `Spelled(Lib, int32)` while
/// `.override AG&lt;int&gt;::M` in the application records `Spelled(App, int32)` -- structurally different
/// values naming one instantiation, so the comparison rejected a MethodTable the CLR builds happily.
///
/// And the declaration was matched against candidates by structural equality of signatures. A nominal
/// type in the signature decodes as `FromDefinition` from the library's MethodDef and as
/// `FromReference` from the application's MemberRef, so a legal match was missed and the walk refused.
///
/// Both are ordinary C#: a covariant-return override in one assembly of a generic base in another
/// emits a MethodImpl whose declaration is a MemberRef on a TypeSpec, whose signature mentions a
/// foreign nominal type. No hand-emitted IL is needed to reach it -- only a second assembly.
[<TestFixture>]
module TestCrossAssemblySlotContent =

    /// `AG<T>` declares `M`, `BG` closes it at `int` and declares nothing, so a covariant override in
    /// another assembly names `AG<int>::M` -- a *grandparent*, across the boundary, with a return type
    /// and a parameter type both foreign to the overriding assembly.
    let private librarySource : string =
        """
public class R0 { public virtual int Tag() { return 10; } }
public class R1 : R0 { public override int Tag() { return 11; } }
public class Arg { }

public class AG<T>
{
    public virtual R0 M(T x) { return new R0(); }
    // A second `M` of a different arity that nothing calls, so that resolving the declaration has to
    // compare signatures and not merely names.
    public virtual R0 M(T x, int y) { return new R0(); }
}

public class BG : AG<Arg> { }
"""

    /// `CG.M` is `newslot` plus `.override AG<Arg>::M` plus `[PreserveBaseOverrides]`, because its
    /// return type narrows. `DG` then overrides it the ordinary way, so the slot `AG<Arg>::M` owns must
    /// end up holding `DG.M` -- which is the unification pass reaching across the boundary.
    let private applicationSource : string =
        """
public class CG : BG { public override R1 M(Arg x) { return new R1(); } }
public class DG : CG { public override R1 M(Arg x) { return new R1(); } }

public static class Driver
{
    public static int Main(string[] args)
    {
        DG d = new DG();
        switch (args[0])
        {
            case "AG": return ((AG<Arg>)d).M(new Arg()).Tag();
            case "BG": return ((BG)d).M(new Arg()).Tag();
            case "CG": return ((CG)d).M(new Arg()).Tag();
            case "DG": return d.M(new Arg()).Tag();
        }
        return 99;
    }
}
"""

    let private images : Map<string, byte[]> =
        CrossAssemblyHarness.compileAssemblies
            [
                CrossAssemblySpec.library "SlotLib" [] [ librarySource ]
                CrossAssemblySpec.entryPoint "SlotApp" [ "SlotLib" ] [ applicationSource ]
            ]

    // Undisposed on purpose, as in the sibling layout fixtures: the DumpedAssembly's logger closes
    // over its sinks, and disposing while the assembly is live would drop events.
    let private corelib : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory (typeof<obj>.Assembly.Location)

    let private readImage (name : string) : DumpedAssembly =
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.read loggerFactory None (new MemoryStream (images.[name]))

    let private library : DumpedAssembly = readImage "SlotLib"
    let private application : DumpedAssembly = readImage "SlotApp"

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies =
        LoadedAssemblies.ofAssemblies [ corelib ; library ; application ]

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private loggerFactory = snd (LoggerFactory.makeTest ())

    let private state () : IlMachineState =
        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
            _LoadedAssemblies = loaded
        }

    let private identityOf (name : string) : ResolvedTypeIdentity =
        let assembly = if name = "CG" || name = "DG" then application else library

        match assembly.TryGetTopLevelTypeDef "" (if name = "AG" then "AG`1" else name) with
        | None -> failwith $"neither image declares %s{name}"
        | Some typeInfo -> ResolvedTypeIdentity.ofDefinitionInAssembly typeInfo.AssemblyFullName typeInfo.TypeDefHandle

    /// Which body the real runtime reaches through each spelling. `R0.Tag` returns 10 and `R1.Tag`
    /// returns 11, so the exit code says which return type came back -- and therefore which body ran.
    let private hostTags : Map<string, int> =
        let tempDir = Path.Combine (Path.GetTempPath (), Path.GetRandomFileName ())
        Directory.CreateDirectory tempDir |> ignore<DirectoryInfo>

        try
            images
            |> Map.iter (fun name bytes -> File.WriteAllBytes (Path.Combine (tempDir, name + ".dll"), bytes))

            let entryPath = Path.Combine (tempDir, "SlotApp.dll")

            [ "AG" ; "BG" ; "CG" ; "DG" ]
            |> List.map (fun spelling ->
                match RealRuntime.executeAssemblyInPlace [| spelling |] entryPath with
                | RealRuntimeResult.NormalExit code -> spelling, code
                | other -> failwith $"real runtime did not exit normally for %s{spelling}: %O{other}"
            )
            |> Map.ofList
        finally
            try
                if Directory.Exists tempDir then
                    Directory.Delete (tempDir, true)
            with
            | :? IOException
            | :? UnauthorizedAccessException -> ()

    let private spellings : string list = [ "AG" ; "BG" ; "CG" ; "DG" ]

    [<TestCaseSource(nameof spellings)>]
    let ``a cross-assembly declaration's slot holds the body the real runtime dispatches to``
        (spelling : string)
        : unit
        =
        let hostTag = hostTags.[spelling]

        // Not vacuous, and not self-fulfilling: 10 is `R0.Tag` and 11 is `R1.Tag`, so the host must
        // have reached one of the two return types rather than the fallthrough.
        [ 10 ; 11 ] |> shouldContain hostTag

        let state = state ()

        let state, placed =
            VirtualSlotLayout.placedSlotsOfDefinition loggerFactory bct "test" state (identityOf "DG")

        let _, content =
            VirtualSlotLayout.contentVtableOfDefinition loggerFactory bct "test" state (identityOf "DG")

        // `BG` declares no `M` of its own -- it only closes `AG<T>` at `Arg` -- so it owns no slot, and
        // a call spelled through it goes through the slot `AG<Arg>::M` owns. Keeping the spelling in
        // the driver is still worth it: it checks that the *host* routes it there too.
        let declaration = identityOf (if spelling = "BG" then "AG" else spelling)

        let slot =
            placed
            |> List.filter (fun (candidate, _) ->
                candidate.DeclaredBy.Identity = declaration
                && candidate.Method.Name = "M"
                // `AG`1` declares two `M`s; the two-parameter one is the overload nothing calls.
                && candidate.Method.Signature.RequiredParameterCount = 1
            )
            |> function
                | [ (_, index) ] -> index
                | [] -> failwith $"PawPrint placed no one-parameter `M` for the declaration on %s{spelling}"
                | several ->
                    let indices = several |> List.map (snd >> string) |> String.concat ", "
                    failwith $"PawPrint placed %s{spelling}'s `M` at more than one slot: %s{indices}"

        let occupant =
            match List.tryItem slot content with
            | Some occupant -> occupant
            | None ->
                failwith
                    $"PawPrint says %s{spelling}::M owns slot %i{slot}, but DG's content table has only %i{List.length content} slots"

        // The tag identifies the *return type* the host saw, and each body returns its declaring
        // type's matching one: `AG`1.M` returns `R0` (10) and every override returns `R1` (11). So the
        // occupant agrees with the host exactly when a body that returns `R1` occupies the slot.
        let occupantReturnsR1 = occupant.DeclaredBy.Identity <> identityOf "AG"

        let pawPrintTag = if occupantReturnsR1 then 11 else 10

        pawPrintTag |> shouldEqual hostTag

    [<Test>]
    let ``the cross-assembly declaration really is a foreign MemberRef on a TypeSpec`` () : unit =
        // If Roslyn stopped emitting the shape -- naming `BG` instead of `AG<Arg>`, say, or emitting no
        // MethodImpl at all -- every case above would still pass while testing nothing of what this
        // fixture is for. So assert the metadata directly.
        let cg =
            match application.TryGetTopLevelTypeDef "" "CG" with
            | None -> failwith "the application image does not declare CG"
            | Some typeInfo -> typeInfo

        let declarations =
            cg.MethodImpls.Values |> Seq.map (fun impl -> impl.Declaration) |> List.ofSeq

        // Exactly one MethodImpl, and its declaration is a MemberRef -- not a MethodDef, which is what
        // a same-assembly declaration would give.
        match declarations with
        | [ MetadataToken.MemberReference handle ] ->
            let memberRef = application.Members.[handle]
            memberRef.PrettyName |> shouldEqual "M"

            // And its parent is a TypeSpec, which is the only way to name a generic instantiation.
            match memberRef.Parent with
            | MetadataToken.TypeSpecification _ -> ()
            | other -> failwith $"CG's MethodImpl declaration names its parent with %O{other}, not a TypeSpec"
        | other ->
            let described = other |> List.map (fun token -> $"%O{token}") |> String.concat ", "

            failwith
                $"expected CG to carry exactly one MethodImpl whose declaration is a MemberRef; got [%s{described}]"
