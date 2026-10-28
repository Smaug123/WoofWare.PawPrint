namespace WoofWare.PawPrint.Test

open System.IO
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `SynchronizedMethodHelpers` against each runtime's own source and CoreLib: the methods
/// `jithelpers.h` binds the JIT's monitor helpers to, through `corelib.h`, and their definitions.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestSynchronizedMethodHelpers =

    /// What an enclosing preprocessor arm makes of the lines in it, on a target with funclets.
    [<RequireQualifiedAccess>]
    type private Arm =
        /// An arm of `FEATURE_EH_FUNCLETS`: whether its lines are compiled.
        | Funclets of bool
        /// An arm of a condition this reader does not evaluate.
        | Unevaluated

    /// The `METHOD__*` binder `jithelpers.h` gives the dynamic helper `helper` on `runtime`, on a
    /// target with funclets, which every `JitTarget` is. Fails where a row for `helper` is under a
    /// condition other than `FEATURE_EH_FUNCLETS`.
    let private binderOf (runtime : EmulatedRuntime) (helper : string) : string =
        let jithelpers =
            Path.Combine (FrameworkUnderTest.requireRuntimeSourceOf runtime, "src", "coreclr", "inc", "jithelpers.h")

        let funclets (condition : string) : bool =
            condition = "defined(FEATURE_EH_FUNCLETS)" || condition = "FEATURE_EH_FUNCLETS"

        // Each row for `helper`, with the arms around it, innermost first.
        let _, rowsWithArms =
            ((([] : Arm list), ([] : (string * Arm list) list)), File.ReadAllLines jithelpers)
            ||> Array.fold (fun (arms, found) line ->
                let line = line.Trim ()

                if line.StartsWith "#ifdef " then
                    let arm =
                        if funclets (line.Substring "#ifdef ".Length) then
                            Arm.Funclets true
                        else
                            Arm.Unevaluated

                    arm :: arms, found
                elif line.StartsWith "#if " then
                    let arm =
                        if funclets (line.Substring "#if ".Length) then
                            Arm.Funclets true
                        else
                            Arm.Unevaluated

                    arm :: arms, found
                elif line.StartsWith "#ifndef " then
                    let arm =
                        if funclets (line.Substring "#ifndef ".Length) then
                            Arm.Funclets false
                        else
                            Arm.Unevaluated

                    arm :: arms, found
                elif line.StartsWith "#elif" then
                    match arms with
                    | _ :: outer -> Arm.Unevaluated :: outer, found
                    | [] -> failwith $"jithelpers.h has an #elif outside any #if: %s{line}"
                elif line.StartsWith "#else" then
                    match arms with
                    | Arm.Funclets compiled :: outer -> Arm.Funclets (not compiled) :: outer, found
                    | Arm.Unevaluated :: outer -> Arm.Unevaluated :: outer, found
                    | [] -> failwith $"jithelpers.h has an #else outside any #if: %s{line}"
                elif line.StartsWith "#endif" then
                    match arms with
                    | _ :: outer -> outer, found
                    | [] -> failwith $"jithelpers.h has an #endif outside any #if: %s{line}"
                elif line.StartsWith $"DYNAMICJITHELPER(%s{helper}," then
                    arms, (line, arms) :: found
                else
                    arms, found
            )

        let rows =
            rowsWithArms
            |> List.filter (fun (row, arms) ->
                arms
                |> List.forall (fun arm ->
                    match arm with
                    | Arm.Funclets compiled -> compiled
                    | Arm.Unevaluated ->
                        failwith $"jithelpers.h's row %s{row} is under a condition this test does not evaluate"
                )
            )
            |> List.map fst

        match rows with
        | [ row ] ->
            match row.Substring(row.IndexOf '(' + 1).TrimEnd(')').Split ',' |> Array.map _.Trim() with
            | [| _ ; "NULL" ; binder |] -> binder
            | _ -> failwith $"jithelpers.h's %s{helper} row is not of a shape this test reads: %s{row}"
        | found -> failwith $"jithelpers.h has %d{found.Length} DYNAMICJITHELPER rows for %s{helper}"

    /// The name and signature `corelib.h` gives the `MONITOR` method `binder` names on `runtime`.
    let private monitorMethodOf (runtime : EmulatedRuntime) (binder : string) : string * string =
        let corelibH =
            Path.Combine (FrameworkUnderTest.requireRuntimeSourceOf runtime, "src", "coreclr", "vm", "corelib.h")

        let prefix = "METHOD__MONITOR__"

        if not (binder.StartsWith prefix) then
            failwith $"jithelpers.h binds a monitor helper to %s{binder}, which is not a MONITOR method"

        let rows =
            File.ReadAllLines corelibH
            |> Array.map _.Trim()
            |> Array.filter _.StartsWith("DEFINE_METHOD(MONITOR,")
            |> Array.choose (fun row ->
                match row.Substring(row.IndexOf '(' + 1).TrimEnd(')').Split ',' |> Array.map _.Trim() with
                | [| _ ; id ; name ; signature |] when id = binder.Substring prefix.Length -> Some (name, signature)
                | [| _ ; _ ; _ ; _ |] -> None
                | _ -> failwith $"corelib.h's MONITOR method row is not of a shape this test reads: %s{row}"
            )
            |> List.ofArray

        match rows with
        | [ found ] -> found
        | found -> failwith $"corelib.h has %d{found.Length} MONITOR rows for %s{binder}"

    let runtimes : TestCaseData list = FrameworkUnderTest.everyRuntime

    [<TestCaseSource(nameof runtimes)>]
    let ``each runtime's helpers are the Monitor methods its JIT binds CORINFO_HELP_MON_ENTER and _EXIT to``
        (runtime : EmulatedRuntime)
        : unit
        =
        let helpers = SynchronizedMethodHelpers.onRuntime runtime

        monitorMethodOf runtime (binderOf runtime "CORINFO_HELP_MON_ENTER")
        |> shouldEqual (helpers.Enter, "SM_Obj_RefBool_RetVoid")

        monitorMethodOf runtime (binderOf runtime "CORINFO_HELP_MON_EXIT")
        |> shouldEqual (helpers.Exit, "SM_Obj_RefBool_RetVoid")

    [<TestCaseSource(nameof runtimes)>]
    let ``each runtime's CoreLib defines its helpers once, as static methods of an object and a ref bool``
        (runtime : EmulatedRuntime)
        : unit
        =
        let _, loggerFactory = LoggerFactory.makeTest ()

        let corelib =
            Assembly.readFile loggerFactory (FrameworkUnderTest.requireCoreLibPathOf runtime)

        let monitor =
            match corelib.TryGetTopLevelTypeDef "System.Threading" "Monitor" with
            | Some monitor -> monitor
            | None -> failwith $"%s{corelib.DefinitionFullName} defines no System.Threading.Monitor"

        let helpers = SynchronizedMethodHelpers.onRuntime runtime

        for name in [ helpers.Enter ; helpers.Exit ] do
            monitor.Methods
            |> Seq.filter (fun m ->
                m.Name = name
                && m.IsStatic
                && m.Signature.GenericParameterCount = 0
                && m.Signature.ParameterTypes = [
                    TypeDefn.PrimitiveType PrimitiveType.Object
                    TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.Boolean)
                ]
            )
            |> Seq.length
            |> shouldEqual 1
