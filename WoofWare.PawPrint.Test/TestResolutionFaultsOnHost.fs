namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The access categories of the `OpcodeFaults` table checked against the host runtime: each
/// metadata-token instruction runs on a token naming what its method may not access, and the host
/// must raise exactly the access exception the entry's resolution set names, or none where it names
/// none.
///
/// The members are in one assembly built here, and each probe is a static method of another, so
/// that CoreCLR checks access as it does between two ordinary assemblies, where neither grants the
/// other `InternalsVisibleTo`. Each probe runs its instruction only when its `bool` argument is
/// true. That it returns when the argument is false shows that the JIT compiled the method, so that
/// what the instruction then raises it raises where it runs, not as the method is compiled.
///
/// `calli` is not probed: its token is a signature, which names no member to access.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestResolutionFaultsOnHost =

    type private Targets =
        {
            /// A public class, with a private static method `Hidden` and a private static method
            /// `HiddenJump` taking a `bool` and an object, each returning an int32 seven; an
            /// internal virtual method `HiddenVirtual` returning an int32 seven; a private static
            /// int32 field `HiddenStatic` and a private instance one `HiddenInstance`; a public
            /// parameterless constructor and a private one taking an int32; and a public static
            /// generic method `Public<T>` returning an int32 seven.
            Shown : Type
            /// An internal class, with a public parameterless constructor.
            HiddenClass : Type
            /// An internal struct with one public int32 field.
            HiddenStruct : Type
            /// A public generic class of one type parameter.
            PublicGeneric : Type
        }

    let private targets : Targets =
        let modul =
            AssemblyBuilder
                .DefineDynamicAssembly(AssemblyName "ResolutionFaultsTarget", AssemblyBuilderAccess.Run)
                .DefineDynamicModule
                "ResolutionFaultsTarget"

        let returnsSeven (builder : MethodBuilder) : unit =
            let il = builder.GetILGenerator ()
            il.Emit (OpCodes.Ldc_I4, 7)
            il.Emit OpCodes.Ret

        let shown =
            modul.DefineType ("Shown", TypeAttributes.Public ||| TypeAttributes.Class)

        shown.DefineMethod ("Hidden", MethodAttributes.Private ||| MethodAttributes.Static, typeof<int32>, [||])
        |> returnsSeven

        shown.DefineMethod (
            "HiddenJump",
            MethodAttributes.Private ||| MethodAttributes.Static,
            typeof<int32>,
            [| typeof<bool> ; typeof<obj> |]
        )
        |> returnsSeven

        shown.DefineMethod (
            "HiddenVirtual",
            MethodAttributes.Assembly
            ||| MethodAttributes.Virtual
            ||| MethodAttributes.NewSlot,
            typeof<int32>,
            [||]
        )
        |> returnsSeven

        let generic =
            shown.DefineMethod ("Public", MethodAttributes.Public ||| MethodAttributes.Static, typeof<int32>, [||])

        generic.DefineGenericParameters [| "T" |]
        |> ignore<GenericTypeParameterBuilder[]>

        returnsSeven generic

        shown.DefineField ("HiddenStatic", typeof<int32>, FieldAttributes.Private ||| FieldAttributes.Static)
        |> ignore<FieldBuilder>

        shown.DefineField ("HiddenInstance", typeof<int32>, FieldAttributes.Private)
        |> ignore<FieldBuilder>

        shown.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        (shown.DefineConstructor (MethodAttributes.Private, CallingConventions.Standard, [| typeof<int32> |]))
            .GetILGenerator()
            .Emit
            OpCodes.Ret

        let hiddenClass =
            modul.DefineType ("HiddenClass", TypeAttributes.NotPublic ||| TypeAttributes.Class)

        hiddenClass.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        let hiddenStruct =
            modul.DefineType (
                "HiddenStruct",
                TypeAttributes.NotPublic
                ||| TypeAttributes.Sealed
                ||| TypeAttributes.SequentialLayout,
                typeof<ValueType>
            )

        hiddenStruct.DefineField ("Value", typeof<int32>, FieldAttributes.Public)
        |> ignore<FieldBuilder>

        let publicGeneric =
            modul.DefineType ("PublicGeneric", TypeAttributes.Public ||| TypeAttributes.Class)

        publicGeneric.DefineGenericParameters [| "T" |]
        |> ignore<GenericTypeParameterBuilder[]>

        publicGeneric.DefineDefaultConstructor MethodAttributes.Public
        |> ignore<ConstructorBuilder>

        {
            Shown = shown.CreateType ()
            HiddenClass = hiddenClass.CreateType ()
            HiddenStruct = hiddenStruct.CreateType ()
            PublicGeneric = publicGeneric.CreateType ()
        }

    let private hidden : BindingFlags =
        BindingFlags.NonPublic ||| BindingFlags.Static ||| BindingFlags.Instance

    let private methodOf (name : string) : MethodInfo = targets.Shown.GetMethod (name, hidden)
    let private fieldOf (name : string) : FieldInfo = targets.Shown.GetField (name, hidden)

    type private Probe =
        {
            Op : UnaryMetadataTokenIlOp
            /// What distinguishes this probe from the instruction's others, for reporting.
            Shape : string
            /// The object the probe's second argument holds.
            Argument : obj
            /// The full name of what the probe raises when its instruction passes any access
            /// check.
            Otherwise : string option
            /// Emits the instruction under test, with what it consumes before it and what it
            /// produces popped after it. The two locals are of `HiddenStruct`.
            Emit : ILGenerator -> LocalBuilder -> LocalBuilder -> unit
        }

    let private probe
        (op : UnaryMetadataTokenIlOp)
        (shape : string)
        (argument : obj)
        (emit : ILGenerator -> LocalBuilder -> LocalBuilder -> unit)
        : Probe
        =
        {
            Op = op
            Shape = shape
            Argument = argument
            Otherwise = None
            Emit = emit
        }

    let private probes : Probe list =
        let t = targets
        let hiddenArray = Array.CreateInstance (t.HiddenStruct, 1) |> box
        let boxedHidden = Activator.CreateInstance t.HiddenStruct
        let shown = Activator.CreateInstance t.Shown
        let overHidden = t.PublicGeneric.MakeGenericType [| t.HiddenClass |]

        [
            probe
                UnaryMetadataTokenIlOp.Call
                "private method"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Call, methodOf "Hidden")
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Call
                "public method over an internal type argument"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Call, t.Shown.GetMethod("Public").MakeGenericMethod [| t.HiddenClass |])
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Callvirt
                "internal method"
                shown
                (fun il _ _ ->
                    il.Emit OpCodes.Ldarg_1
                    il.Emit (OpCodes.Callvirt, methodOf "HiddenVirtual")
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Newobj
                "private constructor"
                (null : obj)
                (fun il _ _ ->
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Newobj, t.Shown.GetConstructor (hidden, [| typeof<int32> |]))
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Newobj
                "public constructor of an internal class"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Newobj, t.HiddenClass.GetConstructor Type.EmptyTypes)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Ldftn
                "private method"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Ldftn, methodOf "Hidden")
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Ldvirtftn
                "internal method"
                shown
                (fun il _ _ ->
                    il.Emit OpCodes.Ldarg_1
                    il.Emit (OpCodes.Ldvirtftn, methodOf "HiddenVirtual")
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Jmp
                "private method"
                (null : obj)
                (fun il _ _ -> il.Emit (OpCodes.Jmp, methodOf "HiddenJump"))
            for op, opcode in
                [
                    UnaryMetadataTokenIlOp.Ldfld, OpCodes.Ldfld
                    UnaryMetadataTokenIlOp.Ldflda, OpCodes.Ldflda
                ] do
                probe
                    op
                    "private field"
                    shown
                    (fun il _ _ ->
                        il.Emit OpCodes.Ldarg_1
                        il.Emit (opcode, fieldOf "HiddenInstance")
                        il.Emit OpCodes.Pop
                    )
            probe
                UnaryMetadataTokenIlOp.Stfld
                "private field"
                shown
                (fun il _ _ ->
                    il.Emit OpCodes.Ldarg_1
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Stfld, fieldOf "HiddenInstance")
                )
            for op, opcode in
                [
                    UnaryMetadataTokenIlOp.Ldsfld, OpCodes.Ldsfld
                    UnaryMetadataTokenIlOp.Ldsflda, OpCodes.Ldsflda
                ] do
                probe
                    op
                    "private field"
                    (null : obj)
                    (fun il _ _ ->
                        il.Emit (opcode, fieldOf "HiddenStatic")
                        il.Emit OpCodes.Pop
                    )
            probe
                UnaryMetadataTokenIlOp.Stsfld
                "private field"
                (null : obj)
                (fun il _ _ ->
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Stsfld, fieldOf "HiddenStatic")
                )
            probe
                UnaryMetadataTokenIlOp.Newarr
                "internal class"
                (null : obj)
                (fun il _ _ ->
                    il.Emit OpCodes.Ldc_I4_1
                    il.Emit (OpCodes.Newarr, t.HiddenClass)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Newarr
                "public class over an internal type argument"
                (null : obj)
                (fun il _ _ ->
                    il.Emit OpCodes.Ldc_I4_1
                    il.Emit (OpCodes.Newarr, overHidden)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Box
                "internal struct"
                (null : obj)
                (fun il local _ ->
                    il.Emit (OpCodes.Ldloc, local)
                    il.Emit (OpCodes.Box, t.HiddenStruct)
                    il.Emit OpCodes.Pop
                )
            for op, opcode in
                [
                    UnaryMetadataTokenIlOp.Isinst, OpCodes.Isinst
                    UnaryMetadataTokenIlOp.Castclass, OpCodes.Castclass
                ] do
                probe
                    op
                    "internal class"
                    (null : obj)
                    (fun il _ _ ->
                        il.Emit OpCodes.Ldarg_1
                        il.Emit (opcode, t.HiddenClass)
                        il.Emit OpCodes.Pop
                    )
            for op, opcode in
                [
                    UnaryMetadataTokenIlOp.Unbox, OpCodes.Unbox
                    UnaryMetadataTokenIlOp.Unbox_Any, OpCodes.Unbox_Any
                ] do
                probe
                    op
                    "internal struct"
                    boxedHidden
                    (fun il _ _ ->
                        il.Emit OpCodes.Ldarg_1
                        il.Emit (opcode, t.HiddenStruct)
                        il.Emit OpCodes.Pop
                    )
            probe
                UnaryMetadataTokenIlOp.Mkrefany
                "internal struct"
                (null : obj)
                (fun il local _ ->
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Mkrefany, t.HiddenStruct)
                    il.Emit OpCodes.Pop
                )
            // A typed reference to an int32, which `refanyval` of anything else fails on with
            // `InvalidCastException`; an access check would raise first.
            { probe
                  UnaryMetadataTokenIlOp.Refanyval
                  "internal struct"
                  (null : obj)
                  (fun il _ _ ->
                      let integer = il.DeclareLocal typeof<int32>
                      il.Emit (OpCodes.Ldloca, integer)
                      il.Emit (OpCodes.Mkrefany, typeof<int32>)
                      il.Emit (OpCodes.Refanyval, t.HiddenStruct)
                      il.Emit OpCodes.Pop
                  ) with
                Otherwise = Some "System.InvalidCastException"
            }
            probe
                UnaryMetadataTokenIlOp.Initobj
                "internal struct"
                (null : obj)
                (fun il local _ ->
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Initobj, t.HiddenStruct)
                )
            probe
                UnaryMetadataTokenIlOp.Ldobj
                "internal struct"
                (null : obj)
                (fun il local _ ->
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Ldobj, t.HiddenStruct)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Stobj
                "internal struct"
                (null : obj)
                (fun il local _ ->
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Ldloc, local)
                    il.Emit (OpCodes.Stobj, t.HiddenStruct)
                )
            probe
                UnaryMetadataTokenIlOp.Cpobj
                "internal struct"
                (null : obj)
                (fun il local other ->
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Ldloca, other)
                    il.Emit (OpCodes.Cpobj, t.HiddenStruct)
                )
            probe
                UnaryMetadataTokenIlOp.Ldelem
                "internal struct"
                hiddenArray
                (fun il _ _ ->
                    il.Emit OpCodes.Ldarg_1
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Ldelem, t.HiddenStruct)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Ldelema
                "internal struct"
                hiddenArray
                (fun il _ _ ->
                    il.Emit OpCodes.Ldarg_1
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Ldelema, t.HiddenStruct)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Stelem
                "internal struct"
                hiddenArray
                (fun il local _ ->
                    il.Emit OpCodes.Ldarg_1
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Ldloc, local)
                    il.Emit (OpCodes.Stelem, t.HiddenStruct)
                )
            probe
                UnaryMetadataTokenIlOp.Sizeof
                "internal struct"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Sizeof, t.HiddenStruct)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Ldtoken
                "internal class"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Ldtoken, t.HiddenClass)
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Ldtoken
                "private method"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Ldtoken, methodOf "Hidden")
                    il.Emit OpCodes.Pop
                )
            probe
                UnaryMetadataTokenIlOp.Ldtoken
                "private field"
                (null : obj)
                (fun il _ _ ->
                    il.Emit (OpCodes.Ldtoken, fieldOf "HiddenStatic")
                    il.Emit OpCodes.Pop
                )
            // The prefix names an internal struct, and the call it prefixes a public method.
            probe
                UnaryMetadataTokenIlOp.Constrained
                "internal struct"
                (null : obj)
                (fun il local _ ->
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Constrained, t.HiddenStruct)
                    il.Emit (OpCodes.Callvirt, typeof<obj>.GetMethod "ToString")
                    il.Emit OpCodes.Pop
                )
        ]

    /// Each probe compiled as a static method `int32 (bool run, object argument)` of one type, in
    /// an assembly of its own.
    let private compiled : (Probe * MethodInfo) list =
        let modul =
            AssemblyBuilder
                .DefineDynamicAssembly(AssemblyName "ResolutionFaultsProbes", AssemblyBuilderAccess.Run)
                .DefineDynamicModule
                "ResolutionFaultsProbes"

        let ty = modul.DefineType ("Probes", TypeAttributes.Public ||| TypeAttributes.Class)

        let names =
            probes
            |> List.mapi (fun index probe ->
                let name = $"Probe%d{index}"

                let il =
                    ty
                        .DefineMethod(
                            name,
                            MethodAttributes.Public ||| MethodAttributes.Static,
                            typeof<int32>,
                            [| typeof<bool> ; typeof<obj> |]
                        )
                        .GetILGenerator ()

                let local = il.DeclareLocal targets.HiddenStruct
                let other = il.DeclareLocal targets.HiddenStruct
                let skip = il.DefineLabel ()
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Brfalse, skip)
                probe.Emit il local other
                il.MarkLabel skip
                il.Emit OpCodes.Ldc_I4_0
                il.Emit OpCodes.Ret
                probe, name
            )

        let created = ty.CreateType ()
        names |> List.map (fun (probe, name) -> probe, created.GetMethod name)

    /// The full name of what `method` raises on `run` and `argument`, if it raises.
    let private raised (method : MethodInfo) (run : bool) (argument : obj) : string option =
        try
            method.Invoke ((null : obj), [| box run ; argument |]) |> ignore<obj>
            None
        with :? TargetInvocationException as e ->
            Some (e.InnerException.GetType().FullName)

    let private accessFaults : ResolutionFault list =
        [
            ResolutionFault.MethodAccess
            ResolutionFault.FieldAccess
            ResolutionFault.TypeAccess
        ]

    let private accessExceptions : Set<string> =
        accessFaults |> List.collect ResolutionFault.typeNames |> Set.ofList

    [<Test>]
    let ``every metadata-token instruction but calli is probed`` () : unit =
        let probed = probes |> List.map _.Op |> Set.ofList

        FSharpType.GetUnionCases typeof<UnaryMetadataTokenIlOp>
        |> Array.map (fun case -> FSharpValue.MakeUnion (case, [||]) :?> UnaryMetadataTokenIlOp)
        |> Array.filter (fun op -> not (probed.Contains op))
        |> List.ofArray
        |> shouldEqual [ UnaryMetadataTokenIlOp.Calli ]

    [<Test>]
    let ``an instruction raises an access exception where its entry says, and at the instruction`` () : unit =
        let failures =
            [
                for probe, method in compiled do
                    let described = $"%O{probe.Op} naming %s{probe.Shape}"

                    let expected =
                        match OpcodeFaults.ofUnaryMetadata probe.Op with
                        | OpcodeFaults.Unmodelled -> failwith $"%s{described}: the entry is unmodelled"
                        | OpcodeFaults.Raises (_, resolution) ->
                            match accessFaults |> List.filter resolution.Contains with
                            | [] -> None
                            | [ fault ] -> Some (ResolutionFault.typeNames fault |> List.exactlyOne)
                            | several -> failwith $"%s{described}: the entry lists %A{several}"

                    match raised method false probe.Argument with
                    | Some thrown -> yield $"%s{described} raised %s{thrown} where it does not run"
                    | None -> ()

                    let thrown = raised method true probe.Argument

                    match expected with
                    | Some expected when thrown = Some expected -> ()
                    | Some expected -> yield $"%s{described} raised %A{thrown}, expected %s{expected}"
                    | None when thrown = probe.Otherwise -> ()
                    | None ->
                        match thrown with
                        | Some thrown when accessExceptions.Contains thrown ->
                            yield $"%s{described} raised %s{thrown}, which its entry omits"
                        | _ -> yield $"%s{described} raised %A{thrown}, expected %A{probe.Otherwise}"
            ]

        failures |> shouldEqual []
