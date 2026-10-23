namespace WoofWare.PawPrint.Test

open System
open System.IO
open System.Reflection
open System.Reflection.Emit
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `BoxPattern`, against the JIT itself: for every short sequence of instructions after a `box` of
/// a byref-like type, it folds the box exactly where the real runtime compiles the method.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestBoxPattern =

    /// An instruction after the box, from an alphabet of those the JIT's patterns name and some
    /// that break them.
    [<RequireQualifiedAccess>]
    type private Step =
        /// To the instruction after it.
        | Brtrue
        /// To the instruction after it.
        | Brfalse
        | Isinst of string
        | UnboxAny of string
        | Pop
        | Ldnull
        | CgtUn
        | Nop

    /// What the stack holds, as far as which of the steps may follow.
    [<RequireQualifiedAccess>]
    type private Slot =
        | Object
        | Int
        | Struct

    let private steps : Step list =
        [
            Step.Brtrue
            Step.Brfalse
            yield!
                [ "Object" ; "IMarker" ; "Held" ; "Plain" ; "Nullable`1" ]
                |> List.map Step.Isinst
            yield! [ "Held" ; "Plain" ] |> List.map Step.UnboxAny
            Step.Pop
            Step.Ldnull
            Step.CgtUn
            Step.Nop
        ]

    /// The stack after `sequence`, starting from the box's object; `None` where some step does not
    /// verify.
    let private stackAfter (sequence : Step list) : Slot list option =
        (Some [ Slot.Object ], sequence)
        ||> List.fold (fun stack step ->
            match stack, step with
            | None, _ -> None
            | Some ((Slot.Object | Slot.Int) :: rest), (Step.Brtrue | Step.Brfalse) -> Some rest
            | Some (Slot.Object :: rest), Step.Isinst _ -> Some (Slot.Object :: rest)
            | Some (Slot.Object :: rest), Step.UnboxAny _ -> Some (Slot.Struct :: rest)
            | Some (_ :: rest), Step.Pop -> Some rest
            | Some stack, Step.Ldnull -> Some (Slot.Object :: stack)
            | Some (a :: b :: rest), Step.CgtUn when a = b && a <> Slot.Struct -> Some (Slot.Int :: rest)
            | Some stack, Step.Nop -> Some stack
            | Some _, _ -> None
        )

    /// Every sequence of at most `length` steps.
    let rec private sequencesUpTo (length : int) : Step list list =
        if length = 0 then
            [ [] ]
        else
            let shorter = sequencesUpTo (length - 1)

            shorter
            @ [
                for sequence in shorter do
                    if sequence.Length = length - 1 then
                        for step in steps -> sequence @ [ step ]
            ]

    /// Each case: the steps after the box, and whether a branch from earlier in the method also
    /// lands on the first of them.
    let private cases : (Step list * bool) list =
        [
            for sequence in sequencesUpTo 3 do
                if not sequence.IsEmpty && (stackAfter sequence).IsSome then
                    yield sequence, false
                    yield sequence, true
        ]

    /// An image whose `Runners` has, for each case `i`, `M<i>(bool)`, which boxes a `Held`, and
    /// `C<i>(bool)`, which boxes an `int` in its place, as a control for whether the rest verifies.
    /// Each takes `false` to reach the box.
    let private fabricate () : byte[] =
        let builder =
            PersistedAssemblyBuilder (AssemblyName "BoxPattern", typeof<obj>.Assembly)

        let modul = builder.DefineDynamicModule "BoxPattern"

        let byRefLike (name : string) : TypeBuilder =
            let ty =
                modul.DefineType (
                    name,
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )

            ty.SetCustomAttribute (
                CustomAttributeBuilder (
                    typeof<Runtime.CompilerServices.IsByRefLikeAttribute>.GetConstructor Type.EmptyTypes,
                    [||]
                )
            )

            ty.DefineField ("X", typeof<int>, FieldAttributes.Public)
            |> ignore<FieldBuilder>

            ty

        let marker =
            modul.DefineType ("IMarker", TypeAttributes.Public ||| TypeAttributes.Interface ||| TypeAttributes.Abstract)

        marker.CreateType () |> ignore<Type>

        let heldBuilder = byRefLike "Held"
        heldBuilder.AddInterfaceImplementation marker
        let held = heldBuilder.CreateType ()
        let plain = (byRefLike "Plain").CreateType ()

        let named (name : string) : Type =
            match name with
            | "Object" -> typeof<obj>
            | "IMarker" -> marker :> Type
            | "Held" -> held
            | "Plain" -> plain
            | "Nullable`1" -> typeof<Nullable<int>>
            | other -> failwith $"BUG: no type %s{other}"

        let runners =
            modul.DefineType ("Runners", TypeAttributes.Public ||| TypeAttributes.Abstract ||| TypeAttributes.Sealed)

        let define (name : string) (pushesBoxed : ILGenerator -> unit) (sequence : Step list) (landedOn : bool) =
            let il =
                runners
                    .DefineMethod(
                        name,
                        MethodAttributes.Public ||| MethodAttributes.Static,
                        typeof<int>,
                        [| typeof<bool> |]
                    )
                    .GetILGenerator ()

            let follows = il.DefineLabel ()

            if landedOn then
                let pushes = il.DefineLabel ()
                il.Emit OpCodes.Ldarg_0
                il.Emit (OpCodes.Brfalse, pushes)
                il.Emit OpCodes.Ldnull
                il.Emit (OpCodes.Br, follows)
                il.MarkLabel pushes

            pushesBoxed il
            il.MarkLabel follows

            for step in sequence do
                match step with
                | Step.Brtrue
                | Step.Brfalse ->
                    let next = il.DefineLabel ()

                    il.Emit (
                        (if step = Step.Brtrue then
                             OpCodes.Brtrue
                         else
                             OpCodes.Brfalse),
                        next
                    )

                    il.MarkLabel next
                | Step.Isinst ty -> il.Emit (OpCodes.Isinst, named ty)
                | Step.UnboxAny ty -> il.Emit (OpCodes.Unbox_Any, named ty)
                | Step.Pop -> il.Emit OpCodes.Pop
                | Step.Ldnull -> il.Emit OpCodes.Ldnull
                | Step.CgtUn -> il.Emit OpCodes.Cgt_Un
                | Step.Nop -> il.Emit OpCodes.Nop

            for _ in Option.get (stackAfter sequence) do
                il.Emit OpCodes.Pop

            il.Emit OpCodes.Ldc_I4_0
            il.Emit OpCodes.Ret

        cases
        |> List.iteri (fun index (sequence, landedOn) ->
            define
                $"M%d{index}"
                (fun il ->
                    let local = il.DeclareLocal held
                    il.Emit (OpCodes.Ldloca, local)
                    il.Emit (OpCodes.Initobj, held)
                    il.Emit (OpCodes.Ldloc, local)
                    il.Emit (OpCodes.Box, held)
                )
                sequence
                landedOn

            define
                $"C%d{index}"
                (fun il ->
                    il.Emit OpCodes.Ldc_I4_7
                    il.Emit (OpCodes.Box, typeof<int>)
                )
                sequence
                landedOn
        )

        runners.CreateType () |> ignore<Type>

        use image = new MemoryStream ()
        builder.Save image
        image.ToArray ()

    [<Test>]
    let ``BoxPattern folds a box of a byref-like type exactly where the JIT compiles the method`` () : unit =
        let image = fabricate ()
        let _, loggerFactory = LoggerFactory.makeTest ()

        let fixture =
            Assembly.read loggerFactory (Some "BoxPattern.dll") (new MemoryStream (image))

        let runners = fixture.TypeDefs.Values |> Seq.find (fun ty -> ty.Name = "Runners")

        let nameOf (operand : MetadataOperand) : string =
            match operand with
            | MetadataOperand.FromMetadata sourced ->
                match sourced.Token with
                | MetadataToken.TypeDefinition handle -> fixture.TypeDefs.[handle].Name
                | MetadataToken.TypeReference handle -> fixture.TypeRefs.[handle].Name
                // The only type specification any step names.
                | MetadataToken.TypeSpecification _ -> "Nullable`1"
                | other -> failwith $"BUG: a step names %O{other}"
            | MetadataOperand.FromDynamicScope _ -> failwith "BUG: a body read from an image has no scope operand"

        // What `BoxPattern` says of the box in `M<index>`. Every type a step names is exact, so the
        // JIT proves two equal where they are the same, and decides every cast but to a nullable.
        let predictedFolded (index : int) : bool =
            let method = runners.Methods |> List.find (fun m -> m.Name = $"M%d{index}")

            let body =
                match method.Body with
                | MethodBody.Il body -> body
                | other -> failwith $"BUG: M%d{index} has no IL: %A{other}"

            let boxOffset =
                body.Instructions
                |> List.pick (fun (op, offset) ->
                    match op with
                    | IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, _) -> Some offset
                    | _ -> None
                )

            BoxPattern.follower body boxOffset
            |> BoxFollower.map nameOf
            |> BoxPattern.foldsByRefLike (=) (fun _ target -> Some (target <> "Nullable`1")) "Held"
            |> Option.get

        let context =
            System.Runtime.Loader.AssemblyLoadContext ("BoxPattern", isCollectible = true)

        try
            let ty = context.LoadFromStream(new MemoryStream (image)).GetType "Runners"

            // Whether the JIT refuses to compile the method.
            let refused (name : string) : bool =
                try
                    ty.GetMethod(name).Invoke ((null : obj), [| box false |]) |> ignore<obj>
                    false
                with :? TargetInvocationException as e ->
                    e.InnerException :? InvalidProgramException

            let compared =
                cases
                |> List.mapi (fun index case -> index, case)
                |> List.filter (fun (index, _) -> not (refused $"C%d{index}"))

            let failures =
                [
                    for index, (sequence, landedOn) in compared do
                        let predicted = predictedFolded index
                        let actual = not (refused $"M%d{index}")

                        if predicted <> actual then
                            yield
                                $"%A{sequence} (landed on: %b{landedOn}): the JIT folded %b{actual}, BoxPattern %b{predicted}"
                ]

            // The comparison covers both answers, and most of the cases.
            let folded =
                compared |> List.filter (fun (index, _) -> predictedFolded index) |> List.length

            (folded > 50, compared.Length - folded > 50, compared.Length * 2 > cases.Length)
            |> shouldEqual (true, true, true)

            match failures with
            | [] -> ()
            | failures -> failures |> String.concat Environment.NewLine |> failwith
        finally
            context.Unload ()
