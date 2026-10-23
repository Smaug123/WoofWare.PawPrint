namespace WoofWare.PawPrint

/// What follows a `box` in its basic block, as CoreCLR's importer reads it when deciding whether
/// to fold the box away (`impBoxPatternMatch`, importer.cpp). Each operand is that of the
/// instruction which names it.
[<RequireQualifiedAccess>]
type BoxFollower<'operand> =
    /// A `brtrue` or `brfalse`, which tests a box that is never null.
    | Branch
    /// `isinst` of this type, then a `brtrue` or `brfalse`.
    | TestedCast of cast : 'operand
    /// `unbox.any` of this type.
    | Unboxed of unboxed : 'operand
    /// `isinst` of the first type, then `unbox.any` of the second.
    | CastUnboxed of cast : 'operand * unboxed : 'operand
    /// Anything else, including the end of the block.
    | Other

[<RequireQualifiedAccess>]
module BoxFollower =

    let map (f : 'a -> 'b) (follower : BoxFollower<'a>) : BoxFollower<'b> =
        match follower with
        | BoxFollower.Branch -> BoxFollower.Branch
        | BoxFollower.TestedCast cast -> BoxFollower.TestedCast (f cast)
        | BoxFollower.Unboxed unboxed -> BoxFollower.Unboxed (f unboxed)
        | BoxFollower.CastUnboxed (cast, unboxed) -> BoxFollower.CastUnboxed (f cast, f unboxed)
        | BoxFollower.Other -> BoxFollower.Other

/// Which `box` of a byref-like type the JIT folds away rather than refusing to compile the method
/// over (`CEE_BOX`, importer.cpp): those whose block goes on to test or unbox the box at once.
[<RequireQualifiedAccess>]
module BoxPattern =

    /// Every offset of `body` at which a basic block starts, beyond its entry: where control lands
    /// other than by falling through (`ControlFlow.landedOn`), and where a protected block or a
    /// handler starts or ends (`fgFindBasicBlocks`, fgbasic.cpp).
    let private blockStarts (body : MethodInstructions<'methodVars>) : Set<int> =
        let regionBounds =
            body.ExceptionRegions
            |> Seq.collect (fun region ->
                let filter, bounds =
                    match region with
                    | ExceptionRegion.Filter (filterOffset, bounds) -> [ filterOffset ], bounds
                    | ExceptionRegion.Catch (_, bounds)
                    | ExceptionRegion.Finally bounds
                    | ExceptionRegion.Fault bounds -> [], bounds

                filter
                @ [
                    bounds.TryOffset
                    bounds.TryOffset + bounds.TryLength
                    bounds.HandlerOffset
                    bounds.HandlerOffset + bounds.HandlerLength
                ]
            )
            |> Set.ofSeq

        Set.union (ControlFlow.landedOn body) regionBounds

    /// What follows the `box` at `boxOffset` in `body`, in its block.
    ///
    /// A prefix after an `isinst` is `Other`, although the JIT reads past it: no prefix may come
    /// before a `brtrue`, `brfalse` or `unbox.any`, and the JIT refuses the method over it anyway.
    let follower (body : MethodInstructions<'methodVars>) (boxOffset : int) : BoxFollower<MetadataOperand> =
        let starts = blockStarts body

        // The instruction after the one at `offset`, with its offset, where it is in the same block.
        let nextInBlock (offset : int) : (IlOp * int) option =
            let next = offset + IlOp.NumberOfBytes body.Locations.[offset]

            match body.Locations.TryFind next with
            | Some op when not (starts.Contains next) -> Some (op, next)
            | _ -> None

        let isBranchOnValue (op : IlOp) : bool =
            match op with
            | IlOp.UnaryConst (UnaryConstIlOp.Brtrue _)
            | IlOp.UnaryConst (UnaryConstIlOp.Brtrue_s _)
            | IlOp.UnaryConst (UnaryConstIlOp.Brfalse _)
            | IlOp.UnaryConst (UnaryConstIlOp.Brfalse_s _) -> true
            | _ -> false

        match body.Locations.TryFind boxOffset with
        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Box, _)) -> ()
        | other -> invalidArg (nameof boxOffset) $"The instruction at offset %d{boxOffset} is %O{other}, not a box"

        match nextInBlock boxOffset with
        | None -> BoxFollower.Other
        | Some (op, _) when isBranchOnValue op -> BoxFollower.Branch
        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox_Any, unboxed), _) -> BoxFollower.Unboxed unboxed
        | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Isinst, cast), isinstOffset) ->
            match nextInBlock isinstOffset with
            | Some (op, _) when isBranchOnValue op -> BoxFollower.TestedCast cast
            | Some (IlOp.UnaryMetadataToken (UnaryMetadataTokenIlOp.Unbox_Any, unboxed), _) ->
                BoxFollower.CastUnboxed (cast, unboxed)
            // `isinst; ldnull; cgt.un` too folds after a box of a type that is not byref-like.
            | _ -> BoxFollower.Other
        | Some _ -> BoxFollower.Other

    /// Whether the JIT folds a `box` of `boxed`, a byref-like type, which `follower` follows. It
    /// refuses the method over any box of one that it does not fold.
    ///
    /// `provesEqual a b` is whether the JIT's `compareTypesForEquality` proves the types equal
    /// (`TypeCompareState::Must`). `decidesCast from target` is whether its `compareTypesForCast`
    /// decides the cast either way (not `TypeCompareState::May`), or `None` where the caller cannot
    /// tell; the answer is then `None`.
    let foldsByRefLike
        (provesEqual : 'ty -> 'ty -> bool)
        (decidesCast : 'ty -> 'ty -> bool option)
        (boxed : 'ty)
        (follower : BoxFollower<'ty>)
        : bool option
        =
        match follower with
        | BoxFollower.Branch -> Some true
        | BoxFollower.Other -> Some false
        | BoxFollower.Unboxed unboxed -> Some (provesEqual unboxed boxed)
        | BoxFollower.CastUnboxed (cast, unboxed) -> Some (provesEqual cast boxed && provesEqual unboxed boxed)
        | BoxFollower.TestedCast cast -> decidesCast boxed cast
