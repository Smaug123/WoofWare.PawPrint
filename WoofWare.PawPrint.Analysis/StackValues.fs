namespace WoofWare.PawPrint.Analysis

open WoofWare.PawPrint

/// An object a stack slot may hold, by the type a body spells for it: an index into
/// `BodySpellings.Spellings`. The spellings are the body's own, so a type variable in one is the
/// body's, and each instance of the body concretises it under its own instantiation.
[<RequireQualifiedAccess>]
type internal SpelledObject =
    /// The null reference.
    | Null
    /// An object whose class is exactly the type spelled: what `newobj` or `ldstr` makes.
    | Exactly of spelling : int
    /// Null, or an object whose class is the type spelled or one derived from it: what a value of
    /// that static type holds. For a value type, that value boxed.
    | Within of spelling : int
    /// What the call at this offset returns: what the `ret`s of the method it reaches return, which
    /// each instance of the body decides; or, where that is not known, `Within` the declared return
    /// type, if it is spelled.
    | Returned of call : int * declared : int option
    /// What the caller passed as the argument at this index, `this` at 0: what the call reaching
    /// each instance of the body says it passes (`MethodInstance.Passed`); or, where that is not
    /// known, `Within` the declared parameter type. The body never stores to the argument or takes
    /// its address, so it holds that object throughout.
    | Argument of index : int * declared : int

/// What a stack slot holds, as far as the body's spellings say.
[<RequireQualifiedAccess>]
type internal StackValue =
    /// One of these objects.
    | Objects of Set<SpelledObject>
    /// Anything: a value that is not an object reference this follows, or one whose type the body
    /// does not say.
    | Unknown

/// A type a body's metadata spells for a stack value.
type internal SpelledType =
    {
        Type : TypeDefn
        /// The full name of the assembly whose metadata spells it.
        SpelledIn : string
        /// Whether a type variable in it is the body's own, so that an instance of the body can
        /// concretise it. One a member's signature spells, such as a callee's return type, has the
        /// member's type variables instead, and names only the type definition it instantiates.
        InBodyContext : bool
    }

/// The values a body's spellings give each source of a stack value.
type internal BodySpellings =
    {
        /// Each type spelled.
        Spellings : SpelledType list
        /// What `ldarg` of each index pushes, `this` at 0 for an instance method.
        Arguments : StackValue list
        /// What `ldloc` of each index pushes.
        Locals : StackValue list
        /// What `ldstr` pushes.
        String : StackValue
        /// What the token of the instruction at each offset makes it push, where it decides that:
        /// a call's result, a new object, a field, a cast, a box, a load. One absent pushes
        /// `Unknown`.
        Tokens : Map<int, StackValue>
        /// What a `catch` handler starting at each offset starts with on the stack.
        Caught : Map<int, StackValue>
        /// What the stack effects need beyond the instructions.
        Effects : StackEffectInputs
    }

/// Which objects each slot of the evaluation stack may hold on entry to every instruction of a
/// body, joined over every path that reaches it. It assumes the IL is well typed: a value held
/// where the body spells a type is of that type, as the JIT also assumes when it devirtualises.
[<RequireQualifiedAccess>]
module internal StackValues =

    /// How many objects a slot may name before it is `Unknown`; it bounds the join.
    let private limit : int = 8

    let private join (_ : int) (_ : int) (a : StackValue) (b : StackValue) : Result<StackValue, StackShapeError> =
        match a, b with
        | StackValue.Objects a, StackValue.Objects b ->
            let joined = Set.union a b

            if joined.Count > limit then
                Ok StackValue.Unknown
            else
                Ok (StackValue.Objects joined)
        | StackValue.Unknown, _
        | _, StackValue.Unknown -> Ok StackValue.Unknown

    let private lattice (spellings : BodySpellings) : SlotLattice<StackValue> =
        let fixedValue (value : StackValue) : Result<StackValue list -> StackValue, StackShapeError> =
            Ok (fun _ -> value)

        let push (offset : int) (_ : IlOp) (pushed : Pushed) =
            match pushed with
            | Pushed.Argument index -> fixedValue (List.item index spellings.Arguments)
            | Pushed.Local index -> fixedValue (List.item index spellings.Locals)
            | Pushed.Operand fromTop -> Ok (fun popped -> List.item fromTop popped)
            | Pushed.Null -> fixedValue (StackValue.Objects (Set.singleton SpelledObject.Null))
            | Pushed.String -> fixedValue spellings.String
            | Pushed.FromToken TokenValue.CallResult
            | Pushed.FromToken TokenValue.NewObject
            | Pushed.FromToken TokenValue.Field
            | Pushed.FromToken TokenValue.Loaded
            | Pushed.FromToken TokenValue.Cast
            | Pushed.FromToken TokenValue.Boxed ->
                fixedValue (Map.tryFind offset spellings.Tokens |> Option.defaultValue StackValue.Unknown)
            | Pushed.FromToken TokenValue.NewArray
            | Pushed.FromToken TokenValue.Handle
            | Pushed.FromToken TokenValue.MethodPointer
            | Pushed.FromToken TokenValue.TypedReference
            | Pushed.Arithmetic
            | Pushed.Bitwise
            | Pushed.Number _
            | Pushed.Address
            | Pushed.Indirect
            | Pushed.Element
            | Pushed.ArgumentHandle
            | Pushed.TypedReferenceType -> fixedValue StackValue.Unknown

        let caught (region : ExceptionRegion) : StackValue =
            match region with
            | ExceptionRegion.Catch (_, offsets) ->
                Map.tryFind offsets.HandlerOffset spellings.Caught
                |> Option.defaultValue StackValue.Unknown
            | ExceptionRegion.Filter _
            | ExceptionRegion.Finally _
            | ExceptionRegion.Fault _ -> StackValue.Unknown

        {
            Push = push
            Join = join
            Caught = caught
            // A join of spelled types widens only this account of the values, never the values.
            WideningConverts = false
        }

    /// The objects each slot may hold on entry to every instruction of `body` that the flow
    /// could type.
    let analyse (spellings : BodySpellings) (body : MethodInstructions<'methodVars>) : StackFlow<StackValue> =
        StackFlow.analyse (lattice spellings) spellings.Effects body
