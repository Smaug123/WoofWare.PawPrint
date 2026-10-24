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

/// An integer as the evaluation stack holds it.
[<RequireQualifiedAccess>]
type internal StackInteger =
    | Int32 of int32
    | Int64 of int64
    /// A native int, of the 64 bits it has on every target the analysis knows (`JitTarget`).
    | NativeInt of int64

/// A `conv` of an integer that does not check for overflow, by the type it converts to.
[<RequireQualifiedAccess>]
type internal IntegerConversion =
    | I1
    | I2
    | I4
    | I8
    | U1
    | U2
    | U4
    | U8
    | I
    | U

[<RequireQualifiedAccess>]
module internal IntegerConversion =

    /// The conversion `op` is, if it is one of these: `conv.r.un` and the floating-point
    /// conversions push no integer, and the analysis follows no constant through one that checks
    /// for overflow.
    let ofNullary (op : NullaryIlOp) : IntegerConversion option =
        match op with
        | NullaryIlOp.Conv_I1 -> Some IntegerConversion.I1
        | NullaryIlOp.Conv_I2 -> Some IntegerConversion.I2
        | NullaryIlOp.Conv_I4 -> Some IntegerConversion.I4
        | NullaryIlOp.Conv_I8 -> Some IntegerConversion.I8
        | NullaryIlOp.Conv_U1 -> Some IntegerConversion.U1
        | NullaryIlOp.Conv_U2 -> Some IntegerConversion.U2
        | NullaryIlOp.Conv_U4 -> Some IntegerConversion.U4
        | NullaryIlOp.Conv_U8 -> Some IntegerConversion.U8
        | NullaryIlOp.Conv_I -> Some IntegerConversion.I
        | NullaryIlOp.Conv_U -> Some IntegerConversion.U
        | _ -> None

[<RequireQualifiedAccess>]
module internal StackInteger =

    /// Whether `value` is zero, which is what `brtrue` and `brfalse` test of it.
    let isZero (value : StackInteger) : bool =
        match value with
        | StackInteger.Int32 value -> value = 0
        | StackInteger.Int64 value
        | StackInteger.NativeInt value -> value = 0L

    /// Whether `integer` has the value `value`, at the width the stack holds it.
    let is (value : IntegerValue) (integer : StackInteger) : bool =
        match integer with
        | StackInteger.Int32 integer -> IntegerValue.holdsOfInt32 value integer
        | StackInteger.Int64 integer
        | StackInteger.NativeInt integer -> IntegerValue.holdsOfInt64 value integer

    /// What `neg` pushes of `value`: its negation, at the width the stack holds it, so that the
    /// least value negates to itself.
    let negated (value : StackInteger) : StackInteger =
        match value with
        | StackInteger.Int32 value -> StackInteger.Int32 (-value)
        | StackInteger.Int64 value -> StackInteger.Int64 (-value)
        | StackInteger.NativeInt value -> StackInteger.NativeInt (-value)

    /// What `conversion` pushes of `value` (ECMA-335 III.3.27): `value` truncated to the width
    /// of the type it converts to, then extended to the stack type, by its sign for a signed type
    /// and with zeros for an unsigned one. A conversion to a type no narrower than `value` extends
    /// it by the sign of the type converted to.
    let converted (conversion : IntegerConversion) (value : StackInteger) : StackInteger =
        let signExtended, zeroExtended =
            match value with
            | StackInteger.Int32 value -> int64 value, int64 (uint32 value)
            | StackInteger.Int64 value
            | StackInteger.NativeInt value -> value, value

        match conversion with
        | IntegerConversion.I1 -> StackInteger.Int32 (int32 (sbyte signExtended))
        | IntegerConversion.I2 -> StackInteger.Int32 (int32 (int16 signExtended))
        | IntegerConversion.I4
        | IntegerConversion.U4 -> StackInteger.Int32 (int32 signExtended)
        | IntegerConversion.U1 -> StackInteger.Int32 (int32 (byte signExtended))
        | IntegerConversion.U2 -> StackInteger.Int32 (int32 (uint16 signExtended))
        | IntegerConversion.I8 -> StackInteger.Int64 signExtended
        | IntegerConversion.U8 -> StackInteger.Int64 zeroExtended
        | IntegerConversion.I -> StackInteger.NativeInt signExtended
        | IntegerConversion.U -> StackInteger.NativeInt zeroExtended

    /// The integer type of a parameter spelled `parameter`, whose values the analysis follows;
    /// `None` for any other type.
    let ofParameter (parameter : TypeDefn) : PrimitiveType option =
        match TypeDefn.stripCustomModifiers parameter with
        | TypeDefn.PrimitiveType primitive ->
            match primitive with
            | PrimitiveType.Boolean
            | PrimitiveType.Char
            | PrimitiveType.SByte
            | PrimitiveType.Byte
            | PrimitiveType.Int16
            | PrimitiveType.UInt16
            | PrimitiveType.Int32
            | PrimitiveType.UInt32
            | PrimitiveType.Int64
            | PrimitiveType.UInt64 -> Some primitive
            | PrimitiveType.Single
            | PrimitiveType.Double
            | PrimitiveType.String
            | PrimitiveType.TypedReference
            | PrimitiveType.IntPtr
            | PrimitiveType.UIntPtr
            | PrimitiveType.Object -> None
        | _ -> None

    /// What `ldarg` pushes of a parameter of the integer type `parameter` (`ofParameter`) that a
    /// call passed `value` as: `value` converted to the parameter's type, truncated where that is
    /// narrower (ECMA-335 III.1.6), then widened as the stack holds it. `None` where the IL would
    /// need a conversion to pass `value` as that type.
    let passedAs (parameter : PrimitiveType) (value : StackInteger) : StackInteger option =
        match parameter, value with
        | PrimitiveType.Boolean, StackInteger.Int32 value
        | PrimitiveType.Byte, StackInteger.Int32 value -> Some (StackInteger.Int32 (int32 (byte value)))
        | PrimitiveType.SByte, StackInteger.Int32 value -> Some (StackInteger.Int32 (int32 (sbyte value)))
        | PrimitiveType.Int16, StackInteger.Int32 value -> Some (StackInteger.Int32 (int32 (int16 value)))
        | PrimitiveType.UInt16, StackInteger.Int32 value
        | PrimitiveType.Char, StackInteger.Int32 value -> Some (StackInteger.Int32 (int32 (uint16 value)))
        | PrimitiveType.Int32, StackInteger.Int32 _
        | PrimitiveType.UInt32, StackInteger.Int32 _
        | PrimitiveType.Int64, StackInteger.Int64 _
        | PrimitiveType.UInt64, StackInteger.Int64 _ -> Some value
        | _ -> None

    /// How many bytes a location of the integer type `integer` (`ofParameter`) takes; `None` for
    /// any other type.
    let private width (integer : PrimitiveType) : int option =
        match integer with
        | PrimitiveType.Boolean
        | PrimitiveType.SByte
        | PrimitiveType.Byte -> Some 1
        | PrimitiveType.Char
        | PrimitiveType.Int16
        | PrimitiveType.UInt16 -> Some 2
        | PrimitiveType.Int32
        | PrimitiveType.UInt32 -> Some 4
        | PrimitiveType.Int64
        | PrimitiveType.UInt64 -> Some 8
        | PrimitiveType.Single
        | PrimitiveType.Double
        | PrimitiveType.String
        | PrimitiveType.TypedReference
        | PrimitiveType.IntPtr
        | PrimitiveType.UIntPtr
        | PrimitiveType.Object -> None

    /// What `ldind` of the integer type `loaded` pushes from a location of the integer type
    /// `location` where the stack's `held` was stored, which a store truncates to the location's
    /// width (ECMA-335 III.1.6): the location's first bytes, as many as `loaded` takes, read as
    /// `loaded`. CoreCLR runs only on little-endian CPUs, where those are its low bytes. `None`
    /// where `loaded` is the wider, so that the load reads bytes past the location, or where the IL
    /// would need a conversion to push `held` as `loaded`.
    let loadedAs (location : PrimitiveType) (loaded : PrimitiveType) (held : StackInteger) : StackInteger option =
        match width location, width loaded with
        | Some locationWidth, Some loadedWidth when loadedWidth <= locationWidth -> passedAs loaded held
        | _ -> None

/// An integer a stack slot may hold.
[<RequireQualifiedAccess>]
type internal SpelledInteger =
    /// This constant, which an `ldc` pushes.
    | Constant of StackInteger
    /// What the caller passed as the argument at this index, a parameter of an integer type
    /// (`StackInteger.ofParameter`): what the call reaching each instance of the body says it passes
    /// (`MethodInstance.Passed`), or, where that is not known, any value of that type. The body never
    /// stores to the argument or takes its address, so it holds that value throughout.
    | Argument of index : int
    /// What `ldind` of the integer type `loaded` reads through the managed pointer the caller passed
    /// as the argument at this index, before the body writes anything but its own arguments and
    /// locals or runs other code: what the location holds on entry (`MethodInstance.Passed`), or,
    /// where that is not known, any value. The body never stores to the argument or takes its
    /// address, so it holds that pointer throughout.
    | Loaded of argument : int * loaded : PrimitiveType

/// A location a managed pointer on the stack may point to.
[<RequireQualifiedAccess>]
type internal SpelledAddress =
    /// The local at this index.
    | Local of index : int
    /// Wherever the managed pointer the caller passed as the argument at this index points. The
    /// body never stores to the argument or takes its address, so it holds that pointer throughout.
    | Argument of index : int

/// What a stack slot holds, as far as the body's spellings say.
[<RequireQualifiedAccess>]
type internal StackValue =
    /// One of these objects.
    | Objects of Set<SpelledObject>
    /// One of these integers.
    | Integers of Set<SpelledInteger>
    /// A managed pointer to one of these locations.
    | Addresses of Set<SpelledAddress>
    /// Anything: a value that is not one this follows, or one whose type the body does not say.
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
        /// What each local may hold where the flow does not follow what is stored in it
        /// (`SlotLattice.Local`): any value of its declared type.
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

/// Which instructions of a body leave every location but the method's own arguments and locals as
/// they found it.
[<RequireQualifiedAccess>]
module internal OwnWrites =

    /// Whether running `op`, where it completes without throwing, stores to nothing but the
    /// arguments and locals of the method running it and runs no code but that method's own, so
    /// that every other location then holds what it held before.
    let only (op : IlOp) : bool =
        match op with
        | IlOp.Nullary nullary ->
            match nullary with
            | NullaryIlOp.Nop
            | NullaryIlOp.LdArg0
            | NullaryIlOp.LdArg1
            | NullaryIlOp.LdArg2
            | NullaryIlOp.LdArg3
            | NullaryIlOp.Ldloc_0
            | NullaryIlOp.Ldloc_1
            | NullaryIlOp.Ldloc_2
            | NullaryIlOp.Ldloc_3
            | NullaryIlOp.Stloc_0
            | NullaryIlOp.Stloc_1
            | NullaryIlOp.Stloc_2
            | NullaryIlOp.Stloc_3
            | NullaryIlOp.Pop
            | NullaryIlOp.Dup
            | NullaryIlOp.Ret
            | NullaryIlOp.LdcI4_0
            | NullaryIlOp.LdcI4_1
            | NullaryIlOp.LdcI4_2
            | NullaryIlOp.LdcI4_3
            | NullaryIlOp.LdcI4_4
            | NullaryIlOp.LdcI4_5
            | NullaryIlOp.LdcI4_6
            | NullaryIlOp.LdcI4_7
            | NullaryIlOp.LdcI4_8
            | NullaryIlOp.LdcI4_m1
            | NullaryIlOp.LdNull
            | NullaryIlOp.Ceq
            | NullaryIlOp.Cgt
            | NullaryIlOp.Cgt_un
            | NullaryIlOp.Clt
            | NullaryIlOp.Clt_un
            | NullaryIlOp.Sub
            | NullaryIlOp.Sub_ovf
            | NullaryIlOp.Sub_ovf_un
            | NullaryIlOp.Add
            | NullaryIlOp.Add_ovf
            | NullaryIlOp.Add_ovf_un
            | NullaryIlOp.Mul
            | NullaryIlOp.Mul_ovf
            | NullaryIlOp.Mul_ovf_un
            | NullaryIlOp.Div
            | NullaryIlOp.Div_un
            | NullaryIlOp.Rem
            | NullaryIlOp.Rem_un
            | NullaryIlOp.Neg
            | NullaryIlOp.Not
            | NullaryIlOp.Shr
            | NullaryIlOp.Shr_un
            | NullaryIlOp.Shl
            | NullaryIlOp.And
            | NullaryIlOp.Or
            | NullaryIlOp.Xor
            | NullaryIlOp.Conv_I
            | NullaryIlOp.Conv_I1
            | NullaryIlOp.Conv_I2
            | NullaryIlOp.Conv_I4
            | NullaryIlOp.Conv_I8
            | NullaryIlOp.Conv_R4
            | NullaryIlOp.Conv_R8
            | NullaryIlOp.Conv_U
            | NullaryIlOp.Conv_U1
            | NullaryIlOp.Conv_U2
            | NullaryIlOp.Conv_U4
            | NullaryIlOp.Conv_U8
            | NullaryIlOp.Conv_r_un
            | NullaryIlOp.Conv_ovf_i
            | NullaryIlOp.Conv_ovf_u
            | NullaryIlOp.Conv_ovf_i1
            | NullaryIlOp.Conv_ovf_i2
            | NullaryIlOp.Conv_ovf_i4
            | NullaryIlOp.Conv_ovf_i8
            | NullaryIlOp.Conv_ovf_u1
            | NullaryIlOp.Conv_ovf_u2
            | NullaryIlOp.Conv_ovf_u4
            | NullaryIlOp.Conv_ovf_u8
            | NullaryIlOp.Conv_ovf_i_un
            | NullaryIlOp.Conv_ovf_u_un
            | NullaryIlOp.Conv_ovf_i1_un
            | NullaryIlOp.Conv_ovf_u1_un
            | NullaryIlOp.Conv_ovf_i2_un
            | NullaryIlOp.Conv_ovf_u2_un
            | NullaryIlOp.Conv_ovf_i4_un
            | NullaryIlOp.Conv_ovf_u4_un
            | NullaryIlOp.Conv_ovf_i8_un
            | NullaryIlOp.Conv_ovf_u8_un
            | NullaryIlOp.Ckfinite
            | NullaryIlOp.LdLen
            | NullaryIlOp.Ldind_ref
            | NullaryIlOp.Ldind_i
            | NullaryIlOp.Ldind_i1
            | NullaryIlOp.Ldind_i2
            | NullaryIlOp.Ldind_i4
            | NullaryIlOp.Ldind_i8
            | NullaryIlOp.Ldind_u1
            | NullaryIlOp.Ldind_u2
            | NullaryIlOp.Ldind_u4
            | NullaryIlOp.Ldind_r4
            | NullaryIlOp.Ldind_r8
            | NullaryIlOp.Ldelem_i
            | NullaryIlOp.Ldelem_i1
            | NullaryIlOp.Ldelem_u1
            | NullaryIlOp.Ldelem_i2
            | NullaryIlOp.Ldelem_u2
            | NullaryIlOp.Ldelem_i4
            | NullaryIlOp.Ldelem_u4
            | NullaryIlOp.Ldelem_i8
            | NullaryIlOp.Ldelem_r4
            | NullaryIlOp.Ldelem_r8
            | NullaryIlOp.Ldelem_ref
            | NullaryIlOp.Throw
            | NullaryIlOp.Rethrow
            | NullaryIlOp.Endfilter
            | NullaryIlOp.Endfinally
            | NullaryIlOp.Volatile
            | NullaryIlOp.Tail
            | NullaryIlOp.Readonly
            | NullaryIlOp.Break
            | NullaryIlOp.Arglist
            | NullaryIlOp.Refanytype -> true
            | NullaryIlOp.Stind_ref
            | NullaryIlOp.Stind_I
            | NullaryIlOp.Stind_I1
            | NullaryIlOp.Stind_I2
            | NullaryIlOp.Stind_I4
            | NullaryIlOp.Stind_I8
            | NullaryIlOp.Stind_R4
            | NullaryIlOp.Stind_R8
            | NullaryIlOp.Stelem_i
            | NullaryIlOp.Stelem_i1
            | NullaryIlOp.Stelem_i2
            | NullaryIlOp.Stelem_i4
            | NullaryIlOp.Stelem_i8
            | NullaryIlOp.Stelem_r4
            | NullaryIlOp.Stelem_r8
            | NullaryIlOp.Stelem_ref
            | NullaryIlOp.Cpblk
            | NullaryIlOp.Initblk
            | NullaryIlOp.Localloc -> false
        | IlOp.UnaryConst unary ->
            match unary with
            | UnaryConstIlOp.Stloc _
            | UnaryConstIlOp.Stloc_s _
            | UnaryConstIlOp.Ldc_I8 _
            | UnaryConstIlOp.Ldc_I4 _
            | UnaryConstIlOp.Ldc_R4 _
            | UnaryConstIlOp.Ldc_R8 _
            | UnaryConstIlOp.Ldc_I4_s _
            | UnaryConstIlOp.Br _
            | UnaryConstIlOp.Br_s _
            | UnaryConstIlOp.Brfalse_s _
            | UnaryConstIlOp.Brtrue_s _
            | UnaryConstIlOp.Brfalse _
            | UnaryConstIlOp.Brtrue _
            | UnaryConstIlOp.Beq_s _
            | UnaryConstIlOp.Blt_s _
            | UnaryConstIlOp.Ble_s _
            | UnaryConstIlOp.Bgt_s _
            | UnaryConstIlOp.Bge_s _
            | UnaryConstIlOp.Beq _
            | UnaryConstIlOp.Blt _
            | UnaryConstIlOp.Ble _
            | UnaryConstIlOp.Bgt _
            | UnaryConstIlOp.Bge _
            | UnaryConstIlOp.Bne_un_s _
            | UnaryConstIlOp.Bge_un_s _
            | UnaryConstIlOp.Bgt_un_s _
            | UnaryConstIlOp.Ble_un_s _
            | UnaryConstIlOp.Blt_un_s _
            | UnaryConstIlOp.Bne_un _
            | UnaryConstIlOp.Bge_un _
            | UnaryConstIlOp.Bgt_un _
            | UnaryConstIlOp.Ble_un _
            | UnaryConstIlOp.Blt_un _
            | UnaryConstIlOp.Ldloc_s _
            | UnaryConstIlOp.Ldloca_s _
            | UnaryConstIlOp.Ldarga _
            | UnaryConstIlOp.Ldarg_s _
            | UnaryConstIlOp.Ldarga_s _
            | UnaryConstIlOp.Starg_s _
            | UnaryConstIlOp.Starg _
            | UnaryConstIlOp.Unaligned _
            | UnaryConstIlOp.Ldloc _
            | UnaryConstIlOp.Ldloca _
            | UnaryConstIlOp.Ldarg _ -> true
            // It runs the `finally` handlers it leaves.
            | UnaryConstIlOp.Leave _
            | UnaryConstIlOp.Leave_s _ -> false
        | IlOp.Switch _ -> true
        // A token can name a method to call, a type whose initializer runs, or a location to
        // store to.
        | IlOp.UnaryMetadataToken _
        | IlOp.UnaryStringToken _ -> false

    /// The offsets of `body` that a run reaches only through instructions of which `only` holds,
    /// so that every location but the method's own arguments and locals holds on entry to each of
    /// them what it held when the method was called. A handler is entered only after something
    /// throws, which runs other code.
    let reachedFirst (body : MethodInstructions<'methodVars>) : Set<int> =
        let successors (offset : int) : int list =
            let op = body.Locations.[offset]
            let next = offset + IlOp.NumberOfBytes op

            match ControlFlow.successorsOf offset op with
            | Successors.None -> []
            | Successors.FallThrough -> [ next ]
            | Successors.Targets targets -> targets
            | Successors.TargetsAndFallThrough targets -> next :: targets
            | Successors.Leave target -> [ target ]

        // Everything control can reach from `starts`, following each instruction for which
        // `follows` holds to its successors.
        let reach (starts : int list) (follows : int -> bool) : Set<int> =
            let seen = System.Collections.Generic.HashSet<int> ()
            let worklist = System.Collections.Generic.Stack<int> ()

            for start in starts do
                if body.Locations.ContainsKey start && seen.Add start then
                    worklist.Push start

            while worklist.Count > 0 do
                let offset = worklist.Pop ()

                if follows offset then
                    for next in successors offset do
                        if body.Locations.ContainsKey next && seen.Add next then
                            worklist.Push next

            Set.ofSeq seen

        let first = reach [ 0 ] (fun offset -> only body.Locations.[offset])

        let after =
            [
                for offset, op in Map.toSeq body.Locations do
                    if not (only op) then
                        yield! successors offset
                for region in body.ExceptionRegions do
                    match region with
                    | ExceptionRegion.Filter (filterOffset, offsets) ->
                        yield filterOffset
                        yield offsets.HandlerOffset
                    | ExceptionRegion.Catch (_, offsets)
                    | ExceptionRegion.Finally offsets
                    | ExceptionRegion.Fault offsets -> yield offsets.HandlerOffset
            ]

        Set.difference first (reach after (fun _ -> true))

/// Which objects each slot of the evaluation stack may hold on entry to every instruction of a
/// body, joined over every path that reaches it. It assumes the IL is well typed: a value held
/// where the body spells a type is of that type, as the JIT also assumes when it devirtualises.
[<RequireQualifiedAccess>]
module internal StackValues =

    /// How many objects, or integers, a slot may name before it is `Unknown`; it bounds the join.
    let private limit : int = 8

    /// The value a slot or a local holds where two paths' values for it meet.
    let private joinValues (a : StackValue) (b : StackValue) : StackValue =
        let bounded (joined : Set<'a>) (value : Set<'a> -> StackValue) : StackValue =
            if joined.Count > limit then
                StackValue.Unknown
            else
                value joined

        match a, b with
        | StackValue.Objects a, StackValue.Objects b -> bounded (Set.union a b) StackValue.Objects
        | StackValue.Integers a, StackValue.Integers b ->
            let joined = Set.union a b

            let holds (width : StackInteger -> bool) : bool =
                joined
                |> Set.exists (fun integer ->
                    match integer with
                    | SpelledInteger.Constant value -> width value
                    | SpelledInteger.Argument _
                    | SpelledInteger.Loaded _ -> false
                )

            let anyInt32 =
                holds (
                    function
                    | StackInteger.Int32 _ -> true
                    | _ -> false
                )

            let anyInt64 =
                holds (
                    function
                    | StackInteger.Int64 _ -> true
                    | _ -> false
                )

            let anyNative =
                holds (
                    function
                    | StackInteger.NativeInt _ -> true
                    | _ -> false
                )

            // Constants of two widths: no stack type merges an int64 with another (ECMA-335
            // III.1.8.1.3), and CoreCLR merges an int32 with a native int as a native int only
            // where it imports both paths, which a branch it folds may not, so the slot's width,
            // and what a conversion makes of it, is the JIT's to decide.
            if List.length (List.filter id [ anyInt32 ; anyInt64 ; anyNative ]) > 1 then
                StackValue.Unknown
            else
                bounded joined StackValue.Integers
        | StackValue.Addresses a, StackValue.Addresses b -> bounded (Set.union a b) StackValue.Addresses
        | StackValue.Objects _, (StackValue.Integers _ | StackValue.Addresses _)
        | StackValue.Integers _, (StackValue.Objects _ | StackValue.Addresses _)
        | StackValue.Addresses _, (StackValue.Objects _ | StackValue.Integers _)
        | StackValue.Unknown, _
        | _, StackValue.Unknown -> StackValue.Unknown

    let private lattice (spellings : BodySpellings) (unwritten : Set<int>) : SlotLattice<StackValue> =
        let fixedValue (value : StackValue) : Result<StackValue list -> StackValue, StackShapeError> =
            Ok (fun _ -> value)

        let constant (value : StackInteger) =
            fixedValue (StackValue.Integers (Set.singleton (SpelledInteger.Constant value)))

        // `operation` of each constant `value` may be, and anything where it may be an integer the
        // body does not say.
        let ofConstants (operation : StackInteger -> StackInteger) (value : StackValue) : StackValue =
            match value with
            | StackValue.Integers integers ->
                let operated =
                    integers
                    |> Seq.map (fun integer ->
                        match integer with
                        | SpelledInteger.Constant value -> Some (SpelledInteger.Constant (operation value))
                        | SpelledInteger.Argument _
                        | SpelledInteger.Loaded _ -> None
                    )
                    |> List.ofSeq

                if List.forall Option.isSome operated then
                    StackValue.Integers (operated |> List.choose id |> Set.ofList)
                else
                    StackValue.Unknown
            | StackValue.Objects _
            | StackValue.Addresses _
            | StackValue.Unknown -> StackValue.Unknown

        // What `conversion` pushes of the integer it pops.
        let convert (conversion : IntegerConversion) =
            Ok (fun popped -> ofConstants (StackInteger.converted conversion) (List.head popped))

        // What `ldind` of the integer type `loaded` at `offset` pushes from the address it pops.
        let load (offset : int) (loaded : PrimitiveType) =
            Ok (fun popped ->
                match List.head popped with
                | StackValue.Addresses addresses when unwritten.Contains offset ->
                    match Set.toList addresses with
                    | [ SpelledAddress.Argument index ] ->
                        StackValue.Integers (Set.singleton (SpelledInteger.Loaded (index, loaded)))
                    | _ -> StackValue.Unknown
                | _ -> StackValue.Unknown
            )

        let push (offset : int) (op : IlOp) (pushed : Pushed) =
            match pushed with
            | Pushed.Argument index -> fixedValue (List.item index spellings.Arguments)
            | Pushed.Local index ->
                failwith
                    $"BUG: StackValues' lattice was asked what ldloc of local %d{index} pushes at offset %d{offset}, which StackFlow answers from the local"
            | Pushed.Operand fromTop -> Ok (fun popped -> List.item fromTop popped)
            | Pushed.Negated fromTop -> Ok (fun popped -> ofConstants StackInteger.negated (List.item fromTop popped))
            | Pushed.Null -> fixedValue (StackValue.Objects (Set.singleton SpelledObject.Null))
            | Pushed.String -> fixedValue spellings.String
            | Pushed.FromToken TokenValue.CallResult
            | Pushed.FromToken TokenValue.NewObject
            | Pushed.FromToken TokenValue.Field
            | Pushed.FromToken TokenValue.Loaded
            | Pushed.FromToken TokenValue.Cast
            | Pushed.FromToken TokenValue.Boxed ->
                fixedValue (Map.tryFind offset spellings.Tokens |> Option.defaultValue StackValue.Unknown)
            | Pushed.Number _ ->
                match op with
                | IlOp.Nullary NullaryIlOp.LdcI4_0 -> constant (StackInteger.Int32 0)
                | IlOp.Nullary NullaryIlOp.LdcI4_1 -> constant (StackInteger.Int32 1)
                | IlOp.Nullary NullaryIlOp.LdcI4_2 -> constant (StackInteger.Int32 2)
                | IlOp.Nullary NullaryIlOp.LdcI4_3 -> constant (StackInteger.Int32 3)
                | IlOp.Nullary NullaryIlOp.LdcI4_4 -> constant (StackInteger.Int32 4)
                | IlOp.Nullary NullaryIlOp.LdcI4_5 -> constant (StackInteger.Int32 5)
                | IlOp.Nullary NullaryIlOp.LdcI4_6 -> constant (StackInteger.Int32 6)
                | IlOp.Nullary NullaryIlOp.LdcI4_7 -> constant (StackInteger.Int32 7)
                | IlOp.Nullary NullaryIlOp.LdcI4_8 -> constant (StackInteger.Int32 8)
                | IlOp.Nullary NullaryIlOp.LdcI4_m1 -> constant (StackInteger.Int32 -1)
                | IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4_s value) -> constant (StackInteger.Int32 (int32 value))
                | IlOp.UnaryConst (UnaryConstIlOp.Ldc_I4 value) -> constant (StackInteger.Int32 value)
                | IlOp.UnaryConst (UnaryConstIlOp.Ldc_I8 value) -> constant (StackInteger.Int64 value)
                | IlOp.Nullary NullaryIlOp.Ldind_i1 -> load offset PrimitiveType.SByte
                | IlOp.Nullary NullaryIlOp.Ldind_u1 -> load offset PrimitiveType.Byte
                | IlOp.Nullary NullaryIlOp.Ldind_i2 -> load offset PrimitiveType.Int16
                | IlOp.Nullary NullaryIlOp.Ldind_u2 -> load offset PrimitiveType.UInt16
                | IlOp.Nullary NullaryIlOp.Ldind_i4 -> load offset PrimitiveType.Int32
                | IlOp.Nullary NullaryIlOp.Ldind_u4 -> load offset PrimitiveType.UInt32
                | IlOp.Nullary NullaryIlOp.Ldind_i8 -> load offset PrimitiveType.Int64
                | IlOp.Nullary nullary ->
                    match IntegerConversion.ofNullary nullary with
                    | Some conversion -> convert conversion
                    | None -> fixedValue StackValue.Unknown
                | _ -> fixedValue StackValue.Unknown
            | Pushed.Address ->
                match op with
                | IlOp.UnaryConst (UnaryConstIlOp.Ldloca_s index) ->
                    fixedValue (StackValue.Addresses (Set.singleton (SpelledAddress.Local (int index))))
                | IlOp.UnaryConst (UnaryConstIlOp.Ldloca index) ->
                    fixedValue (StackValue.Addresses (Set.singleton (SpelledAddress.Local (int index))))
                | _ -> fixedValue StackValue.Unknown
            | Pushed.FromToken TokenValue.NewArray
            | Pushed.FromToken TokenValue.Handle
            | Pushed.FromToken TokenValue.MethodPointer
            | Pushed.FromToken TokenValue.TypedReference
            | Pushed.Arithmetic
            | Pushed.Bitwise
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

        // Anything stored in a local is of its declared type, so that is what it holds where
        // nothing narrower is known.
        let local (index : int) : StackValue = List.item index spellings.Locals

        let narrowest (index : int) (value : StackValue) : StackValue =
            match value with
            | StackValue.Unknown -> local index
            | StackValue.Objects _
            | StackValue.Integers _
            | StackValue.Addresses _ -> value

        {
            Push = push
            Local = local
            Store = narrowest
            JoinLocal = fun index a b -> narrowest index (joinValues a b)
            Join = fun _ _ a b -> Ok (joinValues a b)
            Caught = caught
            // A join of spelled types widens only this account of the values, never the values.
            WideningConverts = false
        }

    /// The objects each slot may hold on entry to every instruction of `body` that the flow
    /// could type.
    let analyse (spellings : BodySpellings) (body : MethodInstructions<'methodVars>) : StackFlow<StackValue> =
        StackFlow.analyse (lattice spellings (OwnWrites.reachedFirst body)) spellings.Effects body
