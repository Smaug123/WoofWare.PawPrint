namespace WoofWare.PawPrint.Test

open System
open System.Reflection.Emit
open System.Runtime.InteropServices
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The `OpcodeFaults` table checked against the host runtime, for the instructions that reach
/// memory through an array or an address: the `ldelem`, `stelem` and `ldelema` families, `ldlen`,
/// the `ldind` and `stind` families, `ldobj`, `stobj`, `cpobj`, `initobj`, `cpblk` and `initblk`;
/// and `throw`, whose own fault is a null operand.
///
/// Each instruction runs as a `DynamicMethod` over every combination of a few inputs: a null array
/// or address and a valid one; indices before, at and beyond each end, as int32 and as native int;
/// and, for a store into an array of references, a value of the array's actual element type and
/// one of its declared element type that the actual one does not accept. Across those, what the
/// host raises must be exactly what the table lists. The inputs are classes rather than a range,
/// so there is no property over random ones.
///
/// Three things are deliberately never tried. A non-null address that is not valid: CoreCLR ends
/// the process on most of those, and the table excludes them (see `OpcodeFaults.Raises`). A block
/// longer than 8 bytes through a null address: CoreCLR copies a long block in native code, where a
/// null address ends the process rather than raising. And a value of the operand of `throw`: what
/// `throw` raises then is the value, not a fault of the instruction.
///
/// `Ldelem_u8`, `Ldind_u8`, `Stelem_u1`, `Stelem_u2`, `Stelem_u4` and `Stelem_u8` are absent because
/// no IL encodes them: `ldelem.u8` and `ldind.u8` are other names for the `.i8` opcodes, and there
/// is no `stelem.u*`.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpcodeFaultsOnHostMemory =

    /// An instruction under test: its name as `IlOp` spells it, its table entry, how the host
    /// encodes it, and the type its token names, for one that takes a token.
    type private Instruction =
        {
            Name : string
            Entry : OpcodeFaults
            OpCode : OpCode
            Token : Type option
        }

    let private nullary (op : NullaryIlOp) (opcode : OpCode) : Instruction =
        {
            Name = $"%O{op}"
            Entry = OpcodeFaults.ofNullary op
            OpCode = opcode
            Token = None
        }

    let private withToken (op : UnaryMetadataTokenIlOp) (opcode : OpCode) (token : Type) : Instruction =
        {
            Name = $"%O{op}"
            Entry = OpcodeFaults.ofUnaryMetadata op
            OpCode = opcode
            Token = Some token
        }

    let private emit (il : ILGenerator) (instruction : Instruction) : unit =
        match instruction.Token with
        | None -> il.Emit instruction.OpCode
        | Some token -> il.Emit (instruction.OpCode, token)

    /// One instruction's `DynamicMethod`, and the argument lists to run it on.
    type private Probe =
        {
            Instruction : Instruction
            Method : DynamicMethod
            Arguments : obj list list
        }

    /// A method taking `parameters`, whose body `body` writes; `ldarg`s for every parameter come
    /// first.
    let private define (parameters : Type list) (returns : Type) (body : ILGenerator -> unit) : DynamicMethod =
        let method =
            DynamicMethod ("Probe", returns, Array.ofList parameters, typeof<OpcodeFault>.Module)

        let il = method.GetILGenerator ()

        for i in 0 .. parameters.Length - 1 do
            il.Emit (OpCodes.Ldarg, int16 i)

        body il
        method

    // ---------- Arrays ----------

    /// What an instruction does with the element of an array it names.
    [<RequireQualifiedAccess>]
    type private ArrayAccess =
        /// Pushes the element.
        | Load
        /// Replaces the element with a value.
        | Store
        /// Pushes the element's address, which the probe loads through so that it is used.
        | Address
        /// Pushes the array's length, and takes no index.
        | Length

    /// Each array instruction, how it reaches the element, and the element type of the array it is
    /// given, which is also the type a token names.
    let private arrayInstructions : (Instruction * ArrayAccess * Type) list =
        [
            nullary NullaryIlOp.Ldelem_i1 OpCodes.Ldelem_I1, ArrayAccess.Load, typeof<sbyte>
            nullary NullaryIlOp.Ldelem_u1 OpCodes.Ldelem_U1, ArrayAccess.Load, typeof<byte>
            nullary NullaryIlOp.Ldelem_i2 OpCodes.Ldelem_I2, ArrayAccess.Load, typeof<int16>
            nullary NullaryIlOp.Ldelem_u2 OpCodes.Ldelem_U2, ArrayAccess.Load, typeof<uint16>
            nullary NullaryIlOp.Ldelem_i4 OpCodes.Ldelem_I4, ArrayAccess.Load, typeof<int32>
            nullary NullaryIlOp.Ldelem_u4 OpCodes.Ldelem_U4, ArrayAccess.Load, typeof<uint32>
            nullary NullaryIlOp.Ldelem_i8 OpCodes.Ldelem_I8, ArrayAccess.Load, typeof<int64>
            nullary NullaryIlOp.Ldelem_i OpCodes.Ldelem_I, ArrayAccess.Load, typeof<nativeint>
            nullary NullaryIlOp.Ldelem_r4 OpCodes.Ldelem_R4, ArrayAccess.Load, typeof<float32>
            nullary NullaryIlOp.Ldelem_r8 OpCodes.Ldelem_R8, ArrayAccess.Load, typeof<float>
            nullary NullaryIlOp.Ldelem_ref OpCodes.Ldelem_Ref, ArrayAccess.Load, typeof<obj>
            nullary NullaryIlOp.Stelem_i1 OpCodes.Stelem_I1, ArrayAccess.Store, typeof<sbyte>
            nullary NullaryIlOp.Stelem_i2 OpCodes.Stelem_I2, ArrayAccess.Store, typeof<int16>
            nullary NullaryIlOp.Stelem_i4 OpCodes.Stelem_I4, ArrayAccess.Store, typeof<int32>
            nullary NullaryIlOp.Stelem_i8 OpCodes.Stelem_I8, ArrayAccess.Store, typeof<int64>
            nullary NullaryIlOp.Stelem_i OpCodes.Stelem_I, ArrayAccess.Store, typeof<nativeint>
            nullary NullaryIlOp.Stelem_r4 OpCodes.Stelem_R4, ArrayAccess.Store, typeof<float32>
            nullary NullaryIlOp.Stelem_r8 OpCodes.Stelem_R8, ArrayAccess.Store, typeof<float>
            nullary NullaryIlOp.Stelem_ref OpCodes.Stelem_Ref, ArrayAccess.Store, typeof<obj>
            nullary NullaryIlOp.LdLen OpCodes.Ldlen, ArrayAccess.Length, typeof<int32>
            for ty in [ typeof<int32> ; typeof<Guid> ; typeof<obj> ] do
                withToken UnaryMetadataTokenIlOp.Ldelem OpCodes.Ldelem ty, ArrayAccess.Load, ty
                withToken UnaryMetadataTokenIlOp.Stelem OpCodes.Stelem ty, ArrayAccess.Store, ty
                withToken UnaryMetadataTokenIlOp.Ldelema OpCodes.Ldelema ty, ArrayAccess.Address, ty
        ]

    let private defaultOf (ty : Type) : obj =
        if ty.IsValueType then Activator.CreateInstance ty else null

    /// Arrays declared to hold `element`: null, one of three elements, and, for `obj`, a `string[]`,
    /// whose actual element type is narrower than the declared one.
    let private arraysOf (element : Type) : obj list =
        [
            (null : obj)
            box (Array.CreateInstance (element, 3))
            if element = typeof<obj> then
                box (Array.CreateInstance (typeof<string>, 3))
        ]

    /// Values to store into an array declared to hold `element`. For `obj`, a `string`, which every
    /// array here accepts, and an `obj`, which a `string[]` does not.
    let private storedValues (element : Type) : obj list =
        if element = typeof<obj> then
            [ (null : obj) ; box "stored" ; obj () ]
        else
            [ defaultOf element ]

    /// Indices into an array of three elements: before, at and beyond each end, as int32 and as
    /// native int, including a native int that no int32 holds.
    let private indices : (Type * obj list) list =
        [
            typeof<int32>, [ -1 ; 0 ; 2 ; 3 ; Int32.MinValue ; Int32.MaxValue ] |> List.map box
            typeof<nativeint>,
            [ -1L ; 0L ; 2L ; 3L ; 1L <<< 32 ; Int64.MinValue ; Int64.MaxValue ]
            |> List.map (nativeint >> box)
        ]

    let private arrayProbes : Probe list =
        [
            for instruction, access, element in arrayInstructions do
                let arrayType = element.MakeArrayType ()

                match access with
                | ArrayAccess.Length ->
                    {
                        Instruction = instruction
                        Method =
                            define
                                [ arrayType ]
                                typeof<nativeint>
                                (fun il ->
                                    emit il instruction
                                    il.Emit OpCodes.Ret
                                )
                        Arguments = arraysOf element |> List.map List.singleton
                    }
                | ArrayAccess.Load
                | ArrayAccess.Address
                | ArrayAccess.Store ->
                    for indexType, indexValues in indices do
                        match access with
                        | ArrayAccess.Store ->
                            {
                                Instruction = instruction
                                Method =
                                    define
                                        [ arrayType ; indexType ; element ]
                                        typeof<Void>
                                        (fun il ->
                                            emit il instruction
                                            il.Emit OpCodes.Ret
                                        )
                                Arguments =
                                    HostFaultProbe.cartesian [ arraysOf element ; indexValues ; storedValues element ]
                            }
                        | _ ->
                            {
                                Instruction = instruction
                                Method =
                                    define
                                        [ arrayType ; indexType ]
                                        element
                                        (fun il ->
                                            emit il instruction

                                            if access = ArrayAccess.Address then
                                                il.Emit (OpCodes.Ldobj, element)

                                            il.Emit OpCodes.Ret
                                        )
                                Arguments = HostFaultProbe.cartesian [ arraysOf element ; indexValues ]
                            }
        ]

    // ---------- Addresses ----------

    /// Two blocks of unmanaged memory that live as long as the test host, long enough for every
    /// type here. Only nulls are ever stored in `referenceSlots`, so a load of an object reference
    /// from it reads null; `valueSlots` takes everything else.
    let private allocateZeroed () : nativeint =
        let block = Marshal.AllocHGlobal 64
        Marshal.Copy (Array.zeroCreate<byte> 64, 0, block, 64)
        block

    let private referenceSlots : nativeint = allocateZeroed ()

    let private valueSlots : nativeint = allocateZeroed ()

    /// A null address and a valid one for a value of `ty`.
    let private addressesFor (ty : Type) : obj list =
        [ box 0n ; box (if ty.IsValueType then valueSlots else referenceSlots) ]

    /// What an instruction does through an address.
    [<RequireQualifiedAccess>]
    type private Indirection =
        /// Pushes the value at the address.
        | Load
        /// Stores a value at the address.
        | Store
        /// Copies a value from the second address to the first.
        | Copy
        /// Zeroes the value at the address.
        | Initialise

    /// Each instruction that goes through an address, what it does, and the type of the value
    /// there, which is also the type a token names.
    let private addressInstructions : (Instruction * Indirection * Type) list =
        [
            nullary NullaryIlOp.Ldind_i1 OpCodes.Ldind_I1, Indirection.Load, typeof<sbyte>
            nullary NullaryIlOp.Ldind_u1 OpCodes.Ldind_U1, Indirection.Load, typeof<byte>
            nullary NullaryIlOp.Ldind_i2 OpCodes.Ldind_I2, Indirection.Load, typeof<int16>
            nullary NullaryIlOp.Ldind_u2 OpCodes.Ldind_U2, Indirection.Load, typeof<uint16>
            nullary NullaryIlOp.Ldind_i4 OpCodes.Ldind_I4, Indirection.Load, typeof<int32>
            nullary NullaryIlOp.Ldind_u4 OpCodes.Ldind_U4, Indirection.Load, typeof<uint32>
            nullary NullaryIlOp.Ldind_i8 OpCodes.Ldind_I8, Indirection.Load, typeof<int64>
            nullary NullaryIlOp.Ldind_i OpCodes.Ldind_I, Indirection.Load, typeof<nativeint>
            nullary NullaryIlOp.Ldind_r4 OpCodes.Ldind_R4, Indirection.Load, typeof<float32>
            nullary NullaryIlOp.Ldind_r8 OpCodes.Ldind_R8, Indirection.Load, typeof<float>
            nullary NullaryIlOp.Ldind_ref OpCodes.Ldind_Ref, Indirection.Load, typeof<obj>
            nullary NullaryIlOp.Stind_I1 OpCodes.Stind_I1, Indirection.Store, typeof<sbyte>
            nullary NullaryIlOp.Stind_I2 OpCodes.Stind_I2, Indirection.Store, typeof<int16>
            nullary NullaryIlOp.Stind_I4 OpCodes.Stind_I4, Indirection.Store, typeof<int32>
            nullary NullaryIlOp.Stind_I8 OpCodes.Stind_I8, Indirection.Store, typeof<int64>
            nullary NullaryIlOp.Stind_I OpCodes.Stind_I, Indirection.Store, typeof<nativeint>
            nullary NullaryIlOp.Stind_R4 OpCodes.Stind_R4, Indirection.Store, typeof<float32>
            nullary NullaryIlOp.Stind_R8 OpCodes.Stind_R8, Indirection.Store, typeof<float>
            nullary NullaryIlOp.Stind_ref OpCodes.Stind_Ref, Indirection.Store, typeof<obj>
            for ty in [ typeof<int32> ; typeof<Guid> ; typeof<obj> ] do
                withToken UnaryMetadataTokenIlOp.Ldobj OpCodes.Ldobj ty, Indirection.Load, ty
                withToken UnaryMetadataTokenIlOp.Stobj OpCodes.Stobj ty, Indirection.Store, ty
                withToken UnaryMetadataTokenIlOp.Cpobj OpCodes.Cpobj ty, Indirection.Copy, ty
                withToken UnaryMetadataTokenIlOp.Initobj OpCodes.Initobj ty, Indirection.Initialise, ty
        ]

    let private addressProbes : Probe list =
        [
            for instruction, indirection, ty in addressInstructions do
                let parameters, returns, arguments =
                    match indirection with
                    | Indirection.Load -> [ typeof<nativeint> ], ty, [ addressesFor ty ]
                    | Indirection.Store ->
                        [ typeof<nativeint> ; ty ], typeof<Void>, [ addressesFor ty ; [ defaultOf ty ] ]
                    | Indirection.Copy ->
                        [ typeof<nativeint> ; typeof<nativeint> ], typeof<Void>, [ addressesFor ty ; addressesFor ty ]
                    | Indirection.Initialise -> [ typeof<nativeint> ], typeof<Void>, [ addressesFor ty ]

                {
                    Instruction = instruction
                    Method =
                        define
                            parameters
                            returns
                            (fun il ->
                                emit il instruction
                                il.Emit OpCodes.Ret
                            )
                    Arguments = HostFaultProbe.cartesian arguments
                }

            // `cpblk` and `initblk` take a length, kept to at most 8 bytes; see the fixture's
            // documentation for why.
            let lengths = [ 0u ; 1u ; 8u ] |> List.map box
            let addresses = [ box 0n ; box valueSlots ]
            let cpblk = nullary NullaryIlOp.Cpblk OpCodes.Cpblk
            let initblk = nullary NullaryIlOp.Initblk OpCodes.Initblk

            {
                Instruction = cpblk
                Method =
                    define
                        [ typeof<nativeint> ; typeof<nativeint> ; typeof<uint32> ]
                        typeof<Void>
                        (fun il ->
                            emit il cpblk
                            il.Emit OpCodes.Ret
                        )
                Arguments = HostFaultProbe.cartesian [ addresses ; addresses ; lengths ]
            }

            {
                Instruction = initblk
                Method =
                    define
                        [ typeof<nativeint> ; typeof<int32> ; typeof<uint32> ]
                        typeof<Void>
                        (fun il ->
                            emit il initblk
                            il.Emit OpCodes.Ret
                        )
                Arguments = HostFaultProbe.cartesian [ addresses ; [ box 0 ] ; lengths ]
            }
        ]

    // ---------- `throw` ----------

    let private throwProbe : Probe =
        let throw = nullary NullaryIlOp.Throw OpCodes.Throw

        {
            Instruction = throw
            Method = define [ typeof<obj> ] typeof<Void> (fun il -> emit il throw)
            Arguments = [ [ (null : obj) ] ]
        }

    let private probes : HostProbe list =
        arrayProbes @ addressProbes @ [ throwProbe ]
        |> List.map (fun probe ->
            {
                Instruction = probe.Instruction.Name
                Entry = probe.Instruction.Entry
                OpCode = probe.Instruction.OpCode
                Shape =
                    match probe.Instruction.Token with
                    | Some token -> token.Name
                    | None -> ""
                Method = probe.Method
                Arguments = probe.Arguments
            }
        )

    [<Test>]
    let ``each instruction is paired with its own encoding`` () : unit =
        HostFaultProbe.misencoded probes |> shouldEqual []

    [<Test>]
    let ``each instruction raises exactly what the table lists`` () : unit =
        match HostFaultProbe.check [] probes with
        | [] -> ()
        | mismatches -> failwith (String.concat Environment.NewLine mismatches)
