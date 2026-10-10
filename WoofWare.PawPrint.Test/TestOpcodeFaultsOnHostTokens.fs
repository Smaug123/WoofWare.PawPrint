namespace WoofWare.PawPrint.Test

open System
open System.Reflection
open System.Reflection.Emit
open Microsoft.FSharp.Reflection
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// The `OpcodeFaults` table checked against the host runtime, for the instructions that take a
/// metadata token and do not reach memory through an array or an address (those are
/// `TestOpcodeFaultsOnHostMemory`'s): casts, boxing, `newarr`, typed references, the field
/// instructions, the invoking instructions, and those that only name a member. `ldstr`, which takes
/// a string token, is here too.
///
/// Each instruction runs as a `DynamicMethod` on inputs that reach each of its faults: a null
/// object, one of the wrong type, and one of the right type; an array length that is negative, too
/// long, or fine. The static constructors an instruction can run are those of types in an assembly
/// built here, whose constructors always throw, beside a type whose constructor is empty. Across
/// those, what the host raises must be exactly what the table lists, except for resource
/// exhaustion and for `calli` through a null pointer, which no input can be made to raise (see
/// `unobservable`).
///
/// Everything a probe does besides the instruction under test cannot raise: it builds its operands
/// from `ldarg`, `ldloca`, `initobj` of a local, `ldftn`, `mkrefany` and constants.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestOpcodeFaultsOnHostTokens =

    /// A type whose static constructor throws, if `poisoned`, and does nothing otherwise, with a
    /// static field `Static`, an instance field `Instance`, a static method `StaticMethod`, an
    /// instance method `InstanceMethod`, a virtual method `VirtualMethod` (each taking nothing and
    /// returning an int32 zero), and a constructor taking an int32.
    let private defineType (modul : ModuleBuilder) (name : string) (isStruct : bool) (poisoned : bool) : Type =
        let builder =
            if isStruct then
                modul.DefineType (
                    name,
                    TypeAttributes.Public
                    ||| TypeAttributes.Sealed
                    ||| TypeAttributes.SequentialLayout,
                    typeof<ValueType>
                )
            else
                modul.DefineType (name, TypeAttributes.Public ||| TypeAttributes.Class)

        if poisoned then
            let il = builder.DefineTypeInitializer().GetILGenerator ()
            il.Emit (OpCodes.Newobj, typeof<InvalidOperationException>.GetConstructor [||])
            il.Emit OpCodes.Throw

        builder.DefineField ("Static", typeof<int32>, FieldAttributes.Public ||| FieldAttributes.Static)
        |> ignore<FieldBuilder>

        builder.DefineField ("Instance", typeof<int32>, FieldAttributes.Public)
        |> ignore<FieldBuilder>

        let returnsZero (name : string) (attributes : MethodAttributes) : unit =
            let il =
                builder.DefineMethod(name, attributes, typeof<int32>, [||]).GetILGenerator ()

            il.Emit OpCodes.Ldc_I4_0
            il.Emit OpCodes.Ret

        returnsZero "StaticMethod" (MethodAttributes.Public ||| MethodAttributes.Static)
        returnsZero "InstanceMethod" MethodAttributes.Public

        returnsZero
            "VirtualMethod"
            (MethodAttributes.Public
             ||| MethodAttributes.Virtual
             ||| MethodAttributes.NewSlot)

        let constructor =
            builder.DefineConstructor (MethodAttributes.Public, CallingConventions.Standard, [| typeof<int32> |])

        constructor.GetILGenerator().Emit OpCodes.Ret
        builder.CreateType ()

    let private poisonedClass, poisonedStruct, healthyClass =
        let modul =
            AssemblyBuilder
                .DefineDynamicAssembly(AssemblyName "OpcodeFaultsTokens", AssemblyBuilderAccess.Run)
                .DefineDynamicModule
                "OpcodeFaultsTokens"

        defineType modul "PoisonedClass" false true,
        defineType modul "PoisonedStruct" true true,
        defineType modul "HealthyClass" false false

    let private methodOf (ty : Type) (name : string) : MethodInfo = ty.GetMethod name
    let private fieldOf (ty : Type) (name : string) : FieldInfo = ty.GetField name

    let private constructorOf (ty : Type) : ConstructorInfo = ty.GetConstructor [| typeof<int32> |]

    /// A probe of `op` whose method takes `parameters`, returns `returns`, and has the body `body`
    /// writes, including any `ldarg`s.
    let private probe
        (op : UnaryMetadataTokenIlOp)
        (opcode : OpCode)
        (shape : string)
        (parameters : Type list)
        (returns : Type)
        (arguments : obj list list)
        (body : ILGenerator -> unit)
        : HostProbe
        =
        let method =
            DynamicMethod ("Probe", returns, Array.ofList parameters, typeof<OpcodeFault>.Module)

        body (method.GetILGenerator ())

        {
            Instruction = $"%O{op}"
            Entry = OpcodeFaults.ofUnaryMetadata op
            OpCode = opcode
            Shape = shape
            Method = method
            Arguments = arguments
        }

    /// A probe of `op`, applied to its single operand, which is the method's only argument.
    let private onArgument
        (op : UnaryMetadataTokenIlOp)
        (opcode : OpCode)
        (shape : string)
        (parameter : Type)
        (returns : Type)
        (arguments : obj list)
        (instruction : ILGenerator -> unit)
        : HostProbe
        =
        probe
            op
            opcode
            shape
            [ parameter ]
            returns
            (arguments |> List.map List.singleton)
            (fun il ->
                il.Emit OpCodes.Ldarg_0
                instruction il
                il.Emit OpCodes.Ret
            )

    /// A probe of `op` that takes no arguments, and runs once.
    let private onNothing
        (op : UnaryMetadataTokenIlOp)
        (opcode : OpCode)
        (shape : string)
        (returns : Type)
        (body : ILGenerator -> unit)
        : HostProbe
        =
        probe op opcode shape [] returns [ [] ] body

    /// Objects of several types, and null, to cast.
    let private objects : obj list =
        [ (null : obj) ; box "text" ; box 1 ; box 1L ; obj () ]

    /// `ldloca` of a fresh local of `ty`, zeroed by `initobj`, which runs no static constructor.
    let private zeroedLocal (il : ILGenerator) (ty : Type) : unit =
        let local = il.DeclareLocal ty
        il.Emit (OpCodes.Ldloca, local)
        il.Emit (OpCodes.Initobj, ty)
        il.Emit (OpCodes.Ldloca, local)

    let private castProbes : HostProbe list =
        [
            for ty in [ typeof<string> ; typeof<int32> ; typeof<Nullable<int32>> ] do
                onArgument
                    UnaryMetadataTokenIlOp.Castclass
                    OpCodes.Castclass
                    ty.Name
                    typeof<obj>
                    typeof<obj>
                    objects
                    (fun il -> il.Emit (OpCodes.Castclass, ty))

                onArgument
                    UnaryMetadataTokenIlOp.Isinst
                    OpCodes.Isinst
                    ty.Name
                    typeof<obj>
                    typeof<obj>
                    objects
                    (fun il -> il.Emit (OpCodes.Isinst, ty))

                onArgument
                    UnaryMetadataTokenIlOp.Unbox_Any
                    OpCodes.Unbox_Any
                    ty.Name
                    typeof<obj>
                    ty
                    objects
                    (fun il -> il.Emit (OpCodes.Unbox_Any, ty))

            for ty in [ typeof<int32> ; typeof<Nullable<int32>> ] do
                onArgument
                    UnaryMetadataTokenIlOp.Unbox
                    OpCodes.Unbox
                    ty.Name
                    typeof<obj>
                    ty
                    objects
                    (fun il ->
                        il.Emit (OpCodes.Unbox, ty)
                        il.Emit (OpCodes.Ldobj, ty)
                    )

            onArgument
                UnaryMetadataTokenIlOp.Box
                OpCodes.Box
                "Int32"
                typeof<int32>
                typeof<obj>
                [ box 0 ]
                (fun il -> il.Emit (OpCodes.Box, typeof<int32>))

            onArgument
                UnaryMetadataTokenIlOp.Box
                OpCodes.Box
                "Nullable`1"
                typeof<Nullable<int32>>
                typeof<obj>
                [ (null : obj) ; box 1 ]
                (fun il -> il.Emit (OpCodes.Box, typeof<Nullable<int32>>))

            // Boxing a value runs no static constructor.
            onNothing
                UnaryMetadataTokenIlOp.Box
                OpCodes.Box
                "PoisonedStruct"
                typeof<obj>
                (fun il ->
                    zeroedLocal il poisonedStruct
                    il.Emit (OpCodes.Ldobj, poisonedStruct)
                    il.Emit (OpCodes.Box, poisonedStruct)
                    il.Emit OpCodes.Ret
                )
        ]

    let private newarrProbes : HostProbe list =
        [
            for ty in [ typeof<int32> ; typeof<string> ; poisonedStruct ] do
                // A negative length overflows, as does a native int no int32 holds; one beyond
                // `Array.MaxLength` is an `OutOfMemoryException`, raised before anything is
                // allocated.
                for lengthType, lengths in
                    [
                        typeof<int32>, [ box -1 ; box 0 ; box 3 ; box Int32.MaxValue ]
                        typeof<nativeint>, [ box -1n ; box 0n ; box (nativeint (1L <<< 32)) ]
                    ] do
                    onArgument
                        UnaryMetadataTokenIlOp.Newarr
                        OpCodes.Newarr
                        $"%s{ty.Name} by %s{lengthType.Name}"
                        lengthType
                        typeof<obj>
                        lengths
                        (fun il -> il.Emit (OpCodes.Newarr, ty))
        ]

    let private typedReferenceProbes : HostProbe list =
        [
            // A typed reference to an int32, asked for as each type.
            for ty in [ typeof<int32> ; typeof<int64> ] do
                onNothing
                    UnaryMetadataTokenIlOp.Refanyval
                    OpCodes.Refanyval
                    ty.Name
                    ty
                    (fun il ->
                        zeroedLocal il typeof<int32>
                        il.Emit (OpCodes.Mkrefany, typeof<int32>)
                        il.Emit (OpCodes.Refanyval, ty)
                        il.Emit (OpCodes.Ldobj, ty)
                        il.Emit OpCodes.Ret
                    )

            // `mkrefany` checks nothing, so a null address is not a fault.
            onArgument
                UnaryMetadataTokenIlOp.Mkrefany
                OpCodes.Mkrefany
                "Int32"
                typeof<nativeint>
                typeof<RuntimeTypeHandle>
                [ box 0n ]
                (fun il ->
                    il.Emit (OpCodes.Mkrefany, typeof<int32>)
                    il.Emit OpCodes.Refanytype
                )

            onNothing
                UnaryMetadataTokenIlOp.Mkrefany
                OpCodes.Mkrefany
                "Int32 local"
                typeof<RuntimeTypeHandle>
                (fun il ->
                    zeroedLocal il typeof<int32>
                    il.Emit (OpCodes.Mkrefany, typeof<int32>)
                    il.Emit OpCodes.Refanytype
                    il.Emit OpCodes.Ret
                )
        ]

    let private fieldProbes : HostProbe list =
        [
            for ty in [ poisonedClass ; healthyClass ] do
                let staticField = fieldOf ty "Static"

                onNothing
                    UnaryMetadataTokenIlOp.Ldsfld
                    OpCodes.Ldsfld
                    ty.Name
                    typeof<int32>
                    (fun il ->
                        il.Emit (OpCodes.Ldsfld, staticField)
                        il.Emit OpCodes.Ret
                    )

                onNothing
                    UnaryMetadataTokenIlOp.Ldsflda
                    OpCodes.Ldsflda
                    ty.Name
                    typeof<int32>
                    (fun il ->
                        il.Emit (OpCodes.Ldsflda, staticField)
                        il.Emit OpCodes.Ldind_I4
                        il.Emit OpCodes.Ret
                    )

                onNothing
                    UnaryMetadataTokenIlOp.Stsfld
                    OpCodes.Stsfld
                    ty.Name
                    typeof<Void>
                    (fun il ->
                        il.Emit OpCodes.Ldc_I4_0
                        il.Emit (OpCodes.Stsfld, staticField)
                        il.Emit OpCodes.Ret
                    )

            // The instruction that takes a receiver, naming an instance field of a healthy class on
            // a null receiver and on an object, and naming a static field on a null receiver, which
            // is a static-field access with the receiver discarded (ECMA-335 III.4.10).
            for ty, fieldName, receivers in
                [
                    healthyClass, "Instance", [ (null : obj) ; Activator.CreateInstance (healthyClass, [| box 0 |]) ]
                    healthyClass, "Static", [ (null : obj) ]
                    poisonedClass, "Static", [ (null : obj) ]
                ] do
                let field = fieldOf ty fieldName
                let shape = $"%s{ty.Name}::%s{fieldName}"

                onArgument
                    UnaryMetadataTokenIlOp.Ldfld
                    OpCodes.Ldfld
                    shape
                    ty
                    typeof<int32>
                    receivers
                    (fun il -> il.Emit (OpCodes.Ldfld, field))

                onArgument
                    UnaryMetadataTokenIlOp.Ldflda
                    OpCodes.Ldflda
                    shape
                    ty
                    typeof<int32>
                    receivers
                    (fun il ->
                        il.Emit (OpCodes.Ldflda, field)
                        il.Emit OpCodes.Ldind_I4
                    )

                probe
                    UnaryMetadataTokenIlOp.Stfld
                    OpCodes.Stfld
                    shape
                    [ ty ]
                    typeof<Void>
                    (receivers |> List.map List.singleton)
                    (fun il ->
                        il.Emit OpCodes.Ldarg_0
                        il.Emit OpCodes.Ldc_I4_0
                        il.Emit (OpCodes.Stfld, field)
                        il.Emit OpCodes.Ret
                    )
        ]

    let private invocationProbes : HostProbe list =
        [
            for ty in [ poisonedClass ; healthyClass ] do
                let staticMethod = methodOf ty "StaticMethod"

                onNothing
                    UnaryMetadataTokenIlOp.Call
                    OpCodes.Call
                    $"%s{ty.Name}::StaticMethod"
                    typeof<int32>
                    (fun il ->
                        il.Emit (OpCodes.Call, staticMethod)
                        il.Emit OpCodes.Ret
                    )

                // A class's instance method runs no static constructor, and `call` does not check
                // its receiver.
                onArgument
                    UnaryMetadataTokenIlOp.Call
                    OpCodes.Call
                    $"%s{ty.Name}::InstanceMethod"
                    ty
                    typeof<int32>
                    [ (null : obj) ]
                    (fun il -> il.Emit (OpCodes.Call, methodOf ty "InstanceMethod"))

                onNothing
                    UnaryMetadataTokenIlOp.Newobj
                    OpCodes.Newobj
                    ty.Name
                    typeof<obj>
                    (fun il ->
                        il.Emit OpCodes.Ldc_I4_0
                        il.Emit (OpCodes.Newobj, constructorOf ty)
                        il.Emit OpCodes.Ret
                    )

                // `jmp` needs its method's signature to be the target's: none, returning an int32.
                onNothing
                    UnaryMetadataTokenIlOp.Jmp
                    OpCodes.Jmp
                    ty.Name
                    typeof<int32>
                    (fun il -> il.Emit (OpCodes.Jmp, staticMethod))

                onNothing
                    UnaryMetadataTokenIlOp.Calli
                    OpCodes.Calli
                    ty.Name
                    typeof<int32>
                    (fun il ->
                        il.Emit (OpCodes.Ldftn, staticMethod)
                        il.EmitCalli (OpCodes.Calli, CallingConventions.Standard, typeof<int32>, [||], null)
                        il.Emit OpCodes.Ret
                    )

                // Naming a method runs no static constructor.
                onNothing
                    UnaryMetadataTokenIlOp.Ldftn
                    OpCodes.Ldftn
                    ty.Name
                    typeof<nativeint>
                    (fun il ->
                        il.Emit (OpCodes.Ldftn, staticMethod)
                        il.Emit OpCodes.Ret
                    )

            // A value type's instance method does run its static constructor (ECMA-335 I.8.9.5),
            // through `call` and through a `constrained.` `callvirt`.
            onNothing
                UnaryMetadataTokenIlOp.Call
                OpCodes.Call
                "PoisonedStruct::InstanceMethod"
                typeof<int32>
                (fun il ->
                    zeroedLocal il poisonedStruct
                    il.Emit (OpCodes.Call, methodOf poisonedStruct "InstanceMethod")
                    il.Emit OpCodes.Ret
                )

            onNothing
                UnaryMetadataTokenIlOp.Callvirt
                OpCodes.Callvirt
                "constrained. PoisonedStruct::VirtualMethod"
                typeof<int32>
                (fun il ->
                    zeroedLocal il poisonedStruct
                    il.Emit (OpCodes.Constrained, poisonedStruct)
                    il.Emit (OpCodes.Callvirt, methodOf poisonedStruct "VirtualMethod")
                    il.Emit OpCodes.Ret
                )

            onNothing
                UnaryMetadataTokenIlOp.Newobj
                OpCodes.Newobj
                "PoisonedStruct"
                typeof<obj>
                (fun il ->
                    il.Emit OpCodes.Ldc_I4_0
                    il.Emit (OpCodes.Newobj, constructorOf poisonedStruct)
                    il.Emit (OpCodes.Box, poisonedStruct)
                    il.Emit OpCodes.Ret
                )

            onArgument
                UnaryMetadataTokenIlOp.Callvirt
                OpCodes.Callvirt
                "Object::GetHashCode"
                typeof<obj>
                typeof<int32>
                [ (null : obj) ; obj () ]
                (fun il -> il.Emit (OpCodes.Callvirt, typeof<obj>.GetMethod "GetHashCode"))

            onArgument
                UnaryMetadataTokenIlOp.Ldvirtftn
                OpCodes.Ldvirtftn
                "Object::ToString"
                typeof<obj>
                typeof<nativeint>
                [ (null : obj) ; obj () ]
                (fun il -> il.Emit (OpCodes.Ldvirtftn, typeof<obj>.GetMethod "ToString"))
        ]

    /// Instructions that name a member of a poisoned type without accessing it.
    let private namingProbes : HostProbe list =
        [
            onNothing
                UnaryMetadataTokenIlOp.Ldtoken
                OpCodes.Ldtoken
                "PoisonedStruct"
                typeof<RuntimeTypeHandle>
                (fun il ->
                    il.Emit (OpCodes.Ldtoken, poisonedStruct)
                    il.Emit OpCodes.Ret
                )

            onNothing
                UnaryMetadataTokenIlOp.Ldtoken
                OpCodes.Ldtoken
                "PoisonedClass::Static"
                typeof<RuntimeFieldHandle>
                (fun il ->
                    il.Emit (OpCodes.Ldtoken, fieldOf poisonedClass "Static")
                    il.Emit OpCodes.Ret
                )

            onNothing
                UnaryMetadataTokenIlOp.Ldtoken
                OpCodes.Ldtoken
                "PoisonedClass::StaticMethod"
                typeof<RuntimeMethodHandle>
                (fun il ->
                    il.Emit (OpCodes.Ldtoken, methodOf poisonedClass "StaticMethod")
                    il.Emit OpCodes.Ret
                )

            onNothing
                UnaryMetadataTokenIlOp.Sizeof
                OpCodes.Sizeof
                "PoisonedStruct"
                typeof<int32>
                (fun il ->
                    il.Emit (OpCodes.Sizeof, poisonedStruct)
                    il.Emit OpCodes.Ret
                )
        ]

    let private ldstrProbe : HostProbe =
        let method =
            DynamicMethod ("Probe", typeof<string>, [||], typeof<OpcodeFault>.Module)

        let il = method.GetILGenerator ()
        il.Emit (OpCodes.Ldstr, "literal")
        il.Emit OpCodes.Ret

        {
            Instruction = $"%O{UnaryStringTokenIlOp.Ldstr}"
            Entry = OpcodeFaults.ofUnaryStringToken UnaryStringTokenIlOp.Ldstr
            OpCode = OpCodes.Ldstr
            Shape = ""
            Method = method
            Arguments = [ [] ]
        }

    let private probes : HostProbe list =
        castProbes
        @ newarrProbes
        @ typedReferenceProbes
        @ fieldProbes
        @ invocationProbes
        @ namingProbes
        @ [ ldstrProbe ]

    /// Faults an entry lists that no input can make the host raise. `calli` through a null pointer
    /// crashes CoreCLR; its `NullReference` entry records PawPrint's own deliberate divergence
    /// (`docs/divergences.md`). Resource exhaustion is exempt for every instruction, in
    /// `HostFaultProbe.mismatches`.
    let private unobservable : (string * OpcodeFault) list =
        [ $"%O{UnaryMetadataTokenIlOp.Calli}", OpcodeFault.NullReference ]

    /// The token-bearing instructions `TestOpcodeFaultsOnHostMemory` checks.
    let private checkedElsewhere : Set<string> =
        [
            UnaryMetadataTokenIlOp.Ldelem
            UnaryMetadataTokenIlOp.Stelem
            UnaryMetadataTokenIlOp.Ldelema
            UnaryMetadataTokenIlOp.Ldobj
            UnaryMetadataTokenIlOp.Stobj
            UnaryMetadataTokenIlOp.Cpobj
            UnaryMetadataTokenIlOp.Initobj
        ]
        |> List.map (fun op -> $"%O{op}")
        |> Set.ofList

    /// Every token-bearing instruction is checked against the host, here or in
    /// `TestOpcodeFaultsOnHostMemory`, except `constrained.`, a prefix with no behaviour of its own
    /// to run.
    [<Test>]
    let ``every token-bearing instruction is checked`` () : unit =
        let checkedHere = probes |> List.map (fun probe -> probe.Instruction) |> Set.ofList

        FSharpType.GetUnionCases typeof<UnaryMetadataTokenIlOp>
        |> Array.map (fun case -> case.Name)
        |> Array.filter (fun name -> not (checkedHere.Contains name) && not (checkedElsewhere.Contains name))
        |> Array.toList
        |> shouldEqual [ $"%O{UnaryMetadataTokenIlOp.Constrained}" ]

    [<Test>]
    let ``each instruction is paired with its own encoding`` () : unit =
        HostFaultProbe.misencoded probes |> shouldEqual []

    [<Test>]
    let ``each instruction raises exactly what the table lists`` () : unit =
        match HostFaultProbe.check unobservable probes with
        | [] -> ()
        | mismatches -> failwith (String.concat Environment.NewLine mismatches)
