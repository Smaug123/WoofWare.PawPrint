namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestFunctionPointerConcretization =

    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private baseClassTypes : BaseClassTypes<DumpedAssembly> =
        Corelib.getBaseTypes corelib

    let private loadedAssemblies : ImmutableDictionary<string, DumpedAssembly> =
        ImmutableDictionary<string, DumpedAssembly>.Empty.Add (corelib.Name.FullName, corelib)

    let private concreteTypes : AllConcreteTypes =
        Corelib.concretizeAll loadedAssemblies baseClassTypes AllConcreteTypes.Empty

    /// Default method-kind signature header. ECMA-335 II.23.2.1: low bits select the
    /// calling convention; bit 0x10 is HASTHIS, bit 0x20 is EXPLICITTHIS, bit 0x40 is
    /// VARARG. Plain method calling convention with no `this` is the canonical zero.
    let private methodHeader : ComparableSignatureHeader =
        ComparableSignatureHeader.Make (SignatureHeader (byte 0))

    let private makeSignature
        (parameters : TypeDefn list)
        (returnType : MethodReturnType<TypeDefn>)
        : TypeMethodSignature<TypeDefn>
        =
        {
            Header = methodHeader
            ParameterTypes = parameters
            GenericParameterCount = 0
            RequiredParameterCount = parameters.Length
            ReturnType = returnType
        }

    let private state () : IlMachineState =
        let _, loggerFactory = LoggerFactory.makeTest ()

        { IlMachineState.initial loggerFactory ImmutableArray.Empty corelib with
            ConcreteTypes = concreteTypes
        }

    let private concretize (state : IlMachineState) (ty : TypeDefn) : IlMachineState * ConcreteTypeHandle =
        let _, loggerFactory = LoggerFactory.makeTest ()

        IlMachineState.concretizeType
            loggerFactory
            baseClassTypes
            state
            corelib.Name
            ImmutableArray.Empty
            ImmutableArray.Empty
            ty

    [<Test>]
    let ``Concretizing TypeDefn.FunctionPointer yields ConcreteTypeHandle.FunctionPointer`` () : unit =
        let signature =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let _, handle = concretize (state ()) (TypeDefn.FunctionPointer signature)

        match handle with
        | ConcreteTypeHandle.FunctionPointer concreteSig ->
            concreteSig.GenericParameterCount |> shouldEqual 0
            concreteSig.RequiredParameterCount |> shouldEqual 1
            concreteSig.ParameterTypes.Length |> shouldEqual 1

            match concreteSig.ReturnType with
            | MethodReturnType.Returns _ -> ()
            | MethodReturnType.Void -> Assert.Fail "Expected Returns, got Void"
        | other -> Assert.Fail $"Expected FunctionPointer handle, got %O{other}"

    [<Test>]
    let ``Function pointer parameters are concretized`` () : unit =
        let signature =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int64))

        let state, handle = concretize (state ()) (TypeDefn.FunctionPointer signature)

        let int32Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int32

        let int64Handle =
            AllConcreteTypes.getRequiredNonGenericHandle state.ConcreteTypes baseClassTypes.Int64

        match handle with
        | ConcreteTypeHandle.FunctionPointer concreteSig ->
            concreteSig.ParameterTypes |> shouldEqual [ int32Handle ]
            concreteSig.ReturnType |> shouldEqual (MethodReturnType.Returns int64Handle)
        | other -> Assert.Fail $"Expected FunctionPointer handle, got %O{other}"

    [<Test>]
    let ``Void-returning function pointers are distinct from value-returning ones`` () : unit =
        let returningSig =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let voidSig =
            makeSignature [ TypeDefn.PrimitiveType PrimitiveType.Int32 ] MethodReturnType.Void

        let state, returningHandle =
            concretize (state ()) (TypeDefn.FunctionPointer returningSig)

        let _, voidHandle = concretize state (TypeDefn.FunctionPointer voidSig)

        returningHandle |> shouldNotEqual voidHandle

    [<Test>]
    let ``Function pointers with different parameter types are distinct`` () : unit =
        let intSig =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let longSig =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int64 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let state, intHandle = concretize (state ()) (TypeDefn.FunctionPointer intSig)
        let _, longHandle = concretize state (TypeDefn.FunctionPointer longSig)

        intHandle |> shouldNotEqual longHandle

    [<Test>]
    let ``Equivalent function pointer signatures concretize to equal handles (dedup)`` () : unit =
        let firstSig =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let secondSig =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let state, firstHandle = concretize (state ()) (TypeDefn.FunctionPointer firstSig)
        let _, secondHandle = concretize state (TypeDefn.FunctionPointer secondSig)

        firstHandle |> shouldEqual secondHandle

    [<Test>]
    let ``Round-tripping a function pointer through concreteHandleToTypeDefn preserves the signature shape`` () : unit =
        let original =
            TypeDefn.FunctionPointer (
                makeSignature
                    [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                    (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int64))
            )

        let state, handle = concretize (state ()) original

        let roundTripped =
            Concretization.concreteHandleToTypeDefn baseClassTypes handle state.ConcreteTypes state._LoadedAssemblies

        // The fnptr wrapper round-trips exactly; element TypeDefns may be re-expressed
        // (e.g. PrimitiveType -> FromDefinition) by the inverse, so compare structure.
        match roundTripped with
        | TypeDefn.FunctionPointer roundTrippedSig ->
            roundTrippedSig.GenericParameterCount |> shouldEqual 0
            roundTrippedSig.RequiredParameterCount |> shouldEqual 1
            roundTrippedSig.ParameterTypes.Length |> shouldEqual 1

            match roundTrippedSig.ReturnType with
            | MethodReturnType.Returns _ -> ()
            | MethodReturnType.Void -> Assert.Fail "Expected Returns, got Void after round-trip"
        | other -> Assert.Fail $"Expected FunctionPointer after round-trip, got %O{other}"

    [<Test>]
    let ``Round-tripping a void-returning function pointer preserves Void`` () : unit =
        let original =
            TypeDefn.FunctionPointer (
                makeSignature [ TypeDefn.PrimitiveType PrimitiveType.Int32 ] MethodReturnType.Void
            )

        let state, handle = concretize (state ()) original

        let roundTripped =
            Concretization.concreteHandleToTypeDefn baseClassTypes handle state.ConcreteTypes state._LoadedAssemblies

        match roundTripped with
        | TypeDefn.FunctionPointer roundTrippedSig ->
            match roundTrippedSig.ReturnType with
            | MethodReturnType.Void -> ()
            | MethodReturnType.Returns _ -> Assert.Fail "Expected Void after round-trip, got Returns"
        | other -> Assert.Fail $"Expected FunctionPointer after round-trip, got %O{other}"

    [<Test>]
    let ``CliType.zeroOf on a function pointer handle yields a null native int`` () : unit =
        let signature =
            makeSignature
                [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))

        let state, handle = concretize (state ()) (TypeDefn.FunctionPointer signature)

        let zero, _ =
            CliType.zeroOf state.ConcreteTypes state._LoadedAssemblies baseClassTypes handle

        match zero with
        | CliType.Numeric (CliNumericType.NativeInt (NativeIntSource.Verbatim 0L)) -> ()
        | other -> Assert.Fail $"Expected null native-int zero for fnptr default, got %O{other}"

    [<Test>]
    let ``Round-tripping then re-concretizing yields the same handle`` () : unit =
        let original =
            TypeDefn.FunctionPointer (
                makeSignature
                    [ TypeDefn.PrimitiveType PrimitiveType.Int32 ]
                    (MethodReturnType.Returns (TypeDefn.PrimitiveType PrimitiveType.Int32))
            )

        let state, firstHandle = concretize (state ()) original

        let roundTripped =
            Concretization.concreteHandleToTypeDefn
                baseClassTypes
                firstHandle
                state.ConcreteTypes
                state._LoadedAssemblies

        let _, secondHandle = concretize state roundTripped

        firstHandle |> shouldEqual secondHandle
