namespace WoofWare.PawPrint.Test

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `UnsafeAccessorDispatch.stripModifiersDeep` is what makes the accessor lookup ignore custom
/// modifiers, as CoreCLR's `MetaSig::CompareState.IgnoreCustomModifiers` does. Its *recursion* is
/// tested here rather than by a guest, because no C# a guest could be written in produces the
/// shapes that distinguish a deep strip from a shallow one -- Roslyn emits at most one modifier per
/// signature position, and never one below an array or a generic argument. ECMA-335 II.23.2.7 puts
/// no such limit on a signature blob, and CoreCLR's `ConsumeCustomModifiers` loops.
///
/// `sourcesPure/UnsafeAccessorCustomModifiers.cs` is the other half: it pins the shapes C# *does*
/// produce, in a real lookup.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeAccessorModifierStrip =

    /// The modifier itself is deleted rather than inspected, so any two distinct types stand in for
    /// `IsVolatile` and `IsConst` here.
    let private isVolatile : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Boolean

    let private isConst : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Char

    let private modify (isRequired : bool) (modifier : TypeDefn) (unmodified : TypeDefn) : TypeDefn =
        TypeDefn.Modified
            {
                Unmodified = unmodified
                Modifier = modifier
                IsRequired = isRequired
            }

    let private int32 : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Int32

    [<Test>]
    let ``an unmodified type is unchanged`` () =
        UnsafeAccessorDispatch.stripModifiersDeep int32 |> shouldEqual int32

    [<Test>]
    let ``a single modifier is deleted`` () =
        modify true isVolatile int32
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual int32

    [<Test>]
    let ``stacked modifiers at one position are all deleted`` () =
        // ECMA-335 II.23.2.7 allows any number of `CustomMod`s in a row, and CoreCLR's
        // `MetaSig::ConsumeCustomModifiers` loops until the element type is not one. A strip that
        // deleted only the outermost would leave the rest.
        int32
        |> modify false isConst
        |> modify true isVolatile
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual int32

    [<Test>]
    let ``a modifier below a byref is deleted`` () =
        TypeDefn.Byref (modify true isVolatile int32)
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (TypeDefn.Byref int32)

    [<Test>]
    let ``a modifier on an array element is deleted, and the rank survives`` () =
        TypeDefn.Array (modify true isVolatile int32, 3)
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (TypeDefn.Array (int32, 3))

    [<Test>]
    let ``a modifier on a szarray element is deleted`` () =
        TypeDefn.OneDimensionalArrayLowerBoundZero (modify true isVolatile int32)
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (TypeDefn.OneDimensionalArrayLowerBoundZero int32)

    [<Test>]
    let ``a modifier below a pointer and a pinned is deleted`` () =
        TypeDefn.Pointer (modify true isVolatile int32)
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (TypeDefn.Pointer int32)

        TypeDefn.Pinned (modify true isVolatile int32)
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (TypeDefn.Pinned int32)

    [<Test>]
    let ``a modifier on a generic argument, and on the generic itself, is deleted`` () =
        let list = TypeDefn.PrimitiveType PrimitiveType.Object

        TypeDefn.GenericInstantiation (
            modify true isVolatile list,
            ImmutableArray.Create (modify false isConst int32, TypeDefn.PrimitiveType PrimitiveType.String)
        )
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (
            TypeDefn.GenericInstantiation (
                list,
                ImmutableArray.Create (int32, TypeDefn.PrimitiveType PrimitiveType.String)
            )
        )

    let private signatureOf
        (returnType : MethodReturnType<TypeDefn>)
        (parameters : TypeDefn list)
        : TypeMethodSignature<TypeDefn>
        =
        {
            Header =
                SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
                |> ComparableSignatureHeader.Make
            ParameterTypes = parameters
            GenericParameterCount = 0
            RequiredParameterCount = List.length parameters
            ReturnType = returnType
        }

    [<Test>]
    let ``a modifier inside a function pointer signature is deleted`` () =
        TypeDefn.FunctionPointer (
            signatureOf (MethodReturnType.Returns (modify true isVolatile int32)) [ modify false isConst int32 ]
        )
        |> UnsafeAccessorDispatch.stripModifiersDeep
        |> shouldEqual (TypeDefn.FunctionPointer (signatureOf (MethodReturnType.Returns int32) [ int32 ]))

    [<Test>]
    let ``a void return under a modifier folds to Void`` () =
        // Every C# `init` accessor is spelled `void modreq(IsExternalInit)`, which decodes as
        // `Returns TypeDefn.Void` rather than `Void`. Without the fold, no accessor's own `void`
        // return could equal it and an `init` setter would be unreachable.
        MethodReturnType.Returns (modify true isVolatile TypeDefn.Void)
        |> UnsafeAccessorDispatch.stripReturnModifiersDeep
        |> shouldEqual MethodReturnType.Void

    [<Test>]
    let ``a signature is stripped in every position`` () =
        signatureOf (MethodReturnType.Returns (modify true isVolatile int32)) [ modify false isConst int32 ; int32 ]
        |> UnsafeAccessorDispatch.stripSignatureModifiersDeep
        |> shouldEqual (signatureOf (MethodReturnType.Returns int32) [ int32 ; int32 ])
