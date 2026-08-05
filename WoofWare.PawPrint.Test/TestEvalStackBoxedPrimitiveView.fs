namespace WoofWare.PawPrint.Test

open System.Runtime.InteropServices
open FsCheck
open FsCheck.FSharp
open NUnit.Framework
open WoofWare.PawPrint

/// `box` of a bare primitive stores it inside the boxed type's *own* single instance field
/// (`System.Int64::m_value`, `System.Boolean::m_value`, ...) — see
/// `UnaryMetadataObjectOps.executeBox`. A byref to that box therefore addresses the wrapper,
/// not the primitive inside it, so `ldind.<width>` through it (which is how every primitive's
/// instance methods start: `ldarg.0; ldind.i8; ...`) pops a `UserDefinedValueType` into a
/// primitive slot. These tests pin the projection that has to undo the wrapping:
/// `EvalStackValue.toCliTypeCoerced` must read the field covering the leading bytes of the
/// value type at the target's width.
///
/// The sibling `TestEvalStackPrimitiveLikeBoundary` covers the *other* half of the boundary:
/// types like `IntPtr` and enums, which `PrimitiveLikeStruct` flattens on push so they never
/// reach the stack as a `UserDefinedValueType` at all.
[<TestFixture>]
module TestEvalStackBoxedPrimitiveView =

    // Factory intentionally undisposed: corelib.Logger outlives this scope.
    let private corelib : DumpedAssembly =
        let corelibPath = typeof<obj>.Assembly.Location
        let _, loggerFactory = LoggerFactory.makeTest ()
        Assembly.readFile loggerFactory corelibPath

    let private bct : BaseClassTypes<DumpedAssembly> = Corelib.getBaseTypes corelib

    let private loaded : LoadedAssemblies = LoadedAssemblies.ofAssemblies [ corelib ]

    let private allCt : AllConcreteTypes =
        Corelib.concretizeAll loaded bct AllConcreteTypes.Empty

    let private handleFor (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) : ConcreteTypeHandle =
        AllConcreteTypes.getRequiredNonGenericHandle allCt ti

    /// The storage shape `executeBox` writes for a bare primitive: the boxed type's own
    /// instance fields, carrying the primitive in the (single) one.
    let private boxShapeOf (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>) (contents : CliType) : CliValueType =
        let declared = handleFor ti

        let field =
            ti.Fields |> List.filter (fun field -> not field.IsStatic) |> List.exactlyOne

        {
            CliField.Id = FieldId.metadata declared field.Handle field.Name
            CliField.Name = field.Name
            Contents = contents
            Offset = field.Offset
            Type = declared
            MarshallingDescriptor = field.MarshallingDescriptor
        }
        |> List.singleton
        |> CliValueType.OfFields bct allCt declared ti.Layout (CharSetMetadata.ofTypeAttributes ti.TypeAttributes)

    /// Every primitive the CLI boxes into a single-field wrapper, paired with the
    /// `PrimitiveType` whose zero is the slot `ldind.<width>` asks the coercion for, and a
    /// generator for the value stored inside the box.
    ///
    /// `System.IntPtr`/`System.UIntPtr` are deliberately absent: `PrimitiveLikeStruct` classifies
    /// them, so `ofCliType` flattens them to a bare `NativeInt` and they never take this path.
    let private boxedPrimitiveCases
        : (string * TypeInfo<GenericParamFromMetadata, TypeDefn> * PrimitiveType * Gen<CliType>) list =
        [
            "Boolean",
            bct.Boolean,
            PrimitiveType.Boolean,
            ArbMap.defaults |> ArbMap.generate<bool> |> Gen.map CliType.ofBool
            "Char", bct.Char, PrimitiveType.Char, ArbMap.defaults |> ArbMap.generate<char> |> Gen.map CliType.ofChar
            "SByte",
            bct.SByte,
            PrimitiveType.SByte,
            ArbMap.defaults
            |> ArbMap.generate<sbyte>
            |> Gen.map (CliNumericType.Int8 >> CliType.Numeric)
            "Byte",
            bct.Byte,
            PrimitiveType.Byte,
            ArbMap.defaults
            |> ArbMap.generate<byte>
            |> Gen.map (CliNumericType.UInt8 >> CliType.Numeric)
            "Int16",
            bct.Int16,
            PrimitiveType.Int16,
            ArbMap.defaults
            |> ArbMap.generate<int16>
            |> Gen.map (CliNumericType.Int16 >> CliType.Numeric)
            "UInt16",
            bct.UInt16,
            PrimitiveType.UInt16,
            ArbMap.defaults
            |> ArbMap.generate<uint16>
            |> Gen.map (CliNumericType.UInt16 >> CliType.Numeric)
            "Int32",
            bct.Int32,
            PrimitiveType.Int32,
            ArbMap.defaults
            |> ArbMap.generate<int32>
            |> Gen.map (CliNumericType.Int32 >> CliType.Numeric)
            // uint32 has no CliNumericType of its own: ECMA-335 stores it in the int32 slot,
            // reinterpreted two's-complement, which is what `zeroOfPrimitive` hands back too.
            "UInt32",
            bct.UInt32,
            PrimitiveType.UInt32,
            ArbMap.defaults
            |> ArbMap.generate<uint32>
            |> Gen.map (int32 >> CliNumericType.Int32 >> CliType.Numeric)
            "Int64",
            bct.Int64,
            PrimitiveType.Int64,
            ArbMap.defaults
            |> ArbMap.generate<int64>
            |> Gen.map (Int64Source.Verbatim >> CliNumericType.Int64 >> CliType.Numeric)
            "UInt64",
            bct.UInt64,
            PrimitiveType.UInt64,
            ArbMap.defaults
            |> ArbMap.generate<uint64>
            |> Gen.map (int64 >> Int64Source.Verbatim >> CliNumericType.Int64 >> CliType.Numeric)
            "Single",
            bct.Single,
            PrimitiveType.Single,
            ArbMap.defaults
            |> ArbMap.generate<float32>
            |> Gen.map (CliNumericType.Float32 >> CliType.Numeric)
            "Double",
            bct.Double,
            PrimitiveType.Double,
            ArbMap.defaults
            |> ArbMap.generate<float>
            |> Gen.map (CliNumericType.Float64 >> CliType.Numeric)
        ]

    /// Float equality under the bit-pattern view the coercion preserves; plain `=` says
    /// `NaN <> NaN`, which would fail the property on NaN-generating seeds.
    let private cliTypesBitEqual (a : CliType) (b : CliType) : bool =
        match a, b with
        | CliType.Numeric (CliNumericType.Float32 x), CliType.Numeric (CliNumericType.Float32 y) ->
            System.BitConverter.SingleToInt32Bits x = System.BitConverter.SingleToInt32Bits y
        | CliType.Numeric (CliNumericType.Float64 x), CliType.Numeric (CliNumericType.Float64 y) ->
            System.BitConverter.DoubleToInt64Bits x = System.BitConverter.DoubleToInt64Bits y
        | _ -> a = b

    /// The oracle: pushing a boxed primitive's storage and popping it into the slot the matching
    /// `ldind.<width>` asks for must give back exactly the primitive that was boxed.
    let private viewIsInverseOfBoxing
        (ti : TypeInfo<GenericParamFromMetadata, TypeDefn>)
        (primitive : PrimitiveType)
        (stored : CliType)
        : bool
        =
        let boxed = boxShapeOf ti stored
        let slot = CliType.zeroOfPrimitive allCt bct primitive

        let popped = EvalStackValue.ofCliType (CliType.ValueType boxed)

        // Sanity: the whole point is that these do *not* flatten on push.
        match popped with
        | EvalStackValue.UserDefinedValueType _ -> ()
        | other -> failwithf "expected boxed %O to push as UserDefinedValueType, got %A" ti.Name other

        EvalStackValue.toCliTypeCoerced slot popped |> cliTypesBitEqual stored

    let private config : Config = Config.QuickThrowOnFailure.WithMaxTest 200

    [<Test>]
    let ``viewing a boxed primitive at its own width recovers the primitive`` () : unit =
        for name, ti, primitive, genStored in boxedPrimitiveCases do
            let property =
                genStored |> Arb.fromGen |> Prop.forAll
                <| fun stored ->
                    if viewIsInverseOfBoxing ti primitive stored then
                        true
                    else
                        failwithf
                            "boxed %s: viewing %A at its own width did not recover it (got %A)"
                            name
                            stored
                            (EvalStackValue.toCliTypeCoerced
                                (CliType.zeroOfPrimitive allCt bct primitive)
                                (EvalStackValue.ofCliType (CliType.ValueType (boxShapeOf ti stored))))

            Check.One (config, property)

    [<Test>]
    let ``the motivating case: ldind i8 through a byref to a boxed Int64`` () : unit =
        // `Int64::TryFormat` and `Int64::ToString` both open with `ldarg.0; ldind.i8`, and the
        // `this` byref a virtual call on a boxed receiver synthesises points at the box.
        let stored =
            CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 1234567890123L))

        let boxed = boxShapeOf bct.Int64 stored

        let ldindI8Slot = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

        EvalStackValue.toCliTypeCoerced ldindI8Slot (EvalStackValue.ofCliType (CliType.ValueType boxed))
        |> function
            | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 1234567890123L)) -> ()
            | other -> failwithf "expected the boxed int64 back, got %A" other

    [<Test>]
    let ``viewing a multi-field value type at a primitive width reads the leading field`` () : unit =
        // The projection is "the field covering the leading `size` bytes", not "the struct's
        // only field": `ldind.i8` through a byref to `struct { long a; long b; }` reads `a`.
        let int64Handle = handleFor bct.Int64

        let field (name : string) (value : int64) : CliField =
            {
                CliField.Id = FieldId.named name
                CliField.Name = name
                Contents = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value))
                Offset = None
                Type = int64Handle
                MarshallingDescriptor = None
            }

        let declared = handleFor bct.TypedReference

        let vt =
            CliValueType.OfFields bct allCt declared Layout.Default CharSet.Ansi [ field "a" 11L ; field "b" 22L ]

        let ldindI8Slot = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

        EvalStackValue.toCliTypeCoerced ldindI8Slot (EvalStackValue.UserDefinedValueType vt)
        |> function
            | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 11L)) -> ()
            | other -> failwithf "expected the leading field, got %A" other

    [<Test>]
    let ``viewing a value type whose leading bytes are aliased reads the latest write`` () : unit =
        // Explicit layout can put two same-size fields at offset 0. `WithFieldSetById`
        // deliberately leaves the *other* alias's `Contents` stale and records which write won in
        // `EditedAtTime`, so picking a field cell by (offset, size) alone can hand back a value
        // that the byte image no longer holds. The projection has to consult the byte image
        // whenever the requested range is aliased.
        let int64Handle = handleFor bct.Int64

        let field (name : string) (value : int64) : CliField =
            {
                CliField.Id = FieldId.named name
                CliField.Name = name
                Contents = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim value))
                Offset = Some 0
                Type = int64Handle
                MarshallingDescriptor = None
            }

        let declared = handleFor bct.TypedReference

        let union =
            CliValueType.OfFields
                bct
                allCt
                declared
                (Layout.Custom (size = 8, packingSize = 0))
                CharSet.Ansi
                [ field "first" 11L ; field "second" 11L ]

        // Write through the alias that is *not* first in declaration order.
        let written =
            CliValueType.WithFieldSetById
                (FieldId.named "second")
                (CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 22L)))
                union

        let ldindI8Slot = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

        EvalStackValue.toCliTypeCoerced ldindI8Slot (EvalStackValue.UserDefinedValueType written)
        |> function
            | CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 22L)) -> ()
            | other -> failwithf "expected the latest write through the aliased range, got %A" other

    [<Test>]
    let ``viewing a value type at a width no leading field covers fails loudly`` () : unit =
        // Two int32s do not make an int64 in this model: the storage is field cells, not bytes,
        // so there is no honest int64 to hand back. Refusing beats inventing one.
        let int32Handle = handleFor bct.Int32

        let field (name : string) (offset : int) (value : int32) : CliField =
            {
                CliField.Id = FieldId.named name
                CliField.Name = name
                Contents = CliType.Numeric (CliNumericType.Int32 value)
                Offset = Some offset
                Type = int32Handle
                MarshallingDescriptor = None
            }

        let declared = handleFor bct.TypedReference

        let vt =
            CliValueType.OfFields
                bct
                allCt
                declared
                (Layout.Custom (size = 8, packingSize = 0))
                CharSet.Ansi
                [ field "lo" 0 1 ; field "hi" 4 2 ]

        let ldindI8Slot = CliType.Numeric (CliNumericType.Int64 (Int64Source.Verbatim 0L))

        let thrown =
            try
                EvalStackValue.toCliTypeCoerced ldindI8Slot (EvalStackValue.UserDefinedValueType vt)
                |> ignore

                None
            with e ->
                Some e.Message

        match thrown with
        | None -> failwith "expected a loud failure when no leading field covers the requested width"
        | Some message ->
            if not (message.Contains "8-byte") then
                failwithf "expected the failure to name the requested width, got: %s" message
