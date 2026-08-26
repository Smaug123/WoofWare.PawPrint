namespace ExnSpike

open WoofWare.PawPrint

/// Which exceptions an opcode can raise by itself, with no callee involved. This is the whole of
/// "IL semantics" that an exception-escape analysis needs: a classification, not an interpreter.
[<RequireQualifiedAccess>]
module Implicit =

    let ofNullary (op : NullaryIlOp) : string list =
        match op with
        | NullaryIlOp.Div
        | NullaryIlOp.Div_un
        | NullaryIlOp.Rem
        | NullaryIlOp.Rem_un -> [ "System.DivideByZeroException" ; "System.OverflowException" ]
        | NullaryIlOp.Add_ovf
        | NullaryIlOp.Add_ovf_un
        | NullaryIlOp.Sub_ovf
        | NullaryIlOp.Sub_ovf_un
        | NullaryIlOp.Mul_ovf
        | NullaryIlOp.Mul_ovf_un -> [ "System.OverflowException" ]
        | NullaryIlOp.Ckfinite -> [ "System.ArithmeticException" ]
        | NullaryIlOp.LdLen -> [ "System.NullReferenceException" ]
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
        | NullaryIlOp.Ldind_ref
        | NullaryIlOp.Stind_I
        | NullaryIlOp.Stind_I1
        | NullaryIlOp.Stind_I2
        | NullaryIlOp.Stind_I4
        | NullaryIlOp.Stind_I8
        | NullaryIlOp.Stind_R4
        | NullaryIlOp.Stind_R8
        | NullaryIlOp.Stind_ref -> [ "System.NullReferenceException" ]
        | NullaryIlOp.Ldelem_i
        | NullaryIlOp.Ldelem_i1
        | NullaryIlOp.Ldelem_i2
        | NullaryIlOp.Ldelem_i4
        | NullaryIlOp.Ldelem_i8
        | NullaryIlOp.Ldelem_u1
        | NullaryIlOp.Ldelem_u2
        | NullaryIlOp.Ldelem_u4
        | NullaryIlOp.Ldelem_u8
        | NullaryIlOp.Ldelem_r4
        | NullaryIlOp.Ldelem_r8
        | NullaryIlOp.Ldelem_ref -> [ "System.NullReferenceException" ; "System.IndexOutOfRangeException" ]
        | NullaryIlOp.Stelem_i
        | NullaryIlOp.Stelem_i1
        | NullaryIlOp.Stelem_i2
        | NullaryIlOp.Stelem_i4
        | NullaryIlOp.Stelem_i8
        | NullaryIlOp.Stelem_r4
        | NullaryIlOp.Stelem_r8
        | NullaryIlOp.Stelem_ref ->
            [
                "System.NullReferenceException"
                "System.IndexOutOfRangeException"
                "System.ArrayTypeMismatchException"
            ]
        | _ -> []

    let ofUnaryMetadata (op : UnaryMetadataTokenIlOp) : string list =
        match op with
        | UnaryMetadataTokenIlOp.Castclass -> [ "System.InvalidCastException" ]
        | UnaryMetadataTokenIlOp.Unbox
        | UnaryMetadataTokenIlOp.Unbox_Any -> [ "System.InvalidCastException" ; "System.NullReferenceException" ]
        | UnaryMetadataTokenIlOp.Newarr -> [ "System.OverflowException" ; "System.OutOfMemoryException" ]
        | UnaryMetadataTokenIlOp.Newobj -> [ "System.OutOfMemoryException" ]
        | UnaryMetadataTokenIlOp.Ldfld
        | UnaryMetadataTokenIlOp.Ldflda
        | UnaryMetadataTokenIlOp.Stfld -> [ "System.NullReferenceException" ]
        | UnaryMetadataTokenIlOp.Callvirt -> [ "System.NullReferenceException" ]
        | UnaryMetadataTokenIlOp.Ldsfld
        | UnaryMetadataTokenIlOp.Ldsflda
        | UnaryMetadataTokenIlOp.Stsfld -> [ "System.TypeInitializationException" ]
        | UnaryMetadataTokenIlOp.Box -> [ "System.OutOfMemoryException" ]
        | _ -> []
