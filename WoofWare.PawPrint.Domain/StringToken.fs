namespace WoofWare.PawPrint

open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

type StringToken =
    | UserString of UserStringHandle
    | String of StringHandle

[<RequireQualifiedAccess>]
module StringToken =
    let ofInt (value : int) : StringToken =
        match LanguagePrimitives.EnumOfValue<byte, HandleKind> (byte (value &&& 0xFF000000 >>> 24)) with
        | HandleKind.UserString -> StringToken.UserString (MetadataTokens.UserStringHandle value)
        | HandleKind.String -> StringToken.String (MetadataTokens.StringHandle value)
        | v -> failwith $"Unrecognised string handle kind: {v}"
