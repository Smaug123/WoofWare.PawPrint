namespace WoofWare.PawPrint

/// An `EmitScope` serialised as an ECMA-335 image.
[<RequireQualifiedAccess>]
module EmitScopeImage =
    /// The PE image holding the scope's metadata.
    let write (scope : EmitScope) : byte[] =
        ignore<EmitScope> scope
        failwith "TODO: EmitScopeImage.write is not implemented"
