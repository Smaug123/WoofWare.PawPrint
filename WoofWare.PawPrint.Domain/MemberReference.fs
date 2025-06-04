namespace WoofWare.PawPrint

open System
open System.Reflection
open System.Reflection.Metadata

type MemberSignature =
    | Field of TypeDefn
    | Method of TypeMethodSignature<TypeDefn>

type MemberReference<'parent> =
    {
        Name : StringToken
        PrettyName : string
        Parent : 'parent
        Signature : MemberSignature
    }

type MemberRefSigSwitch =
    | Default
    | Field
    | VarArg
    | Generic

    static member Identify (b : byte) =
        match b &&& 0xFuy with
        | 0uy -> MemberRefSigSwitch.Default
        | 5uy -> MemberRefSigSwitch.VarArg
        | 6uy -> MemberRefSigSwitch.Field
        | 0x10uy -> MemberRefSigSwitch.Generic
        | n -> failwith $"Bad member ref sig: %i{n}"

[<RequireQualifiedAccess>]
module MemberReference =
    let make<'parent>
        (blobReader : BlobHandle -> BlobReader)
        (getString : StringHandle -> string)
        (makeParent : EntityHandle -> 'parent)
        (assemblyName : AssemblyName)
        (mr : System.Reflection.Metadata.MemberReference)
        : MemberReference<'parent>
        =
        let name = StringToken.String mr.Name

        let br = blobReader mr.Signature
        let header = br.ReadSignatureHeader ()

        let signature =
            match header.Kind with
            | SignatureKind.Method -> mr.DecodeMethodSignature (TypeDefn.typeProvider assemblyName, ()) |> Choice1Of2
            | SignatureKind.Field -> mr.DecodeFieldSignature (TypeDefn.typeProvider assemblyName, ()) |> Choice2Of2
            | SignatureKind.LocalVariables -> failwith "TODO: LocalVariables"
            | SignatureKind.Property -> failwith "TODO: Property"
            | SignatureKind.MethodSpecification -> failwith "TODO: MethodSpec"
            | i -> raise (ArgumentOutOfRangeException $"{i}")

        let signature =
            match signature with
            | Choice1Of2 methodSignature -> TypeMethodSignature.make methodSignature |> MemberSignature.Method
            | Choice2Of2 typeDefn -> MemberSignature.Field typeDefn

        {
            Name = name
            PrettyName = getString mr.Name
            // Horrible abuse to get this as an int
            Parent = makeParent mr.Parent
            Signature = signature
        }
