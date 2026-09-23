namespace WoofWare.PawPrint

open System
open System.Globalization
open System.Security.Cryptography
open System.Text

/// The identity of a method's IL body: sixteen lowercase hex digits of the SHA-256 of
/// `IlBodyFingerprint.canonicalText`.
///
/// A body with different instructions, operands, locals, exception regions or signature has a
/// different fingerprint (up to the hash's collisions). Metadata tokens are rendered by what they
/// name rather than by their row numbers, so a rebuild of the same source that renumbers the
/// image's tables keeps the fingerprint. The exception is a token kind `IlFormatting` does not
/// name, which renders by its row — in practice only `calli`'s standalone signature — so a
/// renumbering can change such a body's fingerprint, and the gate then refuses it until it is
/// reviewed again.
type IlBodyFingerprint =
    private
    | IlBodyFingerprint of string

    /// The sixteen lowercase hex digits.
    member this.Hex : string =
        match this with
        | IlBodyFingerprint hex -> hex

    override this.ToString () : string = this.Hex

    /// Parse a fingerprint written out in full, as `Hex` renders it. Anything else is refused.
    static member OfHex (hex : string) : IlBodyFingerprint =
        let isLowerHex (c : char) : bool =
            (c >= '0' && c <= '9') || (c >= 'a' && c <= 'f')

        if hex.Length <> 16 || not (Seq.forall isLowerHex hex) then
            failwith $"%s{hex} is not an IL body fingerprint: expected sixteen lowercase hex digits"

        IlBodyFingerprint hex

[<RequireQualifiedAccess>]
module IlBodyFingerprint =

    /// The operand text of a MethodDef, with the signature `IlFormatting.formatMetadataToken`
    /// leaves out: it renders a MethodDef as `Type::Name`, under which a call's move between two
    /// overloads would not show.
    let private methodDefSignature
        (assembly : DumpedAssembly)
        (handle : System.Reflection.Metadata.MethodDefinitionHandle)
        : string
        =
        match assembly.Methods.TryGetValue handle with
        | true, callee ->
            let calleeScope = GenericScope.ofMethod callee

            let parameters =
                callee.Signature.ParameterTypes
                |> List.map (IlFormatting.renderTypeDefn assembly calleeScope)
                |> String.concat ", "

            let returns =
                IlFormatting.renderMethodReturnType assembly calleeScope callee.Signature.ReturnType

            $"`%d{callee.Signature.GenericParameterCount}(%s{parameters}) : %s{returns}"
        | false, _ -> ""

    let private renderToken (assembly : DumpedAssembly) (scope : GenericScope) (token : MetadataToken) : string =
        let named = IlFormatting.formatMetadataToken assembly scope token

        match token with
        | MetadataToken.MethodDef handle -> named + methodDefSignature assembly handle
        | MetadataToken.MethodSpecification handle ->
            match assembly.MethodSpecs.TryGetValue handle with
            | true, spec ->
                match spec.Method with
                | MetadataToken.MethodDef generic -> named + methodDefSignature assembly generic
                | _ -> named
            | false, _ -> named
        | _ -> named

    let private invariant (i : int) : string = i.ToString CultureInfo.InvariantCulture

    let private renderOp (assembly : DumpedAssembly) (scope : GenericScope) (op : IlOp) : string =
        match op with
        | IlOp.Nullary op -> op.ToString ()
        // `UnaryConstIlOp.ToString` prints floating-point operands with `%f`, which keeps six
        // decimal places; the bit pattern is the operand itself.
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R4 f) -> $"Ldc_R4 0x%08X{BitConverter.SingleToInt32Bits f}"
        | IlOp.UnaryConst (UnaryConstIlOp.Ldc_R8 f) -> $"Ldc_R8 0x%016X{BitConverter.DoubleToInt64Bits f}"
        | IlOp.UnaryConst op -> op.ToString ()
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromMetadata token) ->
            $"%O{op} %s{renderToken assembly scope token.Token}"
        | IlOp.UnaryMetadataToken (op, MetadataOperand.FromDynamicScope index) ->
            $"%O{op} DynamicScope[%s{invariant index}]"
        | IlOp.UnaryStringToken (op, StringOperand.FromMetadata token) ->
            $"%O{op} \"%s{IlFormatting.escapeStringLiteral (assembly.Strings token.Token)}\""
        | IlOp.UnaryStringToken (op, StringOperand.FromDynamicScope index) ->
            $"%O{op} DynamicScope[%s{invariant index}]"
        | IlOp.Switch targets -> "Switch " + (targets |> Seq.map invariant |> String.concat ",")

    let private renderRegion (assembly : DumpedAssembly) (scope : GenericScope) (region : ExceptionRegion) : string =
        let offsets (o : ExceptionOffset) : string =
            $"try %s{invariant o.TryOffset}+%s{invariant o.TryLength} handler %s{invariant o.HandlerOffset}+%s{invariant o.HandlerLength}"

        match region with
        | ExceptionRegion.Catch (ExceptionCatchType.FromMetadata token, o) ->
            $"catch %s{renderToken assembly scope token} %s{offsets o}"
        | ExceptionRegion.Catch (ExceptionCatchType.FromDynamicScope index, o) ->
            $"catch DynamicScope[%s{invariant index}] %s{offsets o}"
        | ExceptionRegion.Filter (filterOffset, o) -> $"filter %s{invariant filterOffset} %s{offsets o}"
        | ExceptionRegion.Finally o -> $"finally %s{offsets o}"
        | ExceptionRegion.Fault o -> $"fault %s{offsets o}"

    /// The text the fingerprint hashes: `method`'s signature, locals, instructions (each at its
    /// offset) and exception regions, every token rendered by what it names. `None` when the
    /// method has no IL body.
    ///
    /// Exposed so that a mismatch can be diagnosed by comparing two renderings line by line.
    let canonicalText
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : string option
        =
        match method.Body with
        | MethodBody.Il body ->
            let scope = GenericScope.ofMethod method

            let parameters =
                method.Signature.ParameterTypes
                |> List.map (IlFormatting.renderTypeDefn assembly scope)
                |> String.concat ", "

            let returns =
                IlFormatting.renderMethodReturnType assembly scope method.Signature.ReturnType

            let lines =
                [
                    yield
                        $"signature static=%b{method.IsStatic} `%d{method.Signature.GenericParameterCount}(%s{parameters}) : %s{returns}"
                    yield $"localsinit=%b{body.LocalsInit}"

                    match body.LocalVars with
                    | None -> ()
                    | Some locals ->
                        for local in locals do
                            yield $"local %s{IlFormatting.renderTypeDefn assembly scope local}"

                    for op, offset in body.Instructions do
                        yield $"%s{invariant offset} %s{renderOp assembly scope op}"

                    for region in body.ExceptionRegions do
                        yield renderRegion assembly scope region
                ]

            Some (String.concat "\n" lines)
        | MethodBody.InternalCall
        | MethodBody.PInvoke
        | MethodBody.RuntimeProvided _
        | MethodBody.Abstract -> None

    /// `method`'s IL body fingerprint, or `None` when it has no IL body.
    let ofMethod
        (assembly : DumpedAssembly)
        (method : MethodInfo<GenericParamFromMetadata, GenericParamFromMetadata, TypeDefn>)
        : IlBodyFingerprint option
        =
        canonicalText assembly method
        |> Option.map (fun text ->
            let hash = SHA256.HashData (Encoding.UTF8.GetBytes text)
            Convert.ToHexString(hash, 0, 8).ToLowerInvariant () |> IlBodyFingerprint.OfHex
        )
