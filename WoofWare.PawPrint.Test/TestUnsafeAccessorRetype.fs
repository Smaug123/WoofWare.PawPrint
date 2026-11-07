namespace WoofWare.PawPrint.Test

open System.Reflection.Metadata
open FsCheck
open FsCheck.FSharp
open FsUnitTyped
open NUnit.Framework
open WoofWare.PawPrint

/// `UnsafeAccessorDispatch.retypeSignature` against a direct transcription of
/// `UpdateDeclarationSigWithTypes` (unsafeaccessors.cpp): walk the positions return-first, refuse a
/// retyped byref return as `NotSupportedException`, refuse any other retyping of a position not
/// declared `object` (or `ref object`, for a byref named type) as `BadImageFormatException`, and
/// otherwise put the named type in place, keeping the position's leading custom modifiers.
///
/// The guests in `sourcesPure/UnsafeAccessorType*.cs` pin these refusals against real .NET one shape
/// at a time; this is the ordering between them, and the shapes C# cannot spell.
[<TestFixture>]
[<Parallelizable(ParallelScope.All)>]
module TestUnsafeAccessorRetype =

    let private objectType : ConcreteTypeHandle = ConcreteTypeHandle.Concrete 0
    let private int32Type : ConcreteTypeHandle = ConcreteTypeHandle.Concrete 1
    let private namedClass : ConcreteTypeHandle = ConcreteTypeHandle.Concrete 100

    /// Stands in for spelling a resolved type, which needs a type system; any injective spelling
    /// distinguishable from the declared types will do.
    let private spell (handle : ConcreteTypeHandle) : TypeDefn =
        match handle with
        | ConcreteTypeHandle.Byref _ -> TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.String)
        | _ -> TypeDefn.PrimitiveType PrimitiveType.String

    let private modified (unmodified : TypeDefn) : TypeDefn =
        TypeDefn.Modified
            {
                Unmodified = unmodified
                Modifier = TypeDefn.PrimitiveType PrimitiveType.Boolean
                IsRequired = false
            }

    let private object : TypeDefn = TypeDefn.PrimitiveType PrimitiveType.Object

    /// A declared position: how the signature spells it, and what it is over the accessor's
    /// instantiation.
    type private Declared =
        | Object
        | ModifiedObject
        | ByrefObject
        | ByrefModifiedObject
        | Int32
        | ByrefInt32
        /// `!0`, instantiated with `object`. CoreCLR reads the position with no type context, so
        /// it is a type variable there, not `object`.
        | TypeParameterAsObject

    let private spelledOf (declared : Declared) : TypeDefn =
        match declared with
        | Declared.Object -> object
        | Declared.ModifiedObject -> modified object
        | Declared.ByrefObject -> TypeDefn.Byref object
        | Declared.ByrefModifiedObject -> TypeDefn.Byref (modified object)
        | Declared.Int32 -> TypeDefn.PrimitiveType PrimitiveType.Int32
        | Declared.ByrefInt32 -> TypeDefn.Byref (TypeDefn.PrimitiveType PrimitiveType.Int32)
        | Declared.TypeParameterAsObject -> TypeDefn.GenericTypeParameter 0

    let private concreteOf (declared : Declared) : ConcreteTypeHandle =
        match declared with
        | Declared.Object
        | Declared.ModifiedObject
        | Declared.TypeParameterAsObject -> objectType
        | Declared.ByrefObject
        | Declared.ByrefModifiedObject -> ConcreteTypeHandle.Byref objectType
        | Declared.Int32 -> int32Type
        | Declared.ByrefInt32 -> ConcreteTypeHandle.Byref int32Type

    type private Case =
        {
            /// `None` is a `void` return.
            Return : Declared option
            Parameters : Declared list
            /// In Param-row order; a sequence number may repeat.
            Retyped : (int * ConcreteTypeHandle) list
        }

    [<RequireQualifiedAccess>]
    type private Outcome =
        | Retyped
        | NotSupported
        | BadImageFormat

    let private signatures (case : Case) : TypeMethodSignature<TypeDefn> * TypeMethodSignature<ConcreteTypeHandle> =
        let header =
            SignatureHeader (SignatureKind.Method, SignatureCallingConvention.Default, SignatureAttributes.None)
            |> ComparableSignatureHeader.Make

        let spelled : TypeMethodSignature<TypeDefn> =
            {
                Header = header
                ParameterTypes = case.Parameters |> List.map spelledOf
                GenericParameterCount = 0
                RequiredParameterCount = case.Parameters.Length
                ReturnType =
                    match case.Return with
                    | None -> MethodReturnType.Void
                    | Some declared -> MethodReturnType.Returns (spelledOf declared)
            }

        let concrete : TypeMethodSignature<ConcreteTypeHandle> =
            {
                Header = header
                ParameterTypes = case.Parameters |> List.map concreteOf
                GenericParameterCount = 0
                RequiredParameterCount = case.Parameters.Length
                ReturnType =
                    match case.Return with
                    | None -> MethodReturnType.Void
                    | Some declared -> MethodReturnType.Returns (concreteOf declared)
            }

        spelled, concrete

    /// The model: what CoreCLR does with this case, position by position.
    let private expected (case : Case) : Result<AccessorSignature, Outcome> =
        let retyped = Map.ofList case.Retyped

        let declaredAt (position : int) : Declared option =
            if position = 0 then
                case.Return
            else
                Some case.Parameters.[position - 1]

        let isByref (declared : Declared option) : bool =
            match declared with
            | Some (Declared.ByrefObject | Declared.ByrefModifiedObject | Declared.ByrefInt32) -> true
            | _ -> false

        let valid (declared : Declared option) (named : ConcreteTypeHandle) : bool =
            match named, declared with
            | ConcreteTypeHandle.Byref _, Some (Declared.ByrefObject | Declared.ByrefModifiedObject) -> true
            | ConcreteTypeHandle.Byref _, _ -> false
            | _, Some (Declared.Object | Declared.ModifiedObject) -> true
            | _ -> false

        let refusal =
            [ 0 .. case.Parameters.Length ]
            |> List.tryPick (fun position ->
                match Map.tryFind position retyped with
                | None -> None
                | Some named ->
                    let declared = declaredAt position

                    if position = 0 && isByref declared then
                        Some Outcome.NotSupported
                    elif not (valid declared named) then
                        Some Outcome.BadImageFormat
                    else
                        None
            )

        match refusal with
        | Some outcome -> Error outcome
        | None ->

        let spelledDeclared, concreteDeclared = signatures case

        let retypeSpelled (position : int) (spelled : TypeDefn) : TypeDefn =
            match Map.tryFind position retyped, declaredAt position with
            | None, _ -> spelled
            | Some named, Some Declared.ModifiedObject -> modified (spell named)
            | Some named, _ -> spell named

        let retypeConcrete (position : int) (concrete : ConcreteTypeHandle) : ConcreteTypeHandle =
            Map.tryFind position retyped |> Option.defaultValue concrete

        Ok
            {
                Spelled =
                    { spelledDeclared with
                        ParameterTypes =
                            spelledDeclared.ParameterTypes |> List.mapi (fun i t -> retypeSpelled (i + 1) t)
                        ReturnType =
                            match spelledDeclared.ReturnType with
                            | MethodReturnType.Void -> MethodReturnType.Void
                            | MethodReturnType.Returns t -> MethodReturnType.Returns (retypeSpelled 0 t)
                    }
                Concrete =
                    { concreteDeclared with
                        ParameterTypes =
                            concreteDeclared.ParameterTypes
                            |> List.mapi (fun i t -> retypeConcrete (i + 1) t)
                        ReturnType =
                            match concreteDeclared.ReturnType with
                            | MethodReturnType.Void -> MethodReturnType.Void
                            | MethodReturnType.Returns t -> MethodReturnType.Returns (retypeConcrete 0 t)
                    }
                Retyped = retyped
            }

    let private caseGen : Gen<Case> =
        // Weighted towards `object`, so that a good share of cases retype successfully.
        let declaredGen =
            Gen.frequency
                [
                    6, Gen.constant Declared.Object
                    2, Gen.constant Declared.ModifiedObject
                    3, Gen.constant Declared.ByrefObject
                    1, Gen.constant Declared.ByrefModifiedObject
                    1, Gen.constant Declared.Int32
                    1, Gen.constant Declared.ByrefInt32
                    1, Gen.constant Declared.TypeParameterAsObject
                ]

        gen {
            let! returnType = Gen.frequency [ 1, Gen.constant None ; 4, Gen.map Some declaredGen ]
            let! parameterCount = Gen.choose (0, 4)
            let! parameters = Gen.listOfLength parameterCount declaredGen
            let! rowCount = Gen.choose (0, 3)

            let! rows =
                Gen.listOfLength
                    rowCount
                    (Gen.zip
                        (Gen.choose (0, parameterCount))
                        (Gen.elements [ namedClass ; namedClass ; ConcreteTypeHandle.Byref namedClass ]))

            return
                {
                    Return = returnType
                    Parameters = parameters
                    Retyped = rows
                }
        }

    [<Test>]
    let ``retypeSignature agrees with the model of UpdateDeclarationSigWithTypes`` () =
        let property (cover : Outcome -> unit) (case : Case) : unit =
            let spelled, concrete = signatures case

            let actual =
                UnsafeAccessorDispatch.retypeSignature objectType spell spelled concrete case.Retyped
                |> Result.mapError (fun refusal ->
                    match refusal with
                    | UnsafeAccessorRefusal.NotSupported _ -> Outcome.NotSupported
                    | UnsafeAccessorRefusal.BadImageFormat _ -> Outcome.BadImageFormat
                    | other -> failwith $"retypeSignature refused with %A{other}, which it never should"
                )

            let expected = expected case

            match expected with
            | Ok _ -> cover Outcome.Retyped
            | Error outcome -> cover outcome

            actual |> shouldEqual expected

        let coverage =
            CoverageSample.check (Config.QuickThrowOnFailure.WithMaxTest 1000) (Arb.fromGen caseGen) property

        coverage.Count Outcome.Retyped |> shouldBeGreaterThan 200
        coverage.Count Outcome.NotSupported |> shouldBeGreaterThan 30
        coverage.Count Outcome.BadImageFormat |> shouldBeGreaterThan 100

    [<Test>]
    let ``a retyped byref return is refused before a retyped parameter is examined`` () =
        let case =
            {
                Return = Some Declared.ByrefObject
                Parameters = [ Declared.Int32 ]
                Retyped = [ 1, namedClass ; 0, namedClass ]
            }

        let spelled, concrete = signatures case

        match UnsafeAccessorDispatch.retypeSignature objectType spell spelled concrete case.Retyped with
        | Error (UnsafeAccessorRefusal.NotSupported _) -> ()
        | other -> failwith $"expected NotSupported, got %A{other}"
