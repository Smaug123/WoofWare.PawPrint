namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open Microsoft.Extensions.Logging

/// A method instantiated for execution: its declaring type and its own generic parameters bound to
/// concrete types, and its signature and locals concretised against them. Whatever loads an
/// assembly or registers a concrete type on the way returns the state it leaves behind;
/// `dotnetRuntimeDirs` is where the loader looks for an assembly not yet loaded.
[<RequireQualifiedAccess>]
module MethodConcretisation =

    let concretizeMethodWithAllGenerics
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState *
          Result<
              WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
              ConcreteTypeHandle,
              TypeBindingFailure
           >
        =
        // `Concretization.concretizeMethod` reads the definition's metadata, the two handle
        // lists, and of the declaring type only its identity and its generic *arity*; so that
        // is the key. A dynamic method has no declaring type and is not memoised.
        let key : ConcreteMethodKey option =
            match methodToCall.Owner with
            | MethodOwner.DynamicMethodsClass _ -> None
            | MethodOwner.DeclaredOn declaringType ->
                let row, synthesised = methodToCall.IdentityKey

                Some
                    {
                        DeclaringType = declaringType.Identity
                        MethodRow =
                            row
                            |> Option.map (fun h ->
                                MetadataTokens.GetRowNumber (MethodDefinitionHandle.op_Implicit h : EntityHandle)
                            )
                        Synthesised = synthesised
                        TypeGenerics = List.ofSeq typeGenerics
                        MethodGenerics = List.ofSeq methodGenerics
                    }

        match key |> Option.bind (fun key -> Map.tryFind key state._ConcretisedMethods) with
        | Some hit -> state, Ok (hit.Method, hit.DeclaringTypeHandle)
        | None ->

        let concretizedMethod, newConcreteTypes, newAssemblies =
            Concretization.concretizeMethod
                state.ConcreteTypes
                (TypeResolution.directoryLoader loggerFactory dotnetRuntimeDirs)
                state._LoadedAssemblies
                baseClassTypes
                methodToCall
                typeGenerics
                methodGenerics

        let state =
            { state with
                ConcreteTypes = newConcreteTypes
                _LoadedAssemblies = newAssemblies
            }

        // A failure is not memoised: binding the method again fails the same way.
        match concretizedMethod with
        | Error failure -> state, Error failure
        | Ok concretizedMethod ->

        let declaringTypeHandle =
            match
                AllConcreteTypes.findExistingConcreteType
                    state.ConcreteTypes
                    concretizedMethod.RequiredDeclaringType.Identity
                    concretizedMethod.DeclaringTypeGenerics
            with
            | Some handle -> handle
            | None -> failwith "Concretized method's declaring type not found in ConcreteTypes"

        let state =
            match key with
            | None -> state
            | Some key ->
                state.WithConcretisedMethod
                    key
                    {
                        Method = concretizedMethod
                        DeclaringTypeHandle = declaringTypeHandle
                    }

        state, Ok (concretizedMethod, declaringTypeHandle)

    let concretizeMethodWithTypeGenerics
        (loggerFactory : ILoggerFactory)
        (dotnetRuntimeDirs : string seq)
        (baseClassTypes : BaseClassTypes<DumpedAssembly>)
        (typeGenerics : ImmutableArray<ConcreteTypeHandle>)
        (methodToCall : WoofWare.PawPrint.MethodInfo<'ty, GenericParamFromMetadata, TypeDefn>)
        (methodGenerics : TypeDefn ImmutableArray option)
        (callingAssemblyFullName : string)
        (currentExecutingMethodGenerics : ImmutableArray<ConcreteTypeHandle>)
        (state : TypeSystemState)
        : TypeSystemState *
          Result<
              WoofWare.PawPrint.MethodInfo<ConcreteTypeHandle, ConcreteTypeHandle, ConcreteTypeHandle> *
              ConcreteTypeHandle,
              TypeBindingFailure
           >
        =

        // Concretize method generics if any, stopping at the first that does not bind
        let rec concretizeGenerics
            (state : TypeSystemState)
            (handles : ConcreteTypeHandle list)
            (generics : TypeDefn list)
            : TypeSystemState * Result<ImmutableArray<ConcreteTypeHandle>, TypeBindingFailure>
            =
            match generics with
            | [] -> state, Ok (handles |> List.rev |> ImmutableArray.CreateRange)
            | generic :: rest ->
                match
                    TypeSystemState.concretizeType
                        loggerFactory
                        dotnetRuntimeDirs
                        baseClassTypes
                        state
                        callingAssemblyFullName
                        typeGenerics
                        currentExecutingMethodGenerics
                        generic
                with
                | state, Ok handle -> concretizeGenerics state (handle :: handles) rest
                | state, Error failure -> state, Error failure

        match concretizeGenerics state [] (methodGenerics |> Option.map List.ofSeq |> Option.defaultValue []) with
        | state, Error failure -> state, Error failure
        | state, Ok concretizedMethodGenerics ->

        // Now concretize the entire method
        concretizeMethodWithAllGenerics
            loggerFactory
            dotnetRuntimeDirs
            baseClassTypes
            typeGenerics
            methodToCall
            concretizedMethodGenerics
            state
