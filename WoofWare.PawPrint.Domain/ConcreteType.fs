namespace WoofWare.PawPrint

open System.Collections.Immutable
open System.Reflection
open System.Reflection.Metadata

type FakeUnit = private | FakeUnit

[<RequireQualifiedAccess>]
module FakeUnit =
    let ofUnit () = FakeUnit.FakeUnit

    let toUnit (f : FakeUnit) =
        match f with
        | FakeUnit.FakeUnit -> ()

/// A type which has been concretised, runtime-representable, etc.
[<CustomEquality>]
[<NoComparison>]
type ConcreteType<'typeGeneric> =
    private
        {
            /// Do not use this directly; use the accessor `.Identity` instead.
            _Identity : ResolvedTypeIdentity
            /// Temporary cached data for compatibility with existing callers.
            _AssemblyName : AssemblyName
            /// Temporary cached data for compatibility with existing callers.
            _Definition : ComparableTypeDefinitionHandle
            /// Do not use this, because it's intended to be private; use the accessor `.Name` instead.
            _Name : string
            /// Do not use this, because it's intended to be private; use the accessor `.Namespace` instead.
            _Namespace : string
            /// Do not use this, because it's intended to be private; use the accessor `.Generics` instead.
            _Generics : ImmutableArray<'typeGeneric>
        }

    override this.ToString () : string =
        let basic = $"%s{this.Assembly.Name}.%s{this.Namespace}.%s{this.Name}"

        let generics =
            if this.Generics.IsEmpty then
                ""
            else
                this.Generics
                |> Seq.map string
                |> String.concat ", "
                |> fun x -> "<" + x + ">"

        basic + generics

    member this.Identity : ResolvedTypeIdentity = this._Identity
    member this.Assembly : AssemblyName = this._Identity.Assembly
    member this.Definition : ComparableTypeDefinitionHandle = this._Identity.TypeDefinition
    member this.Generics : ImmutableArray<'typeGeneric> = this._Generics
    member this.Name = this._Name
    member this.Namespace = this._Namespace

    override this.Equals (other : obj) : bool =
        match other with
        | :? ConcreteType<'typeGeneric> as other -> this._Generics = other._Generics && this._Identity = other._Identity
        | _ -> false

    override this.GetHashCode () : int = hash (this._Identity, this._Generics)

[<RequireQualifiedAccess>]
module ConcreteType =
    let make
        (assemblyName : AssemblyName)
        (defn : TypeDefinitionHandle)
        (ns : string)
        (name : string)
        (genericParam : ImmutableArray<GenericParamFromMetadata>)
        : ConcreteType<GenericParamFromMetadata>
        =
        {
            _Identity = ResolvedTypeIdentity.ofTypeDefinition assemblyName defn
            _AssemblyName = assemblyName
            _Definition = ComparableTypeDefinitionHandle.Make defn
            _Name = name
            _Namespace = ns
            _Generics = genericParam
        }

    let mapGeneric<'a, 'b> (f : int -> 'a -> 'b) (x : ConcreteType<'a>) : ConcreteType<'b> =
        let generics = x._Generics |> Seq.mapi f |> ImmutableArray.CreateRange

        {
            _Identity = x._Identity
            _AssemblyName = x._AssemblyName
            _Definition = x._Definition
            _Generics = generics
            _Name = x._Name
            _Namespace = x._Namespace
        }

    let makeFromIdentity
        (identity : ResolvedTypeIdentity)
        (ns : string)
        (name : string)
        (genericParam : ImmutableArray<'generic>)
        : ConcreteType<'generic>
        =
        {
            _Identity = identity
            _AssemblyName = identity.Assembly
            _Definition = identity.TypeDefinition
            _Name = name
            _Namespace = ns
            _Generics = genericParam
        }
