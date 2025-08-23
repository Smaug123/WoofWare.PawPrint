namespace WoofWare.PawPrint

open System
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
            /// Do not use this, because it's intended to be private; use the accessor `.Assembly : AssemblyName`
            /// instead.
            _AssemblyName : AssemblyName
            /// Do not use this, because it's intended to be private; use the accessor `.Definition` instead.
            _Definition : ComparableTypeDefinitionHandle
            /// Do not use this, because it's intended to be private; use the accessor `.Name` instead.
            _Name : string
            /// Do not use this, because it's intended to be private; use the accessor `.Namespace` instead.
            _Namespace : string
            /// Do not use this, because it's intended to be private; use the accessor `.Generics` instead.
            _Generics : ImmutableArray<'typeGeneric>
        }

    member this.Assembly : AssemblyName = this._AssemblyName
    member this.Definition : ComparableTypeDefinitionHandle = this._Definition
    member this.Generics : 'typeGeneric ImmutableArray = this._Generics
    member this.Name = this._Name
    member this.Namespace = this._Namespace

    override this.Equals (other : obj) : bool =
        match other with
        | :? ConcreteType<'typeGeneric> as other ->
            this._Generics = other._Generics
            && this._Definition = other._Definition
            && this._AssemblyName.FullName = other._AssemblyName.FullName
        | _ -> false

    override this.GetHashCode () : int =
        hash (this._AssemblyName.FullName, this._Definition, this._Generics)

[<RequireQualifiedAccess>]
module ConcreteType =
    let make
        (assemblyName : AssemblyName)
        (defn : TypeDefinitionHandle)
        (ns : string)
        (name : string)
        (generics : GenericParamFromMetadata ImmutableArray)
        : ConcreteType<GenericParamFromMetadata>
        =
        {
            _AssemblyName = assemblyName
            _Definition = ComparableTypeDefinitionHandle.Make defn
            _Name = name
            _Namespace = ns
            _Generics = generics
        }

    let mapGeneric<'a, 'b> (f : int -> 'a -> 'b) (x : ConcreteType<'a>) : ConcreteType<'b> =
        let generics = x._Generics |> ImmutableArray.mapi f

        {
            _AssemblyName = x._AssemblyName
            _Definition = x._Definition
            _Generics = generics
            _Name = x._Name
            _Namespace = x._Namespace
        }
