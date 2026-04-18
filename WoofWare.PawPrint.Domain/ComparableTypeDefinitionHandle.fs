namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

[<CustomEquality>]
[<CustomComparison>]
type ComparableTypeDefinitionHandle =
    private
        {
            _Inner : TypeDefinitionHandle
        }

    override this.Equals other =
        match other with
        | :? ComparableTypeDefinitionHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparableTypeDefinitionHandle> with
        member this.CompareTo (other : ComparableTypeDefinitionHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparableTypeDefinitionHandle as other ->
                (this :> IComparable<ComparableTypeDefinitionHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    static member Make (h : TypeDefinitionHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
