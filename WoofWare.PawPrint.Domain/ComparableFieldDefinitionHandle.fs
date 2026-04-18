namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

[<CustomEquality>]
[<CustomComparison>]
type ComparableFieldDefinitionHandle =
    private
        {
            _Inner : FieldDefinitionHandle
        }

    override this.Equals other =
        match other with
        | :? ComparableFieldDefinitionHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparableFieldDefinitionHandle> with
        member this.CompareTo (other : ComparableFieldDefinitionHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparableFieldDefinitionHandle as other ->
                (this :> IComparable<ComparableFieldDefinitionHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    static member Make (h : FieldDefinitionHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
