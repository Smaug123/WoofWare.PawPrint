namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata

[<CustomEquality>]
[<CustomComparison>]
type ComparableMethodDefinitionHandle =
    private
        {
            _Inner : MethodDefinitionHandle
        }

    override this.Equals other =
        match other with
        | :? ComparableMethodDefinitionHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparableMethodDefinitionHandle> with
        member this.CompareTo (other : ComparableMethodDefinitionHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparableMethodDefinitionHandle as other ->
                (this :> IComparable<ComparableMethodDefinitionHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    static member Make (h : MethodDefinitionHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
