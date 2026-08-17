namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

[<CustomEquality>]
[<CustomComparison>]
type ComparableTypeReferenceHandle =
    private
        {
            _Inner : TypeReferenceHandle
        }

    override this.Equals other =
        match other with
        | :? ComparableTypeReferenceHandle as other -> this._Inner.GetHashCode () = other._Inner.GetHashCode ()
        | _ -> false

    override this.GetHashCode () : int = this._Inner.GetHashCode ()

    interface IComparable<ComparableTypeReferenceHandle> with
        member this.CompareTo (other : ComparableTypeReferenceHandle) : int =
            this._Inner.GetHashCode().CompareTo (other._Inner.GetHashCode ())

    interface IComparable with
        member this.CompareTo (other : obj) : int =
            match other with
            | :? ComparableTypeReferenceHandle as other ->
                (this :> IComparable<ComparableTypeReferenceHandle>).CompareTo other
            | _ -> failwith "invalid comparison"

    /// `TypeReferenceHandle` inherits `ToString` from `obj`, so rendering one yields the literal
    /// text "System.Reflection.Metadata.TypeReferenceHandle" -- which makes every diagnostic that
    /// names a type reference useless for telling two references apart. Render the metadata token
    /// instead, in the `0x01######` form ildasm and the ECMA-335 tables use.
    override this.ToString () : string =
        let token =
            MetadataTokens.GetToken (TypeReferenceHandle.op_Implicit this._Inner : EntityHandle)

        $"TypeRef(0x%08x{token})"

    static member Make (h : TypeReferenceHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
