namespace WoofWare.PawPrint

open System
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335

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

    /// `FieldDefinitionHandle` inherits `ToString` from `obj`, so rendering one yields the literal
    /// text "System.Reflection.Metadata.FieldDefinitionHandle" -- which makes every diagnostic
    /// that names a field identity useless for telling two fields apart. Render the metadata
    /// token instead, in the `0x04######` form ildasm and the ECMA-335 tables use.
    override this.ToString () : string =
        let token =
            MetadataTokens.GetToken (FieldDefinitionHandle.op_Implicit this._Inner : EntityHandle)

        $"Field(0x%08x{token})"

    static member Make (h : FieldDefinitionHandle) =
        {
            _Inner = h
        }

    member this.Get = this._Inner
