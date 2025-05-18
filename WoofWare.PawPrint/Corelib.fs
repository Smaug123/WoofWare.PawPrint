namespace WoofWare.PawPrint

[<RequireQualifiedAccess>]
module Corelib =

    let getBaseTypes (corelib : DumpedAssembly) : BaseClassTypes<DumpedAssembly> =
        let stringType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "String" then Some v else None)
            |> Seq.exactlyOne

        let arrayType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Array" then Some v else None)
            |> Seq.exactlyOne

        let enumType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Enum" then Some v else None)
            |> Seq.exactlyOne

        let objType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "Object" then Some v else None)
            |> Seq.exactlyOne

        let valueType =
            corelib.TypeDefs
            |> Seq.choose (fun (KeyValue (_, v)) -> if v.Name = "ValueType" then Some v else None)
            |> Seq.exactlyOne

        {
            Corelib = corelib
            String = stringType
            Array = arrayType
            Enum = enumType
            ValueType = valueType
            Object = objType
        }
