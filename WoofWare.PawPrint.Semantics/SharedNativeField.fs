namespace WoofWare.PawPrint

/// An instance field of a non-generic, non-nested class in CoreLib, by its class's namespace and
/// name and its own name.
type CoreLibInstanceField =
    {
        /// The declaring class's namespace, such as `System.Reflection`.
        Namespace : string
        /// The declaring class's name within its namespace, such as `RuntimeMethodInfo`.
        TypeName : string
        /// The field's name, such as `m_handle`.
        FieldName : string
    }

    override this.ToString () : string =
        $"%s{this.Namespace}.%s{this.TypeName}::%s{this.FieldName}"

/// A field of one of CoreCLR's native object layouts which several managed fields of distinct
/// CoreLib classes are laid out as: the rows of the runtime's `vm/corelib.h` that bind a
/// `DEFINE_FIELD_U` of each of those classes to the same field of the same native class.
///
/// CoreCLR checks each such row against the class's real layout when it starts, so the managed
/// fields sit at one offset in every class listed, and CoreLib relies on that to read any of them
/// through a reference to an object of another of the classes (`Unsafe.As`), as .NET 11's
/// `IRuntimeMethodInfo.GetValue` does.
type SharedNativeField =
    {
        /// The native class, as `corelib.h` names it, such as `ReflectMethodObject`.
        NativeClass : string
        /// The native class's field, such as `m_pMD`.
        NativeField : string
        /// Every managed field laid out as that native field, at most one per class.
        ManagedFields : CoreLibInstanceField list
    }

[<RequireQualifiedAccess>]
module SharedNativeField =

    /// Every native field that `corelib.h` lays more than one managed field out as. The same on
    /// every supported runtime: `TestSharedNativeField` checks it against each runtime's own
    /// `corelib.h` and CoreLib.
    let all : SharedNativeField list =
        let field (ns : string) (typeName : string) (fieldName : string) : CoreLibInstanceField =
            {
                Namespace = ns
                TypeName = typeName
                FieldName = fieldName
            }

        [
            {
                NativeClass = "ReflectMethodObject"
                NativeField = "m_pMD"
                ManagedFields =
                    [
                        field "System.Reflection" "RuntimeConstructorInfo" "m_handle"
                        field "System" "RuntimeMethodInfoStub" "m_value"
                        field "System.Reflection" "RuntimeMethodInfo" "m_handle"
                    ]
            }
            {
                NativeClass = "ReflectFieldObject"
                NativeField = "m_pFD"
                ManagedFields =
                    [
                        field "System.Reflection" "RtFieldInfo" "m_fieldHandle"
                        field "System" "RuntimeFieldInfoStub" "m_fieldHandle"
                    ]
            }
        ]

    /// The field of the class `receiverNamespace.receiverTypeName` which is laid out as the same
    /// native field as `requested`, when that class is not `requested`'s own; `None` if
    /// `corelib.h` ties no field of that class to `requested`.
    let counterpartOn
        (requested : CoreLibInstanceField)
        (receiverNamespace : string)
        (receiverTypeName : string)
        : CoreLibInstanceField option
        =
        if requested.Namespace = receiverNamespace && requested.TypeName = receiverTypeName then
            None
        else
            all
            |> List.tryFind (fun shared -> List.contains requested shared.ManagedFields)
            |> Option.bind (fun shared ->
                shared.ManagedFields
                |> List.tryFind (fun field -> field.Namespace = receiverNamespace && field.TypeName = receiverTypeName)
            )
