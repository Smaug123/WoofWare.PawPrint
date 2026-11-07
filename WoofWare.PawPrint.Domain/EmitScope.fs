namespace WoofWare.PawPrint

open System
open System.Collections.Generic
open System.Collections.Immutable
open System.Reflection
open System.Text

/// An offset into an emit scope's `#Strings` heap. Offset 0 is the empty string.
[<Struct>]
type StringOffset =
    internal
    | StringOffset of int

    member this.Offset : int =
        match this with
        | StringOffset offset -> offset

/// An offset into an emit scope's `#Blob` heap. Offset 0 is the empty blob.
[<Struct>]
type BlobOffset =
    internal
    | BlobOffset of int

    member this.Offset : int =
        match this with
        | BlobOffset offset -> offset

/// A 1-based index into an emit scope's `#GUID` heap.
[<Struct>]
type GuidIndex =
    internal
    | GuidIndex of int

    member this.Index : int =
        match this with
        | GuidIndex index -> index

/// The RID of an emit scope's `AssemblyRef` row.
[<Struct>]
type AssemblyRefRid =
    internal
    | AssemblyRefRid of int

    member this.Rid : int =
        match this with
        | AssemblyRefRid rid -> rid

/// The RID of an emit scope's `TypeRef` row.
[<Struct>]
type TypeRefRid =
    internal
    | TypeRefRid of int

    member this.Rid : int =
        match this with
        | TypeRefRid rid -> rid

/// The RID of an emit scope's `TypeDef` row. RID 1 is `<Module>`.
[<Struct>]
type TypeDefRid =
    internal
    | TypeDefRid of int

    member this.Rid : int =
        match this with
        | TypeDefRid rid -> rid

/// The RID of an emit scope's `Field` row: its token, in definition order.
[<Struct>]
type FieldRid =
    internal
    | FieldRid of int

    member this.Rid : int =
        match this with
        | FieldRid rid -> rid

/// The RID of an emit scope's `MethodDef` row: its token, in definition order.
[<Struct>]
type MethodDefRid =
    internal
    | MethodDefRid of int

    member this.Rid : int =
        match this with
        | MethodDefRid rid -> rid

/// The RID of an emit scope's `Param` row: its token, in definition order.
[<Struct>]
type ParamRid =
    internal
    | ParamRid of int

    member this.Rid : int =
        match this with
        | ParamRid rid -> rid

/// The RID of an emit scope's `MemberRef` row.
[<Struct>]
type MemberRefRid =
    internal
    | MemberRefRid of int

    member this.Rid : int =
        match this with
        | MemberRefRid rid -> rid

/// The RID of an emit scope's `StandAloneSig` row.
[<Struct>]
type StandAloneSigRid =
    internal
    | StandAloneSigRid of int

    member this.Rid : int =
        match this with
        | StandAloneSigRid rid -> rid

/// ECMA-335 II.24.2.6's `ResolutionScope` coded index, for the targets an emit scope can hold.
[<RequireQualifiedAccess>]
type ResolutionScope =
    /// This module: the type is defined here.
    | Module
    | AssemblyRef of AssemblyRefRid
    /// The type is nested in the referenced type.
    | TypeRef of TypeRefRid

/// ECMA-335 II.24.2.6's `TypeDefOrRef` coded index, for the targets an emit scope can hold.
[<RequireQualifiedAccess>]
type TypeDefOrRef =
    | TypeDef of TypeDefRid
    | TypeRef of TypeRefRid

/// ECMA-335 II.24.2.6's `MemberRefParent` coded index, for the targets an emit scope can hold.
[<RequireQualifiedAccess>]
type MemberRefParent =
    | TypeDef of TypeDefRid
    | TypeRef of TypeRefRid
    /// A vararg call site's signature, naming the method it calls.
    | MethodDef of MethodDefRid

/// ECMA-335 II.24.2.6's `MethodDefOrRef` coded index.
[<RequireQualifiedAccess>]
type MethodDefOrRef =
    | MethodDef of MethodDefRid
    | MemberRef of MemberRefRid

/// A version as the `Assembly` and `AssemblyRef` tables store it: four 16-bit components.
type MetadataVersion =
    {
        Major : uint16
        Minor : uint16
        Build : uint16
        Revision : uint16
    }

/// The `Module` row (ECMA-335 II.22.30). Its `Generation`, `EncId` and `EncBaseId` are always zero.
type ModuleRow =
    {
        Name : StringOffset
        Mvid : GuidIndex
    }

/// The `Assembly` row (ECMA-335 II.22.2).
type AssemblyRow =
    {
        HashAlgorithm : AssemblyHashAlgorithm
        Version : MetadataVersion
        /// Stored in full: the column is four bytes.
        Flags : AssemblyFlags
        PublicKey : BlobOffset
        Name : StringOffset
        Culture : StringOffset
    }

/// An `AssemblyRef` row (ECMA-335 II.22.5).
type AssemblyRefRow =
    {
        Version : MetadataVersion
        Flags : AssemblyFlags
        PublicKeyOrToken : BlobOffset
        Name : StringOffset
        Culture : StringOffset
        HashValue : BlobOffset
    }

/// A `TypeRef` row (ECMA-335 II.22.38).
type TypeRefRow =
    {
        ResolutionScope : ResolutionScope
        Name : StringOffset
        Namespace : StringOffset
    }

/// A `TypeDef` row (ECMA-335 II.22.37). Its `FieldList` and `MethodList` columns are not held: each
/// field and method names its owner instead, and the writer derives the ranges.
type TypeDefRow =
    {
        Flags : TypeAttributes
        Name : StringOffset
        Namespace : StringOffset
        Extends : TypeDefOrRef option
    }

/// A `Field` row (ECMA-335 II.22.15), with the type that owns it.
type FieldRow =
    {
        Owner : TypeDefRid
        Flags : FieldAttributes
        Name : StringOffset
        Signature : BlobOffset
    }

/// A `MethodDef` row (ECMA-335 II.22.26), with the type that owns it. Its `ParamList` column is not held:
/// each parameter names its method instead.
type MethodDefRow =
    {
        Owner : TypeDefRid
        /// The method body as ECMA-335 II.25.4 lays it out, header and extra sections included; `None`
        /// for a method with no body, whose `RVA` is 0. The writer chooses where it goes.
        Body : ImmutableArray<byte> option
        ImplFlags : MethodImplAttributes
        Flags : MethodAttributes
        Name : StringOffset
        Signature : BlobOffset
    }

/// A `Param` row (ECMA-335 II.22.33), with the method that owns it.
type ParamRow =
    {
        Owner : MethodDefRid
        Flags : ParameterAttributes
        /// 0 for the return value, otherwise the parameter's 1-based position.
        Sequence : uint16
        Name : StringOffset
    }

/// An `InterfaceImpl` row (ECMA-335 II.22.23).
type InterfaceImplRow =
    {
        Class : TypeDefRid
        Interface : TypeDefOrRef
    }

/// A `MemberRef` row (ECMA-335 II.22.25).
type MemberRefRow =
    {
        Parent : MemberRefParent
        Name : StringOffset
        Signature : BlobOffset
    }

/// A `StandAloneSig` row (ECMA-335 II.22.36).
type StandAloneSigRow =
    {
        Signature : BlobOffset
    }

/// A `MethodImpl` row (ECMA-335 II.22.27).
type MethodImplRow =
    {
        Class : TypeDefRid
        Body : MethodDefOrRef
        Declaration : MethodDefOrRef
    }

type private ByteContentComparer () =
    interface IEqualityComparer<ImmutableArray<byte>> with
        member _.Equals (x : ImmutableArray<byte>, y : ImmutableArray<byte>) : bool =
            x.AsSpan().SequenceEqual (y.AsSpan ())

        member _.GetHashCode (x : ImmutableArray<byte>) : int =
            let hash = HashCode ()
            hash.AddBytes (x.AsSpan ())
            hash.ToHashCode ()

/// The metadata of one dynamic module as it is being defined: what CoreCLR's read-write metadata
/// (`CMiniMdRW`) holds for a module that `Reflection.Emit` is building. Every table only grows, and each
/// row gets the RID it will keep, in definition order, so a field's, method's or parameter's RID is the
/// token CoreCLR hands out for it. Each heap only grows too, so an offset handed out once means the same
/// thing in every later state of the scope.
///
/// Every reference a row holds is checked against the scope when the row is added, so a scope never
/// refers to a row or heap entry it lacks.
type EmitScope =
    private
        {
            Strings : ImmutableList<string>
            StringOffsets : ImmutableDictionary<string, StringOffset>
            StringsByOffset : ImmutableDictionary<int, string>
            StringHeapLength : int
            Blobs : ImmutableList<ImmutableArray<byte>>
            BlobOffsets : ImmutableDictionary<ImmutableArray<byte>, BlobOffset>
            BlobsByOffset : ImmutableDictionary<int, ImmutableArray<byte>>
            BlobHeapLength : int
            Guids : ImmutableList<Guid>
            Module : ModuleRow
            Assembly : AssemblyRow option
            AssemblyRefs : ImmutableList<AssemblyRefRow>
            TypeRefs : ImmutableList<TypeRefRow>
            TypeDefs : ImmutableList<TypeDefRow>
            Fields : ImmutableList<FieldRow>
            MethodDefs : ImmutableList<MethodDefRow>
            Params : ImmutableList<ParamRow>
            InterfaceImpls : ImmutableList<InterfaceImplRow>
            MemberRefs : ImmutableList<MemberRefRow>
            StandAloneSigs : ImmutableList<StandAloneSigRow>
            MethodImpls : ImmutableList<MethodImplRow>
        }

/// Building an `EmitScope` by appending rows, and reading it back.
[<RequireQualifiedAccess>]
module EmitScope =
    let private strictUtf8 : UTF8Encoding = UTF8Encoding (false, true)

    // CoreCLR's emitter takes UTF-16 and stores UTF-8, replacing each unpaired surrogate with U+FFFD
    // (measured on .NET 10: an assembly named "Bad" and U+D800 reports the name "Bad" and U+FFFD).
    let private replacingUtf8 : UTF8Encoding = UTF8Encoding (false, false)

    /// The bytes `#Strings` holds for a string `internString` stored, without its terminating NUL.
    let internal utf8 (value : string) : byte[] = strictUtf8.GetBytes value

    /// The length of the compressed unsigned integer (ECMA-335 II.23.2) that prefixes a blob.
    let internal compressedLengthSize (length : int) : int =
        if length < 0x80 then
            1
        elif length < 0x4000 then
            2
        elif length <= 0x1FFFFFFF then
            4
        else
            failwith $"a blob of %d{length} bytes is longer than ECMA-335 II.23.2 can prefix"

    let private requireString (operation : string) (scope : EmitScope) (offset : StringOffset) : unit =
        if not (scope.StringsByOffset.ContainsKey offset.Offset) then
            invalidArg (nameof offset) $"%s{operation}: string offset %d{offset.Offset} is not in this scope's heap"

    let private requireBlob (operation : string) (scope : EmitScope) (offset : BlobOffset) : unit =
        if not (scope.BlobsByOffset.ContainsKey offset.Offset) then
            invalidArg (nameof offset) $"%s{operation}: blob offset %d{offset.Offset} is not in this scope's heap"

    let private requireRid (operation : string) (table : string) (count : int) (rid : int) : unit =
        if rid < 1 || rid > count then
            invalidArg (nameof rid) $"%s{operation}: %s{table} RID %d{rid} is not in this scope, which has %d{count}"

    let private requireTypeDefOrRef (operation : string) (scope : EmitScope) (target : TypeDefOrRef) : unit =
        match target with
        | TypeDefOrRef.TypeDef rid -> requireRid operation "TypeDef" scope.TypeDefs.Count rid.Rid
        | TypeDefOrRef.TypeRef rid -> requireRid operation "TypeRef" scope.TypeRefs.Count rid.Rid

    let private requireMethodDefOrRef (operation : string) (scope : EmitScope) (target : MethodDefOrRef) : unit =
        match target with
        | MethodDefOrRef.MethodDef rid -> requireRid operation "MethodDef" scope.MethodDefs.Count rid.Rid
        | MethodDefOrRef.MemberRef rid -> requireRid operation "MemberRef" scope.MemberRefs.Count rid.Rid

    /// The string at an offset this scope handed out.
    let stringAt (offset : StringOffset) (scope : EmitScope) : string =
        requireString "EmitScope.stringAt" scope offset
        scope.StringsByOffset.[offset.Offset]

    /// The blob at an offset this scope handed out.
    let blobAt (offset : BlobOffset) (scope : EmitScope) : ImmutableArray<byte> =
        requireBlob "EmitScope.blobAt" scope offset
        scope.BlobsByOffset.[offset.Offset]

    /// The GUID at an index this scope handed out.
    let guidAt (index : GuidIndex) (scope : EmitScope) : Guid =
        requireRid "EmitScope.guidAt" "#GUID" scope.Guids.Count index.Index
        scope.Guids.[index.Index - 1]

    /// The offset in the `#Strings` heap of what CoreCLR's emitter stores for a UTF-16 string, adding it
    /// if the heap lacks it, as CoreCLR's string pool interns. That is the string's UTF-8 encoding, with
    /// each unpaired surrogate replaced by U+FFFD, so `stringAt` gives back the string with those
    /// replacements made. Refuses a string with a NUL, which the heap's terminator would cut short.
    let internString (value : string) (scope : EmitScope) : StringOffset * EmitScope =
        if value.Contains '\000' then
            invalidArg (nameof value) $"a #Strings entry cannot hold a NUL, which would end it: %A{value}"

        let value = replacingUtf8.GetString (replacingUtf8.GetBytes value)

        match scope.StringOffsets.TryGetValue value with
        | true, offset -> offset, scope
        | false, _ ->
            let encoded = utf8 value
            let offset = StringOffset scope.StringHeapLength

            offset,
            { scope with
                Strings = scope.Strings.Add value
                StringOffsets = scope.StringOffsets.Add (value, offset)
                StringsByOffset = scope.StringsByOffset.Add (offset.Offset, value)
                StringHeapLength = scope.StringHeapLength + encoded.Length + 1
            }

    /// The offset of a blob in the `#Blob` heap, adding it if the heap lacks it, as CoreCLR's blob pool
    /// interns.
    let internBlob (value : ImmutableArray<byte>) (scope : EmitScope) : BlobOffset * EmitScope =
        if value.IsDefault then
            invalidArg (nameof value) "a #Blob entry must be an initialised array"

        match scope.BlobOffsets.TryGetValue value with
        | true, offset -> offset, scope
        | false, _ ->
            let offset = BlobOffset scope.BlobHeapLength

            offset,
            { scope with
                Blobs = scope.Blobs.Add value
                BlobOffsets = scope.BlobOffsets.Add (value, offset)
                BlobsByOffset = scope.BlobsByOffset.Add (offset.Offset, value)
                BlobHeapLength = scope.BlobHeapLength + compressedLengthSize value.Length + value.Length
            }

    /// The `AssemblyRef` row with this RID, if the scope has one.
    let tryAssemblyRefRid (rid : int) (scope : EmitScope) : AssemblyRefRid option =
        if rid >= 1 && rid <= scope.AssemblyRefs.Count then
            Some (AssemblyRefRid rid)
        else
            None

    /// The `TypeRef` row with this RID, if the scope has one.
    let tryTypeRefRid (rid : int) (scope : EmitScope) : TypeRefRid option =
        if rid >= 1 && rid <= scope.TypeRefs.Count then
            Some (TypeRefRid rid)
        else
            None

    /// The `TypeDef` row with this RID, if the scope has one.
    let tryTypeDefRid (rid : int) (scope : EmitScope) : TypeDefRid option =
        if rid >= 1 && rid <= scope.TypeDefs.Count then
            Some (TypeDefRid rid)
        else
            None

    /// The `MethodDef` row with this RID, if the scope has one.
    let tryMethodDefRid (rid : int) (scope : EmitScope) : MethodDefRid option =
        if rid >= 1 && rid <= scope.MethodDefs.Count then
            Some (MethodDefRid rid)
        else
            None

    /// The `MemberRef` row with this RID, if the scope has one.
    let tryMemberRefRid (rid : int) (scope : EmitScope) : MemberRefRid option =
        if rid >= 1 && rid <= scope.MemberRefs.Count then
            Some (MemberRefRid rid)
        else
            None

    /// The offset `internString` would return for `value` if the heap already holds what CoreCLR's
    /// emitter stores for it, without adding it.
    let tryStringOffset (value : string) (scope : EmitScope) : StringOffset option =
        if value.Contains '\000' then
            invalidArg (nameof value) $"a #Strings entry cannot hold a NUL, which would end it: %A{value}"

        match scope.StringOffsets.TryGetValue (replacingUtf8.GetString (replacingUtf8.GetBytes value)) with
        | true, offset -> Some offset
        | false, _ -> None

    /// The offset `internBlob` would return for `value` if the heap already holds it, without adding it.
    let tryBlobOffset (value : ImmutableArray<byte>) (scope : EmitScope) : BlobOffset option =
        if value.IsDefault then
            invalidArg (nameof value) "a #Blob entry must be an initialised array"

        match scope.BlobOffsets.TryGetValue value with
        | true, offset -> Some offset
        | false, _ -> None

    /// What CoreCLR's emit scope holds once it is opened for a dynamic module: the `Module` row and the
    /// `<Module>` type (RID 1), which owns module-scope members.
    let create (moduleName : string) (mvid : Guid) : EmitScope =
        let empty : EmitScope =
            {
                Strings = ImmutableList.Empty
                StringOffsets =
                    ImmutableDictionary.Create<string, StringOffset>(StringComparer.Ordinal).Add ("", StringOffset 0)
                StringsByOffset = ImmutableDictionary.Empty.Add (0, "")
                StringHeapLength = 1
                Blobs = ImmutableList.Empty
                BlobOffsets =
                    ImmutableDictionary
                        .Create<ImmutableArray<byte>, BlobOffset>(ByteContentComparer ())
                        .Add (ImmutableArray.Empty, BlobOffset 0)
                BlobsByOffset = ImmutableDictionary.Empty.Add (0, ImmutableArray.Empty)
                BlobHeapLength = 1
                Guids = ImmutableList.Create mvid
                Module =
                    {
                        Name = StringOffset 0
                        Mvid = GuidIndex 1
                    }
                Assembly = None
                AssemblyRefs = ImmutableList.Empty
                TypeRefs = ImmutableList.Empty
                TypeDefs = ImmutableList.Empty
                Fields = ImmutableList.Empty
                MethodDefs = ImmutableList.Empty
                Params = ImmutableList.Empty
                InterfaceImpls = ImmutableList.Empty
                MemberRefs = ImmutableList.Empty
                StandAloneSigs = ImmutableList.Empty
                MethodImpls = ImmutableList.Empty
            }

        let name, scope = internString moduleName empty
        let moduleTypeName, scope = internString "<Module>" scope

        { scope with
            Module =
                { scope.Module with
                    Name = name
                }
            TypeDefs =
                scope.TypeDefs.Add
                    {
                        Flags = TypeAttributes.NotPublic
                        Name = moduleTypeName
                        Namespace = StringOffset 0
                        Extends = None
                    }
        }

    /// Adds the `Assembly` row, which a scope holds at most once.
    let defineAssembly (row : AssemblyRow) (scope : EmitScope) : EmitScope =
        let operation = "EmitScope.defineAssembly"

        if scope.Assembly.IsSome then
            invalidOp $"%s{operation}: this scope already has its Assembly row"

        requireBlob operation scope row.PublicKey
        requireString operation scope row.Name
        requireString operation scope row.Culture

        { scope with
            Assembly = Some row
        }

    let addAssemblyRef (row : AssemblyRefRow) (scope : EmitScope) : AssemblyRefRid * EmitScope =
        let operation = "EmitScope.addAssemblyRef"
        requireBlob operation scope row.PublicKeyOrToken
        requireString operation scope row.Name
        requireString operation scope row.Culture
        requireBlob operation scope row.HashValue

        AssemblyRefRid (scope.AssemblyRefs.Count + 1),
        { scope with
            AssemblyRefs = scope.AssemblyRefs.Add row
        }

    let addTypeRef (row : TypeRefRow) (scope : EmitScope) : TypeRefRid * EmitScope =
        let operation = "EmitScope.addTypeRef"

        match row.ResolutionScope with
        | ResolutionScope.Module -> ()
        | ResolutionScope.AssemblyRef rid -> requireRid operation "AssemblyRef" scope.AssemblyRefs.Count rid.Rid
        | ResolutionScope.TypeRef rid -> requireRid operation "TypeRef" scope.TypeRefs.Count rid.Rid

        requireString operation scope row.Name
        requireString operation scope row.Namespace

        TypeRefRid (scope.TypeRefs.Count + 1),
        { scope with
            TypeRefs = scope.TypeRefs.Add row
        }

    let addTypeDef (row : TypeDefRow) (scope : EmitScope) : TypeDefRid * EmitScope =
        let operation = "EmitScope.addTypeDef"
        requireString operation scope row.Name
        requireString operation scope row.Namespace
        Option.iter (requireTypeDefOrRef operation scope) row.Extends

        TypeDefRid (scope.TypeDefs.Count + 1),
        { scope with
            TypeDefs = scope.TypeDefs.Add row
        }

    let addField (row : FieldRow) (scope : EmitScope) : FieldRid * EmitScope =
        let operation = "EmitScope.addField"
        requireRid operation "TypeDef" scope.TypeDefs.Count row.Owner.Rid
        requireString operation scope row.Name
        requireBlob operation scope row.Signature

        FieldRid (scope.Fields.Count + 1),
        { scope with
            Fields = scope.Fields.Add row
        }

    /// A `MethodDef` row's `Flags` and `ImplFlags` columns are two bytes wide.
    let private requireTwoBytes (operation : string) (column : string) (value : int) : unit =
        if value < 0 || value > 0xFFFF then
            invalidArg column $"%s{operation}: a method's %s{column} 0x%x{value} does not fit its column's two bytes"

    /// Add a `MethodDef` row, whose `Flags` and `ImplFlags` must each fit the column's two bytes.
    let addMethodDef (row : MethodDefRow) (scope : EmitScope) : MethodDefRid * EmitScope =
        let operation = "EmitScope.addMethodDef"
        requireRid operation "TypeDef" scope.TypeDefs.Count row.Owner.Rid
        requireString operation scope row.Name
        requireBlob operation scope row.Signature
        requireTwoBytes operation "Flags" (int row.Flags)
        requireTwoBytes operation "ImplFlags" (int row.ImplFlags)

        match row.Body with
        | Some body when body.IsDefaultOrEmpty ->
            invalidArg (nameof row) $"%s{operation}: a method body has at least its header byte"
        | _ -> ()

        MethodDefRid (scope.MethodDefs.Count + 1),
        { scope with
            MethodDefs = scope.MethodDefs.Add row
        }

    /// <summary>
    /// Set the <c>ImplFlags</c> column of a method already in the scope, which CoreCLR's emitter
    /// overwrites after adding the row (<c>RegMeta::SetMethodImplFlags</c>). Every other row, and every
    /// heap, is unchanged.
    /// </summary>
    /// <exception cref="ArgumentException">
    /// The scope has no such method, or <paramref name="flags"/> does not fit the column's two bytes.
    /// </exception>
    let setMethodImplFlags (method : MethodDefRid) (flags : MethodImplAttributes) (scope : EmitScope) : EmitScope =
        let operation = "EmitScope.setMethodImplFlags"
        requireRid operation "MethodDef" scope.MethodDefs.Count method.Rid
        requireTwoBytes operation "ImplFlags" (int flags)
        let index = method.Rid - 1

        { scope with
            MethodDefs =
                scope.MethodDefs.SetItem (
                    index,
                    { scope.MethodDefs.[index] with
                        ImplFlags = flags
                    }
                )
        }

    /// <summary>
    /// Set the <c>Extends</c> column of a type already in the scope, which CoreCLR's emitter overwrites
    /// after adding the row (<c>RegMeta::SetTypeParent</c>). Every other row, and every heap, is
    /// unchanged.
    /// </summary>
    /// <exception cref="ArgumentException">
    /// The scope has no such type, or no row that <paramref name="extends"/> names.
    /// </exception>
    let setTypeDefExtends (typeDef : TypeDefRid) (extends : TypeDefOrRef) (scope : EmitScope) : EmitScope =
        let operation = "EmitScope.setTypeDefExtends"
        requireRid operation "TypeDef" scope.TypeDefs.Count typeDef.Rid
        requireTypeDefOrRef operation scope extends
        let index = typeDef.Rid - 1

        { scope with
            TypeDefs =
                scope.TypeDefs.SetItem (
                    index,
                    { scope.TypeDefs.[index] with
                        Extends = Some extends
                    }
                )
        }

    let addParam (row : ParamRow) (scope : EmitScope) : ParamRid * EmitScope =
        let operation = "EmitScope.addParam"
        requireRid operation "MethodDef" scope.MethodDefs.Count row.Owner.Rid
        requireString operation scope row.Name

        ParamRid (scope.Params.Count + 1),
        { scope with
            Params = scope.Params.Add row
        }

    /// Adds an `InterfaceImpl` row. Nothing hands out its RID: the image sorts the table by class.
    let addInterfaceImpl (row : InterfaceImplRow) (scope : EmitScope) : EmitScope =
        let operation = "EmitScope.addInterfaceImpl"
        requireRid operation "TypeDef" scope.TypeDefs.Count row.Class.Rid
        requireTypeDefOrRef operation scope row.Interface

        { scope with
            InterfaceImpls = scope.InterfaceImpls.Add row
        }

    let addMemberRef (row : MemberRefRow) (scope : EmitScope) : MemberRefRid * EmitScope =
        let operation = "EmitScope.addMemberRef"

        match row.Parent with
        | MemberRefParent.TypeDef rid -> requireRid operation "TypeDef" scope.TypeDefs.Count rid.Rid
        | MemberRefParent.TypeRef rid -> requireRid operation "TypeRef" scope.TypeRefs.Count rid.Rid
        | MemberRefParent.MethodDef rid -> requireRid operation "MethodDef" scope.MethodDefs.Count rid.Rid

        requireString operation scope row.Name
        requireBlob operation scope row.Signature

        MemberRefRid (scope.MemberRefs.Count + 1),
        { scope with
            MemberRefs = scope.MemberRefs.Add row
        }

    let addStandAloneSig (row : StandAloneSigRow) (scope : EmitScope) : StandAloneSigRid * EmitScope =
        requireBlob "EmitScope.addStandAloneSig" scope row.Signature

        StandAloneSigRid (scope.StandAloneSigs.Count + 1),
        { scope with
            StandAloneSigs = scope.StandAloneSigs.Add row
        }

    /// Adds a `MethodImpl` row. Nothing hands out its RID: the image sorts the table by class.
    let addMethodImpl (row : MethodImplRow) (scope : EmitScope) : EmitScope =
        let operation = "EmitScope.addMethodImpl"
        requireRid operation "TypeDef" scope.TypeDefs.Count row.Class.Rid
        requireMethodDefOrRef operation scope row.Body
        requireMethodDefOrRef operation scope row.Declaration

        { scope with
            MethodImpls = scope.MethodImpls.Add row
        }

    /// The `#Strings` entries other than the empty string, in the order they were interned: each
    /// starts where the previous one's terminator ends.
    let strings (scope : EmitScope) : ImmutableList<string> = scope.Strings

    /// The `#Blob` entries other than the empty blob, in the order they were interned.
    let blobs (scope : EmitScope) : ImmutableList<ImmutableArray<byte>> = scope.Blobs

    /// The `#GUID` entries; index `i` is at position `i - 1`.
    let guids (scope : EmitScope) : ImmutableList<Guid> = scope.Guids

    let moduleRow (scope : EmitScope) : ModuleRow = scope.Module
    let assembly (scope : EmitScope) : AssemblyRow option = scope.Assembly

    // Each of these is in RID order: the row at position `i` has RID `i + 1`.
    let assemblyRefs (scope : EmitScope) : ImmutableList<AssemblyRefRow> = scope.AssemblyRefs
    let typeRefs (scope : EmitScope) : ImmutableList<TypeRefRow> = scope.TypeRefs
    let typeDefs (scope : EmitScope) : ImmutableList<TypeDefRow> = scope.TypeDefs
    let fields (scope : EmitScope) : ImmutableList<FieldRow> = scope.Fields
    let methodDefs (scope : EmitScope) : ImmutableList<MethodDefRow> = scope.MethodDefs
    let parameters (scope : EmitScope) : ImmutableList<ParamRow> = scope.Params
    let memberRefs (scope : EmitScope) : ImmutableList<MemberRefRow> = scope.MemberRefs
    let standAloneSigs (scope : EmitScope) : ImmutableList<StandAloneSigRow> = scope.StandAloneSigs

    /// In the order they were added, which is not the image's: the image sorts them by class.
    let interfaceImpls (scope : EmitScope) : ImmutableList<InterfaceImplRow> = scope.InterfaceImpls

    /// In the order they were added, which is not the image's: the image sorts them by class.
    let methodImpls (scope : EmitScope) : ImmutableList<MethodImplRow> = scope.MethodImpls
