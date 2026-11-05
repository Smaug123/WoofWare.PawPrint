namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata
open System.Reflection.Metadata.Ecma335
open System.Reflection.PortableExecutable
open System.Text

/// A PE image with one `.text` section, whose bytes depend on where the section lands. SRM's
/// `ManagedPEBuilder` cannot be used, because it takes only `MetadataBuilder`'s tables.
type private SingleTextSectionBuilder (content : SectionLocation -> BlobBuilder, corHeaderSize : int) =
    inherit
        PEBuilder (
            PEHeaderBuilder.CreateLibraryHeader (),
            Func<_, _> (fun (_ : Blob seq) -> BlobContentId (Guid.Empty, 0u))
        )

    // Set by `SerializeSection`, which `PEBuilder.Serialize` calls before `GetDirectories`.
    let mutable textRva : int option = None

    override _.CreateSections () =
        ImmutableArray.Create (
            PEBuilder.Section (
                ".text",
                SectionCharacteristics.MemRead
                ||| SectionCharacteristics.MemExecute
                ||| SectionCharacteristics.ContainsCode
            )
        )

    override _.SerializeSection (name : string, location : SectionLocation) : BlobBuilder =
        if name <> ".text" then
            failwith $"SingleTextSectionBuilder: asked for section %s{name}, but it made only .text"

        textRva <- Some location.RelativeVirtualAddress
        content location

    override _.GetDirectories () : PEDirectoriesBuilder =
        match textRva with
        | None -> failwith "SingleTextSectionBuilder: PEBuilder asked for directories before laying out .text"
        | Some rva ->
            // The CLI header is at the start of `.text`.
            let directories = PEDirectoriesBuilder ()
            directories.CorHeaderTable <- DirectoryEntry (rva, corHeaderSize)
            directories

/// An `EmitScope` serialised as an ECMA-335 image: CoreCLR's model of a dynamic module's metadata in a
/// form PawPrint's loader reads like any assembly on disk.
///
/// Every RID in the image is the one the scope gave its row, except in the tables ECMA-335 requires to be
/// sorted (`InterfaceImpl`, `MethodImpl`), which are sorted stably by class. A child table whose rows are
/// not grouped by parent, or whose parameters are not in sequence order within their method, gets a
/// pointer table (`FieldPtr`, `MethodPtr`, `ParamPtr`), as CoreCLR's read-write metadata starts one, so the table stream is always the uncompressed `#-` kind, the only one
/// that may hold pointer tables. Every heap offset is the one the scope handed out.
[<RequireQualifiedAccess>]
module EmitScopeImage =
    let private cliHeaderSize : int = 72

    let private tableCount : int = 64

    /// The rule ECMA-335 II.24.2.6 and SRM's reader share: a plain index into a table is two bytes
    /// while the table has fewer than 2^16 rows.
    let private indexSize (rowCount : int) : int = if rowCount < 0x10000 then 2 else 4

    /// A coded index is two bytes while every table it can name has fewer than `2^(16 - tagBits)` rows.
    let private codedIndexSize (rowCounts : int[]) (tables : TableIndex list) (tagBits : int) : int =
        if tables |> List.forall (fun t -> rowCounts.[int t] < (1 <<< (16 - tagBits))) then
            2
        else
            4

    let private writeIndex (builder : BlobBuilder) (size : int) (column : string) (value : int) : unit =
        if size = 2 then
            if value > 0xFFFF then
                failwith
                    $"EmitScopeImage: %s{column} is %d{value}, which its two-byte column cannot hold; ECMA-335 sizes the column by the table's row count alone, so no image can carry this scope"

            builder.WriteUInt16 (uint16 value)
        else
            builder.WriteInt32 value

    /// The children's RIDs in the order their parents list them, which is by `keys`, ties kept in RID
    /// order: the contents of the pointer table, or `[||]` when that order is the RIDs' own and none is
    /// needed.
    let private pointerTable<'key when 'key : comparison> (keys : 'key[]) : int[] =
        let order =
            keys |> Array.mapi (fun i key -> key, i + 1) |> Array.sort |> Array.map snd

        if order |> Array.indexed |> Array.forall (fun (i, rid) -> rid = i + 1) then
            [||]
        else
            order

    /// Each parent's list column: the position, in the children sorted by owner, where its run starts,
    /// or where it would start if it is empty.
    let private listStarts (owners : int[]) (parentCount : int) : int[] =
        let counts = Array.zeroCreate (parentCount + 2)

        for owner in owners do
            if owner < 1 || owner > parentCount then
                failwith $"EmitScopeImage: a child names owner %d{owner}, but there are %d{parentCount} owners"

            counts.[owner] <- counts.[owner] + 1

        let starts = Array.zeroCreate parentCount
        let mutable next = 1

        for parent in 1..parentCount do
            starts.[parent - 1] <- next
            next <- next + counts.[parent]

        starts

    let private stringHeap (scope : EmitScope) : BlobBuilder =
        let heap = BlobBuilder ()
        heap.WriteByte 0uy

        for value in EmitScope.strings scope do
            let offset = (EmitScope.internString value scope |> fst).Offset

            if heap.Count <> offset then
                failwith
                    $"EmitScopeImage: #Strings entry %A{value} was given offset %d{offset}, but lands at %d{heap.Count}"

            heap.WriteBytes (EmitScope.utf8 value)
            heap.WriteByte 0uy

        heap.Align 4
        heap

    let private blobHeap (scope : EmitScope) : BlobBuilder =
        let heap = BlobBuilder ()
        heap.WriteByte 0uy

        for value in EmitScope.blobs scope do
            let offset = (EmitScope.internBlob value scope |> fst).Offset

            if heap.Count <> offset then
                failwith $"EmitScopeImage: a #Blob entry was given offset %d{offset}, but lands at %d{heap.Count}"

            heap.WriteCompressedInteger value.Length
            heap.WriteBytes value

        heap.Align 4
        heap

    let private guidHeap (scope : EmitScope) : BlobBuilder =
        let heap = BlobBuilder ()

        for value in EmitScope.guids scope do
            heap.WriteGuid value

        heap

    /// `#US` holds only its empty entry: nothing an emit scope holds names a user string yet.
    let private userStringHeap () : BlobBuilder =
        let heap = BlobBuilder ()
        heap.WriteByte 0uy
        heap.Align 4
        heap

    /// The `#-` stream. `methodRva` gives each method's body RVA, or 0 for none.
    let private tableStream
        (scope : EmitScope)
        (methodRva : int -> int)
        (largeStrings : bool)
        (largeGuids : bool)
        (largeBlobs : bool)
        : BlobBuilder
        =
        let typeDefs = EmitScope.typeDefs scope
        let fields = EmitScope.fields scope
        let methods = EmitScope.methodDefs scope
        let parameters = EmitScope.parameters scope
        let fieldOwners = fields |> Seq.map _.Owner.Rid |> Array.ofSeq
        let methodOwners = methods |> Seq.map _.Owner.Rid |> Array.ofSeq
        let paramOwners = parameters |> Seq.map _.Owner.Rid |> Array.ofSeq

        let fieldPtr = pointerTable fieldOwners
        let methodPtr = pointerTable methodOwners

        // A method lists its parameters by sequence: `CMiniMdRW::FixParamSequence` slides each new one
        // ahead of every earlier one with a greater sequence, starting a `ParamPtr` to do so.
        let paramPtr =
            parameters
            |> Seq.map (fun p -> p.Owner.Rid, p.Sequence)
            |> Array.ofSeq
            |> pointerTable

        // ECMA-335 II.22.23 and II.22.27 require these sorted by class. Sorting by (class, position) is
        // the stable sort, which keeps each class's rows in the order they were added.
        let interfaceImpls =
            EmitScope.interfaceImpls scope
            |> Seq.indexed
            |> Seq.sortBy (fun (i, row) -> row.Class.Rid, i)
            |> Seq.map snd
            |> Array.ofSeq

        let methodImpls =
            EmitScope.methodImpls scope
            |> Seq.indexed
            |> Seq.sortBy (fun (i, row) -> row.Class.Rid, i)
            |> Seq.map snd
            |> Array.ofSeq

        let rowCounts = Array.zeroCreate tableCount
        rowCounts.[int TableIndex.Module] <- 1
        rowCounts.[int TableIndex.TypeRef] <- (EmitScope.typeRefs scope).Count
        rowCounts.[int TableIndex.TypeDef] <- typeDefs.Count
        rowCounts.[int TableIndex.FieldPtr] <- fieldPtr.Length
        rowCounts.[int TableIndex.Field] <- fields.Count
        rowCounts.[int TableIndex.MethodPtr] <- methodPtr.Length
        rowCounts.[int TableIndex.MethodDef] <- methods.Count
        rowCounts.[int TableIndex.ParamPtr] <- paramPtr.Length
        rowCounts.[int TableIndex.Param] <- parameters.Count
        rowCounts.[int TableIndex.InterfaceImpl] <- interfaceImpls.Length
        rowCounts.[int TableIndex.MemberRef] <- (EmitScope.memberRefs scope).Count
        rowCounts.[int TableIndex.StandAloneSig] <- (EmitScope.standAloneSigs scope).Count
        rowCounts.[int TableIndex.MethodImpl] <- methodImpls.Length
        rowCounts.[int TableIndex.Assembly] <- (if (EmitScope.assembly scope).IsSome then 1 else 0)
        rowCounts.[int TableIndex.AssemblyRef] <- (EmitScope.assemblyRefs scope).Count

        let stringSize = if largeStrings then 4 else 2
        let guidSize = if largeGuids then 4 else 2
        let blobSize = if largeBlobs then 4 else 2

        let typeDefOrRefSize =
            codedIndexSize rowCounts [ TableIndex.TypeDef ; TableIndex.TypeRef ; TableIndex.TypeSpec ] 2

        let resolutionScopeSize =
            codedIndexSize
                rowCounts
                [
                    TableIndex.Module
                    TableIndex.ModuleRef
                    TableIndex.AssemblyRef
                    TableIndex.TypeRef
                ]
                2

        let memberRefParentSize =
            codedIndexSize
                rowCounts
                [
                    TableIndex.TypeDef
                    TableIndex.TypeRef
                    TableIndex.ModuleRef
                    TableIndex.MethodDef
                    TableIndex.TypeSpec
                ]
                3

        let methodDefOrRefSize =
            codedIndexSize rowCounts [ TableIndex.MethodDef ; TableIndex.MemberRef ] 1

        let tables = BlobBuilder ()

        let str (column : string) (offset : StringOffset) =
            writeIndex tables stringSize column offset.Offset

        let blob (column : string) (offset : BlobOffset) =
            writeIndex tables blobSize column offset.Offset

        let index (table : TableIndex) (column : string) (rid : int) =
            writeIndex tables (indexSize rowCounts.[int table]) column rid

        let typeDefOrRef (column : string) (target : TypeDefOrRef option) =
            let coded =
                match target with
                | None -> 0
                | Some (TypeDefOrRef.TypeDef rid) -> (rid.Rid <<< 2) ||| 0
                | Some (TypeDefOrRef.TypeRef rid) -> (rid.Rid <<< 2) ||| 1

            writeIndex tables typeDefOrRefSize column coded

        let methodDefOrRef (column : string) (target : MethodDefOrRef) =
            let coded =
                match target with
                | MethodDefOrRef.MethodDef rid -> (rid.Rid <<< 1) ||| 0
                | MethodDefOrRef.MemberRef rid -> (rid.Rid <<< 1) ||| 1

            writeIndex tables methodDefOrRefSize column coded

        let version (v : MetadataVersion) =
            tables.WriteUInt16 v.Major
            tables.WriteUInt16 v.Minor
            tables.WriteUInt16 v.Build
            tables.WriteUInt16 v.Revision

        // Module (II.22.30)
        let moduleRow = EmitScope.moduleRow scope
        tables.WriteUInt16 0us
        str "Module.Name" moduleRow.Name
        writeIndex tables guidSize "Module.Mvid" moduleRow.Mvid.Index
        writeIndex tables guidSize "Module.EncId" 0
        writeIndex tables guidSize "Module.EncBaseId" 0

        // TypeRef (II.22.38)
        for row in EmitScope.typeRefs scope do
            let coded =
                match row.ResolutionScope with
                | ResolutionScope.Module -> (1 <<< 2) ||| 0
                | ResolutionScope.AssemblyRef rid -> (rid.Rid <<< 2) ||| 2
                | ResolutionScope.TypeRef rid -> (rid.Rid <<< 2) ||| 3

            writeIndex tables resolutionScopeSize "TypeRef.ResolutionScope" coded
            str "TypeRef.TypeName" row.Name
            str "TypeRef.TypeNamespace" row.Namespace

        // TypeDef (II.22.37). Its lists index the pointer tables when there are any; a pointer table
        // has as many rows as the table it points into, so the column is the same size either way.
        let fieldLists = listStarts fieldOwners typeDefs.Count
        let methodLists = listStarts methodOwners typeDefs.Count

        for i in 0 .. typeDefs.Count - 1 do
            let row = typeDefs.[i]
            tables.WriteUInt32 (uint32 row.Flags)
            str "TypeDef.TypeName" row.Name
            str "TypeDef.TypeNamespace" row.Namespace
            typeDefOrRef "TypeDef.Extends" row.Extends
            index TableIndex.Field "TypeDef.FieldList" fieldLists.[i]
            index TableIndex.MethodDef "TypeDef.MethodList" methodLists.[i]

        // FieldPtr (II.22 does not define the pointer tables; their one column is a plain index)
        for rid in fieldPtr do
            index TableIndex.Field "FieldPtr.Field" rid

        // Field (II.22.15)
        for row in fields do
            tables.WriteUInt16 (uint16 row.Flags)
            str "Field.Name" row.Name
            blob "Field.Signature" row.Signature

        for rid in methodPtr do
            index TableIndex.MethodDef "MethodPtr.Method" rid

        // MethodDef (II.22.26)
        let paramLists = listStarts paramOwners methods.Count

        for i in 0 .. methods.Count - 1 do
            let row = methods.[i]
            tables.WriteInt32 (methodRva (i + 1))
            tables.WriteUInt16 (uint16 row.ImplFlags)
            tables.WriteUInt16 (uint16 row.Flags)
            str "MethodDef.Name" row.Name
            blob "MethodDef.Signature" row.Signature
            index TableIndex.Param "MethodDef.ParamList" paramLists.[i]

        for rid in paramPtr do
            index TableIndex.Param "ParamPtr.Param" rid

        // Param (II.22.33)
        for row in parameters do
            tables.WriteUInt16 (uint16 row.Flags)
            tables.WriteUInt16 row.Sequence
            str "Param.Name" row.Name

        // InterfaceImpl (II.22.23)
        for row in interfaceImpls do
            index TableIndex.TypeDef "InterfaceImpl.Class" row.Class.Rid
            typeDefOrRef "InterfaceImpl.Interface" (Some row.Interface)

        // MemberRef (II.22.25)
        for row in EmitScope.memberRefs scope do
            let coded =
                match row.Parent with
                | MemberRefParent.TypeDef rid -> (rid.Rid <<< 3) ||| 0
                | MemberRefParent.TypeRef rid -> (rid.Rid <<< 3) ||| 1
                | MemberRefParent.MethodDef rid -> (rid.Rid <<< 3) ||| 3

            writeIndex tables memberRefParentSize "MemberRef.Class" coded
            str "MemberRef.Name" row.Name
            blob "MemberRef.Signature" row.Signature

        // StandAloneSig (II.22.36)
        for row in EmitScope.standAloneSigs scope do
            blob "StandAloneSig.Signature" row.Signature

        // MethodImpl (II.22.27)
        for row in methodImpls do
            index TableIndex.TypeDef "MethodImpl.Class" row.Class.Rid
            methodDefOrRef "MethodImpl.MethodBody" row.Body
            methodDefOrRef "MethodImpl.MethodDeclaration" row.Declaration

        // Assembly (II.22.2)
        match EmitScope.assembly scope with
        | None -> ()
        | Some row ->
            tables.WriteUInt32 (uint32 row.HashAlgorithm)
            version row.Version
            tables.WriteUInt32 (uint32 row.Flags)
            blob "Assembly.PublicKey" row.PublicKey
            str "Assembly.Name" row.Name
            str "Assembly.Culture" row.Culture

        // AssemblyRef (II.22.5)
        for row in EmitScope.assemblyRefs scope do
            version row.Version
            tables.WriteUInt32 (uint32 row.Flags)
            blob "AssemblyRef.PublicKeyOrToken" row.PublicKeyOrToken
            str "AssemblyRef.Name" row.Name
            str "AssemblyRef.Culture" row.Culture
            blob "AssemblyRef.HashValue" row.HashValue

        // The header (II.24.2.6), then the rows above, which are in table-number order.
        let stream = BlobBuilder ()
        stream.WriteUInt32 0u
        stream.WriteByte 2uy
        stream.WriteByte 0uy

        stream.WriteByte (
            (if largeStrings then 0x01uy else 0uy)
            ||| (if largeGuids then 0x02uy else 0uy)
            ||| (if largeBlobs then 0x04uy else 0uy)
        )

        stream.WriteByte 1uy

        let present =
            rowCounts
            |> Array.indexed
            |> Array.fold (fun mask (table, rows) -> if rows > 0 then mask ||| (1UL <<< table) else mask) 0UL

        stream.WriteUInt64 present
        // No table is declared sorted, so SRM's reader checks the order of each one that must be,
        // rather than trusting the declaration.
        stream.WriteUInt64 0UL

        for rows in rowCounts do
            if rows > 0 then
                stream.WriteInt32 rows

        stream.LinkSuffix tables
        stream.Align 4
        stream

    /// The metadata root (II.24.2.1) and its streams.
    let private metadataRoot (scope : EmitScope) (methodRva : int -> int) : BlobBuilder =
        let strings = stringHeap scope
        let blobs = blobHeap scope
        let guids = guidHeap scope
        let userStrings = userStringHeap ()

        // A heap index is two bytes while the heap is smaller than 2^16 bytes (II.24.2.6).
        let tables =
            tableStream scope methodRva (strings.Count >= 0x10000) (guids.Count >= 0x10000) (blobs.Count >= 0x10000)

        let streams : (string * BlobBuilder) list =
            [
                "#-", tables
                "#Strings", strings
                "#US", userStrings
                "#GUID", guids
                "#Blob", blobs
            ]

        let versionString = Encoding.UTF8.GetBytes "v4.0.30319"
        let paddedVersionLength = (versionString.Length + 1 + 3) &&& ~~~3
        let streamHeaderSize (name : string) = 8 + ((name.Length + 1 + 3) &&& ~~~3)

        let headerSize =
            16 + paddedVersionLength + 4 + (streams |> List.sumBy (fst >> streamHeaderSize))

        let root = BlobBuilder ()
        root.WriteUInt32 0x424A5342u
        root.WriteUInt16 1us
        root.WriteUInt16 1us
        root.WriteUInt32 0u
        root.WriteInt32 paddedVersionLength
        root.WriteBytes versionString
        root.WriteBytes (0uy, paddedVersionLength - versionString.Length)
        root.WriteUInt16 0us
        root.WriteUInt16 (uint16 streams.Length)

        let mutable offset = headerSize

        for name, content in streams do
            root.WriteInt32 offset
            root.WriteInt32 content.Count
            let nameBytes = Encoding.ASCII.GetBytes name
            root.WriteBytes nameBytes
            root.WriteBytes (0uy, streamHeaderSize name - 8 - nameBytes.Length)
            offset <- offset + content.Count

        if root.Count <> headerSize then
            failwith
                $"EmitScopeImage: the metadata root's header is %d{root.Count} bytes, not the %d{headerSize} computed"

        for _, content in streams do
            root.LinkSuffix content

        root

    /// The PE image holding the scope's metadata and method bodies, which `Assembly.read` reads as it
    /// reads any assembly. A function of the scope alone, so a replay rebuilds the same bytes.
    ///
    /// Fails if a list column would have to name the row one past the end of a table of exactly 65535
    /// rows, which no ECMA-335 image can hold.
    let write (scope : EmitScope) : byte[] =
        let methods = EmitScope.methodDefs scope

        let text (location : SectionLocation) : BlobBuilder =
            // `.text` is the CLI header, then the method bodies, then the metadata. Each body starts on a
            // 4-byte boundary, which a fat header needs (II.25.4.3).
            let bodies = BlobBuilder ()
            let bodyOffsets = Array.zeroCreate methods.Count

            for i in 0 .. methods.Count - 1 do
                match methods.[i].Body with
                | None -> ()
                | Some body ->
                    bodies.Align 4
                    bodyOffsets.[i] <- cliHeaderSize + bodies.Count
                    bodies.WriteBytes body

            bodies.Align 4

            let methodRva (rid : int) : int =
                match methods.[rid - 1].Body with
                | None -> 0
                | Some _ -> location.RelativeVirtualAddress + bodyOffsets.[rid - 1]

            let metadata = metadataRoot scope methodRva
            let metadataOffset = cliHeaderSize + bodies.Count

            // The CLI header (II.25.3.3).
            let section = BlobBuilder ()
            section.WriteInt32 cliHeaderSize
            section.WriteUInt16 2us
            section.WriteUInt16 5us
            section.WriteInt32 (location.RelativeVirtualAddress + metadataOffset)
            section.WriteInt32 metadata.Count
            section.WriteUInt32 (uint32 CorFlags.ILOnly)
            // No entry point.
            section.WriteUInt32 0u
            // Resources, StrongNameSignature, CodeManagerTable, VTableFixups, ExportAddressTableJumps and
            // ManagedNativeHeader: all empty.
            section.WriteBytes (0uy, 6 * 8)

            if section.Count <> cliHeaderSize then
                failwith $"EmitScopeImage: the CLI header is %d{section.Count} bytes, not %d{cliHeaderSize}"

            section.LinkSuffix bodies
            section.LinkSuffix metadata
            section

        let image = BlobBuilder ()

        SingleTextSectionBuilder(text, cliHeaderSize).Serialize image
        |> ignore<BlobContentId>

        image.ToArray ()
