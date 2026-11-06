namespace WoofWare.PawPrint

open System
open System.Collections.Immutable
open System.Reflection.Metadata.Ecma335

/// <summary>
/// One way in which a later emit scope fails to extend an earlier one, so that an image of the later
/// scope could not replace the earlier image without changing something the runtime has already read.
/// </summary>
/// <remarks>
/// A row's <c>position</c> is its RID in the scope, which is its RID in the image, except in
/// <c>InterfaceImpl</c> and <c>MethodImpl</c>: there it is the 1-based position at which the scope
/// added the row, because the image sorts those tables by class.
/// </remarks>
[<RequireQualifiedAccess>]
type EmitScopeExtensionViolation =
    /// A row of the earlier scope differs in the later one, or the later scope does not have it.
    | RowChanged of table : TableIndex * position : int

    /// An entry of one of the earlier scope's heaps differs in the later one, or the later scope does
    /// not have it. <c>entry</c> counts from 1 in the order the entries were interned, the empty
    /// string or blob not counted.
    | HeapEntryChanged of heap : HeapIndex * entry : int

    /// A row the later scope adds is a member of a type already created: a field or method it
    /// declares, a parameter of one of its methods, or one of its interface implementations or
    /// method impls.
    | AddedToCreatedType of table : TableIndex * position : int * createdType : TypeDefRid

    /// A row of the earlier scope differs in the later one only in a column the emitter fills in after
    /// adding the row (a method's implementation flags), and the row is a member of a type already
    /// created.
    | FilledInCreatedType of table : TableIndex * position : int * createdType : TypeDefRid

    /// A row of a created type, in a table the image sorts, has a different RID in the later image
    /// than in the earlier one, because the later scope added a row for a type that sorts before it.
    | SortedRowMoved of table : TableIndex * position : int * createdType : TypeDefRid * before : int * after : int

/// Whether one emit scope extends another, given which of its types the runtime has already created.
[<RequireQualifiedAccess>]
module EmitScopeExtension =

    /// <summary>
    /// Every way in which <paramref name="after"/> fails to extend <paramref name="before"/>.
    /// </summary>
    /// <remarks>
    /// Empty when <paramref name="after"/> was built from <paramref name="before"/> by appending rows
    /// and interning entries, none of them for a created type and none sorting before a created type's
    /// rows in a sorted table. Then an image of <paramref name="after"/> reads back every row, heap
    /// entry and created type of <paramref name="before"/>'s as <paramref name="before"/>'s image
    /// does. A history that was rewritten, rather than appended to, is refused even where the image
    /// happens to read back the same.
    /// </remarks>
    /// <param name="created">
    /// The types of <paramref name="before"/> the runtime has created, whose members are fixed from
    /// then on.
    /// </param>
    /// <param name="before">The scope whose image the runtime has read.</param>
    /// <param name="after">The scope whose image is to replace it.</param>
    let violations
        (created : ImmutableHashSet<TypeDefRid>)
        (before : EmitScope)
        (after : EmitScope)
        : EmitScopeExtensionViolation list
        =
        // Each row of `before` at the same position of `after`, compared whole.
        let rewritten (table : TableIndex) (beforeRows : ImmutableList<'row>) (afterRows : ImmutableList<'row>) =
            [
                for i in 0 .. beforeRows.Count - 1 do
                    if i >= afterRows.Count || beforeRows.[i] <> afterRows.[i] then
                        yield EmitScopeExtensionViolation.RowChanged (table, i + 1)
            ]

        let heapRewritten
            (heap : HeapIndex)
            (same : 'entry -> 'entry -> bool)
            (beforeEntries : ImmutableList<'entry>)
            (afterEntries : ImmutableList<'entry>)
            =
            [
                for i in 0 .. beforeEntries.Count - 1 do
                    if i >= afterEntries.Count || not (same beforeEntries.[i] afterEntries.[i]) then
                        yield EmitScopeExtensionViolation.HeapEntryChanged (heap, i + 1)
            ]

        // Each row `after` appends whose type, as `owner` names it, is created.
        let addedToCreated (table : TableIndex) (beforeCount : int) (afterRows : ImmutableList<'row>) owner =
            [
                for i in beforeCount .. afterRows.Count - 1 do
                    let owner : TypeDefRid = owner afterRows.[i]

                    if created.Contains owner then
                        yield EmitScopeExtensionViolation.AddedToCreatedType (table, i + 1, owner)
            ]

        // Each row of a created type in a sorted table, unchanged in `after`, whose image RID moved.
        let moved
            (table : TableIndex)
            (beforeRows : ImmutableList<'row>)
            (afterRows : ImmutableList<'row>)
            (cls : 'row -> TypeDefRid)
            (beforeRids : ImmutableArray<int>)
            (afterRids : ImmutableArray<int>)
            =
            [
                for i in 0 .. beforeRows.Count - 1 do
                    let owner = cls beforeRows.[i]

                    if
                        created.Contains owner
                        && i < afterRows.Count
                        && beforeRows.[i] = afterRows.[i]
                        && beforeRids.[i] <> afterRids.[i]
                    then
                        yield
                            EmitScopeExtensionViolation.SortedRowMoved (
                                table,
                                i + 1,
                                owner,
                                beforeRids.[i],
                                afterRids.[i]
                            )
            ]

        let methodOwner (method : MethodDefRid) : TypeDefRid =
            (EmitScope.methodDefs after).[method.Rid - 1].Owner

        [
            if EmitScope.moduleRow before <> EmitScope.moduleRow after then
                yield EmitScopeExtensionViolation.RowChanged (TableIndex.Module, 1)

            yield! rewritten TableIndex.TypeRef (EmitScope.typeRefs before) (EmitScope.typeRefs after)
            yield! rewritten TableIndex.TypeDef (EmitScope.typeDefs before) (EmitScope.typeDefs after)

            yield! rewritten TableIndex.Field (EmitScope.fields before) (EmitScope.fields after)

            yield!
                addedToCreated
                    TableIndex.Field
                    (EmitScope.fields before).Count
                    (EmitScope.fields after)
                    (fun row -> row.Owner)

            yield! rewritten TableIndex.MethodDef (EmitScope.methodDefs before) (EmitScope.methodDefs after)

            yield!
                addedToCreated
                    TableIndex.MethodDef
                    (EmitScope.methodDefs before).Count
                    (EmitScope.methodDefs after)
                    (fun row -> row.Owner)

            yield! rewritten TableIndex.Param (EmitScope.parameters before) (EmitScope.parameters after)

            yield!
                addedToCreated
                    TableIndex.Param
                    (EmitScope.parameters before).Count
                    (EmitScope.parameters after)
                    (fun row -> methodOwner row.Owner)

            yield! rewritten TableIndex.InterfaceImpl (EmitScope.interfaceImpls before) (EmitScope.interfaceImpls after)

            yield!
                addedToCreated
                    TableIndex.InterfaceImpl
                    (EmitScope.interfaceImpls before).Count
                    (EmitScope.interfaceImpls after)
                    (fun row -> row.Class)

            yield!
                moved
                    TableIndex.InterfaceImpl
                    (EmitScope.interfaceImpls before)
                    (EmitScope.interfaceImpls after)
                    (fun row -> row.Class)
                    (EmitScopeImage.interfaceImplRids before)
                    (EmitScopeImage.interfaceImplRids after)

            yield! rewritten TableIndex.MemberRef (EmitScope.memberRefs before) (EmitScope.memberRefs after)
            yield! rewritten TableIndex.StandAloneSig (EmitScope.standAloneSigs before) (EmitScope.standAloneSigs after)

            yield! rewritten TableIndex.MethodImpl (EmitScope.methodImpls before) (EmitScope.methodImpls after)

            yield!
                addedToCreated
                    TableIndex.MethodImpl
                    (EmitScope.methodImpls before).Count
                    (EmitScope.methodImpls after)
                    (fun row -> row.Class)

            yield!
                moved
                    TableIndex.MethodImpl
                    (EmitScope.methodImpls before)
                    (EmitScope.methodImpls after)
                    (fun row -> row.Class)
                    (EmitScopeImage.methodImplRids before)
                    (EmitScopeImage.methodImplRids after)

            // A scope gains its `Assembly` row once; from then on the row is fixed.
            match EmitScope.assembly before with
            | Some row when EmitScope.assembly after <> Some row ->
                yield EmitScopeExtensionViolation.RowChanged (TableIndex.Assembly, 1)
            | Some _
            | None -> ()

            yield! rewritten TableIndex.AssemblyRef (EmitScope.assemblyRefs before) (EmitScope.assemblyRefs after)

            yield! heapRewritten HeapIndex.String (=) (EmitScope.strings before) (EmitScope.strings after)

            yield!
                heapRewritten
                    HeapIndex.Blob
                    (fun (x : ImmutableArray<byte>) y -> x.AsSpan().SequenceEqual (y.AsSpan ()))
                    (EmitScope.blobs before)
                    (EmitScope.blobs after)

            yield! heapRewritten HeapIndex.Guid (=) (EmitScope.guids before) (EmitScope.guids after)
        ]
